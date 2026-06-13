use config::{GlueConfigSchemaGeneration, GlueConfigSchemaGenerationGo};
use convert_case::Case;
use lang::{AnonModel, AstNode, ConstDef, ConstValue, Enum, Field, GlueIr, Model, SourceCodeMetadata, SymId, Type, TypeAtom};

use crate::{
    CodeGenError, CodeGenerator,
    codegen::CodeGenResult,
    context::{AnonymousTypeNamer, CodeGenContext, FieldExt, NamedExt, TypeMapper, convert_generated_identifier_case, convert_user_identifier_case},
};

#[derive(Default)]
pub struct CodeGenGo;

impl CodeGenerator for CodeGenGo {
    fn generate(&self, ir: GlueIr, source: &SourceCodeMetadata, config: Option<GlueConfigSchemaGeneration>) -> Result<String, CodeGenError> {
        let program = ir
            .into_analyzed_program()
            .ok_or_else(|| CodeGenError::InternalError("Glue IR does not contain an analyzed program".to_string()))?;
        let go_config = config.as_ref().and_then(|c| c.go.clone()).unwrap_or_default();
        let ctx = CodeGenContext::new(program.ast_root.clone(), program.symbols, source, config.as_ref());
        let mut generator = GoGenerator::new(ctx, go_config);
        generator.generate()
    }
}

struct GoGenerator<'a> {
    ctx: CodeGenContext<'a>,
    config: GlueConfigSchemaGenerationGo,
    output: String,
    imports: std::collections::BTreeSet<&'static str>,
    postludes: Vec<String>,
    anon_namer: AnonymousTypeNamer,
    pending_anon_models: Vec<AnonymousModelDef>,
    tuple_helper_arities: std::collections::BTreeSet<usize>,
}

#[derive(Clone)]
struct AnonymousModelDef {
    name: String,
    model: AnonModel,
    scope: Option<SymId>,
    path: Vec<String>,
}

impl<'a> GoGenerator<'a> {
    fn new(ctx: CodeGenContext<'a>, config: GlueConfigSchemaGenerationGo) -> Self {
        let anon_namer = AnonymousTypeNamer::new(&ctx, Case::Pascal);
        Self {
            ctx,
            config,
            output: String::new(),
            imports: std::collections::BTreeSet::new(),
            postludes: Vec::new(),
            anon_namer,
            pending_anon_models: Vec::new(),
            tuple_helper_arities: std::collections::BTreeSet::new(),
        }
    }

    fn generate(&mut self) -> CodeGenResult<String> {
        let package_name = self.config.package_name.clone().unwrap_or_else(|| "glue".to_string());

        for (const_def, scope) in self.ctx.scoped_consts() {
            let code = self.emit_const(&const_def, scope)?;
            self.output.push_str(&code);
        }

        for model in self.ctx.top_level_models().collect::<Vec<_>>() {
            let code = self.emit_model(&model, None)?;
            self.output.push_str(&code);
        }

        for enum_ in self.ctx.top_level_enums().collect::<Vec<_>>() {
            let code = self.emit_enum(&enum_, None)?;
            self.output.push_str(&code);
        }

        for postlude in &self.postludes {
            self.output.push_str(postlude);
        }
        self.emit_pending_anon_models()?;

        let body = std::mem::take(&mut self.output);
        let mut output = format!("package {}\n\n", package_name);
        output.push_str(&self.emit_imports());
        output.push_str(&self.emit_tuple_helpers());
        output.push_str(&body);

        Ok(output)
    }

    fn emit_const(&self, const_def: &ConstDef, scope: Option<SymId>) -> CodeGenResult<String> {
        let name = self.ctx.const_name(const_def, scope, Case::UpperSnake)?;
        let value = self.ctx.eval_const_def_in_scope(const_def, scope)?;
        let (ty, literal) = match value {
            ConstValue::String(value) => ("string", serde_json::to_string(&value).map_err(|e| CodeGenContext::internal_error(e.to_string()))?),
            ConstValue::Int(value) => ("int", value.to_string()),
            ConstValue::Bool(value) => ("bool", value.to_string()),
            ConstValue::List(_) => return Err(self.ctx.error(const_def.syntax(), "Constants can only be int, string, or bool")),
        };
        let docs = const_def.docs().map(|docs| Self::emit_go_docs(&docs, &name)).unwrap_or_default();
        Ok(format!("{}const {} {} = {}\n\n", docs, name, ty, literal))
    }

    fn emit_model(&mut self, model: &Model, parent_scope: Option<SymId>) -> CodeGenResult<String> {
        let mut output = String::new();

        let scope_id = model.scope_id(&self.ctx, parent_scope)?;
        let qualified_name = model.qualified_name(&self.ctx, parent_scope, Case::Pascal)?;
        let model_path = self.ctx.symbol_path(scope_id);

        if let Some(docs) = model.docs() {
            output.push_str(&Self::emit_go_docs(&docs, &qualified_name));
        }

        output.push_str(&format!("type {} struct {{\n", qualified_name));

        let fields = model.fields();
        let mut emitted_fields = Vec::with_capacity(fields.len());
        let mut max_field_name_len = 0usize;
        let mut max_type_len = 0usize;
        let mut max_tag_len = 0usize;

        for field in fields {
            let field_name = field.name()?;
            let field_type = field.field_type()?;
            let go_field_name = convert_user_identifier_case(&field_name, Case::Pascal, self.ctx.preserve_generated_identifiers());
            let mut field_path = model_path.clone();
            field_path.push(field_name.clone());

            let mut type_code = self.emit_type(&field_type, Some(scope_id), &field_path)?;

            if field.is_optional() {
                type_code = format!("*{}", type_code);
            }

            let alias = self.ctx.field_alias(&field, Some(scope_id))?;
            let json_name = alias.unwrap_or_else(|| field_name.clone());
            let mut json_tag = json_name.clone();
            if field.is_optional() {
                json_tag.push_str(",omitempty");
            }
            let tag = format!("`json:\"{}\"`", json_tag);
            let docs = field.docs().map(|d| d.join(" ").trim().to_string());

            max_field_name_len = max_field_name_len.max(go_field_name.len());
            max_type_len = max_type_len.max(type_code.len());
            max_tag_len = max_tag_len.max(tag.len());

            emitted_fields.push((go_field_name, type_code, tag, docs));
        }

        for (go_field_name, type_code, tag, docs) in emitted_fields {
            if let Some(doc_text) = docs {
                output.push_str(&format!(
                    "\t{:<name_width$} {:<type_width$} {:<tag_width$} // {}\n",
                    go_field_name,
                    type_code,
                    tag,
                    doc_text,
                    name_width = max_field_name_len,
                    type_width = max_type_len,
                    tag_width = max_tag_len
                ));
            } else {
                output.push_str(&format!(
                    "\t{:<name_width$} {:<type_width$} {}\n",
                    go_field_name,
                    type_code,
                    tag,
                    name_width = max_field_name_len,
                    type_width = max_type_len
                ));
            }
        }

        output.push_str("}\n\n");

        for nested_model in model.nested_models() {
            let nested_code = self.emit_model(&nested_model, Some(scope_id))?;
            self.postludes.push(nested_code);
        }

        for nested_enum in model.nested_enums() {
            let nested_code = self.emit_enum(&nested_enum, Some(scope_id))?;
            self.postludes.push(nested_code);
        }

        Ok(output)
    }

    fn emit_pending_anon_models(&mut self) -> CodeGenResult<()> {
        let mut index = 0;
        while index < self.pending_anon_models.len() {
            let def = self.pending_anon_models[index].clone();
            let code = self.emit_anon_model(&def)?;
            self.output.push_str(&code);
            index += 1;
        }
        Ok(())
    }

    fn emit_anon_model(&mut self, def: &AnonymousModelDef) -> CodeGenResult<String> {
        let mut output = String::new();
        output.push_str(&format!("type {} struct {{\n", def.name));

        let fields = def.model.fields();
        let mut emitted_fields = Vec::with_capacity(fields.len());
        let mut max_field_name_len = 0usize;
        let mut max_type_len = 0usize;
        let mut max_tag_len = 0usize;

        for field in fields {
            let field_name = field.name()?;
            let field_type = field.field_type()?;
            let go_field_name = convert_user_identifier_case(&field_name, Case::Pascal, self.ctx.preserve_generated_identifiers());
            let mut field_path = def.path.clone();
            field_path.push(field_name.clone());

            let mut type_code = self.emit_type(&field_type, def.scope, &field_path)?;

            if field.is_optional() {
                type_code = format!("*{}", type_code);
            }

            let alias = self.ctx.field_alias(&field, def.scope)?;
            let json_name = alias.unwrap_or_else(|| field_name.clone());
            let mut json_tag = json_name.clone();
            if field.is_optional() {
                json_tag.push_str(",omitempty");
            }
            let tag = format!("`json:\"{}\"`", json_tag);
            let docs = field.docs().map(|d| d.join(" ").trim().to_string());

            max_field_name_len = max_field_name_len.max(go_field_name.len());
            max_type_len = max_type_len.max(type_code.len());
            max_tag_len = max_tag_len.max(tag.len());

            emitted_fields.push((go_field_name, type_code, tag, docs));
        }

        for (go_field_name, type_code, tag, docs) in emitted_fields {
            if let Some(doc_text) = docs {
                output.push_str(&format!(
                    "\t{:<name_width$} {:<type_width$} {:<tag_width$} // {}\n",
                    go_field_name,
                    type_code,
                    tag,
                    doc_text,
                    name_width = max_field_name_len,
                    type_width = max_type_len,
                    tag_width = max_tag_len
                ));
            } else {
                output.push_str(&format!(
                    "\t{:<name_width$} {:<type_width$} {}\n",
                    go_field_name,
                    type_code,
                    tag,
                    name_width = max_field_name_len,
                    type_width = max_type_len
                ));
            }
        }

        output.push_str("}\n\n");
        Ok(output)
    }

    fn emit_enum(&mut self, enum_: &Enum, parent_scope: Option<SymId>) -> CodeGenResult<String> {
        let mut output = String::new();

        let qualified_name = enum_.qualified_name(&self.ctx, parent_scope, Case::Pascal)?;

        if let Some(docs) = enum_.docs() {
            output.push_str(&Self::emit_go_docs(&docs, &qualified_name));
        }

        output.push_str(&format!("type {} string\n\n", qualified_name));

        output.push_str("const (\n");

        let variants = enum_.variants();
        let mut variant_rows = Vec::with_capacity(variants.len());
        let mut max_variant_name_len = 0usize;

        for variant in variants {
            let variant_value = variant.value().ok_or_else(|| CodeGenContext::internal_error("Enum variant missing value"))?;
            let variant_name = format!("{}{}", qualified_name, convert_generated_identifier_case(&variant_value, Case::Pascal));
            max_variant_name_len = max_variant_name_len.max(variant_name.len());
            variant_rows.push((variant, variant_name, variant_value));
        }

        for (variant, variant_name, variant_value) in variant_rows {
            if let Some(docs) = variant.docs() {
                for line in docs {
                    output.push_str(&format!("\t// {}\n", line.trim()));
                }
            }

            output.push_str(&format!(
                "\t{:<name_width$} {} = \"{}\"\n",
                variant_name,
                qualified_name,
                variant_value,
                name_width = max_variant_name_len
            ));
        }

        output.push_str(")\n\n");

        Ok(output)
    }

    fn emit_type_atom(&mut self, atom: &TypeAtom, parent_scope: Option<SymId>, path: &[String]) -> CodeGenResult<String> {
        let is_array = atom.is_array();
        let base_type = self.emit_base_type(atom, parent_scope, path)?;
        if is_array { Ok(format!("[]{}", base_type)) } else { Ok(base_type) }
    }

    fn emit_type(&mut self, ty: &Type, parent_scope: Option<SymId>, path: &[String]) -> CodeGenResult<String> {
        let atoms = ty.type_atoms();
        if atoms.len() > 1 {
            return Ok("interface{}".to_string());
        }
        atoms.first().map(|atom| self.emit_type_atom(atom, parent_scope, path)).unwrap_or_else(|| Ok("interface{}".to_string()))
    }

    fn emit_base_type(&mut self, atom: &TypeAtom, parent_scope: Option<SymId>, path: &[String]) -> CodeGenResult<String> {
        if let Some(primitive) = atom.as_primitive_type() {
            return Ok(TypeMapper::to_go(primitive).to_string());
        }

        if let Some(record_type) = atom.as_record_type() {
            let src_type = record_type.src_type_node().ok_or_else(|| CodeGenContext::internal_error("Record missing source type"))?;
            let dest_type = record_type.dest_type_node().ok_or_else(|| CodeGenContext::internal_error("Record missing destination type"))?;

            let src_atoms = Type::cast(src_type).map(|t: Type| t.type_atoms()).unwrap_or_default();
            let dest_atoms = Type::cast(dest_type).map(|t: Type| t.type_atoms()).unwrap_or_default();

            let mut key_path = path.to_vec();
            key_path.push("Key".to_string());
            let mut value_path = path.to_vec();
            value_path.push("Value".to_string());

            let src_str = src_atoms
                .first()
                .map(|a| self.emit_type_atom(a, parent_scope, &key_path))
                .transpose()?
                .unwrap_or_else(|| "string".to_string());
            let dest_str = dest_atoms
                .first()
                .map(|a| self.emit_type_atom(a, parent_scope, &value_path))
                .transpose()?
                .unwrap_or_else(|| "interface{}".to_string());

            return Ok(format!("map[{}]{}", src_str, dest_str));
        }

        if let Some(tuple_type) = atom.as_tuple_type() {
            let item_types = tuple_type.item_types();
            let mut item_codes = Vec::with_capacity(item_types.len());
            for (index, item_type) in item_types.iter().enumerate() {
                let mut item_path = path.to_vec();
                item_path.push(format!("Item{}", index));
                item_codes.push(self.emit_type(item_type, parent_scope, &item_path)?);
            }
            let arity = item_codes.len();
            if (2..=4).contains(&arity) {
                self.imports.insert("encoding/json");
                self.imports.insert("fmt");
                self.tuple_helper_arities.insert(arity);
                return Ok(format!("Tuple{}[{}]", arity, item_codes.join(", ")));
            }
            return Ok(format!("[{}]interface{{}}", arity));
        }

        if let Some(ref_token) = atom.as_ref_token() {
            let ref_name = ref_token.text().trim();
            if let Some(alias_type) = self.ctx.resolve_type_alias(parent_scope, ref_name)? {
                let alias_atoms = alias_type.type_atoms();
                if alias_atoms.len() > 1 {
                    return Ok("interface{}".to_string());
                }
                if let Some(alias_atom) = alias_atoms.first() {
                    return self.emit_type_atom(alias_atom, parent_scope, path);
                }
                return Err(CodeGenContext::internal_error(format!("Type alias '{}' has no type atoms", ref_name)));
            }

            let resolved = self
                .ctx
                .qualified_name(parent_scope, ref_name, Case::Pascal)
                .ok_or_else(|| CodeGenContext::internal_error(format!("Unresolved type: {}", ref_name)))?;
            return Ok(resolved);
        }

        if let Some(anon_model) = atom.anon_model() {
            let name = self.anon_namer.allocate(&self.ctx, path, Case::Pascal);
            self.pending_anon_models.push(AnonymousModelDef {
                name: name.clone(),
                model: anon_model,
                scope: parent_scope,
                path: path.to_vec(),
            });
            return Ok(name);
        }

        Err(CodeGenContext::internal_error("Unknown type atom"))
    }

    fn emit_imports(&self) -> String {
        if self.imports.is_empty() {
            return String::new();
        }

        let mut output = String::from("import (\n");
        for import in &self.imports {
            output.push_str(&format!("\t\"{}\"\n", import));
        }
        output.push_str(")\n\n");
        output
    }

    fn emit_tuple_helpers(&self) -> String {
        let mut output = String::new();
        for arity in &self.tuple_helper_arities {
            output.push_str(&Self::emit_tuple_helper(*arity));
        }
        output
    }

    fn emit_tuple_helper(arity: usize) -> String {
        let params = (0..arity).map(|index| format!("T{}", index)).collect::<Vec<_>>();
        let param_decl = format!("{} any", params.join(", "));
        let type_args = params.join(", ");

        let mut output = format!("type Tuple{}[{}] struct {{\n", arity, param_decl);
        for param in &params {
            output.push_str(&format!("\tV{} {}\n", &param[1..], param));
        }
        output.push_str("}\n\n");

        output.push_str(&format!("func (t Tuple{}[{}]) MarshalJSON() ([]byte, error) {{\n", arity, type_args));
        output.push_str("\treturn json.Marshal([]interface{}{");
        output.push_str(&(0..arity).map(|index| format!("t.V{}", index)).collect::<Vec<_>>().join(", "));
        output.push_str("})\n");
        output.push_str("}\n\n");

        output.push_str(&format!("func (t *Tuple{}[{}]) UnmarshalJSON(data []byte) error {{\n", arity, type_args));
        output.push_str("\tvar values []json.RawMessage\n");
        output.push_str("\tif err := json.Unmarshal(data, &values); err != nil {\n");
        output.push_str("\t\treturn err\n");
        output.push_str("\t}\n");
        output.push_str(&format!("\tif len(values) != {} {{\n", arity));
        output.push_str(&format!("\t\treturn fmt.Errorf(\"expected tuple of length {}, got %d\", len(values))\n", arity));
        output.push_str("\t}\n");
        for index in 0..arity {
            output.push_str(&format!("\tif err := json.Unmarshal(values[{}], &t.V{}); err != nil {{\n", index, index));
            output.push_str("\t\treturn err\n");
            output.push_str("\t}\n");
        }
        output.push_str("\treturn nil\n");
        output.push_str("}\n\n");

        output
    }

    fn emit_go_docs(docs: &[String], name: &str) -> String {
        let mut output = String::new();
        for (i, line) in docs.iter().enumerate() {
            if i == 0 {
                output.push_str(&format!("// {} {}\n", name, line.trim()));
            } else {
                output.push_str(&format!("// {}\n", line.trim()));
            }
        }
        output
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use config::GlueConfigSchemaGeneration;
    use indoc::indoc;
    use insta::assert_snapshot;

    use crate::test_utils::{gen_test, gen_test_with_config};

    fn gen_go(src: &str) -> String {
        gen_test(&CodeGenGo, src)
    }

    fn gen_go_with_config(src: &str, config: GlueConfigSchemaGeneration) -> String {
        gen_test_with_config(&CodeGenGo, src, Some(config))
    }

    #[test]
    fn test_simple_model() {
        let src = indoc! {r#"
            /// A simple user model
            model User {
                /// User's unique identifier
                id: string
                /// User's display name
                name: string
                /// User's age
                age: int
            }
        "#};
        assert_snapshot!(gen_go(src));
    }

    #[test]
    fn test_constants_emit() {
        let src = indoc! {r#"
            /// Alias used for user IDs.
            const USER_ALIAS = "user_" + "id"
            const DEFAULT_LIMIT = 100 * 2
            const _PRIVATE_FLAG = true

            model Request {
                @field(alias=USER_ALIAS)
                user_id: string
            }
        "#};

        let output = gen_go(src);
        assert!(output.contains("// USER_ALIAS Alias used for user IDs.\nconst USER_ALIAS"), "Expected constant docs:\n{}", output);
        assert!(output.contains("const USER_ALIAS string = \"user_id\""), "Expected folded string constant:\n{}", output);
        assert!(output.contains("const DEFAULT_LIMIT int = 200"), "Expected folded int constant:\n{}", output);
        assert!(output.contains("const _PRIVATE_FLAG bool = true"), "Expected private constant:\n{}", output);
        assert!(output.contains("UserId string `json:\"user_id\"`"), "Expected folded alias:\n{}", output);
    }

    #[test]
    fn test_model_scoped_constants_emit_and_aliases_fold() {
        let src = indoc! {r#"
            model Aliases {
                const SUFFIX = "_alias"
                const USER_ID_ALIAS = "user_id" + SUFFIX
                const _PRIVATE_FLAG = true
            }

            model User {
                const SUFFIX = "_user"

                @field(alias=Aliases.USER_ID_ALIAS)
                user_id: string
            }
        "#};

        let output = gen_go(src);
        assert!(output.contains("const ALIASES_SUFFIX string = \"_alias\""), "Expected model-prefixed string constant:\n{}", output);
        assert!(
            output.contains("const ALIASES_USER_ID_ALIAS string = \"user_id_alias\""),
            "Expected declaration-scope folded constant:\n{}",
            output
        );
        assert!(output.contains("const _ALIASES_PRIVATE_FLAG bool = true"), "Expected private model-prefixed constant:\n{}", output);
        assert!(output.contains("const USER_SUFFIX string = \"_user\""), "Expected second model-prefixed constant:\n{}", output);
        assert!(output.contains("UserId string `json:\"user_id_alias\"`"), "Expected folded qualified alias:\n{}", output);
    }

    #[test]
    fn test_model_with_optional_fields() {
        let src = indoc! {r#"
            model Config {
                /// Required field
                name: string
                /// Optional description
                description?: string
                /// Optional count
                count?: int
            }
        "#};
        assert_snapshot!(gen_go(src));
    }

    #[test]
    fn test_enum() {
        let src = indoc! {r#"
            /// User status enum
            enum Status: "active" | "inactive" | "pending"
        "#};
        assert_snapshot!(gen_go(src));
    }

    #[test]
    fn test_model_with_enum_field() {
        let src = indoc! {r#"
            model User {
                name: string
                status: Status
            }

            enum Status: "active" | "inactive"
        "#};
        assert_snapshot!(gen_go(src));
    }

    #[test]
    fn test_nested_model() {
        let src = indoc! {r#"
            model Parent {
                name: string
                child: Child

                model Child {
                    value: int
                }
            }
        "#};
        assert_snapshot!(gen_go(src));
    }

    #[test]
    fn test_field_alias() {
        let src = indoc! {r#"
            model Item {
                @field("item_id")
                id: string
                @field("display_name")
                name: string
            }
        "#};
        assert_snapshot!(gen_go(src));
    }

    #[test]
    fn test_record_type() {
        let src = indoc! {r#"
            model Data {
                /// A map of string to any
                metadata: Record<string, any>
                /// A map of string to int
                counts: Record<string, int>
            }
        "#};
        assert_snapshot!(gen_go(src));
    }

    #[test]
    fn test_tuple_types() {
        let src = indoc! {r#"
            model Event {
                pair: (string, int)
                quad: (string, int, bool, string)
                large: (string, int, bool, string, int)
            }
        "#};

        let output = gen_go(src);
        assert!(output.contains("\"encoding/json\""), "Expected tuple JSON helper import:\n{}", output);
        assert!(output.contains("\"fmt\""), "Expected tuple length error import:\n{}", output);
        assert!(output.contains("type Tuple2[T0, T1 any] struct"), "Expected Tuple2 helper:\n{}", output);
        assert!(output.contains("type Tuple4[T0, T1, T2, T3 any] struct"), "Expected Tuple4 helper:\n{}", output);
        assert!(output.contains("Pair  Tuple2[string, int64]"), "Expected typed Tuple2 field:\n{}", output);
        assert!(output.contains("Quad  Tuple4[string, int64, bool, string]"), "Expected typed Tuple4 field:\n{}", output);
        assert!(output.contains("Large [5]interface{}"), "Expected large tuple fallback:\n{}", output);
    }

    #[test]
    fn test_anonymous_struct() {
        let src = indoc! {r#"
            model User {
                profile: {
                    bio: string
                    age?: int
                    settings: Record<string, {
                        enabled: bool
                    }>
                }
            }
        "#};
        assert_snapshot!(gen_go(src));
    }

    #[test]
    fn test_model_reference() {
        let src = indoc! {r#"
            model Order {
                id: string
                user: User
                items: Item
            }

            model User {
                name: string
            }

            model Item {
                sku: string
                quantity: int
            }
        "#};
        assert_snapshot!(gen_go(src));
    }

    #[test]
    fn test_array_types() {
        let src = indoc! {r#"
            model User {
                /// List of tags
                tags: string[]
                /// List of scores
                scores: int[]
                /// List of addresses
                addresses: Address[]
                /// Optional list of nicknames
                nicknames?: string[]
            }

            model Address {
                street: string
                city: string
            }
        "#};
        assert_snapshot!(gen_go(src));
    }

    #[test]
    fn test_pascal_type_identifiers_preserve_uppercase_and_generated_identifiers_default_to_pascal() {
        let src = indoc! {r#"
            model XMLDocument {
                XML_version: string
            }

            model XMLParser {
                document: XMLDocument
            }

            enum XMLParseMode: "STRICT_MODE" | "HTML-mode"
        "#};

        let output = gen_go(src);
        assert!(output.contains("type XMLDocument struct {"), "Expected model acronym to be preserved:\n{}", output);
        assert!(output.contains("type XMLParser struct {"), "Expected model acronym to be preserved:\n{}", output);
        assert!(
            output.contains("XMLVersion"),
            "Expected user-provided field acronym to be preserved while exporting the Go field:\n{}",
            output
        );
        assert!(output.contains("Document XMLDocument"), "Expected reference acronym to be preserved:\n{}", output);
        assert!(
            output.contains("XMLParseModeStrictMode"),
            "Expected generated enum variant from uppercase value to default to PascalCase:\n{}",
            output
        );
        assert!(output.contains("XMLParseModeHtmlMode"), "Expected generated enum variant to default to PascalCase:\n{}", output);
    }

    #[test]
    fn test_preserve_generated_identifiers_config() {
        let src = indoc! {r#"
            model xml_document {
                XML_version: string
            }

            enum xml_parse_mode: "STRICT_MODE" | "HTML-mode"
        "#};

        let output = gen_go_with_config(
            src,
            GlueConfigSchemaGeneration {
                preserve_generated_identifiers: Some(true),
                ..Default::default()
            },
        );
        assert!(
            output.contains("type xml_document struct {"),
            "Expected configured model identifier to be preserved exactly:\n{}",
            output
        );
        assert!(output.contains("XML_version string"), "Expected configured field identifier to be preserved exactly:\n{}", output);
        assert!(
            output.contains("type xml_parse_mode string"),
            "Expected configured enum identifier to be preserved exactly:\n{}",
            output
        );
        assert!(
            output.contains("xml_parse_modeStrictMode"),
            "Expected generated enum variant suffix to keep normal PascalCase:\n{}",
            output
        );
        assert!(
            output.contains("xml_parse_modeHtmlMode"),
            "Expected generated enum variant suffix to keep normal PascalCase:\n{}",
            output
        );
    }
}
