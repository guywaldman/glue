use config::{GlueConfigSchemaGeneration, GlueConfigSchemaGenerationGo};
use convert_case::{Case, Casing};
use lang::{AstNode, Enum, Field, GlueIr, Model, SourceCodeMetadata, SymId, Type, TypeAtom};

use crate::{
    CodeGenError, CodeGenerator,
    codegen::CodeGenResult,
    context::{CodeGenContext, FieldExt, NamedExt, TypeMapper},
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
    postludes: Vec<String>,
}

impl<'a> GoGenerator<'a> {
    fn new(ctx: CodeGenContext<'a>, config: GlueConfigSchemaGenerationGo) -> Self {
        Self {
            ctx,
            config,
            output: String::new(),
            postludes: Vec::new(),
        }
    }

    fn generate(&mut self) -> CodeGenResult<String> {
        let package_name = self.config.package_name.as_deref().unwrap_or("glue");
        self.output.push_str(&format!("package {}\n\n", package_name));

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

        Ok(self.output.clone())
    }

    fn emit_model(&mut self, model: &Model, parent_scope: Option<SymId>) -> CodeGenResult<String> {
        let mut output = String::new();

        let scope_id = model.scope_id(&self.ctx, parent_scope)?;
        let qualified_name = model.qualified_name(&self.ctx, parent_scope, Case::Pascal)?;

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
            let go_field_name = field_name.to_case(Case::Pascal);

            let type_atoms = field_type.type_atoms();
            let type_strs: Vec<String> = type_atoms.iter().map(|atom| self.emit_type_atom(atom, Some(scope_id))).collect::<Result<Vec<_>, _>>()?;

            let mut type_code = if type_strs.len() > 1 {
                "interface{}".to_string()
            } else {
                type_strs.first().cloned().unwrap_or_else(|| "interface{}".to_string())
            };

            if field.is_optional() {
                type_code = format!("*{}", type_code);
            }

            let alias = field.alias()?;
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
            let variant_name = format!("{}{}", qualified_name, variant_value.to_case(Case::Pascal));
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

    fn emit_type_atom(&self, atom: &TypeAtom, parent_scope: Option<SymId>) -> CodeGenResult<String> {
        let is_array = atom.is_array();
        let base_type = self.emit_base_type(atom, parent_scope)?;
        if is_array { Ok(format!("[]{}", base_type)) } else { Ok(base_type) }
    }

    fn emit_base_type(&self, atom: &TypeAtom, parent_scope: Option<SymId>) -> CodeGenResult<String> {
        if let Some(primitive) = atom.as_primitive_type() {
            return Ok(TypeMapper::to_go(primitive).to_string());
        }

        if let Some(record_type) = atom.as_record_type() {
            let src_type = record_type.src_type_node().ok_or_else(|| CodeGenContext::internal_error("Record missing source type"))?;
            let dest_type = record_type.dest_type_node().ok_or_else(|| CodeGenContext::internal_error("Record missing destination type"))?;

            let src_atoms = Type::cast(src_type).map(|t: Type| t.type_atoms()).unwrap_or_default();
            let dest_atoms = Type::cast(dest_type).map(|t: Type| t.type_atoms()).unwrap_or_default();

            let src_str = src_atoms.first().map(|a| self.emit_type_atom(a, parent_scope)).transpose()?.unwrap_or_else(|| "string".to_string());
            let dest_str = dest_atoms
                .first()
                .map(|a| self.emit_type_atom(a, parent_scope))
                .transpose()?
                .unwrap_or_else(|| "interface{}".to_string());

            return Ok(format!("map[{}]{}", src_str, dest_str));
        }

        if let Some(ref_token) = atom.as_ref_token() {
            let ref_name = ref_token.text().trim();
            if let Some(alias_type) = self.ctx.resolve_type_alias(parent_scope, ref_name)? {
                let alias_atoms = alias_type.type_atoms();
                if alias_atoms.len() > 1 {
                    return Ok("interface{}".to_string());
                }
                if let Some(alias_atom) = alias_atoms.first() {
                    return self.emit_type_atom(alias_atom, parent_scope);
                }
                return Err(CodeGenContext::internal_error(format!("Type alias '{}' has no type atoms", ref_name)));
            }

            let resolved = self
                .ctx
                .qualified_name(parent_scope, ref_name, Case::Pascal)
                .ok_or_else(|| CodeGenContext::internal_error(format!("Unresolved type: {}", ref_name)))?;
            return Ok(resolved);
        }

        if atom.as_anon_model().is_some() {
            return Err(CodeGenContext::internal_error("Anonymous models not yet supported in Go codegen"));
        }

        Err(CodeGenContext::internal_error("Unknown type atom"))
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
    use indoc::indoc;
    use insta::assert_snapshot;

    use crate::test_utils::gen_test;

    fn gen_go(src: &str) -> String {
        gen_test(&CodeGenGo, src)
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
}
