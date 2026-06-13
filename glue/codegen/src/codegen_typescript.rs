use config::GlueConfigSchemaGeneration;
use convert_case::Case;
use lang::{AnonModel, AstNode, ConstDef, ConstValue, Enum, Field, GlueIr, Model, PrimitiveType, SourceCodeMetadata, SymId, Type, TypeAlias, TypeAtom};

use crate::{
    CodeGenError, CodeGenerator,
    codegen::CodeGenResult,
    context::{CodeGenContext, DocEmitter, EnumVariantExt, FieldExt, NamedExt},
};

#[derive(Default)]
pub struct CodeGenTypeScript;

impl CodeGenerator for CodeGenTypeScript {
    fn generate(&self, ir: GlueIr, source: &SourceCodeMetadata, config: Option<GlueConfigSchemaGeneration>) -> Result<String, CodeGenError> {
        let program = ir
            .into_analyzed_program()
            .ok_or_else(|| CodeGenError::InternalError("Glue IR does not contain an analyzed program".to_string()))?;
        let ts_config = config.as_ref().and_then(|g| g.typescript.clone()).unwrap_or_default();
        let zod_enabled = ts_config.zod.unwrap_or(false);
        let ctx = CodeGenContext::new(program.ast_root.clone(), program.symbols, source, config.as_ref());
        let mut generator = TypeScriptGenerator::new(ctx, zod_enabled);
        generator.generate()
    }
}

struct TypeScriptGenerator<'a> {
    ctx: CodeGenContext<'a>,
    zod_enabled: bool,
    output: String,
}

impl<'a> TypeScriptGenerator<'a> {
    fn new(ctx: CodeGenContext<'a>, zod_enabled: bool) -> Self {
        Self {
            ctx,
            zod_enabled,
            output: String::new(),
        }
    }

    fn generate(&mut self) -> CodeGenResult<String> {
        if self.zod_enabled {
            self.output.push_str("import { z } from \"zod\";\n\n");
        }

        for (const_def, scope) in self.ctx.scoped_consts() {
            self.emit_const(&const_def, scope)?;
        }
        for (type_alias, scope) in self.ctx.scoped_type_aliases() {
            if !type_alias.is_private() {
                self.emit_type_alias(&type_alias, scope)?;
            }
        }
        for model in self.ctx.top_level_models().collect::<Vec<_>>() {
            self.emit_model(&model, None)?;
        }
        for enum_ in self.ctx.top_level_enums().collect::<Vec<_>>() {
            self.emit_enum(&enum_, None)?;
        }

        Ok(self.output.clone())
    }

    fn emit_const(&mut self, const_def: &ConstDef, scope: Option<SymId>) -> CodeGenResult<()> {
        let name = self.ctx.const_name(const_def, scope, Case::UpperSnake)?;
        let export = if const_def.is_private() { "" } else { "export " };
        let value = self.ctx.eval_const_def_in_scope(const_def, scope)?;
        let (ty, literal) = match value {
            ConstValue::String(value) => ("string", serde_json::to_string(&value).map_err(|e| CodeGenContext::internal_error(e.to_string()))?),
            ConstValue::Int(value) => ("number", value.to_string()),
            ConstValue::Bool(value) => ("boolean", value.to_string()),
            ConstValue::List(_) => return Err(self.ctx.error(const_def.syntax(), "Constants can only be int, string, or bool")),
        };
        if let Some(docs) = const_def.docs() {
            self.output.push_str(&DocEmitter::ts_docstring(&docs));
        }
        self.output.push_str(&format!("{}const {}: {} = {};\n\n", export, name, ty, literal));
        Ok(())
    }

    fn emit_type_alias(&mut self, type_alias: &TypeAlias, scope: Option<SymId>) -> CodeGenResult<()> {
        let name = type_alias.qualified_name(&self.ctx, scope, Case::Pascal)?;
        let type_node = type_alias
            .type_node()
            .ok_or_else(|| CodeGenContext::internal_error(format!("Type alias '{}' missing type expression", name)))?;
        let alias_type = Type::cast(type_node).ok_or_else(|| CodeGenContext::internal_error("Expected Type node in type alias"))?;

        if let Some(docs) = type_alias.docs() {
            self.output.push_str(&DocEmitter::ts_docstring(&docs));
        }
        self.output.push_str(&format!("export type {} = {};\n\n", name, self.emit_type(&alias_type, scope)?));
        Ok(())
    }

    fn emit_model(&mut self, model: &Model, parent_scope: Option<SymId>) -> CodeGenResult<()> {
        let scope_id = model.scope_id(&self.ctx, parent_scope)?;
        let name = model.qualified_name(&self.ctx, parent_scope, Case::Pascal)?;

        if let Some(docs) = model.docs() {
            self.output.push_str(&DocEmitter::ts_docstring(&docs));
        }

        if self.zod_enabled {
            self.output.push_str(&format!("export const {}Schema = z.object({{\n", name));
            for field in model.fields() {
                let field_line = self.emit_zod_field(&field, Some(scope_id))?;
                self.output.push_str(&field_line);
            }
            self.output.push_str("});\n");
            self.output.push_str(&format!("export type {} = z.infer<typeof {}Schema>;\n\n", name, name));
        } else {
            self.output.push_str(&format!("export type {} = {{\n", name));
            for field in model.fields() {
                let field_line = self.emit_type_field(&field, Some(scope_id))?;
                self.output.push_str(&field_line);
            }
            self.output.push_str("};\n\n");
        }

        for nested in model.nested_models() {
            self.emit_model(&nested, Some(scope_id))?;
        }
        for nested in model.nested_enums() {
            self.emit_enum(&nested, Some(scope_id))?;
        }

        Ok(())
    }

    fn emit_enum(&mut self, enum_: &Enum, parent_scope: Option<SymId>) -> CodeGenResult<()> {
        let name = enum_.qualified_name(&self.ctx, parent_scope, Case::Pascal)?;
        let values: Vec<String> = enum_.variants().iter().map(|v| v.variant_value()).collect::<Result<Vec<_>, _>>()?;
        let literals = values.iter().map(|v| format!("\"{}\"", v)).collect::<Vec<_>>().join(" | ");

        if let Some(docs) = enum_.docs() {
            self.output.push_str(&DocEmitter::ts_docstring(&docs));
        }

        if self.zod_enabled {
            let zod_values = values.iter().map(|v| format!("\"{}\"", v)).collect::<Vec<_>>().join(", ");
            self.output.push_str(&format!("export const {}Schema = z.enum([{}]);\n", name, zod_values));
            self.output.push_str(&format!("export type {} = z.infer<typeof {}Schema>;\n\n", name, name));
        } else {
            self.output.push_str(&format!("export type {} = {};\n\n", name, literals));
        }

        Ok(())
    }

    fn emit_type_field(&self, field: &Field, scope: Option<SymId>) -> CodeGenResult<String> {
        let field_name = field.name()?;
        let ts_name = field_name;
        let type_code = self.emit_type(&field.field_type()?, scope)?;
        let optional = if field.is_optional() { "?" } else { "" };
        let mut output = String::new();
        if let Some(docs) = field.docs() {
            output.push_str(&self.indent_docstring(&DocEmitter::ts_docstring(&docs), 2));
        }
        output.push_str(&format!("  {}{}: {};\n", ts_name, optional, type_code));
        Ok(output)
    }

    fn emit_zod_field(&self, field: &Field, scope: Option<SymId>) -> CodeGenResult<String> {
        let field_name = field.name()?;
        let ts_name = field_name;
        let mut schema = self.emit_zod_type(&field.field_type()?, scope)?;
        if field.is_optional() {
            schema.push_str(".optional()");
        }
        let mut output = String::new();
        if let Some(docs) = field.docs() {
            output.push_str(&self.indent_docstring(&DocEmitter::ts_docstring(&docs), 2));
        }
        output.push_str(&format!("  {}: {},\n", ts_name, schema));
        Ok(output)
    }

    fn indent_docstring(&self, docs: &str, spaces: usize) -> String {
        let indent = " ".repeat(spaces);
        docs.lines().map(|line| format!("{}{}\n", indent, line)).collect()
    }

    fn emit_type(&self, ty: &Type, scope: Option<SymId>) -> CodeGenResult<String> {
        let atoms = ty.type_atoms();
        if atoms.len() == 1 {
            self.emit_type_atom(&atoms[0], scope)
        } else {
            let codes: Vec<_> = atoms.iter().map(|a| self.emit_type_atom(a, scope)).collect::<Result<Vec<_>, _>>()?;
            Ok(codes.join(" | "))
        }
    }

    fn emit_type_atom(&self, atom: &TypeAtom, scope: Option<SymId>) -> CodeGenResult<String> {
        let mut result = if let Some(primitive) = atom.as_primitive_type() {
            typescript_primitive_type(primitive).to_string()
        } else if let Some(record) = atom.as_record_type() {
            let src = record.src_type_node().ok_or_else(|| CodeGenContext::internal_error("Record missing src type"))?;
            let dest = record.dest_type_node().ok_or_else(|| CodeGenContext::internal_error("Record missing dest type"))?;
            let src_type = Type::cast(src).ok_or_else(|| CodeGenContext::internal_error("Expected Type for record src"))?;
            let dest_type = Type::cast(dest).ok_or_else(|| CodeGenContext::internal_error("Expected Type for record dest"))?;
            format!("Record<{}, {}>", self.emit_type(&src_type, scope)?, self.emit_type(&dest_type, scope)?)
        } else if let Some(tuple) = atom.as_tuple_type() {
            let item_codes = tuple.item_types().iter().map(|item| self.emit_type(item, scope)).collect::<CodeGenResult<Vec<_>>>()?;
            format!("[{}]", item_codes.join(", "))
        } else if let Some(ref_token) = atom.as_ref_token() {
            let type_name = ref_token.text().to_string();
            let sym = self
                .ctx
                .resolve(scope, &type_name)
                .ok_or_else(|| CodeGenContext::internal_error(format!("Unresolved type: {}", type_name)))?;
            if sym.data.kind() == lang::LSyntaxKind::TYPE_ALIAS {
                let alias = TypeAlias::cast(sym.data).ok_or_else(|| CodeGenContext::internal_error("Expected type alias node"))?;
                if alias.is_private() {
                    let alias_type = self
                        .ctx
                        .resolve_type_alias(scope, &type_name)?
                        .ok_or_else(|| CodeGenContext::internal_error(format!("Unresolved type alias: {}", type_name)))?;
                    self.emit_type(&alias_type, scope)?
                } else {
                    self.ctx.symbol_name(&sym.name, Case::Pascal)
                }
            } else {
                self.ctx.symbol_name(&sym.name, Case::Pascal)
            }
        } else if let Some(anon_model) = atom.anon_model() {
            self.emit_anon_type(&anon_model, scope)?
        } else {
            return Err(CodeGenContext::internal_error("Unknown type atom kind"));
        };

        if atom.is_optional() {
            result = format!("{} | null", result);
        }
        if atom.is_array() {
            result = format!("{}[]", result);
        }

        Ok(result)
    }

    fn emit_anon_type(&self, anon_model: &AnonModel, scope: Option<SymId>) -> CodeGenResult<String> {
        let fields = anon_model
            .fields()
            .iter()
            .map(|field| {
                let optional = if field.is_optional() { "?" } else { "" };
                Ok(format!("{}{}: {}", field.name()?, optional, self.emit_type(&field.field_type()?, scope)?))
            })
            .collect::<CodeGenResult<Vec<_>>>()?;
        Ok(format!("{{ {} }}", fields.join("; ")))
    }

    fn emit_zod_type(&self, ty: &Type, scope: Option<SymId>) -> CodeGenResult<String> {
        let atoms = ty.type_atoms();
        if atoms.len() == 1 {
            self.emit_zod_type_atom(&atoms[0], scope)
        } else {
            let codes: Vec<_> = atoms.iter().map(|a| self.emit_zod_type_atom(a, scope)).collect::<Result<Vec<_>, _>>()?;
            Ok(format!("z.union([{}])", codes.join(", ")))
        }
    }

    fn emit_zod_type_atom(&self, atom: &TypeAtom, scope: Option<SymId>) -> CodeGenResult<String> {
        let mut result = if let Some(primitive) = atom.as_primitive_type() {
            self.zod_for_primitive(primitive)
        } else if let Some(record) = atom.as_record_type() {
            let src = record.src_type_node().ok_or_else(|| CodeGenContext::internal_error("Record missing src type"))?;
            let dest = record.dest_type_node().ok_or_else(|| CodeGenContext::internal_error("Record missing dest type"))?;
            let src_type = Type::cast(src).ok_or_else(|| CodeGenContext::internal_error("Expected Type for record src"))?;
            let dest_type = Type::cast(dest).ok_or_else(|| CodeGenContext::internal_error("Expected Type for record dest"))?;
            let key_schema = self.emit_zod_type(&src_type, scope)?;
            let value_schema = self.emit_zod_type(&dest_type, scope)?;
            format!("z.record({}, {})", key_schema, value_schema)
        } else if let Some(tuple) = atom.as_tuple_type() {
            let item_schemas = tuple.item_types().iter().map(|item| self.emit_zod_type(item, scope)).collect::<CodeGenResult<Vec<_>>>()?;
            format!("z.tuple([{}])", item_schemas.join(", "))
        } else if let Some(ref_token) = atom.as_ref_token() {
            let type_name = ref_token.text().to_string();
            if let Some(alias_type) = self.ctx.resolve_type_alias(scope, &type_name)? {
                self.emit_zod_type(&alias_type, scope)?
            } else {
                let sym = self
                    .ctx
                    .resolve(scope, &type_name)
                    .ok_or_else(|| CodeGenContext::internal_error(format!("Unresolved type: {}", type_name)))?;
                let qualified = self.ctx.symbol_name(&sym.name, Case::Pascal);
                format!("{}Schema", qualified)
            }
        } else if let Some(anon_model) = atom.anon_model() {
            self.emit_zod_anon_type(&anon_model, scope)?
        } else {
            return Err(CodeGenContext::internal_error("Unknown type atom kind"));
        };

        if atom.is_optional() {
            result = format!("{}.nullable()", result);
        }
        if atom.is_array() {
            result = format!("z.array({})", result);
        }

        Ok(result)
    }

    fn emit_zod_anon_type(&self, anon_model: &AnonModel, scope: Option<SymId>) -> CodeGenResult<String> {
        let fields = anon_model
            .fields()
            .iter()
            .map(|field| {
                let mut schema = self.emit_zod_type(&field.field_type()?, scope)?;
                if field.is_optional() {
                    schema.push_str(".optional()");
                }
                Ok(format!("{}: {}", field.name()?, schema))
            })
            .collect::<CodeGenResult<Vec<_>>>()?;
        Ok(format!("z.object({{ {} }})", fields.join(", ")))
    }

    fn zod_for_primitive(&self, primitive: PrimitiveType) -> String {
        match primitive {
            primitive if primitive.is_integer() => "z.number()".to_string(),
            PrimitiveType::String => "z.string()".to_string(),
            PrimitiveType::Float => "z.number()".to_string(),
            PrimitiveType::Bool => "z.boolean()".to_string(),
            PrimitiveType::Any => "z.any()".to_string(),
            _ => unreachable!("integer primitive handled above"),
        }
    }
}

trait TsDocEmitter {
    fn ts_docstring(docs: &[String]) -> String;
}

impl TsDocEmitter for DocEmitter {
    fn ts_docstring(docs: &[String]) -> String {
        if docs.is_empty() {
            return String::new();
        }
        let mut out = String::from("/**\n");
        for line in docs {
            out.push_str(&format!(" * {}\n", line.trim()));
        }
        out.push_str(" */\n");
        out
    }
}

fn typescript_primitive_type(primitive: PrimitiveType) -> &'static str {
    match primitive {
        primitive if primitive.is_integer() => "number",
        PrimitiveType::String => "string",
        PrimitiveType::Float => "number",
        PrimitiveType::Bool => "boolean",
        PrimitiveType::Any => "any",
        _ => unreachable!("integer primitive handled above"),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use config::{GlueConfigSchemaGeneration, GlueConfigSchemaGenerationTypeScript};
    use indoc::indoc;
    use insta::assert_snapshot;

    use crate::test_utils::{gen_test, gen_test_with_config};

    fn gen_typescript(src: &str) -> String {
        gen_test(&CodeGenTypeScript, src)
    }

    fn gen_typescript_with_zod(src: &str, zod: bool) -> String {
        let config = GlueConfigSchemaGeneration {
            typescript: Some(GlueConfigSchemaGenerationTypeScript { zod: Some(zod) }),
            ..Default::default()
        };
        gen_test_with_config(&CodeGenTypeScript, src, Some(config))
    }

    const SIMPLE_MODEL: &str = indoc! { r#"
        model User {
            /// The user's display name
            name: string
            age: int
            email?: string
            active: bool = true
        }

        enum UserRole: "admin" | "user" | "guest"
    "# };

    #[test]
    fn test_types_only() {
        assert_snapshot!(gen_typescript(SIMPLE_MODEL));
    }

    #[test]
    fn test_zod() {
        assert_snapshot!(gen_typescript_with_zod(SIMPLE_MODEL, true));
    }

    #[test]
    fn test_constants_emit_before_types() {
        let src = indoc! { r#"
            /// Alias used for user IDs.
            const USER_ALIAS = "user_" + "id"
            const MAX_PAGE_SIZE: int = 100
            const _RETRY_MS = (100 + 50) * 2

            model Request {
                user_id: string
            }
        "# };

        let output = gen_typescript(src);
        assert!(output.contains("/**\n * Alias used for user IDs.\n */\nexport const USER_ALIAS"), "Expected constant docs:\n{}", output);
        assert!(output.contains("export const USER_ALIAS: string = \"user_id\";"), "Expected string constant to be folded:\n{}", output);
        assert!(output.contains("export const MAX_PAGE_SIZE: number = 100;"), "Expected int constant:\n{}", output);
        assert!(output.contains("const _RETRY_MS: number = 300;"), "Expected private constant without export:\n{}", output);
        assert!(output.find("USER_ALIAS").unwrap() < output.find("export type Request").unwrap());
    }

    #[test]
    fn test_type_aliases_export_and_references_are_preserved() {
        let src = indoc! { r#"
            /// Stable user identifier.
            type UserId = string
            type UserIds = UserId[]
            type _InternalId = string
            type PublicInternalIds = _InternalId[]
            type Profile = { nickname: string }
            type Value = string | int

            model Account {
                type LocalId = uint
                type _LocalSecret = string

                id: UserId
                related: UserIds
                internal: _InternalId
                public_internal_ids: PublicInternalIds
                profile: Profile
                value: Value
                local: LocalId
                secret: _LocalSecret
            }
        "# };

        let output = gen_typescript(src);
        assert!(
            output.contains("/**\n * Stable user identifier.\n */\nexport type UserId = string;"),
            "Expected documented type alias:\n{}",
            output
        );
        assert!(output.contains("export type UserIds = UserId[];"), "Expected alias RHS to preserve alias refs:\n{}", output);
        assert!(
            !output.contains("export type InternalId") && !output.contains("export type _InternalId"),
            "Expected private alias to be inlined instead of exported:\n{}",
            output
        );
        assert!(
            !output.contains("export type AccountLocalSecret") && !output.contains("export type Account_LocalSecret"),
            "Expected nested private alias to be inlined instead of exported:\n{}",
            output
        );
        assert!(
            output.contains("export type PublicInternalIds = string[];"),
            "Expected public alias referencing private alias to inline RHS:\n{}",
            output
        );
        assert!(output.contains("export type Profile = { nickname: string };"), "Expected anonymous model alias:\n{}", output);
        assert!(output.contains("export type Value = string | number;"), "Expected union type alias:\n{}", output);
        assert!(output.contains("export type AccountLocalId = number;"), "Expected nested alias to be exported:\n{}", output);
        assert!(output.contains("  id: UserId;\n"), "Expected field to use alias:\n{}", output);
        assert!(output.contains("  related: UserIds;\n"), "Expected field to use array alias:\n{}", output);
        assert!(output.contains("  internal: string;\n"), "Expected private alias field to inline:\n{}", output);
        assert!(output.contains("  public_internal_ids: PublicInternalIds;\n"), "Expected public alias field to use alias:\n{}", output);
        assert!(output.contains("  profile: Profile;\n"), "Expected field to use anonymous model alias:\n{}", output);
        assert!(output.contains("  value: Value;\n"), "Expected field to use union alias:\n{}", output);
        assert!(output.contains("  local: AccountLocalId;\n"), "Expected field to use nested alias:\n{}", output);
        assert!(output.contains("  secret: string;\n"), "Expected nested private alias field to inline:\n{}", output);
    }

    #[test]
    fn test_type_aliases_export_with_zod() {
        let src = indoc! { r#"
            type UserId = string

            model Account {
                id: UserId
            }
        "# };

        let output = gen_typescript_with_zod(src, true);
        assert!(output.contains("export type UserId = string;"), "Expected alias type export in zod mode:\n{}", output);
        assert!(output.contains("id: z.string()"), "Expected zod schema to expand alias structurally:\n{}", output);
    }

    #[test]
    fn test_model_scoped_constants_emit_with_prefixes() {
        let src = indoc! { r#"
            model Aliases {
                const SUFFIX = "_alias"
                const USER_ID_ALIAS = "user_id" + SUFFIX
                const _PRIVATE_FLAG = true
            }

            model User {
                const SUFFIX = "_user"
                user_id: string
            }
        "# };

        let output = gen_typescript(src);
        assert!(
            output.contains("export const ALIASES_SUFFIX: string = \"_alias\";"),
            "Expected model-prefixed string constant:\n{}",
            output
        );
        assert!(
            output.contains("export const ALIASES_USER_ID_ALIAS: string = \"user_id_alias\";"),
            "Expected declaration-scope folded constant:\n{}",
            output
        );
        assert!(output.contains("const _ALIASES_PRIVATE_FLAG: boolean = true;"), "Expected private model-prefixed constant:\n{}", output);
        assert!(output.contains("export const USER_SUFFIX: string = \"_user\";"), "Expected second model-prefixed constant:\n{}", output);
        assert!(output.find("ALIASES_SUFFIX").unwrap() < output.find("export type Aliases").unwrap());
    }

    #[test]
    fn test_anonymous_struct_types_only() {
        let src = indoc! { r#"
            model User {
                profile: {
                    bio: string
                    age?: int
                    address: {
                        city: string
                    }
                }
            }
        "# };

        assert_snapshot!(gen_typescript(src));
    }

    #[test]
    fn test_anonymous_struct_zod() {
        let src = indoc! { r#"
            model User {
                profile: {
                    bio: string
                    age?: int
                }
            }
        "# };

        assert_snapshot!(gen_typescript_with_zod(src, true));
    }

    #[test]
    fn test_comma_separated_members_generate() {
        let src = indoc! { r#"
            model User { id: string, profile: { bio: string, age?: u8 }, tags: string[] }
        "# };

        let output = gen_typescript(src);
        assert!(output.contains("id: string;"), "Expected top-level field:\n{}", output);
        assert!(
            output.contains("profile: { bio: string; age?: number };"),
            "Expected comma-separated anonymous model fields:\n{}",
            output
        );
        assert!(output.contains("tags: string[];"), "Expected comma-separated field:\n{}", output);

        let zod_output = gen_typescript_with_zod(src, true);
        assert!(
            zod_output.contains("profile: z.object({ bio: z.string(), age: z.number().optional() })"),
            "Expected zod anonymous object:\n{}",
            zod_output
        );
    }

    #[test]
    fn test_integer_primitive_mappings_downcast_to_number() {
        let src = indoc! { r#"
            model Numbers {
                int_value: int
                uint_value: uint
                i8_value: i8
                i16_value: i16
                i32_value: i32
                i64_value: i64
                u8_value: u8
                u16_value: u16
                u32_value: u32
                u64_value: u64
            }
        "# };

        let output = gen_typescript(src);
        for field in [
            "int_value",
            "uint_value",
            "i8_value",
            "i16_value",
            "i32_value",
            "i64_value",
            "u8_value",
            "u16_value",
            "u32_value",
            "u64_value",
        ] {
            assert!(output.contains(&format!("{}: number;", field)), "Expected {} to downcast to number:\n{}", field, output);
        }

        let zod_output = gen_typescript_with_zod(src, true);
        for field in [
            "int_value",
            "uint_value",
            "i8_value",
            "i16_value",
            "i32_value",
            "i64_value",
            "u8_value",
            "u16_value",
            "u32_value",
            "u64_value",
        ] {
            assert!(zod_output.contains(&format!("{}: z.number()", field)), "Expected {} to downcast to z.number():\n{}", field, zod_output);
        }
    }

    #[test]
    fn test_tuple_types() {
        let src = indoc! { r#"
            model Event {
                pair: (string, int)
                history: (string, int)[]
            }
        "# };

        let output = gen_typescript(src);
        assert!(output.contains("pair: [string, number];"), "Expected TypeScript tuple:\n{}", output);
        assert!(output.contains("history: [string, number][];"), "Expected TypeScript tuple array:\n{}", output);

        let zod_output = gen_typescript_with_zod(src, true);
        assert!(zod_output.contains("pair: z.tuple([z.string(), z.number()])"), "Expected Zod tuple schema:\n{}", zod_output);
        assert!(
            zod_output.contains("history: z.array(z.tuple([z.string(), z.number()]))"),
            "Expected Zod tuple array schema:\n{}",
            zod_output
        );
    }

    #[test]
    fn test_pascal_identifiers_preserve_uppercase() {
        let src = indoc! { r#"
            model XMLDocument {
                region: string
            }

            model XMLParser {
                document: XMLDocument
            }
        "# };

        let output = gen_typescript(src);
        assert!(output.contains("export type XMLDocument = {"), "Expected model acronym to be preserved:\n{}", output);
        assert!(output.contains("export type XMLParser = {"), "Expected model acronym to be preserved:\n{}", output);
        assert!(output.contains("document: XMLDocument;"), "Expected reference acronym to be preserved:\n{}", output);
    }

    #[test]
    fn test_preserve_generated_identifiers_config() {
        let src = indoc! { r#"
            model xml_document {
                XML_version: string
            }

            model xml_parser {
                document: xml_document
            }

            enum xml_parse_mode: "STRICT_MODE" | "HTML-mode"
        "# };

        let output = gen_test_with_config(
            &CodeGenTypeScript,
            src,
            Some(GlueConfigSchemaGeneration {
                preserve_generated_identifiers: Some(true),
                ..Default::default()
            }),
        );
        assert!(
            output.contains("export type xml_document = {"),
            "Expected configured model identifier to be preserved exactly:\n{}",
            output
        );
        assert!(output.contains("XML_version: string;"), "Expected field identifier to remain unchanged:\n{}", output);
        assert!(
            output.contains("export type xml_parser = {"),
            "Expected configured model identifier to be preserved exactly:\n{}",
            output
        );
        assert!(
            output.contains("document: xml_document;"),
            "Expected configured reference identifier to be preserved exactly:\n{}",
            output
        );
        assert!(
            output.contains("export type xml_parse_mode = \"STRICT_MODE\" | \"HTML-mode\";"),
            "Expected configured enum identifier to be preserved exactly:\n{}",
            output
        );
    }
}
