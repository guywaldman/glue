use config::GlueConfigSchemaGeneration;
use convert_case::{Case, Casing};
use lang::{AstNode, Enum, Field, GlueIr, Model, SourceCodeMetadata, SymId, Type, TypeAtom};

use crate::{
    CodeGenError, CodeGenerator,
    codegen::CodeGenResult,
    context::{CodeGenContext, DocEmitter, EnumVariantExt, FieldExt, NamedExt, TypeMapper},
};

#[derive(Default)]
pub struct CodeGenTypeScript;

impl CodeGenerator for CodeGenTypeScript {
    fn generate(&self, ir: GlueIr, source: &SourceCodeMetadata, config: Option<GlueConfigSchemaGeneration>) -> Result<String, CodeGenError> {
        let program = ir
            .into_analyzed_program()
            .ok_or_else(|| CodeGenError::InternalError("Glue IR does not contain an analyzed program".to_string()))?;
        let ts_config = config.and_then(|g| g.typescript).unwrap_or_default();
        let zod_enabled = ts_config.zod.unwrap_or(false);
        let ctx = CodeGenContext::new(program.ast_root.clone(), program.symbols, source, None);
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

        for model in self.ctx.top_level_models().collect::<Vec<_>>() {
            self.emit_model(&model, None)?;
        }
        for enum_ in self.ctx.top_level_enums().collect::<Vec<_>>() {
            self.emit_enum(&enum_, None)?;
        }

        Ok(self.output.clone())
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
            <TypeMapper as TypeScriptTypeMapper>::to_typescript(primitive).to_string()
        } else if let Some(record) = atom.as_record_type() {
            let src = record.src_type_node().ok_or_else(|| CodeGenContext::internal_error("Record missing src type"))?;
            let dest = record.dest_type_node().ok_or_else(|| CodeGenContext::internal_error("Record missing dest type"))?;
            let src_type = Type::cast(src).ok_or_else(|| CodeGenContext::internal_error("Expected Type for record src"))?;
            let dest_type = Type::cast(dest).ok_or_else(|| CodeGenContext::internal_error("Expected Type for record dest"))?;
            format!("Record<{}, {}>", self.emit_type(&src_type, scope)?, self.emit_type(&dest_type, scope)?)
        } else if let Some(ref_token) = atom.as_ref_token() {
            let type_name = ref_token.text().to_string();
            let sym = self
                .ctx
                .resolve(scope, &type_name)
                .ok_or_else(|| CodeGenContext::internal_error(format!("Unresolved type: {}", type_name)))?;
            lang::symbol_name_to_parts(&sym.name).join("_").to_case(Case::Pascal)
        } else if atom.as_anon_model().is_some() {
            return Err(self.ctx.error(atom.syntax(), "Anonymous models not supported"));
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
        } else if let Some(ref_token) = atom.as_ref_token() {
            let type_name = ref_token.text().to_string();
            let sym = self
                .ctx
                .resolve(scope, &type_name)
                .ok_or_else(|| CodeGenContext::internal_error(format!("Unresolved type: {}", type_name)))?;
            let qualified = lang::symbol_name_to_parts(&sym.name).join("_").to_case(Case::Pascal);
            format!("{}Schema", qualified)
        } else if atom.as_anon_model().is_some() {
            return Err(self.ctx.error(atom.syntax(), "Anonymous models not supported"));
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

    fn zod_for_primitive(&self, primitive: lang::PrimitiveType) -> String {
        match primitive {
            lang::PrimitiveType::String => "z.string()".to_string(),
            lang::PrimitiveType::Int => "z.number()".to_string(),
            lang::PrimitiveType::Float => "z.number()".to_string(),
            lang::PrimitiveType::Bool => "z.boolean()".to_string(),
            lang::PrimitiveType::Any => "z.any()".to_string(),
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

trait TypeScriptTypeMapper {
    fn to_typescript(primitive: lang::PrimitiveType) -> &'static str;
}

impl TypeScriptTypeMapper for TypeMapper {
    fn to_typescript(primitive: lang::PrimitiveType) -> &'static str {
        match primitive {
            lang::PrimitiveType::String => "string",
            lang::PrimitiveType::Int => "number",
            lang::PrimitiveType::Float => "number",
            lang::PrimitiveType::Bool => "boolean",
            lang::PrimitiveType::Any => "any",
        }
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
}
