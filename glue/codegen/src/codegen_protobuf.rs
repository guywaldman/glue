use config::GlueConfigSchemaGeneration;
use convert_case::Case;
use lang::{AnonModel, AstNode, Field, GlueIr, SourceCodeMetadata, SymId, Type, TypeAtom};

use crate::{
    CodeGenError, CodeGenerator,
    codegen::CodeGenResult,
    context::{AnonymousTypeNamer, CodeGenContext, EnumVariantExt, FieldExt, NamedExt, TypeMapper},
};

#[derive(Default)]
pub struct CodeGenProtobuf;

impl CodeGenerator for CodeGenProtobuf {
    fn generate(&self, ir: GlueIr, source: &SourceCodeMetadata, config: Option<GlueConfigSchemaGeneration>) -> Result<String, CodeGenError> {
        let program = ir
            .into_analyzed_program()
            .ok_or_else(|| CodeGenError::InternalError("Glue IR does not contain an analyzed program".to_string()))?;
        let protobuf_config = config.as_ref().and_then(|c| c.protobuf.clone()).unwrap_or_default();
        let package_name = protobuf_config.package_name.as_deref().unwrap_or("glue");
        let ctx = CodeGenContext::new(program.ast_root.clone(), program.symbols, source, config.as_ref());
        let mut generator = ProtobufGenerator::new(ctx, package_name.to_string());
        generator.generate()
    }
}

struct ProtobufGenerator<'a> {
    ctx: CodeGenContext<'a>,
    output: String,
    anon_namer: AnonymousTypeNamer,
    pending_anon_models: Vec<AnonymousModelDef>,
}

#[derive(Clone)]
struct AnonymousModelDef {
    name: String,
    model: AnonModel,
    scope: Option<SymId>,
    path: Vec<String>,
}

impl<'a> ProtobufGenerator<'a> {
    fn new(ctx: CodeGenContext<'a>, package_name: String) -> Self {
        let anon_namer = AnonymousTypeNamer::new(&ctx, Case::Pascal);
        Self {
            ctx,
            output: format!("syntax = \"proto3\";\n\npackage {};\n\n", package_name),
            anon_namer,
            pending_anon_models: Vec::new(),
        }
    }

    fn generate(&mut self) -> CodeGenResult<String> {
        for model in self.ctx.top_level_models().collect::<Vec<_>>() {
            let name = model.name()?;
            let scope = model.scope_id(&self.ctx, None)?;
            let model_path = self.ctx.symbol_path(scope);
            let code = self.emit_message(&name, &model.fields(), Some(scope), &model_path)?;
            self.output.push_str(&code);
        }

        for enum_ in self.ctx.top_level_enums().collect::<Vec<_>>() {
            let name = enum_.name()?;
            self.output.push_str(&format!("enum {} {{\n", name));
            for (i, variant) in enum_.variants().iter().enumerate() {
                let value = variant.variant_value()?;
                self.output.push_str(&format!("    {} = {};\n", value, i));
            }
            self.output.push_str("}\n\n");
        }

        self.emit_pending_anon_models()?;

        Ok(self.output.clone())
    }

    fn emit_message(&mut self, name: &str, fields: &[Field], scope: Option<SymId>, path: &[String]) -> CodeGenResult<String> {
        let mut output = format!("message {} {{\n", name);
        for (i, field) in fields.iter().enumerate() {
            let field_name = field.name()?;
            let field_ty = field.field_type()?;
            let mut field_path = path.to_vec();
            field_path.push(field_name.clone());
            let proto_type = self.emit_type(&field_ty, scope, &field_path)?;
            output.push_str(&format!("    {} {} = {};\n", proto_type, field_name, i + 1));
        }
        output.push_str("}\n\n");
        Ok(output)
    }

    fn emit_pending_anon_models(&mut self) -> CodeGenResult<()> {
        let mut index = 0;
        while index < self.pending_anon_models.len() {
            let def = self.pending_anon_models[index].clone();
            let code = self.emit_message(&def.name, &def.model.fields(), def.scope, &def.path)?;
            self.output.push_str(&code);
            index += 1;
        }
        Ok(())
    }

    fn emit_type(&mut self, ty: &Type, scope: Option<SymId>, path: &[String]) -> CodeGenResult<String> {
        let atoms = ty.type_atoms();
        let atom = atoms.first().ok_or_else(|| CodeGenContext::internal_error("Type should have at least one type atom"))?;

        if atom.is_optional() {
            return Err(self.ctx.error(atom.syntax(), "Protobuf does not support optional types directly"));
        }

        let base = if let Some(primitive) = atom.as_primitive_type() {
            TypeMapper::to_protobuf(primitive).to_string()
        } else if let Some(record) = atom.as_record_type() {
            let src = record.src_type_node().ok_or_else(|| CodeGenContext::internal_error("Record missing source type"))?;
            let dest = record.dest_type_node().ok_or_else(|| CodeGenContext::internal_error("Record missing destination type"))?;
            let src_type = Type::cast(src).ok_or_else(|| CodeGenContext::internal_error("Expected Type for record source"))?;
            let dest_type = Type::cast(dest).ok_or_else(|| CodeGenContext::internal_error("Expected Type for record destination"))?;
            let mut key_path = path.to_vec();
            key_path.push("Key".to_string());
            let mut value_path = path.to_vec();
            value_path.push("Value".to_string());
            format!("map<{}, {}>", self.emit_type(&src_type, scope, &key_path)?, self.emit_type(&dest_type, scope, &value_path)?)
        } else if let Some(ref_token) = atom.as_ref_token() {
            let ref_name = ref_token.text().to_string();
            if let Some(alias_type) = self.ctx.resolve_type_alias(scope, &ref_name)? {
                self.emit_type(&alias_type, scope, path)?
            } else {
                ref_token.to_string()
            }
        } else if let Some(anon_model) = atom.anon_model() {
            let name = self.anon_namer.allocate(&self.ctx, path, Case::Pascal);
            self.pending_anon_models.push(AnonymousModelDef {
                name: name.clone(),
                model: anon_model,
                scope,
                path: path.to_vec(),
            });
            name
        } else {
            return Err(self.ctx.error(atom.syntax(), "Unsupported type atom for protobuf"));
        };

        if atom.is_array() { Ok(format!("repeated {}", base)) } else { Ok(base) }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use indoc::indoc;
    use insta::assert_snapshot;
    use lang::{GlueIr, SourceCodeMetadata};

    use crate::{CodeGenerator, test_utils::analyze_test_glue_file};

    #[test]
    fn test_basic_endpoint() {
        let src = indoc! {r#"
            model User {
                id: int
                name: string
            }
        "#};
        let (program, source) = analyze_test_glue_file(src);
        let ir = GlueIr::from_analyzed(source.file_name, program);
        let codegen = CodeGenProtobuf::default();
        let result = codegen
            .generate(
                ir,
                &SourceCodeMetadata {
                    file_name: source.file_name,
                    file_contents: source.file_contents,
                },
                None,
            )
            .unwrap();

        let result_with_source = format!("// Original source code:\n// {}\n\n{}", src.replace("\n", "\n// "), result);

        assert_snapshot!(result_with_source);
    }

    #[test]
    fn test_anonymous_struct() {
        let src = indoc! {r#"
            model User {
                profile: {
                    bio: string
                    age: int
                }
            }
        "#};
        let (program, source) = analyze_test_glue_file(src);
        let ir = GlueIr::from_analyzed(source.file_name, program);
        let codegen = CodeGenProtobuf::default();
        let result = codegen
            .generate(
                ir,
                &SourceCodeMetadata {
                    file_name: source.file_name,
                    file_contents: source.file_contents,
                },
                None,
            )
            .unwrap();

        let result_with_source = format!("// Original source code:\n// {}\n\n{}", src.replace("\n", "\n// "), result);

        assert_snapshot!(result_with_source);
    }
}
