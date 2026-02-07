use config::GlueConfig;
use lang::{AnalyzedProgram, AstNode, SourceCodeMetadata, Type, TypeAtom};

use crate::{
    CodeGenError, CodeGenerator,
    codegen::CodeGenResult,
    context::{CodeGenContext, EnumVariantExt, FieldExt, NamedExt, TypeMapper},
};

#[derive(Default)]
pub struct CodeGenProtobuf;

impl CodeGenerator for CodeGenProtobuf {
    fn generate(&self, program: AnalyzedProgram, source: &SourceCodeMetadata, _config: Option<GlueConfig>) -> Result<String, CodeGenError> {
        let ctx = CodeGenContext::new(program.ast_root.clone(), program.symbols, source, None);
        let mut output = String::from("syntax = \"proto3\";\n\npackage glue;\n\n");

        for model in ctx.top_level_models().collect::<Vec<_>>() {
            let name = model.name()?;
            output.push_str(&format!("message {} {{\n", name));
            for (i, field) in model.fields().iter().enumerate() {
                let field_name = field.name()?;
                let field_ty = field.field_type()?;
                let proto_type = emit_type(&ctx, &field_ty)?;
                output.push_str(&format!("    {} {} = {};\n", proto_type, field_name, i + 1));
            }
            output.push_str("}\n\n");
        }

        for enum_ in ctx.top_level_enums().collect::<Vec<_>>() {
            let name = enum_.name()?;
            output.push_str(&format!("enum {} {{\n", name));
            for (i, variant) in enum_.variants().iter().enumerate() {
                let value = variant.variant_value()?;
                output.push_str(&format!("    {} = {};\n", value, i));
            }
            output.push_str("}\n\n");
        }

        Ok(output)
    }
}

fn emit_type(ctx: &CodeGenContext, ty: &Type) -> CodeGenResult<String> {
    let atoms = ty.type_atoms();
    let atom = atoms.first().ok_or_else(|| CodeGenContext::internal_error("Type should have at least one type atom"))?;

    if atom.is_optional() {
        return Err(ctx.error(atom.syntax(), "Protobuf does not support optional types directly"));
    }

    let base = if let Some(primitive) = atom.as_primitive_type() {
        TypeMapper::to_protobuf(primitive).to_string()
    } else if let Some(ref_token) = atom.as_ref_token() {
        ref_token.to_string()
    } else {
        return Err(ctx.error(atom.syntax(), "Unsupported type atom for protobuf"));
    };

    if atom.is_array() { Ok(format!("repeated {}", base)) } else { Ok(base) }
}

#[cfg(test)]
mod tests {
    use super::*;
    use indoc::indoc;
    use insta::assert_snapshot;
    use lang::SourceCodeMetadata;

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
        let codegen = CodeGenProtobuf;
        let result = codegen
            .generate(
                program,
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
