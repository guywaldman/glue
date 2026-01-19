use std::collections::{HashMap, HashSet};

use config::GlueConfig;
use convert_case::Case;
use lang::{AnalyzedProgram, AnonModel, AstNode, AstVisitor, DiagnosticContext, Endpoint, Field, LNode, Model, PrimitiveType, RootNode, SourceCodeMetadata, Type, TypeAtom};

use crate::{CodeGenError, CodeGenerator, codegen::CodeGenResult, codegen_utils::qualified_symbol_name_to_case};

pub struct CodeGenProtobuf;

impl CodeGenProtobuf {
    pub fn new() -> Self {
        Self
    }
}

impl Default for CodeGenProtobuf {
    fn default() -> Self {
        Self
    }
}

impl CodeGenerator for CodeGenProtobuf {
    fn generate(&self, program: AnalyzedProgram, source: &SourceCodeMetadata, _config: Option<GlueConfig>) -> Result<String, crate::CodeGenError> {
        let mut codegen = CodeGenProtobufImpl::new(program, source);
        let output = codegen.generate()?;
        Ok(output)
    }
}

impl AstVisitor for CodeGenProtobuf {}

struct CodeGenProtobufImpl {
    diag: DiagnosticContext,
    ast_root: LNode,
    imports: HashSet<String>,
}

impl CodeGenProtobufImpl {
    pub fn new(program: AnalyzedProgram, source: &SourceCodeMetadata) -> Self {
        let diag = DiagnosticContext::new(source.file_name, source.file_contents);
        Self {
            diag,
            ast_root: program.ast_root,
            imports: Default::default(),
        }
    }

    pub fn generate(&mut self) -> CodeGenResult<String> {
        let mut output = String::new();

        // TODO: Get package name from config
        let preamble = "syntax = \"proto3\";\n\npackage glue;\n\n".to_string();
        output.push_str(&preamble);

        let root_node = RootNode::cast(self.ast_root.clone()).expect("Root node should be a RootNode");
        for model in root_node.top_level_models() {
            let model_code = self.visit_model(&model, &root_node)?;
            output.push_str(&model_code);
            output.push('\n');
        }
        for enum_ in root_node.top_level_enums() {
            let enum_code = self.visit_enum(&enum_, &root_node)?;
            output.push_str(&enum_code);
            output.push('\n');
        }

        Ok(output)
    }

    fn visit_model(&mut self, model: &Model, _parent: &impl AstNode) -> CodeGenResult<String> {
        let mut output = String::new();

        for (i, field) in model.fields().iter().enumerate() {
            let field_ty = field.ty().expect("Field should have a type");
            let protobuf_type = self.glue_type_to_protobuf_type(&field_ty)?;
            let field_name = field.ident().ok_or_else(|| self.err(field.syntax().clone(), "Field should have an identifier"))?;
            output.push_str(&format!("    {} {} = {};\n", protobuf_type, field_name, i + 1));
        }

        let model_name = model.ident().ok_or_else(|| self.err(model.syntax().clone(), "Model should have an identifier"))?;
        output = format!("message {} {{\n{}}}\n", model_name, output);

        Ok(output)
    }

    fn visit_enum(&mut self, enum_: &lang::Enum, _parent: &impl AstNode) -> CodeGenResult<String> {
        let mut output = String::new();

        for (i, variant) in enum_.variants().iter().enumerate() {
            let variant_name = variant.value().ok_or_else(|| self.err(variant.syntax().clone(), "Enum variant should have an identifier"))?;
            output.push_str(&format!("    {} = {};\n", variant_name, i));
        }

        let enum_name = enum_.ident().ok_or_else(|| self.err(enum_.syntax().clone(), "Enum should have an identifier"))?;
        output = format!("enum {} {{\n{}}}\n", enum_name, output);

        Ok(output)
    }

    fn glue_type_to_protobuf_type(&mut self, ty: &Type) -> CodeGenResult<String> {
        let type_atoms = ty.type_atoms();
        let type_atom = type_atoms.first().ok_or_else(|| self.err(ty.syntax().clone(), "Type should have at least one type atom"))?;

        let mut output = if let Some(primitive) = type_atom.as_primitive_type() {
            match primitive {
                PrimitiveType::Int => Ok("int32".to_string()),
                PrimitiveType::Float => Ok("float".to_string()),
                PrimitiveType::String => Ok("string".to_string()),
                PrimitiveType::Bool => Ok("bool".to_string()),
                PrimitiveType::Any => {
                    self.imports.insert("import \"google/protobuf/any.proto\";".to_string());
                    Ok("google.protobuf.Any".to_string())
                }
            }
        } else if let Some(ref_token) = type_atom.as_ref_token() {
            let ref_name = ref_token.to_string();
            Ok(ref_name)
        } else {
            Err(self.err(type_atom.syntax().clone(), "Unsupported type atom"))
        };

        if type_atom.is_array() {
            output = Ok(format!("repeated {}", output?));
        }
        if type_atom.is_optional() {
            return Err(self.err(type_atom.syntax().clone(), "Protobuf does not support optional types directly"));
        }

        output
    }

    fn err(&self, node: LNode, message: &str) -> CodeGenError {
        CodeGenError::GenerationError(self.diag.error(node.text_range(), message))
    }
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

        // Prepend the original source code as a comment for reference
        let result_with_source = format!("// Original source code:\n// {}\n\n{}", src.replace("\n", "\n// "), result);

        assert_snapshot!(result_with_source);
    }
}
