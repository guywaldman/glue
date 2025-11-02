use convert_case::Casing;
use lang::{AnalyzedProgram, AstNode, Enum, Field, LNode, LSyntaxKind, Model, SourceCodeMetadata};

use crate::CodeGenerator;

pub struct CodeGenPython;

impl CodeGenerator for CodeGenPython {
    fn generate(&self, program: AnalyzedProgram, _source: &SourceCodeMetadata) -> Result<String, crate::CodeGenError> {
        let ast = program.ast_root;

        let mut output = String::new();

        let mut prelude = String::new();
        prelude.push_str("from pydantic import BaseModel\n");

        output.push_str(&prelude);

        for child in ast.children() {
            match child.kind() {
                LSyntaxKind::MODEL => {
                    Self::walk(child, &mut output);
                }
                LSyntaxKind::ENUM => {
                    Self::walk(child, &mut output);
                }
                _ => {}
            }
        }
        Ok(output)
    }
}

impl CodeGenPython {
    pub fn new() -> Self {
        Self
    }

    fn walk(node: LNode, output: &mut String) {
        let kind = node.kind();
        match kind {
            LSyntaxKind::MODEL => {
                let model = Model::cast(node).expect("Expected model node");
                let model_name_token = model.ident_token().expect("Expected model to have ident token");
                let model_name = model_name_token.text().to_string();
                // TODO: Configurable BaseModel
                output.push_str(&format!("\nclass {}(BaseModel):\n", model_name));
                if let Some(docs) = model.docs() {
                    output.push_str(&Self::emit_docs(docs, 1));
                }
                let field_nodes = model.field_nodes();
                for field_node in field_nodes {
                    Self::walk(field_node, output);
                }
            }
            LSyntaxKind::ENUM => {
                let enum_model = Enum::cast(node).expect("Expected enum node");
                let enum_name_token = enum_model.ident_token().expect("Expected enum to have ident token");
                let enum_name = enum_name_token.text().to_string();
                output.push_str(&format!("\nclass {}(StrEnum):\n", enum_name));
                if let Some(docs) = enum_model.docs() {
                    output.push_str(&Self::emit_docs(docs, 1));
                }

                let variant_nodes = enum_model.variant_nodes();
                for variant_node in variant_nodes {
                    let variant_node_str = variant_node.text().to_string();
                    let variant_node_ident = variant_node_str.trim_start_matches('"').trim_end_matches('"').to_case(convert_case::Case::Constant);
                    output.push_str(&format!("    {} = {}\n", variant_node_ident, variant_node_str));
                }
            }
            LSyntaxKind::FIELD => {
                let field = Field::cast(node).expect("Expected field node");
                let field_name_token = field.ident_token().expect("Expected field to have ident token");
                let field_name = field_name_token.text().to_string();
                output.push_str(&format!("    {}: str\n", field_name));
            }
            _ => {}
        }
    }

    fn emit_docs(lines: Vec<String>, indent_level: usize) -> String {
        let mut output = String::new();
        let indent = " ".repeat(indent_level * 4);
        if lines.len() == 1 {
            return format!("{}\"\"\"{}\"\"\"\n", indent, lines[0].trim());
        }
        output.push_str(&format!("{}\"\"\"\n", indent));
        for line in lines {
            output.push_str(&format!("{}{}\n", indent, line.trim()));
        }
        output.push_str(&format!("{}\"\"\"\n", indent));
        output
    }
}

#[cfg(test)]
mod tests {
    use indoc::indoc;
    use insta::assert_snapshot;
    use lang::print_report;

    use crate::{CodeGenError, CodeGenPython, CodeGenerator, test_utils::analyze_glue_file};

    #[test]
    fn test() {
        let src = indoc! { r#"
				@root
				model GlueConfigSchema {
					/// Configuration for code generation (`glue gen [...]`)
					@default
					generation: Generation

					model Generation {
						/// Mode for the watermark at the top of the generated files
						watermark: Watermark = "short"
						enum Watermark: "full" | "short" | "none"

						/// Configurations for Rust code generation using Serde (`glue gen rust-serde [...]`)
						rust_serde: RustSerde
						model RustSerde {
							include_yaml: bool = false
						}

						/// Configurations for Python code generation using Pydantic (`glue gen py-pydantic [...]`)
						python_pydantic: PythonPydantic
						model PythonPydantic {
							/// The full import path for the base model class to inherit from (e.g., `pydantic.BaseModel` or `my.module.CustomBaseModel`)
							base_model: string = "pydantic.BaseModel"
						}
					}
				}
				"# };

        let (program, source) = analyze_glue_file(src);

        let codegen = CodeGenPython::new();
        let output = codegen
            .generate(program, &source)
            .map_err(|e| match e {
                CodeGenError::GeneralError(msg) => msg,
                CodeGenError::GenerationErrors(diags) => {
                    for diag in diags {
                        print_report(&diag).expect("Failed to print diagnostic");
                    }
                    panic!("Generation errors occurred");
                }
            })
            .unwrap();

        assert_snapshot!(output);
    }
}
