use config::GlueConfig;
use lang::{AnalyzedProgram, AstNode, DiagnosticContext, Endpoint, LNode, Literal, SourceCodeMetadata, SymTable};
use serde::{Deserialize, Serialize, ser};

use crate::{CodeGenError, CodeGenerator, codegen::CodeGenResult};

// OpenAPI structures

#[derive(Debug, Serialize, Deserialize)]
struct OpenAPI {
    #[serde(rename = "openapi")]
    openapi: String,
    #[serde(rename = "info")]
    info: Info,
}

#[derive(Debug, Serialize, Deserialize)]
struct Info {
    #[serde(rename = "title")]
    title: String,
    #[serde(rename = "version")]
    version: String,
}

pub struct CodeGenOpenAPI;

// TODO: Refactor such that visitors also emit contributions, and similar refs are shared and not inlined
impl CodeGenerator for CodeGenOpenAPI {
    fn generate(&self, program: AnalyzedProgram, source: &SourceCodeMetadata, _config: Option<GlueConfig>) -> Result<String, crate::CodeGenError> {
        let ast = program.ast_root.clone();

        let mut codegen = CodeGeneratorImpl::new(ast, program.symbols, source);
        let output = codegen.generate()?;
        Ok(output)
    }
}

impl Default for CodeGenOpenAPI {
    fn default() -> Self {
        Self::new()
    }
}

impl CodeGenOpenAPI {
    pub fn new() -> Self {
        Self
    }
}

struct CodeGeneratorImpl {
    #[allow(dead_code)]
    diag: DiagnosticContext,
    ast: LNode,
    syms: SymTable<LNode>,
    preludes: Vec<String>,
}

impl CodeGeneratorImpl {
    pub fn new(ast: LNode, syms: SymTable<LNode>, source: &SourceCodeMetadata) -> Self {
        let diag = DiagnosticContext::new(source.file_name, source.file_contents);
        Self {
            diag,
            ast,
            syms,
            preludes: Default::default(),
        }
    }

    pub fn generate(&mut self) -> CodeGenResult<String> {
        let output = String::new();

        let top_level_endpoints = self.ast.children().filter(|node| node.kind() == lang::LSyntaxKind::ENDPOINT);

        for endpoint in top_level_endpoints {
            self.visit_endpoint(endpoint)?;
        }

        Ok(output)
    }

    fn visit_endpoint(&mut self, node: LNode) -> CodeGenResult<()> {
        let (method, path) = self.extract_method_and_path_from_endpoint(node.clone())?;
        Ok(())
    }

    fn extract_method_and_path_from_endpoint(&self, endpoint_node: LNode) -> CodeGenResult<(String, String)> {
        let endpoint = Endpoint::cast(endpoint_node.clone()).expect("Expected endpoint node");
        let endpoint_string_literal_node = endpoint.endpoint_string_literal_node().unwrap();
        let endpoint_string_literal = endpoint_string_literal_node.value().unwrap();
        let parts = endpoint_string_literal.split_whitespace().collect::<Vec<&str>>();
        if parts.len() != 2 {
            return Err(CodeGenError::GenerationError(
                self.diag
                    .error(endpoint_string_literal_node.syntax().text_range(), "Endpoint string literal must be in the format 'METHOD /path'"),
            ));
        }
        let method = parts[0].to_string();
        let path = parts[1].to_string();
        Ok((method, path))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use indoc::indoc;
    use insta::assert_snapshot;
    use lang::print_report;

    use crate::{CodeGenError, CodeGenerator, test_utils::analyze_test_glue_file};

    #[test]
    fn test_basic_endpoint() {
        let src = indoc! {r#"
            endpoint "GET /users" ListUsers {
                id: string
                name: string
            }
        "#};
        let (program, source) = analyze_test_glue_file(src);
        let codegen = CodeGenOpenAPI::new();
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
        assert_snapshot!(result);
    }
}
