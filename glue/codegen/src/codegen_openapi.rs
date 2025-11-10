use std::collections::HashMap;

use config::GlueConfig;
use convert_case::Case;
use lang::{
    AnalyzedProgram, AstNode, DiagnosticContext, Endpoint, Field, LNode, Literal, MODEL_FIELD_DECORATOR, MODEL_FIELD_DECORATOR_ALIAS_ARG, MODEL_FIELD_DECORATOR_EXAMPLE_ARG, Model, PrimitiveType,
    SourceCodeMetadata, SymId, SymTable, Type, TypeAtom,
};
use serde::{Deserialize, Serialize, ser};

use crate::{
    CodeGenError, CodeGenerator,
    codegen::CodeGenResult,
    codegen_utils::qualified_symbol_name_to_case,
    models::{OpenApi, OpenApiComponent, OpenApiComponents, OpenApiInfo, OpenApiPath, OpenApiProperty},
};

pub struct CodeGenOpenAPI;

// TODO: Refactor such that visitors also emit contributions, and similar refs are shared and not inlined
impl CodeGenerator for CodeGenOpenAPI {
    fn generate(&self, program: AnalyzedProgram, source: &SourceCodeMetadata, _config: Option<GlueConfig>) -> Result<String, crate::CodeGenError> {
        let ast = program.ast_root.clone();

        let mut codegen = CodeGeneratorImpl::new(ast, program.symbols, source);
        let components = codegen.generate()?;

        // Serialize OpenAPI components to JSON
        let schemas: HashMap<String, HashMap<String, serde_json::Value>> = components
            .into_iter()
            .map(|(k, v)| (k, serde_json::to_value(v).unwrap().as_object().unwrap().clone().into_iter().collect()))
            .collect();

        let openapi = OpenApi {
            openapi: "3.0.0".to_string(),
            info: OpenApiInfo {
                title: "Generated API".to_string(),
                version: "1.0.0".to_string(),
            },
            paths: HashMap::new(),
            components: Some(OpenApiComponents { schemas }),
        };

        let openapi_json = serde_json::to_string_pretty(&openapi).expect("Failed to serialize OpenAPI to JSON");
        Ok(openapi_json)
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

    // Constructed during generation
    components: HashMap<String, OpenApiComponent>,
}

impl CodeGeneratorImpl {
    pub fn new(ast: LNode, syms: SymTable<LNode>, source: &SourceCodeMetadata) -> Self {
        let diag = DiagnosticContext::new(source.file_name, source.file_contents);
        Self {
            diag,
            ast,
            syms,
            preludes: Default::default(),

            // Constructed during generation
            components: Default::default(),
        }
    }

    pub fn generate(&mut self) -> CodeGenResult<HashMap<String, OpenApiComponent>> {
        let top_level_endpoints = self.ast.children().filter(|node| node.kind() == lang::LSyntaxKind::ENDPOINT);
        let top_level_models = self.ast.children().filter(|node| node.kind() == lang::LSyntaxKind::MODEL);

        for endpoint in top_level_endpoints {
            let (method, path, mime, openapi_path) = self.visit_endpoint(endpoint)?;
        }

        for model in top_level_models {
            self.visit_model(model, None)?;
        }

        Ok(self.components.clone())
    }

    // Returns (method, path, MIME type, OpenApiPath)
    fn visit_endpoint(&mut self, node: LNode) -> CodeGenResult<(String, String, String, OpenApiPath)> {
        let mut res = OpenApiPath::default();

        // TODO: Support multiple MIME types (perhaps from decorators)
        let mime_type = "application/json".to_string();

        let (method, path) = self.extract_method_and_path_from_endpoint(node.clone())?;
        let endpoint = Endpoint::cast(node.clone()).expect("Expected endpoint node");
        let endpoint_path = endpoint.path_string().expect("Expected endpoint path string");
        let endpoint_sym = self.syms.resolve_id(None, &endpoint_path).expect("Endpoint symbol not found");

        for nested_model_node in endpoint.nested_model_nodes() {
            self.visit_model(nested_model_node, Some(endpoint_sym))?;
        }

        let Some(responses_field_node) = endpoint.responses_field_node() else {
            return Err(self.err(node, "Endpoint missing responses field"));
        };
        let (_, responses_prop) = self.visit_field(responses_field_node.clone(), endpoint_sym)?;
        let mut responses = HashMap::new();
        // for (status_code, response_schema) in responses_prop.description.unwrap_or_default() {
        // }
        let res = OpenApiPath { ..Default::default() };

        Ok((method, path, mime_type, res))
    }

    fn visit_model(&mut self, node: LNode, parent_sym: Option<SymId>) -> CodeGenResult<()> {
        let model = Model::cast(node.clone()).expect("Expected model node");
        let model_name = model.ident().ok_or(CodeGenError::InternalError("Model missing name".to_string()))?;
        let model_sym = self.syms.resolve_nested_id(parent_sym, &model_name).expect("Model symbol not found");

        let mut fields = Vec::new();
        for field_node in model.field_nodes() {
            let (field_name, field_val) = self.visit_field(field_node, model_sym)?;
            fields.push((field_name, field_val));
        }

        self.components.insert(
            model_name.clone(),
            OpenApiComponent {
                r#type: "object".to_string(),
                properties: fields.into_iter().collect(),
                r#ref: None,
            },
        );
        Ok(())
    }

    fn visit_field(&mut self, node: LNode, parent_sym: SymId) -> CodeGenResult<(String, OpenApiProperty)> {
        let field = Field::cast(node.clone()).expect("Expected field node");
        let field_name = field.ident().ok_or(CodeGenError::InternalError("Field missing name".to_string()))?;
        let field_type_node = field.type_node().ok_or(CodeGenError::InternalError("Field missing type".to_string()))?;
        let mut prop = self.emit_type(field_type_node.clone())?;
        if let Some(docs) = field.docs() {
            prop.description = Some(docs.join("\n"));
        }
        let decorators = field.decorators();
        let field_decorator = decorators.iter().find(|dec| dec.ident() == Some(MODEL_FIELD_DECORATOR.id.to_owned()));
        // TODO: Support multiple examples (e.g., available in response models)
        let mut prop_example = None;
        if let Some(field_decorator) = field_decorator
            && let Some(examples_arg) = field_decorator.arg(MODEL_FIELD_DECORATOR, &MODEL_FIELD_DECORATOR_EXAMPLE_ARG)
        {
            let examples = examples_arg.literal().ok_or(CodeGenError::InternalError("Missing examples argument value".to_string()))?;
            match examples {
                Literal::StringLiteral(string_literal) => {
                    prop_example = Some(string_literal.value().expect("Expected string literal value").to_string());
                }
                _ => {
                    return Err(CodeGenError::InternalError("Example argument must be a string literal".to_string()));
                }
            }
        }
        if let Some(example) = prop_example {
            prop.example = Some(serde_json::Value::String(example));
        }

        Ok((field_name, prop))
    }

    fn emit_type(&mut self, node: LNode) -> CodeGenResult<OpenApiProperty> {
        let ty = Type::cast(node.clone()).expect("Expected type node");

        let type_atom_nodes = ty.type_atom_nodes();
        // TODO: Support unions
        if type_atom_nodes.len() > 1 {
            dbg!(&node);
            return Err(self.err(node, "Union types are not supported in OpenAPI code generation"));
        }
        let type_atom = type_atom_nodes.first().ok_or(CodeGenError::InternalError("Type missing type atom".to_string()))?;
        let type_atom_value = self.emit_type_atom(type_atom.clone())?;

        Ok(type_atom_value)
    }

    fn emit_type_atom(&mut self, node: LNode) -> CodeGenResult<OpenApiProperty> {
        let type_atom = TypeAtom::cast(node.clone()).expect("Expected type atom node");
        let nullable = match type_atom.is_optional() {
            true => Some(true),
            // TODO: Should we distinguish between absent and false?
            false => None,
        };

        if let Some(primitive) = type_atom.as_primitive_type() {
            let primitive_type = match primitive {
                PrimitiveType::Int => "integer",
                PrimitiveType::Float => "number",
                PrimitiveType::String => "string",
                PrimitiveType::Bool => "boolean",
                PrimitiveType::Any => "object",
            };

            if type_atom.is_array() {
                Ok(OpenApiProperty {
                    r#type: "array".to_string(),
                    items: Some({
                        let mut map = HashMap::new();
                        map.insert("type".to_string(), serde_json::Value::String(primitive_type.to_string()));
                        map
                    }),
                    nullable,
                    ..Default::default()
                })
            } else {
                Ok(OpenApiProperty {
                    r#type: primitive_type.to_string(),
                    nullable,
                    ..Default::default()
                })
            }
        } else if let Some(ref_tok) = type_atom.as_ref_token() {
            let ref_str = self.resolve_ref_str(ref_tok.text(), None)?;
            Ok(OpenApiProperty {
                r#type: ref_str,
                nullable: Some(false),
                ..Default::default()
            })
        } else {
            Err(self.err(node, "Unsupported type atom in OpenAPI code generation"))
        }
    }

    fn resolve_ref_str(&self, name: &str, parent_sym: Option<SymId>) -> CodeGenResult<String> {
        let qualified_name = if let Some(parent_sym) = parent_sym {
            let parent_entry = self.syms.get(parent_sym).ok_or(CodeGenError::InternalError("Parent symbol not found".to_string()))?;
            qualified_symbol_name_to_case(name, convert_case::Case::Pascal)
        } else {
            name.to_string()
        };
        Ok(qualified_name)
    }

    fn extract_method_and_path_from_endpoint(&self, endpoint_node: LNode) -> CodeGenResult<(String, String)> {
        let endpoint = Endpoint::cast(endpoint_node.clone()).expect("Expected endpoint node");
        let endpoint_string_literal_node = endpoint.path_string_literal_node().unwrap();
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

    fn err(&self, node: LNode, msg: &str) -> CodeGenError {
        CodeGenError::GenerationError(self.diag.error(node.text_range(), msg))
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
            /// Lists all users
            endpoint "GET /users" ListUsers {
                responses: {
                    200: User[]
                }
            }

            model User {
                id: int
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
