use std::collections::HashMap;

use config::GlueConfig;
use convert_case::Case;
use lang::{AnalyzedProgram, AnonModel, AstNode, AstVisitor, Endpoint, Field, LNode, Model, PrimitiveType, RootNode, SourceCodeMetadata, Type, TypeAtom};

use crate::codegen::CodeGenResult;
use crate::models::openapi;
use crate::{CodeGenerator, codegen_utils::qualified_symbol_name_to_case};

pub struct CodeGenOpenAPI;

impl CodeGenOpenAPI {
    pub fn new() -> Self {
        Self
    }
}

impl Default for CodeGenOpenAPI {
    fn default() -> Self {
        Self
    }
}

impl CodeGenerator for CodeGenOpenAPI {
    fn generate(&self, program: AnalyzedProgram, _source: &SourceCodeMetadata, _config: Option<GlueConfig>) -> CodeGenResult<String> {
        let mut codegen = CodeGenOpenAPIImpl::default();
        codegen.traverse(program.ast_root);

        let openapi = openapi::OpenAPI {
            openapi: "3.0.0".to_string(),
            info: openapi::Info {
                title: "Generated API".to_string(),
                version: "1.0.0".to_string(),
                ..Default::default()
            },
            paths: Some(codegen.paths),
            components: Some(openapi::Components {
                schemas: Some(codegen.schemas),
                ..Default::default()
            }),
            ..Default::default()
        };

        Ok(serde_json::to_string_pretty(&openapi).expect("Failed to serialize OpenAPI"))
    }
}

#[derive(Default)]
struct CodeGenOpenAPIImpl {
    schemas: HashMap<String, openapi::SchemaOrReference<openapi::Schema>>,
    paths: HashMap<String, openapi::PathItem>,
}

// TODO: Remove AstVisitor. This was a bad idea for this :(
impl AstVisitor for CodeGenOpenAPIImpl {
    fn visit_model(&mut self, model: &Model, _parent: &impl AstNode) {
        let Some(name) = model.ident() else { return };
        let schema_name = qualified_symbol_name_to_case(&name, Case::Pascal);

        let fields: Vec<_> = model.fields();
        let properties = self.fields_to_properties(&fields);
        let required: Vec<_> = fields.iter().filter(|f| !f.is_optional()).filter_map(|f| f.ident()).collect();

        let mut schema = openapi::Schema {
            schema_type: Some("object".to_string()),
            properties: if properties.is_empty() { None } else { Some(properties) },
            ..Default::default()
        };

        if !required.is_empty() {
            schema.required = Some(required);
        }

        if let Some(docs) = model.docs() {
            schema.description = Some(docs.join("\n"));
        }

        self.schemas.insert(schema_name, openapi::SchemaOrReference::Item(schema));
    }

    fn visit_endpoint(&mut self, endpoint: &Endpoint, _parent: &impl AstNode) {
        let Some(path_str) = endpoint.path_string() else { return };
        let Some((method, path)) = Self::parse_endpoint_path(&path_str) else { return };

        // Build responses from the `responses` field
        let responses = self.extract_responses(endpoint);

        let operation = openapi::Operation {
            operation_id: endpoint.ident(),
            summary: endpoint.docs().and_then(|d| d.first().cloned()),
            description: endpoint.docs().map(|d| d.join("\n")),
            responses: openapi::Responses { responses },
            ..Default::default()
        };

        let path_item = self.paths.entry(path).or_default();
        path_item.operations.insert(method, operation);
    }
}

impl CodeGenOpenAPIImpl {
    fn type_to_schema(&self, ty: &Type) -> openapi::SchemaOrReference<openapi::Schema> {
        let atoms = ty.type_atoms();
        if atoms.len() != 1 {
            // Union types not yet supported; return generic object
            return openapi::SchemaOrReference::Item(openapi::Schema {
                schema_type: Some("object".to_string()),
                ..Default::default()
            });
        }

        let atom = &atoms[0];
        self.type_atom_to_schema(atom)
    }

    fn type_atom_to_schema(&self, atom: &TypeAtom) -> openapi::SchemaOrReference<openapi::Schema> {
        let nullable = if atom.is_optional() { Some(true) } else { None };

        // Check for primitive types
        if let Some(primitive) = atom.as_primitive_type() {
            let (schema_type, format) = match primitive {
                PrimitiveType::Int => ("integer", None),
                PrimitiveType::Float => ("number", Some("double")),
                PrimitiveType::String => ("string", None),
                PrimitiveType::Bool => ("boolean", None),
                PrimitiveType::Any => ("object", None),
            };

            let base_schema = openapi::Schema {
                schema_type: Some(schema_type.to_string()),
                format: format.map(String::from),
                nullable,
                ..Default::default()
            };

            return if atom.is_array() {
                openapi::SchemaOrReference::Item(openapi::Schema {
                    schema_type: Some("array".to_string()),
                    items: Some(Box::new(openapi::SchemaOrReference::Item(base_schema))),
                    nullable,
                    ..Default::default()
                })
            } else {
                openapi::SchemaOrReference::Item(base_schema)
            };
        }

        // Check for anonymous model (inline object)
        if let Some(anon_model) = atom.as_anon_model().and_then(AnonModel::cast) {
            let properties: HashMap<_, _> = anon_model
                .field_nodes()
                .into_iter()
                .filter_map(Field::cast)
                .filter_map(|f| Some((f.ident()?, self.type_to_schema(&f.ty()?))))
                .collect();

            let base_schema = openapi::Schema {
                schema_type: Some("object".to_string()),
                properties: if properties.is_empty() { None } else { Some(properties) },
                nullable,
                ..Default::default()
            };

            return if atom.is_array() {
                openapi::SchemaOrReference::Item(openapi::Schema {
                    schema_type: Some("array".to_string()),
                    items: Some(Box::new(openapi::SchemaOrReference::Item(base_schema))),
                    nullable,
                    ..Default::default()
                })
            } else {
                openapi::SchemaOrReference::Item(base_schema)
            };
        }

        // Check for reference type
        if let Some(ref_token) = atom.as_ref_token() {
            let reference = format!("#/components/schemas/{}", ref_token.text());

            return if atom.is_array() {
                openapi::SchemaOrReference::Item(openapi::Schema {
                    schema_type: Some("array".to_string()),
                    items: Some(Box::new(openapi::SchemaOrReference::Reference { reference })),
                    nullable,
                    ..Default::default()
                })
            } else {
                openapi::SchemaOrReference::Reference { reference }
            };
        }

        // Fallback
        openapi::SchemaOrReference::Item(openapi::Schema {
            schema_type: Some("object".to_string()),
            ..Default::default()
        })
    }

    /// Build schema properties from fields.
    fn fields_to_properties(&self, fields: &[Field]) -> HashMap<String, openapi::SchemaOrReference<openapi::Schema>> {
        fields
            .iter()
            .filter_map(|f| {
                let name = f.ident()?;
                let ty = f.ty()?;
                let mut schema = self.type_to_schema(&ty);

                // Add description from docs
                if let (Some(docs), openapi::SchemaOrReference::Item(s)) = (f.docs(), &mut schema) {
                    s.description = Some(docs.join("\n"));
                }

                Some((name, schema))
            })
            .collect()
    }

    /// Extract HTTP method and path from endpoint path string (like "GET /users").
    fn parse_endpoint_path(path_str: &str) -> Option<(String, String)> {
        let parts: Vec<&str> = path_str.split_whitespace().collect();
        if parts.len() == 2 { Some((parts[0].to_lowercase(), parts[1].to_string())) } else { None }
    }
}

impl CodeGenOpenAPIImpl {
    /// Extract responses from an endpoint's responses field.
    fn extract_responses(&self, endpoint: &Endpoint) -> HashMap<String, openapi::SchemaOrReference<openapi::Response>> {
        let mut responses = HashMap::new();

        let Some(ty) = endpoint.responses_field_node().and_then(Field::cast).and_then(|f| f.ty()) else {
            return responses;
        };

        for atom in ty.type_atoms() {
            let Some(anon_model) = atom.as_anon_model().and_then(AnonModel::cast) else {
                continue;
            };

            for field_node in anon_model.field_nodes() {
                // TODO: Error handling
                let Some(field) = Field::cast(field_node) else { continue };
                let Some(status_code) = field.ident() else { continue };
                let Some(response_ty) = field.ty() else { continue };

                let schema = self.type_to_schema(&response_ty);
                let response = openapi::Response {
                    description: field.docs().map(|d| d.join("\n")),
                    content: Some(HashMap::from([(
                        // TODO: Extract MIME type from decorator
                        "application/json".to_string(),
                        openapi::MediaType {
                            schema: Some(schema),
                            ..Default::default()
                        },
                    )])),
                };
                responses.insert(status_code, openapi::SchemaOrReference::Item(response));
            }
        }

        responses
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
        let codegen = CodeGenOpenAPI;
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
