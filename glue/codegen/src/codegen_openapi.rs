use std::collections::HashMap;

use config::GlueConfigSchemaGeneration;
use convert_case::Case;
use lang::{
    AnonModel, AstNode, Endpoint, Field, GlueIr, Literal, MODEL_FIELD_DECORATOR, MODEL_FIELD_DECORATOR_ALIAS_ARG, MODEL_FIELD_DECORATOR_EXAMPLE_ARG, Model, SourceCodeMetadata, Type, TypeAtom,
};
use serde_json::Number;

use crate::CodeGenerator;
use crate::codegen::CodeGenResult;
use crate::context::{CodeGenContext, NamedExt, TypeMapper};
use crate::models::openapi;

#[derive(Default)]
pub struct CodeGenOpenAPI;

impl CodeGenerator for CodeGenOpenAPI {
    fn generate(&self, ir: GlueIr, source: &SourceCodeMetadata, _config: Option<GlueConfigSchemaGeneration>) -> CodeGenResult<String> {
        let program = ir
            .into_analyzed_program()
            .ok_or_else(|| crate::CodeGenError::InternalError("Glue IR does not contain an analyzed program".to_string()))?;
        let ctx = CodeGenContext::new(program.ast_root.clone(), program.symbols, source, None);
        let generator = OpenAPIGenerator::new(ctx);
        generator.generate()
    }
}

struct OpenAPIGenerator<'a> {
    ctx: CodeGenContext<'a>,
    schemas: HashMap<String, openapi::SchemaOrReference<openapi::Schema>>,
    paths: HashMap<String, openapi::PathItem>,
}

impl<'a> OpenAPIGenerator<'a> {
    fn new(ctx: CodeGenContext<'a>) -> Self {
        Self {
            ctx,
            schemas: HashMap::new(),
            paths: HashMap::new(),
        }
    }

    fn generate(mut self) -> CodeGenResult<String> {
        for model in self.ctx.top_level_models().collect::<Vec<_>>() {
            self.process_model(&model);
        }

        for endpoint in self.ctx.top_level_endpoints().collect::<Vec<_>>() {
            self.process_endpoint(&endpoint);
        }

        let openapi = openapi::OpenAPI {
            openapi: "3.0.0".to_string(),
            info: openapi::Info {
                title: "Generated API".to_string(),
                version: "1.0.0".to_string(),
                ..Default::default()
            },
            paths: Some(self.paths),
            components: Some(openapi::Components {
                schemas: Some(self.schemas),
                ..Default::default()
            }),
            ..Default::default()
        };

        Ok(serde_json::to_string_pretty(&openapi).expect("Failed to serialize OpenAPI"))
    }

    fn process_model(&mut self, model: &Model) {
        let Ok(name) = model.name() else { return };
        let schema_name = model.qualified_name(&self.ctx, None, Case::Pascal).unwrap_or(name);

        let fields = model.fields();
        let properties = self.fields_to_properties(&fields);
        let required: Vec<_> = fields.iter().filter(|f| !f.is_optional()).filter_map(Self::field_name).collect();

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

        for nested in model.nested_models() {
            self.process_model(&nested);
        }
    }

    fn process_endpoint(&mut self, endpoint: &Endpoint) {
        let Some(path_str) = endpoint.path_string() else { return };
        let Some((method, path)) = Self::parse_endpoint_path(&path_str) else { return };

        let responses = self.extract_responses(endpoint);
        let parameters = Self::extract_path_parameters(&path);
        let request_body = self.extract_request_body(endpoint);

        let operation = openapi::Operation {
            operation_id: endpoint.ident(),
            summary: endpoint.docs().and_then(|d| d.first().cloned()),
            description: endpoint.docs().map(|d| d.join("\n")),
            parameters: if parameters.is_empty() { None } else { Some(parameters) },
            request_body,
            responses: openapi::Responses { responses },
            ..Default::default()
        };

        let path_item = self.paths.entry(path).or_default();
        path_item.operations.insert(method, operation);
    }

    fn extract_path_parameters(path: &str) -> Vec<openapi::SchemaOrReference<openapi::Parameter>> {
        let mut params = Vec::new();
        for segment in path.split('/') {
            if let Some(name) = segment.strip_prefix('{').and_then(|s| s.strip_suffix('}')) {
                params.push(openapi::SchemaOrReference::Item(openapi::Parameter {
                    name: name.to_string(),
                    location: "path".to_string(),
                    required: Some(true),
                    schema: Some(openapi::SchemaOrReference::Item(openapi::Schema {
                        schema_type: Some("string".to_string()),
                        ..Default::default()
                    })),
                    description: None,
                }));
            }
        }
        params
    }

    fn type_to_schema(&self, ty: &Type) -> openapi::SchemaOrReference<openapi::Schema> {
        let atoms = ty.type_atoms();
        if atoms.len() != 1 {
            // Union types not yet supported
            return openapi::SchemaOrReference::Item(openapi::Schema {
                schema_type: Some("object".to_string()),
                ..Default::default()
            });
        }
        self.type_atom_to_schema(&atoms[0])
    }

    fn type_atom_to_schema(&self, atom: &TypeAtom) -> openapi::SchemaOrReference<openapi::Schema> {
        let nullable = atom.is_optional().then_some(true);

        if let Some(primitive) = atom.as_primitive_type() {
            let (schema_type, format) = TypeMapper::to_openapi(primitive);
            let base = openapi::Schema {
                schema_type: Some(schema_type.to_string()),
                format: format.map(String::from),
                nullable,
                ..Default::default()
            };
            return self.wrap_if_array(atom, openapi::SchemaOrReference::Item(base));
        }

        if let Some(anon_model) = atom.as_anon_model().and_then(AnonModel::cast) {
            let properties: HashMap<_, _> = anon_model
                .field_nodes()
                .into_iter()
                .filter_map(Field::cast)
                .filter_map(|f| Some((Self::field_name(&f)?, self.type_to_schema(&f.ty()?))))
                .collect();

            let base = openapi::Schema {
                schema_type: Some("object".to_string()),
                properties: if properties.is_empty() { None } else { Some(properties) },
                nullable,
                ..Default::default()
            };
            return self.wrap_if_array(atom, openapi::SchemaOrReference::Item(base));
        }

        if let Some(record) = atom.as_record_type() {
            let value_schema = record.dest_type_node().and_then(Type::cast).map(|dest_type| self.type_to_schema(&dest_type)).unwrap_or_else(|| {
                openapi::SchemaOrReference::Item(openapi::Schema {
                    schema_type: Some("object".to_string()),
                    ..Default::default()
                })
            });
            let base = openapi::Schema {
                schema_type: Some("object".to_string()),
                additional_properties: Some(Box::new(value_schema)),
                nullable,
                ..Default::default()
            };
            return self.wrap_if_array(atom, openapi::SchemaOrReference::Item(base));
        }

        if let Some(ref_token) = atom.as_ref_token() {
            let type_name = ref_token.text().to_string();
            if let Ok(Some(alias_type)) = self.ctx.resolve_type_alias(None, &type_name) {
                let schema = self.type_to_schema(&alias_type);
                return self.wrap_if_array(atom, schema);
            }
            let reference = format!("#/components/schemas/{}", ref_token.text());
            return self.wrap_if_array(atom, openapi::SchemaOrReference::Reference { reference });
        }

        openapi::SchemaOrReference::Item(openapi::Schema {
            schema_type: Some("object".to_string()),
            ..Default::default()
        })
    }

    fn wrap_if_array(&self, atom: &TypeAtom, schema: openapi::SchemaOrReference<openapi::Schema>) -> openapi::SchemaOrReference<openapi::Schema> {
        if atom.is_array() {
            openapi::SchemaOrReference::Item(openapi::Schema {
                schema_type: Some("array".to_string()),
                items: Some(Box::new(schema)),
                nullable: atom.is_optional().then_some(true),
                ..Default::default()
            })
        } else {
            schema
        }
    }

    fn fields_to_properties(&self, fields: &[Field]) -> HashMap<String, openapi::SchemaOrReference<openapi::Schema>> {
        fields
            .iter()
            .filter_map(|f| {
                let mut name = Self::field_name(f)?;
                let mut example: Option<Literal> = None;

                if let Some(dec) = f.decorators().iter().find(|d| d.ident().as_deref() == Some(MODEL_FIELD_DECORATOR.id)) {
                    if let Some(alias_arg) = dec.arg(MODEL_FIELD_DECORATOR, &MODEL_FIELD_DECORATOR_ALIAS_ARG)
                        && let Some(Literal::StringLiteral(alias)) = alias_arg.literal()
                    {
                        name = alias.value()?;
                    }
                    if let Some(example_arg) = dec.arg(MODEL_FIELD_DECORATOR, &MODEL_FIELD_DECORATOR_EXAMPLE_ARG) {
                        example = example_arg.literal();
                    }
                }

                let mut schema = self.type_to_schema(&f.ty()?);

                // Add description
                if let (Some(docs), openapi::SchemaOrReference::Item(s)) = (f.docs(), &mut schema) {
                    s.description = Some(docs.join("\n"));
                }

                // Add example
                if let (Some(lit), openapi::SchemaOrReference::Item(s)) = (example, &mut schema) {
                    s.example = Self::literal_to_json(&lit);
                }

                Some((name, schema))
            })
            .collect()
    }

    fn field_name(field: &Field) -> Option<String> {
        let raw = field.ident()?;
        Some(Self::decode_field_name(&raw))
    }

    fn decode_field_name(raw: &str) -> String {
        if raw.starts_with('"') && raw.ends_with('"') {
            serde_json::from_str::<String>(raw).unwrap_or_else(|_| raw.to_string())
        } else {
            raw.to_string()
        }
    }

    fn literal_to_json(lit: &Literal) -> Option<serde_json::Value> {
        match lit {
            Literal::StringLiteral(sl) => Some(serde_json::Value::String(sl.value()?.to_string())),
            Literal::IntLiteral { value, .. } => Some(serde_json::Value::Number(Number::from(*value))),
            Literal::FloatLiteral { value, .. } => serde_json::Number::from_f64(*value).map(serde_json::Value::Number),
            Literal::BoolLiteral { value, .. } => Some(serde_json::Value::Bool(*value)),
            _ => None,
        }
    }

    fn parse_endpoint_path(path_str: &str) -> Option<(String, String)> {
        let mut parts = path_str.split_whitespace();
        let method = parts.next()?.to_lowercase();
        let path = parts.next()?.to_string();
        Some((method, path))
    }

    fn extract_responses(&self, endpoint: &Endpoint) -> HashMap<String, openapi::SchemaOrReference<openapi::Response>> {
        let mut responses = HashMap::new();

        let Some(ty) = endpoint.responses_field_node().and_then(Field::cast).and_then(|f| f.ty()) else {
            return responses;
        };

        for atom in ty.type_atoms() {
            let Some(anon_model) = atom.as_anon_model().and_then(AnonModel::cast) else { continue };

            for field_node in anon_model.field_nodes() {
                let Some(field) = Field::cast(field_node) else { continue };
                let Some(status_code) = field.ident() else { continue };
                let Some(response_ty) = field.ty() else { continue };

                let response = openapi::Response {
                    description: Some(field.docs().map(|d| d.join("\n")).unwrap_or_else(|| format!("{} response", status_code))),
                    content: Some(HashMap::from([(
                        "application/json".to_string(),
                        openapi::MediaType {
                            schema: Some(self.type_to_schema(&response_ty)),
                            ..Default::default()
                        },
                    )])),
                };
                responses.insert(status_code, openapi::SchemaOrReference::Item(response));
            }
        }

        // Duplicate 2XX + 200 for clients that expect it
        if responses.contains_key("2XX")
            && !responses.contains_key("200")
            && let Some(response) = responses.get("2XX").cloned()
        {
            responses.insert("200".to_string(), response);
        }

        responses
    }

    fn extract_request_body(&self, endpoint: &Endpoint) -> Option<openapi::SchemaOrReference<openapi::RequestBody>> {
        let body_field = endpoint.body_field_node().and_then(Field::cast)?;
        let body_type = body_field.ty()?;
        let atoms = body_type.type_atoms();

        let mut content = HashMap::new();

        if atoms.len() == 1
            && let Some(anon_model_node) = atoms[0].as_anon_model()
            && let Some(anon_model) = AnonModel::cast(anon_model_node)
        {
            for media_field_node in anon_model.field_nodes() {
                let media_field = Field::cast(media_field_node)?;
                let media_type = Self::field_name(&media_field)?;
                let media_schema = self.type_to_schema(&media_field.ty()?);
                content.insert(
                    media_type,
                    openapi::MediaType {
                        schema: Some(media_schema),
                        ..Default::default()
                    },
                );
            }
        } else {
            content.insert(
                "application/json".to_string(),
                openapi::MediaType {
                    schema: Some(self.type_to_schema(&body_type)),
                    ..Default::default()
                },
            );
        }

        Some(openapi::SchemaOrReference::Item(openapi::RequestBody {
            description: body_field.docs().map(|d| d.join("\n")),
            content,
            required: Some(!body_field.is_optional()),
        }))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use indoc::indoc;
    use insta::{assert_json_snapshot, assert_snapshot};
    use lang::{GlueIr, SourceCodeMetadata};
    use serde_json::Value;

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
        let ir = GlueIr::from_analyzed(source.file_name, program);
        let codegen = CodeGenOpenAPI;
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
        let json_value: serde_json::Value = serde_json::from_str(&result).unwrap();
        assert_json_snapshot!(json_value);
    }

    #[test]
    fn test_record_string_to_model() {
        let src = indoc! {r#"
            model Container {
                metadata: Record<string, UserData>

                model UserData {
                    foo: string
                }
            }
        "#};

        let (program, source) = analyze_test_glue_file(src);
        let ir = GlueIr::from_analyzed(source.file_name, program);
        let codegen = CodeGenOpenAPI;
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

        let json_value: Value = serde_json::from_str(&result).unwrap();
        let metadata = &json_value["components"]["schemas"]["Container"]["properties"]["metadata"];
        assert_eq!(metadata["type"], "object");
        assert_eq!(metadata["additionalProperties"]["$ref"], "#/components/schemas/UserData");
    }

    #[test]
    fn test_quoted_field_names_are_unescaped_in_openapi() {
        let src = indoc! {r#"
            model InputItem {
                "input 1": string
                "input 2": string
            }
        "#};

        let (program, source) = analyze_test_glue_file(src);
        let ir = GlueIr::from_analyzed(source.file_name, program);
        let codegen = CodeGenOpenAPI;
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

        let json_value: Value = serde_json::from_str(&result).unwrap();
        let schema = &json_value["components"]["schemas"]["InputItem"];

        assert!(schema["properties"].get("input 1").is_some());
        assert!(schema["properties"].get("input 2").is_some());
        assert_eq!(schema["required"][0], "input 1");
        assert_eq!(schema["required"][1], "input 2");
    }

    #[test]
    fn test_endpoint_body_implicit_application_json() {
        let src = indoc! {r#"
            endpoint "POST /foo" CreateFoo {
                body: MyModel
                responses: {
                    200: MyModel
                }
            }

            model MyModel {
                id: string
            }
        "#};

        let (program, source) = analyze_test_glue_file(src);
        let ir = GlueIr::from_analyzed(source.file_name, program);
        let codegen = CodeGenOpenAPI;
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

        let json_value: Value = serde_json::from_str(&result).unwrap();
        let request_schema = &json_value["paths"]["/foo"]["post"]["requestBody"]["content"]["application/json"]["schema"];
        assert_eq!(request_schema["$ref"], "#/components/schemas/MyModel");
    }

    #[test]
    fn test_endpoint_body_explicit_media_types() {
        let src = indoc! {r#"
            endpoint "POST /foo" CreateFoo {
                body: {
                    "application/json": MyModel
                    "application/yaml": MyOtherModel
                }
                responses: {
                    200: MyModel
                }
            }

            model MyModel {
                id: string
            }

            model MyOtherModel {
                name: string
            }
        "#};

        let (program, source) = analyze_test_glue_file(src);
        let ir = GlueIr::from_analyzed(source.file_name, program);
        let codegen = CodeGenOpenAPI;
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

        let json_value: Value = serde_json::from_str(&result).unwrap();
        let json_schema = &json_value["paths"]["/foo"]["post"]["requestBody"]["content"]["application/json"]["schema"];
        let yaml_schema = &json_value["paths"]["/foo"]["post"]["requestBody"]["content"]["application/yaml"]["schema"];
        assert_eq!(json_schema["$ref"], "#/components/schemas/MyModel");
        assert_eq!(yaml_schema["$ref"], "#/components/schemas/MyOtherModel");
    }
}
