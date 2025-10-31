use gluelang::{Ast, AstNodeId, Decorator, DiagnosticContext, RawAstNode, RawTypeAtom};
use json::{JsonValue, object::Object};
use miette::Report;

use crate::codegen::{CodeGenError, CodeGenerator, types::EmitResult};

pub struct JsonSchemaCodeGenerator<'a> {
    ast: &'a Ast<'a>,
    diagnostic_ctx: DiagnosticContext,
}

impl CodeGenerator for JsonSchemaCodeGenerator<'_> {
    fn generate(mut self) -> EmitResult {
        let root_model_id = self.find_root_model()?;
        let model_node = self.ast.node(root_model_id).expect("model node must exist");
        let model = model_node.as_model().expect("root node must be a model");
        let title = model.name.to_string();
        let properties = self.emit_model(root_model_id)?;

        let mut schema = Object::new();
        schema["$schema"] = JsonValue::String("http://json-schema.org/draft-07/schema#".to_string());
        schema["title"] = JsonValue::String(title);
        schema["type"] = JsonValue::String("object".to_string());
        schema["properties"] = JsonValue::Object(properties);

        Ok(json::stringify_pretty(JsonValue::Object(schema), 4))
    }
}

impl<'a> JsonSchemaCodeGenerator<'a> {
    pub fn new(ast: &'a Ast<'a>) -> Self {
        let diagnostic_ctx = DiagnosticContext::new(ast.source_code_metadata.file_name, ast.source_code_metadata.contents);
        Self { ast, diagnostic_ctx }
    }

    fn find_root_model(&self) -> Result<AstNodeId, CodeGenError> {
        let mut root_id = None;

        for &node_id in self.ast.top_level_nodes() {
            let Some(node) = self.ast.node(node_id) else {
                continue;
            };
            let Some(model) = node.as_model() else {
                continue;
            };

            let has_root = model.decorators.iter().any(|&decorator_id| {
                self.ast
                    .node(decorator_id)
                    .and_then(|decorator| decorator.as_decorator())
                    .map(|Decorator { name }| *name == "root")
                    .unwrap_or(false)
            });

            if has_root {
                if root_id.is_some() {
                    let report = self.diagnostic_ctx.error(node.span, format_args!("Multiple `@root` models found"), None);
                    return Err(Box::new(report));
                }
                root_id = Some(node_id);
            }
        }

        match root_id {
            Some(id) => Ok(id),
            None => {
                let root_node = self.ast.node(0).expect("root node must exist");
                let report = self.diagnostic_ctx.error(root_node.span, format_args!("Could not find model decorated with `@root`"), None);
                Err(Box::new(report))
            }
        }
    }

    fn emit_model(&mut self, model_id: AstNodeId) -> Result<Object, CodeGenError> {
        let model_node = self.ast.node(model_id).ok_or_else(|| Box::new(Report::msg("Model node missing")))?;
        let RawAstNode::Model(model) = &model_node.payload else {
            let report = self.diagnostic_ctx.error(model_node.span, format_args!("Expected a model node"), None);
            return Err(Box::new(report));
        };

        let mut result = Object::new();

        for &field_id in &model.fields {
            let Some(field_node) = self.ast.node(field_id) else {
                continue;
            };
            let RawAstNode::Field(field) = &field_node.payload else {
                continue;
            };

            if field.type_atoms.is_empty() {
                continue;
            }

            let mut field_object = Object::new();

            if field.type_atoms.len() > 1 {
                let mut any_of = Vec::new();
                for &type_atom_id in &field.type_atoms {
                    any_of.push(self.emit_type_atom(type_atom_id)?);
                }
                field_object["anyOf"] = JsonValue::Array(any_of);
            } else {
                let emitted = self.emit_type_atom(field.type_atoms[0])?;
                match emitted {
                    JsonValue::Object(obj) => field_object = obj,
                    other => field_object["value"] = other,
                }
            }

            if let Some(doc) = &field.doc {
                field_object["description"] = JsonValue::String(doc.content.clone());
            }

            result[field.name] = JsonValue::Object(field_object);
        }

        Ok(result)
    }

    fn emit_type_atom(&mut self, node_id: AstNodeId) -> Result<JsonValue, CodeGenError> {
        let node = self.ast.node(node_id).ok_or_else(|| Box::new(Report::msg("Type atom node missing")))?;
        let RawAstNode::TypeAtom(atom) = &node.payload else {
            let report = self.diagnostic_ctx.error(node.span, format_args!("Expected a type atom"), None);
            return Err(Box::new(report));
        };

        let mut value = match &atom.payload {
            RawTypeAtom::String => self.simple_type("string"),
            RawTypeAtom::Int => self.simple_type("integer"),
            RawTypeAtom::Bool => self.simple_type("boolean"),
            RawTypeAtom::Ref(name) => {
                if let Some(model_id) = self.find_model_by_name(name) {
                    let props = self.emit_model(model_id)?;
                    let mut obj = Object::new();
                    obj["type"] = JsonValue::String("object".to_string());
                    obj["properties"] = JsonValue::Object(props);
                    JsonValue::Object(obj)
                } else {
                    let mut obj = Object::new();
                    obj["type"] = JsonValue::String("string".to_string());
                    obj["description"] = JsonValue::String(format!("Unresolved reference: {}", name));
                    JsonValue::Object(obj)
                }
            }
            RawTypeAtom::AnonModel(model_id) => {
                let props = self.emit_model(*model_id)?;
                let mut obj = Object::new();
                obj["type"] = JsonValue::String("object".to_string());
                obj["properties"] = JsonValue::Object(props);
                JsonValue::Object(obj)
            }
        };

        if atom.is_array {
            value = self.wrap_array(value);
        }

        if atom.is_optional {
            value = self.mark_nullable(value);
        }

        Ok(value)
    }

    fn simple_type(&self, ty: &str) -> JsonValue {
        let mut obj = Object::new();
        obj["type"] = JsonValue::String(ty.to_string());
        JsonValue::Object(obj)
    }

    fn wrap_array(&self, inner: JsonValue) -> JsonValue {
        let mut obj = Object::new();
        obj["type"] = JsonValue::String("array".to_string());
        obj["items"] = inner;
        JsonValue::Object(obj)
    }

    fn mark_nullable(&self, value: JsonValue) -> JsonValue {
        match value {
            JsonValue::Object(mut obj) => {
                obj["nullable"] = JsonValue::Boolean(true);
                JsonValue::Object(obj)
            }
            other => other,
        }
    }

    fn find_model_by_name(&self, name: &str) -> Option<AstNodeId> {
        self.ast
            .nodes()
            .iter()
            .find(|node| matches!(&node.payload, RawAstNode::Model(model) if model.name == name))
            .map(|node| node.id)
    }
}

#[cfg(test)]
mod tests {
    use indoc::indoc;
    use insta::assert_snapshot;

    use crate::test_utils::parse;

    use super::*;

    #[test]
    fn test_jsonschema_basic() {
        let src = indoc! {r#"
            @root
            model Config {
                /// The title of the configuration
                title: string
                /// The version of the configuration
                version: string | int
                /// Configuration for the database
                database: DatabaseConfig

                /// Database configuration model
                model DatabaseConfig {
                    /// The hostnmae
                    host: string
                    /// The port number
                    port: int
                }
            }
        "#};
        let parsed_program = parse(src).expect("Parsing failed");
        let codegen = JsonSchemaCodeGenerator::new(&parsed_program.ast);
        let generated_code = codegen.generate().expect("Code generation failed");
        assert_snapshot!(generated_code);
    }

    #[test]
    fn test_config_schema() {
        let src = indoc! {r#"
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
        "#};
        let parsed_program = parse(src).expect("Parsing failed");
        let codegen = JsonSchemaCodeGenerator::new(&parsed_program.ast);
        let generated_code = codegen.generate().expect("Code generation failed");
        assert_snapshot!(generated_code);
    }
}
