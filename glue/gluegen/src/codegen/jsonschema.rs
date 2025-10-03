use gluelang::{Ast, AstNode, AstNodeKind, AstSymbol, PrimitiveType, SemanticAnalysisArtifacts, SymbolTable, TreeNode, Type, TypeAtom, TypeVariant};

use crate::codegen::{CodeGenError, CodeGenerator, types::EmitResult};

pub struct JsonSchemaCodeGenerator {
    ast: Ast,
    symbols: SymbolTable,
}

impl CodeGenerator for JsonSchemaCodeGenerator {
    fn generate(mut self) -> EmitResult {
        let mut json = json::object::Object::new();

        let top_level_nodes = self
            .ast
            .get_children(self.ast.get_root())
            .ok_or_else(|| CodeGenError::Other("AST root has no children".to_string()))?;

        // Find the model that has the `@root` decorator
        let root_model_node = top_level_nodes.iter().find(|node| {
            if let AstNodeKind::Model { .. } = node.kind() {
                let Some(root_decorators) = self
                    .ast
                    .get_children_fn(node.id(), |n| matches!(n.kind(), AstNodeKind::Decorator { name, .. } if name == "root"))
                else {
                    return false;
                };
                !root_decorators.is_empty()
            } else {
                false
            }
        });
        let Some(root_model_node) = root_model_node else {
            return Err(CodeGenError::Other(
                "For JSON schema generation, one top-level model must be decorated with the `@root` decorator".to_string(),
            ));
        };
        let root_model_node_name = match root_model_node.kind() {
            AstNodeKind::Model { name, .. } => name,
            _ => return Err(CodeGenError::Other("Expected a model node".to_string())),
        };
        let root_model = self.emit_model(root_model_node)?;

        json["$schema"] = json::JsonValue::String("http://json-schema.org/draft-07/schema#".to_string());
        json["title"] = json::JsonValue::String(root_model_node_name.to_string());
        json["type"] = json::JsonValue::String("object".to_string());
        json["properties"] = json::JsonValue::Object(root_model);

        let result = json::stringify_pretty(json, 4);

        Ok(result)
    }
}

impl JsonSchemaCodeGenerator {
    pub fn new(artifacts: SemanticAnalysisArtifacts) -> Self {
        Self {
            ast: artifacts.ast,
            symbols: artifacts.symbols,
        }
    }

    /// Emit JSON Schema model properties from an AST node.
    fn emit_model(&mut self, model: &AstNode) -> Result<json::object::Object, CodeGenError> {
        let mut result = json::object::Object::new();

        let AstNodeKind::Model { name, .. } = model.kind() else {
            return Err(CodeGenError::Other("Expected a model node".to_string()));
        };

        let children = self.ast.get_children(model.id()).unwrap_or_default();
        for child in children {
            match child.kind() {
                AstNodeKind::Field { .. } => {
                    let AstNodeKind::Field { name: field_name, ty, doc, .. } = child.kind() else {
                        continue;
                    };
                    let types = match ty {
                        Type::Single(t) => vec![t.clone()],
                        Type::Union(variants) => variants.clone(),
                    };

                    let mut field_obj = json::object::Object::new();

                    if types.len() > 1 {
                        // Use `anyOf` for union types
                        let mut any_of = vec![];
                        for t in &types {
                            let emitted = self.emit_type_atom(&child, t)?;
                            any_of.push(emitted);
                        }
                        field_obj["anyOf"] = json::JsonValue::Array(any_of);
                    } else {
                        let ty = &types[0];
                        let emitted = self.emit_type_atom(&child, ty)?;
                        // Merge the emitted type properties into field_obj instead of nesting under "type"
                        if let json::JsonValue::Object(type_obj) = emitted {
                            for (key, value) in type_obj.iter() {
                                field_obj[key] = value.clone();
                            }
                        }
                    }

                    if let Some(doc_str) = doc {
                        field_obj["description"] = json::JsonValue::String(doc_str.clone());
                    }
                    result[field_name] = json::JsonValue::Object(field_obj);
                }
                AstNodeKind::Model { .. } => {
                    let AstNodeKind::Model { name: nested_name, .. } = child.kind() else {
                        continue;
                    };
                    // Add model to definitions
                }
                _ => continue,
            }
        }

        Ok(result)
        // let children = self.ast.get_children(model.id()).unwrap_or_default();
    }

    fn emit_type(&mut self, node: &AstNode, ty: &Type) -> Result<json::JsonValue, CodeGenError> {
        match ty {
            Type::Single(atom) => self.emit_type_atom(node, atom),
            Type::Union(variants) => {
                let mut any_of = vec![];
                for variant in variants {
                    let emitted = self.emit_type_atom(node, variant)?;
                    any_of.push(emitted);
                }
                let mut obj = json::object::Object::new();
                obj["anyOf"] = json::JsonValue::Array(any_of);
                Ok(json::JsonValue::Object(obj))
            }
        }
    }

    fn emit_type_atom(&mut self, node: &AstNode, atom: &TypeAtom) -> Result<json::JsonValue, CodeGenError> {
        match &atom.variant {
            TypeVariant::Ref(ref_name) => {
                let symbols = self.symbols.symbols_in_scope(node.id());
                if let Some(symbols) = symbols {
                    if let Some(entry) = symbols.get(&AstSymbol::Model(ref_name.clone())) {
                        let model_node = self
                            .ast
                            .get_node(entry.id)
                            .ok_or_else(|| CodeGenError::Other(format!("Referenced model node with ID {} not found", entry.id)))?;
                        let model_properties = self.emit_model(&model_node)?;

                        if atom.is_array {
                            let mut items_obj = json::object::Object::new();
                            items_obj["type"] = json::JsonValue::String("object".to_string());
                            items_obj["properties"] = json::JsonValue::Object(model_properties);

                            let mut array_obj = json::object::Object::new();
                            array_obj["type"] = json::JsonValue::String("array".to_string());
                            array_obj["items"] = json::JsonValue::Object(items_obj);
                            Ok(json::JsonValue::Object(array_obj))
                        } else {
                            let mut obj = json::object::Object::new();
                            obj["type"] = json::JsonValue::String("object".to_string());
                            obj["properties"] = json::JsonValue::Object(model_properties);
                            Ok(json::JsonValue::Object(obj))
                        }
                    } else {
                        Err(CodeGenError::Other(format!("Referenced model '{ref_name}' not found in symbols")))
                    }
                } else {
                    Err(CodeGenError::Other("No symbols found in scope".to_string()))
                }
            }
            TypeVariant::Primitive(ty) => {
                let type_str = match ty {
                    PrimitiveType::String => "string",
                    PrimitiveType::Int => "integer",
                    PrimitiveType::Bool => "boolean",
                };
                let mut obj = json::object::Object::new();
                obj["type"] = json::JsonValue::String(type_str.to_string());
                if atom.is_array {
                    let mut array_obj = json::object::Object::new();
                    array_obj["type"] = json::JsonValue::String("array".to_string());
                    array_obj["items"] = json::JsonValue::Object(obj);
                    Ok(json::JsonValue::Object(array_obj))
                } else {
                    Ok(json::JsonValue::Object(obj))
                }
            }
        }
    }
}
