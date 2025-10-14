use std::collections::HashMap;

use gluelang::{
    Ast, AstNode, AstNodeId, AstNodeKind, AstNodePayload, ConstantValue, Enum, Field, Model, PrimitiveType, SemanticAnalysisArtifacts, SymbolTable, Type, TypeAtom, TypeVariant,
};

use crate::codegen::{CodeGenError, CodeGenerator, GlueConfigSchema, types::EmitResult};

pub struct RustSerdeCodeGenerator {
    ast: Ast,
    symbols: SymbolTable,
    prelude: String,
}

impl CodeGenerator for RustSerdeCodeGenerator {
    fn generate(mut self) -> EmitResult {
        self.preprocess_ast()?;

        let mut result = String::new();

        result.push_str("use serde::{Serialize, Deserialize};\n\n");

        let top_level_nodes = self
            .ast
            .get_children(self.ast.get_root())
            .ok_or(CodeGenError::Other("Failed to get top-level nodes".to_string()))?;

        for node in top_level_nodes {
            if node.kind == AstNodeKind::Model {
                let model_code = self.emit_model(&node)?;
                result.push_str(&model_code);
                result.push('\n');
            }
        }

        result.push_str(&self.prelude);

        Ok(result)
    }
}

impl RustSerdeCodeGenerator {
    pub fn new(_config: GlueConfigSchema, artifacts: SemanticAnalysisArtifacts) -> Self {
        Self {
            ast: artifacts.ast,
            symbols: artifacts.symbols,
            prelude: String::new(),
        }
    }

    fn preprocess_ast(&mut self) -> Result<(), CodeGenError> {
        let mut effective_name_updates = HashMap::new();

        let top_level_models = self.ast.get_children_fn(self.ast.get_root(), |n| n.kind == AstNodeKind::Model).unwrap_or_default();
        let mut stack = top_level_models.iter().map(|n| n.id).collect::<Vec<_>>();
        while let Some(curr_node_id) = stack.pop() {
            let Some(curr_node) = self.ast.get_node(curr_node_id) else {
                break;
            };
            let Some(Model {
                name: model_name, effective_name, ..
            }) = &curr_node.as_model()
            else {
                break;
            };
            let curr_model_name = effective_name.clone().unwrap_or(model_name.to_string());
            for child in self.ast.get_children(curr_node.id).unwrap_or_default() {
                match child.payload {
                    AstNodePayload::Model(Model { name, effective_name, .. }) => {
                        let new_name = RustSerdeCodeGenerator::name_to_title_case(&format!("{}{}", curr_model_name, effective_name.clone().unwrap_or(name.to_string()).as_str()));
                        effective_name_updates.insert(child.id, new_name);
                        stack.push(child.id);
                    }
                    AstNodePayload::Enum(Enum { name, effective_name, .. }) => {
                        let new_name = RustSerdeCodeGenerator::name_to_title_case(&format!("{}{}", curr_model_name, effective_name.clone().unwrap_or(name.to_string()).as_str()));
                        effective_name_updates.insert(child.id, new_name);
                    }
                    _ => continue,
                }
            }
        }

        for (node_id, new_name) in effective_name_updates {
            self.ast.update_node(node_id, |node| {
                if let AstNodePayload::Model(Model { effective_name, .. }) = &mut node.payload {
                    *effective_name = Some(new_name);
                } else if let AstNodePayload::Enum(Enum { effective_name, .. }) = &mut node.payload {
                    *effective_name = Some(new_name);
                }
            });
        }

        Ok(())
    }

    fn emit_model(&mut self, model_node: &AstNode) -> EmitResult {
        let AstNodePayload::Model(Model { name, effective_name, fields, .. }) = &model_node.payload else {
            return Err(CodeGenError::Other(format!("Node with ID {:?} is not a Model", model_node.id)));
        };
        let name = effective_name.clone().unwrap_or(name.to_string());

        let mut result = String::new();

        result.push_str("#[derive(Serialize, Deserialize, Debug, Clone)]\n");
        result.push_str(&format!("pub struct {} {{\n", name));
        for (field_name, field_node_id) in fields {
            let field_node = self
                .ast
                .get_node(*field_node_id)
                .ok_or(CodeGenError::Other(format!("Failed to get field node with ID for field '{}'", field_name)))?;
            let field_code = self.emit_field(&field_node)?;
            result.push_str(&field_code);
        }
        result.push_str("}\n");

        let nested_idts = self
            .ast
            .get_children(model_node.id)
            .unwrap_or_default()
            .into_iter()
            .filter(|n| matches!(n.kind, AstNodeKind::Model | AstNodeKind::Enum))
            .collect::<Vec<_>>();
        for nested_idt in nested_idts {
            match nested_idt.kind {
                AstNodeKind::Model => result.push_str(&self.emit_model(&nested_idt)?),
                AstNodeKind::Enum => result.push_str(&self.emit_enum(&nested_idt)?),
                _ => {}
            }
            result.push('\n');
        }

        Ok(result)
    }

    fn emit_enum(&self, enum_node: &AstNode) -> EmitResult {
        let Some(Enum { name, variants, doc, .. }) = &enum_node.as_enum() else {
            return Err(CodeGenError::Other(format!("Node with ID {:?} is not an Enum", enum_node.id)));
        };

        let mut result = String::new();

        if let Some(doc) = doc {
            for line in doc.lines() {
                result.push_str(&format!("/// {}\n", line.trim()));
            }
        }

        result.push_str("#[derive(Serialize, Deserialize, Debug, Clone)]\n");
        result.push_str(&format!("pub enum {} {{\n", name));
        for variant in variants {
            result.push_str(&format!("    {},\n", RustSerdeCodeGenerator::name_to_title_case(variant)));
        }
        result.push_str("}\n");

        Ok(result)
    }

    fn emit_field(&mut self, field_node: &AstNode) -> EmitResult {
        let mut result = String::new();

        let Some(Field { name, ty, doc, default, .. }) = &field_node.as_field() else {
            return Err(CodeGenError::Other(format!("Node with ID {:?} is not a Field", field_node.id)));
        };

        if let Some(doc) = doc {
            for line in doc.lines() {
                result.push_str(&format!("    /// {}\n", line.trim()));
            }
        }

        if default.is_some() {
            let default_func_name = Some(format!("default_{}", name));
            result.push_str(&format!("#[serde(default = \"{}\")]\n", default_func_name.as_ref().unwrap()));

            let type_str = self.emit_type(field_node.id, ty)?;
            result.push_str(&format!("    pub {}: {},\n", name, type_str));

            if let Some(default_func_name) = default_func_name {
                let mut value_str = String::new();
                if let Type::Single(TypeAtom {
                    variant: TypeVariant::Ref { name: ref_name, .. },
                    ..
                }) = ty
                {
                    // Ref - check if enum
                    let resolved_ref = self.resolve_ref(field_node.id, ref_name);
                    if let Some(AstNodePayload::Enum { .. }) = &resolved_ref.map(|n| n.payload) {
                        let ConstantValue::String(enum_value) = default.as_ref().ok_or(CodeGenError::Other(format!(
                            "Enum field '{}' must have a string default value corresponding to one of its variants",
                            name
                        )))?
                        else {
                            return Err(CodeGenError::Other(format!(
                                "Enum field '{}' must have a string default value corresponding to one of its variants",
                                name
                            )));
                        };
                        value_str.push_str(&format!("{}::{}", type_str, RustSerdeCodeGenerator::name_to_title_case(enum_value)));
                    }
                } else {
                    match &default {
                        Some(ConstantValue::String(s)) => {
                            value_str.push_str(&format!("\"{}\".to_string()", s));
                        }
                        Some(ConstantValue::Int(i)) => {
                            value_str.push_str(&i.to_string());
                        }
                        Some(ConstantValue::Bool(b)) => {
                            value_str.push_str(&b.to_string());
                        }
                        _ => {}
                    }
                }

                let mut default_value_func = String::new();
                // Emit a function that returns the default enum value
                default_value_func.push_str(&format!("fn {}() -> {} {{\n", default_func_name, type_str));
                default_value_func.push_str(&format!("  {}\n", value_str));
                default_value_func.push_str("}\n\n");
                self.prelude.push_str(&default_value_func);
            }
        }

        Ok(result)
    }

    fn emit_type(&self, field_node_id: AstNodeId, ty: &Type) -> EmitResult {
        let Type::Single(atom) = ty else {
            // TODO: Support union types
            return Ok("".to_string());
        };

        let mut result = String::new();

        let ty = match atom.variant {
            TypeVariant::Primitive(p) => match p {
                PrimitiveType::String => Ok("String".to_string()),
                PrimitiveType::Int => Ok("i64".to_string()),
                PrimitiveType::Bool => Ok("bool".to_string()),
            },
            TypeVariant::AnonymousModel => {
                return Err(CodeGenError::Other("Anonymous models are currently not supported in Rust Serde generation".to_string()));
            }
            TypeVariant::Ref { ref name, .. } => {
                if let Some(ref_node) = self.resolve_ref(field_node_id, name) {
                    match ref_node.kind {
                        AstNodeKind::Model => return Ok(name.clone()),
                        AstNodeKind::Enum => return Ok(name.clone()),
                        _ => return Err(CodeGenError::Other(format!("Referenced type '{}' is not a Model or Enum", name))),
                    };
                }
                return Err(CodeGenError::Other(format!("Failed to resolve reference to type '{}'", &name)));
            }
        }?;
        result.push_str(&ty);

        if atom.is_optional {
            result = format!("Option<{}>", result);
        }
        if atom.is_array {
            result = format!("Vec<{}>", result);
        }

        Ok(result)
    }

    fn resolve_ref(&self, scope: AstNodeId, ref_name: &str) -> Option<AstNode> {
        let ref_symbol = self.symbols.lookup(&self.ast, scope, ref_name)?;
        let ast_node_for_symbol = self.ast.get_node(ref_symbol.id)?;
        Some(ast_node_for_symbol.clone())
    }

    fn name_to_title_case(name: &str) -> String {
        let mut c = name.chars();
        match c.next() {
            None => String::new(),
            Some(f) => f.to_uppercase().collect::<String>() + c.as_str(),
        }
    }
}
