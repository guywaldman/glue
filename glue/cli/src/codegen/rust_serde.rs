use std::collections::{HashMap, HashSet};

use gluelang::{
    Ast, AstNode, AstNodeId, AstNodeKind, AstNodePayload, ConstantValue, Enum, Field, Model, PrimitiveType, SemanticAnalysisArtifacts, SymbolTable, Type, TypeAtom, TypeRef,
    TypeVariant,
};
use log::debug;

use crate::codegen::{CodeGenError, CodeGenerator, GlueConfigSchema, types::EmitResult};

pub struct RustSerdeCodeGenerator {
    ast: Ast,
    symbols: SymbolTable,
    helper_functions: HashMap<String, String>,
    helper_order: Vec<String>,
    seen_helpers: HashSet<String>,
}

impl CodeGenerator for RustSerdeCodeGenerator {
    fn generate(mut self) -> EmitResult {
        self.preprocess_ast()?;

        let mut body = String::new();

        let top_level_nodes = self
            .ast
            .get_children(self.ast.get_root())
            .ok_or(CodeGenError::Other("Failed to get top-level nodes".to_string()))?;

        for node in top_level_nodes {
            if node.kind == AstNodeKind::Model {
                let model_code = self.emit_model(&node, &[])?;
                body.push_str(&model_code);
                body.push('\n');
            }
        }

        let mut result = String::new();
        result.push_str("#![allow(unused_imports)]\n");
        result.push_str("#![allow(dead_code)]\n\n");

        for name in &self.helper_order {
            if let Some(function) = self.helper_functions.get(name) {
                result.push_str(function);
            }
        }

        if !self.helper_order.is_empty() {
            result.push('\n');
        }

        result.push_str("use serde::{Deserialize, Serialize};\n");
        result.push_str("use serde_json::ser;\n\n");

        result.push_str(&body);

        Ok(result)
    }
}

impl RustSerdeCodeGenerator {
    pub fn new(_config: GlueConfigSchema, artifacts: SemanticAnalysisArtifacts) -> Self {
        Self {
            ast: artifacts.ast,
            symbols: artifacts.symbols,
            helper_functions: HashMap::new(),
            helper_order: Vec::new(),
            seen_helpers: HashSet::new(),
        }
    }

    fn preprocess_ast(&mut self) -> Result<(), CodeGenError> {
        let top_level_models = self.ast.get_children_fn(self.ast.get_root(), |n| n.kind == AstNodeKind::Model).unwrap_or_default();
        for model in top_level_models {
            self.apply_prefixes(model.id, None)?;
        }
        Ok(())
    }

    fn apply_prefixes(&mut self, node_id: AstNodeId, parent_prefix: Option<String>) -> Result<(), CodeGenError> {
        let node = self.ast.get_node(node_id).ok_or(CodeGenError::Other(format!("Failed to get node with ID {node_id:?}")))?;

        match node.payload {
            AstNodePayload::Model(ref model) => {
                let base_name = model.effective_name.clone().unwrap_or(model.name.clone());
                let full_name = if let Some(ref prefix) = parent_prefix {
                    format!("{}{}", prefix, base_name)
                } else {
                    base_name.clone()
                };

                let full_name = RustSerdeCodeGenerator::name_to_title_case(&full_name);

                self.ast.update_node(node_id, |n| {
                    if let AstNodePayload::Model(Model { effective_name, .. }) = &mut n.payload {
                        *effective_name = Some(full_name.clone());
                    }
                });

                let children = self.ast.get_children(node_id).unwrap_or_default();
                for child in &children {
                    if matches!(child.payload, AstNodePayload::Model(_) | AstNodePayload::Enum(_)) {
                        self.apply_prefixes(child.id, Some(full_name.clone()))?;
                    }
                }
                for child in &children {
                    if matches!(child.payload, AstNodePayload::Field(_)) {
                        self.update_field_reference_name(child.id)?;
                    }
                }
            }
            AstNodePayload::Enum(ref enm) => {
                let base_name = enm.effective_name.clone().unwrap_or(enm.name.clone());
                let full_name = if let Some(ref prefix) = parent_prefix {
                    format!("{}{}", prefix, base_name)
                } else {
                    base_name.clone()
                };
                let full_name = RustSerdeCodeGenerator::name_to_title_case(&full_name);

                self.ast.update_node(node_id, |n| {
                    if let AstNodePayload::Enum(Enum { effective_name, .. }) = &mut n.payload {
                        *effective_name = Some(full_name.clone());
                    }
                });
            }
            _ => {}
        }

        Ok(())
    }

    fn update_field_reference_name(&mut self, field_node_id: AstNodeId) -> Result<(), CodeGenError> {
        let Some(field_node) = self.ast.get_node(field_node_id) else {
            return Err(CodeGenError::Other(format!("Failed to get field node with ID {field_node_id:?}")));
        };

        let AstNodePayload::Field(field) = &field_node.payload else {
            return Ok(());
        };

        let Some(TypeRef { name: ref_name, .. }) = field.as_ref() else {
            return Ok(());
        };

        let Some(resolved_ref) = self.symbols.resolve_ref(&self.ast, field_node_id, ref_name) else {
            return Ok(());
        };

        let new_name = if let Some(model) = resolved_ref.as_model() {
            model.effective_name.clone().unwrap_or(model.name.clone())
        } else if let Some(enm) = resolved_ref.as_enum() {
            enm.effective_name.clone().unwrap_or(enm.name.clone())
        } else {
            return Ok(());
        };

        self.ast.update_node(field_node_id, |node| {
            if let AstNodePayload::Field(Field {
                ty: Type::Single(TypeAtom {
                    variant: TypeVariant::Ref(TypeRef { effective_name, .. }),
                    ..
                }),
                ..
            }) = &mut node.payload
            {
                *effective_name = new_name.clone();
            }
        });

        Ok(())
    }

    fn emit_model(&mut self, model_node: &AstNode, path: &[String]) -> EmitResult {
        let Some(model) = model_node.as_model() else {
            return Err(Box::new(CodeGenError::Other(format!("Node with ID {:?} is not a Model", model_node.id))));
        };
        let struct_name = model.effective_name.clone().unwrap_or(model.name.clone());
        let mut current_path = path.to_vec();
        current_path.push(model.name.clone());

        let mut result = String::new();

        debug!("Checking model '{}' for all fields having defaults", struct_name);
        let all_fields_have_defaults = model.all_fields_have_defaults(&self.ast, &self.symbols);

        result.push_str("#[derive(Debug, Clone, Serialize, Deserialize)]\n");
        result.push_str(&format!("pub struct {} {{\n", struct_name));
        for field_node_id in model.fields.values() {
            let field_node = self
                .ast
                .get_node(*field_node_id)
                .ok_or(CodeGenError::Other("Failed to get field node for model field".to_string()))?;
            let field_code = self.emit_field(&field_node, &current_path)?;
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
                AstNodeKind::Model => result.push_str(&self.emit_model(&nested_idt, &current_path)?),
                AstNodeKind::Enum => result.push_str(&self.emit_enum(&nested_idt)?),
                _ => {}
            }
            result.push('\n');
        }

        if all_fields_have_defaults {
            result.push_str(&format!("impl Default for {} {{\n", struct_name));
            result.push_str("    fn default() -> Self {\n");
            result.push_str("        Self {\n");
            for field_node_id in model.fields.values() {
                let field_node = self
                    .ast
                    .get_node(*field_node_id)
                    .ok_or(CodeGenError::Other("Failed to get field node for model field".to_string()))?;
                let AstNodePayload::Field(field) = &field_node.payload else {
                    return Err(Box::new(CodeGenError::Other(format!("Node with ID {:?} is not a Field", field_node.id))));
                };
                let default_expr = self.default_expr_for_field(field, &current_path)?;
                result.push_str(&format!("            {}: {},\n", field.name, default_expr));
            }
            result.push_str("        }\n");
            result.push_str("    }\n");
            result.push_str("}\n");
        }

        Ok(result)
    }

    fn emit_enum(&self, enum_node: &AstNode) -> EmitResult {
        let Some(Enum {
            effective_name,
            name,
            variants,
            doc,
            ..
        }) = &enum_node.as_enum()
        else {
            return Err(Box::new(CodeGenError::Other(format!("Node with ID {:?} is not an Enum", enum_node.id))));
        };

        let name = effective_name.clone().unwrap_or(name.to_string());

        let mut result = String::new();

        if let Some(doc) = doc {
            for line in doc.lines() {
                result.push_str(&format!("/// {}\n", line.trim()));
            }
        }

        result.push_str("#[derive(Debug, Clone, Serialize, Deserialize)]\n");
        result.push_str(&format!("pub enum {} {{\n", name));
        for variant in variants {
            result.push_str(&format!("    #[serde(rename = \"{}\")]\n", variant));
            result.push_str(&format!("    {},\n", RustSerdeCodeGenerator::name_to_title_case(variant)));
        }
        result.push_str("}\n");

        Ok(result)
    }

    fn emit_field(&mut self, field_node: &AstNode, path: &[String]) -> EmitResult {
        let mut result = String::new();

        let Some(Field { name, ty, doc, default, .. }) = &field_node.as_field() else {
            return Err(Box::new(CodeGenError::Other(format!("Node with ID {:?} is not a Field", field_node.id))));
        };

        if let Some(doc) = doc {
            for line in doc.lines() {
                result.push_str(&format!("    /// {}\n", line.trim()));
            }
        }

        let type_str = self.emit_type(field_node.id, ty)?;

        let mut attrs: Vec<String> = Vec::new();

        if let Some(default_value) = default {
            let default_func_name = RustSerdeCodeGenerator::default_function_name(path, name, default_value);
            attrs.push(format!("#[serde(default = \"{}\")]", default_func_name));

            let value_str = self.field_default_value_literal(field_node, ty, default_value, &type_str, name)?;
            self.ensure_helper_function(&default_func_name, &type_str, &value_str);
        } else if self.field_refs_model_with_defaults(field_node, ty)? {
            attrs.push("#[serde(default)]".to_string());
        }

        for attr in attrs {
            result.push_str("    ");
            result.push_str(&attr);
            result.push('\n');
        }

        result.push_str(&format!("    pub {}: {},\n", name, type_str));

        Ok(result)
    }

    fn emit_type(&self, field_node_id: AstNodeId, ty: &Type) -> EmitResult {
        let Type::Single(atom) = ty else {
            // TODO: Support union types
            return Ok("".to_string());
        };

        let mut result = String::new();

        let ty: Result<String, Box<CodeGenError>> = match atom.variant {
            TypeVariant::Primitive(p) => match p {
                PrimitiveType::String => Ok("String".to_string()),
                PrimitiveType::Int => Ok("i64".to_string()),
                PrimitiveType::Bool => Ok("bool".to_string()),
            },
            TypeVariant::AnonymousModel => {
                return Err(Box::new(CodeGenError::Other(
                    "Anonymous models are currently not supported in Rust Serde generation".to_string(),
                )));
            }
            TypeVariant::Ref(TypeRef { ref name, .. }) => {
                if let Some(ref_node) = self.symbols.resolve_ref(&self.ast, field_node_id, name) {
                    if let Some(Enum { name, effective_name, .. }) = ref_node.as_enum() {
                        let ref_name = effective_name.clone().unwrap_or(name.to_string());
                        return Ok(ref_name);
                    } else if let Some(Model { name, effective_name, .. }) = ref_node.as_model() {
                        let ref_name = effective_name.clone().unwrap_or(name.to_string());
                        return Ok(ref_name);
                    } else {
                        return Err(Box::new(CodeGenError::Other(format!("Referenced type '{}' is neither a Model nor an Enum", name))));
                    }
                }
                return Err(Box::new(CodeGenError::Other(format!("Failed to resolve reference to type '{}'", &name))));
            }
        };
        result.push_str(ty?.as_str());

        if atom.is_optional {
            result = format!("Option<{}>", result);
        }
        if atom.is_array {
            result = format!("Vec<{}>", result);
        }

        Ok(result)
    }

    fn name_to_title_case(name: &str) -> String {
        let mut c = name.chars();
        match c.next() {
            None => String::new(),
            Some(f) => f.to_uppercase().collect::<String>() + c.as_str(),
        }
    }

    fn default_function_name(path: &[String], field_name: &str, default: &ConstantValue) -> String {
        if let ConstantValue::Bool(value) = default {
            return if *value { "default_true".to_string() } else { "default_false".to_string() };
        }

        let mut segments = path.to_vec();
        segments.push(field_name.to_string());
        let snake_segments: Vec<String> = segments.iter().map(|s| RustSerdeCodeGenerator::to_snake_case(s)).collect();
        format!("{}_default", snake_segments.join("_"))
    }

    fn field_default_value_literal(&self, field_node: &AstNode, ty: &Type, default: &ConstantValue, type_str: &str, field_name: &str) -> Result<String, CodeGenError> {
        if let Type::Single(TypeAtom {
            variant: TypeVariant::Ref(TypeRef { name: ref_name, .. }),
            ..
        }) = ty
            && let Some(resolved_ref) = self.symbols.resolve_ref(&self.ast, field_node.id, ref_name)
            && resolved_ref.as_enum().is_some()
        {
            let ConstantValue::String(enum_value) = default else {
                return Err(CodeGenError::Other(format!(
                    "Enum field '{}' must have a string default value corresponding to one of its variants",
                    field_name
                )));
            };

            return Ok(format!("{}::{}", type_str, RustSerdeCodeGenerator::name_to_title_case(enum_value)));
        }

        match default {
            ConstantValue::String(s) => Ok(format!("\"{}\".to_string()", s)),
            ConstantValue::Int(i) => Ok(i.to_string()),
            ConstantValue::Bool(b) => Ok(b.to_string()),
            ConstantValue::IntRange(_, _) => Err(CodeGenError::Other(format!("Range defaults are not supported for field '{}'", field_name))),
        }
    }

    fn ensure_helper_function(&mut self, name: &str, return_type: &str, body_expr: &str) {
        if self.seen_helpers.contains(name) {
            return;
        }

        let mut function = String::new();
        function.push_str(&format!("fn {}() -> {} {{\n", name, return_type));
        function.push_str(&format!("    {}\n", body_expr));
        function.push_str("}\n\n");

        self.helper_functions.insert(name.to_string(), function);
        self.helper_order.push(name.to_string());
        self.seen_helpers.insert(name.to_string());
    }

    fn field_refs_model_with_defaults(&self, field_node: &AstNode, ty: &Type) -> Result<bool, CodeGenError> {
        let Type::Single(type_atom) = ty else {
            return Ok(false);
        };

        if type_atom.is_optional {
            return Ok(false);
        }

        let TypeVariant::Ref(TypeRef { ref name, .. }) = type_atom.variant else {
            return Ok(false);
        };

        let Some(resolved_ref) = self.symbols.resolve_ref(&self.ast, field_node.id, name) else {
            return Ok(false);
        };

        if let Some(model) = resolved_ref.as_model() {
            return Ok(model.all_fields_have_defaults(&self.ast, &self.symbols));
        }

        Ok(false)
    }

    fn default_expr_for_field(&self, field: &Field, path: &[String]) -> Result<String, CodeGenError> {
        if let Some(default_value) = field.default.as_ref() {
            let func_name = RustSerdeCodeGenerator::default_function_name(path, &field.name, default_value);
            return Ok(format!("{}()", func_name));
        }

        if field.as_ref().is_some() {
            return Ok("Default::default()".to_string());
        }

        Err(CodeGenError::Other(format!("Field '{}' is missing a default value", field.name)))
    }

    fn to_snake_case(name: &str) -> String {
        let mut snake = String::new();
        for (i, ch) in name.chars().enumerate() {
            if ch.is_uppercase() {
                if i != 0 {
                    snake.push('_');
                }
                for lower in ch.to_lowercase() {
                    snake.push(lower);
                }
            } else {
                snake.push(ch);
            }
        }
        snake
    }
}
