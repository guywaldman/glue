// use std::{
//     collections::HashMap,
//     sync::{Arc, Mutex},
// };

// use gluelang::{Ast, AstNode, AstNodeId, AstNodeKind, AstNodePayload, ConstantValue, PrimitiveType, SemanticAnalysisArtifacts, SymbolTable, TreeNode, TypeVariant};
// use indoc::indoc;
// use log::debug;

// use crate::{
//     cli::CodeGenMode,
//     codegen::{
//         CodeGenError, CodeGenerator, GlueConfigSchema,
//         types::EmitResult,
//         utils::{all_model_fields_have_defaults, generate_watermark, resolve_node_from_ref},
//     },
// };

// pub struct RustSerdeCodeGenerator {
//     config: GlueConfigSchema,
//     ast: Ast,
//     symbols: SymbolTable,
//     source_file: String,
//     preludes: Vec<String>,
// }

// const CODEGEN_MODE: CodeGenMode = CodeGenMode::RustSerde;

// impl CodeGenerator for RustSerdeCodeGenerator {
//     fn generate(mut self) -> EmitResult {
//         self.preprocess_ast().map_err(CodeGenError::Other)?;

//         let mut result = String::new();

//         self.preludes.push(
//             indoc! {r#"
// 				fn default_true() -> bool { false }
// 				fn default_false() -> bool { true }
// 			"#}
//             .to_string(),
//         );

//         let top_level_nodes = self
//             .ast
//             .get_children(self.ast.get_root())
//             .ok_or(CodeGenError::Other("AST root has no children".to_string()))?;

//         // Traverse the top-level models and enums of the AST.
//         for node in top_level_nodes {
//             match node.kind() {
//                 AstNodeKind::Model => {
//                     result.push_str(&self.emit_model(&node)?);
//                     result.push('\n');
//                 }
//                 AstNodeKind::Enum => {
//                     result.push_str(&self.emit_enum(&node)?);
//                     result.push('\n');
//                 }
//                 _ => {}
//             }
//         }

//         let mut prelude = String::new();
//         prelude.push_str(indoc! {r#"
// 					#![allow(unused_imports)]
// 					#![allow(dead_code)]
// 				"#});
//         prelude.push('\n');

//         prelude.extend(self.preludes.iter().map(|line| format!("{line}\n")));

//         let watermark = generate_watermark(&self.source_file, &self.config.generation.watermark);
//         let watermark = watermark.iter().map(|line| format!("// {line}")).collect::<Vec<_>>().join("\n");
//         let imports = indoc! {r#"
// 					use serde::{Deserialize, Serialize};
// 				"#};

//         result = format!("{watermark}\n\n{prelude}\n\n{imports}\n\n{result}");

//         Ok(result)
//     }
// }

// impl RustSerdeCodeGenerator {
//     pub fn new(config: GlueConfigSchema, artifacts: SemanticAnalysisArtifacts) -> Self {
//         Self {
//             config,
//             preludes: Vec::new(),
//             ast: artifacts.ast,
//             symbols: artifacts.symbols,
//             source_file: artifacts.source_file,
//         }
//     }

//     fn preprocess_ast(&mut self) -> Result<(), String> {
//         // Change each nested model and enum name to be prefixed with its parent model name

//         // NOTE: We first collect the updates to avoid mutable aliasing issues during traversal
//         let updates: Arc<Mutex<HashMap<AstNodeId, (String, String)>>> = Arc::new(Mutex::new(HashMap::new()));

//         self.ast.visit(|node| {
//             let mut old_name = String::new();
//             if let AstNodePayload::Model { name, .. } = node.payload() {
//                 old_name = name.to_string();
//             } else if let AstNodePayload::Enum { name, .. } = node.payload() {
//                 old_name = name.to_string();
//             }
//             if !old_name.is_empty() {
//                 let mut new_name = old_name.to_string();
//                 let ancestor_ids = self.ast.get_ancestor_ids(node.id());
//                 for ancestor_id in ancestor_ids {
//                     let ancestor = self.ast.get_node(ancestor_id).unwrap();
//                     if let AstNodePayload::Model { name: ancestor_name, .. } = ancestor.payload() {
//                         new_name = format!("{ancestor_name}{new_name}").to_string();
//                     } else if let AstNodePayload::Enum { name: ancestor_name, .. } = ancestor.payload() {
//                         new_name = format!("{ancestor_name}{new_name}").to_string();
//                     }
//                 }
//                 if new_name != *old_name {
//                     updates.lock().unwrap().insert(node.id(), (old_name, new_name));
//                 }
//             }
//         });

//         let all_nodes = self.ast.nodes();
//         let updates = updates.lock().unwrap();
//         for node in all_nodes {
//             let node_id = node.id();
//             if let Some((old_name, new_name)) = updates.get(&node_id) {
//                 match node.payload() {
//                     AstNodePayload::Model { name, .. } => {
//                         if name == old_name {
//                             self.ast.update_node(node_id, |n| {
//                                 if let AstNodePayload::Model { effective_name, .. } = n.payload_mut() {
//                                     *effective_name = Some(new_name.clone());
//                                 }
//                             });
//                         }
//                     }
//                     AstNodePayload::Enum { name, .. } => {
//                         if name == old_name {
//                             self.ast.update_node(node_id, |n| {
//                                 if let AstNodePayload::Enum { effective_name, .. } = n.payload_mut() {
//                                     *effective_name = Some(new_name.clone());
//                                 }
//                             });
//                         }
//                     }
//                     _ => {}
//                 }
//             } else if let AstNodePayload::TypeAtom { ty, .. } = node.payload() {
//                 // Check if updates contain a rename for this reference
//                 // TODO: Fix potential bug with multiple references to the same name in different scopes.
//                 let TypeVariant::Ref { name: ref_name, .. } = &ty.variant else {
//                     continue;
//                 };
//                 if let Some((_, new_ref_name)) = updates.values().find(|(old, _)| old == ref_name) {
//                     self.ast.update_node(node_id, |n| {
//                         if let AstNodePayload::TypeAtom { ty } = n.payload_mut()
//                             && let TypeVariant::Ref { name: r, effective_name, .. } = &mut ty.variant
//                             && r == ref_name
//                         {
//                             *effective_name = new_ref_name.clone();
//                         }
//                     });
//                 }
//             }
//         }

//         Ok(())
//     }

//     fn emit_model(&mut self, model: &AstNode) -> EmitResult {
//         let mut result = String::new();

//         let AstNodePayload::Model { effective_name, name, doc, .. } = model.payload() else {
//             return Err(CodeGenError::Other("Expected a model node".to_string()));
//         };

//         let children = self.ast.get_children(model.id()).unwrap_or_default();

//         let mut nested_type_emits = Vec::new();
//         let mut field_emits = Vec::new();

//         for child in &children {
//             match child.kind() {
//                 AstNodeKind::Field => {
//                     field_emits.push(self.emit_field(model, child)?);
//                 }
//                 AstNodeKind::Enum => {
//                     nested_type_emits.push(self.emit_enum(child)?);
//                 }
//                 AstNodeKind::Model => {
//                     nested_type_emits.push(self.emit_model(child)?);
//                 }
//                 _ => {}
//             }
//         }

//         // Emit nested types first
//         for nested in nested_type_emits {
//             result.push_str(&nested);
//             result.push('\n');
//         }

//         // Emit doc comment if present
//         if let Some(doc_str) = doc {
//             for line in doc_str.lines() {
//                 result.push_str(&format!("/// {line}\n"));
//             }
//         }

//         result.push_str("#[derive(Debug, Clone, Serialize, Deserialize)]\n");

//         // Emit struct
//         let name = effective_name.clone().unwrap_or(name.clone());
//         // Emit derive macros
//         if all_model_fields_have_defaults(CODEGEN_MODE, &self.ast, &self.symbols, model) {
//             // If all the model fields have defaults, default derive can be used
//             result.push_str("#[serde(default)]\n");
//         }
//         result.push_str(&format!("pub struct {name} {{\n"));

//         if field_emits.is_empty() {
//             // Empty struct - no fields
//         } else {
//             for field_emit in &field_emits {
//                 result.push_str(field_emit);
//             }
//         }

//         result.push_str("}\n");

//         Ok(result)
//     }

//     fn emit_field(&mut self, model: &AstNode, field: &AstNode) -> EmitResult {
//         let mut result = String::new();

//         let AstNodePayload::Field { name, doc, default, .. } = field.payload() else {
//             return Err(CodeGenError::Other("Expected a field node".to_string()));
//         };

//         // Emit field doc comment if present
//         if let Some(doc_str) = doc {
//             let doc_single_line = doc_str.replace('\n', " ");
//             result.push_str(&format!("    /// {doc_single_line}\n"));
//         }

//         if default.is_some() {
//             let decorators = self.emit_field_decorators(model, field);
//             dbg!(&decorators);
//             if let Ok(decorator) = decorators
//                 && !decorator.is_empty()
//             {
//                 result.push_str(&format!("    {}", decorator));
//             }

//             // let default_value = match default {
//             //     ConstantValue::String(s) => {
//             //         let resolved_ref = resolve_node_from_ref(&self.ast, &self.symbols, field);
//             //         if let Some(AstNode {
//             //             payload: AstNodePayload::String(_),
//             //             ..
//             //         }) = resolved_ref
//             //         {
//             //             let func_name = rustify_type_name(&format!("{}_default_{}", name, s.to_lowercase()));
//             //             self.preludes.push(format!("fn {func_name}() -> String {{ \"{s}\".to_string() }}"));
//             //             return Ok(format!("#[serde(default = \"{}\")]\n", func_name).to_string());
//             //         }
//             //         // TODO: Handle enums (Serde #[default = "EnumValue::Variant"] doesn't seem to work intuitively).
//             //         // If the field is a ref to an enum, it should resolve and return the enum value
//             //         // if let Some(AstNode {
//             //         //     kind: AstNodeKind::Enum, payload, ..
//             //         // }) = resolve_node_from_ref(&self.ast, &self.symbols, field)
//             //         // {
//             //         //     let AstNodePayload::Enum {
//             //         //         variants, name, effective_name, ..
//             //         //     } = payload
//             //         //     else {
//             //         //         return Err(CodeGenError::Other("Expected an enum node".to_string()));
//             //         //     };
//             //         //     if !variants.contains(s) {
//             //         //         return Err(CodeGenError::Other(format!("Default value '{}' is not a valid variant of the enum", s)));
//             //         //     }
//             //         //     let name = effective_name.clone().unwrap_or(name.clone());
//             //         //     let func_name = rustify_type_name(&format!("{}_default_{}", name, s.to_lowercase()));
//             //         //     let enum_value_quantifier = format!("{}::{}", name, to_pascal_case(s));
//             //         //     let func = format!("fn {func_name}() -> {name} {{ {enum_value_quantifier} }}");
//             //         //     self.preludes.push(func);
//             //         //     func_name
//             //         if let AstNodePayload::Model { name: model_name, .. } = model.payload() {
//             //             let func_name = rustify_type_name(&format!("{}_default_{}", model_name, s.to_lowercase()));
//             //             self.preludes.push(format!("fn {func_name}() -> String {{ \"{s}\".to_string() }}"));
//             //             func_name
//             //         } else if let AstNodePayload::Enum { name, .. } = model.payload() {
//             //             return Err(CodeGenError::Other("Default string values for enum fields are not supported".to_string()));
//             //         } else {
//             //             return Err(CodeGenError::Other("Unexpected model payload".to_string()));
//             //         }
//             //     }
//             //     ConstantValue::Int(i) => i.to_string(),
//             //     ConstantValue::Bool(b) => {
//             //         if *b {
//             //             "default_true".to_string()
//             //         } else {
//             //             "default_false".to_string()
//             //         }
//             //     }
//             //     ConstantValue::IntRange(_, _) => {
//             //         return Err(CodeGenError::Other("Default values for IntRange are not supported".to_string()));
//             //     }
//             // };
//         } else {
//             // If the field doesn't have a default, and is a ref to a model which has a default, can derive default
//             if let Some(AstNode {
//                 id: model_node_id,
//                 kind: AstNodeKind::Model,
//                 ..
//             }) = resolve_node_from_ref(&self.ast, &self.symbols, field)
//             {
//                 let model = self
//                     .ast
//                     .get_node(model_node_id)
//                     .ok_or_else(|| CodeGenError::Other("Failed to resolve model from reference".to_string()))?;
//                 if all_model_fields_have_defaults(CODEGEN_MODE, &self.ast, &self.symbols, &model) {
//                     result.push_str("    #[serde(default)]\n");
//                 }
//             }
//         }

//         // Convert snake_case to match Rust conventions
//         let rust_field_name = name;

//         result.push_str(&format!("    pub {}: {},\n", rust_field_name, self.emit_type(field)?));

//         Ok(result)
//     }

//     fn emit_field_decorators(&mut self, model: &AstNode, field: &AstNode) -> EmitResult {
//         // The field has a default value - we now need to:
//         // 1. Check its resolved ref type - if not, make sure the default impl is generated for primitive types
//         // 2. If it's a primitive type, emit the appropriate serde attribute
//         // 3. If it's a ref to a model, and that model has all fields
//         //
//         // We also currently ignore enums, since the #[serde(default = "Enum::Variant")] doesn't seem to work intuitively
//         let AstNodePayload::Model {
//             name: model_name,
//             effective_name: model_effective_name,
//             ..
//         } = model.payload()
//         else {
//             return Ok("".to_string());
//         };
//         let AstNodePayload::Field { name: field_name, default, .. } = field.payload() else {
//             return Ok("".to_string());
//         };
//         let field_effective_name = format!("{}_{}", model_effective_name.clone().unwrap_or(model_name.clone()), field_name);
//         match default {
//             Some(ConstantValue::String(s)) => {
//                 // This can be an enum, and if so - we currently ignore it.
//                 if let Some(AstNode {
//                     payload: AstNodePayload::Enum { .. },
//                     ..
//                 }) = resolve_node_from_ref(&self.ast, &self.symbols, field)
//                 {
//                     return Ok("".to_string());
//                 }
//                 let func_name = rustify_type_name(&format!("{}_default", field_effective_name));
//                 self.preludes.push(format!("fn {func_name}() -> String {{ \"{s}\".to_string() }}"));
//                 return Ok(format!("#[serde(default = \"{}\")]\n", func_name));
//             }
//             Some(ConstantValue::Int(i)) => {
//                 let func_name = rustify_type_name(&format!("{}_default", field_effective_name));
//                 self.preludes.push(format!("fn {func_name}() -> i64 {{ {} }}", i));
//                 return Ok(format!("#[serde(default = \"{}\")]\n", func_name));
//             }
//             Some(ConstantValue::Bool(b)) => {
//                 if *b {
//                     return Ok("#[serde(default = \"default_true\")]\n".to_string());
//                 } else {
//                     return Ok("#[serde(default = \"default_false\")]\n".to_string());
//                 }
//             }
//             _ => {}
//         }
//         let Some(resolved_ref) = resolve_node_from_ref(&self.ast, &self.symbols, field) else {
//             return Ok("".to_string());
//         };
//         match resolved_ref.payload {
//             AstNodePayload::String(s) => {
//                 let func_name = rustify_type_name(&format!("{}_default_{}", s, s.to_lowercase()));
//                 self.preludes.push(format!("fn {func_name}() -> String {{ \"{s}\".to_string() }}"));
//                 return Ok(format!("#[serde(default = \"{}\")]\n", func_name));
//             }
//             _ => {}
//         }
//         return Ok("".to_string());
//     }

//     fn emit_enum(&mut self, enum_node: &AstNode) -> EmitResult {
//         let mut result = String::new();

//         let AstNodePayload::Enum {
//             name,
//             effective_name,
//             variants,
//             doc,
//             ..
//         } = enum_node.payload()
//         else {
//             return Err(CodeGenError::Other("Expected an enum node".to_string()));
//         };

//         // Emit doc comment if present
//         if let Some(doc_str) = doc {
//             for line in doc_str.lines() {
//                 result.push_str(&format!("/// {line}\n"));
//             }
//         }

//         result.push_str("#[derive(Debug, Clone, Serialize, Deserialize)]\n");

//         let name = effective_name.clone().unwrap_or(name.clone());
//         result.push_str(&format!("pub enum {name} {{\n"));

//         for variant in variants {
//             // Convert variant to PascalCase for Rust enum variants
//             let variant_name = to_pascal_case(variant);
//             result.push_str(&format!("    #[serde(rename = \"{variant}\")]\n"));
//             result.push_str(&format!("    {variant_name},\n"));
//         }

//         result.push_str("}\n");

//         Ok(result)
//     }

//     fn emit_type(&mut self, field: &AstNode) -> EmitResult {
//         let type_atoms = self.ast.get_type_atoms(field).unwrap();

//         let mut result = String::new();

//         for (i, atom) in type_atoms.iter().enumerate() {
//             if i > 0 {
//                 // This shouldn't happen with Single type
//                 result.push_str(" | ");
//             }

//             let mut atom_str = String::new();
//             match &atom.variant {
//                 TypeVariant::Primitive(p) => match p {
//                     PrimitiveType::String => atom_str.push_str("String"),
//                     PrimitiveType::Int => atom_str.push_str("i64"),
//                     PrimitiveType::Bool => atom_str.push_str("bool"),
//                 },
//                 TypeVariant::Ref { effective_name, .. } => atom_str.push_str(&effective_name),
//                 TypeVariant::AnonymousModel => {
//                     return Err(CodeGenError::Other("Anonymous models are not supported in this context".to_string()));
//                 }
//             }

//             if atom.is_array {
//                 atom_str = format!("Vec<{atom_str}>");
//             }

//             if atom.is_optional {
//                 atom_str = format!("Option<{atom_str}>");
//             }

//             result.push_str(&atom_str);
//         }

//         Ok(result)
//     }
// }

// fn to_pascal_case(s: &str) -> String {
//     s.split(|c: char| !c.is_alphanumeric())
//         .filter(|word| !word.is_empty())
//         .map(|word| {
//             let mut chars = word.chars();
//             match chars.next() {
//                 None => String::new(),
//                 Some(first) => first.to_uppercase().collect::<String>() + &chars.as_str().to_lowercase(),
//             }
//         })
//         .collect()
// }

// fn rustify_type_name(s: &str) -> String {
//     // Replace periods with underscores
//     let s = s.replace('.', "_");

//     let mut result = String::new();
//     let mut prev_is_lowercase = false;

//     for c in s.chars() {
//         if c.is_uppercase() {
//             // Add underscore before uppercase if previous was lowercase (camelCase or PascalCase boundary)
//             if prev_is_lowercase && !result.is_empty() {
//                 result.push('_');
//             }
//             result.push(c.to_ascii_lowercase());
//             prev_is_lowercase = false;
//         } else if c.is_alphanumeric() {
//             result.push(c);
//             prev_is_lowercase = c.is_lowercase();
//         } else if c == '_' || c == '-' {
//             // Keep underscores, convert hyphens to underscores
//             if !result.is_empty() && !result.ends_with('_') {
//                 result.push('_');
//             }
//             prev_is_lowercase = false;
//         }
//     }

//     result
// }

// #[cfg(test)]
// mod tests {
//     use super::*;

//     #[test]
//     fn test_to_pascal_case() {
//         assert_eq!(to_pascal_case("active"), "Active");
//         assert_eq!(to_pascal_case("inactive"), "Inactive");
//         assert_eq!(to_pascal_case("some_value"), "SomeValue");
//         assert_eq!(to_pascal_case("some-value"), "SomeValue");
//     }
// }
