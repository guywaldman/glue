use gluelang::{Ast, AstNode, AstNodeKind, AstNodePayload, ConstantValue, PrimitiveType, SemanticAnalysisArtifacts, TreeNode, Type, TypeVariant};
use indoc::indoc;

use crate::codegen::{CodeGenError, CodeGenerator, GlueConfigSchema, types::EmitResult, utils::generate_watermark};

pub struct RustSerdeCodeGenerator {
    config: GlueConfigSchema,
    ast: Ast,
    preludes: Vec<String>,
}

impl CodeGenerator for RustSerdeCodeGenerator {
    fn generate(mut self) -> EmitResult {
        let mut result = String::new();

        self.preludes.push(
            indoc! {r#"
				fn default_true() -> bool { false }
				fn default_false() -> bool { true }
			"#}
            .to_string(),
        );

        let top_level_nodes = self
            .ast
            .get_children(self.ast.get_root())
            .ok_or(CodeGenError::Other("AST root has no children".to_string()))?;

        // Traverse the top-level models and enums of the AST.
        for node in top_level_nodes {
            match node.kind() {
                AstNodeKind::Model => {
                    result.push_str(&self.emit_model(&node)?);
                    result.push('\n');
                }
                AstNodeKind::Enum => {
                    result.push_str(&self.emit_enum(&node)?);
                    result.push('\n');
                }
                _ => {}
            }
        }

        let mut prelude = String::new();
        prelude.push_str(indoc! {r#"
					#![allow(unused_imports)]
					#![allow(dead_code)]
				"#});
        prelude.push('\n');

        prelude.extend(self.preludes.iter().cloned());

        let watermark = generate_watermark(self.config.generation.watermark);
        let watermark = watermark.iter().map(|line| format!("// {line}")).collect::<Vec<_>>().join("\n");
        let imports = indoc! {r#"
					use serde::{Deserialize, Serialize};
				"#};

        result = format!("{watermark}\n\n{prelude}\n\n{imports}\n\n{result}");

        Ok(result)
    }
}

impl RustSerdeCodeGenerator {
    pub fn new(config: GlueConfigSchema, artifacts: SemanticAnalysisArtifacts) -> Self {
        Self {
            config,
            preludes: Vec::new(),
            ast: artifacts.ast,
        }
    }

    fn emit_model(&mut self, model: &AstNode) -> EmitResult {
        let mut result = String::new();

        let AstNodePayload::Model { name, doc, .. } = model.payload() else {
            return Err(CodeGenError::Other("Expected a model node".to_string()));
        };

        let children = self.ast.get_children(model.id()).unwrap_or_default();

        let mut nested_type_emits = Vec::new();
        let mut field_emits = Vec::new();

        for child in &children {
            match child.kind() {
                AstNodeKind::Field => {
                    field_emits.push(self.emit_field(model, child)?);
                }
                AstNodeKind::Enum => {
                    nested_type_emits.push(self.emit_enum(child)?);
                }
                AstNodeKind::Model => {
                    nested_type_emits.push(self.emit_model(child)?);
                }
                _ => {}
            }
        }

        // Emit nested types first
        for nested in nested_type_emits {
            result.push_str(&nested);
            result.push('\n');
        }

        // Emit doc comment if present
        if let Some(doc_str) = doc {
            for line in doc_str.lines() {
                result.push_str(&format!("/// {line}\n"));
            }
        }

        // Emit derive macros
        result.push_str("#[derive(Debug, Clone, Default, Serialize, Deserialize)]\n");

        // Emit struct
        result.push_str(&format!("pub struct {name} {{\n"));

        if field_emits.is_empty() {
            // Empty struct - no fields
        } else {
            for field_emit in field_emits {
                result.push_str(&field_emit);
            }
        }

        result.push_str("}\n");

        Ok(result)
    }

    fn emit_field(&mut self, model: &AstNode, field: &AstNode) -> EmitResult {
        let mut result = String::new();

        let AstNodePayload::Field { name, ty, doc, default, .. } = field.payload() else {
            return Err(CodeGenError::Other("Expected a field node".to_string()));
        };

        // Emit field doc comment if present
        if let Some(doc_str) = doc {
            let doc_single_line = doc_str.replace('\n', " ");
            result.push_str(&format!("    /// {doc_single_line}\n"));
        }

        if let Some(default) = default {
            let default_value = match default {
                ConstantValue::String(s) => {
                    let AstNodePayload::Model { name: model_name, .. } = model.payload() else {
                        return Err(CodeGenError::Other("Expected a model node".to_string()));
                    };
                    let func_name = rustify_type_name(&format!("{}_default_{}", model_name, s.to_lowercase()));
                    self.preludes.push(format!("fn {func_name}() -> String {{ \"{s}\".to_string() }}"));
                    func_name
                }
                ConstantValue::Int(i) => i.to_string(),
                ConstantValue::Bool(b) => {
                    if *b {
                        "default_true".to_string()
                    } else {
                        "default_false".to_string()
                    }
                }
            };
            result.push_str(&format!("    #[serde(default = \"{default_value}\")]\n"));
        } else {
            result.push_str("    #[serde(default)]\n");
        }

        // Convert snake_case to match Rust conventions
        let rust_field_name = name;

        result.push_str(&format!("    pub {}: {},\n", rust_field_name, self.emit_type(ty)?));

        Ok(result)
    }

    fn emit_enum(&mut self, enum_node: &AstNode) -> EmitResult {
        let mut result = String::new();

        let AstNodePayload::Enum {
            name, variants, doc, default, ..
        } = enum_node.payload()
        else {
            return Err(CodeGenError::Other("Expected an enum node".to_string()));
        };

        // Emit doc comment if present
        if let Some(doc_str) = doc {
            for line in doc_str.lines() {
                result.push_str(&format!("/// {line}\n"));
            }
        }

        result.push_str("#[derive(Debug, Clone, Default, Serialize, Deserialize)]\n");

        result.push_str(&format!("pub enum {name} {{\n"));

        for variant in variants {
            // Convert variant to PascalCase for Rust enum variants
            let variant_name = to_pascal_case(variant);
            result.push_str(&format!("    #[serde(rename = \"{variant}\")]\n"));
            if let Some(ConstantValue::String(lit)) = default
                && lit == variant
            {
                result.push_str("    #[default]\n");
            }
            result.push_str(&format!("    {variant_name},\n"));
        }

        result.push_str("}\n");

        Ok(result)
    }

    fn emit_type(&mut self, ty: &Type) -> EmitResult {
        let type_atoms = match ty {
            Type::Single(ty) => vec![ty],
            Type::Union(_) => {
                // For unions, we need to generate an enum or use a different approach
                // For now, let's use a string representation that indicates it's a union
                // In a real implementation, you might want to generate a dedicated enum
                return Err(CodeGenError::UnsupportedError(
                    "Union types not yet fully supported in Rust serde generator".to_string(),
                ));
            }
        };

        let mut result = String::new();

        for (i, atom) in type_atoms.iter().enumerate() {
            if i > 0 {
                // This shouldn't happen with Single type
                result.push_str(" | ");
            }

            let mut atom_str = String::new();
            match &atom.variant {
                TypeVariant::Primitive(p) => match p {
                    PrimitiveType::String => atom_str.push_str("String"),
                    PrimitiveType::Int => atom_str.push_str("i64"),
                    PrimitiveType::Bool => atom_str.push_str("bool"),
                },
                TypeVariant::Ref(name) => atom_str.push_str(name),
            }

            if atom.is_array {
                atom_str = format!("Vec<{atom_str}>");
            }

            if atom.is_optional {
                atom_str = format!("Option<{atom_str}>");
            }

            result.push_str(&atom_str);
        }

        Ok(result)
    }
}

fn to_pascal_case(s: &str) -> String {
    s.split(|c: char| !c.is_alphanumeric())
        .filter(|word| !word.is_empty())
        .map(|word| {
            let mut chars = word.chars();
            match chars.next() {
                None => String::new(),
                Some(first) => first.to_uppercase().collect::<String>() + &chars.as_str().to_lowercase(),
            }
        })
        .collect()
}

fn rustify_type_name(s: &str) -> String {
    // Replace periods with underscores
    let s = s.replace('.', "_");

    let mut result = String::new();
    let mut prev_is_lowercase = false;

    for c in s.chars() {
        if c.is_uppercase() {
            // Add underscore before uppercase if previous was lowercase (camelCase or PascalCase boundary)
            if prev_is_lowercase && !result.is_empty() {
                result.push('_');
            }
            result.push(c.to_ascii_lowercase());
            prev_is_lowercase = false;
        } else if c.is_alphanumeric() {
            result.push(c);
            prev_is_lowercase = c.is_lowercase();
        } else if c == '_' || c == '-' {
            // Keep underscores, convert hyphens to underscores
            if !result.is_empty() && !result.ends_with('_') {
                result.push('_');
            }
            prev_is_lowercase = false;
        }
    }

    result
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_to_pascal_case() {
        assert_eq!(to_pascal_case("active"), "Active");
        assert_eq!(to_pascal_case("inactive"), "Inactive");
        assert_eq!(to_pascal_case("some_value"), "SomeValue");
        assert_eq!(to_pascal_case("some-value"), "SomeValue");
    }
}
