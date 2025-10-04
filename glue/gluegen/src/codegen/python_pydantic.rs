use gluelang::{Ast, AstNode, AstNodeKind, AstNodePayload, PrimitiveType, SemanticAnalysisArtifacts, TreeNode, Type, TypeVariant};
use log::debug;

use crate::codegen::{CodeGenError, CodeGenerator, GlueConfigSchema, types::EmitResult, utils::generate_watermark};

pub struct PythonPydanticCodeGenerator {
    config: GlueConfigSchema,
    source_file: String,
    ast: Ast,
}

impl CodeGenerator for PythonPydanticCodeGenerator {
    fn generate(mut self) -> EmitResult {
        let mut result = String::new();

        let watermark = generate_watermark(&self.source_file, &self.config.generation.watermark);
        result.push_str("\"\"\"\n");
        for line in watermark {
            result.push_str(&line);
            result.push('\n');
        }
        result.push_str("\"\"\"\n\n");

        // TODO: Add imports based on usage.
        let base_model_import = &self.config.generation.python_pydantic.base_model;
        let (module, class) = Self::parse_import(base_model_import);
        result.push_str(&format!("from {module} import {class}\n"));

        result.push_str("from pydantic import Field\n");
        result.push_str("from enum import StrEnum\n");
        result.push_str("from typing import Annotated, List, Optional\n\n");

        let top_level_nodes = self
            .ast
            .get_children(self.ast.get_root())
            .ok_or(CodeGenError::Other("AST root has no children".to_string()))?;

        debug!("Top-level AST nodes: {top_level_nodes:?}");

        // Traverse the top-level models and enums of the AST.
        for node in top_level_nodes {
            match node.kind() {
                AstNodeKind::Model => {
                    result.push_str(&self.emit_model(&node)?);
                }
                AstNodeKind::Enum => {
                    result.push_str(&self.emit_enum(&node)?);
                }
                _ => {}
            }
        }

        Ok(result)
    }
}

impl PythonPydanticCodeGenerator {
    pub fn new(config: GlueConfigSchema, artifacts: SemanticAnalysisArtifacts) -> Self {
        Self {
            config,
            ast: artifacts.ast,
            source_file: artifacts.source_file,
        }
    }

    fn emit_model(&mut self, model: &AstNode) -> EmitResult {
        let mut inner_emits = String::new();

        let AstNodePayload::Model { name, .. } = model.payload() else {
            return Err(CodeGenError::Other("Expected a model node".to_string()));
        };

        let children = self.ast.get_children(model.id()).unwrap_or_default();

        let mut nested_type_emits = Vec::new();

        for child in &children {
            match child.kind() {
                AstNodeKind::Field => {
                    inner_emits.push_str(&self.emit_field(child)?);
                    inner_emits.push('\n');
                }
                AstNodeKind::Enum => {
                    inner_emits.push_str(&self.emit_enum(child)?);
                }
                AstNodeKind::Model => {
                    let nested = &self.emit_model(child)?;
                    nested_type_emits.push(nested.clone());
                }
                _ => {}
            }
        }

        let mut result = String::new();
        let (_, base_model_class) = Self::parse_import(&self.config.generation.python_pydantic.base_model);
        result.push_str(&format!("class {name}({base_model_class}):\n"));
        if let Some(doc) = children
            .iter()
            .find_map(|c| if let AstNodePayload::Model { doc, .. } = c.payload() { doc.clone() } else { None })
        {
            result.push_str(&format!("    \"\"\"{}\"\"\"\n\n", doc.replace("\"\"\"", "\"\"\"\"\"\"")));
        }

        // Indent and emit nested types (models, enums, etc.) before the parent model, since there can be unresolved refs otherwise.
        for emits in nested_type_emits {
            for line in emits.lines() {
                result.push_str("    ");
                result.push_str(line);
                result.push('\n');
            }
            result.push('\n');
        }
        result.push_str(&inner_emits);

        if inner_emits.is_empty() {
            result.push_str("    pass\n");
        }

        inner_emits.push('\n');

        Ok(result)
    }

    fn emit_field(&mut self, field: &AstNode) -> EmitResult {
        let mut result = String::new();

        let AstNodePayload::Field { name, ty, doc, .. } = field.payload() else {
            return Err(CodeGenError::Other("Expected a field node".to_string()));
        };
        let decorator = self
            .ast
            .get_children(field.id())
            .and_then(|children| children.iter().find(|c| c.kind() == AstNodeKind::Decorator).cloned());

        let mut field_str = "Field(".to_string();
        if let Some(AstNodePayload::Decorator { name, positional_args, .. }) = decorator.map(|d| d.payload().clone()) {
            if name == "alias" {
                if let Some(arg) = positional_args.first() {
                    field_str.push_str(&format!("alias={arg}"));
                }
            }
        }
        field_str.push(')');

        result.push_str(&format!("    {}: Annotated[{}, {}]", name, self.emit_type(ty)?, field_str));
        if let Some(doc) = doc {
            result.push('\n');
            result.push_str(&format!("    \"\"\"{}\"\"\"", doc.replace('\n', " ")));
        }

        Ok(result)
    }

    fn emit_enum(&mut self, enum_node: &AstNode) -> EmitResult {
        let mut result = String::new();

        let AstNodePayload::Enum { name, variants, .. } = enum_node.payload() else {
            return Err(CodeGenError::Other("Expected an enum node".to_string()));
        };

        result.push_str(&format!("class {name}(StrEnum):\n"));
        for variant in variants {
            result.push_str(&format!("    {} = \"{}\"\n", variant.to_uppercase(), variant));
        }
        result.push('\n');

        Ok(result)
    }

    fn emit_type(&mut self, ty: &Type) -> EmitResult {
        let mut result = String::new();

        let type_atoms = match ty {
            Type::Single(ty) => vec![ty],
            Type::Union(types) => types.iter().collect(),
        };

        for atom in type_atoms {
            if !result.is_empty() {
                result.push_str(" | ");
            }

            let mut atom_str = String::new();
            match &atom.variant {
                TypeVariant::Primitive(p) => match p {
                    PrimitiveType::String => atom_str.push_str("str"),
                    PrimitiveType::Int => atom_str.push_str("int"),
                    PrimitiveType::Bool => atom_str.push_str("bool"),
                },
                TypeVariant::Ref(name) => atom_str.push_str(name),
            }

            if atom.is_array {
                atom_str = format!("List[{atom_str}]");
            } else if atom.is_optional {
                atom_str = format!("Optional[{atom_str}]");
            }

            result.push_str(&atom_str);
        }

        Ok(result)
    }

    fn parse_import(import_str: &str) -> (String, String) {
        if let Some(idx) = import_str.rfind('.') {
            let module = &import_str[..idx];
            let class = &import_str[idx + 1..];
            (module.to_string(), class.to_string())
        } else {
            ("pydantic".to_string(), import_str.to_string())
        }
    }
}
