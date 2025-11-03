use config::{GlueConfig, GlueConfigSchemaGenerationPythonPydantic};
use convert_case::Casing;
use lang::{AnalyzedProgram, AstNode, DiagnosticContext, Enum, EnumVariant, Field, LNode, LSyntaxKind, Model, PrimitiveType, SourceCodeMetadata, SymId, SymTable, TypeAtom};

use crate::{
    CodeGenError, CodeGenerator,
    codegen_utils::{indent_lines, qualified_symbol_name_to_case},
    types::CodeGenResult,
};

pub struct CodeGenPython;

// TODO: Refactor such that visitors also emit contributions, and similar refs are shared and not inlined
impl CodeGenerator for CodeGenPython {
    fn generate(&self, program: AnalyzedProgram, source: &SourceCodeMetadata, config: Option<GlueConfig>) -> Result<String, crate::CodeGenError> {
        let ast = program.ast_root.clone();
        let config = config
            .and_then(|cfg| cfg.generation)
            .and_then(|gen_cfg| gen_cfg.python_pydantic)
            .unwrap_or(GlueConfigSchemaGenerationPythonPydantic {
                base_model: Some("pydantic.BaseModel".to_string()),
            });

        let mut codegen = CodeGeneratorImpl::new(ast, program.symbols, source, config);
        let output = codegen.generate()?;
        Ok(output)
    }
}

impl Default for CodeGenPython {
    fn default() -> Self {
        Self::new()
    }
}

impl CodeGenPython {
    pub fn new() -> Self {
        Self
    }
}

struct CodeGeneratorImpl {
    #[allow(dead_code)]
    diag: DiagnosticContext,
    config: GlueConfigSchemaGenerationPythonPydantic,
    ast: LNode,
    syms: SymTable<LNode>,
    preludes: Vec<String>,
}

impl CodeGeneratorImpl {
    pub fn new(ast: LNode, syms: SymTable<LNode>, source: &SourceCodeMetadata, config: GlueConfigSchemaGenerationPythonPydantic) -> Self {
        let diag = DiagnosticContext::new(source.file_name, source.file_contents);
        Self {
            diag,
            ast,
            syms,
            preludes: Default::default(),
            config,
        }
    }

    pub fn generate(&mut self) -> CodeGenResult<String> {
        let mut output = String::new();

        let base_model_import = self.config.base_model.clone().unwrap_or("pydantic.BaseModel".to_string());

        self.preludes
            .push("# pylint: disable=missing-class-docstring, missing-function-docstring, missing-module-docstring\n\n".to_string());
        let (base_model_import_module, base_model_class) = match base_model_import.rsplit_once('.') {
            Some((module, class)) => (module, class),
            None => return Err(CodeGenError::InternalError(format!("Invalid base model import path: {}", base_model_import))),
        };
        self.preludes.push(format!("from {} import {}", base_model_import_module, base_model_class));
        self.preludes.push("from pydantic import Field".to_string());
        self.preludes.push("from enum import StrEnum".to_string());
        self.preludes.push("from typing import Annotated, Optional, Union".to_string());

        for node in self.ast.children() {
            match node.kind() {
                LSyntaxKind::MODEL => {
                    let model_code = self.visit_model(node.clone(), None)?;
                    output.push_str(&model_code);
                }
                // LSyntaxKind::ENUM => {
                //     let enum_code = self.visit_enum(node.clone(), None)?;
                //     output.push_str(&enum_code);
                // }
                _ => {}
            }
        }

        output = format!("{}\n{}", self.preludes.join("\n"), output);

        Ok(output)
    }

    fn visit_model(&mut self, node: LNode, parent_scope: Option<SymId>) -> CodeGenResult<String> {
        let mut output = String::new();

        let model = Model::cast(node).ok_or(CodeGenError::InternalError("Expected Model node".to_string()))?;
        let model_name = model.ident().ok_or(CodeGenError::InternalError("Model missing ident".to_string()))?;
        let current_scope = self
            .syms
            .resolve(parent_scope, &model_name)
            .ok_or_else(|| CodeGenError::InternalError(format!("Unresolved model symbol for model: {}", model_name)))?;
        let qualified_model_name = qualified_symbol_name_to_case(&current_scope.name, convert_case::Case::Pascal);

        let base_model_qualified_import = &self.config.base_model.clone().unwrap();
        let base_model_name = base_model_qualified_import
            .split('.')
            .next_back()
            .ok_or(CodeGenError::InternalError(format!("Invalid base model import path: {}", base_model_qualified_import)))?;
        output.push_str(&format!("\nclass {}({}):\n", qualified_model_name, base_model_name));

        if let Some(docs) = model.docs() {
            let docstring = self.emit_docstring(docs);
            output.push_str(&indent_lines(&docstring, 4));
        }

        let field_nodes = model.field_nodes();
        if field_nodes.is_empty() && model.docs().is_none() {
            output.push_str(&indent_lines("pass\n", 4));
        } else {
            for field_node in field_nodes {
                let field = Field::cast(field_node).ok_or(CodeGenError::InternalError("Expected Field node".to_string()))?;
                let field_name = field.ident().ok_or(CodeGenError::InternalError("Field missing ident".to_string()))?;
                let field_type = field.ty().ok_or(CodeGenError::InternalError("Field missing type".to_string()))?;

                let field_type_atoms = field_type.type_atom_nodes();
                let field_type_atom_codes = field_type_atoms
                    .into_iter()
                    .map(|type_atom_node| self.emit_type_atom(type_atom_node, &field_name, Some(current_scope.id)))
                    .collect::<Result<Vec<String>, CodeGenError>>()?;
                let mut field_type_code = if field_type_atom_codes.len() == 1 {
                    field_type_atom_codes[0].clone()
                } else {
                    format!("Union[{}]", field_type_atom_codes.join(", "))
                };

                if field.is_optional() {
                    field_type_code = format!("Optional[{}]", field_type_code);
                }

                let mut annotated_content = String::new();
                annotated_content.push_str(format!("{}, Field(", field_type_code).as_str());
                // TODO: Support defaults
                if field.is_optional() {
                    annotated_content.push_str("default=None");
                }
                annotated_content.push(')');

                output.push_str(&indent_lines(&format!("{}: Annotated[{}]\n", field_name.to_case(convert_case::Case::Snake), annotated_content), 4));
                if let Some(docs) = field.docs() {
                    let docstring = self.emit_docstring(docs);
                    output.push_str(&indent_lines(&docstring, 4));
                }
            }
        }

        for nested_model_node in model.nested_model_nodes() {
            let nested_model_code = self.visit_model(nested_model_node.clone(), Some(current_scope.id))?;
            output.push_str(&nested_model_code);
        }

        for nested_enum_node in model.nested_enum_nodes() {
            let nested_enum_code = self.visit_enum(nested_enum_node.clone(), Some(current_scope.id))?;
            output.push_str(&nested_enum_code);
        }

        Ok(output)
    }

    fn visit_enum(&mut self, node: LNode, parent_scope: Option<SymId>) -> CodeGenResult<String> {
        let mut output = String::new();

        let enum_model = Enum::cast(node).ok_or(CodeGenError::InternalError("Expected Enum node".to_string()))?;
        let enum_name = enum_model.ident().ok_or(CodeGenError::InternalError("Enum missing ident".to_string()))?;
        let current_scope = self
            .syms
            .resolve(parent_scope, &enum_name)
            .ok_or_else(|| CodeGenError::InternalError(format!("Unresolved enum symbol for enum: {}", enum_name)))?;
        let qualified_enum_name = qualified_symbol_name_to_case(&current_scope.name, convert_case::Case::Pascal);

        output.push_str(&format!("\nclass {}(StrEnum):\n", qualified_enum_name));

        if let Some(docs) = enum_model.docs() {
            let docstring = self.emit_docstring(docs);
            output.push_str(&indent_lines(&docstring, 4));
        }

        for variant_node in enum_model.variant_nodes() {
            let variant = EnumVariant::cast(variant_node).ok_or(CodeGenError::InternalError("Expected EnumVariant node".to_string()))?;
            let variant_value = variant.value().ok_or(CodeGenError::InternalError("EnumVariant missing ident".to_string()))?;
            let variant_ident = variant_value.replace('-', "_").to_case(convert_case::Case::UpperSnake);
            output.push_str(&indent_lines(&format!("{} = \"{}\"\n", variant_ident, variant_value), 4));
            if let Some(docs) = variant.docs() {
                let docstring = self.emit_docstring(docs);
                output.push_str(&indent_lines(&docstring, 4));
            }
        }

        Ok(output)
    }

    fn emit_type_atom(&mut self, node: LNode, _field_name: &str, parent_scope: Option<SymId>) -> CodeGenResult<String> {
        let type_atom = TypeAtom::cast(node.clone()).ok_or(CodeGenError::InternalError("Expected TypeAtom node".to_string()))?;
        // TODO: Support enums?
        let mut res = if let Some(primitive_type) = type_atom.as_primitive_type() {
            match primitive_type {
                PrimitiveType::Bool => "bool".to_string(),
                PrimitiveType::Int => "int".to_string(),
                PrimitiveType::Float => "float".to_string(),
                PrimitiveType::String => "str".to_string(),
            }
        } else {
            // Should be a reference to another model
            let Some(type_ident_token) = type_atom.ident_token() else {
                if type_atom.as_anon_model().is_some() {
                    // TODO: Support anonymous models
                    // // Add a pseudo-symbol for the anonymous model
                    // let parent_scope = self.syms.get(parent_scope.unwrap()).unwrap();
                    // let anon_model_qualified_name = SymTable::<LNode>::join_entries(&parent_scope.name, field_name);
                    // self.syms.add_to_scope(Some(parent_scope.id), anon_model_qualified_name, node.clone());
                    return Err(CodeGenError::GenerationError(self.diag.error(node.text_range(), "Anonymous models are currently not supported")));
                }
                return Err(CodeGenError::InternalError("Expected TypeAtom to have ident for non-primitive type".to_string()));
            };
            let type_name = type_ident_token.text().to_string();
            let sym = self
                .syms
                .resolve(parent_scope, &type_name)
                .ok_or_else(|| CodeGenError::InternalError(format!("Unresolved type atom symbol for type: {}", type_name)))?;

            // Wrap in quotes to support forward references
            format!("\"{}\"", qualified_symbol_name_to_case(&sym.name, convert_case::Case::Pascal))
        };

        if type_atom.is_optional() {
            res = format!("Optional[{}]", res);
        }
        if type_atom.is_array() {
            res = format!("list[{}]", res);
        }
        Ok(res)
    }

    fn emit_docstring(&self, lines: Vec<String>) -> String {
        if lines.len() == 1 {
            format!("\"\"\"{}\"\"\"\n", lines[0].trim())
        } else {
            let mut docstring = String::from("\"\"\"\n");
            for line in lines {
                docstring.push_str(&format!("{}\n", line.trim()));
            }
            docstring.push_str("\"\"\"\n");
            docstring
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use indoc::indoc;
    use insta::assert_snapshot;
    use lang::print_report;

    use crate::{CodeGenError, CodeGenerator, test_utils::analyze_glue_file};

    #[test]
    fn test() {
        let src = indoc! { r#"
            model GlueConfigSchema {
                /// Configuration for code generation (`glue gen [...]`)
                generation: Generation

                model Generation {
                    /// Mode for the watermark at the top of the generated files
                    watermark: Watermark = "short"
                    /// Watermark modes for generated files
                    enum Watermark:
                        /// Includes full watermark with generation command and timestamp
                        "full" |
                        /// Includes short watermark with just generation command
                        "short" |
                        /// No watermark included
                        "none"

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
				"# };

        let (program, source) = analyze_glue_file(src);

        let codegen = CodeGenPython::new();
        let output = codegen
            .generate(program, &source, None)
            .map_err(|e| match e {
                CodeGenError::InternalError(msg) => msg,
                CodeGenError::GenerationError(diag) => {
                    print_report(&diag).expect("Failed to print diagnostic");
                    panic!("Generation error occurred");
                }
                CodeGenError::GenerationErrors(diags) => {
                    for diag in diags {
                        print_report(&diag).expect("Failed to print diagnostic");
                    }
                    panic!("Generation errors occurred");
                }
            })
            .unwrap();

        assert_snapshot!(output);
    }
}
