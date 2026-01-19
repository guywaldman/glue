use config::{GlueConfig, GlueConfigSchemaGenerationPythonPydantic};
use convert_case::{Case, Casing};
use lang::{
    AnalyzedProgram, AstNode, Enum, Field, Literal, LiteralExpr, Model, SourceCodeMetadata, SymId,
    Type, TypeAtom, MODEL_FIELD_DECORATOR, MODEL_FIELD_DECORATOR_ALIAS_ARG,
};

use crate::{
    CodeGenError, CodeGenerator,
    codegen::CodeGenResult,
    context::{CodeGenContext, DocEmitter, EnumExt, EnumVariantExt, FieldExt, ModelExt, TypeMapper, indent},
};

pub struct CodeGenPython;

impl Default for CodeGenPython {
    fn default() -> Self {
        Self
    }
}

impl CodeGenPython {
    pub fn new() -> Self {
        Self
    }
}

impl CodeGenerator for CodeGenPython {
    fn generate(&self, program: AnalyzedProgram, source: &SourceCodeMetadata, config: Option<GlueConfig>) -> Result<String, CodeGenError> {
        let pydantic_config = config
            .and_then(|cfg| cfg.generation)
            .and_then(|g| g.python_pydantic)
            .unwrap_or(GlueConfigSchemaGenerationPythonPydantic {
                base_model: Some("pydantic.BaseModel".to_string()),
            });

        let ctx = CodeGenContext::new(program.ast_root.clone(), program.symbols, source, None);
        let mut generator = PythonGenerator::new(ctx, pydantic_config);
        generator.generate()
    }
}

struct PythonGenerator<'a> {
    ctx: CodeGenContext<'a>,
    config: GlueConfigSchemaGenerationPythonPydantic,
    output: String,
}

impl<'a> PythonGenerator<'a> {
    fn new(ctx: CodeGenContext<'a>, config: GlueConfigSchemaGenerationPythonPydantic) -> Self {
        Self { ctx, config, output: String::new() }
    }

    fn generate(&mut self) -> CodeGenResult<String> {
        // Build preludes
        let base_model_import = self.config.base_model.clone().unwrap_or_else(|| "pydantic.BaseModel".to_string());
        let (base_module, base_class) = base_model_import.rsplit_once('.')
            .ok_or_else(|| CodeGenError::InternalError(format!("Invalid base model path: {}", base_model_import)))?;

        let preludes = ["# pylint: disable=missing-class-docstring, missing-function-docstring, missing-module-docstring\n".to_string(),
            format!("from {} import {}", base_module, base_class),
            "from pydantic import Field".to_string(),
            "from enum import StrEnum".to_string(),
            "from typing import Any, Annotated, Optional, Union".to_string()];

        // Generate models and enums
        for model in self.ctx.top_level_models().collect::<Vec<_>>() {
            let code = self.emit_model(&model, None)?;
            self.output.push_str(&code);
        }

        for enum_ in self.ctx.top_level_enums().collect::<Vec<_>>() {
            let code = self.emit_enum(&enum_, None)?;
            self.output.push_str(&code);
        }

        Ok(format!("{}\n{}", preludes.join("\n"), self.output))
    }

    fn base_class_name(&self) -> &str {
        self.config.base_model.as_ref()
            .and_then(|s| s.rsplit_once('.'))
            .map(|(_, class)| class)
            .unwrap_or("BaseModel")
    }

    fn emit_model(&mut self, model: &Model, parent_scope: Option<SymId>) -> CodeGenResult<String> {
        let mut output = String::new();

        let scope_id = model.scope_id(&self.ctx, parent_scope)?;
        let qualified_name = model.qualified_name(&self.ctx, parent_scope, Case::Pascal)?;

        // Class declaration
        output.push_str(&format!("\nclass {}({}):\n", qualified_name, self.base_class_name()));

        // Docstring
        if let Some(docs) = model.docs() {
            output.push_str(&indent(&DocEmitter::python_docstring(&docs), 4));
        }

        // Fields
        let fields = model.fields();
        if fields.is_empty() && model.docs().is_none() {
            output.push_str(&indent("pass\n", 4));
        } else {
            for field in fields {
                let field_code = self.emit_field(&field, Some(scope_id))?;
                output.push_str(&field_code);
            }
        }

        // Nested types
        for nested_model in model.nested_models() {
            let code = self.emit_model(&nested_model, Some(scope_id))?;
            output.push_str(&code);
        }

        for nested_enum in model.nested_enums() {
            let code = self.emit_enum(&nested_enum, Some(scope_id))?;
            output.push_str(&code);
        }

        Ok(output)
    }

    fn emit_field(&self, field: &Field, scope: Option<SymId>) -> CodeGenResult<String> {
        let mut output = String::new();

        let field_name = field.name()?;
        let field_type = field.field_type()?;
        let py_field_name = field_name.to_case(Case::Snake);

        // Build type annotation
        let mut type_code = self.emit_type(&field_type, scope)?;
        if field.is_optional() {
            type_code = format!("Optional[{}]", type_code);
        }

        // Build Field() arguments
        let mut field_args = vec![];

        // Default value
        if field.is_optional() {
            field_args.push("default=None".to_string());
        } else if let Some(default_node) = field.default_literal_expr_node()
            && let Some(default_expr) = LiteralExpr::cast(default_node)
                && let Some(lit) = default_expr.value() {
                    let default_code = self.emit_literal(&lit);
                    field_args.push(format!("default={}", default_code));
                }

        // Alias from @field decorator or auto-generate if names differ
        let decorators = field.decorators();
        let field_decorator = decorators.iter().find(|d| d.ident() == Some(MODEL_FIELD_DECORATOR.id.to_owned()));
        if let Some(dec) = field_decorator {
            if let Some(alias_arg) = dec.arg(MODEL_FIELD_DECORATOR, &MODEL_FIELD_DECORATOR_ALIAS_ARG)
                && let Some(Literal::StringLiteral(v)) = alias_arg.literal()
                    && let Some(alias_value) = v.value() {
                        field_args.push(format!("alias=\"{}\"", alias_value));
                    }
        } else if py_field_name != field_name {
            field_args.push(format!("alias=\"{}\"", field_name));
        }

        output.push_str(&indent(
            &format!("{}: Annotated[{}, Field({})]\n", py_field_name, type_code, field_args.join(", ")),
            4,
        ));

        // Field docstring
        if let Some(docs) = field.docs() {
            output.push_str(&indent(&DocEmitter::python_docstring(&docs), 4));
        }

        Ok(output)
    }

    fn emit_enum(&self, enum_: &Enum, parent_scope: Option<SymId>) -> CodeGenResult<String> {
        let mut output = String::new();

        let qualified_name = enum_.qualified_name(&self.ctx, parent_scope, Case::Pascal)?;

        output.push_str(&format!("\nclass {}(StrEnum):\n", qualified_name));

        // Docstring
        if let Some(docs) = enum_.docs() {
            output.push_str(&indent(&DocEmitter::python_docstring(&docs), 4));
        }

        // Variants
        for variant in enum_.variants() {
            let value = variant.variant_value()?;
            let ident = value.replace('-', "_").to_case(Case::UpperSnake);
            output.push_str(&indent(&format!("{} = \"{}\"\n", ident, value), 4));

            if let Some(docs) = variant.docs() {
                output.push_str(&indent(&DocEmitter::python_docstring(&docs), 4));
            }
        }

        Ok(output)
    }

    fn emit_type(&self, ty: &Type, scope: Option<SymId>) -> CodeGenResult<String> {
        let atoms = ty.type_atoms();

        if atoms.len() == 1 {
            self.emit_type_atom(&atoms[0], scope)
        } else {
            let atom_codes: Vec<_> = atoms.iter()
                .map(|a| self.emit_type_atom(a, scope))
                .collect::<Result<Vec<_>, _>>()?;
            Ok(format!("Union[{}]", atom_codes.join(", ")))
        }
    }

    fn emit_type_atom(&self, atom: &TypeAtom, scope: Option<SymId>) -> CodeGenResult<String> {
        let mut result = if let Some(primitive) = atom.as_primitive_type() {
            TypeMapper::to_python(primitive).to_string()
        } else if let Some(record) = atom.as_record_type() {
            let src = record.src_type_node().ok_or_else(|| CodeGenContext::internal_error("Record missing src type"))?;
            let dest = record.dest_type_node().ok_or_else(|| CodeGenContext::internal_error("Record missing dest type"))?;
            // src and dest are TYPE nodes, need to get the Type and emit it
            let src_type = Type::cast(src).ok_or_else(|| CodeGenContext::internal_error("Expected Type for record src"))?;
            let dest_type = Type::cast(dest).ok_or_else(|| CodeGenContext::internal_error("Expected Type for record dest"))?;
            let src_str = self.emit_type(&src_type, scope)?;
            let dest_str = self.emit_type(&dest_type, scope)?;
            format!("dict[{}, {}]", src_str, dest_str)
        } else if let Some(ref_token) = atom.as_ref_token() {
            let type_name = ref_token.text().to_string();
            let sym = self.ctx.resolve(scope, &type_name)
                .ok_or_else(|| CodeGenContext::internal_error(format!("Unresolved type: {}", type_name)))?;
            let qualified = lang::symbol_name_to_parts(&sym.name).join("_").to_case(Case::Pascal);
            format!("\"{}\"", qualified)  // Forward reference
        } else if atom.as_anon_model().is_some() {
            return Err(self.ctx.error(atom.syntax(), "Anonymous models not supported"));
        } else {
            return Err(CodeGenContext::internal_error("Unknown type atom kind"));
        };

        if atom.is_optional() {
            result = format!("Optional[{}]", result);
        }
        if atom.is_array() {
            result = format!("list[{}]", result);
        }

        Ok(result)
    }

    fn emit_literal(&self, lit: &Literal) -> String {
        match lit {
            Literal::BoolLiteral { value, .. } => if *value { "True" } else { "False" }.to_string(),
            Literal::IntLiteral { value, .. } => value.to_string(),
            Literal::StringLiteral(node) => format!("\"{}\"", node.value().unwrap_or_default()),
            _ => "None".to_string(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use indoc::indoc;
    use insta::assert_snapshot;
    use lang::print_report;

    use crate::{CodeGenError, CodeGenerator, test_utils::analyze_test_glue_file};

    fn gen_python(src: &str) -> String {
        let (program, source) = analyze_test_glue_file(src);
        let codegen = CodeGenPython::new();
        codegen
            .generate(program, &source, None)
            .map_err(|e| match e {
                CodeGenError::InternalError(msg) => panic!("Internal error: {}", msg),
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
                e => panic!("Unexpected error: {:?}", e),
            })
            .unwrap()
    }

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

        assert_snapshot!(gen_python(src));
    }

    #[test]
    fn test_record_string_to_any() {
        let src = indoc! { r#"
            model Item {
                /// A map of string keys to any values
                raw_data: Record<string, any>
            }
        "# };

        let output = gen_python(src);
        assert!(output.contains("dict[str, Any]"), "Expected dict[str, Any] in output:\n{}", output);
        assert_snapshot!(output);
    }

    #[test]
    fn test_record_string_to_int() {
        let src = indoc! { r#"
            model Scores {
                /// Player scores by name
                scores: Record<string, int>
            }
        "# };

        let output = gen_python(src);
        assert!(output.contains("dict[str, int]"), "Expected dict[str, int] in output:\n{}", output);
        assert_snapshot!(output);
    }

    #[test]
    fn test_record_nested_in_model() {
        let src = indoc! { r#"
            model Container {
                inner: Inner
                
                model Inner {
                    /// Nested map field
                    data: Record<string, any>
                }
            }
        "# };

        let output = gen_python(src);
        assert!(output.contains("dict[str, Any]"), "Expected dict[str, Any] in output:\n{}", output);
        assert_snapshot!(output);
    }

    #[test]
    fn test_record_optional() {
        let src = indoc! { r#"
            model Config {
                /// Optional map of settings
                settings?: Record<string, string>
            }
        "# };

        let output = gen_python(src);
        assert!(output.contains("Optional[dict[str, str]]"), "Expected Optional[dict[str, str]] in output:\n{}", output);
        assert_snapshot!(output);
    }

    #[test]
    fn test_record_array() {
        let src = indoc! { r#"
            model MultiConfig {
                /// List of config maps
                configs: Record<string, int>[]
            }
        "# };

        let output = gen_python(src);
        // Note: Record<K,V>[] - check actual output for list wrapping
        assert!(output.contains("dict[str, int]"), "Expected dict in output:\n{}", output);
        assert_snapshot!(output);
    }
}
