use config::{GlueConfig, GlueConfigSchemaGenerationPython, GlueConfigSchemaGenerationPythonDataModelLibrary};
use convert_case::{Case, Casing};
use lang::{AnalyzedProgram, AstNode, Enum, Field, Literal, LiteralExpr, Model, SourceCodeMetadata, SymId, Type, TypeAtom};

use crate::{
    CodeGenError, CodeGenerator,
    codegen::CodeGenResult,
    context::{CodeGenContext, DocEmitter, EnumVariantExt, FieldExt, NamedExt, TypeMapper, indent},
};

enum PyModelLibrary {
    Pydantic { base_model: String },
    Dataclasses,
    Msgspec,
}

impl PyModelLibrary {
    fn from_config(config: GlueConfigSchemaGenerationPython) -> Result<Self, CodeGenError> {
        let kind = config.data_model_library.unwrap_or(GlueConfigSchemaGenerationPythonDataModelLibrary::Pydantic);
        match kind {
            GlueConfigSchemaGenerationPythonDataModelLibrary::Pydantic => {
                let base = config.base_model.unwrap_or_else(|| "pydantic.BaseModel".to_string());
                if !base.contains('.') {
                    return Err(CodeGenError::InternalError(format!("Invalid base model path (needs module.Class): {}", base)));
                }
                Ok(Self::Pydantic { base_model: base })
            }
            GlueConfigSchemaGenerationPythonDataModelLibrary::Dataclasses => Ok(Self::Dataclasses),
            GlueConfigSchemaGenerationPythonDataModelLibrary::Msgspec => Ok(Self::Msgspec),
        }
    }

    fn preludes(&self) -> Vec<String> {
        let lint = "# pylint: disable=missing-class-docstring, missing-function-docstring, missing-module-docstring\n".to_string();
        match self {
            Self::Pydantic { base_model } => {
                let (module, class) = base_model.rsplit_once('.').unwrap();
                vec![
                    lint,
                    format!("from {} import {}", module, class),
                    "from pydantic import Field".to_string(),
                    "from enum import StrEnum".to_string(),
                    "from typing import Any, Annotated, Optional, Union".to_string(),
                ]
            }
            Self::Dataclasses => vec![
                lint,
                "from dataclasses import dataclass, field".to_string(),
                "from enum import StrEnum".to_string(),
                "from typing import Any, Optional, Union".to_string(),
            ],
            Self::Msgspec => vec![
                lint,
                "import msgspec".to_string(),
                "from enum import StrEnum".to_string(),
                "from typing import Any, Optional, Union".to_string(),
            ],
        }
    }

    fn model_decorator(&self) -> Option<&str> {
        match self {
            Self::Dataclasses => Some("@dataclass"),
            _ => None,
        }
    }

    fn base_class(&self) -> &str {
        match self {
            Self::Pydantic { base_model } => base_model.rsplit_once('.').unwrap().1,
            Self::Dataclasses => "",
            Self::Msgspec => "msgspec.Struct",
        }
    }
}

#[derive(Default)]
pub struct CodeGenPython;

impl CodeGenerator for CodeGenPython {
    fn generate(&self, program: AnalyzedProgram, source: &SourceCodeMetadata, config: Option<GlueConfig>) -> Result<String, CodeGenError> {
        let py_config = config.and_then(|c| c.generation).and_then(|g| g.python).unwrap_or_default();

        let lib = PyModelLibrary::from_config(py_config)?;
        let ctx = CodeGenContext::new(program.ast_root.clone(), program.symbols, source, None);
        let mut generator = PythonGenerator::new(ctx, lib);
        generator.generate()
    }
}

struct PythonGenerator<'a> {
    ctx: CodeGenContext<'a>,
    fw: PyModelLibrary,
    output: String,
}

impl<'a> PythonGenerator<'a> {
    fn new(ctx: CodeGenContext<'a>, fw: PyModelLibrary) -> Self {
        Self { ctx, fw, output: String::new() }
    }

    fn generate(&mut self) -> CodeGenResult<String> {
        for model in self.ctx.top_level_models().collect::<Vec<_>>() {
            self.emit_model(&model, None)?;
        }
        for enum_ in self.ctx.top_level_enums().collect::<Vec<_>>() {
            self.emit_enum(&enum_, None)?;
        }

        let preludes = self.fw.preludes().join("\n");
        Ok(format!("{}\n{}", preludes, self.output))
    }

    fn emit_model(&mut self, model: &Model, parent_scope: Option<SymId>) -> CodeGenResult<()> {
        let scope_id = model.scope_id(&self.ctx, parent_scope)?;
        let name = model.qualified_name(&self.ctx, parent_scope, Case::Pascal)?;

        if let Some(decorator) = self.fw.model_decorator() {
            self.output.push_str(&format!("\n{}\n", decorator));
        } else {
            self.output.push('\n');
        }

        let base = self.fw.base_class();
        if base.is_empty() {
            self.output.push_str(&format!("class {}:\n", name));
        } else {
            self.output.push_str(&format!("class {}({}):\n", name, base));
        }

        if let Some(docs) = model.docs() {
            self.output.push_str(&indent(&DocEmitter::python_docstring(&docs), 4));
        }

        let fields = model.fields();
        if fields.is_empty() && model.docs().is_none() {
            self.output.push_str(&indent("pass\n", 4));
        } else {
            for field in fields {
                self.emit_field(&field, Some(scope_id))?;
            }
        }

        for nested in model.nested_models() {
            self.emit_model(&nested, Some(scope_id))?;
        }
        for nested in model.nested_enums() {
            self.emit_enum(&nested, Some(scope_id))?;
        }

        Ok(())
    }

    fn emit_field(&mut self, field: &Field, scope: Option<SymId>) -> CodeGenResult<()> {
        let field_name = field.name()?;
        let py_name = field_name.to_case(Case::Snake);

        let mut type_code = self.emit_type(&field.field_type()?, scope)?;
        if field.is_optional() {
            type_code = format!("Optional[{}]", type_code);
        }

        let line = match &self.fw {
            PyModelLibrary::Pydantic { .. } => self.emit_field_pydantic(field, &py_name, &field_name, &type_code)?,
            PyModelLibrary::Dataclasses => self.emit_field_simple(field, &py_name, &type_code)?,
            PyModelLibrary::Msgspec => self.emit_field_simple(field, &py_name, &type_code)?,
        };

        self.output.push_str(&indent(&line, 4));

        if let Some(docs) = field.docs() {
            self.output.push_str(&indent(&DocEmitter::python_docstring(&docs), 4));
        }

        Ok(())
    }

    /// Pydantic: `name: Annotated[Type, Field(default=..., alias="...")]`
    fn emit_field_pydantic(&self, field: &Field, py_name: &str, orig_name: &str, type_code: &str) -> CodeGenResult<String> {
        let mut args = vec![];

        if field.is_optional() {
            args.push("default=None".to_string());
        } else if let Some(default) = self.field_default_code(field) {
            args.push(format!("default={}", default));
        }

        let alias = field.alias()?;
        if let Some(v) = &alias {
            args.push(format!("alias=\"{}\"", v));
        } else if py_name != orig_name {
            args.push(format!("alias=\"{}\"", orig_name));
        }

        Ok(format!("{}: Annotated[{}, Field({})]\n", py_name, type_code, args.join(", ")))
    }

    /// Dataclasses / msgspec: `name: Type = default`
    fn emit_field_simple(&self, field: &Field, py_name: &str, type_code: &str) -> CodeGenResult<String> {
        if field.is_optional() {
            Ok(format!("{}: {} = None\n", py_name, type_code))
        } else if let Some(default) = self.field_default_code(field) {
            Ok(format!("{}: {} = {}\n", py_name, type_code, default))
        } else {
            Ok(format!("{}: {}\n", py_name, type_code))
        }
    }

    fn field_default_code(&self, field: &Field) -> Option<String> {
        field
            .default_literal_expr_node()
            .and_then(LiteralExpr::cast)
            .and_then(|expr| expr.value())
            .map(|lit| self.emit_literal(&lit))
    }

    // -- Enums --------------------------------------------------------------

    fn emit_enum(&mut self, enum_: &Enum, parent_scope: Option<SymId>) -> CodeGenResult<()> {
        let name = enum_.qualified_name(&self.ctx, parent_scope, Case::Pascal)?;

        self.output.push_str(&format!("\nclass {}(StrEnum):\n", name));

        if let Some(docs) = enum_.docs() {
            self.output.push_str(&indent(&DocEmitter::python_docstring(&docs), 4));
        }

        for variant in enum_.variants() {
            let value = variant.variant_value()?;
            let ident = value.replace('-', "_").to_case(Case::UpperSnake);
            self.output.push_str(&indent(&format!("{} = \"{}\"\n", ident, value), 4));

            if let Some(docs) = variant.docs() {
                self.output.push_str(&indent(&DocEmitter::python_docstring(&docs), 4));
            }
        }

        Ok(())
    }

    // -- Types & literals ---------------------------------------------------

    fn emit_type(&self, ty: &Type, scope: Option<SymId>) -> CodeGenResult<String> {
        let atoms = ty.type_atoms();
        if atoms.len() == 1 {
            self.emit_type_atom(&atoms[0], scope)
        } else {
            let codes: Vec<_> = atoms.iter().map(|a| self.emit_type_atom(a, scope)).collect::<Result<Vec<_>, _>>()?;
            Ok(format!("Union[{}]", codes.join(", ")))
        }
    }

    fn emit_type_atom(&self, atom: &TypeAtom, scope: Option<SymId>) -> CodeGenResult<String> {
        let mut result = if let Some(primitive) = atom.as_primitive_type() {
            TypeMapper::to_python(primitive).to_string()
        } else if let Some(record) = atom.as_record_type() {
            let src = record.src_type_node().ok_or_else(|| CodeGenContext::internal_error("Record missing src type"))?;
            let dest = record.dest_type_node().ok_or_else(|| CodeGenContext::internal_error("Record missing dest type"))?;
            let src_type = Type::cast(src).ok_or_else(|| CodeGenContext::internal_error("Expected Type for record src"))?;
            let dest_type = Type::cast(dest).ok_or_else(|| CodeGenContext::internal_error("Expected Type for record dest"))?;
            format!("dict[{}, {}]", self.emit_type(&src_type, scope)?, self.emit_type(&dest_type, scope)?)
        } else if let Some(ref_token) = atom.as_ref_token() {
            let type_name = ref_token.text().to_string();
            let sym = self
                .ctx
                .resolve(scope, &type_name)
                .ok_or_else(|| CodeGenContext::internal_error(format!("Unresolved type: {}", type_name)))?;
            let qualified = lang::symbol_name_to_parts(&sym.name).join("_").to_case(Case::Pascal);
            format!("\"{}\"", qualified)
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
    use config::{GlueConfigSchemaGeneration, GlueConfigSchemaGenerationPython, GlueConfigSchemaGenerationPythonDataModelLibrary};
    use indoc::indoc;
    use insta::assert_snapshot;

    use crate::test_utils::{gen_test, gen_test_with_config};

    fn gen_python(src: &str) -> String {
        gen_test(&CodeGenPython, src)
    }

    fn gen_python_with_data_model_lib(src: &str, lib: GlueConfigSchemaGenerationPythonDataModelLibrary) -> String {
        let config = GlueConfig {
            generation: Some(GlueConfigSchemaGeneration {
                python: Some(GlueConfigSchemaGenerationPython {
                    data_model_library: Some(lib),
                    base_model: None,
                }),
                ..Default::default()
            }),
        };
        gen_test_with_config(&CodeGenPython, src, Some(config))
    }

    const SIMPLE_MODEL: &str = indoc! { r#"
        model User {
            /// The user's display name
            name: string
            age: int
            email?: string
            active: bool = true
        }
    "# };

    #[test]
    fn test_pydantic() {
        assert_snapshot!(gen_python(SIMPLE_MODEL));
    }

    #[test]
    fn test_dataclasses() {
        assert_snapshot!(gen_python_with_data_model_lib(SIMPLE_MODEL, GlueConfigSchemaGenerationPythonDataModelLibrary::Dataclasses));
    }

    #[test]
    fn test_msgspec() {
        assert_snapshot!(gen_python_with_data_model_lib(SIMPLE_MODEL, GlueConfigSchemaGenerationPythonDataModelLibrary::Msgspec));
    }

    #[test]
    fn test_nested_model() {
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
        assert!(output.contains("dict[str, int]"), "Expected dict in output:\n{}", output);
        assert_snapshot!(output);
    }
}
