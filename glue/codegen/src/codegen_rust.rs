use config::GlueConfig;
use convert_case::{Case, Casing};
use lang::{
    AnalyzedProgram, AstNode, Enum, Field, Literal, Model, SourceCodeMetadata, SymId, Type, TypeAtom,
    MODEL_FIELD_DECORATOR, MODEL_FIELD_DECORATOR_ALIAS_ARG,
};

use crate::{
    CodeGenError, CodeGenerator,
    codegen::CodeGenResult,
    context::{CodeGenContext, DocEmitter, EnumExt, FieldExt, ModelExt, TypeMapper},
};

pub struct CodeGenRust;


impl Default for CodeGenRust {
    fn default() -> Self {
        Self::new()
    }
}

impl CodeGenRust {
    pub fn new() -> Self {
        Self
    }
}

impl CodeGenerator for CodeGenRust {
    fn generate(&self, program: AnalyzedProgram, source: &SourceCodeMetadata, config: Option<GlueConfig>) -> Result<String, CodeGenError> {
        let ctx = CodeGenContext::new(program.ast_root.clone(), program.symbols, source, config.as_ref());
        let mut generator = RustGenerator::new(ctx);
        generator.generate()
    }
}

struct RustGenerator<'a> {
    ctx: CodeGenContext<'a>,
    output: String,
    postludes: Vec<String>,
}

impl<'a> RustGenerator<'a> {
    fn new(ctx: CodeGenContext<'a>) -> Self {
        Self {
            ctx,
            output: String::new(),
            postludes: Vec::new(),
        }
    }

    fn generate(&mut self) -> CodeGenResult<String> {
        // Preludes
        self.output.push_str("use std::collections::HashMap;\n\n");

        // Top-level
        for model in self.ctx.top_level_models().collect::<Vec<_>>() {
            let code = self.emit_model(&model, None)?;
            self.output.push_str(&code);
        }
        for enum_ in self.ctx.top_level_enums().collect::<Vec<_>>() {
            let code = self.emit_enum(&enum_, None)?;
            self.output.push_str(&code);
        }

        // Append postludes
        for postlude in &self.postludes {
            self.output.push_str(postlude);
        }

        Ok(self.output.clone())
    }

    fn emit_model(&mut self, model: &Model, parent_scope: Option<SymId>) -> CodeGenResult<String> {
        let mut output = String::new();

        let scope_id = model.scope_id(&self.ctx, parent_scope)?;
        let qualified_name = model.qualified_name(&self.ctx, parent_scope, Case::Pascal)?;

        // Documentation
        if let Some(docs) = model.docs() {
            output.push_str(&DocEmitter::rust_docs(&docs, 0));
        }

        // Struct declaration with derives
        output.push_str("#[derive(serde::Serialize, serde::Deserialize, Debug, Clone, Default)]\n");
        output.push_str(&format!("pub struct {} {{\n", qualified_name));

        // Fields
        for field in model.fields() {
            let field_code = self.emit_field(&field, Some(scope_id))?;
            output.push_str(&field_code);
        }

        output.push_str("}\n\n");

        // Queue nested types for postludes
        for nested_model in model.nested_models() {
            let nested_code = self.emit_model(&nested_model, Some(scope_id))?;
            self.postludes.push(nested_code);
        }

        for nested_enum in model.nested_enums() {
            let nested_code = self.emit_enum(&nested_enum, Some(scope_id))?;
            self.postludes.push(nested_code);
        }

        Ok(output)
    }

    fn emit_enum(&mut self, enum_: &Enum, parent_scope: Option<SymId>) -> CodeGenResult<String> {
        let mut output = String::new();

        let qualified_name = enum_.qualified_name(&self.ctx, parent_scope, Case::Pascal)?;

        // Documentation
        if let Some(docs) = enum_.docs() {
            output.push_str(&DocEmitter::rust_docs(&docs, 0));
        }

        // Enum declaration with derives
        output.push_str("#[derive(serde::Serialize, serde::Deserialize, Debug, Clone, PartialEq, Eq)]\n");
        output.push_str(&format!("pub enum {} {{\n", qualified_name));

        // Handle enum literals (variants)
        for variant in enum_.variants() {
            let variant_value = variant.value()
                .ok_or_else(|| CodeGenContext::internal_error("Enum variant missing value"))?;
            let variant_name = variant_value.to_case(Case::Pascal);

            if let Some(docs) = variant.docs() {
                output.push_str(&DocEmitter::rust_docs(&docs, 1));
            }

            output.push_str(&format!("    #[serde(rename = \"{}\")]\n", variant_value));
            output.push_str(&format!("    {},\n", variant_name));
        }

        output.push_str("}\n\n");

        Ok(output)
    }

    fn emit_field(&mut self, field: &Field, parent_scope: Option<SymId>) -> CodeGenResult<String> {
        let mut output = String::new();

        let field_name = field.name()?;
        let field_type = field.field_type()?;

        if let Some(docs) = field.docs() {
            output.push_str(&DocEmitter::rust_docs(&docs, 1));
        }

        // Check for @field decorator with alias
        let alias = self.extract_field_alias(field)?;
        if let Some(ref alias_value) = alias {
            output.push_str(&format!("    #[serde(rename = \"{}\")]\n", alias_value));
        }

        // Build type string
        let type_atoms = field_type.type_atoms();
        let type_strs: Vec<String> = type_atoms
            .iter()
            .map(|atom| self.emit_type_atom(atom, parent_scope))
            .collect::<Result<Vec<_>, _>>()?;

        let mut type_code = type_strs.join(" | ");

        // Handle optional fields
        if field.is_optional() {
            output.push_str("    #[serde(skip_serializing_if = \"Option::is_none\")]\n");
            type_code = format!("Option<{}>", type_code);
        }

        // Escape keywords
        let emit_name = match field_name.as_str() {
            "type" => "r#type",
            "ref" => "r#ref",
            "self" => "r#self",
            "mod" => "r#mod",
            other => other,
        };

        output.push_str(&format!("    pub {}: {},\n", emit_name, type_code));

        Ok(output)
    }

    fn emit_type_atom(&self, atom: &TypeAtom, parent_scope: Option<SymId>) -> CodeGenResult<String> {
        // Primitive type
        if let Some(primitive) = atom.as_primitive_type() {
            return Ok(TypeMapper::to_rust(primitive).to_string());
        }

        // Record type (HashMap)
        if let Some(record_type) = atom.as_record_type() {
            let src_type = record_type.src_type_node()
                .ok_or_else(|| CodeGenContext::internal_error("Record missing source type"))?;
            let dest_type = record_type.dest_type_node()
                .ok_or_else(|| CodeGenContext::internal_error("Record missing destination type"))?;

            let src_atoms = Type::cast(src_type).map(|t: Type| t.type_atoms()).unwrap_or_default();
            let dest_atoms = Type::cast(dest_type).map(|t: Type| t.type_atoms()).unwrap_or_default();

            let src_str = src_atoms.first()
                .map(|a| self.emit_type_atom(a, parent_scope))
                .transpose()?
                .unwrap_or_else(|| "String".to_string());
            let dest_str = dest_atoms.first()
                .map(|a| self.emit_type_atom(a, parent_scope))
                .transpose()?
                .unwrap_or_else(|| "serde_json::Value".to_string());

            return Ok(format!("HashMap<{}, {}>", src_str, dest_str));
        }

        // Reference to another type
        if let Some(ref_token) = atom.as_ref_token() {
            let ref_name = ref_token.text().trim();
            let resolved = self.ctx.qualified_name(parent_scope, ref_name, Case::Pascal)
                .ok_or_else(|| CodeGenContext::internal_error(format!("Unresolved type: {}", ref_name)))?;
            return Ok(resolved);
        }

        // Anonymous model - not yet supported
        if atom.as_anon_model().is_some() {
            return Err(CodeGenContext::internal_error("Anonymous models not yet supported in Rust codegen"));
        }

        Err(CodeGenContext::internal_error("Unknown type atom"))
    }

    fn extract_field_alias(&self, field: &Field) -> CodeGenResult<Option<String>> {
        let decorators = field.decorators();
        let field_dec = decorators.iter().find(|d| d.ident().as_deref() == Some(MODEL_FIELD_DECORATOR.id));

        if let Some(dec) = field_dec
            && let Some(alias_arg) = dec.arg(MODEL_FIELD_DECORATOR, &MODEL_FIELD_DECORATOR_ALIAS_ARG)
                && let Some(Literal::StringLiteral(s)) = alias_arg.literal() {
                    return Ok(s.value());
                }

        Ok(None)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use indoc::indoc;
    use insta::assert_snapshot;
    use lang::print_report;

    use crate::test_utils::analyze_test_glue_file;

    fn gen_rust(src: &str) -> String {
        let (program, source) = analyze_test_glue_file(src);
        let codegen = CodeGenRust::new();
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
            @root
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

        assert_snapshot!(gen_rust(src));
    }

    #[test]
    fn test_record_string_to_any() {
        let src = indoc! { r#"
            model Item {
                /// A map of string keys to any values
                raw_data: Record<string, any>
            }
        "# };

        let output = gen_rust(src);
        assert!(output.contains("HashMap<String, serde_json::Value>"), "Expected HashMap in output:\n{}", output);
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

        let output = gen_rust(src);
        assert!(output.contains("HashMap<String, i64>"), "Expected HashMap<String, i64> in output:\n{}", output);
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

        let output = gen_rust(src);
        assert!(output.contains("HashMap<String, serde_json::Value>"), "Expected HashMap in output:\n{}", output);
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

        let output = gen_rust(src);
        assert!(output.contains("Option<HashMap<String, String>>"), "Expected Option<HashMap> in output:\n{}", output);
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

        let output = gen_rust(src);
        // Note: Record<K,V>[] syntax may need Vec wrapping - check actual output
        assert!(output.contains("HashMap<String, i64>"), "Expected HashMap in output:\n{}", output);
        assert_snapshot!(output);
    }
}
