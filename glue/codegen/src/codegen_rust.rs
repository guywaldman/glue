use config::GlueConfig;
use convert_case::Casing;
use lang::{
    AnalyzedProgram, AstNode, DiagnosticContext, Enum, EnumVariant, Field, LNode, LSyntaxKind, Literal, MODEL_FIELD_DECORATOR, MODEL_FIELD_DECORATOR_ALIAS_ARG, Model, PrimitiveType,
    SourceCodeMetadata, SymId, SymTable, Type, TypeAtom,
};

use crate::{CodeGenError, CodeGenerator, codegen::CodeGenResult, codegen_utils::qualified_symbol_name_to_case};

pub struct CodeGenRust;

// TODO: Refactor such that visitors also emit contributions, and similar refs are shared and not inlined
impl CodeGenerator for CodeGenRust {
    fn generate(&self, program: AnalyzedProgram, source: &SourceCodeMetadata, _config: Option<GlueConfig>) -> Result<String, crate::CodeGenError> {
        let ast = program.ast_root.clone();
        let mut codegen = CodeGeneratorImpl::new(ast, program.symbols, source);
        let output = codegen.generate()?;
        Ok(output)
    }
}

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

struct CodeGeneratorImpl {
    #[allow(dead_code)]
    diag: DiagnosticContext,
    ast: LNode,
    syms: SymTable<LNode>,
    postludes: Vec<String>,
}

impl CodeGeneratorImpl {
    pub fn new(ast: LNode, syms: SymTable<LNode>, source: &SourceCodeMetadata) -> Self {
        let diag = DiagnosticContext::new(source.file_name, source.file_contents);
        Self {
            diag,
            ast,
            syms,
            postludes: Default::default(),
        }
    }

    pub fn generate(&mut self) -> CodeGenResult<String> {
        let mut output = String::new();

        output.push_str(&Self::PRELUDES.join("\n"));
        output.push_str("\n\n");

        for node in self.ast.children() {
            match node.kind() {
                LSyntaxKind::MODEL => {
                    let model_code = self.visit_model(node.clone(), None)?;
                    output.push_str(&model_code);
                }
                LSyntaxKind::ENUM => {
                    let enum_code = self.visit_enum(node.clone(), None)?;
                    output.push_str(&enum_code);
                }
                _ => {}
            }
        }

        for postlude in &self.postludes {
            output.push_str(postlude);
        }

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

        // How to emit defaults is tricky:
        // Since Serde does not allow to properly define `#[serde(default = "MyEnum::Variant")]`` for enum fields,
        // we need to implement default ourselves in these cases.
        // Otherwise, we can just add a default derive for structs, and for fields which reference structs with defaults, we can use `#[serde(default)]`.
        // TODO: Optimize, to avoid several passes.
        // TODO: Handle defaults
        // let mut attribute = String::new();
        // if let (true, has_enum) = self.all_model_fields_have_defaults(&model, parent_scope)? {
        //     if !has_enum {
        //         attribute.push_str("#[derive(Serialize, Deserialize, Debug, Clone, Default)]\n");
        //     } else {
        //         // As mentioned above, need to implement Default manually
        //         attribute.push_str("#[derive(Serialize, Deserialize, Debug, Clone)]\n");
        //         model_postludes.push_str(&format!("impl Default for {} {{\n", qualified_model_name));
        //         model_postludes.push_str("    fn default() -> Self {\n");
        //         model_postludes.push_str("        Self {\n");

        //         for field_node in model.field_nodes() {
        //             let field = Field::cast(field_node.clone()).ok_or(CodeGenError::InternalError("Expected Field node".to_string()))?;
        //             let field_name = field.ident().ok_or(CodeGenError::InternalError("Field missing ident".to_string()))?;
        //             let field_scope = self
        //                 .syms
        //                 .resolve(Some(current_scope.id), &field_name)
        //                 .ok_or_else(|| CodeGenError::InternalError(format!("Unresolved field symbol for field: {}", field_name)))?;
        //             let field_type_node = field.type_node().ok_or(CodeGenError::InternalError("Field missing type".to_string()))?;
        //             let field_type = Type::cast(field_type_node).ok_or(CodeGenError::InternalError("Expected Type node".to_string()))?;
        //             if let Some(ref_name) = field_type.as_single_ref() {
        //                 let ref_sym = self
        //                     .syms
        //                     .resolve(Some(current_scope.id), &ref_name)
        //                     .ok_or_else(|| CodeGenError::InternalError(format!("Unresolved type symbol for type: {}", ref_name)))?;
        //                 let ref_node = ref_sym.data.clone();
        //                 if ref_node.kind() == LSyntaxKind::MODEL {
        //                     // References a model - if the model has defaults, use `Default::default()`
        //                     let (all_fields_have_defaults, _) = self.all_model_fields_have_defaults(&Model::cast(ref_node).unwrap(), Some(current_scope.id))?;
        //                     if all_fields_have_defaults {
        //                         model_postludes.push_str(&format!("        {}: Default::default(),\n", field_name));
        //                         continue;
        //                     }
        //                 }
        //             }

        //             // Not a model
        //             let field_qualified_name = qualified_symbol_name_to_case(&field_scope.name, convert_case::Case::Snake);
        //             let default_fn_name = format!("default_{}", field_qualified_name);
        //             model_postludes.push_str(&format!("        {}: {}(),\n", field_name, default_fn_name));
        //         }

        //         model_postludes.push_str("    }\n");
        //         model_postludes.push_str("  }\n");
        //         model_postludes.push_str("}\n\n");
        //     }
        // }

        if let Some(docs) = model.docs() {
            output.push_str(&Self::emit_docs(docs, 0));
        }
        // TODO: Not all structs can derive Default
        output.push_str("#[derive(serde::Serialize, serde::Deserialize, Debug, Clone, Default)]\n");
        output.push_str(&format!("pub struct {} {{\n", qualified_model_name));

        for field_node in model.field_nodes() {
            let field_code = self.visit_field(field_node.clone(), Some(current_scope.id))?;
            output.push_str(&field_code);
        }

        for nested_model in model.nested_model_nodes() {
            let nested_model_code = self.visit_model(nested_model.clone(), Some(current_scope.id))?;
            self.postludes.push(nested_model_code.clone());
            output.push('\n');
        }

        for nested_enum in model.nested_enum_nodes() {
            let nested_enum_code = self.visit_enum(nested_enum.clone(), Some(current_scope.id))?;
            self.postludes.push(nested_enum_code.clone());
            output.push('\n');
        }

        output.push_str("}\n\n");

        output.push('\n');

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

        if let Some(docs) = enum_model.docs() {
            output.push_str(&Self::emit_docs(docs, 0));
        }

        output.push_str("#[derive(serde::Serialize, serde::Deserialize, Debug, Clone, PartialEq, Eq)]\n");
        output.push_str(&format!("pub enum {} {{\n", qualified_enum_name));

        let variant_nodes = enum_model.variant_nodes();
        for variant_node in variant_nodes {
            let enum_variant = EnumVariant::cast(variant_node.clone()).ok_or(CodeGenError::InternalError("Expected EnumVariant node".to_string()))?;
            let enum_variant_value = enum_variant.value().ok_or(CodeGenError::InternalError("EnumVariant missing value".to_string()))?;
            let enum_variant_value_rustified = enum_variant_value.to_case(convert_case::Case::Pascal);

            if let Some(docs) = enum_variant.docs() {
                output.push_str(&Self::emit_docs(docs, 1));
            }

            output.push_str(&format!("    #[serde(rename = \"{}\")]\n", enum_variant_value));
            output.push_str(&format!("    {},\n", enum_variant_value_rustified));
        }

        output.push_str("}\n\n");
        Ok(output)
    }

    fn visit_field(&mut self, node: LNode, parent_scope: Option<SymId>) -> CodeGenResult<String> {
        let mut output = String::new();
        let field = Field::cast(node).ok_or(CodeGenError::InternalError("Expected Field node".to_string()))?;
        let field_name = field.ident().ok_or(CodeGenError::InternalError("Field missing ident".to_string()))?;
        let field_type_node = field.type_node().ok_or(CodeGenError::InternalError("Field missing type".to_string()))?;
        let field_scope = self.syms.resolve(parent_scope, &field_name).map(|s| s.id);

        // TODO: Handle defaults
        // if let Some(field_default_value_node) = field.default_literal_expr_node() {
        //     let field_default_value = LiteralExpr::cast(field_default_value_node).expect("Expected ConstExpr node");

        //     let field_type = Type::cast(field_type_node.clone()).ok_or(CodeGenError::InternalError("Expected Type node".to_string()))?;

        //     let mut default_value_code = self.emit_literal(parent_scope, field_default_value.clone())?;

        //     // If the field type is an enum variant, we need to emit the default differently
        //     // TODO: Support unions
        //     if let Some(field_type_ref_name) = field_type.as_single_ref() {
        //         let ref_sym = self
        //             .syms
        //             .resolve(parent_scope, &field_type_ref_name)
        //             .ok_or_else(|| CodeGenError::InternalError(format!("Unresolved type symbol for type: {}", field_type_ref_name)))?;
        //         let ref_node = ref_sym.data.clone();
        //         if ref_node.kind() == LSyntaxKind::ENUM {
        //             let qualified_enum_name = qualified_symbol_name_to_case(&ref_sym.name, convert_case::Case::Pascal);
        //             let variant_literal = field_default_value.value().ok_or(CodeGenError::InternalError("ConstExpr has no value".to_string()))?;
        //             let Literal::StringLiteral(variant_literal) = variant_literal else {
        //                 return Err(CodeGenError::InternalError("Enum default value is not a string literal".to_string()));
        //             };
        //             default_value_code = format!("{}::{}", qualified_enum_name, variant_literal.to_case(convert_case::Case::Pascal));
        //         }
        //     }
        //     let field_scope = self
        //         .syms
        //         .resolve(parent_scope, &field_name)
        //         .ok_or_else(|| CodeGenError::InternalError(format!("Unresolved field symbol for field: {}", field_name)))?;
        //     let field_qualified_name = qualified_symbol_name_to_case(&field_scope.name, convert_case::Case::Snake);
        //     let default_fn_name = format!("default_{}", field_qualified_name);
        //     output.push_str(&format!("    #[serde(default = \"{}\")]\n", default_fn_name));
        //     let field_type_code = self.visit_type(field.type_node().unwrap(), parent_scope)?;
        //     let default_value_code = format!("fn {}() -> {} {{ {} }}\n", default_fn_name, field_type_code, default_value_code);
        //     self.postludes.push(default_value_code);
        // }

        if let Some(docs) = field.docs() {
            output.push_str(&Self::emit_docs(docs, 1));
        }

        let mut alias = None;
        let decorators = field.decorators();
        let field_decorator = decorators.iter().find(|dec| dec.ident() == Some(MODEL_FIELD_DECORATOR.id.to_owned()));
        if let Some(field_decorator) = field_decorator
            && let Some(alias_arg) = field_decorator.arg(MODEL_FIELD_DECORATOR, &MODEL_FIELD_DECORATOR_ALIAS_ARG)
        {
            let alias_value = alias_arg.literal().ok_or(CodeGenError::InternalError("Missing alias argument value".to_string()))?;
            match alias_value {
                Literal::StringLiteral(node) => {
                    alias = Some(node.value().expect("Expected string literal value").to_string());
                }
                _ => {
                    return Err(CodeGenError::InternalError("Alias argument must be a string literal".to_string()));
                }
            }
        }
        if let Some(alias) = alias {
            output.push_str(&format!("    #[serde(rename = \"{}\")]\n", alias));
        }

        let mut field_type_code = self.visit_type(field_type_node.clone(), field_scope)?;
        if field.is_optional() {
            output.push_str("    #[serde(skip_serializing_if = \"Option::is_none\")]\n");
            field_type_code = format!("Option<{}>", field_type_code);
        }

        // Rust keyword escaping
        let emitted_field_name = match field_name.as_str() {
            "type" => "r#type",
            "ref" => "r#ref",
            other => other,
        };
        output.push_str(&format!("    pub {}: {},\n", emitted_field_name, field_type_code));
        Ok(output)
    }

    fn visit_type(&mut self, node: LNode, parent_scope: Option<SymId>) -> CodeGenResult<String> {
        let ty = Type::cast(node).ok_or(CodeGenError::InternalError("Expected Type node".to_string()))?;

        let mut types = Vec::new();
        for atom_node in &ty.type_atom_nodes() {
            let type_atom_code = self.visit_type_atom(atom_node.clone(), parent_scope)?;
            types.push(type_atom_code);
        }

        Ok(types.join(" | "))
    }

    fn visit_type_atom(&mut self, node: LNode, parent_scope: Option<SymId>) -> CodeGenResult<String> {
        let atom = TypeAtom::cast(node).ok_or(CodeGenError::InternalError("Expected TypeAtom node".to_string()))?;

        if let Some(primitive) = atom.as_primitive_type() {
            match primitive {
                // TODO: Make `any` mapping configurable
                PrimitiveType::Any => Ok("serde_json::Value".to_string()),
                PrimitiveType::String => Ok("String".to_string()),
                PrimitiveType::Int => Ok("i64".to_string()),
                PrimitiveType::Float => Ok("f64".to_string()),
                PrimitiveType::Bool => Ok("bool".to_string()),
            }
        } else if let Some(anon_model) = atom.as_anon_model() {
            let Some(parent_scope_node) = self.syms.get(parent_scope.expect("Expected parent scope")) else {
                return Err(CodeGenError::InternalError("Parent scope not found".to_string()));
            };
            let parent_scope_name = qualified_symbol_name_to_case(&parent_scope_node.name, convert_case::Case::Pascal);
            let anon_model_name = parent_scope_name;
            todo!();
        } else if let Some(record_type) = atom.as_record_type() {
            // Record
            let src_type = record_type.src_type_node().ok_or(CodeGenError::InternalError("Record missing source type".to_string()))?;
            let src_type_code = self.visit_type(src_type, parent_scope)?;
            let dest_type = record_type.dest_type_node().ok_or(CodeGenError::InternalError("Record missing destination type".to_string()))?;
            let dest_type_code = self.visit_type(dest_type, parent_scope)?;
            Ok(format!("HashMap<{}, {}>", src_type_code, dest_type_code))
        } else {
            let ident_token = atom.as_ref_token().ok_or(CodeGenError::InternalError("TypeAtom missing ident".to_string()))?;
            let text_string = ident_token.to_string();
            let ref_ident = text_string.trim();
            let ref_sym = self
                .syms
                .resolve(parent_scope, ref_ident)
                .ok_or_else(|| CodeGenError::InternalError(format!("Unresolved type symbol for type: {}", ref_ident)))?;
            let qualified_ref_name = qualified_symbol_name_to_case(&ref_sym.name, convert_case::Case::Pascal);
            Ok(qualified_ref_name.to_string())
        }
    }

    // fn emit_literal(&mut self, _scope: Option<SymId>, constexpr: LiteralExpr) -> CodeGenResult<String> {
    //     match constexpr.value() {
    //         Some(value) => match value {
    //             Literal::StringLiteral(s) => Ok(format!("\"{}\".to_string()", s)),
    //             Literal::IntLiteral(i) => Ok(format!("{}", i)),
    //             Literal::BoolLiteral(b) => Ok(format!("{}", b)),
    //             Literal::FloatLiteral(f) => Ok(format!("{}", f)),
    //         },
    //         None => Err(CodeGenError::InternalError("ConstExpr has no value".to_string())),
    //     }
    // }

    fn emit_docs(lines: Vec<String>, indent_level: usize) -> String {
        let mut output = String::new();
        let indent = " ".repeat(indent_level * 4);
        for line in lines {
            output.push_str(&format!("{}/// {}\n", indent, line.trim()));
        }
        output
    }

    // TODO: Handle defaults
    // // Returns whether all fields have defaults, and if so, whether it has an enum with a default.
    // fn all_model_fields_have_defaults(&mut self, model: &Model, parent_scope: Option<SymId>) -> CodeGenResult<(bool, bool)> {
    //     let mut has_enum = false;

    //     let model_scope_id = self
    //         .syms
    //         .resolve_id(parent_scope, &model.ident().ok_or(CodeGenError::InternalError("Model missing ident".to_string()))?)
    //         .ok_or_else(|| CodeGenError::InternalError(format!("Unresolved model symbol for model: {}", model.ident().unwrap())))?;
    //     for field_node in model.field_nodes() {
    //         let field = Field::cast(field_node.clone()).ok_or(CodeGenError::InternalError("Expected Field node".to_string()))?;
    //         let field_type_node = field.type_node().ok_or(CodeGenError::InternalError("Field missing type".to_string()))?;
    //         let field_type = Type::cast(field_type_node).ok_or(CodeGenError::InternalError("Expected Type node".to_string()))?;

    //         // TODO: Support unions
    //         if let Some(ref_name) = field_type.as_single_ref() {
    //             let ref_sym = self
    //                 .syms
    //                 .resolve(Some(model_scope_id), &ref_name)
    //                 .ok_or_else(|| CodeGenError::InternalError(format!("Unresolved type symbol for type: {}", ref_name)))?;
    //             let ref_node = ref_sym.data.clone();
    //             if ref_node.kind() == LSyntaxKind::ENUM {
    //                 has_enum = true;
    //             } else if let Some(ref_model) = Model::cast(ref_node) {
    //                 let (all_fields_have_defaults, _) = self.all_model_fields_have_defaults(&ref_model, Some(model_scope_id))?;
    //                 if !all_fields_have_defaults {
    //                     return Ok((false, false));
    //                 }
    //             }
    //         } else if field.default_literal_expr_node().is_none() {
    //             // No default - but if it's optional, that's fine
    //             if field.is_optional() {
    //                 continue;
    //             }
    //             return Ok((false, false));
    //         }
    //     }

    //     Ok((true, has_enum))
    // }

    const PRELUDES: [&'static str; 1] = ["use std::collections::HashMap;"];
}

#[cfg(test)]
mod tests {
    use super::*;
    use indoc::indoc;
    use insta::assert_snapshot;
    use lang::print_report;

    use crate::{CodeGenError, CodeGenerator, test_utils::analyze_test_glue_file};

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

        let (program, source) = analyze_test_glue_file(src);

        let codegen = CodeGenRust::new();
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
                e => panic!("Unexpected error: {:?}", e),
            })
            .unwrap();

        assert_snapshot!(output);
    }
}
