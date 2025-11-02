use std::collections::HashMap;

use lang::{AnalyzedProgram, AstNode, Decorator, DiagnosticContext, Enum, Field, LNode, LSyntaxKind, Model, PrimitiveType, SourceCodeMetadata, SymId, SymTable, Type, TypeAtom};

use crate::{CodeGenError, CodeGenerator, types::CodeGenResult};

pub struct CodeGenJsonSchema;

// TODO: Refactor such that visitors also emit contributions, and similar refs are shared and not inlined
impl CodeGenerator for CodeGenJsonSchema {
    fn generate(&self, program: AnalyzedProgram, source: &SourceCodeMetadata) -> Result<String, crate::CodeGenError> {
        let ast = program.ast_root.clone();
        let mut json = json::object::Object::new();

        json["$schema"] = "https://json-schema.org/draft/2020-12/schema".into();
        json["type"] = "object".into();

        let mut codegen = CodeGeneratorImpl::new(ast, program.symbols.clone(), source);
        let (root_model_name, root_model_obj) = codegen.generate()?;

        // let (model_name, model_obj) = codegen.visit_model(root_model.syntax().clone(), None)?;
        json["title"] = root_model_name.into();
        // Merge JSON and model_obj
        for (k, v) in root_model_obj.entries() {
            json[k] = v.clone();
        }

        for (_, (def_name, def_obj)) in codegen.defs {
            if json.get("$defs").is_none() {
                json["$defs"] = json::object::Object::new().into();
            }
            json["$defs"][&def_name] = def_obj.into();
        }

        sort_json_keys(&mut json);

        Ok(json::stringify_pretty(json, 4))
    }
}

impl Default for CodeGenJsonSchema {
    fn default() -> Self {
        Self::new()
    }
}

impl CodeGenJsonSchema {
    pub fn new() -> Self {
        Self
    }
}

struct CodeGeneratorImpl {
    diag: DiagnosticContext,
    ast: LNode,
    syms: SymTable<LNode>,
    defs: HashMap<SymId, (String, json::object::Object)>,
}

struct CodeGenVisitorContributions<T> {
    data: T,
}

impl CodeGeneratorImpl {
    pub fn new(ast: LNode, syms: SymTable<LNode>, source: &SourceCodeMetadata) -> Self {
        let diag = DiagnosticContext::new(source.file_name, source.file_contents);
        Self {
            diag,
            ast,
            syms,
            defs: Default::default(),
        }
    }

    /// Generates the JSON Schema for the program.
    /// Returns the root model name and its JSON object, along with contributed definitions.
    pub fn generate(&mut self) -> CodeGenResult<(String, json::JsonValue)> {
        let root_model = self
            .ast
            .clone()
            .children_with_tokens()
            .find(|n| n.kind() == LSyntaxKind::MODEL)
            .map(|n| n.into_node().unwrap())
            .filter(|model_node| {
                Model::cast(model_node.clone())
                    .map(|m| m.decorators().iter().any(|d: &Decorator| d.name().as_deref() == Some("root")))
                    .unwrap_or(false)
            })
            .expect("Expected root model");
        let root_model_name = Model::cast(root_model.clone()).and_then(|m| m.ident()).expect("Expected root model to have ident");

        let codegen_result = self.visit_model(root_model, None)?;
        Ok((root_model_name, codegen_result.data.into()))
    }

    fn visit_model(&mut self, node: LNode, parent_sym: Option<SymId>) -> CodeGenResult<CodeGenVisitorContributions<json::object::Object>> {
        let model = Model::cast(node).ok_or(CodeGenError::GeneralError("Expected model node".into()))?;
        let model_name_token = model.ident_token().ok_or(CodeGenError::GeneralError("Expected model to have ident token".into()))?;
        let model_name = model_name_token.text().to_string();

        let current_scope = match parent_sym {
            Some(scope) => self.syms.resolve_id(Some(scope), &model_name),
            None => self.syms.resolve_id(None, &model_name),
        }
        .unwrap_or_else(|| panic!("Unresolved model symbol for model: {}", model_name));

        // Check if model has existing def - if not, contribute to definitions.
        let decorators = model.decorators();
        if decorators.iter().any(|d: &Decorator| d.name().as_deref() == Some("root")) {
            // Root model - do not reference
        } else if let Some((def_name, _)) = self.defs.get(&current_scope) {
            return Ok(CodeGenVisitorContributions {
                data: {
                    let mut obj = json::object::Object::new();
                    obj.insert("$ref", format!("#/definitions/{}", def_name).into());
                    obj
                },
            });
        }

        let mut model_obj = json::object::Object::new();
        model_obj["type"] = "object".into();
        let mut properties_obj = json::object::Object::new();

        let field_nodes = model.field_nodes();
        for field_node in field_nodes {
            let field = Field::cast(field_node).ok_or(CodeGenError::GeneralError("Expected field node".into()))?;
            let field_name_token = field.ident_token().ok_or(CodeGenError::GeneralError("Expected field to have ident token".into()))?;
            let field_name = field_name_token.text().to_string();
            let field_type_node = field.type_node().ok_or(CodeGenError::GeneralError("Expected field to have type node".into()))?;
            let mut field_type_json = self.visit_type(field_type_node.clone(), Some(current_scope))?;
            if let Some(docs) = field.docs() {
                field_type_json["description"] = docs.join("\n").into();
            }
            properties_obj[&field_name] = field_type_json;
        }

        model_obj["properties"] = properties_obj.into();

        // Add to defs
        let sym = self.syms.get(current_scope).expect("Expected symbol entry");
        self.defs.insert(current_scope, (sym.name.clone(), model_obj.clone()));

        Ok(CodeGenVisitorContributions { data: model_obj })
    }

    pub fn visit_enum(&mut self, node: LNode, parent_sym: Option<SymId>) -> CodeGenResult<json::JsonValue> {
        let enum_model = Enum::cast(node).ok_or(CodeGenError::GeneralError("Expected enum node".into()))?;
        let enum_name_token = enum_model.ident_token().ok_or(CodeGenError::GeneralError("Expected enum to have ident token".into()))?;
        let enum_name = enum_name_token.text().to_string();

        let current_scope = match parent_sym {
            Some(scope) => self.syms.resolve_id(Some(scope), &enum_name),
            None => self.syms.resolve_id(None, &enum_name),
        }
        .unwrap_or_else(|| panic!("Unresolved enum symbol for enum: {}", enum_name));

        let variant_nodes = enum_model.variant_nodes();
        let mut variants: Vec<String> = Vec::new();
        for variant_node in variant_nodes {
            let variant_token = variant_node
                .children_with_tokens()
                .find(|t| t.kind() == LSyntaxKind::STRING_LITERAL)
                .and_then(|t| t.into_node())
                .map(|t| t.text().to_string())
                .ok_or(CodeGenError::GenerationErrors(vec![self.diag.error(
                    variant_node.text_range(),
                    format_args!("Expected variant to have string literal"),
                    None,
                )]))?;
            let trimmed = variant_token.trim_matches('"').to_string();
            variants.push(trimmed);
        }

        let mut enum_obj = json::object::Object::new();
        enum_obj.insert("type", "string".into());
        if let Some(docs) = enum_model.docs() {
            enum_obj.insert("description", docs.join("\n").into());
        }
        enum_obj.insert("enum", json::JsonValue::Array(variants.into_iter().map(|v| v.into()).collect()));

        // Add to defs
        let sym = self.syms.get(current_scope).expect("Expected symbol entry");
        self.defs.insert(current_scope, (sym.name.clone(), enum_obj.clone()));

        Ok(enum_obj.into())
    }

    pub fn visit_type(&mut self, node: LNode, parent_sym: Option<SymId>) -> CodeGenResult<json::JsonValue> {
        let type_expr = Type::cast(node).ok_or(CodeGenError::GeneralError("Expected type node".into()))?;
        let type_atom_nodes = type_expr.type_atoms();

        let mut types: Vec<json::JsonValue> = Vec::new();
        for type_atom in type_atom_nodes {
            let type_json = self.visit_type_atom(type_atom.syntax().clone(), parent_sym)?;
            types.push(type_json);
        }

        if types.len() == 1 {
            Ok(types.into_iter().next().unwrap())
        } else {
            Ok(json::JsonValue::Array(types))
        }
    }

    fn visit_type_atom(&mut self, node: LNode, parent_sym: Option<SymId>) -> CodeGenResult<json::JsonValue> {
        let type_atom = TypeAtom::cast(node).ok_or(CodeGenError::GeneralError("Expected type atom node".into()))?;

        if let Some(primitive_type) = type_atom.as_primitive_type() {
            let primitive_type = match primitive_type {
                PrimitiveType::String => Ok("string".into()),
                PrimitiveType::Int => Ok("integer".into()),
                PrimitiveType::Float => Ok("number".into()),
                PrimitiveType::Bool => Ok("boolean".into()),
            };

            let mut type_obj = json::object::Object::new();
            type_obj.insert("type", primitive_type?);
            Ok(type_obj.into())
        } else {
            let ref_name = type_atom
                .ident_token()
                .ok_or(CodeGenError::GeneralError("Expected type atom to have ident token".into()))?
                .text()
                .to_string();
            let sym_id = self.syms.resolve_id(parent_sym, &ref_name).expect("Unresolved symbol");
            if self.defs.contains_key(&sym_id) {
                Ok(self.format_ref(parent_sym, &ref_name).into())
            } else {
                // Def not generated - generate it.
                let ref_sym = self
                    .syms
                    .resolve(parent_sym, &ref_name)
                    .ok_or(CodeGenError::GeneralError(format!("Unresolved type reference: {}", ref_name)))?;
                let ref_node = ref_sym.data;

                match ref_node.kind() {
                    LSyntaxKind::MODEL => {
                        // Visit the model, which also adds it to refs.
                        self.visit_model(ref_node.clone(), parent_sym)?;
                        Ok(self.format_ref(parent_sym, &ref_name).into())
                    }
                    LSyntaxKind::ENUM => {
                        // Visit the enum, which also adds it to refs.
                        self.visit_enum(ref_node.clone(), parent_sym)?;
                        Ok(self.format_ref(parent_sym, &ref_name).into())
                    }
                    _ => Err(CodeGenError::GeneralError(format!("Unsupported type reference kind: {:?}", ref_node.kind()))),
                }
            }
        }
    }

    fn format_ref(&mut self, parent_scope: Option<SymId>, ref_name: &str) -> json::object::Object {
        let ref_sym = self.syms.resolve(parent_scope, ref_name).expect("Unresolved ref");
        let ref_path = format!("#/$defs/{}", ref_sym.name).to_string();
        let mut ref_obj = json::object::Object::new();
        ref_obj.insert("$ref", ref_path.into());
        ref_obj
    }
}

fn sort_json_keys(obj: &mut json::object::Object) {
    let mut sorted_entries: Vec<_> = obj.iter().map(|(k, v)| (k.to_string(), v.clone())).collect();

    // TODO: Make this nicer...
    sorted_entries.sort_by(|a, b| match (a.0.as_str(), b.0.as_str()) {
        ("$schema", _) => std::cmp::Ordering::Less,
        (_, "$schema") => std::cmp::Ordering::Greater,
        ("$id", _) => std::cmp::Ordering::Less,
        (_, "$id") => std::cmp::Ordering::Greater,
        ("title", _) => std::cmp::Ordering::Less,
        (_, "title") => std::cmp::Ordering::Greater,
        ("type", _) => std::cmp::Ordering::Less,
        (_, "type") => std::cmp::Ordering::Greater,
        ("properties", _) => std::cmp::Ordering::Less,
        (_, "properties") => std::cmp::Ordering::Greater,
        _ => a.0.cmp(&b.0),
    });

    obj.clear();
    for (key, mut value) in sorted_entries.iter().map(|(k, v)| (k.to_string(), v.clone())) {
        if let json::JsonValue::Object(ref mut inner_obj) = value {
            sort_json_keys(inner_obj);
        }
        obj.insert(&key, value);
    }
}

#[cfg(test)]
mod tests {
    use crate::test_utils::analyze_glue_file;
    use indoc::indoc;
    use insta::assert_snapshot;
    use lang::print_report;

    use super::*;

    #[test]
    fn test() {
        let src = indoc! { r#"
				@root
				model GlueConfigSchema {
					/// Configuration for code generation (`glue gen [...]`)
					@default
					generation: Generation

					model Generation {
						/// Mode for the watermark at the top of the generated files
						watermark: Watermark = "short"
						enum Watermark: "full" | "short" | "none"

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

        let codegen = CodeGenJsonSchema::new();
        let output = codegen
            .generate(program, &source)
            .map_err(|e| match e {
                CodeGenError::GeneralError(msg) => msg,
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
