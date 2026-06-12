use std::collections::HashMap;

use config::GlueConfigSchemaGeneration;
use lang::{AnonModel, AstNode, Decorator, DiagnosticContext, Enum, Field, GlueIr, LNode, LSyntaxKind, Model, PrimitiveType, SourceCodeMetadata, SymId, SymTable, Type, TypeAtom};

use crate::{CodeGenError, CodeGenerator, codegen::CodeGenResult};

#[derive(Default)]
pub struct CodeGenJsonSchema;

// TODO: Refactor such that visitors also emit contributions, and similar refs are shared and not inlined
impl CodeGenerator for CodeGenJsonSchema {
    fn generate(&self, ir: GlueIr, source: &SourceCodeMetadata, _config: Option<GlueConfigSchemaGeneration>) -> Result<String, crate::CodeGenError> {
        let program = ir
            .into_analyzed_program()
            .ok_or_else(|| CodeGenError::InternalError("Glue IR does not contain an analyzed program".to_string()))?;
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

    /// Returns the root model name and its JSON object, along with contributed definitions.
    pub fn generate(&mut self) -> CodeGenResult<(String, json::JsonValue)> {
        let models = self
            .ast
            .clone()
            .children_with_tokens()
            .filter(|n| n.kind() == LSyntaxKind::MODEL)
            .filter_map(|n| n.into_node())
            .collect::<Vec<LNode>>();

        let root_models = models
            .iter()
            .filter(|m| {
                let model = Model::cast((*m).clone()).expect("Expected model node");
                model.decorators().iter().any(|d: &Decorator| d.ident().as_deref() == Some("root"))
            })
            .cloned()
            .collect::<Vec<LNode>>();

        let root_model = if !root_models.is_empty() {
            if root_models.len() > 1 {
                return Err(CodeGenError::GenerationError(self.diag.error(
                    self.ast.text_range(),
                    "Multiple root models found in the program. Only one root model is supported. Decorate only one model with `@root`.",
                )));
            }
            root_models.first().cloned()
        } else {
            if models.len() > 1 {
                let mut labels = Vec::new();
                for m in models.iter() {
                    let model = Model::cast(m.clone()).expect("Expected model node");
                    if let Some(ident_token) = model.ident_token() {
                        labels.push(self.diag.labeled_span(ident_token.text_range(), "Consider adding `@root` decorator to this model"));
                    }
                }
                return Err(CodeGenError::GenerationError(self.diag.error_with_labels(
                    self.ast.text_range(),
                    "Multiple models found in the program, but no root model specified. Please decorate one model with `@root`.",
                    Some("Add a `@root` decorator to one of the models."),
                    None,
                    labels,
                )));
            }
            models.first().cloned()
        }
        .unwrap();

        let root_model_name = Model::cast(root_model.clone()).and_then(|m| m.ident()).expect("Expected root model to have ident");

        let codegen_result = self.visit_model(root_model.clone(), None)?;
        Ok((root_model_name, codegen_result.data.into()))
    }

    fn visit_model(&mut self, node: LNode, parent_sym: Option<SymId>) -> CodeGenResult<CodeGenVisitorContributions<json::object::Object>> {
        let model = Model::cast(node).ok_or(CodeGenError::InternalError("Expected model node".into()))?;
        let model_name_token = model.ident_token().ok_or(CodeGenError::InternalError("Expected model to have ident token".into()))?;
        let model_name = model_name_token.text().to_string();

        let current_scope = self
            .syms
            .resolve_id(parent_sym, &model_name)
            .unwrap_or_else(|| panic!("Unresolved model symbol for model: {}", model_name));

        // Check if model has existing def - if not, contribute to definitions.
        let decorators = model.decorators();
        if decorators.iter().any(|d: &Decorator| d.ident().as_deref() == Some("root")) {
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
            let field = Field::cast(field_node).ok_or(CodeGenError::InternalError("Expected field node".into()))?;
            let field_name = field.ident().ok_or(CodeGenError::InternalError("Expected field to have ident".into()))?;
            let field_type_node = field.type_node().ok_or(CodeGenError::InternalError("Expected field to have type node".into()))?;
            let mut field_type_json = self.visit_type(field_type_node.clone(), Some(current_scope))?;
            if let Some(docs) = field.docs() {
                field_type_json["description"] = docs.join("\n").trim().into();
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
        let enum_model = Enum::cast(node).ok_or(CodeGenError::InternalError("Expected enum node".into()))?;
        let enum_name_token = enum_model.ident_token().ok_or(CodeGenError::InternalError("Expected enum to have ident token".into()))?;
        let enum_name = enum_name_token.text().to_string();

        let current_scope = self.syms.resolve_id(parent_sym, &enum_name).unwrap_or_else(|| panic!("Unresolved enum symbol for enum: {}", enum_name));

        let variant_nodes = enum_model.variant_nodes();
        let mut variants: Vec<String> = Vec::new();
        for variant_node in variant_nodes {
            let variant_token = variant_node
                .children_with_tokens()
                .find(|t| t.kind() == LSyntaxKind::STRING_LITERAL)
                .and_then(|t| t.into_node())
                .map(|t| t.text().to_string())
                .ok_or(CodeGenError::GenerationErrors(vec![
                    self.diag.error(variant_node.text_range(), "Expected variant to have string literal"),
                ]))?;
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
        let type_expr = Type::cast(node).ok_or(CodeGenError::InternalError("Expected type node".into()))?;
        let type_atom_nodes = type_expr.type_atoms();

        let mut types: Vec<json::JsonValue> = Vec::new();
        for type_atom in type_atom_nodes {
            let type_json = self.visit_type_atom(type_atom.syntax().clone(), parent_sym)?;
            types.push(type_json);
        }

        if types.len() == 1 {
            Ok(types.into_iter().next().unwrap())
        } else {
            let mut union_obj = json::object::Object::new();
            union_obj.insert("anyOf", json::JsonValue::Array(types));
            Ok(union_obj.into())
        }
    }

    fn visit_type_atom(&mut self, node: LNode, parent_sym: Option<SymId>) -> CodeGenResult<json::JsonValue> {
        let type_atom = TypeAtom::cast(node).ok_or(CodeGenError::InternalError("Expected type atom node".into()))?;

        if let Some(primitive_type) = type_atom.as_primitive_type() {
            let primitive_type = match primitive_type {
                PrimitiveType::Any => Ok("object".into()),
                PrimitiveType::String => Ok("string".into()),
                PrimitiveType::Int => Ok("integer".into()),
                PrimitiveType::Float => Ok("number".into()),
                PrimitiveType::Bool => Ok("boolean".into()),
            };

            let mut type_obj = json::object::Object::new();
            if type_atom.is_array() {
                type_obj.insert("type", "array".into());
                let mut items_obj = json::object::Object::new();
                items_obj.insert("type", primitive_type?);
                type_obj.insert("items", items_obj.into());
            } else {
                type_obj.insert("type", primitive_type?);
            }
            Ok(type_obj.into())
        } else if let Some(anon_model) = type_atom.anon_model() {
            let mut type_obj = self.visit_anon_model(&anon_model, parent_sym)?;
            if type_atom.is_array() {
                let mut array_obj = json::object::Object::new();
                array_obj.insert("type", "array".into());
                array_obj.insert("items", type_obj.into());
                type_obj = array_obj;
            }
            Ok(type_obj.into())
        } else {
            let ref_name = type_atom
                .as_ref_token()
                .ok_or(CodeGenError::InternalError("Expected type atom to have ident token".into()))?
                .text()
                .to_string();
            if let Some(ref_sym) = self.syms.resolve(parent_sym, &ref_name)
                && ref_sym.data.kind() == LSyntaxKind::TYPE_ALIAS
            {
                let alias = lang::TypeAlias::cast(ref_sym.data.clone()).ok_or(CodeGenError::InternalError("Expected type alias node".into()))?;
                let alias_type_node = alias.type_node().ok_or(CodeGenError::InternalError("Type alias missing type expression".into()))?;
                let alias_json = self.visit_type(alias_type_node, parent_sym)?;
                return Ok(self.wrap_if_array(&type_atom, alias_json));
            }
            let sym_id = self.syms.resolve_id(parent_sym, &ref_name).expect("Unresolved symbol");
            let type_json = if self.defs.contains_key(&sym_id) {
                self.format_ref(parent_sym, &ref_name).into()
            } else {
                // Def not generated - generate it.
                let ref_sym = self
                    .syms
                    .resolve(parent_sym, &ref_name)
                    .ok_or(CodeGenError::InternalError(format!("Unresolved type reference: {}", ref_name)))?;
                let ref_node = ref_sym.data;

                match ref_node.kind() {
                    LSyntaxKind::MODEL => {
                        // Visit the model, which also adds it to refs.
                        self.visit_model(ref_node.clone(), parent_sym)?;
                        self.format_ref(parent_sym, &ref_name).into()
                    }
                    LSyntaxKind::ENUM => {
                        // Visit the enum, which also adds it to refs.
                        self.visit_enum(ref_node.clone(), parent_sym)?;
                        self.format_ref(parent_sym, &ref_name).into()
                    }
                    _ => return Err(CodeGenError::InternalError(format!("Unsupported type reference kind: {:?}", ref_node.kind()))),
                }
            };
            Ok(self.wrap_if_array(&type_atom, type_json))
        }
    }

    fn visit_anon_model(&mut self, anon_model: &AnonModel, parent_sym: Option<SymId>) -> CodeGenResult<json::object::Object> {
        let mut model_obj = json::object::Object::new();
        model_obj["type"] = "object".into();
        let mut properties_obj = json::object::Object::new();

        for field in anon_model.fields() {
            let field_name = field.ident().ok_or(CodeGenError::InternalError("Expected field to have ident".into()))?;
            let field_type_node = field.type_node().ok_or(CodeGenError::InternalError("Expected field to have type node".into()))?;
            let mut field_type_json = self.visit_type(field_type_node, parent_sym)?;
            if let Some(docs) = field.docs() {
                field_type_json["description"] = docs.join("\n").trim().into();
            }
            properties_obj[&field_name] = field_type_json;
        }

        model_obj["properties"] = properties_obj.into();
        Ok(model_obj)
    }

    fn format_ref(&mut self, parent_scope: Option<SymId>, ref_name: &str) -> json::object::Object {
        let ref_sym = self.syms.resolve(parent_scope, ref_name).expect("Unresolved ref");
        let ref_path = format!("#/$defs/{}", ref_sym.name).to_string();
        let mut ref_obj = json::object::Object::new();
        ref_obj.insert("$ref", ref_path.into());
        ref_obj
    }

    fn wrap_if_array(&self, atom: &TypeAtom, schema: json::JsonValue) -> json::JsonValue {
        if !atom.is_array() {
            return schema;
        }

        let mut array_obj = json::object::Object::new();
        array_obj.insert("type", "array".into());
        array_obj.insert("items", schema);
        array_obj.into()
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
    use crate::test_utils::gen_test;
    use indoc::indoc;
    use insta::assert_snapshot;

    use super::*;

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

        assert_snapshot!(gen_test(&CodeGenJsonSchema, src));
    }

    #[test]
    fn test_anonymous_struct() {
        let src = indoc! { r#"
            @root
            model User {
                profile: {
                    /// Short biography
                    bio: string
                    age?: int
                }
            }
        "# };

        assert_snapshot!(gen_test(&CodeGenJsonSchema, src));
    }

    #[test]
    fn array_refs_emit_items_schema() {
        let src = indoc! { r#"
            @root
            model Config {
                entries: Entry[]
            }

            model Entry {
                id: string
            }
        "# };

        let output = gen_test(&CodeGenJsonSchema, src);
        let schema: serde_json::Value = serde_json::from_str(&output).expect("schema should be valid JSON");

        assert_eq!(schema["properties"]["entries"]["type"], "array");
        assert_eq!(schema["properties"]["entries"]["items"]["$ref"], "#/$defs/Entry");
    }

    #[test]
    fn generated_config_schema_accepts_string_and_list_files_config() {
        let schema_src = include_str!("../../assets/config_schema.glue");
        let schema_json = gen_test(&CodeGenJsonSchema, schema_src);
        let schema: serde_json::Value = serde_json::from_str(&schema_json).expect("schema should be valid JSON");
        let config: serde_json::Value = serde_yaml::from_str(indoc! { r#"
            global:
              output_base_dir: generated

            gen:
              - mode: typescript
                files: "models/*.glue"
                output: "ts/models.{file_ext}"

              - mode: python
                files:
                  - "schemas/*.glue"
                  - "shared/*.glue"
                output: "py/{file_name}.{file_ext}"
                config_overrides:
                  typescript:
                    zod: true
        "# })
        .expect("config should be valid YAML");

        assert_json_matches_schema(&schema, &schema, &config, "$").expect("config should conform to generated schema");
    }

    fn assert_json_matches_schema(root: &serde_json::Value, schema: &serde_json::Value, value: &serde_json::Value, path: &str) -> Result<(), String> {
        if let Some(ref_path) = schema.get("$ref").and_then(|v| v.as_str()) {
            let pointer = ref_path.strip_prefix('#').ok_or_else(|| format!("Unsupported ref at {}: {}", path, ref_path))?;
            let resolved = root.pointer(pointer).ok_or_else(|| format!("Unresolved ref at {}: {}", path, ref_path))?;
            return assert_json_matches_schema(root, resolved, value, path);
        }

        if let Some(any_of) = schema.get("anyOf").and_then(|v| v.as_array()) {
            if any_of.iter().any(|candidate| assert_json_matches_schema(root, candidate, value, path).is_ok()) {
                return Ok(());
            }
            return Err(format!("Value at {} does not match any anyOf schema: {}", path, value));
        }

        if let Some(enum_values) = schema.get("enum").and_then(|v| v.as_array())
            && !enum_values.contains(value)
        {
            return Err(format!("Value at {} is not in enum: {}", path, value));
        }

        let Some(schema_type) = schema.get("type").and_then(|v| v.as_str()) else {
            return Ok(());
        };

        match schema_type {
            "object" => {
                let object = value.as_object().ok_or_else(|| format!("Expected object at {}, got {}", path, value))?;
                if let Some(properties) = schema.get("properties").and_then(|v| v.as_object()) {
                    for (key, property_schema) in properties {
                        if let Some(property_value) = object.get(key) {
                            assert_json_matches_schema(root, property_schema, property_value, &format!("{}.{}", path, key))?;
                        }
                    }
                }
            }
            "array" => {
                let array = value.as_array().ok_or_else(|| format!("Expected array at {}, got {}", path, value))?;
                if let Some(items_schema) = schema.get("items") {
                    for (index, item) in array.iter().enumerate() {
                        assert_json_matches_schema(root, items_schema, item, &format!("{}[{}]", path, index))?;
                    }
                }
            }
            "string" if !value.is_string() => return Err(format!("Expected string at {}, got {}", path, value)),
            "integer" if !value.is_i64() && !value.is_u64() => return Err(format!("Expected integer at {}, got {}", path, value)),
            "number" if !value.is_number() => return Err(format!("Expected number at {}, got {}", path, value)),
            "boolean" if !value.is_boolean() => return Err(format!("Expected boolean at {}, got {}", path, value)),
            _ => {}
        }

        Ok(())
    }
}
