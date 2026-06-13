use config::GlueConfigSchemaGeneration;
use convert_case::Case;
use lang::{AnonModel, AstNode, ConstDef, ConstValue, Enum, Field, GlueIr, Model, SourceCodeMetadata, SymId, Type, TypeAtom};

use crate::{
    CodeGenError, CodeGenerator,
    codegen::CodeGenResult,
    context::{AnonymousTypeNamer, CodeGenContext, DocEmitter, FieldExt, NamedExt, TypeMapper, convert_generated_identifier_case},
};

#[derive(Default)]
pub struct CodeGenRust;

impl CodeGenerator for CodeGenRust {
    fn generate(&self, ir: GlueIr, source: &SourceCodeMetadata, config: Option<GlueConfigSchemaGeneration>) -> Result<String, CodeGenError> {
        let program = ir
            .into_analyzed_program()
            .ok_or_else(|| CodeGenError::InternalError("Glue IR does not contain an analyzed program".to_string()))?;
        let lint_suppressions = config.as_ref().and_then(|g| g.lint_suppressions).unwrap_or(true);
        let serde_struct_derives = config.as_ref().and_then(|g| g.rust.as_ref()).and_then(|r| r.serde_struct_derives).unwrap_or(true);
        let ctx = CodeGenContext::new(program.ast_root.clone(), program.symbols, source, config.as_ref());
        let mut generator = RustGenerator::new(ctx, lint_suppressions, serde_struct_derives);
        generator.generate()
    }
}

struct RustGenerator<'a> {
    ctx: CodeGenContext<'a>,
    output: String,
    imports: std::collections::BTreeSet<&'static str>,
    postludes: Vec<String>,
    anon_namer: AnonymousTypeNamer,
    pending_anon_models: Vec<AnonymousModelDef>,
    pending_union_types: Vec<UnionTypeDef>,
    emitted_union_type_count: usize,
    lint_suppressions: bool,
    serde_struct_derives: bool,
}

#[derive(Clone)]
struct AnonymousModelDef {
    name: String,
    model: AnonModel,
    scope: Option<SymId>,
    path: Vec<String>,
}

#[derive(Clone)]
struct UnionTypeDef {
    name: String,
    atoms: Vec<TypeAtom>,
    scope: Option<SymId>,
    path: Vec<String>,
}

impl<'a> RustGenerator<'a> {
    fn new(ctx: CodeGenContext<'a>, lint_suppressions: bool, serde_struct_derives: bool) -> Self {
        let anon_namer = AnonymousTypeNamer::new(&ctx, Case::Pascal);
        Self {
            ctx,
            output: String::new(),
            imports: std::collections::BTreeSet::new(),
            postludes: Vec::new(),
            anon_namer,
            pending_anon_models: Vec::new(),
            pending_union_types: Vec::new(),
            emitted_union_type_count: 0,
            lint_suppressions,
            serde_struct_derives,
        }
    }

    fn generate(&mut self) -> CodeGenResult<String> {
        let include_yaml = self.ctx.config.and_then(|c| c.rust.as_ref()).and_then(|r| r.include_yaml).unwrap_or(false);

        for (const_def, scope) in self.ctx.scoped_consts() {
            let code = self.emit_const(&const_def, scope)?;
            self.output.push_str(&code);
        }
        for model in self.ctx.top_level_models().collect::<Vec<_>>() {
            let code = self.emit_model(&model, None)?;
            self.output.push_str(&code);
            if include_yaml {
                let yaml_impl = self.emit_model_yaml_impl(&model, None)?;
                self.output.push_str(&yaml_impl);
            }
        }
        for enum_ in self.ctx.top_level_enums().collect::<Vec<_>>() {
            let code = self.emit_enum(&enum_, None)?;
            self.output.push_str(&code);
        }

        for postlude in &self.postludes {
            self.output.push_str(postlude);
        }
        self.emit_pending_union_types()?;
        self.emit_pending_anon_models()?;

        let body = std::mem::take(&mut self.output);
        let mut output = String::new();
        if self.lint_suppressions {
            output.push_str("#![allow(clippy::all, clippy::pedantic, clippy::nursery)]\n\n");
        }
        for import in &self.imports {
            output.push_str(import);
            output.push('\n');
        }
        if !self.imports.is_empty() {
            output.push('\n');
        }
        output.push_str(&body);

        Ok(output)
    }

    fn struct_derive_attr(&self) -> &'static str {
        if self.serde_struct_derives {
            "#[derive(serde::Serialize, serde::Deserialize, Debug, Clone, Default)]\n"
        } else {
            "#[derive(Debug, Clone, Default)]\n"
        }
    }

    fn emit_const(&self, const_def: &ConstDef, scope: Option<SymId>) -> CodeGenResult<String> {
        let name = self.ctx.const_name(const_def, scope, Case::UpperSnake)?;
        let vis = if const_def.is_private() { "" } else { "pub " };
        let value = self.ctx.eval_const_def_in_scope(const_def, scope)?;
        let (ty, literal) = match value {
            ConstValue::String(value) => ("&str", format!("{:?}", value)),
            ConstValue::Int(value) => ("i64", value.to_string()),
            ConstValue::Bool(value) => ("bool", value.to_string()),
            ConstValue::List(_) => return Err(self.ctx.error(const_def.syntax(), "Constants can only be int, string, or bool")),
        };
        let docs = const_def.docs().map(|docs| DocEmitter::rust_docs(&docs, 0)).unwrap_or_default();
        Ok(format!("{}{}const {}: {} = {};\n\n", docs, vis, name, ty, literal))
    }

    fn emit_model(&mut self, model: &Model, parent_scope: Option<SymId>) -> CodeGenResult<String> {
        let mut output = String::new();

        let scope_id = model.scope_id(&self.ctx, parent_scope)?;
        let qualified_name = model.qualified_name(&self.ctx, parent_scope, Case::Pascal)?;
        let model_path = self.ctx.symbol_path(scope_id);

        if let Some(docs) = model.docs() {
            output.push_str(&DocEmitter::rust_docs(&docs, 0));
        }

        output.push_str(self.struct_derive_attr());
        output.push_str(&format!("pub struct {} {{\n", qualified_name));

        for field in model.fields() {
            let field_code = self.emit_field(&field, Some(scope_id), &model_path)?;
            output.push_str(&field_code);
        }

        output.push_str("}\n\n");

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

    fn emit_pending_anon_models(&mut self) -> CodeGenResult<()> {
        let mut index = 0;
        while index < self.pending_anon_models.len() {
            let def = self.pending_anon_models[index].clone();
            let code = self.emit_anon_model(&def)?;
            self.output.push_str(&code);
            index += 1;
        }
        Ok(())
    }

    fn emit_pending_union_types(&mut self) -> CodeGenResult<()> {
        let mut index = self.emitted_union_type_count;
        while index < self.pending_union_types.len() {
            let def = self.pending_union_types[index].clone();
            let code = self.emit_union_type(&def)?;
            self.output.push_str(&code);
            index += 1;
            self.emitted_union_type_count = index;
        }
        Ok(())
    }

    fn emit_anon_model(&mut self, def: &AnonymousModelDef) -> CodeGenResult<String> {
        let mut output = String::new();
        output.push_str(self.struct_derive_attr());
        output.push_str(&format!("pub struct {} {{\n", def.name));

        for field in def.model.fields() {
            let field_code = self.emit_field(&field, def.scope, &def.path)?;
            output.push_str(&field_code);
        }

        output.push_str("}\n\n");
        Ok(output)
    }

    fn emit_enum(&mut self, enum_: &Enum, parent_scope: Option<SymId>) -> CodeGenResult<String> {
        let mut output = String::new();

        let qualified_name = enum_.qualified_name(&self.ctx, parent_scope, Case::Pascal)?;

        if let Some(docs) = enum_.docs() {
            output.push_str(&DocEmitter::rust_docs(&docs, 0));
        }

        output.push_str("#[derive(serde::Serialize, serde::Deserialize, Debug, Clone, PartialEq, Eq)]\n");
        output.push_str(&format!("pub enum {} {{\n", qualified_name));

        for variant in enum_.variants() {
            let variant_value = variant.value().ok_or_else(|| CodeGenContext::internal_error("Enum variant missing value"))?;
            let variant_name = convert_generated_identifier_case(&variant_value, Case::Pascal);

            if let Some(docs) = variant.docs() {
                output.push_str(&DocEmitter::rust_docs(&docs, 1));
            }

            output.push_str(&format!("    #[serde(rename = \"{}\")]\n", variant_value));
            output.push_str(&format!("    {},\n", variant_name));
        }

        output.push_str("}\n\n");

        Ok(output)
    }

    fn emit_field(&mut self, field: &Field, parent_scope: Option<SymId>, owner_path: &[String]) -> CodeGenResult<String> {
        let mut output = String::new();

        let field_name = field.name()?;
        let field_type = field.field_type()?;
        let mut field_path = owner_path.to_vec();
        field_path.push(field_name.clone());

        if let Some(docs) = field.docs() {
            output.push_str(&DocEmitter::rust_docs(&docs, 1));
        }

        let alias = self.ctx.field_alias(field, parent_scope)?;
        if let (true, Some(alias_value)) = (self.serde_struct_derives, alias.as_ref()) {
            output.push_str(&format!("    #[serde(rename = \"{}\")]\n", alias_value));
        }

        let mut type_code = self.emit_type(&field_type, parent_scope, &field_path)?;

        if field.is_optional() {
            if self.serde_struct_derives {
                output.push_str("    #[serde(skip_serializing_if = \"Option::is_none\")]\n");
            }
            type_code = format!("Option<{}>", type_code);
        }

        // Escape keywords
        let emit_name = match field_name.as_str() {
            "type" => "r#type",
            "ref" => "r#ref",
            "self" => "r#self",
            "mod" => "r#mod",
            "gen" => "r#gen",
            other => other,
        };

        output.push_str(&format!("    pub {}: {},\n", emit_name, type_code));

        Ok(output)
    }

    fn emit_model_yaml_impl(&self, model: &Model, parent_scope: Option<SymId>) -> CodeGenResult<String> {
        let qualified_name = model.qualified_name(&self.ctx, parent_scope, Case::Pascal)?;
        Ok(format!(
            "impl {} {{\n    pub fn from_yaml(yaml: &str) -> Result<Self, serde_yaml::Error>\n    where\n        Self: serde::de::DeserializeOwned,\n    {{\n        serde_yaml::from_str(yaml)\n    }}\n\n    pub fn to_yaml(&self) -> Result<String, serde_yaml::Error>\n    where\n        Self: serde::Serialize,\n    {{\n        serde_yaml::to_string(self)\n    }}\n}}\n\n",
            qualified_name
        ))
    }

    fn emit_type(&mut self, ty: &Type, parent_scope: Option<SymId>, path: &[String]) -> CodeGenResult<String> {
        let atoms = ty.type_atoms();
        if atoms.len() == 1 {
            return self.emit_type_atom(&atoms[0], parent_scope, path);
        }

        let name = self.anon_namer.allocate(&self.ctx, path, Case::Pascal);
        self.pending_union_types.push(UnionTypeDef {
            name: name.clone(),
            atoms,
            scope: parent_scope,
            path: path.to_vec(),
        });
        Ok(name)
    }

    fn emit_union_type(&mut self, def: &UnionTypeDef) -> CodeGenResult<String> {
        let mut output = String::new();
        output.push_str("#[derive(serde::Serialize, serde::Deserialize, Debug, Clone)]\n");
        output.push_str("#[serde(untagged)]\n");
        output.push_str(&format!("pub enum {} {{\n", def.name));

        let mut variants = Vec::with_capacity(def.atoms.len());
        let mut used_variant_names = std::collections::HashSet::new();
        for atom in &def.atoms {
            let variant_type = self.emit_type_atom(atom, def.scope, &def.path)?;
            let mut variant_name = self.union_variant_name(atom, def.scope)?;
            if !used_variant_names.insert(variant_name.clone()) {
                let base = variant_name;
                let mut suffix = 2;
                loop {
                    let candidate = format!("{}{}", base, suffix);
                    if used_variant_names.insert(candidate.clone()) {
                        variant_name = candidate;
                        break;
                    }
                    suffix += 1;
                }
            }
            variants.push((variant_name, variant_type));
        }

        for (variant_name, variant_type) in &variants {
            output.push_str(&format!("    {}({}),\n", variant_name, variant_type));
        }
        output.push_str("}\n\n");

        if let Some((variant_name, _)) = variants.first() {
            output.push_str(&format!(
                "impl Default for {} {{\n    fn default() -> Self {{\n        Self::{}(Default::default())\n    }}\n}}\n\n",
                def.name, variant_name
            ));
        }

        Ok(output)
    }

    fn union_variant_name(&self, atom: &TypeAtom, parent_scope: Option<SymId>) -> CodeGenResult<String> {
        let base = if let Some(primitive) = atom.as_primitive_type() {
            match primitive {
                lang::PrimitiveType::Any => "Any".to_string(),
                lang::PrimitiveType::String => "String".to_string(),
                lang::PrimitiveType::Int => "Int".to_string(),
                lang::PrimitiveType::Float => "Float".to_string(),
                lang::PrimitiveType::Bool => "Bool".to_string(),
            }
        } else if atom.as_record_type().is_some() {
            "Record".to_string()
        } else if let Some(ref_token) = atom.as_ref_token() {
            let ref_name = ref_token.text().trim();
            if let Some(alias_type) = self.ctx.resolve_type_alias(parent_scope, ref_name)? {
                let alias_atoms = alias_type.type_atoms();
                if alias_atoms.len() == 1 {
                    self.union_variant_name(&alias_atoms[0], parent_scope)?
                } else {
                    self.ctx.symbol_name(ref_name, Case::Pascal)
                }
            } else {
                self.ctx.symbol_name(ref_name, Case::Pascal)
            }
        } else if atom.anon_model().is_some() {
            "Object".to_string()
        } else {
            "Value".to_string()
        };

        if atom.is_array() { Ok(format!("{}Array", base)) } else { Ok(base) }
    }

    fn emit_type_atom(&mut self, atom: &TypeAtom, parent_scope: Option<SymId>, path: &[String]) -> CodeGenResult<String> {
        let mut base = if let Some(primitive) = atom.as_primitive_type() {
            TypeMapper::to_rust(primitive).to_string()
        } else if let Some(record_type) = atom.as_record_type() {
            self.imports.insert("use std::collections::HashMap;");
            let src_type = record_type.src_type_node().ok_or_else(|| CodeGenContext::internal_error("Record missing source type"))?;
            let dest_type = record_type.dest_type_node().ok_or_else(|| CodeGenContext::internal_error("Record missing destination type"))?;
            let src_type = Type::cast(src_type).ok_or_else(|| CodeGenContext::internal_error("Expected Type for record source"))?;
            let dest_type = Type::cast(dest_type).ok_or_else(|| CodeGenContext::internal_error("Expected Type for record destination"))?;

            let mut key_path = path.to_vec();
            key_path.push("Key".to_string());
            let mut value_path = path.to_vec();
            value_path.push("Value".to_string());

            let src_str = self.emit_type(&src_type, parent_scope, &key_path)?;
            let dest_str = self.emit_type(&dest_type, parent_scope, &value_path)?;

            format!("HashMap<{}, {}>", src_str, dest_str)
        } else if let Some(ref_token) = atom.as_ref_token() {
            let ref_name = ref_token.text().trim();
            if let Some(alias_type) = self.ctx.resolve_type_alias(parent_scope, ref_name)? {
                self.emit_type(&alias_type, parent_scope, path)?
            } else {
                self.ctx
                    .qualified_name(parent_scope, ref_name, Case::Pascal)
                    .ok_or_else(|| CodeGenContext::internal_error(format!("Unresolved type: {}", ref_name)))?
            }
        } else if let Some(anon_model) = atom.anon_model() {
            let name = self.anon_namer.allocate(&self.ctx, path, Case::Pascal);
            self.pending_anon_models.push(AnonymousModelDef {
                name: name.clone(),
                model: anon_model,
                scope: parent_scope,
                path: path.to_vec(),
            });
            name
        } else {
            return Err(CodeGenContext::internal_error("Unknown type atom"));
        };

        if atom.is_array() {
            base = format!("Vec<{}>", base);
        }

        Ok(base)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use config::{GlueConfigSchemaGeneration, GlueConfigSchemaGenerationRust};
    use indoc::indoc;
    use insta::assert_snapshot;

    use crate::test_utils::{gen_test, gen_test_with_config};

    fn gen_rust(src: &str) -> String {
        gen_test(&CodeGenRust, src)
    }

    fn gen_rust_with_config(src: &str, config: GlueConfigSchemaGeneration) -> String {
        gen_test_with_config(&CodeGenRust, src, Some(config))
    }

    #[test]
    fn test_lint_suppressions_default_enabled() {
        let output = gen_rust("model User { id: string }");

        assert!(
            output.starts_with("#![allow(clippy::all, clippy::pedantic, clippy::nursery)]"),
            "Expected clippy suppressions by default:\n{}",
            output
        );
    }

    #[test]
    fn test_lint_suppressions_can_be_disabled() {
        let output = gen_rust_with_config(
            "model User { id: string }",
            GlueConfigSchemaGeneration {
                lint_suppressions: Some(false),
                ..Default::default()
            },
        );

        assert!(!output.contains("#![allow(clippy::"), "Expected no clippy suppressions:\n{}", output);
    }

    #[test]
    fn test_omits_unused_imports_by_default() {
        let output = gen_rust("model User { id: string }");

        assert!(!output.contains("use std::collections::HashMap;"), "Expected unused HashMap import to be omitted:\n{}", output);
    }

    #[test]
    fn test_serde_struct_derives_can_be_disabled() {
        let src = indoc! { r#"
            model User {
                @field(alias="user_id")
                id?: string
            }

            enum Status: "active" | "inactive"
        "# };

        let output = gen_rust_with_config(
            src,
            GlueConfigSchemaGeneration {
                rust: Some(GlueConfigSchemaGenerationRust {
                    serde_struct_derives: Some(false),
                    ..Default::default()
                }),
                ..Default::default()
            },
        );

        assert!(output.contains("#[derive(Debug, Clone, Default)]\npub struct User"), "Expected non-serde struct derive:\n{}", output);
        assert!(
            !output.contains("#[derive(serde::Serialize, serde::Deserialize, Debug, Clone, Default)]\npub struct User"),
            "Expected no serde derives on structs:\n{}",
            output
        );
        assert!(
            !output.contains("#[serde(rename = \"user_id\")]"),
            "Expected no serde field rename without struct serde derives:\n{}",
            output
        );
        assert!(
            !output.contains("#[serde(skip_serializing_if = \"Option::is_none\")]"),
            "Expected no serde optional-field attribute without struct serde derives:\n{}",
            output
        );
        assert!(
            output.contains("#[derive(serde::Serialize, serde::Deserialize, Debug, Clone, PartialEq, Eq)]\npub enum Status"),
            "Expected enum serde derives to remain unchanged:\n{}",
            output
        );
    }

    #[test]
    fn test_yaml_helpers_with_disabled_struct_derives_use_explicit_bounds() {
        let output = gen_rust_with_config(
            "model User { id: string }",
            GlueConfigSchemaGeneration {
                rust: Some(GlueConfigSchemaGenerationRust {
                    include_yaml: Some(true),
                    serde_struct_derives: Some(false),
                }),
                ..Default::default()
            },
        );

        assert!(output.contains("Self: serde::de::DeserializeOwned,"), "Expected from_yaml serde bound:\n{}", output);
        assert!(output.contains("Self: serde::Serialize,"), "Expected to_yaml serde bound:\n{}", output);
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
    fn test_constants_emit() {
        let src = indoc! { r#"
            /// Alias used for user IDs.
            const USER_ALIAS = "user_" + "id"
            const DEFAULT_LIMIT = 100 * 2
            const _PRIVATE_FLAG: bool = true

            model Request {
                @field(alias=USER_ALIAS)
                user_id: string
            }
        "# };

        let output = gen_rust(src);
        assert!(output.contains("/// Alias used for user IDs.\npub const USER_ALIAS"), "Expected constant docs:\n{}", output);
        assert!(output.contains("pub const USER_ALIAS: &str = \"user_id\";"), "Expected folded string constant:\n{}", output);
        assert!(output.contains("pub const DEFAULT_LIMIT: i64 = 200;"), "Expected folded int constant:\n{}", output);
        assert!(output.contains("const _PRIVATE_FLAG: bool = true;"), "Expected private constant:\n{}", output);
        assert!(output.contains("#[serde(rename = \"user_id\")]"), "Expected folded alias:\n{}", output);
    }

    #[test]
    fn test_model_scoped_constants_emit_and_aliases_fold() {
        let src = indoc! { r#"
            model Aliases {
                const SUFFIX = "_alias"
                const USER_ID_ALIAS = "user_id" + SUFFIX
                const _PRIVATE_FLAG = true
            }

            model User {
                const SUFFIX = "_user"

                @field(alias=Aliases.USER_ID_ALIAS)
                user_id: string
            }
        "# };

        let output = gen_rust(src);
        assert!(output.contains("pub const ALIASES_SUFFIX: &str = \"_alias\";"), "Expected model-prefixed string constant:\n{}", output);
        assert!(
            output.contains("pub const ALIASES_USER_ID_ALIAS: &str = \"user_id_alias\";"),
            "Expected declaration-scope folded constant:\n{}",
            output
        );
        assert!(output.contains("const _ALIASES_PRIVATE_FLAG: bool = true;"), "Expected private model-prefixed constant:\n{}", output);
        assert!(output.contains("pub const USER_SUFFIX: &str = \"_user\";"), "Expected second model-prefixed constant:\n{}", output);
        assert!(output.contains("#[serde(rename = \"user_id_alias\")]"), "Expected folded qualified alias:\n{}", output);
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
        // Record<K,V>[] syntax may need Vec wrapping - check actual output
        assert!(output.contains("HashMap<String, i64>"), "Expected HashMap in output:\n{}", output);
        assert_snapshot!(output);
    }

    #[test]
    fn test_anonymous_struct() {
        let src = indoc! { r#"
            model User {
                profile: {
                    bio: string
                    age?: int
                    settings: Record<string, {
                        enabled: bool
                    }>
                }
            }
        "# };

        assert_snapshot!(gen_rust(src));
    }

    #[test]
    fn test_anonymous_struct_name_collision_gets_suffix() {
        let src = indoc! { r#"
            model User {
                profile: {
                    bio: string
                }

                model Profile {
                    id: string
                }
            }
        "# };

        let output = gen_rust(src);
        assert!(output.contains("pub profile: UserProfileAnon,"), "Expected anonymous struct collision suffix:\n{}", output);
        assert!(output.contains("pub struct UserProfileAnon {"), "Expected suffixed anonymous struct declaration:\n{}", output);
    }

    #[test]
    fn test_union_field_emits_untagged_enum() {
        let src = indoc! { r#"
            model Config {
                files: string | string[]
            }
        "# };

        let output = gen_rust(src);
        assert!(output.contains("pub files: ConfigFiles,"), "Expected field to use generated union enum:\n{}", output);
        assert!(output.contains("#[serde(untagged)]"), "Expected serde untagged enum:\n{}", output);
        assert!(output.contains("pub enum ConfigFiles {"), "Expected generated union enum:\n{}", output);
        assert!(output.contains("String(String),"), "Expected scalar string variant:\n{}", output);
        assert!(output.contains("StringArray(Vec<String>),"), "Expected array string variant:\n{}", output);
        assert!(output.contains("impl Default for ConfigFiles"), "Expected generated default impl:\n{}", output);
    }

    #[test]
    fn test_pascal_type_identifiers_preserve_uppercase_and_generated_identifiers_default_to_pascal() {
        let src = indoc! { r#"
            model XMLDocument {
                region: string
            }

            model XMLParser {
                document: XMLDocument
            }

            enum XMLParseMode: "STRICT_MODE" | "HTML-mode"
        "# };

        let output = gen_rust(src);
        assert!(output.contains("pub struct XMLDocument {"), "Expected model acronym to be preserved:\n{}", output);
        assert!(output.contains("pub struct XMLParser {"), "Expected model acronym to be preserved:\n{}", output);
        assert!(output.contains("pub document: XMLDocument,"), "Expected reference acronym to be preserved:\n{}", output);
        assert!(output.contains("pub enum XMLParseMode {"), "Expected enum acronym to be preserved:\n{}", output);
        assert!(
            output.contains("StrictMode,"),
            "Expected generated enum variant from uppercase value to default to PascalCase:\n{}",
            output
        );
        assert!(output.contains("HtmlMode,"), "Expected generated enum variant to default to PascalCase:\n{}", output);
    }

    #[test]
    fn test_preserve_generated_identifiers_config() {
        let src = indoc! { r#"
            model xml_document {
                XML_version: string
            }

            enum xml_parse_mode: "STRICT_MODE" | "HTML-mode"
        "# };

        let output = gen_rust_with_config(
            src,
            GlueConfigSchemaGeneration {
                preserve_generated_identifiers: Some(true),
                ..Default::default()
            },
        );
        assert!(
            output.contains("pub struct xml_document {"),
            "Expected configured model identifier to be preserved exactly:\n{}",
            output
        );
        assert!(output.contains("pub XML_version: String,"), "Expected configured field identifier to be preserved exactly:\n{}", output);
        assert!(output.contains("pub enum xml_parse_mode {"), "Expected configured enum identifier to be preserved exactly:\n{}", output);
        assert!(output.contains("StrictMode,"), "Expected generated enum variant to keep normal PascalCase:\n{}", output);
        assert!(output.contains("HtmlMode,"), "Expected generated enum variant to keep normal PascalCase:\n{}", output);
    }
}
