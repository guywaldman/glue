use config::{GlueConfigSchemaGeneration, GlueConfigSchemaGenerationPython, GlueConfigSchemaGenerationPythonDataModelLibrary};
use convert_case::Case;
use lang::{AnonModel, AstNode, ConstDef, ConstValue, Enum, Field, GlueIr, Model, SourceCodeMetadata, SymId, Type, TypeAtom};

use crate::{
    CodeGenError, CodeGenerator,
    codegen::CodeGenResult,
    context::{AnonymousTypeNamer, CodeGenContext, DocEmitter, EnumVariantExt, FieldExt, NamedExt, TypeMapper, convert_generated_identifier_case, convert_user_identifier_case, indent},
};

enum PyModelLibrary {
    Pydantic { base_model: String },
    Dataclasses,
    Attrs,
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
            GlueConfigSchemaGenerationPythonDataModelLibrary::Attrs => Ok(Self::Attrs),
            GlueConfigSchemaGenerationPythonDataModelLibrary::Msgspec => Ok(Self::Msgspec),
        }
    }

    fn preludes(&self, include_lint: bool) -> Vec<String> {
        let lint = "# pylint: disable=missing-class-docstring, missing-function-docstring, missing-module-docstring".to_string();
        let mut lines = match self {
            Self::Pydantic { base_model } => {
                let (module, class) = base_model.rsplit_once('.').unwrap();
                vec![
                    format!("from {} import {}", module, class),
                    "from pydantic import Field".to_string(),
                    "from enum import StrEnum".to_string(),
                    "from typing import Any, Annotated, Optional, Union".to_string(),
                ]
            }
            Self::Dataclasses => vec![
                "from dataclasses import dataclass".to_string(),
                "from enum import StrEnum".to_string(),
                "from typing import Any, Optional, Union".to_string(),
            ],
            Self::Attrs => vec![
                "from attrs import define, field".to_string(),
                "from enum import StrEnum".to_string(),
                "from typing import Any, Optional, Union".to_string(),
            ],
            Self::Msgspec => vec![
                "import msgspec".to_string(),
                "from enum import StrEnum".to_string(),
                "from typing import Any, Optional, Union".to_string(),
            ],
        };

        if include_lint {
            lines.insert(0, lint);
        }

        lines
    }

    fn model_decorator(&self) -> Option<&str> {
        match self {
            Self::Dataclasses => Some("@dataclass"),
            Self::Attrs => Some("@define"),
            _ => None,
        }
    }

    fn base_class(&self) -> &str {
        match self {
            Self::Pydantic { base_model } => base_model.rsplit_once('.').unwrap().1,
            Self::Dataclasses => "",
            Self::Attrs => "",
            Self::Msgspec => "msgspec.Struct",
        }
    }
}

#[derive(Default)]
pub struct CodeGenPython;

impl CodeGenerator for CodeGenPython {
    fn generate(&self, ir: GlueIr, source: &SourceCodeMetadata, config: Option<GlueConfigSchemaGeneration>) -> Result<String, CodeGenError> {
        let program = ir
            .into_analyzed_program()
            .ok_or_else(|| CodeGenError::InternalError("Glue IR does not contain an analyzed program".to_string()))?;
        let lint_suppressions = config.as_ref().and_then(|g| g.lint_suppressions).unwrap_or(true);
        let py_config = config.as_ref().and_then(|g| g.python.clone()).unwrap_or_default();

        let lib = PyModelLibrary::from_config(py_config)?;
        let ctx = CodeGenContext::new(program.ast_root.clone(), program.symbols, source, config.as_ref());
        let mut generator = PythonGenerator::new(ctx, lib, lint_suppressions);
        generator.generate()
    }
}

struct PythonGenerator<'a> {
    ctx: CodeGenContext<'a>,
    fw: PyModelLibrary,
    lint_suppressions: bool,
    output: String,
    anon_namer: AnonymousTypeNamer,
    pending_anon_models: Vec<AnonymousModelDef>,
}

#[derive(Clone)]
struct AnonymousModelDef {
    name: String,
    model: AnonModel,
    scope: Option<SymId>,
    path: Vec<String>,
}

impl<'a> PythonGenerator<'a> {
    fn new(ctx: CodeGenContext<'a>, fw: PyModelLibrary, lint_suppressions: bool) -> Self {
        let anon_namer = AnonymousTypeNamer::new(&ctx, Case::Pascal);
        Self {
            ctx,
            fw,
            lint_suppressions,
            output: String::new(),
            anon_namer,
            pending_anon_models: Vec::new(),
        }
    }

    fn generate(&mut self) -> CodeGenResult<String> {
        for const_def in self.ctx.top_level_consts().collect::<Vec<_>>() {
            self.emit_const(&const_def)?;
        }
        for model in self.ctx.top_level_models().collect::<Vec<_>>() {
            self.emit_model(&model, None)?;
        }
        for enum_ in self.ctx.top_level_enums().collect::<Vec<_>>() {
            self.emit_enum(&enum_, None)?;
        }
        self.emit_pending_anon_models()?;

        let preludes = self.fw.preludes(self.lint_suppressions).join("\n");
        let body = self.output.trim_start_matches('\n');
        if body.is_empty() { Ok(preludes) } else { Ok(format!("{}\n\n{}", preludes, body)) }
    }

    fn emit_const(&mut self, const_def: &ConstDef) -> CodeGenResult<()> {
        if !self.output.is_empty() && !self.output.ends_with("\n\n") {
            self.output.push_str("\n\n");
        }
        let name = const_def.name()?;
        let value = self.ctx.eval_const_def(const_def)?;
        let (ty, literal) = match value {
            ConstValue::String(value) => ("str", serde_json::to_string(&value).map_err(|e| CodeGenContext::internal_error(e.to_string()))?),
            ConstValue::Int(value) => ("int", value.to_string()),
            ConstValue::Bool(value) => ("bool", if value { "True" } else { "False" }.to_string()),
            ConstValue::List(_) => return Err(self.ctx.error(const_def.syntax(), "Top-level constants can only be int, string, or bool")),
        };
        self.output.push_str(&format!("{}: {} = {}\n", name, ty, literal));
        Ok(())
    }

    fn emit_model(&mut self, model: &Model, parent_scope: Option<SymId>) -> CodeGenResult<()> {
        let scope_id = model.scope_id(&self.ctx, parent_scope)?;
        let name = model.qualified_name(&self.ctx, parent_scope, Case::Pascal)?;
        let model_path = self.ctx.symbol_path(scope_id);

        if !self.output.is_empty() && !self.output.ends_with("\n\n") {
            self.output.push_str("\n\n");
        }

        if let Some(decorator) = self.fw.model_decorator() {
            self.output.push_str(&format!("{}\n", decorator));
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
                self.emit_field(&field, Some(scope_id), &model_path)?;
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

    fn emit_field(&mut self, field: &Field, scope: Option<SymId>, owner_path: &[String]) -> CodeGenResult<()> {
        let field_name = field.name()?;
        let py_name = convert_user_identifier_case(&field_name, Case::Snake, self.ctx.preserve_generated_identifiers());

        let mut field_path = owner_path.to_vec();
        field_path.push(field_name.clone());
        let mut type_code = self.emit_type(&field.field_type()?, scope, &field_path)?;
        if field.is_optional() {
            type_code = format!("Optional[{}]", type_code);
        }

        let line = match &self.fw {
            PyModelLibrary::Pydantic { .. } => self.emit_field_pydantic(field, scope, &py_name, &field_name, &type_code)?,
            PyModelLibrary::Dataclasses => self.emit_field_dataclasses(field, scope, &py_name, &type_code)?,
            PyModelLibrary::Attrs => self.emit_field_attrs(field, scope, &py_name, &field_name, &type_code)?,
            PyModelLibrary::Msgspec => self.emit_field_msgspec(field, scope, &py_name, &field_name, &type_code)?,
        };

        self.output.push_str(&indent(&line, 4));

        if let Some(docs) = field.docs() {
            self.output.push_str(&indent(&DocEmitter::python_docstring(&docs), 4));
        }

        Ok(())
    }

    fn emit_pending_anon_models(&mut self) -> CodeGenResult<()> {
        let mut index = 0;
        while index < self.pending_anon_models.len() {
            let def = self.pending_anon_models[index].clone();
            self.emit_anon_model(&def)?;
            index += 1;
        }
        Ok(())
    }

    fn emit_anon_model(&mut self, def: &AnonymousModelDef) -> CodeGenResult<()> {
        if !self.output.is_empty() && !self.output.ends_with("\n\n") {
            self.output.push_str("\n\n");
        }

        if let Some(decorator) = self.fw.model_decorator() {
            self.output.push_str(&format!("{}\n", decorator));
        }

        let base = self.fw.base_class();
        if base.is_empty() {
            self.output.push_str(&format!("class {}:\n", def.name));
        } else {
            self.output.push_str(&format!("class {}({}):\n", def.name, base));
        }

        let fields = def.model.fields();
        if fields.is_empty() {
            self.output.push_str(&indent("pass\n", 4));
        } else {
            for field in fields {
                self.emit_field(&field, def.scope, &def.path)?;
            }
        }

        Ok(())
    }

    /// Pydantic: `name: Annotated[Type, Field(default=..., alias="...")]`
    fn emit_field_pydantic(&self, field: &Field, scope: Option<SymId>, py_name: &str, orig_name: &str, type_code: &str) -> CodeGenResult<String> {
        let mut args = vec![];

        if field.is_optional() {
            args.push("default=None".to_string());
        } else if let Some(default) = self.field_default_code(field, scope)? {
            args.push(format!("default={}", default));
        }

        let alias = self.ctx.field_alias(field, scope)?;
        if let Some(v) = &alias {
            args.push(format!("alias=\"{}\"", v));
        } else if py_name != orig_name {
            args.push(format!("alias=\"{}\"", orig_name));
        }

        Ok(format!("{}: Annotated[{}, Field({})]\n", py_name, type_code, args.join(", ")))
    }

    /// Dataclasses: `name: Type = default`
    fn emit_field_dataclasses(&self, field: &Field, scope: Option<SymId>, py_name: &str, type_code: &str) -> CodeGenResult<String> {
        if field.is_optional() {
            Ok(format!("{}: {} = None\n", py_name, type_code))
        } else if let Some(default) = self.field_default_code(field, scope)? {
            Ok(format!("{}: {} = {}\n", py_name, type_code, default))
        } else {
            Ok(format!("{}: {}\n", py_name, type_code))
        }
    }

    fn emit_field_attrs(&self, field: &Field, scope: Option<SymId>, py_name: &str, orig_name: &str, type_code: &str) -> CodeGenResult<String> {
        let alias = self.field_alias(field, scope, py_name, orig_name)?;
        let default = self.field_default_code(field, scope)?;
        if alias.is_some() || field.is_optional() || default.is_some() {
            let mut args = Vec::new();
            if let Some(alias) = alias {
                args.push(format!("alias=\"{}\"", alias));
            }
            if field.is_optional() {
                args.push("default=None".to_string());
            } else if let Some(default) = default {
                args.push(format!("default={}", default));
            }
            Ok(format!("{}: {} = field({})\n", py_name, type_code, args.join(", ")))
        } else {
            Ok(format!("{}: {}\n", py_name, type_code))
        }
    }

    fn emit_field_msgspec(&self, field: &Field, scope: Option<SymId>, py_name: &str, orig_name: &str, type_code: &str) -> CodeGenResult<String> {
        let alias = self.field_alias(field, scope, py_name, orig_name)?;
        let default = self.field_default_code(field, scope)?;
        if alias.is_some() || field.is_optional() || default.is_some() {
            let mut args = Vec::new();
            if let Some(alias) = alias {
                args.push(format!("name=\"{}\"", alias));
            }
            if field.is_optional() {
                args.push("default=None".to_string());
            } else if let Some(default) = default {
                args.push(format!("default={}", default));
            }
            Ok(format!("{}: {} = msgspec.field({})\n", py_name, type_code, args.join(", ")))
        } else {
            Ok(format!("{}: {}\n", py_name, type_code))
        }
    }

    fn field_alias(&self, field: &Field, scope: Option<SymId>, py_name: &str, orig_name: &str) -> CodeGenResult<Option<String>> {
        let alias = self.ctx.field_alias(field, scope)?;
        if let Some(v) = alias {
            Ok(Some(v))
        } else if py_name != orig_name {
            Ok(Some(orig_name.to_string()))
        } else {
            Ok(None)
        }
    }

    fn field_default_code(&self, field: &Field, scope: Option<SymId>) -> CodeGenResult<Option<String>> {
        Ok(self.ctx.field_default_value(field, scope)?.map(|value| self.emit_const_value(&value)))
    }

    // -- Enums --------------------------------------------------------------

    fn emit_enum(&mut self, enum_: &Enum, parent_scope: Option<SymId>) -> CodeGenResult<()> {
        let name = enum_.qualified_name(&self.ctx, parent_scope, Case::Pascal)?;

        if !self.output.is_empty() && !self.output.ends_with("\n\n") {
            self.output.push_str("\n\n");
        }

        self.output.push_str(&format!("class {}(StrEnum):\n", name));

        if let Some(docs) = enum_.docs() {
            self.output.push_str(&indent(&DocEmitter::python_docstring(&docs), 4));
        }

        for variant in enum_.variants() {
            let value = variant.variant_value()?;
            let ident = convert_generated_identifier_case(&value.replace('-', "_"), Case::UpperSnake);
            self.output.push_str(&indent(&format!("{} = \"{}\"\n", ident, value), 4));

            if let Some(docs) = variant.docs() {
                self.output.push_str(&indent(&DocEmitter::python_docstring(&docs), 4));
            }
        }

        Ok(())
    }

    // -- Types & literals ---------------------------------------------------

    fn emit_type(&mut self, ty: &Type, scope: Option<SymId>, path: &[String]) -> CodeGenResult<String> {
        let atoms = ty.type_atoms();
        if atoms.len() == 1 {
            self.emit_type_atom(&atoms[0], scope, path)
        } else {
            let codes: Vec<_> = atoms.iter().map(|a| self.emit_type_atom(a, scope, path)).collect::<Result<Vec<_>, _>>()?;
            Ok(format!("Union[{}]", codes.join(", ")))
        }
    }

    fn emit_type_atom(&mut self, atom: &TypeAtom, scope: Option<SymId>, path: &[String]) -> CodeGenResult<String> {
        let mut result = if let Some(primitive) = atom.as_primitive_type() {
            TypeMapper::to_python(primitive).to_string()
        } else if let Some(record) = atom.as_record_type() {
            let src = record.src_type_node().ok_or_else(|| CodeGenContext::internal_error("Record missing src type"))?;
            let dest = record.dest_type_node().ok_or_else(|| CodeGenContext::internal_error("Record missing dest type"))?;
            let src_type = Type::cast(src).ok_or_else(|| CodeGenContext::internal_error("Expected Type for record src"))?;
            let dest_type = Type::cast(dest).ok_or_else(|| CodeGenContext::internal_error("Expected Type for record dest"))?;
            let mut key_path = path.to_vec();
            key_path.push("Key".to_string());
            let mut value_path = path.to_vec();
            value_path.push("Value".to_string());
            format!("dict[{}, {}]", self.emit_type(&src_type, scope, &key_path)?, self.emit_type(&dest_type, scope, &value_path)?)
        } else if let Some(ref_token) = atom.as_ref_token() {
            let type_name = ref_token.text().to_string();
            if let Some(alias_type) = self.ctx.resolve_type_alias(scope, &type_name)? {
                self.emit_type(&alias_type, scope, path)?
            } else {
                let sym = self
                    .ctx
                    .resolve(scope, &type_name)
                    .ok_or_else(|| CodeGenContext::internal_error(format!("Unresolved type: {}", type_name)))?;
                let qualified = self.ctx.symbol_name(&sym.name, Case::Pascal);
                format!("\"{}\"", qualified)
            }
        } else if let Some(anon_model) = atom.anon_model() {
            let name = self.anon_namer.allocate(&self.ctx, path, Case::Pascal);
            self.pending_anon_models.push(AnonymousModelDef {
                name: name.clone(),
                model: anon_model,
                scope,
                path: path.to_vec(),
            });
            format!("\"{}\"", name)
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

    fn emit_const_value(&self, value: &ConstValue) -> String {
        match value {
            ConstValue::Bool(value) => if *value { "True" } else { "False" }.to_string(),
            ConstValue::Int(value) => value.to_string(),
            ConstValue::String(value) => serde_json::to_string(value).unwrap_or_else(|_| "\"\"".to_string()),
            ConstValue::List(_) => "None".to_string(),
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
        let config = GlueConfigSchemaGeneration {
            python: Some(GlueConfigSchemaGenerationPython {
                data_model_library: Some(lib),
                base_model: None,
            }),
            ..Default::default()
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

    const ALIAS_MODEL: &str = indoc! { r#"
        model User {
            id: int
            @field(alias="firstName")
            first_name: string
            metadata: any
        }
    "# };

    #[test]
    fn test_pydantic() {
        assert_snapshot!(gen_python(SIMPLE_MODEL));
    }

    #[test]
    fn test_constants_emit_and_defaults_fold() {
        let src = indoc! { r#"
            const USER_ALIAS = "user_" + "id"
            const DEFAULT_LIMIT = 100 * 2
            const _PRIVATE_FLAG = true

            model Request {
                limit: int = DEFAULT_LIMIT
                @field(alias=USER_ALIAS)
                user_id: string
            }
        "# };

        let output = gen_python(src);
        assert!(output.contains("USER_ALIAS: str = \"user_id\""), "Expected folded string constant:\n{}", output);
        assert!(output.contains("DEFAULT_LIMIT: int = 200"), "Expected folded int constant:\n{}", output);
        assert!(output.contains("_PRIVATE_FLAG: bool = True"), "Expected private bool constant:\n{}", output);
        assert!(output.contains("limit: Annotated[int, Field(default=200)]"), "Expected folded default:\n{}", output);
        assert!(output.contains("user_id: Annotated[str, Field(alias=\"user_id\")]"), "Expected folded alias:\n{}", output);
    }

    #[test]
    fn test_dataclasses() {
        assert_snapshot!(gen_python_with_data_model_lib(SIMPLE_MODEL, GlueConfigSchemaGenerationPythonDataModelLibrary::Dataclasses));
    }

    #[test]
    fn test_attrs() {
        assert_snapshot!(gen_python_with_data_model_lib(SIMPLE_MODEL, GlueConfigSchemaGenerationPythonDataModelLibrary::Attrs));
    }

    #[test]
    fn test_type_aliases_expand_in_generated_python() {
        let src = indoc! { r#"
            type UserId = string
            type UserIds = UserId[]

            model User {
                id: UserId
                related: UserIds
            }
        "# };

        let generated = gen_python(src);
        assert!(generated.contains("id: Annotated[str, Field()]"), "Expected alias to primitive type to resolve to str annotation");
        assert!(generated.contains("related: Annotated[list[str], Field()]"), "Expected nested alias to array type to resolve");
    }

    #[test]
    fn test_msgspec() {
        assert_snapshot!(gen_python_with_data_model_lib(SIMPLE_MODEL, GlueConfigSchemaGenerationPythonDataModelLibrary::Msgspec));
    }

    #[test]
    fn test_anonymous_struct_pydantic() {
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

        assert_snapshot!(gen_python(src));
    }

    #[test]
    fn test_anonymous_struct_dataclasses() {
        let src = indoc! { r#"
            model User {
                profile: {
                    bio: string
                    age?: int
                }
            }
        "# };

        assert_snapshot!(gen_python_with_data_model_lib(src, GlueConfigSchemaGenerationPythonDataModelLibrary::Dataclasses));
    }

    #[test]
    fn test_dataclasses_alias() {
        assert_snapshot!(gen_python_with_data_model_lib(ALIAS_MODEL, GlueConfigSchemaGenerationPythonDataModelLibrary::Dataclasses));
    }

    #[test]
    fn test_attrs_alias() {
        assert_snapshot!(gen_python_with_data_model_lib(ALIAS_MODEL, GlueConfigSchemaGenerationPythonDataModelLibrary::Attrs));
    }

    #[test]
    fn test_msgspec_alias() {
        assert_snapshot!(gen_python_with_data_model_lib(ALIAS_MODEL, GlueConfigSchemaGenerationPythonDataModelLibrary::Msgspec));
    }

    #[test]
    fn test_pascal_identifiers_preserve_uppercase() {
        let src = indoc! { r#"
            model XMLDocument {
                region: string
            }

            model XMLParser {
                document: XMLDocument
            }
        "# };

        let output = gen_python(src);
        assert!(output.contains("class XMLDocument(BaseModel):"), "Expected model acronym to be preserved:\n{}", output);
        assert!(output.contains("class XMLParser(BaseModel):"), "Expected model acronym to be preserved:\n{}", output);
        assert!(
            output.contains("document: Annotated[\"XMLDocument\", Field()]"),
            "Expected reference acronym to be preserved:\n{}",
            output
        );
    }

    #[test]
    fn test_preserve_generated_identifiers_config() {
        let src = indoc! { r#"
            model xml_document {
                XML_version: string
            }

            model xml_parser {
                document: xml_document
            }

            enum xml_parse_mode: "STRICT_MODE" | "HTML-mode"
        "# };

        let output = gen_test_with_config(
            &CodeGenPython,
            src,
            Some(GlueConfigSchemaGeneration {
                preserve_generated_identifiers: Some(true),
                ..Default::default()
            }),
        );
        assert!(
            output.contains("class xml_document(BaseModel):"),
            "Expected configured model identifier to be preserved exactly:\n{}",
            output
        );
        assert!(
            output.contains("XML_version: Annotated[str, Field()]"),
            "Expected configured field identifier to be preserved exactly:\n{}",
            output
        );
        assert!(
            output.contains("class xml_parser(BaseModel):"),
            "Expected configured model identifier to be preserved exactly:\n{}",
            output
        );
        assert!(
            output.contains("document: Annotated[\"xml_document\", Field()]"),
            "Expected configured reference identifier to be preserved exactly:\n{}",
            output
        );
        assert!(
            output.contains("class xml_parse_mode(StrEnum):"),
            "Expected configured enum identifier to be preserved exactly:\n{}",
            output
        );
        assert!(
            output.contains("STRICT_MODE = \"STRICT_MODE\""),
            "Expected generated enum member to keep normal UpperSnake:\n{}",
            output
        );
        assert!(output.contains("HTML_MODE = \"HTML-mode\""), "Expected generated enum member to keep normal UpperSnake:\n{}", output);
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
