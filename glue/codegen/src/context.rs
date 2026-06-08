use std::collections::HashSet;

use config::GlueConfigSchemaGeneration;
use convert_case::{Case, Casing, Converter};
use lang::{
    AstNode, DiagnosticContext, Enum, EnumVariant, Field, LNode, LSyntaxKind, Literal, MODEL_FIELD_DECORATOR, MODEL_FIELD_DECORATOR_ALIAS_ARG, MODEL_FIELD_DECORATOR_PROTO_TAG_ARG, Model,
    PrimitiveType, Service, SourceCodeMetadata, SymId, SymTable, Type, TypeAlias, TypeAtom,
};

use crate::CodeGenError;

pub struct CodeGenContext<'a> {
    pub ast: LNode,
    pub symbols: SymTable<LNode>,
    pub diag: DiagnosticContext,
    pub config: Option<&'a GlueConfigSchemaGeneration>,
}

impl<'a> CodeGenContext<'a> {
    pub fn new(ast: LNode, symbols: SymTable<LNode>, source: &SourceCodeMetadata, config: Option<&'a GlueConfigSchemaGeneration>) -> Self {
        let diag = DiagnosticContext::new(source.file_name, source.file_contents);
        Self { ast, symbols, diag, config }
    }

    pub fn resolve_id(&self, scope: Option<SymId>, name: &str) -> Option<SymId> {
        self.symbols.resolve_id(scope, name)
    }

    pub fn resolve(&self, scope: Option<SymId>, name: &str) -> Option<lang::SymEntry<LNode>> {
        self.symbols.resolve(scope, name)
    }

    pub fn symbol_path(&self, id: SymId) -> Vec<String> {
        self.symbols
            .get(id)
            .map(|entry| lang::symbol_name_to_parts(&entry.name).into_iter().map(str::to_string).collect())
            .unwrap_or_default()
    }

    pub fn symbol_name(&self, name: &str, case: Case) -> String {
        let parts = lang::symbol_name_to_parts(name);
        if self.preserve_generated_identifiers() {
            parts.join("")
        } else {
            convert_identifier_case(&parts.join("_"), case)
        }
    }

    pub fn anonymous_type_base_name(&self, path: &[String], case: Case) -> String {
        let parts = path.iter().map(|part| anonymous_name_part(part)).filter(|part| !part.is_empty()).collect::<Vec<_>>();
        let joined = if parts.is_empty() { "Anonymous".to_string() } else { parts.join("_") };
        if self.preserve_generated_identifiers() {
            if parts.is_empty() { "Anonymous".to_string() } else { parts.join("") }
        } else {
            convert_identifier_case(&joined, case)
        }
    }

    pub fn qualified_name(&self, scope: Option<SymId>, name: &str, case: Case) -> Option<String> {
        self.resolve(scope, name).map(|entry| self.symbol_name(&entry.name, case))
    }

    pub fn preserve_generated_identifiers(&self) -> bool {
        self.config.and_then(|config| config.preserve_generated_identifiers).unwrap_or(false)
    }

    pub fn top_level_models(&self) -> impl Iterator<Item = Model> + '_ {
        self.ast.children().filter(|n| n.kind() == LSyntaxKind::MODEL).filter_map(Model::cast)
    }

    pub fn top_level_enums(&self) -> impl Iterator<Item = Enum> + '_ {
        self.ast.children().filter(|n| n.kind() == LSyntaxKind::ENUM).filter_map(Enum::cast)
    }

    pub fn top_level_endpoints(&self) -> impl Iterator<Item = lang::Endpoint> + '_ {
        self.ast.children().filter(|n| n.kind() == LSyntaxKind::ENDPOINT).filter_map(lang::Endpoint::cast)
    }

    pub fn top_level_services(&self) -> impl Iterator<Item = Service> + '_ {
        self.ast.children().filter(|n| n.kind() == LSyntaxKind::SERVICE).filter_map(Service::cast)
    }

    /// Find the root model (@root-decorated, or the sole model)
    pub fn root_model(&self) -> Result<Model, CodeGenError> {
        let models: Vec<_> = self.top_level_models().collect();
        let root_models: Vec<_> = models.iter().filter(|m| m.decorators().iter().any(|d| d.ident().as_deref() == Some("root"))).cloned().collect();

        match root_models.len() {
            n if n > 1 => Err(CodeGenError::GenerationError(
                self.diag.error(self.ast.text_range(), "Multiple root models found. Only one model should have the @root decorator."),
            )),
            1 => Ok(root_models.into_iter().next().unwrap()),
            _ if models.len() == 1 => Ok(models.into_iter().next().unwrap()),
            _ => Err(CodeGenError::GenerationError(
                self.diag
                    .error(self.ast.text_range(), "Multiple models found but none marked with @root. Please add @root to one model."),
            )),
        }
    }

    pub fn resolve_type_ref(&self, scope: Option<SymId>, name: &str, case: Case) -> Result<String, CodeGenError> {
        self.qualified_name(scope, name, case)
            .ok_or_else(|| CodeGenContext::internal_error(format!("Unresolved type: {}", name)))
    }

    pub fn resolve_type_alias(&self, scope: Option<SymId>, name: &str) -> Result<Option<Type>, CodeGenError> {
        let mut stack = Vec::new();
        self.resolve_type_alias_inner(scope, name, &mut stack)
    }

    fn resolve_type_alias_inner(&self, scope: Option<SymId>, name: &str, stack: &mut Vec<String>) -> Result<Option<Type>, CodeGenError> {
        let Some(sym) = self.resolve(scope, name) else {
            return Ok(None);
        };

        if sym.data.kind() != LSyntaxKind::TYPE_ALIAS {
            return Ok(None);
        }

        let alias = TypeAlias::cast(sym.data.clone()).ok_or_else(|| CodeGenContext::internal_error("Expected type alias node"))?;
        let alias_name = alias.ident().unwrap_or_else(|| name.to_string());
        if stack.contains(&alias_name) {
            stack.push(alias_name);
            return Err(CodeGenContext::internal_error(format!("Circular type alias: {}", stack.join(" -> "))));
        }

        stack.push(alias_name.clone());

        let alias_type_node = alias
            .type_node()
            .ok_or_else(|| CodeGenContext::internal_error(format!("Type alias '{}' missing type expression", alias_name)))?;
        let alias_type = Type::cast(alias_type_node).ok_or_else(|| CodeGenContext::internal_error("Expected Type node in type alias"))?;

        let alias_atoms = alias_type.type_atoms();
        if alias_atoms.len() == 1 {
            let alias_atom = &alias_atoms[0];
            if !alias_atom.is_array()
                && !alias_atom.is_optional()
                && alias_atom.as_record_type().is_none()
                && alias_atom.as_anon_model().is_none()
                && let Some(next_ref) = alias_atom.as_ref_name()
                && let Some(next_sym) = self.resolve(scope, &next_ref)
                && next_sym.data.kind() == LSyntaxKind::TYPE_ALIAS
            {
                return self.resolve_type_alias_inner(scope, &next_ref, stack);
            }
        }

        Ok(Some(alias_type))
    }

    pub fn error(&self, node: &LNode, message: &str) -> CodeGenError {
        CodeGenError::GenerationError(self.diag.error(node.text_range(), message))
    }

    pub fn internal_error(message: impl Into<String>) -> CodeGenError {
        CodeGenError::InternalError(message.into())
    }
}

pub fn convert_identifier_case(name: &str, case: Case) -> String {
    if case == Case::Pascal {
        return Converter::new().to_case(Case::Pascal).set_pattern(pascal_preserve_uppercase).convert(name);
    }

    name.to_case(case)
}

pub fn convert_user_identifier_case(name: &str, case: Case, preserve: bool) -> String {
    if preserve { name.to_string() } else { convert_identifier_case(name, case) }
}

pub fn convert_generated_identifier_case(name: &str, case: Case) -> String {
    name.to_case(case)
}

pub struct AnonymousTypeNamer {
    used_names: HashSet<String>,
}

impl AnonymousTypeNamer {
    pub fn new(ctx: &CodeGenContext, case: Case) -> Self {
        let used_names = ctx
            .symbols
            .all_entries()
            .into_iter()
            .filter(|entry| matches!(entry.data.kind(), LSyntaxKind::MODEL | LSyntaxKind::ENUM))
            .map(|entry| ctx.symbol_name(&entry.name, case))
            .collect();
        Self { used_names }
    }

    pub fn allocate(&mut self, ctx: &CodeGenContext, path: &[String], case: Case) -> String {
        let base = ctx.anonymous_type_base_name(path, case);
        if self.used_names.insert(base.clone()) {
            return base;
        }

        let anon = format!("{}Anon", base);
        if self.used_names.insert(anon.clone()) {
            return anon;
        }

        let mut suffix = 2;
        loop {
            let candidate = format!("{}Anon{}", base, suffix);
            if self.used_names.insert(candidate.clone()) {
                return candidate;
            }
            suffix += 1;
        }
    }
}

fn anonymous_name_part(part: &str) -> String {
    let decoded = if part.starts_with('"') && part.ends_with('"') {
        serde_json::from_str::<String>(part).unwrap_or_else(|_| part.trim_matches('"').to_string())
    } else {
        part.to_string()
    };

    let sanitized = decoded.chars().map(|ch| if ch.is_ascii_alphanumeric() || ch == '_' { ch } else { '_' }).collect::<String>();
    let trimmed = sanitized.trim_matches('_');
    if trimmed.is_empty() { "Field".to_string() } else { trimmed.to_string() }
}

fn pascal_preserve_uppercase(words: &[&str]) -> Vec<String> {
    words.iter().map(|word| uppercase_first_preserve_rest(word)).collect()
}

fn uppercase_first_preserve_rest(word: &str) -> String {
    let mut chars = word.chars();
    match chars.next() {
        Some(first) => first.to_uppercase().chain(chars).collect(),
        None => String::new(),
    }
}

pub trait NamedExt {
    fn ident(&self) -> Option<String>;
    fn label() -> &'static str;

    fn name(&self) -> Result<String, CodeGenError> {
        self.ident().ok_or_else(|| CodeGenContext::internal_error(format!("{} missing identifier", Self::label())))
    }

    fn scope_id(&self, ctx: &CodeGenContext, parent: Option<SymId>) -> Result<SymId, CodeGenError> {
        let name = self.name()?;
        ctx.resolve_id(parent, &name)
            .ok_or_else(|| CodeGenContext::internal_error(format!("Unresolved symbol for {}: {}", Self::label(), name)))
    }

    fn qualified_name(&self, ctx: &CodeGenContext, parent: Option<SymId>, case: Case) -> Result<String, CodeGenError> {
        let name = self.name()?;
        ctx.qualified_name(parent, &name, case)
            .ok_or_else(|| CodeGenContext::internal_error(format!("Failed to get qualified name for: {}", name)))
    }
}

impl NamedExt for Model {
    fn ident(&self) -> Option<String> {
        Model::ident(self)
    }
    fn label() -> &'static str {
        "Model"
    }
}

impl NamedExt for Enum {
    fn ident(&self) -> Option<String> {
        Enum::ident(self)
    }
    fn label() -> &'static str {
        "Enum"
    }
}

pub trait FieldExt {
    fn name(&self) -> Result<String, CodeGenError>;
    fn field_type(&self) -> Result<Type, CodeGenError>;
    fn alias(&self) -> Result<Option<String>, CodeGenError>;
    fn proto_tag(&self) -> Result<Option<i64>, CodeGenError>;
}

impl FieldExt for Field {
    fn name(&self) -> Result<String, CodeGenError> {
        self.ident().ok_or_else(|| CodeGenContext::internal_error("Field missing identifier"))
    }

    fn field_type(&self) -> Result<Type, CodeGenError> {
        self.ty().ok_or_else(|| CodeGenContext::internal_error("Field missing type"))
    }

    fn alias(&self) -> Result<Option<String>, CodeGenError> {
        let decorators = self.decorators();
        let field_dec = decorators.iter().find(|d| d.ident().as_deref() == Some(MODEL_FIELD_DECORATOR.id));
        if let Some(dec) = field_dec
            && let Some(alias_arg) = dec.arg(MODEL_FIELD_DECORATOR, &MODEL_FIELD_DECORATOR_ALIAS_ARG)
            && let Some(Literal::StringLiteral(s)) = alias_arg.literal()
        {
            return Ok(s.value());
        }
        Ok(None)
    }

    fn proto_tag(&self) -> Result<Option<i64>, CodeGenError> {
        let decorators = self.decorators();
        let field_dec = decorators.iter().find(|d| d.ident().as_deref() == Some(MODEL_FIELD_DECORATOR.id));
        if let Some(dec) = field_dec
            && let Some(proto_tag_arg) = dec.arg(MODEL_FIELD_DECORATOR, &MODEL_FIELD_DECORATOR_PROTO_TAG_ARG)
        {
            return match proto_tag_arg.literal() {
                Some(Literal::IntLiteral { value, .. }) => Ok(Some(value)),
                _ => Err(CodeGenContext::internal_error("Field proto_tag must be an integer")),
            };
        }
        Ok(None)
    }
}

pub trait EnumVariantExt {
    fn variant_value(&self) -> Result<String, CodeGenError>;
}

impl EnumVariantExt for EnumVariant {
    fn variant_value(&self) -> Result<String, CodeGenError> {
        self.value().ok_or_else(|| CodeGenContext::internal_error("Enum variant missing value"))
    }
}

pub struct DocEmitter;

impl DocEmitter {
    pub fn rust_docs(docs: &[String], indent: usize) -> String {
        let indent_str = " ".repeat(indent * 4);
        docs.iter().map(|line| format!("{}/// {}\n", indent_str, line.trim())).collect()
    }

    pub fn python_docstring(docs: &[String]) -> String {
        if docs.len() == 1 {
            format!("\"\"\"{}\"\"\"\n", docs[0].trim())
        } else {
            let mut s = String::from("\"\"\"\n");
            for line in docs {
                s.push_str(&format!("{}\n", line.trim()));
            }
            s.push_str("\"\"\"\n");
            s
        }
    }

    pub fn joined(docs: &[String]) -> String {
        docs.join("\n")
    }
}

pub struct TypeMapper;

impl TypeMapper {
    pub fn to_rust(primitive: PrimitiveType) -> &'static str {
        match primitive {
            PrimitiveType::Any => "serde_json::Value",
            PrimitiveType::String => "String",
            PrimitiveType::Int => "i64",
            PrimitiveType::Float => "f64",
            PrimitiveType::Bool => "bool",
        }
    }

    pub fn to_python(primitive: PrimitiveType) -> &'static str {
        match primitive {
            PrimitiveType::Any => "Any",
            PrimitiveType::String => "str",
            PrimitiveType::Int => "int",
            PrimitiveType::Float => "float",
            PrimitiveType::Bool => "bool",
        }
    }

    pub fn to_json_schema(primitive: PrimitiveType) -> &'static str {
        match primitive {
            PrimitiveType::Any => "object",
            PrimitiveType::String => "string",
            PrimitiveType::Int => "integer",
            PrimitiveType::Float => "number",
            PrimitiveType::Bool => "boolean",
        }
    }

    pub fn to_openapi(primitive: PrimitiveType) -> (&'static str, Option<&'static str>) {
        match primitive {
            PrimitiveType::Any => ("object", None),
            PrimitiveType::String => ("string", None),
            PrimitiveType::Int => ("integer", None),
            PrimitiveType::Float => ("number", Some("double")),
            PrimitiveType::Bool => ("boolean", None),
        }
    }

    pub fn to_protobuf(primitive: PrimitiveType) -> &'static str {
        match primitive {
            PrimitiveType::Any => "google.protobuf.Any",
            PrimitiveType::String => "string",
            PrimitiveType::Int => "int32",
            PrimitiveType::Float => "float",
            PrimitiveType::Bool => "bool",
        }
    }

    pub fn to_go(primitive: PrimitiveType) -> &'static str {
        match primitive {
            PrimitiveType::Any => "interface{}",
            PrimitiveType::String => "string",
            PrimitiveType::Int => "int64",
            PrimitiveType::Float => "float64",
            PrimitiveType::Bool => "bool",
        }
    }
}

pub fn indent(text: &str, spaces: usize) -> String {
    let indent_str = " ".repeat(spaces);
    text.lines()
        .map(|line| if line.is_empty() { String::new() } else { format!("{}{}", indent_str, line) })
        .collect::<Vec<_>>()
        .join("\n")
        + if text.ends_with('\n') { "\n" } else { "" }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn pascal_case_preserves_existing_uppercase_runs() {
        assert_eq!(convert_identifier_case("XMLDocument", Case::Pascal), "XMLDocument");
        assert_eq!(convert_identifier_case("HTMLParser", Case::Pascal), "HTMLParser");
        assert_eq!(convert_identifier_case("xml_document", Case::Pascal), "XmlDocument");
        assert_eq!(convert_identifier_case("XML_document", Case::Pascal), "XMLDocument");
    }

    #[test]
    fn generated_identifiers_use_standard_case_conversion() {
        assert_eq!(convert_generated_identifier_case("PRODUCT_NOT_FOUND", Case::Pascal), "ProductNotFound");
        assert_eq!(convert_generated_identifier_case("HTML-mode", Case::Pascal), "HtmlMode");
    }
}
