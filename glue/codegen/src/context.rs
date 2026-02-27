use config::GlueConfigSchemaGeneration;
use convert_case::{Case, Casing};
use lang::{
    AstNode, DiagnosticContext, Enum, EnumVariant, Field, LNode, LSyntaxKind, Literal, MODEL_FIELD_DECORATOR, MODEL_FIELD_DECORATOR_ALIAS_ARG, Model, PrimitiveType, SourceCodeMetadata, SymId,
    SymTable, Type, TypeAlias, TypeAtom,
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

    pub fn qualified_name(&self, scope: Option<SymId>, name: &str, case: Case) -> Option<String> {
        self.resolve(scope, name).map(|entry| lang::symbol_name_to_parts(&entry.name).join("_").to_case(case))
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
