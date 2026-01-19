use config::GlueConfig;
use convert_case::{Case, Casing};
use lang::{AstNode, DiagnosticContext, Enum, EnumVariant, Field, LNode, LSyntaxKind, Literal, Model, PrimitiveType, SourceCodeMetadata, SymId, SymTable, Type, TypeAtom};

use crate::CodeGenError;

/// Shared context for all code generators.
/// Provides common functionality for traversing the AST and emitting code.
pub struct CodeGenContext<'a> {
    pub ast: LNode,
    pub symbols: SymTable<LNode>,
    pub diag: DiagnosticContext,
    pub config: Option<&'a GlueConfig>,
}

impl<'a> CodeGenContext<'a> {
    pub fn new(ast: LNode, symbols: SymTable<LNode>, source: &SourceCodeMetadata, config: Option<&'a GlueConfig>) -> Self {
        let diag = DiagnosticContext::new(source.file_name, source.file_contents);
        Self { ast, symbols, diag, config }
    }

    /// Resolve a symbol name within a scope, returning its ID
    pub fn resolve_id(&self, scope: Option<SymId>, name: &str) -> Option<SymId> {
        self.symbols.resolve_id(scope, name)
    }

    /// Resolve a symbol and get its entry
    pub fn resolve(&self, scope: Option<SymId>, name: &str) -> Option<lang::SymEntry<LNode>> {
        self.symbols.resolve(scope, name)
    }

    /// Get the qualified name for a symbol, formatted in a specific case
    pub fn qualified_name(&self, scope: Option<SymId>, name: &str, case: Case) -> Option<String> {
        self.resolve(scope, name).map(|entry| lang::symbol_name_to_parts(&entry.name).join("_").to_case(case))
    }

    /// Iterate over all top-level models
    pub fn top_level_models(&self) -> impl Iterator<Item = Model> + '_ {
        self.ast.children().filter(|n| n.kind() == LSyntaxKind::MODEL).filter_map(Model::cast)
    }

    /// Iterate over all top-level enums
    pub fn top_level_enums(&self) -> impl Iterator<Item = Enum> + '_ {
        self.ast.children().filter(|n| n.kind() == LSyntaxKind::ENUM).filter_map(Enum::cast)
    }

    /// Iterate over all top-level endpoints
    pub fn top_level_endpoints(&self) -> impl Iterator<Item = lang::Endpoint> + '_ {
        self.ast.children().filter(|n| n.kind() == LSyntaxKind::ENDPOINT).filter_map(lang::Endpoint::cast)
    }

    /// Find the root model (decorated with @root), or the single model if only one exists
    pub fn root_model(&self) -> Result<Model, CodeGenError> {
        let models: Vec<_> = self.top_level_models().collect();

        let root_models: Vec<_> = models.iter().filter(|m| m.decorators().iter().any(|d| d.ident().as_deref() == Some("root"))).cloned().collect();

        if root_models.len() > 1 {
            return Err(CodeGenError::GenerationError(
                self.diag.error(self.ast.text_range(), "Multiple root models found. Only one model should have the @root decorator."),
            ));
        }

        if let Some(root) = root_models.into_iter().next() {
            return Ok(root);
        }

        if models.len() == 1 {
            return Ok(models.into_iter().next().unwrap());
        }

        Err(CodeGenError::GenerationError(
            self.diag
                .error(self.ast.text_range(), "Multiple models found but none marked with @root. Please add @root to one model."),
        ))
    }

    pub fn error(&self, node: &LNode, message: &str) -> CodeGenError {
        CodeGenError::GenerationError(self.diag.error(node.text_range(), message))
    }

    pub fn internal_error(message: impl Into<String>) -> CodeGenError {
        CodeGenError::InternalError(message.into())
    }
}

/// Trait extension for Model to provide ergonomic access patterns
pub trait ModelExt {
    fn name(&self) -> Result<String, CodeGenError>;
    fn scope_id(&self, ctx: &CodeGenContext, parent: Option<SymId>) -> Result<SymId, CodeGenError>;
    fn qualified_name(&self, ctx: &CodeGenContext, parent: Option<SymId>, case: Case) -> Result<String, CodeGenError>;
}

impl ModelExt for Model {
    fn name(&self) -> Result<String, CodeGenError> {
        self.ident().ok_or_else(|| CodeGenContext::internal_error("Model missing identifier"))
    }

    fn scope_id(&self, ctx: &CodeGenContext, parent: Option<SymId>) -> Result<SymId, CodeGenError> {
        let name = self.name()?;
        ctx.resolve_id(parent, &name)
            .ok_or_else(|| CodeGenContext::internal_error(format!("Unresolved symbol for model: {}", name)))
    }

    fn qualified_name(&self, ctx: &CodeGenContext, parent: Option<SymId>, case: Case) -> Result<String, CodeGenError> {
        let name = self.name()?;
        ctx.qualified_name(parent, &name, case)
            .ok_or_else(|| CodeGenContext::internal_error(format!("Failed to get qualified name for: {}", name)))
    }
}

/// Trait extension for Enum to provide ergonomic access patterns
pub trait EnumExt {
    fn name(&self) -> Result<String, CodeGenError>;
    fn scope_id(&self, ctx: &CodeGenContext, parent: Option<SymId>) -> Result<SymId, CodeGenError>;
    fn qualified_name(&self, ctx: &CodeGenContext, parent: Option<SymId>, case: Case) -> Result<String, CodeGenError>;
}

impl EnumExt for Enum {
    fn name(&self) -> Result<String, CodeGenError> {
        self.ident().ok_or_else(|| CodeGenContext::internal_error("Enum missing identifier"))
    }

    fn scope_id(&self, ctx: &CodeGenContext, parent: Option<SymId>) -> Result<SymId, CodeGenError> {
        let name = self.name()?;
        ctx.resolve_id(parent, &name)
            .ok_or_else(|| CodeGenContext::internal_error(format!("Unresolved symbol for enum: {}", name)))
    }

    fn qualified_name(&self, ctx: &CodeGenContext, parent: Option<SymId>, case: Case) -> Result<String, CodeGenError> {
        let name = self.name()?;
        ctx.qualified_name(parent, &name, case)
            .ok_or_else(|| CodeGenContext::internal_error(format!("Failed to get qualified name for: {}", name)))
    }
}

/// Trait extension for Field to provide ergonomic access patterns
pub trait FieldExt {
    fn name(&self) -> Result<String, CodeGenError>;
    fn field_type(&self) -> Result<Type, CodeGenError>;
}

impl FieldExt for Field {
    fn name(&self) -> Result<String, CodeGenError> {
        self.ident().ok_or_else(|| CodeGenContext::internal_error("Field missing identifier"))
    }

    fn field_type(&self) -> Result<Type, CodeGenError> {
        self.ty().ok_or_else(|| CodeGenContext::internal_error("Field missing type"))
    }
}

/// Trait extension for EnumVariant
pub trait EnumVariantExt {
    fn variant_value(&self) -> Result<String, CodeGenError>;
}

impl EnumVariantExt for EnumVariant {
    fn variant_value(&self) -> Result<String, CodeGenError> {
        self.value().ok_or_else(|| CodeGenContext::internal_error("Enum variant missing value"))
    }
}

/// Helper for emitting documentation comments in different formats
pub struct DocEmitter;

impl DocEmitter {
    /// Emit documentation as Rust-style doc comments (/// ...)
    pub fn rust_docs(docs: &[String], indent: usize) -> String {
        let indent_str = " ".repeat(indent * 4);
        docs.iter().map(|line| format!("{}/// {}\n", indent_str, line.trim())).collect()
    }

    /// Emit documentation as Python docstring
    pub fn python_docstring(docs: &[String]) -> String {
        if docs.len() == 1 {
            format!("\"\"\"{}\"\"\"\n", docs[0].trim())
        } else {
            let mut docstring = String::from("\"\"\"\n");
            for line in docs {
                docstring.push_str(&format!("{}\n", line.trim()));
            }
            docstring.push_str("\"\"\"\n");
            docstring
        }
    }

    /// Join docs into a single string with newlines
    pub fn joined(docs: &[String]) -> String {
        docs.join("\n")
    }
}

/// Helper for mapping Glue primitive types to target language types
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

/// Indent multi-line text by a given number of spaces
pub fn indent(text: &str, spaces: usize) -> String {
    let indent_str = " ".repeat(spaces);
    text.lines()
        .map(|line| if line.is_empty() { String::new() } else { format!("{}{}", indent_str, line) })
        .collect::<Vec<_>>()
        .join("\n")
        + if text.ends_with('\n') { "\n" } else { "" }
}
