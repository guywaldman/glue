use crate::{
    AnalyzedProgram, Diagnostic, ParserError, SemanticAnalyzerError,
    syntax::{LNode, LNodeOrToken, LSyntaxKind},
};

#[derive(Debug, serde::Serialize)]
pub struct GlueIr {
    pub version: String,
    pub file_name: String,
    pub root: Option<GlueIrNode>,
    pub errors: Vec<GlueIrError>,
    #[serde(skip_serializing)]
    analyzed_program: Option<AnalyzedProgram>,
}

impl GlueIr {
    pub fn from_analyzed(file_name: &str, analyzed_program: AnalyzedProgram) -> Self {
        Self {
            version: "1".to_string(),
            file_name: file_name.to_string(),
            root: Some(GlueIrNode::from_node(&analyzed_program.ast_root)),
            errors: Vec::new(),
            analyzed_program: Some(analyzed_program),
        }
    }

    pub fn from_analyzed_program(file_name: &str, analyzed_program: AnalyzedProgram) -> Self {
        Self {
            version: "1".to_string(),
            file_name: file_name.to_string(),
            root: None,
            errors: Vec::new(),
            analyzed_program: Some(analyzed_program),
        }
    }

    pub fn from_parsed(file_name: &str, ast_root: &LNode) -> Self {
        Self {
            version: "1".to_string(),
            file_name: file_name.to_string(),
            root: Some(GlueIrNode::from_node(ast_root)),
            errors: Vec::new(),
            analyzed_program: None,
        }
    }

    pub fn from_errors_only(file_name: &str, errors: Vec<GlueIrError>) -> Self {
        Self {
            version: "1".to_string(),
            file_name: file_name.to_string(),
            root: None,
            errors,
            analyzed_program: None,
        }
    }

    pub fn add_errors<I>(&mut self, errors: I)
    where
        I: IntoIterator<Item = GlueIrError>,
    {
        self.errors.extend(errors);
    }

    pub fn into_analyzed_program(self) -> Option<AnalyzedProgram> {
        self.analyzed_program
    }
}

#[derive(Debug, Clone, serde::Serialize)]
pub struct GlueIrNode {
    pub kind: GlueIrNodeKind,
    pub span: GlueIrSpan,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub text: Option<String>,
    pub children: Vec<GlueIrNode>,
}

impl GlueIrNode {
    fn from_node(node: &LNode) -> Self {
        Self {
            kind: GlueIrNodeKind::from_lsyntax(node.kind()),
            span: GlueIrSpan::from_range(node.text_range()),
            text: None,
            children: node.children_with_tokens().filter_map(GlueIrNode::from_element).collect(),
        }
    }

    fn from_element(element: LNodeOrToken) -> Option<Self> {
        match element {
            rowan::NodeOrToken::Node(node) => Some(Self::from_node(&node)),
            rowan::NodeOrToken::Token(token) => {
                let kind = GlueIrNodeKind::from_semantic_token(token.kind())?;
                Some(Self {
                    kind,
                    span: GlueIrSpan::from_range(token.text_range()),
                    text: Some(token.text().to_string()),
                    children: Vec::new(),
                })
            }
        }
    }
}

#[derive(Debug, Clone, Copy, serde::Serialize)]
#[serde(rename_all = "snake_case")]
pub enum GlueIrNodeKind {
    Program,
    Model,
    ModelBody,
    AnonModel,
    Endpoint,
    Enum,
    EnumVariants,
    EnumVariant,
    Field,
    FieldName,
    FieldDefaultValue,
    Type,
    TypeAtom,
    TypeAtomModifiers,
    Decorator,
    DecoratorArgs,
    DecoratorNamedArg,
    DecoratorPositionalArg,
    Literal,
    TrueLiteral,
    FalseLiteral,
    Ident,
    BuiltinType,
    OptionalModifier,
    Record,
    StringLiteral,
    StringLiteralInner,
    Char,
    IntLiteral,
    BoolLiteral,
    ListLiteral,
    DocBlock,
    Unknown,
    Error,
}

impl GlueIrNodeKind {
    fn from_lsyntax(kind: LSyntaxKind) -> Self {
        match kind {
            LSyntaxKind::PROGRAM => Self::Program,
            LSyntaxKind::MODEL => Self::Model,
            LSyntaxKind::MODEL_BODY => Self::ModelBody,
            LSyntaxKind::ANON_MODEL => Self::AnonModel,
            LSyntaxKind::ENDPOINT => Self::Endpoint,
            LSyntaxKind::ENUM => Self::Enum,
            LSyntaxKind::ENUM_VARIANTS => Self::EnumVariants,
            LSyntaxKind::ENUM_VARIANT => Self::EnumVariant,
            LSyntaxKind::FIELD => Self::Field,
            LSyntaxKind::FIELD_NAME => Self::FieldName,
            LSyntaxKind::FIELD_DEFAULT_VALUE => Self::FieldDefaultValue,
            LSyntaxKind::TYPE => Self::Type,
            LSyntaxKind::TYPE_ATOM => Self::TypeAtom,
            LSyntaxKind::TYPE_ATOM_MODIFIERS => Self::TypeAtomModifiers,
            LSyntaxKind::DECORATOR => Self::Decorator,
            LSyntaxKind::DECORATOR_ARGS => Self::DecoratorArgs,
            LSyntaxKind::DECORATOR_NAMED_ARG => Self::DecoratorNamedArg,
            LSyntaxKind::DECORATOR_POSITIONAL_ARG => Self::DecoratorPositionalArg,
            LSyntaxKind::LITERAL => Self::Literal,
            LSyntaxKind::TRUE_LITERAL => Self::TrueLiteral,
            LSyntaxKind::FALSE_LITERAL => Self::FalseLiteral,
            LSyntaxKind::BOOL_LITERAL => Self::BoolLiteral,
            LSyntaxKind::INT_LITERAL => Self::IntLiteral,
            LSyntaxKind::LIST_LITERAL => Self::ListLiteral,
            LSyntaxKind::STRING_LITERAL => Self::StringLiteral,
            LSyntaxKind::STRING_LITERAL_INNER => Self::StringLiteralInner,
            LSyntaxKind::CHAR => Self::Char,
            LSyntaxKind::ERROR => Self::Error,
            _ => Self::Unknown,
        }
    }

    fn from_semantic_token(kind: LSyntaxKind) -> Option<Self> {
        match kind {
            LSyntaxKind::IDENT => Some(Self::Ident),
            LSyntaxKind::BUILTIN_TYPE => Some(Self::BuiltinType),
            LSyntaxKind::OPTIONAL_MODIFIER => Some(Self::OptionalModifier),
            LSyntaxKind::RECORD => Some(Self::Record),
            LSyntaxKind::STRING_LITERAL => Some(Self::StringLiteral),
            LSyntaxKind::STRING_LITERAL_INNER => Some(Self::StringLiteralInner),
            LSyntaxKind::CHAR => Some(Self::Char),
            LSyntaxKind::INT_LITERAL => Some(Self::IntLiteral),
            LSyntaxKind::BOOL_LITERAL => Some(Self::BoolLiteral),
            LSyntaxKind::TRUE_LITERAL => Some(Self::TrueLiteral),
            LSyntaxKind::FALSE_LITERAL => Some(Self::FalseLiteral),
            LSyntaxKind::LIST_LITERAL => Some(Self::ListLiteral),
            LSyntaxKind::DOC_BLOCK => Some(Self::DocBlock),
            _ => None,
        }
    }
}

#[derive(Debug, Clone, Copy, serde::Serialize)]
pub struct GlueIrSpan {
    pub start: u32,
    pub end: u32,
}

impl GlueIrSpan {
    fn from_range(range: rowan::TextRange) -> Self {
        let start = u32::from(range.start());
        let end = u32::from(range.end());
        Self { start, end }
    }
}

#[derive(Debug, Clone, serde::Serialize)]
pub struct GlueIrError {
    pub source: String,
    pub message: String,
    pub span: Option<GlueIrSpan>,
}

pub fn diagnostic_to_ir_error(diagnostic: &Diagnostic) -> GlueIrError {
    GlueIrError {
        source: "parser".to_string(),
        message: diagnostic.message().to_string(),
        span: Some(GlueIrSpan::from_range(diagnostic.span())),
    }
}

pub fn parser_error_to_ir_error(error: &ParserError) -> GlueIrError {
    GlueIrError {
        source: "parser".to_string(),
        message: error.report().to_string(),
        span: None,
    }
}

pub fn semantic_error_to_ir_error(error: &SemanticAnalyzerError) -> GlueIrError {
    GlueIrError {
        source: "semantic".to_string(),
        message: error.report().to_string(),
        span: None,
    }
}
