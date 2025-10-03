use std::{collections::HashMap, default};

use crate::{
    Span,
    lexer::Token,
    parser::tree::{Tree, TreeNode, TreeNodeId},
};

pub type Ast = Tree<AstNode>;

pub type AstNodeId = TreeNodeId;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PrimitiveType {
    String,
    Int,
    Bool,
}

#[derive(Debug, Clone)]
pub enum TypeVariant {
    Primitive(PrimitiveType),
    Ref(String),
}

#[derive(Debug, Clone)]
pub enum ConstantValue {
    String(String),
    Int(i64),
    Bool(bool),
}

#[derive(Debug, Clone)]
pub struct TypeAtom {
    pub variant: TypeVariant,
    pub is_optional: bool,
    pub is_array: bool,
}

impl std::fmt::Display for TypeAtom {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let base = match &self.variant {
            TypeVariant::Primitive(p) => match p {
                PrimitiveType::String => "string".to_string(),
                PrimitiveType::Int => "int".to_string(),
                PrimitiveType::Bool => "bool".to_string(),
            },
            TypeVariant::Ref(name) => name.clone(),
        };
        let array_suffix = if self.is_array { "[]" } else { "" };
        let optional_suffix = if self.is_optional { "?" } else { "" };
        write!(f, "{base}{array_suffix}{optional_suffix}")
    }
}

#[derive(Debug, Clone)]
pub enum Type {
    Single(TypeAtom),
    Union(Vec<TypeAtom>),
}

impl std::fmt::Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Single(atom) => write!(f, "{atom}"),
            Type::Union(atoms) => {
                let types: Vec<String> = atoms.iter().map(|atom| format!("{atom}")).collect();
                write!(f, "{}", types.join(" | "))
            }
        }
    }
}

#[derive(Debug, Clone)]
pub enum AstNodeKind {
    Enum {
        name: String,
        doc: Option<String>,
        variants: Vec<String>,
        default: Option<ConstantValue>,
    },
    Model {
        name: String,
        doc: Option<String>,
    },
    Field {
        name: String,
        doc: Option<String>,
        ty: Type,
        default: Option<ConstantValue>,
    },
    Decorator {
        name: String,
        args: HashMap<String, ConstantValue>,
    },
    Identifier(String),
    Type(Type),
    Root,
}

#[derive(Clone)]
pub struct AstNode {
    id: TreeNodeId,
    kind: AstNodeKind,
    tokens: Vec<Token>,
    span: Span,
}

impl TreeNode for AstNode {
    fn id(&self) -> TreeNodeId {
        self.id
    }

    fn set_id(&mut self, id: TreeNodeId) {
        self.id = id;
    }
}

impl std::fmt::Debug for AstNode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        // We only print the important fields for brevity.
        let label = match &self.kind {
            AstNodeKind::Root => "Root".to_string(),
            AstNodeKind::Model { name, .. } => format!("Model({name})"),
            AstNodeKind::Field { name, ty, .. } => format!("Field(name: {name}, ty: {ty})"),
            AstNodeKind::Enum { name, variants, .. } => format!("Enum(name: {name}, {variants:?})"),
            AstNodeKind::Decorator { name, args } => format!("Decorator(name: {name}, args: {args:?})"),
            AstNodeKind::Identifier(name) => format!("Identifier({name})"),
            AstNodeKind::Type(ty) => format!("Type({ty})"),
        };
        write!(f, "{label}")
    }
}

impl default::Default for AstNode {
    fn default() -> Self {
        Self {
            id: Default::default(),
            kind: AstNodeKind::Root,
            tokens: vec![],
            span: Default::default(),
        }
    }
}

impl AstNode {
    pub fn new(kind: AstNodeKind) -> Self {
        Self { kind, ..Default::default() }
    }

    pub fn new_with_span(kind: AstNodeKind, span: Span) -> Self {
        Self {
            kind,
            span,
            ..Default::default()
        }
    }

    pub fn kind(&self) -> &AstNodeKind {
        &self.kind
    }

    pub fn span(&self) -> &Span {
        &self.span
    }

    pub fn tokens(&self) -> &[Token] {
        &self.tokens
    }

    pub fn set_tokens(&mut self, tokens: Vec<Token>) {
        self.tokens = tokens;
    }
}
