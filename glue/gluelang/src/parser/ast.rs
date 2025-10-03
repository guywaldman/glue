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

#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum AstNodeKind {
    #[default]
    Root,
    Enum,
    Model,
    Field,
    Decorator,
    Identifier,
    Type,
}

#[derive(Debug, Clone)]
pub enum AstNodePayload {
    None,
    String(String),
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
    Type(Type),
}

#[derive(Clone)]
pub struct AstNode {
    id: TreeNodeId,
    kind: AstNodeKind,
    payload: AstNodePayload,
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
        let label = match (&self.kind, &self.payload) {
            (AstNodeKind::Root, _) => "Root".to_string(),
            (AstNodeKind::Model, AstNodePayload::Model { name, .. }) => format!("Model({name})"),
            (AstNodeKind::Field, AstNodePayload::Field { name, ty, .. }) => format!("Field(name: {name}, ty: {ty})"),
            (AstNodeKind::Enum, AstNodePayload::Enum { name, variants, .. }) => format!("Enum(name: {name}, {variants:?})"),
            (AstNodeKind::Decorator, AstNodePayload::Decorator { name, args }) => format!("Decorator(name: {name}, args: {args:?})"),
            (AstNodeKind::Identifier, AstNodePayload::String(name)) => format!("Identifier({name})"),
            (AstNodeKind::Type, AstNodePayload::Type(ty)) => format!("Type({ty})"),
            _ => format!("{:?}", self.kind),
        };
        write!(f, "{label}")
    }
}

impl default::Default for AstNode {
    fn default() -> Self {
        Self {
            id: Default::default(),
            kind: AstNodeKind::Root,
            payload: AstNodePayload::None,
            tokens: vec![],
            span: Default::default(),
        }
    }
}

impl AstNode {
    pub fn new(kind: AstNodeKind, payload: AstNodePayload) -> Self {
        Self {
            kind,
            payload,
            ..Default::default()
        }
    }

    pub fn new_with_span(kind: AstNodeKind, payload: AstNodePayload, span: Span) -> Self {
        Self {
            kind,
            payload,
            span,
            ..Default::default()
        }
    }

    pub fn kind(&self) -> AstNodeKind {
        self.kind
    }

    pub fn payload(&self) -> &AstNodePayload {
        &self.payload
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

    // Convenience methods to extract common payload data
    pub fn as_string(&self) -> Option<&str> {
        match &self.payload {
            AstNodePayload::String(s) => Some(s),
            _ => None,
        }
    }

    pub fn as_model(&self) -> Option<(&str, Option<&str>)> {
        match &self.payload {
            AstNodePayload::Model { name, doc } => Some((name, doc.as_deref())),
            _ => None,
        }
    }

    pub fn as_field(&self) -> Option<(&str, Option<&str>, &Type, Option<&ConstantValue>)> {
        match &self.payload {
            AstNodePayload::Field { name, doc, ty, default } => Some((name, doc.as_deref(), ty, default.as_ref())),
            _ => None,
        }
    }

    pub fn as_enum(&self) -> Option<(&str, Option<&str>, &[String], Option<&ConstantValue>)> {
        match &self.payload {
            AstNodePayload::Enum { name, doc, variants, default } => Some((name, doc.as_deref(), variants, default.as_ref())),
            _ => None,
        }
    }

    pub fn as_type(&self) -> Option<&Type> {
        match &self.payload {
            AstNodePayload::Type(ty) => Some(ty),
            _ => None,
        }
    }

    pub fn as_decorator(&self) -> Option<(&str, &HashMap<String, ConstantValue>)> {
        match &self.payload {
            AstNodePayload::Decorator { name, args } => Some((name, args)),
            _ => None,
        }
    }
}
