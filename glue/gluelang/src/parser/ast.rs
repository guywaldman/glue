use std::{collections::HashMap, default};

use crate::{
    Span,
    lexer::Token,
    parser::tree::{Tree, TreeNode, TreeNodeId},
};

pub type Ast = Tree<AstNode>;

pub type AstNodeId = TreeNodeId;

impl Ast {
    pub fn find_nodes_at_position(&self, line: usize, col: usize) -> Vec<AstNode> {
        self.find(|node| {
            let span = node.span();
            // Spans use half-open ranges (inclusive start, exclusive end)

            if span.lines.0 == span.lines.1 {
                // Single-line span
                line == span.lines.0 && span.cols.0 <= col && col < span.cols.1
            } else {
                // Multi-line span
                if line == span.lines.0 {
                    col >= span.cols.0
                } else if line == span.lines.1 {
                    col < span.cols.1
                } else {
                    line > span.lines.0 && line < span.lines.1
                }
            }
        })
    }

    pub fn find_narrowest_node_at_position(&self, line: usize, col: usize) -> Option<AstNode> {
        self.find_nodes_at_position(line, col).into_iter().min_by_key(|node| {
            let span = node.span();
            let has_children = self.get_children(node.id()).map(|c| !c.is_empty()).unwrap_or(false);
            (has_children, span.chars.1 - span.chars.0)
        })
    }

    // Specialized implementations

    pub fn get_type_atoms(&self, node: &AstNode) -> Option<Vec<TypeAtom>> {
        let mut result = vec![];
        for type_node in self.get_children_fn(node.id(), |n| n.kind() == AstNodeKind::Type)? {
            for type_atom_node in self.get_children_fn(type_node.id(), |n| n.kind() == AstNodeKind::TypeAtom)? {
                if let AstNodePayload::TypeAtom { ty } = type_atom_node.payload().clone() {
                    result.push(ty);
                }
            }
        }
        Some(result)
    }
}

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
    TypeAtom,
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
    TypeAtom {
        ty: TypeAtom,
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
    parent_id: Option<TreeNodeId>,
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

    fn parent_id(&self) -> Option<TreeNodeId> {
        self.parent_id
    }

    fn set_parent_id(&mut self, parent_id: Option<TreeNodeId>) {
        self.parent_id = parent_id;
    }
}

impl std::fmt::Debug for AstNode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let label = match (&self.kind, &self.payload) {
            (AstNodeKind::Root, _) => "Root".to_string(),
            (AstNodeKind::Model, AstNodePayload::Model { name, .. }) => format!("Model(model {name})"),
            (AstNodeKind::Field, AstNodePayload::Field { name, ty, .. }) => format!("Field({name}: {ty})"),
            (AstNodeKind::Enum, AstNodePayload::Enum { name, variants, .. }) => {
                let variants_str = variants.join(" | ");
                format!("Enum({name}: {variants_str})")
            }
            (AstNodeKind::Decorator, AstNodePayload::Decorator { name, args }) => format!("Decorator(@{name}, args: {args:?})"),
            (AstNodeKind::Identifier, AstNodePayload::String(name)) => format!("Identifier({name})"),
            (AstNodeKind::Type, AstNodePayload::Type(ty)) => format!("Type({ty})"),
            (AstNodeKind::TypeAtom, AstNodePayload::TypeAtom { ty }) => match &ty.variant {
                TypeVariant::Primitive(p) => format!("TypeAtom({p:?}{})", if ty.is_array { "[]" } else { "" }),
                TypeVariant::Ref(name) => format!("TypeAtom(#{}{})", name, if ty.is_array { "[]" } else { "" }),
            },
            _ => format!("{:?}", self.kind),
        };
        write!(f, "{label}")
    }
}

impl default::Default for AstNode {
    fn default() -> Self {
        Self {
            id: Default::default(),
            parent_id: None,
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

    pub fn payload_mut(&mut self) -> &mut AstNodePayload {
        &mut self.payload
    }

    pub fn set_payload(&mut self, payload: AstNodePayload) {
        self.payload = payload;
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
}
