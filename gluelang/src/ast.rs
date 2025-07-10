#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Ast {
    pub nodes: Vec<AstNode>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ModelFieldType {
    String,
    Integer,
    Float,
    Boolean,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum AstNode {
    Model {
        name: String,
        fields: Vec<AstNode>,
        pos: (usize, usize),
    },
    Identifier {
        name: String,
        pos: (usize, usize),
    },
    ModelField {
        name: String,
        typ: ModelFieldType,
        pos: (usize, usize),
    },
}

pub trait AstNodeExt {
    fn pos(&self) -> (usize, usize);
}

impl AstNodeExt for AstNode {
    fn pos(&self) -> (usize, usize) {
        match self {
            AstNode::Model { pos, .. } => *pos,
            AstNode::Identifier { pos, .. } => *pos,
            AstNode::ModelField { pos, .. } => *pos,
        }
    }
}
