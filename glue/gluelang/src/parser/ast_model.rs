use std::collections::BTreeMap;

use crate::AstNodeId;

#[derive(Debug, Clone)]
pub struct Model {
    pub name: String,
    pub decorator: Option<AstNodeId>,
    /// The effective name is the name used in the generated code, which may differ from the original name.
    pub effective_name: Option<String>,
    pub doc: Option<String>,
    pub fields: BTreeMap<String, AstNodeId>,
}

impl Model {}
