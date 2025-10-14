use crate::{ConstantValue, Type};

#[derive(Debug, Clone)]
pub struct Field {
    pub name: String,
    pub doc: Option<String>,
    pub ty: Type,
    pub default: Option<ConstantValue>,
}
