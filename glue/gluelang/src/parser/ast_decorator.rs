use std::collections::HashMap;

use crate::ConstantValue;

#[derive(Debug, Clone)]
pub struct Decorator {
    pub name: String,
    pub named_args: HashMap<String, ConstantValue>,
    pub positional_args: Vec<ConstantValue>,
}
