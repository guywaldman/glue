use crate::{ConstantValue, Type, TypeVariant, parser::ast::TypeRef};

#[derive(Debug, Clone)]
pub struct Field {
    pub name: String,
    pub doc: Option<String>,
    pub ty: Type,
    pub default: Option<ConstantValue>,
}

impl Field {
    pub fn as_ref(&self) -> Option<&TypeRef> {
        if let Type::Single(type_atom) = &self.ty
            && let TypeVariant::Ref(r) = &type_atom.variant
        {
            return Some(r);
        }
        None
    }
}
