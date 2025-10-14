use crate::{PrimitiveType, TypeVariant};

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
            TypeVariant::Ref { name, effective_name, .. } => format!("#{}{}", name, if name != effective_name { format!(" (-> {})", effective_name) } else { "".to_string() }),
            TypeVariant::AnonymousModel => "anonymous_model".to_string(),
        };
        let array_suffix = if self.is_array { "[]" } else { "" };
        let optional_suffix = if self.is_optional { "?" } else { "" };
        write!(f, "{base}{array_suffix}{optional_suffix}")
    }
}
