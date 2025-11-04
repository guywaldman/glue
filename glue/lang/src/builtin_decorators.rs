use std::collections::HashSet;

use crate::PrimitiveType;

/// Definition of argument for built-in decorator in Glue.
#[derive(Debug, Clone)]
pub struct DecoratorArgDef {
    /// The identifier/name of the argument.
    pub id: &'static str,
    /// Documentation string for the argument.
    pub doc: &'static str,
    /// The type of the argument.
    pub ty: PrimitiveType,
    /// Whether the argument is required.
    pub required: bool,
    /// The expected position of the argument if it is positional (None if named).
    pub expected_position: Option<usize>,
}

/// Definition of a decorator in Glue.
/// In Glue, the positional arguments can also be substitued for named arguments of the same ID (similarly to Python).
pub struct DecoratorDef {
    /// The name and ID of the decorator (e.g., "field" for `@field`).
    pub id: &'static str,
    /// List of positional arguments for the decorator.
    pub positional_args: &'static [DecoratorArgDef],
    /// List of named arguments for the decorator.
    pub named_args: &'static [DecoratorArgDef],

    doc: &'static str,
}

impl DecoratorDef {
    /// Returns all arguments for the decorator - both positional and named.
    pub fn args(&self) -> Vec<&DecoratorArgDef> {
        self.positional_args.iter().chain(self.named_args.iter()).collect()
    }

    /// Documentation string for the decorator (used in error messages, LSP features, etc.).
    pub fn doc(&self) -> String {
        let mut arg_ids: HashSet<&str> = HashSet::new();
        let arguments_doc = self
            .args()
            .iter()
            .filter_map(|arg| {
                if arg_ids.contains(arg.id) {
                    return None;
                }
                let is_named = self.named_args.iter().any(|named_arg| named_arg.id == arg.id);
                let arg_type = format!("{}", arg.ty);
                let arg_pos = {
                    let mut out = String::new();
                    if is_named {
                        out.push_str("named arg");
                    }
                    if let Some(pos) = arg.expected_position {
                        if !out.is_empty() {
                            out.push_str(", ");
                        }
                        out.push_str(&format!("positional at pos {}", pos));
                    }
                    out
                };
                arg_ids.insert(arg.id);
                Some(format!("- {} ({}) - <{}: {}>{}", arg.id, arg_type, arg_pos, arg.doc, if arg.required { " [required]" } else { "" }))
            })
            .collect::<Vec<String>>()
            .join("\n");
        format!("Decorator: @{}\n\n{}\n\nArguments:\n{}", self.id, self.doc, arguments_doc)
    }
}

/// Decorator applied to model fields to provide additional metadata.
pub const MODEL_FIELD_DECORATOR: &DecoratorDef = {
    &DecoratorDef {
        id: "field",
        doc: "Decorator applied to model fields to provide additional metadata.",
        positional_args: &[MODEL_FIELD_DECORATOR_ALIAS_ARG],
        named_args: &[MODEL_FIELD_DECORATOR_ALIAS_ARG],
    }
};
pub const MODEL_FIELD_DECORATOR_ALIAS_ARG: DecoratorArgDef = DecoratorArgDef {
    id: "alias",
    doc: "Describes the alias for the a field",
    ty: PrimitiveType::String,
    required: true,
    expected_position: Some(0),
};

/// All built-in decorators available in Glue.
pub const BUILTIN_DECORATORS: &[&DecoratorDef] = &[MODEL_FIELD_DECORATOR];
