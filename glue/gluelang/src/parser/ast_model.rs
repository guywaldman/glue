use std::collections::BTreeMap;

use log::debug;

use crate::{Ast, AstNodeId, SymbolTable, TreeNode, parser::ast::TypeRef};

#[derive(Debug, Clone)]
pub struct Model {
    pub name: String,
    pub decorator: Option<AstNodeId>,
    /// The effective name is the name used in the generated code, which may differ from the original name.
    pub effective_name: Option<String>,
    pub doc: Option<String>,
    pub fields: BTreeMap<String, AstNodeId>,
}

impl Model {
    pub fn all_fields_have_defaults(&self, ast: &Ast, symbols: &SymbolTable) -> bool {
        debug!("Checking if all fields in model '{}' have defaults", self.name);
        let res = self.fields.values().map(|field_node_id| ast.get_node(*field_node_id)).all(|field_node| {
            field_node
                .map(|n| {
                    n.as_field()
                        .map(|field| {
                            debug!("Checking field '{}' for default", field.name);

                            if field.default.is_some() {
                                return true;
                            }

                            if let Some(TypeRef {
                                name: ref_name, effective_name, ..
                            }) = field.as_ref()
                            {
                                debug!("Field '{}' is a reference to '{}' (effective '{}')", field.name, ref_name, effective_name);

                                if let Some(resolved_ref_node) = symbols.resolve_ref(ast, n.parent_id().unwrap(), ref_name)
                                    && let Some(referenced_model) = resolved_ref_node.as_model()
                                {
                                    return referenced_model.all_fields_have_defaults(ast, symbols);
                                }

                                return false;
                            }

                            false
                        })
                        .unwrap_or(false)
                })
                .unwrap_or(false)
        });
        debug!("All fields in model '{}' have defaults: {}", self.name, res);
        res
    }
}
