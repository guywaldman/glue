use std::collections::HashMap;

use log::info;

use crate::parser::AstNodeId;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum AstSymbol {
    /// Represents a model by its identifier.
    /// Nesting is represented by concatenating names with dots, e.g. "Post.AdditionalPostDetails"
    Model(String),
    /// Represents an enum by its identifier.
    /// Nesting is represented by concatenating names with dots, e.g. "Post.Status"
    Enum(String),
    /// Represents a field by its identifier.
    /// Nesting is represented by concatenating names with dots, e.g. "Post.title
    Field(String),
}

/// A mapping from symbols to their corresponding AST node IDs within a specific scope.
type SymbolsMapPerScope = HashMap<AstSymbol, SymbolTableEntry>;

#[derive(Debug, Clone, Default, PartialEq)]
pub struct SymbolTableEntry {
    pub id: AstNodeId,
    symbols: SymbolsMapPerScope,
    parent: Option<AstNodeId>,
}

impl SymbolTableEntry {
    fn insert(&mut self, symbol: AstSymbol, node_id: AstNodeId, parent_scope: Option<AstNodeId>) {
        let new_entry = if let Some(parent_scope) = parent_scope {
            SymbolTableEntry {
                id: node_id,
                symbols: HashMap::new(),
                parent: Some(parent_scope),
            }
        } else {
            SymbolTableEntry {
                id: node_id,
                symbols: HashMap::new(),
                parent: None,
            }
        };

        self.symbols.insert(symbol, new_entry);
    }
}

/// A symbol table that maps symbols to their corresponding AST node IDs within different scopes.
/// A scope is represented by an `AstNodeId`, and each scope can have its own set of symbols.
#[derive(Debug, Clone, Default)]
pub struct SymbolTable {
    entries: HashMap<AstNodeId, SymbolTableEntry>,
}

impl SymbolTable {
    /// Inserts a symbol into the symbol table for a given scope.
    /// If the scope does not exist, it will be created.
    pub fn insert(&mut self, scope: AstNodeId, symbol: AstSymbol, node_id: AstNodeId, parent_scope: Option<AstNodeId>) {
        if let Some(parent_scope) = parent_scope {
            let entry = self.entries.entry(scope).or_default();
            entry.insert(symbol, node_id, Some(parent_scope));
        } else {
            let entry = self.entries.entry(scope).or_default();
            entry.insert(symbol, node_id, None);
        }
    }

    /// Retrieves the symbols map for a given scope.
    /// Traverses up the AST to include symbols from ancestor scopes.
    pub fn symbols_in_scope(&self, scope: AstNodeId) -> Option<SymbolsMapPerScope> {
        let mut result = HashMap::new();
        let mut current_id = scope;
        let entry = self.entries.get(&current_id);
        if entry.is_none() {
            current_id = self.get_parent(current_id)?;
            info!("Adjusted current_id to parent: {current_id:?}");
        }

        // Track visited scopes to avoid potential cycles (defensive programming).
        let mut visited = std::collections::HashSet::new();
        loop {
            if !visited.insert(current_id) {
                // Cycle detected; abort to prevent infinite loop.
                break;
            }
            let Some(entry) = self.entries.get(&current_id) else {
                break;
            };

            for (symbol, node_id) in &entry.symbols {
                result.insert(symbol.clone(), node_id.clone());
            }

            if let Some(parent_id) = entry.parent {
                current_id = parent_id;
            } else {
                break;
            }
        }

        Some(result)
    }

    /// Looks up a symbol in the symbol table for a given scope.
    /// Returns `Some(AstNodeId)` if the symbol is found, otherwise returns `None`.
    pub fn lookup(&self, scope: AstNodeId, symbol: &AstSymbol) -> Option<AstNodeId> {
        let scope_symbols = self.symbols_in_scope(scope)?;
        scope_symbols.get(symbol).map(|entry| entry.id)
    }

    fn get_parent(&self, scope: AstNodeId) -> Option<AstNodeId> {
        let entry = self.entries.get(&scope);
        if let Some(e) = entry {
            e.parent
        } else {
            // Entry may be part of a nested scope - attempt to find the entry which contains it (inefficient, but OK for now).
            self.entries
                .values()
                .find(|e| e.symbols.values().any(|se| se.id == scope))
                .map(|entry| entry.id)
        }
    }

    /// Sets the parent scope for a given scope entry (creating the entry if absent).
    pub fn set_scope_parent(&mut self, scope: AstNodeId, parent: AstNodeId) {
        let entry = self.entries.entry(scope).or_default();
        entry.parent = Some(parent);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_symbol_table_insert_and_lookup() {
        // Arrange
        let mut table = SymbolTable::default();
        let user_model_symbol = AstSymbol::Model("User".to_string());
        let user_model_id: AstNodeId = 1.into();
        let user_field_name_symbol = AstSymbol::Field("name".to_string());
        let user_field_name_id: AstNodeId = 2.into();
        let user_field_age_symbol = AstSymbol::Field("age".to_string());
        let user_field_age_id: AstNodeId = 3.into();
        let user_field_address_symbol = AstSymbol::Field("address".to_string());
        let user_field_address_id: AstNodeId = 4.into();
        let nested_model_symbol = AstSymbol::Model("UserAddress".to_string());
        let nested_model_id: AstNodeId = 5.into();

        // Act
        table.insert(user_model_id, user_model_symbol.clone(), user_model_id, None);
        table.insert(user_model_id, user_field_name_symbol.clone(), user_field_name_id, Some(user_model_id));
        table.insert(user_model_id, user_field_age_symbol.clone(), user_field_age_id, Some(user_model_id));
        table.insert(user_model_id, user_field_address_symbol.clone(), user_field_address_id, Some(user_model_id));
        table.insert(user_field_address_id, nested_model_symbol.clone(), nested_model_id, Some(user_field_address_id));

        // Assert
        assert_eq!(table.lookup(user_model_id, &user_model_symbol), Some(user_model_id));
        assert_eq!(table.lookup(user_model_id, &user_field_name_symbol), Some(user_field_name_id));
        assert_eq!(table.lookup(user_model_id, &user_field_age_symbol), Some(user_field_age_id));
        assert_eq!(table.lookup(user_model_id, &user_field_address_symbol), Some(user_field_address_id));
    }
}
