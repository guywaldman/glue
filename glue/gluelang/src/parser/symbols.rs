use std::collections::HashMap;

use crate::{Ast, TreeNode, parser::AstNodeId};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum AstSymbol {
    /// Represents a model by its identifier.
    Model(String),
    /// Represents an enum by its identifier.
    Enum(String),
    /// Represents a field by its identifier.
    Field(String),
    /// Represents an endpoint by its identifier.
    Endpoint(String),
}

impl AstSymbol {
    pub fn name(&self) -> &str {
        match self {
            AstSymbol::Model(name) => name,
            AstSymbol::Enum(name) => name,
            AstSymbol::Field(name) => name,
            AstSymbol::Endpoint(name) => name,
        }
    }
}

/// A mapping from symbols to their corresponding AST node IDs within a specific scope.
pub type SymbolsMapPerScope = HashMap<AstSymbol, SymbolTableEntry>;

#[derive(Debug, Clone, Default, PartialEq)]
pub struct SymbolTableEntry {
    pub id: AstNodeId,
    symbols: SymbolsMapPerScope,
}

impl SymbolTableEntry {
    fn insert(&mut self, symbol: AstSymbol, node_id: AstNodeId) {
        self.symbols.insert(
            symbol,
            SymbolTableEntry {
                id: node_id,
                symbols: HashMap::new(),
            },
        );
    }
}

pub type SymbolTableEntries = HashMap<AstNodeId, SymbolTableEntry>;

/// A symbol table that maps symbols to their corresponding AST node IDs within different scopes.
/// A scope is represented by an `AstNodeId`, and each scope can have its own set of symbols.
#[derive(Debug, Clone, Default)]
pub struct SymbolTable {
    entries: SymbolTableEntries,
}

impl SymbolTable {
    /// Inserts a symbol into the symbol table for a given scope.
    /// If the scope does not exist, it will be created.
    pub fn insert(&mut self, scope: AstNodeId, symbol: AstSymbol, node_id: AstNodeId) {
        let entry = self.entries.entry(scope).or_default();
        entry.insert(symbol, node_id);
    }

    /// Retrieves the symbols map for a given scope.
    /// Traverses up the AST to include symbols from ancestor scopes.
    pub fn symbols_in_scope(&self, ast: &Ast, scope: AstNodeId) -> Option<SymbolsMapPerScope> {
        let mut result = HashMap::new();
        let mut current_id = scope;
        let entry = self.entries.get(&current_id);
        if entry.is_none() {
            let ast_node = ast.get_node(current_id)?;
            let ast_parent_id = ast_node.parent_id();
            if let Some(parent_id) = ast_parent_id {
                current_id = parent_id;
            } else {
                return None;
            }
        }

        // Track visited scopes to avoid potential cycles.
        let mut visited = std::collections::HashSet::new();
        loop {
            if !visited.insert(current_id) {
                break;
            }

            // If this scope has symbols, collect them
            if let Some(entry) = self.entries.get(&current_id) {
                for (symbol, node_id) in &entry.symbols {
                    result.insert(symbol.clone(), node_id.clone());
                }
            }

            // Continue traversing up the tree
            let ast_node = ast.get_node(current_id)?;
            let ast_parent_id = ast_node.parent_id();

            if let Some(parent_id) = ast_parent_id {
                current_id = parent_id;
            } else {
                break;
            }
        }

        Some(result)
    }

    /// Looks up a symbol in the symbol table for a given scope.
    /// Returns `Some(AstNodeId)` if the symbol is found, otherwise returns `None`.
    pub fn lookup(&self, ast: &Ast, scope: AstNodeId, symbol: &AstSymbol) -> Option<AstNodeId> {
        let scope_symbols = self.symbols_in_scope(ast, scope)?;
        scope_symbols.get(symbol).map(|entry| entry.id)
    }
}
