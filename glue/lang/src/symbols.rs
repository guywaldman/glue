use std::collections::HashSet;

use slotmap::{HopSlotMap, new_key_type};

new_key_type! { pub struct SymId; }

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct SymEntry<TData> {
    pub id: SymId,
    pub name: String,
    pub data: TData,
}

#[derive(Clone)]
pub struct SymTable<TData>(HopSlotMap<SymId, SymEntry<TData>>);

const SYMBOL_NAME_SEPARATOR: &str = "::";

impl<TData> SymTable<TData>
where
    TData: Clone + std::hash::Hash + PartialEq + Eq,
{
    pub fn new() -> Self {
        Self(HopSlotMap::with_key())
    }

    pub fn len(&self) -> usize {
        self.0.len()
    }

    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    pub fn add<T: Into<String>>(&mut self, entry_name: T, data: TData) -> SymId {
        let entry = SymEntry {
            id: SymId::default(),
            name: entry_name.into(),
            data,
        };
        let sym_id = self.0.insert(entry);
        self.0.get_mut(sym_id).unwrap().id = sym_id;
        sym_id
    }

    pub fn add_to_scope<T: Into<String>>(&mut self, scope: Option<SymId>, entry_name: T, data: TData) -> SymId {
        let mut new_entry_name = entry_name.into();
        if let Some(scope) = scope
            && let Some(scope_entry) = self.0.get(scope)
        {
            new_entry_name = Self::join_entries(&scope_entry.name, &new_entry_name);
        }
        self.add(new_entry_name, data)
    }

    pub fn get(&self, id: SymId) -> Option<&SymEntry<TData>> {
        self.0.get(id)
    }

    pub fn resolve(&self, scope: Option<SymId>, entry_name: &str) -> Option<SymEntry<TData>> {
        let mut symbol_name_candidates = vec![entry_name.to_string()];
        if let Some(scope) = scope
            && let Some(scope_entry) = self.0.get(scope)
        {
            // Separate scope entry name to parts
            let scope_parts = symbol_name_to_parts(&scope_entry.name);
            for i in (0..=scope_parts.len()).rev() {
                let prefix = scope_parts[..i].join(SYMBOL_NAME_SEPARATOR);
                let candidate = if prefix.is_empty() {
                    entry_name.to_string()
                } else {
                    format!("{}{}{}", prefix, SYMBOL_NAME_SEPARATOR, entry_name)
                };
                symbol_name_candidates.push(candidate.clone());
            }
        }

        // TODO: Optimize
        for (_, sym_entry) in self.0.iter() {
            for candidate in &symbol_name_candidates {
                if sym_entry.name == *candidate {
                    return Some(sym_entry.clone());
                }
            }
        }
        None
    }

    pub fn resolve_id(&self, scope: Option<SymId>, entry_name: &str) -> Option<SymId> {
        self.resolve(scope, entry_name).map(|entry| entry.id)
    }

    pub fn resolve_nested_id(&self, parent_id: Option<SymId>, entry_name: &str) -> Option<SymId> {
        let Some(parent_id) = parent_id else {
            return self.resolve_id(None, entry_name);
        };

        let parent_entry = self.0.get(parent_id)?;
        let full_entry_name = Self::join_entries(&parent_entry.name, entry_name);
        for (_, sym_entry) in self.0.iter() {
            // TODO: Decide one way or the other...
            if sym_entry.name == full_entry_name || sym_entry.name == entry_name {
                return Some(sym_entry.id);
            }
        }
        None
    }

    pub fn entries(&self, scope: Option<SymId>) -> Vec<SymEntry<TData>> {
        // Receives e.g., the scope of "User::Details::name" and reeturns all entries,
        // for which the ID starts with "User::Details::", "User::", or "".

        let top_level_entries = self
            .0
            .iter()
            .filter_map(|(_, sym_entry)| if !sym_entry.name.contains(SYMBOL_NAME_SEPARATOR) { Some(sym_entry.clone()) } else { None })
            .collect::<Vec<_>>();

        let entry = match scope {
            Some(scope) => {
                if let Some(scope_entry) = self.0.get(scope) {
                    scope_entry.name.as_str()
                } else {
                    return vec![];
                }
            }
            None => {
                return top_level_entries;
            }
        };

        let prefixes: Vec<_> = entry.split(SYMBOL_NAME_SEPARATOR).collect();
        let mut results: HashSet<_> = HashSet::from_iter(top_level_entries);
        for (_, sym_entry) in self.0.iter() {
            for i in 0..=prefixes.len() {
                let prefix = prefixes[..i].join(SYMBOL_NAME_SEPARATOR);
                let expected_prefix = if prefix.is_empty() { "".to_string() } else { format!("{}{}", prefix, SYMBOL_NAME_SEPARATOR) };
                if sym_entry.name.starts_with(&expected_prefix) && sym_entry.name != entry {
                    results.insert(sym_entry.clone());
                    break;
                }
            }
        }
        results.into_iter().collect()
    }

    // This symbol table is a very simple implementation where the identifiers are joine with "::" (assumed not to be a valid string in an identifier).
    pub fn join_entries(prefix: &str, entry: &str) -> String {
        if prefix.is_empty() { entry.to_string() } else { format!("{}::{}", prefix, entry) }
    }
}

pub fn symbol_name_to_parts(name: &str) -> Vec<&str> {
    name.split(SYMBOL_NAME_SEPARATOR).collect()
}

impl<TData> std::fmt::Debug for SymTable<TData> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_map().entries(self.0.iter().map(|(k, v)| (k, &v.name))).finish()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_sym_table_insert() {
        let mut sym_table = SymTable::<usize>::new();
        let user_sym = sym_table.add("User".to_string(), 1);
        sym_table.add_to_scope(Some(user_sym), "id".to_string(), 2);
        sym_table.add("Product".to_string(), 3);

        assert!(sym_table.resolve(None, "User").is_some_and(|e| e.data == 1));
        assert!(sym_table.resolve(Some(user_sym), "id").is_some_and(|e| e.data == 2));
        assert!(sym_table.resolve(None, "id").is_none());

        assert_eq!(sym_table.entries(None).len(), 2);
        assert_eq!(sym_table.entries(Some(user_sym)).len(), 3);
        let mut user_scope_entry_names = sym_table.entries(Some(user_sym)).iter().map(|entry| entry.name.clone()).collect::<Vec<_>>();
        user_scope_entry_names.sort();
        assert_eq!(user_scope_entry_names, vec!["Product", "User", "User::id"]);
    }
}
