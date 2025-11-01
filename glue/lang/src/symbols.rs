use std::collections::HashSet;

use slotmap::{HopSlotMap, new_key_type};

new_key_type! { pub struct SymId; }

#[derive(Debug, Clone)]
pub struct SymEntry(String);

pub struct SymTable(HopSlotMap<SymId, SymEntry>);

impl SymTable {
    pub fn new() -> Self {
        Self(HopSlotMap::with_key())
    }

    pub fn add<T: Into<String>>(&mut self, entry: T) -> SymId {
        let entry = SymEntry(entry.into());
        let sym_id = self.0.insert(entry);
        sym_id
    }

    pub fn add_to_scope<T: Into<String>>(&mut self, scope: Option<SymId>, entry: T) -> SymId {
        let mut new_entry_name = entry.into();
        if let Some(scope) = scope
            && let Some(scope_entry) = self.0.get(scope)
        {
            new_entry_name = Self::join_entries(&scope_entry.0, &new_entry_name);
        }
        self.add(new_entry_name)
    }

    pub fn resolve(&self, scope: Option<SymId>, entry: &str) -> Option<SymId> {
        let mut full_entry_name = entry.to_string();
        if let Some(scope) = scope
            && let Some(scope_entry) = self.0.get(scope)
        {
            full_entry_name = Self::join_entries(&scope_entry.0, &full_entry_name);
        }
        for (sym_id, sym_entry) in self.0.iter() {
            if sym_entry.0 == full_entry_name {
                return Some(sym_id);
            }
        }
        None
    }

    pub fn entries(&self, scope: Option<SymId>) -> Vec<(SymId, &str)> {
        // Receives e.g., the scope of "User::Details::name" and reeturns all entries,
        // for which the ID starts with "User::Details::", "User::", or "".

        let top_level_entries = self
            .0
            .iter()
            .filter_map(|(sym_id, sym_entry)| if !sym_entry.0.contains("::") { Some((sym_id, sym_entry.0.as_str())) } else { None })
            .collect::<Vec<_>>();

        let entry = match scope {
            Some(scope) => {
                if let Some(scope_entry) = self.0.get(scope) {
                    scope_entry.0.as_str()
                } else {
                    return vec![];
                }
            }
            None => {
                return top_level_entries;
            }
        };

        let prefixes: Vec<_> = entry.split("::").collect();
        let mut results: HashSet<_> = HashSet::from_iter(top_level_entries);
        for (sym_id, sym_entry) in self.0.iter() {
            for i in 0..=prefixes.len() {
                let prefix = prefixes[..i].join("::");
                let expected_prefix = if prefix.is_empty() { "".to_string() } else { format!("{}::", prefix) };
                if sym_entry.0.starts_with(&expected_prefix) && sym_entry.0 != entry {
                    results.insert((sym_id, sym_entry.0.as_str()));
                    break;
                }
            }
        }
        results.into_iter().collect()
    }

    // This symbol table is a very simple implementation where the identifiers are joine with "::" (assumed not to be a valid string in an identifier).
    fn join_entries(prefix: &str, entry: &str) -> String {
        if prefix.is_empty() { entry.to_string() } else { format!("{}::{}", prefix, entry) }
    }
}

impl std::fmt::Debug for SymTable {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_map().entries(self.0.iter().map(|(k, v)| (k, &v.0))).finish()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_sym_table_insert() {
        let mut sym_table = SymTable::new();
        let user_sym = sym_table.add("User".to_string());
        sym_table.add_to_scope(Some(user_sym), "id".to_string());
        sym_table.add("Product".to_string());

        assert!(sym_table.resolve(None, "User").is_some());
        assert!(sym_table.resolve(Some(user_sym), "id").is_some());
        assert!(sym_table.resolve(None, "id").is_none());

        assert_eq!(sym_table.entries(None).len(), 2);
        assert_eq!(sym_table.entries(Some(user_sym)).len(), 3);
        let mut user_scope_entry_names = sym_table.entries(Some(user_sym)).iter().map(|(_, name)| *name).collect::<Vec<_>>();
        user_scope_entry_names.sort();
        assert_eq!(user_scope_entry_names, vec!["Product", "User", "User::id"]);
    }
}
