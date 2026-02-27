use std::{
    any::Any,
    collections::HashMap,
    sync::{Arc, Mutex},
};

use log::debug;
use miette::Report;

use crate::{
    Decorator, DecoratorArg, Endpoint, EnumVariant, Literal, LiteralExpr, PrimitiveType, SourceCodeMetadata,
    builtin_decorators::{BUILTIN_DECORATORS, DecoratorDef},
    diagnostics::DiagnosticContext,
    symbols::{SymId, SymTable},
    syntax::{AnonModel, AstNode, Enum, Field, LNode, LNodeOrToken, LSyntaxKind, Model, ParsedProgram, Type, TypeAlias, TypeAtom},
    utils::fuzzy_match,
};

#[derive(Debug)]
pub enum SemanticAnalyzerError {
    DuplicateField(Report),
    UndefinedTypeReference(Report),
    ImportNotAtTop(Report),
    CircularTypeAlias(Report),
}

impl SemanticAnalyzerError {
    pub fn report(&self) -> &miette::Report {
        match self {
            SemanticAnalyzerError::DuplicateField(report)
            | SemanticAnalyzerError::UndefinedTypeReference(report)
            | SemanticAnalyzerError::ImportNotAtTop(report)
            | SemanticAnalyzerError::CircularTypeAlias(report) => report,
        }
    }
}

#[derive(Debug)]
pub struct AnalyzedProgram {
    pub ast_root: LNode,
    pub symbols: SymTable<LNode>,
}

pub struct SemanticAnalyzer {}

impl SemanticAnalyzer {
    pub fn new() -> Self {
        Self {}
    }

    pub fn analyze(&self, parsed: &ParsedProgram, source_code_metadata: &SourceCodeMetadata) -> Result<AnalyzedProgram, Vec<SemanticAnalyzerError>> {
        let diagnostic_ctx = DiagnosticContext::new(source_code_metadata.file_name, source_code_metadata.file_contents);
        let mut errors = Vec::new();

        let root = parsed.ast_root.clone();
        Self::check_imports_are_top_level(&root, &mut errors, diagnostic_ctx.clone());
        let targets: Vec<_> = root
            .children()
            .filter(|n| matches!(n.kind(), LSyntaxKind::MODEL | LSyntaxKind::ENDPOINT))
            .map(|n| (n.kind(), n.text_range()))
            .collect();
        let green_node = root.green();

        debug!("Generating symbol table");
        let symbols = Self::generate_symbol_table(parsed, &mut errors, diagnostic_ctx.clone());
        debug!("Symbol table generated with {} entries", symbols.len());

        Self::check_type_aliases(&root, &symbols, &mut errors, diagnostic_ctx.clone());
        Self::check_type_alias_cycles(&root, &symbols, &mut errors, diagnostic_ctx.clone());

        // TODO: Parallelize
        targets.iter().for_each(|&(kind, range)| {
            let local_root: LNode = rowan::SyntaxNode::new_root(green_node.clone().into());
            let element = local_root.covering_element(range);
            let node = element.into_node().expect("expected node at range");
            match kind {
                LSyntaxKind::MODEL => {
                    // TODO: Remove clone for symbols
                    Self::check_model(node, &symbols, None, &mut errors, diagnostic_ctx.clone());
                }
                LSyntaxKind::ENDPOINT => {
                    Self::check_endpoint(node, &symbols, None, &mut errors, diagnostic_ctx.clone());
                }
                _ => {}
            }
        });

        if !errors.is_empty() { Err(errors) } else { Ok(AnalyzedProgram { ast_root: root, symbols }) }
    }

    /// Like [`analyze`], but always returns an [`AnalyzedProgram`] even when there are semantic errors etc.
    pub fn analyze_lenient(&self, parsed: &ParsedProgram, source_code_metadata: &SourceCodeMetadata) -> AnalyzedProgram {
        let diagnostic_ctx = DiagnosticContext::new(source_code_metadata.file_name, source_code_metadata.file_contents);
        let mut errors = Vec::new();
        let root = parsed.ast_root.clone();
        Self::check_imports_are_top_level(&root, &mut errors, diagnostic_ctx.clone());
        let symbols = Self::generate_symbol_table(parsed, &mut errors, diagnostic_ctx);
        let diag = DiagnosticContext::new(source_code_metadata.file_name, source_code_metadata.file_contents);
        Self::check_type_aliases(&root, &symbols, &mut errors, diag.clone());
        Self::check_type_alias_cycles(&root, &symbols, &mut errors, diag);
        AnalyzedProgram { ast_root: root, symbols }
    }

    fn check_imports_are_top_level(root: &LNode, errors: &mut Vec<SemanticAnalyzerError>, diag: DiagnosticContext) {
        let mut seen_non_import_declaration = false;

        for child in root.children() {
            match child.kind() {
                LSyntaxKind::IMPORT_STMT => {
                    if seen_non_import_declaration {
                        let report = diag.error_with_help(
                            child.text_range(),
                            "Import statements must appear at the top of the file",
                            "Move this import above all type, model, endpoint, and enum declarations.",
                        );
                        errors.push(SemanticAnalyzerError::ImportNotAtTop(report));
                    }
                }
                LSyntaxKind::MODEL | LSyntaxKind::ENDPOINT | LSyntaxKind::ENUM | LSyntaxKind::TYPE_ALIAS => {
                    seen_non_import_declaration = true;
                }
                _ => {}
            }
        }
    }

    fn check_type_aliases(root: &LNode, symbols: &SymTable<LNode>, errors: &mut Vec<SemanticAnalyzerError>, diag: DiagnosticContext) {
        let aliases = Self::collect_type_aliases(root, symbols);
        for (_, type_alias, scope) in aliases {
            if let Some(type_node) = type_alias.type_node() {
                Self::check_type(type_node, symbols, scope, errors, diag.clone());
            }
        }
    }

    fn check_type_alias_cycles(root: &LNode, symbols: &SymTable<LNode>, errors: &mut Vec<SemanticAnalyzerError>, diag: DiagnosticContext) {
        let aliases = Self::collect_type_aliases(root, symbols);
        if aliases.is_empty() {
            return;
        }

        let mut deps: HashMap<String, Vec<String>> = HashMap::new();
        let mut alias_ranges: HashMap<String, rowan::TextRange> = HashMap::new();
        let mut alias_order: Vec<String> = Vec::new();

        for (name, alias, scope) in aliases {
            alias_order.push(name.clone());
            if let Some(token) = alias.ident_token() {
                alias_ranges.insert(name.clone(), token.text_range());
            }

            let mut refs = Vec::new();
            if let Some(type_node) = alias.type_node() {
                Self::collect_type_refs(type_node, &mut refs);
            }

            let mut local_deps = Vec::new();
            for ref_name in refs {
                if ref_name.contains('.') {
                    continue;
                }
                if let Some(sym_id) = symbols.resolve_id(scope, &ref_name)
                    && let Some(sym) = symbols.get(sym_id)
                    && sym.data.kind() == LSyntaxKind::TYPE_ALIAS
                    && !local_deps.contains(&sym.name)
                {
                    local_deps.push(sym.name.clone());
                }
            }

            local_deps.sort();
            deps.insert(name, local_deps);
        }

        let mut state: HashMap<String, u8> = HashMap::new();
        for name in &alias_order {
            state.insert(name.clone(), 0);
        }

        let mut stack = Vec::new();
        for name in &alias_order {
            if state.get(name).copied().unwrap_or(0) != 0 {
                continue;
            }
            if let Some(cycle) = Self::detect_alias_cycle(name, &deps, &mut state, &mut stack) {
                let cycle_text = cycle.iter().map(|entry| entry.rsplit("::").next().unwrap_or(entry)).collect::<Vec<_>>().join(" -> ");
                let cycle_start = cycle.first().cloned().unwrap_or_else(|| name.clone());
                let span = alias_ranges.get(&cycle_start).copied().unwrap_or_else(|| root.text_range());
                let report = diag.error_with_help(
                    span,
                    &format!("Circular type alias detected: {}", cycle_text),
                    "Break the cycle by changing at least one alias to point to a non-alias type.",
                );
                errors.push(SemanticAnalyzerError::CircularTypeAlias(report));
                return;
            }
        }
    }

    fn collect_type_aliases(root: &LNode, symbols: &SymTable<LNode>) -> Vec<(String, TypeAlias, Option<SymId>)> {
        let mut aliases = Vec::new();
        for child in root.children() {
            Self::collect_type_aliases_walk(child, symbols, None, &mut aliases);
        }
        aliases
    }

    fn collect_type_aliases_walk(node: LNode, symbols: &SymTable<LNode>, scope: Option<SymId>, out: &mut Vec<(String, TypeAlias, Option<SymId>)>) {
        match node.kind() {
            LSyntaxKind::TYPE_ALIAS => {
                let Some(type_alias) = TypeAlias::cast(node) else {
                    return;
                };
                let Some(alias_name) = type_alias.ident() else {
                    return;
                };
                let Some(alias_id) = symbols.resolve_id(scope, &alias_name) else {
                    return;
                };
                let Some(alias_entry) = symbols.get(alias_id) else {
                    return;
                };
                out.push((alias_entry.name.clone(), type_alias, scope));
            }
            LSyntaxKind::MODEL => {
                let Some(model) = Model::cast(node) else {
                    return;
                };
                let Some(model_name) = model.ident() else {
                    return;
                };
                let Some(model_scope) = symbols.resolve_id(scope, &model_name) else {
                    return;
                };

                for type_alias_node in model.nested_type_alias_nodes() {
                    Self::collect_type_aliases_walk(type_alias_node, symbols, Some(model_scope), out);
                }
                for nested_model_node in model.nested_model_nodes() {
                    Self::collect_type_aliases_walk(nested_model_node, symbols, Some(model_scope), out);
                }
            }
            LSyntaxKind::ENDPOINT => {
                let Some(endpoint) = Endpoint::cast(node) else {
                    return;
                };
                let Some(endpoint_name) = endpoint.path_string_literal_node().and_then(|s| s.value()) else {
                    return;
                };
                let Some(endpoint_scope) = symbols.resolve_id(scope, &endpoint_name) else {
                    return;
                };

                for nested_model_node in endpoint.nested_model_nodes() {
                    Self::collect_type_aliases_walk(nested_model_node, symbols, Some(endpoint_scope), out);
                }
            }
            _ => {}
        }
    }

    fn detect_alias_cycle(current: &str, deps: &HashMap<String, Vec<String>>, state: &mut HashMap<String, u8>, stack: &mut Vec<String>) -> Option<Vec<String>> {
        match state.get(current).copied().unwrap_or(0) {
            1 => {
                let pos = stack.iter().position(|name| name == current).unwrap_or(0);
                let mut cycle = stack[pos..].to_vec();
                cycle.push(current.to_string());
                return Some(cycle);
            }
            2 => return None,
            _ => {}
        }

        state.insert(current.to_string(), 1);
        stack.push(current.to_string());

        if let Some(next) = deps.get(current) {
            for dep in next {
                if let Some(cycle) = Self::detect_alias_cycle(dep, deps, state, stack) {
                    return Some(cycle);
                }
            }
        }

        stack.pop();
        state.insert(current.to_string(), 2);
        None
    }

    fn collect_type_refs(type_node: LNode, out: &mut Vec<String>) {
        let Some(type_expr) = Type::cast(type_node) else {
            return;
        };

        for atom in type_expr.type_atoms() {
            if let Some(ref_name) = atom.as_ref_name() {
                out.push(ref_name);
            }

            if let Some(record) = atom.as_record_type() {
                if let Some(src) = record.src_type_node() {
                    Self::collect_type_refs(src, out);
                }
                if let Some(dest) = record.dest_type_node() {
                    Self::collect_type_refs(dest, out);
                }
            }

            if let Some(anon_model_node) = atom.as_anon_model()
                && let Some(anon_model) = AnonModel::cast(anon_model_node)
            {
                for field in anon_model.field_nodes().into_iter().filter_map(Field::cast) {
                    if let Some(field_type) = field.type_node() {
                        Self::collect_type_refs(field_type, out);
                    }
                }
            }
        }
    }

    fn check_model(node: LNode, symbols: &SymTable<LNode>, scope: Option<SymId>, errors: &mut Vec<SemanticAnalyzerError>, diag: DiagnosticContext) {
        let model = Model::cast(node.clone()).unwrap();
        let model_fields = model.field_nodes();
        let model_ident_token = model.ident_token().unwrap();
        let model_name = model_ident_token.text().to_string();
        let model_scope = symbols.resolve_id(scope, &model_name);

        for field_node in model_fields {
            Self::check_field(field_node, symbols, model_scope, errors, diag.clone());
        }

        for type_alias_node in model.nested_type_alias_nodes() {
            if let Some(type_alias) = TypeAlias::cast(type_alias_node)
                && let Some(type_node) = type_alias.type_node()
            {
                Self::check_type(type_node, symbols, model_scope, errors, diag.clone());
            }
        }

        for nested_model_node in model.nested_model_nodes() {
            Self::check_model(nested_model_node, symbols, model_scope, errors, diag.clone());
        }
    }

    fn check_endpoint(node: LNode, symbols: &SymTable<LNode>, scope: Option<SymId>, errors: &mut Vec<SemanticAnalyzerError>, diag: DiagnosticContext) {
        let endpoint = Endpoint::cast(node).unwrap();
        let endpoint_name = endpoint.path_string_literal_node().unwrap().value().unwrap();
        let endpoint_scope = symbols.resolve_id(scope, &endpoint_name);

        // Check regular (non-responses) fields
        for field_node in endpoint.field_nodes() {
            let field = Field::cast(field_node.clone()).unwrap();
            if field.ident().as_deref() == Some("responses") {
                // Walk the anon model inside `responses` (e.g., `{ 2XX: Foo, 4XX: Bar }` and check each response type reference.
                if let Some(type_node) = field.type_node() {
                    let type_expr = Type::cast(type_node).unwrap();
                    for atom_node in type_expr.type_atom_nodes() {
                        let atom = TypeAtom::cast(atom_node).unwrap();
                        if let Some(anon_model_node) = atom.as_anon_model() {
                            let anon_model = AnonModel::cast(anon_model_node).unwrap();
                            for response_field_node in anon_model.field_nodes() {
                                let response_field = Field::cast(response_field_node).unwrap();
                                if let Some(response_type_node) = response_field.type_node() {
                                    Self::check_type(response_type_node, symbols, endpoint_scope, errors, diag.clone());
                                }
                            }
                        }
                    }
                }
            } else {
                Self::check_field(field_node, symbols, endpoint_scope, errors, diag.clone());
            }
        }

        // Check nested models declared inside the endpoint
        for nested_model_node in endpoint.nested_model_nodes() {
            Self::check_model(nested_model_node, symbols, endpoint_scope, errors, diag.clone());
        }
    }

    fn check_field(node: LNode, symbols: &SymTable<LNode>, scope: Option<SymId>, errors: &mut Vec<SemanticAnalyzerError>, diag: DiagnosticContext) {
        let field = Field::cast(node.clone()).unwrap();

        let field_type = field.type_node().unwrap();
        Self::check_type(field_type, symbols, scope, errors, diag.clone());

        let decorators = field.decorator_nodes();
        for decorator_node in decorators {
            Self::check_decorator(decorator_node, errors, diag.clone());
        }

        // Check that the default matches the type
        if let Some(field_default_value_node) = field.default_literal_expr_node() {
            let literal_expr = LiteralExpr::cast(field_default_value_node.clone()).expect("Expected ConstExpr node");
            let default_value = literal_expr.value().expect("Expected value in LiteralExpr");
            let type_expr = Type::cast(field.type_node().unwrap().clone()).unwrap();
            let type_atom_nodes: Vec<_> = type_expr.type_atom_nodes();

            // TODO: Support unions
            if type_atom_nodes.len() == 1 {
                let type_atom = TypeAtom::cast(type_atom_nodes[0].clone()).unwrap();

                if let Some(primitive_type) = type_atom.as_primitive_type() {
                    let value_matches_primitive = |literal: &Literal| {
                        matches!(
                            (primitive_type, literal),
                            (PrimitiveType::Bool, Literal::BoolLiteral { .. })
                                | (PrimitiveType::Int, Literal::IntLiteral { .. })
                                | (PrimitiveType::Float, Literal::FloatLiteral { .. })
                                | (PrimitiveType::String, Literal::StringLiteral { .. })
                        )
                    };

                    if type_atom.is_array() {
                        let is_valid_array_default = match &default_value {
                            Literal::ListLiteral(list_literal) => list_literal.values().iter().all(value_matches_primitive),
                            _ => false,
                        };

                        if !is_valid_array_default {
                            let report = diag.error(field_default_value_node.text_range(), "Type of default value does not match field type");
                            errors.push(SemanticAnalyzerError::DuplicateField(report));
                        }
                    } else if !value_matches_primitive(&default_value) {
                        let report = diag.error(field_default_value_node.text_range(), "Type of default value does not match field type");
                        errors.push(SemanticAnalyzerError::DuplicateField(report));
                    }
                } else {
                    // Not a literal type - must be a ref
                    if let Some(ref_name) = type_atom.as_ref_name()
                        && !ref_name.contains('.')
                    {
                        let ref_sym = symbols.resolve_id(scope, &ref_name).expect("Expected referenced symbol to exist");
                        let ref_entry = symbols.get(ref_sym).expect("Expected symbol entry to exist");

                        match (&ref_entry.data.kind(), &default_value) {
                            (LSyntaxKind::ENUM, Literal::StringLiteral(string_lit_node)) => {
                                let enum_node = ref_entry.data.clone();
                                let enum_model = Enum::cast(enum_node.clone()).unwrap();
                                let enum_ident_token = enum_model.ident_token().unwrap();
                                let enum_name_str = enum_ident_token.text().to_string();
                                let variant_literal = string_lit_node.value().unwrap();
                                let variant_exists = enum_model.variant_nodes().iter().any(|curr_variant_node| {
                                    let curr_variant = EnumVariant::cast(curr_variant_node.clone()).unwrap();
                                    let curr_variant_name = curr_variant.value().unwrap();
                                    *variant_literal == curr_variant_name
                                });
                                if !variant_exists {
                                    let report_label = diag.labeled_span(enum_node.text_range(), &format!("Enum '{}' defined here", enum_name_str));
                                    let report = diag.error_with_labels(
                                        string_lit_node.syntax().text_range(),
                                        &format!("Enum variant '{}' does not exist in enum '{}'", variant_literal, enum_name_str),
                                        None,
                                        None,
                                        vec![report_label],
                                    );
                                    errors.push(SemanticAnalyzerError::DuplicateField(report));
                                }
                            }
                            _ => {
                                let report = diag.error(field_default_value_node.text_range(), "Type of default value does not match field type");
                                errors.push(SemanticAnalyzerError::DuplicateField(report));
                            }
                        }
                    }
                }
            }
        }
    }

    fn check_type(node: LNode, symbols: &SymTable<LNode>, scope: Option<SymId>, errors: &mut Vec<SemanticAnalyzerError>, diag: DiagnosticContext) {
        let type_expr = Type::cast(node.clone()).unwrap();
        let type_atom_nodes = type_expr.type_atom_nodes();
        for type_atom_node in type_atom_nodes {
            let type_atom = TypeAtom::cast(type_atom_node.clone()).unwrap();
            if let Some(ref_name) = type_atom.as_ref_name() {
                // Ident - could be a non-existent primitive type, or a ref
                if ref_name.contains('.') {
                    continue;
                }
                let ident_token = type_atom.as_ref_token();
                let type_name = ref_name;
                if let Some(scope) = scope
                    && symbols.resolve_id(Some(scope), &type_name).is_none()
                {
                    let mut candidates = vec!["string", "int", "float", "bool", "any"];
                    let symbol_entries = symbols.entries(Some(scope));
                    candidates.extend(symbol_entries.iter().map(|entry| entry.name.rsplit("::").nth(0).unwrap()));
                    let suggested_names = fuzzy_match(&type_name, &candidates, 1);
                    if let Some(suggested_name) = suggested_names.first()
                        && suggested_name.1 >= 50
                    {
                        let span = ident_token.as_ref().map(|t| t.text_range()).unwrap_or(type_atom.syntax().text_range());
                        let report = diag.error_with_help(span, &format!("Undefined type reference '{}'", type_name), &format!("Did you mean '{}'?", suggested_name.0));
                        errors.push(SemanticAnalyzerError::UndefinedTypeReference(report));
                        continue;
                    }
                    let span = ident_token.as_ref().map(|t| t.text_range()).unwrap_or(type_atom.syntax().text_range());
                    let report = diag.error(span, &format!("Undefined type reference '{}'", type_name));
                    errors.push(SemanticAnalyzerError::UndefinedTypeReference(report));
                }
            }

            if let Some(anon_model_node) = type_atom.as_anon_model()
                && let Some(anon_model) = AnonModel::cast(anon_model_node)
            {
                for field_node in anon_model.field_nodes() {
                    if let Some(field) = Field::cast(field_node)
                        && let Some(field_type_node) = field.type_node()
                    {
                        Self::check_type(field_type_node, symbols, scope, errors, diag.clone());
                    }
                }
            }

            if let Some(record) = type_atom.as_record_type() {
                if let Some(src) = record.src_type_node() {
                    Self::check_type(src, symbols, scope, errors, diag.clone());
                }
                if let Some(dest) = record.dest_type_node() {
                    Self::check_type(dest, symbols, scope, errors, diag.clone());
                }
            }
        }
    }

    // TODO: Check decorator contextually (e.g., only allow certain decorators on fields, models, etc.)
    fn check_decorator(node: LNode, errors: &mut Vec<SemanticAnalyzerError>, diag: DiagnosticContext) {
        let decorator = Decorator::cast(node.clone()).unwrap();

        let decorator_name = decorator.ident().unwrap();

        let Some(builtin_decorator) = BUILTIN_DECORATORS.iter().find(|d| d.id == decorator_name) else {
            let builtin_decorator_names: Vec<_> = BUILTIN_DECORATORS.iter().map(|d| d.id).collect();
            let report = diag.error(
                decorator.ident_token().unwrap().text_range(),
                &format!("Decorator '@{}' is not recognized (did you mean one of these? {:?})", decorator_name, builtin_decorator_names),
            );
            errors.push(SemanticAnalyzerError::DuplicateField(report));
            return;
        };

        // Check the fields of the decorator against the built-in decorator definition.
        let effective_arg_nodes = decorator.arg_nodes();
        if effective_arg_nodes.is_empty() {
            // Check if the built-in decorator has any expected arguments.
            if !builtin_decorator.positional_args.is_empty() || !builtin_decorator.named_args.is_empty() {
                let report = diag.error(
                    decorator.ident_token().unwrap().text_range(),
                    &format!("Decorator '@{}' has no arguments, but some are required", decorator_name),
                );
                errors.push(SemanticAnalyzerError::DuplicateField(report));
                return;
            };
            return;
        };

        // Check that all required arguments are present.
        for required_arg_def in builtin_decorator.args().iter().filter(|arg| arg.required) {
            let expected_pos = builtin_decorator.positional_args.iter().position(|arg| arg.id == required_arg_def.id);
            let expected_name = builtin_decorator.named_args.iter().position(|arg| arg.id == required_arg_def.id);
            if let Some(expected_pos) = expected_pos
                && expected_name.is_none()
            {
                // Just positional - check that it's in the right index and has the expected type.
                let effective_arg_at_pos = effective_arg_nodes.get(expected_pos);
                if effective_arg_at_pos.is_none() {
                    let report = diag.error(
                        decorator.ident_token().unwrap().text_range(),
                        &format!("Decorator '@{}' is missing required positional argument '{}'", decorator_name, required_arg_def.id),
                    );
                    errors.push(SemanticAnalyzerError::DuplicateField(report));
                    return;
                }
                // Check the type of the effective argument.
                let effective_arg = DecoratorArg::cast(effective_arg_at_pos.unwrap().clone()).unwrap();
                let literal_expr = effective_arg.literal_expr().unwrap();
                let literal_value = literal_expr.value().unwrap();
                if required_arg_def.ty != literal_value.ty() {
                    let report = diag.error(
                        effective_arg_at_pos.unwrap().text_range(),
                        &format!("Argument '{}' to decorator '@{}' has incorrect type", required_arg_def.id, decorator_name),
                    );
                    errors.push(SemanticAnalyzerError::DuplicateField(report));
                    return;
                }
            }
            // Just named
            else if expected_pos.is_none() {
                let effective_arg_with_name = effective_arg_nodes.iter().find(|arg_node| {
                    let arg = DecoratorArg::cast((*arg_node).clone()).unwrap();
                    let arg_ident = arg.ident().unwrap();
                    arg_ident == required_arg_def.id
                });
                if effective_arg_with_name.is_none() {
                    let report = diag.error(
                        decorator.ident_token().unwrap().text_range(),
                        &format!("Decorator '@{}' is missing required named argument '{}'", decorator_name, required_arg_def.id),
                    );
                    errors.push(SemanticAnalyzerError::DuplicateField(report));
                    return;
                }
                // Check the type of the effective argument.
                let effective_arg = DecoratorArg::cast(effective_arg_with_name.unwrap().clone()).unwrap();
                let literal_expr = effective_arg.literal_expr().unwrap();
                let literal_value = literal_expr.value().unwrap();
                if required_arg_def.ty != literal_value.ty() {
                    let report = diag.error(
                        effective_arg_with_name.unwrap().text_range(),
                        &format!("Argument '{}' to decorator '@{}' has incorrect type", required_arg_def.id, decorator_name),
                    );
                    errors.push(SemanticAnalyzerError::DuplicateField(report));
                    return;
                }
            }
            // TODO: Both positional and named
        }

        // If there are positional args, check that they are in the correct order and have the correct types.
        let positional_args = decorator.positional_args();
        for (idx, effective_arg) in positional_args.iter().enumerate() {
            let expected_arg_def = builtin_decorator.positional_args.get(idx);
            if let Some(expected_arg_def) = expected_arg_def {
                let literal_expr = effective_arg.literal_expr().unwrap();
                let literal_value = literal_expr.value().unwrap();
                if expected_arg_def.ty != literal_value.ty() {
                    let report = diag.error_with_help(
                        effective_arg.syntax().text_range(),
                        &format!(
                            "Argument to decorator `@{}` has incorrect type (expected `{}` of type `{}` , received `{}`)",
                            decorator_name, expected_arg_def.id, expected_arg_def.ty, literal_value
                        ),
                        &builtin_decorator.doc(),
                    );
                    errors.push(SemanticAnalyzerError::DuplicateField(report));
                    return;
                }
            }
        }
    }

    fn generate_symbol_table(parsed: &ParsedProgram, errors: &mut Vec<SemanticAnalyzerError>, diag: DiagnosticContext) -> SymTable<LNode> {
        let root = &parsed.ast_root;

        let mut syms = SymTable::new();
        let top_level_nodes = root.children();
        for child in top_level_nodes {
            if Self::generate_symbol_table_walk(child, &mut syms, None, errors, &diag).is_err() {
                continue;
            }
        }

        syms
    }

    fn generate_symbol_table_walk(
        node: LNode,
        syms: &mut SymTable<LNode>,
        parent_scope: Option<SymId>,
        errors: &mut Vec<SemanticAnalyzerError>,
        diag: &DiagnosticContext,
    ) -> Result<Option<SymId>, ()> {
        match node.kind() {
            LSyntaxKind::MODEL => {
                let model = Model::cast(node.clone()).unwrap();
                let ident_token = model.ident_token().unwrap();
                let model_name = ident_token.text().to_string();
                if syms.resolve(parent_scope, &model_name).is_some() {
                    let report = diag.error(ident_token.text_range(), &format!("Duplicate model name '{}'", model_name));
                    errors.push(SemanticAnalyzerError::DuplicateField(report));
                    return Err(());
                }
                let model_scope_id = syms.add_to_scope(parent_scope, &model_name, node);
                for field_node in model.field_nodes() {
                    let _ = Self::generate_symbol_table_walk(field_node, syms, Some(model_scope_id), errors, diag);
                }
                for nested_model_node in model.nested_model_nodes() {
                    let _ = Self::generate_symbol_table_walk(nested_model_node, syms, Some(model_scope_id), errors, diag);
                }
                for nested_enum_node in model.nested_enum_nodes() {
                    let _ = Self::generate_symbol_table_walk(nested_enum_node, syms, Some(model_scope_id), errors, diag);
                }
                for nested_type_alias_node in model.nested_type_alias_nodes() {
                    let _ = Self::generate_symbol_table_walk(nested_type_alias_node, syms, Some(model_scope_id), errors, diag);
                }
            }
            LSyntaxKind::ENDPOINT => {
                let endpoint = Endpoint::cast(node.clone()).unwrap();
                // We use the endpoint's string literal (the path e.g., "GET /users") as its name, since the friendly name is optional
                let endpoint_name = endpoint.path_string_literal_node().unwrap().value().expect("Expected endpoint string literal");
                if syms.resolve(parent_scope, &endpoint_name).is_some() {
                    let report = diag.error(endpoint.syntax().text_range(), &format!("Duplicate endpoint name '{}'", endpoint_name));
                    errors.push(SemanticAnalyzerError::DuplicateField(report));
                    return Err(());
                }
                let endpoint_scope_id = syms.add_to_scope(parent_scope, &endpoint_name, node);
                for field_node in endpoint.field_nodes() {
                    let _ = Self::generate_symbol_table_walk(field_node, syms, Some(endpoint_scope_id), errors, diag);
                }
                for nested_model_node in endpoint.nested_model_nodes() {
                    let _ = Self::generate_symbol_table_walk(nested_model_node, syms, Some(endpoint_scope_id), errors, diag);
                }
                for nested_enum_node in endpoint.nested_enum_nodes() {
                    let _ = Self::generate_symbol_table_walk(nested_enum_node, syms, Some(endpoint_scope_id), errors, diag);
                }
            }
            LSyntaxKind::ENUM => {
                let enum_model = Enum::cast(node.clone()).unwrap();
                let ident_token = enum_model.ident_token().unwrap();
                let enum_name = ident_token.text().to_string();
                if syms.resolve(parent_scope, &enum_name).is_some() {
                    let report = diag.error(ident_token.text_range(), &format!("Duplicate enum name '{}'", enum_name));
                    errors.push(SemanticAnalyzerError::DuplicateField(report));
                    return Err(());
                }
                syms.add_to_scope(parent_scope, &enum_name, node);
            }
            LSyntaxKind::TYPE_ALIAS => {
                let type_alias = TypeAlias::cast(node.clone()).unwrap();
                let ident_token = type_alias.ident_token().unwrap();
                let alias_name = ident_token.text().to_string();
                if syms.resolve(parent_scope, &alias_name).is_some() {
                    let report = diag.error(ident_token.text_range(), &format!("Duplicate type alias name '{}'", alias_name));
                    errors.push(SemanticAnalyzerError::DuplicateField(report));
                    return Err(());
                }
                syms.add_to_scope(parent_scope, &alias_name, node);
            }
            LSyntaxKind::FIELD => {
                let field = Field::cast(node.clone()).expect("Expected Field node");
                let ident_token = field.ident_node().expect("Expected field ident token");
                let field_name = match ident_token.clone() {
                    LNodeOrToken::Node(n) => n.text().to_string(),
                    LNodeOrToken::Token(tok) => tok.text().to_string(),
                };

                // Build the fully qualified name for this field in the current scope
                let mut fully_qualified_name = field_name.clone();
                if let Some(scope) = parent_scope
                    && let Some(scope_entry) = syms.get(scope)
                {
                    fully_qualified_name = format!("{}::{}", scope_entry.name, field_name);
                }

                // Check if this exact field already exists in the current scope
                let already_exists = syms.entries(parent_scope).iter().any(|entry| entry.name == fully_qualified_name);
                if already_exists {
                    let report = diag.error(ident_token.text_range(), &format!("Duplicate field name '{}'", field_name));
                    errors.push(SemanticAnalyzerError::DuplicateField(report));
                    return Err(());
                }
                syms.add_to_scope(parent_scope, &field_name, node);
            }
            _ => {}
        }
        Ok(None)
    }
}

#[cfg(test)]
mod tests {
    use indoc::indoc;

    use crate::{metadata::SourceCodeMetadata, semantic_analyzer::SemanticAnalyzer, syntax::Parser};

    #[test]
    fn test_record_unknown_type_fails() {
        let src = indoc! { r#"
            model User {
                metadata: Record<string, Foo>
            }
        "# };

        let source = SourceCodeMetadata {
            file_name: "test.glue",
            file_contents: src,
        };
        let parsed = Parser::new().parse(&source).unwrap();
        let result = SemanticAnalyzer::new().analyze(&parsed, &source);
        assert!(result.is_err(), "Expected semantic analysis to fail for unknown record type");
    }

    #[test]
    fn test_valid_model_basic() {
        let src = indoc! { r#"
        model Graph {
            nodes: Record<string, Node>
            edges: Record<string, string>[]

            model Node {
                id: string
                label: string
            }
        }
        "# };
        let metadata = SourceCodeMetadata {
            file_name: "test.glue",
            file_contents: src,
        };
        let parsed = Parser::new().parse(&metadata).unwrap();
        let analyzed = SemanticAnalyzer::new().analyze(&parsed, &metadata);
        assert!(analyzed.is_ok());
    }

    #[test]
    fn test_invalid_model_basic() {
        let src = indoc! { r#"
		// This is a great model
        model Foo {
            name: string
            id: string
            blah: BarEnu

            enum BarEnum: "A" | "B" | "C"
        }

        model Bar {
            id: string
        }
		"# };

        let metadata = SourceCodeMetadata {
            file_name: "test.glue",
            file_contents: src,
        };
        let parsed = Parser::new().parse(&metadata).unwrap();
        let analyzed = SemanticAnalyzer::new().analyze(&parsed, &metadata);
        assert!(analyzed.is_err());
        let errors = analyzed.err().unwrap();
        assert_eq!(errors.len(), 1);
        let error = errors[0].report();
        assert_eq!(error.help().unwrap().to_string(), "Did you mean 'BarEnum'?");
    }

    #[test]
    fn test_endpoint_response_undefined_type_fails() {
        let src = indoc! { r#"
            endpoint "POST /listings" {
                responses: {
                    2XX: Aartment[]
                    4XX: ErrorResponse
                }
            }

            model Apartment {
                id: int
            }

            model ErrorResponse {
                code: int
                message: string
            }
        "# };

        let metadata = SourceCodeMetadata {
            file_name: "test.glue",
            file_contents: src,
        };
        let parsed = Parser::new().parse(&metadata).unwrap();
        let result = SemanticAnalyzer::new().analyze(&parsed, &metadata);
        assert!(result.is_err(), "Expected analysis to fail for typo 'Aartment'");
        let errors = result.err().unwrap();
        assert_eq!(errors.len(), 1, "Expected exactly one error");
        assert!(errors[0].report().to_string().contains("Aartment"), "Expected error to mention 'Aartment'");
        assert!(errors[0].report().help().unwrap().to_string().contains("Apartment"), "Expected 'Did you mean Apartment?' hint");
    }

    #[test]
    fn test_endpoint_response_valid_type_passes() {
        let src = indoc! { r#"
            endpoint "GET /listings" {
                responses: {
                    2XX: Apartment[]
                    4XX: ErrorResponse
                }
            }

            model Apartment {
                id: int
            }

            model ErrorResponse {
                code: int
                message: string
            }
        "# };

        let metadata = SourceCodeMetadata {
            file_name: "test.glue",
            file_contents: src,
        };
        let parsed = Parser::new().parse(&metadata).unwrap();
        let result = SemanticAnalyzer::new().analyze(&parsed, &metadata);
        assert!(result.is_ok(), "Expected analysis to pass for valid endpoint response types");
    }

    #[test]
    fn test_import_must_be_at_top() {
        let src = indoc! { r#"
            model Root {
                id: string
            }

            import * from "./models.glue"
        "# };

        let metadata = SourceCodeMetadata {
            file_name: "test.glue",
            file_contents: src,
        };
        let parsed = Parser::new().parse(&metadata).unwrap();
        let result = SemanticAnalyzer::new().analyze(&parsed, &metadata);

        assert!(result.is_err(), "Expected analysis to fail when import is not at top");
        let errors = result.err().unwrap();
        assert!(
            errors.iter().any(|e| e.report().to_string().contains("Import statements must appear at the top of the file")),
            "Expected informative import-order error"
        );
    }

    #[test]
    fn test_array_default_with_matching_primitive_type_passes() {
        let src = indoc! { r#"
            model Perf {
                tags: string[] = ["perf", "benchmark", "static"]
            }
        "# };

        let metadata = SourceCodeMetadata {
            file_name: "test.glue",
            file_contents: src,
        };
        let parsed = Parser::new().parse(&metadata).unwrap();
        let result = SemanticAnalyzer::new().analyze(&parsed, &metadata);
        assert!(result.is_ok(), "Expected analysis to pass for matching string[] default literal");
    }

    #[test]
    fn test_array_default_with_mismatched_primitive_type_fails() {
        let src = indoc! { r#"
            model Perf {
                tags: string[] = [1, 2, 3]
            }
        "# };

        let metadata = SourceCodeMetadata {
            file_name: "test.glue",
            file_contents: src,
        };
        let parsed = Parser::new().parse(&metadata).unwrap();
        let result = SemanticAnalyzer::new().analyze(&parsed, &metadata);
        assert!(result.is_err(), "Expected analysis to fail for mismatched string[] default literal");
        let errors = result.err().unwrap();
        assert!(
            errors.iter().any(|e| e.report().to_string().contains("Type of default value does not match field type")),
            "Expected informative default-type mismatch error"
        );
    }

    #[test]
    fn test_import_statement_at_top_parses_and_analyzes() {
        let src = indoc! { r#"
            import * as Alpha from "./perf_dep_alpha.glue"

            model Root {
                id: string
            }
        "# };

        let metadata = SourceCodeMetadata {
            file_name: "test.glue",
            file_contents: src,
        };
        let parsed = Parser::new().parse(&metadata).unwrap();
        let result = SemanticAnalyzer::new().analyze(&parsed, &metadata);
        assert!(result.is_ok(), "Expected semantic analyzer to allow top-level import statements");
    }

    #[test]
    fn test_type_alias_to_primitive_analyzes() {
        let src = indoc! { r#"
            type UserId = string

            model User {
                id: UserId
            }
        "# };

        let metadata = SourceCodeMetadata {
            file_name: "test.glue",
            file_contents: src,
        };
        let parsed = Parser::new().parse(&metadata).unwrap();
        let result = SemanticAnalyzer::new().analyze(&parsed, &metadata);
        assert!(result.is_ok(), "Expected semantic analyzer to resolve type aliases");
    }

    #[test]
    fn test_type_alias_cycle_fails_with_informative_error() {
        let src = indoc! { r#"
            type A = B
            type B = A

            model Root {
                value: A
            }
        "# };

        let metadata = SourceCodeMetadata {
            file_name: "test.glue",
            file_contents: src,
        };
        let parsed = Parser::new().parse(&metadata).unwrap();
        let result = SemanticAnalyzer::new().analyze(&parsed, &metadata);
        assert!(result.is_err(), "Expected analysis to fail for circular type aliases");
        let errors = result.err().unwrap();
        assert!(
            errors.iter().any(|e| e.report().to_string().contains("Circular type alias detected: A -> B -> A")),
            "Expected informative circular alias error"
        );
    }

    #[test]
    fn test_nested_type_alias_in_model_scope_analyzes() {
        let src = indoc! { r#"
            model Root {
                type UserId = string
                id: UserId
            }
        "# };

        let metadata = SourceCodeMetadata {
            file_name: "test.glue",
            file_contents: src,
        };
        let parsed = Parser::new().parse(&metadata).unwrap();
        let result = SemanticAnalyzer::new().analyze(&parsed, &metadata);
        assert!(result.is_ok(), "Expected analysis to pass for nested type alias in model scope");
    }

    #[test]
    fn test_nested_type_alias_cycle_fails_with_informative_error() {
        let src = indoc! { r#"
            model Root {
                type A = B
                type B = A
                value: A
            }
        "# };

        let metadata = SourceCodeMetadata {
            file_name: "test.glue",
            file_contents: src,
        };
        let parsed = Parser::new().parse(&metadata).unwrap();
        let result = SemanticAnalyzer::new().analyze(&parsed, &metadata);
        assert!(result.is_err(), "Expected analysis to fail for nested circular type aliases");
        let errors = result.err().unwrap();
        assert!(
            errors.iter().any(|e| e.report().to_string().contains("Circular type alias detected: A -> B -> A")),
            "Expected informative circular alias error"
        );
    }
}
