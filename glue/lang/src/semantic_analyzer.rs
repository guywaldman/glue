use std::{
    any::Any,
    collections::{HashMap, HashSet},
    sync::{Arc, Mutex},
};

use log::debug;
use miette::Report;

use crate::{
    ConstDef, ConstEvaluator, ConstExprType, ConstValue, Decorator, DecoratorArg, Endpoint, EnumVariant, Literal, LiteralExpr, PrimitiveType, Rpc, Service, SourceCodeMetadata,
    builtin_decorators::{BUILTIN_DECORATORS, DecoratorDef, MODEL_FIELD_DECORATOR, MODEL_FIELD_DECORATOR_EXAMPLE_ARG},
    diagnostics::DiagnosticContext,
    is_constant_case,
    symbols::{SymId, SymTable},
    syntax::{AnonModel, AstNode, Enum, Field, LNode, LNodeOrToken, LSyntaxKind, Model, ParsedProgram, Type, TypeAlias, TypeAtom},
    to_constant_case,
    utils::fuzzy_match,
};

#[derive(Debug)]
pub enum SemanticAnalyzerError {
    DuplicateField(Report),
    MissingRequiredField(Report),
    UndefinedTypeReference(Report),
    ImportNotAtTop(Report),
    CircularTypeAlias(Report),
    ConstantError(Report),
}

impl SemanticAnalyzerError {
    pub fn report(&self) -> &miette::Report {
        match self {
            SemanticAnalyzerError::DuplicateField(report)
            | SemanticAnalyzerError::MissingRequiredField(report)
            | SemanticAnalyzerError::UndefinedTypeReference(report)
            | SemanticAnalyzerError::ImportNotAtTop(report)
            | SemanticAnalyzerError::CircularTypeAlias(report)
            | SemanticAnalyzerError::ConstantError(report) => report,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum SemanticWarningCode {
    ConstantCase,
}

impl SemanticWarningCode {
    pub fn as_str(&self) -> &'static str {
        match self {
            Self::ConstantCase => "constant_case",
        }
    }
}

#[derive(Debug)]
pub struct SemanticWarning {
    pub code: SemanticWarningCode,
    pub report: Report,
}

impl SemanticWarning {
    pub fn report(&self) -> &Report {
        &self.report
    }
}

#[derive(Debug, Clone, Default)]
pub struct SemanticAnalyzerOptions {
    pub suppress_warnings: HashSet<String>,
}

impl SemanticAnalyzerOptions {
    fn suppresses(&self, code: SemanticWarningCode) -> bool {
        self.suppress_warnings.contains(code.as_str())
    }
}

#[derive(Debug)]
pub struct AnalyzedProgram {
    pub ast_root: LNode,
    pub symbols: SymTable<LNode>,
    pub warnings: Vec<SemanticWarning>,
}

pub struct SemanticAnalyzer {
    options: SemanticAnalyzerOptions,
}

impl SemanticAnalyzer {
    pub fn new() -> Self {
        Self::with_options(SemanticAnalyzerOptions::default())
    }

    pub fn with_options(options: SemanticAnalyzerOptions) -> Self {
        Self { options }
    }

    pub fn analyze(&self, parsed: &ParsedProgram, source_code_metadata: &SourceCodeMetadata) -> Result<AnalyzedProgram, Vec<SemanticAnalyzerError>> {
        let diagnostic_ctx = DiagnosticContext::new(source_code_metadata.file_name, source_code_metadata.file_contents);
        let mut errors = Vec::new();
        let mut warnings = Vec::new();

        let root = parsed.ast_root.clone();
        Self::check_imports_are_top_level(&root, &mut errors, diagnostic_ctx.clone());
        let targets: Vec<_> = root
            .children()
            .filter(|n| matches!(n.kind(), LSyntaxKind::MODEL | LSyntaxKind::ENDPOINT | LSyntaxKind::SERVICE))
            .map(|n| (n.kind(), n.text_range()))
            .collect();
        let green_node = root.green();

        debug!("Generating symbol table");
        let symbols = Self::generate_symbol_table(parsed, &mut errors, diagnostic_ctx.clone());
        debug!("Symbol table generated with {} entries", symbols.len());

        Self::check_type_aliases(&root, &symbols, &mut errors, diagnostic_ctx.clone());
        Self::check_type_alias_cycles(&root, &symbols, &mut errors, diagnostic_ctx.clone());
        self.check_consts(&root, &symbols, &mut errors, &mut warnings, diagnostic_ctx.clone());

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
                LSyntaxKind::SERVICE => {
                    Self::check_service(node, &symbols, None, &mut errors, diagnostic_ctx.clone());
                }
                _ => {}
            }
        });

        if !errors.is_empty() {
            Err(errors)
        } else {
            Ok(AnalyzedProgram { ast_root: root, symbols, warnings })
        }
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
        AnalyzedProgram {
            ast_root: root,
            symbols,
            warnings: Vec::new(),
        }
    }

    fn check_imports_are_top_level(root: &LNode, errors: &mut Vec<SemanticAnalyzerError>, diag: DiagnosticContext) {
        let mut seen_non_import_declaration = false;

        for child in root.children() {
            match child.kind() {
                LSyntaxKind::IMPORT_STMT if seen_non_import_declaration => {
                    let report = diag.error_with_help(
                        child.text_range(),
                        "Import statements must appear at the top of the file",
                        "Move this import above all const, type, model, endpoint, service, and enum declarations.",
                    );
                    errors.push(SemanticAnalyzerError::ImportNotAtTop(report));
                }
                LSyntaxKind::IMPORT_STMT => {}
                LSyntaxKind::MODEL | LSyntaxKind::ENDPOINT | LSyntaxKind::SERVICE | LSyntaxKind::ENUM | LSyntaxKind::TYPE_ALIAS | LSyntaxKind::CONST_DEF => {
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

    fn check_consts(&self, root: &LNode, symbols: &SymTable<LNode>, errors: &mut Vec<SemanticAnalyzerError>, warnings: &mut Vec<SemanticWarning>, diag: DiagnosticContext) {
        let evaluator = ConstEvaluator::new(symbols, diag.clone());
        for (const_def, scope) in Self::collect_consts(root, symbols) {
            let Some(name) = const_def.ident() else {
                continue;
            };

            if !self.options.suppresses(SemanticWarningCode::ConstantCase)
                && !is_constant_case(&name)
                && let Some(token) = const_def.ident_token()
            {
                let suggested = to_constant_case(&name);
                let report = diag.warning_with_help(
                    token.text_range(),
                    &format!("Constant '{}' should use CONSTANT_CASE", name),
                    &format!(
                        "Rename this constant to '{}' or suppress warning code 'constant_case' in global.diagnostics.suppress_warnings.",
                        suggested
                    ),
                );
                warnings.push(SemanticWarning {
                    code: SemanticWarningCode::ConstantCase,
                    report,
                });
            }

            let value = match evaluator.eval_const_def_in_scope(&const_def, scope) {
                Ok(value) => value,
                Err(report) => {
                    errors.push(SemanticAnalyzerError::ConstantError(report));
                    continue;
                }
            };

            if matches!(value, ConstValue::List(_)) {
                let span = const_def.expr_node().map(|n| n.text_range()).unwrap_or_else(|| const_def.syntax().text_range());
                let report = diag.error(span, "Constants must fold to integer, string, or bool");
                errors.push(SemanticAnalyzerError::ConstantError(report));
                continue;
            }

            if let Some(type_node) = const_def.type_node() {
                let Some(expected_ty) = Self::const_decl_type(type_node.clone(), &diag, errors) else {
                    continue;
                };
                if value.ty() != expected_ty {
                    let span = const_def.expr_node().map(|n| n.text_range()).unwrap_or_else(|| const_def.syntax().text_range());
                    let report = diag.error(span, &format!("Constant '{}' is declared as {} but its value is {}", name, expected_ty, value.ty()));
                    errors.push(SemanticAnalyzerError::ConstantError(report));
                }
            }
        }
    }

    fn collect_consts(root: &LNode, symbols: &SymTable<LNode>) -> Vec<(ConstDef, Option<SymId>)> {
        let mut consts = Vec::new();
        for child in root.children() {
            Self::collect_consts_walk(child, symbols, None, &mut consts);
        }
        consts
    }

    fn collect_consts_walk(node: LNode, symbols: &SymTable<LNode>, scope: Option<SymId>, out: &mut Vec<(ConstDef, Option<SymId>)>) {
        match node.kind() {
            LSyntaxKind::CONST_DEF => {
                if let Some(const_def) = ConstDef::cast(node) {
                    out.push((const_def, scope));
                }
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

                for const_node in model.nested_const_nodes() {
                    Self::collect_consts_walk(const_node, symbols, Some(model_scope), out);
                }
                for nested_model_node in model.nested_model_nodes() {
                    Self::collect_consts_walk(nested_model_node, symbols, Some(model_scope), out);
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
                    Self::collect_consts_walk(nested_model_node, symbols, Some(endpoint_scope), out);
                }
            }
            _ => {}
        }
    }

    fn const_decl_type(type_node: LNode, diag: &DiagnosticContext, errors: &mut Vec<SemanticAnalyzerError>) -> Option<ConstExprType> {
        let type_expr = Type::cast(type_node.clone())?;
        let atoms = type_expr.type_atoms();
        let atom = atoms.first()?;
        if atoms.len() != 1 || atom.is_array() || atom.is_optional() {
            let report = diag.error(type_node.text_range(), "Constants must be declared as integer, string, or bool");
            errors.push(SemanticAnalyzerError::ConstantError(report));
            return None;
        }
        let Some(primitive) = atom.as_primitive_type() else {
            let report = diag.error(type_node.text_range(), "Constants must be declared as integer, string, or bool");
            errors.push(SemanticAnalyzerError::ConstantError(report));
            return None;
        };
        match primitive {
            primitive if primitive.is_integer() => Some(ConstExprType::Int),
            PrimitiveType::String => Some(ConstExprType::String),
            PrimitiveType::Bool => Some(ConstExprType::Bool),
            _ => {
                let report = diag.error(type_node.text_range(), "Constants must be declared as integer, string, or bool");
                errors.push(SemanticAnalyzerError::ConstantError(report));
                None
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

            if let Some(tuple) = atom.as_tuple_type() {
                for item_type in tuple.item_type_nodes() {
                    Self::collect_type_refs(item_type, out);
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

        for const_node in endpoint.nested_const_nodes() {
            let report = diag.error_with_help(
                const_node.text_range(),
                "Constants cannot be declared directly inside endpoints",
                "Move this constant to the top level or to a named model.",
            );
            errors.push(SemanticAnalyzerError::ConstantError(report));
        }

        for field_node in endpoint.field_nodes() {
            Self::check_field(field_node, symbols, endpoint_scope, errors, diag.clone());
        }

        // Check nested models declared inside the endpoint
        for nested_model_node in endpoint.nested_model_nodes() {
            Self::check_model(nested_model_node, symbols, endpoint_scope, errors, diag.clone());
        }
    }

    fn check_service(node: LNode, symbols: &SymTable<LNode>, scope: Option<SymId>, errors: &mut Vec<SemanticAnalyzerError>, diag: DiagnosticContext) {
        let service = Service::cast(node).unwrap();
        let service_name = service.ident().unwrap();
        let service_scope = symbols.resolve_id(scope, &service_name);

        for rpc_node in service.rpc_nodes() {
            Self::check_rpc(rpc_node, symbols, service_scope, errors, diag.clone());
        }
    }

    fn check_rpc(node: LNode, symbols: &SymTable<LNode>, scope: Option<SymId>, errors: &mut Vec<SemanticAnalyzerError>, diag: DiagnosticContext) {
        let rpc = Rpc::cast(node.clone()).unwrap();
        let rpc_name = rpc.ident().unwrap();
        let rpc_scope = symbols.resolve_id(scope, &rpc_name);

        let mut has_body = false;
        let mut has_returns = false;
        for field_node in rpc.field_nodes() {
            let Some(field) = Field::cast(field_node.clone()) else {
                continue;
            };
            let Some(field_name) = field.ident() else {
                continue;
            };

            match field_name.as_str() {
                "body" => has_body = true,
                "returns" => has_returns = true,
                _ => {
                    let span = field.ident_node().map(|node| node.text_range()).unwrap_or_else(|| field.syntax().text_range());
                    let report = diag.error(span, &format!("Unknown rpc field '{}'. Expected 'body' or 'returns'.", field_name));
                    errors.push(SemanticAnalyzerError::DuplicateField(report));
                }
            }

            Self::check_field(field_node, symbols, rpc_scope, errors, diag.clone());
        }

        if !has_body {
            let report = diag.error(rpc.syntax().text_range(), &format!("RPC '{}' is missing required field 'body'", rpc_name));
            errors.push(SemanticAnalyzerError::MissingRequiredField(report));
        }
        if !has_returns {
            let report = diag.error(rpc.syntax().text_range(), &format!("RPC '{}' is missing required field 'returns'", rpc_name));
            errors.push(SemanticAnalyzerError::MissingRequiredField(report));
        }
    }

    fn check_field(node: LNode, symbols: &SymTable<LNode>, scope: Option<SymId>, errors: &mut Vec<SemanticAnalyzerError>, diag: DiagnosticContext) {
        let field = Field::cast(node.clone()).unwrap();

        let field_type = field.type_node().unwrap();
        Self::check_type(field_type, symbols, scope, errors, diag.clone());

        let decorators = field.decorator_nodes();
        for decorator_node in decorators {
            Self::check_decorator(decorator_node, symbols, scope, errors, diag.clone());
        }

        // Check that the default matches the type
        if let Some(field_default_value_node) = field.default_const_expr_node() {
            let evaluator = ConstEvaluator::new(symbols, diag.clone());
            let Some(default_expr) = field.default_const_expr() else {
                return;
            };
            let default_value = match evaluator.eval_expr(&default_expr, scope) {
                Ok(value) => value,
                Err(report) => {
                    errors.push(SemanticAnalyzerError::ConstantError(report));
                    return;
                }
            };
            let type_expr = Type::cast(field.type_node().unwrap().clone()).unwrap();
            let type_atom_nodes: Vec<_> = type_expr.type_atom_nodes();

            // TODO: Support unions
            if type_atom_nodes.len() == 1 {
                let type_atom = TypeAtom::cast(type_atom_nodes[0].clone()).unwrap();

                if let Some(primitive_type) = type_atom.as_primitive_type() {
                    let value_matches_primitive = |literal: &ConstValue| {
                        matches!((primitive_type, literal), (PrimitiveType::Bool, ConstValue::Bool(_)) | (PrimitiveType::String, ConstValue::String(_)))
                            || (primitive_type.is_integer() && matches!(literal, ConstValue::Int(_)))
                    };

                    if type_atom.is_array() {
                        let is_valid_array_default = match &default_value {
                            ConstValue::List(values) => values.iter().all(value_matches_primitive),
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
                } else if type_atom.as_tuple_type().is_some() {
                    let report = diag.error(field_default_value_node.text_range(), "Tuple default values are not supported yet");
                    errors.push(SemanticAnalyzerError::DuplicateField(report));
                } else {
                    // Not a literal type - must be a ref
                    if let Some(ref_name) = type_atom.as_ref_name()
                        && !ref_name.contains('.')
                    {
                        let ref_sym = symbols.resolve_id(scope, &ref_name).expect("Expected referenced symbol to exist");
                        let ref_entry = symbols.get(ref_sym).expect("Expected symbol entry to exist");

                        match (&ref_entry.data.kind(), &default_value) {
                            (LSyntaxKind::ENUM, ConstValue::String(variant_literal)) => {
                                let enum_node = ref_entry.data.clone();
                                let enum_model = Enum::cast(enum_node.clone()).unwrap();
                                let enum_ident_token = enum_model.ident_token().unwrap();
                                let enum_name_str = enum_ident_token.text().to_string();
                                let variant_exists = enum_model.variant_nodes().iter().any(|curr_variant_node| {
                                    let curr_variant = EnumVariant::cast(curr_variant_node.clone()).unwrap();
                                    let curr_variant_name = curr_variant.value().unwrap();
                                    *variant_literal == curr_variant_name
                                });
                                if !variant_exists {
                                    let report_label = diag.labeled_span(enum_node.text_range(), &format!("Enum '{}' defined here", enum_name_str));
                                    let report = diag.error_with_labels(
                                        field_default_value_node.text_range(),
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
                    let mut candidates = PrimitiveType::NAMES.to_vec();
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
                Self::check_anon_model(anon_model, symbols, scope, errors, diag.clone());
            }

            if let Some(record) = type_atom.as_record_type() {
                if let Some(src) = record.src_type_node() {
                    Self::check_type(src, symbols, scope, errors, diag.clone());
                }
                if let Some(dest) = record.dest_type_node() {
                    Self::check_type(dest, symbols, scope, errors, diag.clone());
                }
            }

            if let Some(tuple) = type_atom.as_tuple_type() {
                for item_type in tuple.item_type_nodes() {
                    Self::check_type(item_type, symbols, scope, errors, diag.clone());
                }
            }
        }
    }

    fn check_anon_model(anon_model: AnonModel, symbols: &SymTable<LNode>, scope: Option<SymId>, errors: &mut Vec<SemanticAnalyzerError>, diag: DiagnosticContext) {
        for node in anon_model
            .nested_model_nodes()
            .into_iter()
            .chain(anon_model.nested_enum_nodes())
            .chain(anon_model.nested_type_alias_nodes())
            .chain(anon_model.nested_const_nodes())
        {
            let report = diag.error_with_help(
                node.text_range(),
                "Anonymous structs cannot contain nested declarations",
                "Move nested models, enums, type aliases, or constants to the surrounding model scope.",
            );
            errors.push(SemanticAnalyzerError::DuplicateField(report));
        }

        let mut seen_fields = HashMap::new();
        for field_node in anon_model.field_nodes() {
            let Some(field) = Field::cast(field_node.clone()) else {
                continue;
            };
            let Some(field_name) = field.ident() else {
                continue;
            };
            if let Some(first_range) = seen_fields.insert(field_name.clone(), field_node.text_range()) {
                let first_label = diag.labeled_span(first_range, "First field defined here");
                let report = diag.error_with_labels(field_node.text_range(), &format!("Duplicate field name '{}'", field_name), None, None, vec![first_label]);
                errors.push(SemanticAnalyzerError::DuplicateField(report));
                continue;
            }

            Self::check_field(field_node, symbols, scope, errors, diag.clone());
        }
    }

    fn decorator_arg_value(arg: &DecoratorArg, symbols: &SymTable<LNode>, scope: Option<SymId>, errors: &mut Vec<SemanticAnalyzerError>, diag: DiagnosticContext) -> Option<ConstValue> {
        let evaluator = ConstEvaluator::new(symbols, diag);
        let expr = arg.const_expr()?;
        match evaluator.eval_expr(&expr, scope) {
            Ok(value) => Some(value),
            Err(report) => {
                errors.push(SemanticAnalyzerError::ConstantError(report));
                None
            }
        }
    }

    fn decorator_arg_accepts_value(decorator_name: &str, arg_id: &str, expected_ty: ConstExprType, value: &ConstValue) -> bool {
        if decorator_name == MODEL_FIELD_DECORATOR.id && arg_id == MODEL_FIELD_DECORATOR_EXAMPLE_ARG.id {
            return matches!(value, ConstValue::String(_) | ConstValue::Int(_) | ConstValue::Bool(_) | ConstValue::List(_));
        }
        expected_ty == value.ty()
    }

    // TODO: Check decorator contextually (e.g., only allow certain decorators on fields, models, etc.)
    fn check_decorator(node: LNode, symbols: &SymTable<LNode>, scope: Option<SymId>, errors: &mut Vec<SemanticAnalyzerError>, diag: DiagnosticContext) {
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
                let Some(value) = Self::decorator_arg_value(&effective_arg, symbols, scope, errors, diag.clone()) else {
                    return;
                };
                if !Self::decorator_arg_accepts_value(decorator_name.as_str(), required_arg_def.id, required_arg_def.ty, &value) {
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
                let Some(value) = Self::decorator_arg_value(&effective_arg, symbols, scope, errors, diag.clone()) else {
                    return;
                };
                if !Self::decorator_arg_accepts_value(decorator_name.as_str(), required_arg_def.id, required_arg_def.ty, &value) {
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

        for effective_arg in decorator.named_args() {
            let Some(arg_name) = effective_arg.ident() else {
                continue;
            };
            let Some(expected_arg_def) = builtin_decorator.named_args.iter().find(|arg| arg.id == arg_name) else {
                let report = diag.error(
                    effective_arg.syntax().text_range(),
                    &format!("Decorator '@{}' does not support named argument '{}'", decorator_name, arg_name),
                );
                errors.push(SemanticAnalyzerError::DuplicateField(report));
                return;
            };
            let Some(value) = Self::decorator_arg_value(&effective_arg, symbols, scope, errors, diag.clone()) else {
                return;
            };
            if !Self::decorator_arg_accepts_value(decorator_name.as_str(), expected_arg_def.id, expected_arg_def.ty, &value) {
                let report = diag.error(
                    effective_arg.syntax().text_range(),
                    &format!("Argument '{}' to decorator '@{}' has incorrect type", expected_arg_def.id, decorator_name),
                );
                errors.push(SemanticAnalyzerError::DuplicateField(report));
                return;
            }
        }

        // If there are positional args, check that they are in the correct order and have the correct types.
        let positional_args = decorator.positional_args();
        for (idx, effective_arg) in positional_args.iter().enumerate() {
            let expected_arg_def = builtin_decorator.positional_args.get(idx);
            if let Some(expected_arg_def) = expected_arg_def {
                let Some(value) = Self::decorator_arg_value(effective_arg, symbols, scope, errors, diag.clone()) else {
                    return;
                };
                if !Self::decorator_arg_accepts_value(decorator_name.as_str(), expected_arg_def.id, expected_arg_def.ty, &value) {
                    let report = diag.error_with_help(
                        effective_arg.syntax().text_range(),
                        &format!(
                            "Argument to decorator `@{}` has incorrect type (expected `{}` of type `{}` , received `{}`)",
                            decorator_name,
                            expected_arg_def.id,
                            expected_arg_def.ty,
                            value.ty()
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
                for nested_const_node in model.nested_const_nodes() {
                    let _ = Self::generate_symbol_table_walk(nested_const_node, syms, Some(model_scope_id), errors, diag);
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
            LSyntaxKind::SERVICE => {
                let service = Service::cast(node.clone()).unwrap();
                let ident_token = service.ident_token().unwrap();
                let service_name = ident_token.text().to_string();
                if syms.resolve(parent_scope, &service_name).is_some() {
                    let report = diag.error(ident_token.text_range(), &format!("Duplicate service name '{}'", service_name));
                    errors.push(SemanticAnalyzerError::DuplicateField(report));
                    return Err(());
                }
                let service_scope_id = syms.add_to_scope(parent_scope, &service_name, node);
                for rpc_node in service.rpc_nodes() {
                    let _ = Self::generate_symbol_table_walk(rpc_node, syms, Some(service_scope_id), errors, diag);
                }
            }
            LSyntaxKind::RPC => {
                let rpc = Rpc::cast(node.clone()).unwrap();
                let ident_token = rpc.ident_token().unwrap();
                let rpc_name = ident_token.text().to_string();
                if syms.resolve(parent_scope, &rpc_name).is_some() {
                    let report = diag.error(ident_token.text_range(), &format!("Duplicate rpc name '{}'", rpc_name));
                    errors.push(SemanticAnalyzerError::DuplicateField(report));
                    return Err(());
                }
                let rpc_scope_id = syms.add_to_scope(parent_scope, &rpc_name, node);
                for field_node in rpc.field_nodes() {
                    let _ = Self::generate_symbol_table_walk(field_node, syms, Some(rpc_scope_id), errors, diag);
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
            LSyntaxKind::CONST_DEF => {
                let const_def = ConstDef::cast(node.clone()).unwrap();
                let ident_token = const_def.ident_token().unwrap();
                let const_name = ident_token.text().to_string();
                if syms.resolve(parent_scope, &const_name).is_some() {
                    let report = diag.error(ident_token.text_range(), &format!("Duplicate constant name '{}'", const_name));
                    errors.push(SemanticAnalyzerError::DuplicateField(report));
                    return Err(());
                }
                syms.add_to_scope(parent_scope, &const_name, node);
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

    use crate::{
        metadata::SourceCodeMetadata,
        semantic_analyzer::{AnalyzedProgram, SemanticAnalyzer, SemanticAnalyzerError, SemanticAnalyzerOptions, SemanticWarningCode},
        syntax::Parser,
    };

    fn analyze_source(src: &'static str) -> Result<AnalyzedProgram, Vec<SemanticAnalyzerError>> {
        let metadata = SourceCodeMetadata {
            file_name: "test.glue",
            file_contents: src,
        };
        let parsed = Parser::new().parse(&metadata).unwrap();
        SemanticAnalyzer::new().analyze(&parsed, &metadata)
    }

    #[test]
    fn test_constants_fold_and_validate_defaults_and_decorators() {
        let src = indoc! { r#"
            const MAX_PAGE_SIZE: int = 100
            const DEFAULT_LIMIT = MAX_PAGE_SIZE * 2
            const USER_ALIAS = "user_" + "id"

            model Request {
                limit: int = DEFAULT_LIMIT
                @field(alias=USER_ALIAS)
                user_id: string
            }
        "# };

        let result = analyze_source(src).expect("Expected constants to analyze");
        assert!(result.warnings.is_empty(), "Expected CONSTANT_CASE constants to avoid warnings");
    }

    #[test]
    fn test_inferred_constant_types_validate() {
        let src = indoc! { r#"
            const DEFAULT_LIMIT = 100 * 2
            const USER_ALIAS = "user_" + "id"
            const ENABLED = true

            model Request {
                limit: int = DEFAULT_LIMIT
                @field(alias=USER_ALIAS)
                user_id: string
                enabled: bool = ENABLED
            }
        "# };

        let result = analyze_source(src).expect("Expected inferred constants to analyze");
        assert!(result.warnings.is_empty(), "Expected CONSTANT_CASE constants to avoid warnings");
    }

    #[test]
    fn test_model_scoped_constants_validate_defaults_and_decorators() {
        let src = indoc! { r#"
            model Aliases {
                const USER_ID_ALIAS = "user_" + "id"
                const BASE_TAG = 10
            }

            model User {
                const DEFAULT_LIMIT = 100

                limit: int = DEFAULT_LIMIT
                @field(alias=Aliases.USER_ID_ALIAS)
                user_id: string
                @field(proto_tag=Aliases.BASE_TAG + 1)
                tagged_id: int
            }
        "# };

        let result = analyze_source(src).expect("Expected model-scoped constants to analyze");
        assert!(result.warnings.is_empty(), "Expected CONSTANT_CASE constants to avoid warnings");
    }

    #[test]
    fn test_qualified_model_constant_nested_chain_analyzes() {
        let src = indoc! { r#"
            model Outer {
                model Inner {
                    const USER_ID_ALIAS = "user_id"
                }
            }

            model User {
                @field(alias=Outer.Inner.USER_ID_ALIAS)
                user_id: string
            }
        "# };

        let result = analyze_source(src);
        assert!(result.is_ok(), "Expected nested qualified model constant to analyze");
    }

    #[test]
    fn test_private_model_constant_qualified_from_outside_fails() {
        let src = indoc! { r#"
            model Aliases {
                const _USER_ID_ALIAS = "user_id"
            }

            model User {
                @field(alias=Aliases._USER_ID_ALIAS)
                user_id: string
            }
        "# };

        let result = analyze_source(src);
        assert!(result.is_err(), "Expected private qualified model constant to fail");
        let errors = result.err().unwrap();
        assert!(errors.iter().any(|e| e.report().to_string().contains("private to model")), "Expected private constant error");
    }

    #[test]
    fn test_qualified_model_type_alias_in_decorator_arg_fails_as_not_constant() {
        let src = indoc! { r#"
            model Aliases {
                type USER_ID_ALIAS = string
            }

            model User {
                @field(alias=Aliases.USER_ID_ALIAS)
                user_id: string
            }
        "# };

        let result = analyze_source(src);
        assert!(result.is_err(), "Expected qualified type alias reference to fail as non-constant");
        let errors = result.err().unwrap();
        assert!(errors.iter().any(|e| e.report().to_string().contains("is not a constant")), "Expected not-a-constant error");
    }

    #[test]
    fn test_missing_qualified_model_constant_fails() {
        let src = indoc! { r#"
            model Aliases {
                const USER_ID_ALIAS = "user_id"
            }

            model User {
                @field(alias=Aliases.MISSING_ALIAS)
                user_id: string
            }
        "# };

        let result = analyze_source(src);
        assert!(result.is_err(), "Expected missing qualified model constant to fail");
        let errors = result.err().unwrap();
        assert!(errors.iter().any(|e| e.report().to_string().contains("Undefined constant reference 'Aliases.MISSING_ALIAS'")));
    }

    #[test]
    fn test_model_constant_refs_evaluate_in_declaration_scope() {
        let src = indoc! { r#"
            model Aliases {
                const SUFFIX = "_alias"
                const USER_ID_ALIAS = "user_id" + SUFFIX
            }

            model User {
                const SUFFIX = "_user"

                @field(alias=Aliases.USER_ID_ALIAS)
                user_id: string
            }
        "# };

        let result = analyze_source(src);
        assert!(result.is_ok(), "Expected qualified constant to evaluate in its declaration scope");
    }

    #[test]
    fn test_anonymous_struct_constants_fail() {
        let src = indoc! { r#"
            model User {
                profile: {
                    const PROFILE_ALIAS = "profile"
                    name: string
                }
            }
        "# };

        let result = analyze_source(src);
        assert!(result.is_err(), "Expected constants in anonymous structs to fail");
        let errors = result.err().unwrap();
        assert!(errors.iter().any(|e| e.report().to_string().contains("Anonymous structs cannot contain nested declarations")));
    }

    #[test]
    fn test_endpoint_constants_fail() {
        let src = indoc! { r#"
            endpoint "GET /users" ListUsers {
                const USER_ID_ALIAS = "user_id"
                responses: User[]
            }

            model User {
                id: string
            }
        "# };

        let result = analyze_source(src);
        assert!(result.is_err(), "Expected constants directly inside endpoints to fail");
        let errors = result.err().unwrap();
        assert!(errors.iter().any(|e| e.report().to_string().contains("Constants cannot be declared directly inside endpoints")));
    }

    #[test]
    fn test_private_constant_case_allows_leading_underscore() {
        let src = indoc! { r#"
            const _RETRY_MS = (100 + 50) * 2

            model Request {
                retry_ms: int = _RETRY_MS
            }
        "# };

        let result = analyze_source(src).expect("Expected private constant to analyze");
        assert!(result.warnings.is_empty(), "Expected _CONSTANT_CASE to avoid warnings");
    }

    #[test]
    fn test_constant_case_warning_is_non_fatal() {
        let src = indoc! { r#"
            const MaxPageSize: int = 100

            model Request {
                limit: int = MaxPageSize
            }
        "# };

        let result = analyze_source(src).expect("Expected warning not to fail analysis");
        assert_eq!(result.warnings.len(), 1);
        assert_eq!(result.warnings[0].code, SemanticWarningCode::ConstantCase);
        assert!(result.warnings[0].report().to_string().contains("CONSTANT_CASE"));
    }

    #[test]
    fn test_constant_case_warning_can_be_suppressed() {
        let src = indoc! { r#"
            const MaxPageSize: int = 100

            model Request {
                limit: int = MaxPageSize
            }
        "# };
        let metadata = SourceCodeMetadata {
            file_name: "test.glue",
            file_contents: src,
        };
        let parsed = Parser::new().parse(&metadata).unwrap();
        let analyzer = SemanticAnalyzer::with_options(SemanticAnalyzerOptions {
            suppress_warnings: ["constant_case".to_string()].into_iter().collect(),
        });
        let result = analyzer.analyze(&parsed, &metadata).expect("Expected warning suppression not to fail analysis");
        assert!(result.warnings.is_empty());
    }

    #[test]
    fn test_constant_cycle_fails() {
        let src = indoc! { r#"
            const A: int = B + 1
            const B: int = A + 1

            model Request {
                value: int = A
            }
        "# };

        let result = analyze_source(src);
        assert!(result.is_err(), "Expected constant cycle to fail");
        let errors = result.err().unwrap();
        assert!(errors.iter().any(|e| e.report().to_string().contains("Circular constant reference")));
    }

    #[test]
    fn test_invalid_mixed_constant_addition_fails() {
        let src = indoc! { r#"
            const BAD: string = "user_" + 1

            model Request {
                value: string = BAD
            }
        "# };

        let result = analyze_source(src);
        assert!(result.is_err(), "Expected mixed string/int + to fail");
        let errors = result.err().unwrap();
        assert!(errors.iter().any(|e| e.report().to_string().contains("Operator '+' cannot be applied")));
    }

    #[test]
    fn test_string_multiplication_fails() {
        let src = indoc! { r#"
            const BAD: string = "a" * "b"

            model Request {
                value: string = BAD
            }
        "# };

        let result = analyze_source(src);
        assert!(result.is_err(), "Expected string * to fail");
        let errors = result.err().unwrap();
        assert!(errors.iter().any(|e| e.report().to_string().contains("Operator '*' cannot be applied")));
    }

    #[test]
    fn test_list_constant_fails_even_when_type_is_inferred() {
        let src = indoc! { r#"
            const BAD = [1, 2]

            model Request {
                value: int = 1
            }
        "# };

        let result = analyze_source(src);
        assert!(result.is_err(), "Expected list constant to fail");
        let errors = result.err().unwrap();
        assert!(errors.iter().any(|e| e.report().to_string().contains("Constants must fold to integer, string, or bool")));
    }

    #[test]
    fn test_integer_primitive_variants_parse_and_validate_defaults() {
        let src = indoc! { r#"
            const LIMIT: u32 = 10

            model Numbers {
                int_value: int = LIMIT
                uint_value: uint = 1
                i8_value: i8 = 1
                i16_value: i16 = 1
                i32_value: i32 = 1
                i64_value: i64 = 1
                u8_value: u8 = 1
                u16_value: u16 = 1
                u32_value: u32 = 1
                u64_value: u64 = 1
            }
        "# };

        assert!(analyze_source(src).is_ok(), "Expected integer primitive variants to analyze");
    }

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
    fn test_tuple_unknown_type_fails() {
        let src = indoc! { r#"
            model User {
                pair: (string, Foo)
            }
        "# };

        let source = SourceCodeMetadata {
            file_name: "test.glue",
            file_contents: src,
        };
        let parsed = Parser::new().parse(&source).unwrap();
        let result = SemanticAnalyzer::new().analyze(&parsed, &source);
        assert!(result.is_err(), "Expected semantic analysis to fail for unknown tuple type");
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
    fn test_valid_anonymous_struct_fields_pass() {
        let src = indoc! { r#"
        model User {
            profile: {
                bio: string
                age?: int
                friend: Friend
            }

            model Friend {
                id: string
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
    fn test_anonymous_struct_duplicate_field_fails() {
        let src = indoc! { r#"
        model User {
            profile: {
                bio: string
                bio: int
            }
        }
        "# };
        let metadata = SourceCodeMetadata {
            file_name: "test.glue",
            file_contents: src,
        };
        let parsed = Parser::new().parse(&metadata).unwrap();
        let analyzed = SemanticAnalyzer::new().analyze(&parsed, &metadata);
        assert!(analyzed.is_err());
    }

    #[test]
    fn test_anonymous_struct_field_validation_fails() {
        let src = indoc! { r#"
        model User {
            profile: {
                age: int = "not an int"
                friend: MissingFriend
                @unknown
                name: string
            }
        }
        "# };
        let metadata = SourceCodeMetadata {
            file_name: "test.glue",
            file_contents: src,
        };
        let parsed = Parser::new().parse(&metadata).unwrap();
        let analyzed = SemanticAnalyzer::new().analyze(&parsed, &metadata);
        assert!(analyzed.is_err());
    }

    #[test]
    fn test_anonymous_struct_nested_declaration_is_rejected() {
        let src = indoc! { r#"
        model User {
            profile: {
                model Inner {
                    id: string
                }
            }
        }
        "# };
        let metadata = SourceCodeMetadata {
            file_name: "test.glue",
            file_contents: src,
        };
        let parsed = Parser::new().parse(&metadata);
        assert!(parsed.is_err());
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
    fn test_service_rpc_valid_type_passes() {
        let src = indoc! { r#"
            model GetUserRequest {
                user_id: int
            }

            model User {
                user_id: int
            }

            service UserService {
                rpc GetUser {
                    body: GetUserRequest
                    returns: User
                }
            }
        "# };

        let result = analyze_source(src);
        assert!(result.is_ok(), "Expected analysis to pass for valid service rpc");
    }

    #[test]
    fn test_service_rpc_missing_body_fails() {
        let src = indoc! { r#"
            model User {
                user_id: int
            }

            service UserService {
                rpc GetUser {
                    returns: User
                }
            }
        "# };

        let result = analyze_source(src);
        assert!(result.is_err(), "Expected analysis to fail for missing rpc body");
        let errors = result.err().unwrap();
        assert!(matches!(errors[0], SemanticAnalyzerError::MissingRequiredField(_)));
    }

    #[test]
    fn test_service_rpc_missing_returns_fails() {
        let src = indoc! { r#"
            model GetUserRequest {
                user_id: int
            }

            service UserService {
                rpc GetUser {
                    body: GetUserRequest
                }
            }
        "# };

        let result = analyze_source(src);
        assert!(result.is_err(), "Expected analysis to fail for missing rpc returns");
        let errors = result.err().unwrap();
        assert!(matches!(errors[0], SemanticAnalyzerError::MissingRequiredField(_)));
    }

    #[test]
    fn test_service_duplicate_rpc_name_fails() {
        let src = indoc! { r#"
            model Request {
                id: int
            }

            model Response {
                id: int
            }

            service UserService {
                rpc GetUser {
                    body: Request
                    returns: Response
                }

                rpc GetUser {
                    body: Request
                    returns: Response
                }
            }
        "# };

        let result = analyze_source(src);
        assert!(result.is_err(), "Expected analysis to fail for duplicate rpc name");
        let errors = result.err().unwrap();
        assert!(errors.iter().any(|e| e.report().to_string().contains("Duplicate rpc name 'GetUser'")));
    }

    #[test]
    fn test_service_rpc_unknown_field_fails() {
        let src = indoc! { r#"
            model Request {
                id: int
            }

            model Response {
                id: int
            }

            service UserService {
                rpc GetUser {
                    body: Request
                    returns: Response
                    timeout: int
                }
            }
        "# };

        let result = analyze_source(src);
        assert!(result.is_err(), "Expected analysis to fail for unknown rpc field");
        let errors = result.err().unwrap();
        assert!(errors.iter().any(|e| e.report().to_string().contains("Unknown rpc field 'timeout'")));
    }

    #[test]
    fn test_service_rpc_undefined_type_fails() {
        let src = indoc! { r#"
            model Response {
                id: int
            }

            service UserService {
                rpc GetUser {
                    body: MissingRequest
                    returns: Response
                }
            }
        "# };

        let result = analyze_source(src);
        assert!(result.is_err(), "Expected analysis to fail for undefined rpc type");
        let errors = result.err().unwrap();
        assert!(errors.iter().any(|e| e.report().to_string().contains("MissingRequest")));
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
