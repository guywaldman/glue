use std::sync::{Arc, Mutex};

use miette::Report;

use crate::{
    EnumVariant, Literal, LiteralExpr, PrimitiveType, SourceCodeMetadata,
    diagnostics::DiagnosticContext,
    symbols::{SymId, SymTable},
    syntax::{AstNode, Enum, Field, LNode, LSyntaxKind, Model, ParsedProgram, Type, TypeAtom},
    utils::fuzzy_match,
};

#[derive(Debug)]
pub enum SemanticAnalyzerError {
    DuplicateField(Report),
    UndefinedTypeReference(Report),
}

impl SemanticAnalyzerError {
    pub fn report(&self) -> &Report {
        match self {
            SemanticAnalyzerError::DuplicateField(report) | SemanticAnalyzerError::UndefinedTypeReference(report) => report,
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
        let errors = Arc::new(Mutex::new(Vec::new()));

        let root = parsed.ast_root.clone();
        let targets: Vec<_> = root.children().filter(|n| n.kind() == LSyntaxKind::MODEL).map(|n| (n.kind(), n.text_range())).collect();
        let green_node = root.green();

        let symbols = Self::generate_symbol_table(parsed, errors.clone(), diagnostic_ctx.clone());

        // TODO: Parallelize
        targets.iter().for_each(|&(kind, range)| {
            let local_root: LNode = rowan::SyntaxNode::new_root(green_node.clone().into());
            let element = local_root.covering_element(range);
            let node = element.into_node().expect("expected node at range");
            match kind {
                LSyntaxKind::MODEL => {
                    // TODO: Remove clone for symbols
                    Self::check_model(node, &symbols, None, errors.clone(), diagnostic_ctx.clone());
                }
                _ => {}
            }
        });

        let errors = Arc::try_unwrap(errors).unwrap().into_inner().unwrap();

        if !errors.is_empty() { Err(errors) } else { Ok(AnalyzedProgram { ast_root: root, symbols }) }
    }

    fn check_model(node: LNode, symbols: &SymTable<LNode>, scope: Option<SymId>, errors: Arc<Mutex<Vec<SemanticAnalyzerError>>>, diag: DiagnosticContext) {
        let model = Model::cast(node.clone()).unwrap();
        let model_fields = model.field_nodes();
        let model_ident_token = model.ident_token().unwrap();
        let model_name = model_ident_token.text().to_string();
        let model_scope = symbols.resolve_id(scope, &model_name);
        for field_node in model_fields {
            Self::check_field(field_node, symbols, model_scope, errors.clone(), diag.clone());
        }

        for nested_model_node in model.nested_model_nodes() {
            Self::check_model(nested_model_node, symbols, model_scope, errors.clone(), diag.clone());
        }
    }

    fn check_field(node: LNode, symbols: &SymTable<LNode>, scope: Option<SymId>, errors: Arc<Mutex<Vec<SemanticAnalyzerError>>>, diag: DiagnosticContext) {
        let field = Field::cast(node.clone()).unwrap();
        let field_type = field.type_node().unwrap();
        Self::check_type(field_type, symbols, scope, errors.clone(), diag.clone());

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
                    match (primitive_type, default_value) {
                        (PrimitiveType::Bool, Literal::BoolLiteral(_)) => {}
                        (PrimitiveType::Int, Literal::IntLiteral(_)) => {}
                        (PrimitiveType::Float, Literal::FloatLiteral(_)) => {}
                        (PrimitiveType::String, Literal::StringLiteral(_)) => {}
                        _ => {
                            let report = diag.error(field_default_value_node.text_range(), format_args!("Type of default value does not match field type"), None);
                            errors.lock().unwrap().push(SemanticAnalyzerError::DuplicateField(report));
                        }
                    }
                } else {
                    // Not a literal type - must be a ref
                    let ref_name = type_atom.ident_token().unwrap().text().to_string();
                    let ref_sym = symbols.resolve_id(scope, &ref_name).expect("Expected referenced symbol to exist");
                    let ref_entry = symbols.get(ref_sym).expect("Expected symbol entry to exist");

                    match (&ref_entry.data.kind(), &default_value) {
                        (LSyntaxKind::ENUM, Literal::StringLiteral(variant_literal)) => {
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
                                let report_label = diag.labeled_span(enum_node.text_range(), format!("Enum '{}' defined here", enum_name_str));
                                let report = diag.error_with_labels(
                                    field_default_value_node.text_range(),
                                    format_args!("Enum variant '{}' does not exist in enum '{}'", variant_literal, enum_name_str),
                                    None,
                                    None,
                                    vec![report_label],
                                );
                                errors.lock().unwrap().push(SemanticAnalyzerError::DuplicateField(report));
                            }
                        }
                        _ => {
                            let report = diag.error(field_default_value_node.text_range(), format_args!("Type of default value does not match field type"), None);
                            errors.lock().unwrap().push(SemanticAnalyzerError::DuplicateField(report));
                        }
                    }
                }
            }
        }
    }

    fn check_type(node: LNode, symbols: &SymTable<LNode>, scope: Option<SymId>, errors: Arc<Mutex<Vec<SemanticAnalyzerError>>>, diag: DiagnosticContext) {
        let type_expr = Type::cast(node.clone()).unwrap();
        let type_atom_nodes = type_expr.type_atom_nodes();
        for type_atom_node in type_atom_nodes {
            let type_atom = TypeAtom::cast(type_atom_node.clone()).unwrap();
            if let Some(ident_token) = type_atom.ident_token() {
                // Ident - could be a non-existent primitive type, or a ref
                let type_name = ident_token.text().to_string();
                if let Some(scope) = scope
                    && symbols.resolve_id(Some(scope), &type_name).is_none()
                {
                    let mut candidates = vec!["string", "int", "float", "bool"];
                    let symbol_entries = symbols.entries(Some(scope));
                    candidates.extend(symbol_entries.iter().map(|entry| entry.name.rsplit("::").nth(0).unwrap()));
                    let suggested_names = fuzzy_match(&type_name, &candidates, 1);
                    if let Some(suggested_name) = suggested_names.first()
                        && suggested_name.1 >= 50
                    {
                        let report = diag.error_with_labels(
                            ident_token.text_range(),
                            format_args!("Undefined type reference '{}'", type_name),
                            Some(format!("Did you mean '{}'?", suggested_name.0)),
                            Some("Undefined reference".to_string()),
                            vec![],
                        );
                        errors.lock().unwrap().push(SemanticAnalyzerError::UndefinedTypeReference(report));
                        continue;
                    }
                    let report = diag.error(ident_token.text_range(), format_args!("Undefined type reference '{}'", type_name), None);
                    errors.lock().unwrap().push(SemanticAnalyzerError::UndefinedTypeReference(report));
                }
            }
        }
    }

    fn generate_symbol_table(parsed: &ParsedProgram, errors: Arc<Mutex<Vec<SemanticAnalyzerError>>>, diag: DiagnosticContext) -> SymTable<LNode> {
        let root = &parsed.ast_root;

        let mut syms = SymTable::new();
        let top_level_nodes = root.children();
        for child in top_level_nodes {
            if Self::generate_symbol_table_walk(child, &mut syms, None, errors.clone(), diag.clone()).is_err() {
                continue;
            }
        }

        syms
    }

    fn generate_symbol_table_walk(
        node: LNode,
        syms: &mut SymTable<LNode>,
        parent_scope: Option<SymId>,
        errors: Arc<Mutex<Vec<SemanticAnalyzerError>>>,
        diag: DiagnosticContext,
    ) -> Result<Option<SymId>, ()> {
        match node.kind() {
            LSyntaxKind::MODEL => {
                let model = Model::cast(node.clone()).unwrap();
                let ident_token = model.ident_token().unwrap();
                let model_name = ident_token.text().to_string();
                if syms.resolve(parent_scope, &model_name).is_some() {
                    let report = diag.error(ident_token.text_range(), format_args!("Duplicate model name '{}'", model_name), None);
                    errors.lock().unwrap().push(SemanticAnalyzerError::DuplicateField(report));
                    return Err(());
                }
                let model_scope_id = syms.add_to_scope(parent_scope, &model_name, node);
                for field_node in model.field_nodes() {
                    let _ = Self::generate_symbol_table_walk(field_node, syms, Some(model_scope_id), errors.clone(), diag.clone());
                }
                for nested_model_node in model.nested_model_nodes() {
                    let _ = Self::generate_symbol_table_walk(nested_model_node, syms, Some(model_scope_id), errors.clone(), diag.clone());
                }
                for nested_enum_node in model.nested_enum_nodes() {
                    let _ = Self::generate_symbol_table_walk(nested_enum_node, syms, Some(model_scope_id), errors.clone(), diag.clone());
                }
            }
            LSyntaxKind::ENUM => {
                let enum_model = Enum::cast(node.clone()).unwrap();
                let ident_token = enum_model.ident_token().unwrap();
                let enum_name = ident_token.text().to_string();
                if syms.resolve(parent_scope, &enum_name).is_some() {
                    let report = diag.error(ident_token.text_range(), format_args!("Duplicate enum name '{}'", enum_name), None);
                    errors.lock().unwrap().push(SemanticAnalyzerError::DuplicateField(report));
                    return Err(());
                }
                syms.add_to_scope(parent_scope, &enum_name, node);
            }
            LSyntaxKind::FIELD => {
                let field = Field::cast(node.clone()).unwrap();
                let ident_token = field.ident_token().unwrap();
                let field_name = ident_token.text().to_string();
                if syms.resolve(parent_scope, &field_name).is_some() {
                    let report = diag.error(ident_token.text_range(), format_args!("Duplicate field name '{}'", field_name), None);
                    errors.lock().unwrap().push(SemanticAnalyzerError::DuplicateField(report));
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
    fn invalid_model_basic() {
        let src = indoc! { r#"
		// This is a great model
        model Foo {
            @deprecated
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
}
