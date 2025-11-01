use std::sync::{Arc, Mutex, RwLock};

use colored::Colorize;
use miette::{LabeledSpan, Report};
use rayon::prelude::*;

use crate::{
    diagnostics::DiagnosticContext,
    symbols::{SymId, SymTable},
    syntax::{AstNode, Field, LNode, LSyntaxKind, Model, Parsed, Type, TypeAtom},
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
pub struct AnalyzedProgram<'a> {
    pub parsed: Parsed<'a>,
    pub symbols: SymTable,
}

pub struct SemanticAnalyzer {}

impl SemanticAnalyzer {
    pub fn new() -> Self {
        Self {}
    }

    pub fn analyze<'a>(&self, parsed: Parsed<'a>) -> Result<AnalyzedProgram<'a>, Vec<SemanticAnalyzerError>> {
        let diagnostic_ctx = DiagnosticContext::new(parsed.metadata.file_name, parsed.metadata.file_contents);
        let errors = Arc::new(Mutex::new(Vec::new()));

        let root = parsed.root.clone();
        let targets: Vec<_> = root.children().filter(|n| n.kind() == LSyntaxKind::MODEL).map(|n| (n.kind(), n.text_range())).collect();
        let green_node = root.green();

        let symbols = Self::generate_symbol_table(&parsed, errors.clone(), diagnostic_ctx.clone());
        let symbols = Arc::new(RwLock::new(symbols));

        targets.par_iter().for_each(|&(kind, range)| {
            let local_root: LNode = rowan::SyntaxNode::new_root(green_node.clone().into());
            let element = local_root.covering_element(range);
            let node = element.into_node().expect("expected node at range");
            match kind {
                LSyntaxKind::MODEL => {
                    Self::check_model(node, symbols.clone(), None, errors.clone(), diagnostic_ctx.clone());
                }
                _ => {}
            }
        });

        let errors = Arc::try_unwrap(errors).unwrap().into_inner().unwrap();

        let symbols = Arc::try_unwrap(symbols).unwrap().into_inner().unwrap();
        if !errors.is_empty() { Err(errors) } else { Ok(AnalyzedProgram { parsed, symbols }) }
    }

    fn check_model(node: LNode, symbols: Arc<RwLock<SymTable>>, scope: Option<SymId>, errors: Arc<Mutex<Vec<SemanticAnalyzerError>>>, diag: DiagnosticContext) {
        let model = Model::cast(node.clone()).unwrap();
        let model_fields = model.field_nodes();
        let model_ident_token = model.ident_token().unwrap();
        let model_name = model_ident_token.text().to_string();
        let model_scope = symbols.read().unwrap().resolve(scope, &model_name);
        for field_node in model_fields {
            Self::check_field(field_node, symbols.clone(), model_scope, errors.clone(), diag.clone());
        }
    }

    fn check_field(node: LNode, symbols: Arc<RwLock<SymTable>>, scope: Option<SymId>, errors: Arc<Mutex<Vec<SemanticAnalyzerError>>>, diag: DiagnosticContext) {
        let field = Field::cast(node.clone()).unwrap();
        let field_type = field.type_node().unwrap();
        Self::check_type(field_type, symbols, scope, errors, diag);
    }

    fn check_type(node: LNode, symbols: Arc<RwLock<SymTable>>, scope: Option<SymId>, errors: Arc<Mutex<Vec<SemanticAnalyzerError>>>, diag: DiagnosticContext) {
        let type_expr = Type::cast(node.clone()).unwrap();
        let type_atom_nodes = type_expr.type_atom_nodes();
        for type_atom_node in type_atom_nodes {
            let type_atom = TypeAtom::cast(type_atom_node.clone()).unwrap();
            if let Some(ident_token) = type_atom.ident_token() {
                // Ident - could be a non-existent primitive type, or a ref
                let type_name = ident_token.text().to_string();
                let symbols = symbols.read().unwrap();
                if let Some(scope) = scope
                    && symbols.resolve(Some(scope), &type_name).is_none()
                {
                    let mut candidates = vec!["string", "int", "float", "bool"];
                    candidates.extend(symbols.entries(Some(scope)).iter().map(|(_, name)| name));
                    let suggested_names = fuzzy_match(&type_name, &candidates, 1);
                    if let Some(suggested_name) = suggested_names.first()
                        && suggested_name.1 >= 50
                    {
                        let report = diag.error_with_labels(
                            ident_token.text_range(),
                            format_args!("Undefined type reference '{}'", type_name),
                            None,
                            Some(format!("Did you mean '{}'?", suggested_name.0)),
                            vec![],
                        );
                        errors.lock().unwrap().push(SemanticAnalyzerError::UndefinedTypeReference(report));
                        continue;
                    }
                    let report = diag.error(ident_token.text_range(), format_args!("Undefined type reference '{}'", type_name), None);
                    errors.lock().unwrap().push(SemanticAnalyzerError::UndefinedTypeReference(report));
                }
                // match type_name.as_str() {
                //     "string" | "int" | "float" | "bool" => {}
                //     _ => {
                //         let report = diag.error(ident_token.text_range(), format_args!("Undefined type reference '{}'", type_name), None);
                //         errors.lock().unwrap().push(SemanticAnalyzerError::UndefinedTypeReference(report));
                //     }
                // }
            }
        }
    }

    fn generate_symbol_table(parsed: &Parsed, errors: Arc<Mutex<Vec<SemanticAnalyzerError>>>, diag: DiagnosticContext) -> SymTable {
        let root = &parsed.root;

        let mut syms = SymTable::new();
        let top_level_nodes = root.children();
        for child in top_level_nodes {
            if Self::generate_symbol_table_walk(child, &mut syms, None, errors.clone(), diag.clone()).is_err() {
                continue;
            }
        }

        syms
    }

    fn generate_symbol_table_walk(node: LNode, syms: &mut SymTable, parent_scope: Option<SymId>, errors: Arc<Mutex<Vec<SemanticAnalyzerError>>>, diag: DiagnosticContext) -> Result<Option<SymId>, ()> {
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
                let model_scope_id = syms.add_to_scope(parent_scope, &model_name);
                let field_nodes = model.field_nodes();
                for field_node in field_nodes {
                    let _ = Self::generate_symbol_table_walk(field_node, syms, Some(model_scope_id), errors.clone(), diag.clone());
                }
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
                syms.add_to_scope(parent_scope, &field_name);
            }
            _ => {}
        }
        Ok(None)
    }
}

#[cfg(test)]
mod tests {
    use indoc::indoc;

    use crate::{metadata::SourceCodeMetadata, semantic_analyzer::SemanticAnalyzer, syntax::LParser};

    #[test]
    fn invalid_model_basic() {
        let src = indoc! { r#"
								model todo {
									name: string
								}
								"# };

        let metadata = SourceCodeMetadata {
            file_name: "test.glue",
            file_contents: src,
        };
        let parsed = LParser::new().parse(metadata).unwrap();
        let analyzed = SemanticAnalyzer::new().analyze(parsed);
        dbg!(&analyzed);
    }
}
