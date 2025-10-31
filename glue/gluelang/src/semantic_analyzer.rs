use std::sync::{Arc, Mutex};

use colored::Colorize;
use miette::{LabeledSpan, Report};
use rayon::prelude::*;

use crate::{Ast, DiagnosticContext, RawTypeAtom, Type, ast::Model, utils};

/// Performs semantic analysis on the AST and symbol table.
#[derive(Clone)]
pub struct SemanticAnalyzer<'a> {
    pub ast: &'a Ast<'a>,
    diagnostic_ctx: DiagnosticContext,
}

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

impl<'a> SemanticAnalyzer<'a> {
    pub fn new(ast: &'a Ast<'a>) -> Self {
        let src_file_name = ast.source_code_metadata.file_name;
        let src_file_contents = ast.source_code_metadata.contents;
        let diagnostic_ctx = DiagnosticContext::new(src_file_name, src_file_contents);
        Self { ast, diagnostic_ctx }
    }

    pub fn check(&self) -> Result<(), Vec<SemanticAnalyzerError>> {
        let errors = Arc::new(Mutex::new(Vec::new()));
        self.ast.nodes().par_iter().for_each(|node| {
            if let Some(model) = node.as_model() {
                self.check_model(model, &errors);
            }
        });
        let errors = Arc::try_unwrap(errors).unwrap().into_inner().unwrap();
        if !errors.is_empty() { Err(errors) } else { Ok(()) }
    }

    fn check_model(&self, model: &Model<'a>, errors: &Arc<Mutex<Vec<SemanticAnalyzerError>>>) {
        for field_id in model.fields.iter().copied() {
            self.check_field(field_id, errors);
        }
    }

    fn check_field(&self, field_id: usize, errors: &Arc<Mutex<Vec<SemanticAnalyzerError>>>) {
        let Some(node) = self.ast.node(field_id) else {
            return;
        };

        let Some(field) = node.as_field() else {
            return;
        };

        let scopes = self.ast.scope(node.id);
        let Some((_, siblings)) = scopes.iter().nth(0) else {
            return;
        };

        let mut first_field_id = None;
        for sibling_id in siblings {
            if let Some(sibling) = self.ast.node(sibling_id)
                && let Some(sibling_field) = sibling.as_field()
                && sibling_field.name == field.name
            {
                first_field_id = Some(sibling.id);
                break;
            }
        }

        let Some(first_field_id) = first_field_id else {
            return;
        };

        let duplicate_span = node.span;
        let mut extra_labels = Vec::new();
        if let Some(original_node) = self.ast.node(first_field_id) {
            extra_labels.push(LabeledSpan::at(
                original_node.span.start()..original_node.span.end(),
                format!("previous definition of `{}`", field.name),
            ));
        }
        let report = self.diagnostic_ctx.error_with_labels(
            duplicate_span,
            format_args!("Duplicate field `{}` within the same scope", field.name),
            None,
            Some(format!("`{}` redeclared here", field.name)),
            extra_labels,
        );
        errors.lock().unwrap().push(SemanticAnalyzerError::DuplicateField(report));
    }

    #[allow(dead_code)]
    fn check_type(&self, node_id: usize, errors: &Arc<Mutex<Vec<SemanticAnalyzerError>>>) {
        let Some(node) = self.ast.node(node_id) else {
            return;
        };

        let Some(ty) = node.as_type() else {
            return;
        };

        let Type { atoms, .. } = ty;

        for type_atom_id in atoms {
            let Some(type_atom_node) = self.ast.node(*type_atom_id) else {
                continue;
            };
            let Some(type_atom) = type_atom_node.as_type_atom() else {
                continue;
            };
            if let RawTypeAtom::Ref(ref_name) = type_atom.payload {
                // Check if ref is valid
                let scopes = self.ast.scope(node.id);
                let mut found = false;
                for (_, children) in scopes.iter() {
                    for child_id in children {
                        if let Some(model_node) = self.ast.node(child_id)
                            && let Some(Model { name, .. }) = model_node.as_model()
                            && *name == ref_name
                        {
                            found = true;
                            break;
                        }
                    }
                    if found {
                        break;
                    }
                }

                if !found {
                    let mut candidates = Vec::new();
                    for (_, children) in scopes.iter() {
                        for child_id in children {
                            if let Some(model_node) = self.ast.node(child_id)
                                && let Some(Model { name, .. }) = model_node.as_model()
                            {
                                candidates.push(*name);
                            }
                        }
                    }

                    let mut help_msg = None;
                    let suggestions_vec = utils::fuzzy_match(ref_name, &candidates, 1);
                    if let Some((suggestion, score)) = suggestions_vec.first()
                        && *score > 50
                    {
                        help_msg = Some(format!("Did you mean {}?", format!("`{}`", suggestion).yellow()));
                    }
                    let report = self.diagnostic_ctx.error(node.span, format_args!("Undefined type reference `{}`", ref_name), help_msg);
                    errors.lock().unwrap().push(SemanticAnalyzerError::UndefinedTypeReference(report));
                }
            }
        }
    }
}
