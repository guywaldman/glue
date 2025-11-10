use std::{
    any::Any,
    sync::{Arc, Mutex},
};

use miette::Report;

use crate::{
    Decorator, DecoratorArg, Endpoint, EnumVariant, Literal, LiteralExpr, PrimitiveType, SourceCodeMetadata,
    builtin_decorators::{BUILTIN_DECORATORS, DecoratorDef},
    diagnostics::DiagnosticContext,
    symbols::{SymId, SymTable},
    syntax::{AstNode, Enum, Field, LNode, LNodeOrToken, LSyntaxKind, Model, ParsedProgram, Type, TypeAtom},
    utils::fuzzy_match,
};

#[derive(Debug)]
pub enum SemanticAnalyzerError {
    DuplicateField(Report),
    UndefinedTypeReference(Report),
}

impl SemanticAnalyzerError {
    pub fn report(&self) -> &miette::Report {
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
        let mut errors = Vec::new();

        let root = parsed.ast_root.clone();
        let targets: Vec<_> = root.children().filter(|n| n.kind() == LSyntaxKind::MODEL).map(|n| (n.kind(), n.text_range())).collect();
        let green_node = root.green();

        let symbols = Self::generate_symbol_table(parsed, &mut errors, diagnostic_ctx.clone());

        // TODO: Parallelize
        targets.iter().for_each(|&(kind, range)| {
            let local_root: LNode = rowan::SyntaxNode::new_root(green_node.clone().into());
            let element = local_root.covering_element(range);
            let node = element.into_node().expect("expected node at range");
            if kind == LSyntaxKind::MODEL {
                // TODO: Remove clone for symbols
                Self::check_model(node, &symbols, None, &mut errors, diagnostic_ctx.clone());
            }
        });

        if !errors.is_empty() { Err(errors) } else { Ok(AnalyzedProgram { ast_root: root, symbols }) }
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

        for nested_model_node in model.nested_model_nodes() {
            Self::check_model(nested_model_node, symbols, model_scope, errors, diag.clone());
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
                    match (primitive_type, default_value) {
                        (PrimitiveType::Bool, Literal::BoolLiteral { .. }) => {}
                        (PrimitiveType::Int, Literal::IntLiteral { .. }) => {}
                        (PrimitiveType::Float, Literal::FloatLiteral { .. }) => {}
                        (PrimitiveType::String, Literal::StringLiteral { .. }) => {}
                        _ => {
                            let report = diag.error(field_default_value_node.text_range(), "Type of default value does not match field type");
                            errors.push(SemanticAnalyzerError::DuplicateField(report));
                        }
                    }
                } else {
                    // Not a literal type - must be a ref
                    let ref_name = type_atom.as_ref_token().unwrap().text().to_string();
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

    fn check_type(node: LNode, symbols: &SymTable<LNode>, scope: Option<SymId>, errors: &mut Vec<SemanticAnalyzerError>, diag: DiagnosticContext) {
        let type_expr = Type::cast(node.clone()).unwrap();
        let type_atom_nodes = type_expr.type_atom_nodes();
        for type_atom_node in type_atom_nodes {
            let type_atom = TypeAtom::cast(type_atom_node.clone()).unwrap();
            if let Some(ident_token) = type_atom.as_ref_token() {
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
                        let report = diag.error_with_help(
                            ident_token.text_range(),
                            &format!("Undefined type reference '{}'", type_name),
                            &format!("Did you mean '{}'?", suggested_name.0),
                        );
                        errors.push(SemanticAnalyzerError::UndefinedTypeReference(report));
                        continue;
                    }
                    let report = diag.error(ident_token.text_range(), &format!("Undefined type reference '{}'", type_name));
                    errors.push(SemanticAnalyzerError::UndefinedTypeReference(report));
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
        for (idx, effective_arg_node) in effective_arg_nodes.iter().enumerate() {
            let effective_arg = DecoratorArg::cast(effective_arg_node.clone()).unwrap();
            let expected_arg_def = builtin_decorator.positional_args.get(idx);
            if let Some(expected_arg_def) = expected_arg_def {
                let literal_expr = effective_arg.literal_expr().unwrap();
                let literal_value = literal_expr.value().unwrap();
                if expected_arg_def.ty != literal_value.ty() {
                    let report = diag.error_with_help(
                        effective_arg_node.text_range(),
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
            if Self::generate_symbol_table_walk(child, &mut syms, None, errors, diag.clone()).is_err() {
                continue;
            }
        }

        syms
    }

    fn generate_symbol_table_walk(node: LNode, syms: &mut SymTable<LNode>, parent_scope: Option<SymId>, errors: &mut Vec<SemanticAnalyzerError>, diag: DiagnosticContext) -> Result<Option<SymId>, ()> {
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
                    let _ = Self::generate_symbol_table_walk(field_node, syms, Some(model_scope_id), errors, diag.clone());
                }
                for nested_model_node in model.nested_model_nodes() {
                    let _ = Self::generate_symbol_table_walk(nested_model_node, syms, Some(model_scope_id), errors, diag.clone());
                }
                for nested_enum_node in model.nested_enum_nodes() {
                    let _ = Self::generate_symbol_table_walk(nested_enum_node, syms, Some(model_scope_id), errors, diag.clone());
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
                for nested_model_node in endpoint.nested_model_nodes() {
                    let _ = Self::generate_symbol_table_walk(nested_model_node, syms, parent_scope, errors, diag.clone());
                }
                for nested_enum_node in endpoint.nested_enum_nodes() {
                    let _ = Self::generate_symbol_table_walk(nested_enum_node, syms, parent_scope, errors, diag.clone());
                }
                syms.add_to_scope(parent_scope, &endpoint_name, node);
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
            LSyntaxKind::FIELD => {
                let field = Field::cast(node.clone()).expect("Expected Field node");
                let ident_token = field.ident_node().expect("Expected field ident token");
                let field_name = match ident_token.clone() {
                    LNodeOrToken::Node(n) => n.text().to_string(),
                    LNodeOrToken::Token(tok) => tok.text().to_string(),
                };
                if syms.resolve(parent_scope, &field_name).is_some() {
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
}
