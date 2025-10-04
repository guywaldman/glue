use colored::Colorize;

use crate::{
    AstNodePayload, LangError, Span,
    diagnostics::LangResult,
    parser::{Ast, AstNode, AstNodeId, AstNodeKind, AstSymbol, ParserArtifacts, SymbolTable, TreeNode, Type, TypeVariant},
    utils::fuzzy::fuzzy_match,
};

pub struct SemanticAnalyzer<'a> {
    file_name: &'a str,
    src: &'a str,
    ast: &'a Ast,
    symbols: &'a SymbolTable,
    artifacts: SemanticAnalysisArtifacts,
}

#[derive(Clone)]
pub struct SemanticAnalysisArtifacts {
    pub warnings: Vec<LangError>,
    pub ast: Ast,
    pub symbols: SymbolTable,
}

impl<'a> SemanticAnalyzer<'a> {
    pub fn new(file_name: &'a str, src: &'a str, parser_artifacts: &'a ParserArtifacts) -> Self {
        Self {
            file_name,
            src,
            ast: &parser_artifacts.ast,
            symbols: &parser_artifacts.symbols,
            artifacts: SemanticAnalysisArtifacts {
                warnings: Vec::new(),
                ast: parser_artifacts.ast.clone(),
                symbols: parser_artifacts.symbols.clone(),
            },
        }
    }

    // TODO: Return multiple errors
    pub fn analyze(mut self) -> LangResult<SemanticAnalysisArtifacts> {
        let root_id = self.ast.get_root();
        let Some(top_level_nodes) = self.ast.get_children(root_id) else {
            return Ok(self.artifacts);
        };

        for node in top_level_nodes {
            let node_kind = node.kind();
            let node_id = node.id();
            match node_kind {
                AstNodeKind::Model => {
                    self.analyze_model(&node)?;
                }
                AstNodeKind::Field => {
                    self.analyze_field(node_id)?;
                }
                _ => {}
            }
        }
        Ok(self.artifacts)
    }

    fn analyze_model(&mut self, model: &AstNode) -> LangResult {
        if model.kind() != AstNodeKind::Model {
            return Err(self.err(*model.span(), "Expected a model node".to_string(), None, Some("EInternal")));
        }
        let AstNodePayload::Model { name, .. } = &model.payload() else {
            return Err(self.err(*model.span(), "Expected a model node with payload".to_string(), None, Some("EInternal")));
        };
        // Model names must start with an uppercase letter
        let is_recommended_model_name = name.chars().next().map(|c| c.is_uppercase()).unwrap_or(false);
        if !is_recommended_model_name {
            self.warn(
                *model.span(),
                format!("Model name '{name}' should start with an uppercase letter"),
                Some("Consider renaming the model to start with an uppercase letter".to_string()),
                Some("WModelName"),
            );
        }

        let children = self.ast.get_children(model.id()).unwrap_or_default();
        let fields = children.iter().filter(|n| n.kind() == AstNodeKind::Field).collect::<Vec<_>>();
        for field in fields {
            self.analyze_field(field.id())?;
        }
        Ok(())
    }

    fn analyze_field(&self, node_id: AstNodeId) -> LangResult {
        let node = self
            .ast
            .get_node(node_id)
            .ok_or_else(|| self.err(Span::default(), format!("Field node with ID {node_id} not found"), None, Some("EInternal")))?;
        if node.kind() != AstNodeKind::Field {
            return Err(self.err(*node.span(), "Expected a field node".to_string(), None, Some("EInternal")));
        }
        let AstNodePayload::Field { ty, .. } = &node.payload() else {
            return Err(self.err(*node.span(), "Expected a field node with payload".to_string(), None, Some("EInternal")));
        };
        let type_refs = match ty {
            Type::Single(atom) => {
                if matches!(atom.variant, TypeVariant::Ref(_)) {
                    vec![atom]
                } else {
                    vec![]
                }
            }
            Type::Union(atoms) => atoms.iter().filter(|a| matches!(a.variant, TypeVariant::Ref(_))).collect::<Vec<_>>(),
        };
        // Determine scope: nearest ancestor model else root.
        let mut scope_id = self.ast.get_root();
        for ancestor_id in self.ast.get_ancestors(node_id) {
            if let Some(n) = self.ast.get_node(ancestor_id) {
                if n.kind() == AstNodeKind::Model {
                    scope_id = ancestor_id;
                    break;
                }
            }
        }
        let symbols_in_scope = self.symbols.symbols_in_scope(self.ast, scope_id);
        for (i, ty_ref) in type_refs.iter().enumerate() {
            if let TypeVariant::Ref(ref_name) = &ty_ref.variant {
                let is_defined = symbols_in_scope
                    .as_ref()
                    .map(|s| s.contains_key(&AstSymbol::Model(ref_name.clone())) || s.contains_key(&AstSymbol::Enum(ref_name.clone())))
                    .unwrap_or(false);
                let type_nodes = self.ast.get_children_fn(node_id, |n| n.kind() == AstNodeKind::Type).unwrap_or_default();
                let span = type_nodes.get(i).map(|n| *n.span()).unwrap_or_else(|| *node.span());
                if !is_defined {
                    let symbol_names = symbols_in_scope
                        .as_ref()
                        .map(|s| {
                            s.keys()
                                .filter_map(|sym| match sym {
                                    AstSymbol::Model(name) => Some(name.as_str()),
                                    AstSymbol::Enum(name) => Some(name.as_str()),
                                    _ => None,
                                })
                                .collect::<Vec<_>>()
                        })
                        .unwrap_or_default();
                    let similarity_scores = fuzzy_match(ref_name, &symbol_names, 5);
                    if let Some((best_match, score)) = similarity_scores.first() {
                        if *score > 50 {
                            return Err(self.err(
                                span,
                                format!("Undefined type reference '{ref_name}'"),
                                Some(format!("Did you mean '{}'?", best_match.yellow())),
                                Some("EUndefinedType"),
                            ));
                        } else {
                            // Similarity score too low, no good suggestion
                            return Err(self.err(
                                span,
                                format!("Undefined type reference '{ref_name}'"),
                                Some("Define the referenced model or correct the type name".to_string()),
                                Some("EUndefinedType"),
                            ));
                        }
                    } else {
                        return Err(self.err(
                            span,
                            format!("Undefined type reference '{ref_name}'"),
                            Some("Define the referenced model or correct the type name".to_string()),
                            Some("EUndefinedType"),
                        ));
                    }
                }
            }
        }
        Ok(())
    }

    fn err(&self, span: Span, msg: impl Into<String>, note: Option<String>, code: Option<&str>) -> Box<LangError> {
        Box::new(LangError::error(self.file_name, self.src, span, msg, note, code))
    }

    fn warn(&mut self, span: Span, msg: impl Into<String>, note: Option<String>, code: Option<&str>) {
        self.artifacts.warnings.push(LangError::warning(self.file_name, self.src, span, msg, note, code));
    }
}

#[cfg(test)]
mod tests {
    use crate::{Lexer, Parser};
    use indoc::indoc;

    use super::*;

    fn analyze(src: &str) -> LangResult<SemanticAnalysisArtifacts> {
        let tokens = Lexer::new(src).lex();
        let parser_artifacts = Parser::new("test.glue", src, &tokens).parse()?;
        let semantic = SemanticAnalyzer::new("test.glue", src, &parser_artifacts).analyze()?;
        Ok(semantic.clone())
    }

    #[test]
    fn test_defined_type() {
        let src = indoc! {r#"
            model Post {
                title: string
                status: Status
            }

            model Status {
                code: int
            }
        "#};
        let result = analyze(src);
        assert!(result.is_ok());
    }

    #[test]
    fn test_undefined_type() {
        let src = indoc! {r#"
            model Post {
                title: string
                status: Status
            }
        "#};
        let result = analyze(src);
        assert!(result.is_err());
        let err = result.err().unwrap();
        assert_eq!(err.message, "Undefined type reference 'Status'");
    }

    #[test]
    fn test_undefined_type_with_recommendation() {
        let src = indoc! {r#"
            model Post {
                title: string
                status: Statu
            }

            model Status {
                code: int
            }
        "#};
        let result = analyze(src);
        assert!(result.is_err());
        let err = result.err().unwrap();
        assert_eq!(err.message, "Undefined type reference 'Statu'");
        // Note contains ANSI color codes from .yellow()
        assert!(err.note.as_ref().unwrap().contains("Did you mean"));
        assert!(err.note.as_ref().unwrap().contains("Status"));
    }

    #[test]
    fn test_defined_model_type_nested() {
        let src = indoc! {r#"
            model Post {
                title: string
                details: PostDetails

                model PostDetails {
                    author: string
                }
            }
        "#};
        let result = analyze(src);
        assert!(result.is_ok());
    }

    #[test]
    fn test_defined_enum_type_nested() {
        let src = indoc! {r#"
            model Post {
                title: string
                status: PostStatus

                enum PostStatus = "draft" | "published" | "archived"
            }
        "#};
        let result = analyze(src);
        assert!(result.is_ok());
    }

    #[test]
    fn test_defined_enum_type_top_level() {
        let src = indoc! {r#"
            enum Tier = "free" | "pro" | "enterprise"
            model Post {
                title: string
                // One variation where the enum is at the bottom
                status: PostStatus

                // Another variation where the enum is at the top
                tier: Tier?
            }
            enum PostStatus = "draft" | "published" | "archived"

        "#};
        let result = analyze(src);
        assert!(result.is_ok());
    }
}
