use std::sync::{Arc, RwLock};

use anyhow::Result;
use gluelang::{AstNodePayload, TreeNode, TypeVariant};
use log::{error, info};
use tower_lsp::{Client, LanguageServer, jsonrpc::Error as JsonRpcLspError, jsonrpc::Result as LspResult, lsp_types as lsp};

/// Implementation of the Language Server Protocol.
#[derive(Clone, Default)]
pub struct Lsp {
    client: Option<Client>,
    state: Arc<RwLock<LspState>>,
}

#[derive(Default)]
struct LspState {
    ast: Option<gluelang::Ast>,
    symbols: Option<gluelang::SymbolTable>,
}

impl Lsp {
    pub fn new(client: Client) -> Self {
        Self {
            client: Some(client),
            state: Default::default(),
        }
    }

    fn analyze(&self, uri: &str, text: &str) -> Result<()> {
        info!("Analyzing document: {uri}, length: {}", text.len());
        let tokens = gluelang::Lexer::new(text).lex();
        let parse_result = gluelang::Parser::new(uri, text, &tokens).parse()?;
        let analysis_result = gluelang::SemanticAnalyzer::new(uri, text, &parse_result).analyze()?;

        let mut state = self.state.write().expect("Failed to acquire write lock on LSP state");
        state.ast = Some(parse_result.ast);
        state.symbols = Some(analysis_result.symbols);

        Ok(())
    }
}

#[tower_lsp::async_trait]
impl LanguageServer for Lsp {
    async fn initialize(&self, _: lsp::InitializeParams) -> LspResult<lsp::InitializeResult> {
        info!("Initializing LSP with capabilities: hover, goto definition");
        Ok(lsp::InitializeResult {
            capabilities: lsp::ServerCapabilities {
                text_document_sync: Some(lsp::TextDocumentSyncCapability::Kind(lsp::TextDocumentSyncKind::FULL)),
                definition_provider: Some(lsp::OneOf::Left(true)),
                ..Default::default()
            },
            server_info: None,
        })
    }

    async fn initialized(&self, _: lsp::InitializedParams) {
        if let Some(c) = &self.client {
            let _ = c.log_message(lsp::MessageType::INFO, "glue LSP initialized").await;
        }
    }

    async fn shutdown(&self) -> LspResult<()> {
        Ok(())
    }

    async fn did_open(&self, params: lsp::DidOpenTextDocumentParams) {
        let uri = params.text_document.uri.to_string();
        let text = params.text_document.text;
        if let Err(e) = self.analyze(&uri, &text) {
            if let Some(c) = &self.client {
                let _ = c.log_message(lsp::MessageType::ERROR, format!("Error analyzing document: {e}")).await;
            }
        }
    }

    async fn did_change(&self, params: lsp::DidChangeTextDocumentParams) {
        // Full sync, single change with whole text
        if let Some(change) = params.content_changes.into_iter().last() {
            let uri = params.text_document.uri.to_string();
            if let Err(e) = self.analyze(&uri, &change.text) {
                if let Some(c) = &self.client {
                    let _ = c.log_message(lsp::MessageType::ERROR, format!("Error analyzing document: {e}")).await;
                }
            }
        }
    }

    async fn goto_definition(&self, params: lsp::GotoDefinitionParams) -> LspResult<Option<lsp::GotoDefinitionResponse>> {
        let pos = params.text_document_position_params.position;
        info!("Received goto_definition request at position: {pos:?}");
        let state = self.state.read().expect("Failed to acquire read lock on LSP state");
        let Some(ast) = &state.ast else {
            error!("No AST available");
            return Err(JsonRpcLspError::internal_error());
        };
        let Some(symbols) = &state.symbols else {
            error!("No symbol table available");
            return Err(JsonRpcLspError::internal_error());
        };

        let node = ast.find_narrowest_node_at_position((pos.line + 1) as usize, (pos.character + 1) as usize);
        let Some(node) = node else {
            info!("No AST node found at position");
            return Ok(None);
        };

        // If the node is a ref, find its definition.
        if let AstNodePayload::TypeAtom { ty } = node.payload() {
            let TypeVariant::Ref(ref_name) = &ty.variant else {
                info!("Type at position is not a reference");
                return Ok(None);
            };
            let Some(symbols_for_node) = symbols.symbols_in_scope(ast, node.id()) else {
                error!("No symbol table available for node");
                return Err(JsonRpcLspError::internal_error());
            };
            let def_node = symbols_for_node.iter().find(|sym| sym.0.name() == ref_name).iter().next().cloned();
            let Some((_, def_node_entry)) = def_node else {
                info!("No definition found for reference: {ref_name}");
                return Ok(None);
            };
            let def_node_id = def_node_entry.id;
            let Some(def_node) = ast.get_node(def_node_id) else {
                error!("Definition node not found in AST for id: {def_node_id}");
                return Err(JsonRpcLspError::internal_error());
            };

            return Ok(Some(lsp::GotoDefinitionResponse::Scalar(lsp::Location {
                uri: params.text_document_position_params.text_document.uri.clone(),
                range: lsp::Range {
                    start: lsp::Position {
                        line: (def_node.span().lines.0 - 1) as u32,     // LSP uses 0-indexed lines
                        character: (def_node.span().cols.0 - 1) as u32, // LSP uses 0-indexed columns
                    },
                    end: lsp::Position {
                        line: (def_node.span().lines.1 - 1) as u32,
                        character: (def_node.span().cols.1 - 1) as u32,
                    },
                },
            })));
        }

        Ok(None)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use gluelang::span_of;
    use indoc::indoc;
    use pretty_assertions::assert_eq;
    use tower_lsp::lsp_types as lsp;

    #[tokio::test]
    async fn test_goto_definition_uri() {
        let lsp = Lsp::default();
        let uri = "file:///test.glue";
        let src = indoc! {r#"
            model Foo {
                bar: Bar
            }

            model Bar {
                baz: string
            }
        "#}
        .trim();
        lsp.did_open(lsp::DidOpenTextDocumentParams {
            text_document: lsp::TextDocumentItem {
                uri: uri.parse().unwrap(),
                language_id: "glue".into(),
                version: 1,
                text: src.into(),
            },
        })
        .await;

        // Note: LSP uses 0-indexed lines and columns
        let bar_ref_span = span_of(src, "bar: (Bar)").unwrap();
        let params = lsp::GotoDefinitionParams {
            work_done_progress_params: Default::default(),
            partial_result_params: Default::default(),
            text_document_position_params: lsp::TextDocumentPositionParams {
                text_document: lsp::TextDocumentIdentifier { uri: uri.parse().unwrap() },
                position: lsp::Position {
                    line: (bar_ref_span.lines.0 - 1) as u32,
                    character: (bar_ref_span.cols.0 - 1) as u32,
                },
            },
        };
        let resp = lsp.goto_definition(params).await;
        let Ok(Some(lsp::GotoDefinitionResponse::Scalar(loc))) = &resp else {
            panic!("unexpected response: {resp:?}");
        };

        let bar_model_span = span_of(src, "model (Bar)").unwrap();
        assert_eq!(loc.uri, uri.parse().unwrap());
        assert_eq!(loc.range.start.line, bar_model_span.lines.0 as u32 - 1);
    }
}
