use std::sync::{Arc, RwLock};

use anyhow::Result;
use gluelang::{Parser, SemanticAnalyzer, SourceCodeMetadata};
use log::{error, info};
use tower_lsp::{Client, LanguageServer, jsonrpc::Result as LspResult, lsp_types as lsp};

/// Minimal Language Server Protocol implementation.
#[derive(Clone, Default)]
pub struct Lsp {
    client: Option<Client>,
    state: Arc<RwLock<LspState>>,
}

#[derive(Default)]
struct LspState {
    last_uri: Option<String>,
}

impl Lsp {
    pub fn new(client: Client) -> Self {
        Self {
            client: Some(client),
            state: Default::default(),
        }
    }

    #[allow(clippy::collapsible_if)]
    fn analyze(&self, uri: &str, text: &str) -> Result<()> {
        info!("Analyzing document: {uri}, length: {}", text.len());
        let metadata = SourceCodeMetadata { file_name: uri, contents: text };
        match Parser::new(&metadata).parse() {
            Ok(parsed) => {
                if let Err(errors) = SemanticAnalyzer::new(&parsed.ast).check() {
                    if let Some(first) = errors.first() {
                        error!("Semantic analysis reported error: {}", first.report());
                    }
                }
            }
            Err(err) => {
                error!("Parse error: {err:?}");
            }
        }

        let mut state = self.state.write().expect("Failed to acquire write lock on LSP state");
        state.last_uri = Some(uri.to_string());

        Ok(())
    }
}

#[tower_lsp::async_trait]
impl LanguageServer for Lsp {
    async fn initialize(&self, _: lsp::InitializeParams) -> LspResult<lsp::InitializeResult> {
        info!("Initializing minimal LSP instance");
        Ok(lsp::InitializeResult {
            capabilities: lsp::ServerCapabilities {
                text_document_sync: Some(lsp::TextDocumentSyncCapability::Kind(lsp::TextDocumentSyncKind::FULL)),
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
            error!("Error analyzing document: {e}");
            if let Some(c) = &self.client {
                let _ = c.log_message(lsp::MessageType::ERROR, format!("Error analyzing document: {e}")).await;
            }
        }
    }

    async fn did_change(&self, params: lsp::DidChangeTextDocumentParams) {
        if let Some(change) = params.content_changes.into_iter().last() {
            let uri = params.text_document.uri.to_string();
            if let Err(e) = self.analyze(&uri, &change.text)
                && let Some(c) = &self.client
            {
                let _ = c.log_message(lsp::MessageType::ERROR, format!("Error analyzing document: {e}")).await;
            }
        }
    }

    async fn goto_definition(&self, _: lsp::GotoDefinitionParams) -> LspResult<Option<lsp::GotoDefinitionResponse>> {
        Ok(None)
    }

    async fn completion(&self, _: lsp::CompletionParams) -> LspResult<Option<lsp::CompletionResponse>> {
        Ok(None)
    }
}
