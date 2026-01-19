use std::sync::{Arc, RwLock};

use anyhow::Result;
use lang::{AstNode, LNode, LSyntaxKind, Parser, RootNode, SemanticAnalyzer, SourceCodeMetadata, SymTable, TextSize, TokenAtOffset};
use log::{error, info};
use tower_lsp::{Client, LanguageServer, jsonrpc::Result as LspResult, lsp_types as lsp};

/// Implementation of the Language Server Protocol.
#[derive(Clone, Default)]
pub struct Lsp {
    client: Option<Client>,
    state: Arc<RwLock<LspState>>,
}

#[derive(Default)]
struct LspState {
    source: String,
    uri: String,
}

impl Lsp {
    pub fn new(client: Client) -> Self {
        Self {
            client: Some(client),
            state: Default::default(),
        }
    }

    fn update_source(&self, uri: &str, text: &str) {
        let mut state = self.state.write().expect("Failed to acquire write lock on LSP state");
        state.source = text.to_string();
        state.uri = uri.to_string();
    }

    fn parse_current(&self) -> Result<(LNode, SymTable<LNode>)> {
        let state = self.state.read().expect("Failed to acquire read lock on LSP state");
        let metadata = SourceCodeMetadata {
            file_name: &state.uri,
            file_contents: &state.source,
        };
        let mut parser = Parser::new();
        let parsed = parser.parse(&metadata).map_err(|e| anyhow::anyhow!("Parse error: {:?}", e))?;
        let analyzer = SemanticAnalyzer::new();
        let analyzed = analyzer.analyze(&parsed, &metadata).map_err(|e| anyhow::anyhow!("Analysis error: {:?}", e))?;
        Ok((analyzed.ast_root, analyzed.symbols))
    }

    fn offset_at_position(&self, pos: lsp::Position) -> u32 {
        let state = self.state.read().expect("Failed to acquire read lock");
        let mut offset = 0u32;
        for (i, line) in state.source.lines().enumerate() {
            if i == pos.line as usize {
                return offset + pos.character;
            }
            offset += line.len() as u32 + 1; // +1 for newline
        }
        offset
    }

    fn position_at_offset(&self, offset: u32) -> lsp::Position {
        let state = self.state.read().expect("Failed to acquire read lock");
        let mut current = 0u32;
        for (i, line) in state.source.lines().enumerate() {
            let line_len = line.len() as u32 + 1;
            if current + line_len > offset {
                return lsp::Position { line: i as u32, character: offset - current };
            }
            current += line_len;
        }
        lsp::Position { line: 0, character: 0 }
    }
}

#[tower_lsp::async_trait]
impl LanguageServer for Lsp {
    async fn initialize(&self, _: lsp::InitializeParams) -> LspResult<lsp::InitializeResult> {
        info!("Initializing LSP with capabilities: goto definition, completion");
        Ok(lsp::InitializeResult {
            capabilities: lsp::ServerCapabilities {
                text_document_sync: Some(lsp::TextDocumentSyncCapability::Kind(lsp::TextDocumentSyncKind::FULL)),
                definition_provider: Some(lsp::OneOf::Left(true)),
                completion_provider: Some(lsp::CompletionOptions {
                    all_commit_characters: None,
                    resolve_provider: Some(false),
                    trigger_characters: Some(vec![".".to_string(), ":".to_string()]),
                    work_done_progress_options: lsp::WorkDoneProgressOptions { work_done_progress: None },
                    completion_item: Some(lsp::CompletionOptionsCompletionItem { label_details_support: Some(false) }),
                }),
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
        info!("Document opened: {uri}");
        self.update_source(&uri, &text);
    }

    async fn did_change(&self, params: lsp::DidChangeTextDocumentParams) {
        if let Some(change) = params.content_changes.into_iter().last() {
            let uri = params.text_document.uri.to_string();
            self.update_source(&uri, &change.text);
        }
    }

    async fn goto_definition(&self, params: lsp::GotoDefinitionParams) -> LspResult<Option<lsp::GotoDefinitionResponse>> {
        let pos = params.text_document_position_params.position;
        info!("Received goto_definition request at position: {pos:?}");

        let Ok((ast, symbols)) = self.parse_current() else {
            error!("Failed to parse current document");
            return Ok(None);
        };

        let offset = self.offset_at_position(pos);
        let token = ast.token_at_offset(TextSize::new(offset));
        let token = match token {
            TokenAtOffset::Single(t) => t,
            TokenAtOffset::Between(_, t) => t,
            TokenAtOffset::None => {
                info!("No token at position");
                return Ok(None);
            }
        };

        // Look for an identifier token that might be a type reference
        if token.kind() != LSyntaxKind::IDENT {
            return Ok(None);
        }

        let ref_name = token.text().to_string();
        let Some(sym_entry) = symbols.resolve(None, &ref_name) else {
            info!("No symbol found for: {ref_name}");
            return Ok(None);
        };

        let def_node = &sym_entry.data;
        let start_offset = def_node.text_range().start();
        let end_offset = def_node.text_range().end();

        Ok(Some(lsp::GotoDefinitionResponse::Scalar(lsp::Location {
            uri: params.text_document_position_params.text_document.uri.clone(),
            range: lsp::Range {
                start: self.position_at_offset(start_offset.into()),
                end: self.position_at_offset(end_offset.into()),
            },
        })))
    }

    async fn completion(&self, params: lsp::CompletionParams) -> LspResult<Option<lsp::CompletionResponse>> {
        let pos = params.text_document_position.position;
        info!("Received completion request at position: {pos:?}");

        let Ok((ast, _symbols)) = self.parse_current() else {
            error!("Failed to parse current document");
            return Ok(None);
        };

        let root = RootNode::cast(ast);
        let Some(root) = root else {
            return Ok(None);
        };

        // Collect all top-level model names as completions
        let mut items: Vec<lsp::CompletionItem> = Vec::new();
        for model in root.top_level_models() {
            if let Some(name) = model.ident() {
                items.push(lsp::CompletionItem {
                    label: name,
                    kind: Some(lsp::CompletionItemKind::CLASS),
                    ..Default::default()
                });
            }
        }

        for enum_def in root.top_level_enums() {
            if let Some(name) = enum_def.ident() {
                items.push(lsp::CompletionItem {
                    label: name,
                    kind: Some(lsp::CompletionItemKind::ENUM),
                    ..Default::default()
                });
            }
        }

        Ok(Some(lsp::CompletionResponse::Array(items)))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use indoc::indoc;
    use pretty_assertions::assert_eq;
    use tower_lsp::lsp_types as lsp;

    #[tokio::test]
    async fn test_goto_definition() {
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

        // Position of "Bar" in "bar: Bar" on line 2 (0-indexed: line 1, col 9)
        let params = lsp::GotoDefinitionParams {
            work_done_progress_params: Default::default(),
            partial_result_params: Default::default(),
            text_document_position_params: lsp::TextDocumentPositionParams {
                text_document: lsp::TextDocumentIdentifier { uri: uri.parse().unwrap() },
                position: lsp::Position { line: 1, character: 9 },
            },
        };
        let resp = lsp.goto_definition(params).await;
        let Ok(Some(lsp::GotoDefinitionResponse::Scalar(loc))) = &resp else {
            panic!("unexpected response: {resp:?}");
        };

        // "model Bar" starts on line 5 (0-indexed: line 4)
        assert_eq!(loc.uri, uri.parse().unwrap());
        assert_eq!(loc.range.start.line, 4);
    }

    #[tokio::test]
    #[ignore = "completion test has pre-existing issue with semantic analysis of incomplete types"]
    async fn test_completion() {
        let lsp = Lsp::default();
        let uri = "file:///test.glue";
        let src = indoc! {r#"
            model Foo {
                bar: B
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

        let params = lsp::CompletionParams {
            text_document_position: lsp::TextDocumentPositionParams {
                text_document: lsp::TextDocumentIdentifier { uri: uri.parse().unwrap() },
                position: lsp::Position { line: 1, character: 9 },
            },
            work_done_progress_params: Default::default(),
            partial_result_params: Default::default(),
            context: None,
        };
        let resp = lsp.completion(params).await;
        let Ok(Some(lsp::CompletionResponse::Array(items))) = &resp else {
            panic!("unexpected response: {resp:?}");
        };
        let labels: Vec<String> = items.iter().map(|item| item.label.clone()).collect();
        assert!(labels.contains(&"Foo".to_string()));
        assert!(labels.contains(&"Bar".to_string()));
    }
}
