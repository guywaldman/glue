use std::collections::HashMap;
use std::sync::{Arc, RwLock};

use anyhow::Result;
use lang::{AstNode, Enum, Field, LNode, LSyntaxKind, Model, Parser, RootNode, SemanticAnalyzer, SourceCodeMetadata, SymTable, TextSize, TokenAtOffset, Type};
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
    /// Map of URI to document content
    documents: HashMap<String, String>,
}

/// Information about a symbol for hover display
struct SymbolInfo {
    kind: &'static str,
    name: String,
    docs: Option<String>,
    type_info: Option<String>,
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
        state.documents.insert(uri.to_string(), text.to_string());
    }

    fn parse_document(&self, uri: &str) -> Result<(LNode, SymTable<LNode>)> {
        let state = self.state.read().expect("Failed to acquire read lock on LSP state");
        let source = state.documents.get(uri).ok_or_else(|| anyhow::anyhow!("Document not found: {uri}"))?;
        let metadata = SourceCodeMetadata {
            file_name: uri,
            file_contents: source,
        };
        let mut parser = Parser::new();
        let parsed = parser.parse(&metadata).map_err(|e| anyhow::anyhow!("Parse error: {:?}", e))?;
        let analyzer = SemanticAnalyzer::new();
        let analyzed = analyzer.analyze(&parsed, &metadata).map_err(|e| anyhow::anyhow!("Analysis error: {:?}", e))?;
        Ok((analyzed.ast_root, analyzed.symbols))
    }

    fn offset_at_position(&self, uri: &str, pos: lsp::Position) -> u32 {
        let state = self.state.read().expect("Failed to acquire read lock");
        let source = match state.documents.get(uri) {
            Some(s) => s,
            None => return 0,
        };
        let mut offset = 0u32;
        for (i, line) in source.lines().enumerate() {
            if i == pos.line as usize {
                return offset + pos.character;
            }
            offset += line.len() as u32 + 1; // +1 for newline
        }
        offset
    }

    fn position_at_offset(&self, uri: &str, offset: u32) -> lsp::Position {
        let state = self.state.read().expect("Failed to acquire read lock");
        let source = match state.documents.get(uri) {
            Some(s) => s,
            None => return lsp::Position { line: 0, character: 0 },
        };
        let mut current = 0u32;
        for (i, line) in source.lines().enumerate() {
            let line_len = line.len() as u32 + 1;
            if current + line_len > offset {
                return lsp::Position {
                    line: i as u32,
                    character: offset - current,
                };
            }
            current += line_len;
        }
        lsp::Position { line: 0, character: 0 }
    }

    /// Find the enclosing scope (model/endpoint) for a given offset
    fn find_scope_at_offset(&self, ast: &LNode, offset: u32, symbols: &lang::SymTable<LNode>) -> Option<lang::SymId> {
        // Get token at offset and walk up to find enclosing model
        let token = ast.token_at_offset(TextSize::new(offset));
        let token = match token {
            TokenAtOffset::Single(t) => t,
            TokenAtOffset::Between(_, t) => t,
            TokenAtOffset::None => return None,
        };

        // Walk up the parent chain to find enclosing MODEL
        let mut current = token.parent();
        while let Some(node) = current {
            if node.kind() == LSyntaxKind::MODEL {
                // Found enclosing model - get its name and resolve to SymId
                if let Some(model) = Model::cast(node.clone()) {
                    if let Some(name) = model.ident() {
                        // Try to find this model in the symbol table
                        // We need to resolve it from global scope first
                        return symbols.resolve_id(None, &name);
                    }
                }
            }
            current = node.parent();
        }
        None
    }

    /// Extract symbol information from a node
    fn extract_symbol_info(&self, node: &LNode) -> Option<SymbolInfo> {
        match node.kind() {
            LSyntaxKind::MODEL => {
                let model = Model::cast(node.clone())?;
                let name = model.ident()?;
                let docs = model.docs().map(|d| d.join("\n"));
                let field_count = model.fields().len();
                let type_info = Some(format!("model with {} field(s)", field_count));
                Some(SymbolInfo { kind: "model", name, docs, type_info })
            }
            LSyntaxKind::ENUM => {
                let enum_ = Enum::cast(node.clone())?;
                let name = enum_.ident()?;
                let docs = enum_.docs().map(|d| d.join("\n"));
                let variants: Vec<_> = enum_.variants().iter().filter_map(|v| v.value()).collect();
                let type_info = Some(format!("enum: {}", variants.join(" | ")));
                Some(SymbolInfo { kind: "enum", name, docs, type_info })
            }
            LSyntaxKind::FIELD => {
                let field = Field::cast(node.clone())?;
                let name = field.ident()?;
                let docs = field.docs().map(|d| d.join("\n"));
                let type_info = field.ty().map(|t| self.format_type(&t));
                Some(SymbolInfo { kind: "field", name, docs, type_info })
            }
            _ => None,
        }
    }

    /// Format a type for display
    fn format_type(&self, ty: &Type) -> String {
        // Get the text representation from the type node
        ty.syntax().text().to_string()
    }

    /// Generate markdown hover content for a symbol
    fn generate_hover_content(&self, info: &SymbolInfo) -> String {
        let mut parts = Vec::new();

        // Code block with type info
        let header = if let Some(ref type_info) = info.type_info {
            format!("```glue\n{} {} // {}\n```", info.kind, info.name, type_info)
        } else {
            format!("```glue\n{} {}\n```", info.kind, info.name)
        };
        parts.push(header);

        // Documentation
        if let Some(ref docs) = info.docs {
            parts.push("---".to_string());
            parts.push(docs.clone());
        }

        parts.join("\n\n")
    }
}

#[tower_lsp::async_trait]
impl LanguageServer for Lsp {
    async fn initialize(&self, _: lsp::InitializeParams) -> LspResult<lsp::InitializeResult> {
        info!("Initializing LSP with capabilities: goto definition, hover, completion");
        Ok(lsp::InitializeResult {
            capabilities: lsp::ServerCapabilities {
                text_document_sync: Some(lsp::TextDocumentSyncCapability::Kind(lsp::TextDocumentSyncKind::FULL)),
                definition_provider: Some(lsp::OneOf::Left(true)),
                hover_provider: Some(lsp::HoverProviderCapability::Simple(true)),
                completion_provider: Some(lsp::CompletionOptions {
                    all_commit_characters: None,
                    resolve_provider: Some(false),
                    trigger_characters: Some(vec![".".to_string(), ":".to_string()]),
                    work_done_progress_options: lsp::WorkDoneProgressOptions { work_done_progress: None },
                    completion_item: Some(lsp::CompletionOptionsCompletionItem { label_details_support: Some(false) }),
                }),
                ..Default::default()
            },
            server_info: Some(lsp::ServerInfo {
                name: "Glue Language Server".to_string(),
                version: Some(env!("CARGO_PKG_VERSION").to_string()),
            }),
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
        let uri = params.text_document_position_params.text_document.uri.to_string();
        let pos = params.text_document_position_params.position;
        info!("Received goto_definition request at position: {pos:?} in {uri}");

        let Ok((ast, symbols)) = self.parse_document(&uri) else {
            error!("Failed to parse document: {uri}");
            return Ok(None);
        };

        let offset = self.offset_at_position(&uri, pos);
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
        info!("Looking for definition of: {ref_name}");

        // Find the enclosing scope to resolve nested types
        let scope = self.find_scope_at_offset(&ast, offset, &symbols);
        info!("Enclosing scope: {scope:?}");

        let Some(sym_entry) = symbols.resolve(scope, &ref_name) else {
            info!("No symbol found for: {ref_name}");
            return Ok(None);
        };

        let def_node = &sym_entry.data;
        let start_offset = def_node.text_range().start();
        let end_offset = def_node.text_range().end();

        Ok(Some(lsp::GotoDefinitionResponse::Scalar(lsp::Location {
            uri: params.text_document_position_params.text_document.uri.clone(),
            range: lsp::Range {
                start: self.position_at_offset(&uri, start_offset.into()),
                end: self.position_at_offset(&uri, end_offset.into()),
            },
        })))
    }

    async fn hover(&self, params: lsp::HoverParams) -> LspResult<Option<lsp::Hover>> {
        let uri = params.text_document_position_params.text_document.uri.to_string();
        let pos = params.text_document_position_params.position;
        info!("Received hover request at position: {pos:?} in {uri}");

        let Ok((ast, symbols)) = self.parse_document(&uri) else {
            error!("Failed to parse document: {uri}");
            return Ok(None);
        };

        let offset = self.offset_at_position(&uri, pos);
        let token = ast.token_at_offset(TextSize::new(offset));
        let token = match token {
            TokenAtOffset::Single(t) => t,
            TokenAtOffset::Between(_, t) => t,
            TokenAtOffset::None => {
                info!("No token at hover position");
                return Ok(None);
            }
        };

        // Only handle identifiers
        if token.kind() != LSyntaxKind::IDENT {
            return Ok(None);
        }

        let ref_name = token.text().to_string();
        info!("Hover on identifier: {ref_name}");

        // Try to resolve the symbol with scope awareness
        let scope = self.find_scope_at_offset(&ast, offset, &symbols);
        let Some(sym_entry) = symbols.resolve(scope, &ref_name) else {
            info!("No symbol found for hover: {ref_name}");
            return Ok(None);
        };

        // Extract symbol info from the definition node
        let Some(info) = self.extract_symbol_info(&sym_entry.data) else {
            info!("Could not extract symbol info for: {ref_name}");
            return Ok(None);
        };

        let content = self.generate_hover_content(&info);
        let token_start = token.text_range().start();
        let token_end = token.text_range().end();

        Ok(Some(lsp::Hover {
            contents: lsp::HoverContents::Markup(lsp::MarkupContent {
                kind: lsp::MarkupKind::Markdown,
                value: content,
            }),
            range: Some(lsp::Range {
                start: self.position_at_offset(&uri, token_start.into()),
                end: self.position_at_offset(&uri, token_end.into()),
            }),
        }))
    }

    async fn completion(&self, params: lsp::CompletionParams) -> LspResult<Option<lsp::CompletionResponse>> {
        let uri = params.text_document_position.text_document.uri.to_string();
        let pos = params.text_document_position.position;
        info!("Received completion request at position: {pos:?} in {uri}");

        let Ok((ast, _symbols)) = self.parse_document(&uri) else {
            error!("Failed to parse document: {uri}");
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

    /// Helper to create an LSP instance with a document already opened
    async fn setup_lsp(src: &str) -> (Lsp, String) {
        let lsp = Lsp::default();
        let uri = "file:///test.glue";
        lsp.did_open(lsp::DidOpenTextDocumentParams {
            text_document: lsp::TextDocumentItem {
                uri: uri.parse().unwrap(),
                language_id: "glue".into(),
                version: 1,
                text: src.into(),
            },
        })
        .await;
        (lsp, uri.to_string())
    }

    #[tokio::test]
    async fn test_goto_definition_model_reference() {
        let src = indoc! {r#"
            model Foo {
                bar: Bar
            }

            model Bar {
                baz: string
            }
        "#}
        .trim();

        let (lsp, uri) = setup_lsp(src).await;

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
    async fn test_goto_definition_enum_reference() {
        let src = indoc! {r#"
            model Config {
                mode: Mode
            }

            enum Mode: "fast" | "slow"
        "#}
        .trim();

        let (lsp, uri) = setup_lsp(src).await;

        // Position of "Mode" in "mode: Mode" on line 2 (0-indexed: line 1, col 10)
        let params = lsp::GotoDefinitionParams {
            work_done_progress_params: Default::default(),
            partial_result_params: Default::default(),
            text_document_position_params: lsp::TextDocumentPositionParams {
                text_document: lsp::TextDocumentIdentifier { uri: uri.parse().unwrap() },
                position: lsp::Position { line: 1, character: 10 },
            },
        };
        let resp = lsp.goto_definition(params).await;
        let Ok(Some(lsp::GotoDefinitionResponse::Scalar(loc))) = &resp else {
            panic!("unexpected response: {resp:?}");
        };

        // "enum Mode" starts on line 5 (0-indexed: line 4)
        assert_eq!(loc.uri, uri.parse().unwrap());
        assert_eq!(loc.range.start.line, 4);
    }

    #[tokio::test]
    async fn test_goto_definition_nested_model() {
        let src = indoc! {r#"
            model Item {
                raw_data: RawData

                model RawData {
                    scanned: string
                }
            }
        "#}
        .trim();

        let (lsp, uri) = setup_lsp(src).await;

        // Position of "RawData" in "raw_data: RawData" on line 2 (0-indexed: line 1, col 14)
        let params = lsp::GotoDefinitionParams {
            work_done_progress_params: Default::default(),
            partial_result_params: Default::default(),
            text_document_position_params: lsp::TextDocumentPositionParams {
                text_document: lsp::TextDocumentIdentifier { uri: uri.parse().unwrap() },
                position: lsp::Position { line: 1, character: 14 },
            },
        };
        let resp = lsp.goto_definition(params).await;
        let Ok(Some(lsp::GotoDefinitionResponse::Scalar(loc))) = &resp else {
            panic!("unexpected response: {resp:?}");
        };

        // "model RawData" starts on line 4 (0-indexed: line 3)
        assert_eq!(loc.uri, uri.parse().unwrap());
        assert_eq!(loc.range.start.line, 3, "should jump to nested model definition");
    }

    #[tokio::test]
    async fn test_hover_on_model_reference() {
        let src = indoc! {r#"
            model Foo {
                bar: Bar
            }

            /// This is the Bar model
            /// with multiple doc lines
            model Bar {
                baz: string
            }
        "#}
        .trim();

        let (lsp, uri) = setup_lsp(src).await;

        // Hover on "Bar" in "bar: Bar"
        let params = lsp::HoverParams {
            work_done_progress_params: Default::default(),
            text_document_position_params: lsp::TextDocumentPositionParams {
                text_document: lsp::TextDocumentIdentifier { uri: uri.parse().unwrap() },
                position: lsp::Position { line: 1, character: 9 },
            },
        };
        let resp = lsp.hover(params).await;
        let Ok(Some(hover)) = &resp else {
            panic!("expected hover response, got: {resp:?}");
        };

        let lsp::HoverContents::Markup(markup) = &hover.contents else {
            panic!("expected markup content");
        };

        assert_eq!(markup.kind, lsp::MarkupKind::Markdown);
        assert!(markup.value.contains("model Bar"), "should contain model name");
        assert!(markup.value.contains("This is the Bar model"), "should contain docs");
    }

    #[tokio::test]
    async fn test_hover_on_enum_reference() {
        let src = indoc! {r#"
            model Config {
                mode: Mode
            }

            /// Operating mode
            enum Mode: "fast" | "slow"
        "#}
        .trim();

        let (lsp, uri) = setup_lsp(src).await;

        // Hover on "Mode" in "mode: Mode"
        let params = lsp::HoverParams {
            work_done_progress_params: Default::default(),
            text_document_position_params: lsp::TextDocumentPositionParams {
                text_document: lsp::TextDocumentIdentifier { uri: uri.parse().unwrap() },
                position: lsp::Position { line: 1, character: 10 },
            },
        };
        let resp = lsp.hover(params).await;
        let Ok(Some(hover)) = &resp else {
            panic!("expected hover response, got: {resp:?}");
        };

        let lsp::HoverContents::Markup(markup) = &hover.contents else {
            panic!("expected markup content");
        };

        assert!(markup.value.contains("enum Mode"), "should contain enum name");
        assert!(markup.value.contains("fast"), "should contain variant");
        assert!(markup.value.contains("slow"), "should contain variant");
        assert!(markup.value.contains("Operating mode"), "should contain docs");
    }

    #[tokio::test]
    async fn test_hover_returns_none_for_primitive() {
        let src = indoc! {r#"
            model Foo {
                name: string
            }
        "#}
        .trim();

        let (lsp, uri) = setup_lsp(src).await;

        // Hover on "string" - should return None (primitive type)
        let params = lsp::HoverParams {
            work_done_progress_params: Default::default(),
            text_document_position_params: lsp::TextDocumentPositionParams {
                text_document: lsp::TextDocumentIdentifier { uri: uri.parse().unwrap() },
                position: lsp::Position { line: 1, character: 10 },
            },
        };
        let resp = lsp.hover(params).await;
        // Primitive types are not in the symbol table, so should return None
        assert!(resp.is_ok());
    }

    #[tokio::test]
    async fn test_completion_returns_models_and_enums() {
        let src = indoc! {r#"
            model User {
                name: string
            }

            model Post {
                title: string
            }

            enum Status: "draft" | "published"
        "#}
        .trim();

        let (lsp, uri) = setup_lsp(src).await;

        let params = lsp::CompletionParams {
            text_document_position: lsp::TextDocumentPositionParams {
                text_document: lsp::TextDocumentIdentifier { uri: uri.parse().unwrap() },
                position: lsp::Position { line: 0, character: 0 },
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
        assert!(labels.contains(&"User".to_string()), "should contain User model");
        assert!(labels.contains(&"Post".to_string()), "should contain Post model");
        assert!(labels.contains(&"Status".to_string()), "should contain Status enum");
    }

    #[tokio::test]
    async fn test_did_change_updates_source() {
        let (lsp, uri) = setup_lsp("model Foo { x: int }").await;

        // Change the document
        lsp.did_change(lsp::DidChangeTextDocumentParams {
            text_document: lsp::VersionedTextDocumentIdentifier {
                uri: uri.parse().unwrap(),
                version: 2,
            },
            content_changes: vec![lsp::TextDocumentContentChangeEvent {
                range: None,
                range_length: None,
                text: "model Bar { y: string }".into(),
            }],
        })
        .await;

        // Verify completion now shows Bar instead of Foo
        let params = lsp::CompletionParams {
            text_document_position: lsp::TextDocumentPositionParams {
                text_document: lsp::TextDocumentIdentifier { uri: uri.parse().unwrap() },
                position: lsp::Position { line: 0, character: 0 },
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
        assert!(labels.contains(&"Bar".to_string()), "should contain Bar after update");
        assert!(!labels.contains(&"Foo".to_string()), "should not contain Foo after update");
    }
}
