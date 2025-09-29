use gluelang::{Analyzer, Program, Span};
use log::info;
use lsp_types::{
    GotoDefinitionParams, GotoDefinitionResponse, Hover, HoverParams, InitializeParams, InitializeResult, Location, Position, Range,
    ServerCapabilities, TextDocumentPositionParams, TextDocumentSyncCapability, TextDocumentSyncKind,
};
use std::sync::{Arc, RwLock};
use tower_lsp::{Client, LanguageServer, LspService, Server};
use tower_lsp::{jsonrpc::Result, lsp_types};

#[derive(Default, Clone)]
struct DocState {
    uri: String,
    text: String,
    program: Option<Program>,
}

#[derive(Clone, Default)]
struct Backend {
    client: Option<Client>,
    state: Arc<RwLock<DocState>>,
}

impl Backend {
    fn analyze(&self, uri: &str, text: &str) {
        let mut st = self.state.write().unwrap();
        st.uri = uri.to_string();
        st.text = text.to_string();
        match Analyzer::new(uri, text).analyze() {
            Ok(p) => st.program = Some(p),
            Err(_e) => st.program = None,
        }
    }
}

#[tower_lsp::async_trait]
impl LanguageServer for Backend {
    async fn initialize(&self, _: InitializeParams) -> Result<InitializeResult> {
        info!("Initializing LSP with capabilities: hover, goto definition");
        Ok(InitializeResult {
            capabilities: ServerCapabilities {
                text_document_sync: Some(TextDocumentSyncCapability::Kind(TextDocumentSyncKind::FULL)),
                definition_provider: Some(lsp_types::OneOf::Left(true)),
                hover_provider: Some(lsp_types::HoverProviderCapability::Simple(true)),
                rename_provider: Some(lsp_types::OneOf::Right(lsp_types::RenameOptions {
                    prepare_provider: Some(false),
                    work_done_progress_options: lsp_types::WorkDoneProgressOptions { work_done_progress: None },
                })),
                ..Default::default()
            },
            server_info: None,
        })
    }

    async fn initialized(&self, _: lsp_types::InitializedParams) {
        if let Some(c) = &self.client {
            let _ = c.log_message(lsp_types::MessageType::INFO, "glue LSP initialized").await;
        }
    }

    async fn shutdown(&self) -> Result<()> {
        Ok(())
    }

    async fn did_open(&self, params: lsp_types::DidOpenTextDocumentParams) {
        let uri = params.text_document.uri.to_string();
        let text = params.text_document.text;
        self.analyze(&uri, &text);
    }

    async fn did_change(&self, params: lsp_types::DidChangeTextDocumentParams) {
        // FULL sync, single change with whole text
        if let Some(change) = params.content_changes.into_iter().last() {
            let uri = params.text_document.uri.to_string();
            self.analyze(&uri, &change.text);
        }
    }

    async fn goto_definition(&self, params: GotoDefinitionParams) -> Result<Option<GotoDefinitionResponse>> {
        // Naive: if position is inside a model name or field type ref (#Name), jump to model def
        let st = self.state.read().unwrap();
        let program = match &st.program {
            Some(p) => p,
            None => return Ok(None),
        };
        let TextDocumentPositionParams { position, .. } = params.text_document_position_params;
        let line = position.line as usize + 1; // lexer lines are 1-based
        let col = position.character as usize + 1;

        // Find word under cursor (simple scan on the source text)
        let src = &st.text;
        let mut byte_index = 0usize;
        let mut cur_line = 1usize;
        let mut cur_col = 1usize;
        for (i, ch) in src.char_indices() {
            if cur_line == line && cur_col == col {
                byte_index = i;
                break;
            }
            if ch == '\n' {
                cur_line += 1;
                cur_col = 1;
            } else {
                cur_col += 1;
            }
        }
        // expand to identifier
        let bytes = src.as_bytes();
        let mut start = byte_index;
        while start > 0 && is_ident(bytes[start - 1]) {
            start -= 1;
        }
        let mut end = byte_index;
        while end < bytes.len() && is_ident(bytes[end]) {
            end += 1;
        }
        if start == end {
            return Ok(None);
        }
        let ident = &src[start..end];

        // Look for model by name
        if let Some(m) = program.models.iter().find(|m| m.name == ident) {
            let loc = span_to_location(&m.span, src, &st.uri);
            return Ok(Some(GotoDefinitionResponse::Scalar(loc)));
        }
        Ok(None)
    }

    async fn hover(&self, params: HoverParams) -> Result<Option<Hover>> {
        info!("Received hover request: {params:?}");

        let st = self.state.read().unwrap();
        let program = match &st.program {
            Some(p) => p,
            None => return Ok(None),
        };
        let TextDocumentPositionParams { position, .. } = params.text_document_position_params;
        let line = position.line as usize + 1;
        let col = position.character as usize + 1;

        // Find token under cursor (simple scan on the source text)
        // TODO: Optimize - this is horribly inefficient and unmaintainable.
        for model in &program.models {
            if pos_within_span(line, col, &model.span) {
                let contents = format!(
                    "{}\n```glue\nmodel {} {{ ... }}\n```",
                    model.doc.as_ref().unwrap_or(&"".into()),
                    model.name
                );
                let hover = Hover {
                    contents: lsp_types::HoverContents::Scalar(lsp_types::MarkedString::String(contents)),
                    range: Some(Range {
                        start: Position {
                            line: (model.span.line - 1) as u32,
                            character: (model.span.col_start - 1) as u32,
                        },
                        end: Position {
                            line: (model.span.line - 1) as u32,
                            character: (model.span.col_end) as u32,
                        },
                    }),
                };
                return Ok(Some(hover));
            }
            for field in &model.fields {
                if pos_within_span(line, col, &field.span) {
                    let contents = format!("```glue\n{}: {}\n```", field.name, field.ty);
                    let hover = Hover {
                        contents: lsp_types::HoverContents::Scalar(lsp_types::MarkedString::String(contents)),
                        range: Some(Range {
                            start: Position {
                                line: (field.span.line - 1) as u32,
                                character: (field.span.col_start - 1) as u32,
                            },
                            end: Position {
                                line: (field.span.line - 1) as u32,
                                character: (field.span.col_end) as u32,
                            },
                        }),
                    };
                    return Ok(Some(hover));
                }

                for ty in &field.ty.atoms {
                    // TODO: Optimize.
                    if ty.is_ref && pos_within_span(line, col, &ty.span) {
                        // Lookup model
                        let contents = if let Some(m) = program.models.iter().find(|m| m.name == ty.name) {
                            format!("{}\n```glue\nmodel {} {{ ... }}\n```", m.doc.as_ref().unwrap_or(&"".into()), m.name)
                        } else {
                            format!("(unknown model)```glue\n#{}\n```", ty.name)
                        };
                        let hover = Hover {
                            contents: lsp_types::HoverContents::Scalar(lsp_types::MarkedString::String(contents)),
                            range: Some(Range {
                                start: Position {
                                    line: (ty.span.line - 1) as u32,
                                    character: (ty.span.col_start - 1) as u32,
                                },
                                end: Position {
                                    line: (ty.span.line - 1) as u32,
                                    character: (ty.span.col_end) as u32,
                                },
                            }),
                        };
                        return Ok(Some(hover));
                    } else {
                        // Not a ref type, no hover
                        continue;
                    }
                }
            }
        }
        Ok(None)
    }

    async fn rename(&self, params: lsp_types::RenameParams) -> Result<Option<lsp_types::WorkspaceEdit>> {
        // NOTE: Only models are supported for now.

        let st = self.state.read().unwrap();
        let program = match &st.program {
            Some(p) => p,
            None => return Ok(None),
        };
        let TextDocumentPositionParams { position, .. } = params.text_document_position;
        let new_name = params.new_name;

        // Refactor model names on the model definitions.
        for model in &program.models {
            if pos_within_span(position.line as usize + 1, position.character as usize + 1, &model.span) {
                // Found model to rename
                let mut changes = std::collections::HashMap::new();

                // TODO: Optimize.
                // Very inefficient - we go over ALL fields of ALL models to find references.
                let mut edits = vec![];
                for m in &program.models {
                    for f in &m.fields {
                        for ty in &f.ty.atoms {
                            if ty.is_ref && ty.name == model.name {
                                let start_line = ty.span.line - 1;
                                let mut start_char = ty.span.col_start - 1;
                                let end_line = ty.span.line - 1;
                                let mut end_char = ty.span.col_end;
                                if ty.is_array {
                                    start_char += 1; // skip '['
                                    end_char -= 1; // skip ']'
                                }
                                edits.push(lsp_types::TextEdit {
                                    range: Range {
                                        start: Position {
                                            line: start_line as u32,
                                            character: start_char as u32,
                                        },
                                        end: Position {
                                            line: end_line as u32,
                                            character: end_char as u32,
                                        },
                                    },
                                    new_text: new_name.clone(),
                                });
                            }
                        }
                    }
                }
                // Also rename the model definition itself
                // Adjust the rest of the row on the right, since the new name may be longer.
                let entire_line = st.text.lines().nth(model.span.line - 1).unwrap_or_default().to_string();
                let adjusted_line = format!(
                    "{}{} {}",
                    &entire_line[..model.span.col_start - 1],
                    new_name,
                    &entire_line[model.span.col_end..]
                );

                edits.push(lsp_types::TextEdit {
                    range: Range {
                        start: Position {
                            line: (model.span.line - 1) as u32,
                            character: 0,
                        },
                        end: Position {
                            line: (model.span.line - 1) as u32,
                            character: entire_line.len() as u32,
                        },
                    },
                    new_text: adjusted_line,
                });
                changes.insert(lsp_types::Url::parse(&st.uri).unwrap(), edits);
                let workspace_edit = lsp_types::WorkspaceEdit {
                    changes: Some(changes),
                    document_changes: None,
                    change_annotations: None,
                };
                return Ok(Some(workspace_edit));
            }
        }

        // TODO: Refactor model names from the field types.

        Ok(None)
    }
}

fn is_ident(b: u8) -> bool {
    b.is_ascii_alphanumeric() || b == b'_'
}

#[inline]
fn pos_within_span(line: usize, col: usize, span: &Span) -> bool {
    // Assume single line
    line == span.line && col >= span.col_start && col <= span.col_end
}

fn span_to_location(span: &Span, src: &str, uri: &str) -> Location {
    // Convert byte offsets to line/col. LSP is 0-based for lines and columns, surprisingly.
    let mut line = 0u32;
    let mut col = 0u32;
    let mut start_pos = None;
    let mut end_pos = None;
    for (i, ch) in src.char_indices() {
        if i == span.start {
            start_pos = Some((line, col));
        }
        if i == span.end {
            end_pos = Some((line, col));
            break;
        }
        if ch == '\n' {
            line += 1;
            col = 0;
        } else {
            col += 1;
        }
    }
    let (sl, sc) = start_pos.unwrap_or((0, 0));
    let (el, ec) = end_pos.unwrap_or((sl, sc + 1));
    Location {
        uri: lsp_types::Url::parse(uri).unwrap_or_else(|_| lsp_types::Url::parse("file://invalid").unwrap()),
        range: Range {
            start: Position { line: sl, character: sc },
            end: Position { line: el, character: ec },
        },
    }
}

#[tokio::main]
async fn main() {
    let (service, socket) = LspService::build(|client| Backend {
        client: Some(client),
        ..Default::default()
    })
    .finish();
    Server::new(tokio::io::stdin(), tokio::io::stdout(), socket).serve(service).await;
}

#[cfg(test)]
mod tests {
    use super::*;
    use lsp_types::{
        DidOpenTextDocumentParams, GotoDefinitionParams, TextDocumentIdentifier, TextDocumentItem, TextDocumentPositionParams,
    };

    #[tokio::test]
    async fn test_goto_definition_uri() {
        let backend = Backend::default();
        let uri = "file:///test.glue";
        let src = "model User { id: int }";
        backend
            .did_open(DidOpenTextDocumentParams {
                text_document: TextDocumentItem {
                    uri: uri.parse().unwrap(),
                    language_id: "glue".into(),
                    version: 1,
                    text: src.into(),
                },
            })
            .await;

        let name_offset = src.find("User").unwrap();
        // compute line/char (0-based)
        let mut line = 0u32;
        let mut col = 0u32;
        let mut target_line = 0u32;
        let mut target_col = 0u32;
        for (i, ch) in src.char_indices() {
            if i == name_offset {
                target_line = line;
                target_col = col;
                break;
            }
            if ch == '\n' {
                line += 1;
                col = 0;
            } else {
                col += 1;
            }
        }
        let params = GotoDefinitionParams {
            work_done_progress_params: Default::default(),
            partial_result_params: Default::default(),
            text_document_position_params: TextDocumentPositionParams {
                text_document: TextDocumentIdentifier { uri: uri.parse().unwrap() },
                position: Position {
                    line: target_line,
                    character: target_col,
                },
            },
        };
        let resp = backend.goto_definition(params).await.unwrap();
        match resp {
            Some(GotoDefinitionResponse::Scalar(loc)) => assert_eq!(loc.uri.as_str(), uri),
            _ => panic!("unexpected response: {resp:?}"),
        }
    }
}
