use std::collections::{HashMap, HashSet};
use std::path::{Path, PathBuf};
use std::sync::{Arc, RwLock};

use anyhow::Result;
use lang::BUILTIN_DECORATORS;
use lang::{
    AstNode, Decorator, DecoratorArgDef, DecoratorDef, Endpoint, Enum, Field, LNode, LSyntaxKind, Model, Parser, RootNode, SemanticAnalyzer, SemanticAnalyzerError, SourceCodeMetadata, SymTable,
    TextSize, TokenAtOffset, Type,
};
use log::{error, info};
use miette::LabeledSpan;
use miette::Severity as MietteSeverity;
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

    fn remove_source(&self, uri: &str) {
        let mut state = self.state.write().expect("Failed to acquire write lock on LSP state");
        state.documents.remove(uri);
    }

    fn map_severity(severity: Option<MietteSeverity>) -> lsp::DiagnosticSeverity {
        match severity {
            Some(MietteSeverity::Error) => lsp::DiagnosticSeverity::ERROR,
            Some(MietteSeverity::Warning) => lsp::DiagnosticSeverity::WARNING,
            Some(MietteSeverity::Advice) => lsp::DiagnosticSeverity::INFORMATION,
            None => lsp::DiagnosticSeverity::ERROR,
        }
    }

    fn diagnostics_from_report(&self, uri: &str, report: &miette::Report) -> Vec<lsp::Diagnostic> {
        let severity = Self::map_severity(report.severity());
        let message = report.to_string();

        if let Some(labels) = report.labels() {
            let diagnostics: Vec<lsp::Diagnostic> = labels
                .map(|label: LabeledSpan| {
                    let offset = label.offset();
                    let len = label.len();
                    let start = self.position_at_offset(uri, offset as u32);
                    let end = self.position_at_offset(uri, (offset + len) as u32);
                    lsp::Diagnostic {
                        range: lsp::Range { start, end },
                        severity: Some(severity),
                        source: Some("glue".to_string()),
                        message: label.label().map(ToString::to_string).unwrap_or_else(|| message.clone()),
                        ..Default::default()
                    }
                })
                .collect();

            if !diagnostics.is_empty() {
                return diagnostics;
            }
        }

        vec![lsp::Diagnostic {
            range: lsp::Range {
                start: lsp::Position { line: 0, character: 0 },
                end: lsp::Position { line: 0, character: 1 },
            },
            severity: Some(severity),
            source: Some("glue".to_string()),
            message,
            ..Default::default()
        }]
    }

    fn collect_document_diagnostics(&self, uri: &str) -> Vec<lsp::Diagnostic> {
        let Some(source) = self.load_document_or_file(uri) else {
            return Vec::new();
        };

        let mut extra_diags = self.collect_missing_import_path_diagnostics(uri, &source);

        let metadata = SourceCodeMetadata {
            file_name: uri,
            file_contents: &source,
        };

        let mut parser = Parser::new();
        let parsed = match parser.parse(&metadata) {
            Ok(parsed) => parsed,
            Err(err) => {
                let mut diags = self.diagnostics_from_report(uri, err.report());
                diags.append(&mut extra_diags);
                return diags;
            }
        };

        match SemanticAnalyzer::new().analyze(&parsed, &metadata) {
            Ok(_) => extra_diags,
            Err(errors) => {
                let mut semantic_diags: Vec<lsp::Diagnostic> = errors
                    .iter()
                    .filter(|err| !self.is_undefined_reference_resolved_by_import(uri, err))
                    .flat_map(|err| self.diagnostics_from_report(uri, err.report()))
                    .collect();
                semantic_diags.append(&mut extra_diags);
                semantic_diags
            }
        }
    }

    fn collect_missing_import_path_diagnostics(&self, uri: &str, source: &str) -> Vec<lsp::Diagnostic> {
        let mut diagnostics = Vec::new();
        let Some(base_path) = Self::uri_to_file_path(uri) else {
            return diagnostics;
        };
        let base_dir = base_path.parent().unwrap_or_else(|| Path::new("."));

        for (line_idx, line) in source.lines().enumerate() {
            let Some((import_path, path_start, path_end)) = Self::extract_import_path_with_range(line) else {
                continue;
            };

            let exists = base_dir.join(&import_path).exists();
            if exists {
                continue;
            }

            diagnostics.push(lsp::Diagnostic {
                range: lsp::Range {
                    start: lsp::Position {
                        line: line_idx as u32,
                        character: path_start as u32,
                    },
                    end: lsp::Position {
                        line: line_idx as u32,
                        character: path_end as u32,
                    },
                },
                severity: Some(lsp::DiagnosticSeverity::ERROR),
                source: Some("glue".to_string()),
                message: format!("Import path does not exist: '{}'", import_path),
                ..Default::default()
            });
        }

        diagnostics
    }

    fn extract_import_path_with_range(line: &str) -> Option<(String, usize, usize)> {
        let trimmed = line.trim_start();
        if !trimmed.starts_with("import ") || !trimmed.contains(" from ") {
            return None;
        }
        let leading_ws = line.len() - trimmed.len();
        let from_idx = trimmed.find(" from ")?;
        let after_from = &trimmed[from_idx + 6..];
        let quote_idx = after_from.find('"')?;
        let rest = &after_from[quote_idx + 1..];
        let end_quote_idx = rest.find('"')?;
        let import_path = rest[..end_quote_idx].to_string();

        let path_start = leading_ws + from_idx + 6 + quote_idx + 1;
        let path_end = path_start + end_quote_idx;
        Some((import_path, path_start, path_end))
    }

    fn import_path_completion_context(line: &str, pos: lsp::Position) -> Option<String> {
        let cursor = pos.character.min(line.len() as u32) as usize;
        let prefix = &line[..cursor];
        if !prefix.trim_start().starts_with("import ") || !prefix.contains(" from ") {
            return None;
        }

        let quote_idx = prefix.rfind('"')?;
        if prefix[quote_idx + 1..].contains('"') {
            return None;
        }
        Some(prefix[quote_idx + 1..].to_string())
    }

    fn import_path_completion_items(&self, uri: &str, typed_prefix: &str) -> Vec<lsp::CompletionItem> {
        let Some(file_path) = Self::uri_to_file_path(uri) else {
            return Vec::new();
        };
        let base_dir = file_path.parent().unwrap_or_else(|| Path::new("."));

        let (dir_part, name_prefix) = match typed_prefix.rsplit_once('/') {
            Some((dir_part, name_prefix)) => (format!("{dir_part}/"), name_prefix),
            None => (String::new(), typed_prefix),
        };

        let target_dir = base_dir.join(dir_part.trim_end_matches('/'));
        let Ok(entries) = std::fs::read_dir(&target_dir) else {
            return Vec::new();
        };

        let mut items = Vec::new();
        for entry in entries.flatten() {
            let Ok(file_type) = entry.file_type() else {
                continue;
            };
            let name = entry.file_name().to_string_lossy().to_string();
            if !name.starts_with(name_prefix) {
                continue;
            }

            if file_type.is_dir() {
                items.push(lsp::CompletionItem {
                    label: format!("{}{}{}", dir_part, name, "/"),
                    kind: Some(lsp::CompletionItemKind::FOLDER),
                    sort_text: Some(format!("0{}", name)),
                    ..Default::default()
                });
            } else if name.ends_with(".glue") {
                items.push(lsp::CompletionItem {
                    label: format!("{}{}", dir_part, name),
                    kind: Some(lsp::CompletionItemKind::FILE),
                    sort_text: Some(format!("1{}", name)),
                    ..Default::default()
                });
            }
        }

        items.sort_by(|a, b| a.label.cmp(&b.label));
        items
    }

    fn is_undefined_reference_resolved_by_import(&self, uri: &str, err: &SemanticAnalyzerError) -> bool {
        let SemanticAnalyzerError::UndefinedTypeReference(report) = err else {
            return false;
        };

        let message = report.to_string();
        let marker = "Undefined type reference '";
        let Some(start) = message.find(marker) else {
            return false;
        };
        let rest = &message[start + marker.len()..];
        let Some(end) = rest.find('"').or_else(|| rest.find('\'')) else {
            return false;
        };
        let ty_name = &rest[..end];

        self.resolve_imported_location(uri, ty_name).is_some()
    }

    async fn publish_document_diagnostics(&self, uri: &str) {
        let Some(client) = &self.client else {
            return;
        };

        let Ok(parsed_uri) = uri.parse::<lsp::Url>() else {
            error!("Failed to parse document URI for diagnostics: {uri}");
            return;
        };

        let diagnostics = self.collect_document_diagnostics(uri);
        client.publish_diagnostics(parsed_uri, diagnostics, None).await;
    }

    /// Parses the document and runs lenient semantic analysis, succeeding even when the document
    /// has errors (e.g. an unresolved type reference while the user is still typing).
    /// Used by hover, goto_definition, and completion providers.
    fn parse_document_lenient(&self, uri: &str) -> Result<(LNode, SymTable<LNode>)> {
        let state = self.state.read().expect("Failed to acquire read lock on LSP state");
        let source = state.documents.get(uri).ok_or_else(|| anyhow::anyhow!("Document not found: {uri}"))?;
        let metadata = SourceCodeMetadata {
            file_name: uri,
            file_contents: source,
        };
        let mut parser = Parser::new();
        let parsed = parser.parse(&metadata).map_err(|e| anyhow::anyhow!("Parse error: {:?}", e))?;
        let analyzer = SemanticAnalyzer::new();
        let analyzed = analyzer.analyze_lenient(&parsed, &metadata);
        Ok((analyzed.ast_root, analyzed.symbols))
    }

    fn parse_source_lenient(file_name: &str, source: &str) -> Result<(LNode, SymTable<LNode>)> {
        let metadata = SourceCodeMetadata { file_name, file_contents: source };
        let mut parser = Parser::new();
        let parsed = parser.parse(&metadata).map_err(|e| anyhow::anyhow!("Parse error: {:?}", e))?;
        let analyzer = SemanticAnalyzer::new();
        let analyzed = analyzer.analyze_lenient(&parsed, &metadata);
        Ok((analyzed.ast_root, analyzed.symbols))
    }

    fn uri_to_file_path(uri: &str) -> Option<PathBuf> {
        let parsed = lsp::Url::parse(uri).ok()?;
        parsed.to_file_path().ok()
    }

    fn file_path_to_uri(path: &Path) -> Option<String> {
        let url = lsp::Url::from_file_path(path).ok()?;
        Some(url.to_string())
    }

    fn load_document_or_file(&self, uri: &str) -> Option<String> {
        {
            let state = self.state.read().expect("Failed to acquire read lock on LSP state");
            if let Some(source) = state.documents.get(uri) {
                return Some(source.clone());
            }
        }

        let path = Self::uri_to_file_path(uri)?;
        std::fs::read_to_string(path).ok()
    }

    fn position_at_offset_in_source(source: &str, offset: u32) -> lsp::Position {
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

    fn resolve_imported_location(&self, from_uri: &str, reference_name: &str) -> Option<lsp::Location> {
        let source = self.load_document_or_file(from_uri)?;
        let (ast, _) = Self::parse_source_lenient(from_uri, &source).ok()?;
        let root = RootNode::cast(ast)?;

        let (qualifier, symbol_name) = if let Some((left, right)) = reference_name.split_once('.') {
            (Some(left), right)
        } else {
            (None, reference_name)
        };

        for import_stmt in root.top_level_imports() {
            let Some(import_path) = import_stmt.source_path() else {
                continue;
            };
            let Some(target_uri) = Self::resolve_import_uri(from_uri, &import_path) else {
                continue;
            };

            let target_symbol = if let Some(qualifier) = qualifier {
                if import_stmt.is_wildcard() && import_stmt.wildcard_alias().as_deref() == Some(qualifier) {
                    Some(symbol_name.to_string())
                } else {
                    None
                }
            } else {
                let mut target = None;
                if import_stmt.is_wildcard() && import_stmt.wildcard_alias().is_none() {
                    target = Some(symbol_name.to_string());
                }
                for (imported_name, alias) in import_stmt.named_item_specs() {
                    let visible_name = alias.unwrap_or_else(|| imported_name.clone());
                    if visible_name == symbol_name {
                        target = Some(imported_name);
                        break;
                    }
                }
                target
            };

            let Some(target_symbol) = target_symbol else {
                continue;
            };
            if let Some(location) = self.find_top_level_symbol_location(&target_uri, &target_symbol) {
                return Some(location);
            }
        }

        None
    }

    fn resolve_import_uri(base_uri: &str, import_path: &str) -> Option<String> {
        let base_path = Self::uri_to_file_path(base_uri)?;
        let resolved = base_path.parent().unwrap_or_else(|| Path::new(".")).join(import_path);
        let canonical = resolved.canonicalize().ok()?;
        Self::file_path_to_uri(&canonical)
    }

    fn find_top_level_symbol_location(&self, uri: &str, symbol_name: &str) -> Option<lsp::Location> {
        let source = self.load_document_or_file(uri)?;
        let (ast, _) = Self::parse_source_lenient(uri, &source).ok()?;
        let root = RootNode::cast(ast)?;

        let maybe_node = root
            .top_level_models()
            .into_iter()
            .find_map(|model| (model.ident().as_deref() == Some(symbol_name)).then(|| model.syntax().clone()))
            .or_else(|| {
                root.top_level_enums()
                    .into_iter()
                    .find_map(|enum_| (enum_.ident().as_deref() == Some(symbol_name)).then(|| enum_.syntax().clone()))
            });

        let node = maybe_node?;
        let start = node.text_range().start();
        let end = node.text_range().end();
        let uri = lsp::Url::parse(uri).ok()?;

        Some(lsp::Location {
            uri,
            range: lsp::Range {
                start: Self::position_at_offset_in_source(&source, start.into()),
                end: Self::position_at_offset_in_source(&source, end.into()),
            },
        })
    }

    fn imported_symbol_info(&self, from_uri: &str, reference_name: &str) -> Option<SymbolInfo> {
        let location = self.resolve_imported_location(from_uri, reference_name)?;
        let target_uri = location.uri.to_string();
        let source = self.load_document_or_file(&target_uri)?;
        let (ast, _) = Self::parse_source_lenient(&target_uri, &source).ok()?;
        let root = RootNode::cast(ast)?;

        let symbol_name = reference_name.split('.').next_back()?;

        let node = root
            .top_level_models()
            .into_iter()
            .find_map(|model| (model.ident().as_deref() == Some(symbol_name)).then(|| model.syntax().clone()))
            .or_else(|| {
                root.top_level_enums()
                    .into_iter()
                    .find_map(|enum_| (enum_.ident().as_deref() == Some(symbol_name)).then(|| enum_.syntax().clone()))
            })?;

        self.extract_symbol_info(&node)
    }

    fn import_alias_location(&self, uri: &str, alias: &str) -> Option<lsp::Location> {
        let source = self.load_document_or_file(uri)?;
        let (ast, _) = Self::parse_source_lenient(uri, &source).ok()?;
        let root = RootNode::cast(ast)?;

        let import_node = root
            .top_level_imports()
            .into_iter()
            .find(|import_stmt| import_stmt.wildcard_alias().as_deref() == Some(alias))?
            .syntax()
            .clone();

        let start = import_node.text_range().start();
        let end = import_node.text_range().end();
        let uri = lsp::Url::parse(uri).ok()?;

        Some(lsp::Location {
            uri,
            range: lsp::Range {
                start: Self::position_at_offset_in_source(&source, start.into()),
                end: Self::position_at_offset_in_source(&source, end.into()),
            },
        })
    }

    fn import_alias_info(&self, uri: &str, alias: &str) -> Option<(String, String)> {
        let source = self.load_document_or_file(uri)?;
        let (ast, _) = Self::parse_source_lenient(uri, &source).ok()?;
        let root = RootNode::cast(ast)?;
        let import_stmt = root.top_level_imports().into_iter().find(|import_stmt| import_stmt.wildcard_alias().as_deref() == Some(alias))?;
        let path = import_stmt.source_path()?;
        Some((alias.to_string(), path))
    }

    fn top_level_symbol_completions(&self, uri: &str) -> Vec<(String, lsp::CompletionItemKind)> {
        let Some(source) = self.load_document_or_file(uri) else {
            return Vec::new();
        };
        let Ok((ast, _)) = Self::parse_source_lenient(uri, &source) else {
            return Vec::new();
        };
        let Some(root) = RootNode::cast(ast) else {
            return Vec::new();
        };

        let mut out = Vec::new();
        for model in root.top_level_models() {
            if let Some(name) = model.ident() {
                out.push((name, lsp::CompletionItemKind::STRUCT));
            }
        }
        for enum_def in root.top_level_enums() {
            if let Some(name) = enum_def.ident() {
                out.push((name, lsp::CompletionItemKind::ENUM));
            }
        }
        out
    }

    fn namespace_member_context(&self, uri: &str, pos: lsp::Position) -> Option<(String, String)> {
        let source = self.load_document_or_file(uri)?;
        let line = source.lines().nth(pos.line as usize)?;
        Self::namespace_member_context_from_line(line, pos)
    }

    fn namespace_member_context_from_line(line: &str, pos: lsp::Position) -> Option<(String, String)> {
        let prefix = &line[..pos.character.min(line.len() as u32) as usize];
        let dot_idx = prefix.rfind('.')?;
        let left = &prefix[..dot_idx];
        let right = &prefix[dot_idx + 1..];

        if !right.chars().all(|c| c.is_alphanumeric() || c == '_') {
            return None;
        }

        let alias_rev: String = left.chars().rev().take_while(|c| c.is_alphanumeric() || *c == '_').collect();
        let alias: String = alias_rev.chars().rev().collect();
        if alias.is_empty() {
            return None;
        }
        Some((alias, right.to_string()))
    }

    fn wildcard_alias_import_uri_from_source(from_uri: &str, source: &str, alias: &str) -> Option<String> {
        for line in source.lines() {
            let trimmed = line.trim();
            if !trimmed.starts_with("import * as ") {
                continue;
            }

            let rest = trimmed.strip_prefix("import * as ")?.trim_start();
            let alias_end = rest.find(char::is_whitespace)?;
            let parsed_alias = &rest[..alias_end];
            if parsed_alias != alias {
                continue;
            }

            let from_idx = rest.find("from")?;
            let after_from = rest.get(from_idx + 4..)?.trim();
            let start = after_from.find('"')?;
            let path_rest = after_from.get(start + 1..)?;
            let end = path_rest.find('"')?;
            let import_path = &path_rest[..end];
            return Self::resolve_import_uri(from_uri, import_path);
        }
        None
    }

    fn wildcard_alias_import_uri(&self, from_uri: &str, alias: &str) -> Option<String> {
        let source = self.load_document_or_file(from_uri)?;
        let (ast, _) = Self::parse_source_lenient(from_uri, &source).ok()?;
        let root = RootNode::cast(ast)?;
        let import_stmt = root.top_level_imports().into_iter().find(|i| i.wildcard_alias().as_deref() == Some(alias))?;
        let import_path = import_stmt.source_path()?;
        Self::resolve_import_uri(from_uri, &import_path)
    }

    fn imported_file_location_for_alias_at_offset(&self, ast: &LNode, uri: &str, offset: u32) -> Option<lsp::Location> {
        let token = ast.token_at_offset(TextSize::new(offset));
        let token = match token {
            TokenAtOffset::Single(t) => t,
            TokenAtOffset::Between(_, t) => t,
            TokenAtOffset::None => return None,
        };

        if token.kind() != LSyntaxKind::IDENT {
            return None;
        }

        let mut current = token.parent();
        let mut import_stmt_node = None;
        while let Some(node) = current {
            if node.kind() == LSyntaxKind::IMPORT_STMT {
                import_stmt_node = Some(node);
                break;
            }
            current = node.parent();
        }
        let import_stmt_node = import_stmt_node?;
        let import_stmt = lang::ImportStmt::cast(import_stmt_node)?;

        let alias = token.text().to_string();
        let is_matching_alias = import_stmt.wildcard_alias().as_deref() == Some(alias.as_str())
            || import_stmt
                .named_item_specs()
                .iter()
                .any(|(imported, alias_opt)| alias_opt.as_deref() == Some(alias.as_str()) || imported == &alias);
        if !is_matching_alias {
            return None;
        }

        let import_path = import_stmt.source_path()?;
        let target_uri = Self::resolve_import_uri(uri, &import_path)?;

        Some(lsp::Location {
            uri: lsp::Url::parse(&target_uri).ok()?,
            range: lsp::Range {
                start: lsp::Position { line: 0, character: 0 },
                end: lsp::Position { line: 0, character: 0 },
            },
        })
    }

    fn imported_file_location_for_path_at_offset(&self, ast: &LNode, uri: &str, offset: u32) -> Option<lsp::Location> {
        let token = ast.token_at_offset(TextSize::new(offset));
        let token = match token {
            TokenAtOffset::Single(t) => t,
            TokenAtOffset::Between(_, t) => t,
            TokenAtOffset::None => return None,
        };

        let mut current = token.parent();
        let mut import_stmt_node = None;
        let mut inside_string_literal = false;

        while let Some(node) = current {
            if node.kind() == LSyntaxKind::STRING_LITERAL {
                inside_string_literal = true;
            }
            if node.kind() == LSyntaxKind::IMPORT_STMT {
                import_stmt_node = Some(node);
                break;
            }
            current = node.parent();
        }

        if !inside_string_literal {
            return None;
        }

        let import_stmt_node = import_stmt_node?;
        let import_stmt = lang::ImportStmt::cast(import_stmt_node)?;
        let import_path = import_stmt.source_path()?;
        let target_uri = Self::resolve_import_uri(uri, &import_path)?;

        Some(lsp::Location {
            uri: lsp::Url::parse(&target_uri).ok()?,
            range: lsp::Range {
                start: lsp::Position { line: 0, character: 0 },
                end: lsp::Position { line: 0, character: 0 },
            },
        })
    }

    fn import_path_origin_range_at_position(&self, uri: &str, pos: lsp::Position) -> Option<lsp::Range> {
        let source = self.load_document_or_file(uri)?;
        let line = source.lines().nth(pos.line as usize)?;
        let (_, path_start, path_end) = Self::extract_import_path_with_range(line)?;

        let ch = pos.character as usize;
        if ch < path_start || ch > path_end {
            return None;
        }

        Some(lsp::Range {
            start: lsp::Position {
                line: pos.line,
                character: path_start as u32,
            },
            end: lsp::Position {
                line: pos.line,
                character: path_end as u32,
            },
        })
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

    fn reference_name_at_offset(&self, ast: &LNode, offset: u32) -> Option<String> {
        let token = ast.token_at_offset(TextSize::new(offset));
        let token = match token {
            TokenAtOffset::Single(t) => t,
            TokenAtOffset::Between(_, t) => t,
            TokenAtOffset::None => return None,
        };

        if token.kind() != LSyntaxKind::IDENT {
            return None;
        }

        if let Some(parent) = token.parent()
            && parent.kind() == LSyntaxKind::TYPE_REF
        {
            return Some(parent.text().to_string());
        }

        Some(token.text().to_string())
    }

    fn namespace_alias_at_offset(&self, ast: &LNode, offset: u32) -> Option<String> {
        let token = ast.token_at_offset(TextSize::new(offset));
        let token = match token {
            TokenAtOffset::Single(t) => t,
            TokenAtOffset::Between(_, t) => t,
            TokenAtOffset::None => return None,
        };

        if token.kind() != LSyntaxKind::IDENT {
            return None;
        }

        let parent = token.parent()?;
        if parent.kind() != LSyntaxKind::TYPE_REF {
            return None;
        }

        let full = parent.text().to_string();
        let (qualifier, _) = full.split_once('.')?;
        (qualifier == token.text()).then(|| qualifier.to_string())
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
        // Get token at offset and walk up to find enclosing model or endpoint
        let token = ast.token_at_offset(TextSize::new(offset));
        let token = match token {
            TokenAtOffset::Single(t) => t,
            TokenAtOffset::Between(_, t) => t,
            TokenAtOffset::None => return None,
        };

        // Walk up the parent chain to find enclosing MODEL or ENDPOINT
        // We need to collect scopes from innermost to outermost
        let mut scopes: Vec<(LSyntaxKind, String)> = Vec::new();
        let mut current = token.parent();
        while let Some(node) = current {
            match node.kind() {
                LSyntaxKind::MODEL => {
                    if let Some(model) = Model::cast(node.clone())
                        && let Some(name) = model.ident()
                    {
                        scopes.push((LSyntaxKind::MODEL, name));
                    }
                }
                LSyntaxKind::ENDPOINT => {
                    if let Some(endpoint) = Endpoint::cast(node.clone())
                        && let Some(path_literal) = endpoint.path_string_literal_node()
                        && let Some(path) = path_literal.value()
                    {
                        scopes.push((LSyntaxKind::ENDPOINT, path));
                    }
                }
                _ => {}
            }
            current = node.parent();
        }

        // Build the fully qualified scope name from outermost to innermost
        if scopes.is_empty() {
            return None;
        }

        // Reverse to get outermost first
        scopes.reverse();
        let full_scope_name = scopes.iter().map(|(_, name)| name.as_str()).collect::<Vec<_>>().join("::");
        symbols.resolve_id(None, &full_scope_name)
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
    fn generate_decorator_hover_content(&self, def: &DecoratorDef) -> String {
        let mut parts = Vec::new();
        parts.push(format!("```glue\n@{}\n```", def.id));
        parts.push("---".to_string());
        parts.push(def.doc().to_string());
        parts.join("\n\n")
    }

    fn generate_decorator_arg_hover_content(&self, def: &DecoratorDef, arg: &DecoratorArgDef) -> String {
        let mut parts = Vec::new();
        parts.push(format!("```glue\n@{} — {} ({})\n```", def.id, arg.id, arg.ty));
        parts.push("---".to_string());
        parts.push(arg.doc.to_string());
        if arg.required {
            parts.push("_Required._".to_string());
        }
        parts.join("\n\n")
    }

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
                    trigger_characters: Some(vec![".".to_string(), ":".to_string(), "(".to_string(), "@".to_string(), "/".to_string(), "\"".to_string()]),
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
        self.publish_document_diagnostics(&uri).await;
    }

    async fn did_change(&self, params: lsp::DidChangeTextDocumentParams) {
        if let Some(change) = params.content_changes.into_iter().last() {
            let uri = params.text_document.uri.to_string();
            self.update_source(&uri, &change.text);
            self.publish_document_diagnostics(&uri).await;
        }
    }

    async fn did_save(&self, params: lsp::DidSaveTextDocumentParams) {
        let uri = params.text_document.uri.to_string();
        self.publish_document_diagnostics(&uri).await;
    }

    async fn did_close(&self, params: lsp::DidCloseTextDocumentParams) {
        let uri = params.text_document.uri;
        let uri_str = uri.to_string();
        self.remove_source(&uri_str);
        if let Some(client) = &self.client {
            client.publish_diagnostics(uri, Vec::new(), None).await;
        }
    }

    async fn goto_definition(&self, params: lsp::GotoDefinitionParams) -> LspResult<Option<lsp::GotoDefinitionResponse>> {
        let uri = params.text_document_position_params.text_document.uri.to_string();
        let pos = params.text_document_position_params.position;
        info!("Received goto_definition request at position: {pos:?} in {uri}");

        let Ok((ast, symbols)) = self.parse_document_lenient(&uri) else {
            error!("Failed to parse document for goto_definition: {uri}");
            return Ok(None);
        };

        let offset = self.offset_at_position(&uri, pos);

        if let Some(location) = self.imported_file_location_for_path_at_offset(&ast, &uri, offset) {
            if let Some(origin_range) = self.import_path_origin_range_at_position(&uri, pos) {
                let target_uri = location.uri.clone();
                let target_range = location.range;
                return Ok(Some(lsp::GotoDefinitionResponse::Link(vec![lsp::LocationLink {
                    origin_selection_range: Some(origin_range),
                    target_uri,
                    target_range,
                    target_selection_range: target_range,
                }])));
            }
            return Ok(Some(lsp::GotoDefinitionResponse::Scalar(location)));
        }

        if let Some(location) = self.imported_file_location_for_alias_at_offset(&ast, &uri, offset) {
            return Ok(Some(lsp::GotoDefinitionResponse::Scalar(location)));
        }

        let Some(ref_name) = self.reference_name_at_offset(&ast, offset) else {
            return Ok(None);
        };

        if let Some(namespace_alias) = self.namespace_alias_at_offset(&ast, offset)
            && let Some(location) = self.import_alias_location(&uri, &namespace_alias)
        {
            return Ok(Some(lsp::GotoDefinitionResponse::Scalar(location)));
        }

        info!("Looking for definition of: {ref_name}");

        // Find the enclosing scope to resolve nested types
        let scope = self.find_scope_at_offset(&ast, offset, &symbols);
        info!("Enclosing scope: {scope:?}");

        if let Some(sym_entry) = symbols.resolve(scope, &ref_name) {
            let def_node = &sym_entry.data;
            let start_offset = def_node.text_range().start();
            let end_offset = def_node.text_range().end();
            return Ok(Some(lsp::GotoDefinitionResponse::Scalar(lsp::Location {
                uri: params.text_document_position_params.text_document.uri.clone(),
                range: lsp::Range {
                    start: self.position_at_offset(&uri, start_offset.into()),
                    end: self.position_at_offset(&uri, end_offset.into()),
                },
            })));
        }

        let imported = self.resolve_imported_location(&uri, &ref_name);
        if let Some(location) = imported {
            return Ok(Some(lsp::GotoDefinitionResponse::Scalar(location)));
        }

        info!("No symbol found for: {ref_name}");
        Ok(None)
    }

    async fn hover(&self, params: lsp::HoverParams) -> LspResult<Option<lsp::Hover>> {
        let uri = params.text_document_position_params.text_document.uri.to_string();
        let pos = params.text_document_position_params.position;
        info!("Received hover request at position: {pos:?} in {uri}");

        let Ok((ast, symbols)) = self.parse_document_lenient(&uri) else {
            error!("Failed to parse document for hover: {uri}");
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
        let ref_name_for_import_resolution = self.reference_name_at_offset(&ast, offset).unwrap_or_else(|| ref_name.clone());
        info!("Hover on identifier: {ref_name}");

        if let Some(namespace_alias) = self.namespace_alias_at_offset(&ast, offset)
            && let Some((alias, path)) = self.import_alias_info(&uri, &namespace_alias)
        {
            let value = format!("```glue\nnamespace {}\n```\n\n---\n\nImported from `{}`", alias, path);
            return Ok(Some(lsp::Hover {
                contents: lsp::HoverContents::Markup(lsp::MarkupContent {
                    kind: lsp::MarkupKind::Markdown,
                    value,
                }),
                range: Some(lsp::Range {
                    start: self.position_at_offset(&uri, token.text_range().start().into()),
                    end: self.position_at_offset(&uri, token.text_range().end().into()),
                }),
            }));
        }

        // Check decorator context first: decorator name or named-arg key don't live in the
        // symbol table, so handle them before symbol resolution.
        if let Some(parent) = token.parent() {
            match parent.kind() {
                LSyntaxKind::DECORATOR => {
                    // Cursor is on the decorator name, e.g. `field` in `@field(...)`
                    let content = BUILTIN_DECORATORS.iter().find(|d| d.id == ref_name.as_str()).map(|d| self.generate_decorator_hover_content(d));
                    return Ok(content.map(|value| lsp::Hover {
                        contents: lsp::HoverContents::Markup(lsp::MarkupContent {
                            kind: lsp::MarkupKind::Markdown,
                            value,
                        }),
                        range: Some(lsp::Range {
                            start: self.position_at_offset(&uri, token.text_range().start().into()),
                            end: self.position_at_offset(&uri, token.text_range().end().into()),
                        }),
                    }));
                }
                LSyntaxKind::DECORATOR_NAMED_ARG => {
                    // Cursor is on a named-arg key, e.g. `alias` in `@field(alias="foo")`
                    // Parent chain: DECORATOR_NAMED_ARG -> DECORATOR
                    if let Some(decorator_node) = parent.parent()
                        && let Some(decorator) = Decorator::cast(decorator_node)
                        && let Some(dec_name) = decorator.ident()
                    {
                        let content = BUILTIN_DECORATORS
                            .iter()
                            .find(|d| d.id == dec_name.as_str())
                            .and_then(|d| d.args().into_iter().find(|a| a.id == ref_name.as_str()).map(|a| (d, a)))
                            .map(|(d, a)| self.generate_decorator_arg_hover_content(d, a));
                        return Ok(content.map(|value| lsp::Hover {
                            contents: lsp::HoverContents::Markup(lsp::MarkupContent {
                                kind: lsp::MarkupKind::Markdown,
                                value,
                            }),
                            range: Some(lsp::Range {
                                start: self.position_at_offset(&uri, token.text_range().start().into()),
                                end: self.position_at_offset(&uri, token.text_range().end().into()),
                            }),
                        }));
                    }
                    return Ok(None);
                }
                _ => {}
            }
        }

        // Try to resolve the symbol with scope awareness
        let scope = self.find_scope_at_offset(&ast, offset, &symbols);
        let info = if let Some(sym_entry) = symbols.resolve(scope, &ref_name) {
            self.extract_symbol_info(&sym_entry.data)
        } else {
            self.imported_symbol_info(&uri, &ref_name_for_import_resolution)
        };

        let Some(info) = info else {
            info!("No symbol found for hover: {ref_name}");
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

        // Detect decorator-name context from raw source text.
        // This must happen before parsing because `@fie` (partial decorator) causes a parse
        // error, and we'd return Ok(None) before ever reaching the check if it came later.
        {
            let state = self.state.read().expect("Failed to acquire read lock on LSP state");
            if let Some(source) = state.documents.get(&uri) {
                let line_text = source.lines().nth(pos.line as usize).unwrap_or("");
                let prefix = &line_text[..pos.character.min(line_text.len() as u32) as usize];
                if let Some(at_pos) = prefix.rfind('@') {
                    let partial = &prefix[at_pos + 1..];
                    // Only treat as decorator-name context if everything after `@` is word chars
                    if partial.chars().all(|c| c.is_alphanumeric() || c == '_') {
                        let items: Vec<lsp::CompletionItem> = BUILTIN_DECORATORS
                            .iter()
                            .filter(|d| d.id.starts_with(partial))
                            .map(|d| lsp::CompletionItem {
                                label: format!("@{}", d.id),
                                kind: Some(lsp::CompletionItemKind::FUNCTION),
                                detail: Some("decorator".to_string()),
                                documentation: Some(lsp::Documentation::MarkupContent(lsp::MarkupContent {
                                    kind: lsp::MarkupKind::Markdown,
                                    value: d.doc().to_string(),
                                })),
                                insert_text: Some(format!("{}(${{0}})", d.id)),
                                insert_text_format: Some(lsp::InsertTextFormat::SNIPPET),
                                filter_text: Some(d.id.to_string()),
                                sort_text: Some(format!("0{}", d.id)),
                                ..Default::default()
                            })
                            .collect();
                        if !items.is_empty() {
                            return Ok(Some(lsp::CompletionResponse::Array(items)));
                        }
                    }
                }

                if let Some(prefix) = Self::import_path_completion_context(line_text, pos) {
                    let items = self.import_path_completion_items(&uri, &prefix);
                    if !items.is_empty() {
                        return Ok(Some(lsp::CompletionResponse::Array(items)));
                    }
                }

                if let Some((alias, member_prefix)) = Self::namespace_member_context_from_line(line_text, pos)
                    && let Some(target_uri) = Self::wildcard_alias_import_uri_from_source(&uri, source, &alias)
                {
                    let items: Vec<lsp::CompletionItem> = self
                        .top_level_symbol_completions(&target_uri)
                        .into_iter()
                        .filter(|(name, _)| name.starts_with(&member_prefix))
                        .map(|(name, kind)| lsp::CompletionItem {
                            label: name.clone(),
                            kind: Some(kind),
                            sort_text: Some(format!("0{}", name)),
                            ..Default::default()
                        })
                        .collect();
                    return Ok(Some(lsp::CompletionResponse::Array(items)));
                }
            }
        }

        let Ok((ast, symbols)) = self.parse_document_lenient(&uri) else {
            error!("Failed to parse document for completion: {uri}");
            return Ok(None);
        };

        if let Some((alias, member_prefix)) = self.namespace_member_context(&uri, pos)
            && let Some(target_uri) = self.wildcard_alias_import_uri(&uri, &alias)
        {
            let items: Vec<lsp::CompletionItem> = self
                .top_level_symbol_completions(&target_uri)
                .into_iter()
                .filter(|(name, _)| name.starts_with(&member_prefix))
                .map(|(name, kind)| lsp::CompletionItem {
                    label: name.clone(),
                    kind: Some(kind),
                    sort_text: Some(format!("0{}", name)),
                    ..Default::default()
                })
                .collect();
            return Ok(Some(lsp::CompletionResponse::Array(items)));
        }

        let root = RootNode::cast(ast.clone());
        let Some(root) = root else {
            return Ok(None);
        };

        // Find the current scope to prioritize nested types
        let offset = self.offset_at_position(&uri, pos);
        let current_scope = self.find_scope_at_offset(&ast, offset, &symbols);

        // Detect decorator context by walking up from the token at cursor.
        // If we're inside a DECORATOR node, offer named-arg completions instead of type completions.
        let token_at_cursor = ast.token_at_offset(TextSize::new(offset));
        let token_at_cursor = match token_at_cursor {
            TokenAtOffset::Single(t) | TokenAtOffset::Between(_, t) => Some(t),
            TokenAtOffset::None => None,
        };
        if let Some(ref tok) = token_at_cursor {
            let mut parent = tok.parent();
            let mut decorator_node: Option<LNode> = None;
            while let Some(ref p) = parent {
                if p.kind() == LSyntaxKind::DECORATOR {
                    decorator_node = Some(p.clone());
                    break;
                }
                // Stop ascending past field or model boundaries
                if matches!(p.kind(), LSyntaxKind::FIELD | LSyntaxKind::MODEL | LSyntaxKind::ENDPOINT | LSyntaxKind::PROGRAM) {
                    break;
                }
                parent = p.parent();
            }

            if let Some(dec_node) = decorator_node {
                if let Some(decorator) = Decorator::cast(dec_node)
                    && let Some(dec_name) = decorator.ident()
                {
                    // Collect already-used named arg names so we don't duplicate them
                    let used_named_args: std::collections::HashSet<String> = decorator.named_args().iter().filter_map(|a| a.ident()).collect();

                    if let Some(def) = BUILTIN_DECORATORS.iter().find(|d| d.id == dec_name.as_str()) {
                        let items: Vec<lsp::CompletionItem> = def
                            .named_args
                            .iter()
                            .filter(|a| !used_named_args.contains(a.id))
                            .map(|a| lsp::CompletionItem {
                                label: a.id.to_string(),
                                kind: Some(lsp::CompletionItemKind::PROPERTY),
                                detail: Some(format!("{}", a.ty)),
                                documentation: Some(lsp::Documentation::MarkupContent(lsp::MarkupContent {
                                    kind: lsp::MarkupKind::Markdown,
                                    value: format!("{}{}", a.doc, if a.required { "\n\n_Required._" } else { "" }),
                                })),
                                insert_text: Some(format!("{}=${{1:{}}}", a.id, a.ty)),
                                insert_text_format: Some(lsp::InsertTextFormat::SNIPPET),
                                sort_text: Some(format!("0{}", a.id)),
                                ..Default::default()
                            })
                            .collect();
                        return Ok(Some(lsp::CompletionResponse::Array(items)));
                    }
                }
                // Inside a decorator but unknown/empty — return nothing
                return Ok(Some(lsp::CompletionResponse::Array(vec![])));
            }
        }

        let mut items: Vec<lsp::CompletionItem> = Vec::new();

        // Helper to create completion item with sort priority
        let make_completion = |name: String, kind: lsp::CompletionItemKind, is_in_scope: bool| {
            lsp::CompletionItem {
                label: name.clone(),
                kind: Some(kind),
                // Lower sort_text = higher priority; nested types in scope get "0", others get "1"
                sort_text: Some(if is_in_scope { format!("0{}", name) } else { format!("1{}", name) }),
                ..Default::default()
            }
        };

        // Collect nested types from models
        for model in root.top_level_models() {
            if let Some(name) = model.ident() {
                let model_scope = symbols.resolve_id(None, &name);
                let is_in_scope = current_scope.is_some() && current_scope == model_scope;
                items.push(make_completion(name, lsp::CompletionItemKind::STRUCT, false));

                // Add nested models
                for nested in model.nested_models() {
                    if let Some(nested_name) = nested.ident() {
                        items.push(make_completion(nested_name, lsp::CompletionItemKind::STRUCT, is_in_scope));
                    }
                }
                // Add nested enums
                for nested in model.nested_enums() {
                    if let Some(nested_name) = nested.ident() {
                        items.push(make_completion(nested_name, lsp::CompletionItemKind::ENUM, is_in_scope));
                    }
                }
            }
        }

        // Collect nested types from endpoints
        for endpoint in root.top_level_endpoints() {
            if let Some(path_literal) = endpoint.path_string_literal_node()
                && let Some(path) = path_literal.value()
            {
                let endpoint_scope = symbols.resolve_id(None, &path);
                let is_in_scope = current_scope.is_some() && current_scope == endpoint_scope;

                // Add nested models
                for nested in endpoint.nested_models() {
                    if let Some(nested_name) = nested.ident() {
                        items.push(make_completion(nested_name, lsp::CompletionItemKind::STRUCT, is_in_scope));
                    }
                }
                // Add nested enums
                for nested in endpoint.nested_enums() {
                    if let Some(nested_name) = nested.ident() {
                        items.push(make_completion(nested_name, lsp::CompletionItemKind::ENUM, is_in_scope));
                    }
                }
            }
        }

        // Add top-level enums
        for enum_def in root.top_level_enums() {
            if let Some(name) = enum_def.ident() {
                items.push(make_completion(name, lsp::CompletionItemKind::ENUM, false));
            }
        }

        // Add imported symbols and aliases
        for import_stmt in root.top_level_imports() {
            let Some(import_path) = import_stmt.source_path() else {
                continue;
            };
            let Some(target_uri) = Self::resolve_import_uri(&uri, &import_path) else {
                continue;
            };

            if import_stmt.is_wildcard() {
                if let Some(alias) = import_stmt.wildcard_alias() {
                    items.push(make_completion(alias, lsp::CompletionItemKind::MODULE, false));
                } else {
                    for (name, kind) in self.top_level_symbol_completions(&target_uri) {
                        items.push(make_completion(name, kind, false));
                    }
                }
            } else {
                let exported = self.top_level_symbol_completions(&target_uri);
                for (imported_name, alias_opt) in import_stmt.named_item_specs() {
                    let visible_name = alias_opt.unwrap_or_else(|| imported_name.clone());
                    let kind = exported.iter().find(|(name, _)| *name == imported_name).map(|(_, k)| *k).unwrap_or(lsp::CompletionItemKind::CLASS);
                    items.push(make_completion(visible_name, kind, false));
                }
            }
        }

        let mut seen = HashSet::new();
        items.retain(|item| seen.insert(item.label.clone()));

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

    fn position_of(haystack: &str, needle: &str) -> lsp::Position {
        let offset = haystack.find(needle).expect("needle not found in test source");
        let mut line = 0u32;
        let mut col = 0u32;
        for ch in haystack[..offset].chars() {
            if ch == '\n' {
                line += 1;
                col = 0;
            } else {
                col += 1;
            }
        }
        lsp::Position { line, character: col + 1 }
    }

    fn position_of_last(haystack: &str, needle: &str) -> lsp::Position {
        let offset = haystack.rfind(needle).expect("needle not found in test source");
        let mut line = 0u32;
        let mut col = 0u32;
        for ch in haystack[..offset].chars() {
            if ch == '\n' {
                line += 1;
                col = 0;
            } else {
                col += 1;
            }
        }
        lsp::Position { line, character: col + 1 }
    }

    fn position_after_last(haystack: &str, needle: &str) -> lsp::Position {
        let offset = haystack.rfind(needle).expect("needle not found in test source") + needle.len();
        let mut line = 0u32;
        let mut col = 0u32;
        for ch in haystack[..offset].chars() {
            if ch == '\n' {
                line += 1;
                col = 0;
            } else {
                col += 1;
            }
        }
        lsp::Position { line, character: col }
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

        // Position of "Bar" in "bar: Bar"
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

        // Position of "Mode" in "mode: Mode"
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

        // Position of "RawData" in "raw_data: RawData"
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

        assert_eq!(loc.uri, uri.parse().unwrap());
        assert_eq!(loc.range.start.line, 3, "should jump to nested model definition");
    }

    #[tokio::test]
    async fn test_goto_definition_named_import_reference() {
        let unique = std::time::SystemTime::now().duration_since(std::time::UNIX_EPOCH).unwrap().as_nanos();
        let temp_dir = std::env::temp_dir().join(format!("glue_lsp_import_named_{unique}"));
        std::fs::create_dir_all(&temp_dir).unwrap();

        let imported_path = temp_dir.join("aws_models.glue");
        let imported_src = "model SomeModel {\n    id: string\n}\n";
        std::fs::write(&imported_path, imported_src).unwrap();

        let main_path = temp_dir.join("main.glue");
        let main_src = "import { SomeModel } from \"./aws_models.glue\"\nmodel Root {\n    item: SomeModel\n}\n";
        std::fs::write(&main_path, main_src).unwrap();

        let uri = lsp::Url::from_file_path(&main_path).unwrap();
        let imported_uri = lsp::Url::from_file_path(&imported_path).unwrap();

        let lsp = Lsp::default();
        lsp.did_open(lsp::DidOpenTextDocumentParams {
            text_document: lsp::TextDocumentItem {
                uri: uri.clone(),
                language_id: "glue".into(),
                version: 1,
                text: main_src.into(),
            },
        })
        .await;

        let params = lsp::GotoDefinitionParams {
            work_done_progress_params: Default::default(),
            partial_result_params: Default::default(),
            text_document_position_params: lsp::TextDocumentPositionParams {
                text_document: lsp::TextDocumentIdentifier { uri: uri.clone() },
                position: position_of_last(main_src, "SomeModel"),
            },
        };

        let resp = lsp.goto_definition(params).await;
        let Ok(Some(lsp::GotoDefinitionResponse::Scalar(loc))) = &resp else {
            panic!("unexpected response: {resp:?}");
        };

        let actual_path = loc.uri.to_file_path().unwrap().canonicalize().unwrap();
        let expected_path = imported_uri.to_file_path().unwrap().canonicalize().unwrap();
        assert_eq!(actual_path, expected_path);
        assert_eq!(loc.range.start.line, 0);
    }

    #[tokio::test]
    async fn test_goto_definition_wildcard_alias_import_reference() {
        let unique = std::time::SystemTime::now().duration_since(std::time::UNIX_EPOCH).unwrap().as_nanos();
        let temp_dir = std::env::temp_dir().join(format!("glue_lsp_import_alias_{unique}"));
        std::fs::create_dir_all(&temp_dir).unwrap();

        let imported_path = temp_dir.join("models.glue");
        let imported_src = "model Account {\n    id: string\n}\n";
        std::fs::write(&imported_path, imported_src).unwrap();

        let main_path = temp_dir.join("main.glue");
        let main_src = "import * as Models from \"./models.glue\"\nmodel Root {\n    account: Models.Account\n}\n";
        std::fs::write(&main_path, main_src).unwrap();

        let uri = lsp::Url::from_file_path(&main_path).unwrap();
        let imported_uri = lsp::Url::from_file_path(&imported_path).unwrap();

        let lsp = Lsp::default();
        lsp.did_open(lsp::DidOpenTextDocumentParams {
            text_document: lsp::TextDocumentItem {
                uri: uri.clone(),
                language_id: "glue".into(),
                version: 1,
                text: main_src.into(),
            },
        })
        .await;

        let params = lsp::GotoDefinitionParams {
            work_done_progress_params: Default::default(),
            partial_result_params: Default::default(),
            text_document_position_params: lsp::TextDocumentPositionParams {
                text_document: lsp::TextDocumentIdentifier { uri: uri.clone() },
                position: position_of(main_src, "Account"),
            },
        };

        let resp = lsp.goto_definition(params).await;
        let Ok(Some(lsp::GotoDefinitionResponse::Scalar(loc))) = &resp else {
            panic!("unexpected response: {resp:?}");
        };

        let actual_path = loc.uri.to_file_path().unwrap().canonicalize().unwrap();
        let expected_path = imported_uri.to_file_path().unwrap().canonicalize().unwrap();
        assert_eq!(actual_path, expected_path);
        assert_eq!(loc.range.start.line, 0);
    }

    #[tokio::test]
    async fn test_goto_definition_import_path_jumps_to_file() {
        let unique = std::time::SystemTime::now().duration_since(std::time::UNIX_EPOCH).unwrap().as_nanos();
        let temp_dir = std::env::temp_dir().join(format!("glue_lsp_import_path_def_{unique}"));
        std::fs::create_dir_all(&temp_dir).unwrap();

        let imported_path = temp_dir.join("models.glue");
        let imported_src = "model Account {\n    id: string\n}\n";
        std::fs::write(&imported_path, imported_src).unwrap();

        let main_path = temp_dir.join("main.glue");
        let main_src = "import * as Dep02 from \"./models.glue\"\nmodel Root {\n    account: Dep02.Account\n}\n";
        std::fs::write(&main_path, main_src).unwrap();

        let uri = lsp::Url::from_file_path(&main_path).unwrap();
        let imported_uri = lsp::Url::from_file_path(&imported_path).unwrap();

        let lsp = Lsp::default();
        lsp.did_open(lsp::DidOpenTextDocumentParams {
            text_document: lsp::TextDocumentItem {
                uri: uri.clone(),
                language_id: "glue".into(),
                version: 1,
                text: main_src.into(),
            },
        })
        .await;

        let params = lsp::GotoDefinitionParams {
            work_done_progress_params: Default::default(),
            partial_result_params: Default::default(),
            text_document_position_params: lsp::TextDocumentPositionParams {
                text_document: lsp::TextDocumentIdentifier { uri: uri.clone() },
                position: position_of(main_src, "models.glue"),
            },
        };

        let resp = lsp.goto_definition(params).await;
        let (loc, origin_selection) = match resp {
            Ok(Some(lsp::GotoDefinitionResponse::Scalar(loc))) => (loc, None),
            Ok(Some(lsp::GotoDefinitionResponse::Link(links))) => {
                let link = links.first().expect("expected at least one location link");
                (
                    lsp::Location {
                        uri: link.target_uri.clone(),
                        range: link.target_selection_range,
                    },
                    link.origin_selection_range,
                )
            }
            _ => panic!("unexpected response: {resp:?}"),
        };

        if let Some(origin) = origin_selection {
            let first_line = main_src.lines().next().unwrap();
            let expected_start = first_line.find("./models.glue").unwrap() as u32;
            let expected_end = expected_start + "./models.glue".len() as u32;
            assert_eq!(origin.start.line, 0);
            assert_eq!(origin.start.character, expected_start);
            assert_eq!(origin.end.character, expected_end);
        }

        let actual_path = loc.uri.to_file_path().unwrap().canonicalize().unwrap();
        let expected_path = imported_uri.to_file_path().unwrap().canonicalize().unwrap();
        assert_eq!(actual_path, expected_path);
        assert_eq!(loc.range.start.line, 0);
        assert_eq!(loc.range.start.character, 0);
    }

    #[tokio::test]
    async fn test_goto_definition_named_import_path_jumps_to_file() {
        let unique = std::time::SystemTime::now().duration_since(std::time::UNIX_EPOCH).unwrap().as_nanos();
        let temp_dir = std::env::temp_dir().join(format!("glue_lsp_named_import_path_def_{unique}"));
        std::fs::create_dir_all(&temp_dir).unwrap();

        let imported_path = temp_dir.join("models.glue");
        let imported_src = "model Account {\n    id: string\n}\n";
        std::fs::write(&imported_path, imported_src).unwrap();

        let main_path = temp_dir.join("main.glue");
        let main_src = "import { Account } from \"./models.glue\"\nmodel Root {\n    account: Account\n}\n";
        std::fs::write(&main_path, main_src).unwrap();

        let uri = lsp::Url::from_file_path(&main_path).unwrap();
        let imported_uri = lsp::Url::from_file_path(&imported_path).unwrap();

        let lsp = Lsp::default();
        lsp.did_open(lsp::DidOpenTextDocumentParams {
            text_document: lsp::TextDocumentItem {
                uri: uri.clone(),
                language_id: "glue".into(),
                version: 1,
                text: main_src.into(),
            },
        })
        .await;

        let params = lsp::GotoDefinitionParams {
            work_done_progress_params: Default::default(),
            partial_result_params: Default::default(),
            text_document_position_params: lsp::TextDocumentPositionParams {
                text_document: lsp::TextDocumentIdentifier { uri: uri.clone() },
                position: position_of(main_src, "models.glue"),
            },
        };

        let resp = lsp.goto_definition(params).await;
        let (loc, origin_selection) = match resp {
            Ok(Some(lsp::GotoDefinitionResponse::Scalar(loc))) => (loc, None),
            Ok(Some(lsp::GotoDefinitionResponse::Link(links))) => {
                let link = links.first().expect("expected at least one location link");
                (
                    lsp::Location {
                        uri: link.target_uri.clone(),
                        range: link.target_selection_range,
                    },
                    link.origin_selection_range,
                )
            }
            _ => panic!("unexpected response: {resp:?}"),
        };

        if let Some(origin) = origin_selection {
            let first_line = main_src.lines().next().unwrap();
            let expected_start = first_line.find("./models.glue").unwrap() as u32;
            let expected_end = expected_start + "./models.glue".len() as u32;
            assert_eq!(origin.start.line, 0);
            assert_eq!(origin.start.character, expected_start);
            assert_eq!(origin.end.character, expected_end);
        }

        let actual_path = loc.uri.to_file_path().unwrap().canonicalize().unwrap();
        let expected_path = imported_uri.to_file_path().unwrap().canonicalize().unwrap();
        assert_eq!(actual_path, expected_path);
        assert_eq!(loc.range.start.line, 0);
        assert_eq!(loc.range.start.character, 0);
    }

    #[tokio::test]
    async fn test_goto_definition_namespace_alias_points_to_import() {
        let unique = std::time::SystemTime::now().duration_since(std::time::UNIX_EPOCH).unwrap().as_nanos();
        let temp_dir = std::env::temp_dir().join(format!("glue_lsp_import_ns_alias_{unique}"));
        std::fs::create_dir_all(&temp_dir).unwrap();

        let imported_path = temp_dir.join("models.glue");
        let imported_src = "model Account {\n    id: string\n}\n";
        std::fs::write(&imported_path, imported_src).unwrap();

        let main_path = temp_dir.join("main.glue");
        let main_src = "import * as Dep02 from \"./models.glue\"\nmodel Root {\n    account: Dep02.Account\n}\n";
        std::fs::write(&main_path, main_src).unwrap();

        let uri = lsp::Url::from_file_path(&main_path).unwrap();

        let lsp = Lsp::default();
        lsp.did_open(lsp::DidOpenTextDocumentParams {
            text_document: lsp::TextDocumentItem {
                uri: uri.clone(),
                language_id: "glue".into(),
                version: 1,
                text: main_src.into(),
            },
        })
        .await;

        let params = lsp::GotoDefinitionParams {
            work_done_progress_params: Default::default(),
            partial_result_params: Default::default(),
            text_document_position_params: lsp::TextDocumentPositionParams {
                text_document: lsp::TextDocumentIdentifier { uri: uri.clone() },
                position: position_of_last(main_src, "Dep02"),
            },
        };

        let resp = lsp.goto_definition(params).await;
        let Ok(Some(lsp::GotoDefinitionResponse::Scalar(loc))) = &resp else {
            panic!("unexpected response: {resp:?}");
        };

        assert_eq!(loc.uri, uri);
        assert_eq!(loc.range.start.line, 0);
    }

    #[tokio::test]
    async fn test_goto_definition_deeply_nested_types() {
        let src = indoc! {r#"
            model GlueConfigSchema {
                generation?: Generation

                model Generation {
                    watermark?: Watermark

                    enum Watermark: "full" | "short" | "none"

                    python_pydantic?: PythonPydantic
                    model PythonPydantic {
                        base_model?: string
                    }
                }
            }
        "#}
        .trim();

        let (lsp, uri) = setup_lsp(src).await;

        // Jump to "Generation" from "generation?: Generation"
        let params = lsp::GotoDefinitionParams {
            work_done_progress_params: Default::default(),
            partial_result_params: Default::default(),
            text_document_position_params: lsp::TextDocumentPositionParams {
                text_document: lsp::TextDocumentIdentifier { uri: uri.parse().unwrap() },
                position: lsp::Position { line: 1, character: 17 },
            },
        };
        let resp = lsp.goto_definition(params).await;
        let Ok(Some(lsp::GotoDefinitionResponse::Scalar(loc))) = &resp else {
            panic!("goto Generation: unexpected response: {resp:?}");
        };
        assert_eq!(loc.range.start.line, 3, "should jump to nested Generation model");

        // Jump to "Watermark" from "watermark?: Watermark"
        let params = lsp::GotoDefinitionParams {
            work_done_progress_params: Default::default(),
            partial_result_params: Default::default(),
            text_document_position_params: lsp::TextDocumentPositionParams {
                text_document: lsp::TextDocumentIdentifier { uri: uri.parse().unwrap() },
                position: lsp::Position { line: 4, character: 20 },
            },
        };
        eprintln!("Testing goto Watermark at line 4, char 15");
        let resp = lsp.goto_definition(params).await;
        let Ok(Some(lsp::GotoDefinitionResponse::Scalar(loc))) = &resp else {
            panic!("goto Watermark: unexpected response: {resp:?}");
        };
        eprintln!("Got location: line {}, char {}", loc.range.start.line, loc.range.start.character);
        assert_eq!(loc.range.start.line, 6, "should jump to nested Watermark enum");

        // Jump to "PythonPydantic" from "python_pydantic?: PythonPydantic"
        let params = lsp::GotoDefinitionParams {
            work_done_progress_params: Default::default(),
            partial_result_params: Default::default(),
            text_document_position_params: lsp::TextDocumentPositionParams {
                text_document: lsp::TextDocumentIdentifier { uri: uri.parse().unwrap() },
                position: lsp::Position { line: 8, character: 26 },
            },
        };
        let resp = lsp.goto_definition(params).await;
        let Ok(Some(lsp::GotoDefinitionResponse::Scalar(loc))) = &resp else {
            panic!("goto PythonPydantic: unexpected response: {resp:?}");
        };
        assert_eq!(loc.range.start.line, 9, "should jump to nested PythonPydantic model");
    }

    #[tokio::test]
    async fn test_goto_definition_nested_model_in_endpoint() {
        let src = indoc! {r#"
            endpoint "GET /listings/{listing_id}" {
                responses: {
                    2XX: Apartment[]
                }

                model Apartment {
                    id: int
                }
            }
        "#}
        .trim();

        let (lsp, uri) = setup_lsp(src).await;

        // Position of "Apartment" in "2XX: Apartment[]"
        let params = lsp::GotoDefinitionParams {
            work_done_progress_params: Default::default(),
            partial_result_params: Default::default(),
            text_document_position_params: lsp::TextDocumentPositionParams {
                text_document: lsp::TextDocumentIdentifier { uri: uri.parse().unwrap() },
                position: lsp::Position { line: 2, character: 13 },
            },
        };
        let resp = lsp.goto_definition(params).await;
        let Ok(Some(lsp::GotoDefinitionResponse::Scalar(loc))) = &resp else {
            panic!("unexpected response: {resp:?}");
        };

        assert_eq!(loc.uri, uri.parse().unwrap());
        assert_eq!(loc.range.start.line, 5, "should jump to nested model definition inside endpoint");
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
    async fn test_completion_includes_nested_types() {
        let src = indoc! {r#"
            model Parent {
                child: Child

                model Child {
                    name: string
                }

                enum ChildStatus: "active" | "inactive"
            }

            endpoint "GET /items" {
                responses: {
                    2XX: Item[]
                }

                model Item {
                    id: int
                }
            }
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
        assert!(labels.contains(&"Parent".to_string()), "should contain Parent model");
        assert!(labels.contains(&"Child".to_string()), "should contain nested Child model");
        assert!(labels.contains(&"ChildStatus".to_string()), "should contain nested ChildStatus enum");
        assert!(labels.contains(&"Item".to_string()), "should contain nested Item model from endpoint");

        // Verify icons are correct
        let child_item = items.iter().find(|i| i.label == "Child").unwrap();
        assert_eq!(child_item.kind, Some(lsp::CompletionItemKind::STRUCT), "models should use STRUCT icon");

        let status_item = items.iter().find(|i| i.label == "ChildStatus").unwrap();
        assert_eq!(status_item.kind, Some(lsp::CompletionItemKind::ENUM), "enums should use ENUM icon");
    }

    #[tokio::test]
    async fn test_completion_prioritizes_in_scope_types() {
        let src = indoc! {r#"
            model Parent {
                child: Child

                model Child {
                    name: string
                }
            }

            model Other {
                x: int
            }
        "#}
        .trim();

        let (lsp, uri) = setup_lsp(src).await;

        // Request completion inside Parent model (on "Child" type reference)
        let params = lsp::CompletionParams {
            text_document_position: lsp::TextDocumentPositionParams {
                text_document: lsp::TextDocumentIdentifier { uri: uri.parse().unwrap() },
                position: lsp::Position { line: 1, character: 11 },
            },
            work_done_progress_params: Default::default(),
            partial_result_params: Default::default(),
            context: None,
        };
        let resp = lsp.completion(params).await;
        let Ok(Some(lsp::CompletionResponse::Array(items))) = &resp else {
            panic!("unexpected response: {resp:?}");
        };

        // Child should be prioritized (lower sort_text) since we're inside Parent
        let child_item = items.iter().find(|i| i.label == "Child").unwrap();
        let other_item = items.iter().find(|i| i.label == "Other").unwrap();

        assert!(
            child_item.sort_text.as_ref().unwrap() < other_item.sort_text.as_ref().unwrap(),
            "nested Child should be prioritized over Other when inside Parent scope"
        );
    }

    #[tokio::test]
    async fn test_completion_includes_imported_symbols_and_aliases() {
        let unique = std::time::SystemTime::now().duration_since(std::time::UNIX_EPOCH).unwrap().as_nanos();
        let temp_dir = std::env::temp_dir().join(format!("glue_lsp_completion_imports_{unique}"));
        std::fs::create_dir_all(&temp_dir).unwrap();

        std::fs::write(temp_dir.join("imports_dep01.glue"), "model SomeModel {\n  id: string\n}\n").unwrap();
        std::fs::write(temp_dir.join("imports_dep02.glue"), "model Account {\n  id: string\n}\n").unwrap();
        std::fs::write(
            temp_dir.join("imports_dep03.glue"),
            "model SomeOtherModel {\n  value: string\n}\nmodel AnotherModel {\n  value: string\n}\n",
        )
        .unwrap();

        let main_src = indoc! {r#"
            import * from "./imports_dep01.glue"
            import * as Dep02 from "./imports_dep02.glue"
            import { SomeOtherModel, AnotherModel as RenamedModel } from "./imports_dep03.glue"

            @root
            model Root {
                value: Som
            }
        "#}
        .trim()
        .to_string();
        let main_path = temp_dir.join("main.glue");
        std::fs::write(&main_path, &main_src).unwrap();

        let uri = lsp::Url::from_file_path(&main_path).unwrap();
        let lsp = Lsp::default();
        lsp.did_open(lsp::DidOpenTextDocumentParams {
            text_document: lsp::TextDocumentItem {
                uri: uri.clone(),
                language_id: "glue".into(),
                version: 1,
                text: main_src.clone(),
            },
        })
        .await;

        let params = lsp::CompletionParams {
            text_document_position: lsp::TextDocumentPositionParams {
                text_document: lsp::TextDocumentIdentifier { uri },
                position: position_after_last(&main_src, "Som"),
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
        assert!(labels.contains(&"SomeModel".to_string()), "should include wildcard-imported model");
        assert!(labels.contains(&"SomeOtherModel".to_string()), "should include directly imported model");
        assert!(labels.contains(&"RenamedModel".to_string()), "should include aliased named import");
        assert!(labels.contains(&"Dep02".to_string()), "should include wildcard namespace alias");
    }

    #[tokio::test]
    async fn test_completion_for_namespace_members() {
        let unique = std::time::SystemTime::now().duration_since(std::time::UNIX_EPOCH).unwrap().as_nanos();
        let temp_dir = std::env::temp_dir().join(format!("glue_lsp_completion_ns_{unique}"));
        std::fs::create_dir_all(&temp_dir).unwrap();

        std::fs::write(temp_dir.join("imports_dep02.glue"), "model Account {\n  id: string\n}\nenum Status: \"ok\" | \"bad\"\n").unwrap();

        let main_src = indoc! {r#"
            import * as Dep02 from "./imports_dep02.glue"

            model Root {
                namespaced: Dep02.Acc
            }
        "#}
        .trim()
        .to_string();
        let main_path = temp_dir.join("main.glue");
        std::fs::write(&main_path, &main_src).unwrap();

        let uri = lsp::Url::from_file_path(&main_path).unwrap();
        let lsp = Lsp::default();
        lsp.did_open(lsp::DidOpenTextDocumentParams {
            text_document: lsp::TextDocumentItem {
                uri: uri.clone(),
                language_id: "glue".into(),
                version: 1,
                text: main_src.clone(),
            },
        })
        .await;

        let params = lsp::CompletionParams {
            text_document_position: lsp::TextDocumentPositionParams {
                text_document: lsp::TextDocumentIdentifier { uri },
                position: position_after_last(&main_src, "Dep02."),
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
        assert!(labels.contains(&"Account".to_string()), "should include members of wildcard alias import");
        assert!(labels.contains(&"Status".to_string()), "should include enum members from aliased import file");
    }

    #[tokio::test]
    async fn test_completion_for_namespace_members_on_dot_only() {
        let unique = std::time::SystemTime::now().duration_since(std::time::UNIX_EPOCH).unwrap().as_nanos();
        let temp_dir = std::env::temp_dir().join(format!("glue_lsp_completion_ns_dot_{unique}"));
        std::fs::create_dir_all(&temp_dir).unwrap();

        std::fs::write(temp_dir.join("imports_dep02.glue"), "model Account {\n  id: string\n}\nenum Status: \"ok\" | \"bad\"\n").unwrap();

        let main_src = indoc! {r#"
            import * as Dep02 from "./imports_dep02.glue"

            model Root {
                namespaced: Dep02.
            }
        "#}
        .trim()
        .to_string();
        let main_path = temp_dir.join("main.glue");
        std::fs::write(&main_path, &main_src).unwrap();

        let uri = lsp::Url::from_file_path(&main_path).unwrap();
        let lsp = Lsp::default();
        lsp.did_open(lsp::DidOpenTextDocumentParams {
            text_document: lsp::TextDocumentItem {
                uri: uri.clone(),
                language_id: "glue".into(),
                version: 1,
                text: main_src.clone(),
            },
        })
        .await;

        let params = lsp::CompletionParams {
            text_document_position: lsp::TextDocumentPositionParams {
                text_document: lsp::TextDocumentIdentifier { uri },
                position: position_after_last(&main_src, "Dep02."),
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
        assert!(labels.contains(&"Account".to_string()), "should include model members when completing on namespace dot");
        assert!(labels.contains(&"Status".to_string()), "should include enum members when completing on namespace dot");
    }

    #[tokio::test]
    async fn test_completion_for_import_paths() {
        let unique = std::time::SystemTime::now().duration_since(std::time::UNIX_EPOCH).unwrap().as_nanos();
        let temp_dir = std::env::temp_dir().join(format!("glue_lsp_completion_import_paths_{unique}"));
        std::fs::create_dir_all(&temp_dir).unwrap();
        std::fs::create_dir_all(temp_dir.join("sub")).unwrap();
        std::fs::write(temp_dir.join("imports_dep01.glue"), "model A { x: string }\n").unwrap();
        std::fs::write(temp_dir.join("sub").join("imports_dep02.glue"), "model B { x: string }\n").unwrap();

        let main_src = indoc! {r#"
            import * from "./imp"

            @root
            model Root {
                x: string
            }
        "#}
        .trim()
        .to_string();
        let main_path = temp_dir.join("main.glue");
        std::fs::write(&main_path, &main_src).unwrap();

        let uri = lsp::Url::from_file_path(&main_path).unwrap();
        let lsp = Lsp::default();
        lsp.did_open(lsp::DidOpenTextDocumentParams {
            text_document: lsp::TextDocumentItem {
                uri: uri.clone(),
                language_id: "glue".into(),
                version: 1,
                text: main_src.clone(),
            },
        })
        .await;

        let params = lsp::CompletionParams {
            text_document_position: lsp::TextDocumentPositionParams {
                text_document: lsp::TextDocumentIdentifier { uri },
                position: position_after_last(&main_src, "./imp"),
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
        assert!(labels.contains(&"./imports_dep01.glue".to_string()), "should suggest matching .glue file path");
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

    #[tokio::test]
    async fn test_collect_diagnostics_for_parse_error() {
        let (lsp, uri) = setup_lsp("model Foo {").await;
        let diagnostics = lsp.collect_document_diagnostics(&uri);

        assert!(!diagnostics.is_empty(), "expected parse diagnostics");
        assert!(
            diagnostics.iter().any(|d| d.severity == Some(lsp::DiagnosticSeverity::ERROR)),
            "expected at least one error severity diagnostic"
        );
    }

    #[tokio::test]
    async fn test_collect_diagnostics_for_semantic_error() {
        let src = indoc! {r#"
            model Foo {
                bar: UnknownType
            }
        "#}
        .trim();
        let (lsp, uri) = setup_lsp(src).await;
        let diagnostics = lsp.collect_document_diagnostics(&uri);

        assert!(!diagnostics.is_empty(), "expected semantic diagnostics");
        assert!(diagnostics.iter().any(|d| d.message.contains("Undefined type reference")), "expected undefined type diagnostic");
    }

    #[tokio::test]
    async fn test_collect_diagnostics_resolves_imported_types() {
        let unique = std::time::SystemTime::now().duration_since(std::time::UNIX_EPOCH).unwrap().as_nanos();
        let temp_dir = std::env::temp_dir().join(format!("glue_lsp_import_diags_{unique}"));
        std::fs::create_dir_all(&temp_dir).unwrap();

        let dep01_path = temp_dir.join("imports_dep01.glue");
        std::fs::write(&dep01_path, "model SomeModel {\n  id: string\n}\n").unwrap();

        let dep02_path = temp_dir.join("imports_dep02.glue");
        std::fs::write(&dep02_path, "model Account {\n  id: string\n}\n").unwrap();

        let dep03_path = temp_dir.join("imports_dep03.glue");
        std::fs::write(&dep03_path, "model SomeOtherModel {\n  value: string\n}\n").unwrap();

        let main_src = indoc! {r#"
            import * from "./imports_dep01.glue"
            import * as Dep02 from "./imports_dep02.glue"
            import { SomeOtherModel } from "./imports_dep03.glue"

            @root
            model Root {
                wildcard: SomeModel
                direct: SomeOtherModel
                namespaced: Dep02.Account
            }
        "#}
        .trim()
        .to_string();

        let main_path = temp_dir.join("main.glue");
        std::fs::write(&main_path, &main_src).unwrap();

        let uri = lsp::Url::from_file_path(&main_path).unwrap();
        let lsp = Lsp::default();
        lsp.did_open(lsp::DidOpenTextDocumentParams {
            text_document: lsp::TextDocumentItem {
                uri: uri.clone(),
                language_id: "glue".into(),
                version: 1,
                text: main_src,
            },
        })
        .await;

        let diagnostics = lsp.collect_document_diagnostics(uri.as_str());
        assert!(
            !diagnostics.iter().any(|d| d.message.contains("Undefined type reference")),
            "imported types should not emit undefined type diagnostics: {diagnostics:?}"
        );
    }

    #[tokio::test]
    async fn test_collect_diagnostics_for_missing_import_path() {
        let unique = std::time::SystemTime::now().duration_since(std::time::UNIX_EPOCH).unwrap().as_nanos();
        let temp_dir = std::env::temp_dir().join(format!("glue_lsp_missing_import_diag_{unique}"));
        std::fs::create_dir_all(&temp_dir).unwrap();

        let main_src = indoc! {r#"
            import * from "./does_not_exist.glue"

            @root
            model Root {
                x: string
            }
        "#}
        .trim()
        .to_string();

        let main_path = temp_dir.join("main.glue");
        std::fs::write(&main_path, &main_src).unwrap();

        let uri = lsp::Url::from_file_path(&main_path).unwrap();
        let lsp = Lsp::default();
        lsp.did_open(lsp::DidOpenTextDocumentParams {
            text_document: lsp::TextDocumentItem {
                uri: uri.clone(),
                language_id: "glue".into(),
                version: 1,
                text: main_src,
            },
        })
        .await;

        let diagnostics = lsp.collect_document_diagnostics(uri.as_str());
        assert!(
            diagnostics.iter().any(|d| d.message.contains("Import path does not exist")),
            "expected informative missing-import diagnostic, got: {diagnostics:?}"
        );
    }

    #[tokio::test]
    async fn test_completion_works_with_unresolved_type_reference() {
        // Regression test: completions must be returned even when the document contains an
        // unresolved type reference (e.g. the user is mid-way through typing "ErrorResponse").
        let src = indoc! {r#"
            model Foo {
                bar: ErrorRes
            }

            model ErrorResponse {
                code: int
                message: string
            }
        "#}
        .trim();

        let (lsp, uri) = setup_lsp(src).await;

        let params = lsp::CompletionParams {
            text_document_position: lsp::TextDocumentPositionParams {
                text_document: lsp::TextDocumentIdentifier { uri: uri.parse().unwrap() },
                position: lsp::Position { line: 1, character: 14 },
            },
            work_done_progress_params: Default::default(),
            partial_result_params: Default::default(),
            context: None,
        };
        let resp = lsp.completion(params).await;
        let Ok(Some(lsp::CompletionResponse::Array(items))) = &resp else {
            panic!("completion should succeed even with an unresolved type reference, got: {resp:?}");
        };

        let labels: Vec<String> = items.iter().map(|item| item.label.clone()).collect();
        assert!(labels.contains(&"ErrorResponse".to_string()), "should contain ErrorResponse as a completion candidate");
        assert!(labels.contains(&"Foo".to_string()), "should contain Foo model");
    }

    #[tokio::test]
    async fn test_hover_on_decorator_name() {
        let src = indoc! {r#"
            model Foo {
                @field(alias = "my-name")
                name: string
            }
        "#}
        .trim();

        let (lsp, uri) = setup_lsp(src).await;

        // Hover on "field" in "@field(...)" - line 1, "field" starts at char 5
        let params = lsp::HoverParams {
            work_done_progress_params: Default::default(),
            text_document_position_params: lsp::TextDocumentPositionParams {
                text_document: lsp::TextDocumentIdentifier { uri: uri.parse().unwrap() },
                position: lsp::Position { line: 1, character: 6 },
            },
        };
        let resp = lsp.hover(params).await;
        let Ok(Some(hover)) = &resp else {
            panic!("expected hover response for decorator name, got: {resp:?}");
        };

        let lsp::HoverContents::Markup(markup) = &hover.contents else {
            panic!("expected markup content");
        };
        assert_eq!(markup.kind, lsp::MarkupKind::Markdown);
        assert!(markup.value.contains("@field"), "should contain decorator name");
        assert!(markup.value.contains("alias"), "should mention arguments");
    }

    #[tokio::test]
    async fn test_hover_on_namespace_alias() {
        let src = indoc! {r#"
            import * as Dep02 from "./imports_dep02.glue"

            model Root {
                account: Dep02.Account
            }
        "#}
        .trim();

        let (lsp, uri) = setup_lsp(src).await;

        let params = lsp::HoverParams {
            work_done_progress_params: Default::default(),
            text_document_position_params: lsp::TextDocumentPositionParams {
                text_document: lsp::TextDocumentIdentifier { uri: uri.parse().unwrap() },
                position: position_of_last(src, "Dep02"),
            },
        };
        let resp = lsp.hover(params).await;
        let Ok(Some(hover)) = &resp else {
            panic!("expected hover response for namespace alias, got: {resp:?}");
        };

        let lsp::HoverContents::Markup(markup) = &hover.contents else {
            panic!("expected markup content");
        };
        assert!(markup.value.contains("namespace Dep02"), "should contain namespace heading");
        assert!(markup.value.contains("imports_dep02.glue"), "should mention imported file path");
    }

    #[tokio::test]
    async fn test_hover_on_named_imported_symbol() {
        let unique = std::time::SystemTime::now().duration_since(std::time::UNIX_EPOCH).unwrap().as_nanos();
        let temp_dir = std::env::temp_dir().join(format!("glue_lsp_hover_named_import_{unique}"));
        std::fs::create_dir_all(&temp_dir).unwrap();

        let dep_path = temp_dir.join("models.glue");
        let dep_src = "/// Imported account model\nmodel Account {\n  id: string\n}\n";
        std::fs::write(&dep_path, dep_src).unwrap();

        let main_src = indoc! {r#"
            import { Account } from "./models.glue"

            model Root {
                account: Account
            }
        "#}
        .trim()
        .to_string();
        let main_path = temp_dir.join("main.glue");
        std::fs::write(&main_path, &main_src).unwrap();

        let uri = lsp::Url::from_file_path(&main_path).unwrap();
        let lsp = Lsp::default();
        lsp.did_open(lsp::DidOpenTextDocumentParams {
            text_document: lsp::TextDocumentItem {
                uri: uri.clone(),
                language_id: "glue".into(),
                version: 1,
                text: main_src.clone(),
            },
        })
        .await;

        let params = lsp::HoverParams {
            work_done_progress_params: Default::default(),
            text_document_position_params: lsp::TextDocumentPositionParams {
                text_document: lsp::TextDocumentIdentifier { uri },
                position: position_of_last(&main_src, "Account"),
            },
        };

        let resp = lsp.hover(params).await;
        let Ok(Some(hover)) = &resp else {
            panic!("expected hover response for named imported symbol, got: {resp:?}");
        };

        let lsp::HoverContents::Markup(markup) = &hover.contents else {
            panic!("expected markup content");
        };
        assert!(markup.value.contains("model Account"), "should contain imported model name");
        assert!(markup.value.contains("Imported account model"), "should contain imported model docs");
    }

    #[tokio::test]
    async fn test_hover_on_namespaced_imported_symbol_member() {
        let unique = std::time::SystemTime::now().duration_since(std::time::UNIX_EPOCH).unwrap().as_nanos();
        let temp_dir = std::env::temp_dir().join(format!("glue_lsp_hover_ns_import_{unique}"));
        std::fs::create_dir_all(&temp_dir).unwrap();

        let dep_path = temp_dir.join("models.glue");
        let dep_src = "/// Imported status enum\nenum Status: \"ok\" | \"bad\"\n";
        std::fs::write(&dep_path, dep_src).unwrap();

        let main_src = indoc! {r#"
            import * as Dep02 from "./models.glue"

            model Root {
                status: Dep02.Status
            }
        "#}
        .trim()
        .to_string();
        let main_path = temp_dir.join("main.glue");
        std::fs::write(&main_path, &main_src).unwrap();

        let uri = lsp::Url::from_file_path(&main_path).unwrap();
        let lsp = Lsp::default();
        lsp.did_open(lsp::DidOpenTextDocumentParams {
            text_document: lsp::TextDocumentItem {
                uri: uri.clone(),
                language_id: "glue".into(),
                version: 1,
                text: main_src.clone(),
            },
        })
        .await;

        let params = lsp::HoverParams {
            work_done_progress_params: Default::default(),
            text_document_position_params: lsp::TextDocumentPositionParams {
                text_document: lsp::TextDocumentIdentifier { uri },
                position: position_of_last(&main_src, "Status"),
            },
        };

        let resp = lsp.hover(params).await;
        let Ok(Some(hover)) = &resp else {
            panic!("expected hover response for namespaced imported symbol, got: {resp:?}");
        };

        let lsp::HoverContents::Markup(markup) = &hover.contents else {
            panic!("expected markup content");
        };
        assert!(markup.value.contains("enum Status"), "should contain imported enum name");
        assert!(markup.value.contains("ok"), "should contain imported enum variants");
        assert!(markup.value.contains("Imported status enum"), "should contain imported enum docs");
    }

    #[tokio::test]
    async fn test_hover_on_decorator_named_arg_key() {
        let src = indoc! {r#"
            model Foo {
                @field(alias = "my-name")
                name: string
            }
        "#}
        .trim();

        let (lsp, uri) = setup_lsp(src).await;

        // Hover on "alias" in "@field(alias = ...)" - line 1, "alias" starts at char 11
        let params = lsp::HoverParams {
            work_done_progress_params: Default::default(),
            text_document_position_params: lsp::TextDocumentPositionParams {
                text_document: lsp::TextDocumentIdentifier { uri: uri.parse().unwrap() },
                position: lsp::Position { line: 1, character: 13 },
            },
        };
        let resp = lsp.hover(params).await;
        let Ok(Some(hover)) = &resp else {
            panic!("expected hover response for decorator arg key, got: {resp:?}");
        };

        let lsp::HoverContents::Markup(markup) = &hover.contents else {
            panic!("expected markup content");
        };
        assert_eq!(markup.kind, lsp::MarkupKind::Markdown);
        assert!(markup.value.contains("alias"), "should contain arg name");
        assert!(markup.value.contains("@field"), "should reference decorator name");
    }

    #[tokio::test]
    async fn test_completion_inside_decorator_args() {
        let src = indoc! {r#"
            model Foo {
                @field(alias = "my-name")
                name: string
            }
        "#}
        .trim();

        let (lsp, uri) = setup_lsp(src).await;

        // Completion at position inside @field(alias = ...) - on "alias".
        // alias is already used so should NOT be in the results; example should be offered.
        let params = lsp::CompletionParams {
            text_document_position: lsp::TextDocumentPositionParams {
                text_document: lsp::TextDocumentIdentifier { uri: uri.parse().unwrap() },
                position: lsp::Position { line: 1, character: 13 },
            },
            work_done_progress_params: Default::default(),
            partial_result_params: Default::default(),
            context: None,
        };
        let resp = lsp.completion(params).await;
        let Ok(Some(lsp::CompletionResponse::Array(items))) = &resp else {
            panic!("expected completion items inside decorator, got: {resp:?}");
        };

        let labels: Vec<String> = items.iter().map(|i| i.label.clone()).collect();
        assert!(labels.contains(&"example".to_string()), "should offer 'example' named arg");
        assert!(!labels.contains(&"alias".to_string()), "should NOT offer 'alias' (already used)");

        // Items should be PROPERTY kind
        let example_item = items.iter().find(|i| i.label == "example").unwrap();
        assert_eq!(example_item.kind, Some(lsp::CompletionItemKind::PROPERTY));
        // Snippet insert text should be present
        assert!(example_item.insert_text.as_deref().unwrap_or("").contains("example="), "insert text should contain 'example='");
    }

    #[tokio::test]
    async fn test_completion_decorator_name_after_at() {
        // Typing `@` alone — should offer all decorators
        let src = indoc! {r#"
            model Foo {
                @
                name: string
            }
        "#}
        .trim();

        let (lsp, uri) = setup_lsp(src).await;

        let params = lsp::CompletionParams {
            text_document_position: lsp::TextDocumentPositionParams {
                text_document: lsp::TextDocumentIdentifier { uri: uri.parse().unwrap() },
                // Line 1: "    @"  — cursor after `@` at char 5
                position: lsp::Position { line: 1, character: 5 },
            },
            work_done_progress_params: Default::default(),
            partial_result_params: Default::default(),
            context: None,
        };
        let resp = lsp.completion(params).await;
        let Ok(Some(lsp::CompletionResponse::Array(items))) = &resp else {
            panic!("expected completion items after @, got: {resp:?}");
        };

        let labels: Vec<String> = items.iter().map(|i| i.label.clone()).collect();
        assert!(labels.contains(&"@field".to_string()), "should offer @field");

        let field_item = items.iter().find(|i| i.label == "@field").unwrap();
        assert_eq!(field_item.kind, Some(lsp::CompletionItemKind::FUNCTION));
        assert!(field_item.insert_text.as_deref().unwrap_or("").starts_with("field("), "insert text should start with 'field('");
    }

    #[tokio::test]
    async fn test_completion_decorator_name_partial() {
        // Typing `@fie` — should filter to just `@field`
        let src = "model Foo {\n    @fie\n    name: string\n}";

        let (lsp, uri) = setup_lsp(src).await;

        let params = lsp::CompletionParams {
            text_document_position: lsp::TextDocumentPositionParams {
                text_document: lsp::TextDocumentIdentifier { uri: uri.parse().unwrap() },
                // Line 1: "    @fie" — cursor at char 8
                position: lsp::Position { line: 1, character: 8 },
            },
            work_done_progress_params: Default::default(),
            partial_result_params: Default::default(),
            context: None,
        };
        let resp = lsp.completion(params).await;
        let Ok(Some(lsp::CompletionResponse::Array(items))) = &resp else {
            panic!("expected completion items after @fie, got: {resp:?}");
        };

        let labels: Vec<String> = items.iter().map(|i| i.label.clone()).collect();
        assert!(labels.contains(&"@field".to_string()), "@field should match prefix 'fie'");
    }
}
