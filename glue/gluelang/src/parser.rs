use crate::lexer::{Span, Token, TokenKind};
use miette::{Diagnostic, LabeledSpan, NamedSource, SourceCode};
use std::{error::Error, fmt};

#[derive(Debug, Clone)]
pub struct Program {
    pub models: Vec<Model>,
}

#[derive(Debug, Clone)]
pub struct Model {
    pub parent: Option<String>,
    pub name: String,
    pub doc: Option<String>,
    pub fields: Vec<Field>,
    pub span: Span,
}

impl Model {
    pub fn effective_name(&self) -> String {
        if let Some(parent) = &self.parent {
            format!("{}{}", parent, self.name)
        } else {
            self.name.clone()
        }
    }
}

#[derive(Debug, Clone)]
pub struct Field {
    pub name: String,
    pub ty: RawType,
    pub doc: Option<String>,
    pub span: Span,
}

impl fmt::Display for Field {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}: {}", self.name, self.ty)
    }
}

/// Represents a raw type, e.g., `int | string? | #User`.
#[derive(Debug, Clone)]
pub struct RawType {
    pub atoms: Vec<TypeAtom>,
}

/// A single type atom, like `int`, `string?`, or `#User`.
#[derive(Debug, Clone)]
pub struct TypeAtom {
    pub name: String,
    pub optional: bool,
    pub is_ref: bool,
    pub span: Span,
}

impl fmt::Display for RawType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.atoms.iter().map(|a| a.to_string()).collect::<Vec<_>>().join(" | "))
    }
}

impl fmt::Display for TypeAtom {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}{}{}",
            if self.is_ref { "#" } else { "" },
            self.name,
            if self.optional { "?" } else { "" }
        )
    }
}

#[derive(Debug, Clone)]
pub struct ParserError {
    pub span: Span,
    pub message: String,
    pub note: Option<String>,
    pub code: Option<String>,
    pub source: std::sync::Arc<NamedSource<String>>,
}

impl ParserError {
    fn new(file_name: &str, src: &str, span: Span, message: impl Into<String>, note: Option<String>, code: Option<&str>) -> Self {
        Self {
            span,
            message: message.into(),
            note,
            code: code.map(|c| c.to_string()),
            source: std::sync::Arc::new(NamedSource::new(file_name, src.to_string())),
        }
    }
}

impl fmt::Display for ParserError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.message)
    }
}

impl Error for ParserError {}
impl Diagnostic for ParserError {
    fn source_code(&self) -> Option<&dyn SourceCode> {
        Some(&*self.source)
    }
    fn code<'a>(&'a self) -> Option<Box<dyn fmt::Display + 'a>> {
        self.code.as_ref().map(|c| Box::new(c) as Box<dyn fmt::Display>)
    }
    fn help<'a>(&'a self) -> Option<Box<dyn fmt::Display + 'a>> {
        self.note.as_ref().map(|n| Box::new(n) as Box<dyn fmt::Display>)
    }
    fn labels(&self) -> Option<Box<dyn Iterator<Item = LabeledSpan> + '_>> {
        Some(Box::new(std::iter::once(LabeledSpan::at(
            self.span.start..self.span.end.max(self.span.start + 1),
            self.message.clone(),
        ))))
    }
}

pub struct Parser<'a> {
    tokens: Vec<Token<'a>>,
    src: &'a str,
    file_name: &'a str,
    i: usize,
}

impl<'a> Parser<'a> {
    pub fn new(file_name: &'a str, src: &'a str, tokens: Vec<Token<'a>>) -> Self {
        Self {
            tokens,
            src,
            file_name,
            i: 0,
        }
    }

    pub fn parse_program(&mut self) -> Result<Program, ParserError> {
        let mut models = Vec::new();
        while !self.peek_is(&TokenKind::Eof) {
            // Check for model definitions (including doc block superceded by model)
            if self.peek_is(&TokenKind::KeywordModel) || self.peek_ahead_is(1, &TokenKind::KeywordModel) {
                models.extend(self.parse_model()?);
            } else {
                return Err(self.err(self.peek_span(), "expected 'model'", None, Some("EPARSE")));
            }
        }
        Ok(Program { models })
    }

    /// Parses a model definition.
    /// May return more than one model if nested models are found.
    fn parse_model(&mut self) -> Result<Vec<Model>, ParserError> {
        let mut parsed_models = Vec::new();

        let doc = self.consume_doc_blocks();
        self.expect(&TokenKind::KeywordModel)?; // guaranteed keyword
        let start = self.peek_span();
        let name = self.expect_ident()?; // identifier for model name
        self.expect(&TokenKind::LBrace)?; // opening brace

        let mut fields = Vec::new();
        while !self.peek_is(&TokenKind::RBrace) {
            if self.peek_is(&TokenKind::KeywordModel) || self.peek_ahead_is(1, &TokenKind::KeywordModel) {
                // Nested model (we expect only one here)
                let mut nested_models = self.parse_model()?;
                for model in &mut nested_models {
                    model.parent = Some(name.clone());
                }
                parsed_models.extend(nested_models);
            } else {
                // Expect a field otherwise
                fields.push(self.parse_field()?);
            }
        }
        self.expect(&TokenKind::RBrace)?;

        let root_model = Model {
            parent: None,
            name,
            doc,
            fields,
            span: start,
        };
        parsed_models.push(root_model);

        Ok(parsed_models)
    }

    fn parse_field(&mut self) -> Result<Field, ParserError> {
        let doc = self.consume_doc_blocks();
        let start = self.peek_span();
        let name = self.expect_ident()?;
        self.expect(&TokenKind::Colon)?;
        let ty = self.parse_type()?;
        Ok(Field {
            name,
            ty,
            doc,
            span: start,
        })
    }

    fn parse_type(&mut self) -> Result<RawType, ParserError> {
        let mut atoms = vec![self.parse_type_atom()?];
        while self.peek_is(&TokenKind::Pipe) {
            self.advance()?;
            atoms.push(self.parse_type_atom()?);
        }
        Ok(RawType { atoms })
    }

    fn parse_type_atom(&mut self) -> Result<TypeAtom, ParserError> {
        let start_span = self.peek_span();
        let is_ref = self.peek_is(&TokenKind::Hash);
        if is_ref {
            self.advance()?;
        }
        let end_span = self.peek_span();
        let name = self.expect_ident()?;
        let optional = if self.peek_is(&TokenKind::QuestionMark) {
            self.advance()?;
            true
        } else {
            false
        };
        Ok(TypeAtom {
            name,
            optional,
            is_ref,
            span: Span {
                line: start_span.line,
                start: start_span.start,
                end: end_span.end,
                col_start: start_span.col_start,
                col_end: end_span.col_end,
            },
        })
    }

    fn consume_doc_blocks(&mut self) -> Option<String> {
        let mut parts = Vec::new();
        while let TokenKind::DocBlock(lines) = &self.peek().kind {
            parts.push(lines.join("\n"));
            self.advance().ok();
        }
        if parts.is_empty() { None } else { Some(parts.join("\n\n")) }
    }

    fn expect_ident(&mut self) -> Result<String, ParserError> {
        let span = self.peek_span();
        match self.advance()?.kind {
            TokenKind::Ident(s) => Ok(s.to_string()),
            _ => Err(self.err(span, "expected identifier", None, Some("EIDENT"))),
        }
    }

    #[inline]
    fn peek(&self) -> &Token<'a> {
        self.tokens.get(self.i).unwrap_or_else(|| self.tokens.last().unwrap())
    }

    #[inline]
    fn peek_is(&self, k: &TokenKind<'a>) -> bool {
        std::mem::discriminant(&self.peek().kind) == std::mem::discriminant(k)
    }

    fn peek_ahead_is(&self, n: usize, kind: &TokenKind<'a>) -> bool {
        if let Some(t) = self.tokens.get(self.i + n) {
            std::mem::discriminant(&t.kind) == std::mem::discriminant(kind)
        } else {
            false
        }
    }

    fn advance(&mut self) -> Result<&Token<'a>, ParserError> {
        if self.i >= self.tokens.len() {
            return Err(self.err(self.peek_span(), "unexpected end of input", None, Some("EEOF")));
        }
        let t = &self.tokens[self.i];
        self.i += 1;
        Ok(t)
    }

    fn expect(&mut self, want: &TokenKind<'a>) -> Result<&Token<'a>, ParserError> {
        if self.peek_is(want) {
            self.advance()
        } else {
            let span = self.peek_span();
            Err(self.err(
                span,
                format!("unexpected token; expected `{}`, got `{}`", want, self.peek().kind),
                None,
                Some("ETOKEN"),
            ))
        }
    }

    #[inline]
    fn peek_span(&self) -> Span {
        self.peek().span
    }

    fn err(&self, span: Span, msg: impl Into<String>, note: Option<String>, code: Option<&str>) -> ParserError {
        ParserError::new(self.file_name, self.src, span, msg, note, code)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::Lexer;
    use indoc::indoc;

    #[test]
    fn test_parser_with_refs() {
        let src = indoc!("model User {\n    id: string\n    friend: #User?\n    tags: string | #Tag\n}");
        let tokens = Lexer::new(src).lex();
        let mut p = Parser::new("test.glue", src, tokens);
        let program = p.parse_program().unwrap();
        assert_eq!(program.models.len(), 1);
        let m = &program.models[0];
        assert_eq!(m.fields.len(), 3);
        assert!(m.fields[1].ty.atoms[0].is_ref);
        assert!(m.fields[1].ty.atoms[0].optional);
        assert!(m.fields[2].ty.atoms[1].is_ref);
    }

    #[test]
    fn test_spans_basic() {
        let src = indoc! { "
            model User {
                id: string
                name: string?
                friend: #User
            }
        " }
        .trim();
        let tokens = Lexer::new(src).lex();
        let mut p = Parser::new("test.glue", src, tokens);
        let program = p.parse_program().unwrap();

        let m = &program.models[0];
        assert_eq!(m.name, "User");
        assert_eq!(m.span.line, 1);
        assert_eq!(m.span.start, 6);
        assert_eq!(m.span.end, "model User".len());
        assert_eq!(m.span.col_start, "model ".len() + 1);

        let field1 = &m.fields[0];
        assert_eq!(field1.name, "id");
        assert_eq!(field1.span.line, 2);
        assert_eq!(field1.span.start, "model User {\n    ".len());
        assert_eq!(field1.span.end, "model User {\n    id".len());
        assert_eq!(field1.ty.atoms.len(), 1);
        let field1_ty = &field1.ty.atoms[0];
        assert_eq!(field1_ty.name, "string");
        assert!(!field1_ty.optional);
        assert!(!field1_ty.is_ref);
        assert_eq!(field1_ty.span.line, 2);
        assert_eq!(field1_ty.span.start, "model User {\n    id: ".len());
        assert_eq!(field1_ty.span.end, "model User {\n    id: string".len());

        let field2 = &m.fields[1];
        assert_eq!(field2.name, "name");
        assert_eq!(field2.span.line, 3);
        assert_eq!(field2.span.start, "model User {\n    id: string\n    ".len());
        assert_eq!(field2.span.end, "model User {\n    id: string\n    name".len());
    }
}
