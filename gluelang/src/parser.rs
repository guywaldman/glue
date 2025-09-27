use crate::lexer::{Span, Token, TokenKind};
use colored::Colorize;
use miette::{Diagnostic, LabeledSpan, NamedSource, SourceCode};
use std::{error::Error, fmt};

#[allow(dead_code)]
#[derive(Debug, Clone)]
pub struct Program<'a> {
    pub models: Vec<Model<'a>>,
}

#[allow(dead_code)]
#[derive(Debug, Clone)]
pub struct Model<'a> {
    pub name: &'a str,
    pub doc: Option<String>,
    pub fields: Vec<Field<'a>>,
    pub span: Span,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum FieldType {
    String,
    Int,
    Bool,
    Float,
}

impl FieldType {
    const STRING: &'static str = "string";
    const INT: &'static str = "int";
    const BOOL: &'static str = "bool";
    const FLOAT: &'static str = "float";

    fn from_str(s: &str) -> Option<Self> {
        Some(match s {
            Self::STRING => Self::String,
            Self::INT => Self::Int,
            Self::BOOL => Self::Bool,
            Self::FLOAT => Self::Float,
            _ => return None,
        })
    }
}

impl fmt::Display for FieldType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let s = match self {
            Self::String => Self::STRING,
            Self::Int => Self::INT,
            Self::Bool => Self::BOOL,
            Self::Float => Self::FLOAT,
        };
        write!(f, "{}", s)
    }
}

#[allow(dead_code)]
#[derive(Debug, Clone)]
pub struct Field<'a> {
    pub name: &'a str,
    pub ty: FieldType,
    pub doc: Option<String>,
    pub span: Span,
}

// ================= Errors =================
#[derive(Debug, Clone)]
pub struct ParseError {
    pub span: Span,
    pub message: String,
    pub error_code: Option<String>,
    pub note: Option<String>,
    pub source: NamedSource<String>,
}
impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.message)
    }
}
impl Error for ParseError {}
impl Diagnostic for ParseError {
    fn source_code(&self) -> Option<&dyn SourceCode> {
        Some(&self.source)
    }

    fn code<'a>(&'a self) -> Option<Box<dyn fmt::Display + 'a>> {
        self.error_code
            .as_ref()
            .map(|c| Box::new(c.clone()) as Box<dyn fmt::Display>)
    }

    fn help<'a>(&'a self) -> Option<Box<dyn fmt::Display + 'a>> {
        self.note
            .as_ref()
            .map(|n| Box::new(n.clone()) as Box<dyn fmt::Display>)
    }

    fn labels(&self) -> Option<Box<dyn Iterator<Item = LabeledSpan> + '_>> {
        let s = self.span.start;
        let e = if self.span.end > self.span.start {
            self.span.end
        } else {
            s + 1
        };
        Some(Box::new(std::iter::once(LabeledSpan::at(
            s..e,
            self.message.as_str(),
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

    pub fn parse_program(&mut self) -> Result<Program<'a>, ParseError> {
        let mut models = Vec::new();
        while !self.peek_is(&TokenKind::Eof) {
            models.push(self.parse_model()?);
        }
        Ok(Program { models })
    }

    fn parse_model(&mut self) -> Result<Model<'a>, ParseError> {
        let start = self.peek_span();
        let doc = self.consume_doc_blocks();
        self.expect(&TokenKind::Model)?;
        let name = match &self.advance()?.kind {
            TokenKind::Ident(s) => *s,
            _ => return Err(self.error_prev("expected model name", None, None)),
        };
        self.expect(&TokenKind::LBrace)?;
        let mut fields = Vec::new();
        while !self.peek_is(&TokenKind::RBrace) && !self.peek_is(&TokenKind::Eof) {
            fields.push(self.parse_field()?);
        }
        let end = self.expect(&TokenKind::RBrace)?.span;
        Ok(Model {
            name,
            doc,
            fields,
            span: Span {
                start: start.start,
                end: end.end,
                line: start.line,
                col: start.col,
            },
        })
    }

    fn parse_field(&mut self) -> Result<Field<'a>, ParseError> {
        let doc = self.consume_doc_blocks();
        let start = self.peek_span();
        let name = match self.advance()?.kind {
            TokenKind::Ident(s) => s,
            _ => return Err(self.error_prev("expected field name", None, None)),
        };
        self.expect(&TokenKind::Colon).map_err(|mut err| {
            err.message = "expected ':'".into();
            err.note = Some(format!(
                "Try defining the field type, e.g., {}",
                format!("`{name}: string`").bright_blue().bold()
            ));
            err
        })?;
        let ty = self.parse_field_type()?;
        let end = self.prev_span();
        Ok(Field {
            name,
            ty,
            doc,
            span: Span {
                start: start.start,
                end: end.end,
                line: start.line,
                col: start.col,
            },
        })
    }

    fn parse_field_type(&mut self) -> Result<FieldType, ParseError> {
        let valid_fields = ["string", "int", "bool", "float"].join(", ");
        let ty_ident = match self.advance()?.kind {
            TokenKind::Ident(s) => s,
            _ => {
                return Err(self.error_prev(
                    "expected type",
                    Some(format!("valid types are: {valid_fields}").as_str()),
                    None,
                ));
            }
        };
        FieldType::from_str(ty_ident).ok_or_else(|| {
            self.error_prev(
                "invalid field type",
                Some(format!("valid types are: {valid_fields}").as_str()),
                None,
            )
        })
    }

    fn consume_doc_blocks(&mut self) -> Option<String> {
        let mut parts: Vec<String> = Vec::new();
        loop {
            let kind = self.peek().kind.clone();
            match kind {
                TokenKind::DocBlock(lines) => {
                    // safe to ignore result; advance only fails on EOF which wouldn't produce DocBlock
                    let _ = self.advance();
                    parts.push(lines.join("\n"));
                }
                _ => break,
            }
        }
        if parts.is_empty() {
            None
        } else {
            Some(parts.join("\n\n"))
        }
    }

    fn peek(&self) -> &Token<'a> {
        self.tokens
            .get(self.i)
            .unwrap_or(self.tokens.last().unwrap())
    }

    fn prev(&self) -> &Token<'a> {
        if self.i == 0 {
            &self.tokens[0]
        } else {
            &self.tokens[self.i - 1]
        }
    }

    #[inline]
    fn peek_is(&self, k: &TokenKind<'a>) -> bool {
        std::mem::discriminant(&self.peek().kind) == std::mem::discriminant(k)
    }

    fn advance(&mut self) -> Result<&Token<'a>, ParseError> {
        if self.i >= self.tokens.len() {
            return Err(self.error_here("unexpected end of input", None, None));
        }
        let t = &self.tokens[self.i];
        self.i += 1;
        if let TokenKind::Error(s) = t.kind {
            return Err(self.make_error(t.span, &format!("lex error: {s}"), None, None));
        }
        Ok(t)
    }

    fn expect(&mut self, want: &TokenKind<'a>) -> Result<&Token<'a>, ParseError> {
        if self.peek_is(want) {
            return self.advance();
        }
        Err(self.error_here(
            format!(
                "unexpected token; expected `{}`",
                want.to_string().cyan().bold()
            )
            .as_str(),
            None,
            "unexpected token".into(),
        ))
    }

    #[inline]
    fn error_here(&self, msg: &str, note: Option<&str>, error_code: Option<&str>) -> ParseError {
        self.make_error(self.peek_span(), msg, note, error_code)
    }

    #[inline]
    fn error_prev(&self, msg: &str, note: Option<&str>, error_code: Option<&str>) -> ParseError {
        self.make_error(self.prev_span(), msg, note, error_code)
    }

    #[inline]
    fn make_error(
        &self,
        span: Span,
        msg: &str,
        note: Option<&str>,
        error_code: Option<&str>,
    ) -> ParseError {
        ParseError {
            span,
            message: msg.into(),
            note: note.map(|n| n.into()),
            error_code: error_code.map(|c| c.into()),
            source: NamedSource::new(self.file_name, self.src.to_string()),
        }
    }

    #[inline]
    fn peek_span(&self) -> Span {
        self.peek().span
    }

    #[inline]
    fn prev_span(&self) -> Span {
        self.prev().span
    }
}

// ================= Tests =================
#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::Lexer;
    use indoc::indoc;
    #[test]
    fn test_parser_basic() {
        let src = indoc!(
            "model User {\n    /// doc1\n    id: string\n    /// doc2\n    email: string\n    /// doc3\n    age: int\n}"
        );
        let tokens = Lexer::new(src).lex();
        let mut p = Parser::new("test.glue", src, tokens);
        let program = p.parse_program().unwrap();
        assert_eq!(program.models.len(), 1);
        let m = &program.models[0];
        assert_eq!(m.fields.len(), 3);
        assert_eq!(m.fields[0].doc.as_deref(), Some("doc1"));
    }
    #[test]
    fn test_parser_error_field() {
        let src = indoc!("model User { id: foo }");
        let tokens = Lexer::new(src).lex();
        let mut p = Parser::new("test.glue", src, tokens);
        let err = p.parse_program().expect_err("parse error");
        assert_eq!(err.message, "invalid field type");
    }
    #[test]
    fn test_model_doc() {
        let src = indoc!("/// A user in the system\n/// second line\nmodel User { id: int }");
        let tokens = Lexer::new(src).lex();
        let mut p = Parser::new("test.glue", src, tokens);
        let program = p.parse_program().unwrap();
        assert!(
            program.models[0]
                .doc
                .as_ref()
                .unwrap()
                .contains("second line")
        );
    }
}
