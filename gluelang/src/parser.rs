use crate::lexer::{Span, Token, TokenKind};
use miette::{Diagnostic, LabeledSpan, NamedSource, SourceCode};
use std::{error::Error, fmt, str::FromStr};

// ================= Raw AST (syntax only) =================
#[derive(Debug, Clone)]
pub struct Program<'a> {
    pub models: Vec<Model<'a>>,
    pub endpoints: Vec<Endpoint<'a>>,
}

#[allow(dead_code)]
#[derive(Debug, Clone)]
pub struct Model<'a> {
    pub name: &'a str,
    pub doc: Option<String>,
    pub fields: Vec<Field<'a>>,
    pub span: Span,
}

#[allow(dead_code)]
#[derive(Debug, Clone)]
pub struct Field<'a> {
    pub name: &'a str,
    pub ty: RawType,
    pub doc: Option<String>,
    pub span: Span,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum FieldDecorator {
    // Endpoint field decorators
    Path,
    Query,
    Body,
    Header,
}

impl FromStr for FieldDecorator {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s.to_lowercase().as_str() {
            "path" => Ok(FieldDecorator::Path),
            "query" => Ok(FieldDecorator::Query),
            "body" => Ok(FieldDecorator::Body),
            "header" => Ok(FieldDecorator::Header),
            _ => Err(()),
        }
    }
}

#[derive(Debug, Clone)]
pub struct DecoratedField<'a> {
    pub field: Field<'a>,
    pub decorator: FieldDecorator,
}

#[derive(Debug, Clone)]
pub struct Endpoint<'a> {
    pub doc: Option<String>,
    pub annotation: Option<Annotation<'a>>,
    pub fields: Vec<DecoratedField<'a>>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub enum AnnotationArgument<'a> {
    NamedArg { name: &'a str, value: &'a str },
    PositionalArg { index: usize, value: &'a str },
}

#[derive(Debug, Clone)]
pub struct Annotation<'a> {
    pub name: &'a str,
    pub args: Vec<AnnotationArgument<'a>>,
    pub span: Span,
}

// A type like: int | #User? | string
#[derive(Debug, Clone)]
pub struct RawType {
    pub atoms: Vec<TypeAtom>,
}

#[derive(Debug, Clone)]
pub struct TypeAtom {
    pub is_ref: bool,
    pub name: String,
    pub optional: bool,
    pub span: Span,
}

impl fmt::Display for RawType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let parts: Vec<String> = self.atoms.iter().map(|a| a.to_string()).collect();
        write!(f, "{}", parts.join(" | "))
    }
}
impl fmt::Display for TypeAtom {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.is_ref {
            write!(f, "#{}{}", self.name, if self.optional { "?" } else { "" })
        } else {
            write!(f, "{}{}", self.name, if self.optional { "?" } else { "" })
        }
    }
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
        let mut endpoints = Vec::new();
        while !self.peek_is(&TokenKind::Eof) {
            // Look ahead past any doc blocks to decide what production to parse.
            let mut j = self.i;
            while let Some(Token {
                kind: TokenKind::DocBlock(_),
                ..
            }) = self.tokens.get(j)
            {
                j += 1;
            }
            match self.tokens.get(j).map(|t| &t.kind) {
                Some(TokenKind::Model) => models.push(self.parse_model()?),
                Some(TokenKind::Endpoint) | Some(TokenKind::PoundSign) => {
                    endpoints.push(self.parse_endpoint()?)
                }
                Some(TokenKind::Eof) => break,
                None => break,
                _ => return Err(self.error_here("expected 'model' or 'endpoint'", None, None)),
            }
        }
        Ok(Program { models, endpoints })
    }

    /// Parse an endpoint (syntax only). Annotation & contents are left uninterpreted.
    fn parse_endpoint(&mut self) -> Result<Endpoint<'a>, ParseError> {
        // Parse doc
        let start = self.peek_span();
        let doc = self.consume_doc_blocks();

        // Parse annotation
        let annotation = if self.peek_is(&TokenKind::PoundSign) {
            Some(self.parse_annotation()?)
        } else {
            None
        };
        self.expect(&TokenKind::Endpoint)?;
        self.expect(&TokenKind::LBrace)?;

        // Parse fields
        let mut fields = Vec::new();
        while self.peek_is(&TokenKind::AtSign) || matches!(self.peek().kind, TokenKind::DocBlock(_))
        {
            // Expecting a decorated field (e.g., @path id). The string type is implicit.
            let doc = self.consume_doc_blocks();
            self.advance()?; // consume '@'
            let start = self.peek_span();
            let decorator = match self.advance()?.kind {
                TokenKind::Ident(s) => s,
                _ => return Err(self.error_prev("expected field name", None, None)),
            };
            let decorator = FieldDecorator::from_str(decorator).map_err(|_| {
                self.error_here(
                    "expected field decorator (path, query, body, header)",
                    None,
                    None,
                )
            })?;
            let name = match self.advance()?.kind {
                TokenKind::Ident(s) => s,
                _ => return Err(self.error_prev("expected field name", None, None)),
            };
            let end = self.prev_span();

            fields.push(DecoratedField {
                field: Field {
                    name,
                    ty: RawType {
                        atoms: vec![TypeAtom {
                            is_ref: false,
                            name: "string".to_string(),
                            optional: false,
                            span: Span {
                                start: start.start,
                                end: end.end,
                                line: start.line,
                                col: start.col,
                            },
                        }],
                    },
                    doc,
                    span: Span {
                        start: start.start,
                        end: end.end,
                        line: start.line,
                        col: start.col,
                    },
                },
                decorator,
            });
        }

        // Parse anything else.
        while !self.peek_is(&TokenKind::RBrace) && !self.peek_is(&TokenKind::Eof) {
            self.advance()?;
        }
        let end = self.expect(&TokenKind::RBrace)?.span;
        Ok(Endpoint {
            doc,
            annotation,
            fields,
            span: Span {
                start: start.start,
                end: end.end,
                line: start.line,
                col: start.col,
            },
        })
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
        self.expect(&TokenKind::Colon)?;
        let ty = self.parse_type()?;
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

    fn parse_annotation(&mut self) -> Result<Annotation<'a>, ParseError> {
        let start = self.peek_span();
        self.expect(&TokenKind::PoundSign)?;
        self.expect(&TokenKind::LBracket)?;
        let name = match &self.advance()?.kind {
            TokenKind::Ident(s) => *s,
            TokenKind::Endpoint => "endpoint",
            _ => return Err(self.error_prev("expected annotation name", None, None)),
        };
        self.expect(&TokenKind::LParen)?;
        let mut args = Vec::new();
        let mut arg_index = 0;
        while !self.peek_is(&TokenKind::RParen) && !self.peek_is(&TokenKind::Eof) {
            // Optionally consume a comma before each argument
            self.expect(&TokenKind::Comma).ok();
            match &self.peek().kind {
                TokenKind::Ident(s) | TokenKind::StringLiteral(s) => {
                    if self.peek_ahead(1).kind == TokenKind::Equals {
                        let name = *s;
                        self.advance()?; // consume name
                        self.expect(&TokenKind::Equals)?; // consume '='
                        let value = match &self.advance()?.kind {
                            TokenKind::Ident(v) | TokenKind::StringLiteral(v) => *v,
                            _ => {
                                return Err(self.error_prev(
                                    "expected value after '='",
                                    None,
                                    None,
                                ));
                            }
                        };
                        args.push(AnnotationArgument::NamedArg { name, value });
                    } else {
                        args.push(AnnotationArgument::PositionalArg {
                            index: arg_index,
                            value: *s,
                        });
                        self.advance()?;
                    }
                }
                _ => break,
            }
            arg_index += 1;
        }
        self.expect(&TokenKind::RParen)?;
        self.expect(&TokenKind::RBracket)?;
        let end = self.prev_span();
        Ok(Annotation {
            name,
            args,
            span: Span {
                start: start.start,
                end: end.end,
                line: start.line,
                col: start.col,
            },
        })
    }

    // type := type_atom ('|' type_atom)*
    fn parse_type(&mut self) -> Result<RawType, ParseError> {
        let mut atoms = Vec::new();
        atoms.push(self.parse_type_atom()?);
        while self.peek_is(&TokenKind::Pipe) {
            self.advance()?;
            atoms.push(self.parse_type_atom()?);
        }
        Ok(RawType { atoms })
    }
    fn parse_type_atom(&mut self) -> Result<TypeAtom, ParseError> {
        let start = self.peek_span();
        let mut is_ref = false;
        if self.peek_is(&TokenKind::PoundSign) {
            self.advance()?;
            is_ref = true;
        }
        let name = match self.advance()?.kind {
            TokenKind::Ident(s) => s.to_string(),
            _ => return Err(self.error_prev("expected type name", None, None)),
        };
        let optional = if self.peek_is(&TokenKind::QuestionMark) {
            self.advance()?;
            true
        } else {
            false
        };
        let end = self.prev_span();
        Ok(TypeAtom {
            is_ref,
            name,
            optional,
            span: Span {
                start: start.start,
                end: end.end,
                line: start.line,
                col: start.col,
            },
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
        self.peek_ahead(0)
    }

    fn peek_ahead(&self, n: usize) -> &Token<'a> {
        self.tokens
            .get(self.i + n)
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
                "unexpected token; expected `{}`, got `{}`",
                want.to_string(),
                self.peek().kind.to_string()
            )
            .as_str(),
            None,
            "unexpected token".into(),
        ))
    }

    #[inline]
    fn error_here(&self, msg: &str, note: Option<&str>, error_code: Option<&str>) -> ParseError {
        self.error_at_span(self.peek_span(), msg, note, error_code)
    }

    #[inline]
    fn error_at_span(
        &self,
        span: Span,
        msg: &str,
        note: Option<&str>,
        error_code: Option<&str>,
    ) -> ParseError {
        self.make_error(span, msg, note, error_code)
    }

    #[inline]
    fn error_prev(&self, msg: &str, note: Option<&str>, error_code: Option<&str>) -> ParseError {
        self.error_at_span(self.prev_span(), msg, note, error_code)
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
        // Previously this produced a semantic error; now it's accepted syntactically.
        let src = indoc!("model User { id: foo }");
        let tokens = Lexer::new(src).lex();
        let mut p = Parser::new("test.glue", src, tokens);
        let program = p.parse_program().expect("should parse");
        assert_eq!(program.models[0].fields[0].ty.atoms[0].name, "foo");
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
