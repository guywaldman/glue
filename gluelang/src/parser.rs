use crate::{
    lexer::{Span, Token, TokenKind},
    utils,
};
use miette::{Diagnostic, LabeledSpan, NamedSource, SourceCode};
use std::{error::Error, fmt, str::FromStr};

#[derive(Debug, Clone)]
pub struct Program {
    pub models: Vec<Model>,
    pub endpoints: Vec<Endpoint>,
}

#[allow(dead_code)]
#[derive(Debug, Clone)]
pub struct Model {
    pub name: String,
    pub doc: Option<String>,
    pub fields: Vec<Field>,
    pub span: Span,
}

#[allow(dead_code)]
#[derive(Debug, Clone)]
pub struct Field {
    pub name: String,
    pub ty: RawType,
    pub doc: Option<String>,
    pub decorator: Option<FieldDecorator>,
    pub span: Span,
}

impl fmt::Display for Field {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.decorator.is_some() {
            write!(f, "@{} ", self.decorator.as_ref().unwrap())?;
        }
        let type_str = self
            .ty
            .atoms
            .iter()
            .map(|a| a.to_string())
            .collect::<Vec<_>>()
            .join(" | ");
        write!(f, "{}: {}", self.name, type_str)?;
        Ok(())
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum FieldDecorator {
    // Endpoint field decorators
    Path,
    Status,
    Mime,
    Query,
    Body,
    Header,
}

impl FromStr for FieldDecorator {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s.to_lowercase().as_str() {
            "path" => Ok(FieldDecorator::Path),
            "status" => Ok(FieldDecorator::Status),
            "mime" => Ok(FieldDecorator::Mime),
            "query" => Ok(FieldDecorator::Query),
            "body" => Ok(FieldDecorator::Body),
            "header" => Ok(FieldDecorator::Header),
            _ => Err(()),
        }
    }
}

impl fmt::Display for FieldDecorator {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let s = match self {
            FieldDecorator::Path => "path",
            FieldDecorator::Status => "status",
            FieldDecorator::Mime => "mime",
            FieldDecorator::Query => "query",
            FieldDecorator::Body => "body",
            FieldDecorator::Header => "header",
        };
        write!(f, "{s}")
    }
}

#[derive(Debug, Clone)]
pub struct Endpoint {
    pub name: Option<String>,
    pub doc: Option<String>,
    pub annotation: Option<Annotation>,
    pub fields: Vec<Field>,
    pub responses: Vec<Vec<Field>>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub enum AnnotationArgument {
    NamedArg { name: String, value: String },
    PositionalArg { index: usize, value: String },
}

#[derive(Debug, Clone)]
pub struct Annotation {
    pub name: String,
    pub args: Vec<AnnotationArgument>,
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

    pub fn parse_program(&mut self) -> Result<Program, ParseError> {
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
                Some(TokenKind::KeywordModel) => models.push(self.parse_model()?),
                Some(TokenKind::KeywordEndpoint) | Some(TokenKind::PoundSign) => {
                    endpoints.push(self.parse_endpoint()?)
                }
                Some(TokenKind::Ident(s)) => {
                    let haystack = ["model", "endpoint"];
                    let candidates = utils::fuzzy::fuzzy_match(s, &haystack, 1);
                    let mut help = None;
                    if let Some((candidate, score)) = candidates.first()
                        && score > &50
                    {
                        help = Some(format!("did you mean '{candidate}'?"));
                    }
                    return Err(self.error_here(
                        &format!(
                            "unexpected identifier '{}'",
                            match &self.tokens[j].kind {
                                TokenKind::Ident(s) => s,
                                _ => unreachable!(),
                            }
                        ),
                        help.as_deref(),
                        Some("EUnexpectedIdent"),
                    ));
                }
                Some(TokenKind::Eof) => break,
                None => break,
                _ => return Err(self.error_here("expected 'model' or 'endpoint'", None, None)),
            }
        }
        Ok(Program { models, endpoints })
    }

    /// Parse an endpoint (syntax only). Annotation & contents are left uninterpreted.
    fn parse_endpoint(&mut self) -> Result<Endpoint, ParseError> {
        // Parse doc
        let start = self.peek_span();
        let doc = self.consume_doc_blocks();

        // Parse annotation
        let annotation = if self.peek_is(&TokenKind::PoundSign) {
            Some(self.parse_annotation()?)
        } else {
            None
        };
        self.expect(&TokenKind::KeywordEndpoint)?;
        // Optional name for the endpoint
        let mut name = None;
        if let TokenKind::Ident(s) = self.peek().kind {
            name = Some(s.to_string());
            self.advance()?;
        }
        self.expect(&TokenKind::LBrace)?;

        let mut fields = Vec::new();
        let mut responses = Vec::new();

        while !self.peek_is(&TokenKind::RBrace) && !self.peek_is(&TokenKind::Eof) {
            // Match either a decorated field, or a response
            let next_relevant_token = self.tokens[self.i..]
                .iter()
                .find(|t| !matches!(t.kind, TokenKind::DocBlock(_) | TokenKind::RBrace));
            if next_relevant_token.is_none() {
                // TODO: Error
                break;
            }
            let next_relevant_token = next_relevant_token.unwrap();
            if next_relevant_token.kind == TokenKind::AtSign
                || matches!(next_relevant_token.kind, TokenKind::Ident(_))
            {
                // Violation of separation of concerns, however there is some semantic analysis here;
                // for responses we can accept @path with an implicit string type
                // TODO: Decouple implicit fields from parsing
                fields.push(self.parse_field_with_implicit_type("?")?);
            } else {
                // Response block
                self.expect(&TokenKind::KeywordResponse)?;
                self.expect(&TokenKind::LBrace)?;

                // Parse response fields
                let mut response_fields = Vec::new();
                while !self.peek_is(&TokenKind::RBrace) && !self.peek_is(&TokenKind::Eof) {
                    // Violation of separation of concerns, however there is some semantic analysis here;
                    // for responses we can accept @path with an implicit string type
                    // TODO: Decouple implicit fields from parsing
                    response_fields.push(self.parse_field_with_implicit_type("?")?);
                }
                responses.push(response_fields);
                self.expect(&TokenKind::RBrace)?;
            }
        }

        // Parse anything else.
        while !self.peek_is(&TokenKind::RBrace) && !self.peek_is(&TokenKind::Eof) {
            self.advance()?;
        }

        let end = self.expect(&TokenKind::RBrace)?.span;
        Ok(Endpoint {
            name,
            doc,
            annotation,
            fields,
            responses,
            span: Span {
                start: start.start,
                end: end.end,
                line: start.line,
                col: start.col,
            },
        })
    }

    fn parse_model(&mut self) -> Result<Model, ParseError> {
        let start = self.peek_span();
        let doc = self.consume_doc_blocks();
        self.expect(&TokenKind::KeywordModel)?;
        let name = match &self.advance()?.kind {
            TokenKind::Ident(s) => s.to_string(),
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

    fn parse_field(&mut self) -> Result<Field, ParseError> {
        self.parse_field_internal(None)
    }

    fn parse_field_with_implicit_type(
        &mut self,
        implicit_type: &'a str,
    ) -> Result<Field, ParseError> {
        self.parse_field_internal(Some(implicit_type))
    }

    fn parse_field_internal(
        &mut self,
        implicit_type: Option<&'a str>,
    ) -> Result<Field, ParseError> {
        let doc = self.consume_doc_blocks();

        // Optional decorator
        let mut decorator = None;
        if self.peek_is(&TokenKind::AtSign) {
            self.advance()?; // consume '@'
            decorator = match self.advance()?.kind {
                TokenKind::Ident(s) => {
                    if let Ok(deco) = FieldDecorator::from_str(s) {
                        Some(deco)
                    } else {
                        return Err(self.error_prev(
                            &format!("unknown field decorator '@{s}'"),
                            Some("valid decorators are: @path, @query, @body, @header"),
                            Some("EFieldDecorator"),
                        ));
                    }
                }
                _ => return Err(self.error_prev("expected field decorator after '@'", None, None)),
            };
        }

        let start = self.peek_span();
        let name = match self.advance()?.kind {
            TokenKind::Ident(s) => s.to_string(),
            // Also accept a number if there is no type expected (e.g., `@status 200`)
            TokenKind::Number(n) if implicit_type.is_some() => n.to_string(),
            _ => return Err(self.error_prev("expected field name", None, None)),
        };
        let colon_ok = self.expect(&TokenKind::Colon);
        let ty = match colon_ok {
            Ok(_) => self.parse_type(),
            Err(e) => Err(e),
        };
        let ty = match ty {
            Ok(t) => t,
            Err(e) => {
                if let Some(implicit_type) = implicit_type {
                    RawType {
                        atoms: vec![TypeAtom {
                            is_ref: false,
                            name: implicit_type.to_string(),
                            optional: false,
                            span: Span {
                                start: start.start,
                                end: start.end,
                                line: start.line,
                                col: start.col,
                            },
                        }],
                    }
                } else {
                    return Err(e);
                }
            }
        };
        let end = self.prev_span();
        Ok(Field {
            name,
            ty,
            doc,
            decorator,
            span: Span {
                start: start.start,
                end: end.end,
                line: start.line,
                col: start.col,
            },
        })
    }

    fn parse_annotation(&mut self) -> Result<Annotation, ParseError> {
        let start = self.peek_span();
        self.expect(&TokenKind::PoundSign)?;
        self.expect(&TokenKind::LBracket)?;
        let name = match &self.advance()?.kind {
            TokenKind::Ident(s) => s.to_string(),
            TokenKind::KeywordEndpoint => "endpoint".to_string(),
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
                        let name = s.to_string();
                        self.advance()?; // consume name
                        self.expect(&TokenKind::Equals)?; // consume '='
                        let value = match &self.advance()?.kind {
                            TokenKind::Ident(v) | TokenKind::StringLiteral(v) => v.to_string(),
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
                            value: s.to_string(),
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
