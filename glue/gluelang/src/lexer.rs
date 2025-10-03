use core::fmt;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TokenKind {
    LBrace,
    RBrace,
    LBracket,
    RBracket,
    LParen,
    RParen,
    Comma,
    Equal,
    QuestionMark,
    Colon,
    Hash,
    Pipe,
    AtSign,
    Ident,
    StringLit,
    IntLit,
    BoolLit,
    Number,
    DocBlock,
    Eof,
    Error,
    // Keywords
    KeywordEnum,
    KeywordModel,
    KeywordEndpoint,
    KeywordResponse,
    Noop,
}

impl Default for TokenKind {
    fn default() -> Self {
        TokenKind::Noop
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TokenPayload {
    None,
    String(String),
    Number(i64),
    Bool(bool),
    DocLines(Vec<String>),
}

impl fmt::Display for TokenKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            TokenKind::KeywordEnum => write!(f, "enum"),
            TokenKind::KeywordModel => write!(f, "model"),
            TokenKind::KeywordEndpoint => write!(f, "endpoint"),
            TokenKind::KeywordResponse => write!(f, "response"),
            TokenKind::LBrace => write!(f, "{{"),
            TokenKind::RBrace => write!(f, "}}"),
            TokenKind::LBracket => write!(f, "["),
            TokenKind::RBracket => write!(f, "]"),
            TokenKind::LParen => write!(f, "("),
            TokenKind::RParen => write!(f, ")"),
            TokenKind::QuestionMark => write!(f, "?"),
            TokenKind::Comma => write!(f, ","),
            TokenKind::Equal => write!(f, "="),
            TokenKind::Colon => write!(f, ":"),
            TokenKind::Hash => write!(f, "#"),
            TokenKind::Pipe => write!(f, "|"),
            TokenKind::AtSign => write!(f, "@"),
            TokenKind::Number => write!(f, "number"),
            TokenKind::Ident => write!(f, "identifier"),
            TokenKind::StringLit => write!(f, "string"),
            TokenKind::IntLit => write!(f, "integer"),
            TokenKind::BoolLit => write!(f, "boolean"),
            TokenKind::DocBlock => write!(f, "doc block"),
            TokenKind::Eof => write!(f, "end of file"),
            TokenKind::Error => write!(f, "error"),
            TokenKind::Noop => write!(f, "noop"),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Span {
    pub start: usize,
    pub end: usize,
    pub line: usize,
    pub col_start: usize,
    pub col_end: usize,
}

impl Default for Span {
    fn default() -> Self {
        Self {
            start: 0,
            end: 0,
            line: 1,
            col_start: 1,
            col_end: 1,
        }
    }
}

impl Span {
    pub fn merge(&self, other: &Span) -> Span {
        Span {
            start: self.start.min(other.start),
            end: self.end.max(other.end),
            line: self.line.min(other.line),
            col_start: if self.line <= other.line { self.col_start } else { other.col_start },
            col_end: if self.line >= other.line { self.col_end } else { other.col_end },
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Token {
    pub kind: TokenKind,
    pub payload: TokenPayload,
    pub span: Span,
}

impl Token {
    pub fn as_string(&self) -> Option<&str> {
        match &self.payload {
            TokenPayload::String(s) => Some(s),
            _ => None,
        }
    }

    pub fn as_number(&self) -> Option<i64> {
        match &self.payload {
            TokenPayload::Number(n) => Some(*n),
            _ => None,
        }
    }

    pub fn as_doc_lines(&self) -> Option<&Vec<String>> {
        match &self.payload {
            TokenPayload::DocLines(lines) => Some(lines),
            _ => None,
        }
    }
}

pub struct Lexer<'a> {
    src: &'a str,
    bytes: &'a [u8],
    i: usize,
    line: usize,
    col: usize,
}

impl<'a> Lexer<'a> {
    pub fn new(src: &'a str) -> Self {
        Self {
            src,
            bytes: src.as_bytes(),
            i: 0,
            line: 1,
            col: 1,
        }
    }

    pub fn lex(mut self) -> Vec<Token> {
        let mut tokens = Vec::new();
        loop {
            let token = self.next_token();
            let is_eof = token.kind == TokenKind::Eof;
            tokens.push(token);
            if is_eof {
                break;
            }
        }
        tokens
    }

    pub fn next_token(&mut self) -> Token {
        loop {
            self.skip_ws();
            let sp = self.span_start();

            if self.at_end() {
                return self.make(TokenKind::Eof, TokenPayload::None, sp);
            }

            // Snapshot lookaheads to avoid borrow issues
            let c1 = self.peek();
            let c2 = self.peek_n(1);
            let c3 = self.peek_n(2);

            match c1 {
                b'/' if c2 == b'/' && c3 == b'/' => {
                    // Doc block: one or more lines starting with "///"
                    return self.scan_doc_block(sp);
                }
                b'/' if c2 == b'/' && c3 != b'/' => {
                    // Line comment: "//" followed by anything except another "/"
                    self.skip_line_comment();
                    continue;
                }
                b'{' => {
                    self.advance();
                    return self.make(TokenKind::LBrace, TokenPayload::None, sp);
                }
                b'}' => {
                    self.advance();
                    return self.make(TokenKind::RBrace, TokenPayload::None, sp);
                }
                b'[' => {
                    self.advance();
                    return self.make(TokenKind::LBracket, TokenPayload::None, sp);
                }
                b']' => {
                    self.advance();
                    return self.make(TokenKind::RBracket, TokenPayload::None, sp);
                }
                b'(' => {
                    self.advance();
                    return self.make(TokenKind::LParen, TokenPayload::None, sp);
                }
                b')' => {
                    self.advance();
                    return self.make(TokenKind::RParen, TokenPayload::None, sp);
                }
                b',' => {
                    self.advance();
                    return self.make(TokenKind::Comma, TokenPayload::None, sp);
                }
                b'=' => {
                    self.advance();
                    return self.make(TokenKind::Equal, TokenPayload::None, sp);
                }
                b'?' => {
                    self.advance();
                    return self.make(TokenKind::QuestionMark, TokenPayload::None, sp);
                }
                b':' => {
                    self.advance();
                    return self.make(TokenKind::Colon, TokenPayload::None, sp);
                }
                b'#' => {
                    self.advance();
                    return self.make(TokenKind::Hash, TokenPayload::None, sp);
                }
                b'|' => {
                    self.advance();
                    return self.make(TokenKind::Pipe, TokenPayload::None, sp);
                }
                b'@' => {
                    self.advance();
                    return self.make(TokenKind::AtSign, TokenPayload::None, sp);
                }
                b'"' => {
                    self.advance(); // consume opening quote
                    let start = self.i;
                    while !self.at_end() && self.peek() != b'"' {
                        self.advance();
                    }
                    let end = self.i;
                    if self.at_end() {
                        // Unterminated string
                        let slice = &self.src[start - 1..self.i];
                        return self.make(TokenKind::Error, TokenPayload::String(slice.to_string()), sp);
                    } else {
                        self.advance(); // consume closing quote
                        let s = &self.src[start..end];
                        return self.make(TokenKind::StringLit, TokenPayload::String(s.to_string()), sp);
                    }
                }
                b'1'..=b'9' => {
                    return self.scan_number(sp);
                }
                _ if Self::is_id_start(c1) => {
                    return self.scan_ident_or_kw(sp);
                }
                _ => {
                    let start = self.i;
                    self.advance();
                    let slice = &self.src[start..self.i];
                    return self.make(TokenKind::Error, TokenPayload::String(slice.to_string()), sp);
                }
            }
        }
    }

    fn scan_number(&mut self, sp: Span) -> Token {
        let start = self.i;
        self.advance(); // first is valid
        while !self.at_end() && self.peek().is_ascii_digit() {
            self.advance();
        }
        let s = &self.src[start..self.i];
        let number = s.parse::<i64>().expect("valid number");
        self.make(TokenKind::Number, TokenPayload::Number(number), sp)
    }

    fn scan_ident_or_kw(&mut self, sp: Span) -> Token {
        let start = self.i;
        self.advance(); // first is valid
        while !self.at_end() && Self::is_id_cont(self.peek()) {
            self.advance();
        }
        let s = &self.src[start..self.i];

        let kind = match s {
            "enum" => TokenKind::KeywordEnum,
            "model" => TokenKind::KeywordModel,
            "endpoint" => TokenKind::KeywordEndpoint,
            "response" => TokenKind::KeywordResponse,
            "true" => {
                return self.make(TokenKind::BoolLit, TokenPayload::Bool(true), sp);
            }
            "false" => {
                return self.make(TokenKind::BoolLit, TokenPayload::Bool(false), sp);
            }
            _ => TokenKind::Ident,
        };

        let payload = if kind == TokenKind::Ident {
            TokenPayload::String(s.to_string())
        } else {
            TokenPayload::None
        };

        self.make(kind, payload, sp)
    }

    fn scan_doc_block(&mut self, sp: Span) -> Token {
        let mut lines = Vec::new();

        loop {
            self.advance_n(3); // "///"

            // optional single space after "///"
            while !self.at_end() && self.peek() == b' ' {
                self.advance();
            }

            let start = self.i;
            while !self.at_end() && self.peek() != b'\n' {
                self.advance();
            }
            let mut end = self.i;

            // trim trailing spaces (but not newline)
            while end > start && self.bytes[end - 1].is_ascii_whitespace() && self.bytes[end - 1] != b'\n' {
                end -= 1;
            }

            lines.push(self.src[start..end].to_string());

            // consume newline if present
            if !self.at_end() && self.peek() == b'\n' {
                self.advance();
            }

            // stop if next line is not another "///"
            if !(self.peek() == b'/' && self.peek_n(1) == b'/' && self.peek_n(2) == b'/') {
                break;
            }
        }
        self.make(TokenKind::DocBlock, TokenPayload::DocLines(lines), sp)
    }

    fn skip_line_comment(&mut self) {
        self.advance_n(2); // "//"
        while !self.at_end() {
            let c = self.advance().unwrap();
            if c == b'\n' {
                break;
            }
        }
    }

    fn skip_ws(&mut self) {
        while !self.at_end() {
            match self.peek() {
                b' ' | b'\t' | b'\r' | b'\n' => {
                    self.advance();
                }
                _ => break,
            }
        }
    }

    #[inline]
    fn at_end(&self) -> bool {
        self.i >= self.bytes.len()
    }

    #[inline]
    fn peek(&self) -> u8 {
        *self.bytes.get(self.i).unwrap_or(&0)
    }

    #[inline]
    fn peek_n(&self, n: usize) -> u8 {
        *self.bytes.get(self.i + n).unwrap_or(&0)
    }

    fn advance(&mut self) -> Option<u8> {
        let c = *self.bytes.get(self.i)?;
        self.i += 1;
        if c == b'\n' {
            self.line += 1;
            self.col = 1;
        } else {
            self.col += 1;
        }
        Some(c)
    }

    #[inline]
    fn advance_n(&mut self, n: usize) {
        for _ in 0..n {
            self.advance();
        }
    }

    fn span_start(&self) -> Span {
        Span {
            start: self.i,
            end: self.i,
            line: self.line,
            col_start: self.col,
            col_end: self.col,
        }
    }

    fn make(&self, kind: TokenKind, payload: TokenPayload, mut sp: Span) -> Token {
        sp.end = self.i;
        sp.col_end = self.col;
        Token { kind, payload, span: sp }
    }

    #[inline]
    fn is_id_start(c: u8) -> bool {
        c.is_ascii_alphabetic() || c == b'_'
    }

    #[inline]
    fn is_id_cont(c: u8) -> bool {
        c.is_ascii_alphanumeric() || c == b'_' || c == b'-' || c == b'/' || c == b'.'
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use pretty_assertions::assert_eq;

    #[test]
    fn test_lexer_basic() {
        let src = r#"
        model User {
            /// A unique identifier for the user.
            // INTERNAL NOTE: The ID is a UUID.
            id: string
            /// The user's email address.
            email: string
            /// The user's age.
            age: int
        }
    "#
        .trim();

        let tokens = super::Lexer::new(src).lex();

        let token_kinds = tokens.iter().map(|t| &t.kind).collect::<Vec<_>>();
        assert_eq!(
            token_kinds,
            vec![
                &TokenKind::KeywordModel,
                &TokenKind::Ident,
                &TokenKind::LBrace,
                &TokenKind::DocBlock,
                &TokenKind::Ident,
                &TokenKind::Colon,
                &TokenKind::Ident,
                &TokenKind::DocBlock,
                &TokenKind::Ident,
                &TokenKind::Colon,
                &TokenKind::Ident,
                &TokenKind::DocBlock,
                &TokenKind::Ident,
                &TokenKind::Colon,
                &TokenKind::Ident,
                &TokenKind::RBrace,
                &TokenKind::Eof,
            ]
        );

        // Test specific payloads
        assert_eq!(tokens[1].as_string(), Some("User"));
        assert_eq!(tokens[3].as_doc_lines(), Some(&vec!["A unique identifier for the user.".to_owned()]));
        assert_eq!(tokens[4].as_string(), Some("id"));
    }

    #[test]
    fn test_lexer_multiple_models() {
        let src = r#"
        model User {
            /// A unique identifier for the user.
            // INTERNAL NOTE: The ID is a UUID.
            id: string
            /// The user's email address.
            email: string
            /// The user's age.
            age: int
        }

        model Post {
            /// A unique identifier for the post.
            id: string
            /// The title of the post.
            /// It should be concise yet descriptive.
            title: string
            /// The content of the post.
            content: string
            /// The ID of the user who created the post.
            author_id: string
        }
    "#
        .trim();

        let tokens = super::Lexer::new(src).lex();

        let token_kinds = tokens.iter().map(|t| &t.kind).collect::<Vec<_>>();
        assert_eq!(
            token_kinds,
            vec![
                &TokenKind::KeywordModel,
                &TokenKind::Ident,
                &TokenKind::LBrace,
                &TokenKind::DocBlock,
                &TokenKind::Ident,
                &TokenKind::Colon,
                &TokenKind::Ident,
                &TokenKind::DocBlock,
                &TokenKind::Ident,
                &TokenKind::Colon,
                &TokenKind::Ident,
                &TokenKind::DocBlock,
                &TokenKind::Ident,
                &TokenKind::Colon,
                &TokenKind::Ident,
                &TokenKind::RBrace,
                &TokenKind::KeywordModel,
                &TokenKind::Ident,
                &TokenKind::LBrace,
                &TokenKind::DocBlock,
                &TokenKind::Ident,
                &TokenKind::Colon,
                &TokenKind::Ident,
                &TokenKind::DocBlock,
                &TokenKind::DocBlock,
                &TokenKind::Ident,
                &TokenKind::Colon,
                &TokenKind::Ident,
                &TokenKind::DocBlock,
                &TokenKind::Ident,
                &TokenKind::Colon,
                &TokenKind::Ident,
                &TokenKind::DocBlock,
                &TokenKind::Ident,
                &TokenKind::Colon,
                &TokenKind::Ident,
                &TokenKind::RBrace,
                &TokenKind::Eof,
            ]
        );
    }

    #[test]
    fn test_lexer_ref() {
        let src = r#"
        model Post {
            id: string
            user: int | #User
        }

        model User {
            id: string
            email: string
        }
        "#
        .trim();

        let tokens = super::Lexer::new(src).lex();
        let token_kinds = tokens.iter().map(|t| &t.kind).collect::<Vec<_>>();
        assert_eq!(
            token_kinds,
            vec![
                &TokenKind::KeywordModel,
                &TokenKind::Ident,
                &TokenKind::LBrace,
                &TokenKind::Ident,
                &TokenKind::Colon,
                &TokenKind::Ident,
                &TokenKind::Ident,
                &TokenKind::Colon,
                &TokenKind::Ident,
                &TokenKind::Pipe,
                &TokenKind::Hash,
                &TokenKind::Ident,
                &TokenKind::RBrace,
                &TokenKind::KeywordModel,
                &TokenKind::Ident,
                &TokenKind::LBrace,
                &TokenKind::Ident,
                &TokenKind::Colon,
                &TokenKind::Ident,
                &TokenKind::Ident,
                &TokenKind::Colon,
                &TokenKind::Ident,
                &TokenKind::RBrace,
                &TokenKind::Eof,
            ]
        );
    }

    #[test]
    fn test_lexer_endpoint_empty() {
        let src = r#"
        #[endpoint("/users/{id}")]
        endpoint {
        }
        "#
        .trim();

        let tokens = super::Lexer::new(src).lex();
        let token_kinds = tokens.iter().map(|t| &t.kind).collect::<Vec<_>>();
        assert_eq!(
            token_kinds,
            vec![
                &TokenKind::Hash,
                &TokenKind::LBracket,
                &TokenKind::KeywordEndpoint,
                &TokenKind::LParen,
                &TokenKind::StringLit,
                &TokenKind::RParen,
                &TokenKind::RBracket,
                &TokenKind::KeywordEndpoint,
                &TokenKind::LBrace,
                &TokenKind::RBrace,
                &TokenKind::Eof,
            ]
        );

        // Test string literal payload
        assert_eq!(tokens[4].as_string(), Some("/users/{id}"));
    }
}
