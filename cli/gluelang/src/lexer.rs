use core::fmt;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TokenKind<'a> {
    LBrace,
    RBrace,
    LBracket,
    RBracket,
    LParen,
    RParen,
    Comma,
    Equals,
    QuestionMark,
    Colon,
    Hash,
    Pipe,
    AtSign,
    Ident(&'a str),
    StringLiteral(&'a str),
    Number(i64),
    DocBlock(Vec<&'a str>),
    Eof,
    Error(&'a str),
    // Keywords
    KeywordModel,
    KeywordEndpoint,
    KeywordResponse,
}

impl fmt::Display for TokenKind<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
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
            TokenKind::Equals => write!(f, "="),
            TokenKind::Colon => write!(f, ":"),
            TokenKind::Hash => write!(f, "#"),
            TokenKind::Pipe => write!(f, "|"),
            TokenKind::AtSign => write!(f, "@"),
            TokenKind::Number(n) => write!(f, "number({n})"),
            TokenKind::Ident(s) => write!(f, "identifier({s})"),
            TokenKind::StringLiteral(s) => write!(f, "string(\"{s}\")"),
            TokenKind::DocBlock(_) => write!(f, "doc block"),
            TokenKind::Eof => write!(f, "end of file"),
            TokenKind::Error(s) => write!(f, "error({s})"),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Span {
    pub start: usize,
    pub end: usize,
    pub line: u32,
    pub col: u32,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Token<'a> {
    pub kind: TokenKind<'a>,
    pub span: Span,
}

pub struct Lexer<'a> {
    src: &'a str,
    bytes: &'a [u8],
    i: usize,
    line: u32,
    col: u32,
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

    pub fn lex(&mut self) -> Vec<Token<'a>> {
        let mut tokens = Vec::new();
        loop {
            let token = self.next_token();
            tokens.push(token.clone());
            if token.kind == TokenKind::Eof {
                break;
            }
        }
        tokens
    }

    pub fn next_token(&mut self) -> Token<'a> {
        loop {
            self.skip_ws();
            let sp = self.span_start();

            if self.at_end() {
                return self.make(TokenKind::Eof, sp);
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
                    return self.make(TokenKind::LBrace, sp);
                }
                b'}' => {
                    self.advance();
                    return self.make(TokenKind::RBrace, sp);
                }
                b'[' => {
                    self.advance();
                    return self.make(TokenKind::LBracket, sp);
                }
                b']' => {
                    self.advance();
                    return self.make(TokenKind::RBracket, sp);
                }
                b'(' => {
                    self.advance();
                    return self.make(TokenKind::LParen, sp);
                }
                b')' => {
                    self.advance();
                    return self.make(TokenKind::RParen, sp);
                }
                b',' => {
                    self.advance();
                    return self.make(TokenKind::Comma, sp);
                }
                b'=' => {
                    self.advance();
                    return self.make(TokenKind::Equals, sp);
                }
                b'?' => {
                    self.advance();
                    return self.make(TokenKind::QuestionMark, sp);
                }
                b':' => {
                    self.advance();
                    return self.make(TokenKind::Colon, sp);
                }
                b'#' => {
                    self.advance();
                    return self.make(TokenKind::Hash, sp);
                }
                b'|' => {
                    self.advance();
                    return self.make(TokenKind::Pipe, sp);
                }
                b'@' => {
                    self.advance();
                    return self.make(TokenKind::AtSign, sp);
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
                        return self.make(TokenKind::Error(slice), sp);
                    } else {
                        self.advance(); // consume closing quote
                        let s = &self.src[start..end];
                        return self.make(TokenKind::StringLiteral(s), sp);
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
                    return self.make(TokenKind::Error(slice), sp);
                }
            }
        }
    }

    fn scan_number(&mut self, sp: Span) -> Token<'a> {
        let start = self.i;
        self.advance(); // first is valid
        while !self.at_end() && self.peek().is_ascii_digit() {
            self.advance();
        }
        let s = &self.src[start..self.i];
        let number = s.parse::<i64>().expect("valid number");
        self.make(TokenKind::Number(number), sp)
    }

    fn scan_ident_or_kw(&mut self, sp: Span) -> Token<'a> {
        let start = self.i;
        self.advance(); // first is valid
        while !self.at_end() && Self::is_id_cont(self.peek()) {
            self.advance();
        }
        let s = &self.src[start..self.i];

        let kind = match s {
            "model" => TokenKind::KeywordModel,
            "endpoint" => TokenKind::KeywordEndpoint,
            "response" => TokenKind::KeywordResponse,
            _ => TokenKind::Ident(s),
        };
        self.make(kind, sp)
    }

    fn scan_doc_block(&mut self, sp: Span) -> Token<'a> {
        let mut lines: Vec<&'a str> = Vec::new();
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

            lines.push(&self.src[start..end]);

            // consume newline if present
            if !self.at_end() && self.peek() == b'\n' {
                self.advance();
            }

            // stop if next line is not another "///"
            if !(self.peek() == b'/' && self.peek_n(1) == b'/' && self.peek_n(2) == b'/') {
                break;
            }
        }
        self.make(TokenKind::DocBlock(lines), sp)
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
            col: self.col,
        }
    }

    fn make(&self, kind: TokenKind<'a>, mut sp: Span) -> Token<'a> {
        sp.end = self.i;
        Token { kind, span: sp }
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
                &super::TokenKind::KeywordModel,
                &super::TokenKind::Ident("User"),
                &super::TokenKind::LBrace,
                &super::TokenKind::DocBlock(vec!["A unique identifier for the user."]),
                &super::TokenKind::Ident("id"),
                &super::TokenKind::Colon,
                &super::TokenKind::Ident("string"),
                &super::TokenKind::DocBlock(vec!["The user's email address."]),
                &super::TokenKind::Ident("email"),
                &super::TokenKind::Colon,
                &super::TokenKind::Ident("string"),
                &super::TokenKind::DocBlock(vec!["The user's age."]),
                &super::TokenKind::Ident("age"),
                &super::TokenKind::Colon,
                &super::TokenKind::Ident("int"),
                &super::TokenKind::RBrace,
                &super::TokenKind::Eof,
            ]
        );
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
                &super::TokenKind::KeywordModel,
                &super::TokenKind::Ident("User"),
                &super::TokenKind::LBrace,
                &super::TokenKind::DocBlock(vec!["A unique identifier for the user."]),
                &super::TokenKind::Ident("id"),
                &super::TokenKind::Colon,
                &super::TokenKind::Ident("string"),
                &super::TokenKind::DocBlock(vec!["The user's email address."]),
                &super::TokenKind::Ident("email"),
                &super::TokenKind::Colon,
                &super::TokenKind::Ident("string"),
                &super::TokenKind::DocBlock(vec!["The user's age."]),
                &super::TokenKind::Ident("age"),
                &super::TokenKind::Colon,
                &super::TokenKind::Ident("int"),
                &super::TokenKind::RBrace,
                &super::TokenKind::KeywordModel,
                &super::TokenKind::Ident("Post"),
                &super::TokenKind::LBrace,
                &super::TokenKind::DocBlock(vec!["A unique identifier for the post."]),
                &super::TokenKind::Ident("id"),
                &super::TokenKind::Colon,
                &super::TokenKind::Ident("string"),
                &super::TokenKind::DocBlock(vec!["The title of the post."]),
                &super::TokenKind::Ident("title"),
                &super::TokenKind::Colon,
                &super::TokenKind::Ident("string"),
                &super::TokenKind::DocBlock(vec!["The content of the post."]),
                &super::TokenKind::Ident("content"),
                &super::TokenKind::Colon,
                &super::TokenKind::Ident("string"),
                &super::TokenKind::DocBlock(vec!["The ID of the user who created the post."]),
                &super::TokenKind::Ident("author_id"),
                &super::TokenKind::Colon,
                &super::TokenKind::Ident("string"),
                &super::TokenKind::RBrace,
                &super::TokenKind::Eof,
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
                &super::TokenKind::KeywordModel,
                &super::TokenKind::Ident("Post"),
                &super::TokenKind::LBrace,
                &super::TokenKind::Ident("id"),
                &super::TokenKind::Colon,
                &super::TokenKind::Ident("string"),
                &super::TokenKind::Ident("user"),
                &super::TokenKind::Colon,
                &super::TokenKind::Ident("int"),
                &super::TokenKind::Pipe,
                &super::TokenKind::Hash,
                &super::TokenKind::Ident("User"),
                &super::TokenKind::RBrace,
                &super::TokenKind::KeywordModel,
                &super::TokenKind::Ident("User"),
                &super::TokenKind::LBrace,
                &super::TokenKind::Ident("id"),
                &super::TokenKind::Colon,
                &super::TokenKind::Ident("string"),
                &super::TokenKind::Ident("email"),
                &super::TokenKind::Colon,
                &super::TokenKind::Ident("string"),
                &super::TokenKind::RBrace,
                &super::TokenKind::Eof,
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
                &super::TokenKind::Hash,
                &super::TokenKind::LBracket,
                &super::TokenKind::KeywordEndpoint,
                &super::TokenKind::LParen,
                &super::TokenKind::StringLiteral("/users/{id}"),
                &super::TokenKind::RParen,
                &super::TokenKind::RBracket,
                &super::TokenKind::KeywordEndpoint,
                &super::TokenKind::LBrace,
                &super::TokenKind::RBrace,
                &super::TokenKind::Eof,
            ]
        );
    }
}
