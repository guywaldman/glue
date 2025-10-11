use core::fmt;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
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
    DoubleDot,
    Ident,
    StringLit,
    IntLit,
    BoolLit,
    DocBlock,
    Eof,
    Error,
    // Keywords
    KeywordEnum,
    KeywordModel,
    KeywordEndpoint,
    #[default]
    Noop,
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
            TokenKind::DoubleDot => write!(f, ".."),
            TokenKind::IntLit => write!(f, "number"),
            TokenKind::Ident => write!(f, "identifier"),
            TokenKind::StringLit => write!(f, "string"),
            TokenKind::BoolLit => write!(f, "boolean"),
            TokenKind::DocBlock => write!(f, "doc block"),
            TokenKind::Eof => write!(f, "end of file"),
            TokenKind::Error => write!(f, "error"),
            TokenKind::Noop => write!(f, "noop"),
        }
    }
}

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
pub struct Span {
    pub chars: (usize, usize),
    pub lines: (usize, usize),
    pub cols: (usize, usize),
}

impl fmt::Display for Span {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}:{} → {}:{}", self.lines.0, self.cols.0, self.lines.1, self.cols.1)
    }
}

impl Span {
    pub fn new(chars: (usize, usize), lines: (usize, usize), cols: (usize, usize)) -> Self {
        Self { chars, lines, cols }
    }

    pub fn from_ranges(char_range: std::ops::Range<usize>, line_range: std::ops::Range<usize>, col_range: std::ops::Range<usize>) -> Self {
        Self {
            chars: (char_range.start, char_range.end),
            lines: (line_range.start, line_range.end),
            cols: (col_range.start, col_range.end),
        }
    }

    /// Creates a union span from the minimum start to the maximum end
    pub fn merge(&self, other: &Span) -> Span {
        let start_char = self.chars.0.min(other.chars.0);
        let end_char = self.chars.1.max(other.chars.1);

        let (start_line, start_col) = if self.chars.0 < other.chars.0 {
            (self.lines.0, self.cols.0)
        } else if self.chars.0 > other.chars.0 {
            (other.lines.0, other.cols.0)
        } else {
            (self.lines.0, self.cols.0.min(other.cols.0))
        };

        let (end_line, end_col) = if self.chars.1 > other.chars.1 {
            (self.lines.1, self.cols.1)
        } else if self.chars.1 < other.chars.1 {
            (other.lines.1, other.cols.1)
        } else {
            (self.lines.1, self.cols.1.max(other.cols.1))
        };

        Span {
            chars: (start_char, end_char),
            lines: (start_line, end_line),
            cols: (start_col, end_col),
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
                b'.' if c2 == b'.' => {
                    self.advance_n(2);
                    return self.make(TokenKind::DoubleDot, TokenPayload::None, sp);
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
                b'1'..=b'9' if c2.is_ascii_alphabetic() => {
                    return self.scan_ident_or_kw(sp);
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
        self.make(TokenKind::IntLit, TokenPayload::Number(number), sp)
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
            self.advance_n(3); // Three slashes

            // Optional single space after "///"
            while !self.at_end() && self.peek() == b' ' {
                self.advance();
            }

            let start = self.i;
            while !self.at_end() && self.peek() != b'\n' {
                self.advance();
            }
            let mut end = self.i;

            // Trim trailing spaces (but not newline)
            while end > start && self.bytes[end - 1].is_ascii_whitespace() && self.bytes[end - 1] != b'\n' {
                end -= 1;
            }

            lines.push(self.src[start..end].to_string());

            let has_next_doc_line = !self.at_end() && self.peek() == b'\n' && self.peek_n(1) == b'/' && self.peek_n(2) == b'/' && self.peek_n(3) == b'/';

            if has_next_doc_line {
                // Consume the newline to continue to next doc line
                self.advance();
            } else {
                // Don't consume the newline, not part of the doc block token
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
            chars: (self.i, self.i),
            lines: (self.line, self.line),
            cols: (self.col, self.col),
        }
    }

    fn make(&self, kind: TokenKind, payload: TokenPayload, start_span: Span) -> Token {
        let span = Span {
            chars: (start_span.chars.0, self.i),
            lines: (start_span.lines.0, self.line),
            cols: (start_span.cols.0, self.col),
        };
        Token { kind, payload, span }
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

/// Find the first occurrence of `pattern` in `src` and return its `Span`.
/// This is meant to be used only in tests.
/// IMPORTANT: For convenience, if the pattern contains capturing groups, there is escaping expected. Otherwise, no escaping.
pub fn span_of(src: &str, pattern: &str) -> Option<Span> {
    // For convenience, escaping is not required unless using capturing groups.
    let re = if pattern.contains('(') && pattern.contains(')') {
        regex::Regex::new(pattern).unwrap()
    } else {
        regex::Regex::new(&regex::escape(pattern)).unwrap()
    };
    let bytes = src.as_bytes();

    let cap = re.captures(src)?;
    // Use the first capturing group, or the whole match if no groups
    let m = cap.get(1).or_else(|| cap.get(0)).unwrap();
    let start = m.start();
    let end = m.end();

    // Calculate line and column for start
    let mut line = 1;
    let mut col = 1;
    for &b in &bytes[..start] {
        if b == b'\n' {
            line += 1;
            col = 1;
        } else {
            col += 1;
        }
    }

    let line_start = line;
    let col_start = col;

    // Calculate line and column for end
    for &b in &bytes[start..end] {
        if b == b'\n' {
            line += 1;
            col = 1;
        } else {
            col += 1;
        }
    }

    Some(Span::new((start, end), (line_start, line), (col_start, col)))
}

#[cfg(test)]
mod tests {
    use super::*;
    use indoc::indoc;
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
    fn test_lexer_ident_with_starting_number() {
        let src = r#"
        model Post {
            2a: string
            anon: {
                3a: string
            }
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
                &TokenKind::LBrace,
                &TokenKind::Ident,
                &TokenKind::Colon,
                &TokenKind::Ident,
                &TokenKind::RBrace,
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

    #[test]
    fn test_lexer_spans() {
        let src = indoc! {r#"
            model User {
                /// A unique identifier for the user.
                // INTERNAL NOTE: The ID is a UUID.
                id: string
                /// The user's email address.
                email: string
                /// The user's age.
                age: int
            }
        "#};
        let tokens = super::Lexer::new(src).lex();

        assert_eq!((tokens[0].kind, tokens[0].span), (TokenKind::KeywordModel, span_of(src, "model").unwrap()));
        assert_eq!((tokens[1].kind, tokens[1].span), (TokenKind::Ident, span_of(src, "User").unwrap()));
        assert_eq!((tokens[2].kind, tokens[2].span), (TokenKind::LBrace, span_of(src, "{").unwrap()));
        assert_eq!(
            (tokens[3].kind, tokens[3].span),
            (TokenKind::DocBlock, span_of(src, "/// A unique identifier for the user.").unwrap())
        );
        assert_eq!((tokens[4].kind, tokens[4].span), (TokenKind::Ident, span_of(src, "(id):").unwrap()));
        assert_eq!((tokens[5].kind, tokens[5].span), (TokenKind::Colon, span_of(src, "id(:)").unwrap()));
        assert_eq!((tokens[6].kind, tokens[6].span), (TokenKind::Ident, span_of(src, "id: (string)").unwrap()));
        assert_eq!(
            (tokens[7].kind, tokens[7].span),
            (TokenKind::DocBlock, span_of(src, "/// The user's email address.").unwrap())
        );
        assert_eq!((tokens[8].kind, tokens[8].span), (TokenKind::Ident, span_of(src, "(email):").unwrap()));
        assert_eq!((tokens[9].kind, tokens[9].span), (TokenKind::Colon, span_of(src, "email(:)").unwrap()));
        assert_eq!((tokens[10].kind, tokens[10].span), (TokenKind::Ident, span_of(src, "email: (string)").unwrap()));
        assert_eq!((tokens[11].kind, tokens[11].span), (TokenKind::DocBlock, span_of(src, "/// The user's age.").unwrap()));
        assert_eq!((tokens[12].kind, tokens[12].span), (TokenKind::Ident, span_of(src, "(age):").unwrap()));
        assert_eq!((tokens[13].kind, tokens[13].span), (TokenKind::Colon, span_of(src, "age(:)").unwrap()));
        assert_eq!((tokens[14].kind, tokens[14].span), (TokenKind::Ident, span_of(src, "age: (int)").unwrap()));
        assert_eq!((tokens[15].kind, tokens[15].span), (TokenKind::RBrace, span_of(src, "}").unwrap()));
        assert_eq!(tokens[16].kind, TokenKind::Eof);
    }

    #[test]
    fn test_span_of_helper() {
        let src = indoc! {r#"
            model User {
                id: string | int
                name: UserName
            }
        "#};

        // Test span_of for "User"
        let user_span = span_of(src, "User").unwrap();
        assert_eq!(user_span.chars, (6, 10));
        assert_eq!(user_span.lines, (1, 1));
        assert_eq!(user_span.cols, (7, 11));

        // Test span_of for "id"
        let id_span = span_of(src, "id").unwrap();
        assert_eq!(id_span.chars, (17, 19));
        assert_eq!(id_span.lines, (2, 2));
        assert_eq!(id_span.cols, (5, 7));

        // Test span_of for "string"
        let string_span = span_of(src, "string").unwrap();
        assert_eq!(string_span.chars, (21, 27));
        assert_eq!(string_span.lines, (2, 2));
        assert_eq!(string_span.cols, (9, 15));
    }
}
