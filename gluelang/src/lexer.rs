#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Token {
    pub payload: TokenPayload,
    pub pos: (usize, usize),
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum TokenPayload {
    Identifier(String),
    String(String),
    Colon,
    Comma,
    OpenBrace,
    CloseBrace,
    At,
    Eof,
}

#[derive(Debug, thiserror::Error)]
pub enum LexerError {
    #[error("Unterminated string literal")]
    UnterminatedString,
    #[error("Invalid escape sequence")]
    InvalidEscape,
}

pub struct Lexer {
    input: Vec<char>,
    pos: usize,
    line: usize,
    column: usize,
}

impl Lexer {
    pub fn new(input: &str) -> Self {
        Self {
            input: input.chars().collect(),
            pos: 0,
            line: 1,
            column: 1,
        }
    }

    fn peek(&self) -> char {
        if self.pos >= self.input.len() {
            '\0'
        } else {
            self.input[self.pos]
        }
    }

    fn advance(&mut self) {
        if self.pos < self.input.len() {
            if self.input[self.pos] == '\n' {
                self.line += 1;
                self.column = 1;
            } else {
                self.column += 1;
            }
            self.pos += 1;
        }
    }

    fn skip_whitespace(&mut self) {
        while self.peek().is_whitespace() {
            self.advance();
        }
    }

    fn read_identifier(&mut self) -> String {
        let mut id = String::new();
        while self.peek().is_alphabetic() || self.peek() == '_' {
            id.push(self.peek());
            self.advance();
        }
        id
    }

    fn read_string(&mut self) -> Result<String, LexerError> {
        let mut string = String::new();
        self.advance(); // Skip opening quote

        while self.pos < self.input.len() {
            match self.peek() {
                '"' => {
                    self.advance(); // Consume closing quote
                    return Ok(string);
                }
                '\\' => {
                    self.advance(); // Consume backslash
                    if self.pos >= self.input.len() {
                        return Err(LexerError::UnterminatedString);
                    }

                    // Handle escape sequences
                    let escaped = match self.peek() {
                        'n' => '\n',
                        'r' => '\r',
                        't' => '\t',
                        '\\' => '\\',
                        '"' => '"',
                        _ => return Err(LexerError::InvalidEscape),
                    };
                    string.push(escaped);
                    self.advance();
                }
                c => {
                    string.push(c);
                    self.advance();
                }
            }
        }

        Err(LexerError::UnterminatedString)
    }

    fn next_token(&mut self) -> Result<Token, LexerError> {
        self.skip_whitespace();

        let pos = (self.line, self.column);

        if self.pos >= self.input.len() {
            return Ok(Token {
                payload: TokenPayload::Eof,
                pos,
            });
        }

        let token = match self.peek() {
            '{' => {
                self.advance();
                TokenPayload::OpenBrace
            }
            '}' => {
                self.advance();
                TokenPayload::CloseBrace
            }
            ':' => {
                self.advance();
                TokenPayload::Colon
            }
            ',' => {
                self.advance();
                TokenPayload::Comma
            }
            '@' => {
                self.advance();
                TokenPayload::At
            }
            '"' => TokenPayload::String(self.read_string()?),
            c if c.is_alphabetic() => TokenPayload::Identifier(self.read_identifier()),
            _ => {
                self.advance();
                TokenPayload::Identifier(String::new())
            }
        };

        Ok(Token { payload: token, pos })
    }

    pub fn lex(&mut self) -> Result<Vec<Token>, LexerError> {
        let mut tokens = Vec::new();
        loop {
            let token = self.next_token()?;
            if token.payload == TokenPayload::Eof {
                break;
            }
            tokens.push(token);
        }
        tokens.push(Token {
            payload: TokenPayload::Eof,
            pos: (self.line, self.column),
        });
        Ok(tokens)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use pretty_assertions::assert_eq;

    #[test]
    fn test_basic_model() {
        let code = r#"
				Person {
                    name: string,
                    age: int  }
				"#
        .trim();

        let mut lexer = Lexer::new(code);
        let tokens = lexer.lex().unwrap();
        let token_payloads = tokens.iter().map(|t| t.payload.clone()).collect::<Vec<_>>();

        assert_eq!(
            token_payloads,
            vec![
                TokenPayload::Identifier("Person".to_string()),
                TokenPayload::OpenBrace,
                TokenPayload::Identifier("name".to_string()),
                TokenPayload::Colon,
                TokenPayload::Identifier("string".to_string()),
                TokenPayload::Comma,
                TokenPayload::Identifier("age".to_string()),
                TokenPayload::Colon,
                TokenPayload::Identifier("int".to_string()),
                TokenPayload::CloseBrace,
                TokenPayload::Eof
            ]
        );
    }

    #[test]
    fn test_positions() {
        let code = r#"name: int
}"#
        .trim();

        let mut lexer = Lexer::new(code);
        let tokens = lexer.lex().unwrap();

        assert_eq!(
            tokens[0],
            Token {
                payload: TokenPayload::Identifier("name".to_string()),
                pos: (1, 1)
            }
        );
        assert_eq!(
            tokens[1],
            Token {
                payload: TokenPayload::Colon,
                pos: (1, 5)
            }
        );
        assert_eq!(
            tokens[2],
            Token {
                payload: TokenPayload::Identifier("int".to_string()),
                pos: (1, 7),
            }
        );
        assert_eq!(
            tokens[3],
            Token {
                payload: TokenPayload::CloseBrace,
                pos: (2, 1),
            }
        );
        // assert_eq!(tokens[2].pos, (2, 5));
    }

    #[test]
    fn test_nested_model() {
        let code = r#"
    				Person {
                        name: string,
                        age: int,
                        stats: PersonStats {
                            height_in_cm: int,
                            weight_in_kg: int
                        }
    				}
    				"#;

        let mut lexer = Lexer::new(code);
        let tokens = lexer.lex().unwrap();
        let token_payloads = tokens.iter().map(|t| t.payload.clone()).collect::<Vec<_>>();

        assert_eq!(
            token_payloads,
            vec![
                TokenPayload::Identifier("Person".to_string()),
                TokenPayload::OpenBrace,
                TokenPayload::Identifier("name".to_string()),
                TokenPayload::Colon,
                TokenPayload::Identifier("string".to_string()),
                TokenPayload::Comma,
                TokenPayload::Identifier("age".to_string()),
                TokenPayload::Colon,
                TokenPayload::Identifier("int".to_string()),
                TokenPayload::Comma,
                TokenPayload::Identifier("stats".to_string()),
                TokenPayload::Colon,
                TokenPayload::Identifier("PersonStats".to_string()),
                TokenPayload::OpenBrace,
                TokenPayload::Identifier("height_in_cm".to_string()),
                TokenPayload::Colon,
                TokenPayload::Identifier("int".to_string()),
                TokenPayload::Comma,
                TokenPayload::Identifier("weight_in_kg".to_string()),
                TokenPayload::Colon,
                TokenPayload::Identifier("int".to_string()),
                TokenPayload::CloseBrace,
                TokenPayload::CloseBrace,
                TokenPayload::Eof
            ]
        );
    }

    #[test]
    fn test_referenced_model() {
        let code = r#"
    				Person {
                        name: string,
                        age: int,
                        stats: PersonStats
    				}

                    PersonStats {
                        height_in_cm: int,
                        weight_in_kg: int
                    }
    				"#;

        let mut lexer = Lexer::new(code);
        let tokens = lexer.lex().unwrap();
        let token_payloads = tokens.iter().map(|t| t.payload.clone()).collect::<Vec<_>>();

        assert_eq!(
            token_payloads,
            vec![
                TokenPayload::Identifier("Person".to_string()),
                TokenPayload::OpenBrace,
                TokenPayload::Identifier("name".to_string()),
                TokenPayload::Colon,
                TokenPayload::Identifier("string".to_string()),
                TokenPayload::Comma,
                TokenPayload::Identifier("age".to_string()),
                TokenPayload::Colon,
                TokenPayload::Identifier("int".to_string()),
                TokenPayload::Comma,
                TokenPayload::Identifier("stats".to_string()),
                TokenPayload::Colon,
                TokenPayload::Identifier("PersonStats".to_string()),
                TokenPayload::CloseBrace,
                TokenPayload::Identifier("PersonStats".to_string()),
                TokenPayload::OpenBrace,
                TokenPayload::Identifier("height_in_cm".to_string()),
                TokenPayload::Colon,
                TokenPayload::Identifier("int".to_string()),
                TokenPayload::Comma,
                TokenPayload::Identifier("weight_in_kg".to_string()),
                TokenPayload::Colon,
                TokenPayload::Identifier("int".to_string()),
                TokenPayload::CloseBrace,
                TokenPayload::Eof
            ]
        );
    }

    #[test]
    fn test_string_escapes() {
        let code = r#""Hello\n\t\"World\"""#;
        let mut lexer = Lexer::new(code);
        let tokens = lexer.lex().unwrap();

        assert_eq!(tokens[0].payload, TokenPayload::String("Hello\n\t\"World\"".to_string()));
    }

    #[test]
    fn test_unterminated_string() {
        let code = r#""unterminated"#;
        let mut lexer = Lexer::new(code);
        assert!(matches!(lexer.lex(), Err(LexerError::UnterminatedString)));
    }

    #[test]
    fn test_invalid_escape() {
        let code = r#""invalid\escape""#;
        let mut lexer = Lexer::new(code);
        assert!(matches!(lexer.lex(), Err(LexerError::InvalidEscape)));
    }
}
