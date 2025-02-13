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
    Keyword(Keyword),
    At,
    Eof,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Keyword {}

#[derive(Debug, thiserror::Error)]
pub enum LexerError {
    #[error("Unexpected token {0}")]
    UnexpectedToken(String),
}

pub struct Lexer {
    /// Source code.
    input: Vec<char>,
    /// Current reading position.
    pub pos_byte: usize,
    // Current reading position by row and column.
    /// Current row (line number) starts at 1. Current column (character number) starts at 1.
    pub pos: (usize, usize),
}

impl Lexer {
    fn new(input: &[char]) -> Self {
        Self {
            input: input.to_vec(),
            pos_byte: 0,
            pos: (1, 1),
        }
    }

    pub fn lex(&mut self) -> Result<Vec<Token>, LexerError> {
        let mut tokens = Vec::new();
        while let Ok(Token { payload, pos, .. }) = self.next_token() {
            if payload == TokenPayload::Eof {
                break;
            }
            tokens.push(Token { payload, pos });
            self.advance_char()?;
        }
        Ok(tokens)
    }

    fn peek(&mut self) -> Result<char, LexerError> {
        if self.pos_byte >= self.input.len() - 2 {
            Ok('\0')
        } else {
            Ok(self.input[self.pos_byte])
        }
    }

    fn peek_ahead(&mut self) -> Result<char, LexerError> {
        if self.pos_byte >= self.input.len() - 1 {
            Ok('\0')
        } else {
            Ok(self.input[self.pos_byte + 1])
        }
    }

    /// Read the next character from the input string, updating the current position.
    fn advance_char(&mut self) -> Result<(), LexerError> {
        if self.pos_byte >= self.input.len() - 1 {
            // Past the end of the input.
        } else {
            let ch = self.input[self.pos_byte];

            self.pos_byte += 1;

            if ch == '\n' {
                self.pos.0 += 1;
                self.pos.1 = 1;
            } else {
                self.pos.1 += 1;
            }
        }
        Ok(())
    }

    /// Match the read character and assign the appropriate type.
    fn next_token(&mut self) -> Result<Token, LexerError> {
        // Advance to the next character.
        if self.peek()? == '\0' {
            return Ok(Token {
                payload: TokenPayload::Eof,
                pos: self.pos,
            });
        }

        let token_start_pos = self.pos;

        let tok = self.peek()?;
        dbg!(tok);

        // Skip whitespace and newlines.
        while self.peek()?.is_whitespace() {
            let tok = self.peek()?;
            dbg!(tok);

            self.advance_char()?;
        }

        let tok = self.peek()?;
        dbg!(tok);

        if self.peek()? == '\0' {
            return Ok(Token {
                payload: TokenPayload::Eof,
                pos: token_start_pos,
            });
        }

        let token = match self.peek()? {
            c if c.is_alphabetic() => {
                let mut identifier = String::new();
                while self.peek()?.is_alphabetic() || self.peek()? == '_' {
                    identifier.push(self.peek()?);

                    // Only advance if the next character will be consumed.
                    // TODO: Think of a better way to do this.
                    if self.peek_ahead()?.is_alphabetic() || self.peek_ahead()? == '_' {
                        self.advance_char()?;
                    } else {
                        break;
                    }
                }
                TokenPayload::Identifier(identifier)
            }
            '"' => {
                let mut string_literal = String::new();
                while self.peek()? != '"' {
                    string_literal.push(self.peek()?);
                    self.advance_char()?;
                }
                // Closing quote is consumed by `self.advance_char()`.
                TokenPayload::String(string_literal)
            }
            ',' => TokenPayload::Comma,
            '{' => TokenPayload::OpenBrace,
            '}' => TokenPayload::CloseBrace,
            ':' => TokenPayload::Colon,
            '@' => TokenPayload::At,
            _ => TokenPayload::Identifier(String::new()),
        };

        Ok(Token {
            payload: token,
            pos: token_start_pos,
        })
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
                    age: int
				}
				"#
        .trim();

        let mut lexer = Lexer::new(code.chars().collect::<Vec<_>>().as_slice());
        let tokens = lexer.lex();
        let token_payloads = tokens
            .unwrap()
            .iter()
            .map(|t| t.payload.clone())
            .collect::<Vec<_>>();

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
                TokenPayload::CloseBrace
            ]
        );
    }

    #[test]
    fn test_positions() {
        let code = r#"
        		int
                }
        		"#
        .trim();

        let mut lexer = Lexer::new(code.chars().collect::<Vec<_>>().as_slice());
        let tokens = lexer.lex().unwrap();

        assert_eq!(tokens[0].pos, (1, 1));
        assert_eq!(tokens[1].pos, (1, 8));
        assert_eq!(tokens[2].pos, (2, 5));
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

        let mut lexer = Lexer::new(code.chars().collect::<Vec<_>>().as_slice());
        let tokens = lexer.lex();
        let token_payloads = tokens
            .unwrap()
            .iter()
            .map(|t| t.payload.clone())
            .collect::<Vec<_>>();

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
                TokenPayload::CloseBrace
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

        let mut lexer = Lexer::new(code.chars().collect::<Vec<_>>().as_slice());
        let tokens = lexer.lex();
        let token_payloads = tokens
            .unwrap()
            .iter()
            .map(|t| t.payload.clone())
            .collect::<Vec<_>>();

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
                TokenPayload::CloseBrace
            ]
        );
    }
}
