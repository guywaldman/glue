use crate::{
    ast::{Ast, AstNode, AstNodeExt, ModelFieldType},
    lexer::{Lexeme, LexemeValue},
};

type ParseResult<'a> = Result<(AstNode, &'a [Lexeme]), ParserError>;

#[derive(Debug, thiserror::Error)]
pub enum ParserError {
    #[error("Unexpected token at {row}:{col}: {msg}")]
    UnexpectedToken { row: usize, col: usize, msg: String },
    #[error("Unexpected end of input")]
    UnexpectedEndOfInput,
}

pub struct Parser {}

impl Parser {
    pub fn new() -> Self {
        Self {}
    }

    /// Parse the tokens into an AST.
    ///
    /// The parser is implemented as a recursive descent parser.
    pub fn parse(&self, tokens: &[Lexeme]) -> Result<Ast, ParserError> {
        let (model, _rest) = self.parse_model(tokens)?;
        Ok(Ast { nodes: vec![model] })
    }

    /// Production: Model
    ///  = Identifier OpenBrace ModelFields CloseBrace
    fn parse_model<'a>(&self, tokens: &'a [Lexeme]) -> ParseResult<'a> {
        // Parse identifier
        let (model_name, rest) = self.parse_identifier(tokens)?;
        let AstNode::Identifier { name, pos } = model_name else {
            return Err(ParserError::UnexpectedToken {
                row: model_name.pos().0,
                col: model_name.pos().1,
                msg: "Expected identifier".to_string(),
            });
        };

        // Parse open brace
        let (open_brace, rest) = rest.split_first().ok_or(ParserError::UnexpectedEndOfInput)?;
        if open_brace.value != LexemeValue::OpenBrace {
            return Err(ParserError::UnexpectedToken {
                row: open_brace.pos.0,
                col: open_brace.pos.1,
                msg: "Expected open brace".to_string(),
            });
        }

        let (fields, rest) = self.parse_model_fields(rest)?;

        // Expect close brace
        let (close_brace, rest) = rest.split_first().ok_or(ParserError::UnexpectedEndOfInput)?;
        if close_brace.value != LexemeValue::CloseBrace {
            return Err(ParserError::UnexpectedToken {
                row: close_brace.pos.0,
                col: close_brace.pos.1,
                msg: "Expected close brace".to_string(),
            });
        }

        Ok((AstNode::Model { name, pos, fields }, rest))
    }

    fn parse_model_fields<'a>(&self, tokens: &'a [Lexeme]) -> Result<(Vec<AstNode>, &'a [Lexeme]), ParserError> {
        let mut total_rest = tokens;

        let mut fields = vec![];
        while total_rest.first().ok_or(ParserError::UnexpectedEndOfInput)?.value != LexemeValue::CloseBrace {
            dbg!(1);
            let (field, rest) = self.parse_model_field(total_rest)?;
            fields.push(field);

            total_rest = rest;
        }

        Ok((fields, &tokens[tokens.len() - total_rest.len()..]))
    }

    fn parse_model_field<'a>(&self, tokens: &'a [Lexeme]) -> ParseResult<'a> {
        // Expect identifier
        let (ident, rest) = self.parse_identifier(tokens)?;
        let AstNode::Identifier { name: field_name, pos: _ } = ident else {
            return Err(ParserError::UnexpectedToken {
                row: ident.pos().0,
                col: ident.pos().1,
                msg: "Expected identifier".to_string(),
            });
        };

        // Expect colon
        let (colon, rest) = rest.split_first().ok_or(ParserError::UnexpectedEndOfInput)?;
        if colon.value != LexemeValue::Colon {
            return Err(ParserError::UnexpectedToken {
                row: colon.pos.0,
                col: colon.pos.1,
                msg: "Expected colon".to_string(),
            });
        }

        // Parse model field type
        let (value, rest) = self.parse_identifier(rest)?;
        let AstNode::Identifier { name, pos } = &value else {
            return Err(ParserError::UnexpectedToken {
                row: value.pos().0,
                col: value.pos().1,
                msg: "Expected identifier".to_string(),
            });
        };
        let typ = match name.as_str() {
            "string" => ModelFieldType::String,
            "int" => ModelFieldType::Integer,
            "bool" => ModelFieldType::Boolean,
            _ => {
                return Err(ParserError::UnexpectedToken {
                    row: value.pos().0,
                    col: value.pos().1,
                    msg: format!("Unknown model field {}", name),
                })
            }
        };

        Ok((
            AstNode::ModelField {
                name: field_name,
                typ,
                pos: *pos,
            },
            rest,
        ))
    }

    fn parse_identifier<'a>(&self, tokens: &'a [Lexeme]) -> ParseResult<'a> {
        match tokens.first() {
            Some(Lexeme {
                value: LexemeValue::Identifier(ident),
                pos,
            }) => Ok((
                AstNode::Identifier {
                    name: ident.clone(),
                    pos: *pos,
                },
                &tokens[1..],
            )),
            _ => Err(ParserError::UnexpectedEndOfInput),
        }
    }
}

impl Default for Parser {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use crate::lexer::Lexer;

    use super::*;
    use pretty_assertions::assert_eq;

    #[test]
    fn test_parse_model() {
        let code = r#"
				Person {
                    name: string,
                    age: int  }
				"#
        .trim();

        let mut lexer = Lexer::new(code);
        let tokens = lexer.lex().unwrap();
        let parser = Parser::new();
        let ast = parser.parse(&tokens);
        let Ok(ast) = ast else {
            panic!("Unexpected error parsing model: {:?}", ast);
        };

        assert_eq!(
            ast.nodes,
            vec![AstNode::Model {
                name: "Person".to_string(),
                fields: vec![],
                // fields: vec![
                //     AstNode::ModelField {
                //         name: "name".to_string(),
                //         value: ModelField::String("string".to_string()),
                //         pos: (1, 1)
                //     },
                //     AstNode::ModelField {
                //         name: "age".to_string(),
                //         value: ModelField::Integer(0),
                //         pos: (1, 7)
                //     }
                // ],
                pos: (1, 1)
            }]
        );
    }
}
