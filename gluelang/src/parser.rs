use crate::{ast::Ast, lexer::TokenPayload};

#[derive(Debug, thiserror::Error)]
pub enum ParserError {
    #[error("Unexpected token at {row}:{col}: {msg}")]
    UnexpectedToken { row: usize, col: usize, msg: String },
}

pub struct Parser {}

impl Parser {
    pub fn new() -> Self {
        Self {}
    }

    /// Parse the tokens into an AST.
    ///
    /// The parser is implemented as a recursive descent parser.
    pub fn parse(tokens: &[TokenPayload]) -> Result<Ast, ParserError> {
        // Production: ModelFields
        //  = ModelField (Comma ModelField)* Comma?
        Ok(Ast {})
    }

    /// Production: Model
    ///  = Identifier OpenBrace ModelFields CloseBrace
    fn parse_model(tokens: &[TokenPayload]) -> Result<Ast, ParserError> {
        todo!()
        // if !tokens.starts_with(&[TokenPayload::Identifier(_), TokenPayload::OpenBrace]) {
        //     return Err(ParserError::UnexpectedToken);
        // }
    }
}
