use pest::{
    Parser, Span,
    error::{Error, ErrorVariant},
    iterators::Pair,
};
use pest_derive::Parser;

use crate::ast::{Ast, AstNode, Decorator, Endpoint, Field, Model, Type};

#[derive(Parser)]
#[grammar = "grammar.pest"]
struct GrammarParser;

#[derive(Debug)]
pub struct ParsedProgram<'a> {
    pub ast: Ast<'a>,
}

// We box the error to reduce its size
type ParseResult<'a, T = AstNode<'a>> = Result<T, Box<Error<Rule>>>;

#[derive(Debug, Default)]
pub struct GlueParser;

impl GlueParser {
    pub fn new() -> Self {
        Self {}
    }

    pub fn parse<'a>(&self, input: &'a str) -> miette::Result<ParsedProgram<'a>> {
        let pairs = GrammarParser::parse(Rule::program, input).map_err(|e| e.into_miette())?;
        let mut nodes = Vec::new();
        for pair in pairs {
            match pair.as_rule() {
                Rule::model => {
                    nodes.push(self.parse_model(pair).map_err(|e| e.into_miette())?);
                }
                Rule::endpoint => {
                    nodes.push(self.parse_endpoint(pair).map_err(|e| e.into_miette())?);
                }
                _ => { /* Handle other rules as needed */ }
            }
        }
        Ok(ParsedProgram { ast: Ast { nodes } })
    }

    fn parse_type<'a>(&self, pair: Pair<'a, Rule>) -> ParseResult<'a> {
        let inner = pair.clone().into_inner().next().ok_or_else(|| self.error("Expected type", pair.as_span()))?;
        let span = pair.as_span();
        match inner.as_rule() {
            Rule::anon_model => {
                let model = inner.clone().into_inner().next().ok_or_else(|| self.error("Expected model inside anon_model", inner.as_span()))?;
                Ok(AstNode::Type {
                    payload: Type::AnonModel(Box::new(self.parse_model(model)?)),
                    span,
                })
            }
            Rule::builtin_type => {
                let ty_str = inner.as_str();
                let ty = match ty_str {
                    "string" => Type::String,
                    "int" => Type::Int,
                    _ => return Err(self.error(&format!("Unknown type '{}'", ty_str), inner.as_span())),
                };
                let field_ast_node = AstNode::Type { payload: ty, span: inner.as_span() };
                Ok(field_ast_node)
            }
            Rule::ident => {
                let field_ast_node = AstNode::Type {
                    payload: Type::Ref(inner.as_str()),
                    span: pair.as_span(),
                };
                Ok(field_ast_node)
            }
            _ => Err(self.error(&format!("Unsupported or unknown type: {}", inner.as_str()), pair.as_span())),
        }
    }

    fn parse_field<'a>(&self, pair: Pair<'a, Rule>) -> ParseResult<'a> {
        let field_name = pair.clone().into_inner().find_first_tagged("field_name").unwrap().as_str();
        let ty = self.parse_type(
            pair.clone()
                .into_inner()
                .find_first_tagged("field_type")
                .ok_or_else(|| self.error("Expected field type", pair.as_span()))?,
        )?;
        let field_ast_node = AstNode::Field {
            payload: Field { name: field_name, ty: Box::new(ty) },
            span: pair.as_span(),
        };
        Ok(field_ast_node)
    }

    fn parse_model<'a>(&self, pair: Pair<'a, Rule>) -> ParseResult<'a> {
        let is_anon = pair.as_rule() == Rule::model_body;
        if is_anon {
            // Nested model
            let mut nested_models = Vec::new();
            let mut fields = Vec::new();
            let pairs = pair.clone().into_inner();
            for pair in pairs {
                match pair.as_rule() {
                    Rule::model => {
                        nested_models.push(self.parse_model(pair)?);
                    }
                    Rule::field => {
                        fields.push(self.parse_field(pair)?);
                    }
                    _ => {}
                }
            }
            let model_ast_node = AstNode::Model {
                payload: Model {
                    name: "<anonymous>",
                    is_anon: true,
                    decorators: Vec::new(),
                    nested_models,
                    fields,
                },
                span: pair.as_span(),
            };
            return Ok(model_ast_node);
        }

        let mut decorators = Vec::new();
        let mut nested_models = Vec::new();
        let mut fields = Vec::new();
        let pairs = pair.clone().into_inner();
        for pair in pairs.clone() {
            match pair.as_rule() {
                Rule::decorator => {
                    let pairs = pair.clone().into_inner();
                    let decorator_name = pairs.find_first_tagged("decorator_name").ok_or_else(|| self.error("Expected decorator name", pair.as_span()))?.as_str();
                    let decorator_ast_node = AstNode::Decorator {
                        payload: Decorator { name: decorator_name },
                        span: pair.as_span(),
                    };
                    decorators.push(decorator_ast_node);
                }
                Rule::model_body => {
                    let pairs = pair.clone().into_inner();
                    for pair in pairs {
                        match pair.as_rule() {
                            Rule::model => {
                                nested_models.push(self.parse_model(pair)?);
                            }
                            Rule::field => {
                                fields.push(self.parse_field(pair)?);
                            }
                            _ => {}
                        }
                    }
                }
                _ => {}
            }
        }
        let name = match is_anon {
            false => pairs.find_first_tagged("model_name").ok_or_else(|| self.error("Expected model name", pair.as_span()))?.as_str(),
            true => "<anonymous>",
        };
        let model_ast_node = AstNode::Model {
            payload: Model {
                name,
                is_anon,
                decorators,
                nested_models,
                fields,
            },
            span: pair.as_span(),
        };
        Ok(model_ast_node)
    }

    fn parse_endpoint<'a>(&self, pair: Pair<'a, Rule>) -> ParseResult<'a> {
        let pairs = pair.clone().into_inner();
        let endpoint_name = pairs
            .clone()
            .find_first_tagged("endpoint_name")
            .ok_or_else(|| self.error("Expected endpoint name", pair.as_span()))?
            .as_str();
        let fields: Vec<_> = pairs.clone().find_tagged("top_level_field").map(|p| self.parse_field(p)).collect::<Result<_, _>>()?;
        Ok(AstNode::Endpoint {
            payload: Endpoint { name: endpoint_name, fields },
            span: pair.as_span(),
        })
    }

    fn error(&self, message: &str, span: Span) -> Box<Error<Rule>> {
        Box::new(Error::new_from_span(ErrorVariant::CustomError { message: message.to_string() }, span))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use indoc::indoc;
    use pest::{consumes_to, parses_to};

    #[test]
    fn test_basic() {
        let code = indoc! {r#"
        @endpoint("GET /users/{id}")
        endpoint GetUserById {
            request: {
                id: int
            }

            headers: {
                "X-Request-ID": string
            }

            responses: {
                200: User
                4XX: ErrorResponse
                5XX: ErrorResponse
            }
        }

        @foo(42, "asda", foo="bar")
        model User {
            id: int

            details: UserDetails

            model UserDetails {
                name: string
                email: string
            }
        }
        "#}
        .trim();
        assert!(GlueParser::new().parse(code).is_ok());
    }

    #[test]
    fn test_model_basic() {
        let code = indoc! {r#"
            model User {
                id: string
            }
        "#}
        .trim();
        assert!(GlueParser::new().parse(code).is_ok());
    }
}
