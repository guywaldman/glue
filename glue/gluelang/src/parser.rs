use pest::{
    Parser as PestParser, Span,
    error::{Error, ErrorVariant},
    iterators::Pair,
};
use pest_derive::Parser as PestParserDerive;

use crate::{
    SourceCodeMetadata, TypeAtom,
    ast::{Ast, AstBuilder, AstNodeId, Decorator, DocBlock, Endpoint, Field, Ident, Model, RawAstNode, RawTypeAtom},
};

#[derive(PestParserDerive)]
#[grammar = "grammar.pest"]
struct GrammarParser;

#[derive(Debug, Clone)]
pub struct ParsedProgram<'a> {
    pub ast: Ast<'a>,
}

// We box the error to reduce its size
type ParseError = Box<Error<Rule>>;
type ParseResult<'a, T = AstNodeId> = Result<T, ParseError>;

pub struct Parser<'a> {
    ast_builder: AstBuilder<'a>,
    source_code_metadata: &'a SourceCodeMetadata<'a>,
}

impl<'a> Parser<'a> {
    pub fn new(source_code_metadata: &'a SourceCodeMetadata<'a>) -> Self {
        Self {
            ast_builder: AstBuilder::new(source_code_metadata),
            source_code_metadata,
        }
    }

    pub fn parse(mut self) -> miette::Result<ParsedProgram<'a>> {
        let pairs = GrammarParser::parse(Rule::program, self.source_code_metadata.contents).map_err(|e| e.into_miette())?;
        for pair in pairs {
            match pair.as_rule() {
                Rule::model => {
                    self.parse_model(pair, true).map_err(|e| e.into_miette())?;
                }
                Rule::endpoint => {
                    self.parse_endpoint(pair).map_err(|e| e.into_miette())?;
                }
                _ => { /* Handle other rules as needed */ }
            }
        }

        let ast = self.ast_builder.build();
        Ok(ParsedProgram { ast })
    }

    fn parse_type_atoms(&mut self, pair: Pair<'a, Rule>) -> ParseResult<'a, Vec<AstNodeId>> {
        let mut type_atom_node_ids: Vec<AstNodeId> = vec![];
        for type_atom_pair in pair.clone().into_inner().find_tagged("type_atom") {
            let ty = self.parse_type_atom(type_atom_pair)?;
            type_atom_node_ids.push(ty);
        }
        Ok(type_atom_node_ids)
    }

    fn parse_type_atom(&mut self, pair: Pair<'a, Rule>) -> ParseResult<'a> {
        let span = pair.as_span();

        let mut is_array = false;
        let mut is_optional = false;
        if let Some(modifiers) = pair.clone().into_inner().find_first_tagged("modifiers") {
            let modifiers_str = modifiers.as_str();
            if modifiers_str.contains("[]") {
                is_array = true;
            }
            if modifiers_str.contains("?") {
                is_optional = true;
            }
        }

        match pair.as_rule() {
            Rule::anon_model => {
                let model = pair.clone().into_inner().next().ok_or_else(|| self.error("Expected model inside anon_model", pair.as_span()))?;
                let parsed_model = self.parse_model(model, false)?;
                Ok(self.ast_builder.add_node(
                    RawAstNode::TypeAtom(TypeAtom {
                        payload: RawTypeAtom::AnonModel(parsed_model),
                        is_array,
                        is_optional,
                    }),
                    span,
                ))
            }
            Rule::builtin_type => {
                let ty_str = pair.as_str();
                let ty = match ty_str {
                    "string" => RawTypeAtom::String,
                    "int" => RawTypeAtom::Int,
                    "bool" => RawTypeAtom::Bool,
                    _ => return Err(self.error(&format!("Unknown type '{}'", ty_str), pair.as_span())),
                };
                Ok(self.ast_builder.add_node(RawAstNode::TypeAtom(TypeAtom { payload: ty, is_array, is_optional }), span))
            }
            Rule::ident => Ok(self.ast_builder.add_node(
                RawAstNode::TypeAtom(TypeAtom {
                    payload: RawTypeAtom::Ref(pair.as_str()),
                    is_array,
                    is_optional,
                }),
                pair.as_span(),
            )),
            _ => Err(self.error(&format!("Unsupported or unknown type: {}", pair.as_str()), pair.as_span())),
        }
    }

    fn parse_field(&mut self, pair: Pair<'a, Rule>) -> ParseResult<'a> {
        let name = pair.clone().into_inner().find_first_tagged("field_name").unwrap().as_str();
        let doc = self.parse_doc_block(pair.clone()).ok();

        let field_type = pair.clone().into_inner().find_first_tagged("field_type").unwrap();
        let type_atoms = self.parse_type_atoms(field_type)?;
        let field_node = RawAstNode::Field(Field { name, doc, type_atoms });
        Ok(self.ast_builder.add_node(field_node, pair.as_span()))
    }

    fn parse_doc_block(&mut self, pair: Pair<'a, Rule>) -> Result<DocBlock, ParseError> {
        let doc_lines: Vec<&'a str> = pair.clone().into_inner().filter(|p| p.as_rule() == Rule::doc_block).map(|p| p.as_str()).collect();
        let mut doc = String::new();
        for line in doc_lines {
            let line = line.trim_start_matches("///").trim();
            if !doc.is_empty() {
                doc.push('\n');
            }
            doc.push_str(line);
        }
        Ok(DocBlock { content: doc })
    }

    fn parse_model(&mut self, pair: Pair<'a, Rule>, top_level: bool) -> ParseResult<'a> {
        let is_anon = pair.as_rule() == Rule::model_body;
        if is_anon {
            // Nested model
            let mut nested_models = Vec::new();
            let mut fields = Vec::new();
            let pairs = pair.clone().into_inner();
            for pair in pairs {
                match pair.as_rule() {
                    Rule::model => {
                        nested_models.push(self.parse_model(pair, false)?);
                    }
                    Rule::field => {
                        fields.push(self.parse_field(pair)?);
                    }
                    _ => {}
                }
            }
            let node = RawAstNode::Model(Model {
                name: "<anonymous>",
                ident: None,
                is_anon: true,
                decorators: Vec::new(),
                nested_models,
                fields,
            });
            return Ok(self.ast_builder.add_node(node, pair.as_span()));
        }

        let mut decorators = Vec::new();
        let mut nested_models = Vec::new();
        let mut fields = Vec::new();
        let pairs = pair.clone().into_inner();

        let ident_node = pairs.find_first_tagged("model_name").ok_or_else(|| self.error("Expected model name", pair.as_span()))?;
        let ident_node_id = self.ast_builder.add_node(RawAstNode::Ident(Ident { name: ident_node.as_str() }), ident_node.as_span());
        let name = ident_node.as_str();

        for pair in pairs.clone() {
            match pair.as_rule() {
                Rule::decorator => {
                    let pairs = pair.clone().into_inner();
                    let decorator_name = pairs.find_first_tagged("decorator_name").ok_or_else(|| self.error("Expected decorator name", pair.as_span()))?.as_str();
                    let decorator_id = self.ast_builder.add_node(RawAstNode::Decorator(Decorator { name: decorator_name }), pair.as_span());
                    decorators.push(decorator_id);
                }
                Rule::model_body => {
                    let pairs = pair.clone().into_inner();
                    for pair in pairs {
                        match pair.as_rule() {
                            Rule::model => {
                                nested_models.push(self.parse_model(pair, false)?);
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
        let model_ast_node = RawAstNode::Model(Model {
            name,
            ident: Some(ident_node_id),
            is_anon,
            decorators,
            nested_models,
            fields,
        });

        if top_level {
            Ok(self.ast_builder.add_top_level_node(model_ast_node, pair.as_span()))
        } else {
            Ok(self.ast_builder.add_node(model_ast_node, pair.as_span()))
        }
    }

    fn parse_endpoint(&mut self, pair: Pair<'a, Rule>) -> ParseResult<'a> {
        let pairs = pair.clone().into_inner();
        let name = pairs
            .clone()
            .find_first_tagged("endpoint_name")
            .ok_or_else(|| self.error("Expected endpoint name", pair.as_span()))?
            .as_str();
        let fields: Vec<_> = pairs.clone().find_tagged("top_level_field").map(|p| self.parse_field(p)).collect::<Result<_, _>>()?;
        // Endpoints are always top-level
        Ok(self.ast_builder.add_top_level_node(RawAstNode::Endpoint(Endpoint { name, fields }), pair.as_span()))
    }

    fn error(&self, message: &str, span: Span) -> Box<Error<Rule>> {
        Box::new(Error::new_from_span(ErrorVariant::CustomError { message: message.to_string() }, span))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use indoc::indoc;

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
        assert!(
            Parser::new(&SourceCodeMetadata {
                file_name: "example.glue",
                contents: code
            })
            .parse()
            .is_ok()
        );
    }

    #[test]
    fn test_model_basic() {
        let code = indoc! {r#"
            model User {
                id: string
            }
        "#}
        .trim();
        assert!(
            Parser::new(&SourceCodeMetadata {
                file_name: "example.glue",
                contents: code
            })
            .parse()
            .is_ok()
        );
    }
}
