use std::collections::{BTreeMap, HashMap};

use regex::Regex;

use crate::{
    Field, Span, TypeAtom,
    diagnostics::{LangError, LangResult},
    lexer::{Token, TokenKind, TokenPayload},
    parser::{
        Ast, AstSymbol, Decorator, Endpoint, Enum, SymbolTable,
        ast::{AstNode, AstNodeId, AstNodeKind, AstNodePayload, ConstantValue, PrimitiveType, Type, TypeRef, TypeVariant},
        ast_model::Model,
        tree::TreeNode,
    },
};

/// Macro to expect multiple tokens in succession
macro_rules! expect_tokens {
    ($self:expr, $($token:expr),+ $(,)?) => {
        $(
            $self.expect($token)?;
        )+
    };
}

pub struct Parser<'a> {
    /// AST, constructed during parsing and returned as part of [`ParserArtifacts`].
    ast: Ast,
    /// Symbol table, constructed during parsing and returned as part of [`ParserArtifacts`].
    symbols: SymbolTable,
    file_name: &'a str,
    src: &'a str,
    tokens: &'a [Token],
    current: usize,
}

pub struct ParserArtifacts {
    pub ast: Ast,
    pub symbols: SymbolTable,
}

impl<'a> Parser<'a> {
    pub fn new(file_name: &'a str, src: &'a str, tokens: &'a [Token]) -> Self {
        let root = AstNode::new(AstNodeKind::Root, AstNodePayload::None);
        let ast = Ast::new_with_root(root);
        Self {
            ast,
            symbols: SymbolTable::default(),
            file_name,
            src,
            tokens,
            current: 0,
        }
    }

    /// Parses a sequence of tokens into an AST.
    pub fn parse(mut self) -> LangResult<ParserArtifacts> {
        let root_id = self.ast.get_root();

        if let Some(global_annotations_node_id) = self.parse_global_annotations()? {
            self.ast.append_child(root_id, global_annotations_node_id);
        }

        while self.peek_kind() != TokenKind::Eof {
            let next_relevant_kind = self.tokens[self.current..]
                .iter()
                .find(|t| t.kind == TokenKind::KeywordModel || t.kind == TokenKind::KeywordEnum || t.kind == TokenKind::KeywordEndpoint)
                .map_or(TokenKind::Eof, |t| t.kind);
            match next_relevant_kind {
                TokenKind::KeywordModel | TokenKind::AtSign => {
                    let (node_id, name) = self.parse_model(false)?;
                    self.ast.append_child(root_id, node_id);
                    // Link model's symbol entry parent to root scope for ancestor visibility.
                    self.symbols.insert(root_id, AstSymbol::Model(name.clone()), node_id);
                }
                TokenKind::KeywordEnum => {
                    let (node_id, name) = self.parse_enum()?;
                    self.ast.append_child(root_id, node_id);
                    self.symbols.insert(root_id, AstSymbol::Enum(name.clone()), node_id);
                }
                TokenKind::KeywordEndpoint => {
                    let (node_id, name) = self.parse_endpoint()?;
                    self.ast.append_child(root_id, node_id);
                    self.symbols.insert(root_id, AstSymbol::Endpoint(name.clone()), node_id);
                }
                TokenKind::Eof => break,
                _ => {
                    // Unknown token at top-level, advance to avoid infinite loop.
                    self.advance()?;
                }
            }
        }
        Ok(ParserArtifacts {
            ast: self.ast,
            symbols: self.symbols,
        })
    }

    fn parse_global_annotations(&mut self) -> LangResult<Option<AstNodeId>> {
        let mut decorators = Vec::new();
        let span = self.curr_span();
        // Parse decorators of the form `[@auth(...)]`
        while self.peek_kind() == TokenKind::LBracket {
            self.expect(TokenKind::LBracket)?;
            decorators.push(self.parse_decorator()?);
            self.expect(TokenKind::RBracket)?;
        }

        let span = span.merge(&self.prev_span());

        if decorators.is_empty() {
            Ok(None)
        } else {
            let global_annotations_node_id = self
                .ast
                .add_node(AstNode::new_with_span(AstNodeKind::GlobalAnnotations, AstNodePayload::GlobalAnnotations, span));
            for (decorator_id, _) in decorators.into_iter() {
                self.ast.append_child(global_annotations_node_id, decorator_id);
            }
            Ok(Some(global_annotations_node_id))
        }
    }

    /// Parses an enum definition.
    ///
    /// ```text
    /// enum := [docblock] "enum" ident "=" enum_variants
    /// enum_variants := string_lit ("|" string_lit)*
    /// ```
    ///
    /// Returns the enum's AST node ID and its name.
    fn parse_enum(&mut self) -> LangResult<(AstNodeId, String)> {
        let doc = self.parse_optional_docblocks()?;
        let span = self.curr_span();
        expect_tokens!(self, TokenKind::KeywordEnum);
        let TokenPayload::String(enum_name) = self.expect(TokenKind::Ident)?.payload else {
            return Err(self.err(self.curr_span(), "Expected enum name identifier", None, Some("EExpectedEnumName")));
        };
        let identifier_node_id = self.ast.add_node(AstNode::new_with_span(
            AstNodeKind::Identifier,
            AstNodePayload::String(enum_name.to_string()),
            self.prev_span(),
        ));
        let enum_name = enum_name.to_string();
        expect_tokens!(self, TokenKind::Colon);

        let mut variants = Vec::new();
        loop {
            let token = self.expect(TokenKind::StringLit)?;
            let TokenPayload::String(variant) = token.payload else {
                let token_span = token.span;
                return Err(self.err(token_span, "Expected string literal for enum variant", None, Some("EExpectedEnumVariant")));
            };
            variants.push(variant.to_string());
            if self.peek_kind() == TokenKind::Pipe {
                self.advance()?;
            } else {
                break;
            }
        }

        let enum_node = AstNode::new_with_span(
            AstNodeKind::Enum,
            AstNodePayload::Enum(Enum {
                name: enum_name.clone(),
                effective_name: None,
                doc,
                variants,
            }),
            span,
        );
        let enum_node_id = self.ast.add_node(enum_node);

        self.ast.append_child(enum_node_id, identifier_node_id);

        Ok((enum_node_id, enum_name))
    }

    /// Parses a model definition.
    ///
    /// ```text
    /// model := [docblock] "model" ident "{"" (field)* "}"
    /// ```
    ///
    /// Returns the model's AST node ID and its name.
    fn parse_model(&mut self, anon: bool) -> LangResult<(AstNodeId, String)> {
        let span = self.curr_span();

        let mut doc = None;
        let mut decorator_node_id = None;
        let mut identifier_node_id = None;
        let mut model_name = "<anonymous>".to_string();

        if !anon {
            // Parse leading docblocks
            doc = self.parse_optional_docblocks()?;

            // Parse decorators
            if self.peek_kind() == TokenKind::AtSign {
                // Decorator(s) present
                while self.peek_kind() == TokenKind::AtSign {
                    decorator_node_id = Some(self.parse_decorator()?.0);
                }
            }
            expect_tokens!(self, TokenKind::KeywordModel);

            let TokenPayload::String(curr_model_name) = self.expect(TokenKind::Ident)?.payload else {
                return Err(self.err(self.curr_span(), "Expected model name identifier", None, Some("EExpectedModelName")));
            };
            let identifier_node = AstNode::new_with_span(AstNodeKind::Identifier, AstNodePayload::String(model_name.clone()), self.prev_span());
            identifier_node_id = Some(self.ast.add_node(identifier_node));

            model_name = curr_model_name.to_string();
        }

        expect_tokens!(self, TokenKind::LBrace);

        let mut symbols = Vec::new();
        let mut fields = BTreeMap::new();
        let mut nested_models: Vec<AstNodeId> = Vec::new();
        while {
            let k = self.peek_kind();
            k != TokenKind::RBrace && k != TokenKind::Eof
        } {
            let curr = self.peek_kind();
            let next = self.peek_ahead_kind(1);
            match (curr, next) {
                (TokenKind::KeywordModel, _) | (TokenKind::DocBlock, TokenKind::KeywordModel) => {
                    // Nested model definition
                    let (nested_id, nested_name) = self.parse_model(anon)?;
                    nested_models.push(nested_id);
                    symbols.push((AstSymbol::Model(nested_name.clone()), nested_id));
                }
                (TokenKind::KeywordEnum, _) | (TokenKind::DocBlock, TokenKind::KeywordEnum) => {
                    // Nested enum definition
                    let (nested_id, nested_name) = self.parse_enum()?;
                    nested_models.push(nested_id);
                    symbols.push((AstSymbol::Enum(nested_name.clone()), nested_id));
                }
                _ => {
                    // Field (possibly with leading docblocks)
                    let (field_node_id, field_name) = self.parse_field()?;
                    fields.insert(field_name.clone(), field_node_id);
                    symbols.push((AstSymbol::Field(field_name.clone()), field_node_id));
                }
            }
        }

        expect_tokens!(self, TokenKind::RBrace);

        let span = span.merge(&self.curr_span());
        let model_ast_node = AstNode::new_with_span(
            AstNodeKind::Model,
            AstNodePayload::Model(Model {
                name: model_name.clone(),
                decorator: decorator_node_id,
                doc,
                effective_name: None,
                fields: fields.clone(),
            }),
            span,
        );
        let model_node_id = self.ast.add_node(model_ast_node);

        if let Some(identifier_node_id) = identifier_node_id {
            self.ast.append_child(model_node_id, identifier_node_id);
        }

        for (symbol, node_id) in symbols.into_iter() {
            self.symbols.insert(model_node_id, symbol, node_id);
        }

        for nested_id in nested_models.into_iter() {
            self.ast.append_child(model_node_id, nested_id);
        }

        for (_, field_node_id) in fields.into_iter() {
            self.ast.append_child(model_node_id, field_node_id);
        }

        if let Some(decorator_id) = decorator_node_id {
            self.ast.append_child(model_node_id, decorator_id);
        }

        Ok((model_node_id, model_name))
    }

    /// Parses a type definition.
    ///
    /// ```text
    /// type := primitive_type | compound_type
    /// primitive_type := ("string" | "int" | "float" | "bool) [ "?" ]
    /// compound_type := ident [ "?" ] | primitive_type "|" type
    /// ```
    fn parse_type(&mut self) -> LangResult<(AstNodeId, Type)> {
        let mut types = Vec::new();
        let mut type_atom_nodes = Vec::new();

        let span = self.curr_span();

        if self.peek_kind() == TokenKind::LBrace {
            // Anonymous model
            let (anon_model_id, _) = self.parse_model(true)?;
            types.push(TypeAtom {
                variant: TypeVariant::AnonymousModel,
                is_optional: false,
                is_array: false,
            });
            let ty = Type::Single(types[0].clone());
            let span = span.merge(&self.prev_span());
            let type_node = AstNode::new_with_span(AstNodeKind::Type, AstNodePayload::Type(ty.clone()), span);
            let type_node_id = self.ast.add_node(type_node);
            self.ast.append_child(type_node_id, anon_model_id);
            return Ok((type_node_id, Type::Single(types[0].clone())));
        }

        loop {
            let type_token = self.advance()?;
            let TokenPayload::String(type_name) = type_token.payload else {
                let span = type_token.span;
                return Err(self.err(span, "Expected type", None, Some("EExpectedType")));
            };

            let variant = match type_name.as_ref() {
                "string" => TypeVariant::Primitive(PrimitiveType::String),
                "int" => TypeVariant::Primitive(PrimitiveType::Int),
                "bool" => TypeVariant::Primitive(PrimitiveType::Bool),
                other_type_name => TypeVariant::Ref(TypeRef {
                    name: other_type_name.to_string(),
                    effective_name: other_type_name.to_string(),
                }),
            };

            let mut is_optional = false;
            if let TokenKind::QuestionMark = self.peek_kind() {
                self.advance()?;
                is_optional = true;
            }

            let mut is_array = false;
            if self.peek_kind() == TokenKind::LBracket && self.peek_ahead_kind(1) == TokenKind::RBracket {
                self.advance_n(2)?;
                is_array = true;
            }

            types.push(TypeAtom { variant, is_optional, is_array });
            let type_atom_node = AstNode::new_with_span(AstNodeKind::TypeAtom, AstNodePayload::TypeAtom(types.last().unwrap().clone()), type_token.span);
            let type_atom_node_id = self.ast.add_node(type_atom_node);
            type_atom_nodes.push(type_atom_node_id);

            if self.peek_kind() == TokenKind::Pipe {
                self.advance()?;
                continue;
            } else {
                break;
            }
        }

        let ty = if types.len() == 1 { Type::Single(types[0].clone()) } else { Type::Union(types.clone()) };

        // Use a span that covers from the start of the first type token to the current position
        // For now, we'll use start_span as a reasonable approximation
        let span = span.merge(&self.prev_span());
        let type_node = AstNode::new_with_span(AstNodeKind::Type, AstNodePayload::Type(ty.clone()), span);
        let type_node_id = self.ast.add_node(type_node);

        for type_atom_id in type_atom_nodes.into_iter() {
            self.ast.append_child(type_node_id, type_atom_id);
        }

        Ok((type_node_id, ty))
    }

    /// Parases an endpoint definition.
    fn parse_endpoint(&mut self) -> LangResult<(AstNodeId, String)> {
        let doc = self.parse_optional_docblocks()?;

        let span = self.curr_span();
        // A decorator is required for endpoints to specify method and path.
        // Should look like: @endpoint("GET "/users/{id}")
        let decorator_node_id = self.parse_decorator()?.0;
        let decorator_node = self.ast.get_node(decorator_node_id).unwrap();
        let AstNodePayload::Decorator(Decorator { name, positional_args, .. }) = &decorator_node.payload() else {
            return Err(self.err(*decorator_node.span(), "Expected decorator node", None, Some("EExpectedDecoratorNode")));
        };
        if name != "endpoint" {
            return Err(self.err(*decorator_node.span(), "Expected @endpoint decorator", None, Some("EExpectedEndpointDecorator")));
        }
        let method_and_path = positional_args.first().ok_or_else(|| {
            self.err(
                *decorator_node.span(),
                "Expected method and path (e.g., \"GET /api/users/{id}\") as first positional argument to @endpoint",
                None,
                Some("EExpectedMethodAndPath"),
            )
        })?;
        let ConstantValue::String(method_and_path) = method_and_path else {
            return Err(self.err(
                *decorator_node.span(),
                "Expected method and path as string literal (e.g., \"GET /api/users/{id}\")",
                None,
                Some("EExpectedMethodAndPathString"),
            ));
        };
        let (method, path) = method_and_path.split_once(' ').ok_or_else(|| {
            self.err(
                *decorator_node.span(),
                "Expected method and path separated by space (e.g., \"GET /api/users/{id}\")",
                None,
                Some("EExpectedMethodAndPathFormat"),
            )
        })?;
        let mut path_params = Vec::new();
        for cap in Regex::new(r"\{(\w+)\}").unwrap().captures_iter(path) {
            if let Some(param) = cap.get(1) {
                path_params.push(param.as_str().to_string());
            }
        }

        expect_tokens!(self, TokenKind::KeywordEndpoint);
        let TokenPayload::String(endpoint_name) = self.expect(TokenKind::Ident)?.payload else {
            return Err(self.err(self.curr_span(), "Expected endpoint name identifier", None, Some("EExpectedEndpointName")));
        };
        let identifier_node_id = self.ast.add_node(AstNode::new_with_span(
            AstNodeKind::Identifier,
            AstNodePayload::String(endpoint_name.to_string()),
            self.prev_span(),
        ));
        let endpoint_name = endpoint_name.to_string();
        expect_tokens!(self, TokenKind::LBrace);

        let mut request: Option<AstNodeId> = None;
        let mut headers = HashMap::new();
        let mut responses = HashMap::new();

        let mut fields = Vec::new();
        while {
            let k = self.peek_kind();
            k != TokenKind::RBrace && k != TokenKind::Eof
        } {
            let next_relevant_token = self.tokens[self.current..]
                .windows(2)
                .find(|w| {
                    matches!(
                        (w[0].kind, w[1].kind),
                        (TokenKind::KeywordModel, _) | (TokenKind::KeywordEnum, _) | (TokenKind::Ident, TokenKind::Colon) | (TokenKind::StringLit, TokenKind::Colon)
                    )
                })
                .map(|tt| tt[0].kind)
                .unwrap_or(TokenKind::Eof);
            match next_relevant_token {
                TokenKind::KeywordModel => {
                    // Nested model definition
                    let (nested_id, nested_name) = self.parse_model(false)?;
                    self.ast.append_child(identifier_node_id, nested_id);
                    self.symbols.insert(identifier_node_id, AstSymbol::Model(nested_name.clone()), nested_id);
                }
                TokenKind::KeywordEnum => {
                    // Nested enum definition
                    let (nested_id, nested_name) = self.parse_enum()?;
                    self.ast.append_child(identifier_node_id, nested_id);
                    self.symbols.insert(identifier_node_id, AstSymbol::Enum(nested_name.clone()), nested_id);
                }
                TokenKind::Ident | TokenKind::StringLit => {
                    // Field (possibly with leading docblocks)
                    let (field_node_id, field_name) = self.parse_field()?;
                    match field_name.as_str() {
                        "request" => {
                            request = Some(field_node_id);
                        }
                        "headers" => {
                            let Some(fields) = self.extract_model_fields_from_endpoint_field(field_node_id) else {
                                let span = *self.ast.get_node(field_node_id).unwrap().span();
                                return Err(self.err(span, "Expected headers to be a model", None, Some("EExpectedHeadersModel")));
                            };
                            for (header_name, header_node_id) in fields.into_iter() {
                                headers.insert(header_name, header_node_id);
                            }
                        }
                        "responses" => {
                            let Some(fields) = self.extract_model_fields_from_endpoint_field(field_node_id) else {
                                let span = *self.ast.get_node(field_node_id).unwrap().span();
                                return Err(self.err(span, "Expected responses to be a model", None, Some("EExpectedResponsesModel")));
                            };
                            for (status_code, response_node_id) in fields.into_iter() {
                                responses.insert(status_code, response_node_id);
                            }
                        }
                        _ => {
                            return Err(self.err(
                                *self.ast.get_node(field_node_id).unwrap().span(),
                                "Unexpected field in endpoint body (possible fields: request, headers, responses)",
                                None,
                                Some("EUnexpectedFieldInEndpoint"),
                            ));
                        }
                    }
                    fields.push(field_node_id);
                    self.symbols.insert(identifier_node_id, AstSymbol::Field(field_name.clone()), field_node_id);
                }
                _ => {
                    return Err(self.err(self.curr_span(), "Unexpected token in endpoint body", None, Some("EUnexpectedTokenInEndpoint")));
                }
            }
        }

        expect_tokens!(self, TokenKind::RBrace);

        let span = span.merge(&self.prev_span());
        let endpoint_payload = AstNodePayload::Endpoint(Endpoint {
            name: endpoint_name.clone(),
            doc,
            method: method.to_string(),
            path: path.to_string(),
            path_params,
            request,
            headers,
            responses,
        });
        let endpoint_node = AstNode::new_with_span(AstNodeKind::Endpoint, endpoint_payload, span);
        let endpoint_node_id = self.ast.add_node(endpoint_node);

        if let Some(decorator_node_id) = Some(decorator_node_id) {
            self.ast.append_child(endpoint_node_id, decorator_node_id);
        }
        self.ast.append_child(endpoint_node_id, identifier_node_id);

        for field_node_id in fields.into_iter() {
            self.ast.append_child(endpoint_node_id, field_node_id);
        }

        Ok((endpoint_node_id, endpoint_name))
    }

    fn extract_model_fields_from_endpoint_field(&self, field_node_id: AstNodeId) -> Option<BTreeMap<String, AstNodeId>> {
        let type_nodes = self.ast.get_children(field_node_id)?;
        let type_node = type_nodes.into_iter().find(|child| matches!(child.kind(), AstNodeKind::Type))?;
        let model_nodes = self.ast.get_children(type_node.id())?;
        let model_node = model_nodes.into_iter().find(|child| matches!(child.payload(), AstNodePayload::Model(Model { .. })))?;
        match model_node.payload() {
            AstNodePayload::Model(Model { fields, .. }) => Some(fields.clone()),
            _ => None,
        }
    }

    /// Parses a constant value (string, int, or bool).
    fn parse_constant_value(&mut self) -> LangResult<Option<ConstantValue>> {
        let token = self.advance()?;
        match (token.kind, self.peek_kind()) {
            (TokenKind::StringLit, _) => {
                if let TokenPayload::String(s) = token.payload {
                    Ok(Some(ConstantValue::String(s)))
                } else {
                    Err(self.err(token.span, "Expected string literal", None, Some("EExpectedStringLiteral")))
                }
            }
            (TokenKind::IntLit, t) if t != TokenKind::DoubleDot => {
                if let TokenPayload::Number(i) = token.payload {
                    Ok(Some(ConstantValue::Int(i)))
                } else {
                    Err(self.err(token.span, "Expected integer literal", None, Some("EExpectedIntLiteral")))
                }
            }
            (TokenKind::IntLit, TokenKind::DoubleDot) => {
                // Range literal (e.g., 1..10)
                let start = if let TokenPayload::Number(i) = token.payload {
                    i
                } else {
                    return Err(self.err(token.span, "Expected integer literal", None, Some("EExpectedIntLiteral")));
                };
                self.advance()?; // consume '..'
                let end_token = self.expect(TokenKind::IntLit)?;
                let end = if let TokenPayload::Number(i) = end_token.payload {
                    i
                } else {
                    return Err(self.err(end_token.span, "Expected integer literal", None, Some("EExpectedIntLiteral")));
                };
                Ok(Some(ConstantValue::IntRange(start, end)))
            }
            (TokenKind::BoolLit, _) => {
                if let TokenPayload::Bool(b) = token.payload {
                    Ok(Some(ConstantValue::Bool(b)))
                } else {
                    Err(self.err(token.span, "Expected boolean literal", None, Some("EExpectedBoolLiteral")))
                }
            }
            _ => Err(self.err(
                token.span,
                format!("Expected constant value, received {}", token.kind),
                None,
                Some("EExpectedConstantValue"),
            )),
        }
    }

    fn parse_decorator(&mut self) -> LangResult<(AstNodeId, Decorator)> {
        self.expect(TokenKind::AtSign)?;
        let decorator_name = match self.peek_kind() {
            TokenKind::Ident => {
                let TokenPayload::String(decorator_name) = self.expect(TokenKind::Ident)?.payload else {
                    return Err(self.err(self.curr_span(), "Expected decorator name identifier", None, Some("EExpectedDecoratorName")));
                };
                decorator_name
            }
            // Also accept "endpoint" the decoratorn name, it's lexed as a keyword.
            TokenKind::KeywordEndpoint => {
                self.advance()?;
                "endpoint".to_string()
            }
            _ => {
                return Err(self.err(self.curr_span(), "Expected decorator name identifier", None, Some("EExpectedDecoratorName")));
            }
        };

        let mut named_args = HashMap::new();
        let mut positional_args = Vec::new();

        if self.peek_kind() == TokenKind::LParen {
            self.advance()?; // consume '('
        } else {
            return Err(self.err(self.curr_span(), "Expected '(' after decorator name", None, Some("EExpectedLParenAfterDecoratorName")));
        }

        let mut args_count = -1;
        while self.peek_kind() != TokenKind::RParen && self.peek_kind() != TokenKind::Eof {
            args_count += 1;

            if self.peek_kind() == TokenKind::Comma {
                self.advance()?;
            } else if args_count > 0 {
                return Err(self.err(
                    self.curr_span(),
                    "Expected ',' between decorator arguments",
                    None,
                    Some("EExpectedCommaBetweenDecoratorArgs"),
                ));
            }

            if self.peek().unwrap().kind != TokenKind::Ident {
                // Positional argument
                let arg_value = self
                    .parse_constant_value()?
                    .ok_or_else(|| self.err(self.curr_span(), "Expected constant value for positional argument", None, Some("EExpectedPosArgValue")))?;
                positional_args.push(arg_value);
                continue;
            }

            let TokenPayload::String(arg_name) = self.expect(TokenKind::Ident)?.payload else {
                return Err(self.err(self.curr_span(), "Expected argument name identifier", None, Some("EExpectedArgName")));
            };
            let arg_name = arg_name.to_string();
            self.expect(TokenKind::Equal)?;
            let arg_value = self
                .parse_constant_value()?
                .ok_or_else(|| self.err(self.curr_span(), "Expected constant value for argument", None, Some("EExpectedArgValue")))?;
            named_args.insert(arg_name, arg_value);
        }

        self.expect(TokenKind::RParen)?; // consume ')'

        let decorator = Decorator {
            name: decorator_name,
            named_args,
            positional_args,
        };
        let decorator_node = AstNode::new_with_span(AstNodeKind::Decorator, AstNodePayload::Decorator(decorator.clone()), self.curr_span());
        let decorator_node_id = self.ast.add_node(decorator_node);
        Ok((decorator_node_id, decorator))
    }

    fn parse_optional_docblocks(&mut self) -> LangResult<Option<String>> {
        let mut parts: Vec<String> = Vec::new();
        while self.peek_kind() == TokenKind::DocBlock {
            if let Some(Token {
                payload: TokenPayload::DocLines(lines),
                ..
            }) = self.peek()
            {
                parts.push(lines.join("\n"));
            }
            self.advance()?; // consume
        }
        if parts.is_empty() { Ok(None) } else { Ok(Some(parts.join("\n"))) }
    }

    /// Parses a field definition.
    fn parse_field(&mut self) -> LangResult<(AstNodeId, String)> {
        let doc = self.parse_optional_docblocks()?;

        let mut decorator_node_id = None;
        if self.peek().unwrap().kind == TokenKind::AtSign {
            // Field decorators are not currently supported; skip them.
            decorator_node_id = Some(self.parse_decorator()?.0);
        }

        // Field identifiers can be normal identifiers, string literals, or numeric literals (e.g., HTTP status codes).
        let (identifier_token, ident_string) = match self.peek_kind() {
            TokenKind::Ident => {
                let token = self.peek().unwrap().clone();
                let TokenPayload::String(ident) = self.expect(TokenKind::Ident)?.payload else {
                    let span = token.span;
                    return Err(self.err(span, format!("Expected field name identifier, received {}", token.kind), None, Some("EExpectedFieldName")));
                };
                (token, ident)
            }
            TokenKind::StringLit => {
                let token = self.peek().unwrap().clone();
                let TokenPayload::String(ident) = self.expect(TokenKind::StringLit)?.payload else {
                    let span = token.span;
                    return Err(self.err(span, format!("Expected field name identifier, received {}", token.kind), None, Some("EExpectedFieldName")));
                };
                (token, ident)
            }
            TokenKind::IntLit => {
                let token = self.peek().unwrap().clone();
                let TokenPayload::Number(value) = self.expect(TokenKind::IntLit)?.payload else {
                    let span = token.span;
                    return Err(self.err(span, format!("Expected field name identifier, received {}", token.kind), None, Some("EExpectedFieldName")));
                };
                (token, value.to_string())
            }
            tk => {
                return Err(self.err(
                    self.curr_span(),
                    format!("Expected field name identifier, received {}", tk),
                    None,
                    Some("EExpectedFieldName"),
                ));
            }
        };
        let ident_span = identifier_token.span;

        self.expect(TokenKind::Colon)?;
        let (ty_node_id, ty) = self.parse_type()?;

        let mut default_value = None;
        if self.peek_kind() == TokenKind::Equal {
            // Expecting a default value
            self.advance()?;
            default_value = self.parse_constant_value()?;
        }

        let field_node_id = self.ast.add_node(AstNode::new_with_span(
            AstNodeKind::Field,
            AstNodePayload::Field(Field {
                name: ident_string.clone(),
                doc,
                default: default_value,
                ty: ty.clone(),
            }),
            ident_span,
        ));

        if let Some(decorator_node_id) = decorator_node_id {
            self.ast.append_child(field_node_id, decorator_node_id);
        }

        let identifier_node_id = self
            .ast
            .add_node(AstNode::new_with_span(AstNodeKind::Identifier, AstNodePayload::String(ident_string.clone()), ident_span));
        self.ast.append_child(field_node_id, identifier_node_id);
        self.ast.append_child(field_node_id, ty_node_id);
        Ok((field_node_id, ident_string))
    }

    #[inline]
    fn peek(&self) -> Option<&Token> {
        self.tokens.get(self.current)
    }

    #[inline]
    fn peek_kind(&self) -> TokenKind {
        self.tokens.get(self.current).map_or(Default::default(), |t| t.kind)
    }

    // Removed peek_next_fn to simplify borrow interactions.

    #[inline]
    fn peek_ahead_kind(&self, n: usize) -> TokenKind {
        self.tokens.get(self.current + n).map_or(Default::default(), |t| t.kind)
    }

    #[inline]
    fn prev_span(&self) -> Span {
        if self.current == 0 {
            Default::default()
        } else {
            self.tokens.get(self.current - 1).map_or(Default::default(), |t| t.span)
        }
    }

    #[inline]
    fn curr_span(&self) -> Span {
        self.tokens.get(self.current).map_or(Default::default(), |t| t.span)
    }

    #[inline]
    fn advance(&mut self) -> LangResult<Token> {
        if self.current < self.tokens.len() {
            let token = self.tokens[self.current].clone();
            self.current += 1;
            Ok(token)
        } else {
            Err(self.err(Default::default(), "Unexpected end of input", None, Some("EUnexpectedEof")))
        }
    }

    #[inline]
    fn advance_n(&mut self, n: usize) -> LangResult {
        for _ in 0..n {
            self.advance()?;
        }
        Ok(())
    }

    fn expect(&mut self, expected: TokenKind) -> LangResult<Token> {
        let curr_kind = self.peek_kind();
        match curr_kind {
            kind if kind == expected => {
                let token = self.advance()?;
                Ok(token)
            }
            kind => Err(self.err(self.curr_span(), format!("Expected {expected:?}, found {kind:?}"), None, Some("EUnexpectedToken"))),
        }
    }

    fn err(&self, span: Span, msg: impl Into<String>, note: Option<String>, code: Option<&str>) -> Box<LangError> {
        Box::new(LangError::error(self.file_name, self.src, span, msg, note, code))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::{Lexer, Token};
    // Bring trait into scope so we can call id() on AstNode in tests.
    use crate::parser::tree::TreeNode;
    use crate::{Field, span_of};
    use indoc::indoc;
    use pretty_assertions::{assert_eq, assert_matches};

    fn lex(input: &str) -> Vec<Token> {
        Lexer::new(input).lex()
    }

    fn parse(input: &'static str) -> LangResult<ParserArtifacts> {
        let tokens_vec = lex(input);
        // Leak tokens so the AST can safely reference them for test duration.
        let leaked: &'static [Token] = Box::leak(tokens_vec.into_boxed_slice());
        let parser: Parser<'static> = Parser::new("sample.glue", input, leaked);
        parser.parse()
    }

    #[test]
    fn test_parse_model_with_enum() {
        let input = indoc! {r#"
            /// Status of a user
            enum Status: "active" | "inactive"

            /// This is a user model
            model User {
                /// The user's name
                name: string
                /// The user's age
                age: int
                /// Whether the user is active
                status: Status?
                // /// Pet names
                // pet_names: string[]
            }
        "#};
        let ast = parse(input).unwrap().ast;
        let root_node_id = ast.get_root();
        let root_children = ast.get_children(root_node_id).unwrap();
        assert_eq!(root_children.len(), 2);
        assert_eq!(root_children[0].kind(), AstNodeKind::Enum);
        assert_matches!(root_children[0].payload(), AstNodePayload::Enum(Enum{ name, .. }) if name == "Status");
        assert_eq!(root_children[1].kind(), AstNodeKind::Model);
        assert_matches!(root_children[1].payload(), AstNodePayload::Model(Model{ name, .. }) if name == "User");
    }

    #[test]
    fn test_parse_field_with_numeric_name() {
        let input = indoc! {r#"
            model Responses {
                200: Response200
            }
        "#};
        let ast = parse(input).unwrap().ast;
        let root_children = ast.get_children(ast.get_root()).unwrap();
        assert_eq!(root_children.len(), 1);
        let model_children = ast.get_children(root_children[0].id()).unwrap();
        let field = model_children
            .iter()
            .find(|n| matches!(n.payload(), AstNodePayload::Field(Field{ name, .. }) if name == "200"))
            .expect("Expected field named 200");
        assert_matches!(field.payload(), AstNodePayload::Field(Field{ name, .. }) if name == "200");
    }

    #[test]
    fn test_parse_model_with_decorator() {
        let input = indoc! {r#"
            @foo(bar="baz", count=3)
            model User {
                id: int
                name: string
            }
        "#};
        let ast = parse(input).unwrap().ast;
        let root_node_id = ast.get_root();
        let root_children = ast.get_children(root_node_id).unwrap();
        assert_eq!(root_children.len(), 1);
        assert_eq!(root_children[0].kind(), AstNodeKind::Model);
        let model_node = &root_children[0];
        let model_children = ast.get_children(model_node.id()).unwrap();
        let decorator = model_children.iter().find(|n| n.kind() == AstNodeKind::Decorator);
        assert!(decorator.is_some());
        assert_matches!(decorator.unwrap().payload(), AstNodePayload::Decorator(Decorator { name, named_args, .. }) if name == "foo" && named_args.len() == 2);
    }

    #[test]
    fn test_parse_multiline_field_docblocks() {
        let input = indoc! {r#"
            model Post {
              /// The author of the post
              /// Can be either the user ID or the full user object
              author: int
            }
        "#};
        let ast = parse(input).unwrap().ast;
        let root_children = ast.get_children(ast.get_root()).unwrap();
        assert_eq!(root_children.len(), 1);
        assert_eq!(root_children[0].kind(), AstNodeKind::Model);
        let model_children = ast.get_children(root_children[0].id()).unwrap();
        let fields: Vec<_> = model_children.iter().filter(|n| n.kind() == AstNodeKind::Field).collect();
        assert_eq!(fields.len(), 1);
        let field = fields[0];
        assert_matches!(field.payload(), AstNodePayload::Field(Field {name, .. }) if name == "author");
        assert_matches!(field.payload(), AstNodePayload::Field(Field {name, doc, .. }) if name == "author" && doc.is_some());
    }

    #[test]
    fn test_parse_complex_multiline_field_docs_and_nested_model() {
        let input = indoc! {r#"
            /// Status of a user
            enum Status: "active" | "inactive"

            /// A user of the system
            model User {
              /// The unique ID of the user
              id: int
              /// The user's name
              name: string
              /// Whether the user is active
              status: Status?
              /// Pet names
              pet_names: string[]
            }

            /// A blog post
            model Post {
              /// The unique ID of the post
              id: int
              /// The title of the post
              title: string
              /// The content of the post
              content: string
              /// The author of the post
              /// Can be either the user ID or the full user object
              author: int | User

              /// Optional additional details about the post
              additional_details: AdditionalPostDetails?

              /// Additional details about the post
              model AdditionalPostDetails {
                /// The number of likes the post has received
                likes: int
              }
            }
        "#};
        let ast = parse(input).unwrap().ast;
        let root_children = ast.get_children(ast.get_root()).unwrap();
        assert_eq!(root_children.len(), 3, "Should have Enum + User + Post at root");
        // Find Post model id
        let post_node = root_children
            .iter()
            .find(|n| matches!(n.payload(), AstNodePayload::Model(Model{ name, .. }) if name == "Post"))
            .expect("Post model not found");
        let post_id = post_node.id();
        let post_children = ast.get_children(post_id).unwrap();

        // Check for nested AdditionalPostDetails model
        let has_nested_model = post_children
            .iter()
            .any(|n| matches!(n.payload(), AstNodePayload::Model(Model{ name, .. }) if name == "AdditionalPostDetails"));
        assert!(has_nested_model, "Expected nested AdditionalPostDetails model");

        // Find author field
        let author_field = post_children
            .iter()
            .find(|n| matches!(n.payload(), AstNodePayload::Field(Field{ name, .. }) if name == "author"))
            .expect("author field not found");
        let AstNodePayload::Field(Field { doc, .. }) = &author_field.payload() else {
            panic!("author field should have Field payload");
        };
        let author_doc = doc.clone().expect("author field should have documentation");
        assert!(author_doc.contains("The author of the post"));
        assert!(author_doc.contains("full user object"));
    }

    #[test]
    fn test_spans() {
        let input = indoc! {r#"
            model User {
                id: string | int
                name: string
            }
        "#}
        .trim();
        let artifacts = parse(input).unwrap();
        let ast = &artifacts.ast;

        // Test finding nodes at specific positions
        // Line 2, col 9 should be in the "int" type
        let string_span = span_of(input, "id: (string)").unwrap();
        let string_node = ast.find_narrowest_node_at_position(string_span.lines.0, string_span.cols.0).unwrap();
        assert_eq!(string_node.span(), &string_span);
        assert_eq!(string_node.kind(), AstNodeKind::TypeAtom);
    }

    #[test]
    fn test_symbols() {
        let input = indoc! {r#"
            model User {
                id: int
                name: string
            }

            model Post {
                id: int
                title: string
                additional_details: AdditionalPostDetails?

                model AdditionalPostDetails {
                    likes: int
                }
            }
        "#};
        let artifacts = parse(input).unwrap();
        let ast = &artifacts.ast;
        let symbols = artifacts.symbols;
        let root_id = ast.get_root();

        // Post should be in the root scope
        let post_model_id = ast
            .find(|n| matches!(n.payload(), AstNodePayload::Model(Model{ name, .. }) if name == "Post"))
            .first()
            .unwrap()
            .id();
        let root_scope = symbols.symbols_in_scope(ast, root_id).unwrap();
        let post_in_root_scope = root_scope.get(&AstSymbol::Model("Post".to_string())).unwrap().id;
        assert_eq!(post_in_root_scope, post_model_id);

        // User should be in the root scope
        let user_model_id = ast
            .find(|n| matches!(n.payload(), AstNodePayload::Model(Model{ name, .. }) if name == "User"))
            .first()
            .unwrap()
            .id();
        let user_in_root_scope = root_scope.get(&AstSymbol::Model("User".to_string())).unwrap().id;
        assert_eq!(user_in_root_scope, user_model_id);

        // AdditionalPostDetails should be in Post's scope
        let additional_details_id = ast
            .find(|n| matches!(n.payload(), AstNodePayload::Model(Model{ name, .. }) if name == "AdditionalPostDetails"))
            .first()
            .unwrap()
            .id();
        let post_scope = symbols.symbols_in_scope(ast, post_model_id).unwrap();
        let additional_details_in_post_scope = post_scope.get(&AstSymbol::Model("AdditionalPostDetails".to_string())).unwrap().id;
        assert_eq!(additional_details_in_post_scope, additional_details_id);
        // ...and should also be in its own scope.
        let additional_details_scope = symbols.symbols_in_scope(ast, additional_details_id).unwrap();
        let additional_details_in_its_own_scope = additional_details_scope.get(&AstSymbol::Model("AdditionalPostDetails".to_string())).unwrap().id;
        assert_eq!(additional_details_in_its_own_scope, additional_details_id);
    }

    #[test]
    fn test_symbols_2() {
        let src = indoc! {r#"
            model Foo {
                bar: Bar
            }

            model Bar {
                baz: Baz

                model Baz {
                    value: string
                }
            }
        "#};
        let artifacts = parse(src).unwrap();
        let ast = &artifacts.ast;
        let symbols = artifacts.symbols;

        let root_id = ast.get_root();
        let baz_field_id = ast
            .find(|n| matches!(n.payload(), AstNodePayload::Field(Field{ name, .. }) if name == "baz"))
            .first()
            .unwrap()
            .id();

        let root_scopes = symbols.symbols_in_scope(ast, root_id).unwrap();
        assert!(root_scopes.contains_key(&AstSymbol::Model("Foo".to_string())));
        let baz_field_scopes = symbols.symbols_in_scope(ast, baz_field_id).unwrap();
        assert!(baz_field_scopes.contains_key(&AstSymbol::Model("Baz".to_string())));
    }

    #[test]
    fn test_symbols_3() {
        let src = indoc! {r#"
            model Foo {
                    bar: Bar
            }

            model Bar {
                    baz: string
            }
            "#}
        .trim();

        let artifacts = parse(src).unwrap();
        let ast = &artifacts.ast;
        let symbols = artifacts.symbols;

        let bar_type_span = span_of(src, "bar: (Bar)").unwrap();
        let bar_type_node = ast.find_narrowest_node_at_position(bar_type_span.lines.0, bar_type_span.cols.0).unwrap();
        assert_matches!(bar_type_node.kind(), AstNodeKind::TypeAtom);

        let symbols = symbols.symbols_in_scope(ast, bar_type_node.id()).unwrap();
        assert!(symbols.contains_key(&AstSymbol::Model("Bar".to_string())));
    }

    #[test]
    fn test_global_annotations() {
        let input = indoc! {r#"
            [@auth("api-key", header="Authorization")]
        "#};
        let artifacts = parse(input).unwrap();
        let ast = &artifacts.ast;
        let root_node_id = ast.get_root();
        let root_children = ast.get_children(root_node_id).unwrap();
        assert_eq!(root_children.len(), 1);
        assert_eq!(root_children[0].kind(), AstNodeKind::GlobalAnnotations);
    }
}
