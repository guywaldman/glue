use std::collections::HashMap;

use crate::{
    Span,
    diagnostics::LangError,
    lexer::{Token, TokenKind, TokenPayload},
    parser::{
        Ast, AstSymbol, SymbolTable,
        ast::{AstNode, AstNodeId, AstNodeKind, ConstantValue, PrimitiveType, Type, TypeAtom, TypeVariant},
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
        let root = AstNode::new(AstNodeKind::Root);
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
    pub fn parse(mut self) -> Result<ParserArtifacts, LangError> {
        let root_id = self.ast.get_root();
        while self.peek_kind() != TokenKind::Eof {
            let Some(next_relevant_token) = self.tokens.iter().find(|t| matches!(t.kind, TokenKind::KeywordModel | TokenKind::KeywordEnum)) else {
                break;
            };

            match next_relevant_token.kind {
                TokenKind::KeywordModel => {
                    let (node_id, name) = self.parse_model()?;
                    self.ast.append_child(root_id, node_id);
                    // Link model's symbol entry parent to root scope for ancestor visibility.
                    self.symbols.insert(root_id, AstSymbol::Model(name.clone()), node_id, Some(root_id));
                    // Ensure the model's own scope entry knows its parent for upward traversal.
                    self.symbols.set_scope_parent(node_id, root_id);
                }
                TokenKind::KeywordEnum => {
                    let (node_id, name) = self.parse_enum()?;
                    self.ast.append_child(root_id, node_id);
                    self.symbols.insert(root_id, AstSymbol::Enum(name.clone()), node_id, Some(root_id));
                    self.symbols.set_scope_parent(node_id, root_id);
                }
                TokenKind::Eof => break,
                _ => {
                    // Unknown token at top-level; advance to avoid infinite loop.
                    self.advance()?;
                }
            }
        }
        Ok(ParserArtifacts {
            ast: self.ast,
            symbols: self.symbols,
        })
    }

    /// Parses an enum definition.
    ///
    /// ```text
    /// enum := [docblock] "enum" ident "=" enum_variants
    /// enum_variants := string_lit ("|" string_lit)*
    /// ```
    ///
    /// Returns the enum's AST node ID and its name.
    fn parse_enum(&mut self) -> Result<(AstNodeId, String), LangError> {
        let doc = self.parse_optional_docblocks()?;
        let span = self.curr_span();
        expect_tokens!(self, TokenKind::KeywordEnum);
        let TokenPayload::String(enum_name) = self.expect(TokenKind::Ident)?.payload else {
            return Err(self.err(self.curr_span(), "Expected enum name identifier", None, Some("EExpectedEnumName")));
        };
        let enum_name = enum_name.to_string();
        expect_tokens!(self, TokenKind::Equal);

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

        let default = if self.peek_kind() == TokenKind::Equal {
            self.advance()?; // consume '='
            self.parse_constant_value()?
        } else {
            None
        };

        let enum_node = AstNode::new_with_span(
            AstNodeKind::Enum {
                name: enum_name.clone(),
                doc,
                variants,
                default,
            },
            span,
        );
        let enum_node_id = self.ast.add_node(enum_node);
        Ok((enum_node_id, enum_name))
    }

    /// Parses a model definition.
    ///
    /// ```text
    /// model := [docblock] "model" ident "{"" (field)* "}"
    /// ```
    ///
    /// Returns the model's AST node ID and its name.
    fn parse_model(&mut self) -> Result<(AstNodeId, String), LangError> {
        let doc = self.parse_optional_docblocks()?;

        let mut decorator_node_id = None;
        // Parse decorators
        if self.peek_kind() == TokenKind::AtSign {
            // Decorator(s) present
            while self.peek_kind() == TokenKind::AtSign {
                decorator_node_id = Some(self.parse_decorator()?);
            }
        }

        let span = self.curr_span();

        expect_tokens!(self, TokenKind::KeywordModel);

        let TokenPayload::String(model_name) = self.expect(TokenKind::Ident)?.payload else {
            return Err(self.err(self.curr_span(), "Expected model name identifier", None, Some("EExpectedModelName")));
        };
        let model_name = model_name.to_string();

        expect_tokens!(self, TokenKind::LBrace);

        let model_node = AstNodeKind::Model { name: model_name.clone(), doc };
        let model_ast_node = AstNode::new_with_span(model_node, span);
        let model_node_id = self.ast.add_node(model_ast_node);

        let mut fields = Vec::new();
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
                    let (nested_id, nested_name) = self.parse_model()?;
                    nested_models.push(nested_id);
                    self.symbols
                        .insert(model_node_id, AstSymbol::Model(nested_name.clone()), nested_id, Some(model_node_id));
                    self.symbols.set_scope_parent(nested_id, model_node_id);
                }
                (TokenKind::KeywordEnum, _) | (TokenKind::DocBlock, TokenKind::KeywordEnum) => {
                    // Nested enum definition
                    let (nested_id, nested_name) = self.parse_enum()?;
                    nested_models.push(nested_id);
                    self.symbols
                        .insert(model_node_id, AstSymbol::Enum(nested_name.clone()), nested_id, Some(model_node_id));
                    self.symbols.set_scope_parent(nested_id, model_node_id);
                }
                _ => {
                    // Field (possibly with leading docblocks)
                    let (field_node_id, field_name) = self.parse_field()?;
                    fields.push(field_node_id);
                    self.symbols
                        .insert(model_node_id, AstSymbol::Field(field_name.clone()), field_node_id, Some(model_node_id));
                    self.symbols.set_scope_parent(field_node_id, model_node_id);
                }
            }
        }

        expect_tokens!(self, TokenKind::RBrace);

        for nested_id in nested_models.into_iter() {
            self.ast.append_child(model_node_id, nested_id);
        }

        for field_node_id in fields.into_iter() {
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
    fn parse_type(&mut self) -> Result<Type, LangError> {
        let mut types = Vec::new();

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
                other_type_name => TypeVariant::Ref(other_type_name.to_string()),
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

            types.push(TypeAtom {
                variant,
                is_optional,
                is_array,
            });

            if self.peek_kind() == TokenKind::Pipe {
                self.advance()?;
                continue;
            } else {
                break;
            }
        }

        if types.len() == 1 {
            Ok(Type::Single(types.into_iter().next().unwrap()))
        } else {
            Ok(Type::Union(types))
        }
    }

    fn parse_constant_value(&mut self) -> Result<Option<ConstantValue>, LangError> {
        let token = self.advance()?;
        match token.kind {
            TokenKind::StringLit => {
                if let TokenPayload::String(s) = token.payload {
                    Ok(Some(ConstantValue::String(s)))
                } else {
                    Err(self.err(token.span, "Expected string literal", None, Some("EExpectedStringLiteral")))
                }
            }
            TokenKind::IntLit => {
                if let TokenPayload::Number(i) = token.payload {
                    Ok(Some(ConstantValue::Int(i)))
                } else {
                    Err(self.err(token.span, "Expected integer literal", None, Some("EExpectedIntLiteral")))
                }
            }
            TokenKind::BoolLit => {
                if let TokenPayload::Bool(b) = token.payload {
                    Ok(Some(ConstantValue::Bool(b)))
                } else {
                    Err(self.err(token.span, "Expected boolean literal", None, Some("EExpectedBoolLiteral")))
                }
            }
            _ => Err(self.err(token.span, "Expected constant value", None, Some("EExpectedConstantValue"))),
        }
    }

    fn parse_decorator(&mut self) -> Result<AstNodeId, LangError> {
        self.expect(TokenKind::AtSign)?;
        let TokenPayload::String(decorator_name) = self.expect(TokenKind::Ident)?.payload else {
            return Err(self.err(self.curr_span(), "Expected decorator name identifier", None, Some("EExpectedDecoratorName")));
        };
        let decorator_name = decorator_name.to_string();

        let mut args = HashMap::new();
        if self.peek_kind() == TokenKind::LParen {
            self.advance()?; // consume '('
            while self.peek_kind() != TokenKind::RParen && self.peek_kind() != TokenKind::Eof {
                let TokenPayload::String(arg_name) = self.expect(TokenKind::Ident)?.payload else {
                    return Err(self.err(self.curr_span(), "Expected argument name identifier", None, Some("EExpectedArgName")));
                };
                let arg_name = arg_name.to_string();
                self.expect(TokenKind::Equal)?;
                let arg_value = self
                    .parse_constant_value()?
                    .ok_or_else(|| self.err(self.curr_span(), "Expected constant value for argument", None, Some("EExpectedArgValue")))?;
                args.insert(arg_name, arg_value);
                if self.peek_kind() == TokenKind::Comma {
                    self.advance()?; // consume ','
                } else {
                    break;
                }
            }
            self.expect(TokenKind::RParen)?; // consume ')'
        }

        let decorator_node = AstNode::new_with_span(AstNodeKind::Decorator { name: decorator_name, args }, self.curr_span());
        let decorator_node_id = self.ast.add_node(decorator_node);
        Ok(decorator_node_id)
    }

    fn parse_optional_docblocks(&mut self) -> Result<Option<String>, LangError> {
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
    ///
    /// ```text
    /// field := [docblock] ident ":" type
    /// ```
    fn parse_field(&mut self) -> Result<(AstNodeId, String), LangError> {
        let doc = self.parse_optional_docblocks()?;
        let identifier_token = self.expect(TokenKind::Ident)?;
        let TokenPayload::String(ident) = identifier_token.payload else {
            let span = identifier_token.span;
            return Err(self.err(span, "Expected field name identifier", None, Some("EExpectedFieldName")));
        };
        let ident_span = identifier_token.span;
        let ident_string = ident.to_string();

        self.expect(TokenKind::Colon)?;
        let ty_span = self.curr_span();
        let ty = self.parse_type()?;

        let mut default_value = None;
        if self.peek_kind() == TokenKind::Equal {
            // Expecting a default value
            self.advance()?;
            default_value = self.parse_constant_value()?;
        }

        let field_node_id = self.ast.add_node(AstNode::new_with_span(
            AstNodeKind::Field {
                name: ident_string.clone(),
                doc,
                default: default_value,
                ty: ty.clone(),
            },
            ident_span,
        ));
        let identifier_node_id = self
            .ast
            .add_node(AstNode::new_with_span(AstNodeKind::Identifier(ident_string.clone()), ident_span));

        let type_node_id = self.ast.add_node(AstNode::new_with_span(AstNodeKind::Type(ty), ty_span));
        self.ast.append_child(field_node_id, identifier_node_id);
        self.ast.append_child(field_node_id, type_node_id);
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
    fn curr_span(&self) -> Span {
        self.tokens.get(self.current).map_or(Default::default(), |t| t.span)
    }

    #[inline]
    fn advance(&mut self) -> Result<Token, LangError> {
        if self.current < self.tokens.len() {
            let token = self.tokens[self.current].clone();
            self.current += 1;
            Ok(token)
        } else {
            Err(self.err(Default::default(), "Unexpected end of input", None, Some("EUnexpectedEof")))
        }
    }

    #[inline]
    fn advance_n(&mut self, n: usize) -> Result<(), LangError> {
        for _ in 0..n {
            self.advance()?;
        }
        Ok(())
    }

    fn expect(&mut self, expected: TokenKind) -> Result<Token, LangError> {
        let curr_kind = self.peek_kind();
        match curr_kind {
            kind if kind == expected => {
                let token = self.advance()?;
                Ok(token)
            }
            kind => Err(self.err(
                self.curr_span(),
                format!("Expected {expected:?}, found {kind:?}"),
                None,
                Some("EUnexpectedToken"),
            )),
        }
    }

    fn err(&self, span: Span, msg: impl Into<String>, note: Option<String>, code: Option<&str>) -> LangError {
        LangError::error(self.file_name, self.src, span, msg, note, code)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::{Lexer, Token};
    // Bring trait into scope so we can call id() on AstNode in tests.
    use crate::parser::tree::TreeNode;
    use indoc::indoc;
    use pretty_assertions::assert_eq;

    /// Assert nodes in order match patterns, with optional guards:
    /// assert_match_nodes!(nodes, Pattern1, Pattern2 { field } if guard_expr, ...);
    macro_rules! assert_match_nodes {
        ($nodes:expr, $($pattern:pat $(if $guard:expr)?),+ $(,)?) => {{
            let nodes_ref = &$nodes;
            let expected_len = 0 $(+ { let _ = stringify!($pattern); 1 })+;
            assert_eq!(nodes_ref.len(), expected_len, "Expected {} nodes, got {}", expected_len, nodes_ref.len());
            let mut __idx = 0usize;
            $(
                {
                    let kind = nodes_ref[__idx].kind();
                    match kind {
                        $pattern $(if $guard)? => { /* match ok */ },
                        other => panic!(
                            "Node {} expected pattern `{}`{} but was {:?}",
                            __idx,
                            stringify!($pattern),
                            $(concat!(" with guard `", stringify!($guard), "`"))?,
                            other
                        ),
                    }
                    __idx += 1;
                }
            )+
        }};
        ($nodes:expr) => {{
            assert!($nodes.is_empty(), "Expected no nodes, found {}", $nodes.len());
        }};
    }

    fn lex(input: &str) -> Vec<Token> {
        Lexer::new(input).lex()
    }

    fn parse(input: &'static str) -> Result<ParserArtifacts, LangError> {
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
            enum Status = "active" | "inactive"

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
        assert_match_nodes!(
            root_children,
            AstNodeKind::Enum { name, .. } if name == "Status",
            AstNodeKind::Model { name, .. } if name == "User"
        );
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
        assert_match_nodes!(
            root_children,
            AstNodeKind::Decorator { name, args } if name == "foo" && args.len() == 2,
            AstNodeKind::Model { name, .. } if name == "User"
        );
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
        if let AstNodeKind::Model { .. } = root_children[0].kind() {
            let model_children = ast.get_children(root_children[0].id()).unwrap();
            assert_eq!(model_children.len(), 1);
            if let AstNodeKind::Field { name, doc, .. } = model_children[0].kind() {
                assert_eq!(name, "author");
                let d = doc.clone().unwrap();
                assert!(d.contains("The author of the post"));
                assert!(d.contains("full user object"));
            } else {
                panic!("Expected field node");
            }
        } else {
            panic!("Expected model node");
        }
    }

    #[test]
    fn test_parse_complex_multiline_field_docs_and_nested_model() {
        let input = indoc! {r#"
            /// Status of a user
            enum Status = "active" | "inactive"

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
        // Find Post model id and inspect children
        let root_child_ids = ast.get_children_ids(ast.get_root()).unwrap();
        let mut post_id = None;
        for cid in root_child_ids.clone() {
            // clone to reuse iterator
            if let Some(node) = ast.get_node(cid) {
                if let AstNodeKind::Model { name, .. } = node.kind() {
                    if name == "Post" {
                        post_id = Some(cid);
                        break;
                    }
                }
            }
        }
        let post_id = post_id.expect("Post model not found");
        let post_children = ast.get_children(post_id).unwrap();
        assert!(
            post_children
                .iter()
                .any(|n| matches!(n.kind(), AstNodeKind::Model { name, .. } if name == "AdditionalPostDetails")),
            "Expected nested AdditionalPostDetails model"
        );
        let author_doc = post_children
            .iter()
            .find_map(|n| {
                if let AstNodeKind::Field { name, doc, .. } = n.kind() {
                    if name == "author" {
                        return doc.clone();
                    }
                }
                None
            })
            .expect("author field not found");
        assert!(author_doc.contains("The author of the post"));
        assert!(author_doc.contains("full user object"));
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
            .find(|n| matches!(n.kind(), AstNodeKind::Model { name, .. } if name == "Post"))
            .unwrap()
            .id();
        let root_scope = symbols.symbols_in_scope(root_id).unwrap();
        let post_in_root_scope = root_scope.get(&AstSymbol::Model("Post".to_string())).unwrap().id;
        assert_eq!(post_in_root_scope, post_model_id);

        // User should be in the root scope
        let user_model_id = ast
            .find(|n| matches!(n.kind(), AstNodeKind::Model { name, .. } if name == "User"))
            .unwrap()
            .id();
        let user_in_root_scope = root_scope.get(&AstSymbol::Model("User".to_string())).unwrap().id;
        assert_eq!(user_in_root_scope, user_model_id);

        // AdditionalPostDetails should be in Post's scope
        let additional_details_id = ast
            .find(|n| matches!(n.kind(), AstNodeKind::Model { name, .. } if name == "AdditionalPostDetails"))
            .unwrap()
            .id();
        let post_scope = symbols.symbols_in_scope(post_model_id).unwrap();
        let additional_details_in_post_scope = post_scope.get(&AstSymbol::Model("AdditionalPostDetails".to_string())).unwrap().id;
        assert_eq!(additional_details_in_post_scope, additional_details_id);
        // ...and should also be in its own scope.
        let additional_details_scope = symbols.symbols_in_scope(additional_details_id).unwrap();
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
        let baz_field_id = ast.find(|n| matches!(n.kind(), AstNodeKind::Field { name, .. } if name == "baz")).unwrap().id();

        let root_scopes = symbols.symbols_in_scope(root_id).unwrap();
        assert!(root_scopes.contains_key(&AstSymbol::Model("Foo".to_string())));
        let baz_field_scopes = symbols.symbols_in_scope(baz_field_id).unwrap();
        assert!(baz_field_scopes.contains_key(&AstSymbol::Model("Baz".to_string())));
    }
}
