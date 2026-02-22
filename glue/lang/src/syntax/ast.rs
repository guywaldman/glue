use std::collections::HashMap;

use crate::{
    builtin_decorators::{DecoratorArgDef, DecoratorDef},
    syntax::parser::{LNode, LNodeOrToken, LSyntaxKind, LToken},
};

pub trait AstNode: Sized {
    fn cast(n: LNode) -> Option<Self>;
    fn syntax(&self) -> &LNode;
}

macro_rules! ast_node {
    ($name:ident, $($kind:expr)*) => {
        #[derive(Debug, Clone)]
        pub struct $name(LNode);

        impl AstNode for $name {
            fn cast(n: LNode) -> Option<Self> {
                ($(n.kind() == $kind)||+).then(|| $name(n))
            }

            fn syntax(&self) -> &LNode {
                &self.0
            }
        }
    };
}

ast_node!(RootNode, LSyntaxKind::PROGRAM);

impl RootNode {
    pub fn top_level_imports(&self) -> Vec<ImportStmt> {
        self.0.children().filter(|n| n.kind() == LSyntaxKind::IMPORT_STMT).filter_map(ImportStmt::cast).collect()
    }

    pub fn top_level_models(&self) -> Vec<Model> {
        self.0.children().filter(|n| n.kind() == LSyntaxKind::MODEL).filter_map(Model::cast).collect()
    }

    pub fn top_level_endpoints(&self) -> Vec<Endpoint> {
        self.0.children().filter(|n| n.kind() == LSyntaxKind::ENDPOINT).filter_map(Endpoint::cast).collect()
    }

    pub fn top_level_enums(&self) -> Vec<Enum> {
        self.0.children().filter(|n| n.kind() == LSyntaxKind::ENUM).filter_map(Enum::cast).collect()
    }
}

ast_node!(ImportStmt, LSyntaxKind::IMPORT_STMT);
ast_node!(ImportNamedItem, LSyntaxKind::IMPORT_NAMED_ITEM);

impl ImportNamedItem {
    pub fn imported_name(&self) -> Option<String> {
        self.0
            .children_with_tokens()
            .filter_map(|n| n.into_token())
            .find(|t| t.kind() == LSyntaxKind::IDENT)
            .map(|t| t.text().to_string())
    }

    pub fn alias(&self) -> Option<String> {
        self.0
            .children_with_tokens()
            .filter_map(|n| n.into_token())
            .filter(|t| t.kind() == LSyntaxKind::IDENT)
            .nth(1)
            .map(|t| t.text().to_string())
    }
}

impl ImportStmt {
    pub fn source_path(&self) -> Option<String> {
        let text = self.0.text().to_string();
        let from_idx = text.find("from")?;
        let after_from = text.get(from_idx + 4..)?.trim();
        let start = after_from.find('"')?;
        let rest = after_from.get(start + 1..)?;
        let end = rest.find('"')?;
        Some(rest[..end].to_string())
    }

    pub fn wildcard_alias(&self) -> Option<String> {
        let text = self.0.text().to_string();
        let stripped = text.trim();
        if !stripped.starts_with("import *") {
            return None;
        }
        let (_, rest) = stripped.split_once("import *")?;
        let rest = rest.trim_start();
        if !rest.starts_with("as ") {
            return None;
        }
        let rest = rest.trim_start_matches("as ");
        let alias = rest.split_whitespace().next()?;
        (!alias.is_empty()).then(|| alias.to_string())
    }

    pub fn is_wildcard(&self) -> bool {
        self.0.text().to_string().trim_start().starts_with("import *")
    }

    pub fn named_item_specs(&self) -> Vec<(String, Option<String>)> {
        let text = self.0.text().to_string();
        let Some(start) = text.find('{') else {
            return Vec::new();
        };
        let Some(end) = text[start + 1..].find('}') else {
            return Vec::new();
        };
        let inside = &text[start + 1..start + 1 + end];
        inside
            .split(',')
            .filter_map(|part| {
                let item = part.trim();
                if item.is_empty() {
                    return None;
                }
                if let Some((imported, alias)) = item.split_once(" as ") {
                    Some((imported.trim().to_string(), Some(alias.trim().to_string())))
                } else {
                    Some((item.to_string(), None))
                }
            })
            .collect()
    }

    pub fn named_items(&self) -> Vec<ImportNamedItem> {
        self.0.descendants().filter(|n| n.kind() == LSyntaxKind::IMPORT_NAMED_ITEM).filter_map(ImportNamedItem::cast).collect()
    }
}

ast_node!(AnonModel, LSyntaxKind::ANON_MODEL);

impl AnonModel {
    pub fn field_nodes(&self) -> Vec<LNode> {
        self.0
            .children()
            .find(|n| n.kind() == LSyntaxKind::MODEL_BODY)
            .into_iter()
            .flat_map(|model_body| model_body.children().filter(|n| n.kind() == LSyntaxKind::FIELD))
            .collect()
    }
}

ast_node!(Model, LSyntaxKind::MODEL);

impl Model {
    pub fn ident_token(&self) -> Option<LToken> {
        self.0.children_with_tokens().filter_map(|e| e.into_token()).find(|t| t.kind() == LSyntaxKind::IDENT)
    }

    pub fn ident(&self) -> Option<String> {
        self.ident_token().map(|t| t.text().to_string())
    }

    pub fn docs(&self) -> Option<Vec<String>> {
        self.0
            .children_with_tokens()
            .find(|t| t.kind() == LSyntaxKind::DOC_BLOCK)
            .and_then(|doc_block_node| doc_block_node.into_token().map(|n| n.text().to_string()))
            .map(|text| text.lines().map(|line| line.trim().trim_start_matches("///").trim().to_string()).collect())
    }

    pub fn field_nodes(&self) -> Vec<LNode> {
        self.0
            .children()
            .find(|n| n.kind() == LSyntaxKind::MODEL_BODY)
            .into_iter()
            .flat_map(|model_body| model_body.children().filter(|n| n.kind() == LSyntaxKind::FIELD))
            .collect()
    }

    pub fn fields(&self) -> Vec<Field> {
        self.field_nodes().into_iter().filter_map(Field::cast).collect()
    }

    pub fn nested_model_nodes(&self) -> Vec<LNode> {
        self.0
            .children()
            .find(|n| n.kind() == LSyntaxKind::MODEL_BODY)
            .into_iter()
            .flat_map(|model_body| model_body.children().filter(|n| n.kind() == LSyntaxKind::MODEL))
            .collect()
    }

    pub fn nested_models(&self) -> Vec<Model> {
        self.nested_model_nodes().into_iter().filter_map(Model::cast).collect()
    }

    pub fn nested_enum_nodes(&self) -> Vec<LNode> {
        self.0
            .children()
            .find(|n| n.kind() == LSyntaxKind::MODEL_BODY)
            .into_iter()
            .flat_map(|model_body| model_body.children().filter(|n| n.kind() == LSyntaxKind::ENUM))
            .collect()
    }

    pub fn nested_enums(&self) -> Vec<Enum> {
        self.nested_enum_nodes().into_iter().filter_map(Enum::cast).collect()
    }

    pub fn decorator_nodes(&self) -> Vec<LNode> {
        self.0.children().filter(|n| n.kind() == LSyntaxKind::DECORATOR).collect()
    }

    pub fn decorators(&self) -> Vec<Decorator> {
        self.decorator_nodes().into_iter().filter_map(Decorator::cast).collect()
    }
}

ast_node!(Endpoint, LSyntaxKind::ENDPOINT);

impl Endpoint {
    pub fn path_string_literal_node(&self) -> Option<StringLiteral> {
        self.0
            .children_with_tokens()
            .find(|n| n.kind() == LSyntaxKind::STRING_LITERAL)
            .and_then(|n| n.into_node())
            .and_then(StringLiteral::cast)
    }

    pub fn path_string(&self) -> Option<String> {
        self.path_string_literal_node().and_then(|s| s.value())
    }

    pub fn ident_token(&self) -> Option<LToken> {
        self.0.children_with_tokens().filter_map(|e| e.into_token()).find(|t| t.kind() == LSyntaxKind::IDENT)
    }

    pub fn ident(&self) -> Option<String> {
        self.ident_token().map(|t| t.text().to_string())
    }

    pub fn docs(&self) -> Option<Vec<String>> {
        self.0
            .children_with_tokens()
            .find(|t| t.kind() == LSyntaxKind::DOC_BLOCK)
            .and_then(|doc_block_node| doc_block_node.into_token().map(|n| n.text().to_string()))
            .map(|text| text.lines().map(|line| line.trim().trim_start_matches("///").trim().to_string()).collect())
    }

    pub fn field_nodes(&self) -> Vec<LNode> {
        self.model_body()
            .into_iter()
            .flat_map(|model_body| model_body.children().filter(|n| n.kind() == LSyntaxKind::FIELD))
            .collect()
    }

    pub fn fields(&self) -> Vec<Field> {
        self.field_nodes().into_iter().filter_map(Field::cast).collect()
    }

    pub fn nested_model_nodes(&self) -> Vec<LNode> {
        self.model_body()
            .into_iter()
            .flat_map(|model_body| model_body.children().filter(|n| n.kind() == LSyntaxKind::MODEL))
            .collect()
    }

    pub fn nested_models(&self) -> Vec<Model> {
        self.nested_model_nodes().into_iter().filter_map(Model::cast).collect()
    }

    pub fn nested_enum_nodes(&self) -> Vec<LNode> {
        self.model_body()
            .into_iter()
            .flat_map(|model_body| model_body.children().filter(|n| n.kind() == LSyntaxKind::ENUM))
            .collect()
    }

    pub fn nested_enums(&self) -> Vec<Enum> {
        self.nested_enum_nodes().into_iter().filter_map(Enum::cast).collect()
    }

    pub fn responses_field_node(&self) -> Option<LNode> {
        self.model_body().and_then(|model_body| {
            model_body
                .children()
                .filter(|n| n.kind() == LSyntaxKind::FIELD)
                .find(|field| Field::cast(field.clone()).map(|f| f.ident().as_deref() == Some("responses")).unwrap_or(false))
        })
    }

    pub fn body_field_node(&self) -> Option<LNode> {
        self.model_body().and_then(|model_body| {
            model_body
                .children()
                .filter(|n| n.kind() == LSyntaxKind::FIELD)
                .find(|field| Field::cast(field.clone()).map(|f| f.ident().as_deref() == Some("body")).unwrap_or(false))
        })
    }

    // We reuse the model body production for endpoints - thus it makes sense to have such a convenience method
    fn model_body(&self) -> Option<LNode> {
        self.0.children().find(|n| n.kind() == LSyntaxKind::MODEL_BODY)
    }
}

ast_node!(EnumVariant, LSyntaxKind::ENUM_VARIANT);

impl EnumVariant {
    pub fn value(&self) -> Option<String> {
        self.0
            .children_with_tokens()
            .find(|n| n.kind() == LSyntaxKind::STRING_LITERAL)
            .and_then(|string_node| string_node.into_node())
            .and_then(|n| n.children_with_tokens().find(|t| t.kind() == LSyntaxKind::STRING_LITERAL_INNER))
            .map(|n| n.into_token())
            .and_then(|t| t.map(|token| token.text().to_string()))
            .map(|s| s.trim().trim_start_matches('"').trim_end_matches('"').to_string())
    }

    pub fn docs(&self) -> Option<Vec<String>> {
        self.0
            .children_with_tokens()
            .find(|t| t.kind() == LSyntaxKind::DOC_BLOCK)
            .and_then(|doc_block_node| doc_block_node.into_token().map(|n| n.text().to_string()))
            .map(|text| text.lines().map(|line| line.trim().trim_start_matches("///").trim().to_string()).collect())
    }
}

ast_node!(Enum, LSyntaxKind::ENUM);

impl Enum {
    pub fn ident_token(&self) -> Option<LToken> {
        self.0.children_with_tokens().filter_map(|e| e.into_token()).find(|t| t.kind() == LSyntaxKind::IDENT)
    }

    pub fn ident(&self) -> Option<String> {
        self.ident_token().map(|t| t.text().to_string())
    }

    pub fn docs(&self) -> Option<Vec<String>> {
        self.0
            .children_with_tokens()
            .find(|t| t.kind() == LSyntaxKind::DOC_BLOCK)
            .and_then(|doc_block_node| doc_block_node.into_token().map(|n| n.text().to_string()))
            .map(|text| text.lines().map(|line| line.trim().trim_start_matches("///").trim().to_string()).collect())
    }

    pub fn variant_nodes(&self) -> Vec<LNode> {
        self.0
            .children()
            .find(|n| n.kind() == LSyntaxKind::ENUM_VARIANTS)
            .into_iter()
            .flat_map(|enum_variants| enum_variants.children().filter(|n| n.kind() == LSyntaxKind::ENUM_VARIANT))
            .collect()
    }

    pub fn variants(&self) -> Vec<EnumVariant> {
        self.variant_nodes().into_iter().filter_map(EnumVariant::cast).collect()
    }
}

ast_node!(Decorator, LSyntaxKind::DECORATOR);

ast_node!(DecoratorArg, LSyntaxKind::DECORATOR_NAMED_ARG LSyntaxKind::DECORATOR_POSITIONAL_ARG);

impl DecoratorArg {
    pub fn ident_node(&self) -> Option<LNode> {
        self.0.children().find(|n| n.kind() == LSyntaxKind::IDENT)
    }

    pub fn ident(&self) -> Option<String> {
        self.0
            .children_with_tokens()
            .find(|n| n.kind() == LSyntaxKind::IDENT)
            .and_then(|n| n.into_token())
            .map(|t| t.text().to_string())
    }

    pub fn literal_expr(&self) -> Option<LiteralExpr> {
        let literal_node = self.0.children().find(|n| n.kind() == LSyntaxKind::LITERAL)?;
        LiteralExpr::cast(literal_node)
    }

    pub fn literal(&self) -> Option<Literal> {
        self.literal_expr().and_then(|le| le.value())
    }
}

impl Decorator {
    pub fn ident_token(&self) -> Option<LToken> {
        self.0.children_with_tokens().find(|n| n.kind() == LSyntaxKind::IDENT).and_then(|ident_node| ident_node.into_token())
    }

    pub fn ident(&self) -> Option<String> {
        self.ident_token().map(|t| t.text().to_string())
    }

    pub fn arg_nodes(&self) -> Vec<LNode> {
        self.0
            .children()
            .filter(|n| n.kind() == LSyntaxKind::DECORATOR_NAMED_ARG || n.kind() == LSyntaxKind::DECORATOR_POSITIONAL_ARG)
            .collect()
    }

    pub fn args(&self) -> Vec<DecoratorArg> {
        self.arg_nodes().into_iter().filter_map(DecoratorArg::cast).collect()
    }

    pub fn positional_args(&self) -> Vec<DecoratorArg> {
        self.arg_nodes()
            .into_iter()
            .filter(|n| n.kind() == LSyntaxKind::DECORATOR_POSITIONAL_ARG)
            .filter_map(DecoratorArg::cast)
            .collect()
    }

    pub fn named_args(&self) -> Vec<DecoratorArg> {
        self.arg_nodes()
            .into_iter()
            .filter(|n| n.kind() == LSyntaxKind::DECORATOR_NAMED_ARG)
            .filter_map(DecoratorArg::cast)
            .collect()
    }

    pub fn arg(&self, decorator_def: &DecoratorDef, arg_def: &DecoratorArgDef) -> Option<DecoratorArg> {
        // Either in the expected position if it's a positional arg, or with the expected ident if it's a named arg.
        if let Some(expected_pos) = arg_def.expected_position
            && expected_pos < self.positional_args().len()
        {
            Some(self.positional_args()[expected_pos].clone())
        } else if decorator_def.named_args.iter().any(|arg| arg.id == arg_def.id) {
            self.named_args().into_iter().find(|arg| arg.ident().as_deref() == Some(arg_def.id))
        } else {
            None
        }
    }
}

ast_node!(StringLiteral, LSyntaxKind::STRING_LITERAL);

impl std::fmt::Display for StringLiteral {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.value() {
            Some(value) => write!(f, "\"{}\"", value),
            None => write!(f, "\"\""),
        }
    }
}

impl StringLiteral {
    pub fn value(&self) -> Option<String> {
        self.0
            .children_with_tokens()
            .find(|n| n.kind() == LSyntaxKind::STRING_LITERAL_INNER)
            .and_then(|n| n.into_token())
            .map(|n| n.text().to_string())
            .map(|s| s.trim().trim_start_matches('"').trim_end_matches('"').to_string())
    }
}

ast_node!(ListLiteral, LSyntaxKind::LIST_LITERAL);

impl ListLiteral {
    pub fn values(&self) -> Vec<Literal> {
        self.0
            .children()
            .filter(|n| n.kind() == LSyntaxKind::LITERAL)
            .filter_map(LiteralExpr::cast)
            .filter_map(|le| le.value())
            .collect()
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ConstExprType {
    String,
    Int,
    Float,
    Bool,
    List(&'static ConstExprType),
    Record(&'static ConstExprType, &'static ConstExprType),
}

impl std::fmt::Display for ConstExprType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ConstExprType::String => write!(f, "string"),
            ConstExprType::Int => write!(f, "int"),
            ConstExprType::Float => write!(f, "float"),
            ConstExprType::Bool => write!(f, "bool"),
            ConstExprType::List(inner) => write!(f, "list<{}>", inner),
            ConstExprType::Record(key_type, value_type) => write!(f, "record<{}, {}>", key_type, value_type),
        }
    }
}

#[derive(Debug, Clone)]
pub enum Literal {
    // TODO: Align all types to contain node
    StringLiteral(StringLiteral),
    ListLiteral(ListLiteral),
    IntLiteral { value: i64, node: LNodeOrToken },
    FloatLiteral { value: f64, node: LNodeOrToken },
    BoolLiteral { value: bool, node: LNodeOrToken },
}

impl Literal {
    pub fn ty(&self) -> ConstExprType {
        match self {
            Literal::StringLiteral(_) => ConstExprType::String,
            Literal::ListLiteral(_inner) => ConstExprType::List(&ConstExprType::String), // TODO: Any?
            Literal::IntLiteral { .. } => ConstExprType::Int,
            Literal::FloatLiteral { .. } => ConstExprType::Float,
            Literal::BoolLiteral { .. } => ConstExprType::Bool,
        }
    }
}

impl std::fmt::Display for Literal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Literal::StringLiteral(string_literal) => write!(f, "\"string ({})\"", string_literal.value().unwrap_or_default()),
            Literal::ListLiteral(list_literal) => write!(f, "\"list ({})\"", list_literal.syntax().text()),
            Literal::IntLiteral { value, .. } => write!(f, "\"int ({})\"", value),
            Literal::FloatLiteral { value, .. } => write!(f, "\"float ({})\"", value),
            Literal::BoolLiteral { value, .. } => write!(f, "\"bool ({})\"", value),
        }
    }
}

ast_node!(LiteralExpr, LSyntaxKind::LITERAL);

impl LiteralExpr {
    pub fn value(&self) -> Option<Literal> {
        let child = self.0.children_with_tokens().find(|n| {
            matches!(
                n.kind(),
                LSyntaxKind::STRING_LITERAL
                    | LSyntaxKind::BOOL_LITERAL
                    | LSyntaxKind::TRUE_LITERAL
                    | LSyntaxKind::FALSE_LITERAL
                    | LSyntaxKind::INT_LITERAL
                    | LSyntaxKind::LIST_LITERAL
                    | LSyntaxKind::IDENT
            )
        })?;

        match child.kind() {
            LSyntaxKind::STRING_LITERAL => Some(Literal::StringLiteral(StringLiteral(child.into_node().unwrap()))),
            LSyntaxKind::BOOL_LITERAL => {
                let bool_node = child.into_node().unwrap();
                let inner_literal = bool_node.children_with_tokens().find(|n| matches!(n.kind(), LSyntaxKind::TRUE_LITERAL | LSyntaxKind::FALSE_LITERAL))?;
                match inner_literal.kind() {
                    LSyntaxKind::TRUE_LITERAL => Some(Literal::BoolLiteral {
                        value: true,
                        node: LNodeOrToken::Node(bool_node),
                    }),
                    LSyntaxKind::FALSE_LITERAL => Some(Literal::BoolLiteral {
                        value: false,
                        node: LNodeOrToken::Node(bool_node),
                    }),
                    _ => None,
                }
            }
            LSyntaxKind::INT_LITERAL => {
                let text = child.clone().into_token().unwrap().text().to_string();
                if let Ok(int_value) = text.parse::<i64>() {
                    Some(Literal::IntLiteral {
                        value: int_value,
                        node: LNodeOrToken::Token(child.into_token().unwrap()),
                    })
                } else if let Ok(float_value) = text.parse::<f64>() {
                    Some(Literal::FloatLiteral {
                        value: float_value,
                        node: LNodeOrToken::Token(child.into_token().unwrap()),
                    })
                } else {
                    None
                }
            }
            LSyntaxKind::LIST_LITERAL => Some(Literal::ListLiteral(ListLiteral(child.into_node().unwrap()))),
            LSyntaxKind::IDENT => {
                let text = child.clone().into_token().unwrap().text().to_string();
                if let Ok(int_value) = text.parse::<i64>() {
                    Some(Literal::IntLiteral {
                        value: int_value,
                        node: LNodeOrToken::Token(child.into_token().unwrap()),
                    })
                } else if let Ok(float_value) = text.parse::<f64>() {
                    Some(Literal::FloatLiteral {
                        value: float_value,
                        node: LNodeOrToken::Token(child.into_token().unwrap()),
                    })
                } else {
                    None
                }
            }
            _ => None,
        }
    }
}

ast_node!(Field, LSyntaxKind::FIELD);

impl Field {
    pub fn ident_node(&self) -> Option<LNodeOrToken> {
        self.0
            .children()
            .find(|n| n.kind() == LSyntaxKind::FIELD_NAME)
            .and_then(|field_name| field_name.children_with_tokens().find(|t| t.kind() == LSyntaxKind::IDENT || t.kind() == LSyntaxKind::STRING_LITERAL))
    }

    pub fn ident(&self) -> Option<String> {
        self.ident_node().map(|ident_node_or_token| match ident_node_or_token {
            LNodeOrToken::Node(n) => n.text().to_string(),
            LNodeOrToken::Token(tok) => tok.text().to_string(),
        })
    }

    pub fn is_optional(&self) -> bool {
        self.0.children_with_tokens().any(|n| n.kind() == LSyntaxKind::OPTIONAL_MODIFIER)
    }

    pub fn docs(&self) -> Option<Vec<String>> {
        self.0
            .children_with_tokens()
            .find(|t| t.kind() == LSyntaxKind::DOC_BLOCK)
            .and_then(|doc_block_node| doc_block_node.into_token().map(|n| n.text().to_string()))
            .map(|text| text.lines().map(|line| line.trim().trim_start_matches("///").trim().to_string()).collect())
    }

    pub fn type_node(&self) -> Option<LNode> {
        self.0.children().find(|n| n.kind() == LSyntaxKind::TYPE)
    }

    pub fn default_literal_expr_node(&self) -> Option<LNode> {
        self.0
            .children()
            .find(|n| n.kind() == LSyntaxKind::FIELD_DEFAULT_VALUE)
            .and_then(|node| node.children().find(|n| n.kind() == LSyntaxKind::LITERAL))
    }

    pub fn ty(&self) -> Option<Type> {
        self.type_node().and_then(Type::cast)
    }

    pub fn decorator_nodes(&self) -> Vec<LNode> {
        self.0.children().filter(|n| n.kind() == LSyntaxKind::DECORATOR).collect()
    }

    pub fn decorators(&self) -> Vec<Decorator> {
        self.decorator_nodes().into_iter().filter_map(Decorator::cast).collect()
    }

    pub fn extract_decorator_arg(&self, decorator_def: &DecoratorDef, arg_def: &DecoratorArgDef) -> Option<DecoratorArg> {
        for decorator in self.decorators() {
            if decorator.ident().as_deref() == Some(decorator_def.id) {
                return decorator.arg(decorator_def, arg_def);
            }
        }
        None
    }
}

#[derive(Debug, Clone, Copy)]
pub enum PrimitiveType {
    Any,
    String,
    Int,
    Float,
    Bool,
}

impl std::fmt::Display for PrimitiveType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = match self {
            PrimitiveType::Any => "any",
            PrimitiveType::String => "string",
            PrimitiveType::Int => "int",
            PrimitiveType::Float => "float",
            PrimitiveType::Bool => "bool",
        };
        write!(f, "{}", s)
    }
}

impl TryFrom<&str> for PrimitiveType {
    type Error = ();
    fn try_from(s: &str) -> Result<Self, Self::Error> {
        match s.trim() {
            "any" => Ok(PrimitiveType::Any),
            "string" => Ok(PrimitiveType::String),
            "int" => Ok(PrimitiveType::Int),
            "float" => Ok(PrimitiveType::Float),
            "bool" => Ok(PrimitiveType::Bool),
            _ => Err(()),
        }
    }
}

ast_node!(Type, LSyntaxKind::TYPE);

impl Type {
    pub fn type_atom_nodes(&self) -> Vec<LNode> {
        self.0.children().filter(|n| n.kind() == LSyntaxKind::TYPE_ATOM).collect()
    }

    pub fn type_atoms(&self) -> Vec<TypeAtom> {
        self.type_atom_nodes().into_iter().filter_map(TypeAtom::cast).collect()
    }

    pub fn as_single_ref(&self) -> Option<String> {
        let type_atoms: Vec<_> = self.type_atom_nodes();
        if type_atoms.len() != 1 {
            return None;
        }
        let first_atom = &type_atoms[0];
        let type_atom = TypeAtom::cast(first_atom.clone())?;
        if type_atom.as_primitive_type().is_some() {
            return None;
        }
        let ident_token = type_atom.as_ref_token()?;
        Some(ident_token.text().to_string())
    }
}

ast_node!(RecordType, LSyntaxKind::RECORD);

impl RecordType {
    pub fn src_type_node(&self) -> Option<LNode> {
        self.0.children().find(|n| n.kind() == LSyntaxKind::TYPE)
    }
    pub fn dest_type_node(&self) -> Option<LNode> {
        self.0.children().filter(|n| n.kind() == LSyntaxKind::TYPE).nth(1)
    }
}

ast_node!(TypeAtom, LSyntaxKind::TYPE_ATOM);

impl TypeAtom {
    pub fn is_optional(&self) -> bool {
        let modifiers = self.0.children_with_tokens().find(|n| n.kind() == LSyntaxKind::TYPE_ATOM_MODIFIERS);
        modifiers.is_some_and(|m| m.to_string().contains("?"))
    }

    pub fn is_array(&self) -> bool {
        let modifiers = self.0.children_with_tokens().find(|n| n.kind() == LSyntaxKind::TYPE_ATOM_MODIFIERS);
        modifiers.is_some_and(|m| m.to_string().contains("[]"))
    }

    pub fn as_ref_token(&self) -> Option<LToken> {
        if let Some(type_ref) = self.0.children().find(|n| n.kind() == LSyntaxKind::TYPE_REF)
            && let Some(token) = type_ref.children_with_tokens().find(|n| n.kind() == LSyntaxKind::IDENT).and_then(|ident_node| ident_node.into_token())
        {
            return Some(token);
        }

        self.0.children_with_tokens().find(|n| n.kind() == LSyntaxKind::IDENT).and_then(|ident_node| ident_node.into_token())
    }

    pub fn as_ref_name(&self) -> Option<String> {
        self.0
            .children()
            .find(|n| n.kind() == LSyntaxKind::TYPE_REF)
            .map(|n| n.text().to_string())
            .or_else(|| self.as_ref_token().map(|t| t.text().to_string()))
    }

    pub fn as_anon_model(&self) -> Option<LNode> {
        self.0.children().find(|n| n.kind() == LSyntaxKind::ANON_MODEL)
    }

    pub fn as_record_type(&self) -> Option<RecordType> {
        self.0.children().find(|n| n.kind() == LSyntaxKind::RECORD).and_then(RecordType::cast)
    }

    // Note: Ignores modifiers
    pub fn as_primitive_type(&self) -> Option<PrimitiveType> {
        let mut type_atom_text = self.0.text().to_string();
        // TODO: Improve... this is hacky
        type_atom_text = type_atom_text.replace("?", "").replace("[]", "").trim().to_string();
        PrimitiveType::try_from(type_atom_text.as_str()).ok()
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        metadata::SourceCodeMetadata,
        syntax::{
            ast::{Field, Model, Type, TypeAtom},
            parser::Parser,
        },
    };
    use indoc::indoc;

    use super::*;

    #[test]
    fn test_basic() {
        let src = indoc! { r#"
                @root
				model User {
                    @deprecated
					name: string
                    id: string
				}
				"# };

        let metadata = SourceCodeMetadata {
            file_name: "test.glue",
            file_contents: src,
        };
        let parsed = Parser::new().parse(&metadata).unwrap();
        let ast_root = parsed.ast_root;

        let models: Vec<_> = ast_root.children().filter(|n| n.kind() == LSyntaxKind::MODEL).collect();
        println!("{} models", models.len());

        for model_node in &models {
            let model = Model::cast(model_node.clone()).unwrap();
            let model_ident = model.ident_token().unwrap().text().to_string();
            println!("Model {}", model_ident);

            let model_decorators = model.decorator_nodes();
            for decorator_node in &model_decorators {
                let decorator = Decorator::cast(decorator_node.clone()).unwrap();
                let decorator_ident = decorator.ident_token().unwrap().text().to_string();
                println!("  Decorator {}", decorator_ident);
            }

            let field_nodes = model.field_nodes();

            for field_node in &field_nodes {
                let field = Field::cast(field_node.clone()).unwrap();
                let field_ident = match field.ident_node().unwrap() {
                    LNodeOrToken::Node(n) => n.text().to_string(),
                    LNodeOrToken::Token(tok) => tok.text().to_string(),
                };
                println!("  Field {}", field_ident);
                let field_decorators = field.decorator_nodes();
                for decorator_node in &field_decorators {
                    let decorator = Decorator::cast(decorator_node.clone()).unwrap();
                    let decorator_ident = decorator.ident_token().unwrap().text().to_string();
                    println!("    Decorator {}", decorator_ident);
                }

                let field_type = Type::cast(field.type_node().unwrap()).unwrap();
                let field_type_atoms = field_type.type_atom_nodes();
                for type_atom_node in &field_type_atoms {
                    let type_atom = TypeAtom::cast(type_atom_node.clone()).unwrap();
                    let type_atom_str = type_atom_node.text().to_string();
                    println!("    TypeAtom: {}", type_atom_str);
                    let is_array = type_atom.is_array();
                    let is_optional = type_atom.is_optional();
                    println!("      is_array: {}, is_optional: {}", is_array, is_optional);
                }
            }
        }
    }

    #[test]
    fn test_import_statements_with_aliases() {
        let src = indoc! { r#"
            import * from "./models.glue"
            import { SomeModel } from "./aws_models.glue"
            import * as Models from "./models.glue"

            model Root {
                item: Models.SomeModel
            }
        "# };

        let metadata = SourceCodeMetadata {
            file_name: "test.glue",
            file_contents: src,
        };

        let parsed = Parser::new().parse(&metadata).expect("expected parser to accept import statements");
        let root = RootNode::cast(parsed.ast_root).expect("expected RootNode");
        let imports = root.top_level_imports();
        assert_eq!(imports.len(), 3);
        assert!(imports[0].is_wildcard());
        assert_eq!(imports[0].wildcard_alias(), None);
        assert_eq!(imports[1].named_item_specs(), vec![("SomeModel".to_string(), None)]);
        assert_eq!(imports[2].wildcard_alias().as_deref(), Some("Models"));
    }
}
