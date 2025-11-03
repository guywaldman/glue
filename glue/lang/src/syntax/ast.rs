use crate::syntax::parser::{LNode, LSyntaxKind, LToken};

pub trait AstNode: Sized {
    fn cast(n: LNode) -> Option<Self>;
    fn syntax(&self) -> &LNode;
}

#[derive(Debug, Clone)]
pub struct Model(LNode);

impl AstNode for Model {
    fn cast(n: LNode) -> Option<Self> {
        (n.kind() == LSyntaxKind::MODEL).then(|| Model(n))
    }

    fn syntax(&self) -> &LNode {
        &self.0
    }
}

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

    pub fn nested_model_nodes(&self) -> Vec<LNode> {
        self.0
            .children()
            .find(|n| n.kind() == LSyntaxKind::MODEL_BODY)
            .into_iter()
            .flat_map(|model_body| model_body.children().filter(|n| n.kind() == LSyntaxKind::MODEL))
            .collect()
    }

    pub fn nested_enum_nodes(&self) -> Vec<LNode> {
        self.0
            .children()
            .find(|n| n.kind() == LSyntaxKind::MODEL_BODY)
            .into_iter()
            .flat_map(|model_body| model_body.children().filter(|n| n.kind() == LSyntaxKind::ENUM))
            .collect()
    }

    pub fn decorator_nodes(&self) -> Vec<LNode> {
        self.0.children().filter(|n| n.kind() == LSyntaxKind::DECORATOR).collect()
    }

    pub fn decorators(&self) -> Vec<Decorator> {
        self.decorator_nodes().into_iter().filter_map(Decorator::cast).collect()
    }
}

#[derive(Debug, Clone)]
pub struct EnumVariant(LNode);

impl AstNode for EnumVariant {
    fn cast(n: LNode) -> Option<Self> {
        (n.kind() == LSyntaxKind::ENUM_VARIANT).then(|| EnumVariant(n))
    }

    fn syntax(&self) -> &LNode {
        &self.0
    }
}

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

#[derive(Debug, Clone)]
pub struct Enum(LNode);

impl AstNode for Enum {
    fn cast(n: LNode) -> Option<Self> {
        (n.kind() == LSyntaxKind::ENUM).then(|| Enum(n))
    }

    fn syntax(&self) -> &LNode {
        &self.0
    }
}

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
}

#[derive(Debug, Clone)]
pub struct Decorator(LNode);

impl AstNode for Decorator {
    fn cast(n: LNode) -> Option<Self> {
        (n.kind() == LSyntaxKind::DECORATOR).then(|| Decorator(n))
    }

    fn syntax(&self) -> &LNode {
        &self.0
    }
}

impl Decorator {
    pub fn ident_token(&self) -> Option<LToken> {
        self.0.children_with_tokens().find(|n| n.kind() == LSyntaxKind::IDENT).and_then(|ident_node| ident_node.into_token())
    }

    pub fn name(&self) -> Option<String> {
        self.ident_token().map(|t| t.text().to_string())
    }
}

#[derive(Debug, Clone)]
pub enum Literal {
    StringLiteral(String),
    IntLiteral(i64),
    FloatLiteral(f64),
    BoolLiteral(bool),
}

#[derive(Debug, Clone)]
pub struct LiteralExpr(LNode);

impl AstNode for LiteralExpr {
    fn cast(n: LNode) -> Option<Self> {
        (n.kind() == LSyntaxKind::LITERAL).then(|| LiteralExpr(n))
    }

    fn syntax(&self) -> &LNode {
        &self.0
    }
}

impl LiteralExpr {
    pub fn value(&self) -> Option<Literal> {
        let child = self.0.children_with_tokens().find(|n| {
            matches!(
                n.kind(),
                LSyntaxKind::STRING_LITERAL | LSyntaxKind::BOOL_LITERAL | LSyntaxKind::TRUE_LITERAL | LSyntaxKind::FALSE_LITERAL | LSyntaxKind::IDENT
            )
        })?;

        match child.kind() {
            LSyntaxKind::STRING_LITERAL => Some(Literal::StringLiteral(
                child.into_node().unwrap().text().to_string().trim().trim_start_matches("\"").trim_end_matches("\"").to_string(),
            )),
            LSyntaxKind::BOOL_LITERAL => {
                let bool_node = child.into_node().unwrap();
                let inner_literal = bool_node.children_with_tokens().find(|n| matches!(n.kind(), LSyntaxKind::TRUE_LITERAL | LSyntaxKind::FALSE_LITERAL))?;
                match inner_literal.kind() {
                    LSyntaxKind::TRUE_LITERAL => Some(Literal::BoolLiteral(true)),
                    LSyntaxKind::FALSE_LITERAL => Some(Literal::BoolLiteral(false)),
                    _ => None,
                }
            }
            LSyntaxKind::IDENT => {
                let text = child.into_token().unwrap().text().to_string();
                if let Ok(int_value) = text.parse::<i64>() {
                    Some(Literal::IntLiteral(int_value))
                } else if let Ok(float_value) = text.parse::<f64>() {
                    Some(Literal::FloatLiteral(float_value))
                } else {
                    None
                }
            }
            _ => None,
        }
    }
}

#[derive(Debug, Clone)]
pub struct Field(LNode);

impl AstNode for Field {
    fn cast(n: LNode) -> Option<Self> {
        (n.kind() == LSyntaxKind::FIELD).then(|| Field(n))
    }

    fn syntax(&self) -> &LNode {
        &self.0
    }
}

impl Field {
    pub fn ident_token(&self) -> Option<LToken> {
        self.0
            .children()
            .find(|n| n.kind() == LSyntaxKind::FIELD_NAME)
            .and_then(|field_name| field_name.children_with_tokens().filter_map(|e| e.into_token()).find(|t| t.kind() == LSyntaxKind::IDENT))
    }

    pub fn ident(&self) -> Option<String> {
        self.ident_token().map(|t| t.text().to_string())
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
}

#[derive(Debug, Clone, Copy)]
pub enum PrimitiveType {
    String,
    Int,
    Float,
    Bool,
}

impl TryFrom<&str> for PrimitiveType {
    type Error = ();
    fn try_from(s: &str) -> Result<Self, Self::Error> {
        match s.trim() {
            "string" => Ok(PrimitiveType::String),
            "int" => Ok(PrimitiveType::Int),
            "float" => Ok(PrimitiveType::Float),
            "bool" => Ok(PrimitiveType::Bool),
            _ => Err(()),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Type(LNode);

impl AstNode for Type {
    fn cast(n: LNode) -> Option<Self> {
        (n.kind() == LSyntaxKind::TYPE).then(|| Type(n))
    }
    fn syntax(&self) -> &LNode {
        &self.0
    }
}

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
        let ident_token = type_atom.ident_token()?;
        Some(ident_token.text().to_string())
    }
}

#[derive(Debug, Clone)]
pub struct TypeAtom(LNode);

impl AstNode for TypeAtom {
    fn cast(n: LNode) -> Option<Self> {
        (n.kind() == LSyntaxKind::TYPE_ATOM).then(|| TypeAtom(n))
    }

    fn syntax(&self) -> &LNode {
        &self.0
    }
}

impl TypeAtom {
    pub fn is_optional(&self) -> bool {
        let modifiers = self.0.children_with_tokens().find(|n| n.kind() == LSyntaxKind::TYPE_ATOM_MODIFIERS);
        modifiers.is_some_and(|m| m.to_string().contains("?"))
    }

    pub fn is_array(&self) -> bool {
        let modifiers = self.0.children_with_tokens().find(|n| n.kind() == LSyntaxKind::TYPE_ATOM_MODIFIERS);
        modifiers.is_some_and(|m| m.to_string().contains("[]"))
    }

    pub fn ident_token(&self) -> Option<LToken> {
        self.0.children_with_tokens().find(|n| n.kind() == LSyntaxKind::IDENT).and_then(|ident_node| ident_node.into_token())
    }

    pub fn as_anon_model(&self) -> Option<LNode> {
        self.0.children().find(|n| n.kind() == LSyntaxKind::ANON_MODEL)
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
    fn test_temp() {
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
                let field_ident = field.ident_token().unwrap().text().to_string();
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
}
