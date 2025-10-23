use pest::Span;

#[derive(Debug, Clone)]
pub struct Decorator<'a> {
    pub name: &'a str,
}

#[derive(Debug, Clone)]
pub struct Model<'a> {
    pub name: &'a str,
    pub is_anon: bool,
    pub decorators: Vec<AstNode<'a>>,
    pub nested_models: Vec<AstNode<'a>>,
    pub fields: Vec<AstNode<'a>>,
}

#[derive(Debug, Clone)]
pub struct Endpoint<'a> {
    pub name: &'a str,
    pub fields: Vec<AstNode<'a>>,
}

#[derive(Debug, Clone)]
pub enum Type<'a> {
    String,
    Int,
    Ref(&'a str),
    AnonModel(Box<AstNode<'a>>),
}

#[derive(Debug, Clone)]
pub struct Field<'a> {
    pub name: &'a str,
    pub ty: Box<AstNode<'a>>,
}

#[derive(Debug, Clone)]
pub enum AstNodeKind {
    Model,
    Endpoint,
    Decorator,
    Field,
    Type,
}

#[derive(Clone)]
pub enum AstNode<'a> {
    Model { payload: Model<'a>, span: Span<'a> },
    Endpoint { payload: Endpoint<'a>, span: Span<'a> },
    Decorator { payload: Decorator<'a>, span: Span<'a> },
    Field { payload: Field<'a>, span: Span<'a> },
    Type { payload: Type<'a>, span: Span<'a> },
}

impl<'a> AstNode<'a> {
    pub fn span(&self) -> &Span<'a> {
        match self {
            AstNode::Model { span, .. } => span,
            AstNode::Endpoint { span, .. } => span,
            AstNode::Decorator { span, .. } => span,
            AstNode::Field { span, .. } => span,
            AstNode::Type { span, .. } => span,
        }
    }

    pub fn kind(&self) -> AstNodeKind {
        match self {
            AstNode::Model { .. } => AstNodeKind::Model,
            AstNode::Endpoint { .. } => AstNodeKind::Endpoint,
            AstNode::Decorator { .. } => AstNodeKind::Decorator,
            AstNode::Field { .. } => AstNodeKind::Field,
            AstNode::Type { .. } => AstNodeKind::Type,
        }
    }
}

impl std::fmt::Debug for AstNode<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            AstNode::Model { payload, .. } => write!(f, "<model> {{ name: {}, fields: {:?} }}", payload.name, payload.fields),
            AstNode::Endpoint { payload, .. } => write!(f, "<endpoint> {{ name: {}, fields: {:?} }}", payload.name, payload.fields),
            AstNode::Decorator { payload, .. } => write!(f, "<decorator> @{}", payload.name),
            AstNode::Field { payload, .. } => write!(f, "<field> {}: {:?}", payload.name, payload.ty),
            AstNode::Type { payload, .. } => write!(f, "<type> {:?}", payload),
        }
    }
}

#[derive(Debug, Default, Clone)]
pub struct Ast<'a> {
    pub nodes: Vec<AstNode<'a>>,
}

impl<'a> Ast<'a> {
    pub fn new() -> Self {
        Self { nodes: Vec::new() }
    }
}

// Mermaid generation
impl<'a> Ast<'a> {
    pub fn to_mermaid(&self) -> String {
        let mut result = String::new();
        result.push_str("graph TD\n");
        result.push_str("    0[\"Root\"]\n");
        result.push_str("    style Root fill:#4a0e0e,stroke:#333,stroke-width:2px,color:#fff\n");

        let mut node_id = 0;
        let mut stack = self.nodes.iter().map(|n| (0, n)).collect::<Vec<_>>();
        while let Some((parent_node_id, node)) = stack.pop() {
            node_id += 1;

            let (contents, style) = Self::format_mermaid_node(node, node_id);
            result.push_str(&format!("    {}[\"{}\"]\n", node_id, contents));
            result.push_str(&format!("    style {} {}\n", node_id, style));
            result.push_str(&format!("    {} --> {}\n", parent_node_id, node_id));

            // Push children onto stack
            match node {
                AstNode::Model {
                    payload: Model {
                        decorators, nested_models, fields, ..
                    },
                    ..
                } => {
                    stack.extend(nested_models.iter().map(|m| (node_id, m)));
                    stack.extend(fields.iter().map(|f| (node_id, f)));
                    stack.extend(decorators.iter().map(|d| (node_id, d)));
                }
                AstNode::Endpoint { payload: Endpoint { fields, .. }, .. } => {
                    stack.extend(fields.iter().map(|f| (node_id, f)));
                }
                AstNode::Field { payload: Field { ty, .. }, .. } => {
                    stack.push((node_id, ty));
                }
                AstNode::Type { payload: Type::AnonModel(model), .. } => {
                    stack.push((node_id, model));
                }
                _ => {}
            }
        }
        result
    }

    // Returns contents and style
    fn format_mermaid_node(node: &AstNode, node_id: i64) -> (String, String) {
        let mut contents = String::new();
        let node_title = match node {
            AstNode::Model { .. } => "Model",
            AstNode::Endpoint { .. } => "Endpoint",
            AstNode::Decorator { .. } => "Decorator",
            AstNode::Field { .. } => "Field",
            AstNode::Type { .. } => "Type",
        };
        contents.push_str(node_title);
        let subtitle = match node {
            AstNode::Model { payload: Model { is_anon, .. }, .. } if *is_anon => "(anon)".to_string(),
            AstNode::Model { payload, .. } => payload.name.to_string(),
            AstNode::Endpoint { payload, .. } => payload.name.to_string(),
            AstNode::Decorator { payload, .. } => format!("@{}", payload.name),
            AstNode::Field { payload, .. } => payload.name.to_string(),
            AstNode::Type { payload, .. } => match payload {
                Type::String => "String".to_string(),
                Type::Int => "Int".to_string(),
                Type::Ref(name) => format!("ref({})", name),
                Type::AnonModel(_) => "(anon)".to_string(),
            },
        };
        contents.push_str(&format!("<br/><span style=\"font-size:12px\"><i>{}</i></span>", subtitle));
        let span = node.span();
        let start_pos = span.start_pos().line_col();
        let end_pos = span.end_pos().line_col();
        contents.push_str(&format!("<br/><span style=\"font-size:10px\">({},{} → {},{})</span>", start_pos.0, start_pos.1, end_pos.0, end_pos.1));

        let mut style = String::new();
        let color = match node {
            AstNode::Model { .. } => "#1a4d7a",
            AstNode::Endpoint { .. } => "#cc5200",
            AstNode::Decorator { .. } => "#1a6b1a",
            AstNode::Field { .. } => "#8b1a1a",
            AstNode::Type { .. } => "#5a3d7a",
        };
        style.push_str(&format!("fill:{},stroke:#333,stroke-width:2px,color:#fff", color));

        // Escape contents
        let contents = contents.replace('\n', "\\n").replace('"', "'");

        (contents, style)
    }
}
