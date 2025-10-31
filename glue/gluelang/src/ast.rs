use pest::Span;

use crate::SourceCodeMetadata;

pub type AstNodeId = usize;

#[derive(Debug, Clone)]
pub struct DocBlock {
    pub content: String,
}

#[derive(Debug, Clone)]
pub struct Decorator<'a> {
    pub name: &'a str,
}

#[derive(Debug, Clone)]
pub struct Model<'a> {
    pub name: &'a str,
    pub ident: Option<AstNodeId>,
    pub is_anon: bool,
    pub decorators: Vec<AstNodeId>,
    pub nested_models: Vec<AstNodeId>,
    pub fields: Vec<AstNodeId>,
}

#[derive(Debug, Clone)]
pub struct Endpoint<'a> {
    pub name: &'a str,
    pub fields: Vec<AstNodeId>,
}

#[derive(Debug, Clone)]
pub enum RawTypeAtom<'a> {
    String,
    Int,
    Bool,
    Ref(&'a str),
    AnonModel(AstNodeId),
}

#[derive(Debug, Clone)]
pub struct TypeAtom<'a> {
    pub payload: RawTypeAtom<'a>,
    pub is_array: bool,
    pub is_optional: bool,
}

#[derive(Debug, Clone)]
pub struct Type {
    pub atoms: Vec<AstNodeId>,
}

#[derive(Debug, Clone)]
pub struct Field<'a> {
    pub doc: Option<DocBlock>,
    pub name: &'a str,
    pub type_atoms: Vec<AstNodeId>,
}

#[derive(Debug, Clone)]
pub struct Ident<'a> {
    pub name: &'a str,
}

#[derive(Debug, Clone, Copy)]
pub enum AstNodeKind {
    Root,
    Model,
    Endpoint,
    Decorator,
    Ident,
    Field,
    Type,
    TypeAtom,
}

#[derive(Clone)]
pub enum RawAstNode<'a> {
    Root(Vec<AstNodeId>),
    Model(Model<'a>),
    Endpoint(Endpoint<'a>),
    Decorator(Decorator<'a>),
    Field(Field<'a>),
    Ident(Ident<'a>),
    Type(Type),
    TypeAtom(TypeAtom<'a>),
}

#[derive(Debug, Clone)]
pub struct AstNode<'a> {
    pub payload: RawAstNode<'a>,
    pub span: Span<'a>,
    pub id: AstNodeId,
}

impl<'a> AstNode<'a> {
    pub fn kind(&self) -> AstNodeKind {
        match &self.payload {
            RawAstNode::Root(_) => AstNodeKind::Root,
            RawAstNode::Model(_) => AstNodeKind::Model,
            RawAstNode::Endpoint(_) => AstNodeKind::Endpoint,
            RawAstNode::Decorator(_) => AstNodeKind::Decorator,
            RawAstNode::Ident(_) => AstNodeKind::Ident,
            RawAstNode::Field(_) => AstNodeKind::Field,
            RawAstNode::Type(_) => AstNodeKind::Type,
            RawAstNode::TypeAtom(_) => AstNodeKind::TypeAtom,
        }
    }

    pub fn children_ids(&self) -> Vec<AstNodeId> {
        match &self.payload {
            RawAstNode::Root(children) => children.clone(),
            RawAstNode::Model(Model {
                ident,
                decorators,
                nested_models,
                fields,
                ..
            }) => {
                let mut result = Vec::new();
                if let Some(ident) = ident {
                    result.push(*ident);
                }
                result.extend(decorators.iter().copied());
                result.extend(nested_models.iter().copied());
                result.extend(fields.iter().copied());
                result
            }
            RawAstNode::Endpoint(Endpoint { fields, .. }) => fields.clone(),
            RawAstNode::Decorator(_) => Vec::new(),
            RawAstNode::Field(Field { type_atoms, .. }) => type_atoms.clone(),
            RawAstNode::Ident(_) => Vec::new(),
            RawAstNode::Type(Type { atoms, .. }) => atoms.clone(),
            RawAstNode::TypeAtom(_) => Vec::new(),
        }
    }

    pub fn as_model(&self) -> Option<&Model<'a>> {
        match &self.payload {
            RawAstNode::Model(model) => Some(model),
            _ => None,
        }
    }

    pub fn as_decorator(&self) -> Option<&Decorator<'a>> {
        match &self.payload {
            RawAstNode::Decorator(decorator) => Some(decorator),
            _ => None,
        }
    }

    pub fn as_field(&self) -> Option<&Field<'a>> {
        match &self.payload {
            RawAstNode::Field(field) => Some(field),
            _ => None,
        }
    }

    pub fn as_type(&self) -> Option<&Type> {
        match &self.payload {
            RawAstNode::Type(ty) => Some(ty),
            _ => None,
        }
    }

    pub fn as_type_atom(&self) -> Option<&TypeAtom<'a>> {
        match &self.payload {
            RawAstNode::TypeAtom(atom) => Some(atom),
            _ => None,
        }
    }
}

impl std::fmt::Debug for RawAstNode<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            RawAstNode::Root(children) => write!(f, "<root> (children: {:?})", children),
            RawAstNode::Model(payload) => write!(f, "<model> {{ name: {}, fields: {:?} }}", payload.name, payload.fields),
            RawAstNode::Endpoint(payload) => write!(f, "<endpoint> {{ name: {}, fields: {:?} }}", payload.name, payload.fields),
            RawAstNode::Decorator(payload) => write!(f, "<decorator> @{}", payload.name),
            RawAstNode::Field(payload) => write!(f, "<field> {}: {:?}", payload.name, payload.type_atoms),
            RawAstNode::Ident(payload) => write!(f, "<ident> {}", payload.name),
            RawAstNode::Type(payload) => write!(f, "<type> {:?}", payload),
            RawAstNode::TypeAtom(payload) => write!(f, "<type_atom> {:?}", payload),
        }
    }
}

#[derive(Debug, Clone)]
pub struct AstBuilder<'a> {
    nodes: Vec<AstNode<'a>>,
    top_level_nodes: Vec<AstNodeId>,
    source_code_metadata: &'a SourceCodeMetadata<'a>,
}

impl<'a> AstBuilder<'a> {
    pub fn new(source_code_metadata: &'a SourceCodeMetadata<'a>) -> Self {
        Self {
            nodes: Vec::new(),
            top_level_nodes: Vec::new(),
            source_code_metadata,
        }
    }

    fn next_id(&self) -> AstNodeId {
        // +1 to reserve id 0 for the synthetic root node
        self.nodes.len() + 1
    }

    pub fn add_node(&mut self, node: RawAstNode<'a>, span: Span<'a>) -> AstNodeId {
        let id = self.next_id();
        self.nodes.push(AstNode { payload: node, span, id });
        id
    }

    pub fn add_top_level_node(&mut self, node: RawAstNode<'a>, span: Span<'a>) -> AstNodeId {
        let id = self.add_node(node, span);
        self.top_level_nodes.push(id);
        id
    }

    pub fn build(self) -> Ast<'a> {
        let mut nodes = Vec::with_capacity(self.nodes.len() + 1);
        let root_span = Span::new(self.source_code_metadata.contents, 0, 0).unwrap();
        nodes.push(AstNode {
            payload: RawAstNode::Root(self.top_level_nodes.clone()),
            span: root_span,
            id: 0,
        });
        nodes.extend(self.nodes);
        Ast::new(self.source_code_metadata, nodes)
    }
}

#[derive(Debug, Clone)]
pub struct Ast<'a> {
    pub source_code_metadata: &'a SourceCodeMetadata<'a>,
    nodes: Vec<AstNode<'a>>,
    children: Vec<Vec<AstNodeId>>,
    parents: Vec<Option<AstNodeId>>,
}

impl<'a> Ast<'a> {
    fn new(source_code_metadata: &'a SourceCodeMetadata<'a>, nodes: Vec<AstNode<'a>>) -> Self {
        let mut children: Vec<Vec<AstNodeId>> = vec![Vec::new(); nodes.len()];
        for node in &nodes {
            children[node.id] = node.children_ids();
        }

        let mut parents = vec![None; nodes.len()];
        for node in &nodes {
            for child_id in &children[node.id] {
                if let Some(parent_slot) = parents.get_mut(*child_id) {
                    *parent_slot = Some(node.id);
                }
            }
        }

        Ast {
            source_code_metadata,
            nodes,
            children,
            parents,
        }
    }

    pub fn node(&self, node_id: AstNodeId) -> Option<&AstNode<'a>> {
        self.nodes.get(node_id)
    }

    pub fn nodes(&self) -> &[AstNode<'a>] {
        &self.nodes
    }

    pub fn resolve_node_ids(&self, ids: &[AstNodeId]) -> Vec<&AstNode<'a>> {
        ids.iter().filter_map(|id| self.node(*id)).collect()
    }

    pub fn children(&self, node_id: AstNodeId) -> Vec<&AstNode<'a>> {
        self.children_ids(node_id).iter().filter_map(|id| self.node(*id)).collect()
    }

    pub fn children_fn(&self, node_id: AstNodeId, predicate: impl Fn(&AstNode<'a>) -> bool) -> Vec<&AstNode<'a>> {
        self.children_ids(node_id).iter().filter_map(|id| self.node(*id)).filter(|node| predicate(node)).collect()
    }

    pub fn children_ids(&self, node_id: AstNodeId) -> &[AstNodeId] {
        self.children.get(node_id).map(Vec::as_slice).unwrap_or(&[])
    }

    pub fn top_level_nodes(&self) -> &[AstNodeId] {
        match &self.nodes.first() {
            Some(node) => match &node.payload {
                RawAstNode::Root(children) => children.as_slice(),
                _ => &[],
            },
            None => &[],
        }
    }

    pub fn dfs<F>(&self, start_node: AstNodeId, mut visit: F)
    where
        F: FnMut(&AstNode<'a>),
    {
        let mut visited = vec![false; self.nodes.len()];
        let mut stack = vec![start_node];

        while let Some(node_id) = stack.pop() {
            if node_id >= visited.len() || visited[node_id] {
                continue;
            }
            visited[node_id] = true;

            if let Some(node) = self.node(node_id) {
                visit(node);
                for &child in self.children_ids(node_id).iter().rev() {
                    stack.push(child);
                }
            }
        }
    }

    pub fn scope(&'a self, node_id: AstNodeId) -> Scopes<'a> {
        let mut scopes = Vec::new();
        let mut current = node_id;
        while let Some(parent_id) = self.parents.get(current).and_then(|p| *p) {
            let siblings = self.children[parent_id].clone();
            scopes.push((parent_id, siblings));
            current = parent_id;
        }
        Scopes::new(self, scopes)
    }
}

#[derive(Debug, Clone)]
pub struct Scopes<'a> {
    ast: &'a Ast<'a>,
    scopes: Vec<(AstNodeId, Vec<AstNodeId>)>,
}

impl<'a> Scopes<'a> {
    fn new(ast: &'a Ast<'a>, scopes: Vec<(AstNodeId, Vec<AstNodeId>)>) -> Self {
        Self { ast, scopes }
    }

    pub fn as_slice(&self) -> &[(AstNodeId, Vec<AstNodeId>)] {
        &self.scopes
    }

    pub fn iter(&self) -> impl Iterator<Item = (AstNodeId, Vec<AstNodeId>)> + '_ {
        self.scopes.iter().cloned()
    }

    pub fn flatten(&self) -> Vec<AstNodeId> {
        self.scopes.iter().flat_map(|(_, nodes)| nodes.iter().copied()).collect()
    }

    pub fn find(&self, predicate: impl Fn(&AstNode<'a>) -> bool) -> Option<AstNodeId> {
        for (_, nodes) in &self.scopes {
            for node_id in nodes {
                if let Some(node) = self.ast.node(*node_id) {
                    if predicate(node) {
                        return Some(*node_id);
                    }
                }
            }
        }
        None
    }
}

// Mermaid generation
impl<'a> Ast<'a> {
    pub fn to_mermaid(&self) -> String {
        let mut result = String::new();
        result.push_str("graph TD\n");

        self.dfs(0, |node| {
            let (contents, style) = self.format_mermaid_node(node);
            result.push_str(&format!("    {}[\"{}\"]\n", node.id, contents));
            result.push_str(&format!("    style {} {}\n", node.id, style));

            for child in self.children_ids(node.id) {
                result.push_str(&format!("    {} --> {}\n", node.id, child));
            }
        });

        result
    }

    fn format_mermaid_node(&self, node: &AstNode<'a>) -> (String, String) {
        let mut contents = String::new();
        let node_title = match node.kind() {
            AstNodeKind::Root => "Root",
            AstNodeKind::Model => "Model",
            AstNodeKind::Endpoint => "Endpoint",
            AstNodeKind::Decorator => "Decorator",
            AstNodeKind::Field => "Field",
            AstNodeKind::Ident => "Ident",
            AstNodeKind::Type => "Type",
            AstNodeKind::TypeAtom => "TypeAtom",
        };
        contents.push_str(&format!("<i>({})</i> ", node.id));
        contents.push_str(node_title);
        let subtitle = self.node_subtitle(node);
        contents.push_str(&format!("<br/><span style=\"font-size:12px\"><i>{}</i></span>", subtitle));
        let start_pos = node.span.start_pos().line_col();
        let end_pos = node.span.end_pos().line_col();
        contents.push_str(&format!("<br/><span style=\"font-size:10px\">({},{} → {},{})</span>", start_pos.0, start_pos.1, end_pos.0, end_pos.1));

        let color = match node.kind() {
            AstNodeKind::Root => "#0e0e0eff",
            AstNodeKind::Model => "#1a4d7a",
            AstNodeKind::Endpoint => "#cc5200",
            AstNodeKind::Decorator => "#1a6b1a",
            AstNodeKind::Ident => "#b36b00",
            AstNodeKind::Field => "#8b1a1a",
            AstNodeKind::Type => "#5a3d7a",
            AstNodeKind::TypeAtom => "rgba(46, 23, 34, 1)",
        };

        let mut style = String::new();
        style.push_str(&format!("fill:{},stroke:#333,stroke-width:2px,color:#fff", color));

        let contents = contents.replace('\n', "\\n").replace('"', "'");

        (contents, style)
    }

    fn node_subtitle(&self, node: &AstNode<'a>) -> String {
        match &node.payload {
            RawAstNode::Root(_) => "(root)".to_string(),
            RawAstNode::Model(Model { is_anon, .. }) if *is_anon => "(anon)".to_string(),
            RawAstNode::Model(Model { name, .. }) => (*name).to_string(),
            RawAstNode::Endpoint(Endpoint { name, .. }) => (*name).to_string(),
            RawAstNode::Decorator(Decorator { name, .. }) => format!("@{}", name),
            RawAstNode::Ident(Ident { name, .. }) => (*name).to_string(),
            RawAstNode::Field(Field { name, .. }) => (*name).to_string(),
            RawAstNode::Type(Type { atoms, .. }) => {
                if let Some(first_atom) = atoms.first()
                    && let Some(node) = self.node(*first_atom)
                    && let Some(atom) = node.as_type_atom()
                {
                    Self::format_type_atom(atom)
                } else {
                    "(type)".to_string()
                }
            }
            RawAstNode::TypeAtom(atom) => Self::format_type_atom(atom),
        }
    }

    fn format_type_atom(atom: &TypeAtom<'_>) -> String {
        let mut base = match &atom.payload {
            RawTypeAtom::String => "String".to_string(),
            RawTypeAtom::Int => "Int".to_string(),
            RawTypeAtom::Bool => "Bool".to_string(),
            RawTypeAtom::Ref(name) => format!("ref({})", name),
            RawTypeAtom::AnonModel(_) => "(anon)".to_string(),
        };
        if atom.is_array {
            base.push_str("[]");
        }
        if atom.is_optional {
            base.push('?');
        }
        base
    }
}
