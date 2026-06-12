use miette::Report;

use crate::{ConstDef, ConstExpr, ConstExprType, ConstRef, DiagnosticContext, LNode, LSyntaxKind, Literal, LiteralExpr, SymId, SymTable, syntax::AstNode};

#[derive(Debug, Clone, PartialEq)]
pub enum ConstValue {
    String(String),
    Int(i64),
    Bool(bool),
    List(Vec<ConstValue>),
}

impl ConstValue {
    pub fn ty(&self) -> ConstExprType {
        match self {
            Self::String(_) => ConstExprType::String,
            Self::Int(_) => ConstExprType::Int,
            Self::Bool(_) => ConstExprType::Bool,
            Self::List(_) => ConstExprType::List(&ConstExprType::String),
        }
    }
}

pub struct ConstEvaluator<'a> {
    symbols: &'a SymTable<LNode>,
    diag: DiagnosticContext,
}

impl<'a> ConstEvaluator<'a> {
    pub fn new(symbols: &'a SymTable<LNode>, diag: DiagnosticContext) -> Self {
        Self { symbols, diag }
    }

    pub fn eval_const_def(&self, const_def: &ConstDef) -> Result<ConstValue, Report> {
        self.eval_const_def_in_scope(const_def, None)
    }

    pub fn eval_const_def_in_scope(&self, const_def: &ConstDef, scope: Option<SymId>) -> Result<ConstValue, Report> {
        let expr = const_def
            .expr()
            .ok_or_else(|| self.diag.error(const_def.syntax().text_range(), "Constant is missing a value expression"))?;
        self.eval_expr(&expr, scope)
    }

    pub fn eval_expr(&self, expr: &ConstExpr, scope: Option<SymId>) -> Result<ConstValue, Report> {
        let mut stack = Vec::new();
        self.eval_node(expr.syntax(), scope, &mut stack)
    }

    fn eval_node(&self, node: &LNode, scope: Option<SymId>, stack: &mut Vec<SymId>) -> Result<ConstValue, Report> {
        match node.kind() {
            LSyntaxKind::CONST_EXPR => {
                let child = node.children().find(|n| n.kind() == LSyntaxKind::CONST_ADD).ok_or_else(|| self.internal_expr_error(node))?;
                self.eval_node(&child, scope, stack)
            }
            LSyntaxKind::CONST_ADD => {
                let mut values = node.children().filter(|n| n.kind() == LSyntaxKind::CONST_MUL);
                let first = values.next().ok_or_else(|| self.internal_expr_error(node))?;
                let mut acc = self.eval_node(&first, scope, stack)?;
                for child in values {
                    let rhs = self.eval_node(&child, scope, stack)?;
                    acc = self.add_values(acc, rhs, node)?;
                }
                Ok(acc)
            }
            LSyntaxKind::CONST_MUL => {
                let mut values = node.children().filter(|n| n.kind() == LSyntaxKind::CONST_PRIMARY);
                let first = values.next().ok_or_else(|| self.internal_expr_error(node))?;
                let mut acc = self.eval_node(&first, scope, stack)?;
                for child in values {
                    let rhs = self.eval_node(&child, scope, stack)?;
                    acc = self.multiply_values(acc, rhs, node)?;
                }
                Ok(acc)
            }
            LSyntaxKind::CONST_PRIMARY => {
                if let Some(literal_node) = node.children().find(|n| n.kind() == LSyntaxKind::LITERAL) {
                    return self.eval_literal_node(&literal_node);
                }
                if let Some(ref_node) = node.children().find(|n| n.kind() == LSyntaxKind::CONST_REF) {
                    return self.eval_ref_node(&ref_node, scope, stack);
                }
                if let Some(expr_node) = node.children().find(|n| n.kind() == LSyntaxKind::CONST_EXPR) {
                    return self.eval_node(&expr_node, scope, stack);
                }
                Err(self.internal_expr_error(node))
            }
            LSyntaxKind::CONST_REF => self.eval_ref_node(node, scope, stack),
            LSyntaxKind::LITERAL => self.eval_literal_node(node),
            _ => Err(self.internal_expr_error(node)),
        }
    }

    fn eval_ref_node(&self, node: &LNode, scope: Option<SymId>, stack: &mut Vec<SymId>) -> Result<ConstValue, Report> {
        let const_ref = ConstRef::cast(node.clone()).ok_or_else(|| self.internal_expr_error(node))?;
        let path = const_ref.path();
        let name = const_ref.ident().ok_or_else(|| self.internal_expr_error(node))?;
        let sym_id = if path.len() > 1 {
            self.resolve_qualified_ref(node, scope, &path)?
        } else {
            self.symbols
                .resolve_id(scope, &name)
                .ok_or_else(|| self.diag.error(node.text_range(), &format!("Undefined constant reference '{}'", name)))?
        };
        let Some(sym) = self.symbols.get(sym_id) else {
            return Err(self.diag.error(node.text_range(), &format!("Undefined constant reference '{}'", name)));
        };
        if sym.data.kind() != LSyntaxKind::CONST_DEF {
            return Err(self.diag.error(node.text_range(), &format!("Reference '{}' is not a constant", name)));
        }
        if stack.contains(&sym_id) {
            let cycle = self.cycle_text(stack, sym_id);
            return Err(self.diag.error_with_help(
                node.text_range(),
                &format!("Circular constant reference detected: {}", cycle),
                "Break the cycle by making at least one constant a literal value.",
            ));
        }
        let const_def = ConstDef::cast(sym.data.clone()).ok_or_else(|| self.internal_expr_error(&sym.data))?;
        let expr = const_def.expr().ok_or_else(|| self.internal_expr_error(&sym.data))?;
        let const_scope = self.symbols.parent_scope_id(sym_id);
        stack.push(sym_id);
        let value = self.eval_node(expr.syntax(), const_scope, stack);
        stack.pop();
        value
    }

    fn resolve_qualified_ref(&self, node: &LNode, scope: Option<SymId>, path: &[String]) -> Result<SymId, Report> {
        let ref_text = path.join(".");
        let Some((const_name, model_path)) = path.split_last() else {
            return Err(self.internal_expr_error(node));
        };
        let Some((first_model, nested_models)) = model_path.split_first() else {
            return Err(self.internal_expr_error(node));
        };

        let mut model_id = self
            .symbols
            .resolve_id(scope, first_model)
            .ok_or_else(|| self.diag.error(node.text_range(), &format!("Undefined constant reference '{}'", ref_text)))?;
        self.ensure_model_ref(node, model_id, &ref_text)?;

        for model_name in nested_models {
            model_id = self
                .symbols
                .resolve_direct_child_id(model_id, model_name)
                .ok_or_else(|| self.diag.error(node.text_range(), &format!("Undefined constant reference '{}'", ref_text)))?;
            self.ensure_model_ref(node, model_id, &ref_text)?;
        }

        let const_id = self
            .symbols
            .resolve_direct_child_id(model_id, const_name)
            .ok_or_else(|| self.diag.error(node.text_range(), &format!("Undefined constant reference '{}'", ref_text)))?;
        let Some(const_sym) = self.symbols.get(const_id) else {
            return Err(self.diag.error(node.text_range(), &format!("Undefined constant reference '{}'", ref_text)));
        };
        if const_sym.data.kind() != LSyntaxKind::CONST_DEF {
            return Err(self.diag.error(node.text_range(), &format!("Reference '{}' is not a constant", ref_text)));
        }
        if const_name.starts_with('_') && !self.symbols.is_scope_within(scope, model_id) {
            return Err(self.diag.error(node.text_range(), &format!("Constant '{}' is private to model '{}'", const_name, model_path.join("."))));
        }

        Ok(const_id)
    }

    fn ensure_model_ref(&self, node: &LNode, sym_id: SymId, ref_text: &str) -> Result<(), Report> {
        let Some(sym) = self.symbols.get(sym_id) else {
            return Err(self.diag.error(node.text_range(), &format!("Undefined constant reference '{}'", ref_text)));
        };
        if sym.data.kind() != LSyntaxKind::MODEL {
            return Err(self.diag.error(node.text_range(), &format!("Reference '{}' does not start with a model", ref_text)));
        }
        Ok(())
    }

    fn eval_literal_node(&self, node: &LNode) -> Result<ConstValue, Report> {
        let literal = LiteralExpr::cast(node.clone()).and_then(|expr| expr.value()).ok_or_else(|| self.internal_expr_error(node))?;
        self.eval_literal(&literal, node)
    }

    fn eval_literal(&self, literal: &Literal, node: &LNode) -> Result<ConstValue, Report> {
        match literal {
            Literal::StringLiteral(value) => Ok(ConstValue::String(value.value().unwrap_or_default())),
            Literal::IntLiteral { value, .. } => Ok(ConstValue::Int(*value)),
            Literal::BoolLiteral { value, .. } => Ok(ConstValue::Bool(*value)),
            Literal::ListLiteral(values) => {
                let mut out = Vec::new();
                for value in values.values() {
                    out.push(self.eval_literal(&value, node)?);
                }
                Ok(ConstValue::List(out))
            }
            Literal::FloatLiteral { .. } => Err(self.diag.error(node.text_range(), "Float constants are not supported")),
        }
    }

    fn add_values(&self, left: ConstValue, right: ConstValue, node: &LNode) -> Result<ConstValue, Report> {
        match (left, right) {
            (ConstValue::Int(left), ConstValue::Int(right)) => left
                .checked_add(right)
                .map(ConstValue::Int)
                .ok_or_else(|| self.diag.error(node.text_range(), "Integer constant expression overflowed")),
            (ConstValue::String(left), ConstValue::String(right)) => Ok(ConstValue::String(format!("{}{}", left, right))),
            (left, right) => Err(self.diag.error(node.text_range(), &format!("Operator '+' cannot be applied to {} and {}", left.ty(), right.ty()))),
        }
    }

    fn multiply_values(&self, left: ConstValue, right: ConstValue, node: &LNode) -> Result<ConstValue, Report> {
        match (left, right) {
            (ConstValue::Int(left), ConstValue::Int(right)) => left
                .checked_mul(right)
                .map(ConstValue::Int)
                .ok_or_else(|| self.diag.error(node.text_range(), "Integer constant expression overflowed")),
            (left, right) => Err(self.diag.error(node.text_range(), &format!("Operator '*' cannot be applied to {} and {}", left.ty(), right.ty()))),
        }
    }

    fn cycle_text(&self, stack: &[SymId], repeat: SymId) -> String {
        let mut names = Vec::new();
        let mut seen_repeat = false;
        for id in stack {
            if *id == repeat {
                seen_repeat = true;
            }
            if seen_repeat && let Some(sym) = self.symbols.get(*id) {
                names.push(sym.name.clone());
            }
        }
        if let Some(sym) = self.symbols.get(repeat) {
            names.push(sym.name.clone());
        }
        names.join(" -> ")
    }

    fn internal_expr_error(&self, node: &LNode) -> Report {
        self.diag.error(node.text_range(), "Invalid constant expression")
    }
}

pub fn is_constant_case(name: &str) -> bool {
    let name = name.strip_prefix('_').unwrap_or(name);
    let mut chars = name.chars();
    let Some(first) = chars.next() else {
        return false;
    };
    first.is_ascii_uppercase() && chars.all(|ch| ch.is_ascii_uppercase() || ch.is_ascii_digit() || ch == '_')
}

pub fn to_constant_case(name: &str) -> String {
    let private = name.starts_with('_');
    let name = name.strip_prefix('_').unwrap_or(name);
    let mut out = String::new();
    let mut prev_was_sep = true;
    for ch in name.chars() {
        if ch.is_ascii_alphanumeric() {
            if ch.is_ascii_uppercase() && !prev_was_sep && out.chars().last().is_some_and(|prev| prev.is_ascii_lowercase()) {
                out.push('_');
            }
            out.push(ch.to_ascii_uppercase());
            prev_was_sep = false;
        } else if !prev_was_sep {
            out.push('_');
            prev_was_sep = true;
        }
    }
    while out.ends_with('_') {
        out.pop();
    }
    if private { format!("_{}", out) } else { out }
}
