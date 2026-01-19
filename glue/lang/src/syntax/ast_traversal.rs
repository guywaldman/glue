#![allow(unused_variables)]

use crate::{AstNode, Endpoint, Enum, Field, LNode, Model, RootNode};

/// A trait for visiting AST nodes.
pub trait AstVisitor {
    fn visit_endpoint(&mut self, endpoint: &Endpoint, parent: &impl AstNode) {}
    fn visit_model(&mut self, node: &Model, parent: &impl AstNode) {}
    fn visit_enum(&mut self, node: &Enum, parent: &impl AstNode) {}
    fn visit_field(&mut self, node: &Field, parent: &impl AstNode) {}

    fn traverse(&mut self, root_node: LNode) {
        let root = RootNode::cast(root_node).expect("Expected root node");

        for endpoint in root.top_level_endpoints() {
            for nested_model in endpoint.nested_models() {
                self.visit_model(&nested_model, &endpoint);
            }
            for nested_enum in endpoint.nested_enums() {
                self.visit_enum(&nested_enum, &endpoint);
            }
            for field in endpoint.fields() {
                self.visit_field(&field, &endpoint);
            }
            self.visit_endpoint(&endpoint, &root);
        }

        for model in root.top_level_models() {
            for nested_model in model.nested_models() {
                self.visit_model(&nested_model, &model);
            }
            for nested_enum in model.nested_enums() {
                self.visit_enum(&nested_enum, &model);
            }
            for field in model.fields() {
                self.visit_field(&field, &model);
            }
            self.visit_model(&model, &root);
        }
    }
}
