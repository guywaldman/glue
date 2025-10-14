use std::collections::HashMap;

use crate::AstNodeId;

#[derive(Debug, Clone)]
pub struct Endpoint {
    pub name: String,
    pub doc: Option<String>,
    pub method: String,
    pub path: String,
    pub path_params: Vec<String>,
    /// Model node that represents the request body.
    pub request: Option<AstNodeId>,
    /// List of fields that represent the request header fields
    pub headers: HashMap<String, AstNodeId>,
    /// List of response status codes (e.g. "200", "404", "5XX").
    pub responses: HashMap<String, AstNodeId>,
}
