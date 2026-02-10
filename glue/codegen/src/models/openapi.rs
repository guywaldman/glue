use serde::{Deserialize, Serialize};
use std::collections::HashMap;

/// Root OpenAPI specification document.
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Default)]
pub struct OpenAPI {
    /// OpenAPI specification version (e.g., "3.0.0").
    pub openapi: String,
    /// Metadata about the API.
    pub info: Info,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub servers: Option<Vec<Server>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub paths: Option<HashMap<String, PathItem>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub components: Option<Components>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub security: Option<Vec<SecurityRequirement>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub tags: Option<Vec<Tag>>,
}

/// General information about the API.
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Default)]
pub struct Info {
    pub title: String,
    pub version: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub description: Option<String>,
}

/// A server where the API is hosted.
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct Server {
    /// URL to the target host.
    pub url: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub description: Option<String>,
}

/// Describes operations available on a path.
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Default)]
pub struct PathItem {
    #[serde(skip_serializing_if = "Option::is_none")]
    #[serde(rename = "$ref")]
    pub reference: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub summary: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub description: Option<String>,
    /// Operations for this path, keyed by HTTP method (e.g., "get", "post").
    #[serde(flatten)]
    pub operations: HashMap<String, Operation>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub parameters: Option<Vec<SchemaOrReference<Parameter>>>,
}

/// A single API operation.
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Default)]
pub struct Operation {
    #[serde(skip_serializing_if = "Option::is_none")]
    pub summary: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub description: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    #[serde(rename = "operationId")]
    pub operation_id: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub parameters: Option<Vec<SchemaOrReference<Parameter>>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    #[serde(rename = "requestBody")]
    pub request_body: Option<SchemaOrReference<RequestBody>>,
    pub responses: Responses,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub tags: Option<Vec<String>>,
}

/// A reference to a component or an inline item.
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
#[serde(untagged)]
pub enum SchemaOrReference<T = Schema> {
    Reference {
        #[serde(rename = "$ref")]
        reference: String,
    },
    Item(T),
}

/// Describes a single operation parameter.
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct Parameter {
    pub name: String,
    /// Where the parameter is located: "query", "header", "path", or "cookie".
    #[serde(rename = "in")]
    pub location: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub description: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub required: Option<bool>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub schema: Option<SchemaOrReference<Schema>>,
}

/// Describes a request body.
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct RequestBody {
    #[serde(skip_serializing_if = "Option::is_none")]
    pub description: Option<String>,
    /// Media types for the request body (e.g., "application/json").
    pub content: HashMap<String, MediaType>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub required: Option<bool>,
}

/// Media type and schema for content.
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Default)]
pub struct MediaType {
    #[serde(skip_serializing_if = "Option::is_none")]
    pub schema: Option<SchemaOrReference<Schema>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub example: Option<serde_json::Value>,
}

/// Container for operation responses.
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Default)]
pub struct Responses {
    /// Status codes mapped to responses (e.g., "200", "404", "default").
    #[serde(flatten)]
    pub responses: HashMap<String, SchemaOrReference<Response>>,
}

/// Describes a single response from an operation.
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Default)]
pub struct Response {
    #[serde(skip_serializing_if = "Option::is_none")]
    pub description: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub content: Option<HashMap<String, MediaType>>,
}

/// JSON Schema definition.
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Default)]
pub struct Schema {
    #[serde(skip_serializing_if = "Option::is_none")]
    #[serde(rename = "type")]
    pub schema_type: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub format: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub description: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub example: Option<serde_json::Value>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub nullable: Option<bool>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub required: Option<Vec<String>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub properties: Option<HashMap<String, SchemaOrReference<Schema>>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub items: Option<Box<SchemaOrReference<Schema>>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    #[serde(rename = "additionalProperties")]
    pub additional_properties: Option<Box<SchemaOrReference<Schema>>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    #[serde(rename = "enum")]
    pub enum_values: Option<Vec<serde_json::Value>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    #[serde(rename = "allOf")]
    pub all_of: Option<Vec<SchemaOrReference<Schema>>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    #[serde(rename = "oneOf")]
    pub one_of: Option<Vec<SchemaOrReference<Schema>>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    #[serde(rename = "anyOf")]
    pub any_of: Option<Vec<SchemaOrReference<Schema>>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub default: Option<serde_json::Value>,
}

/// Reusable components for the API.
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Default)]
pub struct Components {
    #[serde(skip_serializing_if = "Option::is_none")]
    pub schemas: Option<HashMap<String, SchemaOrReference<Schema>>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub responses: Option<HashMap<String, SchemaOrReference<Response>>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub parameters: Option<HashMap<String, SchemaOrReference<Parameter>>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    #[serde(rename = "securitySchemes")]
    pub security_schemes: Option<HashMap<String, SchemaOrReference<SecurityScheme>>>,
}

/// Security scheme definition.
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
#[serde(tag = "type", rename_all = "camelCase")]
pub enum SecurityScheme {
    #[serde(rename = "apiKey")]
    ApiKey {
        name: String,
        #[serde(rename = "in")]
        location: String,
    },
    #[serde(rename = "http")]
    Http { scheme: String },
    #[serde(rename = "oauth2")]
    OAuth2 { flows: OAuthFlows },
}

/// OAuth2 flow configurations.
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct OAuthFlows {
    #[serde(skip_serializing_if = "Option::is_none")]
    #[serde(rename = "authorizationCode")]
    pub authorization_code: Option<OAuthFlow>,
    #[serde(skip_serializing_if = "Option::is_none")]
    #[serde(rename = "clientCredentials")]
    pub client_credentials: Option<OAuthFlow>,
}

/// A single OAuth2 flow.
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct OAuthFlow {
    #[serde(skip_serializing_if = "Option::is_none")]
    #[serde(rename = "authorizationUrl")]
    pub authorization_url: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    #[serde(rename = "tokenUrl")]
    pub token_url: Option<String>,
    pub scopes: HashMap<String, String>,
}

/// Maps security scheme names to scopes.
pub type SecurityRequirement = HashMap<String, Vec<String>>;

/// Metadata for grouping operations.
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct Tag {
    pub name: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub description: Option<String>,
}
