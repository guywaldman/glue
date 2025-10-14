use std::collections::HashMap;

use gluelang::{
    Ast, AstNode, AstNodeKind, AstNodePayload, ConstantValue, Enum, Field, Model, PrimitiveType, SemanticAnalysisArtifacts, SymbolTable, TreeNode, Type, TypeAtom, TypeVariant,
};

use crate::codegen::{CodeGenError, CodeGenerator, types::EmitResult};

pub struct OpenApiCodeGenerator {
    ast: Ast,
    symbols: SymbolTable,
}

struct Components {
    schemas: HashMap<String, json::JsonValue>,
    order: Vec<String>,
}

struct Schema {
    ty: String,
    format: Option<String>,
}

struct EndpointOperation {
    path: String,
    method: String,
    operation: json::JsonValue,
}

struct RequestArtifacts {
    body: Option<json::JsonValue>,
    parameters: Vec<json::JsonValue>,
    consumed_path_params: Vec<String>,
}

struct ModelInfo {
    fields: Vec<AstNode>,
}

#[derive(Clone, Default)]
struct ResponseOptions {
    mime: Option<String>,
}

const DEFAULT_CONTENT_TYPE: &str = "application/json";

impl Components {
    fn new() -> Self {
        Self {
            schemas: HashMap::new(),
            order: Vec::new(),
        }
    }

    fn merge(&mut self, other: Components) {
        for name in other.order {
            if let Some(schema) = other.schemas.get(&name) {
                self.insert(name.clone(), schema.clone());
            }
        }
    }

    fn insert(&mut self, name: String, schema: json::JsonValue) {
        let entry = self.schemas.entry(name.clone());
        if matches!(entry, std::collections::hash_map::Entry::Vacant(_)) {
            self.order.push(name.clone());
        }
        entry.and_modify(|existing| *existing = schema.clone()).or_insert(schema);
    }
}

impl Schema {
    fn from_primitive(primitive: PrimitiveType) -> Self {
        match primitive {
            PrimitiveType::String => Self {
                ty: "string".to_string(),
                format: None,
            },
            PrimitiveType::Int => Self {
                ty: "integer".to_string(),
                format: Some("int64".to_string()),
            },
            PrimitiveType::Bool => Self {
                ty: "boolean".to_string(),
                format: None,
            },
        }
    }

    fn into_json(self) -> json::JsonValue {
        let mut value = json::JsonValue::new_object();
        value["type"] = self.ty.into();
        if let Some(format) = self.format {
            value["format"] = format.into();
        }
        value
    }
}

impl CodeGenerator for OpenApiCodeGenerator {
    fn generate(mut self) -> EmitResult {
        let mut result_obj = json::JsonValue::new_object();
        result_obj["openapi"] = "3.0.4".into();
        result_obj["info"] = json::object! {
                title: "Generated API",
                version: "1.0.0"
        };
        let mut paths = json::JsonValue::new_object();
        let mut components = Components::new();

        let top_level_nodes = self
            .ast
            .get_children(self.ast.get_root())
            .ok_or(CodeGenError::Other("AST root has no children".to_string()))?;

        for node in top_level_nodes {
            match node.payload {
                AstNodePayload::Endpoint { .. } => {
                    let endpoint_components = self.contribute_components_from_node(&node)?;
                    components.merge(endpoint_components);
                    let EndpointOperation { path, method, operation } = self.emit_endpoint(&node)?;

                    if !paths.has_key(&path) {
                        paths[&path] = json::JsonValue::new_object();
                    }

                    paths[&path][&method] = operation;
                }
                AstNodePayload::Model { .. } | AstNodePayload::Enum { .. } => {
                    let node_components = self.contribute_components_from_node(&node)?;
                    components.merge(node_components);
                }
                _ => {}
            }
        }

        if let Some(root_node) = self.ast.get_node(self.ast.get_root()) {
            let root_components = self.contribute_components_from_node(&root_node)?;
            components.merge(root_components);
        }

        result_obj["paths"] = paths;
        let mut schemas_json = json::JsonValue::new_object();
        let mut schema_names = components.order;
        schema_names.sort();
        for name in schema_names {
            if let Some(schema) = components.schemas.get(&name) {
                schemas_json[name.as_str()] = schema.clone();
            }
        }

        result_obj["components"] = json::object! {
                        schemas: schemas_json
        };

        Ok(json::stringify_pretty(result_obj, 4))
    }
}

impl OpenApiCodeGenerator {
    pub fn new(artifacts: SemanticAnalysisArtifacts) -> Self {
        Self {
            ast: artifacts.ast,
            symbols: artifacts.symbols,
        }
    }

    fn contribute_components_from_node(&mut self, node: &AstNode) -> Result<Components, CodeGenError> {
        let mut components = Components::new();
        self.collect_components_recursively(node, &mut components)?;
        Ok(components)
    }

    fn collect_components_recursively(&mut self, node: &AstNode, acc: &mut Components) -> Result<(), CodeGenError> {
        match node.payload() {
            AstNodePayload::Model(Model { name, .. }) if name != "<anonymous>" => {
                if !acc.schemas.contains_key(name) {
                    let schema = self.emit_model_schema(node)?;
                    acc.insert(name.clone(), schema);
                }
            }
            AstNodePayload::Enum(Enum { name, .. }) => {
                if !acc.schemas.contains_key(name) {
                    let schema = self.emit_enum_schema(node);
                    acc.insert(name.clone(), schema);
                }
            }
            _ => {}
        }

        if let Some(children) = self.ast.get_children(node.id()) {
            for child in children {
                self.collect_components_recursively(&child, acc)?;
            }
        }

        Ok(())
    }

    fn find_endpoint_field(&self, endpoint: &AstNode, field_name: &str) -> Option<AstNode> {
        self.ast
            .get_children(endpoint.id())
            .unwrap_or_default()
            .into_iter()
            .find(|child| matches!(child.payload(), AstNodePayload::Field(Field{ name, .. }) if name == field_name))
    }

    fn emit_endpoint(&mut self, endpoint: &AstNode) -> Result<EndpointOperation, CodeGenError> {
        let AstNodePayload::Endpoint {
            name,
            doc,
            method,
            path,
            path_params,
            request,
            headers,
            responses,
            ..
        } = endpoint.payload()
        else {
            return Err(CodeGenError::Other("Expected endpoint payload".to_string()));
        };

        let mut operation = json::JsonValue::new_object();
        operation["operationId"] = name.clone().into();
        operation["summary"] = name.clone().into();
        if let Some(doc) = doc {
            operation["description"] = doc.clone().into();
        }

        let mut parameters: Vec<json::JsonValue> = Vec::new();
        let mut consumed_path_params: Vec<String> = Vec::new();
        let mut request_body: Option<json::JsonValue> = None;

        if let Some(request_field_id) = request {
            let request_field = self
                .ast
                .get_node(*request_field_id)
                .ok_or_else(|| CodeGenError::Other("Request field node missing".to_string()))?;
            let artifacts = self.collect_request_artifacts(endpoint, &request_field, path_params)?;
            if let Some(body) = artifacts.body {
                request_body = Some(body);
            }
            parameters.extend(artifacts.parameters);
            consumed_path_params.extend(artifacts.consumed_path_params);
        }

        if !headers.is_empty() {
            let headers_field = self
                .find_endpoint_field(endpoint, "headers")
                .ok_or_else(|| CodeGenError::Other("Headers field node missing".to_string()))?;
            let mut header_parameters = self.emit_header_parameters(endpoint, &headers_field)?;
            parameters.append(&mut header_parameters);
        }

        for path_param in path_params.iter() {
            if !consumed_path_params.iter().any(|consumed| consumed == path_param) {
                let mut parameter = json::JsonValue::new_object();
                parameter["name"] = path_param.clone().into();
                parameter["in"] = "path".into();
                parameter["required"] = true.into();
                let mut schema = json::JsonValue::new_object();
                schema["type"] = "string".into();
                parameter["schema"] = schema;
                parameters.push(parameter);
            }
        }

        if !parameters.is_empty() {
            let mut parameters_json = json::JsonValue::new_array();
            for parameter in parameters {
                parameters_json
                    .push(parameter)
                    .map_err(|err| CodeGenError::Other(format!("failed to push parameter: {err}")))?;
            }
            operation["parameters"] = parameters_json;
        }

        if let Some(body) = request_body {
            operation["requestBody"] = body;
        }

        let responses_field = self.find_endpoint_field(endpoint, "responses");
        let responses_json = if !responses.is_empty() || responses_field.is_some() {
            let responses_field = responses_field.ok_or_else(|| CodeGenError::Other("Responses field node missing".to_string()))?;
            self.emit_responses(endpoint, &responses_field)?
        } else {
            let mut responses_obj = json::JsonValue::new_object();
            let mut default_resp = json::JsonValue::new_object();
            default_resp["description"] = "Success".into();
            responses_obj["default"] = default_resp;
            responses_obj
        };
        operation["responses"] = responses_json;

        Ok(EndpointOperation {
            path: path.clone(),
            method: method.to_lowercase(),
            operation,
        })
    }

    fn collect_request_artifacts(&mut self, endpoint: &AstNode, field: &AstNode, path_params: &[String]) -> Result<RequestArtifacts, CodeGenError> {
        let AstNodePayload::Field(Field { ty, doc, .. }) = field.payload() else {
            return Err(CodeGenError::Other("Expected request field payload".to_string()));
        };

        let schema = self.emit_type_schema(field, ty)?;

        let mut request_body = json::JsonValue::new_object();
        let mut content = json::JsonValue::new_object();
        let mut mime_obj = json::JsonValue::new_object();
        mime_obj["schema"] = schema;
        content[DEFAULT_CONTENT_TYPE] = mime_obj;
        request_body["content"] = content;
        if let Some(doc) = doc {
            request_body["description"] = doc.clone().into();
        }
        request_body["required"] = (!Self::is_type_optional(ty)).into();

        let mut parameters = Vec::new();
        let mut consumed_path_params = Vec::new();

        if let Some(model_info) = self.resolve_model_fields(endpoint, field, ty)? {
            for path_param in path_params {
                if let Some(field_node) = model_info.fields.iter().find(|f| {
                    if let AstNodePayload::Field(Field { name, .. }) = f.payload() {
                        name.as_str() == path_param.as_str()
                    } else {
                        false
                    }
                }) {
                    let (param_schema, _) = self.emit_field_schema(field_node)?;
                    let mut parameter = json::JsonValue::new_object();
                    parameter["name"] = path_param.clone().into();
                    parameter["in"] = "path".into();
                    parameter["required"] = true.into();
                    if let AstNodePayload::Field(Field { doc: Some(field_doc), .. }) = field_node.payload() {
                        parameter["description"] = field_doc.clone().into();
                    }
                    parameter["schema"] = param_schema;
                    parameters.push(parameter);
                    consumed_path_params.push(path_param.clone());
                }
            }
        }

        Ok(RequestArtifacts {
            body: Some(request_body),
            parameters,
            consumed_path_params,
        })
    }

    fn emit_header_parameters(&mut self, endpoint: &AstNode, field: &AstNode) -> Result<Vec<json::JsonValue>, CodeGenError> {
        let Some(Field { ty, .. }) = field.as_field() else {
            return Err(CodeGenError::Other("Expected headers field payload".to_string()));
        };

        let mut parameters = Vec::new();

        if let Some(model_info) = self.resolve_model_fields(endpoint, field, ty)? {
            for header_field in model_info.fields {
                if let Some(Field { name, doc, .. }) = header_field.as_field() {
                    let (schema, is_required) = self.emit_field_schema(&header_field)?;
                    let mut parameter = json::JsonValue::new_object();
                    parameter["name"] = name.clone().into();
                    parameter["in"] = "header".into();
                    parameter["required"] = is_required.into();
                    parameter["schema"] = schema;
                    if let Some(doc) = doc {
                        parameter["description"] = doc.clone().into();
                    }
                    parameters.push(parameter);
                }
            }
        }

        Ok(parameters)
    }

    fn emit_responses(&mut self, endpoint: &AstNode, responses_field: &AstNode) -> Result<json::JsonValue, CodeGenError> {
        let Some(Field { ty, .. }) = responses_field.as_field() else {
            return Err(CodeGenError::Other("Expected responses field payload".to_string()));
        };

        let shared_options = self.extract_response_options(responses_field)?;

        let Some(model_info) = self.resolve_model_fields(endpoint, responses_field, ty)? else {
            return Err(CodeGenError::Other("Responses field must be an object or reference to a model".to_string()));
        };

        let mut responses_obj = json::JsonValue::new_object();

        let response_fields = model_info.fields;

        for response_field in response_fields {
            let Some(Field { name: status_code, doc, ty, .. }) = response_field.as_field() else {
                continue;
            };

            let response_options = self.extract_response_options(&response_field)?.or_else(|| shared_options.clone());
            let mime = response_options.and_then(|opts| opts.mime).unwrap_or_else(|| DEFAULT_CONTENT_TYPE.to_string());

            let schema = self.emit_type_schema(&response_field, ty)?;

            let mut response_obj = json::JsonValue::new_object();
            if let Some(doc) = doc {
                response_obj["description"] = doc.clone().into();
            } else {
                response_obj["description"] = format!("{status_code} response").into();
            }

            let mut content = json::JsonValue::new_object();
            let mut mime_obj = json::JsonValue::new_object();
            mime_obj["schema"] = schema;
            content[&mime] = mime_obj;
            response_obj["content"] = content;

            responses_obj[status_code] = response_obj;
        }

        if responses_obj.is_empty() {
            return Err(CodeGenError::Other("Responses section must contain at least one response".to_string()));
        }

        Ok(responses_obj)
    }

    fn resolve_model_fields(&mut self, scope: &AstNode, field: &AstNode, ty: &Type) -> Result<Option<ModelInfo>, CodeGenError> {
        match ty {
            Type::Single(atom) => self.resolve_model_fields_from_atom(scope, field, atom),
            Type::Union(_) => Ok(None),
        }
    }

    fn resolve_model_fields_from_atom(&mut self, scope: &AstNode, field: &AstNode, atom: &TypeAtom) -> Result<Option<ModelInfo>, CodeGenError> {
        if atom.is_array {
            return Ok(None);
        }

        match &atom.variant {
            TypeVariant::AnonymousModel => {
                let type_node = self
                    .find_type_node(field)
                    .ok_or_else(|| CodeGenError::Other("Anonymous model type node missing".to_string()))?;
                let model_node = self
                    .ast
                    .get_children_fn(type_node.id(), |n| matches!(n.kind(), AstNodeKind::Model))
                    .and_then(|nodes| nodes.into_iter().find(|n| matches!(n.payload(), AstNodePayload::Model { .. })))
                    .ok_or_else(|| CodeGenError::Other("Anonymous model node missing".to_string()))?;
                let fields = self
                    .ast
                    .get_children(model_node.id())
                    .unwrap_or_default()
                    .into_iter()
                    .filter(|n| matches!(n.payload(), AstNodePayload::Field { .. }))
                    .collect::<Vec<_>>();
                Ok(Some(ModelInfo { fields }))
            }
            TypeVariant::Ref { name, .. } => {
                if let Some(model_node) = self.symbols.lookup(&self.ast, scope.id, name) {
                    let fields = self
                        .ast
                        .get_children(model_node.id)
                        .unwrap_or_default()
                        .into_iter()
                        .filter(|n| matches!(n.payload(), AstNodePayload::Field { .. }))
                        .collect::<Vec<_>>();
                    Ok(Some(ModelInfo { fields }))
                } else {
                    Ok(None)
                }
            }
            TypeVariant::Primitive(_) => Ok(None),
        }
    }

    fn extract_response_options(&self, node: &AstNode) -> Result<Option<ResponseOptions>, CodeGenError> {
        let decorators = self.ast.get_children_fn(node.id(), |n| matches!(n.kind(), AstNodeKind::Decorator)).unwrap_or_default();

        for decorator in decorators {
            let AstNodePayload::Decorator {
                name,
                named_args,
                positional_args,
            } = decorator.payload()
            else {
                continue;
            };
            if name != "response" {
                continue;
            }

            let mut options = ResponseOptions::default();

            if let Some(value) = named_args.get("mime") {
                options.mime = Some(Self::constant_to_string(value, "mime")?);
            } else if let Some(ConstantValue::String(value)) = positional_args.first() {
                options.mime = Some(value.clone());
            }

            return Ok(Some(options));
        }

        Ok(None)
    }

    fn find_type_node(&self, field: &AstNode) -> Option<AstNode> {
        self.ast
            .get_children_fn(field.id(), |n| matches!(n.kind(), AstNodeKind::Type))
            .and_then(|nodes| nodes.into_iter().next())
    }

    fn emit_model_schema(&mut self, model_node: &AstNode) -> Result<json::JsonValue, CodeGenError> {
        let Some(Model { doc, .. }) = model_node.as_model() else {
            return Err(CodeGenError::Other("Expected model payload".to_string()));
        };

        let mut schema = json::JsonValue::new_object();
        schema["type"] = "object".into();
        schema["additionalProperties"] = false.into();

        if let Some(doc) = doc {
            schema["description"] = doc.clone().into();
        }

        let mut properties = json::JsonValue::new_object();
        let mut required = json::JsonValue::new_array();

        let children = self.ast.get_children(model_node.id()).unwrap_or_default();
        for child in children {
            if let AstNodePayload::Field(Field { name, .. }) = child.payload() {
                let (field_schema, is_required) = self.emit_field_schema(&child)?;
                properties[name] = field_schema;
                if is_required {
                    required
                        .push(name.clone())
                        .map_err(|err| CodeGenError::Other(format!("failed to push required field '{name}': {err}")))?;
                }
            }
        }

        if !properties.is_empty() {
            schema["properties"] = properties;
        }
        if !required.is_empty() {
            schema["required"] = required;
        }

        Ok(schema)
    }

    fn emit_enum_schema(&self, enum_node: &AstNode) -> json::JsonValue {
        let mut schema = json::JsonValue::new_object();
        schema["type"] = "string".into();

        if let AstNodePayload::Enum(Enum { variants, doc, .. }) = enum_node.payload() {
            let mut values = json::JsonValue::new_array();
            for variant in variants {
                // json::JsonValue::push only fails for non-arrays; this is an array by construction.
                let _ = values.push(variant.clone());
            }
            schema["enum"] = values;
            if let Some(doc) = doc {
                schema["description"] = doc.clone().into();
            }
        }

        schema
    }

    fn emit_field_schema(&mut self, field: &AstNode) -> Result<(json::JsonValue, bool), CodeGenError> {
        let AstNodePayload::Field(Field { ty, doc, default, .. }) = field.payload() else {
            return Err(CodeGenError::Other("Expected field payload".to_string()));
        };

        let mut schema = self.emit_type_schema(field, ty)?;

        if let Some(doc) = doc {
            schema["description"] = doc.clone().into();
        }

        if let Some(default_value) = default {
            schema["default"] = Self::constant_to_json(default_value);
        }

        let is_required = !Self::is_type_optional(ty);
        Ok((schema, is_required))
    }

    fn emit_type_schema(&mut self, field: &AstNode, ty: &Type) -> Result<json::JsonValue, CodeGenError> {
        let type_nodes = self.ast.get_children_fn(field.id(), |n| matches!(n.kind(), AstNodeKind::Type)).unwrap_or_default();
        let type_node = type_nodes.first();

        match ty {
            Type::Single(atom) => self.emit_type_atom_schema(field, type_node, atom),
            Type::Union(atoms) => {
                let mut variants = json::JsonValue::new_array();
                for atom in atoms {
                    let schema = self.emit_type_atom_schema(field, type_node, atom)?;
                    variants
                        .push(schema)
                        .map_err(|err| CodeGenError::Other(format!("failed to push union variant schema: {err}")))?;
                }
                let mut wrapper = json::JsonValue::new_object();
                wrapper["oneOf"] = variants;
                if atoms.iter().any(|atom| atom.is_optional) {
                    wrapper["nullable"] = true.into();
                }
                Ok(wrapper)
            }
        }
    }

    fn emit_type_atom_schema(&mut self, field: &AstNode, type_node: Option<&AstNode>, atom: &TypeAtom) -> Result<json::JsonValue, CodeGenError> {
        let mut schema = match &atom.variant {
            TypeVariant::Primitive(primitive) => Schema::from_primitive(*primitive).into_json(),
            TypeVariant::Ref { name, effective_name } => {
                // Ensure referenced schema is collected
                let Some(ref_node) = self.symbols.lookup(&self.ast, field.id(), name) else {
                    return Err(CodeGenError::Other(format!("Referenced type '{name}' not found in scope")));
                };
                let ref_node_id = ref_node.id;
                let Some(ref_node) = self.ast.get_node(ref_node_id) else {
                    return Err(CodeGenError::Other(format!("Referenced type node with id '{ref_node_id}' not found")));
                };
                let _ = self.contribute_components_from_node(&ref_node);
                let mut ref_schema = json::JsonValue::new_object();
                ref_schema["$ref"] = format!("#/components/schemas/{effective_name}").into();
                ref_schema
            }
            TypeVariant::AnonymousModel => {
                let Some(type_node) = type_node else {
                    return Err(CodeGenError::Other("Anonymous model type node missing".to_string()));
                };
                let Some(model_node) = self
                    .ast
                    .get_children_fn(type_node.id(), |n| matches!(n.kind(), AstNodeKind::Model))
                    .and_then(|nodes| nodes.into_iter().find(|n| matches!(n.payload(), AstNodePayload::Model { .. })))
                else {
                    return Err(CodeGenError::Other("Anonymous model node missing".to_string()));
                };
                self.emit_model_schema(&model_node)?
            }
        };

        if atom.is_array {
            let mut array_schema = json::JsonValue::new_object();
            array_schema["type"] = "array".into();
            array_schema["items"] = schema;
            schema = array_schema;
        }

        if atom.is_optional {
            schema["nullable"] = true.into();
        }

        Ok(schema)
    }

    fn is_type_optional(ty: &Type) -> bool {
        match ty {
            Type::Single(atom) => atom.is_optional,
            Type::Union(atoms) => atoms.iter().any(|atom| atom.is_optional),
        }
    }

    fn constant_to_json(value: &ConstantValue) -> json::JsonValue {
        match value {
            ConstantValue::String(s) => s.clone().into(),
            ConstantValue::Int(i) => (*i).into(),
            ConstantValue::IntRange(start, end) => format!("{start}..{end}").into(),
            ConstantValue::Bool(b) => (*b).into(),
        }
    }

    fn constant_to_string(value: &ConstantValue, label: &str) -> Result<String, CodeGenError> {
        match value {
            ConstantValue::String(s) => Ok(s.clone()),
            _ => Err(CodeGenError::Other(format!("Expected string literal for '{label}' argument in @response decorator"))),
        }
    }
}
