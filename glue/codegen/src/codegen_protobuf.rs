use std::collections::HashSet;

use config::GlueConfigSchemaGeneration;
use convert_case::Case;
use lang::{AnonModel, AstNode, Enum, Field, GlueIr, LSyntaxKind, Literal, Rpc, Service, SourceCodeMetadata, SymId, Type, TypeAtom};

use crate::{
    CodeGenError, CodeGenerator,
    codegen::CodeGenResult,
    context::{AnonymousTypeNamer, CodeGenContext, EnumVariantExt, FieldExt, NamedExt, TypeMapper, convert_generated_identifier_case},
};

#[derive(Default)]
pub struct CodeGenProtobuf;

impl CodeGenerator for CodeGenProtobuf {
    fn generate(&self, ir: GlueIr, source: &SourceCodeMetadata, config: Option<GlueConfigSchemaGeneration>) -> Result<String, CodeGenError> {
        let program = ir
            .into_analyzed_program()
            .ok_or_else(|| CodeGenError::InternalError("Glue IR does not contain an analyzed program".to_string()))?;
        let protobuf_config = config.as_ref().and_then(|c| c.protobuf.clone()).unwrap_or_default();
        let package_name = protobuf_config.package_name.as_deref().unwrap_or("glue");
        let ctx = CodeGenContext::new(program.ast_root.clone(), program.symbols, source, config.as_ref());
        let mut generator = ProtobufGenerator::new(ctx, package_name.to_string());
        generator.generate()
    }
}

struct ProtobufGenerator<'a> {
    ctx: CodeGenContext<'a>,
    output: String,
    anon_namer: AnonymousTypeNamer,
    pending_anon_models: Vec<AnonymousModelDef>,
    emitted_anon_model_count: usize,
}

#[derive(Clone)]
struct AnonymousModelDef {
    name: String,
    model: AnonModel,
    scope: Option<SymId>,
    path: Vec<String>,
}

impl<'a> ProtobufGenerator<'a> {
    fn new(ctx: CodeGenContext<'a>, package_name: String) -> Self {
        let anon_namer = AnonymousTypeNamer::new(&ctx, Case::Pascal);
        Self {
            ctx,
            output: format!("syntax = \"proto3\";\n\npackage {};\n\n", package_name),
            anon_namer,
            pending_anon_models: Vec::new(),
            emitted_anon_model_count: 0,
        }
    }

    fn generate(&mut self) -> CodeGenResult<String> {
        for model in self.ctx.top_level_models().collect::<Vec<_>>() {
            let name = model.name()?;
            let scope = model.scope_id(&self.ctx, None)?;
            let model_path = self.ctx.symbol_path(scope);
            let code = self.emit_message(&name, &model.fields(), Some(scope), &model_path)?;
            self.output.push_str(&code);
        }

        let mut enum_constant_names = HashSet::new();
        for enum_ in self.ctx.top_level_enums().collect::<Vec<_>>() {
            let code = self.emit_enum(&enum_, &mut enum_constant_names)?;
            self.output.push_str(&code);
        }

        self.emit_pending_anon_models()?;

        let mut service_blocks = Vec::new();
        for service in self.ctx.top_level_services().collect::<Vec<_>>() {
            service_blocks.push(self.emit_service(&service)?);
        }

        self.emit_pending_anon_models()?;

        for service_block in service_blocks {
            self.output.push_str(&service_block);
        }

        Ok(self.output.clone())
    }

    fn emit_enum(&self, enum_: &Enum, enum_constant_names: &mut HashSet<String>) -> CodeGenResult<String> {
        let name = enum_.name()?;
        let mut output = format!("enum {} {{\n", name);
        for (i, variant) in enum_.variants().iter().enumerate() {
            let value = variant.variant_value()?;
            let constant_name = Self::enum_constant_name(&value);
            if !Self::is_proto_identifier(&constant_name) {
                return Err(self
                    .ctx
                    .error(variant.syntax(), &format!("Protobuf enum value '{}' normalizes to invalid enum constant '{}'", value, constant_name)));
            }
            if !enum_constant_names.insert(constant_name.clone()) {
                return Err(self.ctx.error(
                    variant.syntax(),
                    &format!("Duplicate Protobuf enum constant '{}' in package scope after CONSTANT_CASE normalization", constant_name),
                ));
            }
            output.push_str(&format!("    {} = {};\n", constant_name, i));
        }
        output.push_str("}\n\n");
        Ok(output)
    }

    fn enum_constant_name(value: &str) -> String {
        convert_generated_identifier_case(value, Case::UpperSnake)
    }

    fn is_proto_identifier(value: &str) -> bool {
        let mut chars = value.chars();
        let Some(first) = chars.next() else {
            return false;
        };
        (first == '_' || first.is_ascii_alphabetic()) && chars.all(|ch| ch == '_' || ch.is_ascii_alphanumeric())
    }

    fn emit_message(&mut self, name: &str, fields: &[Field], scope: Option<SymId>, path: &[String]) -> CodeGenResult<String> {
        let mut output = format!("message {} {{\n", name);
        let field_tags = self.field_tags(name, fields)?;
        for (field, tag) in fields.iter().zip(field_tags) {
            let field_name = field.name()?;
            let field_ty = field.field_type()?;
            let mut field_path = path.to_vec();
            field_path.push(field_name.clone());
            let (proto_type, optional) = self.emit_field_type(field, &field_ty, scope, &field_path)?;
            let label = if optional { "optional " } else { "" };
            output.push_str(&format!("    {}{} {} = {};\n", label, proto_type, field_name, tag));
        }
        output.push_str("}\n\n");
        Ok(output)
    }

    fn field_tags(&self, message_name: &str, fields: &[Field]) -> CodeGenResult<Vec<i64>> {
        let explicit_tags = fields.iter().map(|field| self.field_proto_tag(field)).collect::<CodeGenResult<Vec<_>>>()?;
        let tagged_count = explicit_tags.iter().filter(|tag| tag.is_some()).count();
        if tagged_count == 0 {
            return Ok((1..=fields.len() as i64).collect());
        }
        if tagged_count != fields.len() {
            let field = fields.iter().zip(explicit_tags.iter()).find_map(|(field, tag)| tag.is_none().then_some(field)).unwrap();
            return Err(self.ctx.error(
                field.syntax(),
                &format!(
                    "Protobuf message '{}' mixes tagged and untagged fields; add @field(proto_tag=...) to every field or none of them",
                    message_name
                ),
            ));
        }

        let mut seen = HashSet::new();
        let mut tags = Vec::new();
        for (field, tag) in fields.iter().zip(explicit_tags.into_iter().flatten()) {
            Self::validate_proto_tag(field, tag, &self.ctx)?;
            if !seen.insert(tag) {
                return Err(self.ctx.error(field.syntax(), &format!("Duplicate Protobuf field tag {} in message '{}'", tag, message_name)));
            }
            tags.push(tag);
        }
        Ok(tags)
    }

    fn field_proto_tag(&self, field: &Field) -> CodeGenResult<Option<i64>> {
        let Some(arg) = field.extract_decorator_arg(lang::MODEL_FIELD_DECORATOR, &lang::MODEL_FIELD_DECORATOR_PROTO_TAG_ARG) else {
            return Ok(None);
        };
        match arg.literal() {
            Some(Literal::IntLiteral { value, .. }) => Ok(Some(value)),
            _ => Err(self.ctx.error(arg.syntax(), "Protobuf field tag must be an integer")),
        }
    }

    fn validate_proto_tag(field: &Field, tag: i64, ctx: &CodeGenContext) -> CodeGenResult<()> {
        if !(1..=536_870_911).contains(&tag) {
            return Err(ctx.error(field.syntax(), "Protobuf field tag must be between 1 and 536870911"));
        }
        if (19_000..=19_999).contains(&tag) {
            return Err(ctx.error(field.syntax(), "Protobuf field tags 19000 through 19999 are reserved"));
        }
        Ok(())
    }

    fn emit_field_type(&mut self, field: &Field, ty: &Type, scope: Option<SymId>, path: &[String]) -> CodeGenResult<(String, bool)> {
        let (proto_type, atom_optional) = self.emit_type_inner(ty, scope, path, true)?;
        let optional = (field.is_optional() || atom_optional) && !proto_type.starts_with("repeated ") && !proto_type.starts_with("map<");
        Ok((proto_type, optional))
    }

    fn emit_pending_anon_models(&mut self) -> CodeGenResult<()> {
        let mut index = self.emitted_anon_model_count;
        while index < self.pending_anon_models.len() {
            let def = self.pending_anon_models[index].clone();
            let code = self.emit_message(&def.name, &def.model.fields(), def.scope, &def.path)?;
            self.output.push_str(&code);
            index += 1;
            self.emitted_anon_model_count = index;
        }
        Ok(())
    }

    fn emit_service(&mut self, service: &Service) -> CodeGenResult<String> {
        let service_name = service.ident().ok_or_else(|| CodeGenContext::internal_error("Service missing identifier"))?;
        let service_scope = self
            .ctx
            .resolve_id(None, &service_name)
            .ok_or_else(|| CodeGenContext::internal_error(format!("Unresolved service: {}", service_name)))?;
        let mut output = format!("service {} {{\n", service_name);
        for rpc in service.rpcs() {
            output.push_str(&self.emit_rpc(&rpc, Some(service_scope))?);
        }
        output.push_str("}\n\n");
        Ok(output)
    }

    fn emit_rpc(&mut self, rpc: &Rpc, service_scope: Option<SymId>) -> CodeGenResult<String> {
        let rpc_name = rpc.ident().ok_or_else(|| CodeGenContext::internal_error("RPC missing identifier"))?;
        let rpc_scope = self
            .ctx
            .resolve_id(service_scope, &rpc_name)
            .ok_or_else(|| CodeGenContext::internal_error(format!("Unresolved rpc: {}", rpc_name)))?;
        let body_ty = rpc
            .body_field_node()
            .and_then(Field::cast)
            .and_then(|field| field.ty())
            .ok_or_else(|| self.ctx.error(rpc.syntax(), &format!("RPC '{}' is missing required field 'body'", rpc_name)))?;
        let returns_ty = rpc
            .returns_field_node()
            .and_then(Field::cast)
            .and_then(|field| field.ty())
            .ok_or_else(|| self.ctx.error(rpc.syntax(), &format!("RPC '{}' is missing required field 'returns'", rpc_name)))?;
        let body_path = vec![rpc_name.clone(), "body".to_string()];
        let returns_path = vec![rpc_name.clone(), "returns".to_string()];
        let body_type = self.emit_rpc_type(&body_ty, Some(rpc_scope), &body_path)?;
        let returns_type = self.emit_rpc_type(&returns_ty, Some(rpc_scope), &returns_path)?;
        Ok(format!("    rpc {} ({}) returns ({});\n", rpc_name, body_type, returns_type))
    }

    fn emit_rpc_type(&mut self, ty: &Type, scope: Option<SymId>, path: &[String]) -> CodeGenResult<String> {
        let atoms = ty.type_atoms();
        if atoms.len() != 1 {
            return Err(self.ctx.error(ty.syntax(), "Protobuf RPC body and returns must be a single message type"));
        }
        let atom = &atoms[0];
        if atom.is_optional() || atom.is_array() {
            return Err(self.ctx.error(atom.syntax(), "Protobuf RPC body and returns must be non-optional, non-repeated message types"));
        }
        if atom.as_primitive_type().is_some() || atom.as_record_type().is_some() {
            return Err(self.ctx.error(atom.syntax(), "Protobuf RPC body and returns must be message types"));
        }
        if let Some(ref_token) = atom.as_ref_token() {
            let ref_name = ref_token.text().to_string();
            if let Some(alias_type) = self.ctx.resolve_type_alias(scope, &ref_name)? {
                return self.emit_rpc_type(&alias_type, scope, path);
            }
            let Some(entry) = self.ctx.resolve(scope, &ref_name) else {
                return Err(self.ctx.error(atom.syntax(), &format!("Unresolved Protobuf RPC message type '{}'", ref_name)));
            };
            if entry.data.kind() != LSyntaxKind::MODEL {
                return Err(self.ctx.error(atom.syntax(), "Protobuf RPC body and returns must refer to model types"));
            }
            return Ok(ref_name);
        }
        if let Some(anon_model) = atom.anon_model() {
            let name = self.anon_namer.allocate(&self.ctx, path, Case::Pascal);
            self.pending_anon_models.push(AnonymousModelDef {
                name: name.clone(),
                model: anon_model,
                scope,
                path: path.to_vec(),
            });
            return Ok(name);
        }
        Err(self.ctx.error(atom.syntax(), "Protobuf RPC body and returns must be message types"))
    }

    fn emit_type(&mut self, ty: &Type, scope: Option<SymId>, path: &[String]) -> CodeGenResult<String> {
        let (proto_type, _) = self.emit_type_inner(ty, scope, path, false)?;
        Ok(proto_type)
    }

    fn emit_type_inner(&mut self, ty: &Type, scope: Option<SymId>, path: &[String], allow_optional: bool) -> CodeGenResult<(String, bool)> {
        let atoms = ty.type_atoms();
        let atom = atoms.first().ok_or_else(|| CodeGenContext::internal_error("Type should have at least one type atom"))?;

        if atom.is_optional() && !allow_optional {
            return Err(self.ctx.error(atom.syntax(), "Protobuf optional types are only supported for message fields"));
        }

        let mut optional = atom.is_optional();
        let base = if let Some(primitive) = atom.as_primitive_type() {
            TypeMapper::to_protobuf(primitive).to_string()
        } else if let Some(record) = atom.as_record_type() {
            let src = record.src_type_node().ok_or_else(|| CodeGenContext::internal_error("Record missing source type"))?;
            let dest = record.dest_type_node().ok_or_else(|| CodeGenContext::internal_error("Record missing destination type"))?;
            let src_type = Type::cast(src).ok_or_else(|| CodeGenContext::internal_error("Expected Type for record source"))?;
            let dest_type = Type::cast(dest).ok_or_else(|| CodeGenContext::internal_error("Expected Type for record destination"))?;
            let mut key_path = path.to_vec();
            key_path.push("Key".to_string());
            let mut value_path = path.to_vec();
            value_path.push("Value".to_string());
            format!("map<{}, {}>", self.emit_type(&src_type, scope, &key_path)?, self.emit_type(&dest_type, scope, &value_path)?)
        } else if let Some(ref_token) = atom.as_ref_token() {
            let ref_name = ref_token.text().to_string();
            if let Some(alias_type) = self.ctx.resolve_type_alias(scope, &ref_name)? {
                let (alias_type, alias_optional) = self.emit_type_inner(&alias_type, scope, path, allow_optional)?;
                optional |= alias_optional;
                alias_type
            } else {
                ref_token.to_string()
            }
        } else if let Some(anon_model) = atom.anon_model() {
            let name = self.anon_namer.allocate(&self.ctx, path, Case::Pascal);
            self.pending_anon_models.push(AnonymousModelDef {
                name: name.clone(),
                model: anon_model,
                scope,
                path: path.to_vec(),
            });
            name
        } else {
            return Err(self.ctx.error(atom.syntax(), "Unsupported type atom for protobuf"));
        };

        let proto_type = if atom.is_array() { format!("repeated {}", base) } else { base };
        Ok((proto_type, optional))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use indoc::indoc;
    use insta::assert_snapshot;
    use lang::{GlueIr, SourceCodeMetadata};

    use crate::{CodeGenerator, test_utils::analyze_test_glue_file};

    fn generate(src: &str) -> Result<String, CodeGenError> {
        let (program, source) = analyze_test_glue_file(src);
        let ir = GlueIr::from_analyzed(source.file_name, program);
        CodeGenProtobuf::default().generate(
            ir,
            &SourceCodeMetadata {
                file_name: source.file_name,
                file_contents: source.file_contents,
            },
            None,
        )
    }

    fn commented_source(src: &str) -> String {
        src.lines()
            .map(|line| if line.is_empty() { "//".to_string() } else { format!("// {}", line) })
            .collect::<Vec<_>>()
            .join("\n")
    }

    #[test]
    fn test_basic_endpoint() {
        let src = indoc! {r#"
            model User {
                id: int
                name: string
            }
        "#};
        let (program, source) = analyze_test_glue_file(src);
        let ir = GlueIr::from_analyzed(source.file_name, program);
        let codegen = CodeGenProtobuf::default();
        let result = codegen
            .generate(
                ir,
                &SourceCodeMetadata {
                    file_name: source.file_name,
                    file_contents: source.file_contents,
                },
                None,
            )
            .unwrap();

        let result_with_source = format!("// Original source code:\n// {}\n\n{}", src.replace("\n", "\n// "), result);

        assert_snapshot!(result_with_source);
    }

    #[test]
    fn test_anonymous_struct() {
        let src = indoc! {r#"
            model User {
                profile: {
                    bio: string
                    age: int
                }
            }
        "#};
        let (program, source) = analyze_test_glue_file(src);
        let ir = GlueIr::from_analyzed(source.file_name, program);
        let codegen = CodeGenProtobuf::default();
        let result = codegen
            .generate(
                ir,
                &SourceCodeMetadata {
                    file_name: source.file_name,
                    file_contents: source.file_contents,
                },
                None,
            )
            .unwrap();

        let result_with_source = format!("// Original source code:\n// {}\n\n{}", src.replace("\n", "\n// "), result);

        assert_snapshot!(result_with_source);
    }

    #[test]
    fn test_enum_constants_are_constant_case() {
        let src = indoc! {r#"
            enum Status: "active" | "HTML-mode" | "INVALID_REQUEST"
        "#};
        let result = generate(src).unwrap();
        assert!(result.contains("enum Status {"));
        assert!(result.contains("    ACTIVE = 0;"));
        assert!(result.contains("    HTML_MODE = 1;"));
        assert!(result.contains("    INVALID_REQUEST = 2;"));
    }

    #[test]
    fn test_enum_constant_collision_after_normalization_fails() {
        let src = indoc! {r#"
            enum Status: "foo-bar" | "foo_bar"
        "#};
        let err = generate(src).unwrap_err();
        match err {
            CodeGenError::GenerationError(report) => {
                assert!(report.to_string().contains("Duplicate Protobuf enum constant 'FOO_BAR'"));
            }
            other => panic!("unexpected error: {other:?}"),
        }
    }

    #[test]
    fn test_service_and_explicit_tags() {
        let src = indoc! {r#"
            model GetUserRequest {
                @field(proto_tag=1)
                user_id: int
            }

            model User {
                @field(proto_tag=1)
                user_id: int

                @field(proto_tag=2)
                name: string
            }

            service UserService {
                rpc GetUser {
                    body: GetUserRequest
                    returns: User
                }
            }
        "#};
        let result = generate(src).unwrap();
        let result_with_source = format!("// Original source code:\n{}\n\n{}", commented_source(src), result);

        assert_snapshot!(result_with_source);
    }

    #[test]
    fn test_proto_tag_all_or_none() {
        let src = indoc! {r#"
            model User {
                @field(proto_tag=1)
                id: int
                name: string
            }
        "#};
        let err = generate(src).unwrap_err();
        match err {
            CodeGenError::GenerationError(report) => {
                assert!(report.to_string().contains("mixes tagged and untagged fields"));
            }
            other => panic!("unexpected error: {other:?}"),
        }
    }

    #[test]
    fn test_proto_tag_duplicate_fails() {
        let src = indoc! {r#"
            model User {
                @field(proto_tag=1)
                id: int
                @field(proto_tag=1)
                name: string
            }
        "#};
        let err = generate(src).unwrap_err();
        match err {
            CodeGenError::GenerationError(report) => {
                assert!(report.to_string().contains("Duplicate Protobuf field tag 1"));
            }
            other => panic!("unexpected error: {other:?}"),
        }
    }

    #[test]
    fn test_proto_tag_invalid_fails() {
        let src = indoc! {r#"
            model User {
                @field(proto_tag=19000)
                id: int
            }
        "#};
        let err = generate(src).unwrap_err();
        match err {
            CodeGenError::GenerationError(report) => {
                assert!(report.to_string().contains("reserved"));
            }
            other => panic!("unexpected error: {other:?}"),
        }
    }

    #[test]
    fn test_proto_tag_untagged_legacy_source_order() {
        let src = indoc! {r#"
            model User {
                id: int
                name: string
            }
        "#};
        let result = generate(src).unwrap();
        assert!(result.contains("int32 id = 1;"));
        assert!(result.contains("string name = 2;"));
    }

    #[test]
    fn test_optional_fields() {
        let src = indoc! {r#"
            model Profile {
                nickname?: string
            }

            model User {
                @field(proto_tag=1)
                id: int

                @field(proto_tag=2)
                display_name?: string

                @field(proto_tag=3)
                profile: Profile?
            }
        "#};
        let result = generate(src).unwrap();
        assert!(result.contains("optional string nickname = 1;"));
        assert!(result.contains("optional string display_name = 2;"));
        assert!(result.contains("optional Profile profile = 3;"));
    }

    #[test]
    fn test_optional_repeated_field_emits_repeated() {
        let src = indoc! {r#"
            model User {
                tags?: string[]
            }
        "#};
        let result = generate(src).unwrap();
        assert!(result.contains("repeated string tags = 1;"));
        assert!(!result.contains("optional repeated"));
    }

    #[test]
    fn test_optional_map_field_emits_map() {
        let src = indoc! {r#"
            model User {
                metadata?: Record<string, string>
            }
        "#};
        let result = generate(src).unwrap();
        assert!(result.contains("map<string, string> metadata = 1;"));
        assert!(!result.contains("optional map"));
    }
}
