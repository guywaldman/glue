use config::GlueConfigSchemaGeneration;
use lang::{GlueIr, Parser, ParserError, SemanticAnalyzer, SemanticAnalyzerError, SemanticAnalyzerOptions, SemanticWarning, SourceCodeMetadata};

use log::debug;
use thiserror::Error;

use crate::{
    codegen_go::CodeGenGo, codegen_jsonschema::CodeGenJsonSchema, codegen_openapi::CodeGenOpenAPI, codegen_protobuf::CodeGenProtobuf, codegen_python::CodeGenPython, codegen_rust::CodeGenRust,
    codegen_typescript::CodeGenTypeScript,
};

#[derive(Debug, Error)]
pub enum CodeGenError {
    #[error("Internal error: {0}")]
    InternalError(String),
    #[error("Parsing error")]
    ParserError(ParserError),
    #[error("Semantic analysis error")]
    SemanticAnalysisError(Vec<SemanticAnalyzerError>),
    #[error("Error during code generation")]
    GenerationError(miette::Report),
    #[error("Errors during code generation")]
    GenerationErrors(Vec<miette::Report>),
}

pub struct CodeGenOutput {
    pub code: String,
    pub warnings: Vec<SemanticWarning>,
}

pub trait CodeGenerator {
    fn generate(&self, ir: GlueIr, source: &SourceCodeMetadata, config: Option<GlueConfigSchemaGeneration>) -> Result<String, CodeGenError>;
}

pub type CodeGenResult<T> = Result<T, CodeGenError>;

#[derive(Debug, Clone, Copy, PartialEq, Eq, clap::ValueEnum)]
pub enum CodeGenMode {
    #[value(name = "jsonschema")]
    JsonSchema,
    #[value(name = "openapi")]
    OpenApi,
    #[value(name = "rust")]
    Rust,
    #[value(name = "python")]
    Python,
    #[value(name = "typescript")]
    TypeScript,
    #[value(name = "protobuf")]
    Protobuf,
    #[value(name = "go")]
    Go,
}

impl CodeGenMode {
    pub fn as_str(&self) -> &'static str {
        match self {
            CodeGenMode::JsonSchema => "jsonschema",
            CodeGenMode::OpenApi => "openapi",
            CodeGenMode::Rust => "rust",
            CodeGenMode::Python => "python",
            CodeGenMode::TypeScript => "typescript",
            CodeGenMode::Protobuf => "protobuf",
            CodeGenMode::Go => "go",
        }
    }

    pub fn comment_prefix(&self) -> &'static str {
        match self {
            CodeGenMode::Python => "#",
            _ => "//",
        }
    }

    pub fn file_extension(&self) -> &'static str {
        match self {
            CodeGenMode::JsonSchema => "json",
            CodeGenMode::OpenApi => "yaml",
            CodeGenMode::Rust => "rs",
            CodeGenMode::Python => "py",
            CodeGenMode::TypeScript => "ts",
            CodeGenMode::Protobuf => "proto",
            CodeGenMode::Go => "go",
        }
    }

    pub fn is_json_format(&self) -> bool {
        matches!(self, CodeGenMode::OpenApi | CodeGenMode::JsonSchema)
    }
}

impl TryFrom<&str> for CodeGenMode {
    type Error = CodeGenError;

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        match value.to_lowercase().as_str() {
            "jsonschema" => Ok(CodeGenMode::JsonSchema),
            "openapi" => Ok(CodeGenMode::OpenApi),
            "rust" => Ok(CodeGenMode::Rust),
            "python" => Ok(CodeGenMode::Python),
            "typescript" => Ok(CodeGenMode::TypeScript),
            "protobuf" => Ok(CodeGenMode::Protobuf),
            "go" => Ok(CodeGenMode::Go),
            _ => Err(CodeGenError::InternalError(format!("Unknown code generation mode: {}", value))),
        }
    }
}

#[derive(Debug, Clone)]
pub struct CodeGen;

impl CodeGen {
    fn analyze(mode: CodeGenMode, source: &SourceCodeMetadata, analyzer_options: SemanticAnalyzerOptions) -> Result<(Box<dyn CodeGenerator>, GlueIr, Vec<SemanticWarning>), CodeGenError> {
        debug!("Parsing source code");
        let parsed_program = Parser::new().parse(source).map_err(CodeGenError::ParserError)?;
        debug!("Starting semantic analysis");
        let mut analyzed_program = SemanticAnalyzer::with_options(analyzer_options)
            .analyze(&parsed_program, source)
            .map_err(CodeGenError::SemanticAnalysisError)?;
        let warnings = std::mem::take(&mut analyzed_program.warnings);
        let ir = GlueIr::from_analyzed_program(source.file_name, analyzed_program);
        let codegen: Box<dyn CodeGenerator> = match mode {
            CodeGenMode::JsonSchema => Box::<CodeGenJsonSchema>::default(),
            CodeGenMode::OpenApi => Box::<CodeGenOpenAPI>::default(),
            CodeGenMode::Rust => Box::<CodeGenRust>::default(),
            CodeGenMode::Python => Box::<CodeGenPython>::default(),
            CodeGenMode::TypeScript => Box::<CodeGenTypeScript>::default(),
            CodeGenMode::Protobuf => Box::<CodeGenProtobuf>::default(),
            CodeGenMode::Go => Box::<CodeGenGo>::default(),
        };
        Ok((codegen, ir, warnings))
    }

    pub fn generate(mode: CodeGenMode, source: &SourceCodeMetadata, config: Option<GlueConfigSchemaGeneration>) -> Result<String, CodeGenError> {
        Ok(Self::generate_with_options(mode, source, config, SemanticAnalyzerOptions::default())?.code)
    }

    pub fn generate_with_options(
        mode: CodeGenMode,
        source: &SourceCodeMetadata,
        config: Option<GlueConfigSchemaGeneration>,
        analyzer_options: SemanticAnalyzerOptions,
    ) -> Result<CodeGenOutput, CodeGenError> {
        let (codegen, ir, warnings) = Self::analyze(mode, source, analyzer_options)?;
        let code = codegen.generate(ir, source, config)?;
        Ok(CodeGenOutput { code, warnings })
    }
}
