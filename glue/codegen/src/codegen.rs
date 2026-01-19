use config::GlueConfig;
use lang::{AnalyzedProgram, Parser, ParserError, SemanticAnalyzer, SemanticAnalyzerError, SourceCodeMetadata};

use log::debug;
use thiserror::Error;

use crate::{codegen_go::CodeGenGo, codegen_jsonschema::CodeGenJsonSchema, codegen_openapi::CodeGenOpenAPI, codegen_protobuf::CodeGenProtobuf, codegen_python::CodeGenPython, codegen_rust::CodeGenRust};

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

pub trait CodeGenerator {
    fn generate(&self, program: AnalyzedProgram, source: &SourceCodeMetadata, config: Option<GlueConfig>) -> Result<String, CodeGenError>;
}

pub type CodeGenResult<T> = Result<T, CodeGenError>;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CodeGenMode {
    JsonSchema,
    OpenApi,
    Rust,
    Python,
    Protobuf,
    Go,
}

impl TryFrom<&str> for CodeGenMode {
    type Error = CodeGenError;

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        match value.to_lowercase().as_str() {
            "jsonschema" => Ok(CodeGenMode::JsonSchema),
            "openapi" => Ok(CodeGenMode::OpenApi),
            "rust" => Ok(CodeGenMode::Rust),
            "python" => Ok(CodeGenMode::Python),
            "protobuf" => Ok(CodeGenMode::Protobuf),
            "go" => Ok(CodeGenMode::Go),
            _ => Err(CodeGenError::InternalError(format!("Unknown code generation mode: {}", value))),
        }
    }
}

#[derive(Debug, Clone)]
pub struct CodeGen;

impl CodeGen {
    pub fn generate(mode: CodeGenMode, source: &SourceCodeMetadata, config: Option<GlueConfig>) -> Result<String, CodeGenError> {
        debug!("Parsing source code");
        let parsed_program = Parser::new().parse(source).map_err(CodeGenError::ParserError)?;
        debug!("Parsed program successfully");

        debug!("Starting semantic analysis");
        let analyzed_program = SemanticAnalyzer::new().analyze(&parsed_program, source).map_err(CodeGenError::SemanticAnalysisError)?;
        debug!("Semantic analysis completed successfully");

        let codegen: Box<dyn CodeGenerator> = match mode {
            CodeGenMode::JsonSchema => Box::new(CodeGenJsonSchema::new()),
            CodeGenMode::OpenApi => Box::new(CodeGenOpenAPI::new()),
            CodeGenMode::Rust => Box::new(CodeGenRust::new()),
            CodeGenMode::Python => Box::new(CodeGenPython::new()),
            CodeGenMode::Protobuf => Box::new(CodeGenProtobuf::new()),
            CodeGenMode::Go => Box::new(CodeGenGo::new()),
        };
        debug!("Generating code");
        let generated = codegen.generate(analyzed_program, source, config);
        debug!("Code generation completed successfully");

        generated
    }
}
