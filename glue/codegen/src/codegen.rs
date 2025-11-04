use config::GlueConfig;
use lang::{AnalyzedProgram, SourceCodeMetadata};

use thiserror::Error;

use crate::{codegen_jsonschema::CodeGenJsonSchema, codegen_openapi::CodeGenOpenAPI, codegen_python::CodeGenPython, codegen_rust::CodeGenRust};

#[derive(Debug, Error)]
pub enum CodeGenError {
    #[error("Internal error: {0}")]
    InternalError(String),
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
}

#[derive(Debug, Clone)]
pub struct CodeGen;

impl CodeGen {
    pub fn generate(mode: CodeGenMode, program: AnalyzedProgram, source: &SourceCodeMetadata, config: Option<GlueConfig>) -> Result<String, crate::CodeGenError> {
        let codegen: Box<dyn CodeGenerator> = match mode {
            CodeGenMode::JsonSchema => Box::new(CodeGenJsonSchema::new()),
            CodeGenMode::OpenApi => Box::new(CodeGenOpenAPI::new()),
            CodeGenMode::Rust => Box::new(CodeGenRust::new()),
            CodeGenMode::Python => Box::new(CodeGenPython::new()),
        };
        codegen.generate(program, source, config)
    }
}
