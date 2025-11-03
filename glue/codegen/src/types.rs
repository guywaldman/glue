use config::GlueConfig;
use lang::{AnalyzedProgram, SourceCodeMetadata};
use thiserror::Error;

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
