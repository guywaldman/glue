use lang::{AnalyzedProgram, SourceCodeMetadata};
use thiserror::Error;

#[derive(Debug, Error)]
pub enum CodeGenError {
    #[error("General error: {0}")]
    GeneralError(String),
    #[error("Errors during code generation")]
    GenerationErrors(Vec<miette::Report>),
}

pub trait CodeGenerator {
    fn generate(&self, program: AnalyzedProgram, source: &SourceCodeMetadata) -> Result<String, CodeGenError>;
}

pub type CodeGenResult<T> = Result<T, CodeGenError>;
