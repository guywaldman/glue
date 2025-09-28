use gluelang::Program;
use thiserror::Error;

#[derive(Debug, Error)]
pub enum CodeGenError {
    #[error("I/O error: {0}")]
    IoError(std::io::Error),
    #[error("Unsupported feature: {0}")]
    UnsupportedError(String),
    #[error("Unresolved reference: {0}")]
    UnresolvedReference(String),
    #[error("Other error: {0}")]
    Other(String),
    #[error("Multiple errors: {0:?}")]
    AggregatedError(Vec<CodeGenError>),
}

pub trait CodeGen {
    fn generate(&self, program: &Program) -> Result<String, CodeGenError>;
}
