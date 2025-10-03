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

pub type EmitResult = Result<String, CodeGenError>;

pub trait CodeGenerator {
    fn generate(self) -> EmitResult;
}
