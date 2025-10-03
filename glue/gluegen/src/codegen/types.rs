use thiserror::Error;

#[derive(Debug, Error)]
pub enum CodeGenError {
    #[error("Unsupported feature: {0}")]
    UnsupportedError(String),
    #[error("Other error: {0}")]
    Other(String),
}

pub type EmitResult = Result<String, CodeGenError>;

pub trait CodeGenerator {
    fn generate(self) -> EmitResult;
}
