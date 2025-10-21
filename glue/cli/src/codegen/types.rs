use gluelang::LangError;
use thiserror::Error;

#[derive(Debug, Error)]
pub enum CodeGenError {
    #[error("Language error: {0}")]
    LangError(LangError),
    #[error("Other error: {0}")]
    Other(String),
}

// We box the `CodeGenError` to reduce its size.
pub type EmitResult<T = String> = Result<T, Box<CodeGenError>>;

pub trait CodeGenerator {
    fn generate(self) -> EmitResult;
}
