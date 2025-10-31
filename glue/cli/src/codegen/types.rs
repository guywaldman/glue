// We box the `CodeGenError` to reduce its size.
pub type CodeGenError = Box<miette::Report>;

pub type EmitResult = Result<String, CodeGenError>;

pub trait CodeGenerator {
    fn generate(self) -> EmitResult;
}
