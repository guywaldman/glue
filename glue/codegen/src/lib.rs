// TODO: Remove warning suppressions.
#![allow(dead_code, unused_imports, clippy::new_without_default)]

mod codegen;
mod codegen_go;
mod codegen_jsonschema;
mod codegen_openapi;
mod codegen_protobuf;
mod codegen_python;
mod codegen_rust;
mod codegen_utils;
mod context;
mod models;

#[cfg(test)]
mod test_utils;

pub use codegen::CodeGenerator;
pub use codegen::{CodeGen, CodeGenError, CodeGenMode};
pub use lang::SourceCodeMetadata;
pub use lang::{generate_report, generate_reports, print_report};
