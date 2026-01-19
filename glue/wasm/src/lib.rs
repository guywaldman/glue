use codegen::{CodeGen, CodeGenError, SourceCodeMetadata, generate_report, generate_reports};
use wasm_bindgen::prelude::*;

#[wasm_bindgen]
pub fn generate(mode: &str, code: &str) -> String {
    let source = &SourceCodeMetadata {
        file_name: "demo.glue",
        file_contents: code,
    };
    let Ok(mode) = mode.try_into() else {
        // We return an error string directly since we cannot return Result types.
        // TODO: Find an idiomatic way to return structured errors from wasm functions.
        return format!("ERROR: Unknown code generation mode: {}", mode);
    };
    CodeGen::generate(mode, source, None).unwrap_or_else(|e| match e {
        CodeGenError::GenerationError(diag) => generate_report(&diag).unwrap_or_else(|_| "Error generating report".to_string()),
        CodeGenError::GenerationErrors(diags) => {
            let reports: Vec<_> = diags.iter().collect();
            generate_reports(&reports).unwrap_or_else(|_| "Error generating reports".to_string())
        }
        CodeGenError::ParserError(diag) => generate_report(diag.report()).unwrap_or_else(|_| "Error generating report".to_string()),
        CodeGenError::SemanticAnalysisError(diags) => {
            let reports: Vec<_> = diags.iter().map(|e| e.report()).collect();
            generate_reports(reports.as_slice()).unwrap_or_else(|_| "Error generating reports".to_string())
        }
        CodeGenError::InternalError(e) => {
            format!("Internal error during code generation: {}", e)
        }
    })
}
