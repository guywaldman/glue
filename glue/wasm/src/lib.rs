use codegen::{CodeGen, CodeGenError, SourceCodeMetadata, generate_report, generate_reports};
use config::GlueConfigSchemaGeneration;
#[cfg(target_arch = "wasm32")]
use wasm_bindgen::prelude::*;

#[cfg_attr(target_arch = "wasm32", wasm_bindgen)]
pub struct GenerateResult {
    ok: bool,
    output: String,
}

#[cfg_attr(target_arch = "wasm32", wasm_bindgen)]
impl GenerateResult {
    #[cfg_attr(target_arch = "wasm32", wasm_bindgen(getter))]
    pub fn ok(&self) -> bool {
        self.ok
    }

    #[cfg_attr(target_arch = "wasm32", wasm_bindgen(getter))]
    pub fn output(&self) -> String {
        self.output.clone()
    }
}

#[cfg_attr(target_arch = "wasm32", wasm_bindgen)]
pub fn generate(mode: &str, code: &str) -> GenerateResult {
    generate_with_config_inner(mode, code, None)
}

#[cfg(target_arch = "wasm32")]
#[cfg_attr(target_arch = "wasm32", wasm_bindgen)]
pub fn generate_with_config(mode: &str, code: &str, config: Option<JsValue>) -> GenerateResult {
    let parsed = match config {
        Some(value) => match serde_wasm_bindgen::from_value::<GlueConfigSchemaGeneration>(value) {
            Ok(config) => Some(config),
            Err(err) => {
                return GenerateResult {
                    ok: false,
                    output: format!("ERROR: Invalid config: {}", err),
                };
            }
        },
        None => None,
    };
    generate_with_config_inner(mode, code, parsed)
}

#[cfg(not(target_arch = "wasm32"))]
pub fn generate_with_config(mode: &str, code: &str, config: Option<GlueConfigSchemaGeneration>) -> GenerateResult {
    generate_with_config_inner(mode, code, config)
}

fn generate_with_config_inner(mode: &str, code: &str, config: Option<GlueConfigSchemaGeneration>) -> GenerateResult {
    let source = &SourceCodeMetadata {
        file_name: "demo.glue",
        file_contents: code,
    };
    let Ok(mode) = mode.try_into() else {
        // We return an error string directly since we cannot return Result types.
        // TODO: Find an idiomatic way to return structured errors from wasm functions.
        return GenerateResult {
            ok: false,
            output: format!("ERROR: Unknown code generation mode: {}", mode),
        };
    };
    match CodeGen::generate(mode, source, config) {
        Ok(output) => GenerateResult { ok: true, output },
        Err(e) => {
            let output = match e {
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
            };
            GenerateResult { ok: false, output }
        }
    }
}
