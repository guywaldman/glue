use anyhow::{Result, anyhow};
use insta::assert_snapshot;
use std::{env::temp_dir, path::PathBuf};

use crate::args::CodeGenMode;
use crate::cli::GlueCli;

#[test]
fn test_rust() {
    run_cli_integration_test(CodeGenMode::Rust);
}

#[test]
fn test_openapi() {
    run_cli_integration_test(CodeGenMode::OpenApi);
}

fn run_cli_integration_test(codegen_mode: CodeGenMode) {
    for input_path in load_glue_fixtures().unwrap() {
        let snapshot_name = format!("gen_{}_{}", <&str>::from(codegen_mode), input_path.file_stem().unwrap().to_string_lossy());
        let generated_code = generate_code(input_path, codegen_mode).unwrap();
        assert_snapshot!(snapshot_name, generated_code);
    }
}

/// Read all files in the `/fixtures/glue` directory (same level as this file) and return their paths and contents.
fn load_glue_fixtures() -> Result<Vec<PathBuf>> {
    let mut fixtures = Vec::new();
    let fixtures_dir = std::path::Path::new(env!("CARGO_MANIFEST_DIR")).join("src").join("test").join("fixtures");
    if !fixtures_dir.exists() {
        return Err(anyhow!("Fixtures directory does not exist: {}", fixtures_dir.to_string_lossy()));
    }
    if fixtures_dir.exists() && fixtures_dir.is_dir() {
        for entry in std::fs::read_dir(fixtures_dir).map_err(|e| anyhow!("Failed to read fixtures directory: {}", e))? {
            let entry = entry.map_err(|e| anyhow!("Failed to read directory entry: {}", e))?;
            if entry.path().extension().and_then(|s| s.to_str()) != Some("glue") {
                continue;
            }
            fixtures.push(entry.path());
        }
    }
    Ok(fixtures)
}

fn generate_code(input_path: PathBuf, codegen_mode: CodeGenMode) -> Result<String> {
    let mut output_file_path = temp_dir();
    output_file_path.push("glue_test");
    let config_path = std::path::Path::new(env!("CARGO_MANIFEST_DIR")).join("src").join("test").join("fixtures").join(".gluerc.yaml");
    std::fs::create_dir_all(&output_file_path).map_err(|e| anyhow!("Failed to create temp directory: {}", e))?;

    let ext = match codegen_mode {
        CodeGenMode::JsonSchema => "json",
        CodeGenMode::OpenApi => "yaml",
        CodeGenMode::Rust => "rs",
        CodeGenMode::Python => "py",
        CodeGenMode::Protobuf => "proto",
    };
    output_file_path.push(format!("output.{}", ext));

    // Clean up old file if it exists
    let _ = std::fs::remove_file(&output_file_path);

    let mode_str: &str = codegen_mode.into();
    let cli = GlueCli::new();
    let args = &[
        "glue",
        "gen",
        mode_str,
        "--config",
        config_path.to_str().unwrap(),
        "--input",
        input_path.to_str().unwrap(),
        "--output",
        output_file_path.to_str().unwrap(),
    ];
    cli.run(args).map_err(|e| anyhow!("CLI execution for command '{}' failed: {}", args.join(" "), e))?;
    let output_file_contents = std::fs::read_to_string(&output_file_path).map_err(|e| anyhow!("Failed to read output file: {}", e))?;
    Ok(output_file_contents)
}
