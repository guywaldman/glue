use anyhow::{Result, anyhow};
use insta::{assert_json_snapshot, assert_snapshot};
use std::cell::OnceCell;
use std::collections::HashMap;
use std::{env::temp_dir, path::PathBuf};

use crate::cli::GlueCli;
use codegen::CodeGenMode;

const MOVIES_FIXTURE_NAME: &str = "movies.glue";
const TODOS_FIXTURE_NAME: &str = "todos.glue";

thread_local! {
    static FIXTURES: OnceCell<HashMap<String, PathBuf>> = const { OnceCell::new() };
}

#[test]
fn test_rust_movies_sample() {
    let path = load_glue_fixture(MOVIES_FIXTURE_NAME).unwrap();
    run_cli_integration_test(CodeGenMode::Rust, path);
}

#[test]
fn test_rust_todos_sample() {
    let path = load_glue_fixture(TODOS_FIXTURE_NAME).unwrap();
    run_cli_integration_test(CodeGenMode::Rust, path);
}

#[test]
fn test_openapi_movies_sample() {
    let path = load_glue_fixture(MOVIES_FIXTURE_NAME).unwrap();
    run_cli_integration_test(CodeGenMode::OpenApi, path);
}

#[test]
fn test_openapi_todos_sample() {
    let path = load_glue_fixture(TODOS_FIXTURE_NAME).unwrap();
    run_cli_integration_test(CodeGenMode::OpenApi, path);
}

#[test]
fn test_python_movies_sample() {
    let path = load_glue_fixture(MOVIES_FIXTURE_NAME).unwrap();
    run_cli_integration_test(CodeGenMode::Python, path);
}

#[test]
fn test_python_todos_sample() {
    let path = load_glue_fixture(TODOS_FIXTURE_NAME).unwrap();
    run_cli_integration_test(CodeGenMode::Python, path);
}

fn run_cli_integration_test(codegen_mode: CodeGenMode, fixture_path: PathBuf) {
    let snapshot_name = format!("gen_{}_{}", codegen_mode.as_str(), fixture_path.file_stem().unwrap().to_string_lossy());
    let generated_code = generate_code(fixture_path, codegen_mode).unwrap();
    match codegen_mode {
        CodeGenMode::OpenApi => {
            let generated_json = serde_yaml::from_str::<serde_json::Value>(&generated_code).unwrap();
            assert_json_snapshot!(snapshot_name, generated_json);
        }
        _ => {
            assert_snapshot!(snapshot_name, generated_code);
        }
    }
}

fn load_glue_fixture(fixture_name: &str) -> Result<PathBuf> {
    let load_glue_fixtures_inner = || -> Result<HashMap<String, PathBuf>> {
        let mut result = HashMap::new();
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
                let file_name = entry.file_name().into_string().map_err(|_| anyhow!("Invalid UTF-8 in file name"))?;
                result.insert(file_name, entry.path());
            }
        }
        Ok(result)
    };
    let path = FIXTURES.with(|f| {
        let fixtures = f.get_or_init(move || load_glue_fixtures_inner().unwrap());
        fixtures.get(fixture_name).ok_or_else(|| anyhow!("Fixture '{}' not found", fixture_name)).cloned()
    })?;
    Ok(path)
}

fn generate_code(input_path: PathBuf, codegen_mode: CodeGenMode) -> Result<String> {
    let unique_id: u64 = rand::random();
    let mut output_file_path = temp_dir();
    output_file_path.push(format!("glue_test_{}", unique_id));
    let config_path = std::path::Path::new(env!("CARGO_MANIFEST_DIR")).join("src").join("test").join("fixtures").join(".gluerc.yaml");
    std::fs::create_dir_all(&output_file_path).map_err(|e| anyhow!("Failed to create temp directory: {}", e))?;

    let ext = codegen_mode.file_extension();
    output_file_path.push(format!("output.{}", ext));

    let mode_str = codegen_mode.as_str();
    let cli = GlueCli;
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
