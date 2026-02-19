//! End-to-end tests for Glue codegen.
//!
//! These tests verify that generated code is valid and can be used with actual
//! language tooling (Python, protoc, OpenAPI validators, etc.).

use std::path::{Path, PathBuf};
use std::process::Command;

use anyhow::{Context, Result, anyhow};

/// Run a command via `uv run` with the given Python dependencies.
fn uv_run(deps: &[&str]) -> Command {
    let mut cmd = Command::new("uv");
    cmd.arg("run");
    for dep in deps {
        cmd.args(["--with", dep]);
    }
    cmd
}

fn unique_temp_dir(test_name: &str) -> PathBuf {
    let random: u32 = rand::random();
    std::env::temp_dir().join(format!("glue_e2e_{}_{:x}", test_name, random))
}

struct GlueTestFixture {
    source_path: PathBuf,
    temp_dir: PathBuf,
}

impl GlueTestFixture {
    fn new(test_name: &str, fixture_name: &str) -> Result<Self> {
        let fixtures_dir = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("src").join("test").join("fixtures");

        let source_path = fixtures_dir.join(fixture_name);
        if !source_path.exists() {
            return Err(anyhow!("Fixture not found: {}", source_path.display()));
        }

        let temp_dir = unique_temp_dir(test_name);
        std::fs::create_dir_all(&temp_dir)?;

        Ok(Self { source_path, temp_dir })
    }

    /// Create a fixture from inline Glue source code.
    fn from_source(test_name: &str, name: &str, source: &str) -> Result<Self> {
        let temp_dir = unique_temp_dir(test_name);
        std::fs::create_dir_all(&temp_dir)?;

        let source_path = temp_dir.join(name);
        std::fs::write(&source_path, source)?;

        Ok(Self { source_path, temp_dir })
    }

    /// Write a .gluerc.yaml config file next to the source.
    fn write_config(&self, yaml: &str) -> Result<()> {
        let config_path = self.source_path.parent().unwrap().join(".gluerc.yaml");
        std::fs::write(&config_path, yaml)?;
        Ok(())
    }

    fn generate_python(&self) -> Result<PathBuf> {
        self.run_codegen("python", "py")
    }

    fn generate_openapi(&self) -> Result<PathBuf> {
        self.run_codegen("openapi", "json")
    }

    fn generate_jsonschema(&self) -> Result<PathBuf> {
        self.run_codegen("jsonschema", "json")
    }

    fn generate_protobuf(&self) -> Result<PathBuf> {
        self.run_codegen("protobuf", "proto")
    }

    fn generate_rust(&self) -> Result<PathBuf> {
        self.run_codegen("rust", "rs")
    }

    fn generate_glue_ir_json(&self) -> Result<serde_json::Value> {
        let output = Command::new("cargo")
            .args(["run", "--bin", "glue", "--", "ast", "-i", self.source_path.to_str().unwrap()])
            .current_dir(env!("CARGO_MANIFEST_DIR"))
            .output()
            .map_err(|e| anyhow!("Failed to run glue ast CLI: {}", e))?;

        if !output.status.success() {
            let stderr = String::from_utf8_lossy(&output.stderr);
            let stdout = String::from_utf8_lossy(&output.stdout);
            return Err(anyhow!("Glue ast failed:\nstdout: {}\nstderr: {}", stdout, stderr));
        }

        let stdout = String::from_utf8(output.stdout)?;
        serde_json::from_str(&stdout).context("Failed to parse Glue IR JSON output")
    }

    fn run_codegen(&self, mode: &str, ext: &str) -> Result<PathBuf> {
        let output_name = format!("{}_{}.{}", self.source_path.file_stem().unwrap().to_string_lossy(), mode, ext);
        let output_path = self.temp_dir.join(&output_name);

        let _ = std::fs::remove_file(&output_path);
        let config_path = self.source_path.parent().unwrap().join(".gluerc.yaml");
        let mut args = vec![
            "run".to_string(),
            "--bin".to_string(),
            "glue".to_string(),
            "--".to_string(),
            "gen".to_string(),
            mode.to_string(),
            "-i".to_string(),
            self.source_path.to_str().unwrap().to_string(),
            "-o".to_string(),
            output_path.to_str().unwrap().to_string(),
        ];

        if config_path.exists() {
            args.extend(["-c".to_string(), config_path.to_str().unwrap().to_string()]);
        }

        let output = Command::new("cargo")
            .args(&args)
            .current_dir(env!("CARGO_MANIFEST_DIR"))
            .output()
            .map_err(|e| anyhow!("Failed to run glue CLI: {}", e))?;

        if !output.status.success() {
            let stderr = String::from_utf8_lossy(&output.stderr);
            let stdout = String::from_utf8_lossy(&output.stdout);
            return Err(anyhow!("Glue codegen failed for {}:\nstdout: {}\nstderr: {}", mode, stdout, stderr));
        }

        if !output_path.exists() {
            return Err(anyhow!("Output file was not created: {}", output_path.display()));
        }

        Ok(output_path)
    }
}

fn cleanup(path: &PathBuf) {
    let _ = std::fs::remove_file(path);
}

fn count_ir_error_nodes(value: &serde_json::Value) -> usize {
    fn walk(node: &serde_json::Value) -> usize {
        let mut count = 0;
        if node.get("kind").and_then(|k| k.as_str()) == Some("error") {
            count += 1;
        }
        if let Some(children) = node.get("children").and_then(|c| c.as_array()) {
            for child in children {
                count += walk(child);
            }
        }
        count
    }

    value.get("root").map(walk).unwrap_or(0)
}

#[test]
fn e2e_python_pydantic_todos() -> Result<()> {
    let fixture = GlueTestFixture::new("python_todos", "todos.glue")?;
    let output_path = fixture.generate_python()?;

    validate_python_syntax(&output_path)?;
    validate_python_types(&output_path)?;

    cleanup(&output_path);
    Ok(())
}

#[test]
fn e2e_ast_subcommand_outputs_glue_ir_json() -> Result<()> {
    let fixture = GlueTestFixture::new("ast_json", "movies.glue")?;
    let ir = fixture.generate_glue_ir_json()?;

    assert_eq!(ir.get("version").and_then(|v| v.as_str()), Some("1"));
    assert_eq!(ir.get("file_name").and_then(|v| v.as_str()), fixture.source_path.to_str());
    assert!(ir.get("root").is_some(), "Expected AST root in Glue IR");
    assert!(ir.get("errors").and_then(|v| v.as_array()).is_some(), "Expected errors array in Glue IR");

    Ok(())
}

#[test]
fn e2e_ast_valid_literals_emit_no_error_nodes() -> Result<()> {
    let fixture = GlueTestFixture::from_source(
        "ast_no_error_literals",
        "literals.glue",
        r#"
model A {
  x: int = 1
  y: bool = true
  z: string = "hi"
}
"#,
    )?;

    let ir = fixture.generate_glue_ir_json()?;
    let error_nodes = count_ir_error_nodes(&ir);
    assert_eq!(error_nodes, 0, "valid literal defaults should not produce error nodes in IR");
    Ok(())
}

#[test]
fn e2e_python_pydantic_movies() -> Result<()> {
    let fixture = GlueTestFixture::new("python_movies", "movies.glue")?;
    let output_path = fixture.generate_python()?;

    validate_python_syntax(&output_path)?;
    validate_python_types(&output_path)?;

    cleanup(&output_path);
    Ok(())
}

#[test]
fn e2e_python_import_and_instantiate() -> Result<()> {
    let fixture = GlueTestFixture::new("python_import", "todos.glue")?;
    let output_path = fixture.generate_python()?;

    // Create a test script that imports and uses the generated models
    let test_script = format!(
        r#"
import sys
sys.path.insert(0, "{parent_dir}")

from {module} import *

# Test that we can instantiate the models
# (Pydantic will validate the types)
try:
    # Test Error model
    error = Error(message="test error")
    assert error.message == "test error"
    print("SUCCESS")
except Exception as e:
    print(f"ERROR: Error model failed: {{e}}")
    sys.exit(1)

print("All Python E2E tests passed!")
"#,
        parent_dir = output_path.parent().unwrap().display(),
        module = output_path.file_stem().unwrap().to_string_lossy(),
    );

    let test_script_path = output_path.with_extension("test.py");
    std::fs::write(&test_script_path, test_script)?;

    let result = run_python_script(&test_script_path);

    cleanup(&output_path);
    std::fs::remove_file(&test_script_path).ok();

    result
}

#[test]
fn e2e_python_dataclasses() -> Result<()> {
    let source = r#"
model User {
    name: string
    age: int
    email?: string
    active: bool = true
}
"#;
    let fixture = GlueTestFixture::from_source("python_dataclasses", "dc_test.glue", source)?;
    fixture.write_config("global:\n  config:\n    python:\n      data_model_library: dataclasses\n")?;
    let output_path = fixture.generate_python()?;

    validate_python_syntax(&output_path)?;
    validate_python_types(&output_path)?;

    // Verify the generated code uses dataclasses
    let content = std::fs::read_to_string(&output_path)?;
    assert!(content.contains("@dataclass"), "Expected @dataclass decorator");
    assert!(!content.contains("BaseModel"), "Should not contain BaseModel");

    cleanup(&output_path);
    Ok(())
}

#[test]
fn e2e_python_msgspec() -> Result<()> {
    let source = r#"
model User {
    name: string
    age: int
    email?: string
    active: bool = true
}
"#;
    let fixture = GlueTestFixture::from_source("python_msgspec", "ms_test.glue", source)?;
    fixture.write_config("global:\n  config:\n    python:\n      data_model_library: msgspec\n")?;
    let output_path = fixture.generate_python()?;

    validate_python_syntax(&output_path)?;

    // Verify the generated code uses msgspec
    let content = std::fs::read_to_string(&output_path)?;
    assert!(content.contains("msgspec.Struct"), "Expected msgspec.Struct base class");
    assert!(!content.contains("BaseModel"), "Should not contain BaseModel");

    // Import and instantiate
    let test_script = format!(
        r#"
import sys
sys.path.insert(0, "{parent_dir}")
from {module} import *

user = User(name="Alice", age=30)
assert user.name == "Alice"
assert user.age == 30
print("msgspec OK")
"#,
        parent_dir = output_path.parent().unwrap().display(),
        module = output_path.file_stem().unwrap().to_string_lossy(),
    );

    let script_path = output_path.with_extension("test.py");
    std::fs::write(&script_path, test_script)?;
    let result = run_python_script_with_deps(&script_path, &["msgspec"]);

    cleanup(&output_path);
    std::fs::remove_file(&script_path).ok();

    result
}

#[test]
fn e2e_openapi_todos_valid_json() -> Result<()> {
    let fixture = GlueTestFixture::new("openapi_json", "todos.glue")?;
    let output_path = fixture.generate_openapi()?;

    // Validate it's valid JSON
    let content = std::fs::read_to_string(&output_path)?;
    let _: serde_json::Value = serde_json::from_str(&content).context("Generated OpenAPI is not valid JSON")?;

    cleanup(&output_path);
    Ok(())
}

#[test]
fn e2e_openapi_todos_has_required_fields() -> Result<()> {
    let fixture = GlueTestFixture::new("openapi_fields", "todos.glue")?;
    let output_path = fixture.generate_openapi()?;

    let content = std::fs::read_to_string(&output_path)?;
    let spec: serde_json::Value = serde_json::from_str(&content)?;

    // Verify required OpenAPI 3.0 fields
    assert!(spec.get("openapi").is_some(), "Missing 'openapi' field");
    assert!(spec.get("info").is_some(), "Missing 'info' field");
    assert!(spec.get("paths").is_some(), "Missing 'paths' field");

    // Verify paths were generated from endpoints
    let paths = spec.get("paths").unwrap().as_object().unwrap();
    assert!(!paths.is_empty(), "No paths generated");

    // Verify components/schemas were generated from models
    let schemas = spec.get("components").and_then(|c| c.get("schemas")).and_then(|s| s.as_object());
    assert!(schemas.is_some(), "Missing components/schemas");
    assert!(!schemas.unwrap().is_empty(), "No schemas generated");

    cleanup(&output_path);
    Ok(())
}

#[test]
fn e2e_openapi_validate_with_spectral() -> Result<()> {
    let fixture = GlueTestFixture::new("openapi_spectral", "todos.glue")?;
    let output_path = fixture.generate_openapi()?;

    let output = uv_run(&["openapi-spec-validator"]).args(["openapi-spec-validator", output_path.to_str().unwrap()]).output()?;

    cleanup(&output_path);

    if !output.status.success() {
        let stderr = String::from_utf8_lossy(&output.stderr);
        let stdout = String::from_utf8_lossy(&output.stdout);
        return Err(anyhow!("OpenAPI spec validation failed:\n{}\n{}", stdout, stderr));
    }

    Ok(())
}

#[test]
fn e2e_jsonschema_valid() -> Result<()> {
    // JSON Schema requires a @root model when multiple models exist.
    // Use inline source with a single model for simplicity.
    let source = r#"
/// A simple user model
@root
model User {
    id: int
    name: string
    email?: string
}
"#;
    let fixture = GlueTestFixture::from_source("jsonschema", "jsonschema_test.glue", source)?;
    let output_path = fixture.generate_jsonschema()?;

    let content = std::fs::read_to_string(&output_path)?;
    let schema: serde_json::Value = serde_json::from_str(&content).context("Generated JSON Schema is not valid JSON")?;

    // Verify it has schema fields
    assert!(schema.get("$schema").is_some() || schema.get("type").is_some(), "Missing JSON Schema fields");

    cleanup(&output_path);
    Ok(())
}

#[test]
fn e2e_protobuf_valid_syntax() -> Result<()> {
    // Use inline source for protobuf - the todos.glue has endpoints which
    // may not be fully supported by the protobuf generator
    let source = r#"
/// A simple user model
model User {
    id: int
    name: string
    email?: string
}

/// An error response
model Error {
    message: string
    code?: int
}
"#;
    let fixture = GlueTestFixture::from_source("protobuf_syntax", "protobuf_test.glue", source)?;
    let output_path = fixture.generate_protobuf()?;

    // Check that the file contains valid protobuf syntax markers
    let content = std::fs::read_to_string(&output_path)?;
    assert!(content.contains("syntax = \"proto3\""), "Missing proto3 syntax declaration");
    assert!(content.contains("message "), "Missing message definitions");

    cleanup(&output_path);
    Ok(())
}

#[test]
fn e2e_protobuf_compile_with_protoc() -> Result<()> {
    let source = r#"
/// A simple user model
model User {
    id: int
    name: string
    email?: string
}

/// An error response
model Error {
    message: string
    code?: int
}
"#;
    let fixture = GlueTestFixture::from_source("protobuf_compile", "protobuf_compile_test.glue", source)?;
    let output_path = fixture.generate_protobuf()?;

    let output = Command::new("protoc")
        .args([
            "--proto_path",
            output_path.parent().unwrap().to_str().unwrap(),
            "--python_out",
            output_path.parent().unwrap().to_str().unwrap(),
            output_path.to_str().unwrap(),
        ])
        .output()?;

    // Clean up generated Python files from protoc
    let proto_py = output_path.with_file_name(format!("{}_pb2.py", output_path.file_stem().unwrap().to_string_lossy()));
    std::fs::remove_file(&proto_py).ok();

    cleanup(&output_path);

    if !output.status.success() {
        let stderr = String::from_utf8_lossy(&output.stderr);
        return Err(anyhow!("protoc compilation failed:\n{}", stderr));
    }

    Ok(())
}

#[test]
fn e2e_rust_valid_syntax() -> Result<()> {
    let fixture = GlueTestFixture::new("rust_syntax", "movies.glue")?;
    let output_path = fixture.generate_rust()?;

    // Check basic Rust syntax markers
    let content = std::fs::read_to_string(&output_path)?;
    assert!(content.contains("pub struct "), "Missing struct definitions");
    assert!(content.contains("#[derive("), "Missing derive attributes");

    cleanup(&output_path);
    Ok(())
}

fn validate_python_syntax(path: &Path) -> Result<()> {
    let output = uv_run(&[]).args(["python3", "-m", "py_compile", path.to_str().unwrap()]).output()?;

    if !output.status.success() {
        let stderr = String::from_utf8_lossy(&output.stderr);
        return Err(anyhow!("Python syntax error:\n{}", stderr));
    }
    Ok(())
}

fn validate_python_types(path: &Path) -> Result<()> {
    let output = uv_run(&["mypy"]).args(["mypy", "--ignore-missing-imports", path.to_str().unwrap()]).output()?;

    // mypy may have warnings, we only fail on errors
    if !output.status.success() {
        let stdout = String::from_utf8_lossy(&output.stdout);
        if stdout.contains("error:") {
            return Err(anyhow!("Python type errors:\n{}", stdout));
        }
    }
    Ok(())
}

fn run_python_script(path: &Path) -> Result<()> {
    run_python_script_with_deps(path, &["pydantic"])
}

fn run_python_script_with_deps(path: &Path, deps: &[&str]) -> Result<()> {
    let output = uv_run(deps).args(["python3", path.to_str().unwrap()]).output()?;

    if !output.status.success() {
        let stderr = String::from_utf8_lossy(&output.stderr);
        let stdout = String::from_utf8_lossy(&output.stdout);
        return Err(anyhow!("Python script failed:\n{}\n{}", stdout, stderr));
    }

    let stdout = String::from_utf8_lossy(&output.stdout);
    println!("{}", stdout);
    Ok(())
}
