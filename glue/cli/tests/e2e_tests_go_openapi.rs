use std::path::PathBuf;
use std::process::Command;

use anyhow::{Context, Result, anyhow};

const API_GLUE_SOURCE: &str = include_str!("e2e/fixtures/glue/api.glue");
const GO_OPENAPI_ROUNDTRIP_TEST: &str = include_str!("e2e/fixtures/go/openapi_server_client_e2e_test.go");

fn unique_temp_dir(test_name: &str) -> PathBuf {
    let random: u32 = rand::random();
    std::env::temp_dir().join(format!("glue_e2e_go_openapi_{}_{:x}", test_name, random))
}

struct GlueTestFixture {
    source_path: PathBuf,
    temp_dir: PathBuf,
}

impl GlueTestFixture {
    fn from_source(test_name: &str, name: &str, source: &str) -> Result<Self> {
        let temp_dir = unique_temp_dir(test_name);
        std::fs::create_dir_all(&temp_dir)?;

        let source_path = temp_dir.join(name);
        std::fs::write(&source_path, source)?;

        Ok(Self { source_path, temp_dir })
    }

    fn write_config(&self, yaml: &str) -> Result<()> {
        let config_path = self.source_path.parent().unwrap().join(".gluerc.yaml");
        std::fs::write(&config_path, yaml)?;
        Ok(())
    }

    fn generate_openapi(&self) -> Result<PathBuf> {
        let output_name = format!("{}_openapi.json", self.source_path.file_stem().unwrap().to_string_lossy());
        let output_path = self.temp_dir.join(&output_name);

        let config_path = self.source_path.parent().unwrap().join(".gluerc.yaml");
        let mut args = vec![
            "run".to_string(),
            "--bin".to_string(),
            "glue".to_string(),
            "--".to_string(),
            "gen".to_string(),
            "openapi".to_string(),
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
            return Err(anyhow!(
                "Glue openapi codegen failed:\nstdout: {}\nstderr: {}",
                String::from_utf8_lossy(&output.stdout),
                String::from_utf8_lossy(&output.stderr)
            ));
        }

        if !output_path.exists() {
            return Err(anyhow!("OpenAPI output file was not created: {}", output_path.display()));
        }

        Ok(output_path)
    }
}

fn cleanup(path: &PathBuf) {
    let _ = std::fs::remove_file(path);
}

fn ensure_go_available() -> Result<()> {
    let Ok(output) = Command::new("go").arg("version").output() else {
        return Err(anyhow!("Go toolchain is not available"));
    };
    if !output.status.success() {
        return Err(anyhow!("Go toolchain is not available"));
    }
    Ok(())
}

#[test]
fn e2e_openapi_generated_go_server_and_client() -> Result<()> {
    ensure_go_available()?;

    let fixture = GlueTestFixture::from_source("openapi_go", "api_comprehensive.glue", API_GLUE_SOURCE)?;
    fixture.write_config("global:\n  config:\n    watermark: none\n")?;
    let output_path = fixture.generate_openapi()?;

    let spec_content = std::fs::read_to_string(&output_path)?;
    let spec: serde_json::Value = serde_json::from_str(&spec_content).context("Generated OpenAPI is not valid JSON")?;

    assert!(spec.pointer("/paths/~1users~1{user_id}/get").is_some(), "Missing GET /users/{{user_id}} operation");
    assert!(spec.pointer("/paths/~1users/get").is_some(), "Missing GET /users operation");
    assert!(spec.pointer("/paths/~1users~1{user_id}~1posts/get").is_some(), "Missing GET /users/{{user_id}}/posts operation");
    assert!(spec.pointer("/paths/~1health/get").is_some(), "Missing GET /health operation");

    assert!(spec.pointer("/components/schemas/User/properties/userId").is_some(), "Missing alias-mapped userId property");
    assert_eq!(spec.pointer("/components/schemas/User/properties/userId/example").and_then(|v| v.as_str()), Some("user_123"));
    assert_eq!(spec.pointer("/components/schemas/User/properties/tags/type").and_then(|v| v.as_str()), Some("array"));
    assert!(
        spec.pointer("/components/schemas/User/properties/preferences/additionalProperties").is_some(),
        "Missing Record<string, string> mapping"
    );

    // Verify wildcard error responses are emitted as requested
    assert!(spec.pointer("/paths/~1users~1{user_id}/get/responses/4XX").is_some(), "Missing 4XX response for GET /users/{{user_id}}");
    assert!(spec.pointer("/paths/~1users~1{user_id}/get/responses/5XX").is_some(), "Missing 5XX response for GET /users/{{user_id}}");

    let go_dir = fixture.temp_dir.join("go_openapi");
    std::fs::create_dir_all(&go_dir)?;

    let go_spec_path = go_dir.join("openapi.json");
    std::fs::write(&go_spec_path, &spec_content)?;
    std::fs::write(go_dir.join("go.mod"), "module apitest\n\ngo 1.22\n")?;

    let generator_output = Command::new("go")
        .args([
            "run",
            "github.com/oapi-codegen/oapi-codegen/v2/cmd/oapi-codegen@v2.4.1",
            "-generate",
            "types,client,std-http-server",
            "-package",
            "apitest",
            "openapi.json",
        ])
        .current_dir(&go_dir)
        .output()?;

    if !generator_output.status.success() {
        return Err(anyhow!(
            "oapi-codegen failed:\n{}\n{}",
            String::from_utf8_lossy(&generator_output.stdout),
            String::from_utf8_lossy(&generator_output.stderr)
        ));
    }

    std::fs::write(go_dir.join("api.gen.go"), &generator_output.stdout)?;
    std::fs::write(go_dir.join("openapi_roundtrip_test.go"), GO_OPENAPI_ROUNDTRIP_TEST)?;

    let go_mod_tidy_output = Command::new("go").args(["mod", "tidy"]).current_dir(&go_dir).output()?;
    if !go_mod_tidy_output.status.success() {
        return Err(anyhow!(
            "go mod tidy failed:\n{}\n{}",
            String::from_utf8_lossy(&go_mod_tidy_output.stdout),
            String::from_utf8_lossy(&go_mod_tidy_output.stderr)
        ));
    }

    let go_test_output = Command::new("go").args(["test", "./...", "-v"]).current_dir(&go_dir).output()?;

    cleanup(&output_path);

    if !go_test_output.status.success() {
        return Err(anyhow!(
            "Go OpenAPI server/client roundtrip failed:\n{}\n{}",
            String::from_utf8_lossy(&go_test_output.stdout),
            String::from_utf8_lossy(&go_test_output.stderr)
        ));
    }

    Ok(())
}
