use std::path::{Path, PathBuf};
use std::process::Command;

use anyhow::{Result, anyhow};
use testcontainers::{GenericImage, ImageExt, core::IntoContainerPort, runners::SyncRunner};

const S3_MINIO_GLUE_SOURCE: &str = include_str!("e2e/fixtures/glue/s3.glue");
const S3_MINIO_PYTHON_SCRIPT: &str = include_str!("e2e/fixtures/python/s3_minio_http.py");

fn unique_temp_dir(test_name: &str) -> PathBuf {
    let random: u32 = rand::random();
    std::env::temp_dir().join(format!("glue_e2e_aws_{}_{:x}", test_name, random))
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

    fn generate_python(&self) -> Result<PathBuf> {
        let output_name = format!("{}_python.py", self.source_path.file_stem().unwrap().to_string_lossy());
        let output_path = self.temp_dir.join(&output_name);

        let config_path = self.source_path.parent().unwrap().join(".gluerc.yaml");
        let mut args = vec![
            "run".to_string(),
            "--bin".to_string(),
            "glue".to_string(),
            "--".to_string(),
            "gen".to_string(),
            "python".to_string(),
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
            return Err(anyhow!("Glue codegen failed:\nstdout: {}\nstderr: {}", stdout, stderr));
        }

        Ok(output_path)
    }
}

fn cleanup(path: &PathBuf) {
    let _ = std::fs::remove_file(path);
}

fn docker_is_available() -> Result<()> {
    let Ok(output) = Command::new("docker").args(["version", "--format", "{{.Server.Version}}\n"]).output() else {
        return Err(anyhow!("Docker is not available"));
    };
    if !output.status.success() {
        return Err(anyhow!("Docker is not available"));
    }
    Ok(())
}

fn run_uv_script(path: &Path, args: &[&str]) -> Result<()> {
    let mut cmd_args = vec!["run".to_string(), path.to_str().unwrap().to_string()];
    cmd_args.extend(args.iter().map(|arg| arg.to_string()));

    let output = Command::new("uv").args(&cmd_args).output()?;

    if !output.status.success() {
        let stderr = String::from_utf8_lossy(&output.stderr);
        let stdout = String::from_utf8_lossy(&output.stdout);
        return Err(anyhow!("Python script failed:\n{}\n{}", stdout, stderr));
    }
    Ok(())
}

#[test]
fn e2e_s3_minio() -> Result<()> {
    docker_is_available()?;

    let fixture = GlueTestFixture::from_source("s3_minio_real", "s3_minio.glue", S3_MINIO_GLUE_SOURCE)?;
    fixture.write_config("global:\n  config:\n    watermark: none\n")?;
    let output_path = fixture.generate_python()?;

    let minio = GenericImage::new("minio/minio", "latest")
        .with_exposed_port(9000.tcp())
        .with_env_var("MINIO_ROOT_USER", "minioadmin")
        .with_env_var("MINIO_ROOT_PASSWORD", "minioadmin")
        .with_cmd(vec!["server", "/data", "--address", ":9000"])
        .start()?;

    let port = minio.get_host_port_ipv4(9000.tcp())?;
    let module_name = output_path.file_stem().unwrap().to_string_lossy().to_string();
    let script_path = output_path.with_extension("minio_test.py");
    std::fs::write(&script_path, S3_MINIO_PYTHON_SCRIPT)?;

    let module_parent = output_path.parent().unwrap().to_string_lossy().to_string();
    let port_arg = port.to_string();

    let result = run_uv_script(&script_path, &[module_parent.as_str(), module_name.as_str(), port_arg.as_str()]);

    cleanup(&output_path);
    std::fs::remove_file(&script_path).ok();

    result
}
