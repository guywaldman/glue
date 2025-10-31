use std::path::PathBuf;

use crate::cli::{CodeGenMode, GlueCli, read_config};
use anyhow::Result;

pub fn run_snapshot_test(test_name: &str, glue: &str, codegen_mode: CodeGenMode) -> Result<String> {
    let temp_dir = std::env::temp_dir();
    let test_id: u64 = rand::random();
    let test_dir_path = PathBuf::from(format!("{}/{}_{}", temp_dir.display(), test_name, test_id));

    std::fs::create_dir_all(&test_dir_path)?;
    let out_path = test_dir_path.join("out.d.ts");
    let input_path = format!("{}/in.glue", test_dir_path.display());
    std::fs::write(&input_path, glue)?;

    let config = read_config(None)?;
    let temp_cfg_path = test_dir_path.join("config.yaml");
    let cfg_contents = serde_yaml::to_string(&config).unwrap();
    std::fs::write(&temp_cfg_path, cfg_contents)?;

    let gen_command = match codegen_mode {
        CodeGenMode::JsonSchema => "json-schema",
        CodeGenMode::OpenApi => "openapi",
        CodeGenMode::RustSerde => "rust-serde",
        CodeGenMode::PythonPydantic => "python-pydantic",
    };

    GlueCli::new().run(&["cli", "gen", gen_command, "-c", temp_cfg_path.to_str().unwrap(), "-i", &input_path, "-o", out_path.to_str().unwrap()])?;
    let actual_out = std::fs::read_to_string(&out_path)?;
    Ok(actual_out)
}
