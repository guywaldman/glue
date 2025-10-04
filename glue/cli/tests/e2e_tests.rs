use std::{env::temp_dir, fs};

use indoc::indoc;
use log::debug;

const BINARY: &str = concat!(env!("CARGO_MANIFEST_DIR"), "/../target/release/cli");

macro_rules! run_cmd {
    ($dir:expr, $binary:expr, $($arg:expr),*) => {{
        let res = std::process::Command::new($binary)
            .current_dir($dir)
            $(.arg($arg))*
            .output();
        if let Err(e) = &res {
            panic!("Failed to execute command: {}", e);
        }
        let res = res.unwrap();
        if !res.status.success() {
            let stderr = String::from_utf8_lossy(&res.stderr);
            panic!("Command failed with error: {}", stderr);
        }
        res
    }};
}

#[test]
fn test_e2e_cli_python_pydantic() {
    // let test_dir = format!("cli_test_{}", rand::random::<u32>());
    // let module_name = format!("project_{}", rand::random::<u32>());
    // let test_dir = temp_dir().join(&test_dir).join(&module_name);
    // fs::create_dir_all(&test_dir).expect("Failed to create test directory");
    // let test_dir = test_dir.canonicalize().expect("Failed to canonicalize test directory");
    // debug!("Created test directory at {:?}", &test_dir);
    // let input_file = test_dir.join("sample.glue");
    // fs::write(&input_file, "model Sample { field1: string }").expect("Failed to write input file");

    // let output_file = test_dir.join("output.py");
    // debug!("Generating {:?} to {:?}", &input_file, &output_file);
    // let output = run_cmd!(
    //     &test_dir,
    //     BINARY,
    //     "gen",
    //     "py-pydantic",
    //     "-i",
    //     input_file.to_str().unwrap(),
    //     "-o",
    //     output_file.to_str().unwrap()
    // );
    // assert!(output.status.success(), "Command failed to execute");

    // let _ = run_cmd!(&test_dir, "uv", "init");
    // let _ = run_cmd!(&test_dir, "uv", "add", "pydantic");

    // let test_py_file = test_dir.join("test.py");
    // let python_file = indoc! {r#"
    //     import json
    //     from .{} import Sample

    //     data = {"field1": "value1"}
    //     sample = Sample.model_validate(data)
    //     print(json.dumps(sample.model_dump()))
    // "#};
    // fs::write(&test_py_file, python_file).expect("Failed to write test Python file");

    // let test_output = run_cmd!(&test_dir, "uv", "run", test_py_file.to_str().unwrap());
    // let stdout = String::from_utf8_lossy(&test_output.stdout);
    // let json = serde_json::from_str::<serde_json::Value>(&stdout).expect("Failed to parse JSON output");
    // assert_eq!(json["field1"], "value1");
}
