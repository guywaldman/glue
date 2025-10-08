use log::debug;

const EXAMPLES_DIR: &str = concat!(env!("CARGO_MANIFEST_DIR"), "/examples");
// const BINARY: &str = concat!(env!("CARGO_MANIFEST_DIR"), "/../target/release/cli");

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
    let dir = std::path::Path::new(EXAMPLES_DIR).join("python_pydantic");
    debug!("Running e2e test in dir: {}", dir.display());
    run_cmd!(&dir, "sh", "generate.sh");
    let res = run_cmd!(&dir, "uv", "run", "src/main.py");
    let stdout = String::from_utf8_lossy(&res.stdout);
    insta::assert_snapshot!(stdout);
}

// #[test]
// fn test_e2e_cli_rust_serde() {
//     let dir = std::path::Path::new(EXAMPLES_DIR).join("rust_serde");
//     debug!("Running e2e test in dir: {}", dir.display());
//     run_cmd!(&dir, "sh", "generate.sh");
//     let res = run_cmd!(&dir, "cargo", "run", "--quiet");
//     let stdout = String::from_utf8_lossy(&res.stdout);
//     insta::assert_snapshot!(stdout);
// }
