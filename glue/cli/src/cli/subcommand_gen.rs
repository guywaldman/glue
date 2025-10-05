use std::path::PathBuf;

use anyhow::{Context, Result};
use clap::Subcommand;

use crate::{
    cli::{GlueCli, args::CliError},
    codegen::{CodeGenerator, JsonSchemaCodeGenerator, PythonPydanticCodeGenerator, RustSerdeCodeGenerator},
};

pub struct GenSubcommand;

#[derive(clap::Args)]
// Shared arguments for code generation commands
pub struct GenArgs {
    /// Path to the input .glue file (defaults to stdin if not provided)
    #[arg(short = 'i', long)]
    pub input: Option<PathBuf>,

    /// Output directory for generated code
    #[arg(short = 'o', long)]
    pub output: Option<PathBuf>,

    /// Optional config file for the code generator
    #[arg(short = 'c', long)]
    pub config: Option<PathBuf>,
}

#[derive(Subcommand)]
pub enum CliGenSubcommand {
    #[command(name = "jsonschema")]
    JsonSchema {
        #[command(flatten)]
        args: GenArgs,
    },
    #[command(name = "rust-serde")]
    RustSerde {
        #[command(flatten)]
        args: GenArgs,
    },
    /// Generate Python Pydantic models from a .glue file
    #[command(name = "py-pydantic")]
    PythonPydantic {
        #[command(flatten)]
        args: GenArgs,
    },
}

impl GenSubcommand {
    pub fn new() -> Self {
        Self {}
    }
    pub fn run(&self, cmd: &CliGenSubcommand) -> Result<()> {
        match cmd {
            CliGenSubcommand::JsonSchema {
                args: GenArgs { input, output, config: _, .. },
                ..
            } => {
                let (file_name, file_contents) = GlueCli::handle_file(input.clone())?;

                let artifacts = GlueCli::analyze(&file_name, file_contents)?;
                let generated_code = JsonSchemaCodeGenerator::new(artifacts).generate().map_err(CliError::CodeGen)?;
                if let Some(output) = output {
                    std::fs::write(output, generated_code).with_context(|| format!("failed to write to {}", output.display()))?;
                } else {
                    println!("{generated_code}");
                }
            }
            CliGenSubcommand::RustSerde {
                args: GenArgs { input, output, config, .. },
                ..
            } => {
                let (file_name, file_contents) = GlueCli::handle_file(input.clone())?;
                let config = GlueCli::read_config(config.as_ref())?;

                let artifacts = GlueCli::analyze(&file_name, file_contents)?;
                let generated_code = RustSerdeCodeGenerator::new(config, artifacts).generate().map_err(CliError::CodeGen)?;

                if let Some(output) = output {
                    std::fs::write(output, generated_code).with_context(|| format!("failed to write to {}", output.display()))?;
                } else {
                    println!("{generated_code}");
                }
            }
            CliGenSubcommand::PythonPydantic {
                args: GenArgs { input, output, config, .. },
                ..
            } => {
                let (file_name, file_contents) = GlueCli::handle_file(input.clone())?;
                let config = match config {
                    Some(path) => GlueCli::read_config(Some(path))?,
                    None => Default::default(),
                };

                let artifacts = GlueCli::analyze(&file_name, file_contents)?;
                let generated_code = PythonPydanticCodeGenerator::new(config, artifacts).generate().map_err(CliError::CodeGen)?;
                if let Some(output) = output {
                    std::fs::write(output, generated_code).with_context(|| format!("failed to write to {}", output.display()))?;
                } else {
                    println!("{generated_code}");
                }
            }
        }
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use indoc::indoc;

    use crate::codegen::GlueConfigSchema;

    use super::*;

    #[test]
    fn test_jsonschema_basic() {
        let src = indoc! {r#"
            @root
            model Config {
                title: string
                version: string
                debug: bool
                database: DatabaseConfig

                model DatabaseConfig {
                    host: string
                    port: int
                    username: string
                    password: string
                }
            }"#};

        let snapshot = run_snapshot_test("jsonschema_basic", src, "jsonschema").unwrap();
        insta::assert_snapshot!(snapshot)
    }

    #[test]
    fn test_python_pydantic_basic() {
        let src = indoc! {r#"
            /// A user of the system
            model User {
            /// The unique ID of the user
            id: int
            /// The user's name
            name: string
            /// The user's email address
            email: string
            /// Whether the user is active
            is_active: bool
            }

            /// A blog post
            model Post {
            /// The unique ID of the post
            id: int
            /// The title of the post
            title: string
            /// The content of the post
            content: string
            /// The author of the post
            /// Can be either the user ID or the full user object
            author: int | User

            /// Optional additional details about the post
            additional_details: AdditionalPostDetails?

            model AdditionalPostDetails {
                /// The number of likes the post has received
                likes: int
            }
            }
        "#};
        let snapshot = run_snapshot_test("python_pydantic_basic", src, "py-pydantic").unwrap();
        insta::assert_snapshot!(snapshot)
    }

    #[test]
    fn test_rust_serde_basic() {
        let src = indoc! {r#"
            /// A user of the system
            model User {
            /// The unique ID of the user
            id: int
            /// The user's name
            name: string
            /// The user's email address
            email: string
            /// The status of the user
            status: UserStatus

            enum UserStatus = "active" | "inactive" | "banned"
            }
        "#};
        let snapshot = run_snapshot_test("rust_serde_basic", src, "rust-serde").unwrap();
        insta::assert_snapshot!(snapshot)
    }

    fn run_snapshot_test(test_name: &str, glue: &str, gen_command: &str) -> Result<String> {
        let temp_dir = std::env::temp_dir();
        let test_id: u64 = rand::random();
        let test_dir_path = PathBuf::from(format!("{}/{}_{}", temp_dir.display(), test_name, test_id));

        std::fs::create_dir_all(&test_dir_path)?;
        let out_path = test_dir_path.join("out.d.ts");
        let input_path = format!("{}/in.glue", test_dir_path.display());
        std::fs::write(&input_path, glue)?;

        let mut cfg = GlueConfigSchema::default();
        cfg.generation.watermark = crate::codegen::GlueConfigSchemaGenerationWatermark::None;
        let temp_cfg_path = test_dir_path.join("config.yaml");
        let cfg_contents = serde_yaml::to_string(&cfg).unwrap();
        std::fs::write(&temp_cfg_path, cfg_contents)?;

        GlueCli::new().run(&[
            "cli",
            "gen",
            gen_command,
            "-c",
            temp_cfg_path.to_str().unwrap(),
            "-i",
            &input_path,
            "-o",
            out_path.to_str().unwrap(),
        ])?;
        let actual_out = std::fs::read_to_string(&out_path)?;
        Ok(actual_out)
    }
}
