use anyhow::{Context, Result};

use crate::{
    cli::{
        GlueCli,
        args::{CliError, CliGenArgs, CodeGenMode},
        utils::read_config,
    },
    codegen::{CodeGenerator, JsonSchemaCodeGenerator, PythonPydanticCodeGenerator},
};

pub struct GenSubcommand;

impl GenSubcommand {
    pub fn new() -> Self {
        Self {}
    }
    pub fn run(&self, args: &CliGenArgs) -> Result<()> {
        let CliGenArgs { mode, .. } = args;
        let effective_mode = match mode {
            Some(m) => *m,
            None => {
                let extension = args.output.as_ref().and_then(|p| p.extension()).and_then(|s| s.to_str()).unwrap_or("");
                match extension {
                    "glue" => CodeGenMode::JsonSchema,
                    "json" | "yaml" | "yml" => CodeGenMode::JsonSchema,
                    // "rs" => CodeGenMode::RustSerde,
                    "py" => CodeGenMode::PythonPydantic,
                    _ => {
                        return Err(CliError::BadInput(format!("Unrecognized file extension: .{}; please specify the code generation mode explicitly", extension)).into());
                    }
                }
            }
        };
        self.generate(args, effective_mode)?;
        Ok(())
    }

    fn generate(&self, args: &CliGenArgs, effective_mode: CodeGenMode) -> Result<()> {
        let (file_name, file_contents) = GlueCli::handle_file(args.input.clone())?;

        let artifacts = GlueCli::analyze(&file_name, file_contents)?;
        // TODO: Derive default
        let config = read_config(args.config.as_ref())?;
        let generated_code = match effective_mode {
            CodeGenMode::JsonSchema => JsonSchemaCodeGenerator::new(artifacts).generate().map_err(CliError::CodeGen)?,
            // CodeGenMode::RustSerde => RustSerdeCodeGenerator::new(config, artifacts).generate().map_err(CliError::CodeGen)?,
            CodeGenMode::PythonPydantic => PythonPydanticCodeGenerator::new(config, artifacts).generate().map_err(CliError::CodeGen)?,
        };

        if let Some(output) = &args.output {
            std::fs::write(output, generated_code).with_context(|| format!("failed to write to {}", output.display()))?;
        } else {
            println!("{generated_code}");
        }
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use std::path::PathBuf;

    use indoc::indoc;

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

        let snapshot = run_snapshot_test("jsonschema_basic", src, CodeGenMode::JsonSchema).unwrap();
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
        let snapshot = run_snapshot_test("python_pydantic_basic", src, CodeGenMode::PythonPydantic).unwrap();
        insta::assert_snapshot!(snapshot)
    }

    // #[test]
    // fn test_rust_serde_basic() {
    //     let src = indoc! {r#"
    //         /// A user of the system
    //         model User {
    //         /// The unique ID of the user
    //         id: int
    //         /// The user's name
    //         name: string
    //         /// The user's email address
    //         email: string
    //         /// The status of the user
    //         status: UserStatus

    //         enum UserStatus = "active" | "inactive" | "banned"
    //         }
    //     "#};
    //     let snapshot = run_snapshot_test("rust_serde_basic", src, CodeGenMode::RustSerde).unwrap();
    //     insta::assert_snapshot!(snapshot)
    // }

    fn run_snapshot_test(test_name: &str, glue: &str, codegen_mode: CodeGenMode) -> Result<String> {
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
            // CodeGenMode::RustSerde => "rust-serde",
            CodeGenMode::PythonPydantic => "python-pydantic",
        };

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
