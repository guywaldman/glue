use anyhow::{Context, Result};
use gluelang::{SourceCodeMetadata, print_report};

use crate::{
    cli::{
        GlueCli,
        args::{CliError, CliGenArgs, CodeGenMode},
        utils::read_config,
    },
    codegen::{CodeGenerator, JsonSchemaCodeGenerator},
};

pub struct GenSubcommand;

impl GenSubcommand {
    pub fn new() -> Self {
        Self {}
    }

    pub fn run(&self, args: &CliGenArgs) -> Result<(), CliError> {
        let CliGenArgs { mode, .. } = args;
        let effective_mode = match mode {
            Some(m) => *m,
            None => {
                let name = args.output.as_ref().and_then(|p| p.file_stem()).and_then(|s| s.to_str()).unwrap_or("");
                let extension = args.output.as_ref().and_then(|p| p.extension()).and_then(|s| s.to_str()).unwrap_or("");
                match (name, extension) {
                    (_, "glue") => CodeGenMode::JsonSchema,
                    ("openapi", "json") => CodeGenMode::OpenApi,
                    (_, "rs") => CodeGenMode::RustSerde,
                    (_, "json" | "yaml" | "yml") => CodeGenMode::JsonSchema,
                    (_, "py") => CodeGenMode::PythonPydantic,
                    _ => {
                        return Err(CliError::BadInput(format!(
                            "Unrecognized file extension: .{}; please specify the code generation mode explicitly",
                            extension
                        )));
                    }
                }
            }
        };
        match self.generate(args, effective_mode) {
            Ok(_) => {}
            Err(e) => match e {
                CliError::CodeGen(err) => {
                    print_report(&err).map_err(CliError::FormatError)?;
                    return Err(CliError::CodeGen(err));
                }
                _ => return Err(e),
            },
        }
        Ok(())
    }

    fn generate(&self, args: &CliGenArgs, effective_mode: CodeGenMode) -> Result<(), CliError> {
        let (file_name, file_contents) = GlueCli::handle_file(args.input.clone())?;
        let source_metadata = SourceCodeMetadata {
            file_name: &file_name,
            contents: &file_contents,
        };

        let parsed_program = GlueCli::analyze(&source_metadata)?;
        let ast = &parsed_program.ast;
        // TODO: Derive default
        let _config = read_config(args.config.as_ref())?;
        let generated_code = match effective_mode {
            CodeGenMode::JsonSchema => JsonSchemaCodeGenerator::new(ast).generate().map_err(|e| CliError::CodeGen(*e))?,
            // CodeGenMode::OpenApi => OpenApiCodeGenerator::new(artifacts).generate().map_err(CliError::CodeGen)?,
            _ => unimplemented!("Code generation mode not yet implemented"),
            // CodeGenMode::RustSerde => RustSerdeCodeGenerator::new(config, artifacts).generate().map_err(CliError::CodeGen)?,
            // CodeGenMode::PythonPydantic => PythonPydanticCodeGenerator::new(config, artifacts).generate().map_err(CliError::CodeGen)?,
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
    use indoc::indoc;

    use crate::cli::test_utils::run_snapshot_test;

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

    // #[test]
    // fn test_python_pydantic_basic() {
    //     let src = indoc! {r#"
    //         /// A user of the system
    //         model User {
    //             /// The unique ID of the user
    //             id: int
    //             /// The user's name
    //             name: string
    //             /// The user's email address
    //             email: string
    //             /// Whether the user is active
    //             is_active: bool
    //         }

    //         /// A blog post
    //         model Post {
    //             /// The unique ID of the post
    //             id: int
    //             /// The title of the post
    //             title: string
    //             /// The content of the post
    //             content: string
    //             /// The author of the post
    //             /// Can be either the user ID or the full user object
    //             author: int | User

    //             /// Optional additional details about the post
    //             additional_details: AdditionalPostDetails?

    //             model AdditionalPostDetails {
    //                 /// The number of likes the post has received
    //                 likes: int
    //             }
    //         }
    //     "#};
    //     let snapshot = run_snapshot_test("python_pydantic_basic", src, CodeGenMode::PythonPydantic).unwrap();
    //     insta::assert_snapshot!(snapshot)
    // }

    // #[test]
    // fn test_openapi_basic() {
    //     let src = indoc! {r#"
    //         @endpoint("GET /users/{id}")
    //         endpoint GetUserById {
    //             request: {
    //                 id: int | string
    //             }

    //             headers: {
    //                 "X-Request-ID": string
    //             }

    //             @response(mime="application/json")
    //             responses: {
    //                 /// Successful response containing user data.
    //                 200: User
    //                 /// Client error response.
    //                 4XX: ErrorResponse
    //                 /// Server error response.
    //                 @response(mime="application/xml")
    //                 5XX: ErrorResponse
    //             }
    //         }

    //         model User {
    //             id: int | string
    //             name: string
    //         }

    //         model ErrorResponse {
    //             message: string
    //         }
    //     "#};

    //     let snapshot = run_snapshot_test("openapi_basic", src, CodeGenMode::OpenApi).unwrap();
    //     insta::assert_snapshot!(snapshot)
    // }

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
}
