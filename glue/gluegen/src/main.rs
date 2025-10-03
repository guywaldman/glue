mod codegen;

use std::{
    io::{self},
    path::PathBuf,
};

use anyhow::{Context, Result};
use clap::{Parser, Subcommand};
use gluelang::{LangError, Lexer, Parser as GlueParser, SemanticAnalysisArtifacts, SemanticAnalyzer};
use miette::GraphicalReportHandler;
use thiserror::Error;

use crate::codegen::{CodeGenerator, GlueConfigSchema, JsonSchemaCodeGenerator, PythonPydanticCodeGenerator, RustSerdeCodeGenerator};

#[derive(Parser)]
struct Cli {
    #[command(subcommand)]
    command: CliSubcommand,
}

#[derive(Subcommand)]
enum CliSubcommand {
    /// Operations related to .glue ASTs.
    #[command(name = "ast")]
    Ast {
        #[command(subcommand)]
        command: CliAstSubcommand,
    },
    #[command(name = "check")]
    Check {
        /// Path to the input .glue file (defaults to stdin if not provided)
        input: Option<PathBuf>,
    },

    Gen {
        #[command(subcommand)]
        command: CliGenSubcommand,
    },
}

#[derive(Subcommand)]
enum CliAstSubcommand {
    /// Generate a Mermaid diagram from a .glue file
    #[command(name = "mermaid")]
    Mermaid {
        /// Path to the input .glue file (defaults to stdin if not provided)
        #[arg(short = 'i', long)]
        input: Option<PathBuf>,
        /// Output directory for generated code
        #[arg(short = 'o', long)]
        output: PathBuf,
    },
}

#[derive(clap::Args)]
// Shared arguments for code generation commands
struct GenArgs {
    /// Path to the input .glue file (defaults to stdin if not provided)
    #[arg(short = 'i', long)]
    input: Option<PathBuf>,

    /// Output directory for generated code
    #[arg(short = 'o', long)]
    output: PathBuf,

    /// Optional config file for the code generator
    #[arg(short = 'c', long)]
    config: Option<PathBuf>,
}

#[derive(Subcommand)]
enum CliGenSubcommand {
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

#[derive(Debug, Error)]
pub enum CliError {
    #[error("Failed to read config file")]
    ConfigReadError(#[from] anyhow::Error),
    #[error("I/O error")]
    IoError(#[from] std::io::Error),
    #[error("Compilation error")]
    CompilationError(LangError),
    #[error("Code generation error: {0}")]
    CodeGenError(codegen::CodeGenError),
}

// TODO: Don't return miette::Result from main functions, handle errors properly.
fn main() -> Result<()> {
    pretty_env_logger::init();

    let args = std::env::args().collect::<Vec<String>>();
    run_cli(&args.iter().map(String::as_str).collect::<Vec<&str>>())
}

fn run_cli(cli_args: &[&str]) -> Result<()> {
    let args = Cli::parse_from(cli_args);

    match &args.command {
        CliSubcommand::Ast { command } => match command {
            CliAstSubcommand::Mermaid { input, output } => {
                let (file_name, file_contents) = handle_file(input.clone())?;

                let artifacts = check(&file_name, file_contents)?;
                let mermaid = artifacts.ast.to_mermaid();
                std::fs::write(output, mermaid).with_context(|| format!("failed to write to {}", output.display()))?;
            }
        },
        CliSubcommand::Check { input } => {
            let file_name = match input {
                Some(path) => path.display().to_string(),
                None => "stdin".to_string(),
            };
            let file_contents: Box<dyn io::BufRead> = match input {
                // TODO: Handle file open errors
                Some(path) => Box::new(io::BufReader::new(std::fs::File::open(path).unwrap())),
                None => Box::new(io::BufReader::new(io::stdin())),
            };
            check(&file_name, file_contents).map(|_| {})?;
        }
        CliSubcommand::Gen { command } => match command {
            CliGenSubcommand::JsonSchema {
                args: GenArgs { input, output, config: _, .. },
                ..
            } => {
                let (file_name, file_contents) = handle_file(input.clone())?;

                let artifacts = check(&file_name, file_contents)?;
                let generated_code = JsonSchemaCodeGenerator::new(artifacts).generate().map_err(CliError::CodeGenError)?;
                std::fs::write(output, generated_code).with_context(|| format!("failed to write to {}", output.display()))?;
            }
            CliGenSubcommand::RustSerde {
                args: GenArgs { input, output, config: _, .. },
                ..
            } => {
                let (file_name, file_contents) = handle_file(input.clone())?;

                let artifacts = check(&file_name, file_contents)?;
                let generated_code = RustSerdeCodeGenerator::new(artifacts).generate().map_err(CliError::CodeGenError)?;
                std::fs::write(output, generated_code).with_context(|| format!("failed to write to {}", output.display()))?;
            }
            CliGenSubcommand::PythonPydantic {
                args: GenArgs { input, output, config, .. },
                ..
            } => {
                let (file_name, file_contents) = handle_file(input.clone())?;
                let config = config
                    .clone()
                    .map(|ref path| read_config(path))
                    .transpose()
                    .map_err(CliError::ConfigReadError)?
                    .unwrap_or_default();

                let artifacts = check(&file_name, file_contents)?;
                let generated_code = PythonPydanticCodeGenerator::new(config, artifacts).generate().map_err(CliError::CodeGenError)?;
                std::fs::write(output, generated_code).with_context(|| format!("failed to write to {}", output.display()))?;
            }
        },
    }
    Ok(())
}

fn read_config(path: &PathBuf) -> Result<GlueConfigSchema> {
    let config_contents = std::fs::read_to_string(path).with_context(|| format!("failed to read config file '{}'", path.display()))?;
    let config = serde_yaml::from_str(&config_contents).with_context(|| format!("failed to parse config file '{}'", path.display()))?;
    Ok(config)
}

fn handle_file(input: Option<PathBuf>) -> Result<(String, Box<dyn io::BufRead>)> {
    let file_name = match &input {
        Some(path) => path.display().to_string(),
        None => "stdin".to_string(),
    };
    let file_contents: Box<dyn io::BufRead> = match &input {
        Some(path) => Box::new(io::BufReader::new(
            std::fs::File::open(path).with_context(|| format!("failed to open file '{}'", path.display()))?,
        )),
        None => Box::new(io::BufReader::new(io::stdin())),
    };
    Ok((file_name, file_contents))
}

fn check<'a, T: io::BufRead>(file_name: &str, mut file_contents: T) -> Result<SemanticAnalysisArtifacts, CliError> {
    let mut buf = String::new();
    let _ = file_contents.read_to_string(&mut buf).map_err(CliError::IoError)?;
    let tokens = Lexer::new(&buf).lex();
    let parser_artifacts = GlueParser::new(file_name, &buf, &tokens).parse().map_err(|e| {
        report_errors(&[e.clone()]);
        CliError::CompilationError(e)
    })?;
    let semantic_analyzer_artifacts = SemanticAnalyzer::new(file_name, &buf, &parser_artifacts).analyze().map_err(|e| {
        report_errors(&[e.clone()]);
        CliError::CompilationError(e)
    })?;
    Ok(semantic_analyzer_artifacts)
}

fn report_errors(errs: &[impl miette::Diagnostic]) {
    for e in errs.iter() {
        let mut out = String::new();
        GraphicalReportHandler::new().render_report(&mut out, e).expect("Rendering report failed");
        eprintln!("----");
        eprintln!("{out}");
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    const USER_AND_POST_GLUE: &str = indoc::indoc! {r#"
        /// A user of the system
        model User {
          /// The unique ID of the user
          id: int
          /// The user's name
          name: string
          /// The user's email address
          email: string
          /// Whether the user is active
          #[field(alias="isActive")]
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

    #[test]
    fn test_python_pydantic_basic() {
        let snapshot = run_snapshot_test("python_pydantic_basic", USER_AND_POST_GLUE, "py-pydantic").unwrap();
        insta::assert_snapshot!(snapshot)
    }

    #[test]
    fn test_typescript_zod_basic() {
        let snapshot = run_snapshot_test("typescript_zod_basic", USER_AND_POST_GLUE, "ts-zod").unwrap();
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
        run_cli(&["gluegen", "gen", gen_command, "-i", &input_path, "-o", out_path.to_str().unwrap()])?;
        let actual_out = std::fs::read_to_string(&out_path)?;
        Ok(actual_out)
    }
}
