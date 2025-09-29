mod codegen;

use std::{
    io::{self},
    path::PathBuf,
};

use anyhow::{Context, Result};
use clap::{Parser, Subcommand};
use gluelang::{Analyzer, AnalyzerError, Program};
use miette::GraphicalReportHandler;
use thiserror::Error;

use crate::codegen::{CodeGen, PythonPydanticCodeGen, TypeScriptDefCodeGen, TypeScriptZodCodeGen};

#[derive(Parser)]
struct Cli {
    #[command(subcommand)]
    command: CliSubcommand,
}

#[derive(Subcommand)]
enum CliSubcommand {
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
enum CliGenSubcommand {
    /// Generate Python Pydantic models from a .glue file
    #[command(name = "py-pydantic")]
    PythonPydantic {
        /// Path to the input .glue file (defaults to stdin if not provided)
        #[arg(short = 'i', long)]
        input: Option<PathBuf>,
        /// Output directory for generated code
        #[arg(short = 'o', long)]
        output: PathBuf,
    },
    /// Generate TypeScript definitions (.d.ts) from a .glue file
    #[command(name = "ts-def")]
    TypeScriptDef {
        /// Path to the input .glue file (defaults to stdin if not provided)
        #[arg(short = 'i', long)]
        input: Option<PathBuf>,
        /// Output directory for generated code
        #[arg(short = 'o', long)]
        output: PathBuf,
    },
    /// Generate TypeScript Zod schemas from a .glue file
    #[command(name = "ts-zod")]
    TypeScriptZod {
        /// Path to the input .glue file (defaults to stdin if not provided)
        #[arg(short = 'i', long)]
        input: Option<PathBuf>,
        /// Output directory for generated code
        #[arg(short = 'o', long)]
        output: PathBuf,
    },
}

#[derive(Debug, Error)]
pub enum CliError {
    #[error("I/O error")]
    IoError(#[from] std::io::Error),
    #[error("Analyzer error")]
    AnalyzerError(AnalyzerError),
    #[error("Code generation error: {0}")]
    CodeGenError(codegen::CodeGenError),
}

// TODO: Don't return miette::Result from main functions, handle errors properly.
fn main() -> Result<()> {
    let args = std::env::args().collect::<Vec<String>>();
    run_cli(&args.iter().map(String::as_str).collect::<Vec<&str>>())
}

fn run_cli(cli_args: &[&str]) -> Result<()> {
    let args = Cli::parse_from(cli_args);

    match &args.command {
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
            check(&file_name, file_contents).map(|_| {}).map_err(CliError::AnalyzerError)?;
        }
        CliSubcommand::Gen { command } => match command {
            CliGenSubcommand::PythonPydantic { input, output } => {
                let (file_name, file_contents) = handle_file(input.clone())?;

                let program = check(&file_name, file_contents).map_err(CliError::AnalyzerError)?;
                let generated_code = PythonPydanticCodeGen::new().generate(&program).map_err(CliError::CodeGenError)?;
                std::fs::write(output, generated_code).with_context(|| format!("failed to write to {}", output.display()))?;
            }
            CliGenSubcommand::TypeScriptDef { input, output } => {
                let (file_name, file_contents) = handle_file(input.clone())?;

                let program = check(&file_name, file_contents).map_err(CliError::AnalyzerError)?;
                let generated_code = TypeScriptDefCodeGen::new().generate(&program).map_err(CliError::CodeGenError)?;
                std::fs::write(output, generated_code).with_context(|| format!("failed to write to {}", output.display()))?;
            }
            CliGenSubcommand::TypeScriptZod { input, output } => {
                let (file_name, file_contents) = handle_file(input.clone())?;

                let program = check(&file_name, file_contents).map_err(CliError::AnalyzerError)?;
                let generated_code = TypeScriptZodCodeGen::new().generate(&program).map_err(CliError::CodeGenError)?;
                std::fs::write(output, generated_code)?;
            }
        },
    }
    Ok(())
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

fn check<T: io::BufRead>(file_name: &str, mut file_contents: T) -> Result<Program, AnalyzerError> {
    let mut buf = String::new();
    let _ = file_contents.read_to_string(&mut buf);
    let analyzer = Analyzer::new(file_name, &buf);
    let program = analyzer.analyze();

    let program = match program {
        Ok(program) => program,
        Err(err) => match err {
            AnalyzerError::SemanticErrors(errs) => {
                report_errors(&errs);
                return Err(AnalyzerError::SemanticErrors(errs));
            }
            AnalyzerError::ParserErrors(errs) => {
                report_errors(&errs);
                return Err(AnalyzerError::ParserErrors(errs));
            }
        },
    };
    Ok(program)
}

fn report_errors(errs: &[impl miette::Diagnostic]) {
    for e in errs.iter() {
        let mut out = String::new();
        GraphicalReportHandler::new()
            .render_report(&mut out, e)
            .expect("Rendering report failed");
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
