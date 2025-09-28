mod codegen;

use std::{
    error::Error,
    io::{self},
    path::PathBuf,
};

use clap::{Parser, Subcommand};
use gluelang::{Analyzer, AnalyzerError, Program};
use miette::GraphicalReportHandler;

use crate::codegen::{CodeGen, TypeScriptCodeGen};

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
    /// Generate TypeScript definitions from a .glue file
    #[command(name = "ts")]
    TypeScript {
        /// Path to the input .glue file (defaults to stdin if not provided)
        #[arg(short = 'i', long)]
        input: Option<PathBuf>,
        /// Output directory for generated code
        #[arg(short = 'o', long)]
        output: PathBuf,
    },
}

#[derive(Debug, thiserror::Error)]
pub enum CliError {
    #[error("I/O error")]
    IoError(std::io::Error),
    #[error("Analyzer error")]
    AnalyzerError(AnalyzerError),
    #[error("Code generation error: {0}")]
    CodeGenError(codegen::CodeGenError),
}

// TODO: Don't return miette::Result from main functions, handle errors properly.
fn main() -> Result<(), CliError> {
    let args = Cli::parse();
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
            check(&file_name, file_contents)
                .map(|_| {})
                .map_err(CliError::AnalyzerError)?;
        }
        CliSubcommand::Gen { command } => match command {
            CliGenSubcommand::TypeScript { input, output } => {
                let file_name = match input {
                    Some(path) => path.display().to_string(),
                    None => "stdin".to_string(),
                };
                let file_contents: Box<dyn io::BufRead> = match input {
                    // TODO: Handle file open errors
                    Some(path) => Box::new(io::BufReader::new(std::fs::File::open(path).unwrap())),
                    None => Box::new(io::BufReader::new(io::stdin())),
                };

                let program = check(&file_name, file_contents).map_err(CliError::AnalyzerError)?;
                let generated_code = TypeScriptCodeGen::new()
                    .generate(&program)
                    .map_err(CliError::CodeGenError)?;
                std::fs::write(output, generated_code).map_err(CliError::IoError)?;
            }
        },
    }
    Ok(())
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
