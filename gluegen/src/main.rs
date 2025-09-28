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

// TODO: Don't return miette::Result from main functions, handle errors properly.
fn main() -> Result<(), Box<dyn Error>> {
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
            check(&file_name, file_contents).map(|_| {})?;
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

                let program = check(&file_name, file_contents)?;
                let generated_code = TypeScriptCodeGen::new().generate(&program)?;
                std::fs::write(output, generated_code)?;
            }
        },
    }
    Ok(())
}

fn check(file_name: &str, mut file_contents: impl io::BufRead) -> miette::Result<Program> {
    let mut buf = String::new();
    let _ = file_contents.read_to_string(&mut buf);
    let analyzer = Analyzer::new(file_name, &buf);
    let program = analyzer.analyze();

    let program = match program {
        Ok(program) => program,
        Err(err) => {
            // TOOD: Use IntoDiagnostic
            handle_analyzer_errors(err)?;
            unreachable!();
        }
    };
    Ok(program)
}

fn handle_analyzer_errors(err: AnalyzerError) -> miette::Result<()> {
    match err {
        AnalyzerError::SemanticErrors(errs) => {
            let handler = GraphicalReportHandler::new();
            for e in errs.iter().skip(1) {
                let mut out = String::new();
                handler.render_report(&mut out, e).unwrap();
                eprintln!("{out}");
            }
            Err(miette::Report::new(errs.into_iter().next().unwrap()))
        }
        AnalyzerError::ParserErrors(errs) => {
            let handler = GraphicalReportHandler::new();
            for e in errs.iter().skip(1) {
                let mut out = String::new();
                handler.render_report(&mut out, e).unwrap();
                eprintln!("{out}");
            }
            Err(miette::Report::new(errs.into_iter().next().unwrap()))
        }
    }
}
