use std::path::PathBuf;

use clap::{Parser, Subcommand};
use gluelang::{Analyzer, AnalyzerError};
use miette::GraphicalReportHandler;

#[derive(Parser)]
struct Cli {
    #[command(subcommand)]
    command: Commands,
}

#[derive(Subcommand)]
enum Commands {
    #[command(name = "check")]
    Check {
        /// Path to the input .glue file
        input: PathBuf,
    },
}

// TODO: Don't return miette::Result from main functions, handle errors properly.
fn main() -> miette::Result<()> {
    let args = Cli::parse();
    match &args.command {
        Commands::Check { input } => {
            check(input.clone())?;
        }
    }
    Ok(())
}

fn check(file_path: PathBuf) -> miette::Result<()> {
    let file_contents = std::fs::read_to_string(&file_path)
        .map_err(|e| miette::miette!("Failed to read file {}: {}", file_path.display(), e))?;
    let file_name = file_path
        .to_str()
        .ok_or_else(|| miette::miette!("Invalid UTF-8 in file path"))?;
    let analyzer = Analyzer::new(file_name, &file_contents);
    let program = analyzer.analyze();

    let _ = match program {
        Ok(program) => program,
        Err(err) => {
            // TOOD: Use IntoDiagnostic
            handle_analyzer_errors(err)?;
            unreachable!();
        }
    };

    Ok(())
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
