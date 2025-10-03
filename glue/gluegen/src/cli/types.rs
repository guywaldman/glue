use std::path::PathBuf;

use clap::{Parser, Subcommand};
use gluelang::LangError;
use thiserror::Error;

use crate::{cli::subcommand_gen::CliGenSubcommand, codegen::CodeGenError};

#[derive(Parser)]
pub struct Cli {
    #[command(subcommand)]
    pub command: CliSubcommand,
}

#[derive(Subcommand)]
pub enum CliSubcommand {
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
pub enum CliAstSubcommand {
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

#[derive(Debug, Error)]
pub enum CliError {
    #[error("Failed to read config file")]
    ConfigReadError(#[from] anyhow::Error),
    #[error("I/O error")]
    IoError(#[from] std::io::Error),
    #[error("Compilation error")]
    CompilationError(LangError),
    #[error("Code generation error: {0}")]
    CodeGenError(CodeGenError),
}
