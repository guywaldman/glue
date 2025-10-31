use std::path::PathBuf;

use clap::{Parser, Subcommand, ValueEnum};
use thiserror::Error;

use crate::codegen::CodeGenError;

#[derive(Parser)]
#[command(version, name = "glue", about = "Glue Language CLI")]
pub struct Cli {
    #[command(subcommand)]
    pub command: CliSubcommand,
}

#[derive(Debug, Clone, Copy, ValueEnum, PartialEq, Eq)]
pub enum CodeGenMode {
    #[value(alias("json-schema"))]
    JsonSchema,
    #[value(alias("openapi"))]
    OpenApi,
    #[value(alias("rust-serde"))]
    RustSerde,
    #[value(alias("python-pydantic"))]
    PythonPydantic,
}

#[derive(clap::Args)]
// Shared arguments for code generation commands
pub struct CliGenArgs {
    /// Path to the input .glue file (defaults to stdin if not provided)
    #[arg(short = 'i', long)]
    pub input: Option<PathBuf>,

    /// Output directory for generated code
    #[arg(short = 'o', long)]
    pub output: Option<PathBuf>,

    /// Code generation mode. If this is not provided, it will be inferred from the output file extension
    /// (e.g., `.json` for JSON Schema, `.rs` for Rust Serde, `.py` for Python Pydantic)
    #[arg(value_enum)]
    pub mode: Option<CodeGenMode>,

    /// Optional config file for the code generator
    #[arg(short = 'c', long)]
    pub config: Option<PathBuf>,
}

#[derive(Subcommand)]
pub enum CliSubcommand {
    /// Outputs the AST in Mermaid format
    #[command(name = "ast")]
    Ast {
        /// Path to the input .glue file (defaults to stdin if not provided)
        #[arg(short = 'i', long)]
        input: Option<PathBuf>,
        /// Output directory for generated code
        #[arg(short = 'o', long)]
        output: PathBuf,
    },

    /// Checks for validity of the Glue file
    #[command(name = "check")]
    Check {
        /// Path to the input .glue file (defaults to stdin if not provided)
        input: Option<PathBuf>,
    },

    /// Generates code from the Glue file
    Gen {
        #[command(flatten)]
        args: CliGenArgs,
    },
}

#[derive(Debug, Error)]
pub enum CliError {
    #[error("Failed to read config file")]
    ConfigRead(#[from] anyhow::Error),
    #[error("Bad input: {0}")]
    BadInput(String),
    #[error("I/O error")]
    Io(#[from] std::io::Error),
    #[error("Formatting error")]
    FormatError(#[from] std::fmt::Error),
    #[error("Compilation error")]
    ParsingError,
    #[error("Semantic analysis error")]
    SemanticAnalysisError,
    #[error("Code generation error")]
    CodeGen(miette::Report),
}
