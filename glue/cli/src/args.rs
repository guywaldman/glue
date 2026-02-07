use std::path::PathBuf;

use clap::{Parser, Subcommand};
use codegen::CodeGenMode;

#[derive(Parser)]
#[command(version, name = "glue", about = "Glue Language CLI")]
pub struct Cli {
    #[command(subcommand)]
    pub command: CliSubcommand,
}

#[derive(clap::Args)]
pub struct CliGenArgs {
    /// Path to the input .glue file (defaults to stdin if not provided)
    #[arg(short = 'i', long)]
    pub input: Option<PathBuf>,

    /// Output directory for generated code
    #[arg(short = 'o', long)]
    pub output: Option<PathBuf>,

    /// Code generation mode
    #[arg(value_enum)]
    pub mode: CodeGenMode,

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
        input: Option<PathBuf>,
        /// Output directory for generated code. If not provided, prints to stdout
        #[arg(short = 'o', long)]
        output: Option<PathBuf>,
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
