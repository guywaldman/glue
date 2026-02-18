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

    /// Path to a custom config file. If not specified, the generator will automatically use `.gluerc`, `.gluerc.json`, or `.gluerc.yaml` from the input file's directory when available.
    #[arg(short = 'c', long)]
    pub config: Option<PathBuf>,
}

#[derive(Subcommand)]
pub enum CliSubcommand {
    /// Checks for validity of a Glue file
    #[command(name = "check")]
    Check {
        /// Path to the input .glue file (defaults to stdin if not provided)
        input: Option<PathBuf>,
    },

    /// Generates code from a Glue file
    Gen {
        #[command(flatten)]
        args: CliGenArgs,
    },
}
