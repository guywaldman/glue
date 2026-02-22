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
    /// Path or URL to the input .glue file (defaults to stdin if not provided)
    #[arg(short = 'i', long, conflicts_with = "input_positional")]
    pub input: Option<PathBuf>,

    /// Path or URL to the input .glue file (positional alternative to --input)
    #[arg(index = 2, conflicts_with = "input")]
    pub input_positional: Option<PathBuf>,

    /// Output directory for generated code
    #[arg(short = 'o', long)]
    pub output: Option<PathBuf>,

    /// Code generation mode
    #[arg(value_enum)]
    pub mode: CodeGenMode,

    /// Path to a custom config file. If not specified, the generator will automatically use `.gluerc`, `.gluerc.json`, or `.gluerc.yaml` from the input file's directory when available.
    #[arg(short = 'c', long)]
    pub config: Option<PathBuf>,

    /// Inline config override in the format `<path>=<value>` (repeatable)
    #[arg(long = "set", value_name = "PATH=VALUE")]
    pub set: Vec<String>,

    /// Inline config override in the format `<path>=<value>` where value is always treated as a string (repeatable)
    #[arg(long = "set-string", value_name = "PATH=VALUE")]
    pub set_string: Vec<String>,
}

#[derive(Subcommand)]
pub enum CliSubcommand {
    /// Checks for validity of a Glue file
    #[command(name = "check")]
    Check {
        /// Path or URL to the input .glue file (defaults to stdin if not provided)
        input: Option<PathBuf>,
    },

    /// Generates code from a Glue file
    Gen {
        #[command(flatten)]
        args: CliGenArgs,
    },

    /// Emits Glue IR as JSON
    #[command(name = "ast")]
    Ast {
        /// Path or URL to the input .glue file (defaults to stdin if not provided)
        #[arg(short = 'i', long, conflicts_with = "input_positional")]
        input: Option<PathBuf>,

        /// Path or URL to the input .glue file (positional alternative to --input)
        #[arg(index = 1, conflicts_with = "input")]
        input_positional: Option<PathBuf>,
    },
}
