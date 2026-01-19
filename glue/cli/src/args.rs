use std::path::PathBuf;

use clap::{Parser, Subcommand, ValueEnum};

#[derive(Parser)]
#[command(version, name = "glue", about = "Glue Language CLI")]
pub struct Cli {
    #[command(subcommand)]
    pub command: CliSubcommand,
}

pub const CODEGEN_MODE_JSONSCHEMA: &str = "jsonschema";
pub const CODEGEN_MODE_OPENAPI: &str = "openapi";
pub const CODEGEN_MODE_RUST: &str = "rust";
pub const CODEGEN_MODE_PYTHON: &str = "python";
pub const CODEGEN_MODE_PROTOBUF: &str = "protobuf";

#[derive(Debug, Clone, Copy, ValueEnum, PartialEq, Eq)]
pub enum CodeGenMode {
    #[value(alias(CODEGEN_MODE_JSONSCHEMA))]
    JsonSchema,
    #[value(alias(CODEGEN_MODE_OPENAPI))]
    OpenApi,
    #[value(alias(CODEGEN_MODE_RUST))]
    Rust,
    #[value(alias(CODEGEN_MODE_PYTHON))]
    Python,
    #[value(alias(CODEGEN_MODE_PROTOBUF))]
    Protobuf,
}

impl From<CodeGenMode> for &str {
    fn from(val: CodeGenMode) -> Self {
        match val {
            CodeGenMode::JsonSchema => CODEGEN_MODE_JSONSCHEMA,
            CodeGenMode::OpenApi => CODEGEN_MODE_OPENAPI,
            CodeGenMode::Rust => CODEGEN_MODE_RUST,
            CodeGenMode::Python => CODEGEN_MODE_PYTHON,
            CodeGenMode::Protobuf => CODEGEN_MODE_PROTOBUF,
        }
    }
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
