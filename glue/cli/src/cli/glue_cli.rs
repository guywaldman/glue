use std::path::PathBuf;

use crate::cli::{
    args::{Cli, CliError, CliSubcommand},
    subcommand_gen::GenSubcommand,
};
use anyhow::{Context, Result};
use clap::Parser as ClapParser;
use gluelang::{ParsedProgram, Parser, SemanticAnalyzer, SourceCodeMetadata, print_report};
use log::debug;

pub struct GlueCli;

impl GlueCli {
    pub fn new() -> Self {
        Self {}
    }

    pub fn run(&self, cli_args: &[&str]) -> Result<()> {
        let args = Cli::parse_from(cli_args);

        match &args.command {
            CliSubcommand::Ast { input, output } => {
                let (file_name, file_contents) = Self::handle_file(input.clone())?;
                let source_metadata = SourceCodeMetadata {
                    file_name: &file_name,
                    contents: &file_contents,
                };
                let parsed_program = Self::analyze(&source_metadata)?;
                let mermaid = parsed_program.ast.to_mermaid();
                std::fs::write(output, mermaid).with_context(|| format!("failed to write to {}", output.display()))?;
            }
            CliSubcommand::Check { input } => {
                let (file_name, file_contents) = Self::handle_file(input.clone())?;
                let source_metadata = SourceCodeMetadata {
                    file_name: &file_name,
                    contents: &file_contents,
                };
                Self::analyze(&source_metadata).map(|_| ())?;
            }
            CliSubcommand::Gen { args } => {
                GenSubcommand::new().run(args)?;
            }
        }
        Ok(())
    }

    pub fn analyze<'a>(source_code_metadata: &'a SourceCodeMetadata<'a>) -> Result<ParsedProgram<'a>, CliError> {
        debug!("Parsing file '{}'", source_code_metadata.file_name);
        let parser = Parser::new(source_code_metadata);
        let parsed_program = match parser.parse() {
            Ok(ast) => ast,
            Err(e) => {
                print_report(&e).expect("Rendering report failed");
                return Err(CliError::ParsingError);
            }
        };
        let semantic_analyzer = SemanticAnalyzer::new(&parsed_program.ast);
        match semantic_analyzer.check() {
            Ok(_) => {}
            Err(errs) => {
                for e in errs.iter() {
                    print_report(e.report()).expect("Rendering report failed");
                }
                return Err(CliError::SemanticAnalysisError);
            }
        }
        Ok(parsed_program)
    }

    // TODO: Use BufRead instead of String for file_contents
    pub fn handle_file(input: Option<PathBuf>) -> Result<(String, String)> {
        let file_name = match &input {
            Some(path) => path.display().to_string(),
            None => "stdin".to_string(),
        };
        let file_contents = std::fs::read_to_string(input.as_deref().unwrap_or_else(|| std::path::Path::new("/dev/stdin")))?;
        Ok((file_name, file_contents))
    }
}
