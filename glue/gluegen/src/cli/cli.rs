use std::{io, path::PathBuf};

use crate::{
    cli::{
        subcommand_gen::GenSubcommand,
        types::{Cli, CliAstSubcommand, CliError, CliSubcommand},
    },
    codegen::GlueConfigSchema,
};
use anyhow::{Context, Result};
use clap::Parser;
use gluelang::{Lexer, SemanticAnalysisArtifacts, SemanticAnalyzer};
use miette::GraphicalReportHandler;

pub struct GlueCli;

impl GlueCli {
    pub fn new() -> Self {
        Self {}
    }

    pub fn run(&self, cli_args: &[&str]) -> Result<()> {
        let args = Cli::parse_from(cli_args);

        match &args.command {
            CliSubcommand::Ast { command } => match command {
                CliAstSubcommand::Mermaid { input, output } => {
                    let (file_name, file_contents) = Self::handle_file(input.clone())?;

                    let artifacts = Self::check(&file_name, file_contents)?;
                    let mermaid = artifacts.ast.to_mermaid();
                    std::fs::write(output, mermaid).with_context(|| format!("failed to write to {}", output.display()))?;
                }
            },
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
                Self::check(&file_name, file_contents).map(|_| {})?;
            }
            CliSubcommand::Gen { command } => {
                GenSubcommand::new().run(command)?;
            }
        }
        Ok(())
    }

    pub fn check<T: io::BufRead>(file_name: &str, mut file_contents: T) -> Result<SemanticAnalysisArtifacts, CliError> {
        let mut buf = String::new();
        let _ = file_contents.read_to_string(&mut buf).map_err(CliError::IoError)?;
        let tokens = Lexer::new(&buf).lex();
        let parser_artifacts = gluelang::Parser::new(file_name, &buf, &tokens).parse().map_err(|e| {
            Self::report_errors(&[e.clone()]);
            CliError::CompilationError(e)
        })?;
        let semantic_analyzer_artifacts = SemanticAnalyzer::new(file_name, &buf, &parser_artifacts).analyze().map_err(|e| {
            Self::report_errors(&[e.clone()]);
            CliError::CompilationError(e)
        })?;
        Ok(semantic_analyzer_artifacts)
    }

    pub fn report_errors(errs: &[impl miette::Diagnostic]) {
        for e in errs.iter() {
            let mut out = String::new();
            GraphicalReportHandler::new().render_report(&mut out, e).expect("Rendering report failed");
            eprintln!("----");
            eprintln!("{out}");
        }
    }

    pub fn read_config(path: Option<&PathBuf>) -> Result<GlueConfigSchema> {
        let Some(path) = path else {
            return Ok(GlueConfigSchema::default());
        };
        let config_contents = std::fs::read_to_string(path).with_context(|| format!("failed to read config file '{}'", path.display()))?;
        let config = serde_yaml::from_str(&config_contents).with_context(|| format!("failed to parse config file '{}'", path.display()))?;
        Ok(config)
    }

    pub fn handle_file(input: Option<PathBuf>) -> Result<(String, Box<dyn io::BufRead>)> {
        let file_name = match &input {
            Some(path) => path.display().to_string(),
            None => "stdin".to_string(),
        };
        let file_contents: Box<dyn io::BufRead> = match &input {
            Some(path) => Box::new(io::BufReader::new(
                std::fs::File::open(path).with_context(|| format!("failed to open file '{}'", path.display()))?,
            )),
            None => Box::new(io::BufReader::new(io::stdin())),
        };
        Ok((file_name, file_contents))
    }
}
