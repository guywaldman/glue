mod args;

use anyhow::Result;
use codegen::{CodeGenError, CodeGenJsonSchema, CodeGenPython, CodeGenerator};
use std::{io::Read, path::PathBuf};
use thiserror::Error;

use clap::Parser as ClapParser;
use lang::{AnalyzedProgram, Parser, SemanticAnalyzer, SourceCodeMetadata, print_report};
use log::debug;

use crate::args::{Cli, CliGenArgs, CliSubcommand, CodeGenMode};

#[derive(Debug, Error)]
pub enum CliError {
    #[error("Bad input: {0}")]
    BadInput(String),
    #[error("I/O error: {0}")]
    Io(#[from] std::io::Error),
    #[error("Formatting error")]
    FormatError(#[from] std::fmt::Error),
    #[error("Compilation error")]
    ParsingError,
    #[error("Semantic analysis error")]
    SemanticAnalysisError,
    #[error("Code generation error: {0}")]
    CodeGen(String),
}

fn main() -> Result<()> {
    pretty_env_logger::init();

    let args = std::env::args().collect::<Vec<String>>();
    let args_str: Vec<&str> = args.iter().map(|s| s.as_str()).collect();
    GlueCli::new().run(&args_str)?;

    Ok(())
}

struct GlueCli;

impl GlueCli {
    pub fn new() -> Self {
        Self {}
    }

    pub fn run(&self, cli_args: &[&str]) -> Result<(), CliError> {
        let args = Cli::parse_from(cli_args);

        match &args.command {
            CliSubcommand::Ast { input, output } => {
                let (analyzed_program, _) = Self::analyze(input.clone())?;
                let root = analyzed_program.ast_root;
                Self::write_to_file_or_stdout(output, format!("{:#?}", root))?;
                match output {
                    Some(output_path) => {
                        std::fs::write(output_path, format!("{:#?}", root))?;
                    }
                    None => {
                        println!("{:#?}", root);
                    }
                }
            }
            CliSubcommand::Check { input } => {
                let _analyzed_program = Self::analyze(input.clone())?;
            }
            CliSubcommand::Gen {
                args: CliGenArgs { input, output, config: _, mode },
            } => {
                let mode = match mode {
                    CodeGenMode::Python | CodeGenMode::JsonSchema => mode,
                    _ => {
                        return Err(CliError::BadInput("Only Python code generation is currently supported".to_string()));
                    }
                };
                _ = mode;
                let (analyzed_program, source) = Self::analyze(input.clone())?;
                let codegen: Box<dyn CodeGenerator> = match mode {
                    CodeGenMode::Python => Box::new(CodeGenPython::new()),
                    CodeGenMode::JsonSchema => Box::new(CodeGenJsonSchema::new()),
                    _ => {
                        return Err(CliError::BadInput("Only Python code generation is currently supported".to_string()));
                    }
                };
                let generated_code = match codegen.generate(analyzed_program, &source) {
                    Ok(code) => code,
                    Err(CodeGenError::GenerationErrors(diags)) => {
                        for diag in diags {
                            print_report(&diag).expect("Rendering report failed");
                        }
                        return Err(CliError::CodeGen("Code generation failed with errors".to_string()));
                    }
                    Err(e) => {
                        return Err(CliError::CodeGen(format!("Code generation failed: {:?}", e)));
                    }
                };
                Self::write_to_file_or_stdout(output, generated_code)?;
            }
        }
        Ok(())
    }

    pub fn analyze<'a>(input: Option<PathBuf>) -> Result<(AnalyzedProgram, SourceCodeMetadata<'a>), CliError> {
        let source = Self::handle_file(input.clone())?;
        debug!("Parsing file '{}'", source.file_name);
        let parsed_program = match Parser::new().parse(&source) {
            Ok(ast) => ast,
            Err(e) => {
                print_report(e.report()).expect("Rendering report failed");
                return Err(CliError::ParsingError);
            }
        };
        match SemanticAnalyzer::new().analyze(&parsed_program, &source) {
            Ok(analyzed_program) => Ok((analyzed_program, source)),
            Err(errs) => {
                for e in errs.iter() {
                    print_report(e.report()).expect("Rendering report failed");
                }
                Err(CliError::SemanticAnalysisError)
            }
        }
    }

    fn write_to_file_or_stdout(output: &Option<PathBuf>, content: String) -> Result<(), CliError> {
        match output {
            Some(path) => {
                std::fs::write(path, content).map_err(CliError::Io)?;
            }
            None => {
                println!("{}", content);
            }
        }
        Ok(())
    }

    // TODO: Use BufRead instead of String for file_contents
    pub fn handle_file<'a>(input: Option<PathBuf>) -> Result<SourceCodeMetadata<'a>, CliError> {
        let file_name = match &input {
            Some(path) => path.display().to_string(),
            None => "stdin".to_string(),
        };
        let file_contents = {
            if let Some(input) = input {
                std::fs::read_to_string(input).map_err(CliError::Io)?
            } else {
                let mut buffer = String::new();
                std::io::stdin().read_to_string(&mut buffer)?;
                buffer
            }
        };
        Ok(SourceCodeMetadata {
            file_name: Box::leak(file_name.into_boxed_str()),
            file_contents: Box::leak(file_contents.into_boxed_str()),
        })
    }
}
