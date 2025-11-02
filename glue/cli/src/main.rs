mod args;

use anyhow::Result;
use codegen::{CodeGenError, CodeGenJsonSchema, CodeGenPython, CodeGenRust, CodeGenerator};
use config::{GlueConfig, GlueConfigSchemaGenerationWatermark};
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
                args: CliGenArgs {
                    input,
                    output,
                    config: config_path,
                    mode,
                },
            } => {
                let (analyzed_program, source) = Self::analyze(input.clone())?;
                let codegen: Box<dyn CodeGenerator> = match mode {
                    CodeGenMode::Python => Box::new(CodeGenPython::new()),
                    CodeGenMode::JsonSchema => Box::new(CodeGenJsonSchema::new()),
                    CodeGenMode::Rust => Box::new(CodeGenRust::new()),
                };
                let config = match config_path {
                    Some(path) => {
                        let config_contents = std::fs::read_to_string(path).map_err(CliError::Io)?;
                        let ext = path.extension().and_then(|s| s.to_str()).unwrap_or("");
                        match ext {
                            "json" => GlueConfig::from_json(&config_contents).map_err(|e| CliError::CodeGen(format!("Failed to load config from JSON: {:?}", e)))?,
                            "yaml" | "yml" => GlueConfig::from_yaml(&config_contents).map_err(|e| CliError::CodeGen(format!("Failed to load config from YAML: {:?}", e)))?,
                            _ => return Err(CliError::BadInput("Config file must have .json, .yaml, or .yml extension".to_string())),
                        }
                    }
                    None => GlueConfig::default(),
                };

                let mut generated_code = match codegen.generate(analyzed_program, &source, &config) {
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

                if let Some(output) = &output {
                    // Check if the output file changed - we strip the watermark and compare
                    let output_file_contents = std::fs::read_to_string(output).map_err(CliError::Io)?;
                    let stripped_existing = Self::strip_watermark(&output_file_contents);
                    let stripped_generated = Self::strip_watermark(&generated_code);
                    if stripped_existing == stripped_generated {
                        debug!("Output file '{}' is up to date, skipping write", output.display());
                        return Ok(());
                    }
                }

                if let Ok(Some(watermark)) = Self::generate_watemark(&config, &source, output, mode) {
                    generated_code = format!("{}{}", watermark, generated_code);
                }

                Self::write_to_file_or_stdout(output, generated_code)?;
            }
        }
        Ok(())
    }

    fn strip_watermark(file_contents: &str) -> String {
        let parts: Vec<&str> = file_contents.split(Self::WATERMARK_SEPERATOR).collect();
        let stripped = parts.into_iter().nth(2).unwrap_or(file_contents);
        stripped.trim().to_string()
    }

    fn generate_watemark(config: &GlueConfig, source: &SourceCodeMetadata, output: &Option<PathBuf>, mode: &CodeGenMode) -> Result<Option<String>, CliError> {
        let timestamp = chrono::Utc::now().format("%Y-%m-%d").to_string();
        let source_relative_to_output = output
            .as_ref()
            .and_then(|output_path| {
                output_path
                    .parent()
                    .and_then(|output_dir| {
                        let output_dir = std::fs::canonicalize(output_dir).ok()?;
                        let source_path = std::fs::canonicalize(source.file_name).ok()?;
                        pathdiff::diff_paths(source_path, output_dir)
                    })
                    .map(|rel_path| rel_path.to_string_lossy().to_string())
            })
            .unwrap_or_else(|| source.file_name.to_string());
        let watermark_lines = match config.generation.watermark {
            GlueConfigSchemaGenerationWatermark::Full => {
                vec![
                    format!("Generated by Glue on {}", timestamp),
                    format!("Glue version: {}", env!("CARGO_PKG_VERSION")),
                    format!("Source: {}", source_relative_to_output),
                ]
            }
            GlueConfigSchemaGenerationWatermark::Short => {
                vec![format!("Generated by Glue on {}", timestamp), format!("Source: {}", source_relative_to_output)]
            }
            GlueConfigSchemaGenerationWatermark::None => vec![],
        };

        if watermark_lines.is_empty() {
            return Ok(None);
        }

        let mut watermark = String::new();
        let comment_prefix = match mode {
            CodeGenMode::Python => "#",
            CodeGenMode::JsonSchema => "//",
            CodeGenMode::Rust => "//",
        };
        if !watermark_lines.is_empty() {
            watermark.push_str(&format!("{} {}\n", comment_prefix, Self::WATERMARK_SEPERATOR));
        }
        for line in &watermark_lines {
            watermark.push_str(&format!("{} {}\n", comment_prefix, line));
        }
        if !watermark_lines.is_empty() {
            watermark.push_str(&format!("{} {}\n", comment_prefix, Self::WATERMARK_SEPERATOR));
            watermark.push('\n');
        }
        Ok(Some(watermark))
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

    const WATERMARK_SEPERATOR: &'static str = "------------------------------------";
}
