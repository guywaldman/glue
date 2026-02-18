use anyhow::Result;
use codegen::{CodeGen, CodeGenError, CodeGenMode};
use config::{GlueConfig, GlueConfigSchemaGenConfig, GlueConfigSchemaGeneration, GlueConfigSchemaGenerationWatermark};
use globset::{Glob, GlobSetBuilder};
use std::{
    io::Read,
    path::{Path, PathBuf},
};
use thiserror::Error;

use clap::Parser as ClapParser;
use lang::{AnalyzedProgram, Parser, SemanticAnalyzer, SourceCodeMetadata, print_report};
use log::debug;

use crate::args::{Cli, CliGenArgs, CliSubcommand};

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

pub struct GlueCli;

impl GlueCli {
    pub fn run(&self, cli_args: &[&str]) -> Result<(), CliError> {
        let args = Cli::parse_from(cli_args);

        match &args.command {
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
                let mut output = output.clone();
                let config = match config_path {
                    Some(path) => {
                        let config_contents = std::fs::read_to_string(path).map_err(CliError::Io)?;
                        let ext = path.extension().and_then(|s| s.to_str()).unwrap_or("");
                        match ext {
                            "json" => Some(GlueConfig::from_json(&config_contents).map_err(|e| CliError::CodeGen(format!("Failed to load config from JSON: {:?}", e)))?),
                            "yaml" | "yml" => Some(GlueConfig::from_yaml(&config_contents).map_err(|e| CliError::CodeGen(format!("Failed to load config from YAML: {:?}", e)))?),
                            _ => return Err(CliError::BadInput("Config file must have .json, .yaml, or .yml extension".to_string())),
                        }
                    }
                    None => None,
                };

                let (resolved_config, resolved_output) = match (config.as_ref(), config_path.as_ref()) {
                    (Some(config), Some(config_path)) => Self::resolve_generation_config(config, config_path, input.as_ref())?,
                    _ => (None, None),
                };

                if output.is_none() {
                    output = resolved_output;
                }

                let codegen_mode = *mode;
                let source = Self::handle_file(input.clone())?;
                let mut generated_code = match CodeGen::generate(codegen_mode, &source, resolved_config.clone()) {
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
                    // Strip watermarks before comparing to detect actual content changes
                    if let Ok(output_file_contents) = std::fs::read_to_string(output) {
                        let stripped_existing = Self::strip_watermark(&output_file_contents);
                        let stripped_generated = Self::strip_watermark(&generated_code);
                        if stripped_existing == stripped_generated {
                            debug!("Output file '{}' is up to date, skipping write", output.display());
                            return Ok(());
                        }
                    }
                }

                if let Ok(Some(watermark)) = Self::generate_watemark(config_path, resolved_config, &source, &output, mode) {
                    generated_code = format!("{}{}", watermark, generated_code);
                }

                Self::write_to_file_or_stdout(&output, generated_code)?;
            }
        }
        Ok(())
    }

    fn strip_watermark(file_contents: &str) -> String {
        let parts: Vec<&str> = file_contents.split(Self::WATERMARK_SEPERATOR).collect();
        let stripped = parts.into_iter().nth(2).unwrap_or(file_contents);
        stripped.trim().to_string()
    }

    fn generate_watemark(
        config_path: &Option<PathBuf>,
        config: Option<GlueConfigSchemaGeneration>,
        source: &SourceCodeMetadata,
        output: &Option<PathBuf>,
        mode: &CodeGenMode,
    ) -> Result<Option<String>, CliError> {
        let mut watermark_mode = config.and_then(|gen_cfg| gen_cfg.watermark).unwrap_or(GlueConfigSchemaGenerationWatermark::Short);
        if mode.is_json_format() {
            watermark_mode = GlueConfigSchemaGenerationWatermark::None;
        }
        let timestamp = chrono::Utc::now().format("%Y-%m-%d").to_string();
        let source_relative_to_output = output
            .as_ref()
            .and_then(|output_path| {
                output_path
                    .parent()
                    .and_then(|output_dir| {
                        let output_dir = std::fs::canonicalize(output_dir).ok()?;
                        let source_path = PathBuf::from(source.file_name);
                        let source_path = std::fs::canonicalize(source_path).ok()?;
                        pathdiff::diff_paths(source_path, output_dir)
                    })
                    .map(|rel_path| rel_path.to_string_lossy().to_string())
            })
            .unwrap_or_else(|| source.file_name.to_string());

        let mut watermark_lines = vec![];
        if watermark_mode != GlueConfigSchemaGenerationWatermark::None {
            watermark_lines.push(format!("Generated by Glue on {}", timestamp));
            if watermark_mode == GlueConfigSchemaGenerationWatermark::Full {
                watermark_lines.push(format!("Glue version: {}", env!("CARGO_PKG_VERSION")));
            }
            watermark_lines.push(format!("Source: {}", source_relative_to_output));
            if let Some(config_path) = config_path {
                let config_path_relative_to_output = output
                    .as_ref()
                    .and_then(|output_path| {
                        output_path
                            .parent()
                            .and_then(|output_dir| {
                                let output_dir = std::fs::canonicalize(output_dir).ok()?;
                                let source_path = PathBuf::from(source.file_name);
                                let source_path = std::fs::canonicalize(source_path).ok()?;
                                pathdiff::diff_paths(source_path, output_dir)
                            })
                            .map(|rel_path| rel_path.to_string_lossy().to_string())
                    })
                    .unwrap_or_else(|| config_path.display().to_string());
                watermark_lines.push(format!("Config: {}", config_path_relative_to_output));
            }
        }

        if watermark_lines.is_empty() {
            return Ok(None);
        }

        let mut watermark = String::new();
        let comment_prefix = mode.comment_prefix();
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
                if let Some(parent) = path.parent() {
                    std::fs::create_dir_all(parent).map_err(|e| {
                        log::error!("Failed to create parent directories for '{}': {}", path.display(), e);
                        CliError::Io(e)
                    })?;
                }
                std::fs::write(path, content).map_err(|e| {
                    log::error!("Failed to write to output file '{}': {}", path.display(), e);
                    CliError::Io(e)
                })?;
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
                std::fs::read_to_string(input).map_err(|e| {
                    log::error!("Failed to read input file '{}': {}", file_name, e);
                    CliError::Io(e)
                })?
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

    fn resolve_generation_config(config: &GlueConfig, config_path: &Path, input_path: Option<&PathBuf>) -> Result<(Option<GlueConfigSchemaGeneration>, Option<PathBuf>), CliError> {
        let config_dir = config_path.parent().unwrap_or_else(|| Path::new("."));
        let global_config = config.global.as_ref().and_then(|g| g.config.clone());
        let output_base_dir = config.global.as_ref().and_then(|g| g.output_base_dir.clone());

        let matched = match input_path {
            Some(path) => Self::match_gen_entry(config, config_dir, path)?,
            None => None,
        };

        let overrides = matched.as_ref().and_then(|entry| entry.config_overrides.clone());
        let resolved_config = Self::merge_generation_config(global_config, overrides);

        let output_template = matched.and_then(|entry| entry.output);
        let resolved_output = Self::resolve_output_path(output_template.as_deref(), output_base_dir.as_deref(), input_path, config_dir);

        Ok((resolved_config, resolved_output))
    }

    fn match_gen_entry(config: &GlueConfig, config_dir: &Path, input_path: &Path) -> Result<Option<GlueConfigSchemaGenConfig>, CliError> {
        let Some(entries) = &config.r#gen else {
            return Ok(None);
        };

        for entry in entries {
            if Self::glob_matches(&entry.files, config_dir, input_path)? {
                return Ok(Some(entry.clone()));
            }
        }

        Ok(None)
    }

    fn glob_matches(pattern: &str, config_dir: &Path, input_path: &Path) -> Result<bool, CliError> {
        let glob = Glob::new(pattern).map_err(|e| CliError::BadInput(format!("Invalid glob pattern '{}': {}", pattern, e)))?;
        let mut builder = GlobSetBuilder::new();
        builder.add(glob);
        let set = builder.build().map_err(|e| CliError::BadInput(format!("Invalid glob pattern '{}': {}", pattern, e)))?;

        let target_path = if input_path.is_absolute() {
            input_path.strip_prefix(config_dir).unwrap_or(input_path)
        } else {
            input_path
        };

        Ok(set.is_match(target_path))
    }

    fn merge_generation_config(base: Option<GlueConfigSchemaGeneration>, overrides: Option<GlueConfigSchemaGeneration>) -> Option<GlueConfigSchemaGeneration> {
        match (base, overrides) {
            (None, None) => None,
            (Some(base), None) => Some(base),
            (None, Some(overrides)) => Some(overrides),
            (Some(base), Some(overrides)) => Some(GlueConfigSchemaGeneration {
                lint_suppressions: overrides.lint_suppressions.or(base.lint_suppressions),
                watermark: overrides.watermark.or(base.watermark),
                python: Self::merge_python_config(base.python, overrides.python),
                rust: Self::merge_rust_config(base.rust, overrides.rust),
                typescript: Self::merge_typescript_config(base.typescript, overrides.typescript),
                go: Self::merge_go_config(base.go, overrides.go),
                protobuf: Self::merge_protobuf_config(base.protobuf, overrides.protobuf),
            }),
        }
    }

    fn merge_python_config(base: Option<config::GlueConfigSchemaGenerationPython>, overrides: Option<config::GlueConfigSchemaGenerationPython>) -> Option<config::GlueConfigSchemaGenerationPython> {
        match (base, overrides) {
            (None, None) => None,
            (Some(base), None) => Some(base),
            (None, Some(overrides)) => Some(overrides),
            (Some(base), Some(overrides)) => Some(config::GlueConfigSchemaGenerationPython {
                data_model_library: overrides.data_model_library.or(base.data_model_library),
                base_model: overrides.base_model.or(base.base_model),
            }),
        }
    }

    fn merge_rust_config(base: Option<config::GlueConfigSchemaGenerationRust>, overrides: Option<config::GlueConfigSchemaGenerationRust>) -> Option<config::GlueConfigSchemaGenerationRust> {
        match (base, overrides) {
            (None, None) => None,
            (Some(base), None) => Some(base),
            (None, Some(overrides)) => Some(overrides),
            (Some(base), Some(overrides)) => Some(config::GlueConfigSchemaGenerationRust {
                include_yaml: overrides.include_yaml.or(base.include_yaml),
            }),
        }
    }

    fn merge_go_config(base: Option<config::GlueConfigSchemaGenerationGo>, overrides: Option<config::GlueConfigSchemaGenerationGo>) -> Option<config::GlueConfigSchemaGenerationGo> {
        match (base, overrides) {
            (None, None) => None,
            (Some(base), None) => Some(base),
            (None, Some(overrides)) => Some(overrides),
            (Some(base), Some(overrides)) => Some(config::GlueConfigSchemaGenerationGo {
                package_name: overrides.package_name.or(base.package_name),
            }),
        }
    }

    fn merge_protobuf_config(
        base: Option<config::GlueConfigSchemaGenerationProtobuf>,
        overrides: Option<config::GlueConfigSchemaGenerationProtobuf>,
    ) -> Option<config::GlueConfigSchemaGenerationProtobuf> {
        match (base, overrides) {
            (None, None) => None,
            (Some(base), None) => Some(base),
            (None, Some(overrides)) => Some(overrides),
            (Some(base), Some(overrides)) => Some(config::GlueConfigSchemaGenerationProtobuf {
                package_name: overrides.package_name.or(base.package_name),
            }),
        }
    }

    fn merge_typescript_config(
        base: Option<config::GlueConfigSchemaGenerationTypeScript>,
        overrides: Option<config::GlueConfigSchemaGenerationTypeScript>,
    ) -> Option<config::GlueConfigSchemaGenerationTypeScript> {
        match (base, overrides) {
            (None, None) => None,
            (Some(base), None) => Some(base),
            (None, Some(overrides)) => Some(overrides),
            (Some(base), Some(overrides)) => Some(config::GlueConfigSchemaGenerationTypeScript { zod: overrides.zod.or(base.zod) }),
        }
    }

    fn resolve_output_path(output_template: Option<&str>, output_base_dir: Option<&str>, input_path: Option<&PathBuf>, config_dir: &Path) -> Option<PathBuf> {
        let template = output_template?;
        let mut output = template.to_string();

        if let Some(input_path) = input_path {
            let file_name = input_path.file_stem().and_then(|s| s.to_str()).unwrap_or("");
            let file_ext = input_path.extension().and_then(|s| s.to_str()).unwrap_or("");
            output = output.replace("{file_name}", file_name).replace("{file_ext}", file_ext);
        }

        let output_path = PathBuf::from(output);
        if output_path.is_absolute() {
            return Some(output_path);
        }

        let base_dir = output_base_dir.map(|base| {
            let base_path = PathBuf::from(base);
            if base_path.is_absolute() { base_path } else { config_dir.join(base_path) }
        });

        Some(match base_dir {
            Some(base_dir) => base_dir.join(output_path),
            None => config_dir.join(output_path),
        })
    }

    const WATERMARK_SEPERATOR: &'static str = "------------------------------------";
}
