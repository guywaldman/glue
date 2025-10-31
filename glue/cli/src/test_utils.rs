use std::path::PathBuf;

use crate::cli::{CodeGenMode, GlueCli, read_config};
use anyhow::Result;
use gluelang::{ParsedProgram, Parser, SourceCodeMetadata};

pub fn parse<'a>(source: &'a str) -> Result<ParsedProgram<'a>> {
    let source_code_metadata = Box::new(SourceCodeMetadata {
        file_name: "test.glue",
        contents: source,
    });
    let parsed_program = Parser::new(Box::leak(source_code_metadata)).parse().unwrap();
    Ok(parsed_program)
}
