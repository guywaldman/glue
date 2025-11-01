mod diagnostics;
mod syntax;

mod metadata;
mod semantic_analyzer;
mod symbols;
mod utils;

pub use diagnostics::print_report;
pub use metadata::SourceCodeMetadata;
pub use semantic_analyzer::{SemanticAnalyzer, SemanticAnalyzerError};
pub use syntax::LParser;
