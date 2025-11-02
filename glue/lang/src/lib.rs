mod diagnostics;
mod syntax;

mod metadata;
mod semantic_analyzer;
mod symbols;
mod utils;

pub use diagnostics::{DiagnosticContext, print_report};
pub use metadata::SourceCodeMetadata;
pub use semantic_analyzer::{AnalyzedProgram, SemanticAnalyzer, SemanticAnalyzerError};
pub use symbols::{SymEntry, SymId, SymTable};
pub use syntax::{AstNode, Decorator, Enum, Field, LNode, LSyntaxKind, Model, Parser, PrimitiveType, Type, TypeAtom};
