mod diagnostics;
mod syntax;

mod metadata;
mod semantic_analyzer;
mod builtin_decorators;
mod symbols;
mod utils;

pub use diagnostics::{DiagnosticContext, print_report};
pub use metadata::SourceCodeMetadata;
pub use semantic_analyzer::{AnalyzedProgram, SemanticAnalyzer, SemanticAnalyzerError};
pub use symbols::{SymEntry, SymId, SymTable, symbol_name_to_parts};
pub use syntax::{AstNode, Decorator, Enum, EnumVariant, Field, LNode, LSyntaxKind, Literal, LiteralExpr, Model, Parser, PrimitiveType, Type, TypeAtom};
