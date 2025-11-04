// TODO: Remove warning suppressions.
#![allow(dead_code, unused_imports, clippy::new_without_default)]

mod diagnostics;
mod syntax;

mod builtin_decorators;
mod metadata;
mod semantic_analyzer;
mod symbols;
mod utils;

pub use builtin_decorators::{BUILTIN_DECORATORS, MODEL_FIELD_DECORATOR, MODEL_FIELD_DECORATOR_ALIAS_ARG};
pub use diagnostics::{DiagnosticContext, print_report};
pub use metadata::SourceCodeMetadata;
pub use semantic_analyzer::{AnalyzedProgram, SemanticAnalyzer, SemanticAnalyzerError};
pub use symbols::{SymEntry, SymId, SymTable, symbol_name_to_parts};
pub use syntax::{AstNode, Decorator, DecoratorArg, Enum, EnumVariant, Field, LNode, LSyntaxKind, Literal, LiteralExpr, Model, Parser, PrimitiveType, Type, TypeAtom};
