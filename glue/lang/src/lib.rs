// TODO: Remove warning suppressions.
#![allow(dead_code, unused_imports, clippy::new_without_default)]

mod diagnostics;
mod syntax;

mod builtin_decorators;
mod glue_ir;
mod metadata;
mod semantic_analyzer;
mod symbols;
mod utils;

pub use builtin_decorators::{BUILTIN_DECORATORS, DecoratorArgDef, DecoratorDef, MODEL_FIELD_DECORATOR, MODEL_FIELD_DECORATOR_ALIAS_ARG, MODEL_FIELD_DECORATOR_EXAMPLE_ARG};
pub use diagnostics::{Diagnostic, DiagnosticContext, DiagnosticSeverity, generate_report, generate_reports, print_report};
pub use glue_ir::{GlueIr, GlueIrError, GlueIrNode, GlueIrNodeKind, GlueIrSpan, diagnostic_to_ir_error, parser_error_to_ir_error, semantic_error_to_ir_error};
pub use metadata::SourceCodeMetadata;
pub use rowan::{TextRange, TextSize, TokenAtOffset};
pub use semantic_analyzer::{AnalyzedProgram, SemanticAnalyzer, SemanticAnalyzerError};
pub use symbols::{SymEntry, SymId, SymTable, symbol_name_to_parts};
pub use syntax::{
    AnonModel, AstNode, AstVisitor, ConstExprType, Decorator, DecoratorArg, Endpoint, Enum, EnumVariant, Field, ImportNamedItem, ImportStmt, LNode, LSyntaxKind, Literal, LiteralExpr, Model, Parser,
    ParserError, PrimitiveType, RootNode, Type, TypeAlias, TypeAtom,
};
