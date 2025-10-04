mod diagnostics;
mod lexer;
mod parser;
mod semantic_analysis;
mod utils;

pub use diagnostics::{LangError, LangResult};
pub use lexer::{span_of, Lexer, Span};
pub use parser::{Ast, AstNode, AstNodeId, AstNodeKind, AstNodePayload, AstSymbol, ConstantValue, Parser, PrimitiveType, SymbolTable, TreeNode, Type, TypeAtom, TypeVariant};
pub use semantic_analysis::{SemanticAnalysisArtifacts, SemanticAnalyzer};
