mod lexer;
mod parser;
mod semantic;
mod utils;

// TODO: Abstract analyzed program to something more intuitive.
pub use parser::{Annotation, AnnotationArgument, Program};
pub use semantic::{Analyzer, AnalyzerError};
