use lang::{AnalyzedProgram, Parser, SemanticAnalyzer, SourceCodeMetadata, print_report};

use crate::{CodeGenError, CodeGenerator};

pub fn analyze_test_glue_file(src: &'_ str) -> (AnalyzedProgram, SourceCodeMetadata<'_>) {
    let source_code_metadata = SourceCodeMetadata {
        file_name: "mock.glue",
        file_contents: src,
    };
    let parsed = Parser::new()
        .parse(&source_code_metadata)
        .map_err(|e| {
            panic!("Parsing error: {}", e.report());
        })
        .unwrap();
    let analyzed = SemanticAnalyzer::new()
        .analyze(&parsed, &source_code_metadata)
        .map_err(|errs| {
            let mut messages = Vec::new();
            for e in errs.iter() {
                messages.push(format!("Semantic analysis error: {}", e.report()));
            }
            panic!("{}", messages.join("\n"));
        })
        .unwrap();

    (analyzed, source_code_metadata)
}

pub fn gen_test(codegen: &dyn CodeGenerator, src: &str) -> String {
    let (program, source) = analyze_test_glue_file(src);
    codegen.generate(program, &source, None).unwrap_or_else(|e| match e {
        CodeGenError::InternalError(msg) => panic!("Internal error: {}", msg),
        CodeGenError::GenerationError(diag) => {
            print_report(&diag).expect("Failed to print diagnostic");
            panic!("Generation error occurred");
        }
        CodeGenError::GenerationErrors(diags) => {
            for diag in diags {
                print_report(&diag).expect("Failed to print diagnostic");
            }
            panic!("Generation errors occurred");
        }
        e => panic!("Unexpected error: {:?}", e),
    })
}
