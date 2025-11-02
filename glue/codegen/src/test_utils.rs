use lang::{AnalyzedProgram, Parser, SemanticAnalyzer, SourceCodeMetadata};

/// Test utility for analyzing a Glue file and returning the analyzed program.
pub fn analyze_glue_file(src: &'_ str) -> (AnalyzedProgram, SourceCodeMetadata<'_>) {
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
