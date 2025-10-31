// use gluelang::{Lexer, Parser, SemanticAnalyzer};

use gluelang::{Parser, SemanticAnalyzer, SemanticAnalyzerError, SourceCodeMetadata, print_report};

fn main() -> Result<(), Box<dyn std::error::Error>> {
    pretty_env_logger::init();

    let file_name = std::env::args().nth(1).expect("No input file specified");
    let output_mermaid_file = std::env::args().nth(2).expect("No output file specified");
    let src = std::fs::read_to_string(&file_name).expect("Failed to read input file");

    let source_code_metadata = SourceCodeMetadata {
        file_name: &file_name,
        contents: &src,
    };
    let parser = Parser::new(&source_code_metadata);
    let parsed_program = match parser.parse() {
        Ok(ast) => ast,
        Err(e) => {
            println!("{}", e);
            std::process::exit(1);
        }
    };
    let semantic_analyzer = SemanticAnalyzer::new(&parsed_program.ast);
    match semantic_analyzer.check() {
        Ok(_) => {}
        Err(errs) => {
            let mut out = String::with_capacity(2048);
            for e in errs.iter() {
                match e {
                    SemanticAnalyzerError::DuplicateField(diag) => {
                        print_report(diag).map(|s| out.push_str(&s)).expect("Rendering report failed");
                    }
                    SemanticAnalyzerError::UndefinedTypeReference(diag) => {
                        print_report(diag).map(|s| out.push_str(&s)).expect("Rendering report failed");
                    }
                }
                eprintln!("----");
                eprintln!("{out}");
            }
            std::process::exit(1);
        }
    }

    let mermaid = parsed_program.ast.to_mermaid();
    std::fs::write(&output_mermaid_file, mermaid).expect("Failed to write output file");

    Ok(())
}
