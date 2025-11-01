// use gluelang::{Lexer, Parser, SemanticAnalyzer};

use indoc::indoc;
use lang::{LParser, SemanticAnalyzer, SemanticAnalyzerError, SourceCodeMetadata, print_report};

fn main() -> Result<(), Box<dyn std::error::Error>> {
    pretty_env_logger::init();

    // let file_name = std::env::args().nth(1).expect("No input file specified");
    // let src = std::fs::read_to_string(&file_name).expect("Failed to read input file");
    // let source_code_metadata = SourceCodeMetadata {
    //     file_name: &file_name,
    //     file_contents: &src,
    // };

    let src = indoc! { r#"
        // This is a great model
        model Foo {
            @deprecated
            name: string
            id: string
            blah: Ba
        }

        model Bar {
            id: string
        }
        "# };
    let source_code_metadata = SourceCodeMetadata {
        file_name: "example.glue",
        file_contents: src,
    };
    let parsed = match LParser::new().parse(source_code_metadata) {
        Ok(p) => p,
        Err(e) => {
            let mut out = String::with_capacity(2048);
            print_report(e.report()).map(|s| out.push_str(&s)).expect("Rendering report failed");
            std::process::exit(1);
        }
    };

    let analyzed = match SemanticAnalyzer::new().analyze(parsed) {
        Ok(p) => p,
        Err(errs) => {
            let mut out = String::with_capacity(2048);
            for e in errs.iter() {
                print_report(e.report()).map(|s| out.push_str(&s)).expect("Rendering report failed");
            }
            std::process::exit(1);
        }
    };

    println!("{:?}", &analyzed.symbols);

    Ok(())
}
