// use gluelang::{Lexer, Parser, SemanticAnalyzer};
// use miette::GraphicalReportHandler;

use gluelang::GlueParser;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    pretty_env_logger::init();

    let file_name = std::env::args().nth(1).expect("No input file specified");
    let output_mermaid_file = std::env::args().nth(2).expect("No output file specified");
    let src = std::fs::read_to_string(&file_name).expect("Failed to read input file");

    let parsed_program = match GlueParser::new().parse(&src) {
        Ok(ast) => ast,
        Err(e) => {
            println!("{}", e);
            std::process::exit(1);
        }
    };

    let mermaid = parsed_program.ast.to_mermaid();
    std::fs::write(&output_mermaid_file, mermaid).expect("Failed to write output file");

    Ok(())
}
// }

// fn report_errors<T>(errs: &[T])
// where
//     T: miette::Diagnostic,
// {
//     for e in errs.iter() {
//         let mut out = String::new();
//         GraphicalReportHandler::new().render_report(&mut out, e).expect("Rendering report failed");
//         eprintln!("----");
//         eprintln!("{out}");
//     }
// }
