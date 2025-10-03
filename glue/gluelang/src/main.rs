use gluelang::{Lexer, Parser, SemanticAnalyzer};
use miette::GraphicalReportHandler;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    pretty_env_logger::init();

    let file_name = std::env::args().nth(1).expect("No input file specified");
    let output_mermaid_file = std::env::args().nth(2).expect("No output file specified");

    let src = std::fs::read_to_string(&file_name).expect("Failed to read input file");
    let tokens = Lexer::new(&src).lex();
    let parser_artifacts = Parser::new(&file_name, &src, &tokens).parse().unwrap_or_else(|e| {
        report_errors(&[*e]);
        std::process::exit(1);
    });
    let semantic = SemanticAnalyzer::new(&file_name, &src, &parser_artifacts).analyze().unwrap_or_else(|e| {
        report_errors(&[*e]);
        std::process::exit(1);
    });
    if !semantic.warnings.is_empty() {
        report_errors(&semantic.warnings);
    }

    let mermaid = parser_artifacts.ast.to_mermaid();
    std::fs::write(&output_mermaid_file, mermaid).expect("Failed to write output file");

    Ok(())
}
fn report_errors<T>(errs: &[T])
where
    T: miette::Diagnostic,
{
    for e in errs.iter() {
        let mut out = String::new();
        GraphicalReportHandler::new().render_report(&mut out, e).expect("Rendering report failed");
        eprintln!("----");
        eprintln!("{out}");
    }
}
