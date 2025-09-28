use gluelang::{Analyzer, AnalyzerError};
use indoc::indoc;
use miette::GraphicalReportHandler;

fn main() -> miette::Result<()> {
    let src = indoc! {r#"
    
    /// Fetch user information by their ID.
    #[endpoint(method="GET", path="/users/{id}")]
    endpoint {
        /// The user's unique identifier
        @path id
        /// Whether to include verbose information. False by default
        @query verbose: bool?
        /// A request ID for tracing
        @header X-Request-ID: int

        response {
            @status 200
            @mime application/json
            @header X-Response-ID: string
            body: #User
        }
    }

    /// A user in the system.
    model User {
        /// The user's unique identifier
        id: int
        /// The user's name
        name: string
        /// The user's email address
        email: string
        /// Whether the user is active
        is_active: bool
    }

    /// A post made by a user.
    /// It can contain text and an optional image
    model Post {
        /// The post's unique identifier
        id: #User
        /// Either the ID of the user who made the post, or its data
        user: int | #User
        /// The content of the post
        content: string
        /// An optional URL to an image associated with the post
        image_url: string
    }
    "#};
    let analyzer = Analyzer::new("example.glue", src);
    let program = analyzer.analyze();

    let _ = match program {
        Ok(program) => program,
        Err(err) => {
            // TOOD: Use IntoDiagnostic
            handle_analyzer_errors(err)?;
            unreachable!();
        }
    };

    println!("!!!");

    Ok(())
}

fn handle_analyzer_errors(err: AnalyzerError) -> miette::Result<()> {
    match err {
        AnalyzerError::SemanticErrors(errs) => {
            let handler = GraphicalReportHandler::new();
            for e in errs.iter().skip(1) {
                let mut out = String::new();
                handler.render_report(&mut out, e).unwrap();
                eprintln!("{out}");
            }
            Err(miette::Report::new(errs.into_iter().next().unwrap()))
        }
        AnalyzerError::ParserErrors(errs) => {
            let handler = GraphicalReportHandler::new();
            for e in errs.iter().skip(1) {
                let mut out = String::new();
                handler.render_report(&mut out, e).unwrap();
                eprintln!("{out}");
            }
            Err(miette::Report::new(errs.into_iter().next().unwrap()))
        }
    }
}
