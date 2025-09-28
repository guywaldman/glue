use indoc::indoc;
use miette::GraphicalReportHandler;

use crate::parser::AnnotationArgument;

mod lexer;
mod parser;
mod semantic;
mod utils;

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
    // INTERNAL NOTE: ...
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
        id: int
        /// Either the ID of the user who made the post, or its data
        user: int | #User
        /// The content of the post
        content: string
        /// An optional URL to an image associated with the post
        image_url: string
    }
    "#};

    // Lex
    let tokens = lexer::Lexer::new(src).lex();
    // Parse
    let mut parser = parser::Parser::new("sample.rs", src, tokens);
    let program = parser.parse_program()?;
    // Run semantic analysis
    let analyzer = semantic::Analyzer::new("sample.rs", src);
    let semantic_errors = analyzer.analyze(&program);

    // Print semantic errors
    if !semantic_errors.is_empty() {
        eprintln!("[ERROR] Found {} semantic errors:", semantic_errors.len());
        let handler = GraphicalReportHandler::new();
        for e in semantic_errors.iter().skip(1) {
            let mut out = String::new();
            handler.render_report(&mut out, e).unwrap();
            eprintln!("{out}");
        }
        return Err(miette::Report::new(
            semantic_errors.into_iter().next().unwrap(),
        ));
    }

    // Print program nicely
    for model in program.models {
        println!("Model: {}", model.name);
        if let Some(doc) = model.doc {
            print!("  Doc:");
            for line in doc.lines() {
                print!(" {line}");
            }
            println!();
        }
        for field in model.fields {
            println!("  Field: {}: {}", field.name, field.ty);
            if let Some(doc) = &field.doc {
                println!("  Doc: {doc}");
            }
        }
    }

    for endpoint in program.endpoints {
        println!("Endpoint block:");
        if let Some(doc) = endpoint.doc {
            print!("  Doc:");
            for line in doc.lines() {
                print!(" {line}");
            }
            println!();
        }
        if let Some(ann) = endpoint.annotation {
            print!("  Annotation: {}", ann.name);
            let args_str = ann
                .args
                .iter()
                .map(|arg| match arg {
                    AnnotationArgument::NamedArg { name, value } => format!("{name} = {value}"),
                    AnnotationArgument::PositionalArg { value, .. } => value.to_string(),
                })
                .collect::<Vec<_>>()
                .join(", ");
            if !args_str.is_empty() {
                print!(" ({args_str})");
            }
            println!();
        }

        for response in endpoint.responses {
            println!("  Response:");
            for field in response {
                print!("    Field: {field}");
                println!();
                if let Some(doc) = &field.doc {
                    println!("    Doc: {doc}");
                }
            }
        }
    }
    Ok(())
}
