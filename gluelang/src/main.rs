use indoc::indoc;

mod lexer;
mod parser;

fn main() -> miette::Result<()> {
    let src = indoc! {"
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
        user: int | @User
        /// The content of the post
        content: string
        /// An optional URL to an image associated with the post
        image_url: string
    }
    "};

    let tokens = lexer::Lexer::new(src).lex();
    let mut parser = parser::Parser::new("sample.rs", src, tokens);
    let program = parser.parse_program()?;

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
    Ok(())
}
