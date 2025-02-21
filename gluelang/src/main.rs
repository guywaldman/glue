use gluelang::lexer::Lexer;

fn main() {
    let code = r#"
				Person {
                    name: string,
                    age: int,
                    stats: PersonStats {
                        height_in_cm: int,
                        weight_in_kg: int
                    }
				}
				"#;
    let _tokens = Lexer::new(code).lex();
}
