use convert_case::Casing;
use lang::symbol_name_to_parts;

#[allow(dead_code)]
pub fn indent_lines(input: &str, spaces: usize) -> String {
    let indent = " ".repeat(spaces);
    input.lines().map(|line| format!("{}{}", indent, line)).collect::<Vec<String>>().join("\n")
}

pub fn qualified_symbol_name_to_case(symbol_name: &str, case: convert_case::Case) -> String {
    symbol_name_to_parts(symbol_name).join("_").to_case(case)
}
