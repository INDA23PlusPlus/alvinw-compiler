use crate::c_transpiler::transpile_to_c;
use crate::lexer::lex;
use crate::parser::Parser;
use crate::semantic_analyser::validate;

mod string_reader;
mod lexer;
mod parser;
mod semantic_analyser;
mod c_transpiler;

fn main() {
    let args: Vec<String> = std::env::args().collect();
    let file_name = args[1].trim();
    let output_name = args[2].trim();
    println!("Reading {}", file_name);
    let source = std::fs::read_to_string(file_name).expect("Failed to read file.");

    let tokens = lex(&source).expect("Syntax error");
    let mut parser = Parser::new(tokens);
    let ast = parser.parse().expect("Parsing error");
    validate(&ast).expect("Semantic error");

    println!("ast = {:#?}", ast);

    let c_code = transpile_to_c(&ast);

    std::fs::write(output_name, c_code.as_str()).expect("Failed to write.");
}
