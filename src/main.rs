use std::env;
mod ast;
mod lexer;
mod generator;

use generator::AsmGenerator;
use kekar::{lexer::Lexer, parser::Parser};

fn main() {
    let args: Vec<String> = env::args().collect();

    if args.len() < 2 {
        std::process::exit(69);
    }

    println!("Path to file {}", args[1]);

    let mut lexer = Lexer::new(&args[1]);
    let tokens = lexer.lex_file();

    let mut parser = Parser::new(tokens);
    let ast = parser.parse();

    //dbg!(ast);
    let generator = AsmGenerator::new();
    let assembly = generator.generate_asm(ast);

    println!("{}", assembly);
}
