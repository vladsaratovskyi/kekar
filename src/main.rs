use std::env;
mod lexer;
mod parser;
mod ast;

use lexer::Lexer;
use parser::Parser;

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

    dbg!(ast);
}
