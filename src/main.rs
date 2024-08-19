use std::env;
mod lexer;
mod parser;

use lexer::Lexer;

fn main() {
    let args: Vec<String> = env::args().collect();

    if args.len() < 2 {
        std::process::exit(69);
    }

    println!("Path to file {}", args[1]);
    let mut lexer = Lexer::new(&args[1]);

    let tokens = lexer.lex_file();

    dbg!(tokens);
}
