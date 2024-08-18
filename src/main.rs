use std::env;
mod lexer;

use lexer::Lexer;


fn main() {
    let args: Vec<String> = env::args().collect();

    if args.len() < 2 {
        std::process::exit(69);
    }

    println!("Path to file {}", args[1]);
    let mut parser = Lexer::new(&args[1]);

    let tokens = parser.lex_file();

    dbg!(tokens);
}
