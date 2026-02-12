use std::env;

use kekar::{
    asm_generator::AsmGenerator,
    generator::JsGenerator,
    lexer::Lexer,
    parser::{render_compatibility_diagnostic, Parser},
};

fn main() {
    let args: Vec<String> = env::args().collect();

    if args.len() < 2 {
        std::process::exit(69);
    }

    let mut target = "js".to_string();
    let mut path: Option<String> = None;
    let mut i = 1;

    while i < args.len() {
        match args[i].as_str() {
            "--target" | "-t" => {
                if i + 1 >= args.len() {
                    eprintln!("Missing value for --target");
                    std::process::exit(2);
                }
                target = args[i + 1].clone();
                i += 2;
                continue;
            }
            arg if arg.starts_with("--target=") => {
                target = arg.trim_start_matches("--target=").to_string();
            }
            arg if !arg.starts_with('-') && path.is_none() => {
                path = Some(arg.to_string());
            }
            _ => {}
        }
        i += 1;
    }

    let Some(path) = path else {
        eprintln!("Usage: kekar <source.kek> [--target js|asm]");
        std::process::exit(2);
    };

    let mut lexer = Lexer::new(&path);
    let tokens = lexer.lex_file();

    let mut parser = Parser::new(tokens);
    let ast = parser.parse();
    for diagnostic in parser.take_compatibility_diagnostics() {
        eprintln!("{}", render_compatibility_diagnostic(&diagnostic));
    }

    let output = match target.as_str() {
        "js" => JsGenerator::new().generate(&ast),
        "asm" => AsmGenerator::new().generate(&ast),
        other => {
            eprintln!("Unsupported target '{}'. Use 'js' or 'asm'.", other);
            std::process::exit(2);
        }
    };

    println!("{}", output);
}
