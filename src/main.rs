use std::env;

use kekar::{
    asm_generator::AsmGenerator,
    lexer::Lexer,
    parser::{render_compatibility_diagnostic, Parser},
    workspace::analyze_workspace,
};

fn main() {
    let args: Vec<String> = env::args().collect();

    if args.len() < 2 {
        std::process::exit(69);
    }

    let mut path: Option<String> = None;
    let mut i = 1;

    while i < args.len() {
        match args[i].as_str() {
            "--target" | "-t" => {
                if i + 1 >= args.len() {
                    eprintln!("Missing value for --target");
                    std::process::exit(2);
                }
                if args[i + 1] != "asm" {
                    eprintln!(
                        "Unsupported target '{}'. Only 'asm' is supported.",
                        args[i + 1]
                    );
                    std::process::exit(2);
                }
                i += 2;
                continue;
            }
            arg if arg.starts_with("--target=") => {
                let value = arg.trim_start_matches("--target=");
                if value != "asm" {
                    eprintln!("Unsupported target '{}'. Only 'asm' is supported.", value);
                    std::process::exit(2);
                }
            }
            arg if !arg.starts_with('-') && path.is_none() => {
                path = Some(arg.to_string());
            }
            _ => {}
        }
        i += 1;
    }

    let Some(path) = path else {
        eprintln!("Usage: kekar <source.kek> [--target asm]");
        std::process::exit(2);
    };

    if let Err(errors) = analyze_workspace(&path) {
        for error in errors {
            eprintln!("{}", error.message);
        }
        std::process::exit(1);
    }

    let mut lexer = Lexer::new(&path);
    let tokens = match lexer.lex_with_diagnostics() {
        Ok(tokens) => tokens,
        Err(errors) => {
            for error in errors {
                eprintln!(
                    "{}:{}:{}: {}",
                    path, error.line, error.column, error.message
                );
            }
            std::process::exit(1);
        }
    };

    let mut parser = Parser::new(tokens);
    let ast = match parser.parse_checked() {
        Ok(ast) => ast,
        Err(errors) => {
            for error in errors {
                let near = error
                    .token
                    .map(|token| format!(" near {:?}", token))
                    .unwrap_or_default();
                eprintln!(
                    "{}: parser error at token #{}{}: {}",
                    path, error.token_index, near, error.message
                );
            }
            std::process::exit(1);
        }
    };
    for diagnostic in parser.take_compatibility_diagnostics() {
        eprintln!("{}", render_compatibility_diagnostic(&diagnostic));
    }

    let output = AsmGenerator::new().generate(&ast);

    println!("{}", output);
}
