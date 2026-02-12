use std::env;

use kekar::{asm_generator::AsmGenerator, workspace::build_workspace_program};

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

    let ast = match build_workspace_program(&path) {
        Ok(program) => program,
        Err(errors) => {
            for error in errors {
                eprintln!("{}", error.message);
            }
            std::process::exit(1);
        }
    };

    if ast.stmts.is_empty() {
        eprintln!("No code was generated from workspace '{}'", path);
        std::process::exit(1);
    }

    let output = AsmGenerator::new().generate(&ast);

    println!("{}", output);
}
