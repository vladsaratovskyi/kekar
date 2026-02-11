use kekar::{asm_generator::AsmGenerator, lexer::Lexer, parser::Parser};

fn compile_to_asm(source: &str) -> String {
    let mut lexer = Lexer::from_source(source);
    let tokens = lexer.lex_file();
    let mut parser = Parser::new(tokens);
    let ast = parser.parse();

    AsmGenerator::new().generate(&ast)
}

#[test]
fn generates_entrypoint_and_main_call() {
    let source = r#"
fun main(): Num {
    return 7;
}
"#;

    let output = compile_to_asm(source);

    assert!(output.contains("_start:"));
    assert!(output.contains("call main"));
    assert!(output.contains("mov rax, 60"));
    assert!(output.contains("main:"));
}

#[test]
fn generates_if_comparison_and_return_paths() {
    let source = r#"
fun main(): Num {
    var a: Num = 5;
    if a > 3 {
        return a;
    } else {
        return 0;
    }
}
"#;

    let output = compile_to_asm(source);

    assert!(output.contains("setg al"));
    assert!(output.contains("je .else_"));
    assert!(output.contains("jmp .main_epilogue_"));
}

#[test]
fn generates_function_call_with_register_arguments() {
    let source = r#"
fun add(Num a, Num b): Num {
    return a + b;
}

fun main(): Num {
    return add(3, 4);
}
"#;

    let output = compile_to_asm(source);

    assert!(output.contains("add:"));
    assert!(output.contains("main:"));
    assert!(output.contains("mov rdi, rax"));
    assert!(output.contains("mov rsi, rax"));
    assert!(output.contains("call add"));
}

#[test]
fn unrolls_for_loop_over_literal_array_with_index() {
    let source = r#"
fun main(): Num {
    var acc: Num = 0;
    for item, index in [10, 20] {
        acc = acc + item + index;
    }
    return acc;
}
"#;

    let output = compile_to_asm(source);

    assert!(output.contains("; unrolled loop iteration 0"));
    assert!(output.contains("; unrolled loop iteration 1"));
    assert!(output.contains("mov rax, 0"));
    assert!(output.contains("mov rax, 1"));
}
