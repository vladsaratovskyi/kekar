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

#[test]
fn lowers_match_to_branching_control_flow() {
    let source = r#"
fun main(): Num {
    var x: Num = 2;
    match x {
        1 => { return 10; },
        _ => { return 0; }
    }
}
"#;

    let output = compile_to_asm(source);

    assert!(output.contains("mov r13, rax"));
    assert!(output.contains("cmp r13, 1"));
    assert!(output.contains(".match_end_"));
    assert!(!output.contains("match ignored in function scope"));
}

#[test]
fn emits_struct_enum_and_impl_runtime_metadata() {
    let source = r#"
struct Point {
    x: Num;
    y: Num;
}

enum Maybe {
    Some(Num),
    Empty
}

impl Point {
    fun value() -> Num {
        return 1;
    }
}

fun main(): Num {
    return 0;
}
"#;

    let output = compile_to_asm(source);

    assert!(output.contains("Point__value:"));
    assert!(output.contains("section .rodata"));
    assert!(output.contains("__kek_struct_Point:"));
    assert!(output.contains("__kek_enum_Maybe:"));
    assert!(output.contains("__kek_impl_Point:"));
    assert!(output.contains("__kek_enum_Maybe_Some:"));
}
