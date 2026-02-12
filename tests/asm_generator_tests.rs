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
    assert!(output.contains("pop rsi"));
    assert!(output.contains("pop rdi"));
    assert!(output.contains("call add"));
}

#[test]
fn lowers_for_loop_over_literal_array_with_index() {
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

    assert!(output.contains(".for_loop_"));
    assert!(output.contains("cmp rcx, QWORD [rbx]"));
    assert!(output.contains("mov rax, QWORD [rbx + rcx*8 + 8]"));
    assert!(output.contains("mov rax, rcx"));
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

#[test]
fn lowers_member_access_and_member_method_call() {
    let source = r#"
struct Point {
    value: Num;
}

impl Point {
    fun get() -> Num {
        return this.value;
    }
}

fun main() -> Num {
    var p: Point = Point(42);
    return p.get();
}
"#;

    let output = compile_to_asm(source);

    assert!(output.contains("Point__get:"));
    assert!(output.contains("mov rdi,"));
    assert!(output.contains("call Point__get"));
    assert!(!output.contains("member access unsupported in asm backend"));
    assert!(!output.contains("dynamic/member call unsupported in asm backend"));
}

#[test]
fn lowers_inline_struct_method_call_without_impl_block() {
    let source = r#"
struct Driver {
    state: Num;
    fun apply() -> Num {
        return this.state;
    }
}

fun main() -> Num {
    var d: Driver = Driver(9);
    return d.apply();
}
"#;

    let output = compile_to_asm(source);

    assert!(output.contains("Driver__apply:"));
    assert!(output.contains("call Driver__apply"));
}

#[test]
fn lowers_postfix_try_operator_as_passthrough_expression() {
    let source = r#"
fun id(v: Num) -> Num {
    return v;
}

fun main() -> Num {
    return id(5)?;
}
"#;

    let output = compile_to_asm(source);

    assert!(output.contains("call id"));
    assert!(!output.contains("unsupported unary operator"));
}

#[test]
fn lowers_string_and_array_literals() {
    let source = r#"
fun main() -> Num {
    var s: String = "hello";
    var arr: Num[] = [1, 2, 3];
    return arr[1];
}
"#;

    let output = compile_to_asm(source);

    assert!(output.contains("mov QWORD [rax], 5"));
    assert!(output.contains("mov QWORD [rax+8], 104"));
    assert!(output.contains("call __kek_alloc"));
    assert!(output.contains("mov rax, QWORD [rbx + rcx*8 + 8]"));
}

#[test]
fn lowers_break_and_continue_inside_loop() {
    let source = r#"
fun main() -> Num {
    var sum: Num = 0;
    for item in [1, 2, 3] {
        if item == 2 {
            continue;
        }
        if item == 3 {
            break;
        }
        sum = sum + item;
    }
    return sum;
}
"#;

    let output = compile_to_asm(source);

    assert!(output.contains(".for_continue_"));
    assert!(output.contains(".for_end_"));
    assert!(!output.contains("break is not implemented in asm backend"));
    assert!(!output.contains("continue is not implemented in asm backend"));
}

#[test]
fn lowers_enum_constructor_and_payload_match_runtime_model() {
    let source = r#"
enum Maybe {
    Some(Num),
    Empty
}

fun unwrap(x: Maybe) -> Num {
    match x {
        Some(v) => { return v; },
        Empty() => { return 0; }
    }
    return 0;
}

fun main() -> Num {
    return unwrap(Some(7));
}
"#;

    let output = compile_to_asm(source);

    assert!(output.contains("mov rdi, 24"));
    assert!(output.contains("mov QWORD [rax+8], 1"));
    assert!(output.contains("cmp QWORD [r13], 0"));
    assert!(output.contains("mov r14, QWORD [r13+16]"));
    assert!(!output.contains("variant payload pattern checks are not represented in asm backend"));
}

#[test]
fn lowers_match_struct_enum_and_impl_without_placeholders() {
    let source = r#"
struct Point {
    x: Num;
}

enum Maybe {
    Some(Num),
    Empty
}

impl Point {
    fun wrap() -> Maybe {
        return Some(this.x);
    }
}

fun main() -> Num {
    var p: Point = Point(7);
    var s: String = "ok";
    match s {
        "ok" => { },
        _ => { }
    }

    var m: Maybe = p.wrap();
    match m {
        Some(v) => { return v; },
        Empty() => { return 0; }
    }
    return 0;
}
"#;

    let output = compile_to_asm(source);

    assert!(output.contains("Point__wrap:"));
    assert!(output.contains("call Point__wrap"));
    assert!(output.contains("__kek_struct_Point:"));
    assert!(output.contains("__kek_enum_Maybe:"));
    assert!(output.contains("__kek_impl_Point:"));
    assert!(output.contains("cmp QWORD [r13], 2"));
    assert!(output.contains("cmp QWORD [r13+8], 111"));
    assert!(output.contains("cmp QWORD [r13], 0"));
    assert!(output.contains("mov r14, QWORD [r13+16]"));
    assert!(!output.contains("unsupported literal match pattern in asm backend"));
}

#[test]
fn lowers_string_class_style_array_methods_to_runtime_calls() {
    let source = r#"
fun main() -> Num {
    var text: String = "ab";
    text = text.push('c');
    if text.is_empty() {
        return 0;
    }
    return text.len() + text.pop();
}
"#;

    let output = compile_to_asm(source);

    assert!(output.contains("call __kek_array_push"));
    assert!(output.contains("call __kek_array_is_empty"));
    assert!(output.contains("call __kek_array_len"));
    assert!(output.contains("call __kek_array_pop"));
}

#[test]
fn lowers_array_methods_to_runtime_calls() {
    let source = r#"
fun main() -> Num {
    var values: Array<Num> = [1, 2];
    values = values.push(3);
    var last: Num = values.pop();
    if values.is_empty() {
        return 0;
    }
    return values.len() + last;
}
"#;

    let output = compile_to_asm(source);

    assert!(output.contains("__kek_array_len:"));
    assert!(output.contains("__kek_array_is_empty:"));
    assert!(output.contains("__kek_array_push:"));
    assert!(output.contains("__kek_array_pop:"));
    assert!(output.contains("call __kek_array_push"));
    assert!(output.contains("call __kek_array_pop"));
    assert!(output.contains("call __kek_array_is_empty"));
    assert!(output.contains("call __kek_array_len"));
}
