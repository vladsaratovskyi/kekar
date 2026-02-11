use kekar::{generator::JsGenerator, lexer::Lexer, parser::Parser};

fn compile_to_js(source: &str) -> String {
    let mut lexer = Lexer::from_source(source);
    let tokens = lexer.lex_file();
    let mut parser = Parser::new(tokens);
    let ast = parser.parse();

    JsGenerator::new().generate(&ast)
}

#[test]
fn generates_function_with_if_and_empty_return() {
    let source = r#"
fun main(): Num {
    var a: Num = 5;
    if a > 0 {
        return a;
    }
    return;
}
"#;

    let output = compile_to_js(source);

    assert!(output.contains("function main() {"));
    assert!(output.contains("let a = 5;"));
    assert!(output.contains("if ((a > 0)) {"));
    assert!(output.contains("return a;"));
    assert!(output.contains("return;"));
}

#[test]
fn generates_for_loop_without_index() {
    let source = r#"
fun main(): Num {
    var sum: Num = 0;
    for item in [1, 2, 3] {
        sum = sum + item;
    }
    return sum;
}
"#;

    let output = compile_to_js(source);

    assert!(output.contains("for (const item of [1, 2, 3]) {"));
    assert!(output.contains("sum = (sum + item);"));
}

#[test]
fn generates_member_computed_and_call_expressions() {
    let source = r#"
fun get(Num i): Num {
    return i;
}

fun main(): Num {
    var idx: Num = get(1);
    this.items[idx] = idx;
    return idx;
}
"#;

    let output = compile_to_js(source);

    assert!(output.contains("function get(i) {"));
    assert!(output.contains("let idx = get(1);"));
    assert!(output.contains("this.items[idx] = idx;"));
}

#[test]
fn generates_class_fields_and_methods() {
    let source = r#"
class Counter {
    var value: Num;

    fun init() {
        this.value = 1;
    }
}
"#;

    let output = compile_to_js(source);

    assert!(output.contains("class Counter {"));
    assert!(output.contains("value;"));
    assert!(output.contains("init() {"));
    assert!(output.contains("this.value = 1;"));
}
