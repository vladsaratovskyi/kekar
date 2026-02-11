use kekar::{lexer::Lexer, parser::Parser, sema::SemanticAnalyzer};

fn analyze_source(source: &str) -> Result<(), Vec<kekar::sema::SemanticError>> {
    let mut lexer = Lexer::from_source(source);
    let tokens = lexer.lex_file();
    let mut parser = Parser::new(tokens);
    let ast = parser.parse();

    SemanticAnalyzer::analyze(&ast)
}

fn assert_has_error(source: &str, fragment: &str) {
    let result = analyze_source(source);
    assert!(result.is_err(), "Expected semantic error");

    let errors = result.err().unwrap();
    assert!(
        errors.iter().any(|e| e.message.contains(fragment)),
        "Expected error containing '{fragment}', got: {:?}",
        errors
    );
}

#[test]
fn sema_accepts_valid_loop_program() {
    let source = r#"
fun main() -> Num {
    const limit: Num = 3;
    var i: Num = 0;

    while i < limit {
        i = i + 1;
    }

    return i;
}
"#;

    let result = analyze_source(source);
    assert!(result.is_ok(), "Expected no semantic errors: {result:?}");
}

#[test]
fn sema_rejects_const_reassignment() {
    let source = r#"
fun main() -> Num {
    const a: Num = 1;
    a = 2;
    return a;
}
"#;

    assert_has_error(source, "immutable symbol 'a'");
}

#[test]
fn sema_rejects_break_outside_loop() {
    let source = r#"
fun main() -> Num {
    break;
    return 0;
}
"#;

    assert_has_error(source, "'break' used outside of loop");
}

#[test]
fn sema_rejects_continue_outside_loop() {
    let source = r#"
fun main() -> Num {
    continue;
    return 0;
}
"#;

    assert_has_error(source, "'continue' used outside of loop");
}

#[test]
fn sema_rejects_return_type_mismatch() {
    let source = r#"
fun main() -> Num {
    return true;
}
"#;

    assert_has_error(source, "Return type mismatch");
}

#[test]
fn sema_rejects_non_bool_while_condition() {
    let source = r#"
fun main() -> Num {
    while 1 {
        break;
    }
    return 0;
}
"#;

    assert_has_error(source, "While condition must be Bool");
}

#[test]
fn sema_rejects_unknown_identifier() {
    let source = r#"
fun main() -> Num {
    return missing;
}
"#;

    assert_has_error(source, "Unknown identifier 'missing'");
}

#[test]
fn sema_rejects_function_argument_type_mismatch() {
    let source = r#"
fun add(a: Num, b: Num) -> Num {
    return a + b;
}

fun main() -> Num {
    return add(1, true);
}
"#;

    assert_has_error(source, "Argument 1 for 'add' expected Num, got Bool");
}

#[test]
fn sema_rejects_duplicate_symbol_in_scope() {
    let source = r#"
fun main() -> Num {
    var a: Num = 1;
    const a: Num = 2;
    return a;
}
"#;

    assert_has_error(source, "Duplicate declaration of symbol 'a'");
}

#[test]
fn sema_rejects_return_outside_function() {
    let source = r#"
return 1;
"#;

    assert_has_error(source, "'return' used outside of function");
}
