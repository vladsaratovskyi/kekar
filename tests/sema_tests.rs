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

#[test]
fn sema_accepts_resolved_mod_use_import() {
    let source = r#"
mod core;
import System as Sys from "../src/system.kek";
use core::fmt;
use Sys::io;

fun main() -> Num {
    return 0;
}
"#;

    let result = analyze_source(source);
    assert!(result.is_ok(), "Expected no semantic errors: {result:?}");
}

#[test]
fn sema_rejects_unresolved_use_root() {
    let source = r#"
use missing::io;

fun main() -> Num {
    return 0;
}
"#;

    assert_has_error(source, "Unresolved use path root 'missing'");
}

#[test]
fn sema_rejects_pub_use_of_private_root() {
    let source = r#"
mod internal;
pub use internal::api;

fun main() -> Num {
    return 0;
}
"#;

    assert_has_error(source, "Cannot publicly re-export private root 'internal'");
}

#[test]
fn sema_accepts_pub_use_of_public_root() {
    let source = r#"
pub mod api;
pub use api::client;

fun main() -> Num {
    return 0;
}
"#;

    let result = analyze_source(source);
    assert!(result.is_ok(), "Expected no semantic errors: {result:?}");
}

#[test]
fn sema_rejects_unknown_struct_field_type() {
    let source = r#"
struct User {
    id: Missing;
}

fun main() -> Num {
    return 0;
}
"#;

    assert_has_error(source, "Unknown type in field 'User.id' type");
}

#[test]
fn sema_rejects_impl_for_undeclared_type() {
    let source = r#"
impl Ghost {
    fun value() -> Num {
        return 0;
    }
}

fun main() -> Num {
    return 0;
}
"#;

    assert_has_error(source, "Impl target type 'Ghost' is not declared");
}

#[test]
fn sema_rejects_duplicate_impl_methods() {
    let source = r#"
struct Point {
    x: Num;
}

impl Point {
    fun len() -> Num {
        return 1;
    }

    fun len() -> Num {
        return 2;
    }
}

fun main() -> Num {
    return 0;
}
"#;

    assert_has_error(source, "Duplicate method 'len' in impl 'Point'");
}

#[test]
fn sema_rejects_public_method_on_private_type() {
    let source = r#"
struct Hidden {
    value: Num;
}

impl Hidden {
    pub fun expose() -> Num {
        return 0;
    }
}

fun main() -> Num {
    return 0;
}
"#;

    assert_has_error(
        source,
        "Cannot expose public method 'expose' on private type 'Hidden'",
    );
}

#[test]
fn sema_rejects_local_pub_declaration() {
    let source = r#"
fun main() -> Num {
    pub const VALUE: Num = 1;
    return VALUE;
}
"#;

    assert_has_error(
        source,
        "'pub' is only allowed on top-level declarations and impl methods",
    );
}

#[test]
fn sema_rejects_match_pattern_type_mismatch() {
    let source = r#"
fun main() -> Num {
    var value: Num = 1;
    match value {
        true => { return 1; },
        _ => { return 0; }
    }
}
"#;

    assert_has_error(source, "Match pattern type mismatch");
}

#[test]
fn sema_rejects_non_exhaustive_bool_match() {
    let source = r#"
fun main() -> Num {
    var flag: Bool = true;
    match flag {
        true => { return 1; }
    }
    return 0;
}
"#;

    assert_has_error(
        source,
        "Non-exhaustive match for Bool: expected true and false arms",
    );
}

#[test]
fn sema_rejects_non_exhaustive_enum_match() {
    let source = r#"
enum Maybe {
    Some(Num),
    Empty
}

fun eval(x: Maybe) -> Num {
    match x {
        Some(v) => { return v; }
    }
    return 0;
}

fun main() -> Num {
    return 0;
}
"#;

    assert_has_error(source, "Non-exhaustive match for enum 'Maybe'");
}

#[test]
fn sema_rejects_variant_pattern_argument_type_mismatch() {
    let source = r#"
enum Maybe {
    Some(Num),
    Empty
}

fun eval(x: Maybe) -> Num {
    match x {
        Some(true) => { return 1; },
        Empty() => { return 0; }
    }
    return 0;
}

fun main() -> Num {
    return 0;
}
"#;

    assert_has_error(
        source,
        "Match pattern type mismatch: expected Num, got Bool",
    );
}

#[test]
fn sema_accepts_exhaustive_enum_match() {
    let source = r#"
enum Maybe {
    Some(Num),
    Empty
}

fun eval(x: Maybe) -> Num {
    match x {
        Some(v) => { return v; },
        Empty() => { return 0; }
    }
    return 0;
}

fun main() -> Num {
    return 0;
}
"#;

    let result = analyze_source(source);
    assert!(result.is_ok(), "Expected no semantic errors: {result:?}");
}
