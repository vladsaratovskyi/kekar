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
fn sema_accepts_impl_method_call_on_typed_receiver() {
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
    var p: Point;
    return p.get();
}
"#;

    let result = analyze_source(source);
    assert!(result.is_ok(), "Expected no semantic errors: {result:?}");
}

#[test]
fn sema_accepts_inline_struct_method_call_on_typed_receiver() {
    let source = r#"
struct Point {
    x: Num;
    fun get() -> Num {
        return this.x;
    }
}

fun main() -> Num {
    var p: Point = Point(1);
    return p.get();
}
"#;

    let result = analyze_source(source);
    assert!(result.is_ok(), "Expected no semantic errors: {result:?}");
}

#[test]
fn sema_rejects_impl_method_call_argument_type_mismatch() {
    let source = r#"
struct Point {
    value: Num;
}

impl Point {
    fun set(v: Num) -> Num {
        return v;
    }
}

fun main() -> Num {
    var p: Point;
    return p.set(true);
}
"#;

    assert_has_error(
        source,
        "Argument 0 for method 'Point.set' expected Num, got Bool",
    );
}

#[test]
fn sema_rejects_unknown_impl_method_call() {
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
    var p: Point;
    return p.missing();
}
"#;

    assert_has_error(source, "Unknown method 'Point.missing'");
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

#[test]
fn sema_rejects_duplicate_module_import_binding() {
    let source = r#"
mod core;
import Other as core from "../src/other.kek";

fun main() -> Num {
    return 0;
}
"#;

    assert_has_error(source, "Duplicate module/import/use binding 'core'");
}

#[test]
fn sema_rejects_duplicate_use_binding_name() {
    let source = r#"
mod api;
use api::io;
use api::io;

fun main() -> Num {
    return 0;
}
"#;

    assert_has_error(source, "Duplicate module/import/use binding 'io'");
}

#[test]
fn sema_rejects_public_impl_on_private_type() {
    let source = r#"
struct Hidden {
    value: Num;
}

pub impl Hidden {
    fun value() -> Num {
        return 1;
    }
}

fun main() -> Num {
    return 0;
}
"#;

    assert_has_error(
        source,
        "Cannot declare public impl for private type 'Hidden'",
    );
}

#[test]
fn sema_rejects_unknown_enum_variant_in_match() {
    let source = r#"
enum Maybe {
    Some(Num),
    Empty
}

fun eval(x: Maybe) -> Num {
    match x {
        Unknown(v) => { return v; },
        Empty() => { return 0; }
    }
    return 0;
}

fun main() -> Num {
    return 0;
}
"#;

    assert_has_error(source, "Unknown enum variant 'Unknown'");
}

#[test]
fn sema_rejects_variant_pattern_arity_mismatch() {
    let source = r#"
enum Maybe {
    Some(Num),
    Empty
}

fun eval(x: Maybe) -> Num {
    match x {
        Some(a, b) => { return a; },
        Empty() => { return 0; }
    }
    return 0;
}

fun main() -> Num {
    return 0;
}
"#;

    assert_has_error(source, "Variant 'Some' expects 1 patterns, got 2");
}

#[test]
fn sema_rejects_non_exhaustive_num_match_without_catch_all() {
    let source = r#"
fun main() -> Num {
    var value: Num = 1;
    match value {
        1 => { return 1; }
    }
    return 0;
}
"#;

    assert_has_error(source, "Non-exhaustive match for type Num");
}

#[test]
fn sema_rejects_this_outside_impl_method() {
    let source = r#"
fun main() -> Num {
    return this.value;
}
"#;

    assert_has_error(source, "'this' used outside of impl method");
}

#[test]
fn sema_rejects_member_assignment_type_mismatch() {
    let source = r#"
struct Point {
    x: Num;
}

fun main() -> Num {
    var p: Point;
    p.x = true;
    return 0;
}
"#;

    assert_has_error(
        source,
        "Assignment type mismatch for member 'x': expected Num, got Bool",
    );
}

#[test]
fn sema_accepts_this_member_access_inside_impl() {
    let source = r#"
struct Point {
    x: Num;
}

impl Point {
    fun get() -> Num {
        return this.x;
    }
}

fun main() -> Num {
    return 0;
}
"#;

    let result = analyze_source(source);
    assert!(result.is_ok(), "Expected no semantic errors: {result:?}");
}

#[test]
fn sema_accepts_struct_constructor_and_field_access() {
    let source = r#"
struct Point {
    x: Num;
    y: Num;
}

fun main() -> Num {
    var p: Point = Point(1, 2);
    return p.x;
}
"#;

    let result = analyze_source(source);
    assert!(result.is_ok(), "Expected no semantic errors: {result:?}");
}

#[test]
fn sema_rejects_struct_constructor_arity_mismatch() {
    let source = r#"
struct Point {
    x: Num;
    y: Num;
}

fun main() -> Num {
    var p: Point = Point(1);
    return 0;
}
"#;

    assert_has_error(source, "Struct 'Point' constructor expects 2 args, got 1");
}

#[test]
fn sema_rejects_struct_constructor_argument_type_mismatch() {
    let source = r#"
struct Point {
    x: Num;
    y: Num;
}

fun main() -> Num {
    var p: Point = Point(1, true);
    return 0;
}
"#;

    assert_has_error(
        source,
        "Struct 'Point' constructor argument 1 expected Num, got Bool",
    );
}

#[test]
fn sema_accepts_enum_variant_constructor_call() {
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

    let result = analyze_source(source);
    assert!(result.is_ok(), "Expected no semantic errors: {result:?}");
}

#[test]
fn sema_rejects_enum_variant_constructor_argument_type_mismatch() {
    let source = r#"
enum Maybe {
    Some(Num),
    Empty
}

fun main() -> Num {
    var x: Maybe = Some(true);
    return 0;
}
"#;

    assert_has_error(
        source,
        "Enum variant 'Some' constructor argument 0 expected Num, got Bool",
    );
}

#[test]
fn sema_accepts_class_constructor_via_init_signature() {
    let source = r#"
class Counter {
    var value: Num;

    fun init(start: Num) {
        this.value = start;
    }

    fun get() -> Num {
        return this.value;
    }
}

fun main() -> Num {
    var c: Counter = Counter(2);
    return c.get();
}
"#;

    let result = analyze_source(source);
    assert!(result.is_ok(), "Expected no semantic errors: {result:?}");
}

#[test]
fn sema_rejects_class_constructor_arity_mismatch_via_init_signature() {
    let source = r#"
class Counter {
    var value: Num;

    fun init(start: Num) {
        this.value = start;
    }
}

fun main() -> Num {
    var c: Counter = Counter();
    return 0;
}
"#;

    assert_has_error(source, "Class 'Counter' constructor expects 1 args, got 0");
}

#[test]
fn sema_rejects_class_constructor_argument_type_mismatch_via_init_signature() {
    let source = r#"
class Counter {
    var value: Num;

    fun init(start: Num) {
        this.value = start;
    }
}

fun main() -> Num {
    var c: Counter = Counter(true);
    return 0;
}
"#;

    assert_has_error(
        source,
        "Class 'Counter' constructor argument 0 expected Num, got Bool",
    );
}

#[test]
fn sema_accepts_class_constructor_without_init_using_field_order() {
    let source = r#"
class Pair {
    var left: Num;
    var right: Num;
}

fun main() -> Num {
    var p: Pair = Pair(1, 2);
    return p.left;
}
"#;

    let result = analyze_source(source);
    assert!(result.is_ok(), "Expected no semantic errors: {result:?}");
}

#[test]
fn sema_rejects_zero_payload_enum_variant_constructor_arguments() {
    let source = r#"
enum Maybe {
    Empty
}

fun main() -> Num {
    var x: Maybe = Empty(1);
    return 0;
}
"#;

    assert_has_error(
        source,
        "Enum variant 'Empty' constructor expects 0 args, got 1",
    );
}

#[test]
fn sema_rejects_class_field_without_explicit_type() {
    let source = r#"
class Bad {
    var value;
}

fun main() -> Num {
    return 0;
}
"#;

    assert_has_error(
        source,
        "Class field 'Bad.value' must declare a concrete type",
    );
}

#[test]
fn sema_accepts_generic_type_syntax_for_known_base_type() {
    let source = r#"
struct Boxed {
    value: Num;
}

fun main() -> Num {
    var b: Boxed<Num> = Boxed(7);
    return b.value;
}
"#;

    let result = analyze_source(source);
    assert!(result.is_ok(), "Expected no semantic errors: {result:?}");
}

#[test]
fn sema_accepts_postfix_try_operator_passthrough_typing() {
    let source = r#"
fun id(v: Num) -> Num {
    return v;
}

fun main() -> Num {
    return id(1)?;
}
"#;

    let result = analyze_source(source);
    assert!(result.is_ok(), "Expected no semantic errors: {result:?}");
}

#[test]
fn sema_accepts_array_methods_and_indexing() {
    let source = r#"
fun main() -> Num {
    var values: Num[] = [1, 2];
    values = values.push(3);
    if values.is_empty() {
        return 0;
    }
    var first: Num = values[0];
    var last: Num = values.pop();
    return values.len() + first + last;
}
"#;

    let result = analyze_source(source);
    assert!(result.is_ok(), "Expected no semantic errors: {result:?}");
}

#[test]
fn sema_rejects_non_num_array_index() {
    let source = r#"
fun main() -> Num {
    var values: Num[] = [1, 2];
    return values[true];
}
"#;

    assert_has_error(source, "Array index must be Num, got Bool");
}

#[test]
fn sema_rejects_array_push_type_mismatch() {
    let source = r#"
fun main() -> Num {
    var values: Num[] = [1, 2];
    values = values.push(true);
    return 0;
}
"#;

    assert_has_error(source, "Array method 'push' expects Num, got Bool");
}

#[test]
fn sema_accepts_array_generic_class_style_type() {
    let source = r#"
fun main() -> Num {
    var values: Array<Num> = [1, 2];
    values = values.push(3);
    var first: Num = values[0];
    return values.len() + first;
}
"#;

    let result = analyze_source(source);
    assert!(result.is_ok(), "Expected no semantic errors: {result:?}");
}
