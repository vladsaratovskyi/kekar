use std::{
    fs,
    path::{Path, PathBuf},
    process,
    time::{SystemTime, UNIX_EPOCH},
};

use kekar::{
    ast::{Expr, Literal, Stmt},
    workspace::{analyze_workspace, build_workspace_program},
};

fn temp_workspace(name: &str) -> PathBuf {
    let nanos = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("clock should be after UNIX_EPOCH")
        .as_nanos();
    let mut dir = std::env::temp_dir();
    dir.push(format!(
        "kekar-workspace-test-{}-{}-{}",
        name,
        process::id(),
        nanos
    ));
    fs::create_dir_all(&dir).expect("should create temporary test workspace");
    dir
}

fn write_file(path: &Path, source: &str) {
    if let Some(parent) = path.parent() {
        fs::create_dir_all(parent).expect("should create parent directory");
    }
    fs::write(path, source).expect("should write test file");
}

fn assert_has_error(path: &Path, needle: &str) {
    let result = analyze_workspace(path);
    let errors = result.expect_err("expected workspace analysis to fail");
    assert!(
        errors.iter().any(|error| error.message.contains(needle)),
        "Expected error containing '{needle}', got: {errors:?}"
    );
}

#[test]
fn workspace_resolves_mod_use_across_files() {
    let root = temp_workspace("mod-use-ok");
    let entry = root.join("main.kek");
    let util = root.join("util.kek");

    write_file(
        &entry,
        r#"
pub mod util;
use util::Helper;

fun main() -> Num {
    return 0;
}
"#,
    );

    write_file(
        &util,
        r#"
pub struct Helper {
    value: Num;
}
"#,
    );

    let result = analyze_workspace(&entry);
    assert!(result.is_ok(), "Expected no workspace errors: {result:?}");

    fs::remove_dir_all(root).expect("should clean test workspace");
}

#[test]
fn workspace_rejects_missing_mod_file() {
    let root = temp_workspace("missing-mod");
    let entry = root.join("main.kek");

    write_file(
        &entry,
        r#"
mod missing;

fun main() -> Num {
    return 0;
}
"#,
    );

    assert_has_error(&entry, "Unable to resolve module 'missing'");
    fs::remove_dir_all(root).expect("should clean test workspace");
}

#[test]
fn workspace_rejects_unresolved_use_segment_in_linked_module() {
    let root = temp_workspace("missing-segment");
    let entry = root.join("main.kek");
    let util = root.join("util.kek");

    write_file(
        &entry,
        r#"
mod util;
use util::Missing;

fun main() -> Num {
    return 0;
}
"#,
    );

    write_file(
        &util,
        r#"
pub struct Helper {
    value: Num;
}
"#,
    );

    assert_has_error(&entry, "Unresolved use path segment 'Missing'");
    fs::remove_dir_all(root).expect("should clean test workspace");
}

#[test]
fn workspace_rejects_pub_use_of_private_cross_file_symbol() {
    let root = temp_workspace("pub-use-private");
    let entry = root.join("main.kek");
    let util = root.join("util.kek");

    write_file(
        &entry,
        r#"
pub mod util;
pub use util::Hidden;

fun main() -> Num {
    return 0;
}
"#,
    );

    write_file(
        &util,
        r#"
struct Hidden {
    value: Num;
}
"#,
    );

    assert_has_error(
        &entry,
        "Cannot publicly re-export private path 'util::Hidden'",
    );
    fs::remove_dir_all(root).expect("should clean test workspace");
}

#[test]
fn workspace_resolves_import_from_relative_path() {
    let root = temp_workspace("import-relative");
    let entry = root.join("main.kek");
    let dep = root.join("deps").join("core.kek");

    write_file(
        &entry,
        r#"
import Core as C from "./deps/core.kek";
use C::Thing;

fun main() -> Num {
    return 0;
}
"#,
    );

    write_file(
        &dep,
        r#"
pub struct Thing {
    value: Num;
}
"#,
    );

    let result = analyze_workspace(&entry);
    assert!(result.is_ok(), "Expected no workspace errors: {result:?}");

    fs::remove_dir_all(root).expect("should clean test workspace");
}

#[test]
fn workspace_accepts_public_impl_method_call_across_modules() {
    let root = temp_workspace("method-pub-cross-module");
    let entry = root.join("main.kek");
    let util = root.join("util.kek");

    write_file(
        &entry,
        r#"
mod util;
use util::Point;

fun main() -> Num {
    var p: Point;
    return p.value();
}
"#,
    );

    write_file(
        &util,
        r#"
pub struct Point {
    n: Num;
}

impl Point {
    pub fun value() -> Num {
        return this.n;
    }
}
"#,
    );

    let result = analyze_workspace(&entry);
    assert!(result.is_ok(), "Expected no workspace errors: {result:?}");

    fs::remove_dir_all(root).expect("should clean test workspace");
}

#[test]
fn workspace_accepts_public_inline_struct_method_call_across_modules() {
    let root = temp_workspace("method-inline-pub-cross-module");
    let entry = root.join("main.kek");
    let util = root.join("util.kek");

    write_file(
        &entry,
        r#"
mod util;
use util::Point;

fun main() -> Num {
    var p: Point = Point(4);
    return p.value();
}
"#,
    );

    write_file(
        &util,
        r#"
pub struct Point {
    n: Num;
    pub fun value() -> Num {
        return this.n;
    }
}
"#,
    );

    let result = analyze_workspace(&entry);
    assert!(result.is_ok(), "Expected no workspace errors: {result:?}");

    fs::remove_dir_all(root).expect("should clean test workspace");
}

#[test]
fn workspace_rejects_private_impl_method_call_across_modules() {
    let root = temp_workspace("method-private-cross-module");
    let entry = root.join("main.kek");
    let util = root.join("util.kek");

    write_file(
        &entry,
        r#"
mod util;
use util::Point;

fun main() -> Num {
    var p: Point;
    return p.hidden();
}
"#,
    );

    write_file(
        &util,
        r#"
pub struct Point {
    n: Num;
}

impl Point {
    fun hidden() -> Num {
        return this.n;
    }
}
"#,
    );

    assert_has_error(
        &entry,
        "Method 'Point.hidden' is private and cannot be called",
    );
    fs::remove_dir_all(root).expect("should clean test workspace");
}

#[test]
fn workspace_accepts_public_cross_module_function_call_via_use() {
    let root = temp_workspace("function-pub-use-cross-module");
    let entry = root.join("main.kek");
    let util = root.join("util.kek");

    write_file(
        &entry,
        r#"
mod util;
use util::add;

fun main() -> Num {
    return add(1, 2);
}
"#,
    );

    write_file(
        &util,
        r#"
pub fun add(a: Num, b: Num) -> Num {
    return a + b;
}
"#,
    );

    let result = analyze_workspace(&entry);
    assert!(result.is_ok(), "Expected no workspace errors: {result:?}");

    fs::remove_dir_all(root).expect("should clean test workspace");
}

#[test]
fn workspace_rejects_cross_module_function_argument_type_mismatch_via_use() {
    let root = temp_workspace("function-arg-mismatch-use");
    let entry = root.join("main.kek");
    let util = root.join("util.kek");

    write_file(
        &entry,
        r#"
mod util;
use util::add;

fun main() -> Num {
    add(1, true);
    return 0;
}
"#,
    );

    write_file(
        &util,
        r#"
pub fun add(a: Num, b: Num) -> Num {
    return a + b;
}
"#,
    );

    assert_has_error(
        &entry,
        "Argument 1 for function 'add' expected Num, got Bool",
    );
    fs::remove_dir_all(root).expect("should clean test workspace");
}

#[test]
fn workspace_rejects_private_cross_module_function_call_via_use() {
    let root = temp_workspace("function-private-use");
    let entry = root.join("main.kek");
    let util = root.join("util.kek");

    write_file(
        &entry,
        r#"
mod util;
use util::hidden;

fun main() -> Num {
    return hidden();
}
"#,
    );

    write_file(
        &util,
        r#"
fun hidden() -> Num {
    return 1;
}
"#,
    );

    assert_has_error(&entry, "Function 'hidden' is private and cannot be called");
    fs::remove_dir_all(root).expect("should clean test workspace");
}

#[test]
fn workspace_accepts_public_cross_module_function_call_via_module_alias() {
    let root = temp_workspace("function-pub-module-alias");
    let entry = root.join("main.kek");
    let util = root.join("util.kek");

    write_file(
        &entry,
        r#"
import Util from "./util.kek";

fun main() -> Num {
    return Util.add(1, 2);
}
"#,
    );

    write_file(
        &util,
        r#"
pub fun add(a: Num, b: Num) -> Num {
    return a + b;
}
"#,
    );

    let result = analyze_workspace(&entry);
    assert!(result.is_ok(), "Expected no workspace errors: {result:?}");

    fs::remove_dir_all(root).expect("should clean test workspace");
}

#[test]
fn workspace_rejects_private_cross_module_function_call_via_module_alias() {
    let root = temp_workspace("function-private-module-alias");
    let entry = root.join("main.kek");
    let util = root.join("util.kek");

    write_file(
        &entry,
        r#"
import Util from "./util.kek";

fun main() -> Num {
    return Util.hidden();
}
"#,
    );

    write_file(
        &util,
        r#"
fun hidden() -> Num {
    return 1;
}
"#,
    );

    assert_has_error(&entry, "Function 'hidden' is private and cannot be called");
    fs::remove_dir_all(root).expect("should clean test workspace");
}

#[test]
fn workspace_accepts_generic_type_syntax_and_postfix_try() {
    let root = temp_workspace("generic-and-try");
    let entry = root.join("main.kek");

    write_file(
        &entry,
        r#"
struct Boxed {
    value: Num;
}

fun id(v: Num) -> Num {
    return v;
}

fun main() -> Num {
    var b: Boxed<Num> = Boxed(3);
    return id(b.value)?;
}
"#,
    );

    let result = analyze_workspace(&entry);
    assert!(result.is_ok(), "Expected no workspace errors: {result:?}");

    fs::remove_dir_all(root).expect("should clean test workspace");
}

#[test]
fn workspace_builds_linked_program_for_module_qualified_function_call() {
    let root = temp_workspace("build-linked-program-module-call");
    let entry = root.join("main.kek");
    let util = root.join("util.kek");

    write_file(
        &entry,
        r#"
import Util from "./util.kek";

fun main() -> Num {
    return Util.add(2, 3);
}
"#,
    );

    write_file(
        &util,
        r#"
pub fun add(a: Num, b: Num) -> Num {
    return a + b;
}
"#,
    );

    let linked = build_workspace_program(&entry).expect("workspace linking should succeed");
    let function_names = linked
        .stmts
        .iter()
        .filter_map(|stmt| match stmt {
            Stmt::Fun(fun_stmt) => Some(fun_stmt.name.as_str()),
            Stmt::Pub(pub_stmt) => match pub_stmt.stmt.as_ref() {
                Stmt::Fun(fun_stmt) => Some(fun_stmt.name.as_str()),
                _ => None,
            },
            _ => None,
        })
        .collect::<Vec<_>>();

    assert!(
        function_names.iter().any(|name| *name == "main"),
        "expected linked workspace program to include entry main, got: {function_names:?}"
    );
    assert!(
        function_names
            .iter()
            .any(|name| name.starts_with("__kek_m") && name.ends_with("_add")),
        "expected linked workspace program to include lowered util.add symbol, got: {function_names:?}"
    );

    let main_call = linked.stmts.iter().find_map(|stmt| match stmt {
        Stmt::Fun(fun_stmt) if fun_stmt.name == "main" => match fun_stmt.block.as_ref() {
            Stmt::Block(block) => block.stmts.iter().find_map(|stmt| match stmt {
                Stmt::Return(return_stmt) => match &return_stmt.return_expr {
                    Expr::Call(call) => Some(call.callee.as_ref()),
                    _ => None,
                },
                _ => None,
            }),
            _ => None,
        },
        _ => None,
    });

    let callee = main_call.expect("main should return a call expression");
    match callee {
        Expr::Literal(Literal::Identifier(name)) => {
            assert!(name.starts_with("__kek_m"));
            assert!(name.ends_with("_add"));
        }
        other => panic!("expected lowered direct identifier call, got {other:?}"),
    }

    fs::remove_dir_all(root).expect("should clean test workspace");
}

#[test]
fn workspace_resolves_bundled_stdlib_use_paths_and_calls() {
    let root = temp_workspace("stdlib-use-ok");
    let entry = root.join("main.kek");

    write_file(
        &entry,
        r#"
use std::fs::read_to_string;
use std::string::len;
use std::collections::count;
use std::io::write_line;
use std::path::Path;

fun main() -> Num {
    var text: String = read_to_string("input.kek");
    var p: Path;
    write_line(text);
    return len(text) + count([1, 2, 3]);
}
"#,
    );

    let result = analyze_workspace(&entry);
    assert!(result.is_ok(), "Expected no workspace errors: {result:?}");

    fs::remove_dir_all(root).expect("should clean test workspace");
}

#[test]
fn workspace_rejects_missing_stdlib_symbol_in_use_path() {
    let root = temp_workspace("stdlib-use-missing");
    let entry = root.join("main.kek");

    write_file(
        &entry,
        r#"
use std::fs::missing;

fun main() -> Num {
    return 0;
}
"#,
    );

    assert_has_error(&entry, "Unresolved use path segment 'missing'");
    fs::remove_dir_all(root).expect("should clean test workspace");
}

#[test]
fn workspace_enforces_stdlib_function_argument_types() {
    let root = temp_workspace("stdlib-fn-arg-type");
    let entry = root.join("main.kek");

    write_file(
        &entry,
        r#"
use std::string::len;

fun main() -> Num {
    return len(1);
}
"#,
    );

    assert_has_error(&entry, "Argument 0 for function 'len' expected String, got Num");
    fs::remove_dir_all(root).expect("should clean test workspace");
}
