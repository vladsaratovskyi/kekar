use std::{
    fs,
    path::{Path, PathBuf},
    process,
    time::{SystemTime, UNIX_EPOCH},
};

use kekar::workspace::analyze_workspace;

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
