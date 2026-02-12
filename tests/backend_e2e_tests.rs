use std::{
    fs,
    path::{Path, PathBuf},
    process::{self, Command},
    time::{SystemTime, UNIX_EPOCH},
};

use kekar::{
    asm_generator::AsmGenerator, lexer::Lexer, parser::Parser, workspace::build_workspace_program,
};

fn temp_workspace(name: &str) -> PathBuf {
    let nanos = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("clock should be after UNIX_EPOCH")
        .as_nanos();
    let mut dir = std::env::temp_dir();
    dir.push(format!(
        "kekar-backend-e2e-{}-{}-{}",
        name,
        process::id(),
        nanos
    ));
    fs::create_dir_all(&dir).expect("should create temporary e2e workspace");
    dir
}

fn ensure_backend_e2e_runtime() -> Result<(), String> {
    if !cfg!(all(target_os = "linux", target_arch = "x86_64")) {
        return Err("requires linux x86_64 runtime for generated ASM ABI".to_string());
    }

    if Command::new("nasm").arg("--version").output().is_err() {
        return Err("requires nasm in PATH".to_string());
    }

    if Command::new("ld").arg("--version").output().is_err() {
        return Err("requires ld in PATH".to_string());
    }

    Ok(())
}

fn backend_e2e_runtime_ready() -> bool {
    match ensure_backend_e2e_runtime() {
        Ok(()) => true,
        Err(reason) => {
            if std::env::var("KEKAR_REQUIRE_BACKEND_E2E").as_deref() == Ok("1") {
                panic!("backend e2e runtime is required: {reason}");
            }
            eprintln!("skipping backend e2e test: {reason}");
            false
        }
    }
}

fn compile_asm_and_run(source: &str, name: &str) -> Result<i32, String> {
    let mut lexer = Lexer::from_source(source);
    let tokens = lexer
        .lex_with_diagnostics()
        .map_err(|errors| format!("lex errors: {errors:?}"))?;

    let mut parser = Parser::new(tokens);
    let ast = parser
        .parse_checked()
        .map_err(|errors| format!("parse errors: {errors:?}"))?;

    let asm = AsmGenerator::new().generate(&ast);
    let root = temp_workspace(name);
    let asm_path = root.join("program.asm");
    let obj_path = root.join("program.o");
    let exe_path = root.join("program.out");
    fs::write(&asm_path, asm).map_err(|error| format!("write asm failed: {error}"))?;

    let nasm = Command::new("nasm")
        .arg("-f")
        .arg("elf64")
        .arg(&asm_path)
        .arg("-o")
        .arg(&obj_path)
        .output()
        .map_err(|error| format!("failed to execute nasm: {error}"))?;
    if !nasm.status.success() {
        return Err(format!(
            "nasm failed: {}",
            String::from_utf8_lossy(&nasm.stderr)
        ));
    }

    let ld = Command::new("ld")
        .arg("-o")
        .arg(&exe_path)
        .arg(&obj_path)
        .output()
        .map_err(|error| format!("failed to execute ld: {error}"))?;
    if !ld.status.success() {
        return Err(format!(
            "ld failed: {}",
            String::from_utf8_lossy(&ld.stderr)
        ));
    }

    let run = Command::new(&exe_path)
        .output()
        .map_err(|error| format!("failed to run executable: {error}"))?;

    fs::remove_dir_all(root).ok();
    Ok(run.status.code().unwrap_or(-1))
}

fn compile_workspace_entry_and_run(entry: &Path, name: &str) -> Result<i32, String> {
    let ast = build_workspace_program(entry).map_err(|errors| {
        let rendered = errors
            .into_iter()
            .map(|error| error.message)
            .collect::<Vec<_>>()
            .join("; ");
        format!("workspace errors: {rendered}")
    })?;

    let asm = AsmGenerator::new().generate(&ast);
    let root = temp_workspace(name);
    let asm_path = root.join("program.asm");
    let obj_path = root.join("program.o");
    let exe_path = root.join("program.out");
    fs::write(&asm_path, asm).map_err(|error| format!("write asm failed: {error}"))?;

    let nasm = Command::new("nasm")
        .arg("-f")
        .arg("elf64")
        .arg(&asm_path)
        .arg("-o")
        .arg(&obj_path)
        .output()
        .map_err(|error| format!("failed to execute nasm: {error}"))?;
    if !nasm.status.success() {
        return Err(format!(
            "nasm failed: {}",
            String::from_utf8_lossy(&nasm.stderr)
        ));
    }

    let ld = Command::new("ld")
        .arg("-o")
        .arg(&exe_path)
        .arg(&obj_path)
        .output()
        .map_err(|error| format!("failed to execute ld: {error}"))?;
    if !ld.status.success() {
        return Err(format!(
            "ld failed: {}",
            String::from_utf8_lossy(&ld.stderr)
        ));
    }

    let run = Command::new(&exe_path)
        .output()
        .map_err(|error| format!("failed to run executable: {error}"))?;

    fs::remove_dir_all(root).ok();
    Ok(run.status.code().unwrap_or(-1))
}

#[test]
fn backend_e2e_runs_main_return_value() {
    if !backend_e2e_runtime_ready() {
        return;
    }

    let code = compile_asm_and_run(
        r#"
fun main() -> Num {
    return 7;
}
"#,
        "main-return",
    )
    .expect("assemble/link/run should succeed");

    assert_eq!(code, 7);
}

#[test]
fn backend_e2e_runs_struct_method_and_loop_program() {
    if !backend_e2e_runtime_ready() {
        return;
    }

    let code = compile_asm_and_run(
        r#"
struct Counter {
    value: Num;
    fun inc(delta: Num) -> Num {
        this.value = this.value + delta;
        return this.value;
    }
}

fun main() -> Num {
    var c: Counter = Counter(1);
    var i: Num = 0;
    while i < 3 {
        c.inc(2);
        i = i + 1;
    }
    return c.value;
}
"#,
        "struct-loop",
    )
    .expect("assemble/link/run should succeed");

    assert_eq!(code, 7);
}

#[test]
fn backend_e2e_runs_enum_constructor_and_match_program() {
    if !backend_e2e_runtime_ready() {
        return;
    }

    let code = compile_asm_and_run(
        r#"
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
    return unwrap(Some(5));
}
"#,
        "enum-match",
    )
    .expect("assemble/link/run should succeed");

    assert_eq!(code, 5);
}

#[test]
fn backend_e2e_short_circuits_logical_and_or_rhs() {
    if !backend_e2e_runtime_ready() {
        return;
    }

    let code = compile_asm_and_run(
        r#"
fun crash() -> Num {
    return 1 / 0;
}

fun main() -> Num {
    var a: Bool = false && (crash() > 0);
    var b: Bool = true || (crash() > 0);
    if a {
        return 1;
    }
    if b {
        return 0;
    }
    return 2;
}
"#,
        "short-circuit-and-or",
    )
    .expect("assemble/link/run should succeed");

    assert_eq!(code, 0);
}

#[test]
fn backend_e2e_runs_cross_module_import_function_call() {
    if !backend_e2e_runtime_ready() {
        return;
    }

    let source_root = temp_workspace("workspace-module-call-src");
    let entry = source_root.join("main.kek");
    let util = source_root.join("util.kek");

    fs::write(
        &entry,
        r#"
import Util from "./util.kek";

fun main() -> Num {
    return Util.add(2, 3);
}
"#,
    )
    .expect("should write entry source");

    fs::write(
        &util,
        r#"
pub fun add(a: Num, b: Num) -> Num {
    return a + b;
}
"#,
    )
    .expect("should write util source");

    let code = compile_workspace_entry_and_run(&entry, "workspace-module-call")
        .expect("workspace assemble/link/run should succeed");

    assert_eq!(code, 5);
    fs::remove_dir_all(source_root).ok();
}

#[test]
fn backend_e2e_runs_full_syntax_example_program() {
    if !backend_e2e_runtime_ready() {
        return;
    }

    let entry = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("example")
        .join("full_syntax.kek");

    let code = compile_workspace_entry_and_run(&entry, "full-syntax-example")
        .expect("workspace assemble/link/run should succeed");

    assert_eq!(code, 12);
}

#[test]
fn backend_e2e_runs_bundled_std_string_basic_runtime_api() {
    if !backend_e2e_runtime_ready() {
        return;
    }

    let source_root = temp_workspace("workspace-stdlib-string-runtime");
    let entry = source_root.join("main.kek");

    fs::write(
        &entry,
        r#"
use std::string::concat;
use std::string::equals;
use std::string::is_empty;
use std::string::len;
use std::string::starts_with;
use std::string::char_at;

fun main() -> Num {
    var text: String = concat("ab", "c");
    if !equals(text, "abc") {
        return 11;
    }
    if is_empty(text) {
        return 12;
    }
    if !starts_with(text, "ab") {
        return 13;
    }
    var ch: Char = char_at(text, 1);
    if ch != 'b' {
        return 14;
    }
    return len(text);
}
"#,
    )
    .expect("should write entry source");

    let code = compile_workspace_entry_and_run(&entry, "workspace-stdlib-string-runtime")
        .expect("workspace assemble/link/run should succeed");

    assert_eq!(code, 3);
    fs::remove_dir_all(source_root).ok();
}

#[test]
fn backend_e2e_runs_array_methods_runtime() {
    if !backend_e2e_runtime_ready() {
        return;
    }

    let code = compile_asm_and_run(
        r#"
fun main() -> Num {
    var values: Array<Num> = [1, 2];
    values = values.push(5);
    var popped: Num = values.pop();
    if values.is_empty() {
        return 90;
    }
    return values.len() + popped + values[0];
}
"#,
        "array-methods-runtime",
    )
    .expect("assemble/link/run should succeed");

    assert_eq!(code, 8);
}
