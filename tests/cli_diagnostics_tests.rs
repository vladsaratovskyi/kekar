use std::{
    fs,
    path::PathBuf,
    process::{self, Command},
    time::{SystemTime, UNIX_EPOCH},
};

fn temp_workspace(name: &str) -> PathBuf {
    let nanos = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("clock should be after UNIX_EPOCH")
        .as_nanos();
    let mut dir = std::env::temp_dir();
    dir.push(format!(
        "kekar-cli-test-{}-{}-{}",
        name,
        process::id(),
        nanos
    ));
    fs::create_dir_all(&dir).expect("should create temporary cli test workspace");
    dir
}

#[test]
fn cli_exits_non_zero_with_structured_parser_diagnostic() {
    let root = temp_workspace("parse-error");
    let source_path = root.join("main.kek");
    fs::write(
        &source_path,
        r#"
fun main() -> Num {
    return 1
"#,
    )
    .expect("should write source file");

    let output = Command::new(env!("CARGO_BIN_EXE_kekar"))
        .arg(&source_path)
        .arg("--target")
        .arg("asm")
        .output()
        .expect("should run compiler");

    assert_eq!(output.status.code(), Some(1));
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(
        stderr.contains("Failed to parse module"),
        "unexpected stderr: {stderr}"
    );
    assert!(stderr.contains("token #"), "unexpected stderr: {stderr}");

    fs::remove_dir_all(root).expect("should clean test workspace");
}

#[test]
fn cli_exits_non_zero_with_structured_lexer_diagnostic() {
    let root = temp_workspace("lex-error");
    let source_path = root.join("main.kek");
    fs::write(
        &source_path,
        r#"
fun main() -> Num {
    var s: String = "oops;
    return 1;
}
"#,
    )
    .expect("should write source file");

    let output = Command::new(env!("CARGO_BIN_EXE_kekar"))
        .arg(&source_path)
        .arg("--target")
        .arg("asm")
        .output()
        .expect("should run compiler");

    assert_eq!(output.status.code(), Some(1));
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(
        stderr.contains("Failed to lex module"),
        "unexpected stderr: {stderr}"
    );
    assert!(
        stderr.contains("Unclosed string literal"),
        "unexpected stderr: {stderr}"
    );

    fs::remove_dir_all(root).expect("should clean test workspace");
}
