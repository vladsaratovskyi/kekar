use std::path::PathBuf;

use kekar::{
    asm_generator::AsmGenerator, lexer::Lexer, parser::Parser, sema::SemanticAnalyzer,
    workspace::analyze_workspace,
};

fn bootstrap_program_path() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("example/bootstrap_stage0.kek")
}

#[test]
fn bootstrap_stage0_program_passes_workspace_and_sema() {
    let path = bootstrap_program_path();

    let workspace_result = analyze_workspace(&path);
    assert!(
        workspace_result.is_ok(),
        "Expected workspace analysis success, got: {workspace_result:?}"
    );

    let mut lexer = Lexer::new(&path.display().to_string());
    let tokens = lexer.lex_file();
    let mut parser = Parser::new(tokens);
    let ast = parser.parse();

    let sema_result = SemanticAnalyzer::analyze(&ast);
    assert!(
        sema_result.is_ok(),
        "Expected semantic analysis success, got: {sema_result:?}"
    );
}

#[test]
fn bootstrap_stage0_program_generates_executable_asm_for_v1_constructs() {
    let path = bootstrap_program_path();

    let mut lexer = Lexer::new(&path.display().to_string());
    let tokens = lexer.lex_file();
    let mut parser = Parser::new(tokens);
    let ast = parser.parse();

    let output = AsmGenerator::new().generate(&ast);

    assert!(output.contains("main:"));
    assert!(output.contains("execute:"));
    assert!(output.contains("Driver__apply:"));
    assert!(output.contains("Report__ok:"));
    assert!(output.contains("__kek_struct_Driver:"));
    assert!(output.contains("__kek_enum_Node:"));
    assert!(output.contains("__kek_impl_Driver:"));
    assert!(output.contains(".match_end_"));
    assert!(output.contains(".for_loop_"));
    assert!(!output.contains("dynamic/member call unsupported in asm backend"));
    assert!(!output.contains("member access requires user type"));
    assert!(!output.contains("unsupported literal match pattern in asm backend"));
}
