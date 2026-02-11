use kekar::lexer::{Lexer, Token};

fn lex(source: &str) -> Vec<Token> {
    let mut lexer = Lexer::from_source(source);
    lexer.lex_file()
}

#[test]
fn lexes_new_keywords() {
    let tokens = lex("break continue struct enum impl match mod use pub const as while");

    assert_eq!(
        tokens,
        vec![
            Token::Break,
            Token::Continue,
            Token::Struct,
            Token::Enum,
            Token::Impl,
            Token::Match,
            Token::Mod,
            Token::Use,
            Token::Pub,
            Token::Const,
            Token::As,
            Token::While,
            Token::Eof,
        ]
    );
}

#[test]
fn lexes_compound_operators_and_bitwise_tokens() {
    let tokens = lex("a -> b => c && d || e << 2 >> 1 += -= *= /= %= == != <= >= :: ? & | ^");

    assert_eq!(
        tokens,
        vec![
            Token::Identifier("a".to_string()),
            Token::Arrow,
            Token::Identifier("b".to_string()),
            Token::FatArrow,
            Token::Identifier("c".to_string()),
            Token::And,
            Token::Identifier("d".to_string()),
            Token::Or,
            Token::Identifier("e".to_string()),
            Token::ShiftLeft,
            Token::Number(2.0),
            Token::ShiftRight,
            Token::Number(1.0),
            Token::PlusEqual,
            Token::MinusEqual,
            Token::StarEqual,
            Token::SlashEqual,
            Token::PercentEqual,
            Token::EqualEqual,
            Token::NotEqual,
            Token::LessEqual,
            Token::GreaterEqual,
            Token::ColonColon,
            Token::Question,
            Token::BitAnd,
            Token::BitOr,
            Token::BitXor,
            Token::Eof,
        ]
    );
}

#[test]
fn lexes_block_and_line_comments() {
    let source = r#"
var a: Num = 1;
/* ignored
   block */
var b: Num = 2; // line comment
var c: Num = 3;
"#;

    let tokens = lex(source);

    assert_eq!(
        tokens,
        vec![
            Token::Var,
            Token::Identifier("a".to_string()),
            Token::Colon,
            Token::Identifier("Num".to_string()),
            Token::Equal,
            Token::Number(1.0),
            Token::Semicolon,
            Token::Var,
            Token::Identifier("b".to_string()),
            Token::Colon,
            Token::Identifier("Num".to_string()),
            Token::Equal,
            Token::Number(2.0),
            Token::Semicolon,
            Token::Var,
            Token::Identifier("c".to_string()),
            Token::Colon,
            Token::Identifier("Num".to_string()),
            Token::Equal,
            Token::Number(3.0),
            Token::Semicolon,
            Token::Eof,
        ]
    );
}

#[test]
fn lexes_hex_and_decimal_numbers() {
    let tokens = lex("0x2A 10 3.5");

    assert_eq!(
        tokens,
        vec![
            Token::Number(42.0),
            Token::Number(10.0),
            Token::Number(3.5),
            Token::Eof,
        ]
    );
}

#[test]
fn lexes_char_literals_with_escapes() {
    let tokens = lex("'a' '\\n' '\\'' '\\\\'");

    assert_eq!(
        tokens,
        vec![
            Token::Char('a'),
            Token::Char('\n'),
            Token::Char('\''),
            Token::Char('\\'),
            Token::Eof,
        ]
    );
}

#[test]
fn supports_legacy_keyword_logical_operators() {
    let tokens = lex("and or");

    assert_eq!(tokens, vec![Token::And, Token::Or, Token::Eof]);
}
