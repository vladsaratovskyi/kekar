#![allow(unused)]
use std::fmt::{self, Display};
use std::fs::{self};

#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    // Single-character tokens.
    LeftParen,
    RightParen,
    LeftBracket,
    RightBracket,
    Coma,
    Dot,
    Minus,
    Plus,
    Semicolon,
    Slash,
    Star,
    Percent,
    PlusEqual,
    MinusEqual,
    StarEqual,
    SlashEqual,
    PercentEqual,
    Colon,
    ColonColon,
    LeftBrace,
    RightBrace,
    Question,

    // One or two character tokens.
    Arrow,
    FatArrow,
    Not,
    NotEqual,
    Equal,
    EqualEqual,
    Greater,
    GreaterEqual,
    ShiftRight,
    Less,
    LessEqual,
    ShiftLeft,
    BitAnd,
    BitOr,
    BitXor,

    // Literals.
    Identifier(String),
    String(String),
    Char(char),
    Number(f64),

    // Keywords.
    And,
    Class,
    Else,
    False,
    Fun,
    For,
    If,
    In,
    None,
    Or,
    Print,
    Return,
    Super,
    This,
    True,
    Var,
    While,
    Import,
    From,
    As,
    Break,
    Continue,
    Struct,
    Enum,
    Impl,
    Match,
    Mod,
    Use,
    Pub,
    Const,

    Eof,
}

impl Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", self)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LexError {
    pub message: String,
    pub line: usize,
    pub column: usize,
    pub position: usize,
}

impl LexError {
    fn new(message: impl Into<String>, line: usize, column: usize, position: usize) -> Self {
        Self {
            message: message.into(),
            line,
            column,
            position,
        }
    }
}

fn get_keyword(key: &str) -> Option<Token> {
    match key {
        "var" => Some(Token::Var),
        "class" => Some(Token::Class),
        "else" => Some(Token::Else),
        "fun" => Some(Token::Fun),
        "for" => Some(Token::For),
        "in" => Some(Token::In),
        "if" => Some(Token::If),
        "None" => Some(Token::None),
        "or" => Some(Token::Or),
        "print" => Some(Token::Print),
        "return" => Some(Token::Return),
        "super" => Some(Token::Super),
        "this" => Some(Token::This),
        "true" => Some(Token::True),
        "false" => Some(Token::False),
        "while" => Some(Token::While),
        "and" => Some(Token::And),
        "ge" => Some(Token::GreaterEqual),
        "gt" => Some(Token::Greater),
        "le" => Some(Token::LessEqual),
        "lt" => Some(Token::Less),
        "import" => Some(Token::Import),
        "from" => Some(Token::From),
        "as" => Some(Token::As),
        "break" => Some(Token::Break),
        "continue" => Some(Token::Continue),
        "struct" => Some(Token::Struct),
        "enum" => Some(Token::Enum),
        "impl" => Some(Token::Impl),
        "match" => Some(Token::Match),
        "mod" => Some(Token::Mod),
        "use" => Some(Token::Use),
        "pub" => Some(Token::Pub),
        "const" => Some(Token::Const),
        s => Some(Token::Identifier(s.to_string())),
    }
}

fn is_num(char: char) -> bool {
    let nums = "0123456789";
    nums.contains(char)
}

fn is_letter(char: char) -> bool {
    let nums = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_";
    nums.contains(char)
}

fn is_hex_digit(char: char) -> bool {
    let hex = "0123456789abcdefABCDEF";
    hex.contains(char)
}

pub struct Lexer {
    current: usize,
    start: usize,
    line: usize,
    source: String,
    errors: Vec<LexError>,
}

impl Lexer {
    pub fn new(path_to_file: &String) -> Self {
        Self {
            source: fs::read_to_string(path_to_file).expect("Could not read file."),
            current: 0,
            start: 0,
            line: 0,
            errors: Vec::new(),
        }
    }

    pub fn from_source(source: impl Into<String>) -> Self {
        Self {
            source: source.into(),
            current: 0,
            start: 0,
            line: 0,
            errors: Vec::new(),
        }
    }

    fn current_column(&self, position: usize) -> usize {
        let position = position.min(self.source.len());
        let slice = &self.source[..position];
        match slice.rfind('\n') {
            Some(last_newline) => position.saturating_sub(last_newline),
            None => position + 1,
        }
    }

    fn push_error_at(&mut self, message: impl Into<String>, position: usize) {
        let line = self.line + 1;
        let column = self.current_column(position);
        self.errors
            .push(LexError::new(message, line, column, position));
    }

    fn check_second_char(&mut self, char: char) -> bool {
        if self.is_end() {
            return false;
        }
        if self.source.as_bytes()[self.current] as char != char {
            return false;
        }

        self.current += 1;
        true
    }

    fn peek_char(&mut self) -> char {
        if self.is_end() {
            return '\0';
        }
        self.source.as_bytes()[self.current] as char
    }

    fn peek_next_char(&mut self) -> char {
        if self.current + 1 >= self.source.len() {
            return '\0';
        }
        self.source.as_bytes()[self.current + 1] as char
    }

    fn move_next(&mut self) -> char {
        let c = self.source.as_bytes()[self.current] as char;
        self.current += 1;
        c
    }

    fn is_end(&self) -> bool {
        self.current >= self.source.len()
    }

    fn get_value(&self) -> String {
        self.source[self.start..self.current].to_string()
    }

    fn get_num_value(&self) -> f64 {
        self.source[self.start..self.current]
            .parse::<f64>()
            .unwrap()
    }

    fn parse_string(&mut self) -> Option<Token> {
        while self.peek_char() != '"' && !self.is_end() {
            if self.peek_char() == '\n' {
                self.line += 1
            }
            self.move_next();
        }

        if self.is_end() {
            self.push_error_at("Unclosed string literal", self.current);
            return None;
        }

        self.move_next();
        Some(Token::String(
            self.source[self.start + 1..self.current - 1].to_string(),
        ))
    }

    fn parse_number(&mut self) -> Option<Token> {
        if self.source.as_bytes()[self.start] as char == '0'
            && matches!(self.peek_char(), 'x' | 'X')
        {
            self.move_next();

            let hex_start = self.current;
            while is_hex_digit(self.peek_char()) {
                self.move_next();
            }

            if self.current == hex_start {
                self.push_error_at("Expected hex digits after 0x prefix", self.current);
                return None;
            }

            let value = i64::from_str_radix(&self.source[self.start + 2..self.current], 16)
                .expect("Invalid hex literal") as f64;
            return Some(Token::Number(value));
        }

        while is_num(self.peek_char()) {
            self.move_next();
            continue;
        }

        if self.peek_char() == '.' && is_num(self.peek_next_char()) {
            self.move_next();
            while is_num(self.peek_char()) {
                self.move_next();
                continue;
            }
        }

        Some(Token::Number(self.get_num_value()))
    }

    fn parse_word(&mut self) -> Option<Token> {
        while is_letter(self.peek_char()) || is_num(self.peek_char()) {
            self.move_next();
            continue;
        }

        let value = self.get_value();
        get_keyword(&value)
    }

    fn parse_char_literal(&mut self) -> Option<Token> {
        if self.is_end() {
            self.push_error_at("Unclosed char literal", self.current);
            return None;
        }

        let value = if self.peek_char() == '\\' {
            self.move_next();
            let escaped = self.move_next();
            match escaped {
                'n' => '\n',
                'r' => '\r',
                't' => '\t',
                '\\' => '\\',
                '\'' => '\'',
                '"' => '"',
                c => c,
            }
        } else {
            self.move_next()
        };

        if self.peek_char() != '\'' {
            self.push_error_at("Unclosed char literal", self.current);
            return None;
        }
        self.move_next();

        Some(Token::Char(value))
    }

    fn parse_block_comment(&mut self) {
        while !self.is_end() {
            if self.peek_char() == '*' && self.peek_next_char() == '/' {
                self.move_next();
                self.move_next();
                return;
            }

            if self.peek_char() == '\n' {
                self.line += 1;
            }
            self.move_next();
        }

        self.push_error_at("Unclosed block comment", self.current);
    }

    fn scan_token(&mut self) -> Option<Token> {
        let char = self.move_next();
        match char {
            '(' => Some(Token::LeftParen),
            ')' => Some(Token::RightParen),
            '{' => Some(Token::LeftBracket),
            '}' => Some(Token::RightBracket),
            '[' => Some(Token::LeftBrace),
            ']' => Some(Token::RightBrace),
            ',' => Some(Token::Coma),
            '.' => Some(Token::Dot),
            '-' => {
                if self.check_second_char('=') {
                    Some(Token::MinusEqual)
                } else if self.check_second_char('>') {
                    Some(Token::Arrow)
                } else {
                    Some(Token::Minus)
                }
            }
            '+' => {
                if self.check_second_char('=') {
                    Some(Token::PlusEqual)
                } else {
                    Some(Token::Plus)
                }
            }
            ';' => Some(Token::Semicolon),
            ':' => {
                if self.check_second_char(':') {
                    Some(Token::ColonColon)
                } else {
                    Some(Token::Colon)
                }
            }
            '*' => {
                if self.check_second_char('=') {
                    Some(Token::StarEqual)
                } else {
                    Some(Token::Star)
                }
            }
            '%' => {
                if self.check_second_char('=') {
                    Some(Token::PercentEqual)
                } else {
                    Some(Token::Percent)
                }
            }
            '!' => {
                if self.check_second_char('=') {
                    Some(Token::NotEqual)
                } else {
                    Some(Token::Not)
                }
            }
            '=' => {
                if self.check_second_char('=') {
                    Some(Token::EqualEqual)
                } else if self.check_second_char('>') {
                    Some(Token::FatArrow)
                } else {
                    Some(Token::Equal)
                }
            }
            '>' => {
                if self.check_second_char('=') {
                    Some(Token::GreaterEqual)
                } else if self.check_second_char('>') {
                    Some(Token::ShiftRight)
                } else {
                    Some(Token::Greater)
                }
            }
            '<' => {
                if self.check_second_char('=') {
                    Some(Token::LessEqual)
                } else if self.check_second_char('<') {
                    Some(Token::ShiftLeft)
                } else {
                    Some(Token::Less)
                }
            }
            '&' => {
                if self.check_second_char('&') {
                    Some(Token::And)
                } else {
                    Some(Token::BitAnd)
                }
            }
            '|' => {
                if self.check_second_char('|') {
                    Some(Token::Or)
                } else {
                    Some(Token::BitOr)
                }
            }
            '^' => Some(Token::BitXor),
            '?' => Some(Token::Question),
            '/' => {
                if self.check_second_char('/') {
                    while self.peek_char() != '\n' && !self.is_end() {
                        self.move_next();
                    }
                    None
                } else if self.check_second_char('*') {
                    self.parse_block_comment();
                    None
                } else if self.check_second_char('=') {
                    Some(Token::SlashEqual)
                } else {
                    Some(Token::Slash)
                }
            }
            '"' => self.parse_string(),
            '\'' => self.parse_char_literal(),
            ' ' | '\r' | '\t' => None,
            '\n' => {
                self.line += 1;
                None
            }
            c => {
                if is_num(c) {
                    self.parse_number()
                } else if is_letter(c) {
                    self.parse_word()
                } else {
                    self.push_error_at(format!("Unexpected character '{}'", c), self.start);
                    None
                }
            }
        }
    }

    fn lex_internal(&mut self) -> Vec<Token> {
        self.errors.clear();
        self.current = 0;
        self.start = 0;
        self.line = 0;

        let mut tokens: Vec<Token> = Vec::new();

        while !self.is_end() {
            self.start = self.current;
            let token = self.scan_token();
            if let Some(t) = token {
                tokens.push(t)
            }
        }

        tokens.push(Token::Eof);
        tokens
    }

    pub fn lex_file(&mut self) -> Vec<Token> {
        self.lex_internal()
    }

    pub fn lex_with_diagnostics(&mut self) -> Result<Vec<Token>, Vec<LexError>> {
        let tokens = self.lex_internal();
        if self.errors.is_empty() {
            Ok(tokens)
        } else {
            Err(self.errors.clone())
        }
    }
}
