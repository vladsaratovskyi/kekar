use std::collections::HashMap;
use std::fmt::{self, Display};
use std::fs::{self};

#[derive(Debug)]
pub struct Token {
    pub token_type: TokenType,
    lexemme: String,
    literal: String
}

impl Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Type {}", self.token_type)
    }
}

#[derive(Debug, Clone)]
pub enum TokenType {
    // Single-character tokens.
    LeftParen, RightParen, LeftBrace, RightBrace,
    Coma, Dot, Minus, Plus, Semicolon, Slack, Star,

    // One or two character tokens.
    Bang, BangEqual,
    Equal, EqualEqual,
    Greater, GreaterEqual,
    Less, LessEqueal,

    // Literals.
    Identifier, String, Number,

    // Keywords.
    And, Class, Else, False, Fun, For, If, Nil, Or,
    Print, Return, Super, This, True, Var, While,

    Eof
}

impl Display for TokenType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", self)
    }
}

struct InvalidKeyword;

fn get_keyword(key: &str) -> Option<TokenType> {
    let mut keywords = HashMap::new();
    keywords.insert("var", TokenType::Var);
    keywords.insert("class", TokenType::Class);
    keywords.insert("else", TokenType::Else);
    keywords.insert("fun", TokenType::Fun);
    keywords.insert("for", TokenType::For);
    keywords.insert("if", TokenType::If);
    keywords.insert("nil", TokenType::Nil);
    keywords.insert("or", TokenType::Or);
    keywords.insert("print", TokenType::Print);
    keywords.insert("return", TokenType::Return);
    keywords.insert("super", TokenType::Super);
    keywords.insert("this", TokenType::This);
    keywords.insert("true", TokenType::True);
    keywords.insert("while", TokenType::While);
    
    let res = keywords.get(key).unwrap().clone();
    Some(res)
}

fn is_num(char: char) -> bool {
    let nums = "0123456789";
    nums.contains(char)
}

fn is_letter(char: char) -> bool {
    let nums = "abcdefghijklmnopqrstuvwxyz_";
    nums.contains(char)
}

fn add_token(token_type: TokenType, tokens: &mut Vec<Token>) {
    tokens.push(Token { token_type, lexemme: String::from(""), literal: String::from("")})
}

fn add_token_with_literal(token_type: TokenType, literal: String, tokens: &mut Vec<Token>) {
    tokens.push(Token { token_type, lexemme: String::from(""), literal })
}

fn add_token_with_lexemme(token_type: TokenType, lexemme: String, tokens: &mut Vec<Token>) {
    tokens.push(Token { token_type, lexemme, literal : String::from("") })
}

pub struct Lexer {
    current: usize,
    start: usize,
    line: i32,
    source: String
}

impl Lexer {
    pub fn new(path_to_file: &String) -> Self {
        Self {
            source: fs::read_to_string(path_to_file).expect("Could not read file."),
            current : 0,
            start : 0,
            line : 0
        }
    }

    fn check_second_char(&mut self, char: char) -> bool {
        if self.is_end() { return false }
        if self.source.as_bytes()[self.current] as char != char { return false }

        self.current += 1;
        true
    }

    fn peek_char(&mut self) -> char {
        if self.is_end() { return '\0'}
        self.source.as_bytes()[self.current] as char
    }

    fn peek_next_char(&mut self) -> char {
        if self.is_end() { return '\0' }
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

    fn parse_string(&mut self, tokens: &mut Vec<Token>) {
        while self.peek_char() != '"' && !self.is_end() {
            if self.peek_char() == '\n' { self.line += 1 }
            self.move_next();
        }

        if self.is_end() {
            panic!("Unclosed String at {}!", self.current);
        }

        self.move_next();
        add_token_with_literal(TokenType::String,self.get_value(), tokens)
    }

    fn parse_number(&mut self, tokens: &mut Vec<Token>) {
        while is_num(self.peek_char()) { self.move_next(); continue; }

        if self.peek_char() == '.' && is_num(self.peek_next_char()) {
            self.move_next();
            while is_num(self.peek_char()) { self.move_next(); continue; }
        }

        add_token_with_literal(TokenType::Number, self.get_value(), tokens)
    }

    fn parse_word(&mut self, tokens: &mut Vec<Token>) {
        while is_letter(self.peek_char()) { self.move_next(); continue; }

        let value = self.get_value();
        add_token_with_lexemme(get_keyword(&value).unwrap(), value, tokens)
    }
 
    fn scan_token(&mut self, tokens: &mut Vec<Token>) {
        let char = self.move_next();
        match char {
            '(' => add_token(TokenType::LeftParen, tokens),
            ')' => add_token(TokenType::RightParen, tokens),
            '{' => add_token(TokenType::LeftBrace, tokens),
            '}' => add_token(TokenType::RightBrace, tokens),
            ',' => add_token(TokenType::Coma, tokens),
            '.' => add_token(TokenType::Dot, tokens),
            '-' => add_token(TokenType::Minus, tokens),
            '+' => add_token(TokenType::Plus, tokens),
            ';' => add_token(TokenType::Semicolon, tokens),
            '*' => add_token(TokenType::Star, tokens),
            '!' => {
                let res = self.check_second_char('=');
                if res {
                    add_token(TokenType::BangEqual, tokens);
                } else {
                    add_token(TokenType::Bang, tokens);
                }
            },
            '=' => {
                let res = self.check_second_char('=');
                if res {
                    add_token(TokenType::EqualEqual, tokens);
                } else {
                    add_token(TokenType::Equal, tokens);
                }
            },
            '>' => {
                let res = self.check_second_char('=');
                if res {
                    add_token(TokenType::GreaterEqual, tokens);
                } else {
                    add_token(TokenType::Greater, tokens);
                }
            },
            '<' => {
                let res = self.check_second_char('=');
                if res {
                    add_token(TokenType::LessEqueal, tokens);
                } else {
                    add_token(TokenType::Less, tokens);
                }
            },
            '/' => {
                let res = self.check_second_char('/');
                println!("{}", res);
                if res {
                    while self.peek_char() != '\n' && !self.is_end() {
                        self.move_next();
                    }
                } else {
                    add_token(TokenType::Slack, tokens)
                }
            },
            '"' => self.parse_string(tokens),
            '\n' => self.line += 1,
            c => {
                if is_num(c) {
                    self.parse_number(tokens);
                } else if is_letter(c) {
                    self.parse_word(tokens);
                }
            },
            _ => ()
        }
    }

    pub fn lex_file(&mut self) -> Vec<Token> {
        println!("========= File Contents =========");
        println!("{}", self.source);
        println!("========= ============= =========");
    
        let mut tokens: Vec<Token> = Vec::new();
        let copy = self.source.clone();
        let mut chars = copy.chars();
    
        while !self.is_end() {
            self.start = self.current;
            self.scan_token(&mut tokens);
        }
    
        tokens.push(Token { token_type: TokenType::Eof, lexemme: String::from(""), literal: String::from("") });
        tokens
    }
}