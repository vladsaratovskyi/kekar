use std::collections::HashMap;
use std::fmt::{self, Display};
use std::fs::{self};

#[derive(Debug, Clone)]
pub enum Token {
    // Single-character tokens.
    LeftParen, RightParen, LeftBrace, RightBrace,
    Coma, Dot, Minus, Plus, Semicolon, Slash, Star,

    // One or two character tokens.
    Bang, BangEqual,
    Equal, EqualEqual,
    Greater, GreaterEqual,
    Less, LessEqual,

    // Literals.
    Identifier, String(String), Number(i32),

    // Keywords.
    And, Class, Else, False, Fun, For, If, Nil, Or,
    Print, Return, Super, This, True, Var, While,

    Eof
}

impl Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", self)
    }
}

fn get_keyword(key: &str) -> Option<Token> {
    let mut keywords = HashMap::new();
    keywords.insert("var", Token::Var);
    keywords.insert("class", Token::Class);
    keywords.insert("else", Token::Else);
    keywords.insert("fun", Token::Fun);
    keywords.insert("for", Token::For);
    keywords.insert("if", Token::If);
    keywords.insert("nil", Token::Nil);
    keywords.insert("or", Token::Or);
    keywords.insert("print", Token::Print);
    keywords.insert("return", Token::Return);
    keywords.insert("super", Token::Super);
    keywords.insert("this", Token::This);
    keywords.insert("true", Token::True);
    keywords.insert("false", Token::False);
    keywords.insert("while", Token::While);
    keywords.insert("and", Token::And);
    keywords.insert("ge", Token::GreaterEqual);
    keywords.insert("gt", Token::Greater);
    keywords.insert("le", Token::LessEqual);
    keywords.insert("lt", Token::Less);
    
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
    
    fn get_int_value(&self) -> i32 {
        self.source[self.start..self.current].parse::<i32>().unwrap()
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
        tokens.push(Token::String(self.get_value()))
    }

    fn parse_number(&mut self, tokens: &mut Vec<Token>) {
        while is_num(self.peek_char()) { self.move_next(); continue; }

        if self.peek_char() == '.' && is_num(self.peek_next_char()) {
            self.move_next();
            while is_num(self.peek_char()) { self.move_next(); continue; }
        }

        tokens.push(Token::Number(self.get_int_value()))
    }

    fn parse_word(&mut self, tokens: &mut Vec<Token>) {
        while is_letter(self.peek_char()) { self.move_next(); continue; }

        let value = self.get_value();
        tokens.push(get_keyword(&value).unwrap());
    }
 
    fn scan_token(&mut self, tokens: &mut Vec<Token>) {
        let char = self.move_next();
        match char {
            '(' => tokens.push(Token::LeftParen),
            ')' => tokens.push(Token::RightParen),
            '{' => tokens.push(Token::LeftBrace),
            '}' => tokens.push(Token::RightBrace),
            ',' => tokens.push(Token::Coma),
            '.' => tokens.push(Token::Dot),
            '-' => tokens.push(Token::Minus),
            '+' => tokens.push(Token::Plus),
            ';' => tokens.push(Token::Semicolon),
            '*' => tokens.push(Token::Star),
            '!' => {
                let res = self.check_second_char('=');
                if res {
                    tokens.push(Token::BangEqual);
                } else {
                    tokens.push(Token::Bang);
                }
            },
            '=' => {
                let res = self.check_second_char('=');
                if res {
                    tokens.push(Token::EqualEqual);
                } else {
                    tokens.push(Token::Equal);
                }
            },
            '>' => {
                let res = self.check_second_char('=');
                if res {
                    tokens.push(Token::GreaterEqual);
                } else {
                    tokens.push(Token::Greater);
                }
            },
            '<' => {
                let res = self.check_second_char('=');
                if res {
                    tokens.push(Token::LessEqual);
                } else {
                    tokens.push(Token::Less);
                }
            },
            '/' => {
                let res = self.check_second_char('/');
                if res {
                    while self.peek_char() != '\n' && !self.is_end() {
                        self.move_next();
                    }
                } else {
                    tokens.push(Token::Slash)
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
            }
        }
    }

    pub fn lex_file(&mut self) -> Vec<Token> {
        println!("========= File Contents =========");
        println!("{}", self.source);
        println!("========= ============= =========");
    
        let mut tokens: Vec<Token> = Vec::new();
    
        while !self.is_end() {
            self.start = self.current;
            self.scan_token(&mut tokens);
        }
    
        tokens.push(Token::Eof);
        tokens
    }
}