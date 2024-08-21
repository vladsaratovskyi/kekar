use std::fmt::{self, Display};
use std::fs::{self};

#[derive(Debug, Clone, PartialEq)]
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
    Identifier(String), String(String), Number(f64),

    // Keywords.
    And, Class, Else, False, Fun, For, If, None, Or,
    Print, Return, Super, This, True, Var, While,

    Eof
}

impl Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", self)
    }
}

fn get_keyword(key: &str) -> Option<Token> {
    match key {
        "var" => Some(Token::Var),
        "class"=> Some(Token::Class),
        "else" => Some(Token::Else),
        "fun" => Some(Token::Fun),
        "for" => Some(Token::For),
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
        s => Some(Token::Identifier(s.to_string()))
    }
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
    line: usize,
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
    
    fn get_num_value(&self) -> f64 {
        self.source[self.start..self.current].parse::<f64>().unwrap()
    }

    fn parse_string(&mut self) -> Option<Token> {
        while self.peek_char() != '"' && !self.is_end() {
            if self.peek_char() == '\n' { self.line += 1 }
            self.move_next();
        }

        if self.is_end() {
            panic!("Unclosed String at {}!", self.current);
        }

        self.move_next();
        Some(Token::String(self.get_value()))
    }

    fn parse_number(&mut self) -> Option<Token> {
        while is_num(self.peek_char()) { self.move_next(); continue; }

        if self.peek_char() == '.' && is_num(self.peek_next_char()) {
            self.move_next();
            while is_num(self.peek_char()) { self.move_next(); continue; }
        }

        Some(Token::Number(self.get_num_value()))
    }

    fn parse_word(&mut self) -> Option<Token> {
        while is_letter(self.peek_char()) { self.move_next(); continue; }

        let value = self.get_value();
        Some(get_keyword(&value).unwrap())
    }
 
    fn scan_token(&mut self) -> Option<Token> {
        let char = self.move_next();
        match char {
            '(' => Some(Token::LeftParen),
            ')' => Some(Token::RightParen),
            '{' => Some(Token::LeftBrace),
            '}' => Some(Token::RightBrace),
            ',' => Some(Token::Coma),
            '.' => Some(Token::Dot),
            '-' => Some(Token::Minus),
            '+' => Some(Token::Plus),
            ';' => Some(Token::Semicolon),
            '*' => Some(Token::Star),
            '!' => {
                let res = self.check_second_char('=');
                if res {
                    Some(Token::BangEqual)
                } else {
                    Some(Token::Bang)
                }
            },
            '=' => {
                let res = self.check_second_char('=');
                if res {
                    Some(Token::EqualEqual)
                } else {
                    Some(Token::Equal)
                }
            },
            '>' => {
                let res = self.check_second_char('=');
                if res {
                    Some(Token::GreaterEqual)
                } else {
                    Some(Token::Greater)
                }
            },
            '<' => {
                let res = self.check_second_char('=');
                if res {
                    Some(Token::LessEqual)
                } else {
                    Some(Token::Less)
                }
            },
            '/' => {
                let res = self.check_second_char('/');
                if res {
                    while self.peek_char() != '\n' && !self.is_end() {
                        self.move_next();
                    }
                    None
                } else {
                    Some(Token::Slash)
                }
            },
            '"' => {
                self.parse_string()
            },
            '\n' => {
                self.line += 1;
                None
            },
            c => {
                if is_num(c) {
                    self.parse_number()
                } else if is_letter(c) {
                    self.parse_word()
                } else {
                    None
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
            let token = self.scan_token();
            match token {
                Some(t) => tokens.push(t),
                None => ()
            }
        }
    
        tokens.push(Token::Eof);
        tokens
    }
}