use core::panic;
use std::string::ParseError;

use crate::lexer::Token;
use crate::ast::{BlockStmt, EmptyStmt, Expr, ExprStmt, Literal, Stmt};

#[derive(Debug, Clone, PartialEq, PartialOrd)]
enum Binding {
    Def = 0,
    Coma,
    Assign,
    Logic,
    Relation,
    Add,
    Mult,
    Unary,
    Call,
    Member,
    Primary
}

pub struct Parser {
    tokens: Vec<Token>,
    errors: Vec<ParseError>,
    current: usize
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Parser {
        Parser {
            tokens,
            errors: Vec::new(),
            current: 0
        }
    }

    pub fn parse(&mut self) -> BlockStmt {
        let mut stmts = Vec::new();

        while self.has_tokens() {
            stmts.push(self.parse_stmt());
        }

        BlockStmt { stmts }
    }

    fn current_token(&self) -> &Token {
        &self.tokens[self.current]
    }

    fn get_token_and_move(&mut self) -> &Token {
        let token = &self.tokens[self.current];
        self.current += 1;
        token
    }

    fn has_tokens(&self) -> bool {
        self.current < self.tokens.len() && self.tokens[self.current] != Token::Eof
    }

    fn parse_stmt(&mut self) -> Stmt {
        let stmt = self.handle_stmt();

        let res = match stmt {
            Some(s) => s,
            None => self.parse_exrp_stmt()
        };

        res
    }

    fn parse_exrp_stmt(&mut self) -> Stmt {
        let expr = self.parse_expr(Binding::Def);

        if self.get_token_and_move() != &Token::Semicolon {
            panic!("Expected semicolon, but {}", self.current_token())
        }

        Stmt::Expr(ExprStmt { expr })
    }

    fn parse_expr(&mut self, binding: Binding) -> Expr {
        let mut left = match self.handle_literal() {
            Some(l) => l,
            None => Expr::Semicolon
        };

        println!("curr token {}", self.current_token());

        while self.get_current_token_power() > binding {
            left = self.handle_operator(left);
        }

        left
    }

    fn parse_block_stmt(&mut self) -> Stmt {
        if self.current_token() != &Token::LeftBrace {
            panic!("Expected {{, but found {}", self.current_token())
        }

        let mut body = Vec::new();

        while self.has_tokens() && self.current_token() != &Token::RightBrace {
            println!("curr token {}", self.current_token());
            self.get_token_and_move();
            body.push(self.parse_stmt());
        }

        if self.get_token_and_move() != &Token::RightBrace {
            panic!("Expected }}, but found {}", self.current_token())
        }

        Stmt::Block(BlockStmt { stmts: body })
    }

    fn parse_primary_expr(&mut self) -> Expr {
        match self.get_token_and_move() {
            Token::Number(n) => Expr::Literal(Literal::Num(*n)),
            Token::String(s) => Expr::Literal(Literal::String(s.to_string())),
            Token::Identifier(i) => Expr::Literal(Literal::Identifier(i.to_string())),
            _ => panic!("No expression found for literal token {}", self.current_token())
        }
    }

    fn parse_binary_expr(&mut self, left: Expr, binding: Binding) -> Expr {
        let operator = self.get_token_and_move().clone();
        Expr::Binary(Box::new(left), operator, Box::new(self.parse_expr(binding)))
    }

    fn handle_literal(&mut self) -> Option<Expr> {
        //TODO extend
        match self.current_token() {
            Token::Number(_) => Some(self.parse_primary_expr()),
            Token::String(_) => Some(self.parse_primary_expr()),
            Token::Identifier(_) => Some(self.parse_primary_expr()),
            Token::Semicolon => None,
            _ => panic!("No handler found for literal token {}", self.current_token())
        }
    }

    fn handle_stmt(&mut self) -> Option<Stmt> {
        //TODO extend
        match self.current_token() {
            Token::LeftBrace => Some(self.parse_block_stmt()),
            _ => None
        }
    }

    fn handle_operator(&mut self, left: Expr) -> Expr {
        //TODO extend
        match self.current_token() {
            //Math
            Token::Plus => self.parse_binary_expr(left, Binding::Add),
            Token::Minus => self.parse_binary_expr(left, Binding::Add),
            Token::Star => self.parse_binary_expr(left, Binding::Mult),
            Token::Slash => self.parse_binary_expr(left, Binding::Mult),
            //Relation
            Token::NotEqual => self.parse_binary_expr(left, Binding::Relation),
            Token::Equal => self.parse_binary_expr(left, Binding::Relation),
            Token::Greater => self.parse_binary_expr(left, Binding::Relation),
            Token::GreaterEqual => self.parse_binary_expr(left, Binding::Relation),
            Token::Less => self.parse_binary_expr(left, Binding::Relation),
            Token::LessEqual => self.parse_binary_expr(left, Binding::Relation),
            _ => panic!("No handler found for operator token {}", self.current_token())
        }
    }

    fn get_current_token_power(&self) -> Binding{
        //TODO extend
        match self.current_token() {
            Token::Number(_) => Binding::Primary,
            Token::String(_) => Binding::Primary,
            Token::Identifier(_) => Binding::Primary,
            Token::Plus => Binding::Add,
            Token::Minus => Binding::Add,
            Token::Star => Binding::Mult,
            Token::Slash => Binding::Mult,
            _ => Binding::Def
        }
    }

}
