use core::panic;
use std::string::ParseError;

use crate::lexer::Token;
use crate::ast::{BlockStmt, EmptyStmt, Expr, ExprStmt, Literal, Stmt, VarStmt};

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
        println!("curr token {}", self.current_token());
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
            Token::True => Expr::Literal(Literal::Bool(true)),
            Token::False => Expr::Literal(Literal::Bool(false)),
            Token::String(s) => Expr::Literal(Literal::String(s.to_string())),
            Token::Identifier(i) => Expr::Literal(Literal::Identifier(i.to_string())),
            _ => panic!("No expression found for literal token {}", self.current_token())
        }
    }

    fn parse_binary_expr(&mut self, left: Expr) -> Expr {
        let binding = self.get_current_token_power();
        let operator = self.get_token_and_move().clone();
        Expr::Binary(Box::new(left), operator, Box::new(self.parse_expr(binding)))
    }

    fn parse_prefix(&mut self) -> Expr{
        let operator = self.get_token_and_move();
        Expr::Unary(operator.clone(), Box::new(self.parse_expr(Binding::Unary)))
    }

    fn parse_var(&mut self) -> Stmt {
        let declaratio = self.get_token_and_move();
        let name = match self.get_token_and_move() {
            Token::Identifier(s) => s.to_string(),
            _ => panic!("Token has no value")
        };

        let assignment = self.get_token_and_move();
        let assignment_value = self.parse_expr(Binding::Assign);

        if self.get_token_and_move() != &Token::Semicolon {
            panic!("Expected semicolon, but {}", self.current_token())
        }

        Stmt::Var(VarStmt{ name: name, assignment: assignment_value })
    }

    fn parse_assignment(&mut self, left: Expr) -> Expr {
        let binding = self.get_current_token_power();
        self.get_token_and_move();

        let right = self.parse_expr(binding);

        Expr::Assignment(Box::new(left), Box::new(right))
    }

    fn parse_if(&self) -> Stmt {
        Stmt::Empty(EmptyStmt {  })
    }

    fn parse_for(&self) -> Stmt {
        Stmt::Empty(EmptyStmt {  })
    }

    fn parse_fun(&self) -> Stmt {
        Stmt::Empty(EmptyStmt {  })
    }

    fn parse_class(&self) -> Stmt {
        Stmt::Empty(EmptyStmt {  })
    }

    fn parse_import(&self) -> Stmt {
        Stmt::Empty(EmptyStmt {  })
    }

    fn handle_literal(&mut self) -> Option<Expr> {
        //TODO extend
        match self.current_token() {
            Token::Number(_) => Some(self.parse_primary_expr()),
            Token::String(_) => Some(self.parse_primary_expr()),
            Token::True => Some(self.parse_primary_expr()),
            Token::False => Some(self.parse_primary_expr()),
            Token::Identifier(_) => Some(self.parse_primary_expr()),
            //Unary
            Token::Minus => Some(self.parse_prefix()),
            Token::Bang => Some(self.parse_prefix()),
            Token::Semicolon => None,
            _ => panic!("No handler found for literal token {}", self.current_token())
        }
    }

    fn handle_stmt(&mut self) -> Option<Stmt> {
        //TODO extend
        match self.current_token() {
            Token::LeftBrace => Some(self.parse_block_stmt()),
            Token::Var => Some(self.parse_var()),
            Token::If => Some(self.parse_if()),
            Token::For => Some(self.parse_for()),
            Token::Fun => Some(self.parse_fun()),
            Token::Class => Some(self.parse_class()),
            Token::Import => Some(self.parse_import()),
            _ => None
        }
    }

    fn handle_operator(&mut self, left: Expr) -> Expr {
        //TODO extend
        match self.current_token() {
            //Math
            Token::Plus => self.parse_binary_expr(left),
            Token::Minus => self.parse_binary_expr(left),
            Token::Star => self.parse_binary_expr(left),
            Token::Slash => self.parse_binary_expr(left),
            Token::Percent => self.parse_binary_expr(left),
            //Relation
            Token::NotEqual => self.parse_binary_expr(left),
            Token::EqualEqual => self.parse_binary_expr(left),
            Token::Greater => self.parse_binary_expr(left),
            Token::GreaterEqual => self.parse_binary_expr(left),
            Token::Less => self.parse_binary_expr(left),
            Token::LessEqual => self.parse_binary_expr(left),
            //Logical
            Token::And => self.parse_binary_expr(left),
            Token::Or => self.parse_binary_expr(left),
            //Assignment
            Token::Equal => self.parse_assignment(left),
            Token::PlusEqual => self.parse_assignment(left),
            Token::MinusEqual => self.parse_assignment(left),
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
            Token::Percent => Binding::Mult,
            Token::NotEqual => Binding::Relation,
            Token::EqualEqual => Binding::Relation,
            Token::Greater => Binding::Relation,
            Token::GreaterEqual => Binding::Relation,
            Token::Less => Binding::Relation,
            Token::LessEqual => Binding::Relation,
            Token::And => Binding::Logic,
            Token::Or => Binding::Logic,
            Token::Bang => Binding::Unary,
            Token::Equal => Binding::Assign,
            Token::PlusEqual => Binding::Assign,
            Token::MinusEqual => Binding::Assign,
            _ => Binding::Def
        }
    }

}
