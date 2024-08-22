use core::panic;
use std::string::ParseError;

use crate::lexer::Token;
use crate::ast::{BlockStmt, Expr, ExprStmt, ForStmt, FunStmt, IfStmt, Literal, Param, Stmt, Type, VarStmt};

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

    fn expect(&mut self, expected: &Token) -> &Token {
        let current = self.current_token();
        
        if std::mem::discriminant(current) != std::mem::discriminant(expected) {
            panic!("Expected {} but found {}", expected, current);
        }

        return self.get_token_and_move();
    }

    fn parse_stmt(&mut self) -> Stmt {
        let stmt = self.handle_stmt();

        

        match stmt {
            Some(s) => s,
            None => self.parse_exrp_stmt()
        }
    }

    fn parse_exrp_stmt(&mut self) -> Stmt {
        let expr = self.parse_expr(Binding::Def);

        self.expect(&Token::Semicolon);

        Stmt::Expr(ExprStmt { expr })
    }

    fn parse_expr(&mut self, binding: Binding) -> Expr {
        let mut left = match self.handle_literal() {
            Some(l) => l,
            None => panic!("Unable to parse literal {}", self.current_token())
        };

        while self.get_current_token_power() > binding {
            left = self.handle_operator(left);
        }

        left
    }

    fn parse_block_stmt(&mut self) -> Stmt {
        self.expect(&Token::LeftBrace);

        let mut body = Vec::new();

        while self.has_tokens() && self.current_token() != &Token::RightBrace {
            body.push(self.parse_stmt());
        }

        self.expect(&Token::RightBrace);

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

    fn parse_prefix_expr(&mut self) -> Expr{
        let operator = self.get_token_and_move();
        Expr::Unary(operator.clone(), Box::new(self.parse_expr(Binding::Unary)))
    }

    fn parse_var_stmt(&mut self) -> Stmt {
        self.expect(&Token::Var);
        let name = match self.expect(&Token::Identifier("any".to_string())) {
            Token::Identifier(s) => s.to_string(),
            _ => panic!("Token has no value")
        };

        self.expect(&Token::Equal);
        let assignment = self.parse_expr(Binding::Assign);

        self.expect(&Token::Semicolon);

        Stmt::Var(VarStmt{ name, assignment, var_type: Type::None })
    }

    fn parse_assignment_exrp(&mut self, left: Expr) -> Expr {
        let binding = self.get_current_token_power();
        self.get_token_and_move();

        let right = self.parse_expr(binding);

        Expr::Assignment(Box::new(left), Box::new(right))
    }

    fn parse_if_stmt(&mut self) -> Stmt {
        self.get_token_and_move();
        let condition = self.parse_expr(Binding::Assign);
        let main = self.parse_block_stmt();

        let mut alter = Stmt::Empty;
        if self.current_token() == &Token::Else {
            self.get_token_and_move();

            if self.current_token() == &Token::If {
                alter = self.parse_if_stmt();
            } else {
                alter = self.parse_block_stmt();
            }
        }

        Stmt::If(IfStmt { condition, main: Box::new(main), alter: Box::new(alter) })
    }

    fn parse_for_stmt(&mut self) -> Stmt {
        self.get_token_and_move();
        //initialize with some values
        let mut for_stmt = ForStmt { item : "any".to_string(), use_index: false, iterator: Expr::Empty, body: BlockStmt { stmts: Vec::new() } };
        
        let item_name = self.expect(&Token::Identifier("any".to_string()));
        
        for_stmt.item = item_name.to_string();

        if self.current_token() == &Token::Coma {
            self.expect(&Token::Coma);
            self.expect(&Token::Identifier("any".to_string()));
            for_stmt.use_index = true;
        }
        
        self.expect(&Token::In);
        let iterator = self.parse_expr(Binding::Def);
        for_stmt.iterator = iterator;
        
        let body = match self.parse_block_stmt() {
            Stmt::Block(b) => b,
            _ => panic!("Incorrect type of block statement!")
        };
        for_stmt.body = body;

        Stmt::For(for_stmt)
    }

    fn parse_fun_stmt(&mut self) -> Stmt {
        self.expect(&Token::Fun);
        let fun_name = match self.expect(&Token::Identifier("any".to_string())) {
            Token::Identifier(s) => s.to_string(),
            _ => panic!("Token has no value")
        };

        let mut params = Vec::new();
        self.expect(&Token::LeftParen);

        while self.has_tokens() && self.current_token() != &Token::RightParen {
            let mut param = Param { name: "".to_string(), param_type: Type::None}; 
            let param_type = match self.expect(&Token::Identifier("any".to_string())) {
                Token::Identifier(s) => s,
                _ => panic!("param has invalid type")
            };
            param.param_type = parse_type(param_type);
            let param_name = match self.expect(&Token::Identifier("any".to_string())) {
                Token::Identifier(s) => s.to_string(),
                _ => panic!("param has invalid name")
            };
            param.name = param_name;

            params.push(param);

            if !matches!(self.current_token(), &Token::RightParen | &Token::Eof) {
                self.expect(&Token::Coma);
            }
        }

        self.expect(&Token::RightParen);
        self.expect(&Token::Colon);

        let fun_type = match self.expect(&Token::Identifier("any".to_string())) {
            Token::Identifier(s) => parse_type(s),
            _ => panic!("fn {} has invalid type", fun_name)
        };

        let block = match self.parse_block_stmt() {
            Stmt::Block(b) => b,
            _ => panic!("Incorrect type of block statement!")
        };

        Stmt::Fun(FunStmt { name: fun_name, return_type: fun_type, params, block })
    }

    fn parse_fun_call_expr(&self) -> Expr {
        Expr::Empty
    }

    fn parser_member_exrp(&self) -> Expr {
        Expr::Empty
    }

    fn parse_class_stmt(&self) -> Stmt {
        Stmt::Empty
    }

    fn parse_import_stmt(&self) -> Stmt {
        Stmt::Empty
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
            Token::Minus => Some(self.parse_prefix_expr()),
            Token::Bang => Some(self.parse_prefix_expr()),
            Token::Semicolon => None,
            _ => panic!("No handler found for literal token {}", self.current_token())
        }
    }

    fn handle_stmt(&mut self) -> Option<Stmt> {
        //TODO extend
        match self.current_token() {
            Token::LeftBrace => Some(self.parse_block_stmt()),
            Token::Var => Some(self.parse_var_stmt()),
            Token::If => Some(self.parse_if_stmt()),
            Token::For => Some(self.parse_for_stmt()),
            Token::Fun => Some(self.parse_fun_stmt()),
            Token::Class => Some(self.parse_class_stmt()),
            Token::Import => Some(self.parse_import_stmt()),
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
            Token::Equal => self.parse_assignment_exrp(left),
            Token::PlusEqual => self.parse_assignment_exrp(left),
            Token::MinusEqual => self.parse_assignment_exrp(left),
            //Call, Member
            Token::LeftParen => self.parse_fun_call_expr(),
            Token::LeftBrace => self.parser_member_exrp(),
            Token::Dot => self.parser_member_exrp(),
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

fn parse_type(type_name: &str) -> Type {
    match type_name {
        "num" => Type::Num,
        "String" => Type::String,
        "bool" => Type::Bool,
        "" => Type::None,
        s => Type::Identifier(s.to_string()),
    }
}