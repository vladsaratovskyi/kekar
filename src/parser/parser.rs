use core::panic;
use std::string::ParseError;

use crate::{
    ast::ast::{
        BlockStmt, ClassStmt, Expr, ExprStmt, ForStmt, FunStmt, IfStmt, Literal, Param, Stmt, Type,
        VarStmt,
    },
    lexer::lexer::Token,
};

use super::helper::parse_type;

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
    Primary,
}

pub struct Parser {
    tokens: Vec<Token>,
    errors: Vec<ParseError>,
    current: usize,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Parser {
        Parser {
            tokens,
            errors: Vec::new(),
            current: 0,
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
        println!("{}", token);
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

    fn expect_identifier_get_name(&mut self) -> String {
        match self.expect(&Token::Identifier("any".to_string())) {
            Token::Identifier(s) => s.to_string(),
            _ => panic!("Token has no value"),
        }
    }

    fn parse_stmt(&mut self) -> Stmt {
        let stmt = self.handle_stmt();

        match stmt {
            Some(s) => s,
            None => self.parse_exrp_stmt(),
        }
    }

    fn parse_exrp_stmt(&mut self) -> Stmt {
        let expr = self.parse_expr(Binding::Def);
        self.expect(&Token::Semicolon);

        Stmt::Expr(ExprStmt { expr })
    }

    fn parse_expr(&mut self, binding: Binding) -> Expr {
        let mut left = self
            .handle_literal()
            .expect(&format!("Unable to parse literal {}", self.current_token()));

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
            _ => panic!(
                "No expression found for literal token {}",
                self.current_token()
            ),
        }
    }

    fn parse_binary_expr(&mut self, left: Expr) -> Expr {
        dbg!(left.clone());
        let binding = self.get_current_token_power();
        let operator = self.get_token_and_move().clone();
        Expr::Binary(Box::new(left), operator, Box::new(self.parse_expr(binding)))
    }

    fn parse_prefix_expr(&mut self) -> Expr {
        let operator = self.get_token_and_move();
        Expr::Unary(operator.clone(), Box::new(self.parse_expr(Binding::Unary)))
    }

    fn parse_var_stmt(&mut self) -> Stmt {
        self.expect(&Token::Var);
        let name = self.expect_identifier_get_name();
        let mut field_type = Type::None;

        if self.current_token() == &Token::Colon {
            self.expect(&Token::Colon);
            field_type = parse_type(&self.expect_identifier_get_name());
        }

        let mut assignment = Expr::Empty;
        if self.current_token() != &Token::Semicolon {
            self.expect(&Token::Equal);
            assignment = self.parse_expr(Binding::Assign);
        }

        self.expect(&Token::Semicolon);

        Stmt::Var(VarStmt {
            name,
            assignment,
            var_type: field_type,
        })
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

        Stmt::If(IfStmt {
            condition,
            main: Box::new(main),
            alter: Box::new(alter),
        })
    }

    fn parse_for_stmt(&mut self) -> Stmt {
        self.get_token_and_move();
        //initialize with some values
        let mut for_stmt = ForStmt {
            item: self.expect_identifier_get_name(),
            use_index: false,
            iterator: Expr::Empty,
            body: Box::new(Stmt::Empty),
        };

        if self.current_token() == &Token::Coma {
            self.expect(&Token::Coma);
            self.expect(&Token::Identifier("any".to_string()));
            for_stmt.use_index = true;
        }

        self.expect(&Token::In);
        let iterator = self.parse_expr(Binding::Def);
        for_stmt.iterator = iterator;

        let body = self.parse_block_stmt();
        for_stmt.body = Box::new(body);

        Stmt::For(for_stmt)
    }

    fn parse_fun_stmt(&mut self) -> Stmt {
        self.expect(&Token::Fun);
        let fun_name = self.expect_identifier_get_name();

        let mut params = Vec::new();
        self.expect(&Token::LeftParen);

        while self.has_tokens() && self.current_token() != &Token::RightParen {
            let mut param = Param {
                name: "".to_string(),
                param_type: Type::None,
            };
            param.param_type = parse_type(&self.expect_identifier_get_name());
            param.name = self.expect_identifier_get_name();
            params.push(param);

            if !matches!(self.current_token(), &Token::RightParen | &Token::Eof) {
                self.expect(&Token::Coma);
            }
        }

        self.expect(&Token::RightParen);
        self.expect(&Token::Colon);

        let fun_type = parse_type(&self.expect_identifier_get_name());

        let block = self.parse_block_stmt();

        Stmt::Fun(FunStmt {
            name: fun_name,
            return_type: fun_type,
            params,
            block: Box::new(block),
        })
    }

    fn parse_fun_call_expr(&self) -> Expr {
        Expr::Empty
    }

    fn parser_member_exrp(&self) -> Expr {
        Expr::Empty
    }

    fn parse_class_stmt(&mut self) -> Stmt {
        self.expect(&Token::Class);
        let class_name = self.expect_identifier_get_name();
        let block = self.parse_block_stmt();

        Stmt::Class(ClassStmt {
            name: class_name,
            block: Box::new(block),
        })
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
            Token::Not => Some(self.parse_prefix_expr()),
            Token::Semicolon => None,
            _ => panic!(
                "No handler found for literal token {}",
                self.current_token()
            ),
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
            _ => None,
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
            _ => panic!(
                "No handler found for operator token {}",
                self.current_token()
            ),
        }
    }

    fn get_current_token_power(&self) -> Binding {
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
            Token::Not => Binding::Unary,
            Token::Equal => Binding::Assign,
            Token::PlusEqual => Binding::Assign,
            Token::MinusEqual => Binding::Assign,
            _ => Binding::Def,
        }
    }
}

mod tests {
    use crate::{
        ast::ast::{BlockStmt, ClassStmt, Expr, ExprStmt, ForStmt, FunStmt, IfStmt, Literal, Param, Stmt, Type, VarStmt},
        lexer::lexer::Token,
        parser::parser::{Binding, Parser},
    };

    #[test]
    fn parse_var_stmt_with_literal() {
        let tokens = vec![
            Token::Var,
            Token::Identifier("variable".to_string()),
            Token::Colon,
            Token::Identifier("Num".to_string()),
            Token::Equal,
            Token::Number(1.0),
            Token::Semicolon,
        ];

        let mut parser = Parser::new(tokens);
        let res = parser.parse_var_stmt();

        let expected = Stmt::Var(VarStmt {
            name: "variable".to_string(),
            assignment: Expr::Literal(Literal::Num(1.0)),
            var_type: Type::Num,
        });

        assert_eq!(res, expected);
    }

    #[test]
    fn parse_var_stmt_as_declaration() {
        let tokens = vec![
            Token::Var,
            Token::Identifier("variable".to_string()),
            Token::Colon,
            Token::Identifier("Num".to_string()),
            Token::Semicolon,
        ];

        let mut parser = Parser::new(tokens);
        let res = parser.parse_var_stmt();

        let expected = Stmt::Var(VarStmt {
            name: "variable".to_string(),
            assignment: Expr::Empty,
            var_type: Type::Num,
        });

        assert_eq!(res, expected);
    }

    #[test]
    fn parse_block_stmt_empty() {
        let tokens = vec![Token::LeftBrace, Token::RightBrace];

        let mut parser = Parser::new(tokens);
        let res = parser.parse_block_stmt();

        let expected = Stmt::Block(BlockStmt { stmts: vec![] });

        assert_eq!(res, expected);
    }

    #[test]
    fn parse_block_stmt_with_var() {
        let tokens = vec![
            Token::LeftBrace,
            Token::Var,
            Token::Identifier("variable".to_string()),
            Token::Colon,
            Token::Identifier("Num".to_string()),
            Token::Semicolon,
            Token::RightBrace,
        ];

        let mut parser = Parser::new(tokens);
        let res = parser.parse_block_stmt();

        let expected = Stmt::Block(BlockStmt {
            stmts: vec![Stmt::Var(VarStmt {
                name: "variable".to_string(),
                assignment: Expr::Empty,
                var_type: Type::Num,
            })],
        });

        assert_eq!(res, expected);
    }

    #[test]
    fn parse_binary_expr_binary() {
        let tokens = vec![Token::Plus, Token::Number(2.0), Token::Semicolon];

        let mut parser = Parser::new(tokens);
        let res = parser.parse_binary_expr(Expr::Literal(Literal::Num(1.0)));

        let expected = Expr::Binary(
            Box::new(Expr::Literal(Literal::Num(1.0))),
            Token::Plus,
            Box::new(Expr::Literal(Literal::Num(2.0))),
        );

        assert_eq!(res, expected);
    }

    #[test]
    fn parse_binary_expr_sum_mult_sum() {
        let tokens = vec![
            Token::Number(1.0),
            Token::Plus,
            Token::Number(2.0),
            Token::Star,
            Token::Number(3.0),
            Token::Plus,
            Token::Number(4.0),
            Token::Semicolon,
        ];

        let mut parser = Parser::new(tokens);
        //TODO check why parse_binary_expr misses 1 layer
        //let res = parser.parse_binary_expr(Expr::Literal(Literal::Num(1.0)));
        let res = parser.parse_expr(Binding::Def);

        let expected = Expr::Binary(
            Box::new(Expr::Binary(
                Box::new(Expr::Literal(Literal::Num(1.0))),
                Token::Plus,
                Box::new(Expr::Binary(
                    Box::new(Expr::Literal(Literal::Num(2.0))),
                    Token::Star,
                    Box::new(Expr::Literal(Literal::Num(3.0))),
                )),
            )),
            Token::Plus,
            Box::new(Expr::Literal(Literal::Num(4.0))),
        );

        assert_eq!(res, expected);
    }

    #[test]
    fn parse_binary_expr_no_semicolon() {
        let tokens = vec![Token::Plus, Token::Number(2.0), Token::Eof];

        let mut parser = Parser::new(tokens);
        let res = parser.parse_binary_expr(Expr::Literal(Literal::Num(1.0)));

        let expected = Expr::Binary(
            Box::new(Expr::Literal(Literal::Num(1.0))),
            Token::Plus,
            Box::new(Expr::Literal(Literal::Num(2.0))),
        );

        assert_eq!(res, expected);
    }

    #[test]
    fn parse_prefix_expr_not() {
        let tokens = vec![Token::Not, Token::True, Token::Eof];

        let mut parser = Parser::new(tokens);
        let res = parser.parse_prefix_expr();

        let expected = Expr::Unary(Token::Not, Box::new(Expr::Literal(Literal::Bool(true))));

        assert_eq!(res, expected);
    }

    #[test]
    fn parse_prefix_expr_minus() {
        let tokens = vec![Token::Minus, Token::Number(1.0), Token::Eof];

        let mut parser = Parser::new(tokens);
        let res = parser.parse_prefix_expr();

        let expected = Expr::Unary(Token::Minus, Box::new(Expr::Literal(Literal::Num(1.0))));

        assert_eq!(res, expected);
    }

    #[test]
    fn parse_assign_stmt_literal() {
        let tokens = vec![Token::Equal, Token::Number(1.0), Token::Eof];

        let mut parser = Parser::new(tokens);
        let res = parser.parse_assignment_exrp(Expr::Literal(Literal::Identifier("a".to_string())));

        let expected = Expr::Assignment(
            Box::new(Expr::Literal(Literal::Identifier("a".to_string()))),
            Box::new(Expr::Literal(Literal::Num(1.0))),
        );

        assert_eq!(res, expected);
    }

    #[test]
    fn parse_assign_stmt_expression() {
        let tokens = vec![
            Token::Equal,
            Token::Identifier("a".to_string()),
            Token::Plus,
            Token::Number(2.0),
            Token::Eof,
        ];

        let mut parser = Parser::new(tokens);
        let res = parser.parse_assignment_exrp(Expr::Literal(Literal::Identifier("a".to_string())));

        let expected = Expr::Assignment(
            Box::new(Expr::Literal(Literal::Identifier("a".to_string()))),
            Box::new(Expr::Binary(
                Box::new(Expr::Literal(Literal::Identifier("a".to_string()))),
                Token::Plus,
                Box::new(Expr::Literal(Literal::Num(2.0))),
            )),
        );

        assert_eq!(res, expected);
    }

    #[test]
    fn parse_if_stmt_if_true() {
        let tokens = vec![
            Token::If,
            Token::True,
            Token::LeftBrace,
            Token::Number(1.0),
            Token::Plus,
            Token::Number(2.0),
            Token::Semicolon,
            Token::RightBrace,
            Token::Eof,
        ];

        let mut parser = Parser::new(tokens);
        let res = parser.parse_if_stmt();

        let expected = Stmt::If(IfStmt {
            condition: Expr::Literal(Literal::Bool(true)),
            main: Box::new(Stmt::Block(BlockStmt {
                stmts: vec![Stmt::Expr(ExprStmt {
                    expr: Expr::Binary(
                        Box::new(Expr::Literal(Literal::Num(1.0))),
                        Token::Plus,
                        Box::new(Expr::Literal(Literal::Num(2.0))),
                    ),
                })],
            })),
            alter: Box::new(Stmt::Empty),
        });

        assert_eq!(res, expected);
    }

    #[test]
    fn parse_if_stmt_if_true_else() {
        let tokens = vec![
            Token::If,
            Token::True,
            Token::LeftBrace,
            Token::Number(1.0),
            Token::Plus,
            Token::Number(2.0),
            Token::Semicolon,
            Token::RightBrace,
            Token::Else,
            Token::LeftBrace,
            Token::Number(3.0),
            Token::Plus,
            Token::Number(4.0),
            Token::Semicolon,
            Token::RightBrace,
            Token::Eof,
        ];

        let mut parser = Parser::new(tokens);
        let res = parser.parse_if_stmt();

        let expected = Stmt::If(IfStmt {
            condition: Expr::Literal(Literal::Bool(true)),
            main: Box::new(Stmt::Block(BlockStmt {
                stmts: vec![Stmt::Expr(ExprStmt {
                    expr: Expr::Binary(
                        Box::new(Expr::Literal(Literal::Num(1.0))),
                        Token::Plus,
                        Box::new(Expr::Literal(Literal::Num(2.0))),
                    ),
                })],
            })),
            alter: Box::new(Stmt::Block(BlockStmt {
                stmts: vec![Stmt::Expr(ExprStmt {
                    expr: Expr::Binary(
                        Box::new(Expr::Literal(Literal::Num(3.0))),
                        Token::Plus,
                        Box::new(Expr::Literal(Literal::Num(4.0))),
                    ),
                })],
            })),
        });

        assert_eq!(res, expected);
    }

    #[test]
    fn parse_if_stmt_if_true_else_if() {
        let tokens = vec![
            Token::If,
            Token::True,
            Token::LeftBrace,
            Token::Number(1.0),
            Token::Plus,
            Token::Number(2.0),
            Token::Semicolon,
            Token::RightBrace,
            Token::Else,
            Token::If,
            Token::True,
            Token::LeftBrace,
            Token::Number(3.0),
            Token::Plus,
            Token::Number(4.0),
            Token::Semicolon,
            Token::RightBrace,
            Token::Eof,
        ];

        let mut parser = Parser::new(tokens);
        let res = parser.parse_if_stmt();

        let expected = Stmt::If(IfStmt {
            condition: Expr::Literal(Literal::Bool(true)),
            main: Box::new(Stmt::Block(BlockStmt {
                stmts: vec![Stmt::Expr(ExprStmt {
                    expr: Expr::Binary(
                        Box::new(Expr::Literal(Literal::Num(1.0))),
                        Token::Plus,
                        Box::new(Expr::Literal(Literal::Num(2.0))),
                    ),
                })],
            })),
            alter: Box::new(Stmt::If(IfStmt {
                condition: Expr::Literal(Literal::Bool(true)),
                main: Box::new(Stmt::Block(BlockStmt {
                    stmts: vec![Stmt::Expr(ExprStmt {
                        expr: Expr::Binary(
                            Box::new(Expr::Literal(Literal::Num(3.0))),
                            Token::Plus,
                            Box::new(Expr::Literal(Literal::Num(4.0))),
                        ),
                    })],
                })),
                alter: Box::new(Stmt::Empty),
            })),
        });

        assert_eq!(res, expected);
    }

    #[test]
    fn parse_for_stmt() {
        let tokens = vec![
            Token::For,
            Token::Identifier("num".to_string()),
            Token::Coma,
            Token::Identifier("index".to_string()),
            Token::In,
            Token::Identifier("nums".to_string()),
            Token::LeftBrace,
            Token::Var,
            Token::Identifier("a".to_string()),
            Token::Equal,
            Token::Identifier("index".to_string()),
            Token::Semicolon,
            Token::RightBrace,
            Token::Eof,
        ];

        let mut parser = Parser::new(tokens);
        let res = parser.parse_for_stmt();

        let expected = Stmt::For(ForStmt {
            item: "num".to_string(),
            use_index: true,
            iterator: Expr::Literal(Literal::Identifier("nums".to_string())),
            body: Box::new(Stmt::Block(BlockStmt {
                stmts: vec![Stmt::Var(VarStmt {
                    name: "a".to_string(),
                    assignment: Expr::Literal(Literal::Identifier("index".to_string())),
                    var_type: Type::None,
                })],
            })),
        });

        assert_eq!(res, expected);
    }

    #[test]
    fn parse_fn_stmt() {
        let tokens = vec![
            Token::Fun,
            Token::Identifier("main".to_string()),
            Token::LeftParen,
            Token::Identifier("num".to_string()),
            Token::Identifier("a".to_string()),
            Token::Coma,
            Token::Identifier("num".to_string()),
            Token::Identifier("b".to_string()),
            Token::RightParen,
            Token::Colon,
            Token::Identifier("num".to_string()),
            Token::LeftBrace,
            Token::Var,
            Token::Identifier("c".to_string()),
            Token::Equal,
            Token::Identifier("a".to_string()),
            Token::Plus,
            Token::Identifier("b".to_string()),
            Token::Semicolon,
            Token::RightBrace,
            Token::Eof,
        ];

        let mut parser = Parser::new(tokens);
        let res = parser.parse_fun_stmt();

        let expected = Stmt::Fun(FunStmt {
            name: "main".to_string(),
            return_type: Type::Num,
            params: vec![
                Param {
                    name: "a".to_string(),
                    param_type: Type::Num,
                },
                Param {
                    name: "b".to_string(),
                    param_type: Type::Num,
                },
            ],
            block: Box::new(Stmt::Block(BlockStmt {
                stmts: vec![Stmt::Var(VarStmt {
                    name: "c".to_string(),
                    assignment: Expr::Binary(
                        Box::new(Expr::Literal(Literal::Identifier("a".to_string()))),
                        Token::Plus,
                        Box::new(Expr::Literal(Literal::Identifier("b".to_string()))),
                    ),
                    var_type: Type::None,
                })],
            })),
        });
        
        assert_eq!(res, expected);
    }


    #[test]
    fn parse_class_stmt() {
        let tokens = vec![
            Token::Class,
            Token::Identifier("Animal".to_string()),
            Token::LeftBrace,
            Token::Var,
            Token::Identifier("name".to_string()),
            Token::Colon,
            Token::Identifier("String".to_string()),
            Token::Semicolon,
            Token::Var,
            Token::Identifier("age".to_string()),
            Token::Colon,
            Token::Identifier("Num".to_string()),
            Token::Semicolon,
            Token::Fun,
            Token::Identifier("live".to_string()),
            Token::LeftParen,
            Token::RightParen,
            Token::Colon,
            Token::Identifier("String".to_string()),
            Token::LeftBrace,
            Token::Identifier("name".to_string()),
            Token::Equal,
            Token::String("Living".to_string()),
            Token::Semicolon,
            Token::RightBrace,
            Token::RightBrace,
            Token::Eof,
        ];

        let mut parser = Parser::new(tokens);
        let res = parser.parse_class_stmt();

        let expected = Stmt::Class(ClassStmt {
            name: "Animal".to_string(),
            block: Box::new(Stmt::Block(BlockStmt {
                stmts: vec![
                    Stmt::Var(VarStmt {
                        name: "name".to_string(),
                        assignment: Expr::Empty,
                        var_type: Type::String,
                    }),
                    Stmt::Var(VarStmt {
                        name: "age".to_string(),
                        assignment: Expr::Empty,
                        var_type: Type::Num,
                    }),
                    Stmt::Fun(FunStmt {
                        name: "live".to_string(),
                        return_type: Type::String,
                        params: vec![],
                        block: Box::new(Stmt::Block(BlockStmt {
                            stmts: vec![Stmt::Expr(ExprStmt {
                                expr: Expr::Assignment(
                                    Box::new(Expr::Literal(Literal::Identifier("name".to_string()))),
                                    Box::new(Expr::Literal(Literal::String("Living".to_string()))),
                                ),
                            })],
                        })),
                    }),
                ],
            })),
        });
        
        assert_eq!(res, expected);
    }
}
