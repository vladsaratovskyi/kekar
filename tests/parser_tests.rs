#[cfg(test)]
mod tests {

    use std::vec;

    use kekar::{ast::{BlockStmt, ClassStmt, Expr, ExprStmt, ForStmt, FunStmt, IfStmt, Literal, Param, Stmt, Type, VarStmt}, lexer::Token, parser::Parser};

    #[test]
    fn parse_addition() {
        let tokens = vec![
            Token::Number(1.0),
            Token::Plus,
            Token::Number(2.0),
            Token::Semicolon,
        ];

        let mut parser = Parser::new(tokens);

        let result = parser.parse();
        let stmts = vec![Stmt::Expr(ExprStmt {
            expr: Expr::Binary(
                Box::new(Expr::Literal(Literal::Num(1.0))),
                Token::Plus,
                Box::new(Expr::Literal(Literal::Num(2.0))),
            ),
        })];

        let expected = BlockStmt { stmts };

        assert_eq!(result, expected);
    }

    #[test]
    fn parse_var_with_addition() {
        let tokens = vec![
            Token::Var,
            Token::Identifier("a".to_string()),
            Token::Equal,
            Token::Number(1.0),
            Token::Plus,
            Token::Number(2.0),
            Token::Semicolon,
        ];

        let mut parser = Parser::new(tokens);

        let result = parser.parse();

        //dbg!(result);

        let stmts = vec![Stmt::Var(VarStmt {
            var_type: Type::None,
            name: "a".to_string(),
            assignment: Expr::Binary(
                Box::new(Expr::Literal(Literal::Num(1.0))),
                Token::Plus,
                Box::new(Expr::Literal(Literal::Num(2.0))),
            ),
        })];
        let expected = BlockStmt { stmts };

        assert_eq!(result, expected);
    }

    #[test]
    fn parse_if_else() {
        let tokens = vec![
            Token::LeftBracket,
            Token::If,
            Token::True,
            Token::LeftBracket,
            Token::Var,
            Token::Identifier("a".to_string()),
            Token::Equal,
            Token::Number(0.0),
            Token::Semicolon,
            Token::RightBracket,
            Token::Else,
            Token::LeftBracket,
            Token::Var,
            Token::Identifier("a".to_string()),
            Token::Equal,
            Token::Number(1.0),
            Token::Semicolon,
            Token::RightBracket,
            Token::RightBracket,
        ];

        let mut parser = Parser::new(tokens);

        let result = parser.parse();

        let stmts = vec![Stmt::Block(BlockStmt {
            stmts: vec![Stmt::If(IfStmt {
                condition: Expr::Literal(Literal::Bool(true)),
                then_block: Box::new(Stmt::Block(BlockStmt {
                    stmts: vec![Stmt::Var(VarStmt {
                        name: "a".to_string(),
                        assignment: Expr::Literal(Literal::Num(0.0)),
                        var_type: Type::None,
                    })],
                })),
                else_block: Box::new(Stmt::Block(BlockStmt {
                    stmts: vec![Stmt::Var(VarStmt {
                        name: "a".to_string(),
                        assignment: Expr::Literal(Literal::Num(1.0)),
                        var_type: Type::None,
                    })],
                })),
            })],
        })];

        let expected = BlockStmt { stmts };
        assert_eq!(result, expected);
    }

    #[test]
    fn parse_if_else_if() {
        let tokens = vec![
            Token::LeftBracket,
            Token::If,
            Token::True,
            Token::LeftBracket,
            Token::Var,
            Token::Identifier("a".to_string()),
            Token::Equal,
            Token::Number(0.0),
            Token::Semicolon,
            Token::RightBracket,
            Token::Else,
            Token::If,
            Token::Not,
            Token::False,
            Token::LeftBracket,
            Token::Var,
            Token::Identifier("a".to_string()),
            Token::Equal,
            Token::Number(1.0),
            Token::Semicolon,
            Token::RightBracket,
            Token::RightBracket,
        ];

        let mut parser = Parser::new(tokens);

        let result = parser.parse();

        let stmts = vec![Stmt::Block(BlockStmt {
            stmts: vec![Stmt::If(IfStmt {
                condition: Expr::Literal(Literal::Bool(true)),
                then_block: Box::new(Stmt::Block(BlockStmt {
                    stmts: vec![Stmt::Var(VarStmt {
                        name: "a".to_string(),
                        assignment: Expr::Literal(Literal::Num(0.0)),
                        var_type: Type::None,
                    })],
                })),
                else_block: Box::new(Stmt::If(IfStmt {
                    condition: Expr::Unary(
                        Token::Not,
                        Box::new(Expr::Literal(Literal::Bool(false))),
                    ),
                    then_block: Box::new(Stmt::Block(BlockStmt {
                        stmts: vec![Stmt::Var(VarStmt {
                            name: "a".to_string(),
                            assignment: Expr::Literal(Literal::Num(1.0)),
                            var_type: Type::None,
                        })],
                    })),
                    else_block: Box::new(Stmt::Empty),
                })),
            })],
        })];

        let expected = BlockStmt { stmts };
        assert_eq!(result, expected);
    }

    #[test]
    fn parse_for() {
        let tokens = vec![
            Token::LeftBracket,
            Token::For,
            Token::Identifier("num".to_string()),
            Token::Coma,
            Token::Identifier("index".to_string()),
            Token::In,
            Token::Identifier("nums".to_string()),
            Token::LeftBracket,
            Token::Var,
            Token::Identifier("a".to_string()),
            Token::Equal,
            Token::Identifier("index".to_string()),
            Token::Semicolon,
            Token::RightBracket,
            Token::RightBracket,
        ];

        let mut parser = Parser::new(tokens);

        let result = parser.parse();

        let stmts = vec![Stmt::Block(BlockStmt {
            stmts: vec![Stmt::For(ForStmt {
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
            })],
        })];

        let expected = BlockStmt { stmts };
        assert_eq!(result, expected);
    }

    #[test]
    fn parse_function() {
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
            Token::LeftBracket,
            Token::Var,
            Token::Identifier("c".to_string()),
            Token::Equal,
            Token::Identifier("a".to_string()),
            Token::Plus,
            Token::Identifier("b".to_string()),
            Token::Semicolon,
            Token::RightBracket,
        ];

        let mut parser = Parser::new(tokens);

        let result = parser.parse();

        let stmts = vec![Stmt::Fun(FunStmt {
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
        })];

        let expected = BlockStmt { stmts };
        assert_eq!(result, expected);
    }

    #[test]
    fn parse_class() {
        let tokens = vec![
            Token::Class,
            Token::Identifier("Animal".to_string()),
            Token::LeftBracket,
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
            Token::LeftBracket,
            Token::Identifier("name".to_string()),
            Token::Equal,
            Token::String("Living".to_string()),
            Token::Semicolon,
            Token::RightBracket,
            Token::RightBracket,
        ];

        let mut parser = Parser::new(tokens);

        let result = parser.parse();

        let stmts = vec![Stmt::Class(ClassStmt {
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
        })];

        let expected = BlockStmt { stmts };
        assert_eq!(result, expected);
    }
}
