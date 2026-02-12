#[cfg(test)]
mod tests {

    use std::vec;

    use kekar::{
        ast::{
            BlockStmt, BreakStmt, CallExpr, ClassStmt, ConstStmt, ContinueStmt, EnumStmt,
            EnumVariant, Expr, ExprStmt, FieldDecl, ForStmt, FunStmt, IfStmt, ImplStmt, ImportStmt,
            Literal, MatchArm, MatchStmt, MemberExpr, ModStmt, Param, Pattern, PubStmt, ReturnStmt,
            Stmt, StructStmt, Type, UseStmt, VarStmt, WhileStmt,
        },
        lexer::Token,
        parser::Parser,
    };

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
                index: Some("index".to_string()),
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
                                    Box::new(Expr::Literal(Literal::Identifier(
                                        "name".to_string(),
                                    ))),
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

    #[test]
    fn parse_const_stmt() {
        let tokens = vec![
            Token::Const,
            Token::Identifier("MAX".to_string()),
            Token::Colon,
            Token::Identifier("Num".to_string()),
            Token::Equal,
            Token::Number(42.0),
            Token::Semicolon,
        ];

        let mut parser = Parser::new(tokens);
        let result = parser.parse();

        let expected = BlockStmt {
            stmts: vec![Stmt::Const(ConstStmt {
                name: "MAX".to_string(),
                assignment: Expr::Literal(Literal::Num(42.0)),
                const_type: Type::Num,
            })],
        };

        assert_eq!(result, expected);
    }

    #[test]
    fn parse_while_break_continue() {
        let tokens = vec![
            Token::While,
            Token::True,
            Token::LeftBracket,
            Token::Break,
            Token::Semicolon,
            Token::Continue,
            Token::Semicolon,
            Token::RightBracket,
        ];

        let mut parser = Parser::new(tokens);
        let result = parser.parse();

        let expected = BlockStmt {
            stmts: vec![Stmt::While(WhileStmt {
                condition: Expr::Literal(Literal::Bool(true)),
                body: Box::new(Stmt::Block(BlockStmt {
                    stmts: vec![Stmt::Break(BreakStmt), Stmt::Continue(ContinueStmt)],
                })),
            })],
        };

        assert_eq!(result, expected);
    }

    #[test]
    fn parse_function_canonical_signature_and_arrow_return() {
        let tokens = vec![
            Token::Fun,
            Token::Identifier("main".to_string()),
            Token::LeftParen,
            Token::Identifier("input".to_string()),
            Token::Colon,
            Token::Identifier("String".to_string()),
            Token::Coma,
            Token::Identifier("count".to_string()),
            Token::Colon,
            Token::Identifier("Num".to_string()),
            Token::RightParen,
            Token::Arrow,
            Token::Identifier("Void".to_string()),
            Token::LeftBracket,
            Token::Return,
            Token::Semicolon,
            Token::RightBracket,
        ];

        let mut parser = Parser::new(tokens);
        let result = parser.parse();

        let expected = BlockStmt {
            stmts: vec![Stmt::Fun(FunStmt {
                name: "main".to_string(),
                return_type: Type::Void,
                params: vec![
                    Param {
                        name: "input".to_string(),
                        param_type: Type::String,
                    },
                    Param {
                        name: "count".to_string(),
                        param_type: Type::Num,
                    },
                ],
                block: Box::new(Stmt::Block(BlockStmt {
                    stmts: vec![Stmt::Return(kekar::ast::ReturnStmt {
                        return_expr: Expr::Empty,
                    })],
                })),
            })],
        };

        assert_eq!(result, expected);
    }

    #[test]
    fn parse_import_with_alias() {
        let tokens = vec![
            Token::Import,
            Token::Identifier("System".to_string()),
            Token::As,
            Token::Identifier("Sys".to_string()),
            Token::From,
            Token::String("../src/system.kek".to_string()),
            Token::Semicolon,
        ];

        let mut parser = Parser::new(tokens);
        let result = parser.parse();

        let expected = BlockStmt {
            stmts: vec![Stmt::Import(ImportStmt {
                import: "System".to_string(),
                from: "../src/system.kek".to_string(),
                alias: Some("Sys".to_string()),
            })],
        };

        assert_eq!(result, expected);
    }

    #[test]
    fn parse_mod_and_use_statements() {
        let tokens = vec![
            Token::Mod,
            Token::Identifier("core".to_string()),
            Token::Semicolon,
            Token::Use,
            Token::Identifier("std".to_string()),
            Token::ColonColon,
            Token::Identifier("io".to_string()),
            Token::ColonColon,
            Token::Identifier("print".to_string()),
            Token::Semicolon,
        ];

        let mut parser = Parser::new(tokens);
        let result = parser.parse();

        let expected = BlockStmt {
            stmts: vec![
                Stmt::Mod(ModStmt {
                    name: "core".to_string(),
                }),
                Stmt::Use(UseStmt {
                    path: "std::io::print".to_string(),
                }),
            ],
        };

        assert_eq!(result, expected);
    }

    #[test]
    fn parse_pub_struct_enum_and_function() {
        let tokens = vec![
            Token::Pub,
            Token::Struct,
            Token::Identifier("Point".to_string()),
            Token::LeftBracket,
            Token::Identifier("x".to_string()),
            Token::Colon,
            Token::Identifier("Num".to_string()),
            Token::Semicolon,
            Token::Identifier("y".to_string()),
            Token::Colon,
            Token::Identifier("Num".to_string()),
            Token::Semicolon,
            Token::RightBracket,
            Token::Pub,
            Token::Enum,
            Token::Identifier("MaybeNum".to_string()),
            Token::LeftBracket,
            Token::Identifier("Some".to_string()),
            Token::LeftParen,
            Token::Identifier("Num".to_string()),
            Token::RightParen,
            Token::Coma,
            Token::Identifier("Empty".to_string()),
            Token::RightBracket,
            Token::Pub,
            Token::Fun,
            Token::Identifier("main".to_string()),
            Token::LeftParen,
            Token::RightParen,
            Token::Arrow,
            Token::Identifier("Num".to_string()),
            Token::LeftBracket,
            Token::Return,
            Token::Number(1.0),
            Token::Semicolon,
            Token::RightBracket,
        ];

        let mut parser = Parser::new(tokens);
        let result = parser.parse();

        let expected = BlockStmt {
            stmts: vec![
                Stmt::Pub(PubStmt {
                    stmt: Box::new(Stmt::Struct(StructStmt {
                        name: "Point".to_string(),
                        fields: vec![
                            FieldDecl {
                                name: "x".to_string(),
                                field_type: Type::Num,
                            },
                            FieldDecl {
                                name: "y".to_string(),
                                field_type: Type::Num,
                            },
                        ],
                        methods: vec![],
                    })),
                }),
                Stmt::Pub(PubStmt {
                    stmt: Box::new(Stmt::Enum(EnumStmt {
                        name: "MaybeNum".to_string(),
                        variants: vec![
                            EnumVariant {
                                name: "Some".to_string(),
                                arguments: vec![Type::Num],
                            },
                            EnumVariant {
                                name: "Empty".to_string(),
                                arguments: vec![],
                            },
                        ],
                    })),
                }),
                Stmt::Pub(PubStmt {
                    stmt: Box::new(Stmt::Fun(FunStmt {
                        name: "main".to_string(),
                        return_type: Type::Num,
                        params: vec![],
                        block: Box::new(Stmt::Block(BlockStmt {
                            stmts: vec![Stmt::Return(ReturnStmt {
                                return_expr: Expr::Literal(Literal::Num(1.0)),
                            })],
                        })),
                    })),
                }),
            ],
        };

        assert_eq!(result, expected);
    }

    #[test]
    fn parse_pub_var_declaration() {
        let tokens = vec![
            Token::Pub,
            Token::Var,
            Token::Identifier("count".to_string()),
            Token::Colon,
            Token::Identifier("Num".to_string()),
            Token::Semicolon,
        ];

        let mut parser = Parser::new(tokens);
        let result = parser.parse();

        let expected = BlockStmt {
            stmts: vec![Stmt::Pub(PubStmt {
                stmt: Box::new(Stmt::Var(VarStmt {
                    name: "count".to_string(),
                    assignment: Expr::Empty,
                    var_type: Type::Num,
                })),
            })],
        };

        assert_eq!(result, expected);
    }

    #[test]
    fn parse_impl_block_with_methods() {
        let tokens = vec![
            Token::Impl,
            Token::Identifier("Point".to_string()),
            Token::LeftBracket,
            Token::Fun,
            Token::Identifier("len".to_string()),
            Token::LeftParen,
            Token::RightParen,
            Token::Arrow,
            Token::Identifier("Num".to_string()),
            Token::LeftBracket,
            Token::Return,
            Token::Number(1.0),
            Token::Semicolon,
            Token::RightBracket,
            Token::Pub,
            Token::Fun,
            Token::Identifier("zero".to_string()),
            Token::LeftParen,
            Token::RightParen,
            Token::Arrow,
            Token::Identifier("Num".to_string()),
            Token::LeftBracket,
            Token::Return,
            Token::Number(0.0),
            Token::Semicolon,
            Token::RightBracket,
            Token::RightBracket,
        ];

        let mut parser = Parser::new(tokens);
        let result = parser.parse();

        let expected = BlockStmt {
            stmts: vec![Stmt::Impl(ImplStmt {
                name: "Point".to_string(),
                methods: vec![
                    Stmt::Fun(FunStmt {
                        name: "len".to_string(),
                        return_type: Type::Num,
                        params: vec![],
                        block: Box::new(Stmt::Block(BlockStmt {
                            stmts: vec![Stmt::Return(ReturnStmt {
                                return_expr: Expr::Literal(Literal::Num(1.0)),
                            })],
                        })),
                    }),
                    Stmt::Pub(PubStmt {
                        stmt: Box::new(Stmt::Fun(FunStmt {
                            name: "zero".to_string(),
                            return_type: Type::Num,
                            params: vec![],
                            block: Box::new(Stmt::Block(BlockStmt {
                                stmts: vec![Stmt::Return(ReturnStmt {
                                    return_expr: Expr::Literal(Literal::Num(0.0)),
                                })],
                            })),
                        })),
                    }),
                ],
            })],
        };

        assert_eq!(result, expected);
    }

    #[test]
    fn parse_struct_with_inline_methods() {
        let tokens = vec![
            Token::Struct,
            Token::Identifier("Driver".to_string()),
            Token::LeftBracket,
            Token::Identifier("state".to_string()),
            Token::Colon,
            Token::Identifier("Num".to_string()),
            Token::Semicolon,
            Token::Fun,
            Token::Identifier("apply".to_string()),
            Token::LeftParen,
            Token::RightParen,
            Token::Arrow,
            Token::Identifier("Num".to_string()),
            Token::LeftBracket,
            Token::Return,
            Token::Identifier("this".to_string()),
            Token::Dot,
            Token::Identifier("state".to_string()),
            Token::Semicolon,
            Token::RightBracket,
            Token::Pub,
            Token::Fun,
            Token::Identifier("zero".to_string()),
            Token::LeftParen,
            Token::RightParen,
            Token::Arrow,
            Token::Identifier("Num".to_string()),
            Token::LeftBracket,
            Token::Return,
            Token::Number(0.0),
            Token::Semicolon,
            Token::RightBracket,
            Token::RightBracket,
        ];

        let mut parser = Parser::new(tokens);
        let result = parser.parse();

        let expected = BlockStmt {
            stmts: vec![Stmt::Struct(StructStmt {
                name: "Driver".to_string(),
                fields: vec![FieldDecl {
                    name: "state".to_string(),
                    field_type: Type::Num,
                }],
                methods: vec![
                    Stmt::Fun(FunStmt {
                        name: "apply".to_string(),
                        return_type: Type::Num,
                        params: vec![],
                        block: Box::new(Stmt::Block(BlockStmt {
                            stmts: vec![Stmt::Return(ReturnStmt {
                                return_expr: Expr::Mebmer(MemberExpr {
                                    member: Box::new(Expr::Literal(Literal::Identifier(
                                        "this".to_string(),
                                    ))),
                                    property: "state".to_string(),
                                }),
                            })],
                        })),
                    }),
                    Stmt::Pub(PubStmt {
                        stmt: Box::new(Stmt::Fun(FunStmt {
                            name: "zero".to_string(),
                            return_type: Type::Num,
                            params: vec![],
                            block: Box::new(Stmt::Block(BlockStmt {
                                stmts: vec![Stmt::Return(ReturnStmt {
                                    return_expr: Expr::Literal(Literal::Num(0.0)),
                                })],
                            })),
                        })),
                    }),
                ],
            })],
        };

        assert_eq!(result, expected);
    }

    #[test]
    fn parse_match_with_literal_wildcard_and_variant_patterns() {
        let tokens = vec![
            Token::Match,
            Token::Identifier("value".to_string()),
            Token::LeftBracket,
            Token::Number(1.0),
            Token::FatArrow,
            Token::Identifier("one".to_string()),
            Token::Semicolon,
            Token::Identifier("_".to_string()),
            Token::FatArrow,
            Token::Identifier("zero".to_string()),
            Token::Semicolon,
            Token::Identifier("Some".to_string()),
            Token::LeftParen,
            Token::Identifier("x".to_string()),
            Token::RightParen,
            Token::FatArrow,
            Token::Identifier("x".to_string()),
            Token::Semicolon,
            Token::RightBracket,
        ];

        let mut parser = Parser::new(tokens);
        let result = parser.parse();

        let expected = BlockStmt {
            stmts: vec![Stmt::Match(MatchStmt {
                expr: Expr::Literal(Literal::Identifier("value".to_string())),
                arms: vec![
                    MatchArm {
                        pattern: Pattern::Literal(Literal::Num(1.0)),
                        body: Box::new(Stmt::Expr(ExprStmt {
                            expr: Expr::Literal(Literal::Identifier("one".to_string())),
                        })),
                    },
                    MatchArm {
                        pattern: Pattern::Wildcard,
                        body: Box::new(Stmt::Expr(ExprStmt {
                            expr: Expr::Literal(Literal::Identifier("zero".to_string())),
                        })),
                    },
                    MatchArm {
                        pattern: Pattern::Variant(
                            "Some".to_string(),
                            vec![Pattern::Identifier("x".to_string())],
                        ),
                        body: Box::new(Stmt::Expr(ExprStmt {
                            expr: Expr::Literal(Literal::Identifier("x".to_string())),
                        })),
                    },
                ],
            })],
        };

        assert_eq!(result, expected);
    }

    #[test]
    fn parse_checked_returns_structured_error() {
        let tokens = vec![
            Token::Fun,
            Token::Identifier("main".to_string()),
            Token::LeftParen,
            Token::RightParen,
            Token::Arrow,
            Token::Identifier("Num".to_string()),
            Token::LeftBracket,
            Token::Return,
            Token::Number(1.0),
            Token::Eof,
        ];

        let mut parser = Parser::new(tokens);
        let errors = parser
            .parse_checked()
            .expect_err("expected parser diagnostics");

        assert!(!errors.is_empty(), "expected at least one parser error");
        assert!(errors[0].message.contains("Expected"));
    }

    #[test]
    fn parse_checked_reports_invalid_pub_target_without_panicking() {
        let tokens = vec![Token::Pub, Token::Semicolon, Token::Eof];

        let mut parser = Parser::new(tokens);
        let errors = parser
            .parse_checked()
            .expect_err("expected parser diagnostics");

        assert!(
            errors
                .iter()
                .any(|error| error.message.contains("Unsupported token after 'pub'")),
            "expected unsupported pub target error, got {errors:?}"
        );
    }

    #[test]
    fn parse_checked_reports_invalid_match_pattern_without_panicking() {
        let tokens = vec![
            Token::Match,
            Token::Identifier("value".to_string()),
            Token::LeftBracket,
            Token::Else,
            Token::FatArrow,
            Token::Number(1.0),
            Token::Semicolon,
            Token::RightBracket,
            Token::Eof,
        ];

        let mut parser = Parser::new(tokens);
        let errors = parser
            .parse_checked()
            .expect_err("expected parser diagnostics");

        assert!(
            errors
                .iter()
                .any(|error| error.message.contains("Unsupported pattern token")),
            "expected unsupported pattern error, got {errors:?}"
        );
    }

    #[test]
    fn parse_generic_type_annotation_preserves_generic_structure() {
        let tokens = vec![
            Token::Fun,
            Token::Identifier("main".to_string()),
            Token::LeftParen,
            Token::Identifier("value".to_string()),
            Token::Colon,
            Token::Identifier("Result".to_string()),
            Token::Less,
            Token::Identifier("Num".to_string()),
            Token::Coma,
            Token::Identifier("String".to_string()),
            Token::Greater,
            Token::RightParen,
            Token::Arrow,
            Token::Identifier("Num".to_string()),
            Token::LeftBracket,
            Token::Return,
            Token::Number(1.0),
            Token::Semicolon,
            Token::RightBracket,
            Token::Eof,
        ];

        let mut parser = Parser::new(tokens);
        let result = parser.parse();

        let expected = BlockStmt {
            stmts: vec![Stmt::Fun(FunStmt {
                name: "main".to_string(),
                return_type: Type::Num,
                params: vec![Param {
                    name: "value".to_string(),
                    param_type: Type::Generic {
                        base: "Result".to_string(),
                        args: vec![Type::Num, Type::String],
                    },
                }],
                block: Box::new(Stmt::Block(BlockStmt {
                    stmts: vec![Stmt::Return(ReturnStmt {
                        return_expr: Expr::Literal(Literal::Num(1.0)),
                    })],
                })),
            })],
        };

        assert_eq!(result, expected);
    }

    #[test]
    fn parse_nested_generic_type_closing_with_shift_right_token() {
        let tokens = vec![
            Token::Fun,
            Token::Identifier("main".to_string()),
            Token::LeftParen,
            Token::Identifier("value".to_string()),
            Token::Colon,
            Token::Identifier("Outer".to_string()),
            Token::Less,
            Token::Identifier("Inner".to_string()),
            Token::Less,
            Token::Identifier("Num".to_string()),
            Token::ShiftRight,
            Token::RightParen,
            Token::Arrow,
            Token::Identifier("Num".to_string()),
            Token::LeftBracket,
            Token::Return,
            Token::Number(1.0),
            Token::Semicolon,
            Token::RightBracket,
            Token::Eof,
        ];

        let mut parser = Parser::new(tokens);
        let result = parser.parse();
        let Stmt::Fun(fun_stmt) = &result.stmts[0] else {
            panic!("expected function");
        };

        assert_eq!(
            fun_stmt.params[0].param_type,
            Type::Generic {
                base: "Outer".to_string(),
                args: vec![Type::Generic {
                    base: "Inner".to_string(),
                    args: vec![Type::Num],
                }],
            }
        );
    }

    #[test]
    fn parse_postfix_try_operator() {
        let tokens = vec![
            Token::Fun,
            Token::Identifier("main".to_string()),
            Token::LeftParen,
            Token::RightParen,
            Token::Arrow,
            Token::Identifier("Num".to_string()),
            Token::LeftBracket,
            Token::Return,
            Token::Identifier("call".to_string()),
            Token::LeftParen,
            Token::RightParen,
            Token::Question,
            Token::Semicolon,
            Token::RightBracket,
            Token::Eof,
        ];

        let mut parser = Parser::new(tokens);
        let result = parser.parse();

        let expected = BlockStmt {
            stmts: vec![Stmt::Fun(FunStmt {
                name: "main".to_string(),
                return_type: Type::Num,
                params: vec![],
                block: Box::new(Stmt::Block(BlockStmt {
                    stmts: vec![Stmt::Return(ReturnStmt {
                        return_expr: Expr::Unary(
                            Token::Question,
                            Box::new(Expr::Call(CallExpr {
                                callee: Box::new(Expr::Literal(Literal::Identifier(
                                    "call".to_string(),
                                ))),
                                arguments: vec![],
                            })),
                        ),
                    })],
                })),
            })],
        };

        assert_eq!(result, expected);
    }
}
