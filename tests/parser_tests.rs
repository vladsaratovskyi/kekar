#[cfg(test)]mod tests {
    use std::vec;

    use kakar::{ast::{BlockStmt, Expr, ExprStmt, Literal, Stmt, Type, VarStmt}, lexer::Token, parser::Parser};
    

    #[test]
    fn parse_addition() {
        let tokens = vec![Token::Number(1.0), Token::Plus, Token::Number(2.0), Token::Semicolon];

        let mut parser = Parser::new(tokens);

        let result = parser.parse();
        let mut stmts = Vec::new();
        stmts.push(
            Stmt::Expr(
                ExprStmt{expr: 
                    Expr::Binary(Box::new(
                        Expr::Literal(Literal::Num(1.0))),
                        Token::Plus,
                        Box::new(Expr::Literal(Literal::Num(2.0)))
                    )
                }
            )
        );
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
            Token::Semicolon];

        let mut parser = Parser::new(tokens);

        let result = parser.parse();
        
        //dbg!(result);

        let stmts = vec![ Stmt::Var(
            VarStmt {
                var_type: Type::None,
                name : "a".to_string(),
                assignment: Expr::Binary(Box::new(
                    Expr::Literal(Literal::Num(1.0))),
                    Token::Plus,
                    Box::new(Expr::Literal(Literal::Num(2.0)))
                )
            }
        )];
        let expected = BlockStmt { stmts };

       assert_eq!(result, expected);
    }
}