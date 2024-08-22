#[cfg(test)]mod tests {
    use kakar::{ast::{BlockStmt, Expr, ExprStmt, Literal, Stmt}, lexer::Token, parser::Parser};
    use super::*;

    #[test]
    fn it_works() {
        let mut tokens = Vec::new();
        tokens.push(Token::Number(1.0));
        tokens.push(Token::Plus);
        tokens.push(Token::Number(2.0));
        tokens.push(Token::Semicolon);

        let mut parser = Parser::new(tokens);

        let result = parser.parse();
        //
        let mut stmts = Vec::new();
        stmts.push(Stmt::Expr(ExprStmt{expr: Expr::Binary(Box::new(Expr::Literal(Literal::Num(1.0))), Token::Plus, Box::new(Expr::Literal(Literal::Num(2.0))))}));
        let expected = BlockStmt { stmts : stmts };

        assert_eq!(result, expected);
    }
}