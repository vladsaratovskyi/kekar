use crate::lexer::Token;

#[derive(Debug)]
pub enum Expr {
    Unary(Token, Box<Expr>),
    Binary(Box<Expr>, Token, Box<Expr>),
    Literal(Literal),
    MethodCall,
    For,
    If,
    Semicolon
}

#[derive(Debug)]
pub enum Stmt {
    Block(BlockStmt),
    Expr(ExprStmt),
    Empty(EmptyStmt)
}

impl Expr {
    fn expr(&self) {

    }
}

#[derive(Debug)]
pub struct BlockStmt {
    pub stmts: Vec<Stmt>
}

impl BlockStmt {
    fn stmt(&self) {

    }
}

#[derive(Debug)]
pub struct ExprStmt {
    pub expr: Expr
}

#[derive(Debug)]
pub struct EmptyStmt {}


#[derive(Debug)]
pub enum Literal {
    String(String),
    Num(f64),
    Identifier(String)
}

pub enum ParseError {

}