use crate::lexer::Token;

#[derive(Debug, PartialEq)]
pub enum Expr {
    Unary(Token, Box<Expr>),
    Binary(Box<Expr>, Token, Box<Expr>),
    Literal(Literal),
    Assignment(Box<Expr>, Box<Expr>),
    MethodCall,
    For,
    If,
    Semicolon
}

#[derive(Debug, PartialEq)]
pub enum Stmt {
    Block(BlockStmt),
    Expr(ExprStmt),
    Var(VarStmt),
    Empty(EmptyStmt),
}

impl Expr {
    fn expr(&self) {

    }
}

#[derive(Debug, PartialEq)]
pub struct BlockStmt {
    pub stmts: Vec<Stmt>
}

impl BlockStmt {
    fn stmt(&self) {

    }
}

#[derive(Debug, PartialEq)]
pub struct ExprStmt {
    pub expr: Expr
}

#[derive(Debug, PartialEq)]
pub struct VarStmt {
    pub name: String,
    pub assignment: Expr,
    //type: Type
}

#[derive(Debug, PartialEq)]
pub struct EmptyStmt {}


#[derive(Debug, PartialEq)]
pub enum Literal {
    String(String),
    Num(f64),
    Bool(bool),
    Identifier(String)
}

pub enum ParseError {

}