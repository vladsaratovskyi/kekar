use crate::lexer::Token;

#[derive(Debug, PartialEq)]
pub enum Expr {
    Unary(Token, Box<Expr>),
    Binary(Box<Expr>, Token, Box<Expr>),
    Literal(Literal),
    Assignment(Box<Expr>, Box<Expr>),
    Empty
}

#[derive(Debug, PartialEq)]
pub enum Stmt {
    Block(BlockStmt),
    Expr(ExprStmt),
    Var(VarStmt),
    Empty(EmptyStmt),
    If(IfStmt),
    For(ForStmt),
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
pub struct IfStmt {
    pub condition: Expr,
    pub main: Box<Stmt>,
    pub alter: Box<Stmt>
}

#[derive(Debug, PartialEq)]
pub struct ForStmt {
    pub item: String,
    pub use_index: bool,
    pub iterator: Expr,
    pub body: BlockStmt
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