use crate::lexer::Token;

#[derive(Debug, PartialEq, Clone)]
pub enum Expr {
    Unary(Token, Box<Expr>),
    Binary(Box<Expr>, Token, Box<Expr>),
    Literal(Literal),
    Assignment(Box<Expr>, Box<Expr>),
    Empty
}

#[derive(Debug, PartialEq, Clone)]
pub enum Stmt {
    Block(BlockStmt),
    Expr(ExprStmt),
    Var(VarStmt),
    If(IfStmt),
    For(ForStmt),
    Fun(FunStmt),
    Class(ClassStmt),
    Return(ReturnStmt),
    Empty
}

#[derive(Debug, PartialEq, Clone)]
pub struct BlockStmt {
    pub stmts: Vec<Stmt>
}

#[derive(Debug, PartialEq, Clone)]
pub struct ExprStmt {
    pub expr: Expr
}

#[derive(Debug, PartialEq, Clone)]
pub struct VarStmt {
    pub name: String,
    pub assignment: Expr,
    pub var_type: Type
}

#[derive(Debug, PartialEq, Clone)]
pub struct IfStmt {
    pub condition: Expr,
    pub main: Box<Stmt>,
    pub alter: Box<Stmt>
}

#[derive(Debug, PartialEq, Clone)]
pub struct ForStmt {
    pub item: String,
    pub use_index: bool,
    pub iterator: Expr,
    pub body: Box<Stmt>
}

#[derive(Debug, PartialEq, Clone)]
pub struct FunStmt {
    pub name: String,
    pub return_type: Type,
    pub params: Vec<Param>,
    pub block: Box<Stmt>,
    //pub ret: ReturnStmt
}

#[derive(Debug, PartialEq, Clone)]
pub struct ReturnStmt {
    pub return_expr: Expr
}

#[derive(Debug, PartialEq, Clone)]
pub struct ClassStmt {
    pub name: String,
    pub block: Box<Stmt>
}

#[derive(Debug, PartialEq, Clone)]
pub enum Literal {
    String(String),
    Num(f64),
    Bool(bool),
    Identifier(String)
}

#[derive(Debug, PartialEq, Clone)]
pub struct Param {
    pub name: String,
    pub param_type: Type
}

#[derive(Debug, PartialEq, Clone)]
pub enum Type {
    Num,
    String,
    Bool,
    Identifier(String),
    None
}
