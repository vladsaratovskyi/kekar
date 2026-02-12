#![allow(unused)]
use crate::lexer::Token;

#[derive(Debug, PartialEq, Clone)]
pub enum Expr {
    Unary(Token, Box<Expr>),
    Binary(Box<Expr>, Token, Box<Expr>),
    Literal(Literal),
    Assignment(Box<Expr>, Box<Expr>),
    Call(CallExpr),
    Mebmer(MemberExpr),
    ComputedExpr(ComputedExpr),
    Array(ArrayExpr),
    Empty,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Stmt {
    Block(BlockStmt),
    Expr(ExprStmt),
    Pub(PubStmt),
    Mod(ModStmt),
    Use(UseStmt),
    Var(VarStmt),
    Const(ConstStmt),
    Struct(StructStmt),
    Enum(EnumStmt),
    Impl(ImplStmt),
    If(IfStmt),
    Match(MatchStmt),
    While(WhileStmt),
    For(ForStmt),
    Break(BreakStmt),
    Continue(ContinueStmt),
    Fun(FunStmt),
    Class(ClassStmt),
    Return(ReturnStmt),
    Import(ImportStmt),
    Empty,
}

#[derive(Debug, PartialEq, Clone)]
pub struct BlockStmt {
    pub stmts: Vec<Stmt>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct ExprStmt {
    pub expr: Expr,
}

#[derive(Debug, PartialEq, Clone)]
pub struct PubStmt {
    pub stmt: Box<Stmt>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct ModStmt {
    pub name: String,
}

#[derive(Debug, PartialEq, Clone)]
pub struct UseStmt {
    pub path: String,
}

#[derive(Debug, PartialEq, Clone)]
pub struct VarStmt {
    pub name: String,
    pub assignment: Expr,
    pub var_type: Type,
}

#[derive(Debug, PartialEq, Clone)]
pub struct ConstStmt {
    pub name: String,
    pub assignment: Expr,
    pub const_type: Type,
}

#[derive(Debug, PartialEq, Clone)]
pub struct StructStmt {
    pub name: String,
    pub fields: Vec<FieldDecl>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct FieldDecl {
    pub name: String,
    pub field_type: Type,
}

#[derive(Debug, PartialEq, Clone)]
pub struct EnumStmt {
    pub name: String,
    pub variants: Vec<EnumVariant>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct EnumVariant {
    pub name: String,
    pub arguments: Vec<Type>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct ImplStmt {
    pub name: String,
    pub methods: Vec<Stmt>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct IfStmt {
    pub condition: Expr,
    pub then_block: Box<Stmt>,
    pub else_block: Box<Stmt>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct MatchStmt {
    pub expr: Expr,
    pub arms: Vec<MatchArm>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct MatchArm {
    pub pattern: Pattern,
    pub body: Box<Stmt>,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Pattern {
    Wildcard,
    Literal(Literal),
    Identifier(String),
    Variant(String, Vec<Pattern>),
}

#[derive(Debug, PartialEq, Clone)]
pub struct WhileStmt {
    pub condition: Expr,
    pub body: Box<Stmt>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct ForStmt {
    pub item: String,
    pub index: Option<String>,
    pub iterator: Expr,
    pub body: Box<Stmt>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct BreakStmt;

#[derive(Debug, PartialEq, Clone)]
pub struct ContinueStmt;

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
    pub return_expr: Expr,
}

#[derive(Debug, PartialEq, Clone)]
pub struct ClassStmt {
    pub name: String,
    pub block: Box<Stmt>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct CallExpr {
    pub callee: Box<Expr>,
    pub arguments: Vec<Expr>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct MemberExpr {
    pub member: Box<Expr>,
    pub property: String,
}

#[derive(Debug, PartialEq, Clone)]
pub struct ComputedExpr {
    pub member: Box<Expr>,
    pub property: Box<Expr>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct ArrayExpr {
    pub array: Vec<Expr>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct ImportStmt {
    pub import: String,
    pub from: String,
    pub alias: Option<String>,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Literal {
    String(String),
    Char(char),
    Num(f64),
    Bool(bool),
    Identifier(String),
    This,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Param {
    pub name: String,
    pub param_type: Type,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Type {
    Num,
    Char,
    Byte,
    String,
    Bool,
    Void,
    Identifier(String),
    Array(Box<Type>),
    None,
}
