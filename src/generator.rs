use std::fmt::Display;

use kekar::{
    ast::{BlockStmt, Expr, ExprStmt, Literal, Stmt},
    lexer::Token,
};

pub trait ToAssembly {
    fn to_assembly(&self, ctx: &mut Context) -> String;
}

impl ToAssembly for BlockStmt {
    fn to_assembly(&self, ctx: &mut Context) -> String {
        let mut code = "".to_string();

        for stmt in self.stmts.iter() {
            code.push_str(stmt.to_assembly(ctx).as_str());
        }
        code
    }
}

impl ToAssembly for Stmt {
    fn to_assembly(&self, ctx: &mut Context) -> String {
        let code = match self {
            Stmt::Expr(e) => e.to_assembly(ctx),
            _ => "".to_string(),
        };
        code
    }
}

impl ToAssembly for ExprStmt {
    fn to_assembly(&self, ctx: &mut Context) -> String {
        self.expr.to_assembly(ctx)
    }
}

impl ToAssembly for Expr {
    fn to_assembly(&self, ctx: &mut Context) -> String {
        let code = match self {
            Expr::Binary(l, t, r) => {
                let left = l.to_assembly(ctx);
                let right = r.to_assembly(ctx);
                let operator = match t {
                    Token::Plus => "add",
                    Token::Minus => "sub",
                    Token::Star => "mul",
                    Token::Slash => "div",
                    _ => todo!(),
                };
                ctx.add_code(format!("{}\n{}\n{} rax, rbx", left, right, operator));
                "".to_string()
            }
            Expr::Literal(l) => match l {
                Literal::Num(n) => {
                    let reg = ctx.allocate_register().expect("No registers available");
                    ctx.add_code(format!("mov {}, {}", reg, n));
                    "".to_string()
                }
                _ => todo!(),
            },
            _ => todo!(),
        };
        code
    }
}

pub enum Register {
    Rax,
    Rbx,
    Rcx,
    Rdx,
    Rsi,
    Rdi,
    Esp,
    Ebp,
}

impl Display for Register {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Register::Rax => write!(f, "rax"),
            Register::Rbx => write!(f, "rbx"),
            Register::Rcx => write!(f, "rcx"),
            Register::Rdx => write!(f, "rdx"),
            Register::Rsi => write!(f, "rsi"),
            Register::Rdi => write!(f, "rdi"),
            Register::Esp => write!(f, "esp"),
            Register::Ebp => write!(f, "ebp"),
        }
    }
}

pub struct Context {
    next_label: usize,
    registers: Vec<Register>,
    code: Vec<String>,
}

pub struct AsmGenerator {}

impl Context {
    pub fn new() -> Self {
        Self {
            next_label: 0,
            registers: vec![
                Register::Rax,
                Register::Rbx,
                Register::Rcx,
                Register::Rdx,
                Register::Rsi,
                Register::Rdi,
                Register::Esp,
                Register::Ebp,
            ],
            code: Vec::new(),
        }
    }

    pub fn generate_label(&mut self) -> String {
        let label = format!("label_{}", self.next_label);
        self.next_label += 1;
        label
    }

    pub fn add_code(&mut self, code: String) {
        self.code.push(code);
    }

    pub fn allocate_register(&mut self) -> Option<Register> {
        self.registers.pop()
    }

    pub fn free_register(&mut self, reg: Register) {
        self.registers.push(reg);
    }

    pub fn finalize(self) -> String {
        self.code.join("\n")
    }
}

impl AsmGenerator {
    pub fn new() -> Self {
        Self {}
    }

    pub fn generate_asm(&self, program: BlockStmt) -> String {
        let mut ctx = Context::new();
        program.to_assembly(&mut ctx);
        ctx.finalize()
    }
}
