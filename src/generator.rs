use std::fmt::Display;

use kekar::{
    ast::{BlockStmt, Expr, ExprStmt, IfStmt, Literal, Stmt},
    lexer::Token,
};

pub trait ToAssembly {
    fn to_assembly(&self, ctx: &mut Context) -> Option<Register>;
}

impl ToAssembly for BlockStmt {
    fn to_assembly(&self, ctx: &mut Context) -> Option<Register> {
        for stmt in self.stmts.iter() {
            stmt.to_assembly(ctx);
        }
        None
    }
}

impl ToAssembly for Stmt {
    fn to_assembly(&self, ctx: &mut Context) -> Option<Register> {
        let reg = match self {
            Stmt::Expr(e) => e.to_assembly(ctx),
            Stmt::If(i) => i.to_assembly(ctx),
            Stmt::Block(b) => b.to_assembly(ctx),
            _ => None,
        };
        reg
    }
}

impl ToAssembly for ExprStmt {
    fn to_assembly(&self, ctx: &mut Context) -> Option<Register> {
        self.expr.to_assembly(ctx)
    }
}

impl ToAssembly for IfStmt {
    fn to_assembly(&self, ctx: &mut Context) -> Option<Register> {
        let cond_reg = self.condition.to_assembly(ctx)?;
        let else_label = ctx.generate_label();
        let end_label = ctx.generate_label();

        ctx.add_code(format!("cmp {}, 0", cond_reg));
        ctx.add_code(format!("je {}", else_label));

        self.then_block.to_assembly(ctx);
        ctx.add_code(format!("jmp {}", end_label));

        ctx.add_code(format!("{}:", else_label));
        
        self.else_block.to_assembly(ctx);

        ctx.add_code(format!("{}:", end_label));

        ctx.free_register(cond_reg);
        None
    }
}

impl ToAssembly for Expr {
    fn to_assembly(&self, ctx: &mut Context) -> Option<Register> {
        let res = match self {
            Expr::Binary(l, t, r) => {
                let left = l.to_assembly(ctx).unwrap();
                let right = r.to_assembly(ctx).unwrap();
                let operator = match t {
                    Token::Plus => "add",
                    Token::Minus => "sub",
                    Token::Star => "mul",
                    Token::Slash => "div",
                    _ => todo!(),
                };
                ctx.add_code(format!("{} {}, {}", operator, left, right));
                ctx.free_register(right);
                Some(left)
            }
            Expr::Literal(l) => match l {
                Literal::Num(n) => {
                    let reg = ctx.allocate_register().expect("No registers available");
                    ctx.add_code(format!("mov {}, {}", reg, n));
                    Some(reg)
                },
                _ => None
            },
            _ => None
        };
        res
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
