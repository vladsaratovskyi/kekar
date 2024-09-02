use std::{collections::VecDeque, fmt::Display};

use kekar::{
    ast::{BlockStmt, Expr, ExprStmt, ForStmt, IfStmt, Literal, Stmt},
    lexer::Token,
};

pub trait ToAssembly {
    fn to_assembly(&self, ctx: &mut Context) -> Option<Register>;
}

impl ToAssembly for BlockStmt {
    fn to_assembly(&self, ctx: &mut Context) -> Option<Register> {
        for stmt in self.stmts.iter() {
            let reg = stmt.to_assembly(ctx);
            ctx.free_register(reg);
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
            Stmt::For(f) => f.to_assembly(ctx),
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
        let else_label = ctx.generate_named_label("else");
        let end_label = ctx.generate_label();

        ctx.add_code(format!("    cmp {}, 0", cond_reg));
        ctx.add_code(format!("    je {}", else_label));
        ctx.free_register(Some(cond_reg));

        let then_reg = self.then_block.to_assembly(ctx);
        ctx.free_register(then_reg);
        ctx.add_code(format!("    jmp {}", end_label));
        ctx.add_code(format!("{}:", else_label));
        
        let else_reg = self.else_block.to_assembly(ctx);
        ctx.free_register(else_reg);
        ctx.add_code(format!("{}:", end_label));

        dbg!(&ctx.registers);
        None
    }
}

impl ToAssembly for ForStmt {
    fn to_assembly(&self, ctx: &mut Context) -> Option<Register> {
        // Allocate registers for the index and the array base address
        let index_reg = ctx.allocate_register().expect("No registers available");
        let array_reg = self.iterator.to_assembly(ctx)?;

        // Initialize the index register to 0
        ctx.add_code(format!("    mov {}, 0", index_reg));

        // Let's assume the array length is known (hard-coded or otherwise)
        let array_len = match &self.iterator {
            Expr::Array(arr) => arr.array.len(),
            _ => panic!("Array length unknown"),
        };

        let end_label = ctx.generate_named_label("end");
        let loop_label = ctx.generate_named_label("loop");

        ctx.add_code(format!("{}:", loop_label));

        // Compare index with array length
        ctx.add_code(format!("    cmp {}, {}", index_reg, array_len));
        ctx.add_code(format!("    jge {}", end_label));

        let element_reg = ctx.allocate_register().expect("No registers available");
        // Now the element is in element_reg, and index is in index_reg.
        // You can generate code for the loop body here, using element_reg and index_reg.
        // For simplicity, let's just assume we're adding 1 to the element:
        ctx.add_code(format!("    mov {}, [{} + {} * 4]", element_reg, array_reg, index_reg));

        let modified_element_reg = self.body.to_assembly(ctx);

        // Increment the index
        ctx.add_code(format!("    inc {}", index_reg));

        // Jump back to the beginning of the loop
        ctx.add_code(format!("    jmp {}", loop_label));

        // End of the loop
        ctx.add_code(format!("{}:", end_label));

        ctx.free_register(Some(index_reg));
        ctx.free_register(Some(array_reg));
        ctx.free_register(Some(element_reg));
        ctx.free_register(modified_element_reg);
        None
    }
}

impl ToAssembly for Expr {
    fn to_assembly(&self, ctx: &mut Context) -> Option<Register> {
        let res = match self {
            Expr::Binary(l, t, r) => {
                let left = l.to_assembly(ctx)?;
                let right = r.to_assembly(ctx)?;
                let operator = match t {
                    Token::Plus => "add",
                    Token::Minus => "sub",
                    Token::Star => "mul",
                    Token::Slash => "div",
                    _ => todo!(),
                };
                ctx.add_code(format!("    {} {}, {}", operator, left, right));
                ctx.free_register(Some(right));
                Some(left)
            },
            Expr::Literal(l) => match l {
                Literal::Num(n) => {
                    let reg = ctx.allocate_register().expect("No registers available");
                    ctx.add_code(format!("    mov {}, {}", reg, n));
                    Some(reg)
                },
                Literal::Bool(b) => {
                    let mut n = 1;

                    if !b {
                        n = 0;
                    }

                    let reg = ctx.allocate_register().expect("No registers available");
                    ctx.add_code(format!("    mov {}, {}", reg, n));
                    Some(reg)
                },
                _ => None
            },
            Expr::Array(arr) => {
                let array_label = ctx.generate_named_label("array");

                ctx.add_code(format!("{}:", array_label));

                for element in arr.array.clone() {
                    match element {
                        Expr::Literal(n) => {
                            match n {
                                Literal::Num(num) => {
                                    ctx.add_code(format!("    dd {}", num)); // 'dd' for a 32-bit number
                                },
                                _ => panic!("Not supported literal"),
                            }
                        },
                        _ => panic!("Array can only contain numbers"),
                    }
                }

                Some(Register::Label(array_label))
            },
            _ => None
        };
        res
    }
}

#[derive(Debug)]
pub enum Register {
    Rax,
    Rbx,
    Rcx,
    Rdx,
    R8,
    R9,
    R10,
    R11,
    R12,
    R13,
    R14,
    R15,
    Rsi,
    Rdi,
    Esp,
    Ebp,
    Label(String)
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
            Register::R8 => write!(f, "R8"),
            Register::R9 => write!(f, "R9"),
            Register::R10 => write!(f, "R10"),
            Register::R11 => write!(f, "R11"),
            Register::R12 => write!(f, "R12"),
            Register::R13 => write!(f, "R13"),
            Register::R14 => write!(f, "R14"),
            Register::R15 => write!(f, "R15"),
            Register::Label(l) => write!(f, "{}", l),
        }
    }
}

#[derive(Debug)]
pub struct Context {
    next_label: usize,
    registers: VecDeque<Register>,
    code: Vec<String>,
}

pub struct AsmGenerator {}

impl Context {
    pub fn new() -> Self {
        let mut deque = VecDeque::new();
        deque.push_front(Register::Ebp);
        deque.push_front(Register::Esp);
        deque.push_front(Register::Rdi);
        deque.push_front(Register::Rsi);
        deque.push_front(Register::R15);
        deque.push_front(Register::R14);
        deque.push_front(Register::R13);
        deque.push_front(Register::R12);
        deque.push_front(Register::R11);
        deque.push_front(Register::R10);
        deque.push_front(Register::R9);
        deque.push_front(Register::R8);
        deque.push_front(Register::Rdx);
        deque.push_front(Register::Rcx);
        deque.push_front(Register::Rbx);
        deque.push_front(Register::Rax);
        Self {
            next_label: 0,
            registers: deque,
            code: Vec::new(),
        }
    }

    pub fn generate_label(&mut self) -> String {
        let label = format!("label_{}", self.next_label);
        self.next_label += 1;
        label
    }

    pub fn generate_named_label(&mut self, prefix: &str) -> String {
        let label = format!("{}_{}", prefix, self.next_label);
        self.next_label += 1;
        label
    }

    pub fn add_code(&mut self, code: String) {
        self.code.push(code);
    }

    pub fn allocate_register(&mut self) -> Option<Register> {
        self.registers.pop_front()
    }

    pub fn free_register(&mut self, reg: Option<Register>) {
        match reg {
            Some(r) => self.registers.push_front(r),
            _ => ()
        }
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
        ctx.add_code("section .text".to_string());
        ctx.add_code("    global _start\n".to_string());
        ctx.add_code("_start:".to_string());
        program.to_assembly(&mut ctx);
        ctx.add_code("    mov eax, 60".to_string());
        ctx.add_code("    xor edi, edi".to_string());
        ctx.add_code("    syscall".to_string());

        ctx.finalize()
    }
}
