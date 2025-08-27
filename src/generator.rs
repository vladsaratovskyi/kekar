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

        ctx.emit(Instruction::op(
            "cmp",
            vec![cond_reg.to_string(), "0".to_string()],
        ));
        ctx.emit(Instruction::op("je", vec![else_label.clone()]));
        ctx.free_register(Some(cond_reg));

        let then_reg = self.then_block.to_assembly(ctx);
        ctx.free_register(then_reg);
        ctx.emit(Instruction::op("jmp", vec![end_label.clone()]));
        ctx.emit(Instruction::label(else_label.clone()));

        let else_reg = self.else_block.to_assembly(ctx);
        ctx.free_register(else_reg);
        ctx.emit(Instruction::label(end_label));

        None
    }
}

impl ToAssembly for ForStmt {
    fn to_assembly(&self, ctx: &mut Context) -> Option<Register> {
        // Allocate registers for the index and the array base address
        let index_reg = ctx.allocate_register().expect("No registers available");
        let array_reg = self.iterator.to_assembly(ctx)?;

        // Initialize the index register to 0
        ctx.emit(Instruction::op(
            "mov",
            vec![index_reg.to_string(), "0".to_string()],
        ));

        // Let's assume the array length is known (hard-coded or otherwise)
        let array_len = match &self.iterator {
            Expr::Array(arr) => arr.array.len(),
            _ => panic!("Array length unknown"),
        };

        let end_label = ctx.generate_named_label("end");
        let loop_label = ctx.generate_named_label("loop");

        ctx.emit(Instruction::label(loop_label.clone()));

        // Compare index with array length
        ctx.emit(Instruction::op(
            "cmp",
            vec![index_reg.to_string(), array_len.to_string()],
        ));
        ctx.emit(Instruction::op("jge", vec![end_label.clone()]));

        let element_reg = ctx.allocate_register().expect("No registers available");
        // Now the element is in element_reg, and index is in index_reg.
        // You can generate code for the loop body here, using element_reg and index_reg.
        // For simplicity, let's just assume we're adding 1 to the element:
        ctx.emit(Instruction::raw(format!(
            "    mov {}, [{} + {} * 4]",
            element_reg, array_reg, index_reg
        )));

        let modified_element_reg = self.body.to_assembly(ctx);

        // Increment the index
        ctx.emit(Instruction::op("inc", vec![index_reg.to_string()]));

        // Jump back to the beginning of the loop
        ctx.emit(Instruction::op("jmp", vec![loop_label.clone()]));

        // End of the loop
        ctx.emit(Instruction::label(end_label.clone()));

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
                ctx.emit(Instruction::op(
                    operator,
                    vec![left.to_string(), right.to_string()],
                ));
                ctx.free_register(Some(right));
                Some(left)
            }
            Expr::Literal(l) => match l {
                Literal::Num(n) => {
                    let reg = ctx.allocate_register().expect("No registers available");
                    ctx.emit(Instruction::op("mov", vec![reg.to_string(), n.to_string()]));
                    Some(reg)
                }
                Literal::Bool(b) => {
                    let mut n = 1;

                    if !b {
                        n = 0;
                    }

                    let reg = ctx.allocate_register().expect("No registers available");
                    ctx.emit(Instruction::op("mov", vec![reg.to_string(), n.to_string()]));
                    Some(reg)
                }
                _ => None,
            },
            Expr::Array(arr) => {
                let array_label = ctx.generate_named_label("array");

                ctx.emit(Instruction::label(array_label.clone()));

                for element in arr.array.clone() {
                    match element {
                        Expr::Literal(n) => match n {
                            Literal::Num(num) => {
                                ctx.emit(Instruction::raw(format!("    dd {}", num)));
                            }
                            _ => panic!("Not supported literal"),
                        },
                        _ => panic!("Array can only contain numbers"),
                    }
                }

                Some(Register::Label(array_label))
            }
            _ => None,
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
    Label(String),
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
pub enum Instruction {
    Operation {
        opcode: String,
        operands: Vec<String>,
    },
    Label(String),
    Raw(String),
}

impl Instruction {
    pub fn op(opcode: &str, operands: Vec<String>) -> Self {
        Self::Operation {
            opcode: opcode.to_string(),
            operands,
        }
    }

    pub fn label(label: String) -> Self {
        Self::Label(label)
    }

    pub fn raw<S: Into<String>>(s: S) -> Self {
        Self::Raw(s.into())
    }
}

impl Display for Instruction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Instruction::Operation { opcode, operands } => {
                if operands.is_empty() {
                    write!(f, "    {}", opcode)
                } else {
                    write!(f, "    {} {}", opcode, operands.join(", "))
                }
            }
            Instruction::Label(label) => write!(f, "{}:", label),
            Instruction::Raw(s) => write!(f, "{}", s),
        }
    }
}

#[derive(Debug)]
pub struct Context {
    next_label: usize,
    registers: VecDeque<Register>,
    code: Vec<Instruction>,
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

    pub fn emit(&mut self, instr: Instruction) {
        self.code.push(instr);
    }

    pub fn allocate_register(&mut self) -> Option<Register> {
        self.registers.pop_front()
    }

    pub fn free_register(&mut self, reg: Option<Register>) {
        match reg {
            Some(r) => self.registers.push_front(r),
            _ => (),
        }
    }

    pub fn finalize(self) -> String {
        self.code
            .into_iter()
            .map(|i| i.to_string())
            .collect::<Vec<_>>()
            .join("\n")
    }
}

impl AsmGenerator {
    pub fn new() -> Self {
        Self {}
    }

    pub fn generate_asm(&self, program: BlockStmt) -> String {
        let mut ctx = Context::new();
        ctx.emit(Instruction::raw("section .text"));
        ctx.emit(Instruction::raw("global _start"));
        ctx.emit(Instruction::label("_start".to_string()));
        program.to_assembly(&mut ctx);
        ctx.emit(Instruction::op(
            "mov",
            vec!["eax".to_string(), "60".to_string()],
        ));
        ctx.emit(Instruction::op(
            "xor",
            vec!["edi".to_string(), "edi".to_string()],
        ));
        ctx.emit(Instruction::op("syscall", Vec::new()));

        ctx.finalize()
    }
}
