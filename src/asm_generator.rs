use std::collections::{BTreeSet, HashMap};

use crate::{
    ast::{
        BlockStmt, ConstStmt, Expr, ForStmt, FunStmt, IfStmt, Literal, Stmt, VarStmt, WhileStmt,
    },
    lexer::Token,
};

pub struct AsmGenerator {
    label_counter: usize,
}

impl AsmGenerator {
    pub fn new() -> Self {
        Self { label_counter: 0 }
    }

    pub fn generate(&mut self, program: &BlockStmt) -> String {
        let mut lines = vec![
            "section .text".to_string(),
            "    global _start".to_string(),
            String::new(),
            "_start:".to_string(),
        ];

        let has_main = program
            .stmts
            .iter()
            .any(|stmt| matches!(stmt, Stmt::Fun(fun) if fun.name == "main"));

        if has_main {
            lines.push("    call main".to_string());
            lines.push("    mov rdi, rax".to_string());
        } else {
            lines.push("    mov rdi, 0".to_string());
        }

        lines.push("    mov rax, 60".to_string());
        lines.push("    syscall".to_string());
        lines.push(String::new());

        for stmt in &program.stmts {
            match stmt {
                Stmt::Fun(fun_stmt) => self.emit_function(fun_stmt, &mut lines),
                Stmt::Import(_) => {
                    lines.push("; import statement ignored by asm backend".to_string())
                }
                Stmt::Class(_) => {
                    lines.push("; class statement ignored by asm backend".to_string())
                }
                Stmt::Const(_) => {
                    lines.push("; top-level const ignored by asm backend".to_string())
                }
                _ => lines.push("; top-level statement ignored by asm backend".to_string()),
            }
        }

        lines.join("\n")
    }

    fn emit_function(&mut self, fun_stmt: &FunStmt, lines: &mut Vec<String>) {
        let mut local_names = BTreeSet::new();
        for param in &fun_stmt.params {
            local_names.insert(param.name.clone());
        }
        collect_locals(fun_stmt.block.as_ref(), &mut local_names);

        let mut var_offsets = HashMap::new();
        let mut offset = 0_i64;
        for name in local_names {
            offset += 8;
            var_offsets.insert(name, offset);
        }

        let epilogue_label = self.new_label(&format!("{}_epilogue", fun_stmt.name));
        let mut ctx = FunctionContext {
            var_offsets,
            epilogue_label: epilogue_label.clone(),
        };

        lines.push(format!("{}:", fun_stmt.name));
        lines.push("    push rbp".to_string());
        lines.push("    mov rbp, rsp".to_string());
        if offset > 0 {
            lines.push(format!("    sub rsp, {}", offset));
        }

        let registers = ["rdi", "rsi", "rdx", "rcx", "r8", "r9"];
        for (index, param) in fun_stmt.params.iter().enumerate() {
            if let Some(slot) = ctx.var_offsets.get(&param.name) {
                if let Some(reg) = registers.get(index) {
                    lines.push(format!("    mov QWORD [rbp-{}], {}", slot, reg));
                } else {
                    lines.push(format!(
                        "    ; parameter '{}' exceeds register argument support",
                        param.name
                    ));
                }
            }
        }

        self.emit_stmt(fun_stmt.block.as_ref(), &mut ctx, lines);

        lines.push("    mov rax, 0".to_string());
        lines.push(format!("{}:", epilogue_label));
        lines.push("    mov rsp, rbp".to_string());
        lines.push("    pop rbp".to_string());
        lines.push("    ret".to_string());
        lines.push(String::new());
    }

    fn emit_stmt(&mut self, stmt: &Stmt, ctx: &mut FunctionContext, lines: &mut Vec<String>) {
        match stmt {
            Stmt::Block(block) => {
                for inner in &block.stmts {
                    self.emit_stmt(inner, ctx, lines);
                }
            }
            Stmt::Var(var_stmt) => self.emit_var(var_stmt, ctx, lines),
            Stmt::Const(const_stmt) => self.emit_const(const_stmt, ctx, lines),
            Stmt::Expr(expr_stmt) => {
                self.emit_expr(&expr_stmt.expr, ctx, lines);
            }
            Stmt::If(if_stmt) => self.emit_if(if_stmt, ctx, lines),
            Stmt::While(while_stmt) => self.emit_while(while_stmt, ctx, lines),
            Stmt::For(for_stmt) => self.emit_for(for_stmt, ctx, lines),
            Stmt::Break(_) => {
                lines.push("    ; break is not implemented in asm backend".to_string())
            }
            Stmt::Continue(_) => {
                lines.push("    ; continue is not implemented in asm backend".to_string())
            }
            Stmt::Return(return_stmt) => {
                if matches!(return_stmt.return_expr, Expr::Empty) {
                    lines.push("    mov rax, 0".to_string());
                } else {
                    self.emit_expr(&return_stmt.return_expr, ctx, lines);
                }
                lines.push(format!("    jmp {}", ctx.epilogue_label));
            }
            Stmt::Import(_) => lines.push("    ; import ignored in function scope".to_string()),
            Stmt::Class(_) => lines.push("    ; class ignored in function scope".to_string()),
            Stmt::Fun(_) => lines.push("    ; nested function ignored".to_string()),
            Stmt::Empty => {}
        }
    }

    fn emit_var(&mut self, var_stmt: &VarStmt, ctx: &mut FunctionContext, lines: &mut Vec<String>) {
        if matches!(var_stmt.assignment, Expr::Empty) {
            return;
        }

        self.emit_expr(&var_stmt.assignment, ctx, lines);

        if let Some(offset) = ctx.var_offsets.get(&var_stmt.name) {
            lines.push(format!("    mov QWORD [rbp-{}], rax", offset));
        } else {
            lines.push(format!(
                "    ; variable '{}' is missing stack slot in asm backend",
                var_stmt.name
            ));
        }
    }

    fn emit_const(
        &mut self,
        const_stmt: &ConstStmt,
        ctx: &mut FunctionContext,
        lines: &mut Vec<String>,
    ) {
        self.emit_expr(&const_stmt.assignment, ctx, lines);

        if let Some(offset) = ctx.var_offsets.get(&const_stmt.name) {
            lines.push(format!("    mov QWORD [rbp-{}], rax", offset));
        } else {
            lines.push(format!(
                "    ; const '{}' is missing stack slot in asm backend",
                const_stmt.name
            ));
        }
    }

    fn emit_if(&mut self, if_stmt: &IfStmt, ctx: &mut FunctionContext, lines: &mut Vec<String>) {
        let else_label = self.new_label("else");
        let end_label = self.new_label("ifend");

        self.emit_expr(&if_stmt.condition, ctx, lines);
        lines.push("    cmp rax, 0".to_string());
        lines.push(format!("    je {}", else_label));

        self.emit_stmt(if_stmt.then_block.as_ref(), ctx, lines);
        lines.push(format!("    jmp {}", end_label));

        lines.push(format!("{}:", else_label));
        self.emit_stmt(if_stmt.else_block.as_ref(), ctx, lines);

        lines.push(format!("{}:", end_label));
    }

    fn emit_while(
        &mut self,
        while_stmt: &WhileStmt,
        ctx: &mut FunctionContext,
        lines: &mut Vec<String>,
    ) {
        let loop_label = self.new_label("while_loop");
        let end_label = self.new_label("while_end");

        lines.push(format!("{}:", loop_label));
        self.emit_expr(&while_stmt.condition, ctx, lines);
        lines.push("    cmp rax, 0".to_string());
        lines.push(format!("    je {}", end_label));

        self.emit_stmt(while_stmt.body.as_ref(), ctx, lines);
        lines.push(format!("    jmp {}", loop_label));
        lines.push(format!("{}:", end_label));
    }

    fn emit_for(&mut self, for_stmt: &ForStmt, ctx: &mut FunctionContext, lines: &mut Vec<String>) {
        let Expr::Array(array_expr) = &for_stmt.iterator else {
            lines.push("    ; unsupported for-loop iterator in asm backend".to_string());
            return;
        };

        for (index, value_expr) in array_expr.array.iter().enumerate() {
            lines.push(format!("    ; unrolled loop iteration {}", index));
            self.emit_expr(value_expr, ctx, lines);

            if let Some(item_offset) = ctx.var_offsets.get(&for_stmt.item) {
                lines.push(format!("    mov QWORD [rbp-{}], rax", item_offset));
            }

            if let Some(index_name) = &for_stmt.index {
                if let Some(index_offset) = ctx.var_offsets.get(index_name) {
                    lines.push(format!("    mov rax, {}", index));
                    lines.push(format!("    mov QWORD [rbp-{}], rax", index_offset));
                }
            }

            self.emit_stmt(for_stmt.body.as_ref(), ctx, lines);
        }
    }

    fn emit_expr(&mut self, expr: &Expr, ctx: &mut FunctionContext, lines: &mut Vec<String>) {
        match expr {
            Expr::Literal(literal) => self.emit_literal(literal, ctx, lines),
            Expr::Unary(op, right) => {
                self.emit_expr(right, ctx, lines);
                match op {
                    Token::Minus => lines.push("    neg rax".to_string()),
                    Token::Not => {
                        lines.push("    cmp rax, 0".to_string());
                        lines.push("    sete al".to_string());
                        lines.push("    movzx rax, al".to_string());
                    }
                    _ => lines.push("    ; unsupported unary operator".to_string()),
                }
            }
            Expr::Binary(left, op, right) => {
                self.emit_expr(left, ctx, lines);
                lines.push("    push rax".to_string());
                self.emit_expr(right, ctx, lines);
                lines.push("    mov rbx, rax".to_string());
                lines.push("    pop rax".to_string());

                match op {
                    Token::Plus => lines.push("    add rax, rbx".to_string()),
                    Token::Minus => lines.push("    sub rax, rbx".to_string()),
                    Token::Star => lines.push("    imul rax, rbx".to_string()),
                    Token::Slash => {
                        lines.push("    cqo".to_string());
                        lines.push("    idiv rbx".to_string());
                    }
                    Token::Percent => {
                        lines.push("    cqo".to_string());
                        lines.push("    idiv rbx".to_string());
                        lines.push("    mov rax, rdx".to_string());
                    }
                    Token::EqualEqual => self.emit_setcc("sete", lines),
                    Token::NotEqual => self.emit_setcc("setne", lines),
                    Token::Greater => self.emit_setcc("setg", lines),
                    Token::GreaterEqual => self.emit_setcc("setge", lines),
                    Token::Less => self.emit_setcc("setl", lines),
                    Token::LessEqual => self.emit_setcc("setle", lines),
                    Token::And => {
                        lines.push("    cmp rax, 0".to_string());
                        lines.push("    setne al".to_string());
                        lines.push("    movzx rax, al".to_string());
                        lines.push("    cmp rbx, 0".to_string());
                        lines.push("    setne bl".to_string());
                        lines.push("    movzx rbx, bl".to_string());
                        lines.push("    and rax, rbx".to_string());
                    }
                    Token::Or => {
                        lines.push("    cmp rax, 0".to_string());
                        lines.push("    setne al".to_string());
                        lines.push("    movzx rax, al".to_string());
                        lines.push("    cmp rbx, 0".to_string());
                        lines.push("    setne bl".to_string());
                        lines.push("    movzx rbx, bl".to_string());
                        lines.push("    or rax, rbx".to_string());
                    }
                    _ => lines.push("    ; unsupported binary operator".to_string()),
                }
            }
            Expr::Assignment(left, right) => {
                self.emit_expr(right, ctx, lines);

                match left.as_ref() {
                    Expr::Literal(Literal::Identifier(name)) => {
                        if let Some(offset) = ctx.var_offsets.get(name) {
                            lines.push(format!("    mov QWORD [rbp-{}], rax", offset));
                        } else {
                            lines.push(format!(
                                "    ; assignment target '{}' not found in local slots",
                                name
                            ));
                        }
                    }
                    _ => lines.push("    ; unsupported assignment target".to_string()),
                }
            }
            Expr::Call(call) => {
                let registers = ["rdi", "rsi", "rdx", "rcx", "r8", "r9"];
                for (index, arg) in call.arguments.iter().enumerate() {
                    self.emit_expr(arg, ctx, lines);
                    if let Some(reg) = registers.get(index) {
                        lines.push(format!("    mov {}, rax", reg));
                    } else {
                        lines.push("    ; call argument exceeds register support".to_string());
                    }
                }
                lines.push(format!("    call {}", call.method_name));
            }
            Expr::Mebmer(_) => {
                lines.push("    ; member access unsupported in asm backend".to_string());
                lines.push("    mov rax, 0".to_string());
            }
            Expr::ComputedExpr(_) => {
                lines.push("    ; computed access unsupported in asm backend".to_string());
                lines.push("    mov rax, 0".to_string());
            }
            Expr::Array(_) => {
                lines.push("    ; array literal unsupported as value in asm backend".to_string());
                lines.push("    mov rax, 0".to_string());
            }
            Expr::Empty => lines.push("    mov rax, 0".to_string()),
        }
    }

    fn emit_literal(
        &mut self,
        literal: &Literal,
        ctx: &mut FunctionContext,
        lines: &mut Vec<String>,
    ) {
        match literal {
            Literal::Num(n) => {
                lines.push(format!("    mov rax, {}", *n as i64));
            }
            Literal::Char(c) => {
                lines.push(format!("    mov rax, {}", *c as u32));
            }
            Literal::Bool(v) => {
                lines.push(format!("    mov rax, {}", if *v { 1 } else { 0 }));
            }
            Literal::Identifier(name) => {
                if let Some(offset) = ctx.var_offsets.get(name) {
                    lines.push(format!("    mov rax, QWORD [rbp-{}]", offset));
                } else {
                    lines.push(format!("    ; unknown identifier '{}'", name));
                    lines.push("    mov rax, 0".to_string());
                }
            }
            Literal::String(_) => {
                lines.push("    ; string literals are not supported in asm backend".to_string());
                lines.push("    mov rax, 0".to_string());
            }
            Literal::This => {
                lines.push("    ; 'this' is not supported in asm backend".to_string());
                lines.push("    mov rax, 0".to_string());
            }
        }
    }

    fn emit_setcc(&self, setcc: &str, lines: &mut Vec<String>) {
        lines.push("    cmp rax, rbx".to_string());
        lines.push(format!("    {} al", setcc));
        lines.push("    movzx rax, al".to_string());
    }

    fn new_label(&mut self, prefix: &str) -> String {
        let label = format!(".{}_{}", prefix, self.label_counter);
        self.label_counter += 1;
        label
    }
}

struct FunctionContext {
    var_offsets: HashMap<String, i64>,
    epilogue_label: String,
}

fn collect_locals(stmt: &Stmt, locals: &mut BTreeSet<String>) {
    match stmt {
        Stmt::Block(block) => {
            for nested in &block.stmts {
                collect_locals(nested, locals);
            }
        }
        Stmt::Var(var_stmt) => {
            locals.insert(var_stmt.name.clone());
            collect_expr_locals(&var_stmt.assignment, locals);
        }
        Stmt::Const(const_stmt) => {
            locals.insert(const_stmt.name.clone());
            collect_expr_locals(&const_stmt.assignment, locals);
        }
        Stmt::If(if_stmt) => {
            collect_expr_locals(&if_stmt.condition, locals);
            collect_locals(if_stmt.then_block.as_ref(), locals);
            collect_locals(if_stmt.else_block.as_ref(), locals);
        }
        Stmt::While(while_stmt) => {
            collect_expr_locals(&while_stmt.condition, locals);
            collect_locals(while_stmt.body.as_ref(), locals);
        }
        Stmt::For(for_stmt) => {
            locals.insert(for_stmt.item.clone());
            if let Some(index) = &for_stmt.index {
                locals.insert(index.clone());
            }
            collect_expr_locals(&for_stmt.iterator, locals);
            collect_locals(for_stmt.body.as_ref(), locals);
        }
        Stmt::Return(ret) => collect_expr_locals(&ret.return_expr, locals),
        Stmt::Expr(expr_stmt) => collect_expr_locals(&expr_stmt.expr, locals),
        _ => {}
    }
}

fn collect_expr_locals(expr: &Expr, locals: &mut BTreeSet<String>) {
    match expr {
        Expr::Unary(_, expr) => collect_expr_locals(expr, locals),
        Expr::Binary(left, _, right) => {
            collect_expr_locals(left, locals);
            collect_expr_locals(right, locals);
        }
        Expr::Assignment(left, right) => {
            if let Expr::Literal(Literal::Identifier(name)) = left.as_ref() {
                locals.insert(name.clone());
            }
            collect_expr_locals(left, locals);
            collect_expr_locals(right, locals);
        }
        Expr::Call(call) => {
            for arg in &call.arguments {
                collect_expr_locals(arg, locals);
            }
        }
        Expr::Mebmer(member) => collect_expr_locals(member.member.as_ref(), locals),
        Expr::ComputedExpr(computed) => {
            collect_expr_locals(computed.member.as_ref(), locals);
            collect_expr_locals(computed.property.as_ref(), locals);
        }
        Expr::Array(array) => {
            for item in &array.array {
                collect_expr_locals(item, locals);
            }
        }
        _ => {}
    }
}

#[cfg(test)]
mod tests {
    use std::collections::{BTreeSet, HashMap};

    use crate::{
        asm_generator::{collect_locals, AsmGenerator, FunctionContext},
        ast::{BlockStmt, Expr, ExprStmt, ForStmt, Literal, Stmt, VarStmt},
        lexer::Lexer,
        parser::Parser,
    };

    #[test]
    fn generates_main_with_arithmetic() {
        let source = r#"
fun main(): Num {
    var a: Num = 1 + 2;
    return a;
}
"#;

        let mut lexer = Lexer::from_source(source);
        let tokens = lexer.lex_file();
        let mut parser = Parser::new(tokens);
        let ast = parser.parse();

        let mut generator = AsmGenerator::new();
        let output = generator.generate(&ast);

        assert!(output.contains("_start:"));
        assert!(output.contains("call main"));
        assert!(output.contains("main:"));
        assert!(output.contains("add rax, rbx"));
        assert!(output.contains("ret"));
    }

    #[test]
    fn unrolls_for_loop_over_array_literal() {
        let source = r#"
fun main(): Num {
    for item, index in [10, 20] {
        var sum: Num = item + index;
    }
    return 0;
}
"#;

        let mut lexer = Lexer::from_source(source);
        let tokens = lexer.lex_file();
        let mut parser = Parser::new(tokens);
        let ast = parser.parse();

        let mut generator = AsmGenerator::new();
        let output = generator.generate(&ast);

        assert!(output.contains("; unrolled loop iteration 0"));
        assert!(output.contains("; unrolled loop iteration 1"));
    }

    #[test]
    fn collect_locals_captures_loop_and_assignment_targets() {
        let stmt = Stmt::Block(BlockStmt {
            stmts: vec![
                Stmt::Var(VarStmt {
                    name: "sum".to_string(),
                    assignment: Expr::Literal(Literal::Num(0.0)),
                    var_type: crate::ast::Type::Num,
                }),
                Stmt::For(ForStmt {
                    item: "item".to_string(),
                    index: Some("index".to_string()),
                    iterator: Expr::Array(crate::ast::ArrayExpr {
                        array: vec![
                            Expr::Literal(Literal::Num(1.0)),
                            Expr::Literal(Literal::Num(2.0)),
                        ],
                    }),
                    body: Box::new(Stmt::Block(BlockStmt {
                        stmts: vec![Stmt::Expr(ExprStmt {
                            expr: Expr::Assignment(
                                Box::new(Expr::Literal(Literal::Identifier("acc".to_string()))),
                                Box::new(Expr::Literal(Literal::Identifier("item".to_string()))),
                            ),
                        })],
                    })),
                }),
            ],
        });

        let mut locals = BTreeSet::new();
        collect_locals(&stmt, &mut locals);

        assert!(locals.contains("sum"));
        assert!(locals.contains("item"));
        assert!(locals.contains("index"));
        assert!(locals.contains("acc"));
    }

    #[test]
    fn emit_setcc_writes_expected_instruction_sequence() {
        let generator = AsmGenerator::new();
        let mut lines = Vec::new();
        generator.emit_setcc("sete", &mut lines);

        assert_eq!(
            lines,
            vec![
                "    cmp rax, rbx".to_string(),
                "    sete al".to_string(),
                "    movzx rax, al".to_string()
            ]
        );
    }

    #[test]
    fn emit_literal_for_unknown_identifier_falls_back_to_zero() {
        let mut generator = AsmGenerator::new();
        let mut lines = Vec::new();
        let mut ctx = FunctionContext {
            var_offsets: HashMap::new(),
            epilogue_label: ".ep".to_string(),
        };

        generator.emit_literal(
            &Literal::Identifier("missing".to_string()),
            &mut ctx,
            &mut lines,
        );

        assert!(lines.contains(&"    ; unknown identifier 'missing'".to_string()));
        assert!(lines.contains(&"    mov rax, 0".to_string()));
    }

    #[test]
    fn new_label_increments_counter() {
        let mut generator = AsmGenerator::new();

        let first = generator.new_label("loop");
        let second = generator.new_label("loop");

        assert_eq!(first, ".loop_0");
        assert_eq!(second, ".loop_1");
    }
}
