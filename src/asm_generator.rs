use std::collections::{BTreeSet, HashMap};

use crate::{
    ast::{
        BlockStmt, ConstStmt, EnumStmt, Expr, ForStmt, FunStmt, IfStmt, Literal, MatchStmt,
        Pattern, Stmt, StructStmt, VarStmt, WhileStmt,
    },
    lexer::Token,
};

pub struct AsmGenerator {
    label_counter: usize,
    enum_variant_tags: HashMap<String, i64>,
}

impl AsmGenerator {
    pub fn new() -> Self {
        Self {
            label_counter: 0,
            enum_variant_tags: HashMap::new(),
        }
    }

    pub fn generate(&mut self, program: &BlockStmt) -> String {
        self.enum_variant_tags = self.collect_enum_variant_tags(program);

        let mut text_lines = vec![
            "section .text".to_string(),
            "    global _start".to_string(),
            String::new(),
            "_start:".to_string(),
        ];
        let mut rodata_lines = vec!["section .rodata".to_string()];

        let has_main = program.stmts.iter().any(|stmt| match stmt {
            Stmt::Fun(fun) => fun.name == "main",
            Stmt::Pub(pub_stmt) => {
                matches!(pub_stmt.stmt.as_ref(), Stmt::Fun(fun) if fun.name == "main")
            }
            _ => false,
        });

        if has_main {
            text_lines.push("    call main".to_string());
            text_lines.push("    mov rdi, rax".to_string());
        } else {
            text_lines.push("    mov rdi, 0".to_string());
        }

        text_lines.push("    mov rax, 60".to_string());
        text_lines.push("    syscall".to_string());
        text_lines.push(String::new());

        for stmt in &program.stmts {
            self.emit_top_level_stmt(stmt, &mut text_lines, &mut rodata_lines);
        }

        if rodata_lines.len() > 1 {
            text_lines.push(String::new());
            text_lines.append(&mut rodata_lines);
        }

        text_lines.join("\n")
    }

    fn emit_top_level_stmt(
        &mut self,
        stmt: &Stmt,
        text_lines: &mut Vec<String>,
        rodata_lines: &mut Vec<String>,
    ) {
        match stmt {
            Stmt::Pub(pub_stmt) => {
                self.emit_top_level_stmt(pub_stmt.stmt.as_ref(), text_lines, rodata_lines)
            }
            Stmt::Fun(fun_stmt) => self.emit_function(fun_stmt, text_lines),
            Stmt::Import(_) => {
                text_lines.push("; import statement ignored by asm backend".to_string())
            }
            Stmt::Mod(_) => text_lines.push("; mod statement ignored by asm backend".to_string()),
            Stmt::Use(_) => text_lines.push("; use statement ignored by asm backend".to_string()),
            Stmt::Class(_) => {
                text_lines.push("; class statement ignored by asm backend".to_string())
            }
            Stmt::Const(_) => {
                text_lines.push("; top-level const ignored by asm backend".to_string())
            }
            Stmt::Struct(struct_stmt) => self.emit_struct_metadata(struct_stmt, rodata_lines),
            Stmt::Enum(enum_stmt) => self.emit_enum_metadata(enum_stmt, rodata_lines),
            Stmt::Impl(impl_stmt) => self.emit_impl(impl_stmt, text_lines, rodata_lines),
            Stmt::Match(_) => {
                text_lines.push("; top-level match ignored by asm backend".to_string())
            }
            _ => text_lines.push("; top-level statement ignored by asm backend".to_string()),
        }
    }

    fn emit_impl(
        &mut self,
        impl_stmt: &crate::ast::ImplStmt,
        text_lines: &mut Vec<String>,
        rodata_lines: &mut Vec<String>,
    ) {
        let mut method_labels = Vec::new();
        for method in &impl_stmt.methods {
            let method_fun = match method {
                Stmt::Fun(fun_stmt) => Some(fun_stmt),
                Stmt::Pub(pub_stmt) => match pub_stmt.stmt.as_ref() {
                    Stmt::Fun(fun_stmt) => Some(fun_stmt),
                    _ => None,
                },
                _ => None,
            };

            let Some(fun_stmt) = method_fun else {
                continue;
            };

            let mut lowered = fun_stmt.clone();
            lowered.name = format!("{}__{}", impl_stmt.name, fun_stmt.name);
            method_labels.push(lowered.name.clone());
            self.emit_function(&lowered, text_lines);
        }

        rodata_lines.push(format!("__kek_impl_{}:", impl_stmt.name));
        rodata_lines.push(format!("    dq {}", method_labels.len()));
        for method_label in method_labels {
            rodata_lines.push(format!("    dq {}", method_label));
        }
    }

    fn emit_struct_metadata(&self, struct_stmt: &StructStmt, rodata_lines: &mut Vec<String>) {
        rodata_lines.push(format!("__kek_struct_{}:", struct_stmt.name));
        rodata_lines.push(format!("    dq {}", struct_stmt.fields.len()));
        for field in &struct_stmt.fields {
            rodata_lines.push(format!("    dq 0 ; field {}", field.name));
        }
    }

    fn emit_enum_metadata(&self, enum_stmt: &EnumStmt, rodata_lines: &mut Vec<String>) {
        rodata_lines.push(format!("__kek_enum_{}:", enum_stmt.name));
        rodata_lines.push(format!("    dq {}", enum_stmt.variants.len()));
        for (index, variant) in enum_stmt.variants.iter().enumerate() {
            rodata_lines.push(format!("__kek_enum_{}_{}:", enum_stmt.name, variant.name));
            rodata_lines.push(format!("    dq {}", index));
            rodata_lines.push(format!("    dq {}", variant.arguments.len()));
        }
    }

    fn collect_enum_variant_tags(&self, program: &BlockStmt) -> HashMap<String, i64> {
        let mut tags = HashMap::new();

        for stmt in &program.stmts {
            let enum_stmt = match stmt {
                Stmt::Enum(enum_stmt) => Some(enum_stmt),
                Stmt::Pub(pub_stmt) => match pub_stmt.stmt.as_ref() {
                    Stmt::Enum(enum_stmt) => Some(enum_stmt),
                    _ => None,
                },
                _ => None,
            };

            let Some(enum_stmt) = enum_stmt else {
                continue;
            };

            for (index, variant) in enum_stmt.variants.iter().enumerate() {
                tags.entry(variant.name.clone()).or_insert(index as i64);
            }
        }

        tags
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
            Stmt::Match(match_stmt) => self.emit_match(match_stmt, ctx, lines),
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
            Stmt::Pub(pub_stmt) => self.emit_stmt(pub_stmt.stmt.as_ref(), ctx, lines),
            Stmt::Mod(_) => lines.push("    ; mod ignored in function scope".to_string()),
            Stmt::Use(_) => lines.push("    ; use ignored in function scope".to_string()),
            Stmt::Import(_) => lines.push("    ; import ignored in function scope".to_string()),
            Stmt::Struct(_) => lines.push("    ; struct ignored in function scope".to_string()),
            Stmt::Enum(_) => lines.push("    ; enum ignored in function scope".to_string()),
            Stmt::Impl(_) => lines.push("    ; impl ignored in function scope".to_string()),
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

    fn emit_match(
        &mut self,
        match_stmt: &MatchStmt,
        ctx: &mut FunctionContext,
        lines: &mut Vec<String>,
    ) {
        if match_stmt.arms.is_empty() {
            return;
        }

        self.emit_expr(&match_stmt.expr, ctx, lines);
        lines.push("    mov r13, rax".to_string());

        let end_label = self.new_label("match_end");
        for (index, arm) in match_stmt.arms.iter().enumerate() {
            let fail_label = if index + 1 == match_stmt.arms.len() {
                end_label.clone()
            } else {
                self.new_label("match_next")
            };

            self.emit_pattern_guard(&arm.pattern, "r13", &fail_label, lines);
            self.emit_pattern_bindings(&arm.pattern, "r13", ctx, lines);
            self.emit_stmt(arm.body.as_ref(), ctx, lines);
            lines.push(format!("    jmp {}", end_label));

            if index + 1 != match_stmt.arms.len() {
                lines.push(format!("{}:", fail_label));
            }
        }

        lines.push(format!("{}:", end_label));
    }

    fn emit_pattern_guard(
        &mut self,
        pattern: &Pattern,
        value_reg: &str,
        fail_label: &str,
        lines: &mut Vec<String>,
    ) {
        match pattern {
            Pattern::Wildcard | Pattern::Identifier(_) => {}
            Pattern::Literal(literal) => match literal {
                Literal::Num(num) => {
                    lines.push(format!("    cmp {}, {}", value_reg, *num as i64));
                    lines.push(format!("    jne {}", fail_label));
                }
                Literal::Char(ch) => {
                    lines.push(format!("    cmp {}, {}", value_reg, *ch as u32));
                    lines.push(format!("    jne {}", fail_label));
                }
                Literal::Bool(value) => {
                    let as_num = if *value { 1 } else { 0 };
                    lines.push(format!("    cmp {}, {}", value_reg, as_num));
                    lines.push(format!("    jne {}", fail_label));
                }
                Literal::Identifier(name) if name == "None" => {
                    lines.push(format!("    cmp {}, 0", value_reg));
                    lines.push(format!("    jne {}", fail_label));
                }
                _ => {
                    lines
                        .push("    ; unsupported literal match pattern in asm backend".to_string());
                    lines.push(format!("    jmp {}", fail_label));
                }
            },
            Pattern::Variant(name, nested) => {
                if let Some(tag) = self.enum_variant_tags.get(name) {
                    lines.push(format!("    cmp {}, {}", value_reg, tag));
                    lines.push(format!("    jne {}", fail_label));
                    if !nested.is_empty() {
                        lines.push(
                            "    ; variant payload pattern checks are not represented in asm backend"
                                .to_string(),
                        );
                    }
                } else {
                    lines.push(format!("    ; unknown enum variant '{}'", name));
                    lines.push(format!("    jmp {}", fail_label));
                }
            }
        }
    }

    fn emit_pattern_bindings(
        &self,
        pattern: &Pattern,
        value_reg: &str,
        ctx: &mut FunctionContext,
        lines: &mut Vec<String>,
    ) {
        match pattern {
            Pattern::Identifier(name) => {
                self.store_pattern_binding(name, Some(value_reg), ctx, lines)
            }
            Pattern::Variant(_, nested) => {
                for nested_pattern in nested {
                    self.emit_pattern_bindings_zeroed(nested_pattern, ctx, lines);
                }
            }
            _ => {}
        }
    }

    fn emit_pattern_bindings_zeroed(
        &self,
        pattern: &Pattern,
        ctx: &mut FunctionContext,
        lines: &mut Vec<String>,
    ) {
        match pattern {
            Pattern::Identifier(name) => self.store_pattern_binding(name, None, ctx, lines),
            Pattern::Variant(_, nested) => {
                for nested_pattern in nested {
                    self.emit_pattern_bindings_zeroed(nested_pattern, ctx, lines);
                }
            }
            _ => {}
        }
    }

    fn store_pattern_binding(
        &self,
        name: &str,
        source_reg: Option<&str>,
        ctx: &mut FunctionContext,
        lines: &mut Vec<String>,
    ) {
        if let Some(offset) = ctx.var_offsets.get(name) {
            if let Some(source_reg) = source_reg {
                lines.push(format!("    mov QWORD [rbp-{}], {}", offset, source_reg));
            } else {
                lines.push(format!("    mov QWORD [rbp-{}], 0", offset));
            }
        } else {
            lines.push(format!(
                "    ; match binding '{}' has no stack slot in asm backend",
                name
            ));
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
        Stmt::Pub(pub_stmt) => collect_locals(pub_stmt.stmt.as_ref(), locals),
        Stmt::Match(match_stmt) => {
            collect_expr_locals(&match_stmt.expr, locals);
            for arm in &match_stmt.arms {
                collect_pattern_locals(&arm.pattern, locals);
                collect_locals(arm.body.as_ref(), locals);
            }
        }
        Stmt::Return(ret) => collect_expr_locals(&ret.return_expr, locals),
        Stmt::Expr(expr_stmt) => collect_expr_locals(&expr_stmt.expr, locals),
        _ => {}
    }
}

fn collect_pattern_locals(pattern: &Pattern, locals: &mut BTreeSet<String>) {
    match pattern {
        Pattern::Identifier(name) => {
            locals.insert(name.clone());
        }
        Pattern::Variant(_, nested) => {
            for nested_pattern in nested {
                collect_pattern_locals(nested_pattern, locals);
            }
        }
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
        ast::{
            BlockStmt, EnumStmt, EnumVariant, Expr, ExprStmt, ForStmt, FunStmt, ImplStmt, Literal,
            MatchArm, MatchStmt, Pattern, ReturnStmt, Stmt, Type, VarStmt,
        },
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

    #[test]
    fn collect_enum_variant_tags_assigns_incrementing_tags() {
        let program = BlockStmt {
            stmts: vec![Stmt::Enum(EnumStmt {
                name: "Maybe".to_string(),
                variants: vec![
                    EnumVariant {
                        name: "Some".to_string(),
                        arguments: vec![Type::Num],
                    },
                    EnumVariant {
                        name: "Empty".to_string(),
                        arguments: vec![],
                    },
                ],
            })],
        };

        let generator = AsmGenerator::new();
        let tags = generator.collect_enum_variant_tags(&program);

        assert_eq!(tags.get("Some"), Some(&0));
        assert_eq!(tags.get("Empty"), Some(&1));
    }

    #[test]
    fn emit_pattern_guard_literal_emits_cmp_and_jump() {
        let mut generator = AsmGenerator::new();
        let mut lines = Vec::new();

        generator.emit_pattern_guard(
            &Pattern::Literal(Literal::Num(7.0)),
            "r13",
            ".match_fail",
            &mut lines,
        );

        assert_eq!(
            lines,
            vec![
                "    cmp r13, 7".to_string(),
                "    jne .match_fail".to_string()
            ]
        );
    }

    #[test]
    fn emit_pattern_guard_variant_uses_enum_tag() {
        let mut generator = AsmGenerator::new();
        generator.enum_variant_tags.insert("Some".to_string(), 3);
        let mut lines = Vec::new();

        generator.emit_pattern_guard(
            &Pattern::Variant("Some".to_string(), vec![]),
            "r13",
            ".match_fail",
            &mut lines,
        );

        assert!(lines.contains(&"    cmp r13, 3".to_string()));
        assert!(lines.contains(&"    jne .match_fail".to_string()));
    }

    #[test]
    fn emit_match_binds_identifier_pattern_to_stack_slot() {
        let mut generator = AsmGenerator::new();
        let mut lines = Vec::new();
        let mut ctx = FunctionContext {
            var_offsets: HashMap::from([("bound".to_string(), 8)]),
            epilogue_label: ".ep".to_string(),
        };

        generator.emit_match(
            &MatchStmt {
                expr: Expr::Literal(Literal::Num(1.0)),
                arms: vec![MatchArm {
                    pattern: Pattern::Identifier("bound".to_string()),
                    body: Box::new(Stmt::Return(ReturnStmt {
                        return_expr: Expr::Literal(Literal::Identifier("bound".to_string())),
                    })),
                }],
            },
            &mut ctx,
            &mut lines,
        );

        assert!(lines.contains(&"    mov r13, rax".to_string()));
        assert!(lines.contains(&"    mov QWORD [rbp-8], r13".to_string()));
        assert!(lines.iter().any(|line| line.starts_with(".match_end_")));
    }

    #[test]
    fn emit_impl_produces_method_label_and_metadata_entries() {
        let mut generator = AsmGenerator::new();
        let mut text = Vec::new();
        let mut rodata = vec!["section .rodata".to_string()];

        generator.emit_impl(
            &ImplStmt {
                name: "Point".to_string(),
                methods: vec![Stmt::Fun(FunStmt {
                    name: "value".to_string(),
                    return_type: Type::Num,
                    params: vec![],
                    block: Box::new(Stmt::Block(BlockStmt {
                        stmts: vec![Stmt::Return(ReturnStmt {
                            return_expr: Expr::Literal(Literal::Num(1.0)),
                        })],
                    })),
                })],
            },
            &mut text,
            &mut rodata,
        );

        assert!(text.iter().any(|line| line == "Point__value:"));
        assert!(rodata.iter().any(|line| line == "__kek_impl_Point:"));
        assert!(rodata.iter().any(|line| line == "    dq Point__value"));
    }
}
