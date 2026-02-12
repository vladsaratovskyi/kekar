use std::collections::{BTreeSet, HashMap};

use crate::{
    ast::{
        BlockStmt, ClassStmt, ConstStmt, EnumStmt, Expr, ForStmt, FunStmt, IfStmt, Literal,
        MatchStmt, Param, Pattern, Stmt, StructStmt, Type, VarStmt, WhileStmt,
    },
    lexer::Token,
};

pub struct AsmGenerator {
    label_counter: usize,
    string_counter: usize,
    string_literals: Vec<(String, String)>,
    string_labels: HashMap<String, String>,
    enum_variant_tags: HashMap<String, i64>,
    enum_variant_types: HashMap<String, String>,
    enum_variant_payloads: HashMap<String, usize>,
    function_returns: HashMap<String, Type>,
    type_layouts: HashMap<String, TypeLayout>,
    method_sigs: HashMap<String, HashMap<String, MethodSig>>,
    class_initializers: HashMap<String, MethodSig>,
    emitted_impl_metadata: BTreeSet<String>,
}

#[derive(Clone)]
struct FieldLayout {
    offset: i64,
    ty: Type,
}

#[derive(Clone)]
struct TypeLayout {
    field_order: Vec<String>,
    fields: HashMap<String, FieldLayout>,
}

#[derive(Clone)]
struct MethodSig {
    label: String,
    params: Vec<Type>,
    return_type: Type,
}

#[derive(Clone)]
struct LoopLabels {
    break_label: String,
    continue_label: String,
}

impl AsmGenerator {
    pub fn new() -> Self {
        Self {
            label_counter: 0,
            string_counter: 0,
            string_literals: Vec::new(),
            string_labels: HashMap::new(),
            enum_variant_tags: HashMap::new(),
            enum_variant_types: HashMap::new(),
            enum_variant_payloads: HashMap::new(),
            function_returns: HashMap::new(),
            type_layouts: HashMap::new(),
            method_sigs: HashMap::new(),
            class_initializers: HashMap::new(),
            emitted_impl_metadata: BTreeSet::new(),
        }
    }

    pub fn generate(&mut self, program: &BlockStmt) -> String {
        self.prepare_program_metadata(program);

        let mut text_lines = vec![
            "section .text".to_string(),
            "    global _start".to_string(),
            String::new(),
            "__kek_runtime_init:".to_string(),
            "    lea rax, [rel __kek_heap]".to_string(),
            "    mov [rel __kek_heap_ptr], rax".to_string(),
            "    ret".to_string(),
            String::new(),
            "__kek_alloc:".to_string(),
            "    mov rax, [rel __kek_heap_ptr]".to_string(),
            "    mov rcx, rax".to_string(),
            "    add rcx, rdi".to_string(),
            "    lea rdx, [rel __kek_heap_end]".to_string(),
            "    cmp rcx, rdx".to_string(),
            "    jg __kek_alloc_oom".to_string(),
            "    mov [rel __kek_heap_ptr], rcx".to_string(),
            "    ret".to_string(),
            String::new(),
            "__kek_alloc_oom:".to_string(),
            "    mov rax, 60".to_string(),
            "    mov rdi, 70".to_string(),
            "    syscall".to_string(),
            String::new(),
            "_start:".to_string(),
        ];
        let mut rodata_lines = vec!["section .rodata".to_string()];
        let mut bss_lines = vec![
            "section .bss".to_string(),
            "    align 8".to_string(),
            "__kek_heap:".to_string(),
            "    resb 1048576".to_string(),
            "__kek_heap_end:".to_string(),
            "__kek_heap_ptr:".to_string(),
            "    resq 1".to_string(),
        ];

        let has_main = program.stmts.iter().any(|stmt| match stmt {
            Stmt::Fun(fun) => fun.name == "main",
            Stmt::Pub(pub_stmt) => {
                matches!(pub_stmt.stmt.as_ref(), Stmt::Fun(fun) if fun.name == "main")
            }
            _ => false,
        });

        text_lines.push("    call __kek_runtime_init".to_string());
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

        self.emit_string_literals(&mut rodata_lines);

        if rodata_lines.len() > 1 {
            text_lines.push(String::new());
            text_lines.append(&mut rodata_lines);
        }

        if bss_lines.len() > 1 {
            text_lines.push(String::new());
            text_lines.append(&mut bss_lines);
        }

        text_lines.join("\n")
    }

    fn prepare_program_metadata(&mut self, program: &BlockStmt) {
        self.enum_variant_tags = self.collect_enum_variant_tags(program);
        self.enum_variant_types.clear();
        self.enum_variant_payloads.clear();
        self.function_returns.clear();
        self.type_layouts.clear();
        self.method_sigs.clear();
        self.class_initializers.clear();
        self.emitted_impl_metadata.clear();
        self.string_counter = 0;
        self.string_literals.clear();
        self.string_labels.clear();

        for stmt in &program.stmts {
            let inner = match stmt {
                Stmt::Pub(pub_stmt) => pub_stmt.stmt.as_ref(),
                other => other,
            };

            match inner {
                Stmt::Fun(fun_stmt) => {
                    self.function_returns
                        .insert(fun_stmt.name.clone(), fun_stmt.return_type.clone());
                }
                Stmt::Enum(enum_stmt) => {
                    for variant in &enum_stmt.variants {
                        self.enum_variant_types
                            .insert(variant.name.clone(), enum_stmt.name.clone());
                        self.enum_variant_payloads
                            .insert(variant.name.clone(), variant.arguments.len());
                    }
                }
                Stmt::Struct(struct_stmt) => {
                    self.type_layouts.insert(
                        struct_stmt.name.clone(),
                        self.layout_from_struct(struct_stmt),
                    );
                    self.collect_struct_methods(struct_stmt);
                }
                Stmt::Class(class_stmt) => {
                    self.type_layouts
                        .insert(class_stmt.name.clone(), self.layout_from_class(class_stmt));
                    self.collect_class_methods(class_stmt);
                }
                Stmt::Impl(impl_stmt) => {
                    self.collect_impl_methods(impl_stmt);
                }
                _ => {}
            }
        }
    }

    fn layout_from_struct(&self, struct_stmt: &StructStmt) -> TypeLayout {
        let mut fields = HashMap::new();
        let mut field_order = Vec::new();
        for (index, field) in struct_stmt.fields.iter().enumerate() {
            field_order.push(field.name.clone());
            fields.insert(
                field.name.clone(),
                FieldLayout {
                    offset: (index as i64) * 8,
                    ty: field.field_type.clone(),
                },
            );
        }

        TypeLayout {
            field_order,
            fields,
        }
    }

    fn layout_from_class(&self, class_stmt: &ClassStmt) -> TypeLayout {
        let mut fields = HashMap::new();
        let mut field_order = Vec::new();
        let members = match class_stmt.block.as_ref() {
            Stmt::Block(block) => &block.stmts,
            _ => {
                return TypeLayout {
                    field_order,
                    fields,
                }
            }
        };

        for member in members {
            let member = match member {
                Stmt::Pub(pub_stmt) => pub_stmt.stmt.as_ref(),
                other => other,
            };

            if let Stmt::Var(var_stmt) = member {
                let offset = (field_order.len() as i64) * 8;
                field_order.push(var_stmt.name.clone());
                fields.insert(
                    var_stmt.name.clone(),
                    FieldLayout {
                        offset,
                        ty: var_stmt.var_type.clone(),
                    },
                );
            }
        }

        TypeLayout {
            field_order,
            fields,
        }
    }

    fn collect_impl_methods(&mut self, impl_stmt: &crate::ast::ImplStmt) {
        let methods = self.method_sigs.entry(impl_stmt.name.clone()).or_default();
        for method in &impl_stmt.methods {
            let method = match method {
                Stmt::Pub(pub_stmt) => pub_stmt.stmt.as_ref(),
                other => other,
            };

            let Stmt::Fun(fun_stmt) = method else {
                continue;
            };
            methods.insert(
                fun_stmt.name.clone(),
                MethodSig {
                    label: format!("{}__{}", impl_stmt.name, fun_stmt.name),
                    params: fun_stmt
                        .params
                        .iter()
                        .map(|param| param.param_type.clone())
                        .collect(),
                    return_type: fun_stmt.return_type.clone(),
                },
            );
        }
    }

    fn collect_struct_methods(&mut self, struct_stmt: &StructStmt) {
        let methods = self
            .method_sigs
            .entry(struct_stmt.name.clone())
            .or_default();
        for method in &struct_stmt.methods {
            let method = match method {
                Stmt::Pub(pub_stmt) => pub_stmt.stmt.as_ref(),
                other => other,
            };

            let Stmt::Fun(fun_stmt) = method else {
                continue;
            };
            methods.insert(
                fun_stmt.name.clone(),
                MethodSig {
                    label: format!("{}__{}", struct_stmt.name, fun_stmt.name),
                    params: fun_stmt
                        .params
                        .iter()
                        .map(|param| param.param_type.clone())
                        .collect(),
                    return_type: fun_stmt.return_type.clone(),
                },
            );
        }
    }

    fn collect_class_methods(&mut self, class_stmt: &ClassStmt) {
        let methods = self.method_sigs.entry(class_stmt.name.clone()).or_default();
        let members = match class_stmt.block.as_ref() {
            Stmt::Block(block) => &block.stmts,
            _ => return,
        };

        for member in members {
            let member = match member {
                Stmt::Pub(pub_stmt) => pub_stmt.stmt.as_ref(),
                other => other,
            };

            let Stmt::Fun(fun_stmt) = member else {
                continue;
            };

            let sig = MethodSig {
                label: format!("{}__{}", class_stmt.name, fun_stmt.name),
                params: fun_stmt
                    .params
                    .iter()
                    .map(|param| param.param_type.clone())
                    .collect(),
                return_type: fun_stmt.return_type.clone(),
            };
            if fun_stmt.name == "init" {
                self.class_initializers
                    .insert(class_stmt.name.clone(), sig.clone());
            }
            methods.insert(fun_stmt.name.clone(), sig);
        }
    }

    fn emit_string_literals(&mut self, rodata_lines: &mut Vec<String>) {
        if self.string_literals.is_empty() {
            return;
        }

        for (label, value) in &self.string_literals {
            rodata_lines.push(format!("{}:", label));
            rodata_lines.push(format!("    db {}, 0", escape_asm_string(value)));
        }
    }

    fn intern_string_literal(&mut self, value: &str) -> String {
        if let Some(label) = self.string_labels.get(value) {
            return label.clone();
        }

        let label = format!("__kek_str_{}", self.string_counter);
        self.string_counter += 1;
        self.string_labels.insert(value.to_string(), label.clone());
        self.string_literals
            .push((label.clone(), value.to_string()));
        label
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
            Stmt::Class(class_stmt) => self.emit_class(class_stmt, text_lines, rodata_lines),
            Stmt::Const(_) => {
                text_lines.push("; top-level const ignored by asm backend".to_string())
            }
            Stmt::Struct(struct_stmt) => self.emit_struct(struct_stmt, text_lines, rodata_lines),
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
            lowered.params.insert(
                0,
                Param {
                    name: "this".to_string(),
                    param_type: Type::Identifier(impl_stmt.name.clone()),
                },
            );
            method_labels.push(lowered.name.clone());
            self.emit_function(&lowered, text_lines);
        }

        if self.emitted_impl_metadata.insert(impl_stmt.name.clone()) {
            rodata_lines.push(format!("__kek_impl_{}:", impl_stmt.name));
            rodata_lines.push(format!("    dq {}", method_labels.len()));
            for method_label in method_labels {
                rodata_lines.push(format!("    dq {}", method_label));
            }
        } else {
            rodata_lines.push(format!(
                "; duplicate impl metadata for '{}' skipped",
                impl_stmt.name
            ));
        }
    }

    fn emit_struct(
        &mut self,
        struct_stmt: &StructStmt,
        text_lines: &mut Vec<String>,
        rodata_lines: &mut Vec<String>,
    ) {
        let mut method_labels = Vec::new();
        for method in &struct_stmt.methods {
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
            lowered.name = format!("{}__{}", struct_stmt.name, fun_stmt.name);
            lowered.params.insert(
                0,
                Param {
                    name: "this".to_string(),
                    param_type: Type::Identifier(struct_stmt.name.clone()),
                },
            );
            method_labels.push(lowered.name.clone());
            self.emit_function(&lowered, text_lines);
        }

        self.emit_struct_metadata(struct_stmt, rodata_lines);
        if !method_labels.is_empty() {
            if self.emitted_impl_metadata.insert(struct_stmt.name.clone()) {
                rodata_lines.push(format!("__kek_impl_{}:", struct_stmt.name));
                rodata_lines.push(format!("    dq {}", method_labels.len()));
                for method_label in method_labels {
                    rodata_lines.push(format!("    dq {}", method_label));
                }
            } else {
                rodata_lines.push(format!(
                    "; duplicate impl metadata for '{}' skipped",
                    struct_stmt.name
                ));
            }
        }
    }

    fn emit_class(
        &mut self,
        class_stmt: &ClassStmt,
        text_lines: &mut Vec<String>,
        rodata_lines: &mut Vec<String>,
    ) {
        let mut method_labels = Vec::new();
        let members = match class_stmt.block.as_ref() {
            Stmt::Block(block) => &block.stmts,
            _ => {
                text_lines.push(format!(
                    "; class '{}' body is not a block in asm backend",
                    class_stmt.name
                ));
                return;
            }
        };

        for member in members {
            let method = match member {
                Stmt::Pub(pub_stmt) => pub_stmt.stmt.as_ref(),
                other => other,
            };

            let Stmt::Fun(fun_stmt) = method else {
                continue;
            };

            let mut lowered = fun_stmt.clone();
            lowered.name = format!("{}__{}", class_stmt.name, fun_stmt.name);
            lowered.params.insert(
                0,
                Param {
                    name: "this".to_string(),
                    param_type: Type::Identifier(class_stmt.name.clone()),
                },
            );
            method_labels.push(lowered.name.clone());
            self.emit_function(&lowered, text_lines);
        }

        rodata_lines.push(format!("__kek_class_{}:", class_stmt.name));
        if let Some(layout) = self.type_layouts.get(&class_stmt.name) {
            rodata_lines.push(format!("    dq {}", layout.field_order.len()));
            for field_name in &layout.field_order {
                rodata_lines.push(format!("    dq 0 ; field {}", field_name));
            }
        } else {
            rodata_lines.push("    dq 0".to_string());
        }
        rodata_lines.push(format!("__kek_class_impl_{}:", class_stmt.name));
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
        let mut var_types = HashMap::new();
        for param in &fun_stmt.params {
            local_names.insert(param.name.clone());
            var_types.insert(param.name.clone(), param.param_type.clone());
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
            var_types,
            epilogue_label: epilogue_label.clone(),
            loop_stack: Vec::new(),
        };

        lines.push(format!("{}:", fun_stmt.name));
        lines.push("    push rbp".to_string());
        lines.push("    mov rbp, rsp".to_string());
        lines.push("    push rbx".to_string());
        lines.push("    push r12".to_string());
        lines.push("    push r13".to_string());
        lines.push("    push r14".to_string());
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
        if offset > 0 {
            lines.push(format!("    add rsp, {}", offset));
        }
        lines.push("    pop r14".to_string());
        lines.push("    pop r13".to_string());
        lines.push("    pop r12".to_string());
        lines.push("    pop rbx".to_string());
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
            Stmt::Break(_) => match ctx.loop_stack.last() {
                Some(loop_labels) => lines.push(format!("    jmp {}", loop_labels.break_label)),
                None => lines.push("    ; break outside loop".to_string()),
            },
            Stmt::Continue(_) => match ctx.loop_stack.last() {
                Some(loop_labels) => lines.push(format!("    jmp {}", loop_labels.continue_label)),
                None => lines.push("    ; continue outside loop".to_string()),
            },
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
        let inferred_type = if matches!(var_stmt.var_type, Type::None) {
            self.infer_expr_type(&var_stmt.assignment, ctx)
        } else {
            var_stmt.var_type.clone()
        };
        ctx.var_types.insert(var_stmt.name.clone(), inferred_type);

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
        let inferred_type = if matches!(const_stmt.const_type, Type::None) {
            self.infer_expr_type(&const_stmt.assignment, ctx)
        } else {
            const_stmt.const_type.clone()
        };
        ctx.var_types.insert(const_stmt.name.clone(), inferred_type);

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

        ctx.loop_stack.push(LoopLabels {
            break_label: end_label.clone(),
            continue_label: loop_label.clone(),
        });
        self.emit_stmt(while_stmt.body.as_ref(), ctx, lines);
        ctx.loop_stack.pop();
        lines.push(format!("    jmp {}", loop_label));
        lines.push(format!("{}:", end_label));
    }

    fn emit_for(&mut self, for_stmt: &ForStmt, ctx: &mut FunctionContext, lines: &mut Vec<String>) {
        let loop_label = self.new_label("for_loop");
        let continue_label = self.new_label("for_continue");
        let end_label = self.new_label("for_end");

        self.emit_expr(&for_stmt.iterator, ctx, lines);
        lines.push("    push rax ; for iterator array".to_string());
        lines.push("    push 0 ; for iterator index".to_string());
        lines.push(format!("{}:", loop_label));
        lines.push("    mov rcx, QWORD [rsp]".to_string());
        lines.push("    mov rbx, QWORD [rsp+8]".to_string());
        lines.push("    cmp rcx, QWORD [rbx]".to_string());
        lines.push(format!("    jge {}", end_label));
        lines.push("    mov rax, QWORD [rbx + rcx*8 + 8]".to_string());

        if let Some(item_offset) = ctx.var_offsets.get(&for_stmt.item) {
            lines.push(format!("    mov QWORD [rbp-{}], rax", item_offset));
        }

        if let Some(index_name) = &for_stmt.index {
            if let Some(index_offset) = ctx.var_offsets.get(index_name) {
                lines.push("    mov rax, rcx".to_string());
                lines.push(format!("    mov QWORD [rbp-{}], rax", index_offset));
            }
        }

        ctx.loop_stack.push(LoopLabels {
            break_label: end_label.clone(),
            continue_label: continue_label.clone(),
        });
        self.emit_stmt(for_stmt.body.as_ref(), ctx, lines);
        ctx.loop_stack.pop();

        lines.push(format!("{}:", continue_label));
        lines.push("    mov rcx, QWORD [rsp]".to_string());
        lines.push("    add rcx, 1".to_string());
        lines.push("    mov QWORD [rsp], rcx".to_string());
        lines.push(format!("    jmp {}", loop_label));
        lines.push(format!("{}:", end_label));
        lines.push("    add rsp, 16".to_string());
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
                Literal::String(value) => {
                    let label = self.intern_string_literal(value);
                    lines.push(format!("    lea r14, [rel {}]", label));
                    lines.push(format!("    cmp {}, r14", value_reg));
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
                    lines.push(format!("    cmp {}, 0", value_reg));
                    lines.push(format!("    je {}", fail_label));
                    lines.push(format!("    cmp QWORD [{}], {}", value_reg, tag));
                    lines.push(format!("    jne {}", fail_label));
                    if !nested.is_empty() {
                        lines.push(format!("    cmp QWORD [{}+8], {}", value_reg, nested.len()));
                        lines.push(format!("    jne {}", fail_label));
                        for (index, nested_pattern) in nested.iter().enumerate() {
                            lines.push(format!(
                                "    mov r14, QWORD [{}+{}]",
                                value_reg,
                                16 + index * 8
                            ));
                            self.emit_pattern_guard(nested_pattern, "r14", fail_label, lines);
                        }
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
                self.emit_variant_pattern_bindings(nested, value_reg, ctx, lines);
            }
            _ => {}
        }
    }

    fn emit_variant_pattern_bindings(
        &self,
        patterns: &[Pattern],
        value_reg: &str,
        ctx: &mut FunctionContext,
        lines: &mut Vec<String>,
    ) {
        for (index, pattern) in patterns.iter().enumerate() {
            lines.push(format!(
                "    mov r14, QWORD [{}+{}]",
                value_reg,
                16 + index * 8
            ));
            match pattern {
                Pattern::Identifier(name) => {
                    self.store_pattern_binding(name, Some("r14"), ctx, lines)
                }
                Pattern::Variant(_, nested) => {
                    self.emit_variant_pattern_bindings(nested, "r14", ctx, lines)
                }
                Pattern::Wildcard | Pattern::Literal(_) => {}
            }
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
                    Token::Question => {}
                    _ => lines.push("    ; unsupported unary operator".to_string()),
                }
            }
            Expr::Binary(left, op, right) => {
                if *op == Token::And {
                    self.emit_short_circuit_and(left, right, ctx, lines);
                    return;
                }

                if *op == Token::Or {
                    self.emit_short_circuit_or(left, right, ctx, lines);
                    return;
                }

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
                    _ => lines.push("    ; unsupported binary operator".to_string()),
                }
            }
            Expr::Assignment(left, right) => match left.as_ref() {
                Expr::Literal(Literal::Identifier(name)) => {
                    self.emit_expr(right, ctx, lines);
                    if let Some(offset) = ctx.var_offsets.get(name) {
                        lines.push(format!("    mov QWORD [rbp-{}], rax", offset));
                        if let Some(existing) = ctx.var_types.get(name).cloned() {
                            if matches!(existing, Type::None) {
                                ctx.var_types
                                    .insert(name.clone(), self.infer_expr_type(right, ctx));
                            }
                        }
                    } else {
                        lines.push(format!(
                            "    ; assignment target '{}' not found in local slots",
                            name
                        ));
                    }
                }
                Expr::Mebmer(member) => {
                    let owner_type = self.infer_expr_type(member.member.as_ref(), ctx);
                    self.emit_expr(member.member.as_ref(), ctx, lines);
                    lines.push("    push rax".to_string());
                    self.emit_expr(right, ctx, lines);
                    lines.push("    mov rcx, rax".to_string());
                    lines.push("    pop rbx".to_string());

                    if let Type::Identifier(type_name) = owner_type {
                        if let Some(offset) = self.field_offset(&type_name, &member.property) {
                            lines.push(format!("    mov QWORD [rbx+{}], rcx", offset));
                            lines.push("    mov rax, rcx".to_string());
                        } else {
                            lines.push(format!(
                                "    ; unknown member '{}.{}' in asm backend",
                                type_name, member.property
                            ));
                            lines.push("    mov rax, rcx".to_string());
                        }
                    } else {
                        lines.push("    ; member assignment requires user type".to_string());
                        lines.push("    mov rax, rcx".to_string());
                    }
                }
                Expr::ComputedExpr(computed) => {
                    self.emit_expr(computed.member.as_ref(), ctx, lines);
                    lines.push("    push rax".to_string());
                    self.emit_expr(computed.property.as_ref(), ctx, lines);
                    lines.push("    push rax".to_string());
                    self.emit_expr(right, ctx, lines);
                    lines.push("    mov rcx, QWORD [rsp]".to_string());
                    lines.push("    add rsp, 8".to_string());
                    lines.push("    pop rbx".to_string());
                    lines.push("    mov QWORD [rbx + rcx*8 + 8], rax".to_string());
                }
                _ => {
                    self.emit_expr(right, ctx, lines);
                    lines.push("    ; unsupported assignment target".to_string());
                }
            },
            Expr::Call(call) => match call.callee.as_ref() {
                Expr::Literal(Literal::Identifier(name)) => {
                    if self.type_layouts.contains_key(name) {
                        self.emit_constructor_call(name, &call.arguments, ctx, lines);
                    } else if let Some(enum_name) = self.enum_variant_types.get(name).cloned() {
                        self.emit_enum_variant_constructor(
                            &enum_name,
                            name,
                            &call.arguments,
                            ctx,
                            lines,
                        );
                    } else {
                        let registers = ["rdi", "rsi", "rdx", "rcx", "r8", "r9"];
                        for (index, arg) in call.arguments.iter().enumerate() {
                            self.emit_expr(arg, ctx, lines);
                            if let Some(reg) = registers.get(index) {
                                lines.push(format!("    mov {}, rax", reg));
                            } else {
                                lines.push(
                                    "    ; call argument exceeds register support".to_string(),
                                );
                            }
                        }
                        lines.push(format!("    call {}", name));
                    }
                }
                Expr::Mebmer(member) => {
                    self.emit_member_call(member, &call.arguments, ctx, lines);
                }
                _ => {
                    lines.push("    ; dynamic/member call unsupported in asm backend".to_string());
                    lines.push("    mov rax, 0".to_string());
                }
            },
            Expr::Mebmer(member) => {
                let owner_type = self.infer_expr_type(member.member.as_ref(), ctx);
                self.emit_expr(member.member.as_ref(), ctx, lines);
                if let Type::Identifier(type_name) = owner_type {
                    if let Some(offset) = self.field_offset(&type_name, &member.property) {
                        lines.push(format!("    mov rax, QWORD [rax+{}]", offset));
                    } else {
                        lines.push(format!(
                            "    ; unknown member '{}.{}' in asm backend",
                            type_name, member.property
                        ));
                        lines.push("    mov rax, 0".to_string());
                    }
                } else {
                    lines.push("    ; member access requires user type".to_string());
                    lines.push("    mov rax, 0".to_string());
                }
            }
            Expr::ComputedExpr(computed) => {
                self.emit_expr(computed.member.as_ref(), ctx, lines);
                lines.push("    push rax".to_string());
                self.emit_expr(computed.property.as_ref(), ctx, lines);
                lines.push("    mov rcx, rax".to_string());
                lines.push("    pop rbx".to_string());
                lines.push("    mov rax, QWORD [rbx + rcx*8 + 8]".to_string());
            }
            Expr::Array(array) => {
                let total_bytes = ((array.array.len() + 1) * 8) as i64;
                lines.push(format!("    mov rdi, {}", total_bytes));
                lines.push("    call __kek_alloc".to_string());
                lines.push("    push rax".to_string());
                lines.push(format!("    mov QWORD [rax], {}", array.array.len()));
                for (index, item) in array.array.iter().enumerate() {
                    self.emit_expr(item, ctx, lines);
                    lines.push("    mov rbx, QWORD [rsp]".to_string());
                    lines.push(format!("    mov QWORD [rbx+{}], rax", (index + 1) * 8));
                }
                lines.push("    mov rax, QWORD [rsp]".to_string());
                lines.push("    add rsp, 8".to_string());
            }
            Expr::Empty => lines.push("    mov rax, 0".to_string()),
        }
    }

    fn emit_short_circuit_and(
        &mut self,
        left: &Expr,
        right: &Expr,
        ctx: &mut FunctionContext,
        lines: &mut Vec<String>,
    ) {
        let false_label = self.new_label("logic_and_false");
        let end_label = self.new_label("logic_and_end");

        self.emit_expr(left, ctx, lines);
        lines.push("    cmp rax, 0".to_string());
        lines.push(format!("    je {}", false_label));

        self.emit_expr(right, ctx, lines);
        lines.push("    cmp rax, 0".to_string());
        lines.push("    setne al".to_string());
        lines.push("    movzx rax, al".to_string());
        lines.push(format!("    jmp {}", end_label));

        lines.push(format!("{}:", false_label));
        lines.push("    mov rax, 0".to_string());
        lines.push(format!("{}:", end_label));
    }

    fn emit_short_circuit_or(
        &mut self,
        left: &Expr,
        right: &Expr,
        ctx: &mut FunctionContext,
        lines: &mut Vec<String>,
    ) {
        let true_label = self.new_label("logic_or_true");
        let end_label = self.new_label("logic_or_end");

        self.emit_expr(left, ctx, lines);
        lines.push("    cmp rax, 0".to_string());
        lines.push(format!("    jne {}", true_label));

        self.emit_expr(right, ctx, lines);
        lines.push("    cmp rax, 0".to_string());
        lines.push("    setne al".to_string());
        lines.push("    movzx rax, al".to_string());
        lines.push(format!("    jmp {}", end_label));

        lines.push(format!("{}:", true_label));
        lines.push("    mov rax, 1".to_string());
        lines.push(format!("{}:", end_label));
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
            Literal::String(value) => {
                let label = self.intern_string_literal(value);
                lines.push(format!("    lea rax, [rel {}]", label));
            }
            Literal::This => {
                if let Some(offset) = ctx.var_offsets.get("this") {
                    lines.push(format!("    mov rax, QWORD [rbp-{}]", offset));
                } else {
                    lines.push("    ; 'this' is not in scope".to_string());
                    lines.push("    mov rax, 0".to_string());
                }
            }
        }
    }

    fn field_offset(&self, type_name: &str, field_name: &str) -> Option<i64> {
        self.type_layouts
            .get(type_name)
            .and_then(|layout| layout.fields.get(field_name))
            .map(|field| field.offset)
    }

    fn emit_constructor_call(
        &mut self,
        type_name: &str,
        args: &[Expr],
        ctx: &mut FunctionContext,
        lines: &mut Vec<String>,
    ) {
        let Some(layout) = self.type_layouts.get(type_name).cloned() else {
            lines.push("    ; missing type layout for constructor".to_string());
            lines.push("    mov rax, 0".to_string());
            return;
        };

        let object_bytes = (layout.field_order.len() * 8) as i64;
        let alloc_size = if object_bytes == 0 { 8 } else { object_bytes };
        lines.push(format!("    mov rdi, {}", alloc_size));
        lines.push("    call __kek_alloc".to_string());
        lines.push("    push rax".to_string());

        for (index, field_name) in layout.field_order.iter().enumerate() {
            let Some(field) = layout.fields.get(field_name) else {
                continue;
            };
            let _field_ty = &field.ty;
            if let Some(arg) = args.get(index) {
                self.emit_expr(arg, ctx, lines);
            } else {
                lines.push("    mov rax, 0".to_string());
            }
            lines.push("    mov rbx, QWORD [rsp]".to_string());
            lines.push(format!("    mov QWORD [rbx+{}], rax", field.offset));
        }

        if let Some(init_sig) = self.class_initializers.get(type_name).cloned() {
            let arg_regs = ["rsi", "rdx", "rcx", "r8", "r9"];
            for (index, arg) in args.iter().enumerate() {
                self.emit_expr(arg, ctx, lines);
                if let Some(reg) = arg_regs.get(index) {
                    lines.push(format!("    mov {}, rax", reg));
                } else {
                    lines.push("    ; initializer argument exceeds register support".to_string());
                }
            }
            lines.push("    mov rdi, QWORD [rsp]".to_string());
            lines.push(format!("    call {}", init_sig.label));
        }

        lines.push("    mov rax, QWORD [rsp]".to_string());
        lines.push("    add rsp, 8".to_string());
    }

    fn emit_enum_variant_constructor(
        &mut self,
        enum_name: &str,
        variant_name: &str,
        args: &[Expr],
        ctx: &mut FunctionContext,
        lines: &mut Vec<String>,
    ) {
        let Some(tag) = self.enum_variant_tags.get(variant_name).copied() else {
            lines.push(format!(
                "    ; unknown enum variant '{}::{}'",
                enum_name, variant_name
            ));
            lines.push("    mov rax, 0".to_string());
            return;
        };

        let payload_len = self
            .enum_variant_payloads
            .get(variant_name)
            .copied()
            .unwrap_or(args.len());

        if args.len() != payload_len {
            lines.push(format!(
                "    ; enum constructor '{}::{}' expects {} args, got {}",
                enum_name,
                variant_name,
                payload_len,
                args.len()
            ));
        }

        let alloc_size = ((payload_len + 2) * 8) as i64;
        lines.push(format!("    mov rdi, {}", alloc_size.max(16)));
        lines.push("    call __kek_alloc".to_string());
        lines.push("    push rax".to_string());
        lines.push(format!("    mov QWORD [rax], {}", tag));
        lines.push(format!("    mov QWORD [rax+8], {}", payload_len));

        for index in 0..payload_len {
            if let Some(arg) = args.get(index) {
                self.emit_expr(arg, ctx, lines);
            } else {
                lines.push("    mov rax, 0".to_string());
            }
            lines.push("    mov rbx, QWORD [rsp]".to_string());
            lines.push(format!("    mov QWORD [rbx+{}], rax", 16 + index * 8));
        }

        lines.push("    mov rax, QWORD [rsp]".to_string());
        lines.push("    add rsp, 8".to_string());
    }

    fn emit_member_call(
        &mut self,
        member: &crate::ast::MemberExpr,
        args: &[Expr],
        ctx: &mut FunctionContext,
        lines: &mut Vec<String>,
    ) {
        let receiver_type = self.infer_expr_type(member.member.as_ref(), ctx);
        self.emit_expr(member.member.as_ref(), ctx, lines);
        lines.push("    push rax".to_string());

        let arg_regs = ["rsi", "rdx", "rcx", "r8", "r9"];
        for (index, arg) in args.iter().enumerate() {
            self.emit_expr(arg, ctx, lines);
            if let Some(reg) = arg_regs.get(index) {
                lines.push(format!("    mov {}, rax", reg));
            } else {
                lines.push("    ; method argument exceeds register support".to_string());
            }
        }

        lines.push("    pop rdi".to_string());

        if let Type::Identifier(type_name) = receiver_type {
            if let Some(methods) = self.method_sigs.get(&type_name) {
                if let Some(sig) = methods.get(&member.property) {
                    if args.len() != sig.params.len() {
                        lines.push(format!(
                            "    ; method '{}.{}' expects {} args, got {}",
                            type_name,
                            member.property,
                            sig.params.len(),
                            args.len()
                        ));
                    }
                    lines.push(format!("    call {}", sig.label));
                    return;
                }
            }
            lines.push(format!(
                "    ; unknown method '{}.{}' in asm backend",
                type_name, member.property
            ));
            lines.push("    mov rax, 0".to_string());
            return;
        }

        lines.push("    ; dynamic/member call unsupported in asm backend".to_string());
        lines.push("    mov rax, 0".to_string());
    }

    fn infer_expr_type(&self, expr: &Expr, ctx: &FunctionContext) -> Type {
        match expr {
            Expr::Literal(Literal::Num(_)) => Type::Num,
            Expr::Literal(Literal::Char(_)) => Type::Char,
            Expr::Literal(Literal::Bool(_)) => Type::Bool,
            Expr::Literal(Literal::String(_)) => Type::String,
            Expr::Literal(Literal::Identifier(name)) => {
                ctx.var_types.get(name).cloned().unwrap_or(Type::None)
            }
            Expr::Literal(Literal::This) => {
                ctx.var_types.get("this").cloned().unwrap_or(Type::None)
            }
            Expr::Unary(op, right) => match op {
                Token::Not => Type::Bool,
                Token::Minus => self.infer_expr_type(right, ctx),
                Token::Question => self.infer_expr_type(right, ctx),
                _ => Type::None,
            },
            Expr::Binary(_, op, _) => match op {
                Token::Greater
                | Token::GreaterEqual
                | Token::Less
                | Token::LessEqual
                | Token::EqualEqual
                | Token::NotEqual
                | Token::And
                | Token::Or => Type::Bool,
                _ => Type::Num,
            },
            Expr::Assignment(_, right) => self.infer_expr_type(right, ctx),
            Expr::Array(array) => {
                let element_type = array
                    .array
                    .first()
                    .map(|expr| self.infer_expr_type(expr, ctx))
                    .unwrap_or(Type::None);
                Type::Array(Box::new(element_type))
            }
            Expr::Mebmer(member) => {
                let owner_type = self.infer_expr_type(member.member.as_ref(), ctx);
                if let Type::Identifier(type_name) = owner_type {
                    if let Some(layout) = self.type_layouts.get(&type_name) {
                        if let Some(field) = layout.fields.get(&member.property) {
                            return field.ty.clone();
                        }
                    }
                }
                Type::None
            }
            Expr::ComputedExpr(computed) => {
                let owner_type = self.infer_expr_type(computed.member.as_ref(), ctx);
                if let Type::Array(inner) = owner_type {
                    return (*inner).clone();
                }
                Type::None
            }
            Expr::Call(call) => match call.callee.as_ref() {
                Expr::Literal(Literal::Identifier(name)) => {
                    if let Some(return_ty) = self.function_returns.get(name) {
                        return return_ty.clone();
                    }
                    if self.type_layouts.contains_key(name) {
                        return Type::Identifier(name.clone());
                    }
                    if let Some(enum_name) = self.enum_variant_types.get(name) {
                        return Type::Identifier(enum_name.clone());
                    }
                    Type::None
                }
                Expr::Mebmer(member) => {
                    let receiver_type = self.infer_expr_type(member.member.as_ref(), ctx);
                    if let Type::Identifier(type_name) = receiver_type {
                        if let Some(methods) = self.method_sigs.get(&type_name) {
                            if let Some(sig) = methods.get(&member.property) {
                                return sig.return_type.clone();
                            }
                        }
                    }
                    Type::None
                }
                _ => Type::None,
            },
            Expr::Empty => Type::Void,
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
    var_types: HashMap<String, Type>,
    epilogue_label: String,
    loop_stack: Vec<LoopLabels>,
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
            collect_expr_locals(call.callee.as_ref(), locals);
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

fn escape_asm_string(value: &str) -> String {
    let mut escaped = String::new();
    for byte in value.bytes() {
        match byte {
            b'\\' => escaped.push_str("\\\\"),
            b'"' => escaped.push_str("\\\""),
            0x20..=0x7e => escaped.push(byte as char),
            _ => escaped.push_str(&format!("\\x{:02x}", byte)),
        }
    }
    format!("\"{}\"", escaped)
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
    fn lowers_for_loop_over_array_literal_with_runtime_iteration() {
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

        assert!(output.contains(".for_loop_"));
        assert!(output.contains("cmp rcx, QWORD [rbx]"));
        assert!(output.contains("mov rax, QWORD [rbx + rcx*8 + 8]"));
    }

    #[test]
    fn lowers_logical_and_or_with_short_circuit_control_flow() {
        let source = r#"
fun main() -> Num {
    var a: Bool = false && (1 / 0 > 0);
    var b: Bool = true || (1 / 0 > 0);
    if a {
        return 1;
    }
    if b {
        return 0;
    }
    return 2;
}
"#;

        let mut lexer = Lexer::from_source(source);
        let tokens = lexer.lex_file();
        let mut parser = Parser::new(tokens);
        let ast = parser.parse();

        let mut generator = AsmGenerator::new();
        let output = generator.generate(&ast);

        assert!(output.contains(".logic_and_false_"));
        assert!(output.contains(".logic_and_end_"));
        assert!(output.contains(".logic_or_true_"));
        assert!(output.contains(".logic_or_end_"));
        assert!(output.contains("je .logic_and_false_"));
        assert!(output.contains("jne .logic_or_true_"));
        assert!(!output.contains("and rax, rbx"));
        assert!(!output.contains("or rax, rbx"));
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
            var_types: HashMap::new(),
            epilogue_label: ".ep".to_string(),
            loop_stack: Vec::new(),
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

        assert!(lines.contains(&"    cmp r13, 0".to_string()));
        assert!(lines.contains(&"    je .match_fail".to_string()));
        assert!(lines.contains(&"    cmp QWORD [r13], 3".to_string()));
        assert!(lines.contains(&"    jne .match_fail".to_string()));
    }

    #[test]
    fn emit_pattern_guard_string_uses_interned_label_compare() {
        let mut generator = AsmGenerator::new();
        let mut lines = Vec::new();

        generator.emit_pattern_guard(
            &Pattern::Literal(Literal::String("ok".to_string())),
            "r13",
            ".match_fail",
            &mut lines,
        );

        assert!(lines
            .iter()
            .any(|line| line.starts_with("    lea r14, [rel __kek_str_")));
        assert!(lines.contains(&"    cmp r13, r14".to_string()));
        assert!(lines.contains(&"    jne .match_fail".to_string()));
    }

    #[test]
    fn emit_pattern_bindings_variant_loads_payload_values() {
        let generator = AsmGenerator::new();
        let mut lines = Vec::new();
        let mut ctx = FunctionContext {
            var_offsets: HashMap::from([("v".to_string(), 8)]),
            var_types: HashMap::from([("v".to_string(), Type::Num)]),
            epilogue_label: ".ep".to_string(),
            loop_stack: Vec::new(),
        };

        generator.emit_pattern_bindings(
            &Pattern::Variant(
                "Some".to_string(),
                vec![Pattern::Identifier("v".to_string())],
            ),
            "r13",
            &mut ctx,
            &mut lines,
        );

        assert!(lines.contains(&"    mov r14, QWORD [r13+16]".to_string()));
        assert!(lines.contains(&"    mov QWORD [rbp-8], r14".to_string()));
    }

    #[test]
    fn emit_enum_variant_constructor_allocates_payload_object() {
        let mut generator = AsmGenerator::new();
        generator.enum_variant_tags.insert("Some".to_string(), 0);
        generator
            .enum_variant_payloads
            .insert("Some".to_string(), 1);
        let mut lines = Vec::new();
        let mut ctx = FunctionContext {
            var_offsets: HashMap::new(),
            var_types: HashMap::new(),
            epilogue_label: ".ep".to_string(),
            loop_stack: Vec::new(),
        };

        generator.emit_enum_variant_constructor(
            "Maybe",
            "Some",
            &[Expr::Literal(Literal::Num(7.0))],
            &mut ctx,
            &mut lines,
        );

        assert!(lines.contains(&"    mov rdi, 24".to_string()));
        assert!(lines.contains(&"    call __kek_alloc".to_string()));
        assert!(lines.contains(&"    mov QWORD [rax], 0".to_string()));
        assert!(lines.contains(&"    mov QWORD [rax+8], 1".to_string()));
        assert!(lines.contains(&"    mov QWORD [rbx+16], rax".to_string()));
    }

    #[test]
    fn emit_match_binds_identifier_pattern_to_stack_slot() {
        let mut generator = AsmGenerator::new();
        let mut lines = Vec::new();
        let mut ctx = FunctionContext {
            var_offsets: HashMap::from([("bound".to_string(), 8)]),
            var_types: HashMap::from([("bound".to_string(), Type::Num)]),
            epilogue_label: ".ep".to_string(),
            loop_stack: Vec::new(),
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
