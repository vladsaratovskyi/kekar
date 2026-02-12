use std::collections::HashSet;

use crate::{
    ast::{
        BlockStmt, ConstStmt, EnumStmt, Expr, ForStmt, FunStmt, IfStmt, ImplStmt, ImportStmt,
        Literal, MatchStmt, ModStmt, Pattern, Stmt, StructStmt, UseStmt, VarStmt, WhileStmt,
    },
    lexer::Token,
};

pub struct JsGenerator;

impl JsGenerator {
    pub fn new() -> Self {
        Self
    }

    pub fn generate(&self, program: &BlockStmt) -> String {
        let mut emitter = Emitter::new();

        for stmt in &program.stmts {
            emitter.emit_stmt(stmt, false);
        }

        emitter.finish()
    }
}

struct Emitter {
    out: String,
    indent: usize,
    match_counter: usize,
}

impl Emitter {
    fn new() -> Self {
        Self {
            out: String::new(),
            indent: 0,
            match_counter: 0,
        }
    }

    fn finish(self) -> String {
        self.out
    }

    fn line(&mut self, text: &str) {
        if text.is_empty() {
            self.out.push('\n');
            return;
        }

        self.out.push_str(&"    ".repeat(self.indent));
        self.out.push_str(text);
        self.out.push('\n');
    }

    fn emit_stmt(&mut self, stmt: &Stmt, in_class: bool) {
        match stmt {
            Stmt::Pub(pub_stmt) => self.emit_stmt(pub_stmt.stmt.as_ref(), in_class),
            Stmt::Mod(mod_stmt) => self.emit_mod(mod_stmt),
            Stmt::Use(use_stmt) => self.emit_use(use_stmt),
            Stmt::Import(import_stmt) => self.emit_import(import_stmt),
            Stmt::Struct(struct_stmt) => self.emit_struct(struct_stmt),
            Stmt::Enum(enum_stmt) => self.emit_enum(enum_stmt),
            Stmt::Impl(impl_stmt) => self.emit_impl(impl_stmt),
            Stmt::Class(class_stmt) => {
                self.line(&format!("class {} {{", class_stmt.name));
                self.indent += 1;

                if let Stmt::Block(block) = class_stmt.block.as_ref() {
                    for member in &block.stmts {
                        self.emit_stmt(member, true);
                    }
                }

                self.indent -= 1;
                self.line("}");
            }
            Stmt::Fun(fun_stmt) => self.emit_fun(fun_stmt, in_class),
            Stmt::Var(var_stmt) => self.emit_var(var_stmt, in_class),
            Stmt::Const(const_stmt) => self.emit_const(const_stmt),
            Stmt::If(if_stmt) => self.emit_if(if_stmt, in_class),
            Stmt::Match(match_stmt) => self.emit_match(match_stmt, in_class),
            Stmt::While(while_stmt) => self.emit_while(while_stmt, in_class),
            Stmt::For(for_stmt) => self.emit_for(for_stmt, in_class),
            Stmt::Break(_) => self.line("break;"),
            Stmt::Continue(_) => self.line("continue;"),
            Stmt::Return(return_stmt) => {
                if matches!(return_stmt.return_expr, Expr::Empty) {
                    self.line("return;");
                } else {
                    self.line(&format!(
                        "return {};",
                        self.expr_to_js(&return_stmt.return_expr)
                    ));
                }
            }
            Stmt::Expr(expr_stmt) => {
                if !matches!(expr_stmt.expr, Expr::Empty) {
                    self.line(&format!("{};", self.expr_to_js(&expr_stmt.expr)));
                }
            }
            Stmt::Block(block) => self.emit_block(block, in_class),
            Stmt::Empty => {}
        }
    }

    fn emit_import(&mut self, import_stmt: &ImportStmt) {
        if let Some(alias) = &import_stmt.alias {
            self.line(&format!(
                "import {} as {} from \"{}\";",
                import_stmt.import, alias, import_stmt.from
            ));
        } else {
            self.line(&format!(
                "import {} from \"{}\";",
                import_stmt.import, import_stmt.from
            ));
        }
    }

    fn emit_mod(&mut self, mod_stmt: &ModStmt) {
        self.line(&format!("// mod {};", mod_stmt.name));
    }

    fn emit_use(&mut self, use_stmt: &UseStmt) {
        self.line(&format!("// use {};", use_stmt.path));
    }

    fn emit_struct(&mut self, struct_stmt: &StructStmt) {
        self.line(&format!("class {} {{", struct_stmt.name));
        self.indent += 1;

        let params = struct_stmt
            .fields
            .iter()
            .map(|field| field.name.clone())
            .collect::<Vec<_>>()
            .join(", ");

        self.line(&format!("constructor({}) {{", params));
        self.indent += 1;
        for field in &struct_stmt.fields {
            self.line(&format!("this.{0} = {0};", field.name));
        }
        self.indent -= 1;
        self.line("}");

        self.indent -= 1;
        self.line("}");
    }

    fn emit_enum(&mut self, enum_stmt: &EnumStmt) {
        self.line(&format!("const {} = Object.freeze({{", enum_stmt.name));
        self.indent += 1;
        let mut aliases = Vec::new();
        for variant in &enum_stmt.variants {
            let arg_names = (0..variant.arguments.len())
                .map(|index| format!("arg{}", index))
                .collect::<Vec<_>>();
            let params = if arg_names.is_empty() {
                String::new()
            } else {
                arg_names.join(", ")
            };
            let args_value = if arg_names.is_empty() {
                "[]".to_string()
            } else {
                format!("[{}]", arg_names.join(", "))
            };
            self.line(&format!(
                "{}: ({}) => ({{ __enum: \"{}\", tag: \"{}\", args: {} }}),",
                variant.name, params, enum_stmt.name, variant.name, args_value
            ));
            aliases.push(format!(
                "const {} = {}.{};",
                variant.name, enum_stmt.name, variant.name
            ));
        }
        self.indent -= 1;
        self.line("});");
        for alias in aliases {
            self.line(&alias);
        }
    }

    fn emit_impl(&mut self, impl_stmt: &ImplStmt) {
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

            let params = fun_stmt
                .params
                .iter()
                .map(|p| p.name.clone())
                .collect::<Vec<_>>()
                .join(", ");

            self.line(&format!(
                "{}.prototype.{} = function({}) {{",
                impl_stmt.name, fun_stmt.name, params
            ));
            self.indent += 1;
            if let Stmt::Block(block) = fun_stmt.block.as_ref() {
                for stmt in &block.stmts {
                    self.emit_stmt(stmt, false);
                }
            }
            self.indent -= 1;
            self.line("};");
        }
    }

    fn emit_fun(&mut self, fun_stmt: &FunStmt, in_class: bool) {
        let params = fun_stmt
            .params
            .iter()
            .map(|p| p.name.clone())
            .collect::<Vec<_>>()
            .join(", ");

        if in_class {
            self.line(&format!("{}({}) {{", fun_stmt.name, params));
        } else {
            self.line(&format!("function {}({}) {{", fun_stmt.name, params));
        }

        self.indent += 1;
        if let Stmt::Block(block) = fun_stmt.block.as_ref() {
            for stmt in &block.stmts {
                self.emit_stmt(stmt, false);
            }
        }
        self.indent -= 1;

        self.line("}");
    }

    fn emit_var(&mut self, var_stmt: &VarStmt, in_class: bool) {
        match (&var_stmt.assignment, in_class) {
            (Expr::Empty, true) => self.line(&format!("{};", var_stmt.name)),
            (Expr::Empty, false) => self.line(&format!("let {};", var_stmt.name)),
            (_, true) => self.line(&format!(
                "{} = {};",
                var_stmt.name,
                self.expr_to_js(&var_stmt.assignment)
            )),
            (_, false) => self.line(&format!(
                "let {} = {};",
                var_stmt.name,
                self.expr_to_js(&var_stmt.assignment)
            )),
        }
    }

    fn emit_const(&mut self, const_stmt: &ConstStmt) {
        self.line(&format!(
            "const {} = {};",
            const_stmt.name,
            self.expr_to_js(&const_stmt.assignment)
        ));
    }

    fn emit_if(&mut self, if_stmt: &IfStmt, in_class: bool) {
        self.line(&format!("if ({}) {{", self.expr_to_js(&if_stmt.condition)));
        self.indent += 1;
        self.emit_stmt_block_contents(&if_stmt.then_block, in_class);
        self.indent -= 1;

        if matches!(if_stmt.else_block.as_ref(), Stmt::Empty) {
            self.line("}");
            return;
        }

        self.line("} else {");
        self.indent += 1;
        self.emit_stmt_block_contents(&if_stmt.else_block, in_class);
        self.indent -= 1;
        self.line("}");
    }

    fn emit_match(&mut self, match_stmt: &MatchStmt, in_class: bool) {
        let match_id = self.match_counter;
        self.match_counter += 1;

        let value_name = format!("__kek_match_value_{}", match_id);
        let matched_name = format!("__kek_match_done_{}", match_id);

        self.line("{");
        self.indent += 1;
        self.line(&format!(
            "const {} = {};",
            value_name,
            self.expr_to_js(&match_stmt.expr)
        ));
        self.line(&format!("let {} = false;", matched_name));

        for (index, arm) in match_stmt.arms.iter().enumerate() {
            let mut bindings = Vec::new();
            let condition = self.pattern_condition_to_js(&arm.pattern, &value_name, &mut bindings);

            if index == 0 {
                self.line(&format!("if (!{} && ({})) {{", matched_name, condition));
            } else {
                self.line(&format!(
                    "else if (!{} && ({})) {{",
                    matched_name, condition
                ));
            }
            self.indent += 1;
            self.line(&format!("{} = true;", matched_name));
            let mut declared = HashSet::new();
            for (name, expr) in bindings {
                if declared.insert(name.clone()) {
                    self.line(&format!("const {} = {};", name, expr));
                }
            }
            self.emit_stmt_block_contents(arm.body.as_ref(), in_class);
            self.indent -= 1;
            self.line("}");
        }

        self.line(&format!("if (!{}) {{", matched_name));
        self.indent += 1;
        self.line("throw new Error(\"Non-exhaustive match\");");
        self.indent -= 1;
        self.line("}");
        self.indent -= 1;
        self.line("}");
    }

    fn emit_while(&mut self, while_stmt: &WhileStmt, in_class: bool) {
        self.line(&format!(
            "while ({}) {{",
            self.expr_to_js(&while_stmt.condition)
        ));
        self.indent += 1;
        self.emit_stmt_block_contents(&while_stmt.body, in_class);
        self.indent -= 1;
        self.line("}");
    }

    fn emit_for(&mut self, for_stmt: &ForStmt, in_class: bool) {
        if let Some(index_name) = &for_stmt.index {
            self.line(&format!(
                "for (const [{}, {}] of {}.entries()) {{",
                index_name,
                for_stmt.item,
                self.expr_to_js(&for_stmt.iterator)
            ));
        } else {
            self.line(&format!(
                "for (const {} of {}) {{",
                for_stmt.item,
                self.expr_to_js(&for_stmt.iterator)
            ));
        }

        self.indent += 1;
        self.emit_stmt_block_contents(&for_stmt.body, in_class);
        self.indent -= 1;
        self.line("}");
    }

    fn emit_block(&mut self, block: &BlockStmt, in_class: bool) {
        self.line("{");
        self.indent += 1;
        for stmt in &block.stmts {
            self.emit_stmt(stmt, in_class);
        }
        self.indent -= 1;
        self.line("}");
    }

    fn emit_stmt_block_contents(&mut self, stmt: &Stmt, in_class: bool) {
        match stmt {
            Stmt::Block(block) => {
                for child in &block.stmts {
                    self.emit_stmt(child, in_class);
                }
            }
            _ => self.emit_stmt(stmt, in_class),
        }
    }

    fn expr_to_js(&self, expr: &Expr) -> String {
        match expr {
            Expr::Unary(op, right) => format!("({}{})", self.op_to_js(op), self.expr_to_js(right)),
            Expr::Binary(left, op, right) => format!(
                "({} {} {})",
                self.expr_to_js(left),
                self.op_to_js(op),
                self.expr_to_js(right)
            ),
            Expr::Literal(literal) => self.literal_to_js(literal),
            Expr::Assignment(left, right) => {
                format!("{} = {}", self.expr_to_js(left), self.expr_to_js(right))
            }
            Expr::Call(call) => {
                let args = call
                    .arguments
                    .iter()
                    .map(|arg| self.expr_to_js(arg))
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("{}({})", self.expr_to_js(call.callee.as_ref()), args)
            }
            Expr::Mebmer(member) => {
                format!("{}.{}", self.expr_to_js(&member.member), member.property)
            }
            Expr::ComputedExpr(computed) => format!(
                "{}[{}]",
                self.expr_to_js(&computed.member),
                self.expr_to_js(&computed.property)
            ),
            Expr::Array(array) => {
                let values = array
                    .array
                    .iter()
                    .map(|value| self.expr_to_js(value))
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("[{}]", values)
            }
            Expr::Empty => String::new(),
        }
    }

    fn literal_to_js(&self, literal: &Literal) -> String {
        match literal {
            Literal::String(s) => format!("\"{}\"", escape_js_string(s)),
            Literal::Char(c) => format!("'{}'", escape_js_char(*c)),
            Literal::Num(n) => {
                if n.fract() == 0.0 {
                    format!("{:.0}", n)
                } else {
                    n.to_string()
                }
            }
            Literal::Bool(v) => v.to_string(),
            Literal::Identifier(id) => id.to_string(),
            Literal::This => "this".to_string(),
        }
    }

    fn op_to_js(&self, token: &Token) -> &'static str {
        match token {
            Token::Plus => "+",
            Token::Minus => "-",
            Token::Star => "*",
            Token::Slash => "/",
            Token::Percent => "%",
            Token::NotEqual => "!=",
            Token::EqualEqual => "==",
            Token::Greater => ">",
            Token::GreaterEqual => ">=",
            Token::Less => "<",
            Token::LessEqual => "<=",
            Token::And => "&&",
            Token::Or => "||",
            Token::Not => "!",
            _ => "",
        }
    }

    #[allow(dead_code)]
    fn pattern_to_js(&self, pattern: &Pattern) -> String {
        match pattern {
            Pattern::Wildcard => "_".to_string(),
            Pattern::Literal(lit) => self.literal_to_js(lit),
            Pattern::Identifier(id) => id.clone(),
            Pattern::Variant(name, patterns) => {
                let inner = patterns
                    .iter()
                    .map(|p| self.pattern_to_js(p))
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("{}({})", name, inner)
            }
        }
    }

    fn pattern_condition_to_js(
        &self,
        pattern: &Pattern,
        value_expr: &str,
        bindings: &mut Vec<(String, String)>,
    ) -> String {
        match pattern {
            Pattern::Wildcard => "true".to_string(),
            Pattern::Literal(literal) => {
                format!("{} === {}", value_expr, self.literal_to_js(literal))
            }
            Pattern::Identifier(name) => {
                bindings.push((name.clone(), value_expr.to_string()));
                "true".to_string()
            }
            Pattern::Variant(name, nested) => {
                let mut parts = vec![
                    format!("{} !== null", value_expr),
                    format!("typeof {} === \"object\"", value_expr),
                    format!("{}.tag === \"{}\"", value_expr, name),
                    format!("Array.isArray({}.args)", value_expr),
                    format!("{}.args.length === {}", value_expr, nested.len()),
                ];

                for (index, nested_pattern) in nested.iter().enumerate() {
                    let nested_value = format!("{}.args[{}]", value_expr, index);
                    parts.push(self.pattern_condition_to_js(
                        nested_pattern,
                        &nested_value,
                        bindings,
                    ));
                }

                parts.join(" && ")
            }
        }
    }
}

fn escape_js_string(input: &str) -> String {
    input
        .replace('\\', "\\\\")
        .replace('"', "\\\"")
        .replace('\n', "\\n")
        .replace('\r', "\\r")
        .replace('\t', "\\t")
}

fn escape_js_char(input: char) -> String {
    match input {
        '\\' => "\\\\".to_string(),
        '\'' => "\\'".to_string(),
        '\n' => "\\n".to_string(),
        '\r' => "\\r".to_string(),
        '\t' => "\\t".to_string(),
        c => c.to_string(),
    }
}

#[cfg(test)]
mod tests {
    use super::{escape_js_char, escape_js_string, Emitter, JsGenerator};
    use crate::{
        ast::{
            BlockStmt, ClassStmt, EnumStmt, EnumVariant, Expr, FunStmt, ImplStmt, ImportStmt,
            Literal, MatchArm, MatchStmt, MemberExpr, Param, Pattern, ReturnStmt, Stmt, Type,
            VarStmt,
        },
        lexer::Token,
    };

    #[test]
    fn generates_simple_program() {
        let program = BlockStmt {
            stmts: vec![
                Stmt::Import(ImportStmt {
                    import: "System".to_string(),
                    from: "../src/system.kek".to_string(),
                    alias: None,
                }),
                Stmt::Class(ClassStmt {
                    name: "Person".to_string(),
                    block: Box::new(Stmt::Block(BlockStmt {
                        stmts: vec![
                            Stmt::Var(VarStmt {
                                name: "name".to_string(),
                                assignment: Expr::Empty,
                                var_type: Type::String,
                            }),
                            Stmt::Fun(FunStmt {
                                name: "init".to_string(),
                                return_type: Type::None,
                                params: vec![Param {
                                    name: "value".to_string(),
                                    param_type: Type::String,
                                }],
                                block: Box::new(Stmt::Block(BlockStmt {
                                    stmts: vec![Stmt::Return(ReturnStmt {
                                        return_expr: Expr::Literal(Literal::Identifier(
                                            "value".to_string(),
                                        )),
                                    })],
                                })),
                            }),
                        ],
                    })),
                }),
            ],
        };

        let output = JsGenerator::new().generate(&program);

        assert!(output.contains("import System from \"../src/system.kek\";"));
        assert!(output.contains("class Person {"));
        assert!(output.contains("name;"));
        assert!(output.contains("init(value) {"));
        assert!(output.contains("return value;"));
    }

    #[test]
    fn escapes_js_strings() {
        let escaped = escape_js_string("line1\n\"quoted\"\t\\");
        assert_eq!(escaped, "line1\\n\\\"quoted\\\"\\t\\\\");
    }

    #[test]
    fn escapes_js_chars() {
        assert_eq!(escape_js_char('\n'), "\\n");
        assert_eq!(escape_js_char('\''), "\\'");
        assert_eq!(escape_js_char('a'), "a");
    }

    #[test]
    fn maps_supported_operators_and_unknown() {
        let emitter = Emitter::new();
        assert_eq!(emitter.op_to_js(&Token::Plus), "+");
        assert_eq!(emitter.op_to_js(&Token::And), "&&");
        assert_eq!(emitter.op_to_js(&Token::EqualEqual), "==");
        assert_eq!(emitter.op_to_js(&Token::Semicolon), "");
    }

    #[test]
    fn renders_nested_expression_to_js() {
        let emitter = Emitter::new();
        let expr = Expr::Assignment(
            Box::new(Expr::Mebmer(MemberExpr {
                member: Box::new(Expr::Literal(Literal::This)),
                property: "value".to_string(),
            })),
            Box::new(Expr::Binary(
                Box::new(Expr::Literal(Literal::Num(2.0))),
                Token::Star,
                Box::new(Expr::Literal(Literal::Num(3.0))),
            )),
        );

        assert_eq!(emitter.expr_to_js(&expr), "this.value = (2 * 3)");
    }

    #[test]
    fn emits_var_differently_for_class_and_function_scope() {
        let mut emitter = Emitter::new();
        let var_stmt = VarStmt {
            name: "count".to_string(),
            assignment: Expr::Empty,
            var_type: Type::Num,
        };

        emitter.emit_var(&var_stmt, false);
        emitter.emit_var(&var_stmt, true);

        assert_eq!(emitter.finish(), "let count;\ncount;\n");
    }

    #[test]
    fn pattern_condition_variant_binds_nested_value_and_checks_tag() {
        let emitter = Emitter::new();
        let mut bindings = Vec::new();

        let condition = emitter.pattern_condition_to_js(
            &Pattern::Variant(
                "Some".to_string(),
                vec![
                    Pattern::Identifier("value".to_string()),
                    Pattern::Literal(Literal::Num(1.0)),
                ],
            ),
            "__candidate",
            &mut bindings,
        );

        assert!(condition.contains("__candidate.tag === \"Some\""));
        assert!(condition.contains("__candidate.args.length === 2"));
        assert!(condition.contains("__candidate.args[1] === 1"));
        assert_eq!(
            bindings,
            vec![("value".to_string(), "__candidate.args[0]".to_string())]
        );
    }

    #[test]
    fn emit_enum_and_impl_create_runtime_artifacts() {
        let mut emitter = Emitter::new();

        emitter.emit_stmt(
            &Stmt::Enum(EnumStmt {
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
            }),
            false,
        );

        emitter.emit_stmt(
            &Stmt::Impl(ImplStmt {
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
            }),
            false,
        );

        let output = emitter.finish();

        assert!(output.contains("const Maybe = Object.freeze({"));
        assert!(output.contains("const Some = Maybe.Some;"));
        assert!(output.contains("const Empty = Maybe.Empty;"));
        assert!(output.contains("Point.prototype.value = function() {"));
    }

    #[test]
    fn emit_match_lowers_to_if_else_chain() {
        let mut emitter = Emitter::new();
        emitter.emit_stmt(
            &Stmt::Match(MatchStmt {
                expr: Expr::Literal(Literal::Identifier("x".to_string())),
                arms: vec![
                    MatchArm {
                        pattern: Pattern::Literal(Literal::Num(1.0)),
                        body: Box::new(Stmt::Block(BlockStmt {
                            stmts: vec![Stmt::Return(ReturnStmt {
                                return_expr: Expr::Literal(Literal::Num(10.0)),
                            })],
                        })),
                    },
                    MatchArm {
                        pattern: Pattern::Wildcard,
                        body: Box::new(Stmt::Block(BlockStmt {
                            stmts: vec![Stmt::Return(ReturnStmt {
                                return_expr: Expr::Literal(Literal::Num(0.0)),
                            })],
                        })),
                    },
                ],
            }),
            false,
        );

        let output = emitter.finish();
        assert!(output.contains("const __kek_match_value_0 = x;"));
        assert!(output.contains("if (!__kek_match_done_0 && (__kek_match_value_0 === 1)) {"));
        assert!(output.contains("else if (!__kek_match_done_0 && (true)) {"));
    }
}
