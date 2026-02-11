use crate::{
    ast::{BlockStmt, Expr, ForStmt, FunStmt, IfStmt, ImportStmt, Literal, Stmt, VarStmt},
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
}

impl Emitter {
    fn new() -> Self {
        Self {
            out: String::new(),
            indent: 0,
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
            Stmt::Import(import_stmt) => self.emit_import(import_stmt),
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
            Stmt::If(if_stmt) => self.emit_if(if_stmt, in_class),
            Stmt::For(for_stmt) => self.emit_for(for_stmt, in_class),
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
        self.line(&format!(
            "import {} from \"{}\";",
            import_stmt.import, import_stmt.from
        ));
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
                format!("{}({})", call.method_name, args)
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
}

fn escape_js_string(input: &str) -> String {
    input
        .replace('\\', "\\\\")
        .replace('"', "\\\"")
        .replace('\n', "\\n")
        .replace('\r', "\\r")
        .replace('\t', "\\t")
}

#[cfg(test)]
mod tests {
    use super::{escape_js_string, Emitter, JsGenerator};
    use crate::{
        ast::{
            BlockStmt, ClassStmt, Expr, FunStmt, ImportStmt, Literal, MemberExpr, Param,
            ReturnStmt, Stmt, Type, VarStmt,
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
}
