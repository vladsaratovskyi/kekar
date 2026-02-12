#![allow(unused)]
use crate::{ast::*, lexer::Token};
use std::any::Any;

#[derive(Debug, Clone, PartialEq, PartialOrd)]
enum Binding {
    Def = 0,
    Coma,
    Assign,
    Logic,
    Bitwise,
    Relation,
    Shift,
    Add,
    Mult,
    Unary,
    Call,
    Member,
    Primary,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CompatibilityWarningKind {
    LegacyParamStyle,
    LegacyReturnStyle,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CompatibilityDiagnostic {
    pub kind: CompatibilityWarningKind,
    pub message: String,
    pub hint: String,
}

impl CompatibilityDiagnostic {
    fn legacy_param_style(param_name: &str, param_type: &Type) -> Self {
        let canonical_type = format_type_for_hint(param_type);
        Self {
            kind: CompatibilityWarningKind::LegacyParamStyle,
            message: format!(
                "Legacy parameter syntax 'Type name' is deprecated for '{}'",
                param_name
            ),
            hint: format!("Use canonical syntax '{}: {}'", param_name, canonical_type),
        }
    }

    fn legacy_return_style(fun_name: &str, return_type: &Type) -> Self {
        let canonical_type = format_type_for_hint(return_type);
        Self {
            kind: CompatibilityWarningKind::LegacyReturnStyle,
            message: format!(
                "Legacy return syntax ': Type' is deprecated for function '{}'",
                fun_name
            ),
            hint: format!(
                "Use canonical syntax '-> {}' for '{}'",
                canonical_type, fun_name
            ),
        }
    }
}

fn format_type_for_hint(ty: &Type) -> String {
    match ty {
        Type::Num => "Num".to_string(),
        Type::Char => "Char".to_string(),
        Type::Byte => "Byte".to_string(),
        Type::String => "String".to_string(),
        Type::Bool => "Bool".to_string(),
        Type::Void => "Void".to_string(),
        Type::Identifier(name) => name.clone(),
        Type::Array(inner) => format!("{}[]", format_type_for_hint(inner)),
        Type::None => "None".to_string(),
    }
}

pub fn render_compatibility_diagnostic(diag: &CompatibilityDiagnostic) -> String {
    let (code, label) = match diag.kind {
        CompatibilityWarningKind::LegacyParamStyle => ("KEK-COMPAT-001", "legacy parameter syntax"),
        CompatibilityWarningKind::LegacyReturnStyle => ("KEK-COMPAT-002", "legacy return syntax"),
    };

    format!(
        "[warning][{}] {}. Hint: {}. Compatibility window ends 2026-09-30.",
        code, label, diag.hint
    )
}

#[derive(Debug, Clone, PartialEq)]
pub struct ParseError {
    pub message: String,
    pub token_index: usize,
    pub token: Option<Token>,
}

impl ParseError {
    fn new(message: impl Into<String>, token_index: usize, token: Option<Token>) -> Self {
        Self {
            message: message.into(),
            token_index,
            token,
        }
    }
}

pub struct Parser {
    tokens: Vec<Token>,
    //errors: Vec<ParseError>,
    current: usize,
    compatibility_diagnostics: Vec<CompatibilityDiagnostic>,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Parser {
        Parser {
            tokens,
            //errors: Vec::new(),
            current: 0,
            compatibility_diagnostics: Vec::new(),
        }
    }

    pub fn compatibility_diagnostics(&self) -> &[CompatibilityDiagnostic] {
        &self.compatibility_diagnostics
    }

    pub fn take_compatibility_diagnostics(&mut self) -> Vec<CompatibilityDiagnostic> {
        std::mem::take(&mut self.compatibility_diagnostics)
    }

    pub fn parse(&mut self) -> BlockStmt {
        let mut stmts = Vec::new();

        while self.has_tokens() {
            stmts.push(self.parse_stmt());
        }

        BlockStmt { stmts }
    }

    pub fn parse_checked(&mut self) -> Result<BlockStmt, Vec<ParseError>> {
        let parse_result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| self.parse()));
        match parse_result {
            Ok(ast) => Ok(ast),
            Err(payload) => Err(vec![self.panic_payload_to_error(payload)]),
        }
    }

    fn panic_payload_to_error(&self, payload: Box<dyn Any + Send>) -> ParseError {
        let message = if let Some(msg) = payload.downcast_ref::<String>() {
            msg.clone()
        } else if let Some(msg) = payload.downcast_ref::<&str>() {
            (*msg).to_string()
        } else {
            "Unknown parser failure".to_string()
        };

        let token_index = self.current.min(self.tokens.len().saturating_sub(1));
        let token = self.tokens.get(token_index).cloned();
        ParseError::new(message, token_index, token)
    }

    fn current_token(&self) -> &Token {
        &self.tokens[self.current]
    }

    fn get_token_and_move(&mut self) -> &Token {
        let token = &self.tokens[self.current];
        //println!("{}", token);
        self.current += 1;
        token
    }

    fn has_tokens(&self) -> bool {
        self.current < self.tokens.len() && self.tokens[self.current] != Token::Eof
    }

    fn expect(&mut self, expected: &Token) -> &Token {
        let current = self.current_token();

        if std::mem::discriminant(current) != std::mem::discriminant(expected) {
            panic!("Expected {} but found {}", expected, current);
        }

        self.get_token_and_move()
    }

    fn expect_identifier_get_name(&mut self) -> String {
        match self.expect(&Token::Identifier("any".to_string())) {
            Token::Identifier(s) => s.to_string(),
            _ => panic!("Token has no value"),
        }
    }

    fn is_next_token(&self, expected: &Token) -> bool {
        if self.current + 1 >= self.tokens.len() {
            return false;
        }

        std::mem::discriminant(&self.tokens[self.current + 1]) == std::mem::discriminant(expected)
    }

    fn parse_stmt(&mut self) -> Stmt {
        let stmt = self.handle_stmt();

        match stmt {
            Some(s) => s,
            None => self.parse_exrp_stmt(),
        }
    }

    fn parse_exrp_stmt(&mut self) -> Stmt {
        let expr = self.parse_expr(Binding::Def);
        self.expect(&Token::Semicolon);

        Stmt::Expr(ExprStmt { expr })
    }

    fn parse_expr(&mut self, binding: Binding) -> Expr {
        let mut left = self
            .handle_nud()
            .unwrap_or_else(|| panic!("Unable to parse literal {}", self.current_token()));

        while self.get_current_token_power() > binding {
            left = self.handle_led(left);
        }

        left
    }

    fn parse_block_stmt(&mut self) -> Stmt {
        self.expect(&Token::LeftBracket);

        let mut body = Vec::new();

        while self.has_tokens() && self.current_token() != &Token::RightBracket {
            //TODO refactor check
            if self.current_token() == &Token::Semicolon {
                self.get_token_and_move();

                if self.current_token() == &Token::RightBracket {
                    break;
                }
            }

            body.push(self.parse_stmt());
        }

        self.expect(&Token::RightBracket);

        Stmt::Block(BlockStmt { stmts: body })
    }

    fn parse_primary_expr(&mut self) -> Expr {
        match self.get_token_and_move() {
            Token::Number(n) => Expr::Literal(Literal::Num(*n)),
            Token::True => Expr::Literal(Literal::Bool(true)),
            Token::False => Expr::Literal(Literal::Bool(false)),
            Token::None => Expr::Literal(Literal::Identifier("None".to_string())),
            Token::String(s) => Expr::Literal(Literal::String(s.to_string())),
            Token::Char(c) => Expr::Literal(Literal::Char(*c)),
            Token::Identifier(i) => Expr::Literal(Literal::Identifier(i.to_string())),
            Token::This => Expr::Literal(Literal::This),
            _ => panic!(
                "No expression found for literal token {}",
                self.current_token()
            ),
        }
    }

    fn parse_binary_expr(&mut self, left: Expr) -> Expr {
        let binding = self.get_current_token_power();
        let operator = self.get_token_and_move().clone();
        Expr::Binary(Box::new(left), operator, Box::new(self.parse_expr(binding)))
    }

    fn parse_prefix_expr(&mut self) -> Expr {
        let operator = self.get_token_and_move();
        Expr::Unary(operator.clone(), Box::new(self.parse_expr(Binding::Unary)))
    }

    fn parse_var_stmt(&mut self) -> Stmt {
        self.expect(&Token::Var);
        let name = self.expect_identifier_get_name();
        let mut field_type = Type::None;

        if self.current_token() == &Token::Colon {
            self.expect(&Token::Colon);
            field_type = self.parse_type();
        }

        let mut assignment = Expr::Empty;
        if self.current_token() != &Token::Semicolon {
            self.expect(&Token::Equal);
            assignment = self.parse_expr(Binding::Assign);
        }

        self.expect(&Token::Semicolon);

        Stmt::Var(VarStmt {
            name,
            assignment,
            var_type: field_type,
        })
    }

    fn parse_const_stmt(&mut self) -> Stmt {
        self.expect(&Token::Const);
        let name = self.expect_identifier_get_name();
        let mut const_type = Type::None;

        if self.current_token() == &Token::Colon {
            self.expect(&Token::Colon);
            const_type = self.parse_type();
        }

        self.expect(&Token::Equal);
        let assignment = self.parse_expr(Binding::Assign);

        self.expect(&Token::Semicolon);

        Stmt::Const(ConstStmt {
            name,
            assignment,
            const_type,
        })
    }

    fn parse_mod_stmt(&mut self) -> Stmt {
        self.expect(&Token::Mod);
        let name = self.expect_identifier_get_name();
        self.expect(&Token::Semicolon);
        Stmt::Mod(ModStmt { name })
    }

    fn parse_use_stmt(&mut self) -> Stmt {
        self.expect(&Token::Use);
        let mut parts = vec![self.expect_identifier_get_name()];

        while self.current_token() == &Token::ColonColon {
            self.expect(&Token::ColonColon);
            parts.push(self.expect_identifier_get_name());
        }

        self.expect(&Token::Semicolon);
        Stmt::Use(UseStmt {
            path: parts.join("::"),
        })
    }

    fn parse_struct_stmt(&mut self) -> Stmt {
        self.expect(&Token::Struct);
        let name = self.expect_identifier_get_name();
        self.expect(&Token::LeftBracket);

        let mut fields = Vec::new();
        let mut methods = Vec::new();
        while self.has_tokens() && self.current_token() != &Token::RightBracket {
            if self.current_token() == &Token::Semicolon {
                self.get_token_and_move();
                continue;
            }

            match self.current_token() {
                Token::Fun => {
                    methods.push(self.parse_fun_stmt());
                }
                Token::Pub => {
                    let method = self.parse_pub_stmt();
                    let valid = matches!(&method, Stmt::Pub(pub_stmt) if matches!(pub_stmt.stmt.as_ref(), Stmt::Fun(_)));
                    if !valid {
                        panic!(
                            "Struct blocks can contain only field declarations and function methods"
                        );
                    }
                    methods.push(method);
                }
                Token::Identifier(_) => {
                    let field_name = self.expect_identifier_get_name();
                    self.expect(&Token::Colon);
                    let field_type = self.parse_type();
                    self.expect(&Token::Semicolon);

                    fields.push(FieldDecl {
                        name: field_name,
                        field_type,
                    });
                }
                _ => {
                    panic!(
                        "Struct blocks can contain only field declarations and method declarations"
                    )
                }
            }
        }

        self.expect(&Token::RightBracket);
        Stmt::Struct(StructStmt {
            name,
            fields,
            methods,
        })
    }

    fn parse_enum_stmt(&mut self) -> Stmt {
        self.expect(&Token::Enum);
        let name = self.expect_identifier_get_name();
        self.expect(&Token::LeftBracket);

        let mut variants = Vec::new();
        while self.has_tokens() && self.current_token() != &Token::RightBracket {
            if self.current_token() == &Token::Coma {
                self.expect(&Token::Coma);
                continue;
            }

            let variant_name = self.expect_identifier_get_name();
            let mut arguments = Vec::new();

            if self.current_token() == &Token::LeftParen {
                self.expect(&Token::LeftParen);
                while self.has_tokens() && self.current_token() != &Token::RightParen {
                    arguments.push(self.parse_type());

                    if !matches!(self.current_token(), &Token::RightParen | &Token::Eof) {
                        self.expect(&Token::Coma);
                    }
                }
                self.expect(&Token::RightParen);
            }

            variants.push(EnumVariant {
                name: variant_name,
                arguments,
            });

            if self.current_token() == &Token::Coma {
                self.expect(&Token::Coma);
            }
        }

        self.expect(&Token::RightBracket);
        Stmt::Enum(EnumStmt { name, variants })
    }

    fn parse_assignment_exrp(&mut self, left: Expr) -> Expr {
        let binding = self.get_current_token_power();
        self.get_token_and_move();

        let right = self.parse_expr(binding);

        Expr::Assignment(Box::new(left), Box::new(right))
    }

    fn parse_try_expr(&mut self, left: Expr) -> Expr {
        self.expect(&Token::Question);
        Expr::Unary(Token::Question, Box::new(left))
    }

    fn parse_if_stmt(&mut self) -> Stmt {
        self.get_token_and_move();
        let condition = self.parse_expr(Binding::Assign);
        let main = self.parse_block_stmt();

        let mut alter = Stmt::Empty;
        if self.current_token() == &Token::Else {
            self.get_token_and_move();

            if self.current_token() == &Token::If {
                alter = self.parse_if_stmt();
            } else {
                alter = self.parse_block_stmt();
            }
        }

        Stmt::If(IfStmt {
            condition,
            then_block: Box::new(main),
            else_block: Box::new(alter),
        })
    }

    fn parse_while_stmt(&mut self) -> Stmt {
        self.expect(&Token::While);
        let condition = self.parse_expr(Binding::Assign);
        let body = self.parse_block_stmt();

        Stmt::While(WhileStmt {
            condition,
            body: Box::new(body),
        })
    }

    fn parse_for_stmt(&mut self) -> Stmt {
        self.get_token_and_move();
        //initialize with some values
        let mut for_stmt = ForStmt {
            item: self.expect_identifier_get_name(),
            index: None,
            iterator: Expr::Empty,
            body: Box::new(Stmt::Empty),
        };

        if self.current_token() == &Token::Coma {
            self.expect(&Token::Coma);
            for_stmt.index = Some(self.expect_identifier_get_name());
        }

        self.expect(&Token::In);
        let iterator = self.parse_expr(Binding::Def);
        for_stmt.iterator = iterator;

        let body = self.parse_block_stmt();
        for_stmt.body = Box::new(body);

        Stmt::For(for_stmt)
    }

    fn parse_fun_stmt(&mut self) -> Stmt {
        self.expect(&Token::Fun);
        let fun_name = self.expect_identifier_get_name();
        let mut fun_type = Type::None;

        let mut params = Vec::new();
        self.expect(&Token::LeftParen);

        while self.has_tokens() && self.current_token() != &Token::RightParen {
            params.push(self.parse_fun_param());

            if !matches!(self.current_token(), &Token::RightParen | &Token::Eof) {
                self.expect(&Token::Coma);
            }
        }

        self.expect(&Token::RightParen);

        if self.current_token() == &Token::Arrow {
            self.expect(&Token::Arrow);
            fun_type = self.parse_type();
        } else if self.current_token() == &Token::Colon {
            self.expect(&Token::Colon);
            fun_type = self.parse_type();
            self.compatibility_diagnostics
                .push(CompatibilityDiagnostic::legacy_return_style(
                    &fun_name, &fun_type,
                ));
        }

        let block = self.parse_block_stmt();

        Stmt::Fun(FunStmt {
            name: fun_name,
            return_type: fun_type,
            params,
            block: Box::new(block),
        })
    }

    fn parse_fun_param(&mut self) -> Param {
        // Canonical: name: Type
        if self.is_next_token(&Token::Colon) {
            let name = self.expect_identifier_get_name();
            self.expect(&Token::Colon);
            let param_type = self.parse_type();
            return Param { name, param_type };
        }

        // Legacy compatibility: Type name
        let param_type = self.parse_type();
        let name = self.expect_identifier_get_name();
        self.compatibility_diagnostics
            .push(CompatibilityDiagnostic::legacy_param_style(
                &name,
                &param_type,
            ));
        Param { name, param_type }
    }

    fn parse_fun_call_expr(&mut self, left: Expr) -> Expr {
        self.get_token_and_move();
        let mut arguments = Vec::new();

        while self.has_tokens() && self.current_token() != &Token::RightParen {
            arguments.push(self.parse_expr(Binding::Assign));

            if !matches!(self.current_token(), &Token::RightParen | &Token::Eof) {
                self.expect(&Token::Coma);
            }
        }

        self.expect(&Token::RightParen);

        Expr::Call(CallExpr {
            callee: Box::new(left),
            arguments,
        })
    }

    fn parse_member_exrp(&mut self, left: Expr) -> Expr {
        let is_computed = self.get_token_and_move() == &Token::LeftBrace;

        if is_computed {
            let expr = self.parse_expr(Binding::Def);
            self.expect(&Token::RightBrace);
            return Expr::ComputedExpr(ComputedExpr {
                member: Box::new(left),
                property: Box::new(expr),
            });
        }

        Expr::Mebmer(MemberExpr {
            member: Box::new(left),
            property: self.expect_identifier_get_name(),
        })
    }

    fn parse_class_stmt(&mut self) -> Stmt {
        self.expect(&Token::Class);
        let class_name = self.expect_identifier_get_name();
        let block = self.parse_block_stmt();

        Stmt::Class(ClassStmt {
            name: class_name,
            block: Box::new(block),
        })
    }

    fn parse_impl_stmt(&mut self) -> Stmt {
        self.expect(&Token::Impl);
        let name = self.expect_identifier_get_name();
        self.expect(&Token::LeftBracket);

        let mut methods = Vec::new();
        while self.has_tokens() && self.current_token() != &Token::RightBracket {
            if self.current_token() == &Token::Semicolon {
                self.get_token_and_move();
                continue;
            }

            let method = if self.current_token() == &Token::Pub {
                self.parse_pub_stmt()
            } else {
                self.parse_fun_stmt()
            };

            let valid = matches!(method, Stmt::Fun(_))
                || matches!(&method, Stmt::Pub(pub_stmt) if matches!(pub_stmt.stmt.as_ref(), Stmt::Fun(_)));

            if !valid {
                panic!("Impl blocks can contain only function declarations");
            }

            methods.push(method);
        }

        self.expect(&Token::RightBracket);
        Stmt::Impl(ImplStmt { name, methods })
    }

    fn parse_pub_stmt(&mut self) -> Stmt {
        self.expect(&Token::Pub);
        let inner = match self.current_token() {
            Token::Fun => self.parse_fun_stmt(),
            Token::Var => self.parse_var_stmt(),
            Token::Const => self.parse_const_stmt(),
            Token::Struct => self.parse_struct_stmt(),
            Token::Enum => self.parse_enum_stmt(),
            Token::Class => self.parse_class_stmt(),
            Token::Mod => self.parse_mod_stmt(),
            Token::Use => self.parse_use_stmt(),
            Token::Impl => self.parse_impl_stmt(),
            _ => panic!("Unsupported token after 'pub': {}", self.current_token()),
        };

        Stmt::Pub(PubStmt {
            stmt: Box::new(inner),
        })
    }

    fn parse_import_stmt(&mut self) -> Stmt {
        self.expect(&Token::Import);

        let mut from = "".to_string();
        let import = self.expect_identifier_get_name();
        let mut alias = None;

        if self.current_token() == &Token::As {
            self.expect(&Token::As);
            alias = Some(self.expect_identifier_get_name());
        }

        if self.current_token() == &Token::From {
            self.get_token_and_move();
            from = match self.expect(&Token::String("any".to_string())) {
                Token::String(s) => s.to_string(),
                _ => panic!("Incorrect import from value"),
            }
        } else {
            from = import.clone();
        }

        self.expect(&Token::Semicolon);

        Stmt::Import(ImportStmt {
            import,
            from,
            alias,
        })
    }

    fn parse_match_stmt(&mut self) -> Stmt {
        self.expect(&Token::Match);
        let expr = self.parse_expr(Binding::Def);
        self.expect(&Token::LeftBracket);

        let mut arms = Vec::new();
        while self.has_tokens() && self.current_token() != &Token::RightBracket {
            if self.current_token() == &Token::Coma {
                self.get_token_and_move();
                continue;
            }
            if self.current_token() == &Token::Semicolon {
                self.get_token_and_move();
                continue;
            }

            let pattern = self.parse_pattern();
            self.expect(&Token::FatArrow);

            let body = if self.current_token() == &Token::LeftBracket {
                self.parse_block_stmt()
            } else {
                let arm_expr = self.parse_expr(Binding::Def);
                self.expect(&Token::Semicolon);
                Stmt::Expr(ExprStmt { expr: arm_expr })
            };

            arms.push(MatchArm {
                pattern,
                body: Box::new(body),
            });

            if self.current_token() == &Token::Coma {
                self.expect(&Token::Coma);
            }
        }

        self.expect(&Token::RightBracket);
        Stmt::Match(MatchStmt { expr, arms })
    }

    fn parse_pattern(&mut self) -> Pattern {
        match self.current_token() {
            Token::Identifier(name) => {
                let identifier = self.expect_identifier_get_name();

                if identifier == "_" {
                    return Pattern::Wildcard;
                }

                if self.current_token() == &Token::LeftParen {
                    self.expect(&Token::LeftParen);
                    let mut patterns = Vec::new();

                    while self.has_tokens() && self.current_token() != &Token::RightParen {
                        patterns.push(self.parse_pattern());
                        if !matches!(self.current_token(), &Token::RightParen | &Token::Eof) {
                            self.expect(&Token::Coma);
                        }
                    }

                    self.expect(&Token::RightParen);
                    Pattern::Variant(identifier, patterns)
                } else {
                    Pattern::Identifier(identifier)
                }
            }
            Token::String(s) => {
                let value = match self.get_token_and_move() {
                    Token::String(s) => s.to_string(),
                    _ => unreachable!(),
                };
                Pattern::Literal(Literal::String(value))
            }
            Token::Char(c) => {
                let value = match self.get_token_and_move() {
                    Token::Char(c) => *c,
                    _ => unreachable!(),
                };
                Pattern::Literal(Literal::Char(value))
            }
            Token::Number(n) => {
                let value = match self.get_token_and_move() {
                    Token::Number(n) => *n,
                    _ => unreachable!(),
                };
                Pattern::Literal(Literal::Num(value))
            }
            Token::True => {
                self.expect(&Token::True);
                Pattern::Literal(Literal::Bool(true))
            }
            Token::False => {
                self.expect(&Token::False);
                Pattern::Literal(Literal::Bool(false))
            }
            Token::None => {
                self.expect(&Token::None);
                Pattern::Identifier("None".to_string())
            }
            _ => panic!("Unsupported pattern token {}", self.current_token()),
        }
    }

    fn parse_group_expr(&mut self) -> Expr {
        self.expect(&Token::LeftParen);
        let exrp = self.parse_expr(Binding::Def);
        self.expect(&Token::RightParen);
        exrp
    }

    fn parse_return_stmt(&mut self) -> Stmt {
        self.expect(&Token::Return);

        let expr = if self.current_token() == &Token::Semicolon {
            Expr::Empty
        } else {
            self.parse_expr(Binding::Def)
        };

        self.expect(&Token::Semicolon);

        Stmt::Return(ReturnStmt { return_expr: expr })
    }

    fn parse_break_stmt(&mut self) -> Stmt {
        self.expect(&Token::Break);
        self.expect(&Token::Semicolon);
        Stmt::Break(BreakStmt)
    }

    fn parse_continue_stmt(&mut self) -> Stmt {
        self.expect(&Token::Continue);
        self.expect(&Token::Semicolon);
        Stmt::Continue(ContinueStmt)
    }

    fn parse_array_literal_expr(&mut self) -> Expr {
        self.expect(&Token::LeftBrace);
        let mut array = Vec::new();

        while self.has_tokens() && self.current_token() != &Token::RightBrace {
            array.push(self.parse_expr(Binding::Coma));

            if !matches!(self.current_token(), &Token::RightBrace | &Token::Eof) {
                self.expect(&Token::Coma);
            }
        }

        self.expect(&Token::RightBrace);

        Expr::Array(ArrayExpr { array })
    }

    fn handle_nud(&mut self) -> Option<Expr> {
        //TODO extend
        match self.current_token() {
            Token::Number(_) => Some(self.parse_primary_expr()),
            Token::String(_) => Some(self.parse_primary_expr()),
            Token::Char(_) => Some(self.parse_primary_expr()),
            Token::True => Some(self.parse_primary_expr()),
            Token::False => Some(self.parse_primary_expr()),
            Token::None => Some(self.parse_primary_expr()),
            Token::This => Some(self.parse_primary_expr()),
            Token::Identifier(_) => Some(self.parse_primary_expr()),
            //Unary
            Token::Minus => Some(self.parse_prefix_expr()),
            Token::Not => Some(self.parse_prefix_expr()),
            //Group
            Token::LeftParen => Some(self.parse_group_expr()),
            Token::LeftBrace => Some(self.parse_array_literal_expr()),
            Token::Semicolon => None,
            _ => panic!(
                "No handler found for literal token {}",
                self.current_token()
            ),
        }
    }

    fn handle_stmt(&mut self) -> Option<Stmt> {
        //TODO extend
        match self.current_token() {
            Token::LeftBracket => Some(self.parse_block_stmt()),
            Token::Pub => Some(self.parse_pub_stmt()),
            Token::Mod => Some(self.parse_mod_stmt()),
            Token::Use => Some(self.parse_use_stmt()),
            Token::Var => Some(self.parse_var_stmt()),
            Token::Const => Some(self.parse_const_stmt()),
            Token::Struct => Some(self.parse_struct_stmt()),
            Token::Enum => Some(self.parse_enum_stmt()),
            Token::Impl => Some(self.parse_impl_stmt()),
            Token::If => Some(self.parse_if_stmt()),
            Token::Match => Some(self.parse_match_stmt()),
            Token::While => Some(self.parse_while_stmt()),
            Token::For => Some(self.parse_for_stmt()),
            Token::Break => Some(self.parse_break_stmt()),
            Token::Continue => Some(self.parse_continue_stmt()),
            Token::Fun => Some(self.parse_fun_stmt()),
            Token::Class => Some(self.parse_class_stmt()),
            Token::Import => Some(self.parse_import_stmt()),
            Token::Return => Some(self.parse_return_stmt()),
            _ => None,
        }
    }

    fn handle_led(&mut self, left: Expr) -> Expr {
        //TODO extend
        match self.current_token() {
            //Math
            Token::Plus => self.parse_binary_expr(left),
            Token::Minus => self.parse_binary_expr(left),
            Token::Star => self.parse_binary_expr(left),
            Token::Slash => self.parse_binary_expr(left),
            Token::Percent => self.parse_binary_expr(left),
            Token::ShiftLeft => self.parse_binary_expr(left),
            Token::ShiftRight => self.parse_binary_expr(left),
            Token::BitAnd => self.parse_binary_expr(left),
            Token::BitOr => self.parse_binary_expr(left),
            Token::BitXor => self.parse_binary_expr(left),
            //Relation
            Token::NotEqual => self.parse_binary_expr(left),
            Token::EqualEqual => self.parse_binary_expr(left),
            Token::Greater => self.parse_binary_expr(left),
            Token::GreaterEqual => self.parse_binary_expr(left),
            Token::Less => self.parse_binary_expr(left),
            Token::LessEqual => self.parse_binary_expr(left),
            //Logical
            Token::And => self.parse_binary_expr(left),
            Token::Or => self.parse_binary_expr(left),
            //Assignme
            Token::Equal => self.parse_assignment_exrp(left),
            Token::PlusEqual => self.parse_assignment_exrp(left),
            Token::MinusEqual => self.parse_assignment_exrp(left),
            Token::StarEqual => self.parse_assignment_exrp(left),
            Token::SlashEqual => self.parse_assignment_exrp(left),
            Token::PercentEqual => self.parse_assignment_exrp(left),
            //Call, Member
            Token::LeftParen => self.parse_fun_call_expr(left),
            Token::LeftBrace => self.parse_member_exrp(left),
            Token::Dot => self.parse_member_exrp(left),
            Token::Question => self.parse_try_expr(left),
            _ => panic!(
                "No handler found for operator token {}",
                self.current_token()
            ),
        }
    }

    fn get_current_token_power(&self) -> Binding {
        //TODO extend
        match self.current_token() {
            Token::Number(_) => Binding::Primary,
            Token::String(_) => Binding::Primary,
            Token::Char(_) => Binding::Primary,
            Token::Identifier(_) => Binding::Primary,
            Token::ShiftLeft => Binding::Shift,
            Token::ShiftRight => Binding::Shift,
            Token::BitAnd => Binding::Bitwise,
            Token::BitOr => Binding::Bitwise,
            Token::BitXor => Binding::Bitwise,
            Token::Plus => Binding::Add,
            Token::Minus => Binding::Add,
            Token::Star => Binding::Mult,
            Token::Slash => Binding::Mult,
            Token::Percent => Binding::Mult,
            Token::NotEqual => Binding::Relation,
            Token::EqualEqual => Binding::Relation,
            Token::Greater => Binding::Relation,
            Token::GreaterEqual => Binding::Relation,
            Token::Less => Binding::Relation,
            Token::LessEqual => Binding::Relation,
            Token::And => Binding::Logic,
            Token::Or => Binding::Logic,
            Token::Not => Binding::Unary,
            Token::Equal => Binding::Assign,
            Token::PlusEqual => Binding::Assign,
            Token::MinusEqual => Binding::Assign,
            Token::StarEqual => Binding::Assign,
            Token::SlashEqual => Binding::Assign,
            Token::PercentEqual => Binding::Assign,
            Token::LeftParen => Binding::Call,
            Token::LeftBrace => Binding::Member,
            Token::Dot => Binding::Member,
            Token::Question => Binding::Member,
            _ => Binding::Def,
        }
    }

    pub fn parse_type(&mut self) -> Type {
        let type_name = self.expect_identifier_get_name();
        let t = match type_name.as_str() {
            "num" | "Num" => Type::Num,
            "char" | "Char" => Type::Char,
            "byte" | "Byte" => Type::Byte,
            "string" | "String" => Type::String,
            "bool" | "Bool" => Type::Bool,
            "void" | "Void" => Type::Void,
            "" => Type::None,
            s => Type::Identifier(s.to_string()),
        };

        if self.current_token() == &Token::Less {
            if !matches!(t, Type::Identifier(_)) {
                panic!("Generic type arguments are only allowed for identifier types");
            }

            self.expect(&Token::Less);
            while self.has_tokens()
                && !matches!(self.current_token(), Token::Greater | Token::ShiftRight)
            {
                let _ = self.parse_type();
                if !matches!(
                    self.current_token(),
                    Token::Greater | Token::ShiftRight | Token::Eof
                ) {
                    self.expect(&Token::Coma);
                }
            }

            if self.current_token() == &Token::ShiftRight {
                // Split `>>` into `>` + `>` while parsing nested generic type closings.
                self.tokens[self.current] = Token::Greater;
            } else {
                self.expect(&Token::Greater);
            }
        }

        if self.current_token() == &Token::LeftBrace {
            self.expect(&Token::LeftBrace);
            self.expect(&Token::RightBrace);
            Type::Array(Box::new(t))
        } else {
            t
        }
    }
}

mod tests {
    use crate::{
        ast::*,
        lexer::Token,
        parser::{render_compatibility_diagnostic, Binding, CompatibilityWarningKind, Parser},
    };

    #[test]
    fn parse_var_stmt_with_literal() {
        let tokens = vec![
            Token::Var,
            Token::Identifier("variable".to_string()),
            Token::Colon,
            Token::Identifier("Num".to_string()),
            Token::Equal,
            Token::Number(1.0),
            Token::Semicolon,
        ];

        let mut parser = Parser::new(tokens);
        let res = parser.parse_var_stmt();

        let expected = Stmt::Var(VarStmt {
            name: "variable".to_string(),
            assignment: Expr::Literal(Literal::Num(1.0)),
            var_type: Type::Num,
        });

        assert_eq!(res, expected);
    }

    #[test]
    fn parse_var_stmt_as_declaration() {
        let tokens = vec![
            Token::Var,
            Token::Identifier("variable".to_string()),
            Token::Colon,
            Token::Identifier("Num".to_string()),
            Token::Semicolon,
        ];

        let mut parser = Parser::new(tokens);
        let res = parser.parse_var_stmt();

        let expected = Stmt::Var(VarStmt {
            name: "variable".to_string(),
            assignment: Expr::Empty,
            var_type: Type::Num,
        });

        assert_eq!(res, expected);
    }

    #[test]
    fn parse_var_stmt_array() {
        let tokens = vec![
            Token::Var,
            Token::Identifier("variable".to_string()),
            Token::Colon,
            Token::Identifier("Num".to_string()),
            Token::LeftBrace,
            Token::RightBrace,
            Token::Semicolon,
        ];

        let mut parser = Parser::new(tokens);
        let res = parser.parse_var_stmt();

        let expected = Stmt::Var(VarStmt {
            name: "variable".to_string(),
            assignment: Expr::Empty,
            var_type: Type::Array(Box::new(Type::Num)),
        });

        assert_eq!(res, expected);
    }

    #[test]
    fn parse_block_stmt_empty() {
        let tokens = vec![Token::LeftBracket, Token::RightBracket];

        let mut parser = Parser::new(tokens);
        let res = parser.parse_block_stmt();

        let expected = Stmt::Block(BlockStmt { stmts: vec![] });

        assert_eq!(res, expected);
    }

    #[test]
    fn parse_block_stmt_with_var() {
        let tokens = vec![
            Token::LeftBracket,
            Token::Var,
            Token::Identifier("variable".to_string()),
            Token::Colon,
            Token::Identifier("Num".to_string()),
            Token::Semicolon,
            Token::RightBracket,
        ];

        let mut parser = Parser::new(tokens);
        let res = parser.parse_block_stmt();

        let expected = Stmt::Block(BlockStmt {
            stmts: vec![Stmt::Var(VarStmt {
                name: "variable".to_string(),
                assignment: Expr::Empty,
                var_type: Type::Num,
            })],
        });

        assert_eq!(res, expected);
    }

    #[test]
    fn parse_binary_expr_binary() {
        let tokens = vec![Token::Plus, Token::Number(2.0), Token::Semicolon];

        let mut parser = Parser::new(tokens);
        let res = parser.parse_binary_expr(Expr::Literal(Literal::Num(1.0)));

        let expected = Expr::Binary(
            Box::new(Expr::Literal(Literal::Num(1.0))),
            Token::Plus,
            Box::new(Expr::Literal(Literal::Num(2.0))),
        );

        assert_eq!(res, expected);
    }

    #[test]
    fn parse_binary_expr_sum_mult_sum() {
        let tokens = vec![
            Token::Number(1.0),
            Token::Plus,
            Token::Number(2.0),
            Token::Star,
            Token::Number(3.0),
            Token::Plus,
            Token::Number(4.0),
            Token::Semicolon,
        ];

        let mut parser = Parser::new(tokens);
        //TODO check why parse_binary_expr misses 1 layer
        //let res = parser.parse_binary_expr(Expr::Literal(Literal::Num(1.0)));
        let res = parser.parse_expr(Binding::Def);

        let expected = Expr::Binary(
            Box::new(Expr::Binary(
                Box::new(Expr::Literal(Literal::Num(1.0))),
                Token::Plus,
                Box::new(Expr::Binary(
                    Box::new(Expr::Literal(Literal::Num(2.0))),
                    Token::Star,
                    Box::new(Expr::Literal(Literal::Num(3.0))),
                )),
            )),
            Token::Plus,
            Box::new(Expr::Literal(Literal::Num(4.0))),
        );

        assert_eq!(res, expected);
    }

    #[test]
    fn parse_binary_expr_no_semicolon() {
        let tokens = vec![Token::Plus, Token::Number(2.0), Token::Eof];

        let mut parser = Parser::new(tokens);
        let res = parser.parse_binary_expr(Expr::Literal(Literal::Num(1.0)));

        let expected = Expr::Binary(
            Box::new(Expr::Literal(Literal::Num(1.0))),
            Token::Plus,
            Box::new(Expr::Literal(Literal::Num(2.0))),
        );

        assert_eq!(res, expected);
    }

    #[test]
    fn parse_prefix_expr_not() {
        let tokens = vec![Token::Not, Token::True, Token::Eof];

        let mut parser = Parser::new(tokens);
        let res = parser.parse_prefix_expr();

        let expected = Expr::Unary(Token::Not, Box::new(Expr::Literal(Literal::Bool(true))));

        assert_eq!(res, expected);
    }

    #[test]
    fn parse_prefix_expr_minus() {
        let tokens = vec![Token::Minus, Token::Number(1.0), Token::Eof];

        let mut parser = Parser::new(tokens);
        let res = parser.parse_prefix_expr();

        let expected = Expr::Unary(Token::Minus, Box::new(Expr::Literal(Literal::Num(1.0))));

        assert_eq!(res, expected);
    }

    #[test]
    fn parse_assign_stmt_literal() {
        let tokens = vec![Token::Equal, Token::Number(1.0), Token::Eof];

        let mut parser = Parser::new(tokens);
        let res = parser.parse_assignment_exrp(Expr::Literal(Literal::Identifier("a".to_string())));

        let expected = Expr::Assignment(
            Box::new(Expr::Literal(Literal::Identifier("a".to_string()))),
            Box::new(Expr::Literal(Literal::Num(1.0))),
        );

        assert_eq!(res, expected);
    }

    #[test]
    fn parse_assign_stmt_expression() {
        let tokens = vec![
            Token::Equal,
            Token::Identifier("a".to_string()),
            Token::Plus,
            Token::Number(2.0),
            Token::Eof,
        ];

        let mut parser = Parser::new(tokens);
        let res = parser.parse_assignment_exrp(Expr::Literal(Literal::Identifier("a".to_string())));

        let expected = Expr::Assignment(
            Box::new(Expr::Literal(Literal::Identifier("a".to_string()))),
            Box::new(Expr::Binary(
                Box::new(Expr::Literal(Literal::Identifier("a".to_string()))),
                Token::Plus,
                Box::new(Expr::Literal(Literal::Num(2.0))),
            )),
        );

        assert_eq!(res, expected);
    }

    #[test]
    fn parse_if_stmt_if_true() {
        let tokens = vec![
            Token::If,
            Token::True,
            Token::LeftBracket,
            Token::Number(1.0),
            Token::Plus,
            Token::Number(2.0),
            Token::Semicolon,
            Token::RightBracket,
            Token::Eof,
        ];

        let mut parser = Parser::new(tokens);
        let res = parser.parse_if_stmt();

        let expected = Stmt::If(IfStmt {
            condition: Expr::Literal(Literal::Bool(true)),
            then_block: Box::new(Stmt::Block(BlockStmt {
                stmts: vec![Stmt::Expr(ExprStmt {
                    expr: Expr::Binary(
                        Box::new(Expr::Literal(Literal::Num(1.0))),
                        Token::Plus,
                        Box::new(Expr::Literal(Literal::Num(2.0))),
                    ),
                })],
            })),
            else_block: Box::new(Stmt::Empty),
        });

        assert_eq!(res, expected);
    }

    #[test]
    fn parse_if_stmt_if_true_else() {
        let tokens = vec![
            Token::If,
            Token::True,
            Token::LeftBracket,
            Token::Number(1.0),
            Token::Plus,
            Token::Number(2.0),
            Token::Semicolon,
            Token::RightBracket,
            Token::Else,
            Token::LeftBracket,
            Token::Number(3.0),
            Token::Plus,
            Token::Number(4.0),
            Token::Semicolon,
            Token::RightBracket,
            Token::Eof,
        ];

        let mut parser = Parser::new(tokens);
        let res = parser.parse_if_stmt();

        let expected = Stmt::If(IfStmt {
            condition: Expr::Literal(Literal::Bool(true)),
            then_block: Box::new(Stmt::Block(BlockStmt {
                stmts: vec![Stmt::Expr(ExprStmt {
                    expr: Expr::Binary(
                        Box::new(Expr::Literal(Literal::Num(1.0))),
                        Token::Plus,
                        Box::new(Expr::Literal(Literal::Num(2.0))),
                    ),
                })],
            })),
            else_block: Box::new(Stmt::Block(BlockStmt {
                stmts: vec![Stmt::Expr(ExprStmt {
                    expr: Expr::Binary(
                        Box::new(Expr::Literal(Literal::Num(3.0))),
                        Token::Plus,
                        Box::new(Expr::Literal(Literal::Num(4.0))),
                    ),
                })],
            })),
        });

        assert_eq!(res, expected);
    }

    #[test]
    fn parse_if_stmt_if_true_else_if() {
        let tokens = vec![
            Token::If,
            Token::True,
            Token::LeftBracket,
            Token::Number(1.0),
            Token::Plus,
            Token::Number(2.0),
            Token::Semicolon,
            Token::RightBracket,
            Token::Else,
            Token::If,
            Token::True,
            Token::LeftBracket,
            Token::Number(3.0),
            Token::Plus,
            Token::Number(4.0),
            Token::Semicolon,
            Token::RightBracket,
            Token::Eof,
        ];

        let mut parser = Parser::new(tokens);
        let res = parser.parse_if_stmt();

        let expected = Stmt::If(IfStmt {
            condition: Expr::Literal(Literal::Bool(true)),
            then_block: Box::new(Stmt::Block(BlockStmt {
                stmts: vec![Stmt::Expr(ExprStmt {
                    expr: Expr::Binary(
                        Box::new(Expr::Literal(Literal::Num(1.0))),
                        Token::Plus,
                        Box::new(Expr::Literal(Literal::Num(2.0))),
                    ),
                })],
            })),
            else_block: Box::new(Stmt::If(IfStmt {
                condition: Expr::Literal(Literal::Bool(true)),
                then_block: Box::new(Stmt::Block(BlockStmt {
                    stmts: vec![Stmt::Expr(ExprStmt {
                        expr: Expr::Binary(
                            Box::new(Expr::Literal(Literal::Num(3.0))),
                            Token::Plus,
                            Box::new(Expr::Literal(Literal::Num(4.0))),
                        ),
                    })],
                })),
                else_block: Box::new(Stmt::Empty),
            })),
        });

        assert_eq!(res, expected);
    }

    #[test]
    fn parse_for_stmt() {
        let tokens = vec![
            Token::For,
            Token::Identifier("num".to_string()),
            Token::Coma,
            Token::Identifier("index".to_string()),
            Token::In,
            Token::Identifier("nums".to_string()),
            Token::LeftBracket,
            Token::Var,
            Token::Identifier("a".to_string()),
            Token::Equal,
            Token::Identifier("index".to_string()),
            Token::Semicolon,
            Token::RightBracket,
            Token::Eof,
        ];

        let mut parser = Parser::new(tokens);
        let res = parser.parse_for_stmt();

        let expected = Stmt::For(ForStmt {
            item: "num".to_string(),
            index: Some("index".to_string()),
            iterator: Expr::Literal(Literal::Identifier("nums".to_string())),
            body: Box::new(Stmt::Block(BlockStmt {
                stmts: vec![Stmt::Var(VarStmt {
                    name: "a".to_string(),
                    assignment: Expr::Literal(Literal::Identifier("index".to_string())),
                    var_type: Type::None,
                })],
            })),
        });

        assert_eq!(res, expected);
    }

    #[test]
    fn parse_fn_stmt() {
        let tokens = vec![
            Token::Fun,
            Token::Identifier("main".to_string()),
            Token::LeftParen,
            Token::Identifier("num".to_string()),
            Token::Identifier("a".to_string()),
            Token::Coma,
            Token::Identifier("num".to_string()),
            Token::Identifier("b".to_string()),
            Token::RightParen,
            Token::Colon,
            Token::Identifier("num".to_string()),
            Token::LeftBracket,
            Token::Var,
            Token::Identifier("c".to_string()),
            Token::Equal,
            Token::Identifier("a".to_string()),
            Token::Plus,
            Token::Identifier("b".to_string()),
            Token::Semicolon,
            Token::RightBracket,
            Token::Eof,
        ];

        let mut parser = Parser::new(tokens);
        let res = parser.parse_fun_stmt();

        let expected = Stmt::Fun(FunStmt {
            name: "main".to_string(),
            return_type: Type::Num,
            params: vec![
                Param {
                    name: "a".to_string(),
                    param_type: Type::Num,
                },
                Param {
                    name: "b".to_string(),
                    param_type: Type::Num,
                },
            ],
            block: Box::new(Stmt::Block(BlockStmt {
                stmts: vec![Stmt::Var(VarStmt {
                    name: "c".to_string(),
                    assignment: Expr::Binary(
                        Box::new(Expr::Literal(Literal::Identifier("a".to_string()))),
                        Token::Plus,
                        Box::new(Expr::Literal(Literal::Identifier("b".to_string()))),
                    ),
                    var_type: Type::None,
                })],
            })),
        });

        assert_eq!(res, expected);
    }

    #[test]
    fn parse_fn_stmt_return() {
        let tokens = vec![
            Token::Fun,
            Token::Identifier("main".to_string()),
            Token::LeftParen,
            Token::Identifier("num".to_string()),
            Token::Identifier("a".to_string()),
            Token::Coma,
            Token::Identifier("num".to_string()),
            Token::Identifier("b".to_string()),
            Token::RightParen,
            Token::Colon,
            Token::Identifier("num".to_string()),
            Token::LeftBracket,
            Token::Var,
            Token::Identifier("c".to_string()),
            Token::Equal,
            Token::Identifier("a".to_string()),
            Token::Plus,
            Token::Identifier("b".to_string()),
            Token::Semicolon,
            Token::Return,
            Token::Identifier("c".to_string()),
            Token::Semicolon,
            Token::RightBracket,
            Token::Eof,
        ];

        let mut parser = Parser::new(tokens);
        let res = parser.parse_fun_stmt();

        let expected = Stmt::Fun(FunStmt {
            name: "main".to_string(),
            return_type: Type::Num,
            params: vec![
                Param {
                    name: "a".to_string(),
                    param_type: Type::Num,
                },
                Param {
                    name: "b".to_string(),
                    param_type: Type::Num,
                },
            ],
            block: Box::new(Stmt::Block(BlockStmt {
                stmts: vec![
                    Stmt::Var(VarStmt {
                        name: "c".to_string(),
                        assignment: Expr::Binary(
                            Box::new(Expr::Literal(Literal::Identifier("a".to_string()))),
                            Token::Plus,
                            Box::new(Expr::Literal(Literal::Identifier("b".to_string()))),
                        ),
                        var_type: Type::None,
                    }),
                    Stmt::Return(ReturnStmt {
                        return_expr: Expr::Literal(Literal::Identifier("c".to_string())),
                    }),
                ],
            })),
        });

        assert_eq!(res, expected);
    }

    #[test]
    fn parse_class_stmt() {
        let tokens = vec![
            Token::Class,
            Token::Identifier("Animal".to_string()),
            Token::LeftBracket,
            Token::Var,
            Token::Identifier("name".to_string()),
            Token::Colon,
            Token::Identifier("String".to_string()),
            Token::Semicolon,
            Token::Var,
            Token::Identifier("age".to_string()),
            Token::Colon,
            Token::Identifier("Num".to_string()),
            Token::Semicolon,
            Token::Fun,
            Token::Identifier("live".to_string()),
            Token::LeftParen,
            Token::RightParen,
            Token::Colon,
            Token::Identifier("String".to_string()),
            Token::LeftBracket,
            Token::Identifier("name".to_string()),
            Token::Equal,
            Token::String("Living".to_string()),
            Token::Semicolon,
            Token::RightBracket,
            Token::RightBracket,
            Token::Eof,
        ];

        let mut parser = Parser::new(tokens);
        let res = parser.parse_class_stmt();

        let expected = Stmt::Class(ClassStmt {
            name: "Animal".to_string(),
            block: Box::new(Stmt::Block(BlockStmt {
                stmts: vec![
                    Stmt::Var(VarStmt {
                        name: "name".to_string(),
                        assignment: Expr::Empty,
                        var_type: Type::String,
                    }),
                    Stmt::Var(VarStmt {
                        name: "age".to_string(),
                        assignment: Expr::Empty,
                        var_type: Type::Num,
                    }),
                    Stmt::Fun(FunStmt {
                        name: "live".to_string(),
                        return_type: Type::String,
                        params: vec![],
                        block: Box::new(Stmt::Block(BlockStmt {
                            stmts: vec![Stmt::Expr(ExprStmt {
                                expr: Expr::Assignment(
                                    Box::new(Expr::Literal(Literal::Identifier(
                                        "name".to_string(),
                                    ))),
                                    Box::new(Expr::Literal(Literal::String("Living".to_string()))),
                                ),
                            })],
                        })),
                    }),
                ],
            })),
        });

        assert_eq!(res, expected);
    }

    #[test]
    fn parse_call_expr() {
        let tokens = vec![
            Token::LeftParen,
            Token::Identifier("arg_one".to_string()),
            Token::Coma,
            Token::Identifier("arg_two".to_string()),
            Token::RightParen,
            Token::Semicolon,
            Token::Eof,
        ];

        let mut parser = Parser::new(tokens);
        let res =
            parser.parse_fun_call_expr(Expr::Literal(Literal::Identifier("call".to_string())));

        let expected = Expr::Call(CallExpr {
            callee: Box::new(Expr::Literal(Literal::Identifier("call".to_string()))),
            arguments: vec![
                Expr::Literal(Literal::Identifier("arg_one".to_string())),
                Expr::Literal(Literal::Identifier("arg_two".to_string())),
            ],
        });

        assert_eq!(res, expected);
    }

    #[test]
    fn parse_member_call_expr() {
        let tokens = vec![
            Token::LeftParen,
            Token::Identifier("arg".to_string()),
            Token::RightParen,
            Token::Semicolon,
            Token::Eof,
        ];

        let mut parser = Parser::new(tokens);
        let receiver = Expr::Mebmer(MemberExpr {
            member: Box::new(Expr::Literal(Literal::Identifier("point".to_string()))),
            property: "value".to_string(),
        });
        let res = parser.parse_fun_call_expr(receiver.clone());

        let expected = Expr::Call(CallExpr {
            callee: Box::new(receiver),
            arguments: vec![Expr::Literal(Literal::Identifier("arg".to_string()))],
        });

        assert_eq!(res, expected);
    }

    #[test]
    fn parse_member_expr_prop() {
        let tokens = vec![
            Token::Dot,
            Token::Identifier("prop".to_string()),
            Token::Equal,
            Token::Number(1.0),
            Token::Semicolon,
            Token::Eof,
        ];

        let mut parser = Parser::new(tokens);
        let res =
            parser.parse_member_exrp(Expr::Literal(Literal::Identifier("variable".to_string())));

        let expected = Expr::Mebmer(MemberExpr {
            property: "prop".to_string(),
            member: Box::new(Expr::Literal(Literal::Identifier("variable".to_string()))),
        });

        assert_eq!(res, expected);
    }

    #[test]
    fn parse_member_expr_computed() {
        let tokens = vec![
            Token::LeftBrace,
            Token::Identifier("a".to_string()),
            Token::Plus,
            Token::Identifier("b".to_string()),
            Token::RightBrace,
            Token::Eof,
        ];

        let mut parser = Parser::new(tokens);
        let res = parser.parse_member_exrp(Expr::Literal(Literal::Identifier("array".to_string())));

        let expected = Expr::ComputedExpr(ComputedExpr {
            property: Box::new(Expr::Binary(
                Box::new(Expr::Literal(Literal::Identifier("a".to_string()))),
                Token::Plus,
                Box::new(Expr::Literal(Literal::Identifier("b".to_string()))),
            )),
            member: Box::new(Expr::Literal(Literal::Identifier("array".to_string()))),
        });

        assert_eq!(res, expected);
    }

    #[test]
    fn parse_return_stmt_literal() {
        let tokens = vec![
            Token::Return,
            Token::Number(42.0),
            Token::Semicolon,
            Token::Eof,
        ];

        let mut parser = Parser::new(tokens);
        let res = parser.parse_return_stmt();

        let expected = Stmt::Return(ReturnStmt {
            return_expr: Expr::Literal(Literal::Num(42.0)),
        });

        assert_eq!(res, expected);
    }

    #[test]
    fn parse_return_stmt_expr() {
        let tokens = vec![
            Token::Return,
            Token::Identifier("a".to_string()),
            Token::Plus,
            Token::Number(42.0),
            Token::Semicolon,
            Token::Eof,
        ];

        let mut parser = Parser::new(tokens);
        let res = parser.parse_return_stmt();

        let expected = Stmt::Return(ReturnStmt {
            return_expr: Expr::Binary(
                Box::new(Expr::Literal(Literal::Identifier("a".to_string()))),
                Token::Plus,
                Box::new(Expr::Literal(Literal::Num(42.0))),
            ),
        });

        assert_eq!(res, expected);
    }

    #[test]
    fn parse_import_stmt() {
        let tokens = vec![
            Token::Import,
            Token::Identifier("System".to_string()),
            Token::Semicolon,
            Token::Eof,
        ];

        let mut parser = Parser::new(tokens);
        let res = parser.parse_import_stmt();

        let expected = Stmt::Import(ImportStmt {
            import: "System".to_string(),
            from: "System".to_string(),
            alias: None,
        });

        assert_eq!(res, expected);
    }

    #[test]
    fn parse_import_stmt_from() {
        let tokens = vec![
            Token::Import,
            Token::Identifier("System".to_string()),
            Token::From,
            Token::String("Path".to_string()),
            Token::Semicolon,
            Token::Eof,
        ];

        let mut parser = Parser::new(tokens);
        let res = parser.parse_import_stmt();

        let expected = Stmt::Import(ImportStmt {
            import: "System".to_string(),
            from: "Path".to_string(),
            alias: None,
        });

        assert_eq!(res, expected);
    }

    #[test]
    fn parse_array_expr() {
        let tokens = vec![
            Token::LeftBrace,
            Token::String("item1".to_string()),
            Token::Coma,
            Token::String("item2".to_string()),
            Token::RightBrace,
            Token::Semicolon,
            Token::Eof,
        ];

        let mut parser = Parser::new(tokens);
        let res = parser.parse_array_literal_expr();

        let expected = Expr::Array(ArrayExpr {
            array: vec![
                Expr::Literal(Literal::String("item1".to_string())),
                Expr::Literal(Literal::String("item2".to_string())),
            ],
        });

        assert_eq!(res, expected);
    }

    #[test]
    fn legacy_param_syntax_emits_compat_warning_with_hint() {
        let tokens = vec![
            Token::Fun,
            Token::Identifier("main".to_string()),
            Token::LeftParen,
            Token::Identifier("Num".to_string()),
            Token::Identifier("value".to_string()),
            Token::RightParen,
            Token::Arrow,
            Token::Identifier("Num".to_string()),
            Token::LeftBracket,
            Token::Return,
            Token::Identifier("value".to_string()),
            Token::Semicolon,
            Token::RightBracket,
            Token::Eof,
        ];

        let mut parser = Parser::new(tokens);
        let _ = parser.parse();

        assert_eq!(parser.compatibility_diagnostics().len(), 1);
        let warning = &parser.compatibility_diagnostics()[0];
        assert_eq!(warning.kind, CompatibilityWarningKind::LegacyParamStyle);
        assert!(warning.hint.contains("value: Num"));
    }

    #[test]
    fn legacy_return_syntax_emits_compat_warning_with_hint() {
        let tokens = vec![
            Token::Fun,
            Token::Identifier("main".to_string()),
            Token::LeftParen,
            Token::RightParen,
            Token::Colon,
            Token::Identifier("Num".to_string()),
            Token::LeftBracket,
            Token::Return,
            Token::Number(1.0),
            Token::Semicolon,
            Token::RightBracket,
            Token::Eof,
        ];

        let mut parser = Parser::new(tokens);
        let _ = parser.parse();

        assert_eq!(parser.compatibility_diagnostics().len(), 1);
        let warning = &parser.compatibility_diagnostics()[0];
        assert_eq!(warning.kind, CompatibilityWarningKind::LegacyReturnStyle);
        assert!(warning.hint.contains("-> Num"));
    }

    #[test]
    fn render_compat_warning_includes_window_and_hint() {
        let tokens = vec![
            Token::Fun,
            Token::Identifier("main".to_string()),
            Token::LeftParen,
            Token::Identifier("Num".to_string()),
            Token::Identifier("value".to_string()),
            Token::RightParen,
            Token::Colon,
            Token::Identifier("Num".to_string()),
            Token::LeftBracket,
            Token::Return,
            Token::Identifier("value".to_string()),
            Token::Semicolon,
            Token::RightBracket,
            Token::Eof,
        ];

        let mut parser = Parser::new(tokens);
        let _ = parser.parse();
        let rendered = parser
            .compatibility_diagnostics()
            .iter()
            .map(render_compatibility_diagnostic)
            .collect::<Vec<_>>();

        assert_eq!(rendered.len(), 2);
        assert!(rendered.iter().all(|line| line.contains("2026-09-30")));
        assert!(rendered.iter().any(|line| line.contains("KEK-COMPAT-001")));
        assert!(rendered.iter().any(|line| line.contains("KEK-COMPAT-002")));
    }

    #[test]
    fn canonical_signature_emits_no_compat_warning() {
        let tokens = vec![
            Token::Fun,
            Token::Identifier("main".to_string()),
            Token::LeftParen,
            Token::Identifier("value".to_string()),
            Token::Colon,
            Token::Identifier("Num".to_string()),
            Token::RightParen,
            Token::Arrow,
            Token::Identifier("Num".to_string()),
            Token::LeftBracket,
            Token::Return,
            Token::Identifier("value".to_string()),
            Token::Semicolon,
            Token::RightBracket,
            Token::Eof,
        ];

        let mut parser = Parser::new(tokens);
        let _ = parser.parse();
        assert!(parser.compatibility_diagnostics().is_empty());
    }
}
