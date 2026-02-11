use std::collections::HashMap;

use crate::{
    ast::{
        BlockStmt, Expr, ForStmt, FunStmt, IfStmt, Literal, MatchStmt, Pattern, Stmt, Type,
        VarStmt, WhileStmt,
    },
    lexer::Token,
};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SemanticError {
    pub message: String,
}

impl SemanticError {
    fn new(message: impl Into<String>) -> Self {
        Self {
            message: message.into(),
        }
    }
}

#[derive(Debug, Clone)]
struct Symbol {
    ty: Type,
    mutable: bool,
}

#[derive(Debug, Clone)]
struct FunctionSig {
    params: Vec<Type>,
    return_type: Type,
}

pub struct SemanticAnalyzer {
    errors: Vec<SemanticError>,
    scopes: Vec<HashMap<String, Symbol>>,
    functions: HashMap<String, FunctionSig>,
    current_return_type: Option<Type>,
    loop_depth: usize,
}

impl SemanticAnalyzer {
    pub fn new() -> Self {
        Self {
            errors: Vec::new(),
            scopes: vec![HashMap::new()],
            functions: HashMap::new(),
            current_return_type: None,
            loop_depth: 0,
        }
    }

    pub fn analyze(program: &BlockStmt) -> Result<(), Vec<SemanticError>> {
        let mut analyzer = Self::new();
        analyzer.analyze_program(program);

        if analyzer.errors.is_empty() {
            Ok(())
        } else {
            Err(analyzer.errors)
        }
    }

    pub fn analyze_program(&mut self, program: &BlockStmt) {
        self.collect_function_signatures(program);

        for stmt in &program.stmts {
            self.analyze_stmt(stmt);
        }
    }

    fn collect_function_signatures(&mut self, program: &BlockStmt) {
        for stmt in &program.stmts {
            match stmt {
                Stmt::Fun(fun) => self.register_function_signature(fun),
                Stmt::Pub(pub_stmt) => {
                    if let Stmt::Fun(fun) = pub_stmt.stmt.as_ref() {
                        self.register_function_signature(fun);
                    }
                }
                _ => {}
            }
        }
    }

    fn analyze_stmt(&mut self, stmt: &Stmt) {
        match stmt {
            Stmt::Block(block) => {
                self.push_scope();
                for stmt in &block.stmts {
                    self.analyze_stmt(stmt);
                }
                self.pop_scope();
            }
            Stmt::Pub(pub_stmt) => self.analyze_stmt(pub_stmt.stmt.as_ref()),
            Stmt::Mod(_) => {}
            Stmt::Use(_) => {}
            Stmt::Var(var_stmt) => self.analyze_var_stmt(var_stmt),
            Stmt::Const(const_stmt) => {
                let rhs_type = self.analyze_expr(&const_stmt.assignment);
                let final_type = if matches!(const_stmt.const_type, Type::None) {
                    rhs_type
                } else {
                    if !is_assignable(&const_stmt.const_type, &rhs_type) {
                        self.error(format!(
                            "Type mismatch for const '{}': expected {:?}, got {:?}",
                            const_stmt.name, const_stmt.const_type, rhs_type
                        ));
                    }
                    const_stmt.const_type.clone()
                };

                self.define_symbol(&const_stmt.name, final_type, false);
            }
            Stmt::Struct(_) => {}
            Stmt::Enum(_) => {}
            Stmt::Impl(impl_stmt) => {
                self.push_scope();
                for method in &impl_stmt.methods {
                    self.analyze_stmt(method);
                }
                self.pop_scope();
            }
            Stmt::If(if_stmt) => self.analyze_if_stmt(if_stmt),
            Stmt::Match(match_stmt) => self.analyze_match_stmt(match_stmt),
            Stmt::While(while_stmt) => self.analyze_while_stmt(while_stmt),
            Stmt::For(for_stmt) => self.analyze_for_stmt(for_stmt),
            Stmt::Break(_) => {
                if self.loop_depth == 0 {
                    self.error("'break' used outside of loop");
                }
            }
            Stmt::Continue(_) => {
                if self.loop_depth == 0 {
                    self.error("'continue' used outside of loop");
                }
            }
            Stmt::Fun(fun_stmt) => self.analyze_fun_stmt(fun_stmt),
            Stmt::Class(class_stmt) => {
                if let Stmt::Block(block) = class_stmt.block.as_ref() {
                    self.push_scope();
                    for member in &block.stmts {
                        self.analyze_stmt(member);
                    }
                    self.pop_scope();
                }
            }
            Stmt::Return(return_stmt) => {
                let expected = self.current_return_type.clone();
                match expected {
                    None => self.error("'return' used outside of function"),
                    Some(expected_ty) => {
                        if matches!(return_stmt.return_expr, Expr::Empty) {
                            if !matches!(expected_ty, Type::Void | Type::None) {
                                self.error(format!(
                                    "Missing return value for function returning {:?}",
                                    expected_ty
                                ));
                            }
                        } else {
                            let actual_ty = self.analyze_expr(&return_stmt.return_expr);
                            if matches!(expected_ty, Type::Void) {
                                self.error("Return value provided for function returning Void");
                            } else if !is_assignable(&expected_ty, &actual_ty) {
                                self.error(format!(
                                    "Return type mismatch: expected {:?}, got {:?}",
                                    expected_ty, actual_ty
                                ));
                            }
                        }
                    }
                }
            }
            Stmt::Expr(expr_stmt) => {
                self.analyze_expr(&expr_stmt.expr);
            }
            Stmt::Import(_) => {}
            Stmt::Empty => {}
        }
    }

    fn analyze_var_stmt(&mut self, var_stmt: &VarStmt) {
        let rhs_type = if matches!(var_stmt.assignment, Expr::Empty) {
            Type::None
        } else {
            self.analyze_expr(&var_stmt.assignment)
        };

        let final_type = if matches!(var_stmt.var_type, Type::None) {
            rhs_type
        } else {
            if !matches!(var_stmt.assignment, Expr::Empty)
                && !is_assignable(&var_stmt.var_type, &rhs_type)
            {
                self.error(format!(
                    "Type mismatch for variable '{}': expected {:?}, got {:?}",
                    var_stmt.name, var_stmt.var_type, rhs_type
                ));
            }
            var_stmt.var_type.clone()
        };

        self.define_symbol(&var_stmt.name, final_type, true);
    }

    fn analyze_if_stmt(&mut self, if_stmt: &IfStmt) {
        let condition_type = self.analyze_expr(&if_stmt.condition);
        if !matches!(condition_type, Type::Bool | Type::None) {
            self.error(format!(
                "If condition must be Bool, got {:?}",
                condition_type
            ));
        }

        self.analyze_stmt(if_stmt.then_block.as_ref());
        self.analyze_stmt(if_stmt.else_block.as_ref());
    }

    fn analyze_while_stmt(&mut self, while_stmt: &WhileStmt) {
        let condition_type = self.analyze_expr(&while_stmt.condition);
        if !matches!(condition_type, Type::Bool | Type::None) {
            self.error(format!(
                "While condition must be Bool, got {:?}",
                condition_type
            ));
        }

        self.loop_depth += 1;
        self.analyze_stmt(while_stmt.body.as_ref());
        self.loop_depth -= 1;
    }

    fn analyze_for_stmt(&mut self, for_stmt: &ForStmt) {
        let iterator_type = self.analyze_expr(&for_stmt.iterator);

        self.push_scope();
        match iterator_type {
            Type::Array(item_ty) => {
                self.define_symbol(&for_stmt.item, *item_ty, true);
            }
            Type::None => {
                self.define_symbol(&for_stmt.item, Type::None, true);
            }
            other => {
                self.error(format!("For iterator must be array-like, got {:?}", other));
                self.define_symbol(&for_stmt.item, Type::None, true);
            }
        }

        if let Some(index_name) = &for_stmt.index {
            self.define_symbol(index_name, Type::Num, false);
        }

        self.loop_depth += 1;
        self.analyze_stmt(for_stmt.body.as_ref());
        self.loop_depth -= 1;

        self.pop_scope();
    }

    fn analyze_fun_stmt(&mut self, fun_stmt: &FunStmt) {
        let previous_return = self.current_return_type.clone();
        self.current_return_type = Some(fun_stmt.return_type.clone());

        self.push_scope();
        for param in &fun_stmt.params {
            self.define_symbol(&param.name, param.param_type.clone(), true);
        }

        self.analyze_stmt(fun_stmt.block.as_ref());

        self.pop_scope();
        self.current_return_type = previous_return;
    }

    fn analyze_match_stmt(&mut self, match_stmt: &MatchStmt) {
        self.analyze_expr(&match_stmt.expr);

        for arm in &match_stmt.arms {
            self.push_scope();
            self.bind_pattern(&arm.pattern);
            self.analyze_stmt(arm.body.as_ref());
            self.pop_scope();
        }
    }

    fn bind_pattern(&mut self, pattern: &Pattern) {
        match pattern {
            Pattern::Wildcard => {}
            Pattern::Literal(_) => {}
            Pattern::Identifier(name) => {
                self.define_symbol(name, Type::None, true);
            }
            Pattern::Variant(_, nested) => {
                for pattern in nested {
                    self.bind_pattern(pattern);
                }
            }
        }
    }

    fn analyze_expr(&mut self, expr: &Expr) -> Type {
        match expr {
            Expr::Unary(op, right) => {
                let right_ty = self.analyze_expr(right);
                match op {
                    Token::Minus => {
                        if !matches!(right_ty, Type::Num | Type::None) {
                            self.error(format!("Unary '-' expects Num, got {:?}", right_ty));
                        }
                        Type::Num
                    }
                    Token::Not => {
                        if !matches!(right_ty, Type::Bool | Type::None) {
                            self.error(format!("Unary '!' expects Bool, got {:?}", right_ty));
                        }
                        Type::Bool
                    }
                    _ => Type::None,
                }
            }
            Expr::Binary(left, op, right) => {
                let left_ty = self.analyze_expr(left);
                let right_ty = self.analyze_expr(right);
                self.binary_result_type(op, &left_ty, &right_ty)
            }
            Expr::Literal(lit) => match lit {
                Literal::String(_) => Type::String,
                Literal::Char(_) => Type::Char,
                Literal::Num(_) => Type::Num,
                Literal::Bool(_) => Type::Bool,
                Literal::Identifier(name) => match self.lookup_symbol(name) {
                    Some(symbol) => symbol.ty.clone(),
                    None => {
                        self.error(format!("Unknown identifier '{}'", name));
                        Type::None
                    }
                },
                Literal::This => Type::Identifier("This".to_string()),
            },
            Expr::Assignment(left, right) => {
                let right_ty = self.analyze_expr(right);
                match left.as_ref() {
                    Expr::Literal(Literal::Identifier(name)) => {
                        if let Some(symbol) = self.lookup_symbol(name).cloned() {
                            if !symbol.mutable {
                                self.error(format!("Cannot assign to immutable symbol '{}'", name));
                            }
                            if !is_assignable(&symbol.ty, &right_ty) {
                                self.error(format!(
                                    "Assignment type mismatch for '{}': expected {:?}, got {:?}",
                                    name, symbol.ty, right_ty
                                ));
                            }

                            if matches!(symbol.ty, Type::None) {
                                self.update_symbol_type(name, right_ty.clone());
                            }
                        } else {
                            self.error(format!("Assignment target '{}' is not declared", name));
                        }
                    }
                    Expr::Mebmer(member) => {
                        self.analyze_expr(member.member.as_ref());
                    }
                    Expr::ComputedExpr(computed) => {
                        self.analyze_expr(computed.member.as_ref());
                        self.analyze_expr(computed.property.as_ref());
                    }
                    _ => {
                        self.error("Unsupported assignment target");
                    }
                }

                right_ty
            }
            Expr::Call(call) => {
                let mut arg_types = Vec::new();
                for arg in &call.arguments {
                    arg_types.push(self.analyze_expr(arg));
                }

                if let Some(sig) = self.functions.get(&call.method_name).cloned() {
                    if sig.params.len() != arg_types.len() {
                        self.error(format!(
                            "Function '{}' expects {} args, got {}",
                            call.method_name,
                            sig.params.len(),
                            arg_types.len()
                        ));
                    }

                    for (index, (expected, actual)) in
                        sig.params.iter().zip(arg_types.iter()).enumerate()
                    {
                        if !is_assignable(expected, actual) {
                            self.error(format!(
                                "Argument {} for '{}' expected {:?}, got {:?}",
                                index, call.method_name, expected, actual
                            ));
                        }
                    }

                    sig.return_type
                } else {
                    self.error(format!("Unknown function '{}'", call.method_name));
                    Type::None
                }
            }
            Expr::Mebmer(member) => {
                self.analyze_expr(member.member.as_ref());
                Type::None
            }
            Expr::ComputedExpr(computed) => {
                self.analyze_expr(computed.member.as_ref());
                self.analyze_expr(computed.property.as_ref());
                Type::None
            }
            Expr::Array(array) => {
                let mut element_type = Type::None;
                for item in &array.array {
                    let ty = self.analyze_expr(item);
                    if matches!(element_type, Type::None) {
                        element_type = ty;
                    } else if !is_assignable(&element_type, &ty) {
                        self.error(format!(
                            "Array literal contains incompatible types: {:?} and {:?}",
                            element_type, ty
                        ));
                        element_type = Type::None;
                    }
                }

                Type::Array(Box::new(element_type))
            }
            Expr::Empty => Type::Void,
        }
    }

    fn binary_result_type(&mut self, op: &Token, left: &Type, right: &Type) -> Type {
        match op {
            Token::Plus | Token::Minus | Token::Star | Token::Slash | Token::Percent => {
                if !matches!(left, Type::Num | Type::None)
                    || !matches!(right, Type::Num | Type::None)
                {
                    self.error(format!(
                        "Arithmetic op expects Num operands, got {:?} and {:?}",
                        left, right
                    ));
                }
                Type::Num
            }
            Token::ShiftLeft | Token::ShiftRight | Token::BitAnd | Token::BitOr | Token::BitXor => {
                if !matches!(left, Type::Num | Type::None)
                    || !matches!(right, Type::Num | Type::None)
                {
                    self.error(format!(
                        "Bitwise/shift op expects Num operands, got {:?} and {:?}",
                        left, right
                    ));
                }
                Type::Num
            }
            Token::Greater | Token::GreaterEqual | Token::Less | Token::LessEqual => {
                if !matches!(left, Type::Num | Type::None)
                    || !matches!(right, Type::Num | Type::None)
                {
                    self.error(format!(
                        "Comparison op expects Num operands, got {:?} and {:?}",
                        left, right
                    ));
                }
                Type::Bool
            }
            Token::EqualEqual | Token::NotEqual => {
                if !is_assignable(left, right) && !is_assignable(right, left) {
                    self.error(format!(
                        "Equality op expects compatible operands, got {:?} and {:?}",
                        left, right
                    ));
                }
                Type::Bool
            }
            Token::And | Token::Or => {
                if !matches!(left, Type::Bool | Type::None)
                    || !matches!(right, Type::Bool | Type::None)
                {
                    self.error(format!(
                        "Logical op expects Bool operands, got {:?} and {:?}",
                        left, right
                    ));
                }
                Type::Bool
            }
            _ => Type::None,
        }
    }

    fn define_symbol(&mut self, name: &str, ty: Type, mutable: bool) {
        let current_scope = self
            .scopes
            .last_mut()
            .expect("Scope stack should always have at least one scope");

        if current_scope.contains_key(name) {
            self.error(format!("Duplicate declaration of symbol '{}'", name));
            return;
        }

        current_scope.insert(name.to_string(), Symbol { ty, mutable });
    }

    fn lookup_symbol(&self, name: &str) -> Option<&Symbol> {
        for scope in self.scopes.iter().rev() {
            if let Some(symbol) = scope.get(name) {
                return Some(symbol);
            }
        }
        None
    }

    fn update_symbol_type(&mut self, name: &str, ty: Type) {
        for scope in self.scopes.iter_mut().rev() {
            if let Some(symbol) = scope.get_mut(name) {
                symbol.ty = ty;
                return;
            }
        }
    }

    fn push_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }

    fn pop_scope(&mut self) {
        self.scopes.pop();
    }

    fn error(&mut self, message: impl Into<String>) {
        self.errors.push(SemanticError::new(message));
    }

    fn register_function_signature(&mut self, fun: &FunStmt) {
        if self.functions.contains_key(&fun.name) {
            self.error(format!("Duplicate function '{}'", fun.name));
            return;
        }

        self.functions.insert(
            fun.name.clone(),
            FunctionSig {
                params: fun.params.iter().map(|p| p.param_type.clone()).collect(),
                return_type: fun.return_type.clone(),
            },
        );
    }
}

fn is_assignable(expected: &Type, actual: &Type) -> bool {
    if matches!(expected, Type::None) || matches!(actual, Type::None) {
        return true;
    }

    match (expected, actual) {
        (Type::Array(left), Type::Array(right)) => is_assignable(left, right),
        (Type::Identifier(left), Type::Identifier(right)) => left == right,
        _ => expected == actual,
    }
}

#[cfg(test)]
mod tests {
    use super::{is_assignable, SemanticAnalyzer};
    use crate::{
        ast::{
            BlockStmt, Expr, ExprStmt, FunStmt, Literal, Param, ReturnStmt, Stmt, Type, VarStmt,
        },
        lexer::Token,
    };

    #[test]
    fn assignable_allows_none_and_matches_arrays() {
        assert!(is_assignable(&Type::None, &Type::Num));
        assert!(is_assignable(&Type::Num, &Type::None));
        assert!(is_assignable(
            &Type::Array(Box::new(Type::Num)),
            &Type::Array(Box::new(Type::Num))
        ));
        assert!(!is_assignable(
            &Type::Array(Box::new(Type::Num)),
            &Type::Array(Box::new(Type::Bool))
        ));
    }

    #[test]
    fn symbol_scope_lookup_and_update_work() {
        let mut analyzer = SemanticAnalyzer::new();
        analyzer.define_symbol("x", Type::Num, true);

        let top_symbol = analyzer.lookup_symbol("x").expect("symbol x should exist");
        assert_eq!(top_symbol.ty, Type::Num);
        assert!(top_symbol.mutable);

        analyzer.push_scope();
        analyzer.define_symbol("x", Type::Bool, false);
        let inner_symbol = analyzer
            .lookup_symbol("x")
            .expect("inner symbol x should shadow outer");
        assert_eq!(inner_symbol.ty, Type::Bool);
        assert!(!inner_symbol.mutable);

        analyzer.update_symbol_type("x", Type::String);
        assert_eq!(
            analyzer.lookup_symbol("x").map(|s| s.ty.clone()),
            Some(Type::String)
        );

        analyzer.pop_scope();
        assert_eq!(
            analyzer.lookup_symbol("x").map(|s| s.ty.clone()),
            Some(Type::Num)
        );
    }

    #[test]
    fn duplicate_symbol_definition_adds_error() {
        let mut analyzer = SemanticAnalyzer::new();
        analyzer.define_symbol("dup", Type::Num, true);
        analyzer.define_symbol("dup", Type::Num, true);

        assert_eq!(analyzer.errors.len(), 1);
        assert!(analyzer.errors[0]
            .message
            .contains("Duplicate declaration of symbol 'dup'"));
    }

    #[test]
    fn collect_function_signatures_reports_duplicates() {
        let fun = |name: &str| {
            Stmt::Fun(FunStmt {
                name: name.to_string(),
                return_type: Type::Num,
                params: vec![Param {
                    name: "a".to_string(),
                    param_type: Type::Num,
                }],
                block: Box::new(Stmt::Block(BlockStmt { stmts: vec![] })),
            })
        };

        let program = BlockStmt {
            stmts: vec![fun("main"), fun("main")],
        };

        let mut analyzer = SemanticAnalyzer::new();
        analyzer.collect_function_signatures(&program);

        assert_eq!(analyzer.functions.len(), 1);
        assert_eq!(analyzer.errors.len(), 1);
        assert!(analyzer.errors[0]
            .message
            .contains("Duplicate function 'main'"));
    }

    #[test]
    fn binary_result_type_adds_error_for_invalid_operands() {
        let mut analyzer = SemanticAnalyzer::new();
        let ty = analyzer.binary_result_type(&Token::Plus, &Type::Bool, &Type::Num);

        assert_eq!(ty, Type::Num);
        assert_eq!(analyzer.errors.len(), 1);
        assert!(analyzer.errors[0]
            .message
            .contains("Arithmetic op expects Num operands"));
    }

    #[test]
    fn assignment_to_untyped_var_updates_symbol_type() {
        let mut analyzer = SemanticAnalyzer::new();
        analyzer.define_symbol("x", Type::None, true);

        let expr = Expr::Assignment(
            Box::new(Expr::Literal(Literal::Identifier("x".to_string()))),
            Box::new(Expr::Literal(Literal::Num(10.0))),
        );

        let result_type = analyzer.analyze_expr(&expr);

        assert_eq!(result_type, Type::Num);
        assert_eq!(
            analyzer.lookup_symbol("x").map(|s| s.ty.clone()),
            Some(Type::Num)
        );
        assert!(analyzer.errors.is_empty());
    }

    #[test]
    fn analyze_fun_stmt_restores_previous_return_context() {
        let mut analyzer = SemanticAnalyzer::new();
        analyzer.current_return_type = Some(Type::String);

        let fun = FunStmt {
            name: "inner".to_string(),
            return_type: Type::Num,
            params: vec![],
            block: Box::new(Stmt::Block(BlockStmt {
                stmts: vec![Stmt::Return(ReturnStmt {
                    return_expr: Expr::Literal(Literal::Num(1.0)),
                })],
            })),
        };

        analyzer.analyze_fun_stmt(&fun);

        assert_eq!(analyzer.current_return_type, Some(Type::String));
    }

    #[test]
    fn call_argument_count_mismatch_is_reported() {
        let mut analyzer = SemanticAnalyzer::new();
        let program = BlockStmt {
            stmts: vec![Stmt::Fun(FunStmt {
                name: "sum".to_string(),
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
                block: Box::new(Stmt::Block(BlockStmt { stmts: vec![] })),
            })],
        };
        analyzer.collect_function_signatures(&program);

        analyzer.analyze_expr(&Expr::Call(crate::ast::CallExpr {
            method_name: "sum".to_string(),
            arguments: vec![Expr::Literal(Literal::Num(1.0))],
        }));

        assert_eq!(analyzer.errors.len(), 1);
        assert!(analyzer.errors[0]
            .message
            .contains("Function 'sum' expects 2 args, got 1"));
    }

    #[test]
    fn expr_statement_is_analyzed_without_panicking() {
        let mut analyzer = SemanticAnalyzer::new();
        analyzer.define_symbol("x", Type::Num, true);

        let stmt = Stmt::Expr(ExprStmt {
            expr: Expr::Assignment(
                Box::new(Expr::Literal(Literal::Identifier("x".to_string()))),
                Box::new(Expr::Literal(Literal::Num(2.0))),
            ),
        });

        analyzer.analyze_stmt(&stmt);
        assert!(analyzer.errors.is_empty());
    }

    #[test]
    fn var_stmt_type_mismatch_is_reported() {
        let mut analyzer = SemanticAnalyzer::new();
        analyzer.analyze_var_stmt(&VarStmt {
            name: "v".to_string(),
            assignment: Expr::Literal(Literal::Bool(true)),
            var_type: Type::Num,
        });

        assert_eq!(analyzer.errors.len(), 1);
        assert!(analyzer.errors[0]
            .message
            .contains("Type mismatch for variable 'v'"));
    }
}
