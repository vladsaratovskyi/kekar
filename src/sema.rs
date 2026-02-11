use std::collections::{HashMap, HashSet};

use crate::{
    ast::{
        BlockStmt, ClassStmt, EnumStmt, Expr, ForStmt, FunStmt, IfStmt, ImportStmt, Literal,
        MatchStmt, ModStmt, Pattern, Stmt, StructStmt, Type, UseStmt, VarStmt, WhileStmt,
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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Visibility {
    Public,
    Private,
}

#[derive(Debug, Clone)]
struct ImportBinding {
    visibility: Visibility,
}

#[derive(Debug, Clone)]
struct UseBinding {
    visibility: Visibility,
    path: String,
    root: String,
}

#[derive(Debug, Clone)]
struct StructDef {
    visibility: Visibility,
    fields: HashMap<String, Type>,
}

#[derive(Debug, Clone)]
struct EnumDef {
    visibility: Visibility,
    variants: HashMap<String, Vec<Type>>,
}

#[derive(Debug, Clone)]
struct ClassDef {
    visibility: Visibility,
}

#[derive(Debug, Clone)]
enum TypeDef {
    Struct(StructDef),
    Enum(EnumDef),
    Class(ClassDef),
}

pub struct SemanticAnalyzer {
    errors: Vec<SemanticError>,
    scopes: Vec<HashMap<String, Symbol>>,
    functions: HashMap<String, FunctionSig>,
    function_visibility: HashMap<String, Visibility>,
    modules: HashMap<String, Visibility>,
    imports: HashMap<String, ImportBinding>,
    uses: HashMap<String, UseBinding>,
    type_defs: HashMap<String, TypeDef>,
    impl_methods: HashMap<String, HashMap<String, Visibility>>,
    current_return_type: Option<Type>,
    current_impl_type: Option<String>,
    loop_depth: usize,
}

impl SemanticAnalyzer {
    pub fn new() -> Self {
        Self {
            errors: Vec::new(),
            scopes: vec![HashMap::new()],
            functions: HashMap::new(),
            function_visibility: HashMap::new(),
            modules: HashMap::new(),
            imports: HashMap::new(),
            uses: HashMap::new(),
            type_defs: HashMap::new(),
            impl_methods: HashMap::new(),
            current_return_type: None,
            current_impl_type: None,
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
        self.collect_top_level_declarations(program);
        self.validate_use_bindings();

        for stmt in &program.stmts {
            self.analyze_top_level_stmt(stmt);
        }
    }

    #[cfg(test)]
    fn collect_function_signatures(&mut self, program: &BlockStmt) {
        for stmt in &program.stmts {
            match stmt {
                Stmt::Fun(fun) => self.register_function_signature(fun),
                Stmt::Pub(pub_stmt) => {
                    if let Stmt::Fun(fun) = pub_stmt.stmt.as_ref() {
                        self.register_function_signature(fun);
                        self.function_visibility
                            .insert(fun.name.clone(), Visibility::Public);
                    }
                }
                _ => {}
            }
        }
    }

    fn collect_top_level_declarations(&mut self, program: &BlockStmt) {
        for stmt in &program.stmts {
            self.collect_non_impl_declarations(stmt, Visibility::Private);
        }

        for stmt in &program.stmts {
            self.collect_impl_declarations(stmt, Visibility::Private);
        }
    }

    fn collect_non_impl_declarations(&mut self, stmt: &Stmt, visibility: Visibility) {
        match stmt {
            Stmt::Pub(pub_stmt) => {
                self.collect_non_impl_declarations(pub_stmt.stmt.as_ref(), Visibility::Public)
            }
            Stmt::Fun(fun_stmt) => {
                self.register_function_signature(fun_stmt);
                self.function_visibility
                    .insert(fun_stmt.name.clone(), visibility);
            }
            Stmt::Mod(mod_stmt) => self.register_module(mod_stmt, visibility),
            Stmt::Import(import_stmt) => self.register_import(import_stmt, visibility),
            Stmt::Use(use_stmt) => self.register_use(use_stmt, visibility),
            Stmt::Struct(struct_stmt) => self.register_struct_def(struct_stmt, visibility),
            Stmt::Enum(enum_stmt) => self.register_enum_def(enum_stmt, visibility),
            Stmt::Class(class_stmt) => self.register_class_def(class_stmt, visibility),
            Stmt::Impl(_) => {}
            _ => {}
        }
    }

    fn collect_impl_declarations(&mut self, stmt: &Stmt, visibility: Visibility) {
        match stmt {
            Stmt::Pub(pub_stmt) => {
                self.collect_impl_declarations(pub_stmt.stmt.as_ref(), Visibility::Public)
            }
            Stmt::Impl(impl_stmt) => self.register_impl_methods(impl_stmt, visibility),
            _ => {}
        }
    }

    fn analyze_top_level_stmt(&mut self, stmt: &Stmt) {
        match stmt {
            Stmt::Pub(pub_stmt) => match pub_stmt.stmt.as_ref() {
                Stmt::Fun(_)
                | Stmt::Var(_)
                | Stmt::Const(_)
                | Stmt::Struct(_)
                | Stmt::Enum(_)
                | Stmt::Class(_)
                | Stmt::Mod(_)
                | Stmt::Use(_)
                | Stmt::Import(_)
                | Stmt::Impl(_) => self.analyze_top_level_inner(pub_stmt.stmt.as_ref()),
                other => self.error(format!(
                    "Unsupported top-level pub declaration: {:?}",
                    other
                )),
            },
            _ => self.analyze_top_level_inner(stmt),
        }
    }

    fn analyze_top_level_inner(&mut self, stmt: &Stmt) {
        match stmt {
            Stmt::Mod(_) | Stmt::Use(_) | Stmt::Import(_) => {}
            Stmt::Struct(struct_stmt) => self.analyze_struct_stmt(struct_stmt),
            Stmt::Enum(enum_stmt) => self.analyze_enum_stmt(enum_stmt),
            Stmt::Impl(impl_stmt) => self.analyze_impl_stmt(impl_stmt),
            _ => self.analyze_stmt(stmt),
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
            Stmt::Pub(_) => {
                self.error("'pub' is only allowed on top-level declarations and impl methods")
            }
            Stmt::Mod(_) => self.error("'mod' is only allowed at top level"),
            Stmt::Use(_) => self.error("'use' is only allowed at top level"),
            Stmt::Var(var_stmt) => self.analyze_var_stmt(var_stmt),
            Stmt::Const(const_stmt) => {
                if !matches!(const_stmt.const_type, Type::None) {
                    self.validate_type_exists(&const_stmt.const_type, "const type");
                }
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
            Stmt::Struct(_) => self.error("'struct' is only allowed at top level"),
            Stmt::Enum(_) => self.error("'enum' is only allowed at top level"),
            Stmt::Impl(_) => self.error("'impl' is only allowed at top level"),
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
            Stmt::Import(_) => self.error("'import' is only allowed at top level"),
            Stmt::Empty => {}
        }
    }

    fn register_module(&mut self, mod_stmt: &ModStmt, visibility: Visibility) {
        if self.modules.contains_key(&mod_stmt.name) {
            self.error(format!("Duplicate module declaration '{}'", mod_stmt.name));
            return;
        }
        self.modules.insert(mod_stmt.name.clone(), visibility);
    }

    fn register_import(&mut self, import_stmt: &ImportStmt, visibility: Visibility) {
        if import_stmt.from.trim().is_empty() {
            self.error(format!(
                "Import '{}' has empty source path",
                import_stmt.import
            ));
            return;
        }

        let binding_name = import_stmt
            .alias
            .as_ref()
            .cloned()
            .unwrap_or_else(|| import_stmt.import.clone());

        if self.binding_name_taken(&binding_name) {
            self.error(format!(
                "Duplicate module/import/use binding '{}'",
                binding_name
            ));
            return;
        }

        self.imports
            .insert(binding_name, ImportBinding { visibility });
    }

    fn register_use(&mut self, use_stmt: &UseStmt, visibility: Visibility) {
        let mut parts = use_stmt.path.split("::");
        let Some(root) = parts.next() else {
            self.error("Use declaration path is empty");
            return;
        };
        let binding_name = use_stmt
            .path
            .rsplit("::")
            .next()
            .unwrap_or(&use_stmt.path)
            .to_string();

        if self.binding_name_taken(&binding_name) {
            self.error(format!(
                "Duplicate module/import/use binding '{}'",
                binding_name
            ));
            return;
        }

        self.uses.insert(
            binding_name,
            UseBinding {
                visibility,
                path: use_stmt.path.clone(),
                root: root.to_string(),
            },
        );
    }

    fn register_struct_def(&mut self, struct_stmt: &StructStmt, visibility: Visibility) {
        if self.type_defs.contains_key(&struct_stmt.name) {
            self.error(format!("Duplicate type declaration '{}'", struct_stmt.name));
            return;
        }

        let mut fields = HashMap::new();
        for field in &struct_stmt.fields {
            if fields.contains_key(&field.name) {
                self.error(format!(
                    "Duplicate field '{}' in struct '{}'",
                    field.name, struct_stmt.name
                ));
                continue;
            }
            fields.insert(field.name.clone(), field.field_type.clone());
        }

        self.type_defs.insert(
            struct_stmt.name.clone(),
            TypeDef::Struct(StructDef { visibility, fields }),
        );
    }

    fn register_enum_def(&mut self, enum_stmt: &EnumStmt, visibility: Visibility) {
        if self.type_defs.contains_key(&enum_stmt.name) {
            self.error(format!("Duplicate type declaration '{}'", enum_stmt.name));
            return;
        }

        let mut variants = HashMap::new();
        for variant in &enum_stmt.variants {
            if variants.contains_key(&variant.name) {
                self.error(format!(
                    "Duplicate variant '{}' in enum '{}'",
                    variant.name, enum_stmt.name
                ));
                continue;
            }
            variants.insert(variant.name.clone(), variant.arguments.clone());
        }

        self.type_defs.insert(
            enum_stmt.name.clone(),
            TypeDef::Enum(EnumDef {
                visibility,
                variants,
            }),
        );
    }

    fn register_class_def(&mut self, class_stmt: &ClassStmt, visibility: Visibility) {
        if self.type_defs.contains_key(&class_stmt.name) {
            self.error(format!("Duplicate type declaration '{}'", class_stmt.name));
            return;
        }

        self.type_defs.insert(
            class_stmt.name.clone(),
            TypeDef::Class(ClassDef { visibility }),
        );
    }

    fn register_impl_methods(
        &mut self,
        impl_stmt: &crate::ast::ImplStmt,
        impl_visibility: Visibility,
    ) {
        let Some(type_visibility) = self.type_visibility(&impl_stmt.name) else {
            self.error(format!(
                "Impl target type '{}' is not declared",
                impl_stmt.name
            ));
            return;
        };

        if impl_visibility == Visibility::Public && type_visibility == Visibility::Private {
            self.error(format!(
                "Cannot declare public impl for private type '{}'",
                impl_stmt.name
            ));
        }

        for method in &impl_stmt.methods {
            match method {
                Stmt::Fun(fun_stmt) => {
                    let duplicate = self
                        .impl_methods
                        .entry(impl_stmt.name.clone())
                        .or_default()
                        .insert(fun_stmt.name.clone(), Visibility::Private)
                        .is_some();
                    if duplicate {
                        self.error(format!(
                            "Duplicate method '{}' in impl '{}'",
                            fun_stmt.name, impl_stmt.name
                        ));
                    }
                }
                Stmt::Pub(pub_stmt) => match pub_stmt.stmt.as_ref() {
                    Stmt::Fun(fun_stmt) => {
                        if type_visibility == Visibility::Private {
                            self.error(format!(
                                "Cannot expose public method '{}' on private type '{}'",
                                fun_stmt.name, impl_stmt.name
                            ));
                        }
                        let duplicate = self
                            .impl_methods
                            .entry(impl_stmt.name.clone())
                            .or_default()
                            .insert(fun_stmt.name.clone(), Visibility::Public)
                            .is_some();
                        if duplicate {
                            self.error(format!(
                                "Duplicate method '{}' in impl '{}'",
                                fun_stmt.name, impl_stmt.name
                            ));
                        }
                    }
                    _ => self.error("Impl blocks can contain only functions or pub functions"),
                },
                _ => self.error("Impl blocks can contain only functions or pub functions"),
            }
        }
    }

    fn validate_use_bindings(&mut self) {
        let uses = self.uses.clone();
        for binding in uses.values() {
            let Some(root_visibility) = self.resolve_root_visibility(&binding.root) else {
                self.error(format!(
                    "Unresolved use path root '{}' in '{}'",
                    binding.root, binding.path
                ));
                continue;
            };

            if binding.visibility == Visibility::Public && root_visibility == Visibility::Private {
                self.error(format!(
                    "Cannot publicly re-export private root '{}'",
                    binding.root
                ));
            }
        }
    }

    fn resolve_root_visibility(&self, root: &str) -> Option<Visibility> {
        if matches!(root, "std" | "core" | "crate" | "self" | "super") {
            return Some(Visibility::Public);
        }

        self.modules
            .get(root)
            .copied()
            .or_else(|| self.imports.get(root).map(|binding| binding.visibility))
            .or_else(|| self.uses.get(root).map(|binding| binding.visibility))
    }

    fn binding_name_taken(&self, name: &str) -> bool {
        self.modules.contains_key(name)
            || self.imports.contains_key(name)
            || self.uses.contains_key(name)
            || self.type_defs.contains_key(name)
            || self.functions.contains_key(name)
    }

    fn type_visibility(&self, type_name: &str) -> Option<Visibility> {
        self.type_defs.get(type_name).map(|def| match def {
            TypeDef::Struct(def) => def.visibility,
            TypeDef::Enum(def) => def.visibility,
            TypeDef::Class(def) => def.visibility,
        })
    }

    fn validate_type_exists(&mut self, ty: &Type, context: &str) {
        if !self.is_known_type(ty) {
            self.error(format!("Unknown type in {}: {:?}", context, ty));
        }
    }

    fn is_known_type(&self, ty: &Type) -> bool {
        match ty {
            Type::Num | Type::Char | Type::Byte | Type::String | Type::Bool | Type::Void => true,
            Type::Array(inner) => self.is_known_type(inner),
            Type::Identifier(name) => {
                self.type_defs.contains_key(name)
                    || self.imports.contains_key(name)
                    || self.uses.contains_key(name)
            }
            Type::None => true,
        }
    }

    fn analyze_var_stmt(&mut self, var_stmt: &VarStmt) {
        if !matches!(var_stmt.var_type, Type::None) {
            self.validate_type_exists(&var_stmt.var_type, "variable type");
        }

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
        if !matches!(fun_stmt.return_type, Type::None) {
            self.validate_type_exists(&fun_stmt.return_type, "function return type");
        }
        for param in &fun_stmt.params {
            self.validate_type_exists(&param.param_type, "function parameter type");
        }

        let previous_return = self.current_return_type.clone();
        self.current_return_type = Some(fun_stmt.return_type.clone());

        self.push_scope();
        if let Some(impl_type) = &self.current_impl_type {
            self.define_symbol("this", Type::Identifier(impl_type.clone()), false);
        }
        for param in &fun_stmt.params {
            self.define_symbol(&param.name, param.param_type.clone(), true);
        }

        self.analyze_stmt(fun_stmt.block.as_ref());

        self.pop_scope();
        self.current_return_type = previous_return;
    }

    fn analyze_struct_stmt(&mut self, struct_stmt: &StructStmt) {
        if let Some(TypeDef::Struct(def)) = self.type_defs.get(&struct_stmt.name).cloned() {
            for (field_name, field_ty) in &def.fields {
                self.validate_type_exists(
                    field_ty,
                    &format!("field '{}.{}' type", struct_stmt.name, field_name),
                );
            }
        }
    }

    fn analyze_enum_stmt(&mut self, enum_stmt: &EnumStmt) {
        if let Some(TypeDef::Enum(def)) = self.type_defs.get(&enum_stmt.name).cloned() {
            for (variant_name, args) in &def.variants {
                for arg_ty in args {
                    self.validate_type_exists(
                        arg_ty,
                        &format!(
                            "variant '{}.{}' argument type",
                            enum_stmt.name, variant_name
                        ),
                    );
                }
            }
        }
    }

    fn analyze_impl_stmt(&mut self, impl_stmt: &crate::ast::ImplStmt) {
        let Some(type_visibility) = self.type_visibility(&impl_stmt.name) else {
            self.error(format!(
                "Impl target type '{}' is not declared",
                impl_stmt.name
            ));
            return;
        };

        for method in &impl_stmt.methods {
            match method {
                Stmt::Fun(fun_stmt) => {
                    let previous_impl = self.current_impl_type.clone();
                    self.current_impl_type = Some(impl_stmt.name.clone());
                    self.analyze_fun_stmt(fun_stmt);
                    self.current_impl_type = previous_impl;
                }
                Stmt::Pub(pub_stmt) => match pub_stmt.stmt.as_ref() {
                    Stmt::Fun(fun_stmt) => {
                        if type_visibility == Visibility::Private {
                            self.error(format!(
                                "Cannot expose public method '{}' on private type '{}'",
                                fun_stmt.name, impl_stmt.name
                            ));
                        }
                        let previous_impl = self.current_impl_type.clone();
                        self.current_impl_type = Some(impl_stmt.name.clone());
                        self.analyze_fun_stmt(fun_stmt);
                        self.current_impl_type = previous_impl;
                    }
                    _ => self.error("Impl blocks can contain only functions or pub functions"),
                },
                _ => self.error("Impl blocks can contain only functions or pub functions"),
            }
        }
    }

    fn analyze_match_stmt(&mut self, match_stmt: &MatchStmt) {
        let scrutinee_type = self.analyze_expr(&match_stmt.expr);
        let enum_variants = match &scrutinee_type {
            Type::Identifier(name) => match self.type_defs.get(name) {
                Some(TypeDef::Enum(enum_def)) => Some(enum_def.variants.clone()),
                _ => None,
            },
            _ => None,
        };

        let mut has_catch_all = false;
        let mut seen_enum_variants = HashSet::new();
        let mut seen_bool_literals = HashSet::new();
        for arm in &match_stmt.arms {
            self.push_scope();
            self.bind_pattern(
                &arm.pattern,
                &scrutinee_type,
                enum_variants.as_ref(),
                true,
                &mut has_catch_all,
                &mut seen_enum_variants,
                &mut seen_bool_literals,
            );
            self.analyze_stmt(arm.body.as_ref());
            self.pop_scope();
        }

        if has_catch_all {
            return;
        }

        match &scrutinee_type {
            Type::Bool => {
                if !seen_bool_literals.contains(&true) || !seen_bool_literals.contains(&false) {
                    self.error("Non-exhaustive match for Bool: expected true and false arms");
                }
            }
            Type::Identifier(enum_name) => {
                if let Some(variants) = enum_variants {
                    let missing = variants
                        .keys()
                        .filter(|variant| !seen_enum_variants.contains(*variant))
                        .cloned()
                        .collect::<Vec<_>>();

                    if !missing.is_empty() {
                        self.error(format!(
                            "Non-exhaustive match for enum '{}': missing {}",
                            enum_name,
                            missing.join(", ")
                        ));
                    }
                } else {
                    self.error(format!(
                        "Non-exhaustive match for type {:?}: add '_' arm",
                        scrutinee_type
                    ));
                }
            }
            _ => {
                self.error(format!(
                    "Non-exhaustive match for type {:?}: add '_' arm",
                    scrutinee_type
                ));
            }
        }
    }

    fn bind_pattern(
        &mut self,
        pattern: &Pattern,
        expected_type: &Type,
        enum_variants: Option<&HashMap<String, Vec<Type>>>,
        is_top_level_arm_pattern: bool,
        has_catch_all: &mut bool,
        seen_enum_variants: &mut HashSet<String>,
        seen_bool_literals: &mut HashSet<bool>,
    ) {
        match pattern {
            Pattern::Wildcard => {
                if is_top_level_arm_pattern {
                    *has_catch_all = true;
                }
            }
            Pattern::Literal(literal) => {
                let literal_ty = match literal {
                    Literal::String(_) => Type::String,
                    Literal::Char(_) => Type::Char,
                    Literal::Num(_) => Type::Num,
                    Literal::Bool(v) => {
                        seen_bool_literals.insert(*v);
                        Type::Bool
                    }
                    Literal::Identifier(_) => Type::None,
                    Literal::This => Type::Identifier("This".to_string()),
                };

                if !is_assignable(expected_type, &literal_ty)
                    && !is_assignable(&literal_ty, expected_type)
                {
                    self.error(format!(
                        "Match pattern type mismatch: expected {:?}, got {:?}",
                        expected_type, literal_ty
                    ));
                }
            }
            Pattern::Identifier(name) => {
                if is_top_level_arm_pattern {
                    *has_catch_all = true;
                }
                self.define_symbol(name, expected_type.clone(), true);
            }
            Pattern::Variant(variant_name, nested) => {
                let Some(variants) = enum_variants else {
                    self.error(format!(
                        "Variant pattern '{}' used for non-enum type {:?}",
                        variant_name, expected_type
                    ));
                    return;
                };

                let Some(arg_types) = variants.get(variant_name) else {
                    self.error(format!("Unknown enum variant '{}'", variant_name));
                    return;
                };

                seen_enum_variants.insert(variant_name.clone());

                if arg_types.len() != nested.len() {
                    self.error(format!(
                        "Variant '{}' expects {} patterns, got {}",
                        variant_name,
                        arg_types.len(),
                        nested.len()
                    ));
                    return;
                }

                for (nested_pattern, expected_ty) in nested.iter().zip(arg_types.iter()) {
                    self.bind_pattern(
                        nested_pattern,
                        expected_ty,
                        None,
                        false,
                        has_catch_all,
                        seen_enum_variants,
                        seen_bool_literals,
                    );
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
                Literal::This => {
                    if let Some(impl_type) = &self.current_impl_type {
                        Type::Identifier(impl_type.clone())
                    } else {
                        self.error("'this' used outside of impl method");
                        Type::None
                    }
                }
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
                        let left_ty = self.analyze_expr(left.as_ref());
                        if !matches!(left_ty, Type::None) && !is_assignable(&left_ty, &right_ty) {
                            self.error(format!(
                                "Assignment type mismatch for member '{}': expected {:?}, got {:?}",
                                member.property, left_ty, right_ty
                            ));
                        }
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
                let owner_ty = self.analyze_expr(member.member.as_ref());
                match owner_ty {
                    Type::Identifier(type_name) => match self.type_defs.get(&type_name) {
                        Some(TypeDef::Struct(def)) => {
                            if let Some(field_ty) = def.fields.get(&member.property) {
                                field_ty.clone()
                            } else {
                                self.error(format!(
                                    "Unknown field '{}.{}'",
                                    type_name, member.property
                                ));
                                Type::None
                            }
                        }
                        Some(TypeDef::Class(_)) => Type::None,
                        Some(TypeDef::Enum(_)) => {
                            self.error(format!(
                                "Enum '{}' has no field '{}'",
                                type_name, member.property
                            ));
                            Type::None
                        }
                        None => {
                            self.error(format!("Member access on unknown type '{}'", type_name));
                            Type::None
                        }
                    },
                    Type::None => Type::None,
                    other => {
                        self.error(format!(
                            "Type {:?} has no member '{}'",
                            other, member.property
                        ));
                        Type::None
                    }
                }
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
