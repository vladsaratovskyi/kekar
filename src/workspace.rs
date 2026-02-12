use std::{
    collections::{HashMap, HashSet},
    fs,
    path::{Path, PathBuf},
};

use crate::{
    ast::{
        ArrayExpr, BlockStmt, CallExpr, ClassStmt, ComputedExpr, ConstStmt, EnumStmt, Expr,
        ExprStmt, ForStmt, FunStmt, IfStmt, ImplStmt, ImportStmt, Literal, MatchArm, MatchStmt,
        MemberExpr, Pattern, PubStmt, ReturnStmt, Stmt, StructStmt, Type, UseStmt, VarStmt,
        WhileStmt,
    },
    lexer::Lexer,
    parser::Parser,
    sema::{SemanticAnalyzer, SemanticError},
};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum ItemKind {
    Module,
    Type,
    Symbol,
}

#[derive(Debug, Clone)]
struct ItemInfo {
    kind: ItemKind,
    public: bool,
    target_module: Option<PathBuf>,
}

#[derive(Debug, Clone)]
struct ModuleBinding {
    target: PathBuf,
    from_mod_decl: bool,
}

#[derive(Debug, Clone)]
struct UsePath {
    path: String,
    public: bool,
}

#[derive(Debug, Clone)]
struct LinkedModule {
    path: PathBuf,
    ast: BlockStmt,
    parent: Option<PathBuf>,
    bindings: HashMap<String, ModuleBinding>,
    items: HashMap<String, ItemInfo>,
    uses: Vec<UsePath>,
}

impl LinkedModule {
    fn new(path: PathBuf, ast: BlockStmt) -> Self {
        Self {
            path,
            ast,
            parent: None,
            bindings: HashMap::new(),
            items: HashMap::new(),
            uses: Vec::new(),
        }
    }
}

#[derive(Debug, Clone)]
struct DependencyRequest {
    binding_name: String,
    specifier: String,
    is_mod_decl: bool,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct TypeKey {
    module: PathBuf,
    name: String,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct FunctionKey {
    module: PathBuf,
    name: String,
}

#[derive(Debug, Clone)]
struct MethodInfo {
    visibility: bool,
    params: Vec<Type>,
    return_type: Type,
}

#[derive(Debug, Clone)]
struct TypeInfo {
    module: PathBuf,
    visibility: bool,
    fields: HashMap<String, Type>,
    methods: HashMap<String, MethodInfo>,
}

#[derive(Debug, Clone)]
struct FunctionInfo {
    visibility: bool,
    params: Vec<Type>,
    return_type: Type,
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum ValueType {
    Num,
    Char,
    Byte,
    String,
    Bool,
    Void,
    User(TypeKey),
    GenericUser(TypeKey, Vec<ValueType>),
    Array(Box<ValueType>),
    Unknown,
}

pub fn analyze_workspace(entry_path: impl AsRef<Path>) -> Result<(), Vec<SemanticError>> {
    let (linker, entry_module) = load_workspace(entry_path.as_ref())?;
    let errors = collect_workspace_errors(&linker.modules, &entry_module, linker.errors);

    if errors.is_empty() {
        Ok(())
    } else {
        Err(errors)
    }
}

pub fn build_workspace_program(entry_path: impl AsRef<Path>) -> Result<BlockStmt, Vec<SemanticError>> {
    let (linker, entry_module) = load_workspace(entry_path.as_ref())?;
    let errors = collect_workspace_errors(&linker.modules, &entry_module, linker.errors);
    if !errors.is_empty() {
        return Err(errors);
    }

    Ok(link_workspace_program(&linker.modules, &entry_module))
}

fn load_workspace(entry_path: &Path) -> Result<(WorkspaceLinker, PathBuf), Vec<SemanticError>> {
    let mut linker = WorkspaceLinker::new();
    let Some(entry_module) = linker.load_entry(entry_path) else {
        return Err(linker.errors);
    };

    Ok((linker, entry_module))
}

fn collect_workspace_errors(
    modules: &HashMap<PathBuf, LinkedModule>,
    entry_module: &Path,
    mut errors: Vec<SemanticError>,
) -> Vec<SemanticError> {
    let mut module_paths = modules.keys().cloned().collect::<Vec<_>>();
    module_paths.sort();

    for module_path in &module_paths {
        let Some(module) = modules.get(module_path) else {
            continue;
        };

        if let Err(module_errors) = SemanticAnalyzer::analyze(&module.ast) {
            for error in module_errors {
                errors.push(SemanticError::new(format!(
                    "{}: {}",
                    module.path.display(),
                    error.message
                )));
            }
        }
    }

    for module_path in &module_paths {
        let Some(module) = modules.get(module_path) else {
            continue;
        };

        for use_path in &module.uses {
            if let Err(message) = resolve_use_path(modules, module, entry_module, use_path)
            {
                errors.push(SemanticError::new(format!(
                    "{}: {}",
                    module.path.display(),
                    message
                )));
            }
        }
    }

    let (type_index, module_type_namespaces, type_errors) =
        build_type_index(modules, entry_module);
    errors.extend(type_errors);

    let (function_index, module_function_namespaces) = build_function_index(modules, entry_module);

    let method_errors = validate_module_method_calls(
        modules,
        &type_index,
        &module_type_namespaces,
        &function_index,
        &module_function_namespaces,
    );
    errors.extend(method_errors);

    errors
}

fn link_workspace_program(modules: &HashMap<PathBuf, LinkedModule>, entry_module: &Path) -> BlockStmt {
    let (function_index, module_function_namespaces) = build_function_index(modules, entry_module);

    let mut module_paths = modules.keys().cloned().collect::<Vec<_>>();
    module_paths.sort();

    let module_order = module_paths
        .iter()
        .enumerate()
        .map(|(index, path)| (path.clone(), index))
        .collect::<HashMap<_, _>>();

    let mut function_symbols = HashMap::<FunctionKey, String>::new();
    let mut function_keys = function_index.keys().cloned().collect::<Vec<_>>();
    function_keys.sort_by(|left, right| {
        left.module
            .cmp(&right.module)
            .then_with(|| left.name.cmp(&right.name))
    });
    for key in function_keys {
        function_symbols.insert(
            key.clone(),
            lower_function_symbol(&key, entry_module, &module_order),
        );
    }

    let mut stmts = Vec::new();
    for module_path in module_paths {
        let Some(module) = modules.get(&module_path) else {
            continue;
        };
        let module_function_namespace = module_function_namespaces
            .get(&module_path)
            .cloned()
            .unwrap_or_default();
        for stmt in &module.ast.stmts {
            stmts.push(rewrite_stmt_for_workspace_codegen(
                stmt,
                module,
                &module_function_namespace,
                &module_function_namespaces,
                &function_symbols,
                true,
            ));
        }
    }

    BlockStmt { stmts }
}

fn lower_function_symbol(
    function_key: &FunctionKey,
    entry_module: &Path,
    module_order: &HashMap<PathBuf, usize>,
) -> String {
    if function_key.module == entry_module && function_key.name == "main" {
        return "main".to_string();
    }

    let module_index = module_order
        .get(&function_key.module)
        .copied()
        .unwrap_or_default();
    format!("__kek_m{}_{}", module_index, function_key.name)
}

fn rewrite_stmt_for_workspace_codegen(
    stmt: &Stmt,
    module: &LinkedModule,
    module_function_namespace: &HashMap<String, FunctionKey>,
    module_function_namespaces: &HashMap<PathBuf, HashMap<String, FunctionKey>>,
    function_symbols: &HashMap<FunctionKey, String>,
    rename_top_level_functions: bool,
) -> Stmt {
    match stmt {
        Stmt::Block(block) => Stmt::Block(BlockStmt {
            stmts: block
                .stmts
                .iter()
                .map(|stmt| {
                    rewrite_stmt_for_workspace_codegen(
                        stmt,
                        module,
                        module_function_namespace,
                        module_function_namespaces,
                        function_symbols,
                        false,
                    )
                })
                .collect(),
        }),
        Stmt::Expr(expr_stmt) => Stmt::Expr(ExprStmt {
            expr: rewrite_expr_for_workspace_codegen(
                &expr_stmt.expr,
                module,
                module_function_namespace,
                module_function_namespaces,
                function_symbols,
            ),
        }),
        Stmt::Pub(pub_stmt) => Stmt::Pub(PubStmt {
            stmt: Box::new(rewrite_stmt_for_workspace_codegen(
                pub_stmt.stmt.as_ref(),
                module,
                module_function_namespace,
                module_function_namespaces,
                function_symbols,
                rename_top_level_functions,
            )),
        }),
        Stmt::Var(var_stmt) => Stmt::Var(VarStmt {
            name: var_stmt.name.clone(),
            assignment: rewrite_expr_for_workspace_codegen(
                &var_stmt.assignment,
                module,
                module_function_namespace,
                module_function_namespaces,
                function_symbols,
            ),
            var_type: var_stmt.var_type.clone(),
        }),
        Stmt::Const(const_stmt) => Stmt::Const(ConstStmt {
            name: const_stmt.name.clone(),
            assignment: rewrite_expr_for_workspace_codegen(
                &const_stmt.assignment,
                module,
                module_function_namespace,
                module_function_namespaces,
                function_symbols,
            ),
            const_type: const_stmt.const_type.clone(),
        }),
        Stmt::Struct(struct_stmt) => Stmt::Struct(StructStmt {
            name: struct_stmt.name.clone(),
            fields: struct_stmt.fields.clone(),
            methods: struct_stmt
                .methods
                .iter()
                .map(|method| {
                    rewrite_stmt_for_workspace_codegen(
                        method,
                        module,
                        module_function_namespace,
                        module_function_namespaces,
                        function_symbols,
                        false,
                    )
                })
                .collect(),
        }),
        Stmt::Enum(enum_stmt) => Stmt::Enum(EnumStmt {
            name: enum_stmt.name.clone(),
            variants: enum_stmt.variants.clone(),
        }),
        Stmt::Impl(impl_stmt) => Stmt::Impl(ImplStmt {
            name: impl_stmt.name.clone(),
            methods: impl_stmt
                .methods
                .iter()
                .map(|method| {
                    rewrite_stmt_for_workspace_codegen(
                        method,
                        module,
                        module_function_namespace,
                        module_function_namespaces,
                        function_symbols,
                        false,
                    )
                })
                .collect(),
        }),
        Stmt::If(if_stmt) => Stmt::If(IfStmt {
            condition: rewrite_expr_for_workspace_codegen(
                &if_stmt.condition,
                module,
                module_function_namespace,
                module_function_namespaces,
                function_symbols,
            ),
            then_block: Box::new(rewrite_stmt_for_workspace_codegen(
                if_stmt.then_block.as_ref(),
                module,
                module_function_namespace,
                module_function_namespaces,
                function_symbols,
                false,
            )),
            else_block: Box::new(rewrite_stmt_for_workspace_codegen(
                if_stmt.else_block.as_ref(),
                module,
                module_function_namespace,
                module_function_namespaces,
                function_symbols,
                false,
            )),
        }),
        Stmt::Match(match_stmt) => Stmt::Match(MatchStmt {
            expr: rewrite_expr_for_workspace_codegen(
                &match_stmt.expr,
                module,
                module_function_namespace,
                module_function_namespaces,
                function_symbols,
            ),
            arms: match_stmt
                .arms
                .iter()
                .map(|arm| MatchArm {
                    pattern: arm.pattern.clone(),
                    body: Box::new(rewrite_stmt_for_workspace_codegen(
                        arm.body.as_ref(),
                        module,
                        module_function_namespace,
                        module_function_namespaces,
                        function_symbols,
                        false,
                    )),
                })
                .collect(),
        }),
        Stmt::While(while_stmt) => Stmt::While(WhileStmt {
            condition: rewrite_expr_for_workspace_codegen(
                &while_stmt.condition,
                module,
                module_function_namespace,
                module_function_namespaces,
                function_symbols,
            ),
            body: Box::new(rewrite_stmt_for_workspace_codegen(
                while_stmt.body.as_ref(),
                module,
                module_function_namespace,
                module_function_namespaces,
                function_symbols,
                false,
            )),
        }),
        Stmt::For(for_stmt) => Stmt::For(ForStmt {
            item: for_stmt.item.clone(),
            index: for_stmt.index.clone(),
            iterator: rewrite_expr_for_workspace_codegen(
                &for_stmt.iterator,
                module,
                module_function_namespace,
                module_function_namespaces,
                function_symbols,
            ),
            body: Box::new(rewrite_stmt_for_workspace_codegen(
                for_stmt.body.as_ref(),
                module,
                module_function_namespace,
                module_function_namespaces,
                function_symbols,
                false,
            )),
        }),
        Stmt::Fun(fun_stmt) => {
            let mut rewritten = FunStmt {
                name: fun_stmt.name.clone(),
                return_type: fun_stmt.return_type.clone(),
                params: fun_stmt.params.clone(),
                block: Box::new(rewrite_stmt_for_workspace_codegen(
                    fun_stmt.block.as_ref(),
                    module,
                    module_function_namespace,
                    module_function_namespaces,
                    function_symbols,
                    false,
                )),
            };

            if rename_top_level_functions {
                if let Some(symbol) = resolve_function_symbol_in_namespace(
                    &fun_stmt.name,
                    module_function_namespace,
                    function_symbols,
                ) {
                    rewritten.name = symbol;
                }
            }

            Stmt::Fun(rewritten)
        }
        Stmt::Class(class_stmt) => Stmt::Class(ClassStmt {
            name: class_stmt.name.clone(),
            block: Box::new(rewrite_stmt_for_workspace_codegen(
                class_stmt.block.as_ref(),
                module,
                module_function_namespace,
                module_function_namespaces,
                function_symbols,
                false,
            )),
        }),
        Stmt::Return(return_stmt) => Stmt::Return(ReturnStmt {
            return_expr: rewrite_expr_for_workspace_codegen(
                &return_stmt.return_expr,
                module,
                module_function_namespace,
                module_function_namespaces,
                function_symbols,
            ),
        }),
        _ => stmt.clone(),
    }
}

fn rewrite_expr_for_workspace_codegen(
    expr: &Expr,
    module: &LinkedModule,
    module_function_namespace: &HashMap<String, FunctionKey>,
    module_function_namespaces: &HashMap<PathBuf, HashMap<String, FunctionKey>>,
    function_symbols: &HashMap<FunctionKey, String>,
) -> Expr {
    match expr {
        Expr::Unary(token, right) => Expr::Unary(
            token.clone(),
            Box::new(rewrite_expr_for_workspace_codegen(
                right,
                module,
                module_function_namespace,
                module_function_namespaces,
                function_symbols,
            )),
        ),
        Expr::Binary(left, token, right) => Expr::Binary(
            Box::new(rewrite_expr_for_workspace_codegen(
                left,
                module,
                module_function_namespace,
                module_function_namespaces,
                function_symbols,
            )),
            token.clone(),
            Box::new(rewrite_expr_for_workspace_codegen(
                right,
                module,
                module_function_namespace,
                module_function_namespaces,
                function_symbols,
            )),
        ),
        Expr::Assignment(left, right) => Expr::Assignment(
            Box::new(rewrite_expr_for_workspace_codegen(
                left,
                module,
                module_function_namespace,
                module_function_namespaces,
                function_symbols,
            )),
            Box::new(rewrite_expr_for_workspace_codegen(
                right,
                module,
                module_function_namespace,
                module_function_namespaces,
                function_symbols,
            )),
        ),
        Expr::Call(call) => {
            let rewritten_args = call
                .arguments
                .iter()
                .map(|arg| {
                    rewrite_expr_for_workspace_codegen(
                        arg,
                        module,
                        module_function_namespace,
                        module_function_namespaces,
                        function_symbols,
                    )
                })
                .collect::<Vec<_>>();

            if let Expr::Literal(Literal::Identifier(name)) = call.callee.as_ref() {
                if let Some(symbol) = resolve_function_symbol_in_namespace(
                    name,
                    module_function_namespace,
                    function_symbols,
                ) {
                    return Expr::Call(CallExpr {
                        callee: Box::new(Expr::Literal(Literal::Identifier(symbol))),
                        arguments: rewritten_args,
                    });
                }
            }

            if let Expr::Mebmer(member) = call.callee.as_ref() {
                if let Some(symbol) = resolve_module_qualified_function_symbol(
                    module,
                    member,
                    module_function_namespaces,
                    function_symbols,
                ) {
                    return Expr::Call(CallExpr {
                        callee: Box::new(Expr::Literal(Literal::Identifier(symbol))),
                        arguments: rewritten_args,
                    });
                }
            }

            Expr::Call(CallExpr {
                callee: Box::new(rewrite_expr_for_workspace_codegen(
                    call.callee.as_ref(),
                    module,
                    module_function_namespace,
                    module_function_namespaces,
                    function_symbols,
                )),
                arguments: rewritten_args,
            })
        }
        Expr::Mebmer(member) => Expr::Mebmer(MemberExpr {
            member: Box::new(rewrite_expr_for_workspace_codegen(
                member.member.as_ref(),
                module,
                module_function_namespace,
                module_function_namespaces,
                function_symbols,
            )),
            property: member.property.clone(),
        }),
        Expr::ComputedExpr(computed) => Expr::ComputedExpr(ComputedExpr {
            member: Box::new(rewrite_expr_for_workspace_codegen(
                computed.member.as_ref(),
                module,
                module_function_namespace,
                module_function_namespaces,
                function_symbols,
            )),
            property: Box::new(rewrite_expr_for_workspace_codegen(
                computed.property.as_ref(),
                module,
                module_function_namespace,
                module_function_namespaces,
                function_symbols,
            )),
        }),
        Expr::Array(array) => Expr::Array(ArrayExpr {
            array: array
                .array
                .iter()
                .map(|item| {
                    rewrite_expr_for_workspace_codegen(
                        item,
                        module,
                        module_function_namespace,
                        module_function_namespaces,
                        function_symbols,
                    )
                })
                .collect(),
        }),
        _ => expr.clone(),
    }
}

fn resolve_function_symbol_in_namespace(
    function_name: &str,
    module_function_namespace: &HashMap<String, FunctionKey>,
    function_symbols: &HashMap<FunctionKey, String>,
) -> Option<String> {
    let function_key = module_function_namespace.get(function_name)?;
    function_symbols.get(function_key).cloned()
}

fn resolve_module_qualified_function_symbol(
    module: &LinkedModule,
    member: &MemberExpr,
    module_function_namespaces: &HashMap<PathBuf, HashMap<String, FunctionKey>>,
    function_symbols: &HashMap<FunctionKey, String>,
) -> Option<String> {
    let Expr::Literal(Literal::Identifier(binding_name)) = member.member.as_ref() else {
        return None;
    };

    let target_module = module.bindings.get(binding_name)?.target.clone();
    let target_namespace = module_function_namespaces.get(&target_module)?;
    let function_key = target_namespace.get(&member.property)?;
    function_symbols.get(function_key).cloned()
}

struct WorkspaceLinker {
    modules: HashMap<PathBuf, LinkedModule>,
    loading: HashSet<PathBuf>,
    errors: Vec<SemanticError>,
}

impl WorkspaceLinker {
    fn new() -> Self {
        Self {
            modules: HashMap::new(),
            loading: HashSet::new(),
            errors: Vec::new(),
        }
    }

    fn load_entry(&mut self, entry_path: &Path) -> Option<PathBuf> {
        let resolved = self.resolve_existing_file(
            entry_path,
            format!("Entry module '{}' not found", entry_path.display()),
        )?;
        self.load_module(&resolved)
    }

    fn load_module(&mut self, module_path: &Path) -> Option<PathBuf> {
        let canonical = self.resolve_existing_file(
            module_path,
            format!("Module '{}' not found", module_path.display()),
        )?;

        if self.modules.contains_key(&canonical) {
            return Some(canonical);
        }

        if self.loading.contains(&canonical) {
            return Some(canonical);
        }

        self.loading.insert(canonical.clone());

        let ast = match parse_file_to_ast(&canonical) {
            Ok(ast) => ast,
            Err(error) => {
                self.errors.push(error);
                self.loading.remove(&canonical);
                return None;
            }
        };

        self.modules
            .insert(canonical.clone(), LinkedModule::new(canonical.clone(), ast));

        let dependency_requests = {
            let module = self
                .modules
                .get(&canonical)
                .expect("linked module should exist");
            collect_dependency_requests(&module.ast)
        };

        let mut bindings = HashMap::new();
        let mut mod_children = Vec::new();

        for dependency in dependency_requests {
            let resolved_dependency = if dependency.is_mod_decl {
                self.resolve_mod_decl_path(&canonical, &dependency.specifier)
            } else {
                self.resolve_import_path(&canonical, &dependency.specifier)
            };

            let target_path = match resolved_dependency {
                Ok(path) => path,
                Err(message) => {
                    self.errors.push(SemanticError::new(message));
                    continue;
                }
            };

            let Some(target_module) = self.load_module(&target_path) else {
                continue;
            };

            if dependency.is_mod_decl {
                mod_children.push(target_module.clone());
            }

            bindings
                .entry(dependency.binding_name.clone())
                .or_insert(ModuleBinding {
                    target: target_module,
                    from_mod_decl: dependency.is_mod_decl,
                });
        }

        if let Some(module) = self.modules.get_mut(&canonical) {
            module.bindings = bindings;
            module.items = collect_module_items(&module.ast, &module.bindings);
            module.uses = collect_use_paths(&module.ast);
        }

        for child in mod_children {
            if let Some(child_module) = self.modules.get_mut(&child) {
                if child_module.parent.is_none() {
                    child_module.parent = Some(canonical.clone());
                }
            }
        }

        self.loading.remove(&canonical);
        Some(canonical)
    }

    fn resolve_mod_decl_path(
        &self,
        current_module_path: &Path,
        module_name: &str,
    ) -> Result<PathBuf, String> {
        let Some(base_dir) = current_module_path.parent() else {
            return Err(format!(
                "Unable to resolve module '{}' from '{}'",
                module_name,
                current_module_path.display()
            ));
        };

        let candidates = vec![
            base_dir.join(format!("{}.kek", module_name)),
            base_dir.join(module_name).join("mod.kek"),
        ];

        self.resolve_first_existing(
            &candidates,
            format!(
                "Unable to resolve module '{}' from '{}'",
                module_name,
                current_module_path.display()
            ),
        )
    }

    fn resolve_import_path(
        &self,
        current_module_path: &Path,
        import_specifier: &str,
    ) -> Result<PathBuf, String> {
        let Some(base_dir) = current_module_path.parent() else {
            return Err(format!(
                "Unable to resolve import '{}' from '{}'",
                import_specifier,
                current_module_path.display()
            ));
        };

        let import_path = Path::new(import_specifier);
        let mut candidates = Vec::new();

        if import_path.is_absolute() {
            candidates.push(import_path.to_path_buf());
        } else {
            let joined = base_dir.join(import_path);
            candidates.push(joined.clone());
            if joined.extension().is_none() {
                candidates.push(joined.with_extension("kek"));
                candidates.push(joined.join("mod.kek"));
            }
        }

        dedup_paths(&mut candidates);
        self.resolve_first_existing(
            &candidates,
            format!(
                "Unable to resolve import '{}' from '{}'",
                import_specifier,
                current_module_path.display()
            ),
        )
    }

    fn resolve_first_existing(
        &self,
        candidates: &[PathBuf],
        context_error: String,
    ) -> Result<PathBuf, String> {
        for candidate in candidates {
            if candidate.is_file() {
                return fs::canonicalize(candidate).map_err(|error| {
                    format!(
                        "Failed to canonicalize '{}': {}",
                        candidate.display(),
                        error
                    )
                });
            }
        }

        let searched = candidates
            .iter()
            .map(|path| path.display().to_string())
            .collect::<Vec<_>>()
            .join(", ");
        Err(format!("{context_error}. Searched: [{searched}]"))
    }

    fn resolve_existing_file(&mut self, path: &Path, missing_message: String) -> Option<PathBuf> {
        if !path.is_file() {
            self.errors.push(SemanticError::new(missing_message));
            return None;
        }

        match fs::canonicalize(path) {
            Ok(canonical) => Some(canonical),
            Err(error) => {
                self.errors.push(SemanticError::new(format!(
                    "Failed to canonicalize '{}': {}",
                    path.display(),
                    error
                )));
                None
            }
        }
    }
}

fn parse_file_to_ast(path: &Path) -> Result<BlockStmt, SemanticError> {
    let source = fs::read_to_string(path).map_err(|error| {
        SemanticError::new(format!(
            "Failed to read module '{}': {}",
            path.display(),
            error
        ))
    })?;

    let mut lexer = Lexer::from_source(source);
    let tokens = lexer.lex_with_diagnostics().map_err(|errors| {
        let details = errors
            .into_iter()
            .map(|error| format!("{}:{}: {}", error.line, error.column, error.message))
            .collect::<Vec<_>>()
            .join("; ");
        SemanticError::new(format!(
            "Failed to lex module '{}': {}",
            path.display(),
            details
        ))
    })?;

    let mut parser = Parser::new(tokens);
    parser.parse_checked().map_err(|errors| {
        let details = errors
            .into_iter()
            .map(|error| {
                let near = error
                    .token
                    .map(|token| format!(" near {:?}", token))
                    .unwrap_or_default();
                format!("token #{}{}: {}", error.token_index, near, error.message)
            })
            .collect::<Vec<_>>()
            .join("; ");
        SemanticError::new(format!(
            "Failed to parse module '{}': {}",
            path.display(),
            details
        ))
    })
}

fn resolve_use_path(
    modules: &HashMap<PathBuf, LinkedModule>,
    module: &LinkedModule,
    entry_module: &Path,
    use_path: &UsePath,
) -> Result<(), String> {
    let segments = use_path
        .path
        .split("::")
        .filter(|segment| !segment.is_empty())
        .collect::<Vec<_>>();

    if segments.is_empty() {
        return Err("Use declaration path is empty".to_string());
    }

    let root = segments[0];

    if matches!(root, "std" | "core") {
        return Ok(());
    }

    let mut current_module_path;
    let mut segment_index = 1usize;

    match root {
        "crate" => {
            current_module_path = entry_module.to_path_buf();
        }
        "self" => {
            current_module_path = module.path.clone();
        }
        "super" => {
            let Some(parent) = module.parent.clone() else {
                return Err(format!(
                    "Use path root 'super' is not available in root module '{}'",
                    use_path.path
                ));
            };
            current_module_path = parent;
        }
        _ => {
            let Some(root_item) = module.items.get(root) else {
                return Err(format!(
                    "Unresolved use path root '{}' in '{}'",
                    root, use_path.path
                ));
            };

            if use_path.public && !root_item.public {
                return Err(format!(
                    "Cannot publicly re-export private path '{}'",
                    use_path.path
                ));
            }

            if root_item.kind != ItemKind::Module {
                if segments.len() > 1 {
                    return Err(format!(
                        "Use path root '{}' in '{}' is not a module",
                        root, use_path.path
                    ));
                }
                return Ok(());
            }

            let Some(target_module) = root_item.target_module.clone() else {
                return Err(format!(
                    "Module root '{}' in '{}' has no resolved file target",
                    root, use_path.path
                ));
            };

            current_module_path = target_module;
        }
    }

    if segments.len() <= segment_index {
        return Ok(());
    }

    while segment_index < segments.len() {
        let segment = segments[segment_index];
        let last = segment_index + 1 == segments.len();

        let Some(current_module) = modules.get(&current_module_path) else {
            return Err(format!(
                "Resolved module '{}' is missing while checking '{}'",
                current_module_path.display(),
                use_path.path
            ));
        };

        let Some(item) = current_module.items.get(segment) else {
            return Err(format!(
                "Unresolved use path segment '{}' in '{}'",
                segment, use_path.path
            ));
        };

        if use_path.public && !item.public {
            return Err(format!(
                "Cannot publicly re-export private path '{}'",
                use_path.path
            ));
        }

        if last {
            return Ok(());
        }

        if item.kind != ItemKind::Module {
            return Err(format!(
                "Use path segment '{}' in '{}' is not a module",
                segment, use_path.path
            ));
        }

        let Some(target_module) = item.target_module.clone() else {
            return Err(format!(
                "Module segment '{}' in '{}' has no resolved file target",
                segment, use_path.path
            ));
        };
        current_module_path = target_module;
        segment_index += 1;
    }

    Ok(())
}

fn build_type_index(
    modules: &HashMap<PathBuf, LinkedModule>,
    entry_module: &Path,
) -> (
    HashMap<TypeKey, TypeInfo>,
    HashMap<PathBuf, HashMap<String, TypeKey>>,
    Vec<SemanticError>,
) {
    let mut errors = Vec::new();
    let mut type_index = HashMap::<TypeKey, TypeInfo>::new();
    let mut module_type_namespaces = HashMap::<PathBuf, HashMap<String, TypeKey>>::new();
    let mut local_type_maps = HashMap::<PathBuf, HashMap<String, TypeKey>>::new();

    for (module_path, module) in modules {
        let mut local_types = HashMap::new();
        for stmt in &module.ast.stmts {
            let (inner, public) = strip_pub(stmt);
            match inner {
                Stmt::Struct(struct_stmt) => {
                    let key = TypeKey {
                        module: module_path.clone(),
                        name: struct_stmt.name.clone(),
                    };
                    let mut methods = HashMap::new();
                    for method in &struct_stmt.methods {
                        let method_fun = match method {
                            Stmt::Fun(fun_stmt) => Some((fun_stmt, false)),
                            Stmt::Pub(pub_stmt) => match pub_stmt.stmt.as_ref() {
                                Stmt::Fun(fun_stmt) => Some((fun_stmt, true)),
                                _ => None,
                            },
                            _ => None,
                        };
                        let Some((fun_stmt, public_method)) = method_fun else {
                            continue;
                        };
                        methods.insert(
                            fun_stmt.name.clone(),
                            MethodInfo {
                                visibility: public_method,
                                params: fun_stmt
                                    .params
                                    .iter()
                                    .map(|param| param.param_type.clone())
                                    .collect(),
                                return_type: fun_stmt.return_type.clone(),
                            },
                        );
                    }
                    local_types.insert(struct_stmt.name.clone(), key.clone());
                    type_index.insert(
                        key.clone(),
                        TypeInfo {
                            module: module_path.clone(),
                            visibility: public,
                            fields: struct_stmt
                                .fields
                                .iter()
                                .map(|field| (field.name.clone(), field.field_type.clone()))
                                .collect(),
                            methods,
                        },
                    );
                }
                Stmt::Enum(enum_stmt) => {
                    let key = TypeKey {
                        module: module_path.clone(),
                        name: enum_stmt.name.clone(),
                    };
                    local_types.insert(enum_stmt.name.clone(), key.clone());
                    type_index.insert(
                        key,
                        TypeInfo {
                            module: module_path.clone(),
                            visibility: public,
                            fields: HashMap::new(),
                            methods: HashMap::new(),
                        },
                    );
                }
                Stmt::Class(class_stmt) => {
                    let key = TypeKey {
                        module: module_path.clone(),
                        name: class_stmt.name.clone(),
                    };
                    let mut fields = HashMap::new();
                    let mut methods = HashMap::new();
                    if let Stmt::Block(block) = class_stmt.block.as_ref() {
                        for member in &block.stmts {
                            match member {
                                Stmt::Var(var_stmt) => {
                                    fields.insert(var_stmt.name.clone(), var_stmt.var_type.clone());
                                }
                                Stmt::Fun(fun_stmt) => {
                                    methods.insert(
                                        fun_stmt.name.clone(),
                                        MethodInfo {
                                            visibility: false,
                                            params: fun_stmt
                                                .params
                                                .iter()
                                                .map(|param| param.param_type.clone())
                                                .collect(),
                                            return_type: fun_stmt.return_type.clone(),
                                        },
                                    );
                                }
                                Stmt::Pub(pub_stmt) => match pub_stmt.stmt.as_ref() {
                                    Stmt::Var(var_stmt) => {
                                        fields.insert(
                                            var_stmt.name.clone(),
                                            var_stmt.var_type.clone(),
                                        );
                                    }
                                    Stmt::Fun(fun_stmt) => {
                                        methods.insert(
                                            fun_stmt.name.clone(),
                                            MethodInfo {
                                                visibility: true,
                                                params: fun_stmt
                                                    .params
                                                    .iter()
                                                    .map(|param| param.param_type.clone())
                                                    .collect(),
                                                return_type: fun_stmt.return_type.clone(),
                                            },
                                        );
                                    }
                                    _ => {}
                                },
                                _ => {}
                            }
                        }
                    }
                    local_types.insert(class_stmt.name.clone(), key.clone());
                    type_index.insert(
                        key,
                        TypeInfo {
                            module: module_path.clone(),
                            visibility: public,
                            fields,
                            methods,
                        },
                    );
                }
                _ => {}
            }
        }

        local_type_maps.insert(module_path.clone(), local_types.clone());
        module_type_namespaces.insert(module_path.clone(), local_types);
    }

    for (module_path, module) in modules {
        for stmt in &module.ast.stmts {
            let (inner, _) = strip_pub(stmt);
            if let Stmt::Impl(impl_stmt) = inner {
                let Some(type_key) = local_type_maps
                    .get(module_path)
                    .and_then(|types| types.get(&impl_stmt.name))
                    .cloned()
                else {
                    continue;
                };

                let Some(type_info) = type_index.get_mut(&type_key) else {
                    continue;
                };

                for method in &impl_stmt.methods {
                    match method {
                        Stmt::Fun(fun_stmt) => {
                            type_info.methods.insert(
                                fun_stmt.name.clone(),
                                MethodInfo {
                                    visibility: false,
                                    params: fun_stmt
                                        .params
                                        .iter()
                                        .map(|param| param.param_type.clone())
                                        .collect(),
                                    return_type: fun_stmt.return_type.clone(),
                                },
                            );
                        }
                        Stmt::Pub(pub_stmt) => {
                            if let Stmt::Fun(fun_stmt) = pub_stmt.stmt.as_ref() {
                                type_info.methods.insert(
                                    fun_stmt.name.clone(),
                                    MethodInfo {
                                        visibility: true,
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
                        _ => {}
                    }
                }
            }
        }
    }

    for (module_path, module) in modules {
        let namespace = module_type_namespaces
            .entry(module_path.clone())
            .or_default();

        for stmt in &module.ast.stmts {
            let (inner, _) = strip_pub(stmt);
            if let Stmt::Use(use_stmt) = inner {
                if let Some(type_key) = resolve_type_path(
                    modules,
                    module,
                    entry_module,
                    &use_stmt.path,
                    &type_index,
                    false,
                ) {
                    namespace.insert(use_binding_name(use_stmt), type_key);
                }
            }
        }
    }

    for type_info in type_index.values() {
        let Some(namespace) = module_type_namespaces.get(&type_info.module) else {
            continue;
        };

        for (field_name, field_type) in &type_info.fields {
            if type_to_value_type(field_type, namespace).is_none() {
                errors.push(SemanticError::new(format!(
                    "{}: Unknown field type {:?} for '{}.{}'",
                    type_info.module.display(),
                    field_type,
                    type_info.module.display(),
                    field_name
                )));
            }
        }
    }

    (type_index, module_type_namespaces, errors)
}

fn build_function_index(
    modules: &HashMap<PathBuf, LinkedModule>,
    entry_module: &Path,
) -> (
    HashMap<FunctionKey, FunctionInfo>,
    HashMap<PathBuf, HashMap<String, FunctionKey>>,
) {
    let mut function_index = HashMap::<FunctionKey, FunctionInfo>::new();
    let mut module_function_namespaces = HashMap::<PathBuf, HashMap<String, FunctionKey>>::new();

    for (module_path, module) in modules {
        let mut local_functions = HashMap::new();
        for stmt in &module.ast.stmts {
            let (inner, public) = strip_pub(stmt);
            if let Stmt::Fun(fun_stmt) = inner {
                let key = FunctionKey {
                    module: module_path.clone(),
                    name: fun_stmt.name.clone(),
                };
                local_functions.insert(fun_stmt.name.clone(), key.clone());
                function_index.entry(key).or_insert(FunctionInfo {
                    visibility: public,
                    params: fun_stmt
                        .params
                        .iter()
                        .map(|param| param.param_type.clone())
                        .collect(),
                    return_type: fun_stmt.return_type.clone(),
                });
            }
        }
        module_function_namespaces.insert(module_path.clone(), local_functions);
    }

    for (module_path, module) in modules {
        let namespace = module_function_namespaces
            .entry(module_path.clone())
            .or_default();

        for stmt in &module.ast.stmts {
            let (inner, _) = strip_pub(stmt);
            if let Stmt::Use(use_stmt) = inner {
                if let Some(function_key) = resolve_function_path(
                    modules,
                    module,
                    entry_module,
                    &use_stmt.path,
                    &function_index,
                ) {
                    namespace.insert(use_binding_name(use_stmt), function_key);
                }
            }
        }
    }

    (function_index, module_function_namespaces)
}

fn validate_module_method_calls(
    modules: &HashMap<PathBuf, LinkedModule>,
    type_index: &HashMap<TypeKey, TypeInfo>,
    module_type_namespaces: &HashMap<PathBuf, HashMap<String, TypeKey>>,
    function_index: &HashMap<FunctionKey, FunctionInfo>,
    module_function_namespaces: &HashMap<PathBuf, HashMap<String, FunctionKey>>,
) -> Vec<SemanticError> {
    let mut errors = Vec::new();
    let mut module_paths = modules.keys().cloned().collect::<Vec<_>>();
    module_paths.sort();

    for module_path in module_paths {
        let Some(module) = modules.get(&module_path) else {
            continue;
        };
        let Some(type_namespace) = module_type_namespaces.get(&module_path) else {
            continue;
        };
        let Some(function_namespace) = module_function_namespaces.get(&module_path) else {
            continue;
        };

        let mut resolver = MethodCallResolver::new(
            module,
            type_namespace.clone(),
            type_index,
            module_type_namespaces,
            module_function_namespaces,
            function_namespace.clone(),
            function_index,
        );
        resolver.analyze_module();
        errors.extend(resolver.errors);
    }

    errors
}

struct MethodCallResolver<'a> {
    module: &'a LinkedModule,
    module_type_namespace: HashMap<String, TypeKey>,
    type_index: &'a HashMap<TypeKey, TypeInfo>,
    module_type_namespaces: &'a HashMap<PathBuf, HashMap<String, TypeKey>>,
    module_function_namespaces: &'a HashMap<PathBuf, HashMap<String, FunctionKey>>,
    module_function_namespace: HashMap<String, FunctionKey>,
    function_index: &'a HashMap<FunctionKey, FunctionInfo>,
    scopes: Vec<HashMap<String, ValueType>>,
    current_impl_type: Option<TypeKey>,
    errors: Vec<SemanticError>,
}

impl<'a> MethodCallResolver<'a> {
    fn new(
        module: &'a LinkedModule,
        module_type_namespace: HashMap<String, TypeKey>,
        type_index: &'a HashMap<TypeKey, TypeInfo>,
        module_type_namespaces: &'a HashMap<PathBuf, HashMap<String, TypeKey>>,
        module_function_namespaces: &'a HashMap<PathBuf, HashMap<String, FunctionKey>>,
        module_function_namespace: HashMap<String, FunctionKey>,
        function_index: &'a HashMap<FunctionKey, FunctionInfo>,
    ) -> Self {
        Self {
            module,
            module_type_namespace,
            type_index,
            module_type_namespaces,
            module_function_namespaces,
            module_function_namespace,
            function_index,
            scopes: vec![HashMap::new()],
            current_impl_type: None,
            errors: Vec::new(),
        }
    }

    fn analyze_module(&mut self) {
        for stmt in &self.module.ast.stmts {
            self.analyze_top_level_stmt(stmt);
        }
    }

    fn analyze_top_level_stmt(&mut self, stmt: &Stmt) {
        let (inner, _) = strip_pub(stmt);
        match inner {
            Stmt::Fun(fun_stmt) => self.analyze_fun(fun_stmt),
            Stmt::Struct(struct_stmt) => {
                let struct_type = self
                    .module_type_namespace
                    .get(&struct_stmt.name)
                    .cloned()
                    .or_else(|| {
                        self.type_index.keys().find_map(|key| {
                            if key.module == self.module.path && key.name == struct_stmt.name {
                                Some(key.clone())
                            } else {
                                None
                            }
                        })
                    });

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

                    let previous_impl = self.current_impl_type.clone();
                    self.current_impl_type = struct_type.clone();
                    self.analyze_fun(fun_stmt);
                    self.current_impl_type = previous_impl;
                }
            }
            Stmt::Impl(impl_stmt) => {
                let impl_type = self
                    .module_type_namespace
                    .get(&impl_stmt.name)
                    .cloned()
                    .or_else(|| {
                        self.type_index.keys().find_map(|key| {
                            if key.module == self.module.path && key.name == impl_stmt.name {
                                Some(key.clone())
                            } else {
                                None
                            }
                        })
                    });

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

                    let previous_impl = self.current_impl_type.clone();
                    self.current_impl_type = impl_type.clone();
                    self.analyze_fun(fun_stmt);
                    self.current_impl_type = previous_impl;
                }
            }
            _ => {}
        }
    }

    fn analyze_fun(&mut self, fun_stmt: &FunStmt) {
        self.push_scope();

        if let Some(type_key) = &self.current_impl_type {
            self.define_symbol("this", ValueType::User(type_key.clone()));
        }

        for param in &fun_stmt.params {
            self.define_symbol(
                &param.name,
                self.resolve_declared_type(&param.param_type, &self.module.path),
            );
        }

        self.analyze_stmt(fun_stmt.block.as_ref());

        self.pop_scope();
    }

    fn analyze_stmt(&mut self, stmt: &Stmt) {
        let (inner, _) = strip_pub(stmt);
        match inner {
            Stmt::Block(block) => {
                self.push_scope();
                for child in &block.stmts {
                    self.analyze_stmt(child);
                }
                self.pop_scope();
            }
            Stmt::Var(var_stmt) => {
                let declared = if matches!(var_stmt.var_type, Type::None) {
                    ValueType::Unknown
                } else {
                    self.resolve_declared_type(&var_stmt.var_type, &self.module.path)
                };
                let inferred = self.analyze_expr(&var_stmt.assignment);
                let final_ty = if matches!(declared, ValueType::Unknown) {
                    inferred
                } else {
                    declared
                };
                self.define_symbol(&var_stmt.name, final_ty);
            }
            Stmt::Const(const_stmt) => {
                let declared = if matches!(const_stmt.const_type, Type::None) {
                    ValueType::Unknown
                } else {
                    self.resolve_declared_type(&const_stmt.const_type, &self.module.path)
                };
                let inferred = self.analyze_expr(&const_stmt.assignment);
                let final_ty = if matches!(declared, ValueType::Unknown) {
                    inferred
                } else {
                    declared
                };
                self.define_symbol(&const_stmt.name, final_ty);
            }
            Stmt::If(if_stmt) => {
                self.analyze_expr(&if_stmt.condition);
                self.analyze_stmt(if_stmt.then_block.as_ref());
                self.analyze_stmt(if_stmt.else_block.as_ref());
            }
            Stmt::While(while_stmt) => {
                self.analyze_expr(&while_stmt.condition);
                self.analyze_stmt(while_stmt.body.as_ref());
            }
            Stmt::For(for_stmt) => {
                self.analyze_expr(&for_stmt.iterator);
                self.push_scope();
                self.define_symbol(&for_stmt.item, ValueType::Unknown);
                if let Some(index) = &for_stmt.index {
                    self.define_symbol(index, ValueType::Num);
                }
                self.analyze_stmt(for_stmt.body.as_ref());
                self.pop_scope();
            }
            Stmt::Match(match_stmt) => self.analyze_match(match_stmt),
            Stmt::Return(return_stmt) => {
                self.analyze_expr(&return_stmt.return_expr);
            }
            Stmt::Expr(expr_stmt) => {
                self.analyze_expr(&expr_stmt.expr);
            }
            _ => {}
        }
    }

    fn analyze_match(&mut self, match_stmt: &MatchStmt) {
        let scrutinee_type = self.analyze_expr(&match_stmt.expr);
        for arm in &match_stmt.arms {
            self.push_scope();
            self.bind_pattern(&arm.pattern, &scrutinee_type);
            self.analyze_stmt(arm.body.as_ref());
            self.pop_scope();
        }
    }

    fn bind_pattern(&mut self, pattern: &Pattern, expected_type: &ValueType) {
        match pattern {
            Pattern::Identifier(name) => self.define_symbol(name, expected_type.clone()),
            Pattern::Variant(_, nested) => {
                for nested_pattern in nested {
                    self.bind_pattern(nested_pattern, &ValueType::Unknown);
                }
            }
            _ => {}
        }
    }

    fn analyze_expr(&mut self, expr: &Expr) -> ValueType {
        match expr {
            Expr::Literal(Literal::Num(_)) => ValueType::Num,
            Expr::Literal(Literal::Char(_)) => ValueType::Char,
            Expr::Literal(Literal::String(_)) => ValueType::String,
            Expr::Literal(Literal::Bool(_)) => ValueType::Bool,
            Expr::Literal(Literal::Identifier(name)) => self
                .lookup_symbol(name)
                .cloned()
                .or_else(|| {
                    self.module_type_namespace
                        .get(name)
                        .cloned()
                        .map(ValueType::User)
                })
                .unwrap_or(ValueType::Unknown),
            Expr::Literal(Literal::This) => self
                .current_impl_type
                .clone()
                .map(ValueType::User)
                .unwrap_or(ValueType::Unknown),
            Expr::Unary(_, right) => self.analyze_expr(right),
            Expr::Binary(left, _, right) => {
                let _ = self.analyze_expr(left);
                let _ = self.analyze_expr(right);
                ValueType::Unknown
            }
            Expr::Assignment(left, right) => {
                let right_type = self.analyze_expr(right);
                if let Expr::Literal(Literal::Identifier(name)) = left.as_ref() {
                    self.update_symbol(name, right_type.clone());
                } else {
                    self.analyze_expr(left);
                }
                right_type
            }
            Expr::Call(call) => self.analyze_call(call.callee.as_ref(), &call.arguments),
            Expr::Mebmer(member) => {
                let owner_type = self.analyze_expr(member.member.as_ref());
                if let Some(type_key) = user_type_key(&owner_type).cloned() {
                    if let Some(type_info) = self.type_index.get(&type_key) {
                        if let Some(field_type) = type_info.fields.get(&member.property) {
                            return self.resolve_declared_type(field_type, &type_key.module);
                        }
                    }
                }
                ValueType::Unknown
            }
            Expr::ComputedExpr(computed) => {
                self.analyze_expr(computed.member.as_ref());
                self.analyze_expr(computed.property.as_ref());
                ValueType::Unknown
            }
            Expr::Array(array) => {
                let mut item_type = ValueType::Unknown;
                for item in &array.array {
                    let ty = self.analyze_expr(item);
                    if matches!(item_type, ValueType::Unknown) {
                        item_type = ty;
                    }
                }
                ValueType::Array(Box::new(item_type))
            }
            Expr::Empty => ValueType::Void,
        }
    }

    fn analyze_call(&mut self, callee: &Expr, arguments: &[Expr]) -> ValueType {
        let arg_types = arguments
            .iter()
            .map(|argument| self.analyze_expr(argument))
            .collect::<Vec<_>>();

        match callee {
            Expr::Literal(Literal::Identifier(name)) => {
                self.analyze_identifier_function_call(name, &arg_types)
            }
            Expr::Mebmer(member) => {
                if let Some(target_module) = self.resolve_module_binding(member.member.as_ref()) {
                    return self.analyze_module_function_call(
                        &target_module,
                        &member.property,
                        &arg_types,
                    );
                }

                let receiver_type = self.analyze_expr(member.member.as_ref());
                let Some(type_key) = user_type_key(&receiver_type).cloned() else {
                    return ValueType::Unknown;
                };

                let Some(type_info) = self.type_index.get(&type_key) else {
                    return ValueType::Unknown;
                };

                let Some(method_info) = type_info.methods.get(&member.property) else {
                    self.error(format!(
                        "Unknown method '{}.{}'",
                        type_key.name, member.property
                    ));
                    return ValueType::Unknown;
                };

                if !type_info.visibility && self.module.path != type_key.module {
                    self.error(format!(
                        "Type '{}.{}' is private and cannot be referenced from '{}'",
                        type_key.module.display(),
                        type_key.name,
                        self.module.path.display()
                    ));
                }

                if !method_info.visibility && self.module.path != type_key.module {
                    self.error(format!(
                        "Method '{}.{}' is private and cannot be called from '{}'",
                        type_key.name,
                        member.property,
                        self.module.path.display()
                    ));
                }

                if method_info.params.len() != arg_types.len() {
                    self.error(format!(
                        "Method '{}.{}' expects {} args, got {}",
                        type_key.name,
                        member.property,
                        method_info.params.len(),
                        arg_types.len()
                    ));
                }

                let method_namespace = self
                    .module_type_namespaces
                    .get(&type_key.module)
                    .cloned()
                    .unwrap_or_default();

                for (index, (expected, actual)) in
                    method_info.params.iter().zip(arg_types.iter()).enumerate()
                {
                    let expected_ty = type_to_value_type(expected, &method_namespace)
                        .unwrap_or(ValueType::Unknown);
                    if !value_type_assignable(&expected_ty, actual) {
                        self.error(format!(
                            "Argument {} for method '{}.{}' expected {:?}, got {:?}",
                            index, type_key.name, member.property, expected_ty, actual
                        ));
                    }
                }

                type_to_value_type(&method_info.return_type, &method_namespace)
                    .unwrap_or(ValueType::Unknown)
            }
            _ => {
                self.analyze_expr(callee);
                ValueType::Unknown
            }
        }
    }

    fn analyze_identifier_function_call(
        &mut self,
        name: &str,
        arg_types: &[ValueType],
    ) -> ValueType {
        if self.lookup_symbol(name).is_some() {
            return ValueType::Unknown;
        }

        let Some(function_key) = self.module_function_namespace.get(name).cloned() else {
            return ValueType::Unknown;
        };

        self.analyze_function_call_by_key(&function_key, name, arg_types)
    }

    fn analyze_module_function_call(
        &mut self,
        target_module: &Path,
        function_name: &str,
        arg_types: &[ValueType],
    ) -> ValueType {
        let Some(namespace) = self.module_function_namespaces.get(target_module) else {
            return ValueType::Unknown;
        };

        let Some(function_key) = namespace.get(function_name).cloned() else {
            self.error(format!(
                "Unknown function '{}.{}'",
                target_module.display(),
                function_name
            ));
            return ValueType::Unknown;
        };

        self.analyze_function_call_by_key(&function_key, function_name, arg_types)
    }

    fn analyze_function_call_by_key(
        &mut self,
        function_key: &FunctionKey,
        display_name: &str,
        arg_types: &[ValueType],
    ) -> ValueType {
        let Some(function_info) = self.function_index.get(function_key) else {
            return ValueType::Unknown;
        };

        if !is_function_callable_from_module(function_info, &self.module.path, function_key) {
            self.error(format!(
                "Function '{}' is private and cannot be called from '{}'",
                display_name,
                self.module.path.display()
            ));
        }

        if function_info.params.len() != arg_types.len() {
            self.error(format!(
                "Function '{}' expects {} args, got {}",
                display_name,
                function_info.params.len(),
                arg_types.len()
            ));
        }

        let function_namespace = self
            .module_type_namespaces
            .get(&function_key.module)
            .cloned()
            .unwrap_or_default();

        for (index, (expected, actual)) in function_info
            .params
            .iter()
            .zip(arg_types.iter())
            .enumerate()
        {
            let expected_ty =
                type_to_value_type(expected, &function_namespace).unwrap_or(ValueType::Unknown);
            if !value_type_assignable(&expected_ty, actual) {
                self.error(format!(
                    "Argument {} for function '{}' expected {:?}, got {:?}",
                    index, display_name, expected_ty, actual
                ));
            }
        }

        type_to_value_type(&function_info.return_type, &function_namespace)
            .unwrap_or(ValueType::Unknown)
    }

    fn resolve_module_binding(&self, expr: &Expr) -> Option<PathBuf> {
        match expr {
            Expr::Literal(Literal::Identifier(name)) => self
                .module
                .bindings
                .get(name)
                .map(|binding| binding.target.clone()),
            _ => None,
        }
    }

    fn resolve_declared_type(&self, ty: &Type, module_path: &Path) -> ValueType {
        let namespace = self
            .module_type_namespaces
            .get(module_path)
            .cloned()
            .unwrap_or_default();
        type_to_value_type(ty, &namespace).unwrap_or(ValueType::Unknown)
    }

    fn define_symbol(&mut self, name: &str, ty: ValueType) {
        if let Some(scope) = self.scopes.last_mut() {
            scope.insert(name.to_string(), ty);
        }
    }

    fn lookup_symbol(&self, name: &str) -> Option<&ValueType> {
        for scope in self.scopes.iter().rev() {
            if let Some(ty) = scope.get(name) {
                return Some(ty);
            }
        }
        None
    }

    fn update_symbol(&mut self, name: &str, ty: ValueType) {
        for scope in self.scopes.iter_mut().rev() {
            if let Some(existing) = scope.get_mut(name) {
                *existing = ty;
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
        self.errors.push(SemanticError::new(format!(
            "{}: {}",
            self.module.path.display(),
            message.into()
        )));
    }
}

fn is_function_callable_from_module(
    function_info: &FunctionInfo,
    caller_module: &Path,
    function_key: &FunctionKey,
) -> bool {
    function_info.visibility || caller_module == function_key.module
}

fn value_type_assignable(expected: &ValueType, actual: &ValueType) -> bool {
    if matches!(expected, ValueType::Unknown) || matches!(actual, ValueType::Unknown) {
        return true;
    }

    match (expected, actual) {
        (ValueType::Array(left), ValueType::Array(right)) => value_type_assignable(left, right),
        (ValueType::User(left), ValueType::User(right)) => left == right,
        (ValueType::User(left), ValueType::GenericUser(right, _))
        | (ValueType::GenericUser(left, _), ValueType::User(right)) => left == right,
        (ValueType::GenericUser(left_key, left_args), ValueType::GenericUser(right_key, right_args)) => {
            left_key == right_key
                && left_args.len() == right_args.len()
                && left_args
                    .iter()
                    .zip(right_args.iter())
                    .all(|(left, right)| value_type_assignable(left, right))
        }
        _ => expected == actual,
    }
}

fn type_to_value_type(ty: &Type, namespace: &HashMap<String, TypeKey>) -> Option<ValueType> {
    match ty {
        Type::Num => Some(ValueType::Num),
        Type::Char => Some(ValueType::Char),
        Type::Byte => Some(ValueType::Byte),
        Type::String => Some(ValueType::String),
        Type::Bool => Some(ValueType::Bool),
        Type::Void => Some(ValueType::Void),
        Type::Identifier(name) => namespace.get(name).cloned().map(ValueType::User),
        Type::Generic { base, args } => {
            let key = namespace.get(base).cloned()?;
            let resolved_args = args
                .iter()
                .map(|arg| type_to_value_type(arg, namespace))
                .collect::<Option<Vec<_>>>()?;
            Some(ValueType::GenericUser(key, resolved_args))
        }
        Type::Array(inner) => type_to_value_type(inner, namespace)
            .map(|resolved| ValueType::Array(Box::new(resolved))),
        Type::None => Some(ValueType::Unknown),
    }
}

fn user_type_key(value_type: &ValueType) -> Option<&TypeKey> {
    match value_type {
        ValueType::User(key) => Some(key),
        ValueType::GenericUser(key, _) => Some(key),
        _ => None,
    }
}

fn resolve_type_path(
    modules: &HashMap<PathBuf, LinkedModule>,
    module: &LinkedModule,
    entry_module: &Path,
    path: &str,
    type_index: &HashMap<TypeKey, TypeInfo>,
    require_public: bool,
) -> Option<TypeKey> {
    let segments = path
        .split("::")
        .filter(|segment| !segment.is_empty())
        .collect::<Vec<_>>();
    if segments.is_empty() {
        return None;
    }

    let root = segments[0];
    if matches!(root, "std" | "core") {
        return None;
    }

    let mut current_module_path;
    let mut segment_index = 1usize;

    match root {
        "crate" => {
            current_module_path = entry_module.to_path_buf();
        }
        "self" => {
            current_module_path = module.path.clone();
        }
        "super" => {
            current_module_path = module.parent.clone()?;
        }
        _ => {
            let root_item = module.items.get(root)?;
            if root_item.kind != ItemKind::Module {
                return None;
            }
            if require_public && !root_item.public {
                return None;
            }
            current_module_path = root_item.target_module.clone()?;
        }
    }

    while segment_index < segments.len() {
        let segment = segments[segment_index];
        let is_last = segment_index + 1 == segments.len();
        let current_module = modules.get(&current_module_path)?;
        let item = current_module.items.get(segment)?;

        if require_public && !item.public {
            return None;
        }

        if is_last {
            if item.kind == ItemKind::Type {
                let key = TypeKey {
                    module: current_module_path.clone(),
                    name: segment.to_string(),
                };
                if type_index.contains_key(&key) {
                    return Some(key);
                }
            }
            return None;
        }

        if item.kind != ItemKind::Module {
            return None;
        }
        current_module_path = item.target_module.clone()?;
        segment_index += 1;
    }

    None
}

fn resolve_function_path(
    modules: &HashMap<PathBuf, LinkedModule>,
    module: &LinkedModule,
    entry_module: &Path,
    path: &str,
    function_index: &HashMap<FunctionKey, FunctionInfo>,
) -> Option<FunctionKey> {
    let segments = path
        .split("::")
        .filter(|segment| !segment.is_empty())
        .collect::<Vec<_>>();
    if segments.is_empty() {
        return None;
    }

    let root = segments[0];
    if matches!(root, "std" | "core") {
        return None;
    }

    let mut current_module_path;
    let mut segment_index = 1usize;

    match root {
        "crate" => {
            current_module_path = entry_module.to_path_buf();
        }
        "self" => {
            current_module_path = module.path.clone();
        }
        "super" => {
            current_module_path = module.parent.clone()?;
        }
        _ => {
            let root_item = module.items.get(root)?;
            if root_item.kind == ItemKind::Module {
                current_module_path = root_item.target_module.clone()?;
            } else if root_item.kind == ItemKind::Symbol && segments.len() == 1 {
                let key = FunctionKey {
                    module: module.path.clone(),
                    name: root.to_string(),
                };
                if function_index.contains_key(&key) {
                    return Some(key);
                }
                return None;
            } else {
                return None;
            }
        }
    }

    while segment_index < segments.len() {
        let segment = segments[segment_index];
        let is_last = segment_index + 1 == segments.len();
        let current_module = modules.get(&current_module_path)?;
        let item = current_module.items.get(segment)?;

        if is_last {
            if item.kind == ItemKind::Symbol {
                let key = FunctionKey {
                    module: current_module_path.clone(),
                    name: segment.to_string(),
                };
                if function_index.contains_key(&key) {
                    return Some(key);
                }
            }
            return None;
        }

        if item.kind != ItemKind::Module {
            return None;
        }

        current_module_path = item.target_module.clone()?;
        segment_index += 1;
    }

    None
}

fn collect_dependency_requests(ast: &BlockStmt) -> Vec<DependencyRequest> {
    let mut requests = Vec::new();
    for stmt in &ast.stmts {
        let (inner, _) = strip_pub(stmt);
        match inner {
            Stmt::Mod(mod_stmt) => requests.push(DependencyRequest {
                binding_name: mod_stmt.name.clone(),
                specifier: mod_stmt.name.clone(),
                is_mod_decl: true,
            }),
            Stmt::Import(import_stmt) => requests.push(DependencyRequest {
                binding_name: import_binding_name(import_stmt),
                specifier: import_stmt.from.clone(),
                is_mod_decl: false,
            }),
            _ => {}
        }
    }
    requests
}

fn collect_use_paths(ast: &BlockStmt) -> Vec<UsePath> {
    let mut use_paths = Vec::new();
    for stmt in &ast.stmts {
        let (inner, public) = strip_pub(stmt);
        if let Stmt::Use(use_stmt) = inner {
            use_paths.push(UsePath {
                path: use_stmt.path.clone(),
                public,
            });
        }
    }
    use_paths
}

fn collect_module_items(
    ast: &BlockStmt,
    bindings: &HashMap<String, ModuleBinding>,
) -> HashMap<String, ItemInfo> {
    let mut items = HashMap::new();
    for stmt in &ast.stmts {
        let (inner, public) = strip_pub(stmt);
        match inner {
            Stmt::Mod(mod_stmt) => {
                let target = bindings
                    .get(&mod_stmt.name)
                    .map(|binding| binding.target.clone());
                items.entry(mod_stmt.name.clone()).or_insert(ItemInfo {
                    kind: ItemKind::Module,
                    public,
                    target_module: target,
                });
            }
            Stmt::Import(import_stmt) => {
                let binding_name = import_binding_name(import_stmt);
                let target = bindings
                    .get(&binding_name)
                    .map(|binding| binding.target.clone());
                items.entry(binding_name).or_insert(ItemInfo {
                    kind: ItemKind::Module,
                    public,
                    target_module: target,
                });
            }
            Stmt::Use(use_stmt) => {
                let binding_name = use_binding_name(use_stmt);
                items.entry(binding_name).or_insert(ItemInfo {
                    kind: ItemKind::Symbol,
                    public,
                    target_module: None,
                });
            }
            Stmt::Fun(fun_stmt) => {
                items.entry(fun_stmt.name.clone()).or_insert(ItemInfo {
                    kind: ItemKind::Symbol,
                    public,
                    target_module: None,
                });
            }
            Stmt::Var(var_stmt) => {
                items.entry(var_stmt.name.clone()).or_insert(ItemInfo {
                    kind: ItemKind::Symbol,
                    public,
                    target_module: None,
                });
            }
            Stmt::Const(const_stmt) => {
                items.entry(const_stmt.name.clone()).or_insert(ItemInfo {
                    kind: ItemKind::Symbol,
                    public,
                    target_module: None,
                });
            }
            Stmt::Struct(struct_stmt) => {
                items.entry(struct_stmt.name.clone()).or_insert(ItemInfo {
                    kind: ItemKind::Type,
                    public,
                    target_module: None,
                });
            }
            Stmt::Enum(enum_stmt) => {
                items.entry(enum_stmt.name.clone()).or_insert(ItemInfo {
                    kind: ItemKind::Type,
                    public,
                    target_module: None,
                });
            }
            Stmt::Class(class_stmt) => {
                items.entry(class_stmt.name.clone()).or_insert(ItemInfo {
                    kind: ItemKind::Type,
                    public,
                    target_module: None,
                });
            }
            _ => {}
        }
    }

    // Keep module declarations explicit in case there is no direct top-level stmt item
    for (binding_name, binding) in bindings {
        if binding.from_mod_decl {
            items.entry(binding_name.clone()).or_insert(ItemInfo {
                kind: ItemKind::Module,
                public: false,
                target_module: Some(binding.target.clone()),
            });
        }
    }

    items
}

fn strip_pub(stmt: &Stmt) -> (&Stmt, bool) {
    match stmt {
        Stmt::Pub(pub_stmt) => {
            let (inner, _) = strip_pub(pub_stmt.stmt.as_ref());
            (inner, true)
        }
        other => (other, false),
    }
}

fn import_binding_name(import_stmt: &ImportStmt) -> String {
    import_stmt
        .alias
        .as_ref()
        .cloned()
        .unwrap_or_else(|| import_stmt.import.clone())
}

fn use_binding_name(use_stmt: &UseStmt) -> String {
    use_stmt
        .path
        .rsplit("::")
        .next()
        .unwrap_or(&use_stmt.path)
        .to_string()
}

fn dedup_paths(paths: &mut Vec<PathBuf>) {
    let mut seen = HashSet::new();
    paths.retain(|path| seen.insert(path.clone()));
}

#[cfg(test)]
mod tests {
    use std::{
        collections::HashMap,
        path::{Path, PathBuf},
    };

    use crate::ast::{BlockStmt, ImportStmt, ModStmt, PubStmt, Stmt, StructStmt, Type, UseStmt};

    use super::{
        collect_dependency_requests, collect_module_items, import_binding_name,
        is_function_callable_from_module, resolve_function_path, resolve_type_path,
        type_to_value_type, use_binding_name, value_type_assignable, FunctionInfo, FunctionKey,
        ItemInfo, ItemKind, LinkedModule, MethodInfo, ModuleBinding, TypeInfo, TypeKey, ValueType,
    };

    fn empty_module(path: &str) -> LinkedModule {
        LinkedModule {
            path: PathBuf::from(path),
            ast: BlockStmt { stmts: vec![] },
            parent: None,
            bindings: HashMap::new(),
            items: HashMap::new(),
            uses: vec![],
        }
    }

    #[test]
    fn import_binding_name_prefers_alias() {
        let import_stmt = ImportStmt {
            import: "System".to_string(),
            from: "./system.kek".to_string(),
            alias: Some("Sys".to_string()),
        };

        assert_eq!(import_binding_name(&import_stmt), "Sys");
    }

    #[test]
    fn use_binding_name_takes_last_segment() {
        let use_stmt = UseStmt {
            path: "core::fmt::print".to_string(),
        };
        assert_eq!(use_binding_name(&use_stmt), "print");
    }

    #[test]
    fn collect_dependencies_extracts_mod_and_import() {
        let ast = crate::ast::BlockStmt {
            stmts: vec![
                Stmt::Mod(crate::ast::ModStmt {
                    name: "core".to_string(),
                }),
                Stmt::Import(ImportStmt {
                    import: "System".to_string(),
                    from: "../system.kek".to_string(),
                    alias: Some("Sys".to_string()),
                }),
            ],
        };

        let deps = collect_dependency_requests(&ast);
        let by_name = deps
            .into_iter()
            .map(|dependency| (dependency.binding_name.clone(), dependency))
            .collect::<HashMap<_, _>>();

        let core = by_name.get("core").expect("core mod should exist");
        assert!(core.is_mod_decl);
        assert_eq!(core.specifier, "core");

        let sys = by_name.get("Sys").expect("Sys import should exist");
        assert!(!sys.is_mod_decl);
        assert_eq!(sys.specifier, "../system.kek");
    }

    #[test]
    fn value_type_assignable_handles_user_and_array_types() {
        let point_key = TypeKey {
            module: PathBuf::from("/tmp/a.kek"),
            name: "Point".to_string(),
        };
        let point_key_same = TypeKey {
            module: PathBuf::from("/tmp/a.kek"),
            name: "Point".to_string(),
        };
        let other_key = TypeKey {
            module: PathBuf::from("/tmp/b.kek"),
            name: "Point".to_string(),
        };

        assert!(value_type_assignable(
            &ValueType::User(point_key.clone()),
            &ValueType::User(point_key_same)
        ));
        assert!(!value_type_assignable(
            &ValueType::User(point_key.clone()),
            &ValueType::User(other_key)
        ));
        assert!(value_type_assignable(
            &ValueType::Array(Box::new(ValueType::Num)),
            &ValueType::Array(Box::new(ValueType::Num))
        ));
        assert!(!value_type_assignable(
            &ValueType::Array(Box::new(ValueType::Num)),
            &ValueType::Array(Box::new(ValueType::Bool))
        ));
        assert!(value_type_assignable(&ValueType::Unknown, &ValueType::Num));
        assert!(value_type_assignable(&ValueType::Num, &ValueType::Unknown));
    }

    #[test]
    fn type_to_value_type_resolves_user_types_from_namespace() {
        let user_key = TypeKey {
            module: PathBuf::from("/tmp/types.kek"),
            name: "User".to_string(),
        };
        let mut namespace = HashMap::new();
        namespace.insert("User".to_string(), user_key.clone());

        let resolved = type_to_value_type(
            &crate::ast::Type::Identifier("User".to_string()),
            &namespace,
        );
        assert_eq!(resolved, Some(ValueType::User(user_key)));

        let missing = type_to_value_type(
            &crate::ast::Type::Identifier("Missing".to_string()),
            &namespace,
        );
        assert_eq!(missing, None);
    }

    #[test]
    fn type_to_value_type_preserves_generic_user_arguments() {
        let boxed_key = TypeKey {
            module: PathBuf::from("/tmp/types.kek"),
            name: "Boxed".to_string(),
        };
        let value_key = TypeKey {
            module: PathBuf::from("/tmp/types.kek"),
            name: "Value".to_string(),
        };
        let mut namespace = HashMap::new();
        namespace.insert("Boxed".to_string(), boxed_key.clone());
        namespace.insert("Value".to_string(), value_key.clone());

        let resolved = type_to_value_type(
            &crate::ast::Type::Generic {
                base: "Boxed".to_string(),
                args: vec![crate::ast::Type::Identifier("Value".to_string())],
            },
            &namespace,
        );

        assert_eq!(
            resolved,
            Some(ValueType::GenericUser(
                boxed_key,
                vec![ValueType::User(value_key)]
            ))
        );
    }

    #[test]
    fn resolve_type_path_resolves_public_type_through_module_binding() {
        let root_path = PathBuf::from("/tmp/root.kek");
        let util_path = PathBuf::from("/tmp/util.kek");
        let type_key = TypeKey {
            module: util_path.clone(),
            name: "Point".to_string(),
        };

        let mut root_module = empty_module("/tmp/root.kek");
        root_module.items.insert(
            "util".to_string(),
            ItemInfo {
                kind: ItemKind::Module,
                public: true,
                target_module: Some(util_path.clone()),
            },
        );

        let mut util_module = empty_module("/tmp/util.kek");
        util_module.items.insert(
            "Point".to_string(),
            ItemInfo {
                kind: ItemKind::Type,
                public: true,
                target_module: None,
            },
        );

        let mut modules = HashMap::new();
        modules.insert(root_path.clone(), root_module.clone());
        modules.insert(util_path.clone(), util_module);

        let mut type_index = HashMap::new();
        type_index.insert(
            type_key.clone(),
            TypeInfo {
                module: util_path.clone(),
                visibility: true,
                fields: HashMap::new(),
                methods: HashMap::<String, MethodInfo>::new(),
            },
        );

        let resolved = resolve_type_path(
            &modules,
            &root_module,
            Path::new("/tmp/root.kek"),
            "util::Point",
            &type_index,
            true,
        );

        assert_eq!(resolved, Some(type_key));
    }

    #[test]
    fn resolve_type_path_rejects_private_type_when_public_required() {
        let root_path = PathBuf::from("/tmp/root.kek");
        let util_path = PathBuf::from("/tmp/util.kek");
        let type_key = TypeKey {
            module: util_path.clone(),
            name: "Point".to_string(),
        };

        let mut root_module = empty_module("/tmp/root.kek");
        root_module.items.insert(
            "util".to_string(),
            ItemInfo {
                kind: ItemKind::Module,
                public: true,
                target_module: Some(util_path.clone()),
            },
        );

        let mut util_module = empty_module("/tmp/util.kek");
        util_module.items.insert(
            "Point".to_string(),
            ItemInfo {
                kind: ItemKind::Type,
                public: false,
                target_module: None,
            },
        );

        let mut modules = HashMap::new();
        modules.insert(root_path.clone(), root_module.clone());
        modules.insert(util_path.clone(), util_module);

        let mut type_index = HashMap::new();
        type_index.insert(
            type_key,
            TypeInfo {
                module: util_path.clone(),
                visibility: false,
                fields: HashMap::new(),
                methods: HashMap::<String, MethodInfo>::new(),
            },
        );

        let resolved = resolve_type_path(
            &modules,
            &root_module,
            Path::new("/tmp/root.kek"),
            "util::Point",
            &type_index,
            true,
        );

        assert_eq!(resolved, None);
    }

    #[test]
    fn resolve_function_path_resolves_public_function_through_module_binding() {
        let root_path = PathBuf::from("/tmp/root.kek");
        let util_path = PathBuf::from("/tmp/util.kek");
        let function_key = FunctionKey {
            module: util_path.clone(),
            name: "add".to_string(),
        };

        let mut root_module = empty_module("/tmp/root.kek");
        root_module.items.insert(
            "util".to_string(),
            ItemInfo {
                kind: ItemKind::Module,
                public: true,
                target_module: Some(util_path.clone()),
            },
        );

        let mut util_module = empty_module("/tmp/util.kek");
        util_module.items.insert(
            "add".to_string(),
            ItemInfo {
                kind: ItemKind::Symbol,
                public: true,
                target_module: None,
            },
        );

        let mut modules = HashMap::new();
        modules.insert(root_path.clone(), root_module.clone());
        modules.insert(util_path.clone(), util_module);

        let mut function_index = HashMap::new();
        function_index.insert(
            function_key.clone(),
            FunctionInfo {
                visibility: true,
                params: vec![Type::Num, Type::Num],
                return_type: Type::Num,
            },
        );

        let resolved = resolve_function_path(
            &modules,
            &root_module,
            Path::new("/tmp/root.kek"),
            "util::add",
            &function_index,
        );

        assert_eq!(resolved, Some(function_key));
    }

    #[test]
    fn resolve_function_path_ignores_non_function_symbols() {
        let root_path = PathBuf::from("/tmp/root.kek");
        let util_path = PathBuf::from("/tmp/util.kek");

        let mut root_module = empty_module("/tmp/root.kek");
        root_module.items.insert(
            "util".to_string(),
            ItemInfo {
                kind: ItemKind::Module,
                public: true,
                target_module: Some(util_path.clone()),
            },
        );

        let mut util_module = empty_module("/tmp/util.kek");
        util_module.items.insert(
            "Point".to_string(),
            ItemInfo {
                kind: ItemKind::Type,
                public: true,
                target_module: None,
            },
        );

        let mut modules = HashMap::new();
        modules.insert(root_path.clone(), root_module.clone());
        modules.insert(util_path, util_module);

        let resolved = resolve_function_path(
            &modules,
            &root_module,
            Path::new("/tmp/root.kek"),
            "util::Point",
            &HashMap::new(),
        );

        assert_eq!(resolved, None);
    }

    #[test]
    fn function_visibility_enforces_cross_module_calls() {
        let function_key = FunctionKey {
            module: PathBuf::from("/tmp/util.kek"),
            name: "hidden".to_string(),
        };

        let private_function = FunctionInfo {
            visibility: false,
            params: vec![],
            return_type: Type::Num,
        };
        let public_function = FunctionInfo {
            visibility: true,
            params: vec![],
            return_type: Type::Num,
        };

        assert!(!is_function_callable_from_module(
            &private_function,
            Path::new("/tmp/main.kek"),
            &function_key,
        ));
        assert!(is_function_callable_from_module(
            &private_function,
            Path::new("/tmp/util.kek"),
            &function_key,
        ));
        assert!(is_function_callable_from_module(
            &public_function,
            Path::new("/tmp/main.kek"),
            &function_key,
        ));
    }

    #[test]
    fn collect_module_items_marks_structs_as_type_items() {
        let ast = BlockStmt {
            stmts: vec![
                Stmt::Mod(ModStmt {
                    name: "util".to_string(),
                }),
                Stmt::Pub(PubStmt {
                    stmt: Box::new(Stmt::Struct(StructStmt {
                        name: "Point".to_string(),
                        fields: vec![],
                        methods: vec![],
                    })),
                }),
            ],
        };

        let mut bindings = HashMap::new();
        bindings.insert(
            "util".to_string(),
            ModuleBinding {
                target: PathBuf::from("/tmp/util.kek"),
                from_mod_decl: true,
            },
        );

        let items = collect_module_items(&ast, &bindings);
        let point = items.get("Point").expect("Point item should be present");
        assert_eq!(point.kind, ItemKind::Type);
        assert!(point.public);
    }
}
