use std::{
    collections::{HashMap, HashSet},
    fs,
    path::{Path, PathBuf},
};

use crate::{
    ast::{BlockStmt, ImportStmt, Stmt, UseStmt},
    lexer::Lexer,
    parser::Parser,
    sema::{SemanticAnalyzer, SemanticError},
};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum ItemKind {
    Module,
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

pub fn analyze_workspace(entry_path: impl AsRef<Path>) -> Result<(), Vec<SemanticError>> {
    let mut linker = WorkspaceLinker::new();
    let Some(entry_module) = linker.load_entry(entry_path.as_ref()) else {
        return Err(linker.errors);
    };

    let mut errors = linker.errors;
    let mut module_paths = linker.modules.keys().cloned().collect::<Vec<_>>();
    module_paths.sort();

    for module_path in &module_paths {
        let Some(module) = linker.modules.get(module_path) else {
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
        let Some(module) = linker.modules.get(module_path) else {
            continue;
        };

        for use_path in &module.uses {
            if let Err(message) = resolve_use_path(&linker.modules, module, &entry_module, use_path)
            {
                errors.push(SemanticError::new(format!(
                    "{}: {}",
                    module.path.display(),
                    message
                )));
            }
        }
    }

    if errors.is_empty() {
        Ok(())
    } else {
        Err(errors)
    }
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
    let tokens = lexer.lex_file();
    let mut parser = Parser::new(tokens);
    std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| parser.parse())).map_err(|_| {
        SemanticError::new(format!(
            "Failed to parse module '{}': parser panic",
            path.display()
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
                    kind: ItemKind::Symbol,
                    public,
                    target_module: None,
                });
            }
            Stmt::Enum(enum_stmt) => {
                items.entry(enum_stmt.name.clone()).or_insert(ItemInfo {
                    kind: ItemKind::Symbol,
                    public,
                    target_module: None,
                });
            }
            Stmt::Class(class_stmt) => {
                items.entry(class_stmt.name.clone()).or_insert(ItemInfo {
                    kind: ItemKind::Symbol,
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
    use std::collections::HashMap;

    use crate::ast::{ImportStmt, Stmt, UseStmt};

    use super::{collect_dependency_requests, import_binding_name, use_binding_name};

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
}
