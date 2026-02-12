# Kek v1 Implementation Checklist

Status date: `2026-02-12`  
Last updated for commit: `df2789e` + self-host audit

## Stage 1: Freeze grammar + compatibility rules
- [x] Freeze v1 grammar and compatibility contract in `/Users/vlad/workspace/rusty/kekar/docs/kek-v1.md`
- [x] Freeze v1 lexicon in `/Users/vlad/workspace/rusty/kekar/docs/lexicon-v1.toml`
- [x] Define compatibility window and legacy syntax policy

## Stage 2: Lexer implementation
- [x] Add v1 keyword/operator/punctuation tokens
- [x] Implement lexer support for new v1 tokens (`pub`, `mod`, `use`, `const`, `struct`, `enum`, `impl`, `match`, `as`, `=>`, `::`, `->`)
- [x] Add lexer tests for v1 lexicon behavior

## Stage 3: AST + parser implementation
- [x] Add AST nodes for v1 declarations/statements (`Pub`, `Mod`, `Use`, `Struct`, `Enum`, `Impl`, `Match`, patterns)
- [x] Parse canonical function signatures (`name: Type`) and `->` return types
- [x] Keep compatibility parsing for legacy params (`Type name`) and legacy return `: Type`
- [x] Parse import aliases (`import X as Y from "..."`)
- [x] Parse module/use declarations (`mod`, `use a::b`)
- [x] Parse public declarations (`pub fun`, `pub var`, `pub const`, `pub struct`, `pub enum`, `pub class`, `pub mod`, `pub use`, `pub impl`)
- [x] Parse struct declarations with inline methods (`fun` / `pub fun`) in addition to fields
- [x] Parse `impl` blocks with method declarations
- [x] Parse `match` statements with literal/identifier/wildcard/variant patterns
- [x] Add parser tests for new members and patterns

## Stage 4: Semantic analysis
- [x] Add semantic analyzer scaffold and test coverage
- [x] Validate core control-flow typing (`if`/`while` condition typing, `break`/`continue` loop context)
- [x] Validate function signatures, return typing, call argument typing
- [x] Add baseline handling for new statement kinds to keep analysis exhaustive
- [x] Add baseline `match` analysis with arm scopes and pattern bindings
- [x] Enforce module/import resolution semantics (`mod`/`use`/`import`)
- [x] Enforce `struct`/`enum` members and method typing rules (inline struct methods + `impl`)
- [x] Enforce match exhaustiveness and pattern type compatibility
- [x] Add visibility/access control semantics for `pub`

## Stage 5: Code generation alignment
- [x] ASM generator handles new AST variants (including import alias handling and lowering for module/type declarations)
- [x] ASM generator made exhaustive for new AST variants (unsupported constructs safely ignored with comments)
- [x] Ensure `pub fun main` is recognized for ASM entrypoint generation
- [x] Lower `match` to executable ASM control flow instead of comments/placeholders
- [x] Lower `struct`/`enum`/`impl` into stable runtime representation in ASM backend (including inline struct methods)

## Stage 6: Self-hosting readiness
- [x] Semantic guardrails exist for module/use/import declarations, user-defined types, struct/impl methods, match coverage, and `pub` visibility
- [x] Core frontend pipeline (lexer/parser/sema) supports v1 declaration surface needed by a compiler frontend codebase
- [x] Real multi-file module loading and path-based symbol resolution (filesystem-backed, cross-file linking)
- [x] Namespace-aware method call resolution for struct/impl methods and visibility across module boundaries
- [x] Full user-type semantics needed for bootstrap-scale code (constructor/value typing strategy, richer class/enum runtime model)
- [x] Compatibility diagnostics policy implemented in compiler output (warnings + migration hints)
- [x] Backend lowering support for v1 constructs (`match`, `struct`/`enum`/`impl`) in executable ASM output
- [x] Bootstrap milestone: compile a non-trivial Kek program in Kek (self-host stage-0 target)

## Stage 7: Self-host blocker closure (post ASM-only audit)
- [x] Wire compiler entrypoint to workspace-level analysis/linking before ASM generation (`analyze_workspace` path)
- [x] Replace panic-based lexer/parser failures with structured diagnostics and non-zero exits
- [x] Implement ASM lowering for runtime-critical values (`String`, arrays, `this`, member access, computed access)
- [x] Implement ASM lowering for dynamic/member calls and method dispatch used by struct/impl/class code
- [x] Implement loop control lowering (`break`, `continue`) with correct label/stack semantics
- [x] Implement non-literal iterator lowering for `for` (not just compile-time array unroll)
- [x] Complete match lowering for enum payload pattern checks and payload bindings
- [x] Add cross-module function symbol typing and resolution (not only member-call checks)
- [x] Add callable visibility enforcement for cross-module function calls (`pub`/private)
- [x] Add spec/implementation parity for currently frozen grammar features (generic type syntax, postfix `?`)
- [x] Add end-to-end backend tests that assemble/link/run generated ASM for representative programs
