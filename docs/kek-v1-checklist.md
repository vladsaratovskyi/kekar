# Kek v1 Implementation Checklist

Status date: `2026-02-11`  
Last updated for commit: `working tree (uncommitted)`

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
- [x] Enforce `struct`/`enum`/`impl` member and method typing rules
- [x] Enforce match exhaustiveness and pattern type compatibility
- [x] Add visibility/access control semantics for `pub`

## Stage 5: Code generation alignment
- [x] JS generator handles new AST variants (including import alias emission and placeholder lowering for module/type declarations)
- [x] ASM generator made exhaustive for new AST variants (unsupported constructs safely ignored with comments)
- [x] Ensure `pub fun main` is recognized for ASM entrypoint generation
- [x] Lower `match` to executable JS/ASM control flow instead of comments/placeholders
- [x] Lower `struct`/`enum`/`impl` into stable runtime representation in both backends

## Stage 6: Self-hosting readiness
- [x] Semantic guardrails exist for module/use/import declarations, user-defined types, impl methods, match coverage, and `pub` visibility
- [x] Core frontend pipeline (lexer/parser/sema) supports v1 declaration surface needed by a compiler frontend codebase
- [x] Real multi-file module loading and path-based symbol resolution (filesystem-backed, cross-file linking)
- [x] Namespace-aware method call resolution for impl methods and visibility across module boundaries
- [ ] Full user-type semantics needed for bootstrap-scale code (constructor/value typing strategy, richer class/enum runtime model)
- [ ] Compatibility diagnostics policy implemented in compiler output (warnings + migration hints)
- [x] Backend lowering parity for v1 constructs (`match`, `struct`/`enum`/`impl`) in executable JS/ASM output
- [ ] Bootstrap milestone: compile a non-trivial Kek program in Kek (self-host stage-0 target)
