# Kek v1 Implementation Checklist

Status date: `2026-02-11`  
Last updated for commit: `b3b7f50`

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
- [ ] Enforce module/import resolution semantics (`mod`/`use`/`import`)
- [ ] Enforce `struct`/`enum`/`impl` member and method typing rules
- [ ] Enforce match exhaustiveness and pattern type compatibility
- [ ] Add visibility/access control semantics for `pub`

## Stage 5: Code generation alignment
- [x] JS generator handles new AST variants (including import alias emission and placeholder lowering for module/type declarations)
- [x] ASM generator made exhaustive for new AST variants (unsupported constructs safely ignored with comments)
- [x] Ensure `pub fun main` is recognized for ASM entrypoint generation
- [ ] Lower `match` to executable JS/ASM control flow instead of comments/placeholders
- [ ] Lower `struct`/`enum`/`impl` into stable runtime representation in both backends

## Stage 6: Self-hosting readiness gap
- [ ] Module system and symbol resolution robust enough for multi-file compiler project layout
- [ ] Full type system behavior for user-defined types and methods
- [ ] Diagnostics policy for compatibility warnings and future strict mode
- [ ] Stable IR/codegen behavior required by compiler-in-kek bootstrap
