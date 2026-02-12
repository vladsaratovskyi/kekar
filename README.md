Kekar is a Rust compiler frontend for the `kek` language.

Current pipeline:
- `Lexer` -> `Parser` -> AST -> x86_64 NASM-style assembly (`asm`) generation

Run:
```bash
cargo run -- example/person.kek
cargo run -- example/function.kek --target asm
```

This prints generated output to stdout.

## Kek v1 freeze artifacts
- Spec: `/Users/vlad/workspace/rusty/kekar/docs/kek-v1.md`
- Lexicon source of truth: `/Users/vlad/workspace/rusty/kekar/docs/lexicon-v1.toml`
- Implementation checklist: `/Users/vlad/workspace/rusty/kekar/docs/kek-v1-checklist.md`
