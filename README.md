Kekar is a Rust compiler frontend for the `kek` language.

Current pipeline:
- `Lexer` -> `Parser` -> AST -> x86_64 NASM-style assembly (`asm`) generation

Current v1 syntax note:
- Struct methods are declared inline inside `struct { ... }` (separate `impl` blocks remain supported for compatibility).

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

## VS Code syntax highlighting
- Extension folder: `/Users/vlad/workspace/rusty/kekar/tools/vscode-kek`
- Open that folder in VS Code and press `F5` to run an Extension Development Host.
- Package locally (recommended with pinned `vsce`):
  - `cd /Users/vlad/workspace/rusty/kekar/tools/vscode-kek`
  - `npx @vscode/vsce@3.6.2 package --skip-license`
