Kekar is a Rust compiler frontend for the `kek` language.

Current pipeline:
- `Lexer` -> `Parser` -> AST -> JavaScript (`js`) or x86_64 NASM-style assembly (`asm`) generation

Run:
```bash
cargo run -- example/person.kek
cargo run -- example/person.kek --target js
cargo run -- example/function.kek --target asm
```

This prints generated output to stdout.
