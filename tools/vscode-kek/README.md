# Kek Language Support (VS Code)

This extension provides basic Kek language support in VS Code:

- Syntax highlighting for Kek v1 (`.kek`)
- Comment toggling (`//`, `/* ... */`)
- Auto-closing and surrounding pairs for `()`, `{}`, `[]`, `""`, `''`
- Folding markers (`// region`, `// endregion`)

## Local development

1. Open `/Users/vlad/workspace/rusty/kekar/tools/vscode-kek` in VS Code.
2. Press `F5` to launch an Extension Development Host.
3. Open any `.kek` file to verify highlighting.

## Package/install (optional)

If you have `vsce` installed:

```bash
cd /Users/vlad/workspace/rusty/kekar/tools/vscode-kek
npx @vscode/vsce@3.6.2 package --skip-license
code --install-extension kek-language-0.1.0.vsix
```

Notes:
- Current Homebrew `vsce` 3.7.1 may fail packaging in some environments due to a secret-scan concurrency bug (`Expected concurrency ... got 0`).
- If you use a global install, pin to `@vscode/vsce@3.6.2` for now.
