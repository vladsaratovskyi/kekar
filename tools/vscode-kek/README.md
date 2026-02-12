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
vsce package
code --install-extension kek-language-0.1.0.vsix
```
