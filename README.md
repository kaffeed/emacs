# Emacs Configuration

This repository contains a modern Emacs 29+ configuration tailored for software development, utilizing native features and declarative package management.

## Core Setup
- **Package Manager**: `straight.el` with `use-package`.
- **Initialization**: A fully modular architecture. `init.el` serves as a bootstrap loader for domain-specific files inside the `modules/` directory (e.g., `init-core.el`, `init-ui.el`, `init-prog.el`).
- **Debugging & LSP**: Separated into `eglot-config.el` (LSP) and `dape-config.el` (DAP).
- **Minimum Version**: Emacs 29.1+ (relies on built-in `treesit` and `eglot`).

## Features & Packages

### UI & Aesthetics
- **Theme**: `doom-gruvbox`.
- **UI Enhancements**: `spacious-padding` for clean window layouts, disabled scrollbars and toolbars.
- **Font**: Iosevka Nerd Font Mono.

### Completion & Navigation
- **Minibuffer**: `vertico` (vertical completion), `orderless` (pattern matching), `marginalia` (annotations), `consult` (enhanced search/navigation), `embark` (context actions).
- **In-buffer Completion**: `corfu` (completion popup) and `cape` (completion at point extensions), powered by built-in `capf`.
- **Snippets**: `yasnippet` and `yasnippet-snippets`.

### Development & Languages
- **Syntax Highlighting**: Built-in tree-sitter (`treesit`) with native major modes (`*-ts-mode`). Auto-installs missing grammars. Supported out-of-the-box: TypeScript, JavaScript, HTML, CSS, JSON, YAML, Go, C#, Astro.
- **Language Server Protocol (LSP)**: `eglot` (built-in). Pre-configured for C#, Go, TypeScript, HTML/CSS, JSON, YAML, Angular, and Astro.
- **Debugging (DAP)**: `dape`. Configured for .NET Core (`netcoredbg`), Go (`dlv`), and Node.js/Next.js (`js-debug`).
- **Diagnostics**: Built-in `flymake`.
- **Formatting**: `apheleia` (asynchronous, cursor-preserving formatting).
- **Project Management**: `projectile` combined with `perspective` (`persp-mode`) for isolated, project-aware workspaces.

### Git & Org
- **Version Control**: `magit` and `forge`.
- **Org-mode**: Separate configuration in `org-config.el` including custom agenda views, capture templates, time tracking, and Azure DevOps integration.

## Installation

1. Ensure Emacs 29.1+ is installed.
2. Clone this repository to the user config directory:
   ```bash
   git clone <repository-url> ~/.config/emacs
   ```
3. Ensure you have a working C compiler (or `zig`) in your `PATH` for compiling tree-sitter grammars.
4. On first launch, `straight.el` will bootstrap and install all packages automatically.

## Notes
- Custom variables are saved to `custom-vars.el`.
- Language servers and debug adapters must be installed externally (e.g., via `npm` or `dotnet tool`).
- The `modules/` configuration files are intentionally excluded from `compile-angel` auto-compilation to prevent asynchronous macro expansion issues.
