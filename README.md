# Emacs Configuration

Personal Emacs configuration using straight.el for package management.

## Requirements

- Emacs 27.1 or later
- Git

## Installation

Clone this repository to your home directory:

```bash
git clone <repository-url> ~/.emacs.d
```

On first launch, Emacs will automatically install straight.el and all configured packages. This may take a few minutes.

## Package Management

Uses [straight.el](https://github.com/radian-software/straight.el) with use-package for declarative package configuration. Package.el is explicitly disabled to avoid conflicts.

## Key Features

### Interface
- **nano-emacs**: Minimal, clean theme (dark variant)
- **which-key**: Displays available keybindings
- **pulsar**: Highlights cursor position after navigation
- Line numbers enabled globally (disabled in org/term/eshell)

### Completion
- **vertico**: Vertical minibuffer completion
- **orderless**: Out-of-order pattern matching
- **marginalia**: Rich annotations in completion buffers
- **consult**: Enhanced search and navigation commands
- **embark**: Context-aware actions
- **company**: In-buffer completion with tab-and-go mode

### Development
- **lsp-mode**: Language Server Protocol support (configured for C#)
- **flycheck**: On-the-fly syntax checking
- **projectile**: Project management
- **editorconfig**: Respects .editorconfig files

### Git
- **magit**: Git interface
- **forge**: GitHub/GitLab integration
- **diff-hl**: Highlight uncommitted changes
- **git-gutter**: Show git diff in the fringe

### Editing
- **expand-region**: Semantic region expansion
- **wgrep**: Edit grep results in-place

### Org Mode
- Custom visual configuration with variable-pitch fonts
- List items displayed as bullets

## Key Bindings

### Completion (Company)
- `TAB` - Complete selection
- `C-n` - Next candidate (inserts as you navigate)
- `C-p` - Previous candidate

### Search (Consult)
- `M-s M-g` - Recursive grep
- `M-s M-f` - Find files recursively
- `M-s M-o` - Search outline/headings
- `M-s M-l` - Search current buffer
- `M-s M-b` - Switch buffer/recent file

### Actions (Embark)
- `C-.` - Show actions for thing at point
- `C-c C-c` - Collect results (in minibuffer)
- `C-c C-e` - Export results (in minibuffer)

### Project (Projectile)
- `C-c p` - Projectile command map

### Git (Magit)
- `C-x g` - Magit status
- `C-x M-g` - Magit dispatch
- `C-c M-g` - File-specific git menu

### Region
- `C-=` - Expand region

## File Structure

```
~/.emacs.d/
├── early-init.el    # Early initialization (disables package.el)
├── init.el          # Main configuration
├── custom.el        # Customization settings (auto-generated)
├── straight/        # Package installation directory (ignored)
└── .cache/          # Runtime cache (ignored)
```

## Notes

- Custom settings are stored in `custom.el` and loaded automatically
- LSP servers must be installed separately (e.g., OmniSharp for C#)
- On macOS, `exec-path-from-shell` ensures environment variables are inherited correctly
