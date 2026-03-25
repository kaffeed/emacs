;;; init.el --- Main configuration  -*- lexical-binding: t; -*-

;;; ------------------------------------------------------------
;;; Core Settings
;;; ------------------------------------------------------------


(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)


;;; ------------------------------------------------------------
;;; Emacs Built-in Configuration
;;; ------------------------------------------------------------

(defconst *is-a-linux* (eq system-type 'gnu/linux))
(defconst *is-a-windoof* (eq system-type 'windows-nt))


(defun ss/scroll-half-page-down ()
  "Scroll down half a page, like Vim's C-d."
  (interactive)
  (let ((half-page (/ (window-body-height) 2)))
    (scroll-up-command half-page)))

(defun ss/scroll-half-page-up ()
  "Scroll up half a page, like Vim's C-u."
  (interactive)
  (let ((half-page (/ (window-body-height) 2)))
    (scroll-down-command half-page)))


;; This prevents frame deletion issues during Emacs shutdown
(defun ss/safe-kill-emacs ()
  "Kill Emacs gracefully without frame deletion errors."
  (interactive)
  (condition-case nil
      (save-buffers-kill-terminal)
    (error
     (when (yes-or-no-p "Force quit Emacs? ")
       (kill-emacs)))))

;;; ------------------------------------------------------------
(defun ss/open-external-terminal ()
  "Open external terminal in current directory.
In Dired, opens terminal in the directory being viewed.
Otherwise, opens in the directory of the current file."
  (interactive)
  (let* ((dir (if (eq major-mode 'dired-mode)
                  default-directory
                (file-name-directory (or buffer-file-name default-directory))))
         ;; Convert to Windows path format and remove trailing slash
         (win-dir (directory-file-name (convert-standard-filename dir))))
    (cond
     ;; Windows: Try Windows Terminal first, then PowerShell
     (*is-a-windoof*
      (let ((wt-path (or (executable-find "wt.exe")
                         (executable-find "wt"))))
        (if wt-path
            (progn
              (message "Opening Windows Terminal in: %s" win-dir)
              ;; Use w32-shell-execute for better Windows GUI app launching
              (w32-shell-execute "open" wt-path (format "-d \"%s\"" win-dir)))
          ;; Fallback to PowerShell
          (progn
            (message "Windows Terminal not found, using PowerShell in: %s" win-dir)
            (w32-shell-execute "open" "powershell.exe"
                               (format "-NoExit -Command \"Set-Location '%s'\"" win-dir))))))
     ;; Linux: Try common terminals
     (*is-a-linux*
      (cond
       ((executable-find "gnome-terminal")
        (start-process "external-terminal" nil "gnome-terminal" "--working-directory" dir))
       ((executable-find "konsole")
        (start-process "external-terminal" nil "konsole" "--workdir" dir))
       ((executable-find "xterm")
        (start-process "external-terminal" nil "xterm" "-e" (format "cd '%s' && bash" dir)))
       (t (message "No supported terminal found"))))
     ;; macOS
     ((eq system-type 'darwin)
      (start-process "external-terminal" nil "open" "-a" "Terminal" dir)))))


(use-package emacs
  :init
  ;; Prefer UTF-8 for everything
  (prefer-coding-system 'utf-8)
  (setq locale-coding-system 'utf-8)
  (set-terminal-coding-system 'utf-8)
  (set-keyboard-coding-system 'utf-8)
  (set-selection-coding-system 'utf-8)
  (setq default-buffer-file-coding-system 'utf-8)

  ;; Performance: raise GC threshold to 50 MB during startup to reduce GC pauses
  (setq gc-cons-threshold (* 50 1000 1000))
  (setq read-process-output-max (* 1024 1024))

  (setq inhibit-startup-message t
        inhibit-startup-screen t
        visible-bell t)

  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (column-number-mode)

  (global-display-line-numbers-mode t)
  (dolist (mode '(org-mode-hook
                  term-mode-hook
                  eshell-mode-hook))
    (add-hook mode (lambda () (display-line-numbers-mode 0))))

  (when *is-a-linux*
    (setq-default x-super-keysym 'meta))

  ;; Start frames maximized
  (add-to-list 'default-frame-alist '(fullscreen . maximized))

  ;; Default font: Iosevka Nerd Font Mono 14pt
  (set-face-attribute 'default nil :font "Iosevka NFM" :height 140)
  (add-to-list 'default-frame-alist '(font . "Iosevka NFM-14"))

  :config
  ;; Backup and version control
  (setq backup-directory-alist
        `(("." . ,(expand-file-name "backups/" user-emacs-directory))))
  (setq backup-by-copying t
        delete-old-versions t
        kept-new-versions 6
        kept-old-versions 2
        version-control t)

  ;; Better scrolling behavior
  (setq scroll-margin 3
        scroll-conservatively 101
        scroll-preserve-screen-position t
        auto-window-vscroll nil)

  ;; Case-insensitive completion
  (setq read-buffer-completion-ignore-case t
        read-file-name-completion-ignore-case t
        completion-ignore-case t)

  ;; Save cursor position between sessions
  (save-place-mode 1)

  ;; Better kill ring
  (setq kill-ring-max 200
        save-interprogram-paste-before-kill t)

  ;; Single space ends sentence (modern convention)
  (setq sentence-end-double-space nil)

  ;; Show trailing whitespace in programming modes
  (add-hook 'prog-mode-hook
            (lambda () (setq show-trailing-whitespace t)))

  (setq custom-file (locate-user-emacs-file "custom-vars.el"))
  (load custom-file 'noerror 'nomessage)

  (setq create-lockfiles nil)
  (setq next-line-add-newlines t)

  (let ((autosave-dir (expand-file-name "autosave/" user-emacs-directory)))
    (setq auto-save-list-file-prefix autosave-dir)
    (setq auto-save-file-name-transforms
          `((".*" ,autosave-dir t))))

  ;; Git commit enhancements
  (add-hook 'git-commit-setup-hook #'git-commit-turn-on-flyspell)
  (add-hook 'git-commit-setup-hook #'turn-on-auto-fill)

  :bind
  (("C-x C-z" . nil)
   ("C-z" . nil)
   ("C-v" . ss/scroll-half-page-down)
   ("M-v" . ss/scroll-half-page-up)
   ("C-x C-c" . ss/safe-kill-emacs)
   ("C-c c" . compile)
   ("C-c ]" . next-error)
   ("C-c [" . previous-error)
   ("M-n" . next-error)
   ("M-p" . previous-error)
   ("<f5>" . revert-buffer)
   ("<f6>" . org-capture)
   ("<f7>" . org-agenda)
   ("<f8>" . magit-status)
   ("<f9>" . compile)
   ("<f12>" . eshell)
   ("C-x <up>" . windmove-up)
   ("C-x <down>" . windmove-down)
   ("C-x <left>" . windmove-left)
   ("C-x <right>" . windmove-right)
   ("C-x C-<up>" . enlarge-window)
   ("C-x C-<down>" . shrink-window)
   ("C-x C-<left>" . shrink-window-horizontally)
   ("C-x C-<right>" . enlarge-window-horizontally)
   ("C-<tab>" . next-buffer)
   ("C-S-<tab>" . previous-buffer)
   ("C-c e" . eshell)
   ("C-c t" . org-todo-list)
   ("C-c w" . delete-trailing-whitespace)
   ("C-c RET" . ss/open-external-terminal)
   ("C-c C-b" . ibuffer)
   ("M-s f" . find-name-dired)
   ("M-j" . duplicate-dwim)))

;; Configure use-package to use straight.el by default
(setq straight-use-package-by-default t
      use-package-verbose nil)

;; Install Org mode early to prevent version mismatch
;; This must come before loading org-config.el
(straight-use-package 'org)

(use-package compile-angel
  :demand t
  :config
  (setq compile-angel-verbose nil)
  (push "/init.el" compile-angel-excluded-files)
  (push "/early-init.el" compile-angel-excluded-files)
  (compile-angel-on-load-mode 1))

;;; ------------------------------------------------------------
;;; Org-Mode setup
;;; ------------------------------------------------------------
;; Org-mode configuration is split into a separate file for better organization.
;; See org-config.el for the complete org-mode setup including:
;; - Custom agenda views (dashboard, sprint, backlog, etc.)
;; - Capture templates for tasks, user stories, meetings
;; - Time tracking and effort estimation
;; - Azure DevOps integration
(load (expand-file-name "org-config.el" user-emacs-directory))


;;; ------------------------------------------------------------
;;; OS Settings

(use-package standard-themes
  :init
  (standard-themes-take-over-modus-themes-mode 1)
  :bind
  (("C-<f5>" . modus-themes-rotate)
   ("M-<f5>" . modus-themes-select)
   ("C-M-<f5>" . modus-themes-load-random))
  :config
  (setq modus-themes-mixed-fonts t)
  (setq modus-themes-italic-constructs t)
  (modus-themes-load-theme 'standard-light))

;;; ------------------------------------------------------------
;;; Appearance
;;; ------------------------------------------------------------

;;; ------------------------------------------------------------
;;; Environment Variables (important for macOS)
;;; ------------------------------------------------------------
(use-package exec-path-from-shell
  :init
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

;;; ------------------------------------------------------------
;;; Recentf - Track and quickly access recently opened files
;;; ------------------------------------------------------------
(use-package recentf
  :straight (:type built-in)
  :config
  (recentf-mode 1)
  (setq recentf-max-menu-items 50
        recentf-max-saved-items 50)
  ;; Auto-save recentf list every 5 minutes
  (run-at-time nil (* 5 60) 'recentf-save-list)
  :bind ("C-c r" . recentf-open-files))

;;; ------------------------------------------------------------
;;; Completion UI (Vertico + Consult + Orderless + Marginalia)
;;; ------------------------------------------------------------

;; This section configures a modern completion system using:
;; - Vertico: Vertical completion interface
;; - Orderless: Flexible matching (space-separated patterns)
;; - Marginalia: Rich annotations in completion candidates
;; - Consult: Enhanced search and navigation commands
;; - Embark: Context actions on completion candidates

;; Vertico: Fast, minimal vertical completion UI
;; Preferred over ivy/helm for simplicity and performance
;; Further reading: https://protesilaos.com/emacs/dotemacs#h:cff33514-d3ac-4c16-a889-ea39d7346dc5
(use-package vertico
  :config
  (setq vertico-cycle t)
  (setq vertico-resize nil)
  (vertico-mode 1)
  ;; Load vertico-multiform feature before configuring it
  (require 'vertico-multiform)
  (add-to-list 'vertico-multiform-categories '(embark-keybinding grid))
  (vertico-multiform-mode))

;; The `marginalia' package provides helpful annotations next to
;; completion candidates in the minibuffer.  The information on
;; display depends on the type of content.  If it is about files, it
;; shows file permissions and the last modified date.  If it is a
;; buffer, it shows the buffer's size, major mode, and the like.
;;
;; Further reading: https://protesilaos.com/emacs/dotemacs#h:bd3f7a1d-a53d-4d3e-860e-25c5b35d8e7e
(use-package marginalia
  :config
  (marginalia-mode 1))

;; The `orderless' package lets the minibuffer use an out-of-order
;; pattern matching algorithm.  It matches space-separated words or
;; regular expressions in any order.  In its simplest form, something
;; like "ins pac" matches `package-menu-mark-install' as well as
;; `package-install'.  This is a powerful tool because we no longer
;; need to remember exactly how something is named.
;;
;; Note that Emacs has lots of "completion styles" (pattern matching
;; algorithms), but let us keep things simple.
;;
;; Further reading: https://protesilaos.com/emacs/dotemacs#h:7cc77fd0-8f98-4fc0-80be-48a758fcb6e2
(use-package orderless
  :config
  (setq completion-styles '(orderless basic)))


;; The `consult' package provides lots of commands that are enhanced
;; variants of basic, built-in functionality.  One of the headline
;; features of `consult' is its preview facility, where it shows in
;; another Emacs window the context of what is currently matched in
;; the minibuffer.  Here I define key bindings for some commands you
;; may find useful.  The mnemonic for their prefix is "alternative
;; search" (as opposed to the basic C-s or C-r keys).
;;
;; Further reading: https://protesilaos.com/emacs/dotemacs#h:22e97b4c-d88d-4deb-9ab3-f80631f9ff1d
(use-package consult
  :bind (;; A recursive grep
         ("M-s M-g" . consult-grep)
         ;; Search for files names recursively
         ("M-s M-f" . consult-find)
         ;; Search through the outline (headings) of the file
         ("M-s M-o" . consult-outline)
         ;; Search the current buffer
         ("M-s M-l" . consult-line)
         ;; Switch to another buffer, or bookmarked file, or recently
         ;; opened file.
         ("M-s M-b" . consult-buffer)
         ("C-x C-b" . consult-buffer)
         ("C-c s" . consult-ripgrep)
         ("C-c b" . consult-bookmark)
         ("C-c i" . consult-imenu)))


;; The `embark' package lets you target the thing or context at point
;; and select an action to perform on it.  Use the `embark-act'
;; command while over something to find relevant commands.
;;
;; When inside the minibuffer, `embark' can collect/export the
;; contents to a fully fledged Emacs buffer.  The `embark-collect'
;; command retains the original behaviour of the minibuffer, meaning
;; that if you navigate over the candidate at hit RET, it will do what
;; the minibuffer would have done.  In contrast, the `embark-export'
;; command reads the metadata to figure out what category this is and
;; places them in a buffer whose major mode is specialised for that
;; type of content.  For example, when we are completing against
;; files, the export will take us to a `dired-mode' buffer; when we
;; preview the results of a grep, the export will put us in a
;; `grep-mode' buffer.
;;
;; Further reading: https://protesilaos.com/emacs/dotemacs#h:61863da4-8739-42ae-a30f-6e9d686e1995
(use-package embark
  :bind (("C-." . embark-act)
         ("C-;" . embark-dwim)
         ("C-h B" . embark-bindings)
         :map minibuffer-local-map
         ("C-c C-c" . embark-collect)
         ("C-c C-e" . embark-export))
  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none))))
  )

;; The `embark-consult' package is glue code to tie together `embark'
;; and `consult'.
(use-package embark-consult
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

;; The `wgrep' packages lets us edit the results of a grep search
;; while inside a `grep-mode' buffer.  All we need is to toggle the
;; editable mode, make the changes, and then type C-c C-c to confirm
;; or C-c C-k to abort.
;;
;; Further reading: https://protesilaos.com/emacs/dotemacs#h:9a3581df-ab18-4266-815e-2edd7f7e4852
(use-package wgrep
  :bind ( :map grep-mode-map
          ("e" . wgrep-change-to-wgrep-mode)
          ("C-x C-q" . wgrep-change-to-wgrep-mode)
          ("C-c C-c" . wgrep-finish-edit)))

;;; ------------------------------------------------------------
;;; Projectile
;;; ------------------------------------------------------------
(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  (when (file-directory-p "~/source")
    (setq projectile-project-search-path '("~/source" . 1)))
  (setq projectile-switch-project-action #'projectile-find-file)
  :custom
  ;; Use hybrid indexing: git for git projects, native for others
  ;; This ensures .gitignore files are properly respected
  (projectile-indexing-method 'hybrid)
  (projectile-enable-caching t)
  ;; Explicitly set git command to exclude files per .gitignore
  (projectile-git-command "git ls-files -zco --exclude-standard"))

;;; ------------------------------------------------------------
;;; Company (Completion)
;;; ------------------------------------------------------------

;; Company: Modern completion framework
;; Chosen over alternatives (corfu, auto-complete) for:
;; - Mature, well-tested codebase
;; - Excellent backend support (LSP, dabbrev, files, etc.)
;; - Works seamlessly with LSP mode
(use-package company
  :init
  (global-company-mode)

  :custom
  ;; Make Company less intrusive and more like modern IDE autocomplete
  (company-idle-delay 0.05)        ;; fast popup
  (company-minimum-prefix-length 1)
  (company-tooltip-align-annotations t)
  (company-tooltip-limit 12)
  (company-tooltip-minimum-width 40)
  (company-show-numbers t)
  (company-require-match nil)
  (company-dabbrev-other-buffers t)
  (company-dabbrev-downcase nil)

  :bind
  (:map company-active-map
        ("<tab>" . company-complete-selection)
        ("TAB"   . company-complete-selection)
        ("C-n"   . company-select-next)
        ("C-p"   . company-select-previous))

  :config
  (company-tng-mode)
  ;; Good backends for general programming
  ;; Integrate yasnippet with company for LSP completions
  (setq company-backends
        '((company-capf :with company-yasnippet) ;; LSP + snippets together
           company-dabbrev-code                   ;; fallback for code-like text
          company-dabbrev))                       ;; fallback for everything else
  )

;; Flycheck: Modern syntax checking
;; Preferred over built-in flymake for:
;; - Better LSP integration
;; - More checker support (eslint, pylint, etc.)
;; - Cleaner error reporting
(use-package flycheck
  :config
  (add-hook 'prog-mode-hook 'flycheck-mode) ;; always lint my code
  (add-hook 'after-init-hook #'global-flycheck-mode))

;; Yasnippet: Template system for code snippets
;; Dramatically improves coding speed with pre-defined templates
;; for common patterns (class definitions, methods, loops, etc.)
(use-package yasnippet
  :config
  (yas-global-mode 1)
  :diminish yas-minor-mode)

;; Yasnippet-snippets: Collection of snippets for many languages
(use-package yasnippet-snippets
  :after yasnippet)

;;; ------------------------------------------------------------
;;; Tree-sitter - Modern syntax highlighting and code parsing
;;; ------------------------------------------------------------
;; Tree-sitter provides much better syntax highlighting than traditional
;; regex-based modes by using actual language parsers

(use-package tree-sitter
  :config
  (global-tree-sitter-mode)
  :hook (tree-sitter-after-on-hook . tree-sitter-hl-mode))

;; Tree-sitter language grammars for various programming languages
(use-package tree-sitter-langs
  :after tree-sitter)

;;; ------------------------------------------------------------
;;; Lsp
;;; ------------------------------------------------------------

(use-package lsp-mode
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-c l")
  :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
         (csharp-mode . lsp-deferred)
         (go-mode . lsp-deferred)
         (typescript-mode . lsp-deferred)
         (js-mode . lsp-deferred)

         ;; if you want which-key integration
         (lsp-mode . lsp-enable-which-key-integration))
  :commands (lsp lsp-deferred)
  :config
  (setq lsp-headerline-breadcrumb-enable nil)
  (setq lsp-use-plists t))

;; Performance: Increase process output buffer to 1MB

(use-package lsp-ui :commands lsp-ui-mode
  :config
  (setq lsp-ui-doc-show-with-cursor t)
  (setq lsp-ui-doc-show-with-mouse nil))

(use-package which-key :config (which-key-mode))

;;; ------------------------------------------------------------
;;; DAP Mode - Debug Adapter Protocol
;;; ------------------------------------------------------------
;; DAP Mode provides debugging support for multiple languages using
;; the Debug Adapter Protocol. This integrates with our existing
;; LSP setup to provide a complete IDE-like debugging experience.
;;
;; Supported languages:
;; - .NET Core (C#/F#) via netcoredbg
;; - Go via Delve
;; - Node.js/Next.js (JavaScript/TypeScript) via vscode-node-debug2
;;
;; Quick start:
;; 1. Set breakpoints: C-c D b
;; 2. Start debugging: F10 or C-c D d
;; 3. Use hydra menu: C-c D h for all debug commands

(use-package dap-mode
  :after lsp-mode
  :commands (dap-debug dap-debug-edit-template)

  :init
  ;; Enable dap-mode and dap-ui-mode when LSP is active
  (add-hook 'lsp-mode-hook #'dap-mode)
  (add-hook 'dap-mode-hook #'dap-ui-mode)

  :bind
  (;; Main debug prefix: C-c D
   :map dap-mode-map
   ("C-c D d" . dap-debug)
   ("C-c D l" . dap-debug-last)
   ("C-c D e" . dap-debug-edit-template)
   ("C-c D h" . dap-hydra)

   ;; Breakpoint management
   ("C-c D b" . dap-breakpoint-toggle)
   ("C-c D B" . dap-breakpoint-delete-all)
   ("C-c D c" . dap-breakpoint-condition)

   ;; Session control
   ("C-c D n" . dap-next)
   ("C-c D i" . dap-step-in)
   ("C-c D o" . dap-step-out)
   ("C-c D r" . dap-continue)
   ("C-c D Q" . dap-disconnect)

   ;; UI controls
   ("C-c D u" . dap-ui-repl)

   ;; Quick access
   ("<f10>" . dap-debug))

  :custom
  ;; UI Configuration
  (dap-auto-configure-features
   '(sessions locals breakpoints expressions controls tooltip))
  (dap-auto-show-output t)

  :config
  (dap-ui-mode 1)

  ;; Windows-specific configuration
  (when *is-a-windoof*
    (setq dap-utils-extension-path
          (expand-file-name "dap-extensions" user-emacs-directory))))

(use-package dap-hydra
  :after dap-mode
  :straight nil
  :commands dap-hydra)

;;; .NET Core / C# Debugging
(use-package dap-netcore
  :straight nil
  :after dap-mode
  :demand t  ;; Load immediately after dap-mode
  :custom
  ;; Set the download URL explicitly to avoid auto-detection failures
  ;; Update version number as needed from: https://github.com/Samsung/netcoredbg/releases
  (dap-netcore-download-url "https://github.com/Samsung/netcoredbg/releases/download/3.1.3-1062/netcoredbg-win64.zip")
  :config
  (require 'dap-netcore)

  ;; Console application template - prompts for DLL file
  (dap-register-debug-template
   ".NET Core Launch (console)"
   (list :type "coreclr"
         :request "launch"
         :mode "launch"
         :name ".NET Core Launch"
         :program (lambda () (read-file-name "Select DLL to debug: " (projectile-project-root) nil t nil
                                             (lambda (name) (string-match-p "\\.dll$" name))))
         :cwd (lambda () (projectile-project-root))
         :stopAtEntry nil
         :console "integratedTerminal"))

  ;; Web application template - prompts for DLL file
  (dap-register-debug-template
   ".NET Core Launch (web)"
   (list :type "coreclr"
         :request "launch"
         :name ".NET Core Launch (web)"
         :program (lambda () (read-file-name "Select DLL to debug: " (projectile-project-root) nil t nil
                                             (lambda (name) (string-match-p "\\.dll$" name))))
         :cwd (lambda () (projectile-project-root))
         :stopAtEntry nil
         :env (list "ASPNETCORE_ENVIRONMENT" "Development")
         :console "integratedTerminal"))

  ;; Attach to process
  (dap-register-debug-template
   ".NET Core Attach"
   (list :type "coreclr"
         :request "attach"
         :name ".NET Core Attach"
         :processId "${command:pickProcess}")))

;;; Go Debugging
(use-package dap-go
  :straight nil
  :after dap-mode
  :config
  (require 'dap-go)

  (add-hook 'go-mode-hook
            (lambda () (require 'dap-go)))

  ;; Launch package
  (dap-register-debug-template
   "Go Launch Package"
   (list :type "go"
         :request "launch"
         :name "Launch Package"
         :mode "debug"
         :program "${workspaceFolder}"
         :cwd "${workspaceFolder}"))

  ;; Debug test
  (dap-register-debug-template
   "Go Test Current Function"
   (list :type "go"
         :request "launch"
         :name "Test Current Function"
         :mode "test"
         :program "${workspaceFolder}"
         :args ["-test.run" "${function}"]
         :cwd "${workspaceFolder}")))

;;; Node.js / Next.js / TypeScript Debugging
(use-package dap-node
  :straight nil
  :after dap-mode
  :config
  (require 'dap-node)

  (add-hook 'typescript-mode-hook
            (lambda () (require 'dap-node)))

  (add-hook 'js-mode-hook
            (lambda () (require 'dap-node)))

  ;; Next.js dev server
  (dap-register-debug-template
   "Next.js Dev Server"
   (list :type "node"
         :request "launch"
         :name "Next.js Dev"
         :runtimeExecutable "npm"
         :runtimeArgs ["run" "dev"]
         :cwd "${workspaceFolder}"
         :sourceMaps t
         :protocol "inspector"
         :console "integratedTerminal"
         :serverReadyAction (list :pattern "started server on .+, url: (https?://.+)"
                                  :uriFormat "%s"
                                  :action "openExternally")))

  ;; Next.js server-side debugging
  (dap-register-debug-template
   "Next.js Server-Side"
   (list :type "node"
         :request "launch"
         :name "Next.js Server-Side"
         :runtimeExecutable "npm"
         :runtimeArgs ["run" "dev"]
         :cwd "${workspaceFolder}"
         :sourceMaps t
         :protocol "inspector"
         :outFiles ["${workspaceFolder}/.next/**/*.js"]
         :skipFiles ["<node_internals>/**"]
         :console "integratedTerminal"))

  ;; Jest tests
  (dap-register-debug-template
   "Node Jest Tests"
   (list :type "node"
         :request "launch"
         :name "Jest Tests"
         :program "${workspaceFolder}/node_modules/.bin/jest"
         :args ["--runInBand" "--no-coverage" "${file}"]
         :cwd "${workspaceFolder}"
         :sourceMaps t
         :protocol "inspector"
         :console "integratedTerminal")))


;; Helpful: Much better help buffers with examples, source code, and references
;; Replaces default help commands with more informative versions
(use-package helpful
  :bind (("C-h f" . helpful-callable)     ; Describe function
         ("C-h v" . helpful-variable)     ; Describe variable
         ("C-h k" . helpful-key)          ; Describe key
         ("C-h F" . helpful-function)     ; Describe function (only functions)
         ("C-h C" . helpful-command)))    ; Describe command

(use-package sharper
  :demand t
  :bind
  ("C-c d" . sharper-main-transient))

;;; ------------------------------------------------------------
;;; Magit
;;; ------------------------------------------------------------

(use-package magit
  :commands (magit-status magit-get-current-branch)
  :bind (("C-x g" . magit-status)
         ("C-x M-g" . magit-dispatch)
         ("C-c M-g" . magit-file-dispatch))
  :custom
  (magit-display-buffer-function #'magit-display-buffer-fullcolumn-most-v1)
  (magit-commit-show-diff nil))

(use-package diff-hl
  :hook ((prog-mode . diff-hl-mode)
         (text-mode . diff-hl-mode)
         (magit-pre-refresh . diff-hl-magit-pre-refresh)
         (magit-post-refresh . diff-hl-magit-post-refresh)))

(with-eval-after-load 'projectile
  (define-key projectile-mode-map (kbd "C-c p g") #'magit-status))

;; Git commit enhancements

;; Forge: GitHub/GitLab integration for Magit
;; Enables working with PRs, issues, and code reviews from within Emacs
(use-package forge
  :after magit)

;; Magit keybindings:
;; C-x g     - magit-status (main interface)

;;; ------------------------------------------------------------
;;; Misc packages
;;; ------------------------------------------------------------

;;; ------------------------------------------------------------

;;; ------------------------------------------------------------


(use-package docker)

(use-package dockerfile-mode
  :straight (:type git :host github :repo "spotify/dockerfile-mode")
  :mode ("Dockerfile\\'" "\\.dockerfile\\'"))

;;; ------------------------------------------------------------
;;; Undo-tree - Visual undo/redo with branching history
;;; ------------------------------------------------------------
;; Undo-tree makes Emacs' powerful undo system visual and intuitive
;; Shows undo history as a tree structure you can navigate

(use-package undo-tree
  :config
  (global-undo-tree-mode)
  :bind ("C-x u" . undo-tree-visualize)
  :diminish undo-tree-mode)

(use-package ace-window
  :bind (("M-o" . ace-window))
  :custom
  (aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))

;; Avy: Jump to visible text using character-based decision tree
;; Essential for quick navigation without mouse or arrow keys
(use-package avy
  :bind (("C-:" . avy-goto-char-2)      ; Jump to 2-char combination
         ("C-'" . avy-goto-line)         ; Jump to line
         ("M-g f" . avy-goto-line)       ; Alternative line jump
         ("M-g w" . avy-goto-word-1)))   ; Jump to word

(use-package multiple-cursors
  :bind (("C->" .           mc/mark-next-like-this)
         ("C-<" .           mc/mark-previous-like-this)
         ("C-c C-<" .       mc/mark-all-like-this)
         ("C-S-c C-S-c" .   mc/edit-lines)
         ("C-S-<mouse-1>" . mc/add-cursor-on-click)))

(use-package rg
  :straight (:type git :host github :repo "dajva/rg.el")
  :config (rg-enable-default-bindings))

(use-package expand-region
  :bind ("C-=" . er/expand-region))

;; Smartparens: Intelligent handling of parentheses, quotes, and brackets
;; Auto-pairs delimiters and provides smart navigation/manipulation commands
(use-package smartparens
  :hook (prog-mode text-mode markdown-mode)
  :config
  (require 'smartparens-config)
  (smartparens-global-mode 1)
  (show-smartparens-global-mode 1)
  :diminish smartparens-mode
  :bind (:map smartparens-mode-map
              ("C-M-f" . sp-forward-sexp)
              ("C-M-b" . sp-backward-sexp)
              ("C-M-n" . sp-next-sexp)
              ("C-M-p" . sp-previous-sexp)
              ("C-M-k" . sp-kill-sexp)
              ("C-M-w" . sp-copy-sexp)))

;; Rainbow-delimiters: Color-code nested parentheses by depth
;; Essential for Lisp, helpful for all languages with nested structures
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package pulsar
  :bind
  (:map global-map
        ("C-x l" . pulsar-pulse-line) ; overrides `count-lines-page'
        ("C-x L" . pulsar-highlight-permanently-dwim)) ; or use `pulsar-highlight-temporarily-dwim'
  :init
  (pulsar-global-mode 1)
  :config
  (setq pulsar-delay 0.055)
  (setq pulsar-iterations 5)
  (setq pulsar-face 'pulsar-green)
  (setq pulsar-region-face 'pulsar-yellow)
  (setq pulsar-highlight-face 'pulsar-magenta))

;; EditorConfig: Respect .editorconfig files for consistent coding styles
;; Automatically applies indent style, tab width, line endings, etc.
;; from .editorconfig files in project roots
(use-package editorconfig
  :config
  (editorconfig-mode t))

;;; ------------------------------------------------------------
;;; Window Layout Management
;;; ------------------------------------------------------------

;; Transpose-frame: Quickly rearrange window layouts
;; Useful for rotating, flipping, and transposing window arrangements
(use-package transpose-frame
  :bind (("C-x 5 t" . transpose-frame)
         ("C-x 5 f" . flip-frame)
         ("C-x 5 r" . rotate-frame-clockwise)))

;;; ------------------------------------------------------------
;;; REST API Testing
;;; ------------------------------------------------------------

;; Restclient: Test APIs directly in Emacs
;; Great for backend development and API testing
(use-package restclient
  :mode ("\\.http\\'" . restclient-mode))

;; Company backend for restclient
(use-package company-restclient
  :after (company restclient)
  :config
  (add-to-list 'company-backends 'company-restclient))

;;; ------------------------------------------------------------
;;; PDF Viewing
;;; ------------------------------------------------------------

;; PDF-tools: Much better PDF viewing than DocView
;; Provides smooth scrolling, text search, annotations, and more
(use-package pdf-tools
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :config
  (pdf-tools-install :no-query))

;;; ------------------------------------------------------------
;;; Knowledge Management
;;; ------------------------------------------------------------

;; Org-roam: Zettelkasten note-taking system
;; Creates a network of interconnected notes for knowledge management
(use-package org-roam
  :after org
  :custom
  (org-roam-directory "~/org/roam")
  (org-roam-completion-everywhere t)
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n c" . org-roam-capture)
         ("C-c n g" . org-roam-graph))
  :config
  (org-roam-db-autosync-mode))

;;; ------------------------------------------------------------
;;; Communication & Utilities
;;; ------------------------------------------------------------

(use-package ement
  :straight (:type git :host github :repo "alphapapa/ement.el"))

(use-package powershell
  :config
  (add-hook 'powershell-mode-hook
            (lambda ()
              (ansi-color-for-comint-mode-on)
              (setq comint-process-echoes t)))
  (add-hook 'comint-output-filter-functions
            'ansi-color-process-output))

;;; ------------------------------------------------------------

;; Bind to dired-mode
(with-eval-after-load 'dired
  (define-key dired-mode-map (kbd "C-c RET") #'ss/open-external-terminal))


;;; ------------------------------------------------------------
;;; DevDocs - Browse devdocs.io documentation
;;; ------------------------------------------------------------

(use-package devdocs
  :straight (:type git :host github :repo "astoff/devdocs.el")
  :bind (("C-c m" . devdocs-lookup)
         ("C-c M" . devdocs-install))
  :custom
  ;; Cache directory for downloaded documentation
  (devdocs-data-dir (expand-file-name "devdocs" user-emacs-directory))
  :config
  ;; Automatically install docs for current major mode
  (add-hook 'python-mode-hook
            (lambda () (setq-local devdocs-current-docs '("python~3.12"))))
  (add-hook 'js-mode-hook
            (lambda () (setq-local devdocs-current-docs '("javascript" "node"))))
  (add-hook 'typescript-mode-hook
            (lambda () (setq-local devdocs-current-docs '("typescript" "node"))))
  (add-hook 'csharp-mode-hook
            (lambda () (setq-local devdocs-current-docs '("dotnet~8.0"))))
  (add-hook 'go-mode-hook
            (lambda () (setq-local devdocs-current-docs '("go"))))
  (add-hook 'emacs-lisp-mode-hook
            (lambda () (setq-local devdocs-current-docs '("elisp")))))


;; Optional: Auto-cleanup trailing whitespace on save
;; Uncomment the next line to enable:
;; (add-hook 'before-save-hook 'delete-trailing-whitespace)



(provide 'init)
;;; init.el ends here
