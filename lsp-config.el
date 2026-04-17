;;; lsp-config.el --- LSP and Debugging configuration -*- lexical-binding: t -*-

;;; ------------------------------------------------------------
;;; Lsp
;;; ------------------------------------------------------------

(use-package lsp-mode
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-c l")
  :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
         (csharp-mode . lsp-deferred)
	 (c-mode . lsp-deferred)
         (go-mode . lsp-deferred)
         (typescript-ts-mode . lsp-deferred)
         (tsx-ts-mode . lsp-deferred)
         (js-ts-mode . lsp-deferred)
         (js-mode . lsp-deferred)
         (html-mode . lsp-deferred)
         (mhtml-mode . lsp-deferred)
         (css-mode . lsp-deferred)
         (css-ts-mode . lsp-deferred)
         (json-mode . lsp-deferred)
         (json-ts-mode . lsp-deferred)

         ;; if you want which-key integration
         (lsp-mode . lsp-enable-which-key-integration))
  :commands (lsp lsp-deferred)
  :config
  (require 'lsp-angular)
  (setq lsp-headerline-breadcrumb-enable nil)
  (setq lsp-use-plists t)
 ;; Tell lsp-mode to use the echo area (under the modeline) for signatures
  ;; (setq lsp-signature-function 'eldoc)
  (setq lsp-eldoc-enable-hover t)
  ;; Only use the echo area for quick signatures, let eldoc-box handle the child frame.
  ;; This prevents the *eldoc* buffer from popping up automatically and splitting the window.
  (setq eldoc-display-functions '(eldoc-display-in-echo-area))
  ;; Use nerd-icons for the code action modeline icon
  (when (require 'nerd-icons nil t)
    (setq lsp-modeline-code-action-fallback-icon 
          (propertize " " 'display (nerd-icons-mdicon "nf-md-lightbulb" :face 'warning :v-adjust -0.05)))))

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

  (add-hook 'typescript-ts-mode-hook
            (lambda () (require 'dap-node)))

  (add-hook 'tsx-ts-mode-hook
            (lambda () (require 'dap-node)))

  (add-hook 'js-ts-mode-hook
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

(provide 'lsp-config)
;;; lsp-config.el ends here
