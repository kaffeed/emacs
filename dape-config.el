;;; dape-config.el --- Debug Adapter Protocol configuration -*- lexical-binding: t -*-

;;; ------------------------------------------------------------
;;; Dape - Debug Adapter Protocol (eglot-compatible)
;;; ------------------------------------------------------------
;; Dape is a modern DAP client that works independently of lsp-mode.
;; It replaces dap-mode and integrates cleanly with eglot.
;;
;; Supported debuggers (must be installed separately):
;; - C#:         netcoredbg (https://github.com/Samsung/netcoredbg/releases)
;; - Go:         Delve (go install github.com/go-delve/delve/cmd/dlv@latest)
;; - Node.js/TS: vscode-js-debug (installed automatically by dape)

(use-package dape
  :straight (:type git :host github :repo "svaante/dape")

  :bind
  (("C-c D d" . dape)
   ("C-c D l" . dape-repl)
   ("C-c D b" . dape-breakpoint-toggle)
   ("C-c D B" . dape-breakpoint-remove-all)
   ("C-c D c" . dape-continue)
   ("C-c D n" . dape-next)
   ("C-c D i" . dape-step-in)
   ("C-c D o" . dape-step-out)
   ("C-c D r" . dape-restart)
   ("C-c D Q" . dape-quit)
   ("C-c D w" . dape-watch-dwim)
   ("C-c D e" . dape-evaluate-expression)
   ("<f10>"   . dape))

  :custom
  (dape-buffer-window-arrangement 'right) ;; debug windows on the right

  :config
  ;; Show repl on start
  (add-hook 'dape-start-hook
            (lambda () (save-excursion (dape-repl))))

  ;; Auto-save before debugging
  (add-hook 'dape-start-hook
            (lambda () (save-some-buffers t t)))

  ;; .NET Core / C# via netcoredbg
  ;; Install: https://github.com/Samsung/netcoredbg/releases
  (add-to-list 'dape-configs
               `(netcoredbg-launch
                 modes (csharp-mode csharp-ts-mode)
                 command "netcoredbg"
                 command-args ["--interpreter=vscode"]
                 :type "coreclr"
                 :request "launch"
                 :name "Launch .NET Core"
                 :program (lambda ()
                            (read-file-name "Select DLL: "
                                            (when (fboundp 'projectile-project-root)
                                              (projectile-project-root))
                                            nil t nil
                                            (lambda (n) (string-match-p "\\.dll$" n))))
                 :cwd (lambda ()
                        (or (when (fboundp 'projectile-project-root)
                              (projectile-project-root))
                            default-directory))
                 :stopAtEntry nil
                 :console "integratedTerminal"))

  (add-to-list 'dape-configs
               `(netcoredbg-attach
                 modes (csharp-mode csharp-ts-mode)
                 command "netcoredbg"
                 command-args ["--interpreter=vscode"]
                 :type "coreclr"
                 :request "attach"
                 :name "Attach .NET Core"
                 :processId (lambda () (read-number "Process ID: "))))

  ;; Go via Delve
  ;; Install: go install github.com/go-delve/delve/cmd/dlv@latest
  (add-to-list 'dape-configs
               `(dlv-launch
                 modes (go-mode go-ts-mode)
                 command "dlv"
                 command-args ["dap" "--listen" "127.0.0.1::"]
                 :type "go"
                 :request "launch"
                 :name "Launch Go Package"
                 :mode "debug"
                 :program "${workspaceFolder}"
                 :cwd "${workspaceFolder}"))

  (add-to-list 'dape-configs
               `(dlv-test
                 modes (go-mode go-ts-mode)
                 command "dlv"
                 command-args ["dap" "--listen" "127.0.0.1::"]
                 :type "go"
                 :request "launch"
                 :name "Test Go Function"
                 :mode "test"
                 :program "${workspaceFolder}"
                 :args ["-test.run" "${function}"]
                 :cwd "${workspaceFolder}"))

  ;; Node.js / TypeScript / Next.js via js-debug
  ;; dape will auto-download js-debug on first use if needed
  (add-to-list 'dape-configs
               `(node-launch
                 modes (js-mode js-ts-mode typescript-ts-mode tsx-ts-mode)
                 command "node"
                 command-args [,(expand-file-name "js-debug/src/dapDebugServer.js"
                                                  (locate-user-emacs-file "dape"))]
                 :type "pwa-node"
                 :request "launch"
                 :name "Launch Node.js"
                 :program "${file}"
                 :cwd "${workspaceFolder}"
                 :sourceMaps t
                 :skipFiles ["<node_internals>/**"]))

  (add-to-list 'dape-configs
               `(nextjs-dev
                 modes (typescript-ts-mode tsx-ts-mode js-ts-mode js-mode)
                 command "node"
                 command-args [,(expand-file-name "js-debug/src/dapDebugServer.js"
                                                  (locate-user-emacs-file "dape"))]
                 :type "pwa-node"
                 :request "launch"
                 :name "Next.js Dev Server"
                 :runtimeExecutable "npm"
                 :runtimeArgs ["run" "dev"]
                 :cwd "${workspaceFolder}"
                 :sourceMaps t
                 :skipFiles ["<node_internals>/**"])))

(provide 'dape-config)
;;; dape-config.el ends here
