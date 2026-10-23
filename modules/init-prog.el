;;; init-prog.el --- Generic programming setup: tree-sitter, diagnostics, LSP, debugging -*- lexical-binding: t -*-

;;; Commentary:
;; Language-agnostic tooling only.  Everything specific to one language
;; (modes, servers, debug configs, keys) lives in init-lang.el.

;;; Code:

;;; ------------------------------------------------------------
;;; Tree-sitter
;;; ------------------------------------------------------------
;; Emacs 31 registers grammar sources for its own ts-modes and remaps the
;; classic modes listed here; missing grammars are installed on first use.

(setq treesit-font-lock-level 4)
(setq treesit-auto-install-grammar 'always)
(setopt treesit-enabled-modes
        '(c-ts-mode css-ts-mode js-ts-mode json-ts-mode typescript-ts-mode tsx-ts-mode
          yaml-ts-mode go-ts-mode go-mod-ts-mode dockerfile-ts-mode bash-ts-mode
          toml-ts-mode cmake-ts-mode))

;;; ------------------------------------------------------------
;;; Flymake
;;; ------------------------------------------------------------

(use-package flymake
  :straight (:type built-in)
  :hook (prog-mode . flymake-mode)
  :bind (:map flymake-mode-map
              ("M-n" . flymake-goto-next-error)
              ("M-p" . flymake-goto-prev-error)
              ("C-c ! l" . flymake-show-buffer-diagnostics)
              ("C-c ! L" . flymake-show-project-diagnostics))
  :custom
  (flymake-no-changes-timeout 0.5)
  (flymake-fringe-indicator-position 'right-fringe))

;;; ------------------------------------------------------------
;;; Eglot (LSP)
;;; ------------------------------------------------------------
;; Servers are installed separately; see the language sections in init-lang.el.

(use-package eglot
  :straight (:type built-in)
  :bind
  (:map eglot-mode-map
        ("C-c l r"   . eglot-rename)
        ("C-c l a"   . eglot-code-actions)
        ("C-c l f"   . eglot-format)
        ("C-c l F"   . eglot-format-buffer)
        ("C-c l d"   . eldoc)
        ("C-c l R"   . eglot-reconnect)
        ("C-c l q"   . eglot-shutdown)
        ("C-c l Q"   . eglot-shutdown-all)
        ("C-c l ."   . xref-find-definitions)
        ("C-c l ,"   . xref-go-back)
        ("C-c l ?"   . xref-find-references)
        ("C-c l i"   . eglot-find-implementation)
        ("C-c l t"   . eglot-find-typeDefinition)
        ("C-c l /"   . xref-find-apropos))

  :custom
  ;; Shut down the server when its last buffer is killed
  (eglot-autoshutdown t)
  ;; Boost performance: don't log events unless debugging
  (eglot-events-buffer-size 0)
  ;; Don't confirm when applying code actions
  (eglot-confirm-server-initiated-edits nil)
  ;; Disable blocking on server connection
  (eglot-sync-connect nil)
  ;; Disable noisy progress reports in echo area
  (eglot-report-progress nil)
  ;; Disable heavy server capabilities (highlighting on cursor move, inlay hints)
  (eglot-ignored-server-capabilities '(:documentHighlightProvider
                                       :inlayHintProvider
                                       :semanticTokensProvider))
  (eglot-documentation-renderer 'markdown-ts-view-mode)
  (eglot-code-action-indications nil)
  (eldoc-help-at-pt t)
  (eldoc-echo-area-prefer-doc-buffer t)

  :config
  ;; Integrate yasnippet with eglot via cape
  (defun ss/eglot-capf ()
    "Set up eglot completion with yasnippet and cape."
    (setq-local completion-at-point-functions
                (list (cape-capf-super
                       #'eglot-completion-at-point
                       #'yasnippet-capf)
                      #'cape-file)))
  (add-hook 'eglot-managed-mode-hook #'ss/eglot-capf))

;; Yasnippet capf adapter - bridges yasnippet into capf/corfu
(use-package yasnippet-capf
  :after (yasnippet cape))

;; Workspace symbol search through consult
(use-package consult-eglot
  :bind
  ("M-s M-s" . consult-eglot-symbols))

;;; ------------------------------------------------------------
;;; Dape (debugging)
;;; ------------------------------------------------------------
;; Built-in configs cover the common cases (`netcoredbg', `dlv',
;; `js-debug-node', ...).  Override at the `dape' prompt, e.g.
;;   dlv :mode "test" :args ["-test.run" "TestFoo"]

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
  (dape-buffer-window-arrangement 'right)
  :config
  ;; Show repl on start
  (add-hook 'dape-start-hook (lambda () (save-excursion (dape-repl))))
  ;; Auto-save before debugging
  (add-hook 'dape-start-hook (lambda () (save-some-buffers t t))))

(provide 'init-prog)
;;; init-prog.el ends here
