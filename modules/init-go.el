;;; init-go.el --- Go development configuration -*- lexical-binding: t -*-

;;; Commentary:
;; Go-specific setup: formatting on save, test runner keybindings,
;; and compile defaults.  LSP (gopls) and debugging (dlv) are configured
;; in eglot-config.el and dape-config.el respectively.

;;; Code:

;;; ------------------------------------------------------------
;;; Formatting on save
;;; ------------------------------------------------------------
;; eglot delegates formatting to gopls which calls goimports when available.

(defun ss/go-format-before-save ()
  "Format the current Go buffer via eglot (gopls → goimports) before saving."
  (when (and (derived-mode-p 'go-ts-mode)
             (eglot-managed-p))
    (eglot-format-buffer)))

;;; ------------------------------------------------------------
;;; Go mode hook
;;; ------------------------------------------------------------

(defun ss/go-mode-setup ()
  "Configure buffer-local settings for Go source files."
  ;; Go canonical style: real tabs, width 4
  (setq-local tab-width 4
              indent-tabs-mode t)
  ;; Format (+ organise imports) on save
  (add-hook 'before-save-hook #'ss/go-format-before-save nil :local)
  ;; Sensible default compile command
  (setq-local compile-command "go build ./... 2>&1"))

(add-hook 'go-ts-mode-hook #'ss/go-mode-setup)

;;; ------------------------------------------------------------
;;; gotest — run/debug individual tests from Emacs
;;; ------------------------------------------------------------

(use-package gotest
  :straight t
  :bind (:map go-ts-mode-map
              ("C-c t t" . go-test-current-test)
              ("C-c t f" . go-test-current-file)
              ("C-c t p" . go-test-current-project)
              ("C-c t b" . go-test-current-benchmark)
              ("C-c t r" . go-run)))

;;; ------------------------------------------------------------
;;; Go compilation error regexp
;;; ------------------------------------------------------------
;; Teach compile-mode to recognise Go error output:
;;   ./foo/bar.go:42:13: undefined: Baz

(with-eval-after-load 'compile
  (add-to-list 'compilation-error-regexp-alist-alist
               '(go "^\\([^[:space:]\n]+\\.go\\):\\([0-9]+\\):\\([0-9]+\\):" 1 2 3))
  (add-to-list 'compilation-error-regexp-alist 'go))

(provide 'init-go)
;;; init-go.el ends here
