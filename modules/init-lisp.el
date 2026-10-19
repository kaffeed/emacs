;;; init-lisp.el --- Common Lisp development configuration -*- lexical-binding: t -*-

;;; Commentary:
;; Common Lisp setup built around SLY (SLIME fork) and SBCL.
;; Completion goes through Corfu (SLY provides a completion-at-point
;; function), navigation through xref (M-. / M-,), and structural editing
;; through smartparens (strict mode in Lisp buffers).
;;
;; Requirements: `sbcl' on PATH.  Optionally install Quicklisp
;; (https://www.quicklisp.org) to load systems from within the REPL.

;;; Code:

;;; ------------------------------------------------------------
;;; SLY
;;; ------------------------------------------------------------

(use-package sly
  :straight t
  :mode ("\\.asd\\'" . lisp-mode)
  :custom
  (inferior-lisp-program "sbcl")
  (sly-net-coding-system 'utf-8-unix)
  :config
  ;; Show the REPL in a short window at the bottom of the frame
  (add-to-list 'display-buffer-alist
               '("\\*sly-mrepl for .*\\*"
                 (display-buffer-reuse-window display-buffer-at-bottom)
                 (window-height . 0.3))))

;;; ------------------------------------------------------------
;;; Lisp buffer setup
;;; ------------------------------------------------------------

(defun ss/lisp-mode-setup ()
  "Configure buffer-local settings for Common Lisp source files."
  ;; Lisp style: spaces only, never tabs
  (setq-local indent-tabs-mode nil)
  (aggressive-indent-mode 1))

(add-hook 'lisp-mode-hook #'ss/lisp-mode-setup)

;;; ------------------------------------------------------------
;;; Structural editing
;;; ------------------------------------------------------------
;; Strict mode keeps parens balanced when killing/deleting.

(add-hook 'lisp-mode-hook #'smartparens-strict-mode)
(add-hook 'sly-mrepl-mode-hook #'smartparens-strict-mode)
(add-hook 'sly-mrepl-mode-hook #'rainbow-delimiters-mode)

(with-eval-after-load 'smartparens
  (define-key smartparens-mode-map (kbd "C-)") #'sp-forward-slurp-sexp)
  (define-key smartparens-mode-map (kbd "C-}") #'sp-forward-barf-sexp)
  (define-key smartparens-mode-map (kbd "C-(") #'sp-backward-slurp-sexp)
  (define-key smartparens-mode-map (kbd "C-{") #'sp-backward-barf-sexp)
  (define-key smartparens-mode-map (kbd "C-c s") #'sp-splice-sexp)
  (define-key smartparens-mode-map (kbd "C-c r") #'sp-raise-sexp))

(provide 'init-lisp)
;;; init-lisp.el ends here
