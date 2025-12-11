;;; early-init.el --- Early initialization  -*- lexical-binding: t; -*-

;; Ensure Emacs loads the most recent byte-compiled files.
(setq load-prefer-newer t)

;; Make Emacs Native-compile .elc files asynchronously by setting
;; `native-comp-jit-compilation' to t.
(setq native-comp-jit-compilation t)
(setq native-comp-deferred-compilation native-comp-jit-compilation)  ; Deprecated

;; Disable package.el to use straight.el instead
(setq package-enable-at-startup nil)
(setenv "LSP_USE_PLISTS" "true")

;;; early-init.el ends here
