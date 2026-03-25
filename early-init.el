;;; early-init.el --- Early initialization  -*- lexical-binding: t; -*-

;; Ensure Emacs loads the most recent byte-compiled files.
(setq load-prefer-newer t)

;; Enable async native compilation of .elc files to .eln.
(setq native-comp-jit-compilation t)

;; Disable package.el to use straight.el instead.
(setq package-enable-at-startup nil)
(setenv "LSP_USE_PLISTS" "true")

;;; early-init.el ends here
