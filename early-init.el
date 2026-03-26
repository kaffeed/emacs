;;; early-init.el --- Early initialization  -*- lexical-binding: t; -*-

;; Delay garbage collection while Emacs is booting
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)

;; Ensure Emacs loads the most recent byte-compiled files.
(setq load-prefer-newer t)

;; Enable async native compilation of .elc files to .eln.
(setq native-comp-jit-compilation t)

;; Disable package.el to use straight.el instead.
(setq package-enable-at-startup nil)
(setenv "LSP_USE_PLISTS" "true")

;; Disables unused UI Elements early to avoid flickering
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
(setq menu-bar-mode nil
      tool-bar-mode nil
      scroll-bar-mode nil)

;;; early-init.el ends here
