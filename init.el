;;; init.el --- Main configuration  -*- lexical-binding: t; -*-

;;; Commentary:
;; This is the main entry point for the Emacs configuration.
;; It sets up the load path and bootstraps the modular setup.

;;; Code:

;; Add `modules` directory to `load-path` (at both compile and load time)
(eval-and-compile
  (add-to-list 'load-path (expand-file-name "modules" user-emacs-directory)))

;; Load configuration modules
(require 'init-core)
(require 'init-ui)
(require 'init-completion)
(require 'init-editor)
(require 'init-workspace)
(require 'init-prog)
(require 'init-tools)
(require 'init-org)

;; Load custom interface settings if present
(setq custom-file (locate-user-emacs-file "custom-vars.el"))
(when (file-exists-p custom-file)
  (load custom-file 'noerror 'nomessage))

(provide 'init)
;;; init.el ends here
