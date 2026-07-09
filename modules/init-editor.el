;;; init-editor.el --- Text manipulation and formatting -*- lexical-binding: t -*-

(use-package undo-fu
  :bind (("C-/" . undo-fu-only-undo)
         ("C-S-/" . undo-fu-only-redo)
         ("C-x u" . undo-fu-only-undo)))

(use-package undo-fu-session
  :after undo-fu
  :config
  (setq undo-fu-session-incompatible-files '("/COMMIT_EDITMSG\\'" "/git-rebase-todo\\'"))
  (undo-fu-session-global-mode))

(use-package apheleia
  :config
  (apheleia-global-mode +1)
  (setf (alist-get 'typescript-ts-mode apheleia-mode-alist) 'prettier-typescript)
  (setf (alist-get 'tsx-ts-mode apheleia-mode-alist) 'prettier-typescript)
  (setf (alist-get 'js-ts-mode apheleia-mode-alist) 'prettier-javascript)
  (setf (alist-get 'css-ts-mode apheleia-mode-alist) 'prettier-css)
  (setf (alist-get 'json-ts-mode apheleia-mode-alist) 'prettier-json)
  (setf (alist-get 'yaml-ts-mode apheleia-mode-alist) 'prettier-yaml)
  (setf (alist-get 'go-mode apheleia-mode-alist) 'gofmt)
  (setf (alist-get 'go-ts-mode apheleia-mode-alist) 'gofmt)
  (setf (alist-get 'csharp-mode apheleia-mode-alist) 'csharpier))

(use-package dtrt-indent
  :hook (prog-mode . dtrt-indent-mode)
  :custom
  (dtrt-indent-verbosity 0))

(use-package aggressive-indent
  :hook (emacs-lisp-mode . aggressive-indent-mode)
  :diminish aggressive-indent-mode)

(use-package dumb-jump
  :config
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate)
  :custom
  (dumb-jump-prefer-searcher 'rg)
  (dumb-jump-selector 'completing-read))

(use-package wgrep
  :bind ( :map grep-mode-map
          ("e" . wgrep-change-to-wgrep-mode)
          ("C-x C-q" . wgrep-change-to-wgrep-mode)
          ("C-c C-c" . wgrep-finish-edit)))

(use-package ace-window
  :bind (("M-o" . ace-window))
  :custom
  (aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))

(use-package avy
  :bind (("C-:" . avy-goto-char-2)      
         ("C-'" . avy-goto-line)         
         ("M-g f" . avy-goto-line)       
         ("M-g w" . avy-goto-word-1)))   

(use-package multiple-cursors
  :bind (("C->" .           mc/mark-next-like-this)
         ("C-<" .           mc/mark-previous-like-this)
         ("C-c C-<" .       mc/mark-all-like-this)
         ("C-S-c C-S-c" .   mc/edit-lines)
         ("C-S-<mouse-1>" . mc/add-cursor-on-click)))

(use-package expand-region
  :bind ("C-=" . er/expand-region))

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

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package pulsar
  :bind
  (:map global-map
        ("C-x l" . pulsar-pulse-line) 
        ("C-x L" . pulsar-highlight-permanently-dwim)) 
  :init
  (pulsar-global-mode 1)
  :config
  (setq pulsar-delay 0.055)
  (setq pulsar-iterations 5)
  (setq pulsar-face 'pulsar-green)
  (setq pulsar-region-face 'pulsar-yellow)
  (setq pulsar-highlight-face 'pulsar-magenta))

(use-package editorconfig
  :config
  (editorconfig-mode t))

(provide 'init-editor)
;;; init-editor.el ends here
