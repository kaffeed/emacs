;;; ------------------------------------------------------------
;;; Package Setup
;;; ------------------------------------------------------------

;; Prefer UTF-8 for everything
(prefer-coding-system 'utf-8)
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(setq-default default-buffer-file-coding-system 'utf-8)
(setq gc-cons-threshold (* 50 1000 1000))

(setq inhibit-startup-message t
      inhibit-startup-screen t
      visible-bell t)

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(column-number-mode)

(global-display-line-numbers-mode t)
(dolist (mode '(org-mode-hook
		        term-mode-hook
		        eshell-mode-hook))
  (add-hook mode (lambda() (display-line-numbers-mode 0))))

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)

;; Configure use-package to use straight.el by default
(setq-default straight-use-package-by-default t
              use-package-verbose t)

;;; ------------------------------------------------------------
;;; OS Settings
;;; ------------------------------------------------------------
(defconst *is-a-mac*   (eq system-type 'darwin))
(defconst *is-a-linux* (eq system-type 'gnu/linux))

(when *is-a-mac*
  (setq-default mac-command-modifier 'meta
                mac-option-modifier  'none
                default-input-method "MacOSX"))

(when *is-a-linux*
  (setq-default x-super-keysym 'meta))

(add-to-list 'default-frame-alist '(fullscreen . maximized))

;;; ------------------------------------------------------------
;;; Theme
;;; ------------------------------------------------------------
(use-package nano
  :straight (:type git :host github :repo "rougier/nano-emacs")
  ;; :custom
  :config
  (require 'nano)
  (require 'nano-faces)
  (require 'nano-theme))

;;; ------------------------------------------------------------
;;; Environment Variables (important for macOS)
;;; ------------------------------------------------------------
(use-package exec-path-from-shell
  :init
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

;; The `vertico' package applies a vertical layout to the minibuffer.
;; It also pops up the minibuffer eagerly so we can see the available
;; options without further interactions.  This package is very fast
;; and "just works", though it also is highly customisable in case we
;; need to modify its behaviour.
;;
;; Further reading: https://protesilaos.com/emacs/dotemacs#h:cff33514-d3ac-4c16-a889-ea39d7346dc5
(use-package vertico
  :config
  (setq-default vertico-cycle t)
  (setq-default vertico-resize nil)
  (vertico-mode 1)
  ;; Load vertico-multiform feature before configuring it
  (require 'vertico-multiform)
  (add-to-list 'vertico-multiform-categories '(embark-keybinding grid))
  (vertico-multiform-mode))

;; The `marginalia' package provides helpful annotations next to
;; completion candidates in the minibuffer.  The information on
;; display depends on the type of content.  If it is about files, it
;; shows file permissions and the last modified date.  If it is a
;; buffer, it shows the buffer's size, major mode, and the like.
;;
;; Further reading: https://protesilaos.com/emacs/dotemacs#h:bd3f7a1d-a53d-4d3e-860e-25c5b35d8e7e
(use-package marginalia
  :config
  (marginalia-mode 1))

;; The `orderless' package lets the minibuffer use an out-of-order
;; pattern matching algorithm.  It matches space-separated words or
;; regular expressions in any order.  In its simplest form, something
;; like "ins pac" matches `package-menu-mark-install' as well as
;; `package-install'.  This is a powerful tool because we no longer
;; need to remember exactly how something is named.
;;
;; Note that Emacs has lots of "completion styles" (pattern matching
;; algorithms), but let us keep things simple.
;;
;; Further reading: https://protesilaos.com/emacs/dotemacs#h:7cc77fd0-8f98-4fc0-80be-48a758fcb6e2
(use-package orderless
  :config
  (setq completion-styles '(orderless basic)))


;; The `consult' package provides lots of commands that are enhanced
;; variants of basic, built-in functionality.  One of the headline
;; features of `consult' is its preview facility, where it shows in
;; another Emacs window the context of what is currently matched in
;; the minibuffer.  Here I define key bindings for some commands you
;; may find useful.  The mnemonic for their prefix is "alternative
;; search" (as opposed to the basic C-s or C-r keys).
;;
;; Further reading: https://protesilaos.com/emacs/dotemacs#h:22e97b4c-d88d-4deb-9ab3-f80631f9ff1d
(use-package consult
  :bind (;; A recursive grep
         ("M-s M-g" . consult-grep)
         ;; Search for files names recursively
         ("M-s M-f" . consult-find)
         ;; Search through the outline (headings) of the file
         ("M-s M-o" . consult-outline)
         ;; Search the current buffer
         ("M-s M-l" . consult-line)
         ;; Switch to another buffer, or bookmarked file, or recently
         ;; opened file.
         ("M-s M-b" . consult-buffer)))


;; The `embark' package lets you target the thing or context at point
;; and select an action to perform on it.  Use the `embark-act'
;; command while over something to find relevant commands.
;;
;; When inside the minibuffer, `embark' can collect/export the
;; contents to a fully fledged Emacs buffer.  The `embark-collect'
;; command retains the original behaviour of the minibuffer, meaning
;; that if you navigate over the candidate at hit RET, it will do what
;; the minibuffer would have done.  In contrast, the `embark-export'
;; command reads the metadata to figure out what category this is and
;; places them in a buffer whose major mode is specialised for that
;; type of content.  For example, when we are completing against
;; files, the export will take us to a `dired-mode' buffer; when we
;; preview the results of a grep, the export will put us in a
;; `grep-mode' buffer.
;;
;; Further reading: https://protesilaos.com/emacs/dotemacs#h:61863da4-8739-42ae-a30f-6e9d686e1995
(use-package embark
  :bind (("C-." . embark-act)
         ("C-;" . embark-dwim)
         ("C-h B" . embark-bindings)
         :map minibuffer-local-map
         ("C-c C-c" . embark-collect)
         ("C-c C-e" . embark-export))
  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none))))
  )

;; The `embark-consult' package is glue code to tie together `embark'
;; and `consult'.
(use-package embark-consult
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

;; The `wgrep' packages lets us edit the results of a grep search
;; while inside a `grep-mode' buffer.  All we need is to toggle the
;; editable mode, make the changes, and then type C-c C-c to confirm
;; or C-c C-k to abort.
;;
;; Further reading: https://protesilaos.com/emacs/dotemacs#h:9a3581df-ab18-4266-815e-2edd7f7e4852
(use-package wgrep
  :bind ( :map grep-mode-map
          ("e" . wgrep-change-to-wgrep-mode)
          ("C-x C-q" . wgrep-change-to-wgrep-mode)
          ("C-c C-c" . wgrep-finish-edit)))

;;; ------------------------------------------------------------
;;; Projectile
;;; ------------------------------------------------------------
(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  (when (file-directory-p "~/source")
    (setq projectile-project-search-path '("~/source" . 1)))
  (setq projectile-switch-project-action #'projectile-find-file))

;;; ------------------------------------------------------------
;;; Company
;;; ------------------------------------------------------------

;; Company is the best Emacs completion system.
(use-package company
  :init
  (global-company-mode)

  :custom
  ;; Make Company less intrusive and more like modern IDE autocomplete
  (company-idle-delay 0.05)        ;; fast popup
  (company-minimum-prefix-length 1)
  (company-tooltip-align-annotations t)
  (company-tooltip-limit 12)
  (company-tooltip-minimum-width 40)
  (company-show-numbers t)
  (company-require-match nil)
  (company-dabbrev-other-buffers t)
  (company-dabbrev-downcase nil)

  :bind
  (:map company-active-map
        ("<tab>" . company-complete-selection)
        ("TAB"   . company-complete-selection)
        ("C-n"   . company-select-next)
        ("C-p"   . company-select-previous))

  :config
  (company-tng-mode)
  ;; Good backends for general programming
  (setq company-backends
        '((company-capf            ;; completion-at-point (LSP, modes, etc.)
           company-dabbrev-code)   ;; fallback for code-like text
          company-dabbrev))        ;; fallback for everything else
  )

;; Flycheck is the newer version of flymake and is needed to make lsp-mode not freak out.
(use-package flycheck
  :config
  (add-hook 'prog-mode-hook 'flycheck-mode) ;; always lint my code
  (add-hook 'after-init-hook #'global-flycheck-mode))

;;; ------------------------------------------------------------
;;; Lsp
;;; ------------------------------------------------------------

(use-package lsp-mode
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-c l")
  :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
         (csharp-mode . lsp-deferred)

         ;; if you want which-key integration
         (lsp-mode . lsp-enable-which-key-integration))
  :commands (lsp lsp-deferred)
  :config
  (setq-default lsp-headerline-breadcrumb-enable nil)
  (setq-default lsp-use-plists t))

(setq fast-read-process-output (* 1024 1024))
(use-package lsp-ui :commands lsp-ui-mode)
(use-package which-key :config (which-key-mode))

(use-package sharper
  :demand t
  :bind
  ("C-c d" . sharper-main-transient))

;;; ------------------------------------------------------------
;;; Magit
;;; ------------------------------------------------------------

(use-package magit
  :commands (magit-status magit-get-current-branch)
  :bind (("C-x g" . magit-status))
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(use-package diff-hl
  :hook ((prog-mode . diff-hl-mode)
         (text-mode . diff-hl-mode)
         (magit-pre-refresh . diff-hl-magit-pre-refresh)
         (magit-post-refresh . diff-hl-magit-post-refresh)))

(use-package git-gutter
  :hook ((prog-mode . git-gutter-mode)
         (text-mode . git-gutter-mode))
  :custom
  (git-gutter:update-interval 1))

(with-eval-after-load 'projectile
  (define-key projectile-mode-map (kbd "C-c p g") #'magit-status))

(add-hook 'git-commit-setup-hook #'git-commit-turn-on-flyspell) ;; if using flyspell
;; (add-hook 'git-commit-mode-hook #'turn-on-auto-fill)

(use-package forge
  :after magit)

(global-set-key (kbd "C-x M-g") #'magit-dispatch) ;; access all magit commands
(global-set-key (kbd "C-c M-g") #'magit-file-dispatch) ;; file-based git menu

;;; ------------------------------------------------------------
;;; Org-Mode setup
;;; ------------------------------------------------------------

(defun dw/org-mode-setup ()
  (org-indent-mode)
  (variable-pitch-mode 1)
  (auto-fill-mode 0)
  (visual-line-mode 1))

(use-package org
  :hook (org-mode . dw/org-mode-setup))

;;; ------------------------------------------------------------
;;; Misc packages
;;; ------------------------------------------------------------

;; Compile command
(global-set-key (kbd "C-c c") 'compile)

(use-package ace-window
  :bind (("M-o" . ace-window))
  :custom
  (aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))

(use-package multiple-cursors
  :bind (("C->" .           mc/mark-next-like-this)
         ("C-<" .           mc/mark-previous-like-this)
         ("C-c C-<" .       mc/mark-all-like-this)
         ("C-S-c C-S-c" .   mc/edit-lines)
         ("C-S-<mouse-1>" . mc/add-cursor-on-click)))

(use-package rg
  :straight (:type git :host github :repo "dajva/rg.el")
  ;; :custom
  :config (rg-enable-default-bindings))

(use-package expand-region
  :bind ("C-=" . er/expand-region))

(use-package pulsar
  :bind
  (:map global-map
        ("C-x l" . pulsar-pulse-line) ; overrides `count-lines-page'
        ("C-x L" . pulsar-highlight-permanently-dwim)) ; or use `pulsar-highlight-temporarily-dwim'
  :init
  (pulsar-global-mode 1)
  :config
  (setq pulsar-delay 0.055)
  (setq pulsar-iterations 5)
  (setq pulsar-face 'pulsar-green)
  (setq pulsar-region-face 'pulsar-yellow)
  (setq pulsar-highlight-face 'pulsar-magenta))

(editorconfig-mode t)

;;; ------------------------------------------------------------
;;; WoMan - Man page browser
;;; ------------------------------------------------------------
(use-package woman
  :straight (:type built-in)
  :bind (("C-c m" . woman))
  :init
  ;; Dynamically build man page paths by checking which directories exist
  (setq woman-manpath
        (seq-filter #'file-directory-p
                    '("C:/msys64/usr/share/man"
                      "C:/msys64/mingw64/share/man"
                      "C:/msys64/mingw32/share/man")))
  :custom
  ;; Cache man pages for faster access (important with large man page collections)
  (woman-cache-filename (expand-file-name "woman-cache.el" user-emacs-directory))
  ;; Don't ask which topic when there's only one match
  (woman-use-topic-at-point t)
  ;; Fill column for better readability
  (woman-fill-column 80)
  ;; Use own frame or window
  (woman-use-own-frame nil)
  :config
  ;; Build cache on first run - can take a moment but speeds up future lookups
  (unless (file-exists-p woman-cache-filename)
    (woman-file-name "")))

;;; ------------------------------------------------------------
;;; Custom settings file
;;; ------------------------------------------------------------
(setq custom-file (locate-user-emacs-file "custom-vars.el"))
(load custom-file 'noerror 'nomessage)

(setq next-line-add-newlines t)

(let ((autosave-dir (expand-file-name "autosave/" user-emacs-directory)))
  (setq auto-save-list-file-prefix autosave-dir)
  (setq auto-save-file-name-transforms
        `((".*" ,autosave-dir t)))
  )

(provide 'init)
;;; init.el ends here
