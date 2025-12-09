;; ------------------------------------------------------------
;;; Package Setup
;;; ------------------------------------------------------------
(require 'package)

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

(setq package-archives
      '(("melpa"        . "https://melpa.org/packages/")
        ("melpa-stable" . "https://stable.melpa.org/packages/")
        ("org"          . "https://orgmode.org/elpa/")
        ("gnu"          . "https://elpa.gnu.org/packages/")))

(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t
      use-package-verbose t)


;;; ------------------------------------------------------------
;;; OS Settings
;;; ------------------------------------------------------------
(defconst *is-a-mac*   (eq system-type 'darwin))
(defconst *is-a-linux* (eq system-type 'gnu/linux))

(when *is-a-mac*
  (setq mac-command-modifier 'meta
        mac-option-modifier  'none
        default-input-method "MacOSX"))

(when *is-a-linux*
  (setq x-super-keysym 'meta))

(add-to-list 'default-frame-alist '(fullscreen . maximized))


;;; ------------------------------------------------------------
;;; Theme
;;; ------------------------------------------------------------
(use-package doom-themes
  :init
  (load-theme 'doom-wilmersdorf t))


;;; ------------------------------------------------------------
;;; Environment Variables (important for macOS)
;;; ------------------------------------------------------------
(use-package exec-path-from-shell
  :init
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))


;;; ------------------------------------------------------------
;;; Completion Framework: Vertico + Orderless
;;; ------------------------------------------------------------

(use-package vertico
  :init
  (vertico-mode))

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides
   '((file (styles partial-completion)))))

;;; ------------------------------------------------------------
;;; Helpful Annotations: Marginalia
;;; ------------------------------------------------------------

(use-package marginalia
  :init
  (marginalia-mode))


;;; ------------------------------------------------------------
;;; Consult (search, M-x enhanced, buffer switching...)
;;; ------------------------------------------------------------

(use-package consult
  :bind (
         ;; C-x bindings

         ("C-x b"   . consult-buffer)
         ("C-x 4 b" . consult-buffer-other-window)
         ("C-x 5 b" . consult-buffer-other-frame)

         ;; M-g goto commands
         ("M-g g" . consult-goto-line)
         ("M-g M-g" . consult-goto-line)
         ("M-g o" . consult-outline)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)

         ;; M-s search commands
         ("C-s" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s g" . consult-grep)
         ("M-s r" . consult-ripgrep)

         ;; History
         ("C-x M-:" . consult-complex-command)
         ("M-y"     . consult-yank-pop)

         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)
         ("M-r" . consult-history))

  :config
  (consult-customize
   consult-theme
   :preview-key '(:debounce 0.2 any))

  (setq xref-show-xrefs-function        #'consult-xref
        xref-show-definitions-function #'consult-xref))


;;; ------------------------------------------------------------
;;; Embark (action menus) + Embark-Consult (preview)
;;; ------------------------------------------------------------

(use-package embark
  :bind
  (("C-." . embark-act)
   ("C-;" . embark-dwim)
   ("C-h B" . embark-bindings))

  :init
  (setq prefix-help-command #'embark-prefix-help-command)

  :config
  ;; Hide mode line in Embark buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

(use-package embark-consult
  :after (embark consult)
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))


;;; ------------------------------------------------------------
;;; Projectile
;;; ------------------------------------------------------------
(use-package projectile
  :init
  (projectile-mode 1)

  ;; Makes the "C-x p" prefix available (Emacs 28+)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

  :custom
  (projectile-enable-caching t)
  (projectile-project-search-path '("~/source/work" "~/source/personal"))
  (projectile-sort-order 'recently-active)
  (projectile-track-known-projects-automatically t)
  (projectile-switch-project-action #'projectile-dired)
  (projectile-completion-system 'default) ;; Important for Vertico/Consult
  (projectile-indexing-method 'alien)     ;; fastest for most systems, esp. with ripgrep

  :config
  ;; Ignore common junk
  (add-to-list 'projectile-globally-ignored-directories "node_modules")
  (add-to-list 'projectile-globally-ignored-directories ".git")
  (add-to-list 'projectile-globally-ignored-files "TAGS"))
(use-package projectile
  :init
  (projectile-mode 1)
  :config
  (setq projectile-enable-caching t
        projectile-completion-system 'default))

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
  ;; Good backends for general programming
  (setq company-backends
        '((company-capf            ;; completion-at-point (LSP, modes, etc.)
           company-dabbrev-code)   ;; fallback for code-like text
          company-dabbrev))        ;; fallback for everything else
  (let ((map company-active-map))
    (mapc (lambda (x) (define-key map (format "%d" x)
                        `(lambda () (interactive) (company-complete-number ,x))))
          (number-sequence 0 9))))

;; Flycheck is the newer version of flymake and is needed to make lsp-mode not freak out.
(use-package flycheck
  :config
  (add-hook 'prog-mode-hook 'flycheck-mode) ;; always lint my code
  (add-hook 'after-init-hook #'global-flycheck-mode))

;; Package for interacting with language servers
(use-package lsp-mode
  :commands lsp
  :config
  (setq lsp-prefer-flymake nil ;; Flymake is outdated
        lsp-headerline-breadcrumb-mode nil)) ;; I don't like the symbols on the header a-la-vscode, remove this if you like them.

(use-package which-key :config (which-key-mode))

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
(add-hook 'git-commit-mode-hook #'turn-on-auto-fill)

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
  (visual-line-mode 1)
  (setq evil-auto-indent nil))

(use-package org
  :hook (org-mode . dw/org-mode-setup)
  :config
  (setq org-ellipsis " ▾"
        org-hide-emphasis-markers t))

;; Replace list hyphen with dot
(font-lock-add-keywords 'org-mode
                        '(("^ *\\([-]\\) "
                          (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

(dolist (face '((org-level-1 . 1.2)
                (org-level-2 . 1.1)
                (org-level-3 . 1.05)
                (org-level-4 . 1.0)
                (org-level-5 . 1.1)
                (org-level-6 . 1.1)
                (org-level-7 . 1.1)
                (org-level-8 . 1.1))))

;; Make sure org-indent face is available
(require 'org-indent)

;; Ensure that anything that should be fixed-pitch in Org files appears that way
(set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
(set-face-attribute 'org-code nil   :inherit '(shadow fixed-pitch))
(set-face-attribute 'org-indent nil :inherit '(org-hide fixed-pitch))
(set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
(set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
(set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
(set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch)

;;; ------------------------------------------------------------
;;; Custom settings file
;;; ------------------------------------------------------------
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file t)
