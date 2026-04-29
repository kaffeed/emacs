;;; init.el --- Main configuration  -*- lexical-binding: t; -*-

;;; ------------------------------------------------------------
;;; Core Settings
;;; ------------------------------------------------------------


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


;;; ------------------------------------------------------------
;;; Emacs Built-in Configuration
;;; ------------------------------------------------------------

(defconst *is-a-linux* (eq system-type 'gnu/linux))
(defconst *is-a-windoof* (eq system-type 'windows-nt))


(defun ss/scroll-half-page-down ()
  "Scroll down half a page, like Vim's C-d."
  (interactive)
  (let ((half-page (/ (window-body-height) 2)))
    (scroll-up-command half-page)))

(defun ss/scroll-half-page-up ()
  "Scroll up half a page, like Vim's C-u."
  (interactive)
  (let ((half-page (/ (window-body-height) 2)))
    (scroll-down-command half-page)))


;; This prevents frame deletion issues during Emacs shutdown
(defun ss/safe-kill-emacs ()
  "Kill Emacs gracefully without frame deletion errors."
  (interactive)
  (condition-case nil
      (save-buffers-kill-terminal)
    (error
     (when (yes-or-no-p "Force quit Emacs? ")
       (kill-emacs)))))

;;; ------------------------------------------------------------
(defun ss/open-external-terminal ()
  "Open external terminal in current directory.
In Dired, opens terminal in the directory being viewed.
Otherwise, opens in the directory of the current file."
  (interactive)
  (let* ((dir (if (eq major-mode 'dired-mode)
                  default-directory
                (file-name-directory (or buffer-file-name default-directory))))
         ;; Convert to Windows path format and remove trailing slash
         (win-dir (directory-file-name (convert-standard-filename dir))))
    (cond
     ;; Windows: Try Windows Terminal first, then PowerShell
     (*is-a-windoof*
      (let ((wt-path (or (executable-find "wt.exe")
                         (executable-find "wt"))))
        (if wt-path
            (progn
              (message "Opening Windows Terminal in: %s" win-dir)
              ;; Use w32-shell-execute for better Windows GUI app launching
              (w32-shell-execute "open" wt-path (format "-d \"%s\"" win-dir)))
          ;; Fallback to PowerShell
          (progn
            (message "Windows Terminal not found, using PowerShell in: %s" win-dir)
            (w32-shell-execute "open" "powershell.exe"
                               (format "-NoExit -Command \"Set-Location '%s'\"" win-dir))))))
     ;; Linux: Try common terminals
     (*is-a-linux*
      (cond
       ((executable-find "gnome-terminal")
        (start-process "external-terminal" nil "gnome-terminal" "--working-directory" dir))
       ((executable-find "konsole")
        (start-process "external-terminal" nil "konsole" "--workdir" dir))
       ((executable-find "xterm")
        (start-process "external-terminal" nil "xterm" "-e" (format "cd '%s' && bash" dir)))
       (t (message "No supported terminal found"))))
     ;; macOS
     ((eq system-type 'darwin)
      (start-process "external-terminal" nil "open" "-a" "Terminal" dir)))))


(use-package emacs
  :init
  ;; Prefer UTF-8 for everything
  (prefer-coding-system 'utf-8)
  (setq locale-coding-system 'utf-8)
  (set-terminal-coding-system 'utf-8)
  (set-keyboard-coding-system 'utf-8)
  (set-selection-coding-system 'utf-8)
  (setq default-buffer-file-coding-system 'utf-8)
  (setq-default bidi-display-reordering 'left-to-right
		bidi-paragraph-direction 'left-to-right)
  (setq bidi-inhibit-bpa t)
  (setq redisplay-skip-fontification-on-input t)
  (setq read-process-output-max (* 4 1024 1024)) ; 4MB

  ;; Performance: reset GC threshold to a sensible default after startup
  (add-hook 'after-init-hook
            (lambda ()
              (setq gc-cons-threshold (* 100 1024 1024)
                    gc-cons-percentage 0.1)))
  (setq inhibit-startup-message t
        inhibit-startup-screen t
        visible-bell t)

  (column-number-mode)

  (setq display-line-numbers-type 'relative)
  (global-display-line-numbers-mode t)
  (dolist (mode '(org-mode-hook
                  term-mode-hook
                  eshell-mode-hook))
    (add-hook mode (lambda () (display-line-numbers-mode 0))))

  (when *is-a-linux*
    (setq-default x-super-keysym 'meta))

  ;; Start frames maximized
  (add-to-list 'default-frame-alist '(fullscreen . maximized))

  ;; Default font: Iosevka Nerd Font Mono 14pt
  (set-face-attribute 'default nil :font "Iosevka NFM" :height 140)
  (add-to-list 'default-frame-alist '(font . "Iosevka NFM-14"))

  :config
  ;; Behavior and Window Management
  (setq kill-do-not-save-duplicates t)
  (setq savehist-additional-variables '(search-ring regexp-search-ring kill-ring))
  (setq reb-re-syntax 'string)
  (setq ffap-machine-p-known 'reject)
  (setq window-combination-resize t)
  (setq help-window-select t)

  (add-hook 'after-save-hook #'executable-make-buffer-file-executable-if-script-p)

  (winner-mode +1)
  (defun toggle-delete-other-windows ()
    "Delete other windows in frame if any, or restore previous window config."
    (interactive)
    (if (and winner-mode
             (equal (selected-window) (next-window)))
        (winner-undo)
      (delete-other-windows)))

  (advice-add 'save-place-find-file-hook :after
              (lambda (&rest _)
                (when buffer-file-name (ignore-errors (recenter)))))

  ;; Backup and version control
  (setq backup-directory-alist
        `(("." . ,(expand-file-name "backups/" user-emacs-directory))))
  (setq backup-by-copying t
        delete-old-versions t
        kept-new-versions 6
        kept-old-versions 2
        version-control t)

  ;; Better scrolling behavior
  (setq scroll-margin 3
        scroll-conservatively 101
        scroll-preserve-screen-position t
        auto-window-vscroll nil)

  ;; Case-insensitive completion
  (setq read-buffer-completion-ignore-case t
        read-file-name-completion-ignore-case t
        completion-ignore-case t)

  ;; Save cursor position between sessions
  (save-place-mode 1)

  ;; Auto-revert files when changed on disk
  (global-auto-revert-mode 1)
  (setq global-auto-revert-non-file-buffers t)

  ;; Quick diff preview before saving buffers
  (add-to-list 'save-some-buffers-action-alist
               (list "d"
                     (lambda (buffer) (diff-buffer-with-file (buffer-file-name buffer)))
                     "show diff between the buffer and its file"))

  ;; Ergonomic Repeated Mark Popping
  (setq set-mark-command-repeat-pop t)

  ;; Smarter Minibuffer Quitting (C-g)
  (define-advice keyboard-quit
      (:around (quit) quit-current-context)
    "Quit the current context including an active minibuffer from anywhere."
    (if (active-minibuffer-window)
        (if (minibufferp)
            (minibuffer-keyboard-quit)
          (abort-recursive-edit))
      (unless (or defining-kbd-macro executing-kbd-macro)
        (funcall-interactively quit))))

  ;; Inline Elisp Evaluation Overlay (C-x C-e)
  (defun ss/eval-last-sexp-overlay (arg)
    "Eval last sexp and show result inline as a fading overlay."
    (interactive "P")
    (let ((arrow (if (char-displayable-p ?⇒) " ; ⇒ " " ; => ")))
      (if arg
          (let ((value (elisp--eval-last-sexp nil)))
            (insert arrow (format "%S" value)))
        (let* ((value (elisp--eval-last-sexp nil))
               (str (concat arrow (format "%S" value)))
               (ov (make-overlay (point) (point))))
          (overlay-put ov 'after-string
                       (propertize str 'face 'font-lock-comment-face))
          (run-with-timer 3 nil (lambda (o) (delete-overlay o)) ov)))))
  (global-set-key (kbd "C-x C-e") #'ss/eval-last-sexp-overlay)

  ;; Better kill ring
  (setq kill-ring-max 200
        save-interprogram-paste-before-kill t)

  ;; Single space ends sentence (modern convention)
  (setq sentence-end-double-space nil)

  ;; Show trailing whitespace in programming modes
  (add-hook 'prog-mode-hook
            (lambda () (setq show-trailing-whitespace t)))

  (setq custom-file (locate-user-emacs-file "custom-vars.el"))
  (load custom-file 'noerror 'nomessage)

  ;; Fast Isearch Copy (M-w)
  (defun ss/isearch-copy-selected-word ()
    "Copy the current `isearch` selection to the kill ring."
    (interactive)
    (when isearch-other-end
      (let ((selection (buffer-substring-no-properties isearch-other-end (point))))
        (kill-new selection)
        (isearch-exit))))
  (define-key isearch-mode-map (kbd "M-w") #'ss/isearch-copy-selected-word)

  (setq create-lockfiles nil)
  (setq next-line-add-newlines t)

  (let ((autosave-dir (expand-file-name "autosave/" user-emacs-directory)))
    (setq auto-save-list-file-prefix autosave-dir)
    (setq auto-save-file-name-transforms
          `((".*" ,autosave-dir t))))

  ;; Git commit enhancements
  (add-hook 'git-commit-setup-hook #'git-commit-turn-on-flyspell)
  (add-hook 'git-commit-setup-hook #'turn-on-auto-fill)

  :bind
  (("C-x 1" . toggle-delete-other-windows)
   ("C-x C-z" . nil)
   ("C-z" . nil)
   ("C-v" . ss/scroll-half-page-down)
   ("M-v" . ss/scroll-half-page-up)
   ("C-x C-c" . ss/safe-kill-emacs)
   ("C-c c" . compile)
   ("C-c ]" . next-error)
   ("C-c [" . previous-error)
   ("M-n" . next-error)
   ("M-p" . previous-error)
   ("<f5>" . revert-buffer)
   ("<f6>" . org-capture)
   ("<f7>" . org-agenda)
   ("<f8>" . magit-status)
   ("<f9>" . compile)
   ("<f12>" . eshell)
   ("C-x w t"  . window-layout-transpose)            ; EMACS-31
   ("C-x w r"  . window-layout-rotate-clockwise)     ; EMACS-31
   ("C-x w f h"  . window-layout-flip-leftright)     ; EMACS-31
   ("C-x w f v"  . window-layout-flip-topdown)       ; EMACS-31
   ("C-x 5 l"  . select-frame-by-name)
   ("C-x 5 s"  . set-frame-name)
   ("RET" . newline-and-indent)
   ("M-J" . duplicate-dwim)                          ; As suggest on r/emacs by the_cecep:
   ("M-K" . kill-paragraph)                          ; Expands M-k for kill-sentence
   ("M-Z" . zap-up-to-char)                          ; Expands M-z for zap-to-char
   ("M-F" . forward-to-word)                         ; Expands M-f to jump to beginning of next word
   ("M-B" . backward-to-word)                        ; Expands M-b to jump to end of previous word
   ("M-M" . end-of-line)                             ; Expands M-m to jump to end line, useful for paragraphs
   ("M-T" . transpose-sentences)                     ; Expands M-t for transposing words
   ("C-x M-t" . transpose-paragraphs)                ; Expands C-x C-t for transposing lines
   ([remap capitalize-word] . capitalize-dwim)       ; Make M-c work on regions
   ([remap downcase-word] . downcase-dwim)           ; Make M-l work on regions
   ([remap upcase-word] . upcase-dwim)               ; Make M-u work on regions
   ([remap kill-buffer] . kill-current-buffer)       ; C-x k stops prompting for buffer to kill
   ([remap delete-horizontal-space] . cycle-spacing) ; M-\. Called twice, cycle-spacing has same effect and its default binding (M-SPC) is problematic in macOS
   ("C-x <up>" . windmove-up)
   ("C-x <down>" . windmove-down)
   ("C-x <left>" . windmove-left)
   ("C-x <right>" . windmove-right)
   ("C-x C-<up>" . enlarge-window)
   ("C-x C-<down>" . shrink-window)
   ("C-x C-<left>" . shrink-window-horizontally)
   ("C-x C-<right>" . enlarge-window-horizontally)
   ("C-<tab>" . next-buffer)
   ("C-S-<tab>" . previous-buffer)
   ("C-c e" . eshell)
   ("C-c t" . org-todo-list)
   ("C-c w" . delete-trailing-whitespace)
   ("C-c RET" . ss/open-external-terminal)
   ("C-c C-b" . ibuffer)
   ("M-s f" . find-name-dired)
   ("M-j" . duplicate-dwim))
  )

;; Configure use-package to use straight.el by default
(setq straight-use-package-by-default t
      use-package-verbose nil)

;; Install Org mode early to prevent version mismatch
;; This must come before loading org-config.el
(straight-use-package 'org)

(use-package compile-angel
  :demand t
  :config
  (setq compile-angel-verbose nil)
  (push "/init.el" compile-angel-excluded-files)
  (push "/early-init.el" compile-angel-excluded-files)
  (compile-angel-on-load-mode 1))

;;; ------------------------------------------------------------
;;; Org-Mode setup
;;; ------------------------------------------------------------
;; Org-mode configuration is split into a separate file for better organization.
;; See org-config.el for the complete org-mode setup including:
;; - Custom agenda views (dashboard, sprint, backlog, etc.)
;; - Capture templates for tasks, user stories, meetings
;; - Time tracking and effort estimation
;; - Azure DevOps integration
(load (expand-file-name "org-config.el" user-emacs-directory))


;;; ------------------------------------------------------------
;;; OS Settings

;;; ------------------------------------------------------------
;;; Appearance
;;; ------------------------------------------------------------
(use-package spacious-padding
  :straight t
  :custom
  (spacious-padding-widths
   '( :internal-border-width 15
      :header-line-width 4
      :mode-line-width 6
      :tab-width 4
      :right-divider-width 30
      :scroll-bar-width 8
      :fringe-width 8))
  ;; Read the manual for how to specify `spacious-padding-subtle-mode-line'
  :config
  (spacious-padding-mode 1))

;; (use-package ef-themes
;;   :ensure t
;;   :init
;;   ;; This makes the Modus commands listed below consider only the Ef
;;   ;; themes.  For an alternative that includes Modus and all
;;   ;; derivative themes (like Ef), enable the
;;   ;; `modus-themes-include-derivatives-mode' instead.  The manual of
;;   ;; the Ef themes has a section that explains all the possibilities:
;;   ;;
;;   ;; - Evaluate `(info "(ef-themes) Working with other Modus themes or taking over Modus")'
;;   ;; - Visit <https://protesilaos.com/emacs/ef-themes#h:6585235a-5219-4f78-9dd5-6a64d87d1b6e>
;;   (ef-themes-take-over-modus-themes-mode 1)
;;   :bind
;;   (("<f5>" . modus-themes-rotate)
;;    ("C-<f5>" . modus-themes-select)
;;    ("M-<f5>" . modus-themes-load-random))
;;   :config
;;   ;; All customisations here.
;;   (setq modus-themes-mixed-fonts t)
;;   (setq modus-themes-italic-constructs t)
;; 
;;   ;; Finally, load your theme of choice (or a random one with
;;   ;; `modus-themes-load-random', `modus-themes-load-random-dark',
;;   ;; `modus-themes-load-random-light').
;;   (modus-themes-load-theme 'ef-arbutus))

;; Set up custom themes directory
(add-to-list 'custom-theme-load-path (expand-file-name "themes" user-emacs-directory))

;; Install and configure doom-themes
(use-package doom-themes
  :ensure t
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ;; if nil, bold is universally disabled
        doom-themes-enable-italic t) ;; if nil, italics is universally disabled
  (doom-themes-visual-bell-config)
  ;; Corrects (and improves) org-mode's native fontification.
  ;; (doom-themes-org-config)
  )

;; Load the compline theme
(load-theme 'doom-gruvbox t)

;;; ------------------------------------------------------------
;;; Environment Variables (important for macOS)
;;; ------------------------------------------------------------
(use-package exec-path-from-shell
  :init
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

;;; ------------------------------------------------------------
;;; Recentf - Track and quickly access recently opened files
;;; ------------------------------------------------------------
(use-package recentf
  :straight (:type built-in)
  :config
  (recentf-mode 1)
  (setq recentf-max-menu-items 50
        recentf-max-saved-items 50)
  ;; Auto-save recentf list every 5 minutes
  (run-at-time nil (* 5 60) 'recentf-save-list)
  :bind ("C-c r" . recentf-open-files))

;;; ------------------------------------------------------------
;;; Completion UI (Vertico + Consult + Orderless + Marginalia)
;;; ------------------------------------------------------------

;; This section configures a modern completion system using:
;; - Vertico: Vertical completion interface
;; - Orderless: Flexible matching (space-separated patterns)
;; - Marginalia: Rich annotations in completion candidates
;; - Consult: Enhanced search and navigation commands
;; - Embark: Context actions on completion candidates

;; Vertico: Fast, minimal vertical completion UI
;; Preferred over ivy/helm for simplicity and performance
;; Further reading: https://protesilaos.com/emacs/dotemacs#h:cff33514-d3ac-4c16-a889-ea39d7346dc5
(use-package vertico
  :config
  (setq vertico-cycle t)
  (setq vertico-resize nil)
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
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))


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
         ("M-s M-g" . consult-ripgrep)
         ;; Search for files names recursively
         ("M-s M-f" . consult-find)
         ;; Search through the outline (headings) of the file
         ("M-s M-o" . consult-outline)
         ;; Search the current buffer
         ("M-s M-l" . consult-line)
         ;; Switch to another buffer, or bookmarked file, or recently
         ;; opened file.
         ("M-s M-b" . consult-buffer)
         ("C-x C-b" . consult-buffer)
         ("C-c s" . consult-ripgrep)
         ("C-c b" . consult-bookmark)
         ("C-c i" . consult-imenu)))


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
  (setq projectile-switch-project-action #'projectile-find-file)
  :custom
  ;; Use hybrid indexing: git for git projects, native for others
  ;; This ensures .gitignore files are properly respected
  (projectile-indexing-method 'hybrid)
  (projectile-enable-caching t)
  (projectile-git-submodule-command
   (if *is-a-windoof*
       "powershell.exe -NoProfile -NonInteractive -Command \"git submodule --quiet foreach 'echo $displaypath' | ForEach-Object { Write-Host -NoNewline (\\\"$_\\\" + [char]0) }\""
     "git submodule --quiet foreach 'echo $displaypath' | tr '\\n' '\\0'"))
  ;; Explicitly set git command to exclude files per .gitignore
  (projectile-git-command "git ls-files -zco --exclude-standard"))

;;; ------------------------------------------------------------
;;; Company (Completion)
;;; ------------------------------------------------------------

;; Company: Modern completion framework
;; Chosen over alternatives (corfu, auto-complete) for:
;; - Mature, well-tested codebase
;; - Excellent backend support (LSP, dabbrev, files, etc.)
;; - Works seamlessly with LSP mode
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
  ;; Integrate yasnippet with company for LSP completions
  (setq company-backends
        '((company-capf :with company-yasnippet) ;; LSP + snippets together
           company-dabbrev-code                   ;; fallback for code-like text
          company-dabbrev))                       ;; fallback for everything else
  )

;; Flycheck: Modern syntax checking
;; Preferred over built-in flymake for:
;; - Better LSP integration
;; - More checker support (eslint, pylint, etc.)
;; - Cleaner error reporting
(use-package flycheck
  :config
  (add-hook 'prog-mode-hook 'flycheck-mode) ;; always lint my code
  (add-hook 'after-init-hook #'global-flycheck-mode))

(use-package flycheck-posframe
  :ensure t
  :after flycheck
  :config
  (add-hook 'flycheck-mode-hook #'flycheck-posframe-mode))

;; Yasnippet: Template system for code snippets
;; Dramatically improves coding speed with pre-defined templates
;; for common patterns (class definitions, methods, loops, etc.)
(use-package yasnippet
  :config
  (yas-global-mode 1)
  :diminish yas-minor-mode)

;; Yasnippet-snippets: Collection of snippets for many languages
(use-package yasnippet-snippets
  :after yasnippet)

;;; ------------------------------------------------------------
;;; Tree-sitter - Native syntax highlighting and code parsing
;;; ------------------------------------------------------------
;; Uses Emacs 29+ built-in treesit integration with native *-ts-mode major modes

(setq treesit-language-source-alist
      '((bash "https://github.com/tree-sitter/tree-sitter-bash" "v0.23.3")
        (cmake "https://github.com/uyha/tree-sitter-cmake")
        (css "https://github.com/tree-sitter/tree-sitter-css" "v0.23.2")
        (go "https://github.com/tree-sitter/tree-sitter-go" "v0.23.4")
        (html "https://github.com/tree-sitter/tree-sitter-html")
        (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "v0.23.1")
        (json "https://github.com/tree-sitter/tree-sitter-json")
        (make "https://github.com/alemuller/tree-sitter-make")
        (toml "https://github.com/tree-sitter/tree-sitter-toml")
        (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
        (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
        (yaml "https://github.com/tree-sitter-grammars/tree-sitter-yaml" "v0.6.1")
        ))

;; Automatically install missing tree-sitter grammars
(dolist (lang treesit-language-source-alist)
  (unless (treesit-language-available-p (car lang))
    (treesit-install-language-grammar (car lang))))

;; Configure native tree-sitter modes
(setq major-mode-remap-alist
      '((typescript-mode . typescript-ts-mode)
        (js-mode . js-ts-mode)
        (css-mode . css-ts-mode)
        (json-mode . json-ts-mode)
        (yaml-mode . yaml-ts-mode)))

(add-to-list 'auto-mode-alist '("\\.tsx\\'" . tsx-ts-mode))
(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-ts-mode))
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . tsx-ts-mode))
(add-to-list 'auto-mode-alist '("\\.ya?ml\\'" . yaml-ts-mode))

;;; ------------------------------------------------------------
;;; Lsp
;;; ------------------------------------------------------------

(load (expand-file-name "lsp-config.el" user-emacs-directory))

(use-package which-key :config (which-key-mode))

;; Helpful: Much better help buffers with examples, source code, and references
;; Replaces default help commands with more informative versions
(use-package helpful
  :bind (("C-h f" . helpful-callable)     ; Describe function
         ("C-h v" . helpful-variable)     ; Describe variable
         ("C-h k" . helpful-key)          ; Describe key
         ("C-h F" . helpful-function)     ; Describe function (only functions)
         ("C-h C" . helpful-command)))    ; Describe command

(use-package sharper
  :demand t
  :bind
  ("C-c d" . sharper-main-transient))

;;; ------------------------------------------------------------
;;; Magit
;;; ------------------------------------------------------------

(use-package magit
  :commands (magit-status magit-get-current-branch)
  :bind (("C-x g" . magit-status)
         ("C-x M-g" . magit-dispatch)
         ("C-c M-g" . magit-file-dispatch))
  :custom
  (magit-display-buffer-function #'magit-display-buffer-fullcolumn-most-v1)
  (magit-commit-show-diff nil))

(use-package diff-hl
  :hook ((prog-mode . diff-hl-mode)
         (text-mode . diff-hl-mode)
         (magit-pre-refresh . diff-hl-magit-pre-refresh)
         (magit-post-refresh . diff-hl-magit-post-refresh)))

(with-eval-after-load 'projectile
  (define-key projectile-mode-map (kbd "C-c p g") #'magit-status))

;; Git commit enhancements

;; Forge: GitHub/GitLab integration for Magit
;; Enables working with PRs, issues, and code reviews from within Emacs
(use-package forge
  :after magit)

;;; ------------------------------------------------------------
;;; Misc packages
;;; ------------------------------------------------------------
(use-package opencode
  :straight (opencode :type git :host codeberg :repo "sczi/opencode.el"))

(use-package spacious-padding
  :ensure t
  :hook (after-init . spacious-padding-mode))

(use-package docker)

(use-package dockerfile-mode
  :straight (:type git :host github :repo "spotify/dockerfile-mode")
  :mode ("Dockerfile\\'" "\\.dockerfile\\'"))

;;; ------------------------------------------------------------
;;; Undo-tree - Visual undo/redo with branching history
;;; ------------------------------------------------------------
;; Undo-tree makes Emacs' powerful undo system visual and intuitive
;; Shows undo history as a tree structure you can navigate

(use-package undo-tree
  :config
  (global-undo-tree-mode)
  :bind ("C-x u" . undo-tree-visualize)
  :diminish undo-tree-mode)

(use-package ace-window
  :bind (("M-o" . ace-window))
  :custom
  (aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))

;; Avy: Jump to visible text using character-based decision tree
;; Essential for quick navigation without mouse or arrow keys
(use-package avy
  :bind (("C-:" . avy-goto-char-2)      ; Jump to 2-char combination
         ("C-'" . avy-goto-line)         ; Jump to line
         ("M-g f" . avy-goto-line)       ; Alternative line jump
         ("M-g w" . avy-goto-word-1)))   ; Jump to word

(use-package multiple-cursors
  :bind (("C->" .           mc/mark-next-like-this)
         ("C-<" .           mc/mark-previous-like-this)
         ("C-c C-<" .       mc/mark-all-like-this)
         ("C-S-c C-S-c" .   mc/edit-lines)
         ("C-S-<mouse-1>" . mc/add-cursor-on-click)))

(use-package rg
  :straight (:type git :host github :repo "dajva/rg.el")
  :config (rg-enable-default-bindings))

(use-package expand-region
  :bind ("C-=" . er/expand-region))

;; Smartparens: Intelligent handling of parentheses, quotes, and brackets
;; Auto-pairs delimiters and provides smart navigation/manipulation commands
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

;; Rainbow-delimiters: Color-code nested parentheses by depth
;; Essential for Lisp, helpful for all languages with nested structures
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

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

;; EditorConfig: Respect .editorconfig files for consistent coding styles
;; Automatically applies indent style, tab width, line endings, etc.
;; from .editorconfig files in project roots
(use-package editorconfig
  :config
  (editorconfig-mode t))

;;; ------------------------------------------------------------
;;; Window Layout Management
;;; ------------------------------------------------------------

;; Transpose-frame: Quickly rearrange window layouts
;; Useful for rotating, flipping, and transposing window arrangements
(use-package transpose-frame
  :bind (("C-x 5 t" . transpose-frame)
         ("C-x 5 f" . flip-frame)
         ("C-x 5 r" . rotate-frame-clockwise)))

;;; ------------------------------------------------------------
;;; REST API Testing
;;; ------------------------------------------------------------

;; Restclient: Test APIs directly in Emacs
;; Great for backend development and API testing
(use-package restclient
  :mode ("\\.http\\'" . restclient-mode))

;; Company backend for restclient
(use-package company-restclient
  :after (company restclient)
  :config
  (add-to-list 'company-backends 'company-restclient))

;;; ------------------------------------------------------------
;;; PDF Viewing
;;; ------------------------------------------------------------

;; PDF-tools: Much better PDF viewing than DocView
;; Provides smooth scrolling, text search, annotations, and more
(use-package pdf-tools
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :config
  (pdf-tools-install :no-query))

;;; ------------------------------------------------------------
;;; Knowledge Management
;;; ------------------------------------------------------------

;; Org-roam: Zettelkasten note-taking system
;; Creates a network of interconnected notes for knowledge management
(use-package org-roam
  :after org
  :custom
  (org-roam-directory "~/org/roam")
  (org-roam-completion-everywhere t)
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n c" . org-roam-capture)
         ("C-c n g" . org-roam-graph))
  :config
  (org-roam-db-autosync-mode))

;;; ------------------------------------------------------------
;;; Communication & Utilities
;;; ------------------------------------------------------------

(use-package ement
  :straight (:type git :host github :repo "alphapapa/ement.el"))

(use-package powershell
  :config
  (add-hook 'powershell-mode-hook
            (lambda ()
              (ansi-color-for-comint-mode-on)
              (setq comint-process-echoes t)))
  (add-hook 'comint-output-filter-functions
            'ansi-color-process-output))

;;; ------------------------------------------------------------

;; Bind to dired-mode
(with-eval-after-load 'dired
  (define-key dired-mode-map (kbd "C-c RET") #'ss/open-external-terminal))


;;; ------------------------------------------------------------
;;; DevDocs - Browse devdocs.io documentation
;;; ------------------------------------------------------------

(use-package devdocs
  :straight (:type git :host github :repo "astoff/devdocs.el")
  :bind (("C-c m" . devdocs-lookup)
         ("C-c M" . devdocs-install))
  :custom
  ;; Cache directory for downloaded documentation
  (devdocs-data-dir (expand-file-name "devdocs" user-emacs-directory))
  :config
  ;; Automatically install docs for current major mode
  (add-hook 'python-mode-hook
            (lambda () (setq-local devdocs-current-docs '("python~3.12"))))
  (add-hook 'js-mode-hook
            (lambda () (setq-local devdocs-current-docs '("javascript" "node"))))
  (add-hook 'js-ts-mode-hook
            (lambda () (setq-local devdocs-current-docs '("javascript" "node"))))
  (add-hook 'typescript-ts-mode-hook
            (lambda () (setq-local devdocs-current-docs '("typescript" "node"))))
  (add-hook 'tsx-ts-mode-hook
            (lambda () (setq-local devdocs-current-docs '("react" "next.js" "typescript" "node"))))
  (add-hook 'csharp-mode-hook
            (lambda () (setq-local devdocs-current-docs '("dotnet~8.0"))))
  (add-hook 'go-mode-hook
            (lambda () (setq-local devdocs-current-docs '("go"))))
  (add-hook 'emacs-lisp-mode-hook
            (lambda () (setq-local devdocs-current-docs '("elisp")))))


;; Optional: Auto-cleanup trailing whitespace on save
;; Uncomment the next line to enable:
;; (add-hook 'before-save-hook 'delete-trailing-whitespace)



;;; ------------------------------------------------------------
;;; Custom Modeline

(use-package nerd-icons
  :straight t
  :custom
  (nerd-icons-font-family "Symbols Nerd Font Mono"))

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1))
;; (load (expand-file-name "custom-modeline.el" user-emacs-directory) t t)

;;; ------------------------------------------------------------
;;; agent-shell - Native Emacs buffer for LLM agents via ACP
;;; ------------------------------------------------------------
(use-package shell-maker :straight t)

(use-package acp :straight t)

(use-package agent-shell
  :straight t
  :config
  ;; :none t = no API key injected; opencode handles auth itself
  (setq agent-shell-opencode-authentication
        (agent-shell-opencode-make-authentication :none t))
  ;; Set OpenCode as the default for M-x agent-shell
  (setq agent-shell-preferred-agent-config
        (agent-shell-opencode-make-agent-config)))

(defun ss/agent-shell-dot-subdir (subdir)
  (let* ((cwd (string-remove-suffix "/" (agent-shell-cwd)))
         (sanitized (replace-regexp-in-string "/" "-" (string-remove-prefix "/" cwd))))
    (expand-file-name subdir (locate-user-emacs-file (concat "agent-shell/" sanitized)))))

(setopt agent-shell-dot-subdir-function #'ss/agent-shell-dot-subdir)

(provide 'init)
;;; init.el ends here














