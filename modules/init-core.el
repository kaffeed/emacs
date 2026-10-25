;;; init-core.el --- Core Emacs configuration -*- lexical-binding: t -*-

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
(setq straight-use-package-by-default t
      use-package-verbose nil)

;; Force straight to use built-in versions of core packages to prevent conflicts
(use-package project :straight (:type built-in))
(use-package xref :straight (:type built-in))
(use-package eldoc :straight (:type built-in))
(use-package seq :straight (:type built-in))
(use-package map :straight (:type built-in))

;; Fix Org version mismatch by loading the Straight version before any
;; other package can trigger loading the built-in Org version.
(straight-use-package 'org)

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

(defun ss/safe-kill-emacs ()
  "Kill Emacs gracefully without frame deletion errors."
  (interactive)
  (condition-case nil
      (save-buffers-kill-terminal)
    (error
     (when (yes-or-no-p "Force quit Emacs? ")
       (kill-emacs)))))

(use-package emacs
  :init
  ;; Only files/processes: Windows clipboard must stay utf-16le
  (prefer-coding-system 'utf-8)
  (setq-default bidi-display-reordering 'left-to-right
                bidi-paragraph-direction 'left-to-right)
  (setq bidi-inhibit-bpa t)
  (setq redisplay-skip-fontification-on-input t)
  (setq read-process-output-max (* 4 1024 1024))

  (setq inhibit-startup-screen t)

  (when *is-a-linux*
    (setq-default x-super-keysym 'meta))

  :config
  (setq kill-do-not-save-duplicates t)
  (setq savehist-additional-variables '(search-ring regexp-search-ring))
  (setq reb-re-syntax 'string)
  (setq ffap-machine-p-known 'reject)
  (setq window-combination-resize t)
  (setq help-window-select t)

  (add-hook 'after-save-hook #'executable-make-buffer-file-executable-if-script-p)

  (advice-add 'save-place-find-file-hook :after
              (lambda (&rest _)
                (when buffer-file-name (ignore-errors (recenter)))))

  (setq backup-directory-alist
        `(("." . ,(expand-file-name "backups/" user-emacs-directory))))
  (setq backup-by-copying t
        delete-old-versions t
        kept-new-versions 6
        kept-old-versions 2
        version-control t)

  (setq scroll-margin 3
        scroll-conservatively 101
        scroll-preserve-screen-position t
        auto-window-vscroll nil)

  (setq read-buffer-completion-ignore-case t
        read-file-name-completion-ignore-case t
        completion-ignore-case t)

  (save-place-mode 1)
  (savehist-mode 1)

  (global-auto-revert-mode 1)
  (setq global-auto-revert-non-file-buffers t)

  (add-to-list 'save-some-buffers-action-alist
               (list "d"
                     (lambda (buffer) (diff-buffer-with-file (buffer-file-name buffer)))
                     "show diff between the buffer and its file"))

  (setq set-mark-command-repeat-pop t)

  (define-advice keyboard-quit
      (:around (quit) quit-current-context)
    "Quit the current context including an active minibuffer from anywhere."
    (if (active-minibuffer-window)
        (if (minibufferp)
            (minibuffer-keyboard-quit)
          (abort-recursive-edit))
      (unless (or defining-kbd-macro executing-kbd-macro)
        (funcall-interactively quit))))

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

  (setq kill-ring-max 200
        save-interprogram-paste-before-kill t)

  (setq sentence-end-double-space nil)

  (defun ss/isearch-copy-selected-word ()
    "Copy the current `isearch` selection to the kill ring."
    (interactive)
    (when isearch-other-end
      (let ((selection (buffer-substring-no-properties isearch-other-end (point))))
        (kill-new selection)
        (isearch-exit))))

  (setq create-lockfiles nil)

  (let ((autosave-dir (expand-file-name "autosave/" user-emacs-directory)))
    (setq auto-save-list-file-prefix autosave-dir)
    (setq auto-save-file-name-transforms
          `((".*" ,autosave-dir t))))

  :bind
  (("C-x C-z" . nil)
   ("C-z" . nil)
   ("C-v" . ss/scroll-half-page-down)
   ("M-v" . ss/scroll-half-page-up)
   ("C-x C-c" . ss/safe-kill-emacs)
   ("M-n" . next-error)
   ("M-p" . previous-error)
   ("<f5>" . revert-buffer)
   ("C-x 5 l"  . select-frame-by-name)
   ("C-x 5 s"  . set-frame-name)
   ("M-K" . kill-paragraph)
   ("M-Z" . zap-up-to-char)
   ("M-F" . forward-to-word)
   ("M-B" . backward-to-word)
   ("M-M" . end-of-line)
   ("M-T" . transpose-sentences)
   ("C-x M-t" . transpose-paragraphs)
   ([remap capitalize-word] . capitalize-dwim)
   ([remap downcase-word] . downcase-dwim)
   ([remap upcase-word] . upcase-dwim)
   ([remap kill-buffer] . kill-current-buffer)
   ([remap delete-horizontal-space] . cycle-spacing)
   ("C-<tab>" . next-buffer)
   ("C-S-<tab>" . previous-buffer)
   ("C-c e" . eshell)
   ("C-c w" . delete-trailing-whitespace)
   ("C-c B" . ibuffer)
   ("M-s f" . find-name-dired)
   ("M-j" . duplicate-dwim)
   ("C-x C-e" . ss/eval-last-sexp-overlay)
   :map isearch-mode-map
   ("M-w" . ss/isearch-copy-selected-word))
  )

(use-package exec-path-from-shell
  :init
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

(when *is-a-windoof*
  ;; only interactive M-x shell; shell-file-name stays cmdproxy so grep/find-dired/compile keep working
  ;; -NoProfile: the user profile prompt makes pwsh exit on a pipe
  (setq explicit-shell-file-name "pwsh.exe"
        explicit-pwsh.exe-args '("-NoLogo" "-NoProfile" "-NoExit"))
  ;; pwsh echoes piped input; drop the duplicate line
  (add-hook 'shell-mode-hook (lambda () (setq-local comint-process-echoes t))))

(use-package gcmh
  :config
  (gcmh-mode 1)
  :custom
  (gcmh-idle-delay 5)
  (gcmh-high-cons-threshold (* 256 1024 1024)))

(setq repeat-keep-prefix t)
(use-package repeat
  :init
  (repeat-mode 1))

(require 'ansi-color)
(add-hook 'compilation-filter-hook #'ansi-color-compilation-filter)

(with-eval-after-load 'compile
  (add-to-list 'compilation-error-regexp-alist-alist
               '(nextjs "^[ \t]*\\([^ \t\n\r:]+\\.[tj]sx?\\):\\([0-9]+\\):\\([0-9]+\\)" 1 2 3))
  (add-to-list 'compilation-error-regexp-alist 'nextjs))

(provide 'init-core)
;;; init-core.el ends here
