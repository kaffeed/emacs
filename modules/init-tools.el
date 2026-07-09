;;; init-tools.el --- External tools, Git, terminals, and utilities -*- lexical-binding: t -*-

(defun ss/open-external-terminal ()
  "Open external terminal in current directory.
In Dired, opens terminal in the directory being viewed.
Otherwise, opens in the directory of the current file."
  (interactive)
  (let* ((dir (if (eq major-mode 'dired-mode)
                  default-directory
                (file-name-directory (or buffer-file-name default-directory))))
         (win-dir (directory-file-name (convert-standard-filename dir))))
    (cond
     (*is-a-windoof*
      (let ((wt-path (or (executable-find "wt.exe")
                         (executable-find "wt"))))
        (if wt-path
            (progn
              (message "Opening Windows Terminal in: %s" win-dir)
              (w32-shell-execute "open" wt-path (format "-d \"%s\"" win-dir)))
          (progn
            (message "Windows Terminal not found, using PowerShell in: %s" win-dir)
            (w32-shell-execute "open" "powershell.exe"
                               (format "-NoExit -Command \"Set-Location '%s'\"" win-dir))))))
     (*is-a-linux*
      (cond
       ((executable-find "gnome-terminal")
        (start-process "external-terminal" nil "gnome-terminal" "--working-directory" dir))
       ((executable-find "konsole")
        (start-process "external-terminal" nil "konsole" "--workdir" dir))
       ((executable-find "xterm")
        (start-process "external-terminal" nil "xterm" "-e" (format "cd '%s' && bash" dir)))
       (t (message "No supported terminal found"))))
     ((eq system-type 'darwin)
      (start-process "external-terminal" nil "open" "-a" "Terminal" dir)))))

(global-set-key (kbd "C-c RET") 'ss/open-external-terminal)
(with-eval-after-load 'dired
  (define-key dired-mode-map (kbd "C-c RET") #'ss/open-external-terminal))

(use-package recentf
  :straight (:type built-in)
  :config
  (recentf-mode 1)
  (setq recentf-max-menu-items 50
        recentf-max-saved-items 50)
  (run-at-time nil (* 5 60) 'recentf-save-list)
  :bind ("C-c r" . recentf-open-files))

(use-package magit
  :commands (magit-status magit-get-current-branch)
  :bind (("C-x g" . magit-status)
         ("C-x M-g" . magit-dispatch)
         ("C-c M-g" . magit-file-dispatch)
         ("<f8>" . magit-status))
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

(add-hook 'git-commit-setup-hook #'git-commit-turn-on-flyspell)
(add-hook 'git-commit-setup-hook #'turn-on-auto-fill)

(use-package forge
  :after magit)

(use-package opencode
  :straight (opencode :type git :host codeberg :repo "sczi/opencode.el"))

(use-package git-modes)

(use-package eat
  :straight (:type git :host codeberg :repo "akib/emacs-eat"
                   :files ("*.el" ("term" "term/*.el") "*.texi"
                           "*.ti" ("terminfo/e" "terminfo/e/*")
                           ("terminfo/65" "terminfo/65/*")
                           ("integration" "integration/*")
                           (:exclude ".dir-locals.el" "*-tests.el")))
  :bind (("C-c T" . eat)
         ("C-c C-t" . eat-project)
         ("<f12>" . eshell))
  :custom
  (eat-kill-buffer-on-exit t)
  (eat-shell "pwsh"))

(use-package restclient
  :mode ("\\.http\\'" . restclient-mode))

(use-package pdf-tools
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :config
  (pdf-tools-install :no-query))

(use-package shell-maker :straight t)

(use-package acp :straight t)

(use-package agent-shell
  :straight t
  :config
  (setq agent-shell-opencode-authentication
        (agent-shell-opencode-make-authentication :none t))
  (setq agent-shell-preferred-agent-config
        (agent-shell-opencode-make-agent-config)))

(defun ss/agent-shell-dot-subdir (subdir)
  (let* ((cwd (string-remove-suffix "/" (agent-shell-cwd)))
         (sanitized (replace-regexp-in-string "/" "-" (string-remove-prefix "/" cwd))))
    (expand-file-name subdir (locate-user-emacs-file (concat "agent-shell/" sanitized)))))

(setopt agent-shell-dot-subdir-function #'ss/agent-shell-dot-subdir)

(use-package docker)

(use-package helpful
  :bind (("C-h f" . helpful-callable)     
         ("C-h v" . helpful-variable)     
         ("C-h k" . helpful-key)          
         ("C-h F" . helpful-function)     
         ("C-h C" . helpful-command)))    

(use-package sharper
  :demand t
  :bind
  ("C-c d" . sharper-main-transient))

(provide 'init-tools)
;;; init-tools.el ends here
