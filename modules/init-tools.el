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
  :bind ("C-x C-r" . recentf-open-files))

(use-package magit
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

(keymap-set project-prefix-map "m" #'magit-project-status)

(add-hook 'git-commit-setup-hook #'git-commit-turn-on-flyspell)
(add-hook 'git-commit-setup-hook #'turn-on-auto-fill)

(use-package forge
  :after magit)

(use-package git-modes)

(use-package restclient
  :mode ("\\.http\\'" . restclient-mode))

(use-package pdf-tools
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :config
  (pdf-tools-install :no-query))

(use-package agent-shell
  :config
  (add-to-list 'exec-path (expand-file-name "~/.local/bin"))
  (setq agent-shell-antigravity-acp-command
        (if (eq system-type 'windows-nt)
            '("agy_acp_server.exe")
          '("agy_acp_server.par")))
  (setq agent-shell-opencode-authentication
        (agent-shell-opencode-make-authentication :none t))
  (setq agent-shell-preferred-agent-config
        (agent-shell-opencode-make-agent-config))
  ;; Antigravity: launch with M-x agent-shell-antigravity-start-agent
  (setq agent-shell-antigravity-authentication
        (agent-shell-antigravity-make-authentication :login t)))

(defun ss/agent-shell-dot-subdir (subdir)
  (let* ((cwd (string-remove-suffix "/" (agent-shell-cwd)))
         (sanitized (replace-regexp-in-string "/" "-" (string-remove-prefix "/" cwd))))
    (expand-file-name subdir (locate-user-emacs-file (concat "agent-shell/" sanitized)))))

(setopt agent-shell-dot-subdir-function #'ss/agent-shell-dot-subdir)

(use-package docker
  :bind ("C-c M-d" . docker)
  :custom
  (docker-command "docker")
  (docker-compose-command "docker compose"))

(use-package helpful
  :bind (("C-h f" . helpful-callable)     
         ("C-h v" . helpful-variable)     
         ("C-h k" . helpful-key)          
         ("C-h F" . helpful-function)     
         ("C-h C" . helpful-command)))    

;;; ------------------------------------------------------------
;;; Logs
;;; ------------------------------------------------------------
;; Log viewing built around `logview' (highlighting, filtering, format
;; detection), `vlf' for very large files, `auto-revert-tail-mode' for
;; tailing and `ansi-color' for logs containing escape sequences.
;;
;; Keys: C-c L t  tail a log file
;;       C-c L v  open file in vlf

(use-package logview
  :mode ("\\.log\\(?:\\.[0-9]+\\)?\\'" . logview-mode)
  :custom
  (logview-auto-revert-mode 'auto-revert-tail-mode)
  (logview-cache-filename (locate-user-emacs-file "var/logview-cache.el")))

(use-package vlf
  :init (require 'vlf-setup)
  :custom
  (vlf-application 'ask)
  (large-file-warning-threshold (* 50 1024 1024)))

(defun ss/log-apply-ansi ()
  "Render ANSI escape sequences in a log buffer, then make it read-only."
  (when (and (derived-mode-p 'logview-mode)
             (save-excursion
               (goto-char (point-min))
               (re-search-forward "\e\\[" nil t)))
    (let ((inhibit-read-only t)
          (modified (buffer-modified-p)))
      (ansi-color-apply-on-region (point-min) (point-max))
      (set-buffer-modified-p modified))
    (setq buffer-read-only t)))

(add-hook 'logview-mode-hook #'ss/log-apply-ansi)

(defun ss/log-tail (file)
  "Open FILE, follow its tail and jump to the end."
  (interactive "fLog file: ")
  (find-file file)
  (auto-revert-tail-mode 1)
  (goto-char (point-max)))

(global-set-key (kbd "C-c L t") #'ss/log-tail)
(global-set-key (kbd "C-c L v") #'vlf)

(provide 'init-tools)
;;; init-tools.el ends here
