;;; init-workspace.el --- Project and workspace management -*- lexical-binding: t -*-

(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  (setq projectile-project-search-path '(("~/source/work" . 1) ("~/source/personal" . 1)))
  (setq projectile-switch-project-action #'projectile-find-file)
  :custom
  (projectile-indexing-method 'hybrid)
  (projectile-enable-caching t)
  (projectile-git-submodule-command
   (if *is-a-windoof*
       "powershell.exe -NoProfile -NonInteractive -Command \"git submodule --quiet foreach 'echo $displaypath' | ForEach-Object { Write-Host -NoNewline (\\\"$_\\\" + [char]0) }\""
     "git submodule --quiet foreach 'echo $displaypath' | tr '\\n' '\\0'"))
  (projectile-git-command "git ls-files -zco --exclude-standard"))

(use-package perspective
  :demand t
  :custom
  (persp-mode-prefix-key (kbd "C-c x"))
  (persp-state-default-file (expand-file-name "persp-state" user-emacs-directory))
  (persp-show-modestring t)
  :init
  (persp-mode)
  :config
  (add-hook 'kill-emacs-hook #'persp-state-save)

  (defun ss/ibuffer-set-persp-filter-groups ()
    "Set ibuffer filter groups based on active perspectives."
    (setq ibuffer-filter-groups
          (append
           (mapcar (lambda (name)
                     (let* ((persp (persp-get-by-name name))
                            (buf-names (mapcar #'buffer-name (persp-buffers persp))))
                       (list name
                             `(predicate . (member (buffer-name) ',buf-names)))))
                   (persp-names))
           '(("Other" (name . ".*")))))
    (ibuffer-update nil t))

  (add-hook 'ibuffer-hook #'ss/ibuffer-set-persp-filter-groups)

  (with-eval-after-load 'consult
    (add-to-list 'consult-buffer-sources 'persp-consult-source 'append)
    (setq consult-buffer-sources
          (cons 'persp-consult-source
                (delq 'persp-consult-source consult-buffer-sources)))))

(use-package persp-projectile
  :after (perspective projectile)
  :bind (:map projectile-command-map
              ("x" . projectile-persp-switch-project)
              ("p" . projectile-persp-switch-project)))

(provide 'init-workspace)
;;; init-workspace.el ends here
