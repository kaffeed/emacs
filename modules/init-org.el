;;; init-org.el --- Org mode and Knowledge Management -*- lexical-binding: t -*-

;; Install Org mode early to prevent version mismatch
(straight-use-package 'org)

(load (expand-file-name "org-config.el" user-emacs-directory))

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

(provide 'init-org)
;;; init-org.el ends here
