;;; init-logs.el --- Log file viewing -*- lexical-binding: t -*-

;;; Commentary:
;; Log viewing built around `logview' (highlighting, filtering, format
;; detection), `vlf' for very large files, `auto-revert-tail-mode' for
;; tailing and `ansi-color' for logs containing escape sequences.
;;
;; Keys: C-c L t  tail a log file
;;       C-c L v  open file in vlf

;;; Code:

(use-package logview
  :straight t
  :mode ("\.log\(?:\.[0-9]+\)?\'" . logview-mode)
  :custom
  (logview-auto-revert-mode 'auto-revert-tail-mode)
  (logview-cache-filename (locate-user-emacs-file "var/logview-cache.el")))

(use-package vlf
  :straight t
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

(provide 'init-logs)
;;; init-logs.el ends here
