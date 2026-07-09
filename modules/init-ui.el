;;; init-ui.el --- Appearance and UI configuration -*- lexical-binding: t -*-

;; Default font: Iosevka Nerd Font Mono 14pt
(set-face-attribute 'default nil :font "Iosevka NFM" :height 140)
(add-to-list 'default-frame-alist '(font . "Iosevka NFM-14"))

;; Start frames maximized
(add-to-list 'default-frame-alist '(fullscreen . maximized))

(column-number-mode)

(setq display-line-numbers-type 'relative)
(global-display-line-numbers-mode t)
(dolist (mode '(org-mode-hook
                term-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(winner-mode +1)
(defun toggle-delete-other-windows ()
  "Delete other windows in frame if any, or restore previous window config."
  (interactive)
  (if (and winner-mode
           (equal (selected-window) (next-window)))
      (winner-undo)
    (delete-other-windows)))

(global-set-key (kbd "C-x 1") 'toggle-delete-other-windows)
(global-set-key (kbd "C-x <up>") 'windmove-up)
(global-set-key (kbd "C-x <down>") 'windmove-down)
(global-set-key (kbd "C-x <left>") 'windmove-left)
(global-set-key (kbd "C-x <right>") 'windmove-right)
(global-set-key (kbd "C-x C-<up>") 'enlarge-window)
(global-set-key (kbd "C-x C-<down>") 'shrink-window)
(global-set-key (kbd "C-x C-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "C-x C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "C-x w t") 'window-layout-transpose)
(global-set-key (kbd "C-x w r") 'window-layout-rotate-clockwise)
(global-set-key (kbd "C-x w f h") 'window-layout-flip-leftright)
(global-set-key (kbd "C-x w f v") 'window-layout-flip-topdown)

(use-package spacious-padding
  :ensure t
  :hook (after-init . spacious-padding-mode)
  :custom
  (spacious-padding-widths
   '( :internal-border-width 15
      :header-line-width 4
      :mode-line-width 6
      :tab-width 4
      :right-divider-width 30
      :scroll-bar-width 8
      :fringe-width 8)))

;; Set up custom themes directory
(add-to-list 'custom-theme-load-path (expand-file-name "themes" user-emacs-directory))

;; Install and configure doom-themes
(use-package doom-themes
  :ensure t
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ;; if nil, bold is universally disabled
        doom-themes-enable-italic t) ;; if nil, italics is universally disabled
  (doom-themes-visual-bell-config))

;; Load the theme
(load-theme 'doom-gruvbox t)

(use-package indent-bars
  :straight (indent-bars :type git :host github :repo "jdtsmith/indent-bars")
  :hook ((prog-mode yaml-ts-mode) . indent-bars-mode)
  :custom
  (indent-bars-treesit-support t)
  (indent-bars-no-descend-string t)
  (indent-bars-width-frac 0.2)
  (indent-bars-pad-frac 0.1))

(use-package transpose-frame
  :bind (("C-x 5 t" . transpose-frame)
         ("C-x 5 f" . flip-frame)
         ("C-x 5 r" . rotate-frame-clockwise)))

;; Modeline
(use-package all-the-icons
  :straight t
  :if (display-graphic-p))

(use-package nerd-icons
  :straight t
  :custom
  (nerd-icons-font-family "Symbols Nerd Font Mono"))

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1))

(use-package which-key :config (which-key-mode))

(provide 'init-ui)
;;; init-ui.el ends here
