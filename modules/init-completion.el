;;; init-completion.el --- Completion frameworks -*- lexical-binding: t -*-

(use-package vertico
  :config
  (setq vertico-cycle t)
  (setq vertico-resize nil)
  (vertico-mode 1)
  (require 'vertico-multiform)
  (add-to-list 'vertico-multiform-categories '(embark-keybinding grid))
  (vertico-multiform-mode))

(use-package marginalia
  :config
  (marginalia-mode 1))

(use-package orderless
  :config
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

(use-package consult
  :bind (("M-s M-g" . consult-ripgrep)
         ("M-s M-f" . consult-find)
         ("M-s M-o" . consult-outline)
         ("M-s M-l" . consult-line)
         ("M-s M-b" . consult-buffer)
         ("C-x C-b" . consult-buffer)
         ("C-c s" . consult-ripgrep)
         ("C-c b" . consult-bookmark)
         ("C-c i" . consult-imenu)))

(use-package embark
  :bind (("C-." . embark-act)
         ("C-;" . embark-dwim)
         ("C-h B" . embark-bindings)
         :map minibuffer-local-map
         ("C-c C-c" . embark-collect)
         ("C-c C-e" . embark-export))
  :config
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

(use-package embark-consult
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package corfu
  :custom
  (corfu-auto t)                
  (corfu-auto-delay 0.05)       
  (corfu-auto-prefix 1)         
  (corfu-cycle t)               
  (corfu-quit-no-match 'separator) 
  (corfu-preselect 'prompt)     
  :bind
  (:map corfu-map
        ("TAB"   . corfu-next)
        ("<tab>" . corfu-next)
        ("S-TAB" . corfu-previous)
        ("<backtab>" . corfu-previous)
        ("RET"   . corfu-insert)
        ("C-n"   . corfu-next)
        ("C-p"   . corfu-previous))
  :init
  (global-corfu-mode))

(use-package cape
  :init
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-file)
  (add-hook 'completion-at-point-functions #'cape-keyword)
  :config
  (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-silent)
  (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-purify))

(use-package prescient
  :config
  (prescient-persist-mode 1))

(use-package corfu-prescient
  :after (corfu prescient)
  :config
  (corfu-prescient-mode 1))

(use-package nerd-icons-corfu
  :after corfu
  :config
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

(use-package yasnippet
  :config
  (yas-global-mode 1)
  :diminish yas-minor-mode)

(use-package yasnippet-snippets
  :after yasnippet)

(provide 'init-completion)
;;; init-completion.el ends here
