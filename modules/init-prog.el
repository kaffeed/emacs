;;; init-prog.el --- Programming, syntax, and LSP configurations -*- lexical-binding: t -*-

(setq treesit-font-lock-level 4)
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
        (astro "https://github.com/virchau13/tree-sitter-astro")
        ))

(dolist (lang treesit-language-source-alist)
  (unless (treesit-language-available-p (car lang))
    (treesit-install-language-grammar (car lang))))

(setq major-mode-remap-alist
      '((typescript-mode . typescript-ts-mode)
        (js-mode . js-ts-mode)
        (css-mode . css-ts-mode)
        (json-mode . json-ts-mode)
        (yaml-mode . yaml-ts-mode)))

(add-to-list 'auto-mode-alist '("\\.tsx\\'" . tsx-ts-mode))
(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-ts-mode))
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . tsx-ts-mode))
(add-to-list 'auto-mode-alist '("\\.yaml\\'" . yaml-ts-mode))
(add-to-list 'auto-mode-alist '("\\.xaml\\'" . nxml-mode))

(use-package web-mode
  :mode ("\\.cshtml\\'" "\\.razor\\'")
  :hook (web-mode . (lambda ()
                      (setq-local web-mode-engine "razor")))
  :custom
  (web-mode-markup-indent-offset 2)
  (web-mode-code-indent-offset 4)
  (web-mode-css-indent-offset 2)
  (web-mode-enable-auto-pairing t)
  (web-mode-enable-css-colorization t)
  (web-mode-enable-current-element-highlight t))

(use-package astro-ts-mode
  :straight (astro-ts-mode :type git :host github :repo "Sorixelle/astro-ts-mode" :branch "master")
  :mode "\\.astro\\'")

(use-package flymake
  :straight (:type built-in)
  :hook (prog-mode . flymake-mode)
  :bind (:map flymake-mode-map
              ("M-n" . flymake-goto-next-error)
              ("M-p" . flymake-goto-prev-error)
              ("C-c ! l" . flymake-show-buffer-diagnostics)
              ("C-c ! L" . flymake-show-project-diagnostics))
  :custom
  (flymake-no-changes-timeout 0.5)
  (flymake-fringe-indicator-position 'right-fringe))

(load (expand-file-name "eglot-config.el" user-emacs-directory))
(load (expand-file-name "dape-config.el" user-emacs-directory) t t)

(use-package dotenv-mode
  :mode ("\\.env\\'" "\\.env\\..*\\'" ".*\\.env\\'"))

(use-package dockerfile-mode
  :straight (:type git :host github :repo "spotify/dockerfile-mode")
  :mode ("Dockerfile\\'" "\\.dockerfile\\'"))

(use-package markdown-mode
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :custom
  (markdown-command "pandoc")
  (markdown-fontify-code-blocks-natively t))

(use-package powershell
  :config
  (add-hook 'powershell-mode-hook
            (lambda ()
              (ansi-color-for-comint-mode-on)
              (setq comint-process-echoes t)))
  (add-hook 'comint-output-filter-functions
            'ansi-color-process-output))

(provide 'init-prog)
;;; init-prog.el ends here
