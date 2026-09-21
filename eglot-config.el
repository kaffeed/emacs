;;; eglot-config.el --- Eglot (LSP) configuration -*- lexical-binding: t -*-

;;; ------------------------------------------------------------
;;; Eglot - Built-in LSP Client
;;; ------------------------------------------------------------
;; Eglot is the built-in LSP client since Emacs 29.
;; It uses existing Emacs infrastructure (flymake, eldoc, xref, capf)
;; and is simpler and faster than lsp-mode.
;;
;; Supported language servers (must be installed separately):
;; - C#:         OmniSharp or roslyn (dotnet tool install -g csharp-ls)
;; - Go:         gopls (go install golang.org/x/tools/gopls@latest)
;; - TypeScript: typescript-language-server (npm i -g typescript-language-server)
;; - HTML/CSS:   vscode-langservers-extracted (npm i -g vscode-langservers-extracted)
;; - JSON:       vscode-langservers-extracted (same as above)
;; - YAML:       yaml-language-server (npm i -g yaml-language-server)
;; - Angular:    @angular/language-server (npm i -g @angular/language-server)

(use-package eglot
  :straight (:type built-in)
  :hook
  ((csharp-mode          . eglot-ensure)
   (c-mode               . eglot-ensure)
   (go-mode              . eglot-ensure)
   (go-ts-mode           . eglot-ensure)
   (typescript-ts-mode   . eglot-ensure)
   (tsx-ts-mode          . eglot-ensure)
   (js-ts-mode           . eglot-ensure)
   (js-mode              . eglot-ensure)
   (html-mode            . eglot-ensure)
   (mhtml-mode           . eglot-ensure)
   (css-mode             . eglot-ensure)
   (css-ts-mode          . eglot-ensure)
   (json-mode            . eglot-ensure)
   (json-ts-mode         . eglot-ensure)
   (yaml-ts-mode         . eglot-ensure)
   (astro-ts-mode        . eglot-ensure)
   (web-mode             . eglot-ensure))

  :bind
  (:map eglot-mode-map
        ("C-c l r"   . eglot-rename)
        ("C-c l a"   . eglot-code-actions)
        ("C-c l f"   . eglot-format)
        ("C-c l F"   . eglot-format-buffer)
        ("C-c l d"   . eldoc)
        ("C-c l R"   . eglot-reconnect)
        ("C-c l q"   . eglot-shutdown)
        ("C-c l Q"   . eglot-shutdown-all)
        ("C-c l ."   . xref-find-definitions)
        ("C-c l ,"   . xref-go-back)
        ("C-c l ?"   . xref-find-references)
        ("C-c l i"   . eglot-find-implementation)
        ("C-c l t"   . eglot-find-typeDefinition)
        ("C-c l /"   . xref-find-apropos))

  :custom
  ;; Don't litter the modeline with eglot server name
  (eglot-autoshutdown t)
  ;; Use eldoc echo area (consistent with previous lsp-mode setup)
  ;; (eldoc-display-functions '(eldoc-display))
  ;; Boost performance: don't log events unless debugging
  (eglot-events-buffer-size 0)
  ;; Don't confirm when applying code actions
  (eglot-confirm-server-initiated-edits nil)
  ;; Disable blocking on server connection
  (eglot-sync-connect nil)
  ;; Disable noisy progress reports in echo area
  (eglot-report-progress nil)
  ;; Disable heavy server capabilities (highlighting on cursor move, inlay hints)
  (eglot-ignored-server-capabilities '(:documentHighlightProvider
                                       :inlayHintProvider))

  (eglot-documentation-renderer 'markdown-ts-view-mode) ;; EMACS-31
  (eglot-code-action-indications nil)

  (eldoc-help-at-pt t) ;; EMACS-31
  (eldoc-echo-area-prefer-doc-buffer t)

  :config
  ;; Angular Language Server
  ;; Requires: npm i -g @angular/language-server typescript
  (add-to-list 'eglot-server-programs
               '((typescript-ts-mode tsx-ts-mode js-ts-mode js-mode)
                 . ("typescript-language-server" "--stdio")))

  (with-eval-after-load 'eglot
    (add-to-list 'eglot-server-programs
                 ;; Angular detection: presence of angular.json in project root
                 `(typescript-ts-mode
                   . ,(lambda (interactive)
                        (if (and (projectile-project-root)
                                 (file-exists-p
                                  (expand-file-name "angular.json"
                                                    (projectile-project-root))))
                            '("ngserver" "--stdio"
                              "--tsProbeLocations" ".,c:/Users/s.schubert/AppData/Roaming/npm/node_modules"
                              "--ngProbeLocations" ".,c:/Users/s.schubert/AppData/Roaming/npm/node_modules")
                          '("typescript-language-server" "--stdio"))))))

  ;; YAML Language Server with schema associations
  ;; Requires: npm i -g yaml-language-server
  (add-to-list 'eglot-server-programs
               `(yaml-ts-mode . ("yaml-language-server" "--stdio")))

  ;; Razor and Angular Language Servers for web-mode
  ;; Dynamically selects ngserver if in an Angular project, else rzls.
  (with-eval-after-load 'eglot
    (add-to-list 'eglot-server-programs
                 `(web-mode
                   . ,(lambda (interactive)
                        (if (and (projectile-project-root)
                                 (file-exists-p
                                  (expand-file-name "angular.json"
                                                    (projectile-project-root))))
                            '("ngserver" "--stdio"
                              "--tsProbeLocations" ".,c:/Users/s.schubert/AppData/Roaming/npm/node_modules"
                              "--ngProbeLocations" ".,c:/Users/s.schubert/AppData/Roaming/npm/node_modules")
                          '("rzls" "--logLevel" "Information"))))))

  ;; Astro Language Server
  ;; Requires: npm i -g @astrojs/language-server
  (add-to-list 'eglot-server-programs
               '(astro-ts-mode . ("astro-ls" "--stdio"
                                  :initializationOptions
                                  (:typescript (:tsdk "node_modules/typescript/lib")))))

  ;; Pass YAML schema settings via workspace configuration
  (setq-default eglot-workspace-configuration
                '(:yaml
                  (:validate t
                             :hover t
                             :completion t
                             :format (:enable t)
                             :schemas
                             (:https://raw.githubusercontent.com/yannh/kubernetes-json-schema/master/v1.31.0-standalone-strict/all.json
                              ["/*.k8s.yaml" "/*.k8s.yml" "/kubernetes/**/*.yaml" "/k8s/**/*.yaml" "/manifests/**/*.yaml"]
                              :https://raw.githubusercontent.com/compose-spec/compose-spec/master/schema/compose-spec.json
                              ["docker-compose.yaml" "docker-compose.yml" "compose.yaml" "compose.yml"]
                              :https://json.schemastore.org/github-workflow.json
                              [".github/workflows/*.yaml" ".github/workflows/*.yml"]
                              :https://json.schemastore.org/github-action.json
                              ["action.yaml" "action.yml"]
                              :https://json.schemastore.org/chart.json
                              ["Chart.yaml" "Chart.yml"]
                              :https://json.schemastore.org/pre-commit-config.json
                              [".pre-commit-config.yaml"]
                              :https://gitlab.com/gitlab-org/gitlab/-/raw/master/app/assets/javascripts/editor/schema/ci.json
                              [".gitlab-ci.yml" ".gitlab-ci.yaml"]))
                  :gopls
                  (:usePlaceholders t
                   :staticcheck t
                   :analyses
                   (:unusedparams t
                    :shadow t
                    :fieldalignment t)
                   :hints
                   (:assignVariableTypes t
                    :compositeLiteralFields t
                    :functionTypeParameters t
                    :parameterNames t
                    :rangeVariableTypes t))))

  ;; Integrate yasnippet with eglot via cape
  (defun ss/eglot-capf ()
    "Set up eglot completion with yasnippet and cape."
    (setq-local completion-at-point-functions
                (list (cape-capf-super
                       #'eglot-completion-at-point
                       #'yasnippet-capf)
                      #'cape-file)))
  (add-hook 'eglot-managed-mode-hook #'ss/eglot-capf))

;; Yasnippet capf adapter - bridges yasnippet into capf/corfu
(use-package yasnippet-capf
  :after (yasnippet cape))

;;; ------------------------------------------------------------
;;; consult-eglot - Workspace Symbol Browser
;;; ------------------------------------------------------------
;; Provides consult-eglot-symbols: fuzzy/orderless search over all
;; workspace symbols via LSP workspace/symbol request, displayed
;; through the Vertico/Consult interface.
;;
;; Keybindings:
;;   M-s M-s  — browse workspace symbols (global, alongside other M-s consult bindings)
;;   C-c l /  — xref-find-apropos (built-in fallback, active in eglot-managed buffers)

(use-package consult-eglot
  :straight t
  :bind
  ("M-s M-s" . consult-eglot-symbols))

(provide 'eglot-config)
;;; eglot-config.el ends here
