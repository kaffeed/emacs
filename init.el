;;; ------------------------------------------------------------
;;; Package Setup
;;; ------------------------------------------------------------

;; Prefer UTF-8 for everything
(prefer-coding-system 'utf-8)
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(setq-default default-buffer-file-coding-system 'utf-8)
(setq gc-cons-threshold (* 50 1000 1000))

(setq inhibit-startup-message t
      inhibit-startup-screen t
      visible-bell t)

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(column-number-mode)

(global-display-line-numbers-mode t)
(dolist (mode '(org-mode-hook
		        term-mode-hook
		        eshell-mode-hook))
  (add-hook mode (lambda() (display-line-numbers-mode 0))))

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
(setq-default straight-use-package-by-default t
              use-package-verbose t)

;;; ------------------------------------------------------------
;;; OS Settings
;;; ------------------------------------------------------------
(defconst *is-a-mac*   (eq system-type 'darwin))
(defconst *is-a-linux* (eq system-type 'gnu/linux))

(when *is-a-mac*
  (setq-default mac-command-modifier 'meta
                mac-option-modifier  'none
                default-input-method "MacOSX"))

(when *is-a-linux*
  (setq-default x-super-keysym 'meta))

(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Unbind annoying suspend frame keybinding
(global-unset-key (kbd "C-x C-z"))

;;; ------------------------------------------------------------
;;; Theme
;;; ------------------------------------------------------------
(use-package nano
  :straight (:type git :host github :repo "rougier/nano-emacs")
  ;; :custom
  :config
  (require 'nano)
  (require 'nano-faces)
  (require 'nano-theme))

;;; ------------------------------------------------------------
;;; Environment Variables (important for macOS)
;;; ------------------------------------------------------------
(use-package exec-path-from-shell
  :init
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

;; The `vertico' package applies a vertical layout to the minibuffer.
;; It also pops up the minibuffer eagerly so we can see the available
;; options without further interactions.  This package is very fast
;; and "just works", though it also is highly customisable in case we
;; need to modify its behaviour.
;;
;; Further reading: https://protesilaos.com/emacs/dotemacs#h:cff33514-d3ac-4c16-a889-ea39d7346dc5
(use-package vertico
  :config
  (setq-default vertico-cycle t)
  (setq-default vertico-resize nil)
  (vertico-mode 1)
  ;; Load vertico-multiform feature before configuring it
  (require 'vertico-multiform)
  (add-to-list 'vertico-multiform-categories '(embark-keybinding grid))
  (vertico-multiform-mode))

;; The `marginalia' package provides helpful annotations next to
;; completion candidates in the minibuffer.  The information on
;; display depends on the type of content.  If it is about files, it
;; shows file permissions and the last modified date.  If it is a
;; buffer, it shows the buffer's size, major mode, and the like.
;;
;; Further reading: https://protesilaos.com/emacs/dotemacs#h:bd3f7a1d-a53d-4d3e-860e-25c5b35d8e7e
(use-package marginalia
  :config
  (marginalia-mode 1))

;; The `orderless' package lets the minibuffer use an out-of-order
;; pattern matching algorithm.  It matches space-separated words or
;; regular expressions in any order.  In its simplest form, something
;; like "ins pac" matches `package-menu-mark-install' as well as
;; `package-install'.  This is a powerful tool because we no longer
;; need to remember exactly how something is named.
;;
;; Note that Emacs has lots of "completion styles" (pattern matching
;; algorithms), but let us keep things simple.
;;
;; Further reading: https://protesilaos.com/emacs/dotemacs#h:7cc77fd0-8f98-4fc0-80be-48a758fcb6e2
(use-package orderless
  :config
  (setq completion-styles '(orderless basic)))


;; The `consult' package provides lots of commands that are enhanced
;; variants of basic, built-in functionality.  One of the headline
;; features of `consult' is its preview facility, where it shows in
;; another Emacs window the context of what is currently matched in
;; the minibuffer.  Here I define key bindings for some commands you
;; may find useful.  The mnemonic for their prefix is "alternative
;; search" (as opposed to the basic C-s or C-r keys).
;;
;; Further reading: https://protesilaos.com/emacs/dotemacs#h:22e97b4c-d88d-4deb-9ab3-f80631f9ff1d
(use-package consult
  :bind (;; A recursive grep
         ("M-s M-g" . consult-grep)
         ;; Search for files names recursively
         ("M-s M-f" . consult-find)
         ;; Search through the outline (headings) of the file
         ("M-s M-o" . consult-outline)
         ;; Search the current buffer
         ("M-s M-l" . consult-line)
         ;; Switch to another buffer, or bookmarked file, or recently
         ;; opened file.
         ("M-s M-b" . consult-buffer)))


;; The `embark' package lets you target the thing or context at point
;; and select an action to perform on it.  Use the `embark-act'
;; command while over something to find relevant commands.
;;
;; When inside the minibuffer, `embark' can collect/export the
;; contents to a fully fledged Emacs buffer.  The `embark-collect'
;; command retains the original behaviour of the minibuffer, meaning
;; that if you navigate over the candidate at hit RET, it will do what
;; the minibuffer would have done.  In contrast, the `embark-export'
;; command reads the metadata to figure out what category this is and
;; places them in a buffer whose major mode is specialised for that
;; type of content.  For example, when we are completing against
;; files, the export will take us to a `dired-mode' buffer; when we
;; preview the results of a grep, the export will put us in a
;; `grep-mode' buffer.
;;
;; Further reading: https://protesilaos.com/emacs/dotemacs#h:61863da4-8739-42ae-a30f-6e9d686e1995
(use-package embark
  :bind (("C-." . embark-act)
         ("C-;" . embark-dwim)
         ("C-h B" . embark-bindings)
         :map minibuffer-local-map
         ("C-c C-c" . embark-collect)
         ("C-c C-e" . embark-export))
  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none))))
  )

;; The `embark-consult' package is glue code to tie together `embark'
;; and `consult'.
(use-package embark-consult
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

;; The `wgrep' packages lets us edit the results of a grep search
;; while inside a `grep-mode' buffer.  All we need is to toggle the
;; editable mode, make the changes, and then type C-c C-c to confirm
;; or C-c C-k to abort.
;;
;; Further reading: https://protesilaos.com/emacs/dotemacs#h:9a3581df-ab18-4266-815e-2edd7f7e4852
(use-package wgrep
  :bind ( :map grep-mode-map
          ("e" . wgrep-change-to-wgrep-mode)
          ("C-x C-q" . wgrep-change-to-wgrep-mode)
          ("C-c C-c" . wgrep-finish-edit)))

;;; ------------------------------------------------------------
;;; Projectile
;;; ------------------------------------------------------------
(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  (when (file-directory-p "~/source")
    (setq projectile-project-search-path '("~/source" . 1)))
  (setq projectile-switch-project-action #'projectile-find-file))

;;; ------------------------------------------------------------
;;; Company
;;; ------------------------------------------------------------

;; Company is the best Emacs completion system.
(use-package company
  :init
  (global-company-mode)

  :custom
  ;; Make Company less intrusive and more like modern IDE autocomplete
  (company-idle-delay 0.05)        ;; fast popup
  (company-minimum-prefix-length 1)
  (company-tooltip-align-annotations t)
  (company-tooltip-limit 12)
  (company-tooltip-minimum-width 40)
  (company-show-numbers t)
  (company-require-match nil)
  (company-dabbrev-other-buffers t)
  (company-dabbrev-downcase nil)

  :bind
  (:map company-active-map
        ("<tab>" . company-complete-selection)
        ("TAB"   . company-complete-selection)
        ("C-n"   . company-select-next)
        ("C-p"   . company-select-previous))

  :config
  (company-tng-mode)
  ;; Good backends for general programming
  (setq company-backends
        '((company-capf            ;; completion-at-point (LSP, modes, etc.)
           company-dabbrev-code)   ;; fallback for code-like text
          company-dabbrev))        ;; fallback for everything else
  )

;; Flycheck is the newer version of flymake and is needed to make lsp-mode not freak out.
(use-package flycheck
  :config
  (add-hook 'prog-mode-hook 'flycheck-mode) ;; always lint my code
  (add-hook 'after-init-hook #'global-flycheck-mode))

;;; ------------------------------------------------------------
;;; Lsp
;;; ------------------------------------------------------------

(use-package lsp-mode
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-c l")
  :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
         (csharp-mode . lsp-deferred)

         ;; if you want which-key integration
         (lsp-mode . lsp-enable-which-key-integration))
  :commands (lsp lsp-deferred)
  :config
  (setq-default lsp-headerline-breadcrumb-enable nil)
  (setq-default lsp-use-plists t))

(setq fast-read-process-output (* 1024 1024))
(use-package lsp-ui :commands lsp-ui-mode)
(use-package which-key :config (which-key-mode))

(use-package sharper
  :demand t
  :bind
  ("C-c d" . sharper-main-transient))

;;; ------------------------------------------------------------
;;; Magit
;;; ------------------------------------------------------------

(use-package magit
  :commands (magit-status magit-get-current-branch)
  :bind (("C-x g" . magit-status))
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(use-package diff-hl
  :hook ((prog-mode . diff-hl-mode)
         (text-mode . diff-hl-mode)
         (magit-pre-refresh . diff-hl-magit-pre-refresh)
         (magit-post-refresh . diff-hl-magit-post-refresh)))

(use-package git-gutter
  :hook ((prog-mode . git-gutter-mode)
         (text-mode . git-gutter-mode))
  :custom
  (git-gutter:update-interval 1))

(with-eval-after-load 'projectile
  (define-key projectile-mode-map (kbd "C-c p g") #'magit-status))

(add-hook 'git-commit-setup-hook #'git-commit-turn-on-flyspell) ;; if using flyspell
(add-hook 'git-commit-mode-hook #'turn-on-auto-fill)

(use-package forge
  :after magit)

(global-set-key (kbd "C-x M-g") #'magit-dispatch) ;; access all magit commands
(global-set-key (kbd "C-c M-g") #'magit-file-dispatch) ;; file-based git menu

;;; ------------------------------------------------------------
;;; Org-Mode setup
;;; ------------------------------------------------------------

(defun dw/org-mode-setup ()
  (org-indent-mode)
  (variable-pitch-mode 1)
  (auto-fill-mode 0)
  (visual-line-mode 1))

(use-package org
  :hook (org-mode . dw/org-mode-setup)
  :bind (;; Global org shortcuts
         ("C-c a" . org-agenda)
         ("C-c n" . org-capture)
         ("C-c l" . org-store-link)
         ("C-c j" . org-journal-new-entry)

         ;; Org-mode specific shortcuts
         :map org-mode-map
         ("C-c C-q" . org-set-tags-command)
         ("C-c C-t" . org-todo)
         ("C-c C-s" . org-schedule)
         ("C-c C-d" . org-deadline)
         ("C-c C-w" . org-refile)
         ("C-c C-a" . org-archive-subtree)
         ("C-c C-x C-i" . org-clock-in)
         ("C-c C-x C-o" . org-clock-out)
         ("C-c C-x C-j" . org-clock-goto)
         ("C-c C-x C-r" . org-clock-report)
         ("C-c C-l" . org-insert-link)
         ("C-c C-o" . org-open-at-point)
         ("C-c ," . org-priority)
         ("C-c ;" . org-toggle-comment)
         ("M-<up>" . org-move-subtree-up)
         ("M-<down>" . org-move-subtree-down)
         ("C-c C-x C-v" . org-toggle-inline-images)
         ("C-c C-x p" . org-set-property)
         ("C-c !" . org-time-stamp-inactive)
         ("C-c ." . org-time-stamp))
  :custom
  ;; Set default directory for org files
  (org-directory "~/org")

  ;; Define which files to include in the agenda
  (org-agenda-files '("~/org/inbox.org"
                      "~/org/todo.org"
                      "~/org/projects.org"
                      "~/org/journal.org"
                      "~/org/userstories.org"
                      "~/org/backlog.org"
                      "~/org/sprints.org"
                      "~/org/recurring.org"
                      "~/org/meetings.org"))

  ;; Agenda settings
  (org-agenda-start-with-log-mode t)
  (org-log-done 'time)
  (org-log-into-drawer t)

  ;; TODO keywords
  (org-todo-keywords
   '((sequence "TODO(t)" "NEXT(n)" "STARTED(s)" "|" "DONE(d!)" "DELEGATED(g)")
     (sequence "BACKLOG(b)" "REFINED(f)" "READY(r)" "ACTIVE(a)" "REVIEW(v)" "TESTING(e)" "WAIT(w@/!)" "HOLD(h)" "|" "COMPLETED(c!)" "CANC(k@)")))

  ;; Properties for effort tracking and Azure DevOps
  (org-global-properties
   '(("Effort_ALL" . "0:15 0:30 1:00 2:00 3:00 4:00 8:00 16:00 24:00 40:00")
     ("STORY_POINTS_ALL" . "1 2 3 5 8 13 21")
     ("DEVOPS_ID_ALL" . "")
     ("SPRINT_ALL" . "")
     ("ATTENDEES_ALL" . "")))

  ;; Column view for comprehensive tracking
  (org-columns-default-format
   "%50ITEM(Task) %TODO %PRIORITY %10Effort(Effort){:} %10CLOCKSUM(Clocked) %6STORY_POINTS(SP) %12DEVOPS_ID(Azure) %TAGS")

  ;; Automatically change to STARTED when clocking in
  (org-clock-in-switch-to-state "STARTED")

  ;; Clock persistence
  (org-clock-persist t)
  (org-clock-mode-line-total 'current)

  ;; Priority system
  (org-priority-highest ?A)
  (org-priority-lowest ?C)
  (org-priority-default ?B)
  (org-priority-faces
   '((?A . (:foreground "red" :weight bold))
     (?B . (:foreground "yellow"))
     (?C . (:foreground "green"))))

  ;; Enhanced tagging system
  (org-tag-alist
   '(;; Context
     (:startgroup)
     ("@po" . ?p)
     ("@dev" . ?d)
     (:endgroup)
     ;; Activities
     ("planning" . ?P)
     ("grooming" . ?g)
     ("review" . ?r)
     ("retro" . ?R)
     ("meeting" . ?m)
     ("1on1" . ?1)
     ("stakeholder" . ?s)
     ;; Products
     (:startgroup)
     ("p1" . ?1)
     ("cc" . ?c)
     ("perdis" . ?i)
     ("backoffice" . ?b)
     ("shared" . ?S)
     (:endgroup)
     ;; Technical
     ("frontend" . ?f)
     ("backend" . ?k)
     ("infra" . ?I)
     ("testing" . ?t)
     ("bug" . ?B)
     ("design" . ?D)
     ;; Priority/Status
     ("quick" . ?q)
     ("blocked" . ?x)
     ("urgent" . ?u)
     ("habit" . ?h)))

  ;; Refile targets
  (org-refile-targets '((nil :maxlevel . 3)
                        (org-agenda-files :maxlevel . 2)
                        ("~/org/someday.org" :maxlevel . 2)
                        ("~/org/archive" :maxlevel . 1)))

  (org-refile-use-outline-path 'file)
  (org-outline-path-complete-in-steps nil)
  (org-refile-allow-creating-parent-nodes 'confirm)

  ;; Save org buffers after refiling
  (advice-add 'org-refile :after 'org-save-all-org-buffers)

  ;; Habit tracking
  (org-modules '(org-habit))
  (org-habit-graph-column 60)

  :config
  ;; Load org-habit module
  (require 'org-habit)
  (add-to-list 'org-modules 'org-habit)

  ;; Initialize clock persistence
  (org-clock-persistence-insinuate)

  ;; Agenda appearance enhancements
  (setq org-agenda-block-separator ?─)
  (setq org-agenda-time-grid
        '((daily today require-timed)
          (800 1000 1200 1400 1600 1800 2000)
          " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄"))
  (setq org-agenda-current-time-string
        "◀── now ─────────────────────────────────────────────────")

  ;; Custom agenda views
  (setq org-agenda-custom-commands
        '(;; MAIN DASHBOARD
          ("d" "Dashboard"
           ((agenda "" ((org-deadline-warning-days 7)
                        (org-agenda-span 'day)
                        (org-agenda-overriding-header "Today's Schedule")))
            (tags "PRIORITY=\"A\""
                  ((org-agenda-overriding-header "High Priority Tasks")
                   (org-agenda-skip-function '(org-agenda-skip-entry-if 'done))))
            (todo "NEXT|STARTED"
                  ((org-agenda-overriding-header "In Progress / Next Actions")
                   (org-agenda-sorting-strategy '(priority-down effort-up))))
            (todo "WAIT"
                  ((org-agenda-overriding-header "Blocked/Waiting Items")
                   (org-agenda-skip-function '(org-agenda-skip-entry-if 'scheduled))))
            (tags-todo "+SCHEDULED<\"<+7d>\"/!TODO|NEXT"
                       ((org-agenda-overriding-header "Coming Up This Week")))
            (tags "PRIORITY=\"B\"+Effort<\"1:00\""
                  ((org-agenda-overriding-header "Quick Wins (<1h)")
                   (org-agenda-skip-function '(org-agenda-skip-entry-if 'done))
                   (org-agenda-max-entries 5)))))

          ;; SPRINT VIEWS
          ("s" "Sprint Views")
          ("ss" "Current Sprint"
           ((tags-todo "ACTIVE|READY|STARTED|REVIEW|TESTING"
                       ((org-agenda-overriding-header "Sprint Backlog")
                        (org-agenda-files '("~/org/userstories.org" "~/org/todo.org"))
                        (org-agenda-sorting-strategy '(priority-down))))
            (tags "planning|review"
                  ((org-agenda-overriding-header "Sprint Events")
                   (org-agenda-files '("~/org/sprints.org"))))))

          ("sb" "Sprint Burndown"
           ((tags-todo "TODO=\"ACTIVE\"|TODO=\"STARTED\""
                       ((org-agenda-overriding-header "In Progress")
                        (org-agenda-prefix-format "  %?-12t% s")))
            (tags-todo "TODO=\"READY\""
                       ((org-agenda-overriding-header "Ready to Start")))
            (tags-todo "TODO=\"REVIEW\"|TODO=\"TESTING\""
                       ((org-agenda-overriding-header "In Review/Testing")))
            (tags-todo "TODO=\"COMPLETED\"+CLOSED>=\"<-7d>\""
                       ((org-agenda-overriding-header "Completed This Sprint")))))

          ;; BACKLOG VIEWS
          ("b" "Backlog Views")
          ("bb" "Product Backlog"
           ((tags-todo "TODO=\"BACKLOG\"+PRIORITY=\"A\""
                       ((org-agenda-overriding-header "High Priority Backlog")
                        (org-agenda-files '("~/org/userstories.org" "~/org/backlog.org"))))
            (tags-todo "TODO=\"REFINED\""
                       ((org-agenda-overriding-header "Refined & Ready for Planning")))
            (tags-todo "TODO=\"BACKLOG\"+PRIORITY=\"B\""
                       ((org-agenda-overriding-header "Medium Priority Backlog")
                        (org-agenda-max-entries 15)))))

          ("bg" "Grooming Candidates"
           ((tags-todo "TODO=\"BACKLOG\"-EFFORT"
                       ((org-agenda-overriding-header "Missing Effort Estimates")))
            (tags-todo "TODO=\"BACKLOG\"-PROPERTIES={:CREATED:}"
                       ((org-agenda-overriding-header "Needs Refinement")))
            (tags-todo "TODO=\"BACKLOG\"+PRIORITY<>\"A\"+PRIORITY<>\"B\"+PRIORITY<>\"C\""
                       ((org-agenda-overriding-header "Needs Priority")))))

          ;; WORKFLOW STATUS
          ("w" "Workflow Status"
           ((todo "WAIT"
                  ((org-agenda-overriding-header "⏸ Waiting on External")
                   (org-agenda-files org-agenda-files)))
            (todo "REVIEW|TESTING"
                  ((org-agenda-overriding-header "🔍 In Review/Testing")
                   (org-agenda-files org-agenda-files)))
            (todo "HOLD"
                  ((org-agenda-overriding-header "🛑 On Hold")
                   (org-agenda-files org-agenda-files)))
            (tags-todo "+blocked"
                       ((org-agenda-overriding-header "🚫 Blocked Items")))))

          ;; PO-SPECIFIC VIEWS
          ("p" "Product Owner Views")
          ("pm" "Meetings & Planning"
           ((tags "planning|grooming|review|retro|stakeholder|1on1"
                  ((org-agenda-overriding-header "PO Activities")
                   (org-agenda-files '("~/org/recurring.org" "~/org/meetings.org" "~/org/sprints.org"))
                   (org-agenda-span 14)))))

          ("pa" "Product Areas"
           ((tags-todo "+backoffice"
                       ((org-agenda-overriding-header "Backoffice")
                        (org-agenda-files '("~/org/userstories.org"))))
            (tags-todo "+perdis"
                       ((org-agenda-overriding-header "Perdis")
                        (org-agenda-files '("~/org/userstories.org"))))
            (tags-todo "+cc"
                       ((org-agenda-overriding-header "ComCenter")
                        (org-agenda-files '("~/org/userstories.org"))))))

          ("pd" "DevOps Integration"
           ((tags "DEVOPS_ID<>\"\""
                  ((org-agenda-overriding-header "Items with Azure DevOps ID")
                   (org-agenda-files '("~/org/userstories.org"))
                   (org-agenda-prefix-format "  %?-12t [%s] ")))))

          ;; DEV-SPECIFIC VIEWS
          ("v" "Developer Views")
          ("vn" "Next Actions"
           ((todo "NEXT"
                  ((org-agenda-overriding-header "Next Actions")
                   (org-agenda-sorting-strategy '(priority-down effort-up))))
            (tags "+quick"
                  ((org-agenda-overriding-header "Quick Wins")
                   (org-agenda-skip-function '(org-agenda-skip-entry-if 'done))))))

          ("ve" "By Effort"
           ((tags "+TODO=\"NEXT\"+Effort<\"1:00\""
                  ((org-agenda-overriding-header "< 1 Hour")
                   (org-agenda-max-todos 10)))
            (tags "+TODO=\"NEXT\"+Effort>=\"1:00\"+Effort<\"3:00\""
                  ((org-agenda-overriding-header "1-3 Hours")
                   (org-agenda-max-todos 10)))
            (tags "+TODO=\"NEXT\"+Effort>=\"3:00\""
                  ((org-agenda-overriding-header "3+ Hours")
                   (org-agenda-max-todos 10)))))

          ("vt" "By Technology"
           ((tags-todo "+frontend"
                       ((org-agenda-overriding-header "Frontend Work")))
            (tags-todo "+backend"
                       ((org-agenda-overriding-header "Backend Work")))
            (tags-todo "+infra"
                       ((org-agenda-overriding-header "Infrastructure")))))

          ;; TIME-BASED VIEWS
          ("t" "Time-based Views")
          ("tt" "Today's Focus"
           ((agenda "" ((org-agenda-span 1)
                        (org-agenda-overriding-header "Today")))
            (tags "PRIORITY=\"A\""
                  ((org-agenda-overriding-header "Must Do Today")
                   (org-agenda-skip-function '(org-agenda-skip-entry-if 'done))))
            (todo "STARTED"
                  ((org-agenda-overriding-header "Currently Working On")))))

          ("tw" "This Week"
           ((agenda "" ((org-agenda-span 7)
                        (org-agenda-start-on-weekday 1)
                        (org-agenda-overriding-header "This Week")))
            (tags-todo "+SCHEDULED<\"<+7d>\""
                       ((org-agenda-overriding-header "Due This Week")))))

          ;; REVIEW VIEWS
          ("r" "Review Views")
          ("ri" "Inbox Processing"
           ((tags "LEVEL=2"
                  ((org-agenda-overriding-header "Inbox Items to Process")
                   (org-agenda-files '("~/org/inbox.org"))))))

          ("rd" "Done Recently"
           ((tags "+TODO=\"DONE\"+CLOSED>=\"<-7d>\""
                  ((org-agenda-overriding-header "Completed Last 7 Days")))
            (tags "+TODO=\"COMPLETED\"+CLOSED>=\"<-14d>\""
                  ((org-agenda-overriding-header "Completed Last 14 Days")))))

          ("ru" "Unscheduled Tasks"
           ((tags-todo "-SCHEDULED-DEADLINE"
                       ((org-agenda-overriding-header "Unscheduled Tasks")
                        (org-agenda-skip-function
                         '(org-agenda-skip-entry-if 'regexp "Inbox"))))))))

  ;; Capture templates
  (setq org-capture-templates
        '(;; Quick Captures
          ("i" "Inbox - Quick Capture" entry (file "~/org/inbox.org")
           "* TODO %?\n:PROPERTIES:\n:CREATED: %U\n:END:\n" :empty-lines 1)

          ;; Tasks
          ("t" "Tasks")
          ("tt" "Task" entry (file "~/org/todo.org")
           "* TODO [#B] %?\n:PROPERTIES:\n:CREATED: %U\n:EFFORT: 1:00\n:END:\n%a\n" :empty-lines 1)
          ("tn" "Next Action" entry (file "~/org/todo.org")
           "* NEXT [#A] %?\n:PROPERTIES:\n:CREATED: %U\n:EFFORT: 1:00\n:END:\n%a\n" :empty-lines 1)
          ("td" "Dev Task" entry (file "~/org/todo.org")
           "* TODO [#B] %? :@dev:\n:PROPERTIES:\n:CREATED: %U\n:EFFORT: 2:00\n:END:\n%a\n** Technical Details\n\n** Acceptance Criteria\n- [ ] \n" :empty-lines 1)

          ;; Projects & Features
          ("p" "Projects")
          ("pp" "Project" entry (file "~/org/projects.org")
           "* BACKLOG [#B] %? :project:\n:PROPERTIES:\n:CREATED: %U\n:END:\n** Goal\n\n** Success Criteria\n\n** Tasks\n*** TODO Define scope\n*** TODO Create user stories\n*** TODO Plan implementation\n" :empty-lines 1)
          ("pf" "Feature" entry (file "~/org/projects.org")
           "* BACKLOG [#B] %? :feature:\n:PROPERTIES:\n:CREATED: %U\n:EFFORT: 8:00\n:END:\n** Description\n\n** User Value\n\n** Technical Approach\n" :empty-lines 1)

          ;; User Stories
          ("u" "User Stories")
          ("us" "User Story" entry (file "~/org/userstories.org")
           "* BACKLOG [#B] %? :@po:\n:PROPERTIES:\n:CREATED: %U\n:DEVOPS_ID: \n:STORY_POINTS: 5\n:END:\n** User Story\nAls <Rolle> möchte ich <Funktion>, damit <Nutzen>.\n\n** Acceptance Criteria\n- [ ] \n- [ ] \n- [ ] \n\n** Technical Notes\n\n** Related Items\n" :empty-lines 1)
          ("ub" "Bug/Issue" entry (file "~/org/userstories.org")
           "* TODO [#A] Bug: %? :bug:\n:PROPERTIES:\n:CREATED: %U\n:DEVOPS_ID: \n:EFFORT: 2:00\n:END:\n** Problem Description\n\n** Steps to Reproduce\n1. \n\n** Expected Behavior\n\n** Actual Behavior\n\n** Fix Approach\n" :empty-lines 1)

          ;; Backlog
          ("b" "Backlog Item" entry (file "~/org/backlog.org")
           "* BACKLOG [#C] %? :backlog:\n:PROPERTIES:\n:CREATED: %U\n:END:\n** Description\n\n** Value\n\n** Effort Estimate\n" :empty-lines 1)

          ;; Sprint Planning
          ("s" "Sprint Planning")
          ("sp" "Sprint Planning Session" entry (file "~/org/sprints.org")
           "* ACTIVE Sprint %^{Sprint Number} Planning :planning:@po:\nSCHEDULED: %^T\n:PROPERTIES:\n:CREATED: %U\n:SPRINT_START: %^{Start Date}T\n:SPRINT_END: %^{End Date}T\n:SPRINT_GOAL: %^{Sprint Goal}\n:END:\n** Sprint Goal\n%\\3\n\n** Committed Stories\n\n** Sprint Capacity\n- Team capacity: %^{Capacity in hours} hours\n- Story points committed: \n\n** Notes\n" :empty-lines 1)
          ("sr" "Sprint Review" entry (file "~/org/sprints.org")
           "* Sprint %^{Sprint Number} Review :review:@po:\nSCHEDULED: %^T\n:PROPERTIES:\n:CREATED: %U\n:END:\n** Completed Stories\n\n** Demo Items\n\n** Incomplete Items\n\n** Stakeholder Feedback\n\n** Action Items\n- [ ] \n" :empty-lines 1)
          ("st" "Sprint Retrospective" entry (file "~/org/sprints.org")
           "* Sprint %^{Sprint Number} Retrospective :retro:@po:\nSCHEDULED: %^T\n:PROPERTIES:\n:CREATED: %U\n:END:\n** What went well\n- \n\n** What could be improved\n- \n\n** Action Items\n- [ ] \n\n** Team Mood: %^{1-5}\n" :empty-lines 1)

          ;; Recurring PO Activities
          ("r" "Recurring Activities")
          ("rg" "Backlog Grooming" entry (file "~/org/recurring.org")
           "* TODO Backlog Grooming :grooming:@po:\nSCHEDULED: %^T\n:PROPERTIES:\n:CREATED: %U\n:EFFORT: 2:00\n:END:\n** Agenda\n- Review top backlog items\n- Refine acceptance criteria\n- Update priorities\n- Add effort estimates\n\n** Items Reviewed\n\n** Action Items\n- [ ] \n" :empty-lines 1)
          ("rs" "Stakeholder Meeting" entry (file "~/org/meetings.org")
           "* TODO Meeting: %^{Stakeholder} :stakeholder:meeting:@po:\nSCHEDULED: %^T\n:PROPERTIES:\n:CREATED: %U\n:EFFORT: 1:00\n:ATTENDEES: %^{Attendees}\n:END:\n** Agenda\n- \n\n** Discussion Points\n\n** Decisions\n\n** Action Items\n- [ ] \n" :empty-lines 1)
          ("r1" "1-on-1 Meeting" entry (file "~/org/meetings.org")
           "* TODO 1-on-1: %^{Person} :1on1:meeting:@po:\nSCHEDULED: %^T\n:PROPERTIES:\n:CREATED: %U\n:EFFORT: 0:30\n:END:\n** Topics\n- \n\n** Discussion\n\n** Action Items\n- [ ] \n\n** Next Meeting Topics\n- \n" :empty-lines 1)
          ("rt" "Team Sync" entry (file "~/org/meetings.org")
           "* TODO Team Sync :meeting:@po:\nSCHEDULED: %^T\n:PROPERTIES:\n:CREATED: %U\n:EFFORT: 0:30\n:END:\n** Blockers\n\n** Updates\n\n** Upcoming Work\n\n** Action Items\n- [ ] \n" :empty-lines 1)

          ;; UX/Design
          ("x" "UX/Design Item" entry (file "~/org/projects.org")
           "* TODO [#B] %? :design:\n:PROPERTIES:\n:CREATED: %U\n:END:\n** Design Requirements\n\n** Figma Links\n\n** Decision\n" :empty-lines 1)

          ;; Journal
          ("j" "Journal Entries")
          ("jj" "Journal" entry
           (file+olp+datetree "~/org/journal.org")
           "\n* %<%H:%M> - %? :journal:\n:PROPERTIES:\n:CREATED: %U\n:END:\n"
           :clock-in :clock-resume
           :empty-lines 1)
          ("jm" "Meeting Notes" entry
           (file+olp+datetree "~/org/journal.org")
           "\n* %<%H:%M> - Meeting: %^{Meeting Title} :meeting:\n:PROPERTIES:\n:CREATED: %U\n:ATTENDEES: %^{Attendees}\n:END:\n** Agenda\n%?\n\n** Notes\n\n** Action Items\n- [ ] \n"
           :clock-in :clock-resume
           :empty-lines 1)

          ;; Notes
          ("n" "Note" entry (file "~/org/notes.org")
           "* %? :note:\n:PROPERTIES:\n:CREATED: %U\n:END:\n" :empty-lines 1)

          ;; Someday/Maybe
          ("y" "Someday/Maybe" entry (file "~/org/someday.org")
           "* BACKLOG [#C] %?\n:PROPERTIES:\n:CREATED: %U\n:END:\n" :empty-lines 1))))

(use-package org-journal
  :after org
  :bind ("C-c j" . org-journal-new-entry)
  :custom
  (org-journal-dir "~/org/")
  (org-journal-file-type 'yearly)
  (org-journal-file-format "journal.org")
  (org-journal-date-format "%A, %d %B %Y")
  (org-journal-enable-agenda-integration t))

;;; ------------------------------------------------------------
;;; Misc packages
;;; ------------------------------------------------------------

;; Compile command
(global-set-key (kbd "C-c c") 'compile)

(use-package ace-window
  :bind (("M-o" . ace-window))
  :custom
  (aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))

(use-package multiple-cursors
  :bind (("C->" .           mc/mark-next-like-this)
         ("C-<" .           mc/mark-previous-like-this)
         ("C-c C-<" .       mc/mark-all-like-this)
         ("C-S-c C-S-c" .   mc/edit-lines)
         ("C-S-<mouse-1>" . mc/add-cursor-on-click)))

(use-package rg
  :straight (:type git :host github :repo "dajva/rg.el")
  ;; :custom
  :config (rg-enable-default-bindings))

(use-package expand-region
  :bind ("C-=" . er/expand-region))

(use-package pulsar
  :bind
  (:map global-map
        ("C-x l" . pulsar-pulse-line) ; overrides `count-lines-page'
        ("C-x L" . pulsar-highlight-permanently-dwim)) ; or use `pulsar-highlight-temporarily-dwim'
  :init
  (pulsar-global-mode 1)
  :config
  (setq pulsar-delay 0.055)
  (setq pulsar-iterations 5)
  (setq pulsar-face 'pulsar-green)
  (setq pulsar-region-face 'pulsar-yellow)
  (setq pulsar-highlight-face 'pulsar-magenta))

(editorconfig-mode t)

;;; ------------------------------------------------------------
;;; WoMan - Man page browser
;;; ------------------------------------------------------------
(use-package woman
  :straight (:type built-in)
  :bind (("C-c m" . woman))
  :init
  ;; Dynamically build man page paths by checking which directories exist
  (setq woman-manpath
        (seq-filter #'file-directory-p
                    '("C:/msys64/usr/share/man"
                      "C:/msys64/mingw64/share/man"
                      "C:/msys64/mingw32/share/man")))
  :custom
  ;; Cache man pages for faster access (important with large man page collections)
  (woman-cache-filename (expand-file-name "woman-cache.el" user-emacs-directory))
  ;; Don't ask which topic when there's only one match
  (woman-use-topic-at-point t)
  ;; Fill column for better readability
  (woman-fill-column 80)
  ;; Use own frame or window
  (woman-use-own-frame nil)
  :config
  ;; Build cache on first run - can take a moment but speeds up future lookups
  (unless (file-exists-p woman-cache-filename)
    (woman-file-name "")))

;;; ------------------------------------------------------------
;;; Custom settings file
;;; ------------------------------------------------------------
(setq custom-file (locate-user-emacs-file "custom-vars.el"))
(load custom-file 'noerror 'nomessage)

(setq next-line-add-newlines t)

(let ((autosave-dir (expand-file-name "autosave/" user-emacs-directory)))
  (setq auto-save-list-file-prefix autosave-dir)
  (setq auto-save-file-name-transforms
        `((".*" ,autosave-dir t)))
  )

(provide 'init)
;;; init.el ends here
