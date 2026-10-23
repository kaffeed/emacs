;;; init-org.el --- Org mode, journal, pomodoro and org-roam -*- lexical-binding: t -*-

;; Author: s.schubert
;; Description: Streamlined org-mode setup for a developer in a Scrum team.
;;              Focuses on essential task management, PR tracking, and
;;              Azure DevOps integration without administrative overhead.

;;; Commentary:

;; Simplified workflow focusing on:
;; - Single TODO sequence (TODO -> NEXT -> IN-PROGRESS -> REVIEW -> WAITING -> DONE)
;; - Two trees with the same layout (inbox, tasks, journal, roam):
;;     ~/org/       private git repo, remote on private GitHub
;;     ~/org/work/  work git repo, ignored by ~/org, remote on the company
;;                  server only.  Never push it anywhere private.
;;   `ss/org-sync' syncs whichever of the two exist on this machine.
;; - Developer-centric tags and properties
;; - Clean dashboard agenda

;;; Code:

;;; ------------------------------------------------------------
;;; Helper Functions
;;; ------------------------------------------------------------

(defun ss/org-mode-visual-setup ()
  "Configure visual settings for org-mode buffers."
  (auto-fill-mode 0)
  (visual-line-mode 1))

(defun ss/org-clock-in-state (state)
  "Switch to IN-PROGRESS on clock-in, but only for headings that are tasks.
Without this, clocking into a meeting or log entry turns it into a task."
  (when state "IN-PROGRESS"))

(defun ss/org-git (dir &rest args)
  "Run git with ARGS in DIR, log to *org-sync*, return the exit code."
  (with-current-buffer (get-buffer-create "*org-sync*")
    (let ((default-directory dir))
      (goto-char (point-max))
      (insert "\n$ git " (string-join args " ") "  (" dir ")\n")
      (apply #'call-process "git" nil t nil args))))

(defun ss/org-sync ()
  "Commit, pull and push each org repo present on this machine.
Each repo only talks to its own remote; a repo without one is just committed."
  (interactive)
  (save-some-buffers t (lambda () (and buffer-file-name
                                       (file-in-directory-p buffer-file-name "~/org/"))))
  (let (failed)
    (dolist (dir '("~/org/" "~/org/work/"))
      (when (file-directory-p (expand-file-name ".git" dir))
        (ss/org-git dir "add" "-A")
        (ss/org-git dir "commit" "-q" "-m" (format "sync from %s" (system-name)))
        (when (and (let ((default-directory dir)) (process-lines "git" "remote"))
                   (not (and (zerop (ss/org-git dir "pull" "--rebase" "-q"))
                             (zerop (ss/org-git dir "push" "-q")))))
          (push dir failed))))
    (if (not failed)
        (message "org synced")
      (pop-to-buffer "*org-sync*")
      (user-error "Org sync failed for %s, see *org-sync*" (string-join failed ", ")))))

;;; ------------------------------------------------------------
;;; Org-Mode Package Configuration
;;; ------------------------------------------------------------

(use-package org
  :hook (org-mode . ss/org-mode-visual-setup)
  :bind (;; Global org shortcuts
         ("C-c a" . org-agenda)
         ("C-c l" . org-store-link)
         ("C-c c" . org-capture)
         ("C-c n s" . ss/org-sync)
         ;; Everything else in org-mode-map uses org's defaults
         :map org-mode-map
         ("C-c C-a" . org-archive-subtree))

  :custom
  ;; Set default directory for org files
  (org-directory "~/org")

  ;; Every .org file in both trees; work/ is absent on private machines
  (org-agenda-files (seq-filter #'file-directory-p '("~/org/" "~/org/work/")))

  ;; Agenda settings
  (org-agenda-start-with-log-mode t)
  (org-log-done 'time)
  (org-log-into-drawer t)

  ;; Streamlined Developer Workflow
  (org-todo-keywords
   '((sequence "TODO(t)" "NEXT(n)" "IN-PROGRESS(i)" "REVIEW(r)" "WAITING(w@/!)" "|" "DONE(d!)" "CANCELED(c@)")))

  ;; Properties for effort tracking and Azure DevOps integration
  (org-global-properties
   '(("Effort_ALL" . "0:15 0:30 1:00 2:00 4:00 8:00")
     ("STORY_POINTS_ALL" . "1 2 3 5 8 13")
     ("DEVOPS_ID_ALL" . "")))

  ;; Column view for comprehensive tracking
  (org-columns-default-format
   "%50ITEM(Task) %TODO %PRIORITY %10Effort(Effort){:} %10CLOCKSUM(Clocked) %6STORY_POINTS(SP) %12DEVOPS_ID(Azure) %TAGS")

  ;; Automatically change to IN-PROGRESS when clocking in
  (org-clock-in-switch-to-state #'ss/org-clock-in-state)

  ;; Clock persistence - remember running clocks across Emacs sessions
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

  ;; Developer-focused tags
  (org-tag-alist
   '(("@dev" . ?d)
     ("frontend" . ?f)
     ("backend" . ?b)
     ("infra" . ?i)
     ("bug" . ?B)
     ("meeting" . ?m)
     ("review" . ?r)
     ("blocked" . ?x)
     ("quick" . ?q)))

  ;; Refile targets
  (org-refile-targets '((nil :maxlevel . 3)
                        (org-agenda-files :maxlevel . 2)))

  (org-refile-use-outline-path 'file)
  (org-outline-path-complete-in-steps nil)
  (org-refile-allow-creating-parent-nodes 'confirm)

  ;; Habit tracking
  (org-habit-graph-column 60)

  :config
  ;; Save org buffers after refiling
  (advice-add 'org-refile :after 'org-save-all-org-buffers)

  ;; Load org-habit module
  (require 'org-habit)

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

  ;; Streamlined Dashboard
  (setq org-agenda-custom-commands
        '(("d" "Developer Dashboard"
           ((agenda "" ((org-deadline-warning-days 7)
                        (org-agenda-span 1)
                        (org-agenda-overriding-header "Today's Schedule")))
            (todo "IN-PROGRESS"
                  ((org-agenda-overriding-header "Currently Working On")))
            (todo "REVIEW"
                  ((org-agenda-overriding-header "In Review / PR Open")))
            (todo "WAITING"
                  ((org-agenda-overriding-header "Blocked / Waiting")))
            (todo "NEXT"
                  ((org-agenda-overriding-header "Next Actions (Ready to pick up)")
                   (org-agenda-sorting-strategy '(priority-down effort-up))))
            (tags "LEVEL=1"
                  ((org-agenda-overriding-header "Inbox (Needs Processing)")
                   (org-agenda-files (seq-filter #'file-exists-p
                                                 '("~/org/inbox.org" "~/org/work/inbox.org")))))))))

  ;; Capture: lowercase keys are private, the "w" prefix is work
  (setq org-capture-templates
        '(("i" "Inbox task" entry (file "~/org/inbox.org")
           "* TODO %?\n:PROPERTIES:\n:CREATED: %U\n:END:\n" :empty-lines 1)

          ("n" "Inbox note" entry (file "~/org/inbox.org")
           "* %?\n:PROPERTIES:\n:CREATED: %U\n:END:\n" :empty-lines 1)

          ("j" "Journal" entry (file+olp+datetree "~/org/journal.org")
           "* %<%H:%M> %?\n" :empty-lines 1)

          ("w" "Work")

          ("wi" "Inbox task" entry (file "~/org/work/inbox.org")
           "* TODO %?\n:PROPERTIES:\n:CREATED: %U\n:END:\n" :empty-lines 1)

          ("wn" "Inbox note" entry (file "~/org/work/inbox.org")
           "* %?\n:PROPERTIES:\n:CREATED: %U\n:END:\n" :empty-lines 1)

          ("wt" "Task" entry (file "~/org/work/work.org")
           "* TODO [#B] %?\n:PROPERTIES:\n:CREATED: %U\n:DEVOPS_ID: \n:EFFORT: 2:00\n:END:\n%a\n" :empty-lines 1)

          ("wb" "Bug" entry (file "~/org/work/work.org")
           "* TODO [#A] Bug: %? :bug:\n:PROPERTIES:\n:CREATED: %U\n:DEVOPS_ID: \n:END:\n** Reproduction Steps\n1. \n\n** Expected Behavior\n\n** Actual Behavior\n" :empty-lines 1)

          ("wm" "Meeting" entry (file+olp+datetree "~/org/work/journal.org")
           "* %<%H:%M> Meeting: %^{Meeting Title} :meeting:\n** Notes\n%?\n** Action Items\n- [ ] \n"
           :clock-in t :clock-resume t :empty-lines 1)

          ("wj" "Log / time track" entry (file+olp+datetree "~/org/work/journal.org")
           "* %<%H:%M> %? :journal:\n"
           :clock-in t :clock-resume t :empty-lines 1))))

;;; ------------------------------------------------------------
;;; Org-Pomodoro
;;; ------------------------------------------------------------
(use-package org-pomodoro
  :after org
  :bind ("C-c P" . org-pomodoro)
  :custom
  (org-pomodoro-length 25)
  (org-pomodoro-short-break-length 5)
  (org-pomodoro-long-break-length 15)
  (org-pomodoro-keep-killed-pomodoro-time t))

;;; ------------------------------------------------------------
;;; Org-roam
;;; ------------------------------------------------------------
(use-package org-roam
  :custom
  ;; One graph over both trees; links into work/ break on private machines
  (org-roam-directory "~/org/")
  (org-roam-completion-everywhere t)
  (org-roam-capture-templates
   '(("p" "private" plain "%?"
      :target (file+head "roam/%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
      :unnarrowed t)
     ("w" "work" plain "%?"
      :target (file+head "work/roam/%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
      :unnarrowed t)))
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n c" . org-roam-capture)
         ("C-c n g" . org-roam-graph))
  :config
  (org-roam-db-autosync-mode))

(use-package org-roam-ui
  :bind (("C-c n u" . org-roam-ui-mode))
  :custom
  (org-roam-ui-sync-theme t)
  (org-roam-ui-follow t)
  (org-roam-ui-update-on-save t)
  (org-roam-ui-open-on-start t))

;;; ------------------------------------------------------------
;;; Mermaid in org-babel
;;; ------------------------------------------------------------
(use-package ob-mermaid
  :after org
  :config
  (setq ob-mermaid-cli-path "mmdc")
  (add-to-list 'org-babel-load-languages '(mermaid . t) t)
  (org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages))

(provide 'init-org)
;;; init-org.el ends here
