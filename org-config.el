;;; org-config.el --- Org-mode configuration -*- lexical-binding: t -*-

;; Author: s.schubert
;; Description: Streamlined org-mode setup for a developer in a Scrum team.
;;              Focuses on essential task management, PR tracking, and
;;              Azure DevOps integration without administrative overhead.

;;; Commentary:

;; Simplified workflow focusing on:
;; - Single TODO sequence (TODO -> NEXT -> IN-PROGRESS -> REVIEW -> WAITING -> DONE)
;; - Consolidated files (work, personal, inbox, journal)
;; - Developer-centric tags and properties
;; - Clean dashboard agenda

;;; Code:

;;; ------------------------------------------------------------
;;; Helper Functions
;;; ------------------------------------------------------------

(defun ss/org-mode-visual-setup ()
  "Configure visual settings for org-mode buffers."
  (variable-pitch-mode 1)
  (auto-fill-mode 0)
  (visual-line-mode 1))

(defun ss/org-journal-find-location ()
  "Find or create journal entry for today's date."
  (org-journal-new-entry t)
  (unless (eq org-journal-file-type 'daily)
    (org-narrow-to-subtree))
  (goto-char (point-max)))

;;; ------------------------------------------------------------
;;; Org-Mode Package Configuration
;;; ------------------------------------------------------------

(use-package org
  :hook (org-mode . ss/org-mode-visual-setup)
  :bind (;; Global org shortcuts
         ("C-c a" . org-agenda)
         ("C-c l" . org-store-link)
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
                      "~/org/work.org"
                      "~/org/personal.org"
                      "~/org/journal.org"))

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
  (org-clock-in-switch-to-state "IN-PROGRESS")

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
                        (org-agenda-files :maxlevel . 2)
                        ("~/org/archive.org" :maxlevel . 1)))

  (org-refile-use-outline-path 'file)
  (org-outline-path-complete-in-steps nil)
  (org-refile-allow-creating-parent-nodes 'confirm)

  ;; Habit tracking
  (org-modules '(org-habit))
  (org-habit-graph-column 60)

  :config
  ;; Save org buffers after refiling
  (advice-add 'org-refile :after 'org-save-all-org-buffers)

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
            (tags "LEVEL=2"
                  ((org-agenda-overriding-header "Inbox (Needs Processing)")
                   (org-agenda-files '("~/org/inbox.org"))))))))

  ;; Simplified Capture Templates
  (setq org-capture-templates
        '(("i" "Inbox - Quick Capture" entry (file "~/org/inbox.org")
           "* TODO %?\n:PROPERTIES:\n:CREATED: %U\n:END:\n" :empty-lines 1)
          
          ("t" "Work Task" entry (file "~/org/work.org")
           "* TODO [#B] %?\n:PROPERTIES:\n:CREATED: %U\n:DEVOPS_ID: \n:EFFORT: 2:00\n:END:\n%a\n" :empty-lines 1)
          
          ("b" "Bug" entry (file "~/org/work.org")
           "* TODO [#A] Bug: %? :bug:\n:PROPERTIES:\n:CREATED: %U\n:DEVOPS_ID: \n:END:\n** Reproduction Steps\n1. \n\n** Expected Behavior\n\n** Actual Behavior\n" :empty-lines 1)
          
          ("m" "Meeting Notes" plain (function ss/org-journal-find-location)
           "** %(format-time-string \"%H:%M\") - Meeting: %^{Meeting Title} :meeting:\n*** Notes\n%?\n*** Action Items\n- [ ] \n"
           :clock-in :clock-resume :empty-lines 1)
          
          ("j" "Journal / Time Track" plain (function ss/org-journal-find-location)
           "** %(format-time-string \"%H:%M\") - %? :journal:\n"
           :clock-in :clock-resume :empty-lines 1))))

;;; ------------------------------------------------------------
;;; Org-Super-Agenda
;;; ------------------------------------------------------------
(use-package org-super-agenda
  :after org-agenda
  :config
  (org-super-agenda-mode))

;;; ------------------------------------------------------------
;;; Org-Pomodoro
;;; ------------------------------------------------------------
(use-package org-pomodoro
  :after org
  :bind ("C-c C-x p" . org-pomodoro)
  :custom
  (org-pomodoro-length 25)
  (org-pomodoro-short-break-length 5)
  (org-pomodoro-long-break-length 15)
  (org-pomodoro-keep-killed-pomodoro-time t))

;;; ------------------------------------------------------------
;;; Org-Journal
;;; ------------------------------------------------------------
(use-package org-journal
  :ensure t
  :after org
  :bind ("C-c j" . org-journal-new-entry)
  :custom
  (org-journal-dir "~/org/")
  (org-journal-file-type 'monthly)
  (org-journal-file-format "%Y-%m.org")
  (org-journal-date-format "%A, %d %B %Y")
  (org-journal-enable-agenda-integration t)
  (org-journal-hide-entries-p nil))

(provide 'org-config)
;;; org-config.el ends here