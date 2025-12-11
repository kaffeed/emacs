;;; org-config.el --- Org-mode configuration -*- lexical-binding: t -*-

;; Author: s.schubert
;; Description: Comprehensive org-mode setup for task management,
;;              project planning, and agile workflow integration.
;;              Includes custom agenda views, capture templates, and
;;              Azure DevOps integration.

;;; Commentary:

;; This configuration provides a complete org-mode workflow system with:
;; - Task management with multiple TODO states
;; - Agile/Scrum workflow support (user stories, sprints, backlog)
;; - Custom agenda views for different perspectives (PO, Dev, Sprint)
;; - Rich capture templates for various task types
;; - Time tracking and effort estimation
;; - Azure DevOps integration

;;; Code:

;;; ------------------------------------------------------------
;;; Constants
;;; ------------------------------------------------------------

;; Org file paths used throughout configuration
;; These constants eliminate repetition and make path updates easier
(defconst ss/org-user-story-files '("~/org/userstories.org" "~/org/todo.org")
  "Files containing user stories and development tasks.")

(defconst ss/org-backlog-files '("~/org/userstories.org" "~/org/backlog.org")
  "Files containing backlog items.")

(defconst ss/org-sprint-files '("~/org/sprints.org")
  "Files containing sprint planning and ceremonies.")

(defconst ss/org-meeting-files '("~/org/recurring.org" "~/org/meetings.org" "~/org/sprints.org")
  "Files containing meetings and recurring activities.")

;;; ------------------------------------------------------------
;;; Helper Functions
;;; ------------------------------------------------------------

(defun ss/org-mode-visual-setup ()
  "Configure visual settings for org-mode buffers.

Enables:
- org-indent-mode: Visual indentation without changing file content
- variable-pitch-mode: Proportional fonts for better readability
- visual-line-mode: Soft line wrapping without breaking words

Disables:
- auto-fill-mode: Prevents hard wrapping which conflicts with visual-line-mode"
  (org-indent-mode)
  (variable-pitch-mode 1)
  (auto-fill-mode 0)
  (visual-line-mode 1))

;;; ------------------------------------------------------------
;;; Org-Mode Package Configuration
;;; ------------------------------------------------------------

(use-package org
  :hook (org-mode . ss/org-mode-visual-setup)
  :bind (;; Global org shortcuts
         ("C-c a" . org-agenda)
         ("C-c n" . org-capture)
         ("C-c l" . org-store-link)
         ;;("C-c j" . org-journal-new-entry)

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

  ;; TODO keywords define the workflow states
  ;; First sequence: General task workflow
  ;; Second sequence: Agile/Scrum workflow for user stories and sprint items
  (org-todo-keywords
   '((sequence "TODO(t)" "NEXT(n)" "STARTED(s)" "|" "DONE(d!)" "DELEGATED(g)")
     (sequence "BACKLOG(b)" "REFINED(f)" "READY(r)" "ACTIVE(a)" "REVIEW(v)" "TESTING(e)" "WAIT(w@/!)" "HOLD(h)" "|" "COMPLETED(c!)" "CANC(k@)")))

  ;; Properties for effort tracking and Azure DevOps integration
  (org-global-properties
   '(("Effort_ALL" . "0:15 0:30 1:00 2:00 3:00 4:00 8:00 16:00 24:00 40:00")
     ("STORY_POINTS_ALL" . "1 2 3 5 8 13 21")       ;; Fibonacci sequence for story points
     ("DEVOPS_ID_ALL" . "")                          ;; Azure DevOps work item ID
     ("SPRINT_ALL" . "")                             ;; Sprint identifier
     ("ATTENDEES_ALL" . "")))                        ;; Meeting attendees

  ;; Column view for comprehensive tracking
  ;; Format: %WIDTH(PROPERTY) where:
  ;; - ITEM: Task title
  ;; - TODO: Current state
  ;; - PRIORITY: A/B/C priority level
  ;; - Effort: Estimated time
  ;; - CLOCKSUM: Actual time clocked
  ;; - STORY_POINTS: Agile story point estimate
  ;; - DEVOPS_ID: Azure DevOps work item ID
  ;; - TAGS: Task tags
  (org-columns-default-format
   "%50ITEM(Task) %TODO %PRIORITY %10Effort(Effort){:} %10CLOCKSUM(Clocked) %6STORY_POINTS(SP) %12DEVOPS_ID(Azure) %TAGS")

  ;; Automatically change to STARTED when clocking in
  (org-clock-in-switch-to-state "STARTED")

  ;; Clock persistence - remember running clocks across Emacs sessions
  (org-clock-persist t)
  (org-clock-mode-line-total 'current)

  ;; Priority system (A = highest, C = lowest)
  (org-priority-highest ?A)
  (org-priority-lowest ?C)
  (org-priority-default ?B)
  (org-priority-faces
   '((?A . (:foreground "red" :weight bold))
     (?B . (:foreground "yellow"))
     (?C . (:foreground "green"))))

  ;; Enhanced tagging system
  ;; Tags are organized into groups for:
  ;; - Context: @po (Product Owner), @dev (Developer)
  ;; - Activities: planning, grooming, review, retro, meeting, 1on1, stakeholder
  ;; - Products: p1, cc (ComCenter), perdis, backoffice, shared
  ;; - Technical: frontend, backend, infra, testing, bug, design
  ;; - Priority/Status: quick, blocked, urgent, habit
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

  ;; Refile targets - where tasks can be moved to
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
        '(;; ========================================
          ;; MAIN DASHBOARD
          ;; ========================================
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

          ;; ========================================
          ;; SPRINT VIEWS
          ;; ========================================
          ("s" "Sprint Views")

          ;; Current Sprint - Active work in the current sprint
          ("ss" "Current Sprint"
           ((tags-todo "ACTIVE|READY|STARTED|REVIEW|TESTING"
                       ((org-agenda-overriding-header "Sprint Backlog")
                        (org-agenda-files ss/org-user-story-files)
                        (org-agenda-sorting-strategy '(priority-down))))
            (tags "planning|review"
                  ((org-agenda-overriding-header "Sprint Events")
                   (org-agenda-files ss/org-sprint-files)))))

          ;; Sprint Burndown - Track progress through sprint
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

          ;; ========================================
          ;; BACKLOG VIEWS
          ;; ========================================
          ("b" "Backlog Views")

          ;; Product Backlog - Prioritized list of upcoming work
          ("bb" "Product Backlog"
           ((tags-todo "TODO=\"BACKLOG\"+PRIORITY=\"A\""
                       ((org-agenda-overriding-header "High Priority Backlog")
                        (org-agenda-files ss/org-backlog-files)))
            (tags-todo "TODO=\"REFINED\""
                       ((org-agenda-overriding-header "Refined & Ready for Planning")))
            (tags-todo "TODO=\"BACKLOG\"+PRIORITY=\"B\""
                       ((org-agenda-overriding-header "Medium Priority Backlog")
                        (org-agenda-max-entries 15)))))

          ;; Grooming Candidates - Items needing refinement
          ("bg" "Grooming Candidates"
           ((tags-todo "TODO=\"BACKLOG\"-EFFORT"
                       ((org-agenda-overriding-header "Missing Effort Estimates")))
            (tags-todo "TODO=\"BACKLOG\"-PROPERTIES={:CREATED:}"
                       ((org-agenda-overriding-header "Needs Refinement")))
            ;; Items without any priority set (not A, B, or C)
            (tags-todo "TODO=\"BACKLOG\"+PRIORITY<>\"A\"+PRIORITY<>\"B\"+PRIORITY<>\"C\""
                       ((org-agenda-overriding-header "Needs Priority")))))

          ;; ========================================
          ;; WORKFLOW STATUS
          ;; ========================================
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

          ;; ========================================
          ;; PO-SPECIFIC VIEWS
          ;; ========================================
          ("p" "Product Owner Views")

          ;; Meetings & Planning - PO ceremonies and activities
          ("pm" "Meetings & Planning"
           ((tags "planning|grooming|review|retro|stakeholder|1on1"
                  ((org-agenda-overriding-header "PO Activities")
                   (org-agenda-files ss/org-meeting-files)
                   (org-agenda-span 14)))))

          ;; Product Areas - View work by product/component
          ("pa" "Product Areas"
           ((tags-todo "+backoffice"
                       ((org-agenda-overriding-header "Backoffice")
                        (org-agenda-files ss/org-user-story-files)))
            (tags-todo "+perdis"
                       ((org-agenda-overriding-header "Perdis")
                        (org-agenda-files ss/org-user-story-files)))
            (tags-todo "+cc"
                       ((org-agenda-overriding-header "ComCenter")
                        (org-agenda-files ss/org-user-story-files)))))

          ;; DevOps Integration - Items linked to Azure DevOps
          ("pd" "DevOps Integration"
           ((tags "DEVOPS_ID<>\"\""
                  ((org-agenda-overriding-header "Items with Azure DevOps ID")
                   (org-agenda-files ss/org-user-story-files)
                   (org-agenda-prefix-format "  %?-12t [%s] ")))))

          ;; ========================================
          ;; DEV-SPECIFIC VIEWS
          ;; ========================================
          ("v" "Developer Views")

          ;; Next Actions - What to work on next
          ("vn" "Next Actions"
           ((todo "NEXT"
                  ((org-agenda-overriding-header "Next Actions")
                   (org-agenda-sorting-strategy '(priority-down effort-up))))
            (tags "+quick"
                  ((org-agenda-overriding-header "Quick Wins")
                   (org-agenda-skip-function '(org-agenda-skip-entry-if 'done))))))

          ;; By Effort - Tasks grouped by estimated effort
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

          ;; By Technology - Tasks grouped by technical area
          ("vt" "By Technology"
           ((tags-todo "+frontend"
                       ((org-agenda-overriding-header "Frontend Work")))
            (tags-todo "+backend"
                       ((org-agenda-overriding-header "Backend Work")))
            (tags-todo "+infra"
                       ((org-agenda-overriding-header "Infrastructure")))))

          ;; ========================================
          ;; TIME-BASED VIEWS
          ;; ========================================
          ("t" "Time-based Views")

          ;; Today's Focus - What matters today
          ("tt" "Today's Focus"
           ((agenda "" ((org-agenda-span 1)
                        (org-agenda-overriding-header "Today")))
            (tags "PRIORITY=\"A\""
                  ((org-agenda-overriding-header "Must Do Today")
                   (org-agenda-skip-function '(org-agenda-skip-entry-if 'done))))
            (todo "STARTED"
                  ((org-agenda-overriding-header "Currently Working On")))))

          ;; This Week - Weekly overview
          ("tw" "This Week"
           ((agenda "" ((org-agenda-span 7)
                        (org-agenda-start-on-weekday 1)
                        (org-agenda-overriding-header "This Week")))
            (tags-todo "+SCHEDULED<\"<+7d>\""
                       ((org-agenda-overriding-header "Due This Week")))))

          ;; ========================================
          ;; REVIEW VIEWS
          ;; ========================================
          ("r" "Review Views")

          ;; Inbox Processing - Unprocessed items
          ("ri" "Inbox Processing"
           ((tags "LEVEL=2"
                  ((org-agenda-overriding-header "Inbox Items to Process")
                   (org-agenda-files '("~/org/inbox.org"))))))

          ;; Done Recently - Recently completed work
          ("rd" "Done Recently"
           ((tags "+TODO=\"DONE\"+CLOSED>=\"<-7d>\""
                  ((org-agenda-overriding-header "Completed Last 7 Days")))
            (tags "+TODO=\"COMPLETED\"+CLOSED>=\"<-14d>\""
                  ((org-agenda-overriding-header "Completed Last 14 Days")))))

          ;; Unscheduled Tasks - Tasks without a schedule
          ("ru" "Unscheduled Tasks"
           ((tags-todo "-SCHEDULED-DEADLINE"
                       ((org-agenda-overriding-header "Unscheduled Tasks")
                        (org-agenda-skip-function
                         '(org-agenda-skip-entry-if 'regexp "Inbox"))))))))

  ;;; ------------------------------------------------------------
  ;;; Capture Templates
  ;;; ------------------------------------------------------------

  ;; Capture templates for quickly creating different types of tasks
  ;; Templates are organized by workflow:
  ;; - Quick captures (inbox)
  ;; - Standard tasks (general TODOs)
  ;; - Projects & features
  ;; - Agile workflow (user stories, bugs, backlog)
  ;; - Sprint ceremonies
  ;; - Recurring PO activities
  ;; - Meetings
  ;; - Design/UX
  ;; - Journal entries
  ;; - Notes and someday items

  (setq org-capture-templates
        '(;; ========================================
          ;; QUICK CAPTURES
          ;; ========================================
          ("i" "Inbox - Quick Capture" entry (file "~/org/inbox.org")
           "* TODO %?\n:PROPERTIES:\n:CREATED: %U\n:END:\n" :empty-lines 1)

          ;; ========================================
          ;; TASKS
          ;; ========================================
          ("t" "Tasks")
          ("tt" "Task" entry (file "~/org/todo.org")
           "* TODO [#B] %?\n:PROPERTIES:\n:CREATED: %U\n:EFFORT: 1:00\n:END:\n%a\n" :empty-lines 1)
          ("tn" "Next Action" entry (file "~/org/todo.org")
           "* NEXT [#A] %?\n:PROPERTIES:\n:CREATED: %U\n:EFFORT: 1:00\n:END:\n%a\n" :empty-lines 1)
          ("td" "Dev Task" entry (file "~/org/todo.org")
           "* TODO [#B] %? :@dev:\n:PROPERTIES:\n:CREATED: %U\n:EFFORT: 2:00\n:END:\n%a\n** Technical Details\n\n** Acceptance Criteria\n- [ ] \n" :empty-lines 1)

          ;; ========================================
          ;; PROJECTS & FEATURES
          ;; ========================================
          ("p" "Projects")
          ("pp" "Project" entry (file "~/org/projects.org")
           "* BACKLOG [#B] %? :project:\n:PROPERTIES:\n:CREATED: %U\n:END:\n** Goal\n\n** Success Criteria\n\n** Tasks\n*** TODO Define scope\n*** TODO Create user stories\n*** TODO Plan implementation\n" :empty-lines 1)
          ("pf" "Feature" entry (file "~/org/projects.org")
           "* BACKLOG [#B] %? :feature:\n:PROPERTIES:\n:CREATED: %U\n:EFFORT: 8:00\n:END:\n** Description\n\n** User Value\n\n** Technical Approach\n" :empty-lines 1)

          ;; ========================================
          ;; USER STORIES (Agile Workflow)
          ;; ========================================
          ("u" "User Stories")
          ("us" "User Story" entry (file "~/org/userstories.org")
           "* BACKLOG [#B] %? :@po:\n:PROPERTIES:\n:CREATED: %U\n:DEVOPS_ID: \n:STORY_POINTS: 5\n:END:\n** User Story\nAls <Rolle> möchte ich <Funktion>, damit <Nutzen>.\n\n** Acceptance Criteria\n- [ ] \n- [ ] \n- [ ] \n\n** Technical Notes\n\n** Related Items\n" :empty-lines 1)
          ("ub" "Bug/Issue" entry (file "~/org/userstories.org")
           "* TODO [#A] Bug: %? :bug:\n:PROPERTIES:\n:CREATED: %U\n:DEVOPS_ID: \n:EFFORT: 2:00\n:END:\n** Problem Description\n\n** Steps to Reproduce\n1. \n\n** Expected Behavior\n\n** Actual Behavior\n\n** Fix Approach\n" :empty-lines 1)

          ;; ========================================
          ;; BACKLOG
          ;; ========================================
          ("b" "Backlog Item" entry (file "~/org/backlog.org")
           "* BACKLOG [#C] %? :backlog:\n:PROPERTIES:\n:CREATED: %U\n:END:\n** Description\n\n** Value\n\n** Effort Estimate\n" :empty-lines 1)

          ;; ========================================
          ;; SPRINT PLANNING (Scrum Ceremonies)
          ;; ========================================
          ("s" "Sprint Planning")
          ("sp" "Sprint Planning Session" entry (file "~/org/sprints.org")
           "* ACTIVE Sprint %^{Sprint Number} Planning :planning:@po:\nSCHEDULED: %^T\n:PROPERTIES:\n:CREATED: %U\n:SPRINT_START: %^{Start Date}T\n:SPRINT_END: %^{End Date}T\n:SPRINT_GOAL: %^{Sprint Goal}\n:END:\n** Sprint Goal\n%\\3\n\n** Committed Stories\n\n** Sprint Capacity\n- Team capacity: %^{Capacity in hours} hours\n- Story points committed: \n\n** Notes\n" :empty-lines 1)
          ("sr" "Sprint Review" entry (file "~/org/sprints.org")
           "* Sprint %^{Sprint Number} Review :review:@po:\nSCHEDULED: %^T\n:PROPERTIES:\n:CREATED: %U\n:END:\n** Completed Stories\n\n** Demo Items\n\n** Incomplete Items\n\n** Stakeholder Feedback\n\n** Action Items\n- [ ] \n" :empty-lines 1)
          ("st" "Sprint Retrospective" entry (file "~/org/sprints.org")
           "* Sprint %^{Sprint Number} Retrospective :retro:@po:\nSCHEDULED: %^T\n:PROPERTIES:\n:CREATED: %U\n:END:\n** What went well\n- \n\n** What could be improved\n- \n\n** Action Items\n- [ ] \n\n** Team Mood: %^{1-5}\n" :empty-lines 1)

          ;; ========================================
          ;; RECURRING PO ACTIVITIES
          ;; ========================================
          ("r" "Recurring Activities")
          ("rg" "Backlog Grooming" entry (file "~/org/recurring.org")
           "* TODO Backlog Grooming :grooming:@po:\nSCHEDULED: %^T\n:PROPERTIES:\n:CREATED: %U\n:EFFORT: 2:00\n:END:\n** Agenda\n- Review top backlog items\n- Refine acceptance criteria\n- Update priorities\n- Add effort estimates\n\n** Items Reviewed\n\n** Action Items\n- [ ] \n" :empty-lines 1)
          ("rs" "Stakeholder Meeting" entry (file "~/org/meetings.org")
           "* TODO Meeting: %^{Stakeholder} :stakeholder:meeting:@po:\nSCHEDULED: %^T\n:PROPERTIES:\n:CREATED: %U\n:EFFORT: 1:00\n:ATTENDEES: %^{Attendees}\n:END:\n** Agenda\n- \n\n** Discussion Points\n\n** Decisions\n\n** Action Items\n- [ ] \n" :empty-lines 1)
          ("r1" "1-on-1 Meeting" entry (file "~/org/meetings.org")
           "* TODO 1-on-1: %^{Person} :1on1:meeting:@po:\nSCHEDULED: %^T\n:PROPERTIES:\n:CREATED: %U\n:EFFORT: 0:30\n:END:\n** Topics\n- \n\n** Discussion\n\n** Action Items\n- [ ] \n\n** Next Meeting Topics\n- \n" :empty-lines 1)
          ("rt" "Team Sync" entry (file "~/org/meetings.org")
           "* TODO Team Sync :meeting:@po:\nSCHEDULED: %^T\n:PROPERTIES:\n:CREATED: %U\n:EFFORT: 0:30\n:END:\n** Blockers\n\n** Updates\n\n** Upcoming Work\n\n** Action Items\n- [ ] \n" :empty-lines 1)

          ;; ========================================
          ;; UX/DESIGN
          ;; ========================================
          ("x" "UX/Design Item" entry (file "~/org/projects.org")
           "* TODO [#B] %? :design:\n:PROPERTIES:\n:CREATED: %U\n:END:\n** Design Requirements\n\n** Figma Links\n\n** Decision\n" :empty-lines 1)

          ;; ========================================
          ;; JOURNAL
          ;; ========================================
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

          ;; ========================================
          ;; NOTES
          ;; ========================================
          ("n" "Note" entry (file "~/org/notes.org")
           "* %? :note:\n:PROPERTIES:\n:CREATED: %U\n:END:\n" :empty-lines 1)

          ;; ========================================
          ;; SOMEDAY/MAYBE
          ;; ========================================
          ("y" "Someday/Maybe" entry (file "~/org/someday.org")
           "* BACKLOG [#C] %?\n:PROPERTIES:\n:CREATED: %U\n:END:\n" :empty-lines 1))))

;;; ------------------------------------------------------------
;;; Org-Journal Configuration
;;; ------------------------------------------------------------

(use-package org-journal
  :after org
  :bind ("C-c j" . org-journal-new-entry)
  :custom
  (org-journal-dir "~/org/")
  (org-journal-file-type 'yearly)
  (org-journal-file-format "journal.org")
  (org-journal-date-format "%A, %d %B %Y")
  (org-journal-enable-agenda-integration t))

(provide 'org-config)
;;; org-config.el ends here
