;;; org-sync-test.el --- Tests for ss/org-sync -*- lexical-binding: t -*-

;; Run: emacs -Q --batch -l test/org-sync-test.el -f ert-run-tests-batch-and-exit
;; Uses throwaway repos under a temporary HOME; the real ~/org is never touched.

;;; Code:

(require 'ert)

;; Load only the sync functions: the use-package forms need straight.el
(defmacro use-package (&rest _) nil)
(load (expand-file-name "../modules/init-org.el"
                        (file-name-directory (or load-file-name buffer-file-name)))
      nil 'nomessage)

(defun org-sync-test--git (dir &rest args)
  "Run git with ARGS in DIR, fail the test on a non-zero exit."
  (let ((default-directory dir))
    (unless (zerop (apply #'call-process "git" nil nil nil args))
      (error "git %s failed in %s" (string-join args " ") dir))))

(defun org-sync-test--write (file text)
  (with-temp-file file (insert text)))

(ert-deftest org-sync-conflict-leaves-no-rebase ()
  "A conflicting pull reports failure and leaves the repo out of a rebase."
  (let* ((tmp (make-temp-file "org-sync-" t))
         (home (expand-file-name "home/" tmp))
         (org (expand-file-name "org/" home))
         (other (expand-file-name "other/" tmp))
         (remote (expand-file-name "remote.git" tmp))
         (process-environment
          (append (list (concat "HOME=" home)
                        "GIT_CONFIG_NOSYSTEM=1"
                        "GIT_AUTHOR_NAME=t" "GIT_AUTHOR_EMAIL=t@t"
                        "GIT_COMMITTER_NAME=t" "GIT_COMMITTER_EMAIL=t@t")
                  process-environment)))
    (unwind-protect
        (progn
          (make-directory home t)
          (org-sync-test--git tmp "init" "-q" "--bare" "-b" "main" remote)
          (org-sync-test--git tmp "clone" "-q" remote other)
          (org-sync-test--write (expand-file-name "note.org" other) "* base\n")
          (org-sync-test--git other "add" "-A")
          (org-sync-test--git other "commit" "-q" "-m" "base")
          (org-sync-test--git other "push" "-q" "origin" "HEAD:main")
          (org-sync-test--git home "clone" "-q" remote org)
          ;; The same line changed on both machines
          (org-sync-test--write (expand-file-name "note.org" other) "* other\n")
          (org-sync-test--git other "commit" "-q" "-am" "other")
          (org-sync-test--git other "push" "-q")
          (org-sync-test--write (expand-file-name "note.org" org) "* mine\n")
          (should-error (ss/org-sync) :type 'user-error)
          (should-not (file-exists-p (expand-file-name ".git/rebase-merge" org)))
          (should-not (file-exists-p (expand-file-name ".git/rebase-apply" org)))
          (should (equal (with-temp-buffer
                           (insert-file-contents (expand-file-name "note.org" org))
                           (buffer-string))
                         "* mine\n")))
      (delete-directory tmp t))))

(provide 'org-sync-test)
;;; org-sync-test.el ends here
