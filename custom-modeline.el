;;; custom-modeline.el --- Custom mode line based on prot-modeline -*- lexical-binding: t -*-

;; Adapted from Protesilaos Stavrou's prot-modeline.el

;;; Code:

(require 'nerd-icons)

(defgroup custom-modeline nil
  "Custom modeline that is stylistically close to the default."
  :group 'mode-line)

(defgroup custom-modeline-faces nil
  "Faces for my custom modeline."
  :group 'custom-modeline)

(defcustom custom-modeline-string-truncate-length 9
  "String length after which truncation should be done in small windows."
  :type 'natnum)

;;;; Faces

(defface custom-modeline-indicator-button nil
  "Generic face used for indicators that have a background.")

(defface custom-modeline-indicator-small
  '((t :height 0.8))
  "Face for modeline indicators."
  :group 'custom-modeline-faces)

(defface custom-modeline-indicator-red
  '((default :inherit bold)
    (((class color) (min-colors 88) (background light))
     :foreground "#880000")
    (((class color) (min-colors 88) (background dark))
     :foreground "#ff9f9f")
    (t :foreground "red"))
  "Face for modeline indicators."
  :group 'custom-modeline-faces)

(defface custom-modeline-indicator-red-bg
  '((default :inherit (bold custom-modeline-indicator-button))
    (((class color) (min-colors 88) (background light))
     :background "#aa1111" :foreground "white")
    (((class color) (min-colors 88) (background dark))
     :background "#ff9090" :foreground "black")
    (t :background "red" :foreground "black"))
  "Face for modeline indicators with a background."
  :group 'custom-modeline-faces)

(defface custom-modeline-indicator-green
  '((default :inherit bold)
    (((class color) (min-colors 88) (background light))
     :foreground "#005f00")
    (((class color) (min-colors 88) (background dark))
     :foreground "#73fa7f")
    (t :foreground "green"))
  "Face for modeline indicators."
  :group 'custom-modeline-faces)

(defface custom-modeline-indicator-green-bg
  '((default :inherit (bold custom-modeline-indicator-button))
    (((class color) (min-colors 88) (background light))
     :background "#207b20" :foreground "white")
    (((class color) (min-colors 88) (background dark))
     :background "#77d077" :foreground "black")
    (t :background "green" :foreground "black"))
  "Face for modeline indicators with a background."
  :group 'custom-modeline-faces)

(defface custom-modeline-indicator-yellow
  '((default :inherit bold)
    (((class color) (min-colors 88) (background light))
     :foreground "#6f4000")
    (((class color) (min-colors 88) (background dark))
     :foreground "#f0c526")
    (t :foreground "yellow"))
  "Face for modeline indicators."
  :group 'custom-modeline-faces)

(defface custom-modeline-indicator-yellow-bg
  '((default :inherit (bold custom-modeline-indicator-button))
    (((class color) (min-colors 88) (background light))
     :background "#805000" :foreground "white")
    (((class color) (min-colors 88) (background dark))
     :background "#ffc800" :foreground "black")
    (t :background "yellow" :foreground "black"))
  "Face for modeline indicators with a background."
  :group 'custom-modeline-faces)

(defface custom-modeline-indicator-blue
  '((default :inherit bold)
    (((class color) (min-colors 88) (background light))
     :foreground "#00228a")
    (((class color) (min-colors 88) (background dark))
     :foreground "#88bfff")
    (t :foreground "blue"))
  "Face for modeline indicators."
  :group 'custom-modeline-faces)

(defface custom-modeline-indicator-blue-bg
  '((default :inherit (bold custom-modeline-indicator-button))
    (((class color) (min-colors 88) (background light))
     :background "#0000aa" :foreground "white")
    (((class color) (min-colors 88) (background dark))
     :background "#77aaff" :foreground "black")
    (t :background "blue" :foreground "black"))
  "Face for modeline indicators with a background."
  :group 'custom-modeline-faces)

(defface custom-modeline-indicator-magenta
  '((default :inherit bold)
    (((class color) (min-colors 88) (background light))
     :foreground "#6a1aaf")
    (((class color) (min-colors 88) (background dark))
     :foreground "#e0a0ff")
    (t :foreground "magenta"))
  "Face for modeline indicators."
  :group 'custom-modeline-faces)

(defface custom-modeline-indicator-magenta-bg
  '((default :inherit (bold custom-modeline-indicator-button))
    (((class color) (min-colors 88) (background light))
     :background "#6f0f9f" :foreground "white")
    (((class color) (min-colors 88) (background dark))
     :background "#e3a2ff" :foreground "black")
    (t :background "magenta" :foreground "black"))
  "Face for modeline indicators with a background."
  :group 'custom-modeline-faces)

(defface custom-modeline-indicator-cyan
  '((default :inherit bold)
    (((class color) (min-colors 88) (background light))
     :foreground "#004060")
    (((class color) (min-colors 88) (background dark))
     :foreground "#30b7cc")
    (t :foreground "cyan"))
  "Face for modeline indicators."
  :group 'custom-modeline-faces)

(defface custom-modeline-indicator-cyan-bg
  '((default :inherit (bold custom-modeline-indicator-button))
    (((class color) (min-colors 88) (background light))
     :background "#006080" :foreground "white")
    (((class color) (min-colors 88) (background dark))
     :background "#40c0e0" :foreground "black")
    (t :background "cyan" :foreground "black"))
  "Face for modeline indicators with a background."
  :group 'custom-modeline-faces)

(defface custom-modeline-indicator-gray
  '((t :inherit (bold shadow)))
  "Face for modeline indicators."
  :group 'custom-modeline-faces)

(defface custom-modeline-indicator-gray-bg
  '((default :inherit (bold custom-modeline-indicator-button))
    (((class color) (min-colors 88) (background light))
     :background "#808080" :foreground "white")
    (((class color) (min-colors 88) (background dark))
     :background "#a0a0a0" :foreground "black")
    (t :inverse-video t))
  "Face for modeline indicators with a background."
  :group 'custom-modeline-faces)

;;;; Common helper functions

(defun custom-modeline-window-narrow-p ()
  "Return non-nil if window is narrow."
  (< (window-total-width) split-width-threshold))

(defun custom-modeline--string-truncate-p (str)
  "Return non-nil if STR should be truncated."
  (cond
   ((or (not (stringp str))
        (string-empty-p str)
        (string-blank-p str))
    nil)
   ((and (custom-modeline-window-narrow-p)
         (> (length str) custom-modeline-string-truncate-length)
         (not (one-window-p :no-minibuffer))))))

(defun custom-modeline--truncate-p ()
  "Return non-nil if truncation should happen."
  (and (custom-modeline-window-narrow-p)
       (not (one-window-p :no-minibuffer))))

(defun custom-modeline-string-cut-end (str)
  "Return truncated STR, if appropriate, else return STR."
  (if (custom-modeline--string-truncate-p str)
      (concat (substring str 0 custom-modeline-string-truncate-length) "...")
    str))

(defun custom-modeline-string-cut-middle (str)
  "Return truncated STR, if appropriate, else return STR."
  (let ((half (floor custom-modeline-string-truncate-length 2)))
    (if (custom-modeline--string-truncate-p str)
        (concat (substring str 0 half) "..." (substring str (- half)))
      str)))

(defun custom-modeline--first-char (str)
  "Return first character from STR."
  (substring str 0 1))

(defun custom-modeline-string-abbreviate-but-last (str nthlast)
  "Abbreviate STR, keeping NTHLAST words intact."
  (if (custom-modeline--string-truncate-p str)
      (let* ((all-strings (split-string str "[_-]"))
             (nbutlast-strings (nbutlast (copy-sequence all-strings) nthlast))
             (last-strings (nreverse (ntake nthlast (nreverse (copy-sequence all-strings)))))
             (first-component (mapconcat #'custom-modeline--first-char nbutlast-strings "-"))
             (last-component (mapconcat #'identity last-strings "-")))
        (if (string-empty-p first-component)
            last-component
          (concat first-component "-" last-component)))
    str))

;;;; Kbd Macro

(defvar-local custom-modeline-kbd-macro
    '(:eval
      (when (and (mode-line-window-selected-p) defining-kbd-macro)
        (propertize " KMacro " 'face 'custom-modeline-indicator-blue-bg)))
  "Mode line construct displaying `mode-line-defining-kbd-macro'.")

;;;; Narrow indicator

(defvar-local custom-modeline-narrow
    '(:eval
      (when (and (mode-line-window-selected-p)
                 (buffer-narrowed-p)
                 (not (derived-mode-p 'Info-mode 'help-mode 'special-mode 'message-mode)))
        (propertize " Narrow " 'face 'custom-modeline-indicator-cyan-bg)))
  "Mode line construct to report the narrowed state.")

;;;; Input method

(defvar-local custom-modeline-input-method
    '(:eval
      (when current-input-method-title
        (propertize (format " %s " current-input-method-title)
                    'face 'custom-modeline-indicator-green-bg
                    'mouse-face 'mode-line-highlight)))
  "Mode line construct to report the multilingual environment.")

;;;; Buffer status

(defvar-local custom-modeline-buffer-status
    '(:eval
      (when (file-remote-p default-directory)
        (propertize " @ "
                    'face 'custom-modeline-indicator-red-bg
                    'mouse-face 'mode-line-highlight)))
  "Mode line construct for showing remote file name.")

(defvar-local custom-modeline-window-dedicated-status
    '(:eval
      (when (window-dedicated-p)
        (propertize " = "
                    'face 'custom-modeline-indicator-gray-bg
                    'mouse-face 'mode-line-highlight)))
  "Mode line construct for dedicated window indicator.")

;;;; Buffer name and modified status

(defun custom-modeline-buffer-identification-face ()
  "Return appropriate face or face list for `custom-modeline-buffer-identification'."
  (let ((file (buffer-file-name)))
    (cond
     ((and (mode-line-window-selected-p)
           file
           (buffer-modified-p))
      '(italic mode-line-buffer-id))
     ((and file (buffer-modified-p))
      'italic)
     ((mode-line-window-selected-p)
      'mode-line-buffer-id))))

(defun custom-modeline--buffer-name ()
  "Return `buffer-name', truncating it if necessary."
  (when-let* ((name (buffer-name)))
    (custom-modeline-string-cut-middle name)))

(defun custom-modeline-buffer-name ()
  "Return buffer name, with read-only indicator if relevant."
  (let ((name (custom-modeline--buffer-name)))
    (if buffer-read-only
        (format "%s %s" (char-to-string #xE0A2) name)
      name)))

(defvar-local custom-modeline-buffer-identification
    '(:eval
      (propertize (custom-modeline-buffer-name)
                  'face (custom-modeline-buffer-identification-face)
                  'mouse-face 'mode-line-highlight))
  "Mode line construct for identifying the buffer being displayed.")

;;;; Major mode

(defun custom-modeline-major-mode-icon ()
  "Return icon for the major mode using nerd-icons."
  (if (require 'nerd-icons nil t)
      (propertize (nerd-icons-icon-for-mode major-mode) 'face 'custom-modeline-indicator-gray)
    ""))

(defun custom-modeline-major-mode-name ()
  "Return capitalized `major-mode' without the -mode suffix."
  (string-replace "-mode" "" (symbol-name major-mode)))

(defvar-local custom-modeline-major-mode
    (list
     (propertize "%[" 'face 'custom-modeline-indicator-red)
     '(:eval
       (concat
        (custom-modeline-major-mode-icon)
        " "
        (propertize
         (custom-modeline-string-abbreviate-but-last
          (custom-modeline-major-mode-name)
          2)
         'mouse-face 'mode-line-highlight)))
     (propertize "%]" 'face 'custom-modeline-indicator-red))
  "Mode line construct for displaying major modes.")

(defvar-local custom-modeline-process
    (list '("" mode-line-process))
  "Mode line construct for the running process indicator.")

;;;; Git branch and diffstat

(declare-function vc-git--symbolic-ref "vc-git" (file))

(defun custom-modeline--vc-branch-name (file backend)
  "Return capitalized VC branch name for FILE with BACKEND."
  (when-let* ((rev (vc-working-revision file backend))
              (branch (or (vc-git--symbolic-ref file)
                          (substring rev 0 7))))
    branch))

(defun custom-modeline--vc-text (file branch &optional face)
  "Prepare text for Git controlled FILE, given BRANCH."
  (format "%s %s "
          (propertize "" 'face 'custom-modeline-indicator-gray) ; plain unicode branch char
          (propertize branch
                      'face face
                      'mouse-face 'mode-line-highlight)))

(defun custom-modeline--vc-details (file branch &optional face)
  "Return Git BRANCH details for FILE, truncating it if necessary."
  (custom-modeline-string-cut-end
   (custom-modeline--vc-text file branch face)))

(defvar custom-modeline--vc-faces
  '((added . vc-locally-added-state)
    (edited . vc-edited-state)
    (removed . vc-removed-state)
    (missing . vc-missing-state)
    (conflict . vc-conflict-state)
    (locked . vc-locked-state)
    (up-to-date . vc-up-to-date-state))
  "VC state faces.")

(defun custom-modeline--vc-get-face (key)
  "Get face from KEY in `custom-modeline--vc-faces'."
  (alist-get key custom-modeline--vc-faces 'vc-up-to-date-state))

(defun custom-modeline--vc-face (file backend)
  "Return VC state face for FILE with BACKEND."
  (when-let* ((key (vc-state file backend)))
    (custom-modeline--vc-get-face key)))

(defvar-local custom-modeline-vc-branch
    '(:eval
      (when-let* (((mode-line-window-selected-p))
                  (file (or buffer-file-name default-directory))
                  (backend (or (vc-backend file) 'Git))
                  (branch (custom-modeline--vc-branch-name file backend))
                  (face (custom-modeline--vc-face file backend)))
        (custom-modeline--vc-details file branch face)))
  "Mode line construct to return propertized VC branch.")

;;;; Flymake errors, warnings, notes

(declare-function flymake--severity "flymake" (type))
(declare-function flymake-diagnostic-type "flymake" (diag))

(defun custom-modeline-flymake-counter (type)
  "Compute number of diagnostics in buffer with TYPE's severity."
  (let ((count 0))
    (dolist (d (flymake-diagnostics))
      (when (= (flymake--severity type)
               (flymake--severity (flymake-diagnostic-type d)))
        (cl-incf count)))
    (when (cl-plusp count)
      (number-to-string count))))

(defmacro custom-modeline-flymake-type (type indicator &optional face)
  "Return function that handles Flymake TYPE with stylistic INDICATOR and FACE."
  `(defun ,(intern (format "custom-modeline-flymake-%s" type)) ()
     (when-let* ((count (custom-modeline-flymake-counter
                         ,(intern (format ":%s" type)))))
       (concat
        (propertize ,indicator 'face 'custom-modeline-indicator-gray)
        (propertize count
                    'face ',(or face type)
                    'mouse-face 'mode-line-highlight)
        " "))))

(custom-modeline-flymake-type error "☣ " error)
(custom-modeline-flymake-type warning "⚠ " warning)
(custom-modeline-flymake-type note "· " success)

(defvar-local custom-modeline-flymake
    `(:eval
      (when (and (bound-and-true-p flymake-mode)
                 (mode-line-window-selected-p))
        (list
         '(:eval (custom-modeline-flymake-error))
         '(:eval (custom-modeline-flymake-warning))
         '(:eval (custom-modeline-flymake-note)))))
  "Mode line construct displaying Flymake diagnostics.")

;;;; LSP Mode

(defvar-local custom-modeline-lsp
    `(:eval
      (when (and (bound-and-true-p lsp-mode) (mode-line-window-selected-p) (fboundp 'lsp-workspaces))
        (let ((servers (mapconcat (lambda (w) (symbol-name (lsp--workspace-server-id w))) (lsp-workspaces) "/")))
          (if (string-empty-p servers)
              " LSP:? "
            (list " "
                  (propertize "LSP" 'face 'custom-modeline-indicator-blue)
                  ":"
                  (propertize servers 'face 'custom-modeline-indicator-gray)
                  " ")))))
  "Mode line construct displaying LSP information.")

;;;; Time

(defvar-local custom-modeline-time-aligned
    `(:eval
      (when (mode-line-window-selected-p)
        (let* ((time-str (format-time-string " %a, %d %b %H:%M  "))
               ;; Calculate space needed to push time-str to the right
               (spacer (propertize " " 'display `((space :align-to (- right ,(length time-str)))))))
          (concat spacer time-str))))
  "Mode line construct to display the date and time right-aligned.")

;;;; Miscellaneous

(defvar-local custom-modeline-misc-info
    '(:eval
      (when (mode-line-window-selected-p)
        mode-line-misc-info))
  "Mode line construct displaying `mode-line-misc-info'.")

;;;; Risky local variables

(dolist (construct '(custom-modeline-kbd-macro
                     custom-modeline-narrow
                     custom-modeline-input-method
                     custom-modeline-buffer-status
                     custom-modeline-window-dedicated-status
                     custom-modeline-buffer-identification
                     custom-modeline-major-mode
                     custom-modeline-process
                     custom-modeline-vc-branch
                     custom-modeline-flymake
                     custom-modeline-lsp
                     custom-modeline-time-aligned
                     custom-modeline-misc-info))
  (put construct 'risky-local-variable t))


;;;; Format definition

(setq-default mode-line-format
              '("%e"
                mode-line-front-space
                custom-modeline-kbd-macro
                custom-modeline-narrow
                custom-modeline-input-method
                custom-modeline-window-dedicated-status
                custom-modeline-buffer-status
                " "
                custom-modeline-buffer-identification
                "  "
                custom-modeline-vc-branch
                "  "
                custom-modeline-flymake
                custom-modeline-lsp
                "  "
                custom-modeline-major-mode
                custom-modeline-process
                "  "
                custom-modeline-misc-info
                custom-modeline-time-aligned
                mode-line-end-spaces))

(provide 'custom-modeline)
;;; custom-modeline.el ends here
