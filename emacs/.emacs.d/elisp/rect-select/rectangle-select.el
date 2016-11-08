;;;; rect-select.el --- Support for column-based highlighting and editing operations.

;; Copyright (C) 2012-2013 G. Brener

;; Author: G. Brener
;; Created: 23 Dec 2012
;; Keywords: convenience
;; Version: 1.0

;;; Commentary:

;;
;; * Initial mappings upon loading this file with "M-x `load-file'" or (require 'rectangle-select) are as follows:
;;
;;     map "C-x <space>" to rectangle-select-mode
(global-set-key "\C-x " 'enable-rectangle-select)
;;
;;     map "Shift <left-mouse-drag>" to mouse-drag-rectangle
(global-set-key [S-down-mouse-1] 'rs-mouse-drag-rectangle)
;;
;;     These can be remapped in an init file after this library is loaded or by altering this file directly.
;;
;; * It's highly recommended to byte-compile this file prior to loading it. This can be accomplished using "M-x `byte-compile-file'".
;;
;; * Buffer-local mark `rs-mark' stores the buffer position where rectangle select mode was activated.
;;   All functions in this file reference `rs-mark' instead of The Mark normally used by interactive functions.
;;
;; * Keymap `rectangle-select-map' inherits `global-keymap' and the functions in `rs-global-map-subst-alist'.
;;   When a function is defined in `rectangle-select-map', its binding takes precedence in the current buffer while rectangle select mode is active.
;;   One way to add/override bindings would be:
;;
;;     (eval-after-load "rectangle-select"
;;       '(progn
;;          (define-key rectangle-select-map (kbd "C-x") 'kill-region)
;;          (define-key rectangle-select-map (kbd "C-c") 'kill-ring-save)))
;;
;; * Custom variables include:
;;   `rectangle-select-map'      - Keymap. Described in commentary above.
;;   `rs-echo-selection'         - Boolean value. When t, information about the selected region is displayed in the echo area during rectangle select mode.
;;   `rs-marker-insertion-type'  - Boolean value. When nil, marker position remains static when text is inserted before it.
;;                                   When t, marker position is automatically adjusted forward when text is inserted before it.
;;   `rs-selection-plist'        - Property list. Text-properties that get added/removed to the selected region during rectangle select mode.
;;   `rs-global-map-subst-alist' - Association list. Association list of functions to substitute during rectangle-select mode.
;;                                   The 'key-type' is a function to (temporarily) replace all keybindings of 'value'.
;;                                   The 'value-type' is a function in `global-map' or a key-sequence (i.e. a result of `kbd').
;;   `rs-disregard-line-endings' - Boolean value. When nil, lines are limited by the current line's ending.
;;                                   When t, lines aren't limited (but "C-e" goes to the end of the longest line in the selected region).
;;
;; * When `rs-disregard-line-endings' is non-nil, whitespace may be added at the ends of the lines. This is cleaned up on deactivation of rectangle select mode
;;   by calling `rs-delete-trailing-whitespace'.
;;
;; * Three functions do most of the "heavy-lifting":
;;
;; (1) `rs-select-rectangle' - added to `post-command-hook' while rectangle select mode is active.
;;   After each command that triggers `post-command-hook', `rs-select-rectangle' highlights the region between `rs-mark' and the point.
;;   It accomplishes this via adding the text-properties in `rs-selection-plist' to the selected region.
;;
;; (2) `rs-deselect-rectangle' - added to `pre-command-hook' while rectangle select mode is active.
;;   After each command that triggers `pre-command-hook', `rs-deselect-rectangle' unhighlights the region between `rs-mark' and the point.
;;   It accomplishes this via removing the text-properties in `rs-selection-plist' from the selected region and calling `jit-lock-fontify-now' on the current buffer.
;;   `jit-lock-fontify-now' is called in order to restore font highlighting to the deselected region.
;;
;; (3) `rs-update-selection-after-change' - added to the `after-change-functions' hook while rectangle select mode is active.
;;   After each buffer change that calls `after-change-functions', `rs-update-selection-after-change' checks whether text was inserted or deleted.
;;   If there was an insertion or deletion, `rs-update-selection-after-change' applies that operation across each line in the selected region.
;;
;; * Dependencies include `rect.el' and `jit-lock.el' for the `...-rectangle' and `jit-lock-...' functions used throughout this file.
;;

;; This file is not part of GNU Emacs.

;; This file is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this file.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

;; load font-lock and rectangle operations when byte-compiling this library
(eval-and-compile
  (require 'jit-lock)
  (require 'rect))

(defgroup rectangle-select nil
  "Customization group for rectangle select mode."
  :group 'editing-basics
  :group 'convenience
  :version "24.2"
  :link '(emacs-commentary-link :tag "Commentary" "rect-select.el")
  :link '(emacs-library-link :tag "Lisp File" "rect-select.el")
  :link '(function-link :tag "Main Function" "rectangle-select-mode")
  :load "rect-select"
  :tag "Rectangle-Select Mode"
  :prefix "rs-")

(defface rect-select-hl-face '((default :inherit region)
                               (((class color)) :foreground "white" :background "red"))
  "Font used in rect-select to highlight the rectangle-selection."
  :group 'rectangle-select)

(defcustom rs-global-map-subst-alist '((rs-exchange-point-and-mark exchange-point-and-mark)
                                       (rs-kill-region             kill-region)
                                       (rs-delete-region           delete-rectangle)
                                       (rs-kill-ring-save          kill-ring-save)
                                       (rs-indent-region           indent-region)
                                       (rs-forward-char            forward-char)
                                       (rs-next-line               next-line)
                                       (rs-previous-line           previous-line)
                                       (rs-move-end-of-line        move-end-of-line))
  "Association list of functions to substitute during rectangle-select mode.
The 'key' is a function to (temporarily) replace all keybindings of 'value'.
The 'value' is a function in `global-map' or a key-sequence (i.e. a result of `kbd')."
  :type '(alist :key-type function :value-type function)
  :risky t
  :group 'rectangle-select)

(defcustom rectangle-select-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map (current-global-map))
    (mapc #'(lambda (fns)
              ;; `rs-global-map-subst-alist' is an associated list with each cons cell containing
              ;; a "new function" as the first element and either a key-sequence (as returned by `kbd')
              ;; or an "old function" as a value. `define-key' is called here if the value is a
              ;; key-sequence; otherwise all key-sequences that are bound to the "old function"
              ;; in `global-map' are rebound to "new function" (with respect to `rectangle-select-map')
              (let ((fn (car fns))
                    (fn-or-keys (cadr fns)))
                (cond
                 ((functionp fn-or-keys) (substitute-key-definition fn-or-keys fn map))
                 ((or (stringp fn-or-keys)
                      (vectorp fn-or-keys)) (define-key map fn-or-keys fn))
                 (t (error "rs-global-map-subst-alist must be of the form: (list (list function (or function keys)))")))))
          rs-global-map-subst-alist)
    map)
  "Buffer-local keymap used by rectangle-select mode. Is re-read every time rectangle-select mode is activated."
  :type 'keymap
  :set-after '(rs-global-map-subst-alist)
  :group 'rectangle-select)
(make-variable-buffer-local 'rectangle-select-map)

(defcustom rs-disregard-line-endings t
  "Boolean value representing whether the current line's length should limit the rectangular-selection.
nil represents the default behavior of `rect.el'."
  :type 'boolean
  :group 'rectangle-select)
(make-variable-buffer-local 'rs-disregard-line-endings)

(defcustom rs-echo-selection nil
  "Boolean value representing whether information about the rectangular-selection is displayed in the echo area."
  :type 'boolean
  :group 'rectangle-select)
(make-variable-buffer-local 'rs-echo-selection)

(defcustom rs-marker-insertion-type t
  "Insertion type for `rs-mark', a buffer-local marker used by rectangle-select mode."
  :type 'boolean
  :group 'rectangle-select)
(make-variable-buffer-local 'rs-marker-insertion-type)

(defvar rs-mark (make-marker)
  "Buffer-local marker used internally by rectangle-select mode.")
(set-marker-insertion-type rs-mark rs-marker-insertion-type)
(make-variable-buffer-local 'rs-mark)

(defvar rs-highest-modified-line 0
  "Integer set by `rs-update-modified-lines' that stores the topmost line that was modified by rectangle select mode.
Reset by `rs-delete-trailing-whitespace'.")

(defvar rs-lowest-modified-line 0
  "Integer set by `rs-update-modified-lines' that stores the bottommost line that was modified by rectangle select mode.
Reset by `rs-delete-trailing-whitespace'.")

(defcustom rs-selection-plist (list 'fontified      t
                                    'region         t
                                    'face           'rect-select-hl-face
                                    'font-lock-face 'rect-select-hl-face)
  "Property list applied while selecting/deselecting a region in rectangle-select mode."
  :type 'plist
  :options '(("fontified" boolean)
             ("region" boolean)
             ("face" face)
             ("font-lock-face" face))
  :set-after '(rect-select-hl-face)
  :group 'rectangle-select)

(defsubst beforep (n1 n2)
  "Returns t if one \"integer | line | column | etc...\" N1 comes before the other, N2; otherwise returns nil."
  (< n1 n2))

(defmacro without-updating-selection (&rest body)
  "An alternative to using `inhibit-modification-hooks' that prevents `rs-update-selection-after-change' from executing during the evaluation of FORM.
Returns the result of evaluating FORM."
  `(prog2
       (remove-hook 'after-change-functions 'rs-update-selection-after-change t)
       (progn
         ,@body)
     (add-hook 'after-change-functions 'rs-update-selection-after-change nil t)))

(defmacro with-selected-rectangle (start end &rest body)
  "Execute BODY on each line of the selected region. Locally-bind the variables `startcol' and `endcol' on every line.
Assume START is where `startcol' is and END is where `endcol' is.
Borrows heavily from `apply-on-rectangle' in `rect.el'."
  (let ((startpt (make-symbol "--a-unique-startpt-012--"))
        (endpt   (make-symbol "--a-unique-endpt-345--")))
    `(let (startcol endcol ,startpt ,endpt)
       (save-excursion
         (goto-char ,start)
         (setq startcol (current-column))
         (beginning-of-line)
         (setq ,startpt (point))
         (goto-char ,end)
         (setq endcol (current-column))
         (forward-line 1)
         (setq ,endpt (point-marker))
         ;;(message "%d %d" startcol endcol)
         (goto-char ,startpt)
         (while (< (point) ,endpt)
           ,@body
           (forward-line 1)))
       nil)))

(define-minor-mode rectangle-select-mode
  "Toggles `rectangle-select-mode'.

Adds functions to `pre-command-hook', `post-command-hook', `deactivate-mark-hook', and `after-change-functions'.

`jit-lock-after-change' and `jit-lock-function' are removed from
`after-change-functions' and `fontification-functions' respectively,
  to prevent string and comment faces from overwriting text properties."
  :group 'rectangle-select
  :global nil
  :init-value nil
  :lighter " Rectangle-Select"
  :keymap rectangle-select-map
  :after-hook post-command-hook
  (let ((enable (if (eq arg 'toggle) (not rectangle-select-mode)
                  (> (prefix-numeric-value arg) 0))))
    (cond
     ((not enable) (deactivate-rectangle-select-mode))
     (t
      (when rectangle-select-mode
        (deactivate-rectangle-select-mode))
      (setq rs-mark (point-marker))
      (push-mark)
      (add-hook 'pre-command-hook           'rs-deselect-rectangle nil t)
      (when (not rs-disregard-line-endings)
        (add-hook 'pre-command-hook           'rs-delete-trailing-whitespace t t))
      (add-hook 'post-command-hook          'rs-select-rectangle t t)
      (add-hook 'post-command-hook          'rs-update-modified-lines t t)
      (when rs-echo-selection
        (add-hook 'post-command-hook          'rs-display-selection t t))
      (add-hook 'deactivate-mark-hook       'disable-rectangle-select nil t)
      (add-hook 'after-change-functions     'rs-update-selection-after-change nil t)
      (remove-hook 'after-change-functions  'jit-lock-after-change t)
      (remove-hook 'fontification-functions 'jit-lock-function t)))
    nil))

(defun deactivate-rectangle-select-mode ()
  "Deactivate rectangular select-mode.

Removes functions from `pre-command-hook', `post-command-hook', `deactivate-mark-hook', and `after-change-functions'."
  (remove-hook 'deactivate-mark-hook   'disable-rectangle-select t)
  (remove-hook 'after-change-functions 'rs-update-selection-after-change t)
  (remove-hook 'post-command-hook      'rs-display-selection t)
  (remove-hook 'post-command-hook      'rs-update-modified-lines t)
  (remove-hook 'post-command-hook      'rs-select-rectangle t)
  (remove-hook 'pre-command-hook       'rs-deselect-rectangle t)
  (remove-hook 'pre-command-hook       'rs-delete-trailing-whitespace t)
  (add-hook 'fontification-functions   'jit-lock-function nil t)
  (add-hook 'after-change-functions    'jit-lock-after-change nil t)
  (rs-deselect-rectangle)
  (rs-delete-trailing-whitespace)
  (when (marker-position rs-mark)
    (set-marker rs-mark nil))
  (jit-lock-fontify-now)
  nil)

(defun enable-rectangle-select ()
  "Enable rectangle select mode. This is particularly useful when binded to a key, i.e. within `global-set-key'."
  (interactive)
  (rectangle-select-mode 1))

(defun disable-rectangle-select ()
  "Disable rectangle select mode. This should not be bound to a key, but may be used inside a Lisp program."
  (rectangle-select-mode -1))

(defun rs-update-modified-lines ()
  "Updates the variables `rs-highest-modified-line' and `rs-lowest-modified-line'.
Meant to be used internally to rectangle select mode via the `after-change-functions'."
  (when (marker-position rs-mark)
    (let ((this-line (line-number-at-pos))
          (mark-line (save-excursion
                       (goto-char (marker-position rs-mark))
                       (line-number-at-pos))))
      (setq rs-highest-modified-line (min rs-highest-modified-line this-line mark-line)
            rs-lowest-modified-line (max rs-lowest-modified-line this-line mark-line))))
  nil)

(defun rs-delete-trailing-whitespace (&optional beg end)
  "Calls `delete-trailing-whitespace' on the selection made by rectangle select mode (if there is one).
Resets `rs-highest-modified-line' and `rs-lowest-modified-line' to their default values.
If rectangle select mode isn't activated, this simply calls `delete-trailing-whitespace'."
  (when (marker-position rs-mark)
    (when (not (and beg end))
      (setq beg nil
            end nil)
      (save-excursion
        (goto-char (point-min))
        ;; if we can move to the highest line that was modified, set "beg" to be that line's beginning position
        ;;   if we can move to the lowest line that was modified, set "end" to that line's ending position
        ;; if either of these movements are impossible, set "beg" and "end" to the minimum and maximum-permissible positions in the current buffer
        (setq beg (cond
                   ((eq 0 (forward-line (1- rs-highest-modified-line))) (line-beginning-position))
                   (t (setq end (point-max))
                      (point-min))))
        (when (not end)
          (setq end (cond
                     ((eq 0 (forward-line (- rs-lowest-modified-line
                                             rs-highest-modified-line
                                             1))) (line-end-position))
                     (t (setq beg (point-min))
                        (point-max)))))))
    (save-restriction
      (narrow-to-region beg end)
      (without-updating-selection
       (delete-trailing-whitespace)))
    (setq rs-highest-modified-line 0
          rs-lowest-modified-line  0)))

(defun rs-select-rectangle (&optional begin end)
  "\"Highlight\" the rectangular selection between BEGIN and END.
Uses custom-variable `rs-selection-plist' for the list of properties to remove.

If called interactively, this function uses the mark if the region is active. Otherwise it uses `rs-mark'."
  (interactive (if (use-region-p) (list (region-beginning) (region-end))
                 (list nil nil)))
  (when (marker-position rs-mark)
    (let ((pos (point))
          (inhibit-modification-hooks t))
      (setq begin (or begin (min rs-mark pos))
            end (or end (max rs-mark pos)))
      (with-selected-rectangle begin end
                               (add-text-properties (progn
                                                      (move-to-column startcol t)
                                                      (point))
                                                    (progn
                                                      (move-to-column endcol t)
                                                      (point))
                                                    rs-selection-plist)))
    nil))

(defun rs-deselect-rectangle (&optional begin end)
  "\"Un-highlight\" the rectangular selection between BEGIN and END.
Uses custom-variable `rs-selection-plist' for the list of properties to remove.

If called interactively, this function uses the mark if the region is active. Otherwise it uses `rs-mark'."
  (interactive (if (use-region-p) (list (region-beginning) (region-end))
                 (list nil nil)))
  (when (marker-position rs-mark)
    (let ((pos (point))
          (inhibit-modification-hooks t))
      (setq begin (or begin (min rs-mark pos))
            end (if (not rs-disregard-line-endings) (copy-marker (or end (max rs-mark pos)))
                  (or end (max rs-mark pos))))
      (when (not rs-disregard-line-endings)
        (rs-delete-trailing-whitespace))
      (with-selected-rectangle begin (if (markerp end) (marker-position end) end)
                               (remove-text-properties (progn
                                                         (move-to-column startcol t)
                                                         (point))
                                                       (progn
                                                         (move-to-column endcol t)
                                                         (point))
                                                       rs-selection-plist)))
    (jit-lock-fontify-now)
    (when (markerp end)
      (set-marker end nil)))
  nil)

(defun rs-display-selection ()
  "While in rectangular-select mode, this function updates the echo area with information about the selection."
  (when (marker-position rs-mark)
    (let (cursor-pos cursor-line cursor-column
                     mark-line mark-column top-line bottom-line right-column left-column
                     tl bl l-range lc rc c-range)
      (setq cursor-pos (point)
            cursor-line (line-number-at-pos)
            cursor-column (current-column))
      (save-excursion
        (goto-char (marker-position rs-mark))
        (setq mark-line (line-number-at-pos)
              mark-column (current-column)))
      (setq top-line (min mark-line cursor-line)
            bottom-line (max mark-line cursor-line)
            left-column (min mark-column cursor-column)
            right-column (max mark-column cursor-column)
            tl (number-to-string top-line)
            bl (number-to-string bottom-line)
            l-range (number-to-string (1+ (- bottom-line top-line))))
      (message (concat "Lines: " tl "-" bl " (" l-range ")"
                       (when t ;; left open for 'line-mode' if i decide to implement it later
                         (setq lc (number-to-string left-column)
                               rc (number-to-string right-column)
                               c-range (number-to-string (1+ (- right-column left-column))))
                         (cond
                          ((= left-column right-column) (concat ", Column: " lc " (1)"))
                          (t (concat ", Columns: " lc "-" rc " (" c-range ")")))))))
    nil))

(defun rs-exchange-point-and-mark ()
  "Put `rs-mark' where point is now, and point where `rs-mark' is now.
This command works even when the mark is not active,
and it reactivates the mark.

Unlike `exchange-point-and-mark', this function doesn't accept a prefix
argument and acts independently of Transient Mark mode."
  (interactive)
  (cond
   ((not (marker-position rs-mark)) (call-interactively 'exchange-point-and-mark))
   (t (let ((omark (marker-position rs-mark)))
        (when (null omark)
          (error "No mark set in this buffer"))
        (rectangle-select-mode 1)
        (goto-char omark)
        nil))))

(defun rs-kill-region (start end &optional fill)
  "Calls `kill-rectangle', sets the killed rectangle as the latest kill in the kill ring, and disables rectangle-select mode."
  (interactive (if rs-mark (list (min rs-mark (point))
                                 (max rs-mark (point)))
                 (list nil nil)))
  (cond
   ((not (marker-position rs-mark)) (call-interactively 'kill-region))
   (t (barf-if-buffer-read-only)
      (setq killed-rectangle (without-updating-selection
                              (delete-extract-rectangle start end fill)))
      (kill-new (mapconcat 'identity killed-rectangle "\n") t)
      (disable-rectangle-select))))

(defun rs-delete-region (start end &optional fill)
  "Calls `delete-rectangle' and disables rectangle-select mode."
  (interactive (if rs-mark (list (min rs-mark (point))
                                 (max rs-mark (point)))
                 (list nil nil)))
  (cond
   ((not (marker-position rs-mark)) (call-interactively 'delete-region))
   (t (barf-if-buffer-read-only)
      (without-updating-selection
       (delete-rectangle start end fill))
      (disable-rectangle-select))))

(defun rs-kill-ring-save (beg end)
  "Makes the selected rectangle the latest kill in the kill ring and disables rectangle-select mode."
  (interactive (if rs-mark (list (min rs-mark (point))
                                 (max rs-mark (point)))
                 (list nil nil)))
  (cond
   ((not (marker-position rs-mark)) (call-interactively 'kill-ring-save))
   (t (barf-if-buffer-read-only)
      (setq killed-rectangle (extract-rectangle beg end))
      (kill-new (mapconcat 'identity killed-rectangle "\n") t)
      (disable-rectangle-select))))

(defun rs-indent-region (start end &optional column)
  "Indents all lines in the selection and disables rectangle-select mode."
  (interactive (if rs-mark (list (min rs-mark (point))
                                 (max rs-mark (point))
                                 (prefix-numeric-value current-prefix-arg))
                 (list nil nil)))
  (cond
   ((not (marker-position rs-mark)) (call-interactively 'indent-region))
   (t (barf-if-buffer-read-only)
      (without-updating-selection
       (indent-region start end column))
      (disable-rectangle-select))))

(defun rs-forward-char (&optional n)
  "Alternative to `forward-line' that allows movement beyond the end
of the current line (if `rs-disregard-line-endings' is set).
This also \"marks\" the whitespace that gets inserted after the current
line (if it exists) with text-properties that indicate to delete
it after rectangle-select mode is deactivated."
  (interactive "p")
  (if (not rs-disregard-line-endings) (forward-char n)
    (when (marker-position rs-mark)
      (let ((inhibit-modification-hooks t)
            (line-end (line-end-position)))
        (move-to-column (+ (or n 1) (current-column)) t))))
  nil)

(defun rs-next-line (&optional arg try-vscroll)
  "Alternative to `next-line' that functions similarly to `set-goal-column' and
allows movement beyond the end of the next line (if `rs-disregard-line-endings'
is set).
This also \"marks\" the whitespace that gets inserted after the next
line (if it exists) with text-properties that indicate to delete
it after rectangle-select mode is deactivated."
  (interactive "^p\np")
  (if (not rs-disregard-line-endings) (call-interactively 'next-line)
    (when (marker-position rs-mark)
      (let ((inhibit-modification-hooks t)
            (line-end (line-end-position 2))
            (start-column (current-column)))
        (forward-line (or arg 1))
        (move-to-column start-column t))))
  nil)

(defun rs-previous-line (&optional arg try-vscroll)
  "Alternative to `previous-line' that functions similarly to `set-goal-column' and
allows movement beyond the end of the previous line (if `rs-disregard-line-endings'
is set).
This also \"marks\" the whitespace that gets inserted after the previous
line (if it exists) with text-properties that indicate to delete
it after rectangle-select mode is deactivated."
  (interactive "^p\np")
  (if (not rs-disregard-line-endings) (call-interactively 'previous-line)
    (when (marker-position rs-mark)
      (let ((inhibit-modification-hooks t)
            (line-end (line-end-position 0))
            (start-column (current-column)))
        (forward-line (* -1 (or arg 1)))
        (move-to-column start-column t))))
  nil)

(defun rs-move-end-of-line (arg)
  "Alternative to `move-end-of-line' allows movement beyond the end of the current line.
This also \"marks\" the whitespace that gets inserted after the current
line (if it exists) with text-properties that indicate to delete
it after rectangle-select mode is deactivated."
  (interactive "^p")
  (if (not rs-disregard-line-endings) (call-interactively 'move-end-of-line)
    (when (marker-position rs-mark)
      (let ((max-eol-col 0)
            (inhibit-modification-hooks t))
        (with-selected-rectangle (min rs-mark (point))
                                 (max rs-mark (point))
                                 ;; get the rightmost column for the rectangle and assign it to "max-eol-col"
                                 (setq max-eol-col (max max-eol-col (progn
                                                                      (end-of-line)
                                                                      (current-column)))))
        (move-to-column max-eol-col t))
      nil)))

(defun rs-update-selection-after-change (beg end old-len)
  "Applies the current function across all lines in the selection during rectangle-select mode.
Meant to be locally-bound to the `after-change-functions' hook."
  ;; bypass this function if any of the following conditions are true:
  ;; 1. rectangle-selection mode isn't active,
  ;; 2. the current command is "undo",
  ;; 3. we're in the minibuffer, or
  ;; 4. the current buffer is read-only
  (when (and (marker-position rs-mark)
             (not (or (eq this-command 'undo)
                      (minibufferp)
                      buffer-read-only)))
    (let* ((bytes-deleted (and (eq beg end) old-len)) ;; nil or int
           (range (or bytes-deleted (- end beg)))) ;; int
      (when (not (integerp range)) (princ range))
      ;; bypass the rest of the function unless a range of text was inserted or deleted
      (when (not (zerop range))
        (let ((this-posn (point))
              (this-line (line-number-at-pos))
              (mark-column (save-excursion
                             (goto-char (marker-position rs-mark))
                             (current-column)))
              (bd-fn (if (eq this-command 'kill-line) '(kill-line)
                       `(delete-char ,range)))
              (substr (when (not bytes-deleted)
                        (buffer-substring-no-properties beg end))))
          (with-selected-rectangle (min rs-mark this-posn)
                                   (max rs-mark this-posn)
                                   ;; do the parallel-actions if we're not on the line we started at,
                                   ;; and we can move to the column we started on (or `rs-disregard-line-endings' is set)
                                   (let* ((cur-line (line-number-at-pos))
                                          (real-endcol (if (> cur-line this-line) startcol endcol)))
                                     (when (and (not (eq this-line cur-line))
                                                (eq real-endcol
                                                    (move-to-column real-endcol
                                                                    (or rs-disregard-line-endings 'coerce))))
                                       (save-restriction
                                         (narrow-to-region (line-beginning-position) (line-end-position))
                                         (cond
                                          (bytes-deleted (apply (car bd-fn) (cdr bd-fn)))
                                          (t (forward-char (- range))
                                             (insert substr))))))))
        nil))))

(defun rs-mouse-drag-rectangle (ev)
  "Highlights a rectangle selection as the mouse is dragged."
  ;; select the start-window and capture the mouse event
  (interactive "@e")
  (let ((inhibit-modification-hooks t)
        (ev-pos (event-start ev)) ;; event position
        (top-edge (cadr (window-inside-edges)))
        (frame-height (frame-pixel-height))
        start-frame
        start-window
        next-window
        pos-or-area               ;; integer or symbol
        pos)                      ;; buffer position
    (setq start-window (posn-window ev-pos)
          pos-or-area (posn-area ev-pos)
          start-frame (window-frame start-window))
    ;; proceed only when the event began in the text-area
    ;; of a window that isn't the minibuffer
    (when (and (not pos-or-area)
               (not (minibufferp (window-buffer start-window))))

      (rectangle-select-mode 1)
      (track-mouse
        (while (mouse-movement-p (setq ev (read-event)))
          (setq ev-pos (event-start ev)
                next-window (posn-window ev-pos)
                pos-or-area (posn-area ev-pos)
                pos (posn-point ev-pos))
                                        ;(princ (+ (cdr (posn-x-y ev-pos)) top-edge))
          (cond
           ;; if there cursor is in a window and we're in the same frame as we started, proceed to more checks
           ((and (windowp next-window)
                 (eq start-frame (window-frame next-window)))
            (cond
             ;; if we're in the text area of the window that we started, do the normal selection routine
             ((and pos-or-area
                   (eq start-window next-window))
              (rs-deselect-rectangle)
              (when (not rs-disregard-line-endings)
                (rs-delete-trailing-whitespace))
              (goto-char pos)
              (rs-select-rectangle)
              (when rs-echo-selection
                (rs-display-selection)))
             ;; if we're in a window other than the one that we started in, scroll up or down as necessary
             ((not (eq start-window next-window))
              (let ((x-y-height (cdr (posn-x-y ev-pos)))
                    (next-top-edge (cadr (window-edges next-window))))
                (mouse-scroll-subr start-window (/ (- (+ x-y-height next-top-edge) ;; frame-relative-pixel-pos in next-window
                                                      (+ x-y-height top-edge))     ;; frame-relative-pixel-pos in start-window
                                                   10)))) ;; arbitrarily-chosen multiplier for pixel-height to line-height
             (t nil)))
           (t nil))))))
  nil)

;; not used
(defun delete-text-containing-property (prop &optional buffer start end)
  "Deletes all text in BUFFER between START and END that contains text-property PROP.

PROP is a text property, BUFFER is a buffer (defaults to the current buffer),
and START and END are buffer positions.

This function calls `next-single-property-change' internally; START, PROP, BUFFER, and END map to its arguments respectively."
  (barf-if-buffer-read-only)
  (let ((start-pos (or start (point-min)))
        (end-pos (or start (point-max))))
    (save-excursion
      (while (not (eq end-pos
                      (setq start-pos (next-single-property-change start-pos prop nil end-pos))))
        (goto-char start-pos)
        (delete-region start-pos
                       (next-single-property-change start-pos prop nil (line-end-position))))))
  nil)

(add-to-list 'minor-mode-alist '(rectangle-select-mode " Rectangle-Select"))

(provide 'rectangle-select)

;;; rect-select.el ends here
