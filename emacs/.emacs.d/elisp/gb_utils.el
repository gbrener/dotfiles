;;; utils.el --- Emacs lisp utilities                -*- lexical-binding: t; -*-


;; Remap modifier keys if necessary
(when (eq system-type 'darwin)
  (setq mac-option-modifier 'super
        mac-command-modifier 'meta))

;; Use "y or n" for answers instead of "yes or no"
(fset 'yes-or-no-p 'y-or-n-p)

;; Don't prompt me before creating a new file or buffer
(setq confirm-nonexistant-file-or-buffer nil)

;; Enable features that are turned-off by default
(dolist (sym '(set-goal-column
               narrow-to-region
               upcase-region
               downcase-region))
  (put sym 'disabled nil))

(defsubst prefix-is-a-number-p (prefix)
  "Inline function that determines whether its argument (a raw prefix argument) is 'C-u' without a number."
  (not (and (consp arg)
            (= (prefix-numeric-value arg) 4))))

;; close completion buffer when finished with it
(defun comint-close-completions ()
  "Close the comint completions buffer.
Used in advice to various comint functions to automatically close
the completions buffer as soon as I'm done with it. Based on
Dmitriy Igrishin's patched version of comint.el."
  (when comint-dynamic-list-completions-config
    (set-window-configuration comint-dynamic-list-completions-config)
    (setq comint-dynamic-list-completions-config nil)))

(defadvice comint-send-input (after close-completions activate)
  (comint-close-completions))

(defadvice comint-dynamic-complete-as-filename (after close-completions activate)
  (when ad-return-value
    (comint-close-completions)))

(defadvice comint-dynamic-simple-complete (after close-completions activate)
  (when (member ad-return-value '(sole shortest partial))
    (comint-close-completions)))

(defadvice comint-dynamic-list-completions (after close-completions activate)
    (comint-close-completions)
    (when (not unread-command-events)
      ;; comint's "Type space to flush" swallows space. put it back in.
      (setq unread-command-events (listify-key-sequence " "))))


;; display a helpful message while decompressing/loading the Emacs manual
(defadvice info-emacs-manual (before display-helpful-message last)
  (message "Opening Emacs manual..."))
(ad-activate 'info-emacs-manual)


(defun buffer-name-matches-p (match-prefix)
  "Return a function which has the following behavior:
      Return `buf'\'s buffer-name if it matches the prefix `match-prefix';
      Otherwise return `nil'."
  #'(lambda (buf)
      (let ((name (buffer-name buf))
            (min-length (length match-prefix)))
        (when (> (length name) min-length)
          (let ((name-prefix (substring name 0 min-length)))
            (when (string= match-prefix name-prefix)
              name))))))



;;; Blog-related stuff
(defun filter-out-nils (lst)
  "Remove `nil' values from `lst'"
  (delete nil lst))

(defun sort-strings-asc (lst)
  "Sort `lst', a list of strings, lexographically in ascending-order"
  (sort lst #'string<))


(defun connected-to-internet-p ()
  (let ((net-interfaces (network-interface-list)))
    (or (assoc "wlo1" net-interfaces)
        (assoc "en0" net-interfaces))))


(provide 'gb_utils)
;;; utils.el ends here
