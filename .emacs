;;;;
;;;; gbrener's .emacs file
;;;; Created: December 23, 2012
;;;;


;;; SYSTEM SETTINGS
(cond
 ((eq system-type 'darwin)
  (setq mac-option-modifier 'super
	mac-command-modifier 'meta
        ispell-program-name "/usr/local/bin/aspell")
  (setenv "PATH"
          (concat (concat "/Users/" (getenv "USER") "/anaconda/bin") ":"
                  "/usr/local/bin" ":"
                  (getenv "PATH"))))
 (t
  ;; load my private utility functions
  (setq ispell-program-name "/usr/bin/aspell")
  (add-to-list 'load-path (concat (getenv "HOME") "/.emacs.d/elisp"))

  (require 'utils)

  ;; add erlang-mode to path
  (let* ((erlang-dir "/usr/lib/erlang")
	 (erlang-bin-dir (concat erlang-dir "/bin"))
	 (erlang-etools-dir (shell-command-to-string (concat "find " erlang-dir " -type d -name \"emacs\" | sed q"))))
    (add-to-list 'load-path erlang-etools-dir)
    (setq erlang-root-dir erlang-dir)
    (add-to-list 'exec-path (cons erlang-bin-dir exec-path))
    ;(require 'erlang-start)
    )))

;; allow Linux commands that use the pager,
;; e.g. 'man', 'less', etc
;; to display their output like 'cat'.
(setenv "PAGER" "cat")


;;; STARTUP SETTINGS
;; enable features
(dolist (sym '(set-goal-column
               narrow-to-region
               upcase-region
               downcase-region))
  (put sym 'disabled nil))


;;; UTILITY FUNCTIONS
(defun swap-windows ()
  "Swap the current window with the previous one."
  (interactive)
  ;; get this window and a window above or to the left of this one
  (let* ((first-child (selected-window))
	 (next-child  (next-window)))
    (if (not next-child) nil
      ;; some bindings to "remember" the cursor and buffer-start positions
      (let ((this-buffer     (window-buffer first-child))
            (this-buffer-pos (window-start  first-child))
            (this-point	     (window-point  first-child))
            (next-buffer     (window-buffer next-child))
            (next-buffer-pos (window-start  next-child))
            (next-point	     (window-point  next-child)))
        ;; swap the windows
        (set-window-buffer first-child next-buffer)
        (set-window-start  first-child next-buffer-pos)
        (set-window-point  first-child next-point)
        (set-window-buffer next-child  this-buffer)
        (set-window-start  next-child  this-buffer-pos)
        (set-window-point  next-child  this-point)))))

;; utility function
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


;;; HOOKS, ADVICE, etc
;; hook `abbrev-mode' to `comint-mode' and all its derived modes
(add-hook 'comint-mode-hook 'abbrev-mode)

;; turn off line numbers in `comint-mode' and all its derived modes
(add-hook 'comint-mode-hook '(lambda () (linum-mode -1)))

;; add ANSI colors to `comint-mode'
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
(add-to-list 'comint-output-filter-functions 'ansi-color-process-output)

(autoload 'markdown-mode "markdown-mode" "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

;; activate flyspell-mode for .rst files, .md files, and Python comments/docstrings
(add-hook 'rst-mode 'flyspell-mode)
(add-hook 'markdown-mode 'flyspell-mode)
(add-hook 'python-mode-hook 'flyspell-prog-mode)

;; display a helpful message while decompressing/loading the Emacs manual
(defadvice info-emacs-manual (before display-helpful-message last)
  (message "Opening Emacs manual..."))
(ad-activate 'info-emacs-manual)


;;; CUSTOMIZATIONS
;; just in case the Alt key isn't working, and we need M-x
(global-set-key (kbd "C-x C-m") 'execute-extended-command)

;; Org Mode settings
(setq org-catch-invisible-edits 'smart
      org-enforce-todo-dependencies t
      org-closed-keep-when-no-todo t
      org-log-done 'note
      org-hierarchical-todo-statistics t
      org-todo-keywords '((sequence "TODO" "IN-PROGRESS" "FEEDBACK" "|" "DONE"))
      org-todo-keyword-faces '(("TODO" . org-warning)
                               ("IN-PROGRESS" . "yellow")
                               ("FEEDBACK" . "blue")))
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

;; remap C-M-spacebar and C-M-@ as "canonically-space-region or just-one-space", whichever is appropriate
(global-unset-key (kbd "C-M-SPC"))
(global-unset-key (kbd "C-M-@"))
(global-set-key (kbd "C-M-SPC")
                '(lambda ()
                   "Call `canonically-space-region' if region is active; otherwise calls `just-one-space'."
                   (interactive)
                   (call-interactively
                    (if (use-region-p)
			'canonically-space-region
                      'just-one-space))))

;; bind C-c + to 'increment-at-point'
(global-set-key (kbd "C-c +")
                '(lambda ()
                   "Increment number at point."
                   (interactive)
                   (skip-chars-backward "0123456789")
                   (or (looking-at "[0-9]+")
                       (error "No number at point"))
                   (replace-match (number-to-string (1+ (string-to-number (match-string 0)))))))

;; remap query-replace-regexp to C-c r
(global-set-key "\C-cr" 'query-replace-regexp)

;; set global keys for org-mode
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

;; use "y or n" for answers instead of "yes or no"
(fset 'yes-or-no-p 'y-or-n-p)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-compression-mode t)
 '(auto-insert-mode t)
 '(auto-insert-query t)
 '(blink-cursor-mode nil)
 '(column-number-mode t)
 '(comint-buffer-maximum-size 1024)
 '(default-frame-alist
    (quote
     ((background-color . "black")
      (foreground-color . "white"))))
 '(dired-listing-switches "-vAlhF --time-style=long-iso")
 '(file-name-shadow-mode t)
 '(frame-background-mode (quote dark))
 '(global-font-lock-mode t)
 '(global-subword-mode t)
 '(ido-mode (quote both) nil (ido))
 '(indent-tabs-mode nil)
 '(inhibit-startup-echo-area-message (getenv "USER"))
 '(inhibit-startup-screen t)
 '(initial-scratch-message
   ";;
;; The best way to predict the future is to invent it. -Alan Kay
;;

")
 '(line-move-visual nil)
 '(mark-ring-max 23)
 '(menu-bar-mode nil)
 '(occur-hook (quote ((lambda nil (occur-rename-buffer t)))))
 '(org-agenda-files nil)
 '(python-indent 4)
 '(scroll-bar-mode nil)
 '(scroll-conservatively 1000000)
 '(scroll-preserve-screen-position 1)
 '(shell-file-name "/bin/bash")
 '(shift-select-mode nil)
 '(show-paren-mode t)
 '(show-paren-style (quote parenthesis))
 '(tool-bar-mode nil)
 '(tooltip-mode nil)
 '(tramp-default-method "ssh")
 '(transient-mark-mode t)
 '(vc-handled-backends nil)
 '(version-control (quote never))
 '(visible-bell t)
 '(windmove-wrap-around t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(link ((t (:foreground "green" :underline t))))
 '(minibuffer-prompt ((t (:foreground "green"))))
 '(show-paren-match ((t (:background "white" :foreground "black"))))
 '(show-paren-mismatch ((t (:underline t :weight ultra-bold :width extra-expanded)))))

;; enable shell-mode
(shell)
