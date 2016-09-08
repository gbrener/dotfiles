;;;;
;;;; gbrener's .emacs file
;;;; Created: December 23, 2012
;;;;


(defconst ELISP-DIR  "~/.emacs.d/elisp"
  "Directory containing Emacs libraries")

(defconst PLUGINS-DIR
  "~/.emacs.d/plugins"
  "Directory containing Emacs plugins (libraries that don't have 'modes')")

(defconst THEMES-DIR
  "~/.emacs.d/themes"
  "Directory containing Emacs themes")

(defconst BACKUPS-DIR
  "~/.emacs.d/backups"
  "Directory where backup files are stored")

;; Create the elisp and plugins directories if they don't already exist
(dolist (dir (list ELISP-DIR PLUGINS-DIR THEMES-DIR BACKUPS-DIR))
  (when (not (file-exists-p dir))
    (make-directory dir t)))

;; Add elisp directory to load-path
(add-to-list 'load-path ELISP-DIR)

;; Most of the features below are provided from "elisp" directory
(require 'gb_utils)
(require 'gb_keybindings)
(require 'gb_shell)
(require 'gb_org)
(require 'gb_yasnippet)
(require 'gb_flyspell)
(require 'gb_erlang)
(require 'gb_markdown)
(require 'gb_yaml)
(require 'gb_json)
(require 'gb_use-package)
(require 'gb_rect-select)
(require 'gb_annotate)
(require 'gb_xt-mouse)
(require 'gb_web)
(require 'gb_define-word)

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(when (< emacs-major-version 24)
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/") t))

;; For the non-local packages, install them automatically.
;; This is handy for when a package has a lot of dependencies.
(when (connected-to-internet-p)
  (package-initialize)
  (package-refresh-contents)
  (use-package flycheck :ensure t :init (global-flycheck-mode))
  (use-package magit :ensure t)
  (use-package jedi :ensure t :init (progn (add-hook 'python-mode-hook 'jedi:setup)
                                           (jedi:install-server)))
  (use-package ein :ensure t :init (add-hook 'ein:connect-mode-hook 'ein:jedi-setup)))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-compression-mode t)
 '(auto-insert-mode t)
 '(auto-insert-query t)
 '(backup-directory-alist (list (cons ".*" BACKUPS-DIR)))
 '(blink-cursor-mode nil)
 '(column-number-mode t)
 '(comint-buffer-maximum-size 1024)
 '(custom-enabled-themes (quote (wombat)))
 '(custom-safe-themes (quote (wombat default)))
 '(custom-theme-directory "THEMES-DIR")
 '(dired-listing-switches
   (concat "-vAlhF"
           (if
               (eq system-type
                   (quote darwin))
               "" " --time-style=long-iso")))
 '(file-name-shadow-mode t)
 '(frame-background-mode (quote dark))
 '(gdb-many-windows t)
 '(gdb-speedbar-auto-raise t)
 '(global-font-lock-mode t)
 '(global-subword-mode t)
 '(history-delete-duplicates t)
 '(history-length 1000)
 '(ido-create-new-buffer (quote always))
 '(ido-enable-flex-matching t)
 '(ido-everywhere t)
 '(ido-mode (quote both) nil (ido))
 '(indent-tabs-mode nil)
 '(inhibit-startup-echo-area-message (getenv "USER"))
 '(inhibit-startup-screen t)
 '(initial-scratch-message
   ";;
;; The best way to predict the future is to invent it. -Alan Kay
;;

")
 '(jedi:complete-on-dot t)
 '(line-move-visual nil)
 '(make-backup-files t)
 '(mark-ring-max 23)
 '(menu-bar-mode nil)
 '(occur-hook (quote ((lambda nil (occur-rename-buffer t)))))
 '(org-agenda-files nil)
 '(org-catch-invisible-edits (quote smart) t)
 '(org-closed-keep-when-no-todo t t)
 '(org-completion-use-ido t)
 '(org-default-notes-file "~/notes.org")
 '(org-enforce-todo-checkbox-dependencies t t)
 '(org-enforce-todo-dependencies t t)
 '(org-fast-tag-selection-single-key t t)
 '(org-todo-keywords
   (quote
    ((sequence "TODO(t)" "BACKLOG(b)" "|" "DONE(d)")
     (sequence "WAITING(w@/!)" "|" "CANCELED(c@/!)"))) t)
 '(python-indent-offset 4)
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
 '(windmove-wrap-around t)
 '(x-stretch-cursor t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(region ((t (:background "sky blue" :foreground "black")))))
