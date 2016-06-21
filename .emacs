;;;;
;;;; gbrener's .emacs file
;;;; Created: December 23, 2012
;;;;


(defconst ELISP-DIR  "~/.emacs.d/elisp"
  "Emacs library directory")

(defconst PLUGINS-DIR
  "~/.emacs.d/plugins"
  "Emacs plugins directory (elisp files that don't have 'modes')")

(defconst THEMES-DIR
  "~/.emacs.d/themes/"
  "Emacs themes directory")

;; Create the elisp and plugins directories if they don't already exist
(when (not (file-exists-p ELISP-DIR))
  (make-directory ELISP-DIR t))
(when (not (file-exists-p PLUGINS-DIR))
  (make-directory PLUGINS-DIR t))

;; Add elisp directory to load-path
(add-to-list 'load-path ELISP-DIR)

;; Add themes direcory to load-path
(add-to-list 'custom-theme-load-path THEMES-DIR)

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
  (use-package magit :ensure t)
  (use-package ein :ensure t))

;; Load theme
(load-theme 'wombat t)
;(load-theme 'badwolf t)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-compression-mode t)
 '(auto-insert-mode t)
 '(auto-insert-query t)
 '(backup-directory-alist (quote ((".*" . "~/.emacs.d/backups"))))
 '(blink-cursor-mode nil)
 '(column-number-mode t)
 '(comint-buffer-maximum-size 1024)
 '(dired-listing-switches
   (concat "-vAlhF"
           (if
               (eq system-type
                   (quote darwin))
               "" " --time-style=long-iso")))
 '(file-name-shadow-mode t)
 '(frame-background-mode (quote dark))
 '(global-font-lock-mode t)
 '(global-subword-mode t)
 '(history-delete-duplicates t)
 '(history-length 1000)
 '(ido-create-new-buffer (quote always))
 '(ido-enable-flex-matching t)
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
 '(make-backup-files t)
 '(mark-ring-max 23)
 '(menu-bar-mode nil)
 '(occur-hook (quote ((lambda nil (occur-rename-buffer t)))))
 '(org-agenda-files nil)
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
