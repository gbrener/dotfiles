;;;;
;;;; gbrener's .emacs file
;;;; Created: December 23, 2012
;;;;

;;; Package repos
(require 'package)
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(when (< emacs-major-version 24)
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/") t))
(package-initialize)

;;; Encryption
(require 'epa-file)
(epa-file-enable)
(setq epa-file-select-keys nil)

;; There's no easier way to customize this (that I know of)
(setq c-default-style "linux")
;; Use "y or n" for answers instead of "yes or no"
(fset 'yes-or-no-p 'y-or-n-p)

;;; Advanced features
(put 'set-goal-column 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
;; Remap modifier keys if necessary
(when (eq system-type 'darwin)
  (setq mac-option-modifier 'super
        mac-command-modifier 'meta))

;;; Keybindings
;; In case Alt doesn't work
(global-set-key "\C-x\C-m" #'execute-extended-command)

;; Org-mode
(define-key global-map "\C-ca" #'org-agenda)
(define-key global-map "\C-cc" #'org-capture)

;; Starting modes
; shell-mode
(shell)

;; Customizations
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes '(wombat))
 '(ido-mode 'both #'nil (ido))
 '(indent-tabs-mode nil)
 '(menu-bar-mode nil)
 '(org-agenda-files '("~/org"))
 '(org-capture-templates
   '(("t" "TODO entry" entry
      (file+headline
       (lambda nil
         (concat org-directory "/todos.org"))
       "Capture")
      "* TODO %^{Description} %^g
  %?" :empty-lines-after 1)
     ("j" "Journal entry" entry
      (file
       (lambda nil
         (concat org-directory "/journal.org")))
      "* DONE %t: %^{Description}
  %?" :prepend t :empty-lines-before 1)
     ("c" "Company (or Project) idea" entry
      (file+headline org-default-notes-file "Projects")
      "" :empty-lines-after 1)
     ("b" "Book notes" entry
      (file+headline org-default-notes-file "Books")
      "" :empty-lines-after 1)
     ("p" "Podcast notes" entry
      (file+headline org-default-notes-file "Podcasts")
      "" :empty-lines-after 1)))
 '(org-default-notes-file (concat org-directory "/notes.org"))
 '(org-directory "~/org")
 '(package-selected-packages
   '(gnuplot typescript-mode flx-ido python-black company-web company-erlang org-projectile))
 '(column-number-mode t)
 '(comint-buffer-maximum-size 1024)
 '(comint-password-prompt-regexp (concat comint-password-prompt-regexp "\\|^Password for "))
 '(scroll-bar-mode nil)
 '(tab-always-indent t)
 '(tool-bar-mode nil))
 '(dired-listing-switches
   (concat "-vAlhF"
           (if
               (eq system-type
                   (quote darwin))
               "" " --time-style=long-iso")))
 '(fill-column 108)
 '(frame-background-mode (quote dark))
 '(global-subword-mode t)
 '(history-delete-duplicates t)
 '(history-length 1000)
 '(ido-create-new-buffer (quote always))
 '(ido-enable-flex-matching t)
 '(ido-everywhere t)
 '(ido-use-filename-at-point (quote guess))
 '(indent-tabs-mode nil)
 '(inhibit-startup-echo-area-message (getenv "USER"))
 '(inhibit-startup-screen t)
 '(line-move-visual nil)
 '(org-completion-use-ido t)
 '(python-indent-offset 4)
 '(tramp-default-method "ssh")
 '(version-control (quote never))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
