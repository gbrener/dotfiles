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

;; GPTel
(setq gptel-model 'qwen3:32b
      gptel-backend (gptel-make-ollama "Ollama"
                      :host "localhost:11434"
                      :stream t
                      :models '(qwen3:32b codellama:34b)))

;;; Encryption
(require 'epa-file)
(epa-file-enable)
(setq epa-file-select-keys nil)

;; Eglot
(setq-default eglot-workspace-configuration
              '(:pylsp (:plugins (:flake8 (:enabled t)
                                  :pycodestyle (:enabled :json-false)
                                  :mccabe (:enabled :json-false)
                                  :pyflakes (:enabled :json-false))
                        :configurationSources ["flake8"])))

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

;; Web mode
(add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.css\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tsx?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsx?\\'" . web-mode))

;; Starting modes
; shell-mode
(setenv "PAGER" "cat")
(shell)

;; Customizations
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-save-file-name-transforms
   '((".*\\([^/].*\\)" "/tmp/\\1" t)
     ("\\`/[^/]*:\\([^/]*/\\)*\\([^/]*\\)\\'" "/tmp/\\2" t)))
 '(backup-directory-alist '(("" . "~/.emacs.d/backups")))
 '(c-default-style
   '((c-mode . "python")
     (c++-mode . "stroustrup")
     (java-mode . "java")
     (other . "k&r")))
 '(column-number-mode t)
 '(comment-fill-column 112)
 '(company-idle-delay 0.0)
 '(company-minimum-prefix-length 1)
 '(custom-enabled-themes '(wombat))
 '(eglot-autoshutdown t)
 '(eglot-extend-to-xref t)
 '(fido-mode nil)
 '(fido-vertical-mode t)
 '(fill-column 112)
 '(gc-cons-threshold 100000000)
 '(indent-tabs-mode nil)
 '(menu-bar-mode nil)
 '(org-capture-templates
   '(("j" "Journal" entry
      (file "~/org/notes.org")
      "* %T: %^{Description}\12  %?" :prepend t)))
 '(org-pretty-entities t)
 '(org-refile-allow-creating-parent-nodes 'confirm)
 '(org-startup-folded t)
 '(org-startup-with-latex-preview t)
 '(org-todo-interpretation 'sequence)
 '(org-todo-keywords
   '((sequence "TODO(t!)" "IN-PROGRESS(p!)" "|" "CANCELED(c!)" "DONE(d!)")))
 '(org-use-fast-tag-selection t)
 '(package-selected-packages
   '(consult-eglot eglot company gptel tree-sitter-langs rust-mode web-mode))
 '(scroll-bar-mode nil)
 '(tool-bar-mode nil)
 '(treemacs-space-between-root-nodes nil)
 '(visual-line-fringe-indicators '(left-curly-arrow right-curly-arrow))
 '(web-mode-code-indent-offset 2)
 '(web-mode-css-indent-offset 2)
 '(web-mode-markup-indent-offset 2))
 '(dired-listing-switches
   (concat "-vAlhF"
           (if
               (eq system-type
                   (quote darwin))
               "" " --time-style=long-iso")))
 '(frame-background-mode (quote dark))
 '(global-subword-mode t)
 '(history-delete-duplicates t)
 '(history-length 10000)
 '(indent-tabs-mode nil)
 '(inhibit-startup-echo-area-message (getenv "USER"))
 '(inhibit-startup-screen t)
 '(python-indent-offset 4)
 '(tramp-default-method "ssh")
 '(version-control (quote never))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :extend nil :stipple nil :background "#242424" :foreground "#f6f3e8" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 94 :width normal :family "Monospace"))))
 '(mode-line ((t (:background "lime green" :foreground "black" :height 1.0 :family "Monospace"))))
 '(mode-line-inactive ((t (:background "DarkSeaGreen4" :foreground "black" :height 1.0 :family "Monospace")))))
