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

;; LSP
(setq node-bin-dir "/home/greg/.nvm/versions/node/latest/bin")
(setenv "PATH" (concat node-bin-dir ":" (getenv "PATH")))
(setq exec-path (cons node-bin-dir exec-path))
(add-to-list 'load-path (expand-file-name "lib/lsp-mode" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "lib/lsp-mode/clients" user-emacs-directory))
(setq read-process-output-max (* 1024 1024)) ;; 1mb

;;; Encryption
(require 'epa-file)
(epa-file-enable)
(setq epa-file-select-keys nil)

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

(add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.css\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tsx?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsx?\\'" . web-mode))
(add-hook 'web-mode 'lsp)

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
 '(c-default-style '((c-mode . "python") (java-mode . "java") (other . "k&r")))
 '(column-number-mode t)
 '(comment-fill-column 110)
 '(company-idle-delay 0.1)
 '(company-minimum-prefix-length 1)
 '(custom-enabled-themes '(wombat))
 '(fill-column 110)
 '(gc-cons-threshold 100000000)
 '(ido-mode 'both nil (ido))
 '(indent-tabs-mode nil)
 '(lsp-file-watch-ignored-directories
   '("[/\\\\]\\.git\\'" "[/\\\\]\\.github\\'" "[/\\\\]\\.gitlab\\'" "[/\\\\]\\.circleci\\'" "[/\\\\]\\.hg\\'" "[/\\\\]\\.bzr\\'" "[/\\\\]_darcs\\'" "[/\\\\]\\.svn\\'" "[/\\\\]_FOSSIL_\\'" "[/\\\\]\\.idea\\'" "[/\\\\]\\.ensime_cache\\'" "[/\\\\]\\.eunit\\'" "[/\\\\]node_modules" "[/\\\\]\\.yarn\\'" "[/\\\\]\\.fslckout\\'" "[/\\\\]\\.tox\\'" "[/\\\\]\\.nox\\'" "[/\\\\]dist\\'" "[/\\\\]dist-newstyle\\'" "[/\\\\]\\.stack-work\\'" "[/\\\\]\\.bloop\\'" "[/\\\\]\\.metals\\'" "[/\\\\]target\\'" "[/\\\\]\\.ccls-cache\\'" "[/\\\\]\\.vscode\\'" "[/\\\\]\\venv\\'" "[/\\\\]\\.venv\\'" "[/\\\\]\\.mypy_cache\\'" "[/\\\\]\\.pytest_cache\\'" "[/\\\\]\\.build\\'" "[/\\\\]__pycache__\\'" "[/\\\\]\\.deps\\'" "[/\\\\]build-aux\\'" "[/\\\\]autom4te.cache\\'" "[/\\\\]\\.reference\\'" "[/\\\\]bazel-[^/\\\\]+\\'" "[/\\\\]\\.meta\\'" "[/\\\\]Library\\'" "[/\\\\]\\.lsp\\'" "[/\\\\]\\.clj-kondo\\'" "[/\\\\]\\.shadow-cljs\\'" "[/\\\\]\\.babel_cache\\'" "[/\\\\]\\.cpcache\\'" "[/\\\\]\\checkouts\\'" "[/\\\\]\\.gradle\\'" "[/\\\\]\\.m2\\'" "[/\\\\]bin/Debug\\'" "[/\\\\]obj\\'" "[/\\\\]_opam\\'" "[/\\\\]_build\\'" "[/\\\\]\\.elixir_ls\\'" "[/\\\\]\\.elixir-tools\\'" "[/\\\\]\\.terraform\\'" "[/\\\\]\\.terragrunt-cache\\'" "[/\\\\]\\result" "[/\\\\]\\result-bin" "[/\\\\]\\.direnv\\'"))
 '(lsp-idle-delay 0.1)
 '(lsp-tailwindcss-add-on-mode t)
 '(lsp-tailwindcss-rustywind-command (concat node-bin-dir "rustywind"))
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
   '(company lsp-pyright lsp-tailwindcss lsp-ui corfu lsp-mode tree-sitter-langs rust-mode ollama-buddy web-mode jinja2-mode))
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
 '(history-length 1000)
 '(ido-create-new-buffer (quote always))
 '(ido-enable-flex-matching t)
 '(ido-everywhere t)
 '(ido-use-filename-at-point (quote guess))
 '(indent-tabs-mode nil)
 '(inhibit-startup-echo-area-message (getenv "USER"))
 '(inhibit-startup-screen t)
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
