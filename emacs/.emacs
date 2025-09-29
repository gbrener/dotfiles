;;;;
;;;; gbrener's .emacs file
;;;; Created: December 23, 2012
;;;;

;; Package repos
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; Notifications
(require 'notifications)

;; Encryption
(require 'epa-file)
(epa-file-enable)
(setq epa-file-select-keys nil)

;; Eglot + Tree-sitter
(require 'eglot)
(setq treesit-language-source-alist
      '((c "https://github.com/tree-sitter/tree-sitter-c" "v0.20.7")
        (cpp "https://github.com/tree-sitter/tree-sitter-cpp" "v0.20.5")
        (python "https://github.com/tree-sitter/tree-sitter-cpp" "v0.20.4")))
(add-to-list 'eglot-server-programs '((c++-ts-mode c++-mode) "clangd" "--query-driver=/usr/bin/clang++"))
(add-to-list 'eglot-server-programs '((c-ts-mode c-mode) "clangd" "--query-driver=/usr/bin/clang"))
(add-to-list 'eglot-server-programs '((python-ts-mode python-mode) . ("/home/greg/Desktop/sources/projects/triton/venv/bin/pyrefly" "lsp")))
(dolist (hook '(c++-ts-mode-hook
                c++-mode-hook
                c-ts-mode-hook
                c-mode-hook
                python-ts-mode-hook
                python-mode-hook))
  (add-hook hook 'eglot-ensure))
(setq-default eglot-workspace-configuration
              '(:pylsp (:plugins (:flake8 (:enabled t)
                                  :pycodestyle (:enabled :json-false)
                                  :mccabe (:enabled :json-false)
                                  :pyflakes (:enabled :json-false))
                        :configurationSources ["flake8"])))

;; Company
(require 'company)
(add-hook 'shell-mode-hook (lambda()
                             (setq-local global-company-mode nil)
                             (company-mode -1)))

;; Elfeed
(require 'elfeed)
(add-hook 'elfeed-new-entry-hook (elfeed-make-tagger :before "2 days ago" :remove 'unread))

;; Advanced features
(put 'set-goal-column 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
;; Remap modifier keys if necessary
(when (eq system-type 'darwin)
  (setq mac-option-modifier 'super
        mac-command-modifier 'meta))
(setq y-or-n-p-use-read-key t)

;; Keybindings
;; In case Alt doesn't work
(global-set-key "\C-x\C-m" #'execute-extended-command)

;; Org-mode
(define-key global-map "\C-ca" #'org-agenda)
(define-key global-map "\C-cc" #'org-capture)
(add-hook 'org-mode-hook 'flyspell-mode)

;; Web mode
(add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.css\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tsx?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsx?\\'" . web-mode))

;; Shell-mode
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
 '(browse-url-browser-function 'eww-browse-url)
 '(c-default-style
   '((c-mode . "python")
     (c++-mode . "stroustrup")
     (java-mode . "java")
     (other . "k&r")))
 '(c-max-one-liner-length 112)
 '(column-number-mode t)
 '(comment-fill-column 112)
 '(company-idle-delay 0.0)
 '(company-minimum-prefix-length 1)
 '(create-lockfiles nil)
 '(custom-enabled-themes '(wombat))
 '(delete-old-versions t)
 '(eglot-autoshutdown t)
 '(eglot-extend-to-xref t)
 '(elfeed-feeds
   '(("https://www.quantamagazine.org/feed/" math news)
     ("https://www.techspot.com/backend.xml" dev news)
     ("https://www.tomshardware.com/feeds.xml" dev news)
     ("https://lwn.net/headlines/rss" dev news)
     ("https://techpolicy.press/rss/feed.xml" dev news)
     ("https://news.ycombinator.com/rss" dev startup news)
     ("https://www.libhunt.com/feed" dev news)
     ("https://qz.com/rss" dev news)
     ("https://queue.acm.org/rss/feeds/searchengines.xml" dev news)
     ("https://queue.acm.org/rss/feeds/webservices.xml" dev news)
     ("https://queue.acm.org/rss/feeds/programminglanguages.xml" dev news)
     ("https://queue.acm.org/rss/feeds/processors.xml" dev news)
     ("https://queue.acm.org/rss/feeds/opensource.xml" dev news)
     ("https://queue.acm.org/rss/feeds/distributedcomputing.xml" dev news)
     ("https://queue.acm.org/rss/feeds/computerarchitecture.xml" dev news)
     ("https://queue.acm.org/rss/feeds/queuecontent.xml" dev news)
     ("https://www.infoinc.com/acm/TechNews.rss" dev news)
     ("https://feed.infoq.com/performance-scalability/" dev news)
     ("https://feed.infoq.com/CPlusPlus/" dev news)
     ("https://feed.infoq.com/python/" dev news)
     ("https://broadbandbreakfast.com/rss/" news)
     ("https://www.moneymacro.rocks/feed.xml" economics blog)
     ("https://zeihan.com/feed/" economics blog)
     ("https://lemire.me/blog/feed/" dev blog)
     ("https://danluu.com/atom.xml" dev blog)
     ("https://eli.thegreenplace.net/feeds/all.atom.xml" dev blog)
     ("https://www.schneier.com/feed/atom/" dev security blog)
     ("https://mtlynch.io/posts/index.xml" dev startup blog)
     ("https://yosefk.com/blog/feed" dev blog)
     ("https://steveblank.com/feed/" startup blog)
     ("https://lukasatkinson.de/feed.atom.xml" dev blog)
     ("https://nullprogram.com/feed/" dev blog)
     ("https://www.brendangregg.com/blog/rss.xml" dev blog)
     ("https://antirez.com/rss" dev blog)
     ("https://fabiensanglard.net/rss.xml" dev blog)
     ("https://reasonablypolymorphic.com/atom.xml" dev blog)
     ("https://orlp.net/blog/atom.xml" dev blog)
     ("https://lunduke.substack.com/feed" dev blog)
     ("http://feeds.feedburner.com/collabfund" startup blog)
     ("https://commoncog.com/rss/" startup blog)
     ("https://www.farnamstreetblog.com/feed/" startup blog)
     ("https://acoup.blog/feed/" history blog)
     ("https://xkcd.com/atom.xml" comic)))
 '(fido-mode nil)
 '(fido-vertical-mode t)
 '(fill-column 112)
 '(gc-cons-threshold 100000000)
 '(global-company-mode t)
 '(indent-tabs-mode nil)
 '(major-mode-remap-alist
   '((c++-mode . c++-ts-mode)
     (c-mode . c-ts-mode)
     (c-or-c++-mode . c-or-c++-ts-mode)
     (python-mode . python-ts-mode)))
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
   '(eldoc-box vterm eglot clang-format elfeed cmake-mode consult-eglot company rust-mode web-mode))
 '(scroll-bar-mode nil)
 '(tool-bar-mode nil)
 '(treemacs-space-between-root-nodes nil)
 '(url-privacy-level 'high)
 '(use-short-answers t)
 '(visual-line-fringe-indicators '(left-curly-arrow right-curly-arrow))
 '(vterm-min-window-width 4)
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
