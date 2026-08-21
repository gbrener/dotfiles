;;;;
;;;; gbrener's .emacs file
;;;; Created: December 23, 2012
;;;;

;;; Package repos
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; Notifications
(require 'notifications)

;;; Encryption
(require 'epa-file)
(epa-file-enable)
(setq epa-file-select-keys nil)

;; Eglot + Tree-sitter
(require 'eglot)
;(setq treesit-language-source-alist
;      '((c "https://github.com/tree-sitter/tree-sitter-c" "v0.20.7")
;        (cpp "https://github.com/tree-sitter/tree-sitter-cpp" "v0.20.5")
;        (python "https://github.com/tree-sitter/tree-sitter-cpp" "v0.20.4")
;        (java "https://github.com/tree-sitter/tree-sitter-java" "v0.20.2")
;        (html "https://github.com/tree-sitter/tree-sitter-html" "v0.23.2")
;        (css "https://github.com/tree-sitter/tree-sitter-css" "v0.25.0")
;        (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "v0.20.6" "typescript/src")
;        (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "v0.20.6" "tsx/src")))
(add-to-list 'eglot-server-programs '((c++-ts-mode) "clangd" "--query-driver=/usr/bin/clang++"))
(add-to-list 'eglot-server-programs '((c-ts-mode) "clangd" "--query-driver=/usr/bin/clang"))
(add-to-list 'eglot-server-programs '((python-ts-mode) . ("pyrefly" "lsp")))
(add-to-list 'eglot-server-programs '((java-ts-mode) . ("jdtls" "-configuration" "config_linux" "-data" "/tmp/.eglot-java-lsp-cache/jdtls")))
(add-to-list 'eglot-server-programs '((typescript-ts-mode tsx-ts-mode) . ("vtsls" "--stdio")))
(dolist (hook '(c++-ts-mode-hook
                c-ts-mode-hook
                python-ts-mode-hook
                java-ts-mode-hook
                html-ts-mode-hook
                css-ts-mode-hook
                typescript-ts-mode-hook
                tsx-ts-mode-hook))
  (add-hook hook 'eglot-ensure))

;(add-hook 'eglot-managed-mode-hook #'eldoc-box-hover-mode t)
(setq-default eglot-workspace-configuration
              '(:pylsp (:plugins (:flake8 (:enabled t)
                                  :pycodestyle (:enabled :json-false)
                                  :mccabe (:enabled :json-false)
                                  :pyflakes (:enabled :json-false))
                        :configurationSources ["flake8"])))

(define-key eglot-mode-map "\C-ca" #'eglot-code-actions)

;; Company
(require 'company)
(add-hook 'shell-mode-hook #'(lambda() (company-mode -1)))

;; Elfeed
(require 'elfeed)
(add-hook 'elfeed-new-entry-hook (elfeed-make-tagger :before "2 days ago" :remove 'unread))

;;; Advanced features
(put 'set-goal-column 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
;; Remap modifier keys if necessary
(when (eq system-type 'darwin)
  (setq mac-option-modifier 'super
        mac-command-modifier 'meta))
(setq y-or-n-p-use-read-key t)

;;; Keybindings
;; In case Alt doesn't work
(global-set-key "\C-x\C-m" #'execute-extended-command)

;; Org-mode
(define-key global-map "\C-ca" #'org-agenda)
(define-key global-map "\C-cc" #'org-capture)
(add-hook 'org-mode-hook 'flyspell-mode)

;; Org-roam
(setq package-install-upgrade-built-in t)
(setq org-roam-completion-everywhere t)
;;;(org-roam-setup)
(org-roam-db-autosync-mode)
(define-key global-map "\C-cnf" #'org-roam-node-find)
(define-key global-map "\C-cnr" #'org-roam-node-random)
(define-key org-mode-map "\C-cni" #'org-roam-node-insert)
(define-key org-mode-map "\C-cno" #'org-id-get-create)
(define-key org-mode-map "\C-cnt" #'org-roam-tag-add)
(define-key org-mode-map "\C-cna" #'org-roam-alias-add)
(define-key org-mode-map "\C-cnl" #'org-roam-buffer-toggle)
(define-key org-mode-map "\C-\M-i" #'completion-at-point)

;; Shell-mode
;(add-hook 'after-init-hook #'(lambda () (progn (setenv "PAGER" "cat") (shell))))
(require 'vterm)
(require 'multi-vterm)
(vterm)

;; Appearance
;;; TUI transparency
(defun my/transparent-tui-background (frame)
  (unless (display-graphic-p frame)
    (set-face-background 'default "unspecified-bg" frame)))
(add-hook 'after-make-frame-functions #'my/transparent-tui-background)
(my/transparent-tui-background (selected-frame))

;; Customizations
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-save-file-name-transforms
   '((".*\\([^/].*\\)" "/tmp/\\1" t) ("\\`/[^/]*:\\([^/]*/\\)*\\([^/]*\\)\\'" "/tmp/\\2" t)))
 '(backup-directory-alist '(("" . "~/.emacs.d/backups")))
 '(browse-url-browser-function 'eww-browse-url)
 '(c-default-style
   '((c-mode . "python") (c++-mode . "stroustrup") (java-mode . "java") (other . "k&r")))
 '(c-max-one-liner-length 112)
 '(column-number-mode t)
 '(comment-fill-column 112)
 '(company-backends
   '(company-bbdb company-semantic company-dabbrev company-cmake company-capf company-clang company-files
                  (company-dabbrev-code company-gtags company-etags company-keywords) company-oddmuse
                  company-dabbrev))
 '(company-dabbrev-code-other-buffers nil)
 '(company-idle-delay 0.0)
 '(company-minimum-prefix-length 1)
 '(create-lockfiles nil)
 '(custom-enabled-themes '(wombat))
 '(default-frame-alist '((vertical-scroll-bars) (alpha-background . 92)))
 '(delete-old-versions t)
 '(eglot-autoshutdown t)
 '(eglot-events-buffer-config '(:size 0 :format full))
 '(eglot-extend-to-xref t)
 '(elfeed-feeds
   '(("https://acoup.blog/feed/" history blog)
     ("http://feeds.feedburner.com/martinkl" dev blog)
     ("https://antirez.com/rss" dev blog)
     ("https://blog.nyman.re/feed.xml" dev blog)
     ("https://broadbandbreakfast.com/rss/" news)
     ("https://commoncog.com/rss/" startup blog)
     ("https://danluu.com/atom.xml" dev blog)
     ("https://dayvster.com/rss.xml" dev blog)
     ("https://eli.thegreenplace.net/feeds/all.atom.xml" dev blog)
     ("https://entropicthoughts.com/feed" dev blog)
     ("https://fabiensanglard.net/rss.xml" dev blog)
     ("https://feed.infoq.com/CPlusPlus/" dev news)
     ("https://feed.infoq.com/performance-scalability/" dev news)
     ("https://feed.infoq.com/python/" dev news)
     ("https://feeds.feedblitz.com/marginalrevolution&x=1" dev blog)
     ("https://feeds.feedburner.com/collabfund" startup blog)
     ("https://feeds.megaphone.fm/israelupdate" news)
     ("https://lemire.me/blog/feed/" dev blog)
     ("https://lukasatkinson.de/feed.atom.xml" dev blog)
     ("https://lunduke.substack.com/feed" dev blog)
     ("https://lwn.net/headlines/rss" dev news)
     ("https://mtlynch.io/posts/index.xml" dev startup blog)
     ("https://nationalpost.com/feed/atom" news)
     ("https://news.ycombinator.com/rss" dev startup news)
     ("https://nullprogram.com/feed/" dev blog)
     ("https://orlp.net/blog/atom.xml" dev blog)
     ("https://queue.acm.org/rss/feeds/computerarchitecture.xml" dev news)
     ("https://queue.acm.org/rss/feeds/distributedcomputing.xml" dev news)
     ("https://queue.acm.org/rss/feeds/opensource.xml" dev news)
     ("https://queue.acm.org/rss/feeds/processors.xml" dev news)
     ("https://queue.acm.org/rss/feeds/programminglanguages.xml" dev news)
     ("https://queue.acm.org/rss/feeds/queuecontent.xml" dev news)
     ("https://queue.acm.org/rss/feeds/searchengines.xml" dev news)
     ("https://queue.acm.org/rss/feeds/webservices.xml" dev news)
     ("https://quillette.com/articles/rss/" news)
     ("https://qz.com/rss" dev news)
     ("https://reasonablypolymorphic.com/atom.xml" dev blog)
     ("https://scottaaronson.blog/?feed=rss2" dev blog)
     ("https://steveblank.com/feed/" startup blog)
     ("https://techpolicy.press/rss/feed.xml" dev news)
     ("https://therecord.media/feed/" dev news)
     ("https://www.bleepingcomputer.com/feed/" dev news)
     ("https://www.bloomberg.com/authors/ARbTQlRLRjE/matthew-s-levine.rss" economics news)
     ("https://www.brendangregg.com/blog/rss.xml" dev blog)
     ("https://www.chabad.org/tools/rss/magazine_rss.xml" news)
     ("https://www.cppstories.com/index.xml" dev blog)
     ("https://www.ethanhein.com/wp/feed/" music blog)
     ("https://www.farnamstreetblog.com/feed/" startup blog)
     ("https://www.foreignaffairs.com/rss.xml" news)
     ("https://www.grantspub.com/adg.rss" economics news)
     ("https://www.infoinc.com/acm/TechNews.rss" dev news)
     ("https://www.jta.org/feed" news)
     ("https://www.kitces.com/blog/category/1-taxes/feed/" economics blog)
     ("https://www.libhunt.com/feed" dev news)
     ("https://www.moneymacro.rocks/feed.xml" economics blog)
     ("https://www.osnews.com/feed/" dev news)
     ("https://www.phoronix.com/linux/Programming" dev news)
     ("https://www.quantamagazine.org/feed/" math news)
     ("https://www.schneier.com/feed/atom/" dev security blog)
     ("https://www.techspot.com/backend.xml" dev news)
     ("https://www.tomshardware.com/feeds.xml" dev news)
     ("https://xkcd.com/atom.xml" comic)
     ("https://yosefk.com/blog/feed" dev blog)
     ("https://zeihan.com/feed/" economics blog)))
 '(fido-mode nil)
 '(fido-vertical-mode t)
 '(fill-column 112)
 '(gc-cons-threshold 100000000)
 '(global-company-mode t)
 '(indent-tabs-mode nil)
 '(major-mode-remap-alist
   '((c++-mode . c++-ts-mode) (c-mode . c-ts-mode) (c-or-c++-mode . c-or-c++-ts-mode)
     (python-mode . python-ts-mode)))
 '(menu-bar-mode nil)
 '(org-capture-templates
   '(("j" "Journal" entry (file "~/org/notes.org") "* %T: %^{Description}\12  %?" :prepend t)))
 '(org-pretty-entities t)
 '(org-refile-allow-creating-parent-nodes 'confirm)
 '(org-roam-capture-templates
   '(("d" "default" plain "%?" :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}") :unnarrowed t
      :kill-buffer t)))
 '(org-roam-completion-everywhere t)
 '(org-roam-db-autosync-mode t)
 '(org-startup-folded t)
 '(org-startup-with-latex-preview t)
 '(org-todo-interpretation 'sequence)
 '(org-todo-keywords
   '((sequence "TODO(t!)" "IN-PROGRESS(p!)" "|" "CANCELED(c!)" "DONE(d!)")))
 '(org-use-fast-tag-selection t)
 '(package-selected-packages
   '(transient org-ref org-roam org-roam-ui gradle-mode eldoc-box vterm eglot clang-format elfeed cmake-mode
               consult-eglot company rust-mode web-mode))
 '(scroll-bar-mode nil)
 '(tool-bar-mode nil)
 '(treemacs-space-between-root-nodes nil)
 '(url-privacy-level 'high)
 '(use-short-answers t)
 '(visible-bell t)
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
 )
