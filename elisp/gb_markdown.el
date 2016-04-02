;;; markdown.el ---                                  -*- lexical-binding: t; -*-



(when (file-exists-p (concat ELISP-DIR "markdown-mode/markdown-mode.el"))
  (require 'markdown-mode)
  (autoload 'markdown-mode "markdown-mode" 
    "Major mode for editing Markdown files" t)
  (add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
  (add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode)))



(provide 'gb_markdown)
;;; markdown.el ends here
