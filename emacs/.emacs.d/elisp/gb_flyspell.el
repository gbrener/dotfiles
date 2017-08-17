;;; flyspell.el ---                                  -*- lexical-binding: t; -*-



;; Find the ispell directory
(setq ispell-program-name (if (file-exists-p "/usr/bin/aspell") "/usr/bin/aspell"
                            "/usr/local/bin/aspell"))

;; Activate flyspell-mode for .rst files, .md files, and Python comments/docstrings
(add-hook 'rst-mode 'flyspell-mode)
(add-hook 'markdown-mode 'flyspell-mode)



(provide 'gb_flyspell)
;;; flyspell.el ends here
