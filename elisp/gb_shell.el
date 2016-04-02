;;; shell.el ---                                  -*- lexical-binding: t; -*-



;; Allow Linux commands that use the pager,
;; e.g. 'man', 'less', etc
;; to display their output like 'cat'.
(setenv "PAGER" "cat")


;; hook `abbrev-mode' to `comint-mode' and all its derived modes
(add-hook 'comint-mode-hook 'abbrev-mode)

;; turn off line numbers in `comint-mode' and all its derived modes
(add-hook 'comint-mode-hook '(lambda () (linum-mode -1)))

;; add ANSI colors to `comint-mode'
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
(add-to-list 'comint-output-filter-functions 'ansi-color-process-output)


;; Enable shell-mode
(shell)



(provide 'gb_shell)
;;; shell.el ends here
