;;; org.el ---                                       -*- lexical-binding: t; -*-



(setq org-catch-invisible-edits 'smart
      org-enforce-todo-dependencies t
      org-fast-tag-selection-include-todo t
      org-fast-tag-selection-single-key t
      org-agenda-dim-blocked-tasks t
      org-enforce-todo-checkbox-dependencies t
      org-closed-keep-when-no-todo t
      ;org-log-done 'note
      org-hierarchical-todo-statistics t
      org-todo-keywords '((sequence "TODO(t)" "BACKLOG(b)" "|" "DONE(d)")
                          (sequence "WAITING(w@/!)" "|" "CANCELED(c@/!)"))
      org-todo-keyword-faces '(("TODO" . org-warning)
                               ("DONE" :foreground "green" :weight bold)
                               ("BACKLOG" :foreground "blue" :weight bold)
                               ("WAITING" :foreground "yellow" :weight bold)
                               ("CANCELED" :foreground "green" :weight bold)))



(provide 'gb_org)
;;; org.el ends here
