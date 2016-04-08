;;; org.el ---                                       -*- lexical-binding: t; -*-



(setq org-catch-invisible-edits 'smart
      org-enforce-todo-dependencies t
      org-closed-keep-when-no-todo t
      org-log-done 'note
      org-hierarchical-todo-statistics t
      org-todo-keywords '((sequence "TODO" "IN-PROGRESS" "PREEMPTED" "|" "DONE"))
      org-todo-keyword-faces '(("TODO" . org-warning)
                               ("IN-PROGRESS" . "yellow")
                               ("PREEMPTED" . "blue")))



(provide 'gb_org)
;;; org.el ends here
