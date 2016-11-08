;;; gb_org.el ---                                       -*- lexical-binding: t; -*-



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
                          (sequence "WAITING(w@/!)" "|" "CANCELED(c@/!)")
                          (sequence "|" "IDEA(i)"))
      org-todo-keyword-faces '(("TODO" . org-warning)
                               ("DONE" :foreground "green" :weight bold)
                               ("BACKLOG" :foreground "blue")
                               ("WAITING" :foreground "yellow" :weight bold)
                               ("CANCELED" :foreground "green" :weight bold)
                               ("IDEA" :foreground "orange")))


(defun org-summary-todo (n-done n-not-done)
  "Switch entry to DONE when all subentries are done, to TODO otherwise."
  (let (org-log-done org-log-states)   ; turn off logging
    (org-todo (if (= n-not-done 0) "DONE" "TODO"))))

(add-hook 'org-after-todo-statistics-hook 'org-summary-todo)



(provide 'gb_org)
;;; gb_org.el ends here
