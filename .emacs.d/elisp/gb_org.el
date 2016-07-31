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


;; (custom-name feed-url org-file-path headline)
(setq org-feed-alist
      '(("ACL-Live" "http://acltv.com/news/feed/" "~/feeds.org" "ACL-Live")
        ("Yossi Krenin's blog" "http://yosefk.com/blog/feed" "~/feeds.org" "Yossi Krenin's blog")
        ("Dan Luu's blog" "http://danluu.com/atom.xml" "~/feeds.org" "Dan Luu's blog")
        ("Dan McFunley's blog" "http://mcfunley.com/feed/rss" "~/feeds.org" "Dan McFunley's blog")
        ("Havoc Pennington's blog" "http://blog.ometer.com/feed/" "~/feeds.org" "Havoc Pennington's blog")
        ("Steven Sinofsky's blog" "http://feeds.feedburner.com/LearningByShipping" "~/feeds.org" "Steven Sinofsky's blog")
        ("Fred Hebert's blog" "http://ferd.ca/feed.rss" "~/feeds.org" "Fred Hebert's blog (from Learn You Some Erlang)")
        ("Entropy Coordinator" "http://feeds.feedburner.com/EntropyCoordinator" "~/feeds.org" "Vern Eastley's blog")
        ("Lean Crew" "http://leancrew.com/all-this/feed/" "~/feeds.org" "guy who wrote an aggregator like mine")
        ("Programming is terrible" "http://programmingisterrible.com/rss" "~/feeds.org" "Humorous blog")
        ("Gustavo Duarte's blog" "http://feeds.feedburner.com/GustavoDuarte" "~/feeds.org" "Gustavo Duarte's blog")
        ("Max Schireson's blog" "https://maxschireson.com/feed/" "~/feeds.org" "ex-CEO of MongoDB's blog")
        ("Scott W Harden's blog" "http://www.swharden.com/wp/feed/atom/" "~/feeds.org" "Scott W Harden's blog")
        ("ProPublica" "http://feeds.propublica.org/propublica/main" "~/feeds.org" "ProPublica")
        ("The Technium" "http://feedpress.me/thetechnium" "~/feeds.org" "The Technium")
        ("Giles Bowkett's blog" "http://gilesbowkett.blogspot.com/feeds/posts/default" "~/feeds.org" "Giles Bowkett's blog")
        ("Isaac Slavitt's blog" "http://isaacslavitt.com/all.atom.xml" "~/feeds.org" "Isaac Slavitt's blog (Data Science)")
        ;("Tony Arcieri's blog" "http://tonyarcieri.com/feed" "~/feeds.org" "Tony Arcieri's blog")
        ("Slashdot" "http://rss.slashdot.org/Slashdot/slashdot" "~/feeds.org" "Slashdot")
        ("Math Babe" "https://mathbabe.org/feed/" "~/feeds.org" "Math Babe")
        ("Techdirt" "http://feeds.feedburner.com/techdirt/feed" "~/feeds.org" "Techdirt news")
        ("Techdirt (Innovation)" "https://www.techdirt.com/blog/innovation/techdirt_rss.xml" "~/feeds.org" "Techdirt innovation news")
        ("HPCWire" "https://www.hpcwire.com/feed/" "~/feeds.org" "High Performance Computing news")
        ("MIT Tech Review (BioMed)" "https://www.technologyreview.com/c/biomedicine/rss/" "~/feeds.org" "MIT Tech Review - biomedicine")
        ("MIT Tech Review (Business)" "https://www.technologyreview.com/c/business/rss/" "~/feeds.org" "MIT Tech Review - business")
        ("MIT Tech Review (Computing)" "https://www.technologyreview.com/c/computing/rss/" "~/feeds.org" "MIT Tech Review - computing")
        ("MIT Tech Review (Energy)" "https://www.technologyreview.com/c/energy/rss/" "~/feeds.org" "MIT Tech Review - energy")
        ("MIT Tech Review (Robotics)" "https://www.technologyreview.com/c/robotics/rss/" "~/feeds.org" "MIT Tech Review - robotics")
        ("MIT Tech Review (Top News)" "https://www.technologyreview.com/topnews.rss" "~/feeds.org" "MIT Tech Review - top news")
        ("Michael Feathers' blog" "https://michaelfeathers.silvrback.com/feed" "~/feeds.org" "Michael Feathers' blog")
        )
      )



(provide 'gb_org)
;;; org.el ends here
