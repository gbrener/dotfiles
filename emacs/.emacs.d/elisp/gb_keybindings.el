;;; keybindings.el ---                               -*- lexical-binding: t; -*-



;; "C-x f" does a find-file-at-point
(ffap-bindings)

;; Just in case the Alt key isn't working, and we need M-x
(global-set-key (kbd "C-x C-m") 'execute-extended-command)

;; Org Mode settings
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)
(global-set-key "\C-cg" 'org-feed-update-all)
(global-set-key "\C-cG" 'org-feed-goto-inbox)

;; Remap C-M-spacebar and C-M-@ as "canonically-space-region or just-one-space", whichever is appropriate
(global-unset-key (kbd "C-M-SPC"))
(global-unset-key (kbd "C-M-@"))
(global-set-key (kbd "C-M-SPC")
                '(lambda ()
                   "Call `canonically-space-region' if region is active; otherwise calls `just-one-space'."
                   (interactive)
                   (call-interactively
                    (if (use-region-p)
			'canonically-space-region
                      'just-one-space))))

;; bind C-c + to 'increment-at-point'
(global-set-key (kbd "C-c +")
                '(lambda ()
                   "Increment number at point."
                   (interactive)
                   (skip-chars-backward "0123456789")
                   (or (looking-at "[0-9]+")
                       (error "No number at point"))
                   (replace-match (number-to-string (1+ (string-to-number (match-string 0)))))))

;; Remap query-replace-regexp to C-c r
(global-set-key "\C-cr" 'query-replace-regexp)

;; Set global keys for org-mode
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

(global-set-key (kbd "C-x R") 'recentf-open-files)



(provide 'gb_keybindings)
;;; keybindings.el ends here
