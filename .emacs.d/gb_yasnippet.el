;;; gb_yasnippet.el ---                              -*- lexical-binding: t; -*-



(when (file-exists-p (concat ELISP-DIR "/plugins/yasnippet"))
  (add-to-list 'load-path (concat ELISP-DIR "/plugins/yasnippet"))
  (require 'yasnippet)
  (setq yas-snippet-dirs (concat ELISP-DIR "/plugins/yasnippet/snippets"))
  (yas-global-mode 1))



(provide 'gb_yasnippet)
;;; gb_yasnippet.el ends here
