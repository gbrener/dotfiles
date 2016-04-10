;;; gb_yasnippet.el ---                              -*- lexical-binding: t; -*-



(when (file-exists-p (concat PLUGINS-DIR "/yasnippet"))
  (add-to-list 'load-path (concat PLUGINS-DIR "/yasnippet"))
  (require 'yasnippet)
  (setq yas-snippet-dirs '("~/.emacs.d/snippets"))
  (yas-global-mode 1))



(provide 'gb_yasnippet)
;;; gb_yasnippet.el ends here
