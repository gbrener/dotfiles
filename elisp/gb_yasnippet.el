;;; gb_yasnippet.el ---                              -*- lexical-binding: t; -*-



(when (file-exists-p (concat ELISP-DIR "/yasnippet"))
  (add-to-list 'load-path (concat ELISP-DIR "/yasnippet"))
  (require 'yasnippet)
  (yas-global-mode 1))



(provide 'gb_yasnippet)
;;; gb_yasnippet.el ends here
