;;; gb_define-word.el ---                            -*- lexical-binding: t; -*-



(when (file-exists-p (concat PLUGINS-DIR "/define-word/define-word.el"))
  (add-to-list 'load-path (concat PLUGINS-DIR "/define-word"))
  (require 'define-word)



(provide 'gb_define-word)
;;; gb_define-word.el ends here
