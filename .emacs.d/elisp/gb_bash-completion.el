;;; gb_bash-completion.el ---                        -*- lexical-binding: t; -*-



(when (file-exists-p (concat PLUGINS-DIR "/bash-completion"))
  (add-to-list 'load-path (concat PLUGINS-DIR "/bash-completion"))
  (require 'bash-completion)
  (bash-completion-setup))




(provide 'gb_bash-completion)
;;; gb_bash-completion.el ends here
