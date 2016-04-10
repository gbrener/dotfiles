;;; gb_yaml.el ---                                   -*- lexical-binding: t; -*-



(when (file-exists-p (concat ELISP-DIR "/yaml-mode/yaml-mode.el"))
  (add-to-list 'load-path (concat ELISP-DIR "/yaml-mode"))
  (require 'yaml-mode)
  (add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
  (add-to-list 'auto-mode-alist '("\\.yaml$" . yaml-mode)))



(provide 'gb_yaml)
;;; gb_yaml.el ends here
