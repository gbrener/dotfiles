;;; gb_json.el ---                                   -*- lexical-binding: t; -*-



(when (file-exists-p (concat ELISP-DIR "/json-mode"))
  (add-to-list 'load-path (concat ELISP-DIR "/json-mode"))
  (require 'json-mode)
  (add-to-list 'auto-mode-alist '("\\.json\\'" . json-mode)))



(provide 'gb_json)
;;; gb_json.el ends here
