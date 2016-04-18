;;; gb_annotate.el ---                               -*- lexical-binding: t; -*-



(when (file-exists-p (concat ELISP-DIR "/annotate-mode"))
  (add-to-list 'load-path (concat ELISP-DIR "/annotate-mode"))
  (require 'annotate-mode))




(provide 'gb_annotate)
;;; gb_annotate.el ends here
