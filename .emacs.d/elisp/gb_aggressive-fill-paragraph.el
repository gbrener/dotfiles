;;; gb_aggressive-fill-paragraph.el ---              -*- lexical-binding: t; -*-



(when (file-exists-p (concat ELISP-DIR "/aggressive-fill-paragraph-mode"))
  (add-to-list 'load-path (concat ELISP-DIR "/aggressive-fill-paragraph-mode"))
  (require 'aggressive-fill-paragraph)
  (afp-setup-recommended-hooks))



(provide 'gb_aggressive-fill-paragraph)
;;; gb_aggressive-fill-paragraph.el ends here
