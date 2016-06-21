;;; gb_web.el ---                               -*- lexical-binding: t; -*-


(when (file-exists-p (concat ELISP-DIR "/web-mode"))
  (add-to-list 'load-path (concat ELISP-DIR "/web-mode"))
  (require 'web-mode)
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode)))



(provide 'gb_web)
;;; gb_web.el ends here
