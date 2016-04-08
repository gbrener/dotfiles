;;; use-package.el ---                                       -*- lexical-binding: t; -*-



(when (file-exists-p (concat ELISP-DIR "/plugins/use-package"))
  (add-to-list 'load-path (concat ELISP-DIR "/plugins/use-package"))
  (require 'use-package))



(provide 'gb_use-package)
;;; use-package.el ends here
