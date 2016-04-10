;;; erlang.el ---                                    -*- lexical-binding: t; -*-



;; Add erlang-mode to path
(when (file-exists-p "/usr/lib/erlang/bin")
  (let* ((erlang-dir "/usr/lib/erlang")
         (erlang-bin-dir (concat erlang-dir "/bin"))
         (erlang-etools-dir (shell-command-to-string (concat "find " erlang-dir " -type d -name \"emacs\" -print0 -quit"))))
    ;; Remove trailing null character
    (setq erlang-etools-dir (replace-regexp-in-string "\0$" "" erlang-etools-dir))
    (add-to-list 'load-path erlang-etools-dir)
    (setq erlang-root-dir erlang-dir)
    (add-to-list 'exec-path erlang-bin-dir)
    (require 'erlang-start))

  (add-to-list 'auto-mode-alist '("\\.erl\\'" . erlang-mode)))



(provide 'gb_erlang)
;;; erlang.el ends here
