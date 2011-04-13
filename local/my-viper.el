;; Make Emacs feel like home

(setq viper-expert-level '5)
(setq viper-inhibit-startup-message 't)
(setq viper-mode t)

(setq viper-auto-indent nil)

(require 'viper)
(require 'vimpulse)

(eval-after-load 'vimpulse
  '(progn
     ;; (setq viper-vi-state-id (concat (propertize "<V>" 'face 'hi-blue-b) " "))
     (setq viper-emacs-state-id (concat (propertize "<E>" 'face 'hi-red-b) " "))
     ;; (setq viper-insert-state-id (concat (propertize "<I>" 'face 'hi-blue-b) " "))
     ;; (setq viper-replace-state-id (concat (propertize "<R>" 'face 'hi-blue-b) " "))
     ;; The property `risky-local-variable' is a security measure
     ;; for mode line variables that have properties
     (put 'viper-mode-string 'risky-local-variable t)))

(add-hook 'erlang-mode 'viper-mode)
(add-hook 'org-mode 'viper-mode)

(provide 'my-viper)
