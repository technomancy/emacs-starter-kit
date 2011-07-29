;; Make Emacs feel like home

(setq viper-expert-level '5)
(setq viper-inhibit-startup-message 't)
(setq viper-mode t)

(setq viper-ESC-keyseq-timeout 0)
(setq viper-fast-keyseq-timeout 0)
(setq viper-auto-indent nil)

(require 'viper)
(require 'vimpulse)

(eval-after-load 'vimpulse
  '(progn
     (setq viper-emacs-state-id (concat (propertize "<E>" 'face 'hi-red-b) " "))
     (put 'viper-mode-string 'risky-local-variable t)))

(add-hook 'erlang-mode 'viper-mode)
(add-hook 'org-mode 'viper-mode)
(add-hook 'clojure-mode 'viper-mode)

(provide 'my-vim)
