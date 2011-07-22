;;; starter-kit-js.el --- Some helpful Javascript helpers
;;
;; Part of the Emacs Starter Kit


;; NB: js-mode is part of Emacs since version 23.2 (with an alias
;; javascript-mode). It is derived and updated from Espresso mode.

(if (< (string-to-number emacs-version) 23.2)
    (progn
      (autoload 'espresso-mode "espresso" "Start espresso-mode" t)
      (add-to-list 'auto-mode-alist '("\\.js$" . espresso-mode))
      (add-to-list 'auto-mode-alist '("\\.json$" . espresso-mode))
      (add-hook 'espresso-mode-hook 'moz-minor-mode)
      (add-hook 'espresso-mode-hook 'esk-paredit-nonlisp)
      (add-hook 'espresso-mode-hook 'run-coding-hook)
      (setq espresso-indent-level 2)

      ;; If you prefer js2-mode, use this instead:
      ;; (add-to-list 'auto-mode-alist '("\\.js$" . espresso-mode))

      (eval-after-load 'espresso
        '(progn (define-key espresso-mode-map "{" 'paredit-open-curly)
                (define-key espresso-mode-map "}" 'paredit-close-curly-and-newline)
                ;; fixes problem with pretty function font-lock
                (define-key espresso-mode-map (kbd ",") 'self-insert-command)
                (font-lock-add-keywords
                 'espresso-mode `(("\\(function *\\)("
                                   (0 (progn (compose-region (match-beginning 1)
                                                             (match-end 1) "ƒ")
                                             nil)))))))
      )

  (autoload 'js-mode "js" "Start js-mode" t)
  (add-to-list 'auto-mode-alist '("\\.js$" . js-mode))
  (add-to-list 'auto-mode-alist '("\\.json$" . js-mode))
  (add-hook 'js-mode-hook 'moz-minor-mode)
  (add-hook 'js-mode-hook 'esk-paredit-nonlisp)
  (add-hook 'js-mode-hook 'run-coding-hook)
  (setq js-indent-level 2)

  (eval-after-load 'js
    '(progn (define-key js-mode-map "{" 'paredit-open-curly)
            (define-key js-mode-map "}" 'paredit-close-curly-and-newline)
            ;; fixes problem with pretty function font-lock
            (define-key js-mode-map (kbd ",") 'self-insert-command)
            (font-lock-add-keywords
             'js-mode `(("\\(function *\\)("
                               (0 (progn (compose-region (match-beginning 1)
                                                         (match-end 1) "ƒ")
                                         nil))))))))

(provide 'starter-kit-js)
;;; starter-kit-js.el ends here
