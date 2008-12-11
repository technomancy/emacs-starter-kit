;;; starter-kit-js.el --- Some helpful Javascript helpers
;;
;; Part of the Emacs Starter Kit

(eval-after-load 'js2-mode
  '(progn

     ;; Cosmetics
     (font-lock-add-keywords
      'js2-mode `(("\\(function *\\)("
                   (0 (progn (compose-region (match-beginning 1) (match-end 1)
                                             ,(make-char 'greek-iso8859-7 107))
                             nil)))))

     (font-lock-add-keywords
      'js2-mode
      '(("\\<\\(FIX\\|TODO\\|FIXME\\|HACK\\|REFACTOR\\):"
         1 font-lock-warning-face t)))

     (defun js-lambda () (interactive) (insert "function () {\n};")
       (backward-char 6))

     (define-key js2-mode-map (kbd "C-c l") 'js-lambda)
     (define-key js2-mode-map "\C-\M-h" 'backward-kill-word)

     (add-hook 'js2-mode-hook 'coding-hook)
     (setq js2-bounce-indent-flag nil
           js2-indent-on-enter-key t)))

(provide 'starter-kit-js)
;;; starter-kit-js.el ends here