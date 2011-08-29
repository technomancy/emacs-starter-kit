;;; starter-kit-js.el --- Some helpful Javascript helpers
;;
;; Part of the Emacs Starter Kit

;; NB: js-mode is part of Emacs since version 23.2 (with an alias
;; javascript-mode). It is derived and updated from Espresso mode.

(defvar esk-js-mode-hook nil)
(defun run-esk-js-mode-hook ()
  (run-hooks 'esk-js-mode-hook))

(defmacro esk-configure-javascript (name)
  (let ((sym (intern name))
        (mode (intern (concat name "-mode")))
        (hook (intern (concat name "-mode-hook")))
        (keymap (intern (concat name "-mode-map")))
        (indent (intern (concat name "-indent-level"))))
    `(progn
       (autoload ',mode ,name ,(concat "Start " name "-mode") t)
       (add-to-list 'auto-mode-alist '("\\.js$" . ,mode))
       (add-to-list 'auto-mode-alist '("\\.json$" . ,mode))
       (add-hook ',hook 'moz-minor-mode)
       (add-hook ',hook 'esk-paredit-nonlisp)
       (add-hook ',hook 'run-coding-hook)
       (add-hook ',hook 'run-esk-js-mode-hook)
       (setq ,indent 2)

       (eval-after-load ',sym
         '(progn (define-key ,keymap "{" 'paredit-open-curly)
                 (define-key ,keymap "}" 'paredit-close-curly-and-newline)
                 (define-key ,keymap (kbd ",") 'self-insert-command))))))

(defun pretty-functions ()
  (font-lock-add-keywords
   nil `(("\\(function *\\)("
          (0 (progn (compose-region (match-beginning 1)
                                    (match-end 1) "ƒ")
                    nil))))))
(add-hook 'esk-js-mode-hook 'pretty-functions)

(if (< (string-to-number emacs-version) 23.2)
    (esk-configure-javascript "espresso")
  (esk-configure-javascript "js"))

(provide 'starter-kit-js)
;;; starter-kit-js.el ends here
