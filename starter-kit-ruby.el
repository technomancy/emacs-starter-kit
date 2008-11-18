;;; starter-kit-ruby.el --- Some helpful Ruby code
;;
;; Part of the Emacs Starter Kit


;; TODO: move this stuff to autoloads?
(require 'ruby-mode)
(require 'ruby-compilation)

(defun rr (&optional arg)
  "Run a Ruby interactive shell session in a buffer."
  (interactive "P")
  (let ((impl (if (not arg)
                  "mri"
                (completing-read "Ruby Implementation: "
                                 '("ruby" "jruby" "rubinius" "yarv")))))
    (run-ruby (cdr (assoc impl '(("mri" . "irb")
                                 ("jruby" . "jruby -S irb")
                                 ("rubinius" . "rbx")
                                 ("yarv" . "irb1.9")))))
    (with-current-buffer "*ruby*"
      (rename-buffer (format "*%s*" impl) t))))

(define-key ruby-mode-map (kbd "C-M-h") 'backward-kill-word)
(define-key ruby-mode-map (kbd "RET") 'ruby-reindent-then-newline-and-indent)
(define-key ruby-mode-map (kbd "C-c l") (lambda ()
                                          (interactive) (insert "lambda")))

(global-set-key (kbd "C-h r") 'ri)

;; We never want to edit Rubinius bytecode
(add-to-list 'completion-ignored-extensions ".rbc")

;; TODO: set up ri
;; TODO: electric
;; TODO: flymake

(provide 'starter-kit-ruby)
;; starter-kit-ruby.el ends here