;;; starter-kit-perl.el --- Some helpful Perl code
;;
;; Part of the Emacs Starter Kit

(eval-after-load 'cperl-mode
  '(progn
     (define-key cperl-mode-map (kbd "RET") 'reindent-then-newline-and-indent)
     (define-key cperl-mode-map (kbd "C-M-h") 'backward-kill-word)))

(define-key 'help-command "P" 'perldoc)

(add-to-list 'auto-mode-alist '("\\.p[lm]$" . cperl-mode))
(add-to-list 'auto-mode-alist '("\\.pod$" . pod-mode))
(add-to-list 'auto-mode-alist '("\\.tt$" . tt-mode))

;; TODO: flymake
;; TODO: electric bugaloo 

(provide 'starter-kit-perl)
;; starter-kit-perl.el ends here
