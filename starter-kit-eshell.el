;;; starter-kit-eshell.el --- Making the defaults a bit saner
;;
;; Part of the Emacs Starter Kit

(setq eshell-cmpl-cycle-completions nil
      eshell-save-history-on-exit t
      eshell-cmpl-dir-ignore (rx bos (or (and "." (opt "."))
                                         "CVS"
                                         ".svn"
                                         ".git")
                                 "/"
                                 eos))

(eval-after-load 'esh-opt
  '(progn
     (require 'em-prompt)
     (require 'em-term)
     (require 'em-cmpl)
     (setenv "PAGER" "cat")
     (set-face-attribute 'eshell-prompt nil :foreground "turquoise1")
     (add-hook 'eshell-mode-hook ;; for some reason this needs to be a hook
	       '(lambda () (define-key eshell-mode-map "\C-a" 'eshell-bol)))
     (add-to-list 'eshell-visual-commands "ssh")
     (add-to-list 'eshell-visual-commands "tail")
     (add-to-list 'eshell-command-completions-alist
                  `("gunzip" ,(rx "gz" eos)))
     (add-to-list 'eshell-command-completions-alist
                  `("tar" ,(rx (group (or ".tar" ".tgz" ".tar.gz" eos)))))
     (add-to-list 'eshell-output-filter-functions 'eshell-handle-ansi-color)))

(provide 'starter-kit-eshell)
;;; starter-kit-eshell.el ends here