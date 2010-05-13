;;; starter-kit-eshell.el --- Making the defaults a bit saner
;;
;; Part of the Emacs Starter Kit

(setq eshell-cmpl-cycle-completions nil
      eshell-save-history-on-exit t
      eshell-cmpl-dir-ignore "\\`\\(\\.\\.?\\|CVS\\|\\.svn\\|\\.git\\)/\\'")

(eval-after-load 'esh-opt
  '(progn
     (require 'em-prompt)
     (require 'em-term)
     (require 'em-cmpl)
     (setenv "PAGER" "cat")
     (set-face-attribute 'eshell-prompt nil :foreground "turquoise1")
     (when (< emacs-major-version 23)
       (add-hook 'eshell-mode-hook ;; for some reason this needs to be a hook
                 '(lambda () (define-key eshell-mode-map "\C-a" 'eshell-bol)))
       (add-to-list 'eshell-output-filter-functions 'eshell-handle-ansi-color))

     ;; TODO: submit these via M-x report-emacs-bug
     (add-to-list 'eshell-visual-commands "ssh")
     (add-to-list 'eshell-visual-commands "tail")
     (add-to-list 'eshell-command-completions-alist
                  '("gunzip" "gz\\'"))
     (add-to-list 'eshell-command-completions-alist
                  '("tar" "\\(\\.tar|\\.tgz\\|\\.tar\\.gz\\)\\'"))))

(defun eshell/cds ()
  "Change directory to the project's root."
  (eshell/cd (locate-dominating-file default-directory "src")))

(defun eshell/find (dir &rest opts)
  (find-dired dir (mapconcat 'identity opts " ")))

;; Port features from
;; http://blog.peepcode.com/tutorials/2009/shell-method-missing/shell_method_missing.rb
;; * cloning git repos, github repos
;; * downloading http urls
;; * extracting archives
;; * changing to directories

(provide 'starter-kit-eshell)
;;; starter-kit-eshell.el ends here
