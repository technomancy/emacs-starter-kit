;;; starter-kit-lisp.el --- Some helpful Lisp code
;;
;; Part of the Emacs Starter Kit

(add-hook 'emacs-lisp-mode-hook 'eldoc-mode)

(add-hook 'emacs-lisp-mode-hook
	  ;; If you're saving an elisp file, likely the .elc is no longer valid.
	  (lambda ()
	    (make-local-variable 'after-save-hook)
	    (add-hook 'after-save-hook
		      (lambda ()
			(if (file-exists-p (concat buffer-file-name "c"))
			    (delete-file (concat buffer-file-name "c")))))))

(font-lock-add-keywords 'emacs-lisp-mode
			'(("(\\|)" . 'paren-face)))

(font-lock-add-keywords 'scheme-mode
			'(("(\\|)" . 'paren-face)))

(defface paren-face
   '((((class color) (background dark))
      (:foreground "grey20"))
     (((class color) (background light))
      (:foreground "grey55")))
   "Face used to dim parentheses."
   :group 'starter-kit-faces)

(provide 'starter-kit-lisp)