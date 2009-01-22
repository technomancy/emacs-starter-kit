;;; starter-kit-lisp.el --- Some helpful Lisp code
;;
;; Part of the Emacs Starter Kit

(define-key read-expression-map (kbd "TAB") 'lisp-complete-symbol)
(define-key lisp-mode-shared-map (kbd "C-c l") "lambda")
(define-key lisp-mode-shared-map (kbd "RET") 'reindent-then-newline-and-indent)
(define-key lisp-mode-shared-map (kbd "C-\\") 'lisp-complete-symbol)
(define-key lisp-mode-shared-map (kbd "C-c v") 'eval-buffer)

(eval-after-load 'paredit-mode
  '(progn
     ;; Not sure why paredit behaves this way with comments; it's annoying
     (define-key paredit-mode-map (kbd ";")   'self-insert-command)
     (add-hook 'emacs-lisp-mode-hook (lambda () (paredit-mode +1)))
     (add-hook 'lisp-mode-hook (lambda () (paredit-mode +1)))))

(defface esk-paren-face
   '((((class color) (background dark))
      (:foreground "grey20"))
     (((class color) (background light))
      (:foreground "grey55")))
   "Face used to dim parentheses."
   :group 'starter-kit-faces)

;;; Emacs Lisp

(add-hook 'emacs-lisp-mode-hook 'eldoc-mode)
(add-hook 'emacs-lisp-mode-hook 'coding-hook)
(add-hook 'emacs-lisp-mode-hook 'esk-remove-elc-on-save)

(defun esk-remove-elc-on-save ()
  "If you're saving an elisp file, likely the .elc is no longer valid."
  (make-local-variable 'after-save-hook)
  (add-hook 'after-save-hook
            (lambda ()
              (if (file-exists-p (concat buffer-file-name "c"))
                  (delete-file (concat buffer-file-name "c"))))))

(font-lock-add-keywords 'emacs-lisp-mode
			'(("(\\|)" . 'esk-paren-face)))

;;; Clojure

(defun esk-clojure (&optional src-path)
  "Load clojure support. Takes an optional argument for the checkout root.

Since there's no single conventional place to keep Clojure, this
is bundled up as a function that you can call with your source
root as an argument."

  (setq src-path (or src-path "~/src"))

  (add-to-list 'load-path (concat src-path "/slime"))
  (add-to-list 'load-path (concat src-path "/swank-clojure"))

  (autoload 'slime "slime" "" t)
  (load "swank-clojure-autoload")

  (setq swank-clojure-jar-path (concat src-path "/clojure/clojure.jar")
        swank-clojure-extra-classpaths
        (list (concat src-path "/clojure-contrib/clojure-contrib.jar")))

  (add-hook 'clojure-mode-hook 'coding-hook)
  (font-lock-add-keywords 'clojure-mode
                          '(("(\\|)" . 'esk-paren-face))))

(defun esk-clojure-install (src-path)
  "Perform the initial clojure install along with Emacs support libs."
  (interactive (list
                (read-from-minibuffer "Install Clojure in (default: ~/src): "
                                      nil nil nil nil "~/src")))
  (setq clojure-src-path src-path)
  (mkdir src-path t)

  (if (file-exists-p (concat src-path "/clojure"))
      (error "Clojure is already installed at %s/clojure" src-path))

  (cd src-path)
  (message "Checking out source... this will take a while...")
  (dolist (cmd '("git clone git://github.com/kevinoneill/clojure.git"
                 "git clone git://github.com/kevinoneill/clojure-contrib.git"
                 "git clone git://github.com/jochu/swank-clojure.git"
                 "git clone git://git.boinkor.net/slime.git"))
    (unless (= 0 (shell-command cmd))
      (error "Clojure installation step failed: %s" cmd)))
  (message "Checked out Clojure and friends. Compiling...")

  (cd (concat src-path "/clojure"))
  (unless (= 0 (shell-command "ant")) (error "Couldn't compile Clojure."))
  (cd (concat src-path "/clojure-contrib"))
  (unless (= 0 (shell-command "ant")) (error "Couldn't compile Clojure contrib."))

  (esk-clojure src-path)
  (message "Installed Clojure successfully. Press M-x slime to continue."))

;; You might like this, but it's a bit disorienting at first:
;; (setq clojure-enable-paredit t)

;;; Scheme

(add-hook 'scheme-mode-hook 'coding-hook)
(font-lock-add-keywords 'scheme-mode
			'(("(\\|)" . 'esk-paren-face)))

;;; Common Lisp

(add-hook 'lisp-mode-hook 'coding-hook)
(font-lock-add-keywords 'lisp-mode
			'(("(\\|)" . 'esk-paren-face)))

(provide 'starter-kit-lisp)
;; starter-kit-lisp.el ends here