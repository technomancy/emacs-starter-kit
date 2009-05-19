;;; starter-kit-lisp.el --- Some helpful Lisp code
;;
;; Part of the Emacs Starter Kit

(define-key read-expression-map (kbd "TAB") 'lisp-complete-symbol)
(define-key lisp-mode-shared-map (kbd "C-c l") "lambda")
(define-key lisp-mode-shared-map (kbd "RET") 'reindent-then-newline-and-indent)
(define-key lisp-mode-shared-map (kbd "C-\\") 'lisp-complete-symbol)
(define-key lisp-mode-shared-map (kbd "C-c v") 'eval-buffer)

(eval-after-load 'paredit
  '(progn
     ;; Not sure why paredit behaves this way with comments; it's annoying
     (define-key paredit-mode-map (kbd ";")   'self-insert-command)
     (add-hook 'emacs-lisp-mode-hook (lambda () (paredit-mode +1)))
     (add-hook 'lisp-mode-hook (lambda () (paredit-mode +1)))))

(defface esk-paren-face
   '((((class color) (background dark))
      (:foreground "grey50"))
     (((class color) (background light))
      (:foreground "grey55")))
   "Face used to dim parentheses."
   :group 'starter-kit-faces)

;;; Emacs Lisp

(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'emacs-lisp-mode-hook 'run-coding-hook)
(add-hook 'emacs-lisp-mode-hook 'esk-remove-elc-on-save)
(add-hook 'emacs-lisp-mode-hook 'idle-highlight)

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

(add-hook 'clojure-mode-hook 'run-coding-hook)
(add-hook 'clojure-mode-hook 'idle-highlight)

(font-lock-add-keywords 'clojure-mode
                        '(("(\\|)" . 'esk-paren-face)))

(defface esk-clojure-trace-face
   '((((class color) (background dark))
      (:foreground "grey50"))
     (((class color) (background light))
      (:foreground "grey55")))
   "Face used to dim parentheses."
   :group 'starter-kit-faces)

(setq esk-clojure-trace-face 'esk-clojure-trace-face)

;; This will make relevant lines stand out more in stack traces
(defun sldb-font-lock ()
  (font-lock-add-keywords nil
                          '(("[0-9]+: \\(clojure\.\\(core\\|lang\\).*\\)"
                             1 esk-clojure-trace-face)
                            ("[0-9]+: \\(java.*\\)"
                             1 font-lock-comment-face)
                            ("[0-9]+: \\(swank.*\\)"
                             1 esk-clojure-trace-face)
                            ("\\[\\([A-Z]+\\)\\]"
                             1 font-lock-function-name-face))))

(add-hook 'sldb-mode-hook 'sldb-font-lock)

(defun slime-jump-to-trace (&optional on)
  "Jump to the file/line that the current stack trace line references.
Only works with files in your project root's src/, not in dependencies."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (search-forward-regexp "[0-9]: \\([^$(]+\\).*?\\([0-9]*\\))")
    (let ((line (string-to-number (match-string 2)))
          (ns-path (split-string (match-string 1) "\\."))
          (project-root (locate-dominating-file default-directory "src/")))
      (find-file (format "%s/src/%s.clj" project-root
                         (mapconcat 'identity ns-path "/")))
      (goto-line line))))

(eval-after-load 'slime
  '(progn
     (defalias 'sldb-toggle-details 'slime-jump-to-trace)
     (defun sldb-prune-initial-frames (frames)
       "Show all stack trace lines by default."
       frames)))

(eval-after-load 'find-file-in-project
  '(add-to-list 'ffip-patterns "*.clj"))

;; You might like this, but it's a bit disorienting at first:
;; (setq clojure-enable-paredit t)

;;; Scheme

(add-hook 'scheme-mode-hook 'run-coding-hook)
(add-hook 'scheme-mode-hook 'idle-highlight)
(font-lock-add-keywords 'scheme-mode
			'(("(\\|)" . 'esk-paren-face)))

;;; Common Lisp

(add-hook 'lisp-mode-hook 'run-coding-hook)
(add-hook 'lisp-mode-hook 'idle-highlight)
(font-lock-add-keywords 'lisp-mode
			'(("(\\|)" . 'esk-paren-face)))

(provide 'starter-kit-lisp)
;; starter-kit-lisp.el ends here