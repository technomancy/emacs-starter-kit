;;; starter-kit-lisp.el --- Some helpful Lisp code
;;
;; Part of the Emacs Starter Kit

(define-key read-expression-map (kbd "TAB") 'lisp-complete-symbol)
(define-key lisp-mode-shared-map (kbd "C-c l") "lambda")
(define-key lisp-mode-shared-map (kbd "RET") 'reindent-then-newline-and-indent)
(define-key lisp-mode-shared-map (kbd "C-\\") 'lisp-complete-symbol)
(define-key lisp-mode-shared-map (kbd "C-c v") 'eval-buffer)

(defun turn-on-paredit ()
  (paredit-mode +1))

;; (eval-after-load 'paredit
;;      ;; Not sure why paredit behaves this way with comments; it's annoying
;;   '(define-key paredit-mode-map (kbd ";")   'self-insert-command))

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
(add-hook 'emacs-lisp-mode-hook 'turn-on-paredit)

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
                             1 esk-clojure-trace-face)
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
(add-hook 'clojure-mode-hook 'turn-on-paredit)

(defun clojure-project (path)
  "Setup classpaths for a clojure project and starts a new SLIME session.

Kills existing SLIME session, if any."
  (interactive (list
                (ido-read-directory-name
                 "Project root: "
                 (locate-dominating-file default-directory "pom.xml"))))
  (when (get-buffer "*inferior-lisp*")
    (kill-buffer "*inferior-lisp*"))
  (add-to-list 'swank-clojure-extra-vm-args
               (format "-Dclojure.compile.path=%s"
                       (expand-file-name "target/classes/" path)))
  (setq swank-clojure-binary nil
        swank-clojure-jar-path (expand-file-name "target/dependency/" path)
        swank-clojure-extra-classpaths
        (append (mapcar (lambda (d) (expand-file-name d path))
                        '("src/" "target/classes/" "test/"))
                (let ((lib (expand-file-name "lib" path)))
                  (if (file-exists-p lib)
                      (directory-files lib t ".jar$"))))
        slime-lisp-implementations
        (cons `(clojure ,(swank-clojure-cmd) :init swank-clojure-init)
              (remove-if #'(lambda (x) (eq (car x) 'clojure))
                         slime-lisp-implementations)))
  (save-window-excursion
    (slime)))

;;; Scheme

(add-hook 'scheme-mode-hook 'run-coding-hook)
(add-hook 'scheme-mode-hook 'idle-highlight)
(font-lock-add-keywords 'scheme-mode
			'(("(\\|)" . 'esk-paren-face)))

;;; Common Lisp

(add-hook 'lisp-mode-hook 'run-coding-hook)
(add-hook 'lisp-mode-hook 'idle-highlight)
(add-hook 'lisp-mode-hook 'turn-on-paredit)
(font-lock-add-keywords 'lisp-mode
			'(("(\\|)" . 'esk-paren-face)))

(provide 'starter-kit-lisp)
;; starter-kit-lisp.el ends here