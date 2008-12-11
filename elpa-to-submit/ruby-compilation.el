;;; ruby-compilation.el --- run a ruby process in a compilation buffer

;; Copyright (C) 2008 Eric Schulte

;; Author: Eric Schulte
;; URL: http://www.emacswiki.org/cgi-bin/emacs/ruby-compilation.el
;; Version: 0.5
;; Created: 2008-08-23
;; Keywords: test convenience
;; Package-Requires: (("ruby-mode") ("inf-ruby"))

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; Allow for execution of ruby processes dumping the results into a
;; compilation buffer.  Useful for executing tests, or rake tasks
;; where the ability to jump to errors in source code is desirable.
;;
;; The functions you will probably want to use are
;; 
;; ruby-compilation-run
;; ruby-compilation-rake
;; ruby-compilation-this-buffer (C-x t)
;; ruby-compilation-this-buffer (C-x C-t)
;;

;;; TODO:

;; Clean up function names so they use a common prefix.
;; "p" doesn't work at the end of the compilation buffer.
;; Package it up with dependencies for ELPA.

;;; Code:

(require 'ansi-color)
(require 'compile)
(require 'which-func)

(defvar ruby-compilation-error-regexp
  "^\\([[:space:]]*\\|.*\\[\\|[^\*].*at \\)\\[?\\([^[:space:]]*\\):\\([[:digit:]]+\\)[]:)\n]?"
  "regular expression to match errors in ruby process output")

(defvar ruby-compilation-error-regexp-alist
  `((,ruby-compilation-error-regexp 2 3))
  "a version of `compilation-error-regexp-alist' to be used in
  rails logs (should be used with `make-local-variable')")

(defvar ruby-compilation-executable "ruby"
  "What bin to use to launch the tests. Override if you use JRuby etc.")

(defvar ruby-compilation-test-name-flag "-n"
  "What flag to use to specify that you want to run a single test.")

(defun ruby-compilation-run (cmd)
  "Run a ruby process dumping output to a ruby compilation buffer."
  (interactive "FRuby Comand: ")
  (let ((name (file-name-nondirectory (car (split-string cmd))))
	(cmdlist (cons ruby-compilation-executable
                       ;; What on earth is ruby-args-to-list?
                       (ruby-args-to-list (expand-file-name cmd)))))
    (pop-to-buffer (ruby-compilation-do name cmdlist))))

;;;###autoload
(defun ruby-compilation-rake (&optional edit task)
  "Run a rake process dumping output to a ruby compilation buffer."
  (interactive "P")
  (let* ((task (or task (if (stringp edit) edit)
		   (completing-read "Rake: " (ruby-compilation-rake-tasks))))
	 (rake-args (if (and edit (not (stringp edit)))
			(read-from-minibuffer "Edit Rake Command: " (concat task " "))
		      task)))
    (pop-to-buffer (ruby-compilation-do
		    "rake" (cons "rake"
				 (ruby-args-to-list rake-args))))))

;;;###autoload
(defun ruby-compilation-this-buffer ()
  "Run the current buffer through Ruby compilation."
  (interactive)
  (ruby-compilation-run (buffer-file-name)))

;;;###autoload
(defun ruby-compilation-this-test ()
  "Run the test at point through Ruby compilation."
  (interactive)
  (let ((test-name (ruby-compilation-this-test-name)))
    (pop-to-buffer (ruby-compilation-do
                    (format "ruby: %s - %s"
                            (file-name-nondirectory (buffer-file-name))
                            test-name)
                    (list ruby-compilation-executable
                          (buffer-file-name)
                          ruby-compilation-test-name-flag test-name)))))

(defun ruby-compilation-this-test-name ()
  "Which test are we currently in?"
  (let ((this-test (which-function)))
    (if (listp this-test) (setq this-test (car this-test)))
    (if (or (not this-test)
            (not (string-match "#test_" this-test)))
        (message "Point is not in a test.")
      (cadr (split-string this-test "#")))))

(defun ruby-compilation-do (name cmdlist)
  (let ((comp-buffer-name (format "*%s*" name)))
    (unless (comint-check-proc comp-buffer-name)
      ;; (if (get-buffer comp-buffer-name) (kill-buffer comp-buffer-name)) ;; actually rather keep
      (let* ((buffer (apply 'make-comint name (car cmdlist) nil (cdr cmdlist)))
	     (proc (get-buffer-process buffer)))
	(save-excursion
	  (set-buffer buffer) ;; set buffer local variables and process ornaments
	  (set-process-sentinel proc 'ruby-compilation-sentinel)
	  (set-process-filter proc 'ruby-compilation-insertion-filter)
	  (set (make-local-variable 'compilation-error-regexp-alist)
	       ruby-compilation-error-regexp-alist)
	  (set (make-local-variable 'kill-buffer-hook)
	       (lambda ()
		 (let ((orphan-proc (get-buffer-process (buffer-name))))
		   (if orphan-proc
		       (kill-process orphan-proc)))))
	  (compilation-minor-mode t)
	  (ruby-compilation-minor-mode t))))
    comp-buffer-name))

(defun ruby-compilation-insertion-filter (proc string)
  "Insert text to buffer stripping ansi color codes"
  ;; Can we use ansi-color-apply-on-region instead?
  (with-current-buffer (process-buffer proc)
    (let ((moving (= (point) (process-mark proc))))
      (save-excursion
	(goto-char (process-mark proc))
	(insert (ansi-color-filter-apply string))
	(set-marker (process-mark proc) (point)))
      (if moving (goto-char (process-mark proc))))))

(defun ruby-compilation-sentinel (proc msg)
  "Notify to changes in process state"
  (message "%s - %s" proc (replace-regexp-in-string "\n" "" msg)))

(defun ruby-compilation-previous-error-group ()
  "Jump to the start of the previous error group in the current
compilation buffer."
  (interactive)
  (compilation-previous-error 1)
  (while (string-match ruby-compilation-error-regexp (thing-at-point 'line))
    (forward-line -1))
  (forward-line 1) (recenter))

(defun ruby-compilation-next-error-group ()
  "Jump to the start of the previous error group in the current
compilation buffer."
  (interactive)
  (while (string-match ruby-compilation-error-regexp (thing-at-point 'line))
    (forward-line 1))
  (compilation-next-error 1) (recenter))

(defvar ruby-compilation-minor-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "q"    'quit-window)
    (define-key map "p"    'previous-error-no-select)
    (define-key map "n"    'next-error-no-select)
    (define-key map "\M-p" 'ruby-compilation-previous-error-group)
    (define-key map "\M-n" 'ruby-compilation-next-error-group)
    (define-key map (kbd "C-c C-c") 'comint-interrupt-subjob)
    map)
  "Key map for Ruby Compilation minor mode.")

(define-minor-mode ruby-compilation-minor-mode
  "Enable Ruby Compilation minor mode providing some key-bindings
  for navigating ruby compilation buffers."
  nil
  " ruby:comp"
  ruby-compilation-minor-mode-map)

;; So we can invoke it easily.
(eval-after-load 'ruby-mode
  '(progn
     (define-key ruby-mode-map (kbd "C-x t") 'ruby-compilation-this-buffer)
     (define-key ruby-mode-map (kbd "C-x C-t") 'ruby-compilation-this-test)))

;; So we don't get warnings with .dir-settings.el files
(dolist (executable (list "jruby" "rbx" "ruby1.9" "ruby1.8" "ruby"))
  (add-to-list 'safe-local-variable-values
               (cons 'ruby-compilation-executable executable)))

(defun ruby-compilation-rake-tasks ()
   "Return a list of all the rake tasks defined in the current
projects.  I know this is a hack to put all the logic in the
exec-to-string command, but it works and seems fast"
   (delq nil (mapcar '(lambda(line)
			(if (string-match "rake \\([^ ]+\\)" line) (match-string 1 line)))
		     (split-string (shell-command-to-string "rake -T") "[\n]"))))

;;;###autoload
(defun pcomplete/rake ()
  "Completion rules for the `ssh' command."
  (pcomplete-here (pcmpl-rake-tasks)))

(provide 'ruby-compilation)
;;; ruby-compilation.el ends here