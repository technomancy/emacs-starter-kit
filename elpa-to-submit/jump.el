;;; jump.el --- build functions which contextually jump between files

;; Copyright (C) 2008 Eric Schulte

;; Author: Eric Schulte
;; URL: http://github.com/eschulte/jump.el/tree/master
;; Version: 2.0
;; Created: 2008-08-21
;; Keywords: project, convenience, navigation

;; This file is NOT part of GNU Emacs.

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING. If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; This library is intended to aid in the construction of functions
;; for navigating projects.  The `defjump' function using a hopefully
;; convenient specification schema which jumps to new file/methods
;; based upon the file/method context of the current buffer/point.

;; This effort was inspired heavily by find-file-in-project.el by Phil
;; Hagelberg and Doug Alcorn, and toggle.el by Ryan Davis.  The
;; initial goal of jump.el was to subsume both of these tools.

;;; Example: (jumping to the related model in a rails application)

;; (defjump
;;   'rinari-find-model
;;   '(("app/controllers/\\1_controller.rb#\\2"  . "app/models/\\1.rb#\\2")
;;     ("app/views/\\1/.*"                       . "app/models/\\1.rb")
;;     ("app/helpers/\\1_helper.rb"              . "app/models/\\1.rb")
;;     ("db/migrate/.*create_\\1.rb"             . "app/models/\\1.rb")
;;     ("test/functional/\\1_controller_test.rb" . "app/models/\\1.rb")
;;     ("test/unit/\\1_test.rb#test_\\2"         . "app/models/\\1.rb#\\2")
;;     ("test/unit/\\1_test.rb"                  . "app/models/\\1.rb")
;;     ("test/fixtures/\\1.yml"                  . "app/models/\\1.rb")
;;     (t                                        . "app/models/"))
;;   'rinari-root
;;   "Go to the most logical model given the current location."
;;   '(lambda (path)
;;      (message (shell-command-to-string
;; 	       (format "ruby %sscript/generate model %s"
;; 		       (rinari-root)
;; 		       (and (string-match ".*/\\(.+?\\)\.rb" path)
;; 			    (match-string 1 path))))))
;;   'ruby-add-log-current-method)

;;; Code:
(require 'which-func)
(require 'findr)
(require 'inflections)

(defvar jump-ignore-file-regexp ;; TODO actually start using this
  "\\(.*\\.\\(git\\|svn\\|cvs\\).*\\|.*~\\|.*\\#.*\\#\\)"
  "regexp for the find shell command to ignore undesirable files")

(defun jump-completing-read (prompt choices &optional predicate require-match initial-input hist def)
  "if `ido-mode' is turned on use ido speedups completing the read"
  (if ido-mode
      (ido-completing-read prompt choices predicate require-match initial-input hist def)
    (completing-read prompt choices predicate require-match initial-input hist def)))

(defun jump-find-file-in-dir (dir)
  "if `ido-mode' is turned on use ido speedups finding the file"
  (if ido-mode
      (ido-find-file-in-dir dir)
    (let ((default-dir dir)) (find-file))))

(defun jump-method ()
  "Return the method defined at the current position in current
buffer."
  (let ((func (funcall method-command)))
    (or (and func (string-match "#\\(.+\\)" func) (match-string 1 func))
	func)))

(defun jump-uniqueify (file-cons)
  "Set the car of the argument to include the directory name plus the file name."
  (setcar file-cons
	  (concat (car file-cons) " "
		  (cadr (reverse (split-string (cdr file-cons) "/"))))))

(defun jump-select-and-find-file (files)
  "Select a single file from an alist of file names and paths.
Return the path selected or nil if files was empty."
  (let ((file   (case (length files)
		  (0 nil)
		  (1 (caar files))
		  (t (jump-completing-read "Jump to: "
					   (mapcar 'car files))))))
    (if file (find-file (cdr (assoc file files))))))

(defun jump-remove-unwanted-files (files)
  (delete-if nil (mapcar (lambda (file-cons)
			   (unless (string-match
				    jump-ignore-file-regexp (cdr file-cons)) file-cons))
			 files)))

(defun jump-to-file (&optional file)
  "Open the file located at file if file ends in a / then look in
the related directory, and if file contains regexps then select
from all matches."
  (interactive "Mfile: ")
  (let ((file-cons (cons (file-name-nondirectory file) file))
	file-alist)
    (if (string-match "/$" file) ;; TODO: ensure that the directory exists
	(jump-find-file-in-dir (expand-file-name file root)) ;; open directory
      (if (file-exists-p file)
	  (find-file file) ;; open file
	(jump-select-and-find-file ;; open with regexp
	 (jump-remove-unwanted-files
	  (mapcar (lambda (file)
		    (let ((file-cons (cons (file-name-nondirectory file)
					   (expand-file-name file))))
		      (when (assoc (car file-cons) file-alist)
			(jump-uniqueify (assoc (car file-cons) file-alist))
			(jump-uniqueify file-cons))
		      (add-to-list 'file-alist file-cons)
		      file-cons))
		  (findr (car file-cons)
			 (expand-file-name (or (file-name-directory
						(cdr file-cons)) "") root)))))))))

(defun jump-to-method (&optional method)
  "If `jump-method' returns method in buffer, go to the first
line inside of method."
  (interactive "Mmethod: ")
  (goto-char (point-min))
  (let (results)
    (while (not (setf results
		      (or (string-equal (jump-method) method)
			  (and (> (forward-line 1) 0)
			       (goto-char (point-min)))))))
    (unless (equal results 1) t)))

(defun jump-to-path (path)
  "Jump to the location specified by PATH (regexp allowed in
path).  If path ends in / then just look in that directory"
  (let ((file path)
	method)
    (when (string-match "^\\(.*\\)#\\(.*\\)$" path)
      (setf method (match-string 2 path))
      (setf file (match-string 1 path)))
    (if (jump-to-file file) ;; returns t as long as a file was found
	(when method (jump-to-method method) t))))

(defun jump-insert-matches (spec matches)
  (message (format "%S" (cons spec matches)))
  (if matches
      (let ((count 1) (new-spec spec) (spec nil))
	(while (not (equal spec new-spec))
	  (setf spec new-spec)
	  (setf new-spec
		(replace-regexp-in-string (format "\\\\%d" count)
					  (or (nth (- count 1) matches) ".*?")
					  spec))
	  (setf count (+ 1 count)))
	new-spec) spec))

(defun jump-inflections (terms)
  "Return all combinations of the singular and pluralizations of TERMS."
  (let ((terms (mapcar
		(lambda (term)
		  (delete-dups (list term
				     (singularize-string term)
				     (pluralize-string term))))
		terms))
	results interum-results)
    (dolist (group terms)
      (dolist (term group)
	(if results
	    (dolist (combination results)
	      (setf interum-results (cons
				     (cons term combination)
				     interum-results)))
	  (setf interum-results (cons (list term) interum-results))))
      (setf results interum-results)
      (setf interum-results nil))
    (mapcar 'reverse results)))

(defun jump-to-all-inflections (spec matches)
  (let (status) ;; TODO maybe try file first and method second
    (loop for path in (mapcar (lambda (option)
				(jump-insert-matches spec option))
			      (jump-inflections matches))
	  until (setf status (jump-to-path path)))
    status))

(defun jump-to (spec &optional matches make)
  "Jump to a spot defined by SPEC.  If optional argument MATCHES
replace all '\\n' portions of SPEC with the nth (1 indexed)
element of MATCHES.  If optiona argument MAKE, then create the
target file if it doesn't exist, if MAKE is a function then use
MAKE to create the target file."
  (if (functionp spec) (eval (list spec matches)) ;; custom function in spec
    (let ((path (jump-insert-matches spec matches)))
      (unless (or (jump-to-path path)
		  (and matches (jump-to-all-inflections spec matches)))
	(when make (message (format "making %s" path))
	      (let ((path (if (or (string-match "^\\(.*?\\)\\.\\*" path)
				  (string-match "^\\(.*/\\)$" path))
			      (read-from-minibuffer "create " (match-string 1 path))
			    path)))
		(when (functionp make) (eval (list make path)))
		(find-file (concat root (if (string-match "^\\(.*\\)#" path)
					    (match-string 1 path) path)))))))))

(defun jump-from (spec)
  "Match SPEC to the current location returning a list of any matches"
  (cond ((stringp spec)
	 (let* ((file (or (and (buffer-file-name)
			       (expand-file-name (buffer-file-name)))
			  (buffer-name)))
		(method (jump-method))
		(path (if (string-match "#.+" spec)
			  (concat file "#" method)
			file)))
	   (and (string-match spec path)
		(or (let ((counter 1) mymatch matches)
		      (while (setf mymatch (match-string counter path))
			(setf matches (cons mymatch matches))
			(setf counter (+ 1 counter)))
		      (reverse matches)) t))))
	((functionp spec) (eval (list spec)))
	((equal t spec) t)
	(t (message (format "unrecognized jump-from specification format %s")))))

(defun defjump (name specs root &optional doc make method-command)
  "Define NAME as a function with behavior determined by SPECS.
SPECS should be a list of cons cells of the form

   (jump-from-spec . jump-to-spec)

NAME will then try subsequent jump-from-specs until one succeeds,
at which point any resulting match information, along with the
related jump-to-spec will be used to jump to the intended buffer.
See `jump-to' and `jump-from' for information on spec
construction.

ROOT should specify the root of the project in which all jumps
take place, it can be either a string directory path, or a
function returning

Optional argument DOC specifies the documentation of the
resulting function.

Optional argument MAKE can be used to specify that missing files
should be created.  If MAKE is a function then it will be called
with the file path as it's only argument.  After possibly calling
MAKE `find-file' will be used to open the path.

Optional argument METHOD-COMMAND overrides the function used to
find the current method which defaults to `which-function'."
  (eval
   `(defun ,name (&optional create) ,(concat doc "\n\nautomatically created by `defjump'")
      (interactive "P")
      (let ((root ,(if (functionp root) `(,root) root))
	    (method-command (quote ,(or method-command 'which-function)))
	    matches)
	(loop ;; try every rule in mappings
	 for spec in (quote ,(mapcar
			      (lambda (spec)
				(if (stringp (car spec))
				    (cons (replace-regexp-in-string
					   "\\\\[[:digit:]]+" "\\\\(.*?\\\\)"
					   (car spec)) (cdr spec))
				  spec))
			      specs))
	 until (setf matches (jump-from (car spec)))
	 finally (cond
		  ((equal t matches)
		   (jump-to (cdr spec) nil (if create (quote ,make))))
		  ((consp matches)
		   (jump-to (cdr spec) matches (if create (quote ,make))))))))))

(provide 'jump)
;;; jump.el ends here