;; find-recursive.el -- Find files recursively into a directory
;;
;; Copyright (C) 2001 Ovidiu Predescu
;;
;; Author: Ovidiu Predescu <ovidiu@cup.hp.com>
;; Date: March 26, 2001
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;
;; Setup: put this file in your Lisp path and add the following line in
;; your .emacs:
;;
;; (require 'find-recursive)
;;
;;; Change log:
;;
;; 2007-12-04 (Lennart Borgman)
;; - Removed advice
;; - Removed global key binding
;; - Made file name handling platform independent.
;; 2008-03-14 (Lennart Borgman)
;; - Tried to fix bugs I introduced when trying to make code platform
;;   independent.

(error "this is broken, use find-files-in-project.el instead")
(eval-when-compile (require 'cl))

(defcustom find-recursive-exclude-files '(".*.class$" ".*~$" ".*.elc$")
  "List of regular expressions of files to be excluded when recursively searching for files."
  :type '(repeat (string :tag "File regexp")))

(defun find-file-recursively (file-regexp directory)
  (interactive "sFile name regexp to search for recursively: \nDIn directory: ")
  (let ((directory (file-name-as-directory directory))
        (matches
         (find-recursive-filter-out
          find-recursive-exclude-files
          (find-recursive-directory-relative-files directory directory file-regexp))))
    (cond ((eq (length matches) 0) (message "No file(s) found!"))
          ((eq (length matches) 1)
           (find-file (expand-file-name (car matches) directory)))
          (t
           (isearch-unread 'tab) (isearch-unread 'tab)
           (let ((file (completing-read "Choose file: "
                                        (mapcar 'list matches)
                                        nil t)))
             (if (or (eq file nil) (equal file ""))
                 (message "No file selected.")
               (find-file (expand-file-name file directory))))))))

(defun find-recursive-directory-relative-files (directory
                                                relative-directory
                                                file-regexp)
  (let* ((directory (file-name-as-directory directory))
         (relative-directory (file-name-as-directory relative-directory))
         (full-dir (concat directory relative-directory))
         (matches
          (mapcar
           (function (lambda (x)
                       ;;(concat relative-directory x)
                       (file-relative-name
                        (expand-file-name x directory)
                        relative-directory)))
           (find-recursive-filter-out '(nil)
                                      (find-recursive-directory-files full-dir nil
                                                                      file-regexp nil t))))
         (inner
          (mapcar
           (function
            (lambda (dir)
              (find-recursive-directory-relative-files directory
                                                       ;;(concat relative-directory (file-name-as-directory dir))
                                                       (file-relative-name
                                                        (expand-file-name dir directory)
                                                        relative-directory)
                                                       file-regexp)))
           (find-recursive-filter-out '(nil "\\." "\\.\\.")
                                      (find-recursive-directory-files full-dir nil ".*"
                                                                      nil 'directories)))))
    (mapcar (function (lambda (dir) (setq matches (append matches dir))))
            inner)
    matches))

(defun find-recursive-filter-out (remove-list list)
  "Remove all the elements in *remove-list* from *list*"
  (if (eq list nil)
      nil
    (let ((elem (car list))
          (rest (cdr list)))
      (if (some
           (lambda (regexp)
             (if (or (eq elem nil) (eq regexp nil))
                 nil
               (not (eq (string-match regexp elem) nil))))
           remove-list)
          (find-recursive-filter-out remove-list rest)
        (cons elem (find-recursive-filter-out remove-list rest))))))

(defvar find-recursive-running-xemacs (string-match "XEmacs\\|Lucid" emacs-version))

(defun find-recursive-directory-files (dirname &optional full match nosort files-only)
  "Same as `directory-files' with additional arg FILES-ONLY.
When FILES-ONLY is t return only files, not directories.  If it
is nil return both files and directories. Finally, if it is
neither nil nor t, then return only subdirectories."
  (setq dirname (file-name-as-directory dirname))
  (if find-recursive-running-xemacs
      (directory-files dirname full match nosort files-only)
    (setq dirname (file-name-as-directory dirname))
    (let ((default-directory dirname))
      (cond ((null files-only)
             (directory-files dirname full match nosort))
            ((eq files-only t)
             (remq nil
                   (mapcar (lambda (f)
                             (if (file-directory-p f)
                                 nil
                               f))
                           (directory-files dirname full match nosort))))
            (t
             (remq nil
                   (mapcar (lambda (f)
                             (if (not (file-directory-p f))
                                 nil
                               f))
                           (directory-files dirname full match nosort))))))))

(defvar find-cached-filenames nil)
(length find-cached-filenames)

;;(find-cache-filenames "c:/emacsW32/nxhtml/tests/")
(defun find-cache-filenames (root)
  (let ((files
         ;;(find-recursive-directory-files root nil)
         (find-recursive-directory-relative-files root "" nil)
         ))
    (setq find-cached-filenames files)))

(provide 'find-recursive)
