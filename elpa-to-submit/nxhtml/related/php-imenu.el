;;; php-imenu.el --- object-oriented, hierarchical imenu for PHP
;;;
;;; Maintainer: Marcel Cary <marcel-cary of care2.com>
;;; Keywords: php languages oop
;;; Created: 2008-06-23
;;; Modified: 2008-07-18
;;; X-URL: http://www.oak.homeunix.org/~marcel/blog/articles/2008/07/14/nested-imenu-for-php
;;;
;;; Copyright (C) 2008 Marcel Cary
;;;
;;; License
;;;
;;; This program is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU General Public License
;;; as published by the Free Software Foundation; either version 2
;;; of the License, or (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program; if not, write to the Free Software
;;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
;;;
;;;
;;; Usage
;;;
;;; Rename this file to php-imenu.el if it isn't already then place it in
;;; your Emacs lisp path (eg. site-lisp) and add to your .emacs file:
;;;
;;;--------------cut here-------------------------------------------
;   ;; Load the php-imenu index function
;   (autoload 'php-imenu-create-index "php-imenu" nil t)
;   ;; Add the index creation function to the php-mode-hook 
;   (add-hook 'php-mode-user-hook 'php-imenu-setup)
;   (defun php-imenu-setup ()
;     (setq imenu-create-index-function (function php-imenu-create-index))
;     ;; uncomment if you prefer speedbar:
;     ;(setq php-imenu-alist-postprocessor (function reverse))
;     (imenu-add-menubar-index)
;   )
;;;----------------end here--------------------------------------------
;;;
;;; Commentary
;;;
;;; Refines php-mode's imenu support.  Imenu provides a menubar entry called
;;; "Index" that allows you to jump to a structural element of a file.  While
;;; php-mode generates separate lists of functions and classes in imenu,
;;; php-imenu.el (this code) generates a tree of class and function
;;; definitions.  It lists functions under the classes in which they're
;;; defined.  The hierarchical display of functions within their classes makes
;;; the "Index" menu far more useful in understanding the high-level structure
;;; of a file, and it makes it easier to find a method when a file contains
;;; multiple by the same name.
;;;
;;; Code:

(require 'imenu)
(require 'thingatpt)

;;; Alas, speedbar shows menu items in reverse, but only below the top level.
;;; Provide a way to fix it. See sample configuration in file comment.
(defvar php-imenu-alist-postprocessor (function identity))

;;; Want to see properties or defines?  Add an entry for them here.
(defvar php-imenu-patterns nil)
(setq php-imenu-patterns
  (list
   ;; types: classes and interfaces
   (list
    ;; for some reason [:space:] and \s- aren't matching \n
    (concat "^\\s-*"
            "\\(\\(abstract[[:space:]\n]+\\)?class\\|interface\\)"
            "[[:space:]\n]+"
            "\\([a-zA-Z0-9_]+\\)[[:space:]\n]*" ; class/iface name
            "\\([a-zA-Z0-9_[:space:]\n]*\\)" ; extends / implements clauses
            "[{]")
    (lambda ()
      (message "%S %S" 
               (match-string-no-properties 3) 
               (match-string-no-properties 1)) 
      (concat (match-string-no-properties 3) 
              " - " 
              (match-string-no-properties 1)))
    (lambda () 
      (save-excursion
        (backward-up-list 1)
        (forward-sexp)
        (point))))
   ;; functions
   (list
    (concat "^[[:space:]\n]*"
            "\\(\\(public\\|protected\\|private\\|"
                  "static\\|abstract\\)[[:space:]\n]+\\)*"
            "function[[:space:]\n]*&?[[:space:]\n]*"
            "\\([a-zA-Z0-9_]+\\)[[:space:]\n]*" ; function name
            "[(]")
    (lambda ()
      (concat (match-string-no-properties 3) "()"))
    (lambda ()
      (save-excursion
        (backward-up-list 1)
        (forward-sexp)
        (when (not (looking-at "\\s-*;"))
            (forward-sexp))
        (point))))
   ))

;;; Global variable to pass to imenu-progress-message in multiple functions
(defvar php-imenu-prev-pos nil)

;;; An implementation of imenu-create-index-function
(defun php-imenu-create-index ()
  (let (prev-pos)
    (imenu-progress-message php-imenu-prev-pos 0)
    (let ((result (php-imenu-create-index-helper (point-min) (point-max) nil)))
      ;(message "bye %S" result)
      (imenu-progress-message php-imenu-prev-pos 100)
      result)))

(defun php-imenu-create-index-helper (min max name)
  (let ((combined-pattern 
         (concat "\\(" 
                 (mapconcat
                  (function (lambda (pat) (first pat))) 
		  php-imenu-patterns "\\)\\|\\(") 
		 "\\)"))
        (index-alist '()))
    (goto-char min)
    (save-match-data 
      (while (re-search-forward combined-pattern max t)
        (let ((pos (set-marker (make-marker) (match-beginning 0)))
              (min (match-end 0))
              (pat (save-excursion 
                     (goto-char (match-beginning 0))
                     (find-if (function 
                               (lambda (pat) (looking-at (first pat))))
                              php-imenu-patterns))))
          (when (not pat)
            (message "php-imenu: How can no pattern get us here! %S" pos))
          (when (and pat 
                      (not (php-imenu-in-string-p))
                     )
            (let* ((name (funcall (second pat)))
                   (max  (funcall (third pat)))
                   (children (php-imenu-create-index-helper min max name)))
              ;; should validate max: what happens if unmatched curly?
              ;(message "%S %S %S" nm name (mapcar (function first) children))
              (if (equal '() children)
                  (push (cons name pos) index-alist)
                (push (cons name
                            (funcall php-imenu-alist-postprocessor
                                     (cons (cons "*go*" pos)
                                           children)))
                      index-alist))
              ))
          (imenu-progress-message php-imenu-prev-pos nil)
          )))
    (reverse index-alist)))

;;; Recognize when in quoted strings or heredoc-style string literals
(defun php-imenu-in-string-p ()
  (save-match-data
    (or (in-string-p)
        (let ((pt (point)))
          (save-excursion 
            (and (re-search-backward "<<<\\([A-Za-z0-9_]+\\)$" nil t)
                 (not (re-search-forward (concat "^"
                                                 (match-string-no-properties 1)
                                                 ";$")
                                         pt t))))))))
