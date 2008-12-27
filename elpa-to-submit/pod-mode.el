;;; pod-mode.el --- Major mode for editing .pod-files

;;; It mainly defines a grammar for syntax highlighting.
;;; POD is the Plain Old Documentation format of Perl.

;;; Copyright 2003 Steffen Schwigon

;;; Author: Steffen Schwigon <schwigon@webit.de>
;;; Version: $Id: pod-mode.el,v 1.2 2005/04/01 09:26:01 ss5 Exp $
;;; Keywords: perl pod
;;; X-URL: not distributed yet

;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2, or (at your option)
;;; any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program; if not, write to the Free Software
;;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Tested on i386-linux with XEmacs 21.4.
;;; Tested on i386-linux with GNU Emacs 21.2.1.
;;; Tested on i386-windows-2k with XEmacs 21.4.

;;; Commentary:

;;; This mode is built with help of the
;;; "Emacs language mode creation tutorial" at
;;; http://two-wugs.net/emacs/mode-tutorial.html
;;;
;;; Regexes are defined for the following font-lock-faces:
;;;
;;;   font-lock-keyword-face
;;;   font-lock-type-face
;;;   font-lock-comment-face
;;;   font-lock-reference-face
;;;   font-lock-doc-string-face
;;;   font-lock-function-name-face
;;;

;;; Usage:

;;; Put this file into your load-path and the following into your ~/.emacs:
;;;
;;;    (require 'pod-mode)
;;;
;;;
;;; To associate pod-mode with .pod files add the following to your ~/.emacs
;;;
;;;    (setq auto-mode-alist
;;;       (append auto-mode-alist
;;;         '(("\\.pod$" . pod-mode))))
;;;
;;;
;;; To automatically turn on font-lock-mode add the following to your ~/.emacs
;;;
;;;    (add-hook 'pod-mode-hook 'font-lock-mode)
;;;

;;; Code:

;; default variables
(defvar pod-mode-hook nil)

;; keymap
(defvar pod-mode-map nil "Keymap for POD major mode.")
(if pod-mode-map nil
  (let ((map (make-sparse-keymap)))
    ;; insert (define-key map ...) stuff here
    (setq pod-mode-map map)))

;; syntax highlighting: standard keywords
(defconst pod-font-lock-keywords-1
  '(
    ("^=\\(head1\\|head2\\|head3\\|head4\\|item\\|over\\|back\\|cut\\|pod\\|for\\|begin\\|end\\|encoding\\)" 0 font-lock-keyword-face)
    ("^[ \t]+\\(.*\\)$" 1 font-lock-type-face)
    )
  "Minimal highlighting expressions for POD mode.")

;; syntax highlighting: additional keywords
(defconst pod-font-lock-keywords-2
  (append pod-font-lock-keywords-1
	  '(
	    ("^=\\(head1\\|head2\\|head3\\|head4\\|item\\|over\\|back\\|cut\\|pod\\|for\\|begin\\|end\\)\\(.*\\)" 2 font-lock-comment-face)
	    ))
  "Additional Keywords to highlight in POD mode.")

;; syntax highlighting: even more keywords
(defconst pod-font-lock-keywords-3
  (append pod-font-lock-keywords-2
	  '(
	    ("[IBCFXZS]<\\([^>]*\\)>" 1 font-lock-reference-face)
	    ("L<\\([^|>]*|\\)?\\([^|>]*|\\)?\\([^>]+\\)>" 3 font-lock-function-name-face)
	    ("L<\\([^|>]*|\\)?\\([^|>]*|\\)?\\([^>]+\\)>" 2 font-lock-reference-face)
	    ("L<\\([^|>]*|\\)?\\([^|>]*\\)|\\([^>]*\\)>" 1 font-lock-doc-string-face)
	    ("E<\\([^>]*\\)>" 1 font-lock-function-name-face)
	    ("\"\\([^\"]+\\)\"" 0 font-lock-string-face)
	    ))
  "Balls-out highlighting in POD mode.")

;; default level of highlight to maximum
(defvar pod-font-lock-keywords pod-font-lock-keywords-3
  "Default highlighting expressions for POD mode")

;; no special indenting, just pure text mode
(defun pod-indent-line ()
  "Indent current line as POD code. Does nothing yet."
  (interactive)
  )

;; no special syntax table
(defvar pod-mode-syntax-table nil
  "Syntax table for pod-mode.")

;; create and activate syntax table
(defun pod-create-syntax-table ()
  (if pod-mode-syntax-table
      ()
    (setq pod-mode-syntax-table (make-syntax-table))
    (set-syntax-table pod-mode-syntax-table)
    ))

;; main
;;;###autoload
(defun pod-mode ()
  "Major mode for editing POD files (Plain Old Documentation for Perl)."
  (interactive)
  (kill-all-local-variables)
  (pod-create-syntax-table)
  (use-local-map pod-mode-map)
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults '(pod-font-lock-keywords 't))
  ;; (make-local-variable 'indent-line-function)
  ;; (setq indent-line-function 'pod-indent-line)
  (setq major-mode 'pod-mode)
  (setq mode-name "POD")
  (run-hooks 'pod-mode-hook))


(provide 'pod-mode)

;;; pod-mode.el ends here
