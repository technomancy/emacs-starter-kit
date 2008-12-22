;;; fmode.el --- Changing default major modes for files

;; This library is depreceated. Use majmodpri.el instead.

;; Copyright (C) 2005, 2006, 2007 by Lennart Borgman

;; Author:  Lennart Borgman <lennart DOT borgman DOT 073 AT student DOT lu DOT se>
;; Created: 2005-06-21
;; Last-Updated: Mon Apr 23 05:35:24 2007 (7200 +0200)
;; Version: 0.5
;; Keywords: languages
;; Features that might be required by this library:
;;
;;   None
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This file is not part of Emacs

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; Functions to help to change default major modes.  Changes
;; `magic-mode-alist' and `auto-mode-alist'.
;;
;; Usage:
;;
;; To change default major mode for all file names that previously had
;; `xml-mode' as default major mode to `nxml-mode' enter this in your
;; .emacs:
;;
;;   (require 'fmode)
;;   (fmode-replace-default-mode 'xml-mode 'nxml-mode)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; History:
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(eval-when-compile (require 'cl))

(defun fmode--rassoc-replace (from to alist)
  "Replace in all cons cells cdr equal to FROM to TO."
  (let ((asscons (rassoc from alist))
        replaced)
    (while asscons
      (setq replaced
            (cons (cons (car asscons)
                        (cdr asscons))
                  replaced))
      (setcdr asscons to)
      (setq asscons (rassoc from alist)))
    replaced))

(defun fmode-replace-default-mode (from to &optional magic-to)
  "Replace default major modes in `magic-mode-alist' and `auto-mode-alist'.
If default major mode is FROM set it to TO."
  (when (boundp 'magic-mode-alist)
    (unless magic-to (setq magic-to to))
    (unless (eq magic-to t)
      (fmode--rassoc-replace from magic-to magic-mode-alist)))
  (fmode--rassoc-replace from to auto-mode-alist))


(provide 'fmode)

;;; fmode.el ends here
