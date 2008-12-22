;;; html-quote.el --- Simple quoting of html characters
;;
;; Author: Lennart Borgman (lennart O borgman A gmail O com)
;; Created: Sun Dec 30 12:55:38 2007
;; Version:
;; Last-Updated: Sun Dec 30 12:59:43 2007 (3600 +0100)
;; URL:
;; Keywords:
;; Compatibility:
;;
;; Features that might be required by this library:
;;
;;   None
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; Just simple quoting of & < > etc
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change log:
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:


(defcustom html-quote-html '((?<   . "&lt;")
                             (?&   . "&amp;"))
  "*Alist of char -> entity mappings used to make the text html-safe."
  :group 'html-qoute
  :type  '(alist :key-type character
                 :value-type string))


(defun html-quote-html-char (char)
  "Return CHAR as string if safe, otherwise its html entity."
  (or (cdr (assoc char html-quote-html))
      (char-to-string char)))

(defun html-quote-html-string (str)
  "Return html escaped STR."
  (mapconcat 'html-quote-html-char
             (append str nil)
             ""))

;; (html-quote-html-string "is & < s")

(provide 'html-quote)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; html-quote.el ends here
