;;; django.el ---
;;
;; Author: Lennart Borgman (lennart O borgman A gmail O com)
;; Created: Sun Nov 18 18:29:41 2007
;; Version: 0.3
;; Last-Updated: 2008-08-08T13:22:19+0200 Fri
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
;; Simple highlighting for Django for use with mumamo.
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

;; Maybe there are something to get here?
;; http://github.com/cosmin/emacs-utils/tree/85cc1d2bd447cb9b2fc98e27b5f8780453e5b978/django-html-mode.el
(defconst django-font-lock-keywords
  (list
   (cons (rx
          word-start
          (or "as" "in"
              (seq
               (opt "end")
               (or "autoescape" "block" "comment" "cycle" "debug" "else"
                   "extends" "filter" "firstof" "for" "if" "ifchanged" "ifequal"
                   "ifnotequal" "include" "load" "now" "regroup"
                   "spaceless" "ssi" "templatetag" "url" "widthratio"
                   "with")))
          word-end)
         font-lock-keyword-face)
   )
   "Minimal highlighting expressions for Django mode")

;;;###autoload
(define-derived-mode django-mode nil "Django"
  "Simple Django mode for use with mumamo.
This mode only provides syntax highlighting."
  (setq font-lock-defaults '(django-font-lock-keywords)))

;;; Comments mode
(defconst django-comment-font-lock-keywords
  (list
   (cons "\\(.*\\)" (list 1 font-lock-comment-face))
   ))

(defvar django-comment-font-lock-defaults
  '(django-comment-font-lock-keywords t t))

(define-derived-mode django-comment-mode nil "Django comment"
  "For django comment blocks."
  (set (make-local-variable 'font-lock-defaults) django-comment-font-lock-defaults))

;;; Variables mode

(defconst django-variable-font-lock-keywords
  (list
   ;; Built in filters:
   (cons (rx
          "|"
          (submatch
           (or "add" "addslashes" "capfirst" "center" "cut"
               "date" "default" "default_if_none"
               "dictsort" "dictsortreversed"
               "divisibleby"
               "escape"
               "filesizeformat"
               "first"
               "fixampersands"
               "floatformat"
               "force_escape"
               "iriencode"
               "join"
               "length" "length_is"
               "linebreaks" "linebreaksbr" "linenumbers"
               "ljust"
               "lower"
               "make_list"
               "phone2numeric"
               "pluralize"
               "pprint"
               "random"
               "removetags"
               "rjust"
               "safe" "slice" "slugify" "stringformat" "striptags"
               "time" "timesince" "timeuntil"
               "title" "truncatewords" "truncatewords_html"
               "unordered_list"
               "upper" "urlencode" "urlize" "urlizetrunc"
               "wordcount" "wordwrap" "yesno")))
         (list 1 font-lock-builtin-face))
   (cons (rx
          "|"
          (submatch
           (0+ (any "a-z"))))
         (list 1 font-lock-function-name-face))
   (cons "\\([^|]*\\)" (list 1 font-lock-variable-name-face))
   ))

(defvar django-variable-font-lock-defaults
  '(django-variable-font-lock-keywords
    t t
    ;; This still gives teh syntax symbol to |, why?
    ((?| . ". "))
    ))

(define-derived-mode django-variable-mode nil "Django variable"
  "For django comment blocks."
  ;;(modify-syntax-entry ?| ?.)
  (set (make-local-variable 'font-lock-defaults) django-variable-font-lock-defaults))

(provide 'django)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; django.el ends here
