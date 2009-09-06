;;; company-elisp.el --- a company-mode completion back-end for emacs-lisp-mode
;;
;; Copyright (C) 2009 Nikolaj Schumacher
;;
;; This file is part of company 0.4.3.
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
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(require 'company)
(eval-when-compile (require 'cl))
(require 'help-mode)

(defcustom company-elisp-detect-function-context t
  "*If enabled, offer lisp functions only in appropriate contexts.
Functions are offered for completion only after ' and \(."
  :group 'company
  :type '(choice (const :tag "Off" nil)
                 (const :tag "On" t)))

(defun company-grab-lisp-symbol ()
  (let ((prefix (company-grab-symbol)))
    (if prefix
        (unless (and (company-in-string-or-comment)
                     (/= (char-before (- (point) (length prefix))) ?`))
          prefix)
      'stop)))

(defun company-elisp-predicate (symbol)
  (or (boundp symbol)
      (fboundp symbol)))

(defvar company-elisp-parse-limit 30)
(defvar company-elisp-parse-depth 100)

(defvar company-elisp-binding-regexp
  (concat "([ \t\n]*\\_<" (regexp-opt '("let" "defun" "defmacro" "defsubst"
                                        "lambda" "lexical-let" "flet" "labels"))
          "\\*?")
  "Regular expression matching sexps containing variable bindings.")

(defvar company-elisp-binding-regexp-1
  (concat "([ \t\n]*\\_<" (regexp-opt '("dolist" "dotimes")))
  "Regular expression matching sexps containing one variable binding.")

(defun company-elisp-parse-local (prefix vars)
  (let ((regexp (concat "[ \t\n]*\\(\\_<" (regexp-quote prefix)
                        "\\(?:\\sw\\|\\s_\\)*\\_>\\)"))
        (pos (point)))
    (ignore-errors
      (save-excursion
        (dotimes (i company-elisp-parse-depth)
          (up-list -1)
          (save-excursion
            (cond
             ((looking-at company-elisp-binding-regexp)
              (down-list 2)
              (ignore-errors
                (dotimes (i company-elisp-parse-limit)
                  (save-excursion
                    (when (looking-at "[ \t\n]*(")
                      (down-list 1))
                    (and (looking-at regexp)
                         ;; Don't add incomplete text as candidate.
                         (not (eq (match-end 0) pos))
                         (add-to-list 'vars (match-string-no-properties 1))))
                  (forward-sexp))))
             ((looking-at company-elisp-binding-regexp-1)
              (down-list 2)
              (and (looking-at regexp)
                   ;; Don't add incomplete text as candidate.
                   (not (eq (match-end 0) pos))
                   (add-to-list 'vars (match-string-no-properties 1)))))))))
    vars))

(defun company-elisp-candidates (prefix)
  (let* ((completion-ignore-case nil)
         (before (char-before (- (point) (length prefix))))
         (predicate (if (and company-elisp-detect-function-context
                             (not (eq before ?')))
                        (if (eq before ?\()
                            'fboundp
                          'boundp)
                      'company-elisp-predicate))
         (candidates (all-completions prefix obarray predicate)))
    (company-elisp-parse-local prefix candidates)))

(defun company-elisp-doc (symbol)
  (let* ((symbol (intern symbol))
         (doc (if (fboundp symbol)
                  (documentation symbol t)
                (documentation-property symbol 'variable-documentation t))))
    (and (stringp doc)
         (string-match ".*$" doc)
         (match-string 0 doc))))

;;;###autoload
(defun company-elisp (command &optional arg &rest ignored)
  "A `company-mode' completion back-end for `emacs-lisp-mode'."
  (interactive (list 'interactive))
  (case command
    ('interactive (company-begin-backend 'company-elisp))
    ('prefix (and (eq (derived-mode-p 'emacs-lisp-mode) 'emacs-lisp-mode)
                  (company-grab-lisp-symbol)))
    ('candidates (company-elisp-candidates arg))
    ('meta (company-elisp-doc arg))
    ('doc-buffer (let ((symbol (intern arg)))
                   (save-window-excursion
                     (when (or (ignore-errors (describe-function symbol))
                               (ignore-errors (describe-variable symbol)))
                       (help-buffer)))))
    ('location (let ((sym (intern arg)))
                 (or (ignore-errors (find-definition-noselect sym nil))
                     (ignore-errors (find-definition-noselect sym 'defvar))
                     (ignore-errors (find-definition-noselect sym t)))))))

(provide 'company-elisp)
;;; company-elisp.el ends here
