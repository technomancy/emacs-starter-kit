;; tt-mode.el --- Emacs major mode for editing Template Toolkit files
;;
;; Copyright (c) 2002 Dave Cross, all rights reserved.
;;
;; This file may be distributed under the same terms as GNU Emacs.
;;
;; $Id: tt-mode.el 13 2008-01-27 09:35:16Z dave $
;;
;; This file adds simple font highlighting of TT directives when you are
;; editing Template Toolkit files.
;;
;; I usually give these files an extension of .tt and in order to automatically
;; invoke this mode for these files, I have the following in my .emacs file.
;;
;; (setq load-path
;;      (cons "/home/dave/xemacs" load-path))
;; (autoload 'tt-mode "tt-mode")
;; (setq auto-mode-alist
;;  (append '(("\\.tt$" . tt-mode))  auto-mode-alist ))
;;
;; Something similar may well work for you.
;;
;; Author: Dave Cross <dave@dave.org.uk>
;;
;; Modified by Lennart Borgman 2008-08-06

(require 'font-lock)

(defvar tt-mode-hook nil
  "List of functions to call when entering TT mode")

(defvar tt-keywords
  (concat "\\b\\(?:"
          (regexp-opt (list "GET" "CALL" "SET" "DEFAULT" "INSERT" "INCLUDE"
                            "BLOCK" "END" "PROCESS" "WRAPPER" "IF" "UNLESS"
                            "ELSIF" "ELSE" "SWITCH" "CASE" "FOR" "FOREACH"
                            "WHILE" "FILTER" "USE" "MACRO" "PERL" "RAWPERL"
                            "TRY" "THROW" "CATCH" "FINAL" "LAST" "RETURN"
                            "STOP" "CLEAR" "META" "TAGS"))
          "\\)\\b"))

(defvar tt-font-lock-keywords
   (list
    ;; Fontify [& ... &] expressions
    '("\\(\\[%[-+]?\\)\\(\\(.\\|\n\\)+?\\)\\([-+]?%\\]\\)"
      (1 font-lock-string-face t)
      (2 font-lock-variable-name-face t)
      (4 font-lock-string-face t))
    ;; Look for keywords within those expressions
    ;;
    ;; Comment out whole directive tag
    '("\\[%\\(#.*?\\)%\\]"
      (1 font-lock-comment-face t))
    ;; Comments to end of line
;;;     '("\\[%\\(?:.\\|\n\\)*\\(#.*\\)"
;;;       (1 font-lock-comment-face t))
    '("\\[% *\\([a-z_0-9]*\\) *%\\]"
      (1 font-lock-constant-face t))
    (list (concat
	   "\\(\\[%[-+]?\\|;\\)[ \n\t]*\\("
	   tt-keywords
	   "\\)")
	  2 font-lock-keyword-face t)
    )
  "Expressions to font-lock in tt-mode.")

;; (defvar tt-font-lock-keywords
;;   ;; Since this is used in a multi major mode we
;;    (list
;;     ;; Fontify [& ... &] expressions
;; ;;;     '("^\\([-+]?\\)\\(\\(.\\|\n\\)+?\\)\\([-+]?\\)$"
;; ;;;       (1 font-lock-string-face t)
;; ;;;       (2 font-lock-variable-name-face t)
;; ;;;       (4 font-lock-string-face t))
;;     '("\\(#.*\\)$"
;;       (1 font-lock-comment-face t))
;;     '("^ *\\([a-z_0-9]*\\) *$"
;;       (1 font-lock-constant-face t))
;;     (list (concat
;;            "^\\(?:[-+]?\\|;\\)[ \n\t]*\\("
;;            tt-keywords
;;            "\\)")
;;            )
;; 	  1 font-lock-keyword-face t)
;;     )
;;   "Expressions to font-lock in tt-mode.")

(defvar tt-font-lock-defaults
  '(tt-font-lock-keywords nil t))

(defun tt-mode-after-change (beg end pre-len)
  ;; add/remove font-lock-multiline
  ;; Fix-me: add variable for search lengths
  (let* ((here (point))
         (beg-is-ml (get-text-property beg 'font-lock-multiline))
         tt-beg
         tt-end
         )
    (when beg-is-ml
      (let ((beg-ok (not (previous-single-property-change
                          here 'font-lock-multiline
                          nil (- here 1))))
            )
        (when beg-ok
          (goto-char beg)
          (search-forward "%]" end t)
          )
            (search-forward "[%" end t)
        ))
    (when tt-end
      (remove-list-of-text-properties here tt-beg '(font-lock-multiline))
      (set-text-properties tt-beg tt-end '(font-lock-multiline t))))
  )


;;;###autoload
(define-derived-mode tt-mode fundamental-mode "TT"
  "Major mode for editing Template Toolkit files."
  (set (make-local-variable 'font-lock-defaults) tt-font-lock-defaults)
  (add-hook 'after-change-functions 'tt-mode-after-change nil t)
  )

(provide 'tt-mode)
;; tt-mode.el ends here
