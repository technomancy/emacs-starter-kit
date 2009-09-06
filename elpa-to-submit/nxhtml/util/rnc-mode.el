;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;   A major mode for editing RELAX NG Compact syntax.
;;   Version: 1.0b3
;;   Date: 2002-12-05
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;   Copyright (c) 2002, Pantor Engineering AB
;;   All rights reserved.
;;
;;   Redistribution and use in source and binary forms, with or
;;   without modification, are permitted provided that the following
;;   conditions are met:
;;
;;   * Redistributions of source code must retain the above copyright
;;     notice, this list of conditions and the following disclaimer.
;;
;;   * Redistributions in binary form must reproduce the above
;;     copyright notice, this list of conditions and the following
;;     disclaimer in the documentation and/or other materials provided
;;     with the distribution.
;;
;;   * Neither the name of Pantor Engineering AB nor the names of its
;;     contributors may be used to endorse or promote products derived
;;     from this software without specific prior written permission.
;;
;;   THIS   SOFTWARE   IS PROVIDED BY    THE   COPYRIGHT  HOLDERS  AND
;;   CONTRIBUTORS "AS IS"   AND ANY  EXPRESS OR  IMPLIED   WARRANTIES,
;;   INCLUDING,  BUT  NOT LIMITED  TO,   THE  IMPLIED  WARRANTIES   OF
;;   MERCHANTABILITY    AND  FITNESS  FOR   A  PARTICULAR  PURPOSE ARE
;;   DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS
;;   BE  LIABLE   FOR ANY    DIRECT, INDIRECT,   INCIDENTAL,  SPECIAL,
;;   EXEMPLARY, OR CONSEQUENTIAL DAMAGES  (INCLUDING, BUT NOT  LIMITED
;;   TO, PROCUREMENT  OF  SUBSTITUTE GOODS OR  SERVICES;  LOSS OF USE,
;;   DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
;;   ANY THEORY OF  LIABILITY, WHETHER IN CONTRACT,  STRICT LIABILITY,
;;   OR  TORT (INCLUDING NEGLIGENCE OR  OTHERWISE) ARISING  IN ANY WAY
;;   OUT OF  THE  USE OF   THIS  SOFTWARE,  EVEN IF ADVISED   OF   THE
;;   POSSIBILITY OF SUCH DAMAGE.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;   Created by David.Rosenborg@pantor.com
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;   Example setup for your ~/.emacs file:
;;
;;   (autoload 'rnc-mode "rnc-mode")
;;   (setq auto-mode-alist
;;         (cons '("\\.rnc\\'" . rnc-mode) auto-mode-alist))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;   Changes since 1.0b:
;;     Added a couple of defvars for faces to handle differences
;;     between GNU Emacs and XEmacs.
;;
;; 2008-12-28: Changed forward-char-command => forward-char
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'font-lock)

(defvar rnc-indent-level 3 "The RNC indentation level.")

(defvar rnc-keywords
  (mapcar (lambda (kw) (concat "\\b" kw "\\b"))
	  '("attribute" "div" "element"
	    "empty" "external" "grammar" "include" "inherit" "list"
	    "mixed" "notAllowed" "parent" "start" "string"
	    "text" "token"))
  "RNC keywords")

(defvar rnc-atoms
  (mapcar (lambda (kw) (concat "\\b" kw "\\b"))
	  '("empty" "notAllowed" "string" "text" "token"))
  "RNC atomic pattern keywords")

(defun rnc-make-regexp-choice (operands)
  "(op1 op2 ...) -> \"\\(op1\\|op2\\|...\\)\""
  (let ((result "\\("))
    (mapc (lambda (op) (setq result (concat result op "\\|"))) operands)
    (concat (substring result 0 -2) "\\)")))

;; Font lock treats face names differently in GNU Emacs and XEmacs
;; The following defvars is a workaround

(defvar italic 'italic)
(defvar default 'default)
(defvar font-lock-preprocessor-face 'font-lock-preprocessor-face)

(defvar rnc-font-lock-keywords
  (list
   '("\\b\\(attribute\\|element\\)\\b\\([^{]+\\){" 2
     font-lock-variable-name-face)
   '("[a-zA-Z][-a-zA-Z0-9._]*:[a-zA-Z][-a-zA-Z0-9._]*" . italic)
   '("\\b\\(default\\(\\s +namespace\\)?\\|namespace\\|datatypes\\)\\(\\s +[a-zA-Z][-a-zA-Z0-9._]*\\)?\\s *=" 1 font-lock-preprocessor-face)
   '("\\([a-zA-Z][-a-zA-Z0-9._]*\\)\\(\\s \\|\n\\)*[|&]?=" 1
     font-lock-function-name-face)
   '("[a-zA-Z][a-zA-Z0-9._]*\\(-[a-zA-Z][a-zA-Z0-9._]*\\)+" . default)
   (cons (rnc-make-regexp-choice rnc-atoms) 'italic)
   (cons (rnc-make-regexp-choice rnc-keywords) font-lock-keyword-face)
   )
  "RNC Highlighting")


(defun rnc-find-column (first start)
  "Find which column to indent to."

  ;; FIXME: backward-sexp doesn't work with unbalanced braces in comments

  (let* (column
	 pos
	 ;; Find start of enclosing block or assignment
	 (token
	  (if (member first '("]" "}" ")"))
	      (progn
		(goto-char (+ start 1))
		(backward-sexp)
		(beginning-of-line)
		(re-search-forward "\\S ")
		(setq pos (point))
		(setq column (- (current-column) 1))
		'lpar)
	    (catch 'done
	      (while (setq pos (re-search-backward "[{}()=]\\|\\[\\|\\]"
						   (point-min) t))
		(let ((c (match-string 0)))
		  (beginning-of-line)
		  (re-search-forward "\\S ")
		  (setq column (- (current-column) 1))
		  (beginning-of-line)
		  (cond
		   ;; Don't match inside comments
		   ;; FIXME: Should exclude matches inside string literals too
		   ((re-search-forward "#" pos t) (beginning-of-line))
		   ;; Skip block
		   ((member c '("]" "}" ")"))
		    (goto-char (+ pos 1))
		    (backward-sexp))

		   ((string= c "=") (throw 'done 'eq))
		   (t (throw 'done 'lpar)))))))))

    (cond
     ((not pos) 0)
     ((member first '("]" "}" ")")) column)
     ((member first '("{" "(")) (+ column rnc-indent-level))

     ;; Give lines starting with an operator a small negative indent.
     ;; This allows for the following indentation style:
     ;;   foo =
     ;;      bar
     ;;    | baz
     ;;    | oof
     ((member first '("," "&" "|")) (+ column (- rnc-indent-level 2)))

     ;; Check if first preceding non-whitespace character was an operator
     ;; If not, this is most likely a new assignment.
     ;; FIXME: This doesn't play well with name classes starting on a new
     ;; line
     ((eq token 'eq)
      (goto-char start)
      (if (and (re-search-backward "[^ \t\n]" (point-min) t)
	       (member (match-string 0) '("&" "|" "," "=" "~")))
	  (+ column rnc-indent-level)
	column))

     (t (+ column rnc-indent-level)))))

(defun rnc-indent-line ()
  "Indents the current line."
  (interactive)
  (let ((orig-point (point)))
    (beginning-of-line)
    (let* ((beg-of-line (point))
	   (pos (re-search-forward "\\(\\S \\|\n\\)" (point-max) t))
	   (first (match-string 0))
	   (start (match-beginning 0))
	   (col (- (current-column) 1)))

      (goto-char beg-of-line)

      (let ((indent-column (rnc-find-column first start)))
	(goto-char beg-of-line)

	(cond
	 ;; Only modify buffer if the line must be reindented
	 ((not (= col indent-column))
	  (if (not (or (null pos)
		       (= beg-of-line start)))
	      (kill-region beg-of-line start))

	  (goto-char beg-of-line)

	  (while (< 0 indent-column)
	    (insert " ")
	    (setq indent-column (- indent-column 1))))

	 ((< orig-point start) (goto-char start))
	 (t (goto-char orig-point)))))))


(defun rnc-electric-brace (arg)
  (interactive "*P")
  (self-insert-command (prefix-numeric-value arg))
  (rnc-indent-line)
  (let ((p (point)))
    (when (save-excursion
	    (beginning-of-line)
	    (let ((pos (re-search-forward "\\S " (point-max) t)))
	      (and pos (= (- pos 1) p))))
      (forward-char))))

(defvar rnc-mode-map () "Keymap used in RNC mode.")
(when (not rnc-mode-map)
  (setq rnc-mode-map (make-sparse-keymap))
  (define-key rnc-mode-map "\C-c\C-c" 'comment-region)
  (define-key rnc-mode-map "}" 'rnc-electric-brace)
  (define-key rnc-mode-map "{" 'rnc-electric-brace)
  (define-key rnc-mode-map "]" 'rnc-electric-brace)
  (define-key rnc-mode-map "[" 'rnc-electric-brace))

;;;###autoload
(defun rnc-mode ()
  "Major mode for editing RELAX NG Compact Syntax schemas.
\\{rnc-mode-map}"
  (interactive)

  (kill-all-local-variables)

  (make-local-variable 'indent-line-function)
  (setq indent-line-function 'rnc-indent-line)

  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults '(rnc-font-lock-keywords nil t nil nil))

  (use-local-map rnc-mode-map)

  (make-local-variable 'comment-start)
  (make-local-variable 'comment-end)
  (make-local-variable 'comment-start-skip)

  (setq comment-start "#"
	comment-end ""
	comment-start-skip "\\([ \n\t]+\\)##?[ \n\t]+")

  (let ((rnc-syntax-table (copy-syntax-table)))
    (modify-syntax-entry ?# "<   " rnc-syntax-table)
    (modify-syntax-entry ?\n ">   " rnc-syntax-table)
    (modify-syntax-entry ?\^m ">   " rnc-syntax-table)
    (modify-syntax-entry ?\\ "w   " rnc-syntax-table)
    (modify-syntax-entry ?' "\"   " rnc-syntax-table)
    (modify-syntax-entry ?. "w   " rnc-syntax-table)
    (modify-syntax-entry ?- "w   " rnc-syntax-table)
    (modify-syntax-entry ?_ "w   " rnc-syntax-table)
    (set-syntax-table rnc-syntax-table))

  (setq mode-name "RNC"
	major-mode 'rnc-mode)
  (run-hooks 'rnc-mode-hook))

(provide 'rnc-mode)
