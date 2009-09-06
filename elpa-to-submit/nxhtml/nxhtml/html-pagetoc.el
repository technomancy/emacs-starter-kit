;;; html-pagetoc.el --- Insert/rebuild table of contents for html page

;; Copyright (C) 2004 by Lennart Borgman

;; Author:     Lennart Borgman <lennart DOT borgman DOT 073 AT student DOT lu DOT se>
;; Created: 2005-08-03
;; Last-Updated: Sat Apr 21 14:11:13 2007 (7200 +0200)
(defconst html-pagetoc:version "0.85") ;; Version:
;; Keywords: tools hypermedia html
;; Features that might be required by this library:
;;
;;   None
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This file is not part of Emacs

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:

;; html-pagetoc.el has functions for building (and rebuilding) a
;; simple table of contents for a single html file. It is supposed to
;; be a quick tool for this.  The table of contents are made from the
;; header tags (H1, H2, H3 etc).  If you have ID attributes on the
;; header the table of contents will have links to those. Otherwise it
;; is just text.

;; To use this module put it in emacs load-path and enter the line
;; below in your .emacs:
;;
;;    (require 'html-pagetoc)
;;
;; When editing a html file put your cursor where you want the table
;; of contents and do M-x html-pagetoc-insert-toc.
;;
;; To rebuild the table of contents use M-x html-pagetoc-rebuild-toc.
;; If you want to add styles to it you can use M-x
;; html-pagetoc-insert-style-guide.
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

;;(define-key global-map [f2] 'eval-buffer)
;;(define-key global-map [f3] 'html-pagetoc-insert-toc)

(defgroup html-pagetoc nil
  "Html page local table of contents settings"
  :group 'nxhtml
  :group 'hypermedia)

(defcustom html-pagetoc-tocheads
  '(
    ("" . "On THIS Page:")
    )
  "Head titles for table of contents.
The titles are put above the table of contents.

The value of this variable should be a list of cons cells where
the car is a regexp to match against file names and the cdr is
the head title to use.  The first match in the list is used.  If
there is no match then no head title is inserted."
  :type '(repeat (cons regexp string))
  :group 'html-pagetoc)

(defcustom html-pagetoc-min 1
  "Default for min header level"
  :type 'integer
  :group 'html-pagetoc)
(make-variable-buffer-local 'html-pagetoc-min)

(defcustom html-pagetoc-max 3
  "Default for max header level"
  :type 'integer
  :group 'html-pagetoc)
(make-variable-buffer-local 'html-pagetoc-max)

(defconst html-pagetoc-begin-cmnt "<!-- Table of contents BEGIN -->\n")
(defconst html-pagetoc-end-cmnt   "<!-- END of Table of contents -->\n")
(defconst html-pagetoc-maxmin-cmnt "<!-- Table of contents min=%s max=%s -->\n")

;;(defconst html-pagetoc-buffers nil)

(defun html-pagetoc-get-title (filename)
  "Find the head title for filename.
See `html-pagetoc-tocheads'."
  (when filename
    (let ((ths html-pagetoc-tocheads)
          th
          re
          header)
      (while (and ths (not header))
        (setq th (car ths))
        (setq ths (cdr ths))
        (setq re (car th))
        (when (string-match re filename)
          (setq header (cdr th))))
      header)))

;;;###autoload
(defun html-pagetoc-insert-toc (&optional min-level max-level)
  "Inserts a table of contents for the current html file.
The html header tags h1-h6 found in the file are inserted into
this table.  MIN-LEVEL and MAX-LEVEL specifies the minimum and
maximum level of h1-h6 to include.  They should be integers."
  (interactive (let* ((maxstr)
		       (max 0)
		       (min 1)
		       (prmax (format "Max header level (%s): " html-pagetoc-max))
		       (prmax2 (concat "Please give an integer 1-5. " prmax))
		       (prmin "Include header level 1? ")
		       )
		  (while (= max 0)
		    (setq maxstr (read-string prmax))
		    (if (equal maxstr "")
			(setq max html-pagetoc-max)
		      (when (not (string-match "\\." maxstr))
			(setq max (string-to-number maxstr)) ))
		    (when (> max 5) (setq max 0))
		    (when (< max 0) (setq max 0))
		    (setq prmax prmax2) )
		  (when (> max 1)
		    (when (not (y-or-n-p prmin)) (setq min 2)))
		  (list min max)))

  (let* ((curr-buffer (current-buffer))
         (header (html-pagetoc-get-title (buffer-file-name)))
	 (toc-buffer (get-buffer-create "*html-pagetoc*"))
	 (toc)
	 (buffer-val (cons (buffer-file-name) (list min-level max-level)))
	)
    (setq html-pagetoc-min min-level)
    (setq html-pagetoc-max max-level)
    (save-excursion
      (set-buffer toc-buffer)
      (erase-buffer))
    (with-temp-buffer
      (insert-buffer-substring curr-buffer)
      ;;(replace-regexp "<!--.*?-->" "")
      (save-excursion
        (goto-char (point-min))
        (while (re-search-forward "<!--.*?-->" nil t)
          (replace-match "" nil nil))
	(goto-char (point-min))
	(let ((b (current-buffer))
	      (standard-output toc-buffer)
	      (level (- min-level 1))
	      (skip-level (- min-level 1))
	      (prev-level)
	      )
	  (princ html-pagetoc-begin-cmnt)
          (princ (format
                  html-pagetoc-maxmin-cmnt
                  min-level
                  max-level))
	  (princ "<table id=\"PAGETOC\"><tr><td>\n")
          (when header
            (princ "<span class=\"tochead\">")
            (princ header)
            (princ "</span>\n"))
	  (while (re-search-forward
		  (concat "\\(?:<h\\([1-9]\\)\\([^>]*\\)>\\(.*?\\)</h[1-9]>"
			  "\\|"
			  "<!--\\(?:.\\|\n\\)-->\\)")
		  nil t)
	    (let ((m0 (match-string 0))
		  (m1 (match-string 1))
		  (m2 (match-string 2))
		  (title (match-string 3))
		  (id)
		  (new-level)
		  )
	    (unless (not m1)
	      (setq new-level (string-to-number m1))
	      (when (and (<= new-level max-level) (<= min-level new-level))
		(setq prev-level level)
		(setq level new-level)
		(while (< prev-level level)
		  (princ (make-string (* (- prev-level skip-level) 4) 32))
		  ;; class liul is a fix for a problem in IE
		  (when (> prev-level (- min-level 1)) (princ "<li class=\"liul\">"))
		  (princ "<ul>\n")
		  (setq prev-level (+ prev-level 1)))
		(while (> prev-level level)
		  (princ (make-string (* (- prev-level skip-level) 4) 32))
		  (princ "</ul></li>\n")(setq prev-level (- prev-level 1)))
		(when (nth 3 (match-data t))
		  (when (string-match "id=\"\\([^\"]*\\)\"" m2)
		    (setq id (substring m2 (match-beginning 1) (match-end 1)))))
		(princ (make-string (* (- level skip-level) 4) 32))
		(princ "<li>")
		(if id
		    (princ (format "<a href=\"#%s\">%s</a>" id title))
		  (princ title))
		(princ "</li>\n")
		))))
	  (while (> level (- min-level 1))
	    (setq level (- level 1))
	    (princ (concat (make-string (* (- level skip-level) 4) 32) "</ul>"))
	    (when (> level (- min-level 1)) (princ "</li>"))
	    (princ "\n"))
	  (princ "</td></tr></table>\n")
	  (princ html-pagetoc-end-cmnt)
	  (save-excursion
	    (set-buffer toc-buffer)
	    (setq toc (buffer-string)))
	  )
	) ; save-excursion
      ) ; with-temp-buffer
    (when toc
      (when (re-search-forward "<body.*?>" nil t)
        (forward-line))
      (set-mark (point))
      (insert toc)
      (let ((start (copy-marker (region-beginning)))
	    (end (copy-marker (region-end))))
	(indent-region (region-beginning) (region-end) nil)
	(set-mark start)
	(goto-char end))
      (setq deactivate-mark nil)
      (message "Toc created"))
    )
  )

(defun html-pagetoc-insert-style-guide ()
  "Inserts a style tag for toc inserted by `html-pagetoc-insert-toc'.
This can be used as a guide for creating your own style sheet for
the table of contents."
  (interactive)
  (goto-char (point-min))
  (unless (re-search-forward "^\\s-*</head>")
    (error "%s" "Can not find ^\\s-*</head>"))
  (beginning-of-line)
  (set-mark (point))
  (insert "\n")
  (insert "<!-- Style for the table of contents. -->\n")
  (insert "<style type=\"text/css\">\n")
  (insert "#PAGETOC {\n")
  (insert "    background-color: #df7;\n")
  (insert "    padding: 0.5em;\n")
  (insert "}\n")
  ;;(insert "#PAGETOC strong { color: #ac4; }\n")
  (insert "#PAGETOC a { color: maroon; display: block; }\n")
  (insert "#PAGETOC a:hover { background-color: yellow; }\n")
  (insert "#PAGETOC ul {\n")
  (insert "    list-style-type: none;\n")
  (insert "    margin-left: 0;\n")
  (insert "    padding-left: 1.5em;\n")
  (insert "}\n")
  (insert "#PAGETOC ul li { font-weight: bold; }\n")
  (insert "#PAGETOC ul li ul { }\n")
  (insert "#PAGETOC ul li ul li {  font-weight: normal;}\n")
  (insert "#PAGETOC .liul {\n")
  (insert "    //display:inline; /* IE fix */\n")
  (insert "}\n")
  (insert "#PAGETOC .tochead {\n")
  (insert "    font-weight: bold;\n")
  (insert "    margin-bottom: 0.5em;\n")
  (insert "}\n")
  (insert "</style>\n")
  (insert "\n")
  (let ((start (copy-marker (region-beginning)))
	(end (copy-marker (region-end))))
    (indent-region (region-beginning) (region-end) nil)
    (set-mark start)
    (goto-char end))
  (setq deactivate-mark nil)
  (message "Please edit the style guide!")
  )

;;;###autoload
(defun html-pagetoc-rebuild-toc ()
  "Update the table of contents inserted by `html-pagetoc-insert-toc'."
  (interactive)
  (let* (;;(old-val (assoc (buffer-file-name) html-pagetoc-buffers))
	 ;;(old-min (nth 1 old-val))
	 ;;(old-max (nth 2 old-val))
	 (old-min html-pagetoc-min)
	 (old-max html-pagetoc-max)
         )
    (goto-char (point-min))
    (if (not (search-forward html-pagetoc-begin-cmnt nil t))
        (when (y-or-n-p "Could not find table of contents. Insert one here? ")
          (html-pagetoc-insert-toc))
      (backward-char 4)
      (beginning-of-line)
      (let ((minmax-patt (format html-pagetoc-maxmin-cmnt "\\([[:alnum:]]+\\)" "\\([[:alnum:]]+\\)")))
        (save-excursion
          (when (search-forward-regexp minmax-patt nil t)
            (setq old-min (string-to-number (match-string 1)))
            (setq old-max (string-to-number (match-string 2))))))
      (let ((start-toc (point)))
	(when (search-forward html-pagetoc-end-cmnt)
	  (beginning-of-line)
	  (let ((end-toc (point)))
	    (set-mark start-toc)
	    (goto-char end-toc)
	    (when (y-or-n-p "Rebuild this TOC? ")
	      ;;(unless old-min (setq old-min 1))
              (setq old-min (eval-minibuffer "Min TOC level: " (format "%s" old-min)))
	      ;;(unless old-max (setq old-max 3))
              (setq old-max (eval-minibuffer "Max TOC level: " (format "%s" old-max)))
	      (delete-region start-toc end-toc)
	      (html-pagetoc-insert-toc old-min old-max ))))))))

;;;###autoload
(defconst html-pagetoc-menu-map
  (let ((map (make-sparse-keymap)))
    (define-key map [html-pagetoc-rebuild-toc]
      (list 'menu-item "Update Page TOC" 'html-pagetoc-rebuild-toc))
    (define-key map [html-pagetoc-insert-style-guide]
      (list 'menu-item "Insert CSS Style for Page TOC" 'html-pagetoc-insert-style-guide))
    (define-key map [html-pagetoc-insert-toc]
      (list 'menu-item "Insert Page TOC" 'html-pagetoc-insert-toc))
    map))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; Ready:
(provide 'html-pagetoc)

;;; html-pagetoc.el ends here
