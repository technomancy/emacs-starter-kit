;;; css-palette.el

(defconst css-palette:version "0.02")
;; Copyright (C) 2008 Niels Giesen

;; Author: Niels Giesen <nielsforkgiesen@gmailspooncom, but please
;; replace the kitchen utensils with a dot before hitting "Send">
;; Keywords: processes, css, multimedia, extensions, tools
;; Homepage: http://niels.kicks-ass.org/

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; css-palette defines commands to have "palettes" inside a block
;; comment to circumvent the absence of (color or other) variable
;; definitions in the CSS specification. It can import and export GIMP
;; color palettes. See the documentation of `css-palette-mode'
;; for details of usage.

;;; Installation:

;; Something like:

;; put it in your load-path.

;; (autoload 'css-palette-mode "css-palette" "" t)
;; (add-hook 'css-mode-hook
;; 	  (lambda ()
;; 	    (css-palette-mode t)))

;; Notes:

;; css-palette depends on css-color.el to do font-locking.

;; ccs-palette is orthogonal to css-mode, so it could probably be used
;; inside other language modes, provided they support multiline block
;; comments.

;;; Change log:

;; 2009-01-11 Lennart Borgman
;;   - Minor code clean up.

;;; Code:
(require 'css-color)
(eval-when-compile (require 'cl))		;i'm a bad bad boy...

(defconst css-palette-hex-chars "0123456789abcdefABCDEF"
  "Composing chars in hexadecimal notation, save for the hash (#) sign.")

(defvar css-palette-mode-map
  (let ((m (make-sparse-keymap)))
    (define-key m "\C-c\C-c" 'css-palette-update-all)
    (define-key m "\C-c\C-i" 'css-palette-insert-reference)
    (define-key m "\C-c\C-p" 'css-palette-import-from-GIMP)
    (define-key m "\C-c\C-f" 'css-palette-insert-files)
    m)
  "Mode map for `css-palette-mode'")

;;;###autoload
(define-minor-mode css-palette-mode
  "Minor mode for palettes in CSS.

The mode `css-palette-mode' acts on the first COLORS declaration in your
  file of the form:

COLORS:
\(
c0 \"#6f5d25\"	;tainted sand
c1 \"#000000\"	;Black
c2 \"#cca42b\"	;goldenslumber
c3 \"#6889cb\"	;far off sky
c4 \"#fff\"	;strange aeons
)

Such declarations should appear inside a block comment, in order
  to be parsed properly by the LISP reader.

Type \\[css-palette-update-all], and any occurence of

  color: #f55; /*[c3]*/

will be updated with

  color: #6899cb; /*[c3]*/

The following commands are available to insert key-value pairs
  and palette declarations:
  \\{css-palette-mode-map}

You can extend or redefine the types of palettes by defining a
  new palette specification of the form (PATTERN REGEXP
  REF-FOLLOWS-VALUE), named according to the naming scheme
  css-palette:my-type, where

PATTERN is a pattern containing two (%s) format directives which
  will be filled in with the variable and its value,

REGEXP is a regular expression to match a value - variable
  pattern,

and REF-FOLLOWS-VALUE defined whether or not the reference comes
  after the value. This allows for more flexibility.

Note that, although the w3c spec at URL
  `http://www.w3.org/TR/CSS2/syndata.html#comments' says that
  comments \" may occur anywhere between tokens, and their
  contents have no influence on the rendering\", Internet
  Explorer does not think so. Better keep all your comments after
  a \"statement\", as per the default. This means `css-palette'
  is ill-suited for use within shorthands.

See variable `css-palette:colors' for an example of a palette
  type.

The extension mechanism means that palette types can be used to
  contain arbitrary key-value mappings.

Besides the colors palette, css-palette defines the palette
  definition variables `css-palette:colors-outside' and
  `css-palette:files', for colors with the reference outside and
  for file url()'s respectively.

You can fine-control which palette types css-palette should look
  at via the variable `css-palette-types'.

"
  nil
  "-palette"
  css-palette-mode-map
  (css-color-mode +1))

;;;###autoload
(defgroup css-palette nil
  "Customization group for css-palette library.

See function `css-palette-mode' for documentation"
  :group 'css-color)

(defcustom css-palette:colors
  `("%s; /*[%s]*/ "
    ,(concat "\\("
	     css-color-color-re
;; 	      (mapconcat
;; 	       'identity
;; 	       (list css-color-hex-re
;; 		     css-color-hsl-re
;; 		     css-color-rgb-re) "\\|")
	      "\\)"
	      "[[:space:]]*;[[:space:]]*\/\\*\\[\\([^[:space:]]+\\)\\]\\*\/")
    t)
  "Color palette specification.

See function `css-palette-mode' for documentation"
  :group 'css-palette
  :type '(list
	  (string :tag "Pattern")
	  (regexp :tag "Regexp")
	  (boolean :tag "Reversed")))

(defcustom css-palette:files
  '("url(%s); /*[%s]*/ "
    "url(\\([^)]+\\))[[:space:]]*;[[:space:]]*\/\\*\\[\\([^[:space:]]+\\)\\]\\*\/"
    t)
  "File palette specification.

See function `css-palette-mode' for documentation"
  :group 'css-palette
  :type '(list
	  (string :tag "Pattern")
	  (regexp :tag "Regexp")
	  (boolean :tag "Reversed")))

(defcustom css-palette-types
  '(colors)
  "List of palette types to check for in buffer.

See function `css-palette-mode' for documentation"
  :group 'css-palette
  :type '(repeat (symbol :tag "Palette type")))
(make-variable-buffer-local 'css-palette-types)

;; (defun css-palette-mode-turn-on ()
;;   "Turn on `css-palette-mode'."
;;   (css-palette-mode 1))

;; ;;;###autoload
;; (defcustom css-palette-mode-activate-p nil
;; "Start `css-palette-mode' when `css-mode' is activated."
;;   :group 'css-palette
;;   :set (lambda (sym val)
;;          (set-default sym val)
;;          (if val
;;              (add-hook 'css-mode-hook 'css-palette-mode-turn-on)
;;            (remove-hook 'css-mode-hook 'css-palette-mode-turn-on)))
;;   :type 'boolean)

(defun css-palette-turn-on-in-buffer ()
  "Turn on `css-palette-mode' in `css-mode'."
  (when (derived-mode-p 'css-mode)
    (message "turn-on-in-b:before (css-palette-mode 1) cb=%s" (current-buffer))
    (css-palette-mode 1)
    (message "turn-on-in-b:after (css-palette-mode 1)")
    ))

;;;###autoload
(define-globalized-minor-mode css-palette-global-mode css-palette-mode
  css-palette-turn-on-in-buffer
  :group 'css-color)

(defun css-palette-get (key spec)
  (plist-get
   (css-palette-spec-to-plist
    (symbol-value
     (intern-soft
      (format "css-palette:%s" spec)))) key))

(defun css-palette-spec-to-plist (palette)
  (destructuring-bind (pattern regexp ref-follows-value) palette
    (list :regexp regexp
	  :pattern pattern
	  :ref-follows-value ref-follows-value)))

(defun css-palette-choose-type ()
  (intern-soft
   (if (null (cdr css-palette-types))
       (car css-palette-types)
     (completing-read "Type: "
		      (mapcar 'symbol-name css-palette-types)))))

(defun css-palette-get-declaration (type)
  "Return `css-palette' declaration of TYPE in current buffer.

If none is found, throw an error."
  (let ((type (symbol-name type)))
    (save-excursion
      (goto-char (point-min))
      (or (re-search-forward (format "%s:"
				     (upcase type)) nil t)
	  (error "No %s declaration found in buffer; check value of variable
	  `css-palette-types'" type))
      (let ((palette (read (current-buffer))))
	;; Check (could be better..)
	(if (not (and
		  (listp palette)
		  (= 0 (% (length palette) 2))))
	    (error "Invalid %s " type))
	palette))))

(defun css-palette-update (type)
"Update buffer references for palette of TYPE."
  (interactive (list
		(css-palette-choose-type)))
  (let ((palette (css-palette-get-declaration type))
	(regexp (css-palette-get :regexp type))
	(ref-follows-value (css-palette-get :ref-follows-value type)))
    (flet ((getval (key palette)
		      (let ((value (plist-get palette (intern-soft key))))
			(if (null value)
			    (error
			     "%S not specified in %S palette "
			     key
			     type
			     ;;  (signal 'css-palette-not-found-error nil)
			     )
			  value))))
      (save-excursion
	(goto-char (point-min))
	(while (re-search-forward
		regexp
		(point-max) t)
	  (replace-match
	   (getval (match-string-no-properties (if ref-follows-value 2 1)) palette)
	   nil nil nil (if ref-follows-value 1 2))))))
   (css-color-mode 1))

(defun css-palette-update-all ()
  "Update all references for palettes in `css-palette-types'"
  (interactive)
  (catch 'err
    (mapc (lambda (type)
	    (condition-case err
		(css-palette-update type)
	      (if (y-or-n-p (format "%s, skip? " err))
		  nil)))
	  css-palette-types)))

;; Reference Insertion
(defun css-palette-insert-reference (type)
  "Insert `css-palette' reference of TYPE at point."
  (interactive
    (list (css-palette-choose-type)))
  (let* ((palette (css-palette-get-declaration type))
	 (ref-follows-value (css-palette-get :ref-follows-value type))
	 (pattern (css-palette-get :pattern type))
	 (var
	  (completing-read (format "%s variable: "
				   (capitalize
				    (substring (symbol-name type)
					       0 -1)))
			   (loop for i on
				 palette
				 by 'cddr
				 collect
				 (css-palette-colorify
				  (symbol-name (car i))
				  (cadr i)))))
	 (val  (plist-get palette (read var))))
    (insert (apply 'format
		   pattern
		   (if ref-follows-value
		       (list val var)
		     (list var val))))
    (css-color-mode +1)))

(defun css-palette-hex-color-p (str)
  (string-match "#\\([a-fA-F[:digit:]]\\{6\\}\\|[a-fA-F[:digit:]]\\{3\\}\\)" str))

(defun css-palette-colorify (string color)
  (let ((color (if (css-palette-hex-color-p color)
		   color
		 "#000")))
  (propertize string
	      'font-lock-face
	      (list :background color
		    :foreground (css-color-foreground-color color)
		    string)
	      'fontified t)))

;; Imports
(defun css-palette-from-existing-colors ()
  (interactive)
  (let ((palette)
	(count -1))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "#[[:digit:]a-fA-F]\\{6\\}\\>" nil t)
	(if (not (member (match-string-no-properties 0) palette))
	    (setq palette (append (list
				   (match-string-no-properties 0)
				   (intern(format "c%d" (incf count))))
				  palette)))
	(save-match-data (re-search-forward ";" nil t))
	(insert (format "/*[%S]*/" (cadr (member (match-string-no-properties 0) palette))))))
    (insert (format "COLORS:\n%S" (nreverse palette)))
    (forward-sexp -1)
    (forward-char 1)
    (while
	(not (looking-at ")"))
      (forward-sexp 2)
      (newline)
      (indent-for-tab-command))))

(defun css-palette-newest-GIMP-dir ()
  "Return newest (version-wise) ~/.gimp-n.n/palettes directory on disk.

Return `nil' if none such directory is found."
  (catch 'none
    (concat
     (or
      (car
       (last
	(directory-files "~/" t "^.gimp-[[:digit:].]\\{3,\\}")))
      (throw 'none ()))
   "/palettes/")))

(defun css-palette-import-from-GIMP ()
  "Import GIMP palette file as a `css-palette' palette.

GIMP palettes can be made with the GIMP or on-line tools such as
found at URL `http://colourlovers.com'."
  (interactive)
  (let ((file (read-file-name "File: " (css-palette-newest-GIMP-dir)))
	(this-buffer (current-buffer))
	(count -1))
    (insert "\nCOLORS:\n(\n")
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (while (re-search-forward
	      (concat
	       "^"
	       "[[:space:]]*\\([[:digit:]]+\\)"  ;red
	       "[[:space:]]+\\([[:digit:]]+\\)"  ;green
	       "[[:space:]]+\\([[:digit:]]+\\)"	 ;blue
	       "[[:space:]]+\\(.*\\)$") ;name (=> used as comment)
	      nil t)
	(destructuring-bind (rb re gb ge bb be nb ne &rest ignore)
	    (cddr (match-data t))
	  (let ((color
		 (apply 'format "c%d \"#%02x%02x%02x\" ;%s\n"
			(incf count)
			(append
			 (mapcar 'string-to-number
				 (list
				  (buffer-substring-no-properties rb re)
				  (buffer-substring-no-properties gb ge)
				  (buffer-substring-no-properties bb be)))
			 (list (buffer-substring-no-properties nb ne))))))
	    (save-excursion
	      (set-buffer this-buffer)
	      (insert color))))))
    (insert ")")
    (message "C-c C-c to update colors")))

(defun css-palette-insert-files (dir)
  "Insert a `css-palette' declaration for all files in DIR.

Filenames are relative.
Main use-case: an image directory."
  (interactive "DDirectory: ")
  (save-excursion
    (let ((image-count -1))
      (insert "\nFILES:\n(\n")
      (mapc
       (lambda (f)
	 (insert
	  (format "file-%d %S\n"
		  (incf image-count)
		  (file-relative-name
		   f
		   (file-name-directory (buffer-file-name))))))
       (directory-files dir t "...+"))
      (insert ")\n\n"))))

;; Exports
(defun css-palette-export-to-GIMP (type name columns)
  "Export the COLORS declaration to a GIMP (.gpl) palette.

See also `gpl-mode' at URL
`http://niels.kicks-ass.org/public/elisp/gpl.el'."
  (interactive
   (list
    (css-palette-choose-type)
    (read-string "Name: ")
    (read-number "Number of columns: " 2)))
  (let ((palette (css-palette-get-declaration type)))
    (find-file
     (concat (css-palette-newest-GIMP-dir)
	     name
	     ".gpl"))
    (insert
     (format "GIMP Palette
Name: %s
Columns: %d
#
" name columns))
    (loop for i on palette
	  by 'cddr
	  do
	  (multiple-value-bind (r g b)(css-color-hex-to-rgb
				       (css-color-hexify-anystring (cadr i)))
	    (insert (format "%3d %3d %3d\t%s\n"
			    r g b
			    (car i))))))
  (if (featurep 'gpl)
      (gpl-mode)))

(provide 'css-palette)
;; css-palette.el ends here
