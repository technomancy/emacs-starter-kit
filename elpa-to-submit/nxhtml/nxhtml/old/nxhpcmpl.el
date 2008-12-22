;;; nxhpcmpl.el --- Popup completion
;;
;; Author: Lennart Borgman
;; Created: Thu Dec 28 23:23:21 2006
(setq nxhpcmpl:version "0.50") ;; Version:
;; Lxast-Updated: Fri Dec 29 01:42:55 2006 (3600 +0100)
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
;; Functions for pop style completion for nxhtml-mode.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change log:
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(require 'appmenu)

(defun nxhpcmpl-complete ()
  "Complete the string before point using the current schema.
Return non-nil if in a context it understands."
  (interactive)
  ;;(message "where=%s" (nxhtml-check-where)) (sit-for 1)
  (unless (eq 'in-tag-start (nxhtml-check-where))
    (error "Not in start tag"))
  (and rng-validate-mode
       (let ((lt-pos (save-excursion (search-backward "<" nil t)))
	     xmltok-dtd)
	 (and lt-pos
	      (= (rng-set-state-after lt-pos) lt-pos)
	      (or (nxhpcmpl-complete-tag lt-pos)
;;                   t
;; 		  (rng-complete-end-tag lt-pos)
;; 		  (rng-complete-attribute-name lt-pos)
;; 		  (rng-complete-attribute-value lt-pos)
                  )))))

(defun nxhpcmpl-complete-tag (lt-pos)
  (let (rng-complete-extra-strings)
    (when (and (= lt-pos (1- (point)))
	       rng-complete-end-tags-after-<
	       rng-open-elements
	       (not (eq (car rng-open-elements) t))
	       (or rng-collecting-text
		   (rng-match-save
		     (rng-match-end-tag))))
      (setq rng-complete-extra-strings
	    (cons (concat "/"
			  (if (caar rng-open-elements)
			      (concat (caar rng-open-elements)
				      ":"
				      (cdar rng-open-elements))
			    (cdar rng-open-elements)))
		  rng-complete-extra-strings)))
    (when (save-excursion
	    (re-search-backward rng-in-start-tag-name-regex
				lt-pos
				t))
      (and rng-collecting-text (rng-flush-text))
;;       (let ((completion
;; 	     (let ((rng-complete-target-names
;; 		    (rng-match-possible-start-tag-names))
;; 		   (rng-complete-name-attribute-flag nil))
      (let* ((orig (buffer-substring-no-properties (1+ lt-pos) (point)))
             (rng-complete-target-names
              (rng-match-possible-start-tag-names))
             (rng-complete-name-attribute-flag nil)
             my-list
             (olen (length orig)))
        ;;(message "orig=%s, l=%s" orig olen)(sit-for 1)
        (dolist (alt (rng-generate-qname-list))
          (when (string= orig
                         (if (> (length alt) olen)
                             (substring alt 0 olen)
                           alt))
            (setq my-list (cons alt my-list))))
        ;;(message "qnl=%s" (rng-generate-qname-list))(sit-for 2)
        ;;(message "myl=%s" my-list)
        ;;(message "%s" (try-completion "a" 'rng-complete-qname-function nil))(sit-for 2)
        ;;(rng-complete-before-point (1+ lt-pos) 'rng-complete-qname-function "Tag: " nil 'rng-tag-history)
        (let ((sets nil)
              s-cdr)
          (dolist (tg my-list)
            (let (found)
              (dolist (s nxhpcmpl-tag-sets)
                (when (member tg (cdr s))
                  (setq found t)
                  (let ((sets-entry (assq (car s) sets)))
                    (unless sets-entry
                      (setq sets (cons (list (car s)) sets))
                      (setq sets-entry (assq (car s) sets)))
                    (setcdr sets-entry (cons tg (cdr sets-entry))))))
              (unless found
                  (let ((sets-entry (assq 'unsorted sets)))
                    (unless sets-entry
                      (setq sets (cons (list 'unsorted) sets))
                      (setq sets-entry (assq 'unsorted sets)))
                    (setcdr sets-entry (cons tg (cdr sets-entry)))))))
          (setq sets (sort sets (lambda(a b)
                                  (string< (format "%s" b)
                                           (format "%s" a)))))
          (dolist (s sets)
            (setcdr s (reverse (cdr s))))
          ;;(message "sets=%s" sets)
          (let ((pop-map (make-sparse-keymap "Insert Tag")))
            (dolist (s sets)
              (let ((k (make-sparse-keymap)))
                (dolist (tg (cdr s))
                  ;;(message "tg=%s" tg)
                  (define-key k
                    (read (format "[nx-%s]" tg))
                    (list 'menu-item
                          (format "%s" (let ((h (gethash tg nxhtml-completion-hash)))
                                         (if h h tg)))
                          `(lambda()
                             (interactive)
                             (unless (looking-back (concat "<" ,orig))
                               (error "Not looking at %s" ,orig))
                             (delete-char (- (length (match-string 0))))
                             (insert "<" ,tg)
                             ;;(message "e=%s" ,tg)
                             )
                          :value tg)))
                (define-key pop-map
                  (read (format "[nxp-%s]" (car s)))
                  (list 'menu-item
                        (capitalize (format "%s" (car s)))
                        k))))
            ;;(message "pm=%s" pop-map)
            (let ((where (appmenu-point-to-coord (point)))
                  (ret t))
              (condition-case err
                  (progn
                    (popup-menu pop-map where)
                    t)
                (quit nil)))))))))

(defconst nxhpcmpl-tag-sets nil)

(defun nxhpcmpl-make-tag-sets(elt)
  (let* ((group (nth 0 elt))
         (tag   (nth 2 elt))
         (saved-grp (assq group nxhpcmpl-tag-sets))
         tmp
         name
         )
    (dolist (c (cdr (append tag nil)))
      (unless name
        (if (memq c '(?\  ?>))
            (setq name (concat (reverse tmp)))
          (setq tmp (cons c tmp)))))
    (unless name
      (setq name (concat (reverse tmp))))
    ;;(message "group=%s, sg=%s, name=%s, tag=%s" group saved-grp name tag)
    (if saved-grp
        (let ((saved-cdr (cdr saved-grp)))
          (unless (member name saved-cdr)
            (setcdr saved-grp (cons name (cdr saved-grp)))))
      (setq nxhpcmpl-tag-sets (cons (list group name) nxhpcmpl-tag-sets)))
    ))

(mapcar
 'nxhpcmpl-make-tag-sets
 '(
   ;;entities
   (entity  "\C-c#"   "&#"              "Ascii Code"      ("&#" (r "Ascii: ") ";"))
   (entity  "\C-c\""  "&quot;"          "Quotation mark"  ("&quot;"))
   (entity  "\C-c$"   "&reg;"           "Registered"      ("&reg;"))
   (entity  "\C-c@"   "&copy;"          "Copyright"       ("&copy;"))
   (entity  "\C-c-"   "&shy;"           "Soft Hyphen"     ("&shy;"))
   (entity  "\C-c "   "&nbsp;"		"Nonbreaking Space"  ("&nbsp;"))
   (entity  "\C-c&"   "&amp;"		"Ampersand"	  ("&amp;"))
   (entity  "\C-c>"   "&gt;"	  	"Greater Than"       ("&gt;"))
   (entity  "\C-c<"   "&lt;"		"Less Than"	  ("&lt;"))
   ;; letters with accents common in italian
   (entity  nil   "&agrave;"        "a` (&&agrave;)"          ("&agrave;"))
   (entity  nil   "&egrave;"        "e` (&&egrave;)"          ("&egrave;"))
   (entity  nil   "&eacute;"        "e' (&&eacute;)"          ("&eacute;"))
   (entity  nil   "&ograve;"        "o` (&&ograve;)"          ("&ograve;"))
   (entity  nil   "&igrave;"        "i` (&&igrave;)"          ("&igrave;"))
   (entity  nil   "&ugrave;"        "u` (&&ugrave;)"          ("&ugrave;"))

   ;; logical styles
   (logical nil "<q")
   (logical nil "<del")
   (logical nil "<ins")
   (logical nil "<abbr")
   (logical nil "<acronym")
   (logical nil "<fieldset")
   (logical "b"       "<blockquote>"	"Blockquote"
	    ("<blockquote>" (r "Quote: ") "</blockquote>"))
   (logical "c"       "<code>"		"Code"
	    ("<code>" (r "Code: ") "</code>"))
   (logical "x"       "<samp>"		"Sample"
	    ("<samp>" (r "Sample code") "</samp>"))
   (logical "r"       "<cite>"		"Citation"
	    ("<cite>" (r "Citation: ") "</cite>"))
   (logical "k"       "<kbd>"		"Keyboard Input"
	    ("<kbd>" (r "Keyboard: ") "</kbd>"))
   (logical "v"       "<var>"		"Variable"
	    ("<var>" (r "Variable: ") "</var>"))
   (logical "d"       "<dfn>"		"Definition"
	    ("<dfn>" (r "Definition: ") "</dfn>"))
   (logical "a"	      "<address>"	"Address"
	    ("<address>" r "</address>"))
   (logical "e"       "<em>"		"Emphasized"
	    ("<em>" (r "Text: ") "</em>"))
   (logical "s"       "<strong>"	"Strong"
	    ("<strong>" (r "Text: ") "</strong>"))
   (logical "p"       "<pre>"		"Preformatted"
	    ("<pre>" (r "Text: ") "</pre>"))

   ;;physical styles
   (physical    nil       "<hr")
   (physical    nil       "<sup")
   (physical    nil       "<sub")
   (physical    nil       "<font")
   (physical    nil       "<br")
   (physical    nil       "<big>")
   (physical    nil       "<small>")
   (physical    "s"       "<strike>"	"Strikethru"
	    ("<strike>" (r "Text: ") "</strike>"))
   (physical    "u"       "<u>"		"Underline"
	    ("<u>" (r "Text: ") "</u>"))
   (physical    "i"       "<i>"		"Italic"
	    ("<i>" (r "Text: ") "</i>"))
   (physical    "b"	      "<b>"    		"Bold"
	    ("<b>" (r "Text: ") "</b>"))
   (physical    "f"       "<tt>"		"Fixed"
	    ("<tt>" (r "Text: ") "</tt>"))
   (physical    "c"       "<center>"        "Center"
	    ("<center>" (r "Text: ") "</center>"))

;; html4.0 stuff, omitted

;    (physical    "5" "<span style=" "Spanning style"
; 	     ("<span style=\"" (p "style: ") "\">" 'r "</span>"))
;    (physical    "l" "<span class=" "Spanning class"
; 	     ("<span class=\"" (p "class: ") "\">" 'r "</span>"))


   (scripting nil "<script")
   (scripting nil "<noscript")
   (scripting nil "<object")
   (scripting nil "<applet")


   ;;headers
   (structure  nil "<iframe")
   (structure  nil "<p")
   (structure  nil "<div")
   (structure  nil "<span")
   (structure  "6"       "<h6>"		"Header 6"
	    ("<h6>" (r "Header: ") "</h6>"))
   (structure  "5"       "<h5>"		"Header 5"
	    ("<h5>" (r "Header: ") "</h5>"))
   (structure  "4"       "<h4>"		"Header 4"
	    ("<h4>" (r "Header: ") "</h4>"))
   (structure  "3"       "<h3>"		"Header 3"
	    ("<h3>" (r "Header: ") "</h3>"))
   (structure  "2"       "<h2>"		"Header 2"
	    ("<h2>" (r "Header: ") "</h2>"))
   (structure  "1"	      "<h1>"     	"Header 1"
	    ("<h1>" (r "Header: ") "</h1>"))

   ;; forms
   (form    nil "<isindex")
   (form    nil "<label")
   (form    nil "<button")
   (form    "o"       "<option>"        "Option"
	    (& "<option>" > ))
   (form    "v"       "<option value"   "Option with Value"
	    (& "<option value=\"" (r "Value: ") "\">" >))
   (form    "s"       "<select"		"Selections"
	    ("<select"
	     (html-helper-insert-or-wipe "name") ">\n<option>" > "\n</select>")"<select")
   (form    "z"	      "<input"		"Reset Form"
	    ("<input type=\"RESET\""
	     (html-helper-insert-or-wipe "value") ">"))
   (form    "m"	      "<input"		"Submit Form"
	    ("<input type=\"SUBMIT\""
	     (html-helper-insert-or-wipe "value") ">"))
   (form    "b"       "<input"          "Button"
	    ("<input type=\"BUTTON\""
	     (html-helper-insert-or-wipe "value")
	     (html-helper-insert-or-wipe "name") ">"))
   (form    "i"	      "<input"		"Image Field"
	    ("<input type=\"IMAGE\""
	     (html-helper-insert-or-wipe "Name")
	     (html-helper-insert-or-wipe "src") ">"))
   (form    "h"       "<input"          "Hidden Field"
	    ("<input type=\"HIDDEN\""
	     (html-helper-insert-or-wipe "name")
	     (html-helper-insert-or-wipe "value") ">"))
   (form    "p"	      "<textarea"	"Text Area"
	    ("<textarea"
	     (html-helper-insert-or-wipe "name")
	     (html-helper-insert-or-wipe "rows")
	     (html-helper-insert-or-wipe "cols") ">" r "</textarea>"))
   (form    "c"	      "<input"		"Checkbox"
	    ("<input type=\"CHECKBOX\""
	     (html-helper-insert-or-wipe "name") ">"))
   (form    "r"	      "<input"		"Radiobutton"
	    ("<input type=\"RADIO\""
	     (html-helper-insert-or-wipe "Name") ">"))
   (form    "t"	      "<input"		"Text Field"
	    ("<input type=\"TEXT\""
	     (html-helper-insert-or-wipe "name")
	     (html-helper-insert-or-wipe "size") ">"))
   (form    "f"	      "<form"           "Form"
	    ("<form"
	     (html-helper-insert-or-wipe "action")
	     (html-helper-insert-or-wipe "method") ">\n</form>\n"))

   ;;lists
   (list    "t"       "<dt>"            "Definition Item"
	    (& "<dt>" > (r "Term: ") "\n<dd>" >
	       (r "Definition: ")))
   (list    "l"       "<li>"            "List Item"
	    (& "<li>" > (r "Item: ") > "</li>"))
   (list    "r"	      "<dir>"		"DirectoryList"
	    (& "<dir>" > "\n<li>" > (r "Item: ") "\n</li>" > "\n</dir>" >))
   (list    "m"	      "<menu>"		"Menu List"
	    (& "<menu>" > "\n<li>" > (r "Item: ") "\n</li>" > "\n</menu>" >))
   (list    "o"	      "<ol>"		"Ordered List"
	    (& "<ol>" > "\n<li>" > (r "Item: ") "\n</li>" > "\n</ol>" >))
   (list    "d"	      "<dl>"		"Definition List"
	    (& "<dl>" > "\n<dt>" >
	       (p "Term: ") "\n<dd>" >
	       (r "Definition: ") "\n</dl>" >))
   (list    "u"	      "<ul>"		"Unordered List"
	    (& "<ul>" > "\n<li>" > (r "Item: ") "\n</li>\n</ul>" >))

   ;;anchors
   (link  "n"	      "<a name="	"Link Target"
	    ("<a " (html-helper-insert-or-wipe "name") ">"
	     (r "Anchor text: ") "</a>"))
   (link  "l"	      "<a href="        "Hyperlink"
	    ("<a href=\"" (p "href: ") "\" >"
	     (r "Anchor text: ") "</a>"))

   ;;graphics
;    (image   "a"       nil               "Aligned Image"
; 	    ("<img align=\""
; 	     (r "Alignment: ") "\" src=\"" (r "Image URL: ") "\">"))
;    (image   "i"       "<img src="	"Image"
; 	    ("<img src=\""
; 	     (r "Image URL: ") "\">"))
;    (image   "e"       "<img align="     "Aligned Image With Alt. Text"
; 	    ("<img align=\""
; 	     (r "Alignment: ") "\" src=\""
; 	     (r "Image URL: ") "\" alt=\""
; 	     (r "Text URL: ") "\">"))
;    (image   "t"       "<img alt="	"Image With Alternate Text"
; 	    ("<img alt=\""
; 	     (r "Text URL: ") "\" src=\""
; 	     (r "Image URL: ") "\">"))
;; New, (almost) all including, single menu item entry
;; src has to be there!
   (image   nil "<img")
   (image   nil "<map")
   (image	"a"	nil	"Image"
		("<img src=\""
		 (p "src" ) "\" "
		 (html-helper-insert-or-wipe  "alt" )
		 (html-helper-insert-or-wipe  "height" )
		 (html-helper-insert-or-wipe  "width" )
		 (html-helper-insert-or-wipe  "align" )
		 ">"))
   ;; table
   (table   "t"       "<table>"         "Table"
	    ("<table"
	     (html-helper-insert-or-wipe  "border" )
	     (html-helper-insert-or-wipe "width" )">\n</table>"))
   (table   "r"       "<tr>"         "Table Row"
	    ("<TR>\n</TR>"))
   (table   "h"       "<th>"         "Table Header"
	    ("<TH"
	     (html-helper-insert-or-wipe "rowspan" )
	     (html-helper-insert-or-wipe "colspan")">\n</TH>"))
   (table   "d"       "<td>"         "Table Data"
	    ("<TD"
	     (html-helper-insert-or-wipe "align" )
	     (html-helper-insert-or-wipe "colspan")"></TD>"))
   (table   "p"	"<caption>"	"html table caption"
	    ("<caption>" (r . "Table: ") "</caption>"))
   ;;text elements
   (textel  "\C-c="    nil		"Horizontal Line"
	    (& "<hr>\n"))
   (textel  "\C-c\C-m" nil		"Line Break"
	    ("<br>\n"))
   (textel  "\e\C-m"  nil		"Paragraph"
	    ("<p>"
	     (r "Text: ") "</p>"))

   ;;head elements
   ;;(document    "isindex"    "<isindex>")
   (document    "base"    "<base>")
   (document    "style"    "<style>")
   (document    "link"    "<link>")

   (document    "H"       "<head>"          "Head"
	    ("<head>\n" "</head>\n"))
   (document    "B"       "<body>"          "Body"
	    ("<body>\n" "</body>\n"))
;;    (document    "i"	      "<isindex>"	"Isindex"
;; 	    ("<isindex>\n"))
   (document    "n"	      "<nextid>"	"Nextid"
	    ("<nextid>\n"))
   (document    "h"       "<meta http-equiv=" "HTTP Equivalent"
	    ("<meta"
	     (html-helper-insert-or-wipe "http-equiv") " content=\""
	     (r "Content: ") "\">\n"))
   (document    "m"       "<meta name="     "Meta Name"
	    ("<meta"
	     (html-helper-insert-or-wipe "name") " content=\""
	     (r "Content: ") "\">\n"))
   (document    "l"	      "<link"		"Link"
	    ("<link href=\"" p "\">"))
   (document    "b"       "<base"		"Base"
	    ("<base href=\"" r "\">"))
   (document    "t"	      "<title>"		"Title"
	    ("<title>" (r "Document title: ") "</title>"))
   ;; scripting elements
   (script  "j"    "<SCRIPT>"       "JavaScript"
	    ("<SCRIPT TYPE=\"text/javascript\">\n"
	     (r "Script") "</SCRIPT>"))
   (script  "v"    "<SCRIPT>"       "VBScript"
	    ("<SCRIPT TYPE=\"text/vbscript\">\n"
	     (r "Script") "</SCRIPT>"))
   (script  "%"    "<%="            "ASP output"
	    ("<%="(p " Variabile: ")"%>"))
   (script  "a"    "<%xx%>"         "JSP/ASP code"
	    ("<%\n"(r "Code: " )"\n%>"))
   (script  "<"    "<%xx%>"         "JSP/ASP break"
	    ("%>\n"(r "Code: " )"\n<%"))
   (script  "="    "<?="            "PHP output"
	    ("<?="(p " Variabile: ")"?>"))
   (script  "p"    "<?xx?>"         "PHP code"
	    ("<? PHP\n"(r "Code: " )"\n?>"))
   (script  "?"    "<?xx?>"         "PHP break"
	    ("?>\n"(r " Code: " )"\n<? PHP"))
   ))


(provide 'nxhpcmpl)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; nxhpcmpl.el ends here
