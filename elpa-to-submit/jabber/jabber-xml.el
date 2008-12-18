;; jabber-xml.el - XML functions

;; Copyright (C) 2002, 2003, 2004 - tom berger - object@intelectronica.net
;; Copyright (C) 2003, 2004 - Magnus Henoch - mange@freemail.hu

;; This file is a part of jabber.el.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

(require 'xml)
(require 'jabber-util)

(defun jabber-escape-xml (str)
  "escape strings for xml"
  (if (stringp str)
      (let ((newstr (concat str)))
	;; Form feeds might appear in code you copy, etc.  Nevertheless,
	;; it's invalid XML.
	(setq newstr (jabber-replace-in-string newstr "\f" "\n"))
	;; Other control characters are also illegal, except for
	;; tab, CR, and LF.
	(setq newstr (jabber-replace-in-string newstr "[\000-\010\013\014\016-\037]" " "))
	(setq newstr (jabber-replace-in-string newstr "&" "&amp;"))
	(setq newstr (jabber-replace-in-string newstr "<" "&lt;"))
	(setq newstr (jabber-replace-in-string newstr ">" "&gt;"))
	(setq newstr (jabber-replace-in-string newstr "'" "&apos;"))
	(setq newstr (jabber-replace-in-string newstr "\"" "&quot;"))
	newstr)
    str))

(defun jabber-unescape-xml (str)
  "unescape xml strings"
  ;; Eventually this can be done with `xml-substitute-special', but the
  ;; version in xml.el of GNU Emacs 21.3 is buggy.
  (if (stringp str)
      (let ((newstr str))
	(setq newstr (jabber-replace-in-string newstr "&quot;" "\""))
	(setq newstr (jabber-replace-in-string newstr "&apos;" "'"))
	(setq newstr (jabber-replace-in-string newstr "&gt;" ">"))
	(setq newstr (jabber-replace-in-string newstr "&lt;" "<"))
	(setq newstr (jabber-replace-in-string newstr "&amp;" "&"))
	newstr)
    str))

(defun jabber-sexp2xml (sexp)
  "converts an SEXP in the format (tagname ((attribute-name . attribute-value)...) children...) and converts it to well-formatted xml."
  (cond
   ((stringp sexp)
    sexp)
   ((listp (car sexp))
    (let ((xml ""))
      (dolist (tag sexp)
	(setq xml (concat xml (jabber-sexp2xml tag))))
      xml))
   ;; work around bug in old versions of xml.el, where ("") can appear
   ;; as children of a node
   ((and (consp sexp)
	 (stringp (car sexp))
	 (zerop (length (car sexp))))
    "")
   (t
    (let ((xml ""))
      (setq xml (concat "<" 
			(symbol-name (car sexp))))
      (dolist (attr (cadr sexp))
	(if (consp attr)
	    (setq xml (concat xml
			      (format " %s='%s'"
				      (symbol-name (car attr))
				      (cdr attr))))))
      (if (cddr sexp)
	  (progn
	    (setq xml (concat xml ">"))
	    (dolist (child (cddr sexp))
	      (setq xml (concat xml
				(jabber-sexp2xml child))))
	    (setq xml (concat xml
			      "</"
			      (symbol-name (car sexp))
			      ">")))
	(setq xml (concat xml
			  "/>")))
      xml))))

(defun jabber-xml-skip-tag-forward ()
  "Skip to end of tag or matching closing tag if present.
Return t iff after a closing tag, otherwise throws an 'unfinished
tag with value nil.

The version of `sgml-skip-tag-forward' in Emacs 21 isn't good
enough for us."
  (skip-chars-forward "^<")
  (if (not (looking-at "<\\([^ \t\n/>]+\\)\\([ \t\n]+[^=]+='[^']*'\\|[ \t\n]+[^=]+=\"[^\"]*\"\\)*"))
      (throw 'unfinished nil)
    (let ((node-name (match-string 1)))
      (goto-char (match-end 0))
      (cond
       ((looking-at "/>")
	(goto-char (match-end 0))
	t)
       ((looking-at ">")
	(forward-char 1)
	(loop 
	 do (skip-chars-forward "^<")
	 until (looking-at (regexp-quote (concat "</" node-name ">")))
	 do (jabber-xml-skip-tag-forward))
	(goto-char (match-end 0))
	t)
       (t
	(throw 'unfinished nil))))))

(defsubst jabber-xml-node-name (node)
  "Return the tag associated with NODE.
The tag is a lower-case symbol."
  (if (listp node) (car node)))

(defsubst jabber-xml-node-attributes (node)
  "Return the list of attributes of NODE.
The list can be nil."
  (if (listp node) (nth 1 node)))

(defsubst jabber-xml-node-children (node)
  "Return the list of children of NODE.
This is a list of nodes, and it can be nil."
  (let ((children (cddr node)))
    ;; Work around a bug in early versions of xml.el
    (if (equal children '(("")))
	nil
      children)))

(defun jabber-xml-get-children (node child-name)
  "Return the children of NODE whose tag is CHILD-NAME.
CHILD-NAME should be a lower case symbol."
  (let ((match ()))
    (dolist (child (jabber-xml-node-children node))
      (if child
	  (if (equal (jabber-xml-node-name child) child-name)
	      (push child match))))
    (nreverse match)))

;; `xml-get-attribute' returns "" if the attribute is not found, which
;; is not very useful.  Therefore, we use `xml-get-attribute-or-nil'
;; if present, or emulate its behavior.
(if (fboundp 'xml-get-attribute-or-nil)
    (defalias 'jabber-xml-get-attribute 'xml-get-attribute-or-nil)
  (defsubst jabber-xml-get-attribute (node attribute)
    "Get from NODE the value of ATTRIBUTE.
Return nil if the attribute was not found."
    (let ((result (xml-get-attribute node attribute)))
      (and (> (length result) 0) result))))

(defun jabber-xml-path (xml-data path)
  "Find sub-node of XML-DATA according to PATH.
PATH is a vaguely XPath-inspired list.  Each element can be:

a symbol     go to first child node with this node name
cons cell    car is string containing namespace URI,
             cdr is string containing node name.  Find
             first matching child node.
any string   character data of this node"
  (let ((node xml-data))
    (while (and path node)
      (let ((step (car path)))
	(cond
	 ((symbolp step)
	  (setq node (car (jabber-xml-get-children node step))))
	 ((consp step)
	  ;; This will be easier with namespace-aware use
	  ;; of xml.el.  It will also be more correct.
	  ;; Now, it only matches explicit namespace declarations.
	  (setq node
		(dolist (x (jabber-xml-get-children node (intern (cdr step))))
		  (when (string= (jabber-xml-get-attribute x 'xmlns)
				 (car step))
		    (return x)))))
	 ((stringp step)
	  (setq node (car (jabber-xml-node-children node)))
	  (unless (stringp node)
	    (setq node nil)))
	 (t
	  (error "Unknown path step: %s" step))))
      (setq path (cdr path)))
    node))

(defmacro jabber-xml-let-attributes (attributes xml-data &rest body)
  "Bind variables to the same-name attribute values in XML-DATA."
  `(let ,(mapcar #'(lambda (attr)
		     (list attr `(jabber-xml-get-attribute ,xml-data ',attr)))
		 attributes)
     ,@body))

(provide 'jabber-xml)

;;; arch-tag: ca206e65-7026-4ee8-9af2-ff6a9c5af98a
