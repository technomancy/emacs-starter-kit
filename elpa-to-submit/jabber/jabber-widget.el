;; jabber-widget.el - display various kinds of forms

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

(require 'widget)
(require 'wid-edit)
(require 'jabber-util)
(require 'jabber-disco)

(defvar jabber-widget-alist nil
  "Alist of widgets currently used")

(defvar jabber-form-type nil
  "Type of form.  One of:
'x-data, jabber:x:data
'register, as used in jabber:iq:register and jabber:iq:search")

(defvar jabber-submit-to nil
  "JID of the entity to which form data is to be sent")

(add-to-list 'jabber-advertised-features "jabber:x:data")

(defun jabber-init-widget-buffer (submit-to)
  "Setup buffer-local variables for widgets."
  (make-local-variable 'jabber-widget-alist)
  (make-local-variable 'jabber-submit-to)
  (setq jabber-widget-alist nil)
  (setq jabber-submit-to submit-to)
  (setq buffer-read-only nil)
  ;; XXX: This is because data from other queries would otherwise be
  ;; appended to this buffer, which would fail since widget buffers
  ;; are read-only... or something like that.  Maybe there's a
  ;; better way.
  (rename-uniquely))

(defun jabber-render-register-form (query)
  "Display widgets from <query/> element in jabber:iq:{register,search} namespace."
  (make-local-variable 'jabber-widget-alist)
  (setq jabber-widget-alist nil)
  (make-local-variable 'jabber-form-type)
  (setq jabber-form-type 'register)

  (if (jabber-xml-get-children query 'instructions)
      (widget-insert "Instructions: " (car (jabber-xml-node-children (car (jabber-xml-get-children query 'instructions)))) "\n"))
  (if (jabber-xml-get-children query 'registered)
      (widget-insert "You are already registered.  You can change your details here.\n"))
  (widget-insert "\n")

  (let ((possible-fields
	 ;; taken from JEP-0077
	 '((username . "Username")
	   (nick . "Nickname")
	   (password . "Password")
	   (name . "Full name")
	   (first . "First name")
	   (last . "Last name")
	   (email . "E-mail")
	   (address . "Address")
	   (city . "City")
	   (state . "State")
	   (zip . "Zip")
	   (phone . "Telephone")
	   (url . "Web page")
	   (date . "Birth date"))))
    (dolist (field (jabber-xml-node-children query))
      (let ((entry (assq (jabber-xml-node-name field) possible-fields)))
	(when entry
	  (widget-insert (cdr entry) "\t")
	  ;; Special case: when registering a new account, the default
	  ;; username is the one specified in jabber-username.  Things
	  ;; will break if the user changes that name, though...
	  (let ((default-value (if (and jabber-register-p
					(eq (jabber-xml-node-name field) 'username))
				   jabber-username
				 "")))
	    (setq jabber-widget-alist 
		  (cons
		   (cons (car entry)
			 (widget-create 'editable-field
					:secret  (if (eq (car entry) 'password)
						     ?* nil)
					(or (car (jabber-xml-node-children
						  field)) default-value)))
		   jabber-widget-alist)))
	  (widget-insert "\n"))))))

(defun jabber-parse-register-form ()
  "Return children of a <query/> tag containing information entered in the widgets of the current buffer."
  (mapcar
   (lambda (widget-cons)
     (list (car widget-cons)
	   nil
	   (widget-value (cdr widget-cons))))
   jabber-widget-alist))

(defun jabber-render-xdata-form (x &optional defaults)
  "Display widgets from <x/> element in jabber:x:data namespace.
DEFAULTS is an alist associating variable names with default values.
DEFAULTS takes precedence over values specified in the form."
  (make-local-variable 'jabber-widget-alist)
  (setq jabber-widget-alist nil)
  (make-local-variable 'jabber-form-type)
  (setq jabber-form-type 'xdata)

  (let ((title (car (jabber-xml-node-children (car (jabber-xml-get-children x 'title))))))
    (if (stringp title)
	(widget-insert (jabber-propertize title 'face 'jabber-title-medium) "\n\n")))
  (let ((instructions (car (jabber-xml-node-children (car (jabber-xml-get-children x 'instructions))))))
    (if (stringp instructions)
	(widget-insert "Instructions: " instructions "\n\n")))

  (dolist (field (jabber-xml-get-children x 'field))
    (let* ((var (jabber-xml-get-attribute field 'var))
	   (label (jabber-xml-get-attribute field 'label))
	   (type (jabber-xml-get-attribute field 'type))
	   (required (jabber-xml-get-children field 'required))
	   (values (jabber-xml-get-children field 'value))
	   (options (jabber-xml-get-children field 'option))
	   (desc (car (jabber-xml-get-children field 'desc)))
	   (default-value (assoc var defaults)))
      ;; "required" not implemented yet

      (cond
       ((string= type "fixed")
	(widget-insert (car (jabber-xml-node-children (car values)))))

       ((string= type "text-multi")
	(if (or label var)
	    (widget-insert (or label var) ":\n"))
	(push (cons (cons var type)
		    (widget-create 'text (or (cdr default-value)
					      (mapconcat #'(lambda (val)
							     (car (jabber-xml-node-children val)))
							 values "\n")
					     "")))
	      jabber-widget-alist))

       ((string= type "list-single")
	(if (or label var)
	    (widget-insert (or label var) ":\n"))
	(push (cons (cons var type)
		    (apply 'widget-create
			   'radio-button-choice 
			   :value (or (cdr default-value)
				      (car (xml-node-children (car values))))
			   (mapcar (lambda (option)
				     `(item :tag ,(jabber-xml-get-attribute option 'label)
					    :value ,(car (jabber-xml-node-children (car (jabber-xml-get-children option 'value))))))
				   options)))
	      jabber-widget-alist))
				    
       ((string= type "boolean")
	(push (cons (cons var type)
		    (widget-create 'checkbox 
				   :tag (or label var)
				   :value (if default-value
					      (cdr default-value)
					    (not (null
						  (member (car (xml-node-children (car values))) '("1" "true")))))))
	      jabber-widget-alist)
	(if (or label var)
	    (widget-insert " " (or label var) "\n")))

       (t	; in particular including text-single and text-private
	(if (or label var)
	    (widget-insert (or label var) ": "))
	(setq jabber-widget-alist
	      (cons
	       (cons (cons var type)
		     (widget-create 'editable-field
				    :secret (if (string= type "text-private") ?* nil)
				    (or (cdr default-value)
					(car (jabber-xml-node-children (car values)))
					"")))
	       jabber-widget-alist))))
      (when (and desc (car (jabber-xml-node-children desc)))
	(widget-insert "\n" (car (jabber-xml-node-children desc))))
      (widget-insert "\n"))))

(defun jabber-parse-xdata-form ()
  "Return an <x/> tag containing information entered in the widgets of the current buffer."
  `(x ((xmlns . "jabber:x:data")
       (type . "submit"))
      ,@(mapcar
	 (lambda (widget-cons)
	   (let ((values (jabber-xdata-value-convert (widget-value (cdr widget-cons)) (cdar widget-cons))))
	     ;; empty fields are not included
	     (when values
	       `(field ((var . ,(caar widget-cons)))
		       ,@(mapcar
			  (lambda (value)
			    (list 'value nil value))
			  values)))))
	 jabber-widget-alist)))

(defun jabber-xdata-value-convert (value type)
  "Convert VALUE from form used by widget library to form required by JEP-0004.
Return a list of strings, each of which to be included as cdata in a <value/> tag."
  (cond
   ((string= type "boolean")
    (if value (list "1") (list "0")))
   ((string= type "text-multi")
    (split-string value "[\n\r]"))
   (t					; in particular including text-single, text-private and list-single
    (if (zerop (length value))
	nil
      (list value)))))

(defun jabber-render-xdata-search-results (xdata)
  "Render search results in x:data form."

  (let ((title (car (jabber-xml-get-children xdata 'title))))
    (when title
      (insert (jabber-propertize (car (jabber-xml-node-children title)) 'face 'jabber-title-medium) "\n")))
  
  (if (jabber-xml-get-children xdata 'reported)
      (jabber-render-xdata-search-results-multi xdata)
    (jabber-render-xdata-search-results-single xdata)))

(defun jabber-render-xdata-search-results-multi (xdata)
  "Render multi-record search results."
  (let (fields
	(jid-fields 0))
    (let ((reported (car (jabber-xml-get-children xdata 'reported)))
	  (column 0))
      (dolist (field (jabber-xml-get-children reported 'field))
	(let (width)
	  ;; Clever algorithm for estimating width based on field type goes here.
	  (setq width 20)

	  (setq fields
		(append
		 fields
		 (list (cons (jabber-xml-get-attribute field 'var)
			     (list 'label (jabber-xml-get-attribute field 'label)
				   'type (jabber-xml-get-attribute field 'type)
				   'column column)))))
	  (setq column (+ column width))
	  (if (string= (jabber-xml-get-attribute field 'type) "jid-single")
	      (setq jid-fields (1+ jid-fields))))))

    (dolist (field-cons fields)
      (indent-to (plist-get (cdr field-cons) 'column) 1)
      (insert (jabber-propertize (plist-get (cdr field-cons) 'label) 'face 'bold)))
    (insert "\n\n")

    ;; Now, the items
    (dolist (item (jabber-xml-get-children xdata 'item))
		  
      (let ((start-of-line (point))
	    jid)

	;; The following code assumes that the order of the <field/>s in each
	;; <item/> is the same as in the <reported/> tag.
	(dolist (field (jabber-xml-get-children item 'field))
	  (let ((field-plist (cdr (assoc (jabber-xml-get-attribute field 'var) fields)))
		(value (car (jabber-xml-node-children (car (jabber-xml-get-children field 'value))))))

	    (indent-to (plist-get field-plist 'column) 1)

	    ;; Absent values are sometimes "", sometimes nil.  insert
	    ;; doesn't like nil.
	    (when value
	      ;; If there is only one JID field, let the whole row
	      ;; have the jabber-jid property.  If there are many JID
	      ;; fields, the string belonging to each field has that
	      ;; property.
	      (if (string= (plist-get field-plist 'type) "jid-single")
		  (if (not (eq jid-fields 1))
		      (insert (jabber-propertize value 'jabber-jid value))
		    (setq jid value)
		    (insert value))
		(insert value)))))
	      
	(if jid
	    (put-text-property start-of-line (point)
			       'jabber-jid jid))
	(insert "\n")))))

(defun jabber-render-xdata-search-results-single (xdata)
  "Render single-record search results."
  (dolist (field (jabber-xml-get-children xdata 'field))
    (let ((label (jabber-xml-get-attribute field 'label))
	  (type (jabber-xml-get-attribute field 'type))
	  (values (mapcar #'(lambda (val)
			      (car (jabber-xml-node-children val)))
			  (jabber-xml-get-children field 'value))))
      ;; XXX: consider type
      (insert (jabber-propertize (concat label ": ") 'face 'bold))
      (indent-to 30)
      (insert (apply #'concat values) "\n"))))

(defun jabber-xdata-formtype (x)
  "Return the form type of the xdata form in X, by JEP-0068.
Return nil if no form type is specified."
  (catch 'found-formtype
    (dolist (field (jabber-xml-get-children x 'field))
      (when (and (string= (jabber-xml-get-attribute field 'var) "FORM_TYPE")
		 (string= (jabber-xml-get-attribute field 'type) "hidden"))
	(throw 'found-formtype (car (jabber-xml-node-children
				     (car (jabber-xml-get-children field 'value)))))))))

(provide 'jabber-widget)

;;; arch-tag: da3312f3-1970-41d5-a974-14b8d76156b8
