;; jabber-search.el - searching by JEP-0055, with x:data support

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

(require 'jabber-register)

(add-to-list 'jabber-jid-service-menu
	     (cons "Search directory" 'jabber-get-search))
(defun jabber-get-search (to)
  "Send IQ get request in namespace \"jabber:iq:search\"."
  (interactive (list (jabber-read-jid-completing "Search what database: ")))
  (jabber-send-iq to
		  "get"
		  '(query ((xmlns . "jabber:iq:search")))
		  #'jabber-process-data #'jabber-process-register-or-search
		  #'jabber-report-success "Search field retrieval"))

;; jabber-process-register-or-search logically comes here, rendering
;; the search form, but since register and search are so similar,
;; having two functions would be serious code duplication.  See
;; jabber-register.el.

;; jabber-submit-search is called when the "submit" button of the
;; search form is activated.
(defun jabber-submit-search (&rest ignore)
  "Submit search.  See `jabber-process-register-or-search'."
  
  (let ((text (concat "Search at " jabber-submit-to)))
    (jabber-send-iq jabber-submit-to
		    "set"

		    (cond
		     ((eq jabber-form-type 'register)
		      `(query ((xmlns . "jabber:iq:search"))
			      ,@(jabber-parse-register-form)))
		     ((eq jabber-form-type 'xdata)
		      `(query ((xmlns . "jabber:iq:search"))
			      ,(jabber-parse-xdata-form)))
		     (t
		      (error "Unknown form type: %s" jabber-form-type)))
		    #'jabber-process-data #'jabber-process-search-result
		    #'jabber-report-success text))

  (message "Search sent"))

(defun jabber-process-search-result (xml-data)
  "Receive and display search results."

  ;; This function assumes that all search results come in one packet,
  ;; which is not necessarily the case.
  (let ((query (jabber-iq-query xml-data))
	(have-xdata nil)
	xdata fields (jid-fields 0))

    ;; First, check for results in jabber:x:data form.
    (dolist (x (jabber-xml-get-children query 'x))
      (when (string= (jabber-xml-get-attribute x 'xmlns) "jabber:x:data")
	(setq have-xdata t)
	(setq xdata x)))

    (if have-xdata
	(jabber-render-xdata-search-results xdata)

      (insert (jabber-propertize "Search results" 'face 'jabber-title-medium) "\n")
	
      (setq fields '((first . (label "First name" column 0))
		     (last . (label "Last name" column 15))
		     (nick . (label "Nickname" column 30))
		     (jid . (label "JID" column 45))
		     (email . (label "E-mail" column 65))))
      (setq jid-fields 1)

      (dolist (field-cons fields)
	(indent-to (plist-get (cdr field-cons) 'column) 1)
	(insert (jabber-propertize (plist-get (cdr field-cons) 'label) 'face 'bold)))
      (insert "\n\n")

      ;; Now, the items
      (dolist (item (jabber-xml-get-children query 'item))
	(let ((start-of-line (point))
	      jid)

	    (dolist (field-cons fields)
	      (let ((field-plist (cdr field-cons))
		    (value (if (eq (car field-cons) 'jid) 
			       (setq jid (jabber-xml-get-attribute item 'jid))
			     (car (jabber-xml-node-children (car (jabber-xml-get-children item (car field-cons))))))))
		(indent-to (plist-get field-plist 'column) 1)
		(if value (insert value))))
	      
	    (if jid
		(put-text-property start-of-line (point)
				   'jabber-jid jid))
	    (insert "\n"))))))

(provide 'jabber-search)

;;; arch-tag: c39e9241-ab6f-4ac5-b1ba-7908bbae009c
