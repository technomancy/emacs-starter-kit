;; jabber-disco.el - service discovery functions

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


;;; All the client part should be seriously rewritten, or at least
;;; reconsidered.  I'm imagining a separation between backend and
;;; frontend, so that various functions can perform disco queries for
;;; their own purposes, and maybe some caching with that.

(require 'jabber-iq)
(require 'jabber-xml)
(require 'jabber-menu)

;; Advertise your features here.  Add the namespace to this list.
(defvar jabber-advertised-features
  (list "http://jabber.org/protocol/disco#info")
  "Features advertised on service discovery requests")

(defvar jabber-disco-items-nodes
  (list
   (list "" nil nil))
  "Alist of node names and information about returning disco item data.
Key is node name as a string, or \"\" for no node specified.  Value is
a list of two items.

First item is data to return.  If it is a function, that function is
called and its return value is used; if it is a list, that list is
used.  The list should be the XML data to be returned inside the
<query/> element, like this:

((item ((name . \"Name of first item\")
	(jid . \"first.item\")
	(node . \"node\"))))

Second item is access control function.  That function is passed the
JID, and returns non-nil if access is granted.  If the second item is
nil, access is always granted.")

(defvar jabber-disco-info-nodes
  (list
   (list "" #'jabber-disco-return-client-info nil))
  "Alist of node names and information returning disco info data.
Key is node name as a string, or \"\" for no node specified.  Value is
a list of two items.

First item is data to return.  If it is a function, that function is
called and its return value is used; if it is a list, that list is
used.  The list should be the XML data to be returned inside the
<query/> element, like this:

((identity ((category . \"client\")
	    (type . \"pc\")
	    (name . \"Jabber client\")))
 (feature ((var . \"some-feature\"))))

Second item is access control function.  That function is passed the
JID, and returns non-nil if access is granted.  If the second item is
nil, access is always granted.")

(defun jabber-process-disco-info (xml-data)
  "Handle results from info disco requests."

  (let ((beginning (point)))
    (dolist (x (jabber-xml-node-children (jabber-iq-query xml-data)))
      (cond
       ((eq (jabber-xml-node-name x) 'identity)
	(let ((name (jabber-xml-get-attribute x 'name))
	      (category (jabber-xml-get-attribute x 'category))
	      (type (jabber-xml-get-attribute x 'type)))
	  (insert (jabber-propertize (if name
				  (jabber-unescape-xml name)
				  "Unnamed")
			      'face 'jabber-title-medium)
		  "\n\nCategory:\t" category "\n")
	  (if type
	      (insert "Type:\t\t" type "\n"))
	  (insert "\n")))
       ((eq (jabber-xml-node-name x) 'feature)
	(let ((var (jabber-xml-get-attribute x 'var)))
	  (insert "Feature:\t" var "\n")))))
    (put-text-property beginning (point) 'jabber-jid (jabber-xml-get-attribute xml-data 'from))))

(defun jabber-process-disco-items (xml-data)
  "Handle results from items disco requests."

  (let ((items (jabber-xml-get-children (jabber-iq-query xml-data) 'item)))
    (if items
	(dolist (item items)
	  (let ((jid (jabber-xml-get-attribute item 'jid))
		(name (jabber-xml-get-attribute item 'name))
		(node (jabber-xml-get-attribute item 'node)))
	    (insert 
	     (jabber-propertize 
	      (concat
	       (jabber-propertize
		(concat jid "\n" (if node (format "Node: %s\n" node)))
		'face 'jabber-title-medium)
	       (jabber-unescape-xml name) "\n\n")
	      'jabber-jid jid
	      'jabber-node node))))
      (insert "No items found.\n"))))

(add-to-list 'jabber-iq-get-xmlns-alist
	     (cons "http://jabber.org/protocol/disco#info" 'jabber-return-disco-info))
(add-to-list 'jabber-iq-get-xmlns-alist
	     (cons "http://jabber.org/protocol/disco#items" 'jabber-return-disco-info))
(defun jabber-return-disco-info (xml-data)
  "Respond to a service discovery request.
See JEP-0030."
  (let* ((to (jabber-xml-get-attribute xml-data 'from))
	 (id (jabber-xml-get-attribute xml-data 'id))
	 (xmlns (jabber-iq-xmlns xml-data))
	 (which-alist (eval (cdr (assoc xmlns
					(list
					 (cons "http://jabber.org/protocol/disco#info" 'jabber-disco-info-nodes)
					 (cons "http://jabber.org/protocol/disco#items" 'jabber-disco-items-nodes))))))
	 (node (or
		(jabber-xml-get-attribute (jabber-iq-query xml-data) 'node)
		""))
	 (return-list (cdr (assoc node which-alist)))
	 (func (nth 0 return-list))
	 (access-control (nth 1 return-list)))
    (if return-list
	(if (and (functionp access-control)
		 (not (funcall access-control to)))
	    (jabber-signal-error "cancel" 'not-allowed)
	  ;; Access control passed
	  (let ((result (if (functionp func)
			    (funcall func xml-data)
			  func)))
	    (jabber-send-iq to "result"
			    `(query ((xmlns . ,xmlns))
				    ,@result)
			    nil nil nil nil id)))

      ;; No such node
      (jabber-signal-error "cancel" 'item-not-found))))

(defun jabber-disco-return-client-info (xml-data)
  `(
    ;; If running under a window system, this is
    ;; a GUI client.  If not, it is a console client.
    (identity ((category . "client")
	       (name . "Emacs Jabber client")
	       (type . ,(if (memq window-system
				  '(x w32 mac))
			    "pc"
			  "console"))))
    ,@(mapcar
       #'(lambda (featurename)
	   `(feature ((var . ,featurename))))
       jabber-advertised-features)))
	
(add-to-list 'jabber-jid-info-menu
	     (cons "Send items disco query" 'jabber-get-disco-items))
(defun jabber-get-disco-items (to &optional node)
  "Send a service discovery request for items"
  (interactive (list (jabber-read-jid-completing "Send items disco request to: ")
		     (jabber-read-node "Node (or leave empty): ")))
  (jabber-send-iq to
		  "get"
		  (list 'query (append (list (cons 'xmlns "http://jabber.org/protocol/disco#items"))
				       (if (> (length node) 0)
					   (list (cons 'node node)))))
		  #'jabber-process-data #'jabber-process-disco-items
		  #'jabber-process-data "Item discovery failed"))

(add-to-list 'jabber-jid-info-menu
	     (cons "Send info disco query" 'jabber-get-disco-info))
(defun jabber-get-disco-info (to &optional node)
  "Send a service discovery request for info"
  (interactive (list (jabber-read-jid-completing "Send info disco request to: ")
		     (jabber-read-node "Node (or leave empty): ")))
  (jabber-send-iq to
		  "get"
		  (list 'query (append (list (cons 'xmlns "http://jabber.org/protocol/disco#info"))
				       (if (> (length node) 0)
					   (list (cons 'node node)))))
		  #'jabber-process-data #'jabber-process-disco-info
		  #'jabber-process-data "Info discovery failed"))

(provide 'jabber-disco)

;;; arch-tag: 71f5c76f-2956-4ed2-b871-9f5fe198092d
