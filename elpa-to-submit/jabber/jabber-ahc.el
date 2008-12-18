;; jabber-ahc.el - Ad-Hoc Commands by JEP-0050

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

(require 'jabber-disco)
(require 'jabber-widget)

(defvar jabber-ahc-sessionid nil
  "session id of Ad-Hoc Command session")

(defvar jabber-ahc-node nil
  "node to send commands to")

(defvar jabber-ahc-commands nil
  "Commands provided

This is an alist, where the keys are node names as strings (which
means that they must not conflict).  The values are plists having
following properties:

acl	- function taking JID as single argument, return non-nil for
	  access allowed.  No function means open for everyone.
name	- name of command
func	- function receiving entire IQ stanza as single argument
	  and returning a <command/> node

Use the function `jabber-ahc-add' to add a command to this list.")


;;; SERVER
(add-to-list 'jabber-disco-info-nodes
	     (list "http://jabber.org/protocol/commands"
		   '((identity ((category . "automation")
				(type . "command-list")
				(name . "Ad-Hoc Command list")))
		     (feature ((var . "http://jabber.org/protocol/commands")))
		     (feature ((var . "http://jabber.org/protocol/disco#items")))
		     (feature
		      ((var . "http://jabber.org/protocol/disco#info"))))))

(defun jabber-ahc-add (node name func acl)
  "Add a command to internal lists.
NODE is the node name to be used.  It must be unique.
NAME is the natural-language name of the command.
FUNC is a function taking the entire IQ stanza as single argument when
this command is invoked, and returns a <command/> node.
ACL is a function taking JID as single argument, returning non-nil for
access allowed.  nil means open for everyone."
  (add-to-list 'jabber-ahc-commands (cons node (list 'name name
						     'func func
						     'acl acl)))
  (add-to-list 'jabber-disco-info-nodes
	       (list node `((identity ((category . "automation")
				       (type . "command-node")
				       (name . ,name)))
			    (feature ((var . "http://jabber.org/protocol/commands")))
			    (feature ((var . "http://jabber.org/protocol/disco#info")))
			    (feature ((var . "jabber:x:data")))))))

(add-to-list 'jabber-advertised-features "http://jabber.org/protocol/commands")
(add-to-list 'jabber-disco-items-nodes 
	     (list "http://jabber.org/protocol/commands" #'jabber-ahc-disco-items nil))
(defun jabber-ahc-disco-items (xml-data)
  "Return commands in response to disco#items request"
  (let ((jid (jabber-xml-get-attribute xml-data 'from)))
    (mapcar (function
	     (lambda (command)
	       (let ((node (car command))
		     (plist (cdr command)))
		 (let ((acl (plist-get plist 'acl))
		       (name (plist-get plist 'name))
		       (func (plist-get plist 'func)))
		   (when (or (not (functionp acl))
			     (funcall acl jid))
		     `(item ((name . ,name)
			     (jid . ,(format "%s@%s/%s" jabber-username jabber-server jabber-resource))
			     (node . ,node))))))))
	    jabber-ahc-commands)))

(add-to-list 'jabber-iq-set-xmlns-alist
	     (cons "http://jabber.org/protocol/commands" 'jabber-ahc-process))
(defun jabber-ahc-process (xml-data)

  (let ((to (jabber-xml-get-attribute xml-data 'from))
	(id (jabber-xml-get-attribute xml-data 'id))
	(node (jabber-xml-get-attribute (jabber-iq-query xml-data) 'node)))
    ;; find command
    (let* ((plist (cdr (assoc node jabber-ahc-commands)))
	   (acl (plist-get plist 'acl))
	   (func (plist-get plist 'func)))
      (if plist
	  ;; found
	  (if (or (not (functionp acl))
		  (funcall acl to))
	      ;; access control passed
	      (jabber-send-iq to "result"
			      (funcall func xml-data)
			      nil nil nil nil id)
	    ;; ...or failed
	    (jabber-signal-error "cancel" 'not-allowed))
	;; No such node
	(jabber-signal-error "cancel" 'item-not-found)))))

;;; CLIENT
(add-to-list 'jabber-jid-service-menu
	     (cons "Request command list" 'jabber-ahc-get-list))
(defun jabber-ahc-get-list (to)
  "Request list of ad-hoc commands.  (JEP-0050)"
  (interactive (list (jabber-read-jid-completing "Request command list from: ")))
  (jabber-get-disco-items to "http://jabber.org/protocol/commands"))

(add-to-list 'jabber-jid-service-menu
	     (cons "Execute command" 'jabber-ahc-execute-command))
(defun jabber-ahc-execute-command (to node)
  "Execute ad-hoc command.  (JEP-0050)"
  (interactive (list (jabber-read-jid-completing "Execute command of: ")
		     (jabber-read-node "Node of command: ")))
  (jabber-send-iq to
		  "set"
		  `(command ((xmlns . "http://jabber.org/protocol/commands")
			     (node . ,node)
			     (action . "execute")))
		  #'jabber-process-data #'jabber-ahc-display
		  #'jabber-process-data "Command execution failed"))

(defun jabber-ahc-display (xml-data)
  (let* ((from (jabber-xml-get-attribute xml-data 'from))
	 (query (jabber-iq-query xml-data))
	 (node (jabber-xml-get-attribute query 'node))
	 (notes (jabber-xml-get-children query 'note))
	 (sessionid (jabber-xml-get-attribute query 'sessionid))
	 (status (jabber-xml-get-attribute query 'status))
	 (actions (car (jabber-xml-get-children query 'actions)))
	 xdata
	 (inhibit-read-only t))

    (make-local-variable 'jabber-ahc-sessionid)
    (setq jabber-ahc-sessionid sessionid)
    (make-local-variable 'jabber-ahc-node)
    (setq jabber-ahc-node node)

    (dolist (x (jabber-xml-get-children query 'x))
      (when (string= (jabber-xml-get-attribute x 'xmlns) "jabber:x:data")
	(setq xdata x)))

    (cond
     ((string= status "executing")
      (insert "Executing command\n\n"))
     ((string= status "completed")
      (insert "Command completed\n\n"))
     ((string= status "canceled")
      (insert "Command canceled\n\n")))

    (dolist (note notes)
      (let ((note-type (jabber-xml-get-attribute note 'type)))
	(cond
	 ((string= note-type "warn")
	  (insert "Warning: "))
	 ((string= note-type "error")
	  (insert "Error: ")))
	(insert (car (jabber-xml-node-children note)) "\n")))
    (insert "\n")

    (when xdata
      (jabber-init-widget-buffer from)

      (let ((formtype (jabber-xml-get-attribute xdata 'type)))
	(if (string= formtype "result")
	    (jabber-render-xdata-search-results xdata)
	  (jabber-render-xdata-form xdata)

	  (when (string= status "executing")
	    (let ((button-titles 
		   (cond
		    ((null actions)
		     '(complete cancel))
		    (t
		     (let ((children (mapcar #'jabber-xml-node-name (jabber-xml-node-children actions)))
			   (default-action (jabber-xml-get-attribute actions 'execute)))
		       (if (or (null default-action) (memq (intern default-action) children))
			   children
			 (cons (intern default-action) children)))))))
	      (dolist (button-title button-titles)
		(widget-create 'push-button :notify `(lambda (&rest ignore) (jabber-ahc-submit (quote ,button-title))) (symbol-name button-title))
		(widget-insert "\t")))
	    (widget-insert "\n"))))

      (widget-setup)
      (widget-minor-mode 1))))

(defun jabber-ahc-submit (action)
  "Submit Ad-Hoc Command."

  (jabber-send-iq jabber-submit-to
		  "set"
		  `(command ((xmlns . "http://jabber.org/protocol/commands")
			     (sessionid . ,jabber-ahc-sessionid)
			     (node . ,jabber-ahc-node)
			     (action . ,(symbol-name action)))
			    ,(if (and (not (eq action 'cancel))
				      (eq jabber-form-type 'xdata))
				 (jabber-parse-xdata-form)))

		  #'jabber-process-data #'jabber-ahc-display
		  #'jabber-process-data "Command execution failed"))

(provide 'jabber-ahc)

;;; arch-tag: c0d5ed8c-50cb-44e1-8e0f-4058b79ee353
