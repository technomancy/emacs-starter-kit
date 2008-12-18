;; jabber-presence.el - roster and presence bookkeeping

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

(require 'jabber-core)
(require 'jabber-iq)
(require 'jabber-alert)
(require 'jabber-util)
(require 'jabber-menu)
(require 'jabber-muc)

(add-to-list 'jabber-iq-set-xmlns-alist
	     (cons "jabber:iq:roster" (function (lambda (x) (jabber-process-roster x nil)))))
(defun jabber-process-roster (xml-data closure-data)
  "process an incoming roster infoquery result
CLOSURE-DATA should be 'initial if initial roster push, nil otherwise."

  ;; Perform sanity check on "from" attribute: it should be either absent
  ;; or match our own JID.
  (let ((from (jabber-xml-get-attribute xml-data 'from))
	(type (jabber-xml-get-attribute xml-data 'type))
	(id (jabber-xml-get-attribute xml-data 'id)))
    (if (not (or (null from)
		 (string= from (concat jabber-username "@" jabber-server))
		 (string= from (concat jabber-username "@" jabber-server "/" jabber-resource))))
	(message "Roster push with invalid \"from\": \"%s\"" from)

      ;; If *jabber-roster* is empty, we just fill up the roster with
      ;; the given data.  If not, we have received a partial roster
      ;; update, so just fill in that data.  These cases can be
      ;; differentiated by the type attribute of the iq tag: if
      ;; type='result', we asked for the whole roster.  If type='set',
      ;; we are getting a "roster push".
      (dolist (item (jabber-xml-get-children (car (jabber-xml-get-children xml-data 'query)) 'item))
	(let (roster-item
	      (jid (jabber-jid-symbol (jabber-xml-get-attribute item 'jid))))

	  ;; Find contact if already in roster
	  (setq roster-item (car (memq jid *jabber-roster*)))

	  ;; If not found, create a new roster item.
	  (when (null roster-item)
	    (message "%s added to roster" jid)
	    (setq roster-item jid)
	    (setq *jabber-roster* (cons roster-item *jabber-roster*)))

	  ;; Now, get all data associated with the contact.
	  (put roster-item 'name (jabber-xml-get-attribute item 'name))
	  (put roster-item 'subscription (jabber-xml-get-attribute item 'subscription))
	  (put roster-item 'ask (jabber-xml-get-attribute item 'ask))

	  ;; Since roster items can't be changed incrementally, we
	  ;; save the original XML to be able to modify it, instead of
	  ;; having to reproduce it.  This is for forwards
	  ;; compatibility.
	  (put roster-item 'xml item)

	  ;; xml-parse-tag will put "" as the only child element of an
	  ;; empty element, (e.g. <item jid="foo@bar"/> as opposed to
	  ;; <item jid="foo@bar"><group>baz</group></item>) which
	  ;; xml-get-children subsequently will choke on.  We want to
	  ;; avoid that with an extra check.
	  (put roster-item 'groups (mapcar #'(lambda (foo) (nth 2 foo)) (jabber-xml-get-children item 'group)))

	  ;; If subscripton="remove", contact is to be removed from roster
	  (when (string= (get roster-item 'subscription) "remove")
	    (message "%s removed from roster" jid)
	    (setq *jabber-roster* (delq roster-item *jabber-roster*)))

	  )))
    (jabber-display-roster)
    (if (and id (string= type "set"))
	(jabber-send-iq jabber-server "result" nil
			nil nil nil nil id))))

(add-to-list 'jabber-presence-chain 'jabber-process-presence)
(defun jabber-process-presence (xml-data)
  "process incoming presence tags"
  (let ((from (jabber-xml-get-attribute xml-data 'from))
	(to (jabber-xml-get-attribute xml-data 'to))
	(type (jabber-xml-get-attribute xml-data 'type))
	(presence-show (car (jabber-xml-node-children
			     (car (jabber-xml-get-children xml-data 'show)))))
	(presence-status (car (jabber-xml-node-children
			       (car (jabber-xml-get-children xml-data 'status)))))
	(error (car (jabber-xml-get-children xml-data 'error)))
	(priority (string-to-number (or (car (jabber-xml-node-children (car (jabber-xml-get-children xml-data 'priority))))
					"0"))))
    (cond
     ((string= type "subscribe")
      (run-with-idle-timer 0.01 nil #'jabber-process-subscription-request from presence-status))

     ((jabber-muc-presence-p xml-data)
      (jabber-muc-process-presence xml-data))

     (t
      ;; XXX: Think about what to do about out-of-roster presences.
      (let ((buddy (jabber-jid-symbol from)))
	(if (memq buddy *jabber-roster*)
	    (let* ((oldstatus (get buddy 'show))
		   (resource (or (jabber-jid-resource from) ""))
		   (resource-plist (cdr (assoc resource
					       (get buddy 'resources))))
		   newstatus)
	      (cond
	       ((string= type "unavailable")
		(setq resource-plist
		      (plist-put resource-plist 'connected nil))
		(setq resource-plist
		      (plist-put resource-plist 'show nil))
		(setq resource-plist
		      (plist-put resource-plist 'status
				 (jabber-unescape-xml presence-status))))

	       ((string= type "error")
		(setq newstatus "error")
		(setq resource-plist
		      (plist-put resource-plist 'connected nil))
		(setq resource-plist
		      (plist-put resource-plist 'show "error"))
		(setq resource-plist
		      (plist-put resource-plist 'status
				 (if error
				     (jabber-parse-error error)
				   (jabber-unescape-xml presence-status)))))
	       ((or
		 (string= type "unsubscribe")
		 (string= type "subscribed")
		 (string= type "unsubscribed"))
		;; Do nothing, except letting the user know.  The Jabber protocol
		;; places all this complexity on the server.
		(setq newstatus type))
	       (t
		(setq resource-plist
		      (plist-put resource-plist 'connected t))
		(setq resource-plist
		      (plist-put resource-plist 'show (or presence-show "")))
		(setq resource-plist
		      (plist-put resource-plist 'status
				 (jabber-unescape-xml presence-status)))
		(setq resource-plist
		      (plist-put resource-plist 'priority priority))
		(setq newstatus (or presence-show ""))))

	      ;; this is for `assoc-set!' in guile
	      (if (assoc resource (get buddy 'resources))
		  (setcdr (assoc resource (get buddy 'resources)) resource-plist)
		(put buddy 'resources (cons (cons resource resource-plist) (get buddy 'resources))))
	      (jabber-prioritize-resources buddy)

	      (dolist (hook '(jabber-presence-hooks jabber-alert-presence-hooks))
		(run-hook-with-args hook
				    buddy
				    oldstatus
				    newstatus
				    (jabber-unescape-xml 
				     (plist-get resource-plist 'status))
				    (funcall jabber-alert-presence-message-function 
					     buddy
					     oldstatus
					     newstatus
					     (jabber-unescape-xml
					      (plist-get resource-plist 'status))))))))))))

(defun jabber-process-subscription-request (from presence-status)
  "process an incoming subscription request"
  (dolist (hook '(jabber-presence-hooks jabber-alert-presence-hooks))
    (run-hook-with-args hook (jabber-jid-symbol from) nil "subscribe" presence-status (funcall jabber-alert-presence-message-function (jabber-jid-symbol from) nil "subscribe" presence-status)))
  (jabber-send-sexp 
   (list 'presence
	 (list (cons 'to from)
	       (cons 'type
		     (if (yes-or-no-p (format "the user  - %s -  has requested to subscribe to your presence (%s). allow? "
					      (jabber-jid-displayname from)
					      (jabber-unescape-xml presence-status)))
			 "subscribed"
		       "unsubscribed")))))
  (when (yes-or-no-p (format "Do you want to subscribe to %s's presence? " from))
    (jabber-send-sexp
     (list 'presence (list (cons 'to from)
			   (cons 'type "subscribe"))))))

(defun jabber-prioritize-resources (buddy)
  "Set connected, show and status properties for BUDDY from highest-priority resource."
  (let ((resource-alist (get buddy 'resources))
	(highest-priority nil))
    ;; Reset to nil at first, for cases (a) resource-alist is nil
    ;; and (b) all resources are disconnected.
    (put buddy 'connected nil)
    (put buddy 'show nil)
    (put buddy 'status nil)
    (mapc #'(lambda (resource)
	      (let* ((resource-plist (cdr resource))
		     (priority (plist-get resource-plist 'priority)))
		(if (plist-get resource-plist 'connected)
		    (when (or (null highest-priority)
			      (and priority
				   (> priority highest-priority)))
		      ;; if no priority specified, interpret as zero
		      (setq highest-priority (or priority 0))
		      (put buddy 'connected (plist-get resource-plist 'connected))
		      (put buddy 'show (plist-get resource-plist 'show))
		      (put buddy 'status (plist-get resource-plist 'status))
		      (put buddy 'resource (car resource)))

		  ;; if we have not found a connected resource yet, but this
		  ;; disconnected resource has a status message, display it.
		  (when (not (get buddy 'connected))
		    (if (plist-get resource-plist 'status)
			(put buddy 'status (plist-get resource-plist 'status)))
		    (if (plist-get resource-plist 'show)
			(put buddy 'show (plist-get resource-plist 'show)))))))
	  resource-alist)))

(defun jabber-count-connected-resources (buddy)
  "Return the number of connected resources for BUDDY."
  (let ((resource-alist (get buddy 'resources))
	(count 0))
    (dolist (resource resource-alist)
      (if (plist-get (cdr resource) 'connected)
	  (setq count (1+ count))))
    count))

(defun jabber-send-presence (show status priority)
  "send a presence tag to the server"
  (interactive (list (completing-read "show:"
				      '(("" . nil)
					("away" . nil)
					("xa" . nil)
					("dnd" . nil)
					("chat" . nil))
				      nil t)
		     (jabber-read-with-input-method "status message: " *jabber-current-status* '*jabber-status-history*)
		     (read-string "priority: " (progn
						 (unless *jabber-current-priority*
						   (setq *jabber-current-priority*
							 jabber-default-priority))
						 (int-to-string *jabber-current-priority*)))))
  (if (numberp priority)
      (setq priority (int-to-string priority)))
  (setq *jabber-current-status* status)
  (setq *jabber-current-show* show)
  (setq *jabber-current-priority* (string-to-int priority))
  (jabber-send-sexp `(presence ()
                               ,(if (> (length status) 0)
                                    `(status () ,(jabber-escape-xml status)))
			       ,(if (> (length show) 0)
                                    `(show () ,(jabber-escape-xml show)))
			       (priority () ,(jabber-escape-xml (int-to-string *jabber-current-priority*)))))
  (jabber-display-roster))

(defun jabber-send-away-presence ()
  "Set status to away.
Status description is empty.  Priority is unchanged."
  (interactive)
  (jabber-send-presence "away" "" *jabber-current-priority*))

(defun jabber-send-xa-presence ()
  "Send extended away presence.
Status description is empty.  Priority is unchanged."
  (interactive)
  (jabber-send-presence "xa" "" *jabber-current-priority*))

(defun jabber-send-default-presence ()
  "Send default presence.
Default presence is specified by `jabber-default-priority', `jabber-default-show',
and `jabber-default-status'."
  (interactive)
  (jabber-send-presence jabber-default-show jabber-default-status jabber-default-priority))

(add-to-list 'jabber-jid-roster-menu
	     (cons "Send subscription request" 'jabber-send-subscription-request))
(defun jabber-send-subscription-request (to &optional request)
  "send a subscription request to jid, showing him your request text, if specified"
  (interactive (list (jabber-read-jid-completing "to: ")
		     (jabber-read-with-input-method "request: ")))
  (jabber-send-sexp `(presence ((to . ,to)
                                (type . "subscribe"))
                               ,(if (and request (> (length request) 0))
                                   request))))

(add-to-list 'jabber-jid-roster-menu
	     (cons "Add/modify roster entry" 'jabber-roster-change))
(defun jabber-roster-change (jid name groups)
  "Add or change a roster item."
  (interactive (let* ((jid (jabber-jid-symbol
			    (jabber-read-jid-completing "Add/change JID: ")))
		      (name (get jid 'name))
		      (groups (get jid 'groups)))
		 (list jid (jabber-read-with-input-method (format "Name: (default `%s') " name) nil nil name)
		       (car (read-from-string (jabber-read-with-input-method (format "Groups: (default `%S') " groups) nil nil (format "%S" groups)))))))
  ;; If new fields are added to the roster XML structure in a future standard,
  ;; they will be clobbered by this function.
  (jabber-send-iq nil "set" 
		  (list 'query (list (cons 'xmlns "jabber:iq:roster"))
			(list 'item (append
				     (list (cons 'jid (symbol-name jid)))
				     (if (and name (> (length name) 0))
					 (list (cons 'name name))))
			      (mapcar #'(lambda (x) `(group () ,x))
				      groups))) 
		  #'jabber-report-success "Roster item change"
		  #'jabber-report-success "Roster item change"))

(add-to-list 'jabber-jid-roster-menu
	     (cons "Delete roster entry" 'jabber-roster-delete))
(defun jabber-roster-delete (jid)
  (interactive (list (jabber-read-jid-completing "Delete from roster: ")))
  (jabber-send-iq nil "set"
		  `(query ((xmlns . "jabber:iq:roster"))
			  (item ((jid . ,jid)
				 (subscription . "remove"))))
		  #'jabber-report-success "Roster item removal"
		  #'jabber-report-success "Roster item removal"))

(defun jabber-roster-delete-jid-at-point ()
  "Delete JID at point from roster.
Signal an error if there is no JID at point."
  (interactive)
  (let ((jid-at-point (get-text-property (point)
					 'jabber-jid)))
    (if (and jid-at-point
	     (yes-or-no-p (format "Really delete %s from roster? " jid-at-point)))
	(jabber-roster-delete jid-at-point)
      (error "No contact at point"))))

(provide 'jabber-presence)

;;; arch-tag: b8616d4c-dde8-423e-86c7-da7b4928afc3
