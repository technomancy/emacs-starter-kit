;;; jabber-events.el --- Message events (JEP-0022) implementation

;; Copyright (C) 2005  Magnus Henoch

;; Author: Magnus Henoch <mange@freemail.hu>

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

(require 'cl)

(defgroup jabber-events nil 
  "Message events and notifications."
  :group 'jabber)

;;; INCOMING
;;; Code for requesting event notifications from others and handling
;;; them.

(defcustom jabber-events-request-these '(offline
					 delivered
					 displayed
					 composing)
  "Request these kinds of event notifications from others."
  :type '(set (const :tag "Delivered to offline storage" offline)
	      (const :tag "Delivered to user's client" delivered)
	      (const :tag "Displayed to user" displayed)
	      (const :tag "User is typing a reply" composing))
  :group 'jabber-events)

(defvar jabber-events-composing-p nil
  "Is the other person composing a message?")
(make-variable-buffer-local 'jabber-events-composing-p)

(defvar jabber-events-arrived nil
  "In what way has the message reached the recipient?
Possible values are nil (no information available), offline
\(queued for delivery when recipient is online), delivered
\(message has reached the client) and displayed (user is
probably reading the message).")
(make-variable-buffer-local 'jabber-events-arrived)

(defvar jabber-events-message ""
  "Human-readable presentation of event information")
(make-variable-buffer-local 'jabber-events-message)

(defun jabber-events-update-message ()
  (setq jabber-events-message 
	(concat (cdr (assq jabber-events-arrived
			   '((offline . "In offline storage")
			     (delivered . "Delivered")
			     (displayed . "Displayed"))))
		(when jabber-events-composing-p
		  " (typing a message)"))))

(add-hook 'jabber-chat-send-hooks 'jabber-events-when-sending)
(defun jabber-events-when-sending (text id)
  (setq jabber-events-arrived nil)
  (jabber-events-update-message)
  `((x ((xmlns . "jabber:x:event"))
       ,@(mapcar #'list jabber-events-request-these))))

;;; OUTGOING
;;; Code for handling requests for event notifications and providing
;;; them, modulo user preferences.

(defcustom jabber-events-confirm-delivered t
  "Send delivery confirmation if requested?"
  :group 'jabber-events
  :type 'boolean)

(defcustom jabber-events-confirm-displayed t
  "Send display confirmation if requested?"
  :group 'jabber-events
  :type 'boolean)

(defcustom jabber-events-confirm-composing t
  "Send notifications about typing a reply?"
  :group 'jabber-events
  :type 'boolean)

(defvar jabber-events-requested ()
  "List of events requested")
(make-variable-buffer-local 'jabber-events-requested)

(defvar jabber-events-last-id nil
  "Id of last message received, or nil if none.")
(make-variable-buffer-local 'jabber-events-last-id)

(defvar jabber-events-delivery-confirmed nil
  "Has delivery confirmation been sent?")
(make-variable-buffer-local 'jabber-events-delivery-confirmed)

(defvar jabber-events-display-confirmed nil
  "Has display confirmation been sent?")
(make-variable-buffer-local 'jabber-events-display-confirmed)

(defvar jabber-events-composing-sent nil
  "Has composing notification been sent?
It can be sent and cancelled several times.")

(add-hook 'window-configuration-change-hook
	  'jabber-events-confirm-display)
(defun jabber-events-confirm-display ()
  "Send display confirmation if appropriate.
That is, if user allows it, if the other user requested it,
and it hasn't been sent before."
  (walk-windows #'jabber-events-confirm-display-in-window))

(defun jabber-events-confirm-display-in-window (window)
  (with-current-buffer (window-buffer window)
    (when (and jabber-events-confirm-displayed
	       (not jabber-events-display-confirmed)
	       (memq 'displayed jabber-events-requested)
	       ;; don't send to bare jids
	       (jabber-jid-resource jabber-chatting-with))
      (jabber-send-sexp 
       `(message 
	 ((to . ,jabber-chatting-with))
	 (x ((xmlns . "jabber:x:event"))
	    (displayed)
	    (id () ,jabber-events-last-id))))
      (setq jabber-events-display-confirmed t))))

(defun jabber-events-after-change ()
  (let ((composing-now (not (eq (point-max) jabber-point-insert))))
    (when (and jabber-events-confirm-composing
	       jabber-chatting-with
	       (not (eq composing-now jabber-events-composing-sent)))
      (jabber-send-sexp 
       `(message 
	 ((to . ,jabber-chatting-with))
	 (x ((xmlns . "jabber:x:event"))
	    ,@(if composing-now '((composing)) nil)
	    (id () ,jabber-events-last-id))))
      (setq jabber-events-composing-sent composing-now))))

;;; COMMON

(add-to-list 'jabber-chat-printers 'jabber-handle-incoming-message-events)

(defun jabber-handle-incoming-message-events (xml-data)
  (let ((x (find "jabber:x:event"
		 (jabber-xml-get-children xml-data 'x)
		 :key #'(lambda (x) (jabber-xml-get-attribute x 'xmlns))
		 :test #'string=)))
    ;; If there's a body, it's not an incoming message event.
    (if (jabber-xml-get-children xml-data 'body)
	;; User is done composing, obviously.
	(progn
	  (setq jabber-events-composing-p nil)
	  (jabber-events-update-message)

	  ;; Reset variables
	  (setq jabber-events-display-confirmed nil)
	  (setq jabber-events-delivery-confirmed nil)

	  ;; User requests message events
	  (setq jabber-events-requested 
		;; There might be empty strings in the XML data,
		;; which car chokes on.  Having nil values in
		;; the list won't hurt, therefore car-safe.
		(mapcar #'car-safe 
			(jabber-xml-node-children x)))
	  (setq jabber-events-last-id (jabber-xml-get-attribute
				       xml-data 'id))

	  ;; Send notifications we already know about
	  (flet ((send-notification 
		  (type)
		  (jabber-send-sexp 
		   `(message 
		     ((to . ,(jabber-xml-get-attribute xml-data 'from)))
		     (x ((xmlns . "jabber:x:event"))
			(,type)
			(id () ,jabber-events-last-id))))))
	    ;; Send delivery confirmation if appropriate
	    (when (and jabber-events-confirm-delivered
		       (memq 'delivered jabber-events-requested))
	      (send-notification 'delivered)
	      (setq jabber-events-delivery-confirmed t))

	    ;; Send display confirmation if appropriate
	    (when (and jabber-events-confirm-displayed
		       (get-buffer-window (current-buffer) 'visible)
		       (memq 'displayed jabber-events-requested))
	      (send-notification 'displayed)
	      (setq jabber-events-display-confirmed t))

	    ;; Set up hooks for composition notification
	    (when (and jabber-events-confirm-composing
		       (memq 'composing jabber-events-requested))
	      (add-hook 'post-command-hook 'jabber-events-after-change
			nil t))))
      ;; So it has no body.  If it's a message event,
      ;; the <x/> node should be the only child of the
      ;; message, and it should contain an <id/> node.
      ;; We check the latter.
      (when (and x (jabber-xml-get-children x 'id))
	;; Currently we don't care about the <id/> node.
	
	;; There's only one node except for the id.
	(unless
	    (dolist (possible-node '(offline delivered displayed))
	      (when (jabber-xml-get-children x possible-node)
		(setq jabber-events-arrived possible-node)
		(jabber-events-update-message)
		(return t)))
	  ;; Or maybe even zero, which is a negative composing node.
	  (setq jabber-events-composing-p
		(not (null (jabber-xml-get-children x 'composing))))
	  (jabber-events-update-message))))))

(provide 'jabber-events)
;; arch-tag: 7b6e61fe-a9b3-11d9-afca-000a95c2fcd0
