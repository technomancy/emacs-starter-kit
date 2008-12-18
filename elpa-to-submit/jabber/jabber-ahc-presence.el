;; jabber-ahc-presence.el - provide remote control of presence

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

(require 'jabber-ahc)

(defconst jabber-ahc-presence-node "presence"
  "Node used by jabber-ahc-presence")

(jabber-ahc-add jabber-ahc-presence-node "Set presence" 'jabber-ahc-presence
		'jabber-my-jid-p)

(defun jabber-ahc-presence (xml-data)
  "Process presence change command."

  (let* ((query (jabber-iq-query xml-data))
	 (sessionid (jabber-xml-get-attribute query 'sessionid))
	 (action (jabber-xml-get-attribute query 'action)))
    ;; No session state is kept; instead, lack of session-id is used
    ;; as indication of first command.
    (cond
     ;; command cancelled
     ((string= action "cancel")
      `(command ((xmlns . "http://jabber.org/protocol/commands")
		 (sessionid . ,sessionid)
		 (node . ,jabber-ahc-presence-node)
		 (status . "canceled"))))
     ;; return form
     ((null sessionid)
      `(command ((xmlns . "http://jabber.org/protocol/commands")
		 (sessionid . "jabber-ahc-presence")
		 (node . ,jabber-ahc-presence-node)
		 (status . "executing"))
		(x ((xmlns . "jabber:x:data")
		    (type . "form"))
		   (title nil ,(format "Set presence of %s@%s/%s" jabber-username jabber-server jabber-resource))
		   (instructions nil "Select new presence status.")
		   (field ((var . "show")
			   (label . "Show")
			   (type . "list-single"))
			  (value nil ,(if (string= *jabber-current-show* "")
					  "online"
					*jabber-current-show*))
			  (option ((label . "Online")) (value nil "online"))
			  (option ((label . "Chatty")) (value nil "chat"))
			  (option ((label . "Away")) (value nil "away"))
			  (option ((label . "Extended away")) (value nil "xa"))
			  (option ((label . "Do not disturb")) (value nil "dnd")))
		   (field ((var . "status")
			   (label . "Status text")
			   (type . "text-single"))
			  (value nil ,*jabber-current-status*))
		   (field ((var . "priority")
			   (label . "Priority")
			   (type . "text-single"))
			  (value nil ,(int-to-string *jabber-current-priority*))))))
     ;; process form
     (t
      (let* ((x (car (jabber-xml-get-children query 'x)))
	;; we assume that the first <x/> is the jabber:x:data one
	     (fields (jabber-xml-get-children x 'field))
	     (new-show *jabber-current-show*)
	     (new-status *jabber-current-status*)
	     (new-priority *jabber-current-priority*))
	(dolist (field fields)
	  (let ((var (jabber-xml-get-attribute field 'var))
		;; notice that multi-value fields won't be handled properly
		;; by this
		(value (car (jabber-xml-node-children (car (jabber-xml-get-children field 'value))))))
	    (cond
	     ((string= var "show")
	      (setq new-show (if (string= value "online")
				 ""
			       value)))
	     ((string= var "status")
	      (setq new-status value))
	     ((string= var "priority")
	      (setq new-priority (string-to-int value))))))
	(jabber-send-presence new-show new-status new-priority))
      `(command ((xmlns . "http://jabber.org/protocol/commands")
		 (sessionid . ,sessionid)
		 (node . ,jabber-ahc-presence-node)
		 (status . "completed"))
		(note ((type . "info")) "Presence has been changed."))))))

(provide 'jabber-ahc-presence)

;;; arch-tag: 4b8cbbe7-00a9-4d42-a4ac-b824ab914fba
