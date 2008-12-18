;; jabber-util.el - various utility functions

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

(defvar jabber-jid-history nil
  "History of entered JIDs")

(defvar *jabber-sound-playing* nil
  "is a sound playing right now?")

(cond
 ((fboundp 'replace-in-string)
  (defsubst jabber-replace-in-string (str regexp newtext)
    (replace-in-string str regexp newtext t)))
 ((fboundp 'replace-regexp-in-string)
  (defsubst jabber-replace-in-string (str regexp newtext)
    (replace-regexp-in-string regexp newtext str t t))))

;;; XEmacs compatibility.  Stolen from ibuffer.el
(if (fboundp 'propertize)
    (defalias 'jabber-propertize 'propertize)
  (defun jabber-propertize (string &rest properties)
    "Return a copy of STRING with text properties added.

 [Note: this docstring has been copied from the Emacs 21 version]

First argument is the string to copy.
Remaining arguments form a sequence of PROPERTY VALUE pairs for text
properties to add to the result."
    (let ((str (copy-sequence string)))
      (add-text-properties 0 (length str)
			   properties
			   str)
      str)))

(unless (fboundp 'bound-and-true-p)
  (defmacro bound-and-true-p (var)
    "Return the value of symbol VAR if it is bound, else nil."
    `(and (boundp (quote ,var)) ,var)))

;;; more XEmacs compatibility
;;; Preserve input method when entering a minibuffer
(if (featurep 'xemacs)
    ;; I don't know how to do this
    (defsubst jabber-read-with-input-method (prompt &optional initial-contents history default-value)
      (read-string prompt initial-contents history default-value))
  (defsubst jabber-read-with-input-method (prompt &optional initial-contents history default-value)
    (read-string prompt initial-contents history default-value t)))

(unless (fboundp 'delete-and-extract-region)
  (defsubst delete-and-extract-region (start end)
    (prog1
	(buffer-substring start end)
      (delete-region start end))))

(unless (fboundp 'access-file)
  (defsubst access-file (filename error-message)
    (unless (file-readable-p filename)
      (error error-message))))

(if (fboundp 'float-time)
    (defalias 'jabber-float-time 'float-time)
  (defun jabber-float-time (&optional specified-time)
    (unless specified-time
      (setq specified-time (current-time)))
    ;; second precision is good enough for us
    (+ (* 65536.0 (car specified-time))
       (cadr specified-time))))

(cond
 ((fboundp 'cancel-timer)
  (defalias 'jabber-cancel-timer 'cancel-timer))
 ((fboundp 'delete-itimer)
  (defalias 'jabber-cancel-timer 'delete-itimer))
 (t
  (error "No `cancel-timer' function found")))

(defun jabber-jid-username (string)
  "return the username portion of a JID, or nil if no username"
  (when (string-match "\\(.*\\)@.*\\(/.*\\)?" string)
    (match-string 1 string)))

(defun jabber-jid-user (string)
  "return the user (username@server) portion of a JID"
  ;;transports don't have @, so don't require it
  ;;(string-match ".*@[^/]*" string)
  (string-match "[^/]*" string)
  (match-string 0 string))

(defun jabber-jid-server (string)
  "Return the server portion of a JID."
  (string-match "^\\(.*@\\)?\\([^@/]+\\)\\(/.*\\)?$" string)
  (match-string 2 string))

(defun jabber-jid-rostername (string)
  "return the name of the user, if given in roster, else nil"
  (let ((user (jabber-jid-symbol string)))
    (if (> (length (get user 'name)) 0)
	(get user 'name))))

(defun jabber-jid-displayname (string)
  "return the name of the user, if given in roster, else username@server"
  (or (jabber-jid-rostername string)
      (jabber-jid-user (if (symbolp string)
			   (symbol-name string)
			 string))))

(defun jabber-jid-resource (string)
  "return the resource portion of a JID, or nil if there is none."
  (when (string-match "^\\(\\([^/]*@\\)?[^/]*\\)/\\(.*\\)" string)
    (match-string 3 string)))

(defun jabber-jid-symbol (string)
  "return the symbol for the given JID"
  ;; If it's already a symbol, just return it.
  (if (symbolp string)
      string
    ;; XXX: "downcase" is poor man's nodeprep.  See XMPP CORE.
    (intern (downcase (jabber-jid-user string)) jabber-jid-obarray)))

(defun jabber-my-jid-p (jid)
  "Return non-nil if the specified JID is equal to the user's JID, modulo resource."
  (equal (jabber-jid-user jid)
	 (concat jabber-username "@" jabber-server)))

(defun jabber-read-jid-completing (prompt &optional subset require-match default)
  "read a jid out of the current roster from the minibuffer.
If SUBSET is non-nil, it should be a list of symbols from which
the JID is to be selected, instead of using the entire roster.
If REQUIRE-MATCH is non-nil, the JID must be in the list used.
If DEFAULT is non-nil, it's used as the default value, otherwise
the default is inferred from context."
  (let ((jid-at-point (or 
		       (and default
			    ;; default can be either a symbol or a string
			    (if (symbolp default)
				(symbol-name default)
			      default))
		       (get-text-property (point) 'jabber-jid)
		       (bound-and-true-p jabber-chatting-with)
		       (bound-and-true-p jabber-group)))
	(completion-ignore-case t)
	(jid-completion-table (mapcar #'(lambda (item)
					  (cons (symbol-name item) item))
				      (or subset *jabber-roster*))))
    (dolist (item (or subset *jabber-roster*))
      (if (get item 'name)
	  (push (cons (get item 'name) item) jid-completion-table)))
    ;; if the default is not in the allowed subset, it's not a good default
    (if (and subset (not (assoc jid-at-point jid-completion-table)))
	(setq jid-at-point nil))
    (let ((input
	   (completing-read (concat prompt
				    (if jid-at-point
					(format "(default %s) " jid-at-point)))
			    jid-completion-table
			    nil require-match nil 'jabber-jid-history jid-at-point)))
      (if (and input (assoc-ignore-case input jid-completion-table))
	  (symbol-name (cdr (assoc-ignore-case input jid-completion-table)))
	(and (not (zerop (length input)))
	     input)))))

(defun jabber-read-node (prompt)
  "Read node name, taking default from disco item at point."
  (let ((node-at-point (get-text-property (point) 'jabber-node)))
    (read-string (concat prompt
			 (if node-at-point
			     (format "(default %s) " node-at-point)))
		 node-at-point)))

(defun jabber-read-passwd (&optional prompt)
  "Read Jabber password, either from customized variable or from minibuffer.
See `jabber-password'."
  (or jabber-password (read-passwd (or prompt "Jabber password: "))))

(defun jabber-iq-query (xml-data)
  "Return the query part of an IQ stanza.
An IQ stanza may have zero or one query child, and zero or one <error/> child.
The query child is often but not always <query/>."
  (let (query)
    (dolist (x (jabber-xml-node-children xml-data))
      (if (and
	   (listp x)
	   (not (eq (jabber-xml-node-name x) 'error)))
	  (setq query x)))
    query))

(defun jabber-iq-error (xml-data)
  "Return the <error/> part of an IQ stanza, if any."
  (car (jabber-xml-get-children xml-data 'error)))

(defun jabber-iq-xmlns (xml-data)
  "Return the namespace of an IQ stanza, i.e. the namespace of its query part."
  (jabber-xml-get-attribute (jabber-iq-query xml-data) 'xmlns))

(defun jabber-x-delay (xml-data)
  "Return timestamp given a <x/> tag in namespace jabber:x:delay.
Return nil if no such data available."
  (when (and (eq (jabber-xml-node-name xml-data) 'x)
	     (string= (jabber-xml-get-attribute xml-data 'xmlns) "jabber:x:delay"))
    (let ((stamp (jabber-xml-get-attribute xml-data 'stamp)))
      (if (and (stringp stamp)
	       (= (length stamp) 17))
	  (jabber-parse-legacy-time stamp)))))
      
(defun jabber-parse-legacy-time (timestamp)
  "Parse timestamp in ccyymmddThh:mm:ss format (UTC) and return as internal time value."
  (let ((year (string-to-number (substring timestamp 0 4)))
	(month (string-to-number (substring timestamp 4 6)))
	(day (string-to-number (substring timestamp 6 8)))
	(hour (string-to-number (substring timestamp 9 11)))
	(minute (string-to-number (substring timestamp 12 14)))
	(second (string-to-number (substring timestamp 15 17))))
    (encode-time second minute hour day month year 0)))

(defun jabber-encode-legacy-time (timestamp)
  "Parse TIMESTAMP as internal time value and encode as ccyymmddThh:mm:ss (UTC)."
  (if (featurep 'xemacs)
      ;; XEmacs doesn't have `universal' argument to format-time-string,
      ;; so we have to do it ourselves.
      (format-time-string "%Y%m%dT%H:%M:%S" 
			  (time-subtract timestamp 
					 (list 0 (car (current-time-zone)))))
    (format-time-string "%Y%m%dT%H:%M:%S" timestamp t)))
    
(defun jabber-encode-time (time)
  "Convert TIME to a string by JEP-0082.
TIME is in a format accepted by `format-time-string'."
  (let ((time-zone-offset (nth 0 (current-time-zone))))
    (if (null time-zone-offset)
	;; no time zone information available; pretend it's UTC
	(format-time-string "%Y-%m-%dT%H:%M:%SZ" time)
      (let* ((positivep (>= time-zone-offset 0))
	     (hours (/ (abs time-zone-offset) 3600))
	     (minutes (/ (% (abs time-zone-offset) 3600) 60)))
	(format "%s%s%02d:%02d" (format-time-string "%Y-%m-%dT%H:%M:%S" time)
		(if positivep "+" "-") hours minutes)))))

(defun jabber-parse-time (time)
  "Parse the DateTime encoded in TIME according to JEP-0082."
  (let* ((year (string-to-number (substring time 0 4)))
	 (month (string-to-number (substring time 5 7)))
	 (day (string-to-number (substring time 8 10)))
	 (hour (string-to-number (substring time 11 13)))
	 (minute (string-to-number (substring time 14 16)))
	 (second (string-to-number (substring time 17 19)))
	 ;; fractions are optional
	 (fraction (if (eq (aref time 19) ?.)
		       (string-to-number (substring time 20 23))))
	 (timezone (substring time (if fraction 23 19))))
    ;; timezone is either Z (UTC) or [+-]HH:MM
    (let ((timezone-seconds
	   (if (string= timezone "Z")
	       0
	     (* (if (eq (aref timezone 0) ?+) 1 -1)
		(* 60 (+ (* 60 (string-to-number (substring timezone 1 3)))
			 (string-to-number (substring timezone 4 6))))))))
      (encode-time second minute hour day month year timezone-seconds))))

(defun jabber-report-success (xml-data context)
  "IQ callback reporting success or failure of the operation.
CONTEXT is a string describing the action."
  (let ((type (jabber-xml-get-attribute xml-data 'type)))
    (message (concat context
		     (if (string= type "result")
			 " succeeded"
		       (concat
			" failed: "
			(let ((the-error (jabber-iq-error xml-data)))
			  (if the-error
			      (jabber-parse-error the-error)
			    "No error message given"))))))))

(defconst jabber-error-messages
  (list
   (cons 'bad-request "Bad request")
   (cons 'conflict "Conflict")
   (cons 'feature-not-implemented "Feature not implemented")
   (cons 'forbidden "Forbidden")
   (cons 'gone "Gone")
   (cons 'internal-server-error "Internal server error")
   (cons 'item-not-found "Item not found")
   (cons 'jid-malformed "JID malformed")
   (cons 'not-acceptable "Not acceptable")
   (cons 'not-allowed "Not allowed")
   (cons 'not-authorized "Not authorized")
   (cons 'payment-required "Payment required")
   (cons 'recipient-unavailable "Recipient unavailable")
   (cons 'redirect "Redirect")
   (cons 'registration-required "Registration required")
   (cons 'remote-server-not-found "Remote server not found")
   (cons 'remote-server-timeout "Remote server timeout")
   (cons 'resource-constraint "Resource constraint")
   (cons 'service-unavailable "Service unavailable")
   (cons 'subscription-required "Subscription required")
   (cons 'undefined-condition "Undefined condition")
   (cons 'unexpected-request "Unexpected request"))
  "String descriptions of XMPP stanza errors")

(defconst jabber-legacy-error-messages
  (list
   (cons 302 "Redirect")
   (cons 400 "Bad request")
   (cons 401 "Unauthorized")
   (cons 402 "Payment required")
   (cons 403 "Forbidden")
   (cons 404 "Not found")
   (cons 405 "Not allowed")
   (cons 406 "Not acceptable")
   (cons 407 "Registration required")
   (cons 408 "Request timeout")
   (cons 409 "Conflict")
   (cons 500 "Internal server error")
   (cons 501 "Not implemented")
   (cons 502 "Remote server error")
   (cons 503 "Service unavailable")
   (cons 504 "Remote server timeout")
   (cons 510 "Disconnected"))
  "String descriptions of legacy errors (JEP-0086)")
  
(defun jabber-parse-error (error-xml)
  "Parse the given <error/> tag and return a string fit for human consumption.
See secton 9.3, Stanza Errors, of XMPP Core, and JEP-0086, Legacy Errors."
  (let ((error-type (jabber-xml-get-attribute error-xml 'type))
	(error-code (jabber-xml-get-attribute error-xml 'code))
	condition text)
    (if error-type
	;; If the <error/> tag has a type element, it is new-school.
	(dolist (child (jabber-xml-node-children error-xml))
	  (when (string=
		 (jabber-xml-get-attribute child 'xmlns)
		 "urn:ietf:params:xml:ns:xmpp-stanzas")
	    (if (eq (jabber-xml-node-name child) 'text)
		(setq text (car (jabber-xml-node-children child)))
	      (setq condition
		    (or (cdr (assq (jabber-xml-node-name child) jabber-error-messages))
			(symbol-name (jabber-xml-node-name child)))))))
      (setq condition (or (cdr (assq (string-to-number error-code) jabber-legacy-error-messages))
			  error-code))
      (setq text (car (jabber-xml-node-children error-xml))))
    (concat condition
	    (if text (format ": %s" text)))))

(defvar jabber-stream-error-messages
  (list
   (cons 'bad-format "Bad XML format")
   (cons 'bad-namespace-prefix "Bad namespace prefix")
   (cons 'conflict "Conflict")
   (cons 'connection-timeout "Connection timeout")
   (cons 'host-gone "Host gone")
   (cons 'host-unknown "Host unknown")
   (cons 'improper-addressing "Improper addressing") ; actually only s2s
   (cons 'internal-server-error "Internal server error")
   (cons 'invalid-from "Invalid from")
   (cons 'invalid-id "Invalid id")
   (cons 'invalid-namespace "Invalid namespace")
   (cons 'invalid-xml "Invalid XML")
   (cons 'not-authorized "Not authorized")
   (cons 'policy-violation "Policy violation")
   (cons 'remote-connection-failed "Remote connection failed")
   (cons 'resource-constraint "Resource constraint")
   (cons 'restricted-xml "Restricted XML")
   (cons 'see-other-host "See other host")
   (cons 'system-shutdown "System shutdown")
   (cons 'undefined-condition "Undefined condition")
   (cons 'unsupported-encoding "Unsupported encoding")
   (cons 'unsupported-stanza-type "Unsupported stanza type")
   (cons 'unsupported-version "Unsupported version")
   (cons 'xml-not-well-formed "XML not well formed"))
  "String descriptions of XMPP stream errors")

(defun jabber-parse-stream-error (error-xml)
  "Parse the given <stream:error/> tag and return a sting fit for human consumption."
  (let ((text-node (car (jabber-xml-get-children error-xml 'text)))
	condition)
    ;; as we don't know the node name of the condition, we have to
    ;; search for it.
    (dolist (node (jabber-xml-node-children error-xml))
      (when (and (string= (jabber-xml-get-attribute node 'xmlns) 
			  "urn:ietf:params:xml:ns:xmpp-streams")
		 (assq (jabber-xml-node-name node)
		       jabber-stream-error-messages))
	(setq condition (jabber-xml-node-name node))
	(return)))
    (concat (if condition (cdr (assq condition jabber-stream-error-messages))
	      "Unknown stream error")
	    (if (and text-node (stringp (car (jabber-xml-node-children text-node))))
		(concat ": " (car (jabber-xml-node-children text-node)))))))

(put 'jabber-error
     'error-conditions
     '(error jabber-error))
(put 'jabber-error
     'error-message
     "Jabber error")

(defun jabber-signal-error (error-type condition &optional text app-specific)
  "Signal an error to be sent by Jabber.
ERROR-TYPE is one of \"cancel\", \"continue\", \"modify\", \"auth\"
and \"wait\".
CONDITION is a symbol denoting a defined XMPP condition.
TEXT is a string to be sent in the error message, or nil for no text.
APP-SPECIFIC is a list of extra XML tags.

See section 9.3 of XMPP Core."
  (signal 'jabber-error
	  (list error-type condition text app-specific)))

(defun jabber-play-sound-file (soundfile)
  (if (not *jabber-sound-playing*)
      (progn
	(setq *jabber-sound-playing* t)
	(run-with-idle-timer 0.01 nil 
			     #'(lambda (sf)
			       (condition-case nil
				   ;; play-sound-file might display "Could not set sample rate" in
				   ;; echo area.  Don't let this erase the previous message.
				   (let ((old-message (current-message)))
				     (play-sound-file sf)
				     (setq *jabber-sound-playing* nil)
				     (message "%s" old-message))
				 (error (setq *jabber-sound-playing* nil))))
			     soundfile))))

(provide 'jabber-util)

;;; arch-tag: cfbb73ac-e2d7-4652-a08d-dc789bcded8a
