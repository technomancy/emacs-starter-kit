;; jabber-socks5.el - SOCKS5 bytestreams by JEP-0065

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

(require 'jabber-iq)
(require 'jabber-disco)
(require 'jabber-si-server)
(require 'jabber-si-client)

(defvar jabber-socks5-pending-sessions nil
  "List of pending sessions.

Each entry is a list, containing:
 * Stream ID
 * Full JID of initiator
 * Profile data function, to be called when data is received")

(defvar jabber-socks5-active-sessions nil
  "List of active sessions.

Each entry is a list, containing:
 * Network connection
 * Stream ID
 * Full JID of initiator
 * Profile data function")

(defcustom jabber-socks5-proxies nil
  "JIDs of JEP-0065 proxies to use for file transfer.
Put preferred ones first."
  :type '(repeat string)
  :group 'jabber
;  :set 'jabber-socks5-set-proxies)
  )

(defvar jabber-socks5-proxies-data nil
  "Alist containing information about proxies.
Keys of the alist are strings, the JIDs of the proxies.
Values are \"streamhost\" XML nodes.")

(add-to-list 'jabber-advertised-features "http://jabber.org/protocol/bytestreams")

(add-to-list 'jabber-si-stream-methods
	     (list "http://jabber.org/protocol/bytestreams"
		   'jabber-socks5-accept))

(add-to-list 'jabber-si-client-methods
	     (list "http://jabber.org/protocol/bytestreams"
		   'jabber-socks5-client-1))

(defun jabber-socks5-set-proxies (symbol value)
  "Set `jabber-socks5-proxies' and query proxies.
This is the set function of `jabber-socks5-proxies-data'."
  (set-default symbol value)
  (when *jabber-connected*
    (jabber-socks5-query-all-proxies)))

(defun jabber-socks5-query-all-proxies ()
  "Ask all proxies in `jabber-socks5-proxies' for connection information."
  (interactive)
  (setq jabber-socks5-proxies-data nil)
  (dolist (proxy jabber-socks5-proxies)
    (jabber-socks5-query-proxy proxy)))

(defun jabber-socks5-query-proxy (jid)
  "Query the SOCKS5 proxy specified by JID for IP and port number."
  (jabber-send-iq jid "get"
		  '(query ((xmlns . "http://jabber.org/protocol/bytestreams")))
		  #'jabber-socks5-process-proxy-response t
		  #'jabber-socks5-process-proxy-response nil))

(defun jabber-socks5-process-proxy-response (xml-data successp)
  "Process response from proxy query."
  (let* ((query (jabber-iq-query xml-data))
	 (from (jabber-xml-get-attribute xml-data 'from))
	 (streamhosts (jabber-xml-get-children query 'streamhost)))

    (let ((existing-entry (assoc from jabber-socks5-proxies-data)))
      (when existing-entry
	(setq jabber-socks5-proxies-data
	      (delq existing-entry jabber-socks5-proxies-data))))

    (when successp
      (setq jabber-socks5-proxies-data
	    (cons (cons from streamhosts)
		  jabber-socks5-proxies-data)))
    (message "%s from %s.  %d of %d proxies have answered."
	     (if successp "Response" "Error") from
	     (length jabber-socks5-proxies-data) (length jabber-socks5-proxies))))

(defun jabber-socks5-accept (jid sid profile-data-function)
  "Remember that we are waiting for connection from JID, with stream id SID"
  ;; asking the user for permission is done in the profile
  (add-to-list 'jabber-socks5-pending-sessions
	       (list sid jid profile-data-function)))

(add-to-list 'jabber-iq-set-xmlns-alist
	     (cons "http://jabber.org/protocol/bytestreams" 'jabber-socks5-process))
(defun jabber-socks5-process (xml-data)
  "Accept IQ get for SOCKS5 bytestream"
  (let* ((jid (jabber-xml-get-attribute xml-data 'from))
	 (id (jabber-xml-get-attribute xml-data 'id))
	 (query (jabber-iq-query xml-data))
	 (sid (jabber-xml-get-attribute query 'sid))
	 (session (dolist (pending-session jabber-socks5-pending-sessions)
		    (when (and (equal sid (nth 0 pending-session))
			       (equal jid (nth 1 pending-session)))
		      (return pending-session))))
	 (profile-data-function (nth 2 session)))
    ;; check that we really are expecting this session
    (unless session
      (jabber-signal-error "auth" 'not-acceptable))

    (setq jabber-socks5-pending-sessions (delq session jabber-socks5-pending-sessions))
    ;; find streamhost to connect to
    (let* ((streamhosts (jabber-xml-get-children query 'streamhost))
	   (streamhost (dolist (streamhost streamhosts)
			 (let ((connection (jabber-socks5-connect streamhost sid jid (concat jabber-username "@" jabber-server "/" jabber-resource))))
			   (when connection
			     ;; We select the first streamhost that we are able to connect to.
			     (push (list connection sid jid profile-data-function)
				   jabber-socks5-active-sessions)
			     ;; Now set the filter, for the rest of the output
			     (set-process-filter connection #'jabber-socks5-filter)
			     (set-process-sentinel connection #'jabber-socks5-sentinel)
			     (return streamhost))))))
      (unless streamhost
	(jabber-signal-error "cancel" 'item-not-found))
      
      ;; tell initiator which streamhost we use
      (jabber-send-iq jid "result"
		      `(query ((xmlns . "http://jabber.org/protocol/bytestreams"))
			      (streamhost-used ((jid . ,(jabber-xml-get-attribute streamhost 'jid)))))
		      nil nil nil nil id)
      ;; now, as data is sent, it will be passed to the profile.
      )))

(defun jabber-socks5-connect (streamhost sid initiator target)
  "Attempt to connect to STREAMHOST, authenticating with SID, INITIATOR and TARGET.
Return nil on error.  Return connection object on success.

STREAMHOST has the form
\(streamhost ((host . HOST)
	     (port . PORT)))

Zeroconf is not supported."
  (message "Attempting SOCKS5 connection to %s (%s->%s, %s)" streamhost initiator target sid)
  (condition-case e
      (let ((coding-system-for-read 'binary)
	    (coding-system-for-write 'binary)
	    (host (jabber-xml-get-attribute streamhost 'host))
	    (port (string-to-number (jabber-xml-get-attribute streamhost 'port))))
	;; is this the best way to send binary network output?
	(let ((socks5-connection (open-network-stream "socks5" (generate-new-buffer-name "socks5") host port)))
	  (with-current-buffer (process-buffer socks5-connection)
	    ;; version: 5.  number of auth methods supported: 1.
	    ;; which one: no authentication.
	    (process-send-string socks5-connection (string 5 1 0))
	    ;; wait for response
	    (accept-process-output socks5-connection 15)
	    ;; should return:
	    ;; version: 5.  auth method to use: none
	    (unless (string= (buffer-substring 1 3) (string 5 0))
	      (error "SOCKS5 authentication required"))

	    ;; send connect command
	    (let ((hash (sha1-string (concat sid initiator target))))
	      (process-send-string 
	       socks5-connection
	       (concat (string 5 1 0 3 (length hash))
		       hash
		       (string 0 0))))

	    (accept-process-output socks5-connection 15)
	    (unless (string= (buffer-substring 3 5) (string 5 0))
	      (error "SOCKS5 failure"))

	    (message "SOCKS5 connection established")

	    ;; The information returned here is exactly the same that we sent...
	    ;; Not very exciting.  Anyway, this part is done, we have a connection.
	    (let* ((address-type (aref (buffer-substring 6 7) 0))
		   (address-length (aref (buffer-substring 7 8) 0))
		   (address (buffer-substring 8 (+ 8 address-length)))
		   (address-port-string (buffer-substring (+ 8 address-length) (+ 8 address-length 2)))
		   (address-port (+
				  (* 256 (aref address-port-string 0))
				  (*   1 (aref address-port-string 1)))))
	      ;;(message "Address type: %d\nAddress: %s\nPort: %d" address-type address address-port)

	      ;; Delete all SOCKS5 data, leave room for the stream.
	      (delete-region 1 (+ 8 address-length 2)))

	    socks5-connection)))
    (error
     (message "SOCKS5 connection failed: %s" e)
     nil)))

(defun jabber-socks5-filter (connection data)
  "Pass data from connection to profile data function"
  (let* ((session (assq connection jabber-socks5-active-sessions))
	 (sid (nth 1 session))
	 (jid (nth 2 session))
	 (profile-data-function (nth 3 session)))
    ;; If the data function requests it, tear down the connection.
    (unless (funcall profile-data-function jid sid data)
      (jabber-socks5-sentinel connection nil))))

(defun jabber-socks5-sentinel (process event-string)
  ;; Connection terminated.  Shuffle together the remaining data,
  ;; and kill the buffer.
  (let* ((session (assq process jabber-socks5-active-sessions))
	 (buffer (process-buffer process))
	 (sid (nth 1 session))
	 (jid (nth 2 session))
	 (profile-data-function (nth 3 session)))
    (kill-buffer buffer)
    (delete-process process)
    (funcall profile-data-function jid sid nil)
    (setq jabber-socks5-active-sessions (delq session jabber-socks5-pending-sessions))))

(defun jabber-socks5-client-1 (jid sid profile-function)
  "Negotiate a SOCKS5 connection with JID.
This function simply sends a request; the response is handled elsewhere."
  ;; TODO: start our own server if we can.
  (unless jabber-socks5-proxies
    (error "No proxies defined.  Set `jabber-socks5-proxies'."))
  (unless jabber-socks5-proxies-data
    (error "No proxy data available.  Run `jabber-socks5-query-all-proxies'."))

  ;; Sort the alist jabber-socks5-proxies-data such that the
  ;; keys are in the same order as in jabber-socks5-proxies.
  (setq jabber-socks5-proxies-data
	(sort jabber-socks5-proxies-data
	      #'(lambda (a b)
		  (> (length (member (car a) jabber-socks5-proxies))
		     (length (member (car b) jabber-socks5-proxies))))))

  (jabber-send-iq jid "set"
		  `(query ((xmlns . "http://jabber.org/protocol/bytestreams")
			   (sid . ,sid))
			  ,@(mapcar 
			     #'(lambda (proxy)
				 (mapcar
				  #'(lambda (streamhost)
				      (list 'streamhost
					    (list (cons 'jid (jabber-xml-get-attribute streamhost 'jid))
						  (cons 'host (jabber-xml-get-attribute streamhost 'host))
						  (cons 'port (jabber-xml-get-attribute streamhost 'port)))))
				  (cdr proxy)))
			     jabber-socks5-proxies-data))
		  `(lambda (xml-data closure-data)
		     (jabber-socks5-client-2 xml-data ,jid ,sid ,profile-function)) nil
		  ;; TODO: error handling
		  #'jabber-report-success "SOCKS5 negotiation"))

(defun jabber-socks5-client-2 (xml-data jid sid profile-function)
  "Contact has selected a streamhost to use.  Connect to the proxy."
  (let* ((query (jabber-iq-query xml-data))
	 (streamhost-used (car (jabber-xml-get-children query 'streamhost-used)))
	 (proxy-used (jabber-xml-get-attribute streamhost-used 'jid))
	 connection)
    (let ((streamhosts-left (cdr (assoc proxy-used jabber-socks5-proxies-data))))
      (while (and streamhosts-left (not connection))
	(setq connection
	      (jabber-socks5-connect (car streamhosts-left)
				     sid
				     (concat jabber-username "@" jabber-server "/" jabber-resource)
				     jid))
	(setq streamhosts-left (cdr streamhosts-left))))
    (unless connection
      (error "Couldn't connect to proxy %s" proxy-used))

    ;; Activation is only needed for proxies.
    (jabber-send-iq proxy-used "set"
		    `(query ((xmlns . "http://jabber.org/protocol/bytestreams")
			     (sid . ,sid))
			    (activate () ,jid))
		    `(lambda (xml-data closure-data)
		       (jabber-socks5-client-3 xml-data ,jid ,sid ,profile-function ,connection)) nil
		       ;; TODO: report error to contact?
		       #'jabber-report-success "Proxy activation")))

(defun jabber-socks5-client-3 (xml-data jid sid profile-function proxy-connection)
  "Proxy is activated.  Start the transfer."
  ;; The response from the proxy does not contain any interesting
  ;; information, beyond success confirmation.

  (funcall profile-function jid sid `(lambda (data)
				       (process-send-string ,proxy-connection data))))

(provide 'jabber-socks5)

;;; arch-tag: 9e70dfea-2522-40c6-a79f-302c8fb82ac5
