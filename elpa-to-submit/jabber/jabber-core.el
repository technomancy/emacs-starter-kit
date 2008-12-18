;; jabber-core.el - core functions

;; Copyright (C) 2002, 2003, 2004 - tom berger - object@intelectronica.net
;; Copyright (C) 2003, 2004 - Magnus Henoch - mange@freemail.hu

;; SSL-Connection Parts:
;; Copyright (C) 2005 - Georg Lehner - jorge@magma.com.ni

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

(eval-when-compile (require 'cl))

(require 'jabber-util)
(require 'jabber-logon)
(require 'jabber-conn)

;; SASL depends on FLIM.
(eval-and-compile
  (condition-case nil
      (require 'jabber-sasl)
    (error nil)))

(defvar *jabber-roster* nil
  "the roster list")

(defvar jabber-jid-obarray (make-vector 127 0)
  "obarray for keeping JIDs")

(defvar *jabber-connected* nil
  "boolean - are we connected")

(defvar *jabber-authenticated* nil
  "boolean - are we authenticated")

(defvar *jabber-disconnecting* nil
  "boolean - are we in the process of disconnecting by free will")

(defvar *xmlq* ""
  "a string with all the incoming xml that is waiting to be parsed")

(defvar jabber-register-p nil
  "Register a new account in this session?")

(defvar jabber-session-id nil
  "id of the current session")

(defvar jabber-stream-version nil
  "Stream version indicated by server")

(defvar jabber-register-p nil
  "Is account registration occurring in this session?")

(defvar jabber-call-on-connection nil
  "Function to be called on connection.
This is set by `jabber-connect' on each call, and later picked up in
`jabber-filter'.")

(defvar jabber-short-circuit-input nil
  "Function that receives all stanzas, instead of the usual ones.
Used for SASL authentication.")

(defvar jabber-message-chain nil
  "Incoming messages are sent to these functions, in order.")

(defvar jabber-iq-chain nil
  "Incoming infoqueries are sent to these functions, in order.")

(defvar jabber-presence-chain nil
  "Incoming presence notifications are sent to these functions, in order.")

(defvar jabber-stream-error-chain '(jabber-process-stream-error)
  "Stream errors are sent to these functions, in order")

(defvar jabber-choked-count 0
  "Number of successive times that the process buffer has been nonempty.")

(defvar jabber-choked-timer nil)

(defgroup jabber-core nil "customize core functionality"
  :group 'jabber)

(defcustom jabber-post-connect-hook '(jabber-send-default-presence
				      jabber-muc-autojoin)
  "*Hooks run after successful connection and authentication."
  :type 'hook
  :group 'jabber-core)

(defcustom jabber-pre-disconnect-hook nil
  "*Hooks run just before voluntary disconnection
This might be due to failed authentication.  Check `*jabber-authenticated*'."
  :type 'hook
  :group 'jabber-core)

(defcustom jabber-lost-connection-hook nil
  "*Hooks run after involuntary disconnection"
  :type 'hook
  :group 'jabber-core)

(defcustom jabber-post-disconnect-hook nil
  "*Hooks run after disconnection"
  :type 'hook
  :group 'jabber-core)

(defcustom jabber-roster-buffer "*-jabber-*"
  "The name of the roster buffer"
  :type 'string
  :group 'jabber-core)

(defvar jabber-process-buffer " *-jabber-process-*"
  "The name of the process buffer")

(defcustom jabber-use-sasl t
  "If non-nil, use SASL if possible.
SASL will still not be used if the library for it is missing or
if the server doesn't support it.

Disabling this shouldn't be necessary, but it may solve certain
problems."
  :type 'boolean
  :group 'jabber-core)

(defsubst jabber-have-sasl-p ()
  "Return non-nil if SASL functions are available."
  (fboundp 'jabber-sasl-start-auth))

(defun jabber-connect (&optional registerp)
  "connect to the jabber server and start a jabber xml stream
With prefix argument, register a new account."
  (interactive "P")
  (if *jabber-connected*
      (message "Already connected")
    (setq *xmlq* "")
    (setq *jabber-authenticated* nil)
    (jabber-clear-roster)
    (jabber-reset-choked)

    ;; Call the function responsible for establishing a bidirectional
    ;; data stream  to the Jabber Server, *jabber-connection* is set
    ;; afterwards.
    (jabber-setup-connect-method)
    (funcall jabber-connect-function)
    (unless *jabber-connection*
      (error "Connection failed"))

    ;; TLS connections leave data in the process buffer, which
    ;; the XML parser will choke on.
    (with-current-buffer (process-buffer *jabber-connection*)
      (erase-buffer))
    (set-process-filter *jabber-connection* #'jabber-pre-filter)
    (set-process-sentinel *jabber-connection* #'jabber-sentinel)

    (setq jabber-short-circuit-input nil)
    (setq jabber-register-p registerp)

    (setq jabber-call-on-connection (if registerp
					#'(lambda (stream-features) (jabber-get-register jabber-server))
				      #'jabber-auth-somehow))
    (let ((stream-header (concat "<?xml version='1.0'?><stream:stream to='" 
				 jabber-server 
				 "' xmlns='jabber:client' xmlns:stream='http://etherx.jabber.org/streams'"
				 ;; Not supporting SASL is not XMPP compliant,
				 ;; so don't pretend we are.
				 (if (and (jabber-have-sasl-p) jabber-use-sasl)
				     " version='1.0'"
				   "")
				 ">
")))

      (funcall jabber-conn-send-function stream-header)
      (if jabber-debug-log-xml
	  (with-current-buffer (get-buffer-create "*-jabber-xml-log-*")
	    (save-excursion
	      (goto-char (point-max))
	      (insert (format "sending %S\n\n" stream-header)))))

      (setq jabber-choked-timer
	    (run-with-timer 5 5 #'jabber-check-choked))

      (accept-process-output *jabber-connection*))
    ;; Next thing happening is the server sending its own <stream:stream> start tag.
    ;; That is handled in jabber-filter.

    (setq *jabber-connected* t)))

(defun jabber-auth-somehow (stream-features)
  "Start authentication with SASL if the server supports it,
otherwise JEP-0077.  The STREAM-FEATURES argument is the stream features
tag, or nil if we're connecting to a pre-XMPP server."
  (if (and stream-features
	   jabber-use-sasl
	   (jabber-have-sasl-p)
	   jabber-stream-version
	   (>= (string-to-number jabber-stream-version) 1.0))
      (jabber-sasl-start-auth stream-features)
    (jabber-get-auth jabber-server)))

(defun jabber-disconnect ()
  "disconnect from the jabber server and re-initialise the jabber package variables"
  (interactive)
  (unless *jabber-disconnecting*	; avoid reentry
    (let ((*jabber-disconnecting* t))
      (when (and *jabber-connection*
		 (memq (process-status *jabber-connection*) '(open run)))
	(run-hooks 'jabber-pre-disconnect-hook)
	(funcall jabber-conn-send-function "</stream:stream>")
	;; let the server close the stream
	(accept-process-output *jabber-connection* 3)
	;; and do it ourselves as well, just to be sure
	(delete-process *jabber-connection*))
      (jabber-disconnected)
      (if (interactive-p)
	  (message "Disconnected from Jabber server")))))

(defun jabber-disconnected ()
  "Re-initialise jabber package variables.
Call this function after disconnection."
  (when jabber-choked-timer
    (jabber-cancel-timer jabber-choked-timer)
    (setq jabber-choked-timer nil))

  (when (get-buffer jabber-roster-buffer)
    (with-current-buffer (get-buffer jabber-roster-buffer)
      (let ((inhibit-read-only t))
	(erase-buffer))))

  (setq *jabber-connection* nil)
  (jabber-clear-roster)
  (setq *xmlq* "")
  (setq *jabber-authenticated* nil)
  (setq *jabber-connected* nil)
  (setq *jabber-active-groupchats* nil)
  (setq jabber-session-id nil)
  (run-hooks 'jabber-post-disconnect-hook))

(defun jabber-sentinel (process event)
  "alert user about lost connection"
  (unless (or *jabber-disconnecting* (not *jabber-connected*))
    (beep)
    (run-hooks 'jabber-lost-connection-hook)
    (message "Jabber connection lost: `%s'" event)
    ;; If there is data left (maybe a stream error) process it first
    (with-current-buffer (process-buffer process)
      (unless (zerop (buffer-size))
	(jabber-filter process)))
    (jabber-disconnected)))

(defun jabber-pre-filter (process string)
  (with-current-buffer (process-buffer process)
    ;; Append new data
    (goto-char (point-max))
    (insert string)

    (unless (boundp 'jabber-filtering)
      (let (jabber-filtering)
	(jabber-filter process)))))

(defun jabber-filter (process)
  "the filter function for the jabber process"
  (with-current-buffer (process-buffer process)
    ;; Start from the beginning
    (goto-char (point-min))
    (let (xml-data)
      (loop 
       do
       ;; Skip whitespace
       (unless (zerop (skip-chars-forward " \t\r\n"))
	 (delete-region (point-min) (point)))
       ;; Skip processing directive
       (when (looking-at "<\\?xml[^?]*\\?>")
	 (delete-region (match-beginning 0) (match-end 0)))

       ;; Stream end?
       (when (looking-at "</stream:stream>")
	 (return (jabber-disconnect)))

       ;; Stream header?
       (when (looking-at "<stream:stream[^>]*>")
	 (let ((stream-header (match-string 0))
	       (ending-at (match-end 0)))
	   ;; These regexps extract attribute values from the stream
	   ;; header, taking into account that the quotes may be either
	   ;; single or double quotes.
	   (setq jabber-session-id
		 (and (or (string-match "id='\\([^']+\\)'" stream-header)
			  (string-match "id=\"\\([^\"]+\\)\"" stream-header))
		      (jabber-unescape-xml (match-string 1 stream-header))))
	   (setq jabber-stream-version
		 (and (or
		       (string-match "version='\\([0-9.]+\\)'" stream-header)
		       (string-match "version=\"\\([0-9.]+\\)\"" stream-header))
		      (match-string 1 stream-header)))
	   (if jabber-debug-log-xml
	       (with-current-buffer (get-buffer-create "*-jabber-xml-log-*")
		 (save-excursion
		   (goto-char (point-max))
		   (insert (format "receive %S\n\n" stream-header)))))

	   ;; If the server is XMPP compliant, i.e. there is a version attribute
	   ;; and it's >= 1.0, there will be a stream:features tag shortly,
	   ;; so just wait for that.

	   ;; the stream feature is only sent if the initiating entity has
	   ;; sent 1.0 in the stream header. if sasl is not supported then
	   ;; we don't send 1.0 in the header and therefore we shouldn't wait
	   ;; even if 1.0 is present in the receiving stream.
	   (unless (and jabber-stream-version
			(>= (string-to-number jabber-stream-version) 1.0)
			jabber-use-sasl
			(jabber-have-sasl-p))
	     ;; Logon or register
	     (funcall jabber-call-on-connection nil))
	 
	   (delete-region (point-min) ending-at)))
       
       ;; Normal tag

       ;; XXX: do these checks make sense?  If so, reinstate them.
       ;;(if (active-minibuffer-window)
       ;;    (run-with-idle-timer 0.01 nil #'jabber-filter process string)

       ;; This check is needed for xml.el of Emacs 21, as it chokes on
       ;; empty attribute values.
       (save-excursion
	 (while (search-forward-regexp " \\w+=''" nil t)
           (replace-match "")))
       
       (setq xml-data (and (catch 'unfinished
			     (jabber-xml-skip-tag-forward)
			     (> (point) (point-min)))
			   (xml-parse-region (point-min) (point))))
       (if xml-data
	   (jabber-reset-choked))

       while xml-data
       do
       ;; If there's a problem with writing the XML log,
       ;; make sure the stanza is delivered, at least.
       (condition-case e
	   (if jabber-debug-log-xml
	       (with-current-buffer (get-buffer-create "*-jabber-xml-log-*")
		 (save-excursion
		   (goto-char (point-max))
		   (insert (format "receive %S\n\n" (car xml-data))))))
	 (error
	  (ding)
	  (message "Couldn't write XML log: %s" (error-message-string e))
	  (sit-for 2)))
       (delete-region (point-min) (point))
       ;; We explicitly don't catch errors in jabber-process-input,
       ;; to facilitate debugging.
       (jabber-process-input (car xml-data))))))

(defun jabber-reset-choked ()
  (setq jabber-choked-count 0))

(defun jabber-check-choked ()
  ;; "Choked" means that data is sitting in the process buffer
  ;; without being parsed, despite several attempts.
  (if (zerop (buffer-size (process-buffer *jabber-connection*)))
      (jabber-reset-choked)
    (incf jabber-choked-count)
    (if (and (> jabber-choked-count 3)
	     ;; Now we're definitely choked.  Take action.
	     ;; But ask user first.
	     (yes-or-no-p "jabber.el is severely confused.  Bail out? "))
	(run-with-idle-timer 0.1 nil 'jabber-choked-bail-out)
      (jabber-reset-choked))))

(defun jabber-choked-bail-out ()
  ;; So here we are.  Something in the process buffer prevents us
  ;; from continuing normally.  Let's die honorably by providing
  ;; bug report material.
  (with-current-buffer (generate-new-buffer "*jabber-bug*")
    (insert "jabber.el couldn't cope with the data received from the server.
This should never happen, but apparently it did.

The information below will be helpful in tracking down and fixing
the bug.  You may want to edit out any sensitive information.

Please go to
http://sourceforge.net/tracker/?group_id=88346&atid=586350 and
submit a bug report, including the information below.

")
    (goto-address)
    (emacs-version t)
    (insert "\n\nThe following couldn't be parsed:\n")
    (insert-buffer-substring (process-buffer *jabber-connection*))
    (switch-to-buffer (current-buffer)))
  (jabber-disconnect))

(defun jabber-process-input (xml-data)
  "process an incoming parsed tag"
  (let* ((tag (jabber-xml-node-name xml-data))
	 (functions (eval (cdr (assq tag '((iq . jabber-iq-chain)
					   (presence . jabber-presence-chain)
					   (message . jabber-message-chain)
					   (stream:error . jabber-stream-error-chain)))))))
    ;; Special treatment of the stream:features tag.  The first time we get it,
    ;; it means that we should authenticate.  The second time, we should
    ;; establish a session.  (The zeroth time it's STARTTLS, but that's not
    ;; implemented yet.)
    (if (eq tag 'stream:features)
	(if *jabber-authenticated*
	    (jabber-bind-and-establish-session xml-data)
	  (funcall jabber-call-on-connection xml-data))
      (if jabber-short-circuit-input
	  (funcall jabber-short-circuit-input xml-data)
	(dolist (f functions)
	  (funcall f xml-data))))))

(defun jabber-process-stream-error (xml-data)
  "Process an incoming stream error."
  (beep)
  (run-hooks 'jabber-lost-connection-hook)
  (message "Stream error, connection lost: %s" (jabber-parse-stream-error xml-data))
  (jabber-disconnect))

(defun jabber-bind-and-establish-session (xml-data)
  ;; Now we have a stream:features tag.  We expect it to contain bind and
  ;; session tags.  If it doesn't, the server we are connecting to is no
  ;; IM server.
  (unless (and (jabber-xml-get-children xml-data 'bind)
	       (jabber-xml-get-children xml-data 'session))
    (jabber-disconnect)
    (error "Server doesn't permit resource binding and session establishing"))

  ;; So let's bind a resource.  We can either pick a resource ourselves,
  ;; or have the server pick one for us.
  (jabber-send-iq nil "set"
		  `(bind ((xmlns . "urn:ietf:params:xml:ns:xmpp-bind"))
			 (resource () ,jabber-resource))
		  #'jabber-process-bind t
		  #'jabber-process-bind nil))

(defun jabber-process-bind (xml-data successp)
  (unless successp
    (jabber-disconnect)
    (error "Resource binding failed: %s" 
	   (jabber-parse-error (car (jabber-xml-get-children xml-data 'error)))))

  (let ((jid (car
	      (jabber-xml-node-children
	       (car
		(jabber-xml-get-children
		 (jabber-iq-query xml-data) 'jid))))))
    ;; Maybe this isn't the JID we asked for.
    (setq jabber-username (jabber-jid-username jid))
    (setq jabber-server (jabber-jid-server jid))
    (setq jabber-resource (jabber-jid-resource jid)))

  ;; Been there, done that.  Time to establish a session.
  (jabber-send-iq nil "set"
		  '(session ((xmlns . "urn:ietf:params:xml:ns:xmpp-session")))
		  #'jabber-process-session t
		  #'jabber-process-session nil))

(defun jabber-process-session (xml-data successp)
  (unless successp
    (jabber-disconnect)
    (error "Session establishing failed: %s" 
	   (jabber-parse-error (car (jabber-xml-get-children xml-data 'error)))))

  ;; Now, request roster.
  (jabber-send-iq nil
		  "get" 
		  '(query ((xmlns . "jabber:iq:roster")))
		  #'jabber-process-roster 'initial
		  #'jabber-report-success "Roster retrieval")

  (run-hooks 'jabber-post-connect-hook))

(defun jabber-clear-roster ()
  "Clean up the roster."
  ;; This is made complicated by the fact that the JIDs are symbols with properties.
  (mapatoms #'(lambda (x)
		(unintern x jabber-jid-obarray))
	    jabber-jid-obarray)
  (setq *jabber-roster* nil))

(defun jabber-send-sexp (sexp)
  "send the xml corresponding to SEXP to the jabber server"
  (condition-case e
      (if jabber-debug-log-xml
	  (with-current-buffer (get-buffer-create "*-jabber-xml-log-*")
	    (save-excursion
	      (goto-char (point-max))
	      (insert (format "sending %S\n\n" sexp)))))
    (error
     (ding)
     (message "Couldn't write XML log: %s" (error-message-string e))
     (sit-for 2)))
  (funcall jabber-conn-send-function (jabber-sexp2xml sexp)))

(provide 'jabber-core)

;;; arch-tag: 9d273ce6-c45a-447b-abf3-21d3ce73a51a
