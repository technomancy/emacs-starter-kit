;; jabber-chat.el - one-to-one chats

;; Copyright (C) 2005 - Magnus Henoch - mange@freemail.hu

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
(require 'jabber-chatbuffer)
(require 'jabber-history)

(defgroup jabber-chat nil "chat display options"
  :group 'jabber)

(defcustom jabber-chat-buffer-format "*-jabber-chat-%n-*"
  "The format specification for the name of chat buffers.

These fields are available (all are about the person you are chatting
with):

%n   Nickname, or JID if no nickname set
%j   Bare JID (without resource)
%r   Resource"
  :type 'string
  :group 'jabber-chat)

(defcustom jabber-chat-header-line-format
  '(" " (:eval (jabber-jid-displayname jabber-chatting-with))
    "\t" (:eval (let ((buddy (jabber-jid-symbol jabber-chatting-with)))
		  (propertize 
		   (or
		    (cdr (assoc (get buddy 'show) jabber-presence-strings))
		    (get buddy 'show))
		   'face
		   (or (cdr (assoc (get buddy 'show) jabber-presence-faces))
		       'jabber-roster-user-online))))
    "\t" (:eval (get (jabber-jid-symbol jabber-chatting-with) 'status))
    "\t" jabber-events-message)		;see jabber-events.el
  "The specification for the header line of chat buffers.

The format is that of `mode-line-format' and `header-line-format'."
  :type 'sexp
  :group 'jabber-chat)

(defcustom jabber-chat-time-format "%H:%M"
  "The format specification for instant messages in the chat buffer.
See also `jabber-chat-delayed-time-format'.

See `format-time-string' for valid values."
  :type 'string
  :group 'jabber-chat)

(defcustom jabber-chat-delayed-time-format "%Y-%m-%d %H:%M"
  "The format specification for delayed messages in the chat buffer.
See also `jabber-chat-time-format'.

See `format-time-string' for valid values."
  :type 'string
  :group 'jabber-chat)

(defcustom jabber-print-rare-time t
  "Non-nil means to print \"rare time\" indications in chat buffers.
The default settings tell every new hour."
  :type 'boolean
  :group 'jabber-chat)

(defcustom jabber-rare-time-format "%a %e %b %Y %H:00"
  "The format specification for the rare time information.
Rare time information will be printed whenever the current time,
formatted according to this string, is different to the last
rare time printed."
  :type 'string
  :group 'jabber-chat)

(defface jabber-rare-time-face
  '((t (:foreground "darkgreen" :underline t)))
  "face for displaying the rare time info"
  :group 'jabber-chat)

(defvar jabber-rare-time ""
  "Latest rare time printed")
(make-variable-buffer-local 'jabber-rare-time)

(defcustom jabber-chat-local-prompt-format "[%t] %n> "
  "The format specification for lines you type in the chat buffer.

These fields are available:

%t   Time, formatted according to `jabber-chat-time-format'
     or `jabber-chat-delayed-time-format'
%n   Nickname (`jabber-nickname')
%u   Username
%r   Resource
%j   Bare JID (without resource)"
  :type 'string
  :group 'jabber-chat)

(defcustom jabber-chat-foreign-prompt-format "[%t] %n> "
  "The format specification for lines others type in the chat buffer.

These fields are available:

%t   Time, formatted according to `jabber-chat-time-format'
     or `jabber-chat-delayed-time-format'
%n   Nickname, or JID if no nickname set
%u   Username
%r   Resource
%j   Bare JID (without resource)"
  :type 'string
  :group 'jabber-chat)

(defcustom jabber-chat-system-prompt-format "[%t] *** "
  "The format specification for lines from the system or that are special in the chat buffer."
  :type 'string
  :group 'jabber-chat)

(defface jabber-chat-prompt-local
  '((t (:foreground "blue" :weight bold)))
  "face for displaying the chat prompt for what you type in"
  :group 'jabber-chat)

(defface jabber-chat-prompt-foreign
  '((t (:foreground "red" :weight bold)))
  "face for displaying the chat prompt for what they send"
  :group 'jabber-chat)

(defface jabber-chat-prompt-system
  '((t (:foreground "green" :weight bold)))
  "face used for system and special messages"
  :group 'jabber-chat)

(defface jabber-chat-text-local nil
  "Face used for text you write"
  :group 'jabber-chat)

(defface jabber-chat-text-foreign nil
  "Face used for text others write"
  :group 'jabber-chat)

(defface jabber-chat-error
  '((t (:foreground "red" :weight bold)))
  "Face used for error messages"
  :group 'jabber-chat)

(defvar jabber-chatting-with nil
  "JID of the person you are chatting with")

(defvar jabber-chat-printers '(jabber-chat-print-subject
			       jabber-chat-print-body
			       jabber-chat-print-url
			       jabber-chat-goto-address)
  "List of functions that may be able to print part of a message.
Each function receives the entire <message/> stanza as argument.")

(defvar jabber-body-printers '(jabber-chat-normal-body)
  "List of functions that may be able to print a body for a message.
Each function receives the entire <message/> stanza as argument, and
should either output a representation of the body part of the message
and return non-nil, or output nothing and return nil.  These functions
are called in order, until one of them returns non-nil.

Add a function to the beginning of this list if the tag it handles
replaces the contents of the <body/> tag.")

(defvar jabber-chat-send-hooks nil
  "List of functions called when a chat message is sent.
The arguments are the text to send, and the id attribute of the
message.

The functions should return a list of XML nodes they want to be
added to the outgoing message.")

(defvar jabber-chat-earliest-backlog nil
  "Float-time of earliest backlog entry inserted into buffer.
nil if no backlog has been inserted.")

(defun jabber-chat-get-buffer (chat-with)
  "Return the chat buffer for chatting with CHAT-WITH (bare or full JID).
Either a string or a buffer is returned, so use `get-buffer' or
`get-buffer-create'."
  (format-spec jabber-chat-buffer-format
	       (list
		(cons ?n (jabber-jid-displayname chat-with))
		(cons ?j (jabber-jid-user chat-with))
		(cons ?r (jabber-jid-resource chat-with)))))

(defun jabber-chat-create-buffer (chat-with)
  "Prepare a buffer for chatting with CHAT-WITH.
This function is idempotent."
  (with-current-buffer (get-buffer-create (jabber-chat-get-buffer chat-with))
    (if (not (eq major-mode 'jabber-chat-mode)) (jabber-chat-mode))
    (make-local-variable 'jabber-chatting-with)
    (setq jabber-chatting-with chat-with)
    (setq jabber-send-function 'jabber-chat-send)
    (setq header-line-format jabber-chat-header-line-format)

    (make-local-variable 'jabber-chat-earliest-backlog)

    ;; insert backlog
    (when (zerop (buffer-size))
      (let ((backlog-entries (jabber-history-backlog chat-with)))
	(when backlog-entries
	  (setq jabber-chat-earliest-backlog 
		(jabber-float-time (jabber-parse-time
				    (aref (car backlog-entries) 0))))
	  (mapc 'jabber-chat-insert-backlog-entry backlog-entries))))
    
    (current-buffer)))

(defun jabber-chat-insert-backlog-entry (msg)
  "Insert backlog entry MSG at point."
  (if (string= (aref msg 1) "in")
      (let ((fake-stanza `(message ((from . ,(aref msg 2)))
				   (body nil ,(aref msg 4))
				   (x ((xmlns . "jabber:x:delay")
				       (stamp . ,(jabber-encode-legacy-time (jabber-parse-time (aref msg 0)))))))))
	(jabber-chat-buffer-display-at-point 'jabber-chat-print-prompt
					     fake-stanza
					     jabber-chat-printers
					     fake-stanza))
    (jabber-chat-buffer-display-at-point 'jabber-chat-self-prompt
					 (jabber-parse-time (aref msg 0))
					 '(insert)
					 (jabber-propertize
					  (aref msg 4)
					  'face 'jabber-chat-text-local))))

(add-to-list 'jabber-jid-chat-menu
	     (cons "Display more context" 'jabber-chat-display-more-backlog))

(defun jabber-chat-display-more-backlog (how-many)
  (interactive "nHow many more messages? ")
  (let* ((inhibit-read-only t)
	 (jabber-backlog-days nil)
	 (jabber-backlog-number how-many)
	 (backlog-entries (jabber-history-backlog
			   jabber-chatting-with jabber-chat-earliest-backlog)))
    (when backlog-entries
      (setq jabber-chat-earliest-backlog 
	    (jabber-float-time (jabber-parse-time
				(aref (car backlog-entries) 0))))
      (save-excursion
	(goto-char (point-min))
	(mapc 'jabber-chat-insert-backlog-entry backlog-entries)))))

(add-to-list 'jabber-message-chain 'jabber-process-chat)

(defun jabber-process-chat (xml-data)
  "If XML-DATA is a one-to-one chat message, handle it as such."
  ;; XXX: there's more to being a chat message than not being MUC.
  ;; Maybe make independent predicate.
  (when (not (jabber-muc-message-p xml-data))
    ;; Note that we handle private MUC messages here.
    (let ((from (jabber-xml-get-attribute xml-data 'from))
	  (error-p (jabber-xml-get-children xml-data 'error))
	  (body-text (car (jabber-xml-node-children
			   (car (jabber-xml-get-children
				 xml-data 'body))))))
      (with-current-buffer (if (jabber-muc-sender-p from)
			       (jabber-muc-private-create-buffer
				(jabber-jid-user from)
				(jabber-jid-resource from))
			     (jabber-chat-create-buffer from))
	;; Call alert hooks only when something is output
	(when
	    (jabber-chat-buffer-display (if (jabber-muc-sender-p from)
					    'jabber-muc-private-print-prompt
					  'jabber-chat-print-prompt)
					xml-data
					(if error-p
					    '(jabber-chat-print-error)
					  jabber-chat-printers)
					xml-data)

	  (dolist (hook '(jabber-message-hooks jabber-alert-message-hooks))
	    (run-hook-with-args hook
				from (current-buffer) body-text
				(funcall jabber-alert-message-function 
					 from (current-buffer) body-text))))))))

(defun jabber-chat-send (body)
  "Send BODY, and display it in chat buffer."
  (let* ((id (apply 'format "emacs-msg-%d.%d.%d" (current-time)))
	 (stanza-to-send `(message 
			   ((to . ,jabber-chatting-with)
			    (type . "chat")
			    (id . ,id))
			   (body () ,(jabber-escape-xml body)))))
    (dolist (hook jabber-chat-send-hooks)
      (nconc stanza-to-send (funcall hook body id)))
    (jabber-send-sexp stanza-to-send))

  ;; Note that we pass a string, not an XML stanza,
  ;; to the print functions.
  (jabber-chat-buffer-display 'jabber-chat-self-prompt
			      nil
			      '(insert)
			      (jabber-propertize
			       body
			       'face 'jabber-chat-text-local)))

(defun jabber-maybe-print-rare-time (timestamp)
  "Print rare time, if changed since last time printed."
  (let ((new-time (format-time-string jabber-rare-time-format timestamp)))
    (unless (string= new-time jabber-rare-time)
      (setq jabber-rare-time new-time)
      (when jabber-print-rare-time
	(let ((inhibit-read-only t))
	  (goto-char jabber-point-insert)
	  (insert (jabber-propertize jabber-rare-time 'face 'jabber-rare-time-face) "\n")
	  (setq jabber-point-insert (point)))))))

(defun jabber-chat-print-prompt (xml-data)
  "Print prompt for received message in XML-DATA."
  (let ((from (jabber-xml-get-attribute xml-data 'from))
	(timestamp (car (delq nil (mapcar 'jabber-x-delay (jabber-xml-get-children xml-data 'x))))))
    (jabber-maybe-print-rare-time timestamp)
    (insert (jabber-propertize 
	     (format-spec jabber-chat-foreign-prompt-format
			  (list
			   (cons ?t (format-time-string 
				     (if timestamp
					 jabber-chat-delayed-time-format
				       jabber-chat-time-format)
				     timestamp))
			   (cons ?n (jabber-jid-displayname from))
			   (cons ?u (or (jabber-jid-username from) from))
			   (cons ?r (jabber-jid-resource from))
			   (cons ?j (jabber-jid-user from))))
	     'face 'jabber-chat-prompt-foreign
	     'help-echo
	     (concat (format-time-string "On %Y-%m-%d %H:%M:%S" timestamp) " from " from)))))

(defun jabber-chat-self-prompt (timestamp)
  "Print prompt for sent message.
TIMESTAMP is the timestamp to print, or nil for now."
  (jabber-maybe-print-rare-time timestamp)
  (insert (jabber-propertize 
	   (format-spec jabber-chat-local-prompt-format
			(list
			 (cons ?t (format-time-string 
				   (if timestamp
				       jabber-chat-delayed-time-format
				     jabber-chat-time-format)
				   timestamp))
			 (cons ?n jabber-nickname)
			 (cons ?u jabber-username)
			 (cons ?r jabber-resource)
			 (cons ?j (concat jabber-username "@" jabber-server))))
	   'face 'jabber-chat-prompt-local
	   'help-echo
	   (concat (format-time-string "On %Y-%m-%d %H:%M:%S" timestamp) " from you"))))

(defun jabber-chat-print-error (xml-data)
  "Print error in given <message/> in a readable way."
  (let ((the-error (car (jabber-xml-get-children xml-data 'error))))
    (insert 
     (jabber-propertize
      (concat "Error: " (jabber-parse-error the-error))
      'face 'jabber-chat-error))))

(defun jabber-chat-print-subject (xml-data)
  "Print subject of given <message/>, if any."
  (let ((subject (car
		  (jabber-xml-node-children
		   (car
		    (jabber-xml-get-children xml-data 'subject))))))
    (when (not (zerop (length subject)))
      (insert (jabber-propertize
	       "Subject: " 'face 'jabber-chat-prompt-system)
	      (jabber-propertize
	       subject
	       'face 'jabber-chat-text-foreign)
	      "\n"))))

(defun jabber-chat-print-body (xml-data)
  (run-hook-with-args-until-success 'jabber-body-printers xml-data))

(defun jabber-chat-normal-body (xml-data)
  "Print body for received message in XML-DATA."
  (let ((body (car
	       (jabber-xml-node-children
		(car
		 (jabber-xml-get-children xml-data 'body))))))
    (when body
      (if (string-match "^/me \\(.*\\)$" body)
	  (let ((action (match-string 1 body))
		(nick (if (jabber-muc-message-p xml-data)
			  (jabber-jid-resource (jabber-xml-get-attribute xml-data 'from))
			(jabber-jid-displayname (jabber-xml-get-attribute xml-data 'from)))))
	    (insert (jabber-propertize
		     (concat nick
			     " "
			     action)
		     'face 'jabber-chat-prompt-system)))
	(insert (jabber-propertize body
				   'face 'jabber-chat-text-foreign)))
      t)))

(defun jabber-chat-print-url (xml-data)
  "Print URLs provided in jabber:x:oob namespace."
  (dolist (x (jabber-xml-node-children xml-data))
    (when (and (listp x) (eq (jabber-xml-node-name x) 'x)
	       (string= (jabber-xml-get-attribute x 'xmlns) "jabber:x:oob"))

      (let ((url (car (jabber-xml-node-children
		       (car (jabber-xml-get-children x 'url)))))
	    (desc (car (jabber-xml-node-children
			(car (jabber-xml-get-children x 'desc))))))
	(insert (jabber-propertize
		 "URL: " 'face 'jabber-chat-prompt-system))
	(insert (format "%s <%s>" desc url))
	(insert "\n")))))

(defun jabber-chat-goto-address (&rest ignore)
  "Call `goto-address' on the newly written text."
  (goto-address))

(add-to-list 'jabber-jid-chat-menu
	     (cons "Send message" 'jabber-send-message))

(defun jabber-send-message (to subject body type)
  "send a message tag to the server"
  (interactive (list (jabber-read-jid-completing "to: ")
		     (jabber-read-with-input-method "subject: ")
		     (jabber-read-with-input-method "body: ")
		     (read-string "type: ")))
  (jabber-send-sexp `(message ((to . ,to)
                               ,(if (> (length type) 0)
                                    `(type . ,type)))
                              ,(if (> (length subject) 0)
                                   `(subject () ,(jabber-escape-xml subject)))
                              ,(if (> (length body) 0)
                                   `(body () ,(jabber-escape-xml body)))))
  (if (and jabber-history-enabled (not (string= type "groupchat")))
      (jabber-history-log-message "out" nil to body (current-time))))

(add-to-list 'jabber-jid-chat-menu
	     (cons "Start chat" 'jabber-chat-with))

(defun jabber-chat-with (jid &optional other-window)
  "Open an empty chat window for chatting with JID.
With a prefix argument, open buffer in other window."
  (interactive (list (jabber-read-jid-completing "chat with:")
		     current-prefix-arg))
  (let ((buffer (jabber-chat-create-buffer jid)))
    (if other-window
	(switch-to-buffer-other-window buffer)
      (switch-to-buffer buffer))))

(defun jabber-chat-with-jid-at-point (&optional other-window)
  "Start chat with JID at point.
Signal an error if there is no JID at point.
With a prefix argument, open buffer in other window."
  (interactive "P")
  (let ((jid-at-point (get-text-property (point)
					 'jabber-jid)))
    (if jid-at-point
	(jabber-chat-with jid-at-point other-window)
      (error "No contact at point"))))

(provide 'jabber-chat)

;; arch-tag: f423eb92-aa87-475b-b590-48c93ccba9be
