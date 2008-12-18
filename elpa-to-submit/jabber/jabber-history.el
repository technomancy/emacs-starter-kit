;; jabber-history.el - recording message history

;; Copyright (C) 2004 - Mathias Dahl
;; Copyright (C) 2004 - Magnus Henoch - mange@freemail.hu

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

;;; Log format:
;; Each message is on one separate line, represented as a vector with
;; five elements.  The first element is time encoded according to
;; JEP-0082.  The second element is direction, "in" or "out".
;; The third element is the sender, "me" or a JID.  The fourth
;; element is the recipient.  The fifth element is the text
;; of the message.

;; FIXME: when rotation is enabled, jabber-history-query won't look
;; for older history files if the current history file doesn't contain
;; enough backlog entries.

(require 'jabber-core)

(defgroup jabber-history nil "Customization options for Emacs
Jabber history files."
  :group 'jabber)

(defcustom jabber-history-enabled nil
  "Non-nil means message logging is enabled."
  :type 'boolean
  :group 'jabber-history)

(defcustom jabber-use-global-history t
  "Indicate whether Emacs Jabber should use a global file for
  store messages.  If non-nil, jabber-global-history-filename is
  used, otherwise, messages are stored in per-user files under
  the jabber-history-dir directory."
  :type 'boolean
  :group 'jabber-history)

(defcustom jabber-history-dir "~/.emacs-jabber"
  "Base directory where per-contact history files are stored.
  Used only when jabber-use-global-history is not true."
  :type 'directory
  :group 'jabber-history)

(defcustom jabber-global-history-filename "~/.jabber_global_message_log"
  "Global file where all messages are logged.  Used when
  jabber-use-global-history is non-nil."
  :type 'file
  :group 'jabber-history)

(defcustom jabber-history-enable-rotation nil
  "Whether history files should be renamed when reach
  jabber-history-size-limit kilobytes.  If nil, history files
  will grow indefinitely, otherwise they'll be renamed to
  <history-file>-<number>, where <number> is 1 or the smallest
  number after the last rotation."
  :type 'boolean
  :group 'jabber-history)

(defcustom jabber-history-size-limit 1024
  "Maximum history file size in kilobytes.  When history file
  reaches this limit, it is renamed to <history-file>-<number>,
  where <number> is 1 or the smallest number after the last
  rotation."
  :type 'integer
  :group 'jabber-history)


(defun jabber-rotate-history-p (history-file)
  "Return true if HISTORY-FILE should be rotated."
  (when (and jabber-history-enable-rotation
	     (file-exists-p history-file))
    (> (/ (nth 7 (file-attributes history-file)) 1024)
       jabber-history-size-limit)))

(defun jabber-history-rotate (history-file &optional try)
  "Rename HISTORY-FILE to HISTORY-FILE-TRY."
  (let ((suffix (number-to-string (or try 1))))
    (if (file-exists-p (concat history-file "-"  suffix))
	(jabber-history-rotate history-file (if try (1+ try) 1))
      (rename-file history-file (concat history-file "-" suffix)))))

(add-to-list 'jabber-message-chain 'jabber-message-history)
(defun jabber-message-history (xml-data)
  "Log message to log file."
  (when (and (not jabber-use-global-history)
	     (not (file-directory-p jabber-history-dir)))
    (make-directory jabber-history-dir))
  (if (and jabber-history-enabled (not (jabber-muc-message-p xml-data)))
      (let ((from (jabber-xml-get-attribute xml-data 'from))
	    (text (car (jabber-xml-node-children
			(car (jabber-xml-get-children xml-data 'body)))))
	    (timestamp (car (delq nil (mapcar 'jabber-x-delay (jabber-xml-get-children xml-data 'x))))))
	(when (and from text)
	  (jabber-history-log-message "in" from nil text timestamp)))))

(add-hook 'jabber-chat-send-hooks 'jabber-history-send-hook)

(defun jabber-history-send-hook (body id)
  "Log outgoing message to log file."
  (when (and (not jabber-use-global-history)
	     (not (file-directory-p jabber-history-dir)))
    (make-directory jabber-history-dir))
  ;; This function is called from a chat buffer, so jabber-chatting-with
  ;; contains the desired value.
  (if jabber-history-enabled
      (jabber-history-log-message "out" nil jabber-chatting-with body (current-time))))

(defun jabber-history-filename (contact)
  "Return a history filename for CONTACT if the per-user file
  loggin strategy is used or the global history filename."
  (if jabber-use-global-history
      jabber-global-history-filename
    (concat jabber-history-dir "/" (jabber-jid-user contact))))

(defun jabber-history-log-message (direction from to body timestamp)
  "Log a message"
  (with-temp-buffer
    ;; Remove properties
    (set-text-properties 0 (length body) nil body)
    ;; Encode text as Lisp string - get decoding for free
    (setq body (prin1-to-string body))
    ;; Encode LF and CR
    (while (string-match "\n" body)
      (setq body (replace-match "\\n" nil t body nil)))
    (while (string-match "\r" body)
      (setq body (replace-match "\\r" nil t body nil)))
    (insert (format "[\"%s\" \"%s\" \"%s\" \"%s\" %s]\n"
		    (jabber-encode-time (or timestamp (current-time)))
		    (or direction
			"in")
		    (or from
			"me")
		    (or to
			"me")
		    body))
    (let ((coding-system-for-write 'utf-8)
	  (history-file (jabber-history-filename (or from to))))
      (when (and (not jabber-use-global-history)
		 (not (file-directory-p jabber-history-dir)))
	(make-directory jabber-history-dir))
      (when (jabber-rotate-history-p history-file)
	(jabber-history-rotate history-file))
      (write-region (point-min) (point-max) history-file t 'quiet))))

(defun jabber-history-query (start-time
			     end-time
			     number
			     direction
			     jid-regexp
			     history-file)
  "Return a list of vectors, one for each message matching the criteria.
START-TIME and END-TIME are floats as obtained from `float-time'.
Either or both may be nil, meaning no restriction.
NUMBER is the maximum number of messages to return, or t for
unlimited.
DIRECTION is either \"in\" or \"out\", or t for no limit on direction.
JID-REGEXP is a regexp which must match the JID.
HISTORY-FILE is the file in which to search.

Currently jabber-history-query performs a linear search from the end
of the log file."
  (when (file-readable-p history-file)
    (with-temp-buffer
      (let ((coding-system-for-read 'utf-8))
	(insert-file-contents history-file))
      (let (collected current-line)
	(goto-char (point-max))
	(catch 'beginning-of-file
	    (while (progn
		     (backward-sexp)
		     (setq current-line (car (read-from-string
					      (buffer-substring
					       (point)
					       (save-excursion
						 (forward-sexp)
						 (point))))))
		     (and (or (null start-time)
			      (> (jabber-float-time (jabber-parse-time
						     (aref current-line 0)))
				 start-time))
			  (or (eq number t)
			      (< (length collected) number))))
	      (if (and (or (eq direction t)
			   (string= direction (aref current-line 1)))
		       (or (null end-time)
			   (> end-time (jabber-float-time (jabber-parse-time
							   (aref current-line 0)))))
		       (string-match
			jid-regexp 
			(car
			 (remove "me"
				 (list (aref current-line 2)
				       (aref current-line 3))))))
		  (push current-line collected))
	      (when (bobp)
		(throw 'beginning-of-file nil))))
	collected))))

(defcustom jabber-backlog-days 3.0
  "Age limit on messages in chat buffer backlog, in days"
  :group 'jabber
  :type '(choice (number :tag "Number of days")
		 (const :tag "No limit" nil)))

(defcustom jabber-backlog-number 10
  "Maximum number of messages in chat buffer backlog"
  :group 'jabber
  :type 'integer)

(defun jabber-history-backlog (jid &optional before)
  "Fetch context from previous chats with JID.
Return a list of history entries (vectors), limited by
`jabber-backlog-days' and `jabber-backlog-number'.
If BEFORE is non-nil, it should be a float-time after which
no entries will be fetched.  `jabber-backlog-days' still
applies, though."
  (interactive)
  (jabber-history-query 
   (and jabber-backlog-days
	(- (jabber-float-time) (* jabber-backlog-days 86400.0)))
   before
   jabber-backlog-number
   t					; both incoming and outgoing
   (concat "^" (regexp-quote (jabber-jid-user jid)) "\\(/.*\\)?$")
   (jabber-history-filename jid)))

(provide 'jabber-history)

;; arch-tag: 0AA0C235-3FC0-11D9-9FE7-000A95C2FCD0
