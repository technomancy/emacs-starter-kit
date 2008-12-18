;; jabber-alert.el - alert hooks

;; Copyright (C) 2002, 2003, 2004 - tom berger - object@intelectronica.net
;; Copyright (C) 2003, 2004, 2005 - Magnus Henoch - mange@freemail.hu

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

(require 'jabber-util)

(require 'cl)

(defgroup jabber-alerts nil "auditory and visual alerts for jabber events"
  :group 'jabber)

(defcustom jabber-alert-message-hooks '(jabber-message-echo
					jabber-message-scroll)
  "Hooks run when a new message arrives.

Arguments are FROM, BUFFER, TEXT and PROPOSED-ALERT.  FROM is the JID
of the sender, BUFFER is the the buffer where the message can be read,
and TEXT is the text of the message.  PROPOSED-ALERT is the string
returned by `jabber-alert-message-function' for these arguments, so that
hooks do not have to call it themselves.

This hook is meant for user customization of message alerts.  For
other uses, see `jabber-message-hooks'."
  :type 'hook
  :options '(jabber-message-beep 
	     jabber-message-wave
	     jabber-message-echo
	     jabber-message-switch
	     jabber-message-display
	     jabber-message-scroll)
  :group 'jabber-alerts)

(defvar jabber-message-hooks nil
  "Internal hooks run when a new message arrives.

This hook works just like `jabber-alert-message-hooks', except that
it's not meant to be customized by the user.")

(defcustom jabber-alert-message-function
  'jabber-message-default-message
  "Function for constructing message alert messages.

Arguments are FROM, BUFFER, and TEXT.  This function should return a
string containing an appropriate text message, or nil if no message
should be displayed.

The provided hooks displaying a text message get it from this function,
and show no message if it returns nil.  Other hooks do what they do
every time."
  :type 'function
  :group 'jabber-alerts)

(defcustom jabber-alert-muc-hooks '(jabber-muc-echo jabber-muc-scroll)
  "Hooks run when a new MUC message arrives.

Arguments are NICK, GROUP, BUFFER, TEXT and PROPOSED-ALERT.  NICK
is the nickname of the sender.  GROUP is the JID of the group.
BUFFER is the the buffer where the message can be read, and TEXT
is the text of the message.  PROPOSED-ALERT is the string
returned by `jabber-alert-muc-function' for these arguments,
so that hooks do not have to call it themselves."
  :type 'hook
  :options '(jabber-muc-beep 
	     jabber-muc-wave
	     jabber-muc-echo
	     jabber-muc-switch
	     jabber-muc-display
	     jabber-muc-scroll)
  :group 'jabber-alerts)

(defvar jabber-muc-hooks '()
  "Internal hooks run when a new MUC message arrives.

This hook works just like `jabber-alert-muc-hooks', except that
it's not meant to be customized by the user.")

(defcustom jabber-alert-muc-function
  'jabber-muc-default-message
  "Function for constructing message alert messages.

Arguments are NICK, GROUP, BUFFER, and TEXT.  This function
should return a string containing an appropriate text message, or
nil if no message should be displayed.

The provided hooks displaying a text message get it from this function,
and show no message if it returns nil.  Other hooks do what they do
every time."
  :type 'function
  :group 'jabber-alerts)

(defcustom jabber-alert-presence-hooks 
  '(jabber-presence-update-roster
    jabber-presence-echo)
  "Hooks run when a user's presence changes.

Arguments are WHO, OLDSTATUS, NEWSTATUS, STATUSTEXT and
PROPOSED-ALERT.  WHO is a symbol whose text is the JID of the contact,
and which has various interesting properties.  OLDSTATUS is the old
presence or nil if disconnected.  NEWSTATUS is the new presence, or
one of \"subscribe\", \"unsubscribe\", \"subscribed\" and
\"unsubscribed\".  PROPOSED-ALERT is the string returned by
`jabber-alert-presence-message-function' for these arguments."
  :type 'hook
  :options '(jabber-presence-beep
	     jabber-presence-wave
	     jabber-presence-update-roster
	     jabber-presence-switch
	     jabber-presence-display
	     jabber-presence-echo)
  :group 'jabber-alerts)

(defvar jabber-presence-hooks '(jabber-presence-watch)
  "Internal hooks run when a user's presence changes.

This hook works just like `jabber-alert-presence-hooks', except that
it's not meant to be customized by the user.")

(defcustom jabber-alert-presence-message-function
  'jabber-presence-default-message
  "Function for constructing presence alert messages.

Arguments are WHO, OLDSTATUS, NEWSTATUS and STATUSTEXT.  See
`jabber-alert-presence-hooks' for documentation. This function
should return a string containing an appropriate text message, or nil
if no message should be displayed.

The provided hooks displaying a text message get it from this function.
All hooks refrain from action if this function returns nil."
  :type 'function
  :group 'jabber-alerts)

(defcustom jabber-alert-info-message-hooks '(jabber-info-display jabber-info-echo)
  "Hooks run when an info request is completed.

First argument is WHAT, a symbol telling the kind of info request completed.
That might be 'roster, for requested roster updates, and 'browse, for
browse requests.  Second argument in BUFFER, a buffer containing the result.
Third argument is PROPOSED-ALERT, containing the string returned by
`jabber-alert-info-message-function' for these arguments."
  :type 'hook
  :options '(jabber-info-beep
	     jabber-info-wave
	     jabber-info-echo
	     jabber-info-switch
	     jabber-info-display)
  :group 'jabber-alerts)

(defvar jabber-info-message-hooks '()
  "Internal hooks run when an info request is completed.

This hook works just like `jabber-alert-info-message-hooks',
except that it's not meant to be customized by the user.")

(defcustom jabber-alert-info-message-function
  'jabber-info-default-message
  "Function for constructing info alert messages.

Arguments are WHAT, a symbol telling the kind of info request completed,
and BUFFER, a buffer containing the result."
  :type 'function
  :group 'jabber-alerts)

(defcustom jabber-info-message-alist
  '((roster . "Roster display updated")
    (browse . "Browse request completed"))
  "Alist for info alert messages, used by `jabber-info-default-message'."
  :type '(alist :key-type symbol :value-type string
		:options (roster browse))
  :group 'jabber-alerts)

(defcustom jabber-alert-message-wave ""
  "a sound file to play when a message arrived"
  :type 'file
  :group 'jabber-alerts)

(defcustom jabber-alert-muc-wave ""
  "a sound file to play when a MUC message arrived"
  :type 'file
  :group 'jabber-alerts)

(defcustom jabber-alert-presence-wave ""
  "a sound file to play when a presence arrived"
  :type 'file
  :group 'jabber-alerts)

(defcustom jabber-alert-info-wave ""
  "a sound file to play when an info query result arrived"
  :type 'file
  :group 'jabber-alerts)

(defmacro define-jabber-alert (name docstring function)
  "Define a new family of external alert hooks.
Use this macro when your hooks do nothing except displaying a string
in some new innovative way.  You write a string display function, and
this macro does all the boring and repetitive work.

NAME is the name of the alert family.  The resulting hooks will be
called jabber-{message,muc,presence,info}-NAME.
DOCSTRING is the docstring to use for those hooks.
FUNCTION is a function that takes one argument, a string,
and displays it in some meaningful way.  It can be either a
lambda form or a quoted function name.
The created functions are inserted as options in Customize.

Examples:
\(define-jabber-alert foo \"Send foo alert\" 'foo-message)
\(define-jabber-alert bar \"Send bar alert\" 
  (lambda (msg) (bar msg 42)))"
  (let ((sn (symbol-name name)))
    (let ((msg (intern (format "jabber-message-%s" sn)))
	  (muc (intern (format "jabber-muc-%s" sn)))
	  (pres (intern (format "jabber-presence-%s" sn)))
	  (info (intern (format "jabber-info-%s" sn))))
      `(progn
	 (defun ,msg (from buffer text proposed-alert)
	   ,docstring
	   (when proposed-alert
	     (funcall ,function proposed-alert)))
	 (pushnew (quote ,msg) (get 'jabber-alert-message-hooks 'custom-options))
	 (defun ,muc (nick group buffer text proposed-alert)
	   ,docstring
	   (when proposed-alert
	     (funcall ,function proposed-alert)))
	 (pushnew (quote ,muc) (get 'jabber-alert-muc-hooks 'custom-options))
	 (defun ,pres (who oldstatus newstatus statustext proposed-alert)
	   ,docstring
	   (when proposed-alert
	     (funcall ,function proposed-alert)))
	 (pushnew (quote ,pres) (get 'jabber-alert-presence-hooks 'custom-options))
	 (defun ,info (infotype buffer proposed-alert)
	   ,docstring
	   (when proposed-alert
	     (funcall ,function proposed-alert)))
	 (pushnew (quote ,info) (get 'jabber-alert-info-message-hooks 'custom-options))))))

;; Alert hooks
(define-jabber-alert echo "Show a message in the echo area"
  (lambda (msg) (message "%s" msg)))
(define-jabber-alert beep "Beep on event"
  (lambda (&rest ignore) (beep)))

;; External notifiers
(require 'jabber-screen)
(require 'jabber-ratpoison)
(require 'jabber-sawfish)
(require 'jabber-festival)
(require 'jabber-xmessage)

;; Message alert hooks
(defun jabber-message-default-message (from buffer text)
  (when (or jabber-message-alert-same-buffer
	    (not (memq (selected-window) (get-buffer-window-list buffer))))
    (if (jabber-muc-sender-p from)
	(format "Private message from %s in %s"
		(jabber-jid-resource from)
		(jabber-jid-displayname (jabber-jid-user from)))
      (format "Message from %s" (jabber-jid-displayname from)))))

(defcustom jabber-message-alert-same-buffer t
  "If nil, don't display message alerts for the current buffer."
  :type 'boolean
  :group 'jabber-alerts)

(defun jabber-message-wave (from buffer text proposed-alert)
  "Play the wave file specified in `jabber-alert-message-wave'"
  (when proposed-alert
    (jabber-play-sound-file jabber-alert-message-wave)))

(defun jabber-message-display (from buffer text proposed-alert)
  "Display the buffer where a new message has arrived."
  (when proposed-alert
    (display-buffer buffer)))

(defun jabber-message-switch (from buffer text proposed-alert)
  "Switch to the buffer where a new message has arrived."
  (when proposed-alert
    (switch-to-buffer buffer)))

(defun jabber-message-scroll (from buffer text proposed-alert)
  "Scroll all nonselected windows where the chat buffer is displayed."
  ;; jabber-chat-buffer-display will DTRT with point in the buffer.
  ;; But this change will not take effect in nonselected windows.
  ;; Therefore we do that manually here.
  ;;
  ;; There are three cases:
  ;; 1. The user started typing a message in this window.  Point is
  ;;    greater than jabber-point-insert.  In that case, we don't
  ;;    want to move point.
  ;; 2. Point was at the end of the buffer, but no message was being
  ;;    typed.  After displaying the message, point is now close to
  ;;    the end of the buffer.  We advance it to the end.
  ;; 3. The user was perusing history in this window.  There is no
  ;;    simple way to distinguish this from 2, so the user loses.
  (let ((windows (get-buffer-window-list buffer nil t))
	(new-point-max (with-current-buffer buffer (point-max))))
    (dolist (w windows)
      (unless (eq w (selected-window))
	(set-window-point w new-point-max)))))

;; MUC alert hooks
(defun jabber-muc-default-message (nick group buffer text)
  (when (or jabber-message-alert-same-buffer
	    (not (memq (selected-window) (get-buffer-window-list buffer))))
    (if nick
	(format "Message from %s in %s" nick (jabber-jid-displayname
					      group))
      (format "Message in %s" (jabber-jid-displayname group)))))

(defun jabber-muc-wave (nick group buffer text proposed-alert)
  "Play the wave file specified in `jabber-alert-muc-wave'"
  (when proposed-alert
    (jabber-play-sound-file jabber-alert-muc-wave)))

(defun jabber-muc-display (nick group buffer text proposed-alert)
  "Display the buffer where a new message has arrived."
  (when proposed-alert
    (display-buffer buffer)))

(defun jabber-muc-switch (nick group buffer text proposed-alert)
  "Switch to the buffer where a new message has arrived."
  (when proposed-alert
    (switch-to-buffer buffer)))

(defun jabber-muc-scroll (nick group buffer text proposed-alert)
  "Scroll buffer even if it is in an unselected window."
  (jabber-message-scroll nil buffer nil nil))

;; Presence alert hooks
(defun jabber-presence-default-message (who oldstatus newstatus statustext)
  "This function returns nil if OLDSTATUS and NEWSTATUS are equal, and in other
cases a string of the form \"'name' (jid) is now NEWSTATUS (STATUSTEXT)\".

This function is not called directly, but is the default for
`jabber-alert-presence-message-function'."
  (cond
   ((equal oldstatus newstatus)
      nil)
   (t
    (let ((formattedname
	   (if (> (length (get who 'name)) 0)
	       (get who 'name)
	     (symbol-name who)))
	  (formattedstatus
	   (or
	    (cdr (assoc newstatus
			'(("subscribe" . " requests subscription to your presence")
			  ("subscribed" . " has granted presence subscription to you")
			  ("unsubscribe" . " no longer subscribes to your presence")
			  ("unsubscribed" . " cancels your presence subscription"))))
	    (concat " is now "
		    (or
		     (cdr (assoc newstatus jabber-presence-strings))
		     newstatus))))
	  (formattedtext
	   (if (> (length statustext) 0)
	       (concat " (" (jabber-unescape-xml statustext) ")")
	     "")))
      (concat formattedname formattedstatus formattedtext)))))

(defun jabber-presence-wave (who oldstatus newstatus statustext proposed-alert)
  "Play the wave file specified in `jabber-alert-presence-wave'"
  (if proposed-alert
      (jabber-play-sound-file jabber-alert-presence-wave)))

;; This is now defined in jabber-roster.el.
;; (defun jabber-presence-update-roster (who oldstatus newstatus statustext proposed-alert)
;;   "Update the roster display by calling `jabber-display-roster'"
;;   (jabber-display-roster))

(defun jabber-presence-display (who oldstatus newstatus statustext proposed-alert)
  "Display the roster buffer"
  (when proposed-alert
    (display-buffer jabber-roster-buffer)))

(defun jabber-presence-switch (who oldstatus newstatus statustext proposed-alert)
  "Switch to the roster buffer"
  (when proposed-alert
    (switch-to-buffer jabber-roster-buffer)))

;;; Info alert hooks

(defun jabber-info-default-message (infotype buffer)
  "Function for constructing info alert messages.

The argument is INFOTYPE, a symbol telling the kind of info request completed.
This function uses `jabber-info-message-alist' to find a message."
  (concat (cdr (assq infotype jabber-info-message-alist))
	  " (buffer "(buffer-name buffer) ")"))

(defun jabber-info-wave (infotype buffer proposed-alert)
  "Play the wave file specified in `jabber-alert-info-wave'"
  (if proposed-alert
      (jabber-play-sound-file jabber-alert-info-wave)))

(defun jabber-info-display (infotype buffer proposed-alert)
  "Display buffer of completed request"
  (when proposed-alert
    (display-buffer buffer)))

(defun jabber-info-switch (infotype buffer proposed-alert)
  "Switch to buffer of completed request"
  (when proposed-alert
    (switch-to-buffer buffer)))

(provide 'jabber-alert)

;;; arch-tag: 725bd73e-c613-4fdc-a11d-3392a7598d4f
