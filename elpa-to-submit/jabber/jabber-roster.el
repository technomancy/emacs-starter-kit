;; jabber-roster.el - displaying the roster    -*- coding: utf-8; -*-

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

(require 'jabber-presence)
(require 'jabber-util)
(require 'jabber-alert)
(require 'jabber-keymap)
(require 'format-spec)

(defgroup jabber-roster nil "roster display options"
  :group 'jabber)

(defcustom jabber-roster-line-format " %c %-25n %u %-8s  %S\n"
  "The format specification of the lines in the roster display.

These fields are available:

%c   \"*\" if the contact is connected, or \" \" if not
%u   sUbscription state - see below
%n   Nickname of contact, or JID if no nickname
%j   Bare JID of contact (without resource)
%r   Highest-priority resource of contact
%s   Availability of contact as string (\"Online\", \"Away\" etc)
%S   Status string specified by contact

%u is replaced by one of the strings given by
`jabber-roster-subscription-display'."
  :type 'string
  :group 'jabber-roster)

(defcustom jabber-roster-subscription-display '(("none" . "   ")
						("from" . "<  ")
						("to" . "  >")
						("both" . "<->"))
  "Strings used for indicating subscription status of contacts.
\"none\" means that there is no subscription between you and the
contact.
\"from\" means that the contact has a subscription to you, but you
have no subscription to the contact.
\"to\" means that you have a subscription to the contact, but the
contact has no subscription to you.
\"both\" means a mutual subscription.

Having a \"presence subscription\" means being able to see the
other person's presence.

Some fancy arrows you might want to use, if your system can
display them: ← → ⇄ ↔"
  :type '(list (cons :format "%v" (const :format "" "none") (string :tag "None"))
	       (cons :format "%v" (const :format "" "from") (string :tag "From"))
	       (cons :format "%v" (const :format "" "to") (string :tag "To"))
	       (cons :format "%v" (const :format "" "both") (string :tag "Both")))
  :group 'jabber-roster)

(defcustom jabber-resource-line-format "     %r - %s (%S), priority %p\n"
  "The format specification of resource lines in the roster display.
These are displayed when `jabber-show-resources' permits it.

These fields are available:

%c   \"*\" if the contact is connected, or \" \" if not
%n   Nickname of contact, or JID if no nickname
%j   Bare JID of contact (without resource)
%p   Priority of this resource
%r   Name of this resource
%s   Availability of resource as string (\"Online\", \"Away\" etc)
%S   Status string specified by resource"
  :type 'string
  :group 'jabber-roster)

(defcustom jabber-roster-sort-functions
  '(jabber-roster-sort-by-status jabber-roster-sort-by-displayname)
  "Sort roster according to these criteria.

These functions should take two roster items A and B, and return:
<0 if A < B
0  if A = B
>0 if A > B"
  :type 'hook
  :options '(jabber-roster-sort-by-status 
	     jabber-roster-sort-by-displayname)
  :group 'jabber-roster)

(defcustom jabber-sort-order '("chat" "" "away" "dnd" "xa")
  "Sort by status in this order.  Anything not in list goes last.
Offline is represented as nil."
  :type '(repeat (restricted-sexp :match-alternatives (stringp nil)))
  :group 'jabber-roster)

(defcustom jabber-show-resources 'sometimes
  "Show resources in roster?"
  :type '(radio (const :tag "Never" nil)
		(const :tag "When more than one connected resource" sometimes)
		(const :tag "Always" always))
  :group 'jabber-roster)

(defcustom jabber-remove-newlines t
  "Remove newlines in status messages?
Newlines in status messages mess up the roster display.  However,
they are essential to status message poets.  Therefore, you get to
choose the behaviour.

Trailing newlines are always removed, regardless of this variable."
  :type 'boolean
  :group 'jabber-roster)

(defcustom jabber-roster-show-bindings t
  "Show keybindings in roster buffer?"
  :type 'boolean
  :group 'jabber-roster)

(defcustom jabber-roster-mode-hook nil
  "Hook run when entering Roster mode."
  :group 'jabber-roster
  :type 'hook)

(defface jabber-roster-user-online
  '((t (:foreground "blue" :weight bold :slant normal)))
  "face for displaying online users"
  :group 'jabber-roster)

(defface jabber-roster-user-xa
  '((((background dark)) (:foreground "magenta" :weight normal :slant italic))
    (t (:foreground "black" :weight normal :slant italic)))
  "face for displaying extended away users"
  :group 'jabber-roster)

(defface jabber-roster-user-dnd
  '((t (:foreground "red" :weight normal :slant italic)))
  "face for displaying do not disturb users"
  :group 'jabber-roster)

(defface jabber-roster-user-away
  '((t (:foreground "dark green" :weight normal :slant italic)))
  "face for displaying away users"
  :group 'jabber-roster)

(defface jabber-roster-user-chatty
  '((t (:foreground "dark orange" :weight bold :slant normal)))
  "face for displaying chatty users"
  :group 'jabber-roster)

(defface jabber-roster-user-error
  '((t (:foreground "red" :weight light :slant italic)))
  "face for displaying users sending presence errors"
  :group 'jabber-roster)

(defface jabber-roster-user-offline
  '((t (:foreground "dark grey" :weight light :slant italic)))
  "face for displaying offline users"
  :group 'jabber-roster)

(defvar jabber-roster-mode-map 
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map jabber-common-keymap)
    (define-key map [mouse-2] 'jabber-popup-combined-menu)
    (define-key map (kbd "TAB") 'jabber-go-to-next-jid)
    (define-key map (kbd "RET") 'jabber-chat-with-jid-at-point)
    (define-key map (kbd "C-k") 'jabber-roster-delete-jid-at-point)

    (define-key map "e" 'jabber-roster-change)
    (define-key map "s" 'jabber-send-subscription-request)
    (define-key map "q" 'bury-buffer)
    (define-key map "i" 'jabber-get-disco-items)
    (define-key map "j" 'jabber-groupchat-join)
    (define-key map "I" 'jabber-get-disco-info)
    (define-key map "b" 'jabber-get-browse)
    (define-key map "v" 'jabber-get-version)
    (define-key map "a" 'jabber-send-presence)
    (define-key map "g" 'jabber-display-roster)
    ;;(define-key map "D" 'jabber-disconnect)
    map))

(defun jabber-roster-mode ()
  "Major mode for Jabber roster display.
Use the keybindings (mnemonic as Chat, Roster, Info, MUC, Service) to
bring up menus of actions.
\\{jabber-roster-mode-map}"
  (kill-all-local-variables)
  (setq major-mode 'jabber-roster-mode
	mode-name "jabber-roster")
  (use-local-map jabber-roster-mode-map)
  (setq buffer-read-only t)
  (if (fboundp 'run-mode-hooks)
      (run-mode-hooks 'jabber-roster-mode-hook)
    (run-hooks 'jabber-roster-mode-hook)))

(put 'jabber-roster-mode 'mode-class 'special)

(defun jabber-switch-to-roster-buffer ()
  "Switch to roster buffer."
  (interactive)
  (switch-to-buffer jabber-roster-buffer))

(defun jabber-sort-roster ()
  "sort roster according to online status"
  (setq *jabber-roster*
	(sort *jabber-roster* #'jabber-roster-sort-items)))

(defun jabber-roster-sort-items (a b)
  "Sort roster items A and B according to `jabber-roster-sort-functions'.
Return t if A is less than B."
  (dolist (fn jabber-roster-sort-functions)
    (let ((comparison (funcall fn a b)))
      (cond
       ((< comparison 0)
	(return t))
       ((> comparison 0)
	(return nil))))))

(defun jabber-roster-sort-by-status (a b)
  "Sort roster items by online status.
See `jabber-sort-order' for order used."
  (flet ((order (item) (length (member (get item 'show) jabber-sort-order))))
    (let ((a-order (order a))
	  (b-order (order b)))
      ;; Note reversed test.  Items with longer X-order go first.
      (cond
       ((< a-order b-order)
	1)
       ((> a-order b-order)
	-1)
       (t
	0)))))

(defun jabber-roster-sort-by-displayname (a b)
  "Sort roster items by displayed name."
  (cond
   ((string-lessp a b) -1)
   ((string= a b) 0)
   (t 1)))

(defun jabber-fix-status (status)
  "Make status strings more readable"
  (when (string-match "\n+$" status)
    (setq status (replace-match "" t t status)))
  (when jabber-remove-newlines
    (while (string-match "\n" status)
      (setq status (replace-match " " t t status))))
  status)

(defvar jabber-roster-positions nil
  "Alist tracking positions of items in the roster.
Keys are bare JID symbols.  Values are conses of markers,
marking the extent of the roster entry.")

(defun jabber-display-roster ()
  "switch to the main jabber buffer and refresh the roster display to reflect the current information"
  (interactive)
  (with-current-buffer (get-buffer-create jabber-roster-buffer)
    (if (not (eq major-mode 'jabber-roster-mode))
	(jabber-roster-mode))
    (setq buffer-read-only nil)
    ;; line-number-at-pos is in Emacs >= 21.4.  Only used to avoid
    ;; excessive scrolling when updating roster, so not absolutely
    ;; necessary.
    (let ((current-line (and (fboundp 'line-number-at-pos) (line-number-at-pos)))
	  (current-column (current-column)))
      (erase-buffer)
      (setq jabber-roster-positions nil)
      (insert (jabber-propertize jabber-server 'face 'jabber-title-large) "\n")
      (when jabber-roster-show-bindings
	(insert "RET      Open chat buffer        C-k      Delete roster item
e        Edit item               s        Send subscription request
q        Bury buffer             i        Get disco items
I        Get disco info          b        Browse
j        Join groupchat (MUC)    v        Get client version
a        Send presence
C-c C-c  Chat menu               C-c C-m  Multi-User Chat menu
C-c C-i  Info menu               C-c C-r  Roster menu
C-c C-s  Service menu
"))
      (insert "__________________________________\n\n")
      (let ((map (make-sparse-keymap)))
	(define-key map [mouse-2] #'jabber-send-presence)
	(insert (jabber-propertize (concat (format " - %s"
						   (cdr (assoc *jabber-current-show* jabber-presence-strings)))
					   (if (not (zerop (length *jabber-current-status*)))
					       (format " (%s)"
						       (jabber-fix-status *jabber-current-status*)))
					   " -")
				   'face (or (cdr (assoc *jabber-current-show* jabber-presence-faces))
					     'jabber-roster-user-online)
				   ;;'mouse-face (cons 'background-color "light grey")
				   'keymap map)
		"\n__________________________________\n\n"))

      (jabber-sort-roster)
      (dolist (buddy *jabber-roster*)
	(let ((entry-start (point)))
	  (jabber-display-roster-entry buddy)

	  ;; Keep track of this roster entry's position
	  (let ((entry (assq buddy jabber-roster-positions)))
	    (unless entry
	      (setq entry (cons buddy nil))
	      (push entry jabber-roster-positions))
	      (let ((marker-start (set-marker (make-marker) entry-start))
		    (marker-end (set-marker (make-marker) (point))))
		;; Text is inserted before start markers, but after
		;; end markers.
		(set-marker-insertion-type marker-start t)
		(setcdr entry (cons marker-start marker-end))))))
      (insert "__________________________________")
      (goto-char (point-min))
      (setq buffer-read-only t)
      (if (interactive-p)
	  (dolist (hook '(jabber-info-message-hooks jabber-alert-info-message-hooks))
	    (run-hook-with-args hook 'roster (current-buffer) (funcall jabber-alert-info-message-function 'roster (current-buffer)))))
      (when current-line
	(goto-line current-line)
	(move-to-column current-column)))))

(defun jabber-display-roster-entry (buddy)
  "Format and insert a roster entry for BUDDY at point."
  (let ((buddy-str (format-spec jabber-roster-line-format
				(list 
				 (cons ?c (if (get buddy 'connected) "*" " "))
				 (cons ?u (cdr (assoc (or (get buddy 'subscription) "none")
						      jabber-roster-subscription-display)))
				 (cons ?n (if (> (length (get buddy 'name)) 0)
					      (get buddy 'name)
					    (symbol-name buddy)))
				 (cons ?j (symbol-name buddy))
				 (cons ?r (or (get buddy 'resource) ""))
				 (cons ?s (or
					   (cdr (assoc (get buddy 'show) jabber-presence-strings))
					   (get buddy 'show)))
				 (cons ?S (if (get buddy 'status)
					      (jabber-fix-status (get buddy 'status))
					    ""))))))
    (add-text-properties 0
			 (length buddy-str)
			 (list
			  'face
			  (or (cdr (assoc (get buddy 'show) jabber-presence-faces))
			      'jabber-roster-user-online)
			  ;;'mouse-face
			  ;;(cons 'background-color "light grey")
			  'help-echo
			  (symbol-name buddy)
			  'jabber-jid
			  (symbol-name buddy))
			 buddy-str)
    ;; (let ((map (make-sparse-keymap))
    ;; 	      (chat-with-func (make-symbol (concat "jabber-chat-with" (symbol-name buddy)))))
    ;; 	  (fset chat-with-func `(lambda () (interactive) (jabber-chat-with ,(symbol-name buddy))))
    ;; 	  (define-key map [mouse-2] chat-with-func)
    ;; 	  (put-text-property 0
    ;; 			     (length buddy-str)
    ;; 			     'keymap 
    ;; 			     map
    ;; 			     buddy-str))
    (insert buddy-str)

    (when (or (eq jabber-show-resources 'always)
	      (and (eq jabber-show-resources 'sometimes)
		   (> (jabber-count-connected-resources buddy) 1)))
      (dolist (resource (get buddy 'resources))
	(when (plist-get (cdr resource) 'connected)
	  (let ((resource-str (format-spec jabber-resource-line-format
					   (list
					    (cons ?c "*")
					    (cons ?n (if (> (length (get buddy 'name)) 0)
							 (get buddy 'name)
						       (symbol-name buddy)))
					    (cons ?j (symbol-name buddy))
					    (cons ?r (if (> (length (car resource)) 0)
							 (car resource)
						       "empty"))
					    (cons ?s (or
						      (cdr (assoc (plist-get (cdr resource) 'show) jabber-presence-strings))
						      (plist-get (cdr resource) 'show)))
					    (cons ?S (if (plist-get (cdr resource) 'status)
							 (jabber-fix-status (plist-get (cdr resource) 'status))
						       ""))
					    (cons ?p (number-to-string (plist-get (cdr resource) 'priority)))))))
	    (add-text-properties 0
				 (length resource-str)
				 (list
				  'face
				  (or (cdr (assoc (plist-get (cdr resource) 'show) jabber-presence-faces))
				      'jabber-roster-user-online)
				  'jabber-jid
				  (format "%s/%s" (symbol-name buddy) (car resource)))
				 resource-str)
	    (insert resource-str)))))))

(defun jabber-presence-update-roster (who &rest ignore)
  "Update roster without redrawing all of it, if possible."
  
  (let* ((bare-jid (jabber-jid-symbol 
		    (jabber-jid-user
		     (symbol-name who))))
	 (entry (assq bare-jid jabber-roster-positions))
	 (inhibit-read-only t))
    (jabber-sort-roster)
    (if (null entry)
	(jabber-display-roster)
      (let ((old-start (cadr entry))
	    (old-end (cddr entry))
	    (insert-before-this (cadr (memq bare-jid *jabber-roster*))))
	(with-current-buffer jabber-roster-buffer
	  (delete-region old-start old-end)
	  (save-excursion
	    (let ((new-start 
		   (marker-position
		    (if insert-before-this
			;; If this is not the last entry, go to start
			;; position of next entry.
			(cadr (assq insert-before-this jabber-roster-positions))
		      ;; If this is the last entry, go to end position of second
		      ;; to last entry.
		      (cddr (car (last jabber-roster-positions 2)))))))
	      (goto-char new-start)
	      (jabber-display-roster-entry bare-jid)
	      (let ((marker-start (set-marker (make-marker) new-start))
		    (marker-end (set-marker (make-marker) (point))))
		;; Text is inserted before start markers, but after
		;; end markers.
		(set-marker-insertion-type marker-start t)
		(setcdr entry (cons marker-start marker-end))))))))))

(defun jabber-go-to-next-jid ()
  "Move the cursor to the next jid in the buffer"
  (interactive)
  (let ((next (next-single-property-change (point) 'jabber-jid)))
    (when (and next
               (not (get-text-property next 'jabber-jid)))
      (setq next (next-single-property-change next 'jabber-jid)))
    (unless next
      (setq next (next-single-property-change (point-min) 'jabber-jid)))
    (if next (goto-char (1+ next))
      (goto-char (point-min)))))

(provide 'jabber-roster)

;;; arch-tag: 096af063-0526-4dd2-90fd-bc6b5ba07d32
