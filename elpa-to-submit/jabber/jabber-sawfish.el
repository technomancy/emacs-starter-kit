;; jabber-sawfish.el - emacs-jabber interface to sawfish

;; Copyright (C) 2005 - Mario Domenech Goulart

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

(defcustom jabber-sawfish-display-time 3
  "Time in seconds for displaying a jabber message through the
Sawfish window manager."
  :type 'integer
  :group 'jabber-alerts)

(defun jabber-sawfish-display-message (message)
  "Displays MESSAGE through the Sawfish window manager."
  (let ((process-connection-type nil))
    (start-process-shell-command 
     "jabber-sawfish" nil "echo" 
     (concat "'(progn (display-message \"" 
	     message
	     "\")(make-timer (lambda () (display-message nil)) 3))' | sawfish-client - &> /dev/null"))))

(define-jabber-alert sawfish "Display a message through the Sawfish window manager"
  'jabber-sawfish-display-message)

(provide 'jabber-sawfish)
;; arch-tag: 4F0154ED-5D05-11D9-9E6B-000A95C2FCD0
