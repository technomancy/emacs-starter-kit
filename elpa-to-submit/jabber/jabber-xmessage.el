;; jabber-xmessage.el - emacs-jabber interface to xmessage

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

(defun jabber-xmessage-display-message (message)
  "Displays MESSAGE using the xmessage program."
  (let ((process-connection-type nil))
    (start-process "xmessage" nil "xmessage" message)))

(define-jabber-alert xmessage "Display a message using the xmessage program."
  'jabber-xmessage-display-message)

(provide 'jabber-xmessage)
;; arch-tag: 10A74D00-5D2C-11D9-A294-000A95C2FCD0
