;; jabber-screen.el - emacs-jabber interface to screen

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

(defun jabber-screen-message (msg)
  "Show MSG in screen"
  (call-process "screen" nil nil nil "-X" "echo" msg))

(define-jabber-alert screen "Show a message through the Screen terminal manager"
  'jabber-screen-message)

(provide 'jabber-screen)
;; arch-tag: B576ADDA-5D04-11D9-AA52-000A95C2FCD0
