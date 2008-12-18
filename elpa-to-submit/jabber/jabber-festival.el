;;; jabber-festival.el --- Festival alert hooks

;; Copyright (C) 2005  Magnus Henoch

;; This file is a part of jabber.el.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

(condition-case e
    (progn
      ;; Most people don't have Festival, so this will often fail
      (require 'festival)
      (define-jabber-alert festival "Voice messages through Festival"
	'festival-say-string))
  (error nil))

(provide 'jabber-festival)
;; arch-tag: 8922D096-5D07-11D9-B4C2-000A95C2FCD0


