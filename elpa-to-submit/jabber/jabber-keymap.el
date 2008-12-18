;; jabber-keymap.el - common keymap for many modes

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


;; button.el was introduced in Emacs 22
(condition-case e
    (require 'button)
  (error nil))

(defvar jabber-common-keymap 
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-c\C-c" 'jabber-popup-chat-menu)
    (define-key map "\C-c\C-r" 'jabber-popup-roster-menu)
    (define-key map "\C-c\C-i" 'jabber-popup-info-menu)
    (define-key map "\C-c\C-m" 'jabber-popup-muc-menu)
    (define-key map "\C-c\C-s" 'jabber-popup-service-menu)
    ;; note that {forward,backward}-button are not autoloaded.
    ;; thus the `require' above.
    (when (fboundp 'forward-button)
      (define-key map [?\t] 'forward-button)
      (define-key map [backtab] 'backward-button))
    map))

(defvar jabber-global-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-c" 'jabber-connect)
    (define-key map "\C-d" 'jabber-disconnect)
    (define-key map "\C-r" 'jabber-switch-to-roster-buffer)
    (define-key map "\C-j" 'jabber-chat-with)
    (define-key map "\C-a" 'jabber-send-away-presence)
    (define-key map "\C-o" 'jabber-send-default-presence)
    (define-key map "\C-x" 'jabber-send-xa-presence)
    map)
  "Global Jabber keymap (usually under C-x C-j)")

(define-key ctl-x-map "\C-j" jabber-global-keymap)

(provide 'jabber-keymap)

;;; arch-tag: 22a9993d-a4a7-40ef-a025-7cff6c3f5587
