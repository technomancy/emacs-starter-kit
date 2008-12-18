;; jabber.el - a minimal jabber client

;; Copyright (C) 2002, 2003, 2004 - tom berger - object@intelectronica.net
;; Copyright (C) 2003, 2004 - Magnus Henoch - mange@freemail.hu

;; SSL - Support, mostly inspired by Gnus
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

;;; load Unicode tables if this needed
(when (and (featurep 'xemacs) (not (emacs-version>= 21 5 5)))
    (require 'un-define))

;;; these customize fields should come first
(defgroup jabber nil "Jabber instant messaging"
  :group 'applications)

(defcustom jabber-username "emacs"
  "jabber username (user part of JID)" 
  :type 'string
  :group 'jabber)

(defcustom jabber-server "magaf.org" 
  "jabber server (domain part of JID)" 
  :type 'string
  :group 'jabber)

(defcustom jabber-password nil
  "jabber password" 
  :type '(radio (const :tag "Prompt for password" nil)
		 (string :tag "Save password in .emacs"))
  :group 'jabber)

(defcustom jabber-resource "emacs"
  "jabber resource" 
  :type 'string
  :group 'jabber)

(defcustom jabber-default-show ""
  "default show state"
  :type '(choice (const :tag "Online" "")
		 (const :tag "Chatty" "chat")
		 (const :tag "Away" "away")
		 (const :tag "Extended away" "xa")
		 (const :tag "Do not disturb" "dnd"))
  :group 'jabber)

(defcustom jabber-default-status ""
  "default status string"
  :type 'string
  :group 'jabber)

(defcustom jabber-default-priority 10
  "default priority"
  :type 'integer
  :group 'jabber)

(defcustom jabber-nickname jabber-username
  "jabber nickname, used in chat buffer prompts and as default groupchat nickname." 
  :type 'string
  :group 'jabber)

;;; guess internal dependencies!
(require 'jabber-util)
(require 'jabber-menu)
(require 'jabber-xml)
(require 'jabber-conn)
(require 'jabber-core)
(require 'jabber-logon)
(require 'jabber-roster)
(require 'jabber-presence)
(require 'jabber-alert)
(require 'jabber-chat)
(require 'jabber-disco)
(require 'jabber-iq)
(require 'jabber-widget)
(require 'jabber-register)
(require 'jabber-search)
(require 'jabber-browse)
(require 'jabber-muc)
(require 'jabber-version)
(require 'jabber-ahc-presence)
(require 'jabber-modeline)
(require 'jabber-keepalive)
(require 'jabber-watch)
(require 'jabber-activity)
(require 'jabber-vcard)
(require 'jabber-events)

;; XXX: automate this some time
(autoload 'jabber-export-roster "jabber-export"
  "Create buffer from which roster can be exported to a file."
  t)
(autoload 'jabber-import-roster "jabber-export"
  "Create buffer for roster import from FILE."
  t)

(defvar *jabber-current-status* ""
  "the users current presence staus")

(defvar *jabber-current-show* ""
  "the users current presence show")

(defvar *jabber-current-priority* 10
  "the user's current priority")

(defvar *jabber-status-history* nil
  "history of status messages")

(defgroup jabber-faces nil "faces for displaying jabber instant messaging"
  :group 'jabber)

(defface jabber-title-small
  '((t (:weight bold :width semi-expanded :height 1.0 :inherit variable-pitch)))
  "face for small titles"
  :group 'jabber-faces)

(defface jabber-title-medium
  '((t (:weight bold :width expanded :height 2.0 :inherit variable-pitch)))
  "face for medium titles"
  :group 'jabber-faces)

(defface jabber-title-large
  '((t (:weight bold :width ultra-expanded :height 3.0 :inherit variable-pitch)))
  "face for large titles"
  :group 'jabber-faces)

(defgroup jabber-debug nil "debugging options"
  :group 'jabber)

(defcustom jabber-debug-log-xml nil
  "log all XML i/o in *-jabber-xml-log-*"
  :type 'boolean
  :group 'jabber-debug)

(defconst jabber-presence-faces
 '(("" . jabber-roster-user-online)
   ("away" . jabber-roster-user-away)
   ("xa" . jabber-roster-user-xa)
   ("dnd" . jabber-roster-user-dnd)
   ("chat" . jabber-roster-user-chatty)
   ("error" . jabber-roster-user-error)
   (nil . jabber-roster-user-offline))
 "Mapping from presence types to faces")

(defconst jabber-presence-strings
  '(("" . "Online")
    ("away" . "Away")
    ("xa" . "Extended Away")
    ("dnd" . "Do not Disturb")
    ("chat" . "Chatty")
    ("error" . "Error")
    (nil . "Offline"))
  "Mapping from presence types to readable strings")

(defun jabber-customize ()
  "customize jabber options"
  (interactive)
  (customize-group 'jabber))

(defun jabber-info ()
  "open jabber.el manual"
  (interactive)
  (info "jabber"))

(provide 'jabber)

;;; arch-tag: 5145153e-4d19-4dc2-800c-b1282feb155d
