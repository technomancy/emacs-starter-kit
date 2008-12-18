;; jabber-logon.el - logon functions

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

(require 'jabber-xml)
(require 'jabber-util)
;; sha1-el is known under two names
(condition-case e
    (require 'sha1)
  (error (require 'sha1-el)))

(defun jabber-get-auth (to)
  "Send IQ get request in namespace \"jabber:iq:auth\"."
  (jabber-send-iq to
		  "get"
		  `(query ((xmlns . "jabber:iq:auth"))
			  (username () ,jabber-username))
		  #'jabber-do-logon nil
		  #'jabber-report-success "Impossible error - auth field request"))

(defun jabber-do-logon (xml-data closure-data)
  "send username and password in logon attempt"
  (cond
   ((string= (jabber-xml-get-attribute xml-data 'type) "result")
    (let (auth)
      (if (jabber-xml-get-children (jabber-iq-query xml-data) 'digest)
	  ;; SHA1 digest passwords allowed
	  (let ((passwd (jabber-read-passwd)))
	    (if passwd
		(setq auth `(digest () ,(sha1 (concat jabber-session-id passwd))))))
	(if (yes-or-no-p "Jabber server only allows cleartext password transmission!  Continue? ")
	    (let ((passwd (jabber-read-passwd)))
	      (if passwd
		  (setq auth `(password () ,passwd))))))
      
      ;; If auth is still nil, user cancelled process somewhere
      (if auth
	  (jabber-send-iq jabber-server
			  "set"
			  `(query ((xmlns . "jabber:iq:auth"))
				  (username () ,jabber-username)
				  ,auth
				  (resource () ,jabber-resource))
			  #'jabber-process-logon t
			  #'jabber-process-logon nil)
	(jabber-disconnect))))
   (t
    (error "Logon error ended up in the wrong place"))))

(defun jabber-process-logon (xml-data closure-data)
  "receive login success or failure, and request roster.
CLOSURE-DATA should be t on success and nil on failure."
  (if closure-data
      ;; Logon success
      (progn
	(setq *jabber-authenticated* t)
	(jabber-send-iq nil
			"get" 
			'(query ((xmlns . "jabber:iq:roster")))
			#'jabber-process-roster 'initial
			#'jabber-report-success "Roster retrieval")

	(run-hooks 'jabber-post-connect-hook))

    ;; Logon failure
    (jabber-report-success xml-data "Logon")
    (jabber-disconnect)))

(provide 'jabber-logon)

;;; arch-tag: f24ebe5e-3420-44bb-af81-d4de21f378b0
