;; jabber-version.el - version reporting by JEP-0092

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

(require 'jabber-iq)
(require 'jabber-util)

(defconst jabber-version "0.6.1pre"
  "version returned to those who query us")

(add-to-list 'jabber-jid-info-menu
	     (cons "Request software version" 'jabber-get-version))
(defun jabber-get-version (to)
  "Request software version"
  (interactive (list (jabber-read-jid-completing "Request version of: ")))
  ;; XXX: you will not get any result unless you add the resource to the JID.
  (jabber-send-iq to
		  "get"
		  '(query ((xmlns . "jabber:iq:version")))
		  #'jabber-process-data #'jabber-process-version
		  #'jabber-process-data "Version request failed"))

;; called by jabber-process-data
(defun jabber-process-version (xml-data)
  "Handle results from jabber:iq:version requests."
  
  (let ((query (jabber-iq-query xml-data)))
    (dolist (x '((name . "Name:\t\t") (version . "Version:\t") (os . "OS:\t\t")))
      (let ((data (car (jabber-xml-node-children (car (jabber-xml-get-children query (car x)))))))
	(when data
	  (insert (cdr x) data "\n"))))))

(add-to-list 'jabber-iq-get-xmlns-alist (cons "jabber:iq:version" 'jabber-return-version))
(add-to-list 'jabber-advertised-features "jabber:iq:version")
(defun jabber-return-version (xml-data)
  "Return client version as defined in JEP-0092.  Sender and ID are
determined from the incoming packet passed in XML-DATA."
  ;; Things we might check: does this iq message really have type='get' and
  ;; exactly one child, namely query with xmlns='jabber:iq:version'?
  ;; Then again, jabber-process-iq should take care of that.
  (let ((to (jabber-xml-get-attribute xml-data 'from))
	(id (jabber-xml-get-attribute xml-data 'id)))
    (jabber-send-iq to "result"
		    `(query ((xmlns . "jabber:iq:version"))
			    (name () "jabber.el")
			    (version () ,jabber-version)
			    ;; Booting... /vmemacs.el
			    ;; Shamelessly stolen from someone's sig.
			    (os () ,(jabber-escape-xml (emacs-version))))
		    nil nil nil nil
		    id)))

(provide 'jabber-version)

;;; arch-tag: 2051dbe7-01b5-401e-bd8a-fe24afb88e1e
