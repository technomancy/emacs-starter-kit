;; jabber-browse.el - jabber browsing by JEP-0011

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
(require 'jabber-xml)
(require 'jabber-util)

;; jabber.el can perform browse requests, but will not answer them.

(add-to-list 'jabber-jid-info-menu
	     (cons "Send browse query" 'jabber-get-browse))
(defun jabber-get-browse (to)
  "send a browse infoquery request to someone"
  (interactive (list (jabber-read-jid-completing "browse: ")))
  (jabber-send-iq to 
                  "get"
                  '(query ((xmlns . "jabber:iq:browse")))
                  #'jabber-process-data #'jabber-process-browse
		  #'jabber-process-data "Browse failed"))

;; called from jabber-process-data
(defun jabber-process-browse (xml-data)
  "Handle results from jabber:iq:browse requests."
  (dolist (item (jabber-xml-node-children xml-data))
    (when (and (listp item)
	       (not (eq (jabber-xml-node-name item) 'ns)))
      (let ((jid (jabber-xml-get-attribute item 'jid))
	    (beginning (point)))
	(cond
	 ((or
	   (eq (jabber-xml-node-name item) 'user)
	   (string= (jabber-xml-get-attribute item 'category) "user"))
	  (insert (jabber-propertize "$ USER"
			      'face 'jabber-title-medium)
		  "\n\n"))
	 ((or
	   (eq (jabber-xml-node-name item) 'service)
	   (string= (jabber-xml-get-attribute item 'category) "service"))
	  (insert (jabber-propertize "* SERVICE"
			      'face 'jabber-title-medium)
		  "\n\n"))
	 ((or
	   (eq (jabber-xml-node-name item) 'conference)
	   (string= (jabber-xml-get-attribute item 'category) "conference"))
	  (insert (jabber-propertize "@ CONFERENCE"
			      'face 'jabber-title-medium)
		  "\n\n"))
	 (t
	  ;; So far I've seen "server" and "directory", both in the node-name.
	  ;; Those are actually service disco categories, but jabberd 2 seems
	  ;; to use them for browse results as well.  It's not right (as in
	  ;; JEP-0011), but it's reasonable.
	  (let ((category (jabber-xml-get-attribute item 'category)))
	    (if (= (length category) 0)
		(setq category (jabber-xml-node-name item)))
	    (insert (jabber-propertize (format "! OTHER: %s" category)
				'face 'jabber-title-medium)
		    "\n\n"))))
	(dolist (attr '((type . "Type:\t\t")
			(jid . "JID:\t\t")
			(name . "Name:\t\t")
			(version . "Version:\t")))
	  (let ((data (jabber-xml-get-attribute item (car attr))))
	    (if (> (length data) 0)
		(insert (cdr attr) (jabber-unescape-xml data) "\n"))))

	(dolist (ns (jabber-xml-get-children item 'ns))
	  (if (stringp (car (jabber-xml-node-children ns)))
	      (insert "Namespace:\t" (car (jabber-xml-node-children ns)) "\n")))

	(insert "\n")
	(put-text-property beginning (point) 'jabber-jid jid)

	;; XXX: Is this kind of recursion really needed?
	(if (listp (car (jabber-xml-node-children item)))
	    (jabber-process-browse item))))))

(provide 'jabber-browse)

;;; arch-tag: be01ab34-96eb-4fcb-aa35-a0d3e6c446c3
