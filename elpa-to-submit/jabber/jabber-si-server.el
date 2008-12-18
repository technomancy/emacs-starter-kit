;; jabber-si-server.el - handle incoming stream requests, by JEP-0095

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
(require 'jabber-disco)
(require 'jabber-feature-neg)

(add-to-list 'jabber-advertised-features "http://jabber.org/protocol/si")

;; Now, stream methods push data to profiles.  It could be the other
;; way around; not sure which is better.
(defvar jabber-si-profiles nil
  "Supported SI profiles.

Each entry is a list, containing:
 * The namespace URI of the profile
 * Accept function, taking entire IQ stanza, and signalling a 'forbidden'
   error if request is declined; returning an XML node to return in
   response, or nil of none needed
 * Data function, taking JID of initiator, stream ID, and string
   containing received data in binary form; receives `nil' on EOF.
   Returns non-nil to keep connection; nil to close it.")

(defvar jabber-si-stream-methods nil
  "Supported SI stream methods.

Each entry is a list, containing:
 * The namespace URI of the stream method
 * Accept function, taking JID of initiator, stream ID, profile
   data function (as above), preparing to accept a request")

(add-to-list 'jabber-iq-set-xmlns-alist
	     (cons "http://jabber.org/protocol/si" 'jabber-si-process))
(defun jabber-si-process (xml-data)

  (let* ((to (jabber-xml-get-attribute xml-data 'from))
	 (id (jabber-xml-get-attribute xml-data 'id))
	 (query (jabber-iq-query xml-data))
	 (profile (jabber-xml-get-attribute query 'profile))
	 (si-id (jabber-xml-get-attribute query 'id))
	 (feature (car (jabber-xml-get-children query 'feature))))
    (message "Receiving SI with profile '%s'" profile)

    (let (stream-method
	  ;; Find profile
	  (profile-data (assoc profile jabber-si-profiles)))
      ;; Now, feature negotiation for stream type (errors
      ;; don't match JEP-0095, so convert)
      (condition-case err
	  (setq stream-method (jabber-fn-intersection
			       (jabber-fn-parse feature 'request)
			       (list (cons "stream-method" (mapcar 'car jabber-si-stream-methods)))))
	(jabber-error
	 (jabber-signal-error "cancel" 'bad-request nil
			      '((no-valid-streams ((xmlns . "http://jabber.org/protocol/si")))))))
      (unless profile-data
	;; profile not understood
	(jabber-signal-error "cancel" 'bad-request nil
			     '((bad-profile ((xmlns . "http://jabber.org/protocol/si"))))))
      (let* ((profile-accept-function (nth 1 profile-data))
	     ;; accept-function might throw a "forbidden" error
	     ;; on user cancel
	     (profile-response (funcall profile-accept-function xml-data))
	     (profile-data-function (nth 2 profile-data))
	     (stream-method-id (nth 1 (assoc "stream-method" stream-method)))
	     (stream-data (assoc stream-method-id jabber-si-stream-methods))
	     (stream-accept-function (nth 1 stream-data)))
	;; prepare stream for the transfer
	(funcall stream-accept-function to si-id profile-data-function)
	;; return result of feature negotiation of stream type
	(jabber-send-iq to "result" 
			`(si ((xmlns . "http://jabber.org/protocol/si"))
			     ,@profile-response
			     (feature ((xmlns . "http://jabber.org/protocol/feature-neg"))
				      ,(jabber-fn-encode stream-method 'response)))
			nil nil nil nil
			id)
	))))

(provide 'jabber-si-server)

;;; arch-tag: d3c75c66-4052-4cf5-8f04-8765adfc8b96
