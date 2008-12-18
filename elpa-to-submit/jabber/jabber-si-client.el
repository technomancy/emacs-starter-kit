;; jabber-si-client.el - send stream requests, by JEP-0095

;; Copyright (C) 2004 - Magnus Henoch - mange@freemail.hu

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
(require 'jabber-feature-neg)

(defvar jabber-si-client-methods nil
  "Supported SI stream methods for initiation.

Each entry is a list, containing:
 * The namespace URI of the stream method
 * A function taking three arguments: JID, SID and profile function to call")

(defun jabber-si-initiate (jid profile-namespace profile-data profile-function &optional mime-type)
  "Try to initiate a stream to JID.
PROFILE-NAMESPACE is, well, the namespace of the profile to use.
PROFILE-DATA is the XML data to send within the SI request.
PROFILE-FUNCTION is the function to call upon success.
MIME-TYPE is the MIME type to specify.
Returns the SID."

  (let ((sid (apply 'format "emacs-sid-%d.%d.%d" (current-time))))
    (jabber-send-iq jid "set"
		    `(si ((xmlns . "http://jabber.org/protocol/si")
			  (id . ,sid)
			  ,(if mime-type
			       (cons 'mime-type mime-type))
			  (profile . ,profile-namespace))
			 ,profile-data
			 (feature ((xmlns . "http://jabber.org/protocol/feature-neg"))
				  ,(jabber-fn-encode (list
						      (cons "stream-method"
							    (mapcar 'car jabber-si-client-methods)))
						     'request)))
		    #'jabber-si-initiate-process (cons profile-function sid)
		    ;; XXX: use other function here?
		    #'jabber-report-success "Stream initiation")
    sid))

(defun jabber-si-initiate-process (xml-data closure-data)
  "Act on response to our SI query."

  (let* ((profile-function (car closure-data))
	 (sid (cdr closure-data))
	 (from (jabber-xml-get-attribute xml-data 'from))
	 (query (jabber-iq-query xml-data))
	 (feature-node (car (jabber-xml-get-children query 'feature)))
	 (feature-alist (jabber-fn-parse feature-node 'response))
	 (chosen-method (cadr (assoc "stream-method" feature-alist)))
	 (method-data (assoc chosen-method jabber-si-client-methods)))
    ;; Our work is done.  Hand it over to the stream method.
    (let ((stream-negotiate (nth 1 method-data)))
      (funcall stream-negotiate from sid profile-function))))

(provide 'jabber-si-client)

;;; arch-tag: e14ec451-3f18-4f36-b92a-e8a8aa1f5acd
