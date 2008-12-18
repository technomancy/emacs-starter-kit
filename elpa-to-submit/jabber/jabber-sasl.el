;; jabber-sasl.el - SASL authentication

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

(require 'cl)

;;; This file uses sasl.el from FLIM, and expects to find it.  If it
;;; can't be found, jabber-core.el catches the error.
(require 'sasl)

;;; Alternatives to FLIM would be the command line utility of GNU SASL,
;;; or anything the Gnus people decide to use.

;;; See XMPP-CORE and XMPP-IM for details about the protocol.

(require 'jabber-xml)

(defvar jabber-sasl-mechanism nil)
(defvar jabber-sasl-client nil)
(defvar jabber-sasl-step nil)

(defun jabber-sasl-start-auth (stream-features)
  ;; This shouldn't be necessary
  ;;(setq jabber-call-on-connection nil)

  ;; Reset our own state.
  (setq jabber-sasl-mechanism nil)
  (setq jabber-sasl-client nil)
  (setq jabber-sasl-step nil)

  ;; Hijack all stanzas for a while.
  (setq jabber-short-circuit-input #'jabber-sasl-process-input)

  ;; Find a suitable common mechanism.
  (let ((mechanisms (car (jabber-xml-get-children stream-features 'mechanisms))))
    (setq jabber-sasl-mechanism
	  (sasl-find-mechanism 
	   (mapcar
	    (lambda (tag)
	      (car (jabber-xml-node-children tag)))
	    (jabber-xml-get-children mechanisms 'mechanism)))))
  (if (null jabber-sasl-mechanism)
      ;; Maybe we can use legacy authentication
      (let ((node (find "http://jabber.org/features/iq-auth"
			(jabber-xml-get-children stream-features 'auth)
			:key #'(lambda (node) (jabber-xml-get-attribute node 'xmlns))
			:test #'string=)))
	(if node
	    (progn
	      (setq jabber-short-circuit-input nil)
	      (jabber-get-auth jabber-server))
	  (error "No suitable SASL mechanism found")))

    ;; Start authentication.
    (setq jabber-sasl-client (sasl-make-client jabber-sasl-mechanism jabber-username "xmpp" jabber-server))
    (setq jabber-sasl-step (sasl-next-step jabber-sasl-client nil))
    (jabber-send-sexp
     `(auth ((xmlns . "urn:ietf:params:xml:ns:xmpp-sasl")
	     (mechanism . ,(sasl-mechanism-name jabber-sasl-mechanism)))
	    ,(when (sasl-step-data jabber-sasl-step)
	       (base64-encode-string (sasl-step-data jabber-sasl-step) t))))))

(defun jabber-sasl-stop ()
  (setq jabber-short-circuit-input nil))

(defun jabber-sasl-process-input (xml-data)
  (let ((sasl-read-passphrase #'jabber-read-passwd))
    (cond
     ((eq (car xml-data) 'challenge)
      (sasl-step-set-data jabber-sasl-step (base64-decode-string (car (jabber-xml-node-children xml-data))))
      (setq jabber-sasl-step (sasl-next-step jabber-sasl-client jabber-sasl-step))
      (jabber-send-sexp
       `(response ((xmlns . "urn:ietf:params:xml:ns:xmpp-sasl"))
		  ,(when (sasl-step-data jabber-sasl-step)
		     (base64-encode-string (sasl-step-data jabber-sasl-step) t)))))

     ((eq (car xml-data) 'failure)
      (ding)
      (message "SASL authentication failure: %s"
	       (jabber-xml-node-name (car (jabber-xml-node-children xml-data))))
      (sit-for 3)
      (jabber-disconnect)
      (jabber-sasl-stop))

     ((eq (car xml-data) 'success)
      (message "Authentication succeeded")
      (setq *jabber-authenticated* t)
      (jabber-sasl-stop)

      ;; Now, we send another stream header.
      (funcall jabber-conn-send-function
	       (concat
		"<stream:stream to='"
		jabber-server
		"' xmlns='jabber:client' xmlns:stream='http://etherx.jabber.org/streams' version='1.0'>"))
      ;; now see what happens
))))

(provide 'jabber-sasl)
;;; arch-tag: 2a4a234d-34d3-49dd-950d-518c899c0fd0
