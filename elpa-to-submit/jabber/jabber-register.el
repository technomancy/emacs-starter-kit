;; jabber-register.el - registration according to JEP-0077

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
(require 'jabber-widget)

(add-to-list 'jabber-jid-service-menu
	     (cons "Register with service" 'jabber-get-register))
(defun jabber-get-register (to)
  "Send IQ get request in namespace \"jabber:iq:register\"."
  (interactive (list (jabber-read-jid-completing "Register with: ")))
  (jabber-send-iq to
		  "get"
		  '(query ((xmlns . "jabber:iq:register")))
		  #'jabber-process-data #'jabber-process-register-or-search
		  #'jabber-report-success "Registration"))

(defun jabber-process-register-or-search (xml-data)
  "Display results from jabber:iq:{register,search} query as a form."

  (let ((query (jabber-iq-query xml-data))
	(have-xdata nil)
	(type (cond
	       ((string= (jabber-iq-xmlns xml-data) "jabber:iq:register")
		'register)
	       ((string= (jabber-iq-xmlns xml-data) "jabber:iq:search")
		'search)
	       (t
		(error "Namespace %s not handled by jabber-process-register-or-search" (jabber-iq-xmlns xml-data))))))
	       
    (cond
     ((eq type 'register)
      ;; If there is no `from' attribute, we are registering with the server
      (jabber-init-widget-buffer (or (jabber-xml-get-attribute xml-data 'from) jabber-server)))

     ((eq type 'search)
      ;; no such thing here
      (jabber-init-widget-buffer (jabber-xml-get-attribute xml-data 'from))))

    (widget-insert (if (eq type 'register) "Register with " "Search ") jabber-submit-to "\n\n")
    (when (and (eq type 'register)
	     jabber-register-p)
	(widget-insert "Don't change the username here unless you also change ")
	(widget-create 'link
		       :notify (lambda (&rest ignore)
				 (customize-variable 'jabber-username))
		       "jabber-username")
	(widget-insert ".\n\n"))

    (dolist (x (jabber-xml-get-children query 'x))
      (when (string= (jabber-xml-get-attribute x 'xmlns) "jabber:x:data")
	(setq have-xdata t)
	;; If the registration form obeys JEP-0068, we know
	;; for sure how to put a default username in it.
	(jabber-render-xdata-form x
				  (if (and jabber-register-p
					   (string= (jabber-xdata-formtype x) "jabber:iq:register"))
				      (list (cons "username" jabber-username))
				    nil))))
    (if (not have-xdata)
	(jabber-render-register-form query))

    (widget-create 'push-button :notify (if (eq type 'register)
					    #'jabber-submit-register
					  #'jabber-submit-search) "Submit")
    (when (eq type 'register)
      (widget-insert "\t")
      (widget-create 'push-button :notify #'jabber-remove-register "Cancel registration"))
    (widget-insert "\n")
    (widget-setup)
    (widget-minor-mode 1)))

(defun jabber-submit-register (&rest ignore)
  "Submit registration input.  See `jabber-process-register-or-search'."
  
  (let ((handler (if jabber-register-p 
		     #'jabber-process-register-secondtime
		   #'jabber-report-success))
	(text (concat "Registration with " jabber-submit-to)))
    (jabber-send-iq jabber-submit-to
		    "set"

		    (cond
		     ((eq jabber-form-type 'register)
		      `(query ((xmlns . "jabber:iq:register"))
			      ,@(jabber-parse-register-form)))
		     ((eq jabber-form-type 'xdata)
		      `(query ((xmlns . "jabber:iq:register"))
			      ,(jabber-parse-xdata-form)))
		     (t
		      (error "Unknown form type: %s" jabber-form-type)))
		    handler (if jabber-register-p 'success text)
		    handler (if jabber-register-p 'failure text)))

  (message "Registration sent"))

(defun jabber-process-register-secondtime (xml-data closure-data)
  "Receive registration success or failure.
CLOSURE-DATA is either 'success or 'error."
  (setq jabber-register-p nil)
  (cond
   ((eq closure-data 'success)
    (message "Registration successful.  Your JID is %s@%s."
	     jabber-username jabber-server)
    (sit-for 3)
    (jabber-get-auth jabber-server))
   (t
    (jabber-report-success xml-data "Account registration")
    (sit-for 3)
    (jabber-disconnect))))

(defun jabber-remove-register (&rest ignore)
  "Cancel registration.  See `jabber-process-register-or-search'."

  (if (yes-or-no-p (concat "Are you sure that you want to cancel your registration to " jabber-submit-to "? "))
      (jabber-send-iq jabber-submit-to
		      "set"
		      '(query ((xmlns . "jabber:iq:register"))
			      (remove))
		      #'jabber-report-success "Unregistration"
		      #'jabber-report-success "Unregistration")))

(provide 'jabber-register)

;;; arch-tag: e6b349d6-b1ad-4d19-a412-74459dfae239
