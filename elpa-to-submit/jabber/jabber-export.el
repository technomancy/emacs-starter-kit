;;; jabber-export.el --- export Jabber roster to file

;; Copyright (C) 2005  Magnus Henoch

;; Author: Magnus Henoch <mange@freemail.hu>

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

(require 'cl)

(defvar jabber-export-roster-widget nil)

(defvar jabber-import-subscription-p-widget nil)

(defun jabber-export-roster (&optional roster)
  "Create buffer from which roster can be exported to a file."
  (interactive)
  (with-current-buffer (get-buffer-create "Export roster")
    (jabber-init-widget-buffer nil)

    (widget-insert (jabber-propertize "Export roster\n"
				      'face 'jabber-title-large))
    (widget-insert "You are about to save your roster to a file.  Here
you can edit it before saving.  Changes done here will
not affect your actual roster.

")

    (widget-create 'push-button :notify #'jabber-export-save "Save to file")
    (widget-insert " ")
    (widget-create 'push-button :notify #'jabber-export-remove-regexp "Remove by regexp")
    (widget-insert "\n\n")
    (make-local-variable 'jabber-export-roster-widget)

    (jabber-export-display (or roster (jabber-roster-to-sexp *jabber-roster*)))

    (widget-setup)
    (widget-minor-mode 1)
    (goto-char (point-min))
    (switch-to-buffer (current-buffer))))

(defun jabber-import-roster (file)
  "Create buffer for roster import from FILE."
  (interactive "fImport roster from file: ")
  (let ((roster
	 (with-temp-buffer
	   (let ((coding-system-for-read 'utf-8))
	     (jabber-roster-xml-to-sexp
	      (car (xml-parse-file file)))))))
    (with-current-buffer (get-buffer-create "Import roster")
      (jabber-init-widget-buffer nil)
      
      (widget-insert (jabber-propertize "Import roster\n"
					'face 'jabber-title-large))
      (widget-insert "You are about to import the contacts below to your roster.

")
      
      (make-local-variable 'jabber-import-subscription-p-widget)
      (setq jabber-import-subscription-p-widget
	    (widget-create 'checkbox))
      (widget-insert "Adjust subscriptions\n")

      (widget-create 'push-button :notify #'jabber-import-doit "Import to roster")
      (widget-insert " ")
      (widget-create 'push-button :notify #'jabber-export-remove-regexp "Remove by regexp")
      (widget-insert "\n\n")
      (make-local-variable 'jabber-export-roster-widget)
      
      (jabber-export-display roster)

      (widget-setup)
      (widget-minor-mode 1)
      (goto-char (point-min))
      (switch-to-buffer (current-buffer)))))

(defun jabber-export-remove-regexp (&rest ignore)
  (let* ((value (widget-value jabber-export-roster-widget))
	 (length-before (length value))
	 (regexp (read-string "Remove JIDs matching regexp: ")))
    (setq value (delete-if
		 #'(lambda (a)
		     (string-match regexp (nth 0 a)))
		 value))
    (widget-value-set jabber-export-roster-widget value)
    (widget-setup)
    (message "%d items removed" (- length-before (length value)))))

(defun jabber-export-save (&rest ignore)
  "Export roster to file."
  (let ((items (mapcar #'jabber-roster-sexp-to-xml (widget-value jabber-export-roster-widget)))
	(coding-system-for-write 'utf-8))
    (with-temp-file (read-file-name "Export roster to file: ")
      (insert "<iq xmlns='jabber:client'><query xmlns='jabber:iq:roster'>\n")
      (dolist (item items)
	(insert (jabber-sexp2xml item) "\n"))
      (insert "</query></iq>\n"))
    (message "Roster saved")))

(defun jabber-import-doit (&rest ignore)
  "Import roster being edited in widget."
  (let (roster-delta)
    (dolist (n (widget-value jabber-export-roster-widget))
      (let* ((jid (nth 0 n))
	     (name (and (not (zerop (length (nth 1 n))))
			(nth 1 n)))
	     (subscription (nth 2 n))
	     (groups (nth 3 n))
	     (jid-symbol (jabber-jid-symbol jid))
	     (in-roster-p (memq jid-symbol *jabber-roster*))
	     (jid-name (and in-roster-p (get jid-symbol 'name)))
	     (jid-subscription (and in-roster-p (get jid-symbol 'subscription)))
	     (jid-groups (and in-roster-p (get jid-symbol 'groups))))
	;; Do we need to change the roster?
	(when (or
	       ;; If the contact is not in the roster already,
	       (not in-roster-p)
	       ;; or if the import introduces a name,
	       (and name (not jid-name))
	       ;; or changes a name,
	       (and name jid-name (not (string= name jid-name)))
	       ;; or introduces new groups.
	       (set-difference groups jid-groups :test #'string=))
	  (push (jabber-roster-sexp-to-xml
		 (list jid (or name jid-name) nil (union groups jid-groups :test #'string=))
		 t)
		roster-delta))
	;; And adujst subscription.
	(when (widget-value jabber-import-subscription-p-widget)
	  (let ((want-to (member subscription '("to" "both")))
		(want-from (member subscription '("from" "both")))
		(have-to (member jid-subscription '("to" "both")))
		(have-from (member jid-subscription '("from" "both"))))
	    (flet ((request-subscription 
		    (type)
		    (jabber-send-sexp `(presence ((to . ,jid)
						  (type . ,type))))))
	      (cond
	       ((and want-to (not have-to))
		(request-subscription "subscribe"))
	       ((and have-to (not want-to))
		(request-subscription "unsubscribe")))
	      (cond
	       ((and want-from (not have-from))
		;; not much to do here
		)
	       ((and have-from (not want-from))
		(request-subscription "unsubscribed"))))))))
    (when roster-delta
      (jabber-send-iq nil "set"
		      `(query ((xmlns . "jabber:iq:roster")) ,@roster-delta)
		      #'jabber-report-success "Roster import"
		      #'jabber-report-success "Roster import"))))

(defun jabber-roster-to-sexp (roster)
  "Convert ROSTER to simpler sexp format.
Return a list, where each item is a vector:
\[jid name subscription groups]
where groups is a list of strings."
  (mapcar
   #'(lambda (n)
       (list
	(symbol-name n)
	(or (get n 'name) "")
	(get n 'subscription)
	(get n 'groups)))
   roster))

(defun jabber-roster-sexp-to-xml (sexp &optional omit-subscription)
  "Convert SEXP to XML format.
Return an XML node."
  `(item ((jid . ,(nth 0 sexp))
	  ,@(let ((name (nth 1 sexp)))
	      (unless (zerop (length name))
		`((name . ,name))))
	  ,@(unless omit-subscription
	      `((subscription . ,(nth 2 sexp)))))
	 ,@(mapcar
	    #'(lambda (g)
		(list 'group nil g))
	    (nth 3 sexp))))

(defun jabber-roster-xml-to-sexp (xml-data)
  "Convert XML-DATA to simpler sexp format.
XML-DATA is an <iq> node with a <query xmlns='jabber:iq:roster'> child.
See `jabber-roster-to-sexp' for description of output format."
  (assert (eq (jabber-xml-node-name xml-data) 'iq))
  (let ((query (car (jabber-xml-get-children xml-data 'query))))
    (assert query)
    (mapcar
     #'(lambda (n)
	 (list
	  (jabber-xml-get-attribute n 'jid)
	  (or (jabber-xml-get-attribute n 'name) "")
	  (jabber-xml-get-attribute n 'subscription)
	  (mapcar
	   #'(lambda (g)
	       (car (jabber-xml-node-children g)))
	   (jabber-xml-get-children n 'group))))
     (jabber-xml-get-children query 'item))))

(defun jabber-export-display (roster)
  (setq jabber-export-roster-widget
	(widget-create 
	 '(repeat
	   :tag "Roster"
	   (list :format "%v"
		   (string :tag "JID")
		   (string :tag "Name")
		   (choice :tag "Subscription"
			   (const "none")
			   (const "both")
			   (const "to")
			   (const "from"))
		   (repeat :tag "Groups"
			   (string :tag "Group"))))
	 :value roster)))

(provide 'jabber-export)

;;; arch-tag: 9c6b94a9-290a-4c0f-9286-72bd9c1fb8a3
