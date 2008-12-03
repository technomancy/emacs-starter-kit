;;; xml-rpc.el -- An elisp implementation of clientside XML-RPC

;; Copyright (C) 2001 CodeFactory AB.
;; Copyright (C) 2001 Daniel Lundin.
;; Parts Copyright (C) 2002-2005 Mark A. Hershberger

;; Author: Daniel Lundin <daniel@codefactory.se>
;; Maintainer: Mark A. Hershberger <mah@everybody.org>
;; Version: 1.6.4
;; Created: May 13 2001
;; Keywords: xml rpc network
;; URL: http://elisp.info/package/xml-rpc/

;; This file is NOT (yet) part of GNU Emacs.

;; This is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This software is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.


;;; Commentary:

;; This is an XML-RPC client implementation in elisp, capable of both
;; synchronous and asynchronous method calls (using the url package's async
;; retrieval functionality).
;; XML-RPC is remote procedure calls over HTTP using XML to describe the
;; function call and return values.

;; xml-rpc.el represents XML-RPC datatypes as lisp values, automatically
;; converting to and from the XML datastructures as needed, both for method
;; parameters and return values, making using XML-RPC methods fairly
;; transparent to the lisp code.

;; Requirements
;; ------------

;; xml-rpc.el uses the url package for http handling and xml.el for XML
;; parsing. url is a part of the W3 browser package (but now as a separate
;; module in the CVS repository).
;; xml.el is a part of GNU Emacs 21, but can also be downloaded from
;; here: <URL:ftp://ftp.codefactory.se/pub/people/daniel/elisp/xml.el>


;; XML-RPC datatypes are represented as follows
;; --------------------------------------------

;;          int:  42
;; float/double:  42.0
;;       string:  "foo"
;;        array:  '(1 2 3 4)   '(1 2 3 (4.1 4.2))
;;       struct:  '(("name" . "daniel") ("height" . 6.1))


;; Examples
;; ========
;; Here follows some examples demonstrating the use of xml-rpc.el

;; Normal synchronous operation
;; ----------------------------

;; (xml-rpc-method-call "http://localhost:80/RPC" 'foo-method foo bar zoo)

;; Asynchronous example (cb-foo will be called when the methods returns)
;; ---------------------------------------------------------------------

;; (defun cb-foo (foo)
;;   (print (format "%s" foo)))

;; (xml-rpc-method-call-async 'cb-foo "http://localhost:80/RPC"
;;                            'foo-method foo bar zoo)


;; Some real world working examples for fun and play
;; -------------------------------------------------

;; Check the temperature (celsius) outside jonas@codefactory.se's apartment

;; (xml-rpc-method-call
;;      "http://flint.bengburken.net:80/xmlrpc/onewire_temp.php"
;;      'onewire.getTemp)


;; Fetch the latest NetBSD news the past 5 days from O'reillynet

;; (xml-rpc-method-call "http://www.oreillynet.com/meerkat/xml-rpc/server.php"
;;  		     'meerkat.getItems
;;  		     '(("channel" . 1024)
;;  		       ("search" . "/NetBSD/")
;;  		       ("time_period" . "5DAY")
;;  		       ("ids" . 0)
;;  		       ("descriptions" . 200)
;;  		       ("categories" . 0)
;;  		       ("channels" . 0)
;;  		       ("dates" . 0)
;;  		       ("num_items" . 5)))


;;; History:

;; 1.6.2 - Fix whitespace issues to work better with new xml.el
;;         Fix bug in string handling.
;;         Add support for gzip-encoding when needed.

;; 1.6.1 - base64 support added.
;;         url-insert-entities-in-string done on string types now.

;; 1.6 - Fixed dependencies (remove w3, add cl).
;;       Move string-to-boolean and boolean-to-string into xml-rpc namespace.
;;       Fix bug in xml-rpc-xml-to-response where non-existent var was.
;;       More tweaking of "Connection: close" header.
;;       Fix bug in xml-rpc-request-process-buffer so that this works with
;;         different mixes of the url.el code.

;; 1.5.1 - Added Andrew J Cosgriff's patch to make the
;;         xml-rpc-clean-string function work in XEmacs.

;; 1.5 - Added headers to the outgoing url-retreive-synchronously
;;       so that it would close connections immediately on completion.

;; 1.4 - Added conditional debugging code.  Added version tag.

;; 1.2 - Better error handling.  The documentation didn't match
;;       the code.  That was changed so that an error was
;;       signaled.  Also, better handling of various and
;;       different combinations of xml.el and url.el.

;; 1.1 - Added support for boolean types.  If the type of a
;;       returned value is not specified, string is assumed

;; 1.0 - First version


;;; Code:

(defun xml-rpc-clean-string (s)
  (if (string-match "\\`[ \t\n\r]*\\'" s)
					;"^[ \t\n]*$" s)
      nil
    s))

(require 'custom)
(require 'xml)
(require 'url)
(eval-when-compile
  (require 'cl))

(defcustom xml-rpc-load-hook nil
  "*Hook run after loading xml-rpc."
  :type 'hook :group 'xml-rpc)

(defcustom xml-rpc-base64-encode-unicode t
  "If non-nil, then strings with non-ascii characters will be turned
into Base64."
  :type 'boolean :group 'xml-rpc)

(defcustom xml-rpc-base64-decode-unicode t
  "If non-nil, then base64 strings will be decoded using the
utf-8 coding system."
  :type 'boolean :group 'xml-rpc)

(defcustom xml-rpc-debug 0
  "Set this to 1 or greater to avoid killing temporary buffers.
Set it higher to get some info in the *Messages* buffer")

(defconst xml-rpc-version "1.6"
  "Current Version of xml-rpc.el")

;;
;; Value type handling functions
;;

(defun xml-rpc-value-intp (value)
  "Return t if VALUE is an integer."
  (integerp value))

(defun xml-rpc-value-doublep (value)
  "Return t if VALUE is a double precision number."
  (floatp value))

(defun xml-rpc-value-stringp (value)
  "Return t if VALUE is a string."
  (stringp value))

(defun xml-rpc-value-booleanp (value)
  "Return t if VALUE is a boolean"
  (or (eq value nil)
      (eq value t)))

(defun xml-rpc-string-to-boolean (value)
  "Return t if VALUE is a boolean"
  (or (string-equal value "true") (string-equal value "1")))

(defun xml-rpc-caddar-safe (list)
  (car-safe (cdr-safe (cdr-safe (car-safe list)))))

;; An XML-RPC struct is a list where every car is a list of length 1 or 2 and
;; has a string for car.
(defsubst xml-rpc-value-structp (value)
  "Return t if VALUE is an XML-RPC struct."
  (and (listp value)
       (let ((vals value)
	     (result t)
	     curval)
	 (while (and vals result)
	   (setq result (and
			 (setq curval (car-safe vals))
			 (memq (safe-length curval) '(1 2))
			 (stringp (car-safe curval))))
	   (setq vals (cdr-safe vals)))
	 result)))

;; A somewhat lazy predicate for arrays
(defsubst xml-rpc-value-arrayp (value)
  "Return t if VALUE is an XML-RPC struct."
  (and (listp value)
       (not (xml-rpc-value-structp value))))

(defun xml-rpc-xml-list-to-value (xml-list)
  "Convert an XML-RPC structure in an xml.el style XML-LIST to an elisp list, \
interpreting and simplifying it while retaining its structure."
  (cond 
   ((and (xml-rpc-caddar-safe xml-list)
	 (listp (car-safe (cdr-safe (cdr-safe (car-safe xml-list))))))

    (setq valtype (car (caddar xml-list))
	  valvalue (caddr (caddar xml-list)))
    (cond
     ;; Base64
     ((eq valtype 'base64)
      (if xml-rpc-base64-decode-unicode
	  (decode-coding-string (base64-decode-string valvalue) 'utf-8)
	(base64-decode-string valvalue)))
     ;; Boolean
     ((eq valtype 'boolean)
      (xml-rpc-string-to-boolean valvalue))
     ;; String
     ((eq valtype 'string)
      valvalue)
     ;; Integer
     ((eq valtype 'int)
      (string-to-int valvalue))
     ;; Double/float
     ((eq valtype 'double)
      (string-to-number valvalue))
     ;; Struct
     ((eq valtype 'struct)
      (mapcar (lambda (member)
		(let ((membername (cadr (cdaddr member)))
		      (membervalue (xml-rpc-xml-list-to-value (cdddr member))))
		  (cons membername membervalue)))
	      (cddr (caddar xml-list))))
     ;; Fault
     ((eq valtype 'fault)
      (let* ((struct (xml-rpc-xml-list-to-value (list valvalue)))
		   (fault-string (cdr (assoc "faultString" struct)))
		   (fault-code (cdr (assoc "faultCode" struct))))
	      (list 'fault fault-code fault-string)))
           ;; DateTime
           ((eq valtype 'dateTime\.iso8601)
            valvalue)
	   ;; Array
	   ((eq valtype 'array)
	    (mapcar (lambda (arrval)
		(xml-rpc-xml-list-to-value (list arrval)))
	      (cddr valvalue)))))
   ((xml-rpc-caddar-safe xml-list))))

(defun xml-rpc-boolean-to-string (value)
  "Convert a boolean value to a string"
  (if value
      "1"
    "0"))

(defun xml-rpc-value-to-xml-list (value)
  "Return XML representation of VALUE properly formatted for use with the  \
functions in xml.el."
  (cond
					;   ((not value)
					;    nil)
   ((xml-rpc-value-booleanp value)
    `((value nil (boolean nil ,(xml-rpc-boolean-to-string value)))))
   ((listp value)
    (let ((result nil)
	  (xmlval nil))
      (if (xml-rpc-value-structp value)
	  ;; Value is a struct
	  (progn
	    (while (setq xmlval `((member nil (name nil ,(caar value))
					  ,(car (xml-rpc-value-to-xml-list
						 (cdar value)))))
			 result (if t (append result xmlval) (car xmlval))
			 value (cdr value)))
	    `((value nil ,(append '(struct nil) result))))
	;; Value is an array
	(while (setq xmlval (xml-rpc-value-to-xml-list (car value))
		     result (if result (append result xmlval)
			      xmlval)
		     value (cdr value)))
	`((value nil (array nil ,(append '(data nil) result)))))))
   ;; Value is a scalar
   ((xml-rpc-value-intp value)
    `((value nil (int nil ,(int-to-string value)))))
   ((xml-rpc-value-stringp value)
    (let ((charset-list (find-charset-string value)))
      (if (or (and (eq 1 (length charset-list))
		   (eq 'ascii (car charset-list)))
	      (not xml-rpc-base64-encode-unicode))
	  `((value nil (string nil ,(url-insert-entities-in-string value))))
	`((value nil (base64 nil ,(base64-encode-string
				   (encode-coding-string value 'utf-8))))))))
   ((xml-rpc-value-doublep value)
    `((value nil (double nil ,(number-to-string value)))))
   (t
    `((value nil (base64 nil ,(base64-encode-string value)))))))

(defun xml-rpc-xml-to-string (xml)
  "Return a string representation of the XML tree as valid XML markup."
  (let ((tree (xml-node-children xml))
	(result (concat "<" (symbol-name (xml-node-name xml)) ">")))
    (while tree
      (cond
       ((listp (car tree))
	(setq result (concat result (xml-rpc-xml-to-string (car tree)))))
       ((stringp (car tree))
	(setq result (concat result (car tree))))
       (t
	(error "Invalid XML tree")))
      (setq tree (cdr tree)))
    (setq result (concat result "</" (symbol-name (xml-node-name xml)) ">"))
    result))

;;
;; Response handling
;;

(defsubst xml-rpc-response-errorp (response)
  "An 'xml-rpc-method-call'  result value is always a list, where the first \
element in RESPONSE is either nil or if an error occured, a cons pair \
according to (errnum .  \"Error string\"),"
  (eq 'fault (car-safe (caddar response))))

(defsubst xml-rpc-response-error-code (response)
  "Return the error code from RESPONSE."
  (and (xml-rpc-response-errorp response)
       (nth 1 (xml-rpc-xml-list-to-value response))))

(defsubst xml-rpc-response-error-string (response)
  "Return the error code from RESPONSE."
  (and (xml-rpc-response-errorp response)
       (nth 2 (xml-rpc-xml-list-to-value response))))

(defun xml-rpc-xml-to-response (xml)
  "Convert an XML list to a method response list.  An error is
signaled if there is a fault or if the response does not appear
to be an XML-RPC response (i.e. no methodResponse).  Otherwise,
the parsed XML response is returned."
  ;; Check if we have a methodResponse
  (cond
   ((not (eq (car-safe (car-safe xml)) 'methodResponse))
    (error "No methodResponse found"))

   ;; Did we get a fault response
   ((xml-rpc-response-errorp xml)
    (let ((resp (xml-rpc-xml-list-to-value xml)))
      (setq xml-rpc-fault-string (nth 2 resp))
      (setq xml-rpc-fault-code   (nth 1 resp))
      (error "XML-RPC fault `%s'" xml-rpc-fault-string)))
 
   ;; Interpret the XML list and produce a more useful data structure
   (t
    (let ((valpart (cdr (cdaddr (caddar xml)))))
      (xml-rpc-xml-list-to-value valpart)))))

;;
;; Misc
;;

(defun xml-rpc-get-temp-buffer-name ()
  "Get a working buffer name such as ` *XML-RPC-<i>*' without a live process \
and empty it"
  (let ((num 1)
	name buf)
    (while (progn (setq name (format " *XML-RPC-%d*" num)
			buf (get-buffer name))
		  (and buf (or (get-buffer-process buf)
			       (save-excursion (set-buffer buf)
					       (> (point-max) 1)))))
      (setq num (1+ num)))
    name))



;;
;; Method handling
;;

(defun xml-rpc-request (server-url xml &optional async-callback-function)
  "Perform http post request to SERVER-URL using XML.

If ASYNC-CALLBACK-FUNCTION is non-nil, the request will be performed
asynchronously and ASYNC-CALLBACK-FUNCTION should be a callback function to
be called when the reuest is finished.  ASYNC-CALLBACK-FUNCTION is called with
a single argument being an xml.el style XML list.

It returns an XML list containing the method response from the XML-RPC server,
or nil if called with ASYNC-CALLBACK-FUNCTION."
  (unwind-protect
      (save-excursion
	(let ((url-working-buffer (get-buffer-create
				   (xml-rpc-get-temp-buffer-name)))
	      (url-request-method "POST")
	      (url-package-name "xml-rpc.el")
	      (url-package-version xml-rpc-version)
	      (url-request-data (concat "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
					(with-temp-buffer
					  (xml-print xml)
					  (buffer-string))))
	      (url-request-coding-system 'utf-8)
	      (url-http-attempt-keepalives nil)
	      (url-request-extra-headers (list 
					  (cons "Content-Type" "text/xml; charset=utf-8"))))
	  (if (> xml-rpc-debug 1)
	      (print url-request-data (create-file-buffer "request-data")))
	  (set-buffer url-working-buffer)

	  (cond ((boundp 'url-be-asynchronous) ; Sniff for w3 lib capability
		 (if async-callback-function
		     (setq url-be-asynchronous t
			   url-current-callback-data (list
						      async-callback-function
						      (current-buffer))	
			   url-current-callback-func 'xml-rpc-request-callback-handler)
		   (setq url-be-asynchronous nil))
		 (url-retrieve server-url t)

		 (if url-be-asynchronous
		     nil
		   (let ((result (xml-rpc-request-process-buffer
				  url-working-buffer)))
		     (if (> xml-rpc-debug 1) 
			 (save-excursion
			   (set-buffer (create-file-buffer "result-data"))
			   (insert result)))
		     (if (< xml-rpc-debug 1)
			 (kill-buffer (current-buffer)))
		     result)))
		(t			; Post emacs20 w3-el
		 (if async-callback-function
		     (url-retrieve server-url async-callback-function)
		   (let ((buffer (url-retrieve-synchronously server-url))
			 result)
		     (set-buffer buffer)
		     (url-http-parse-headers)
		     (if (> url-http-response-status 299)
			 (error "Error during request: %s"
				url-http-response-status))
		     (url-extract-mime-headers)
		     (setq result (xml-rpc-request-process-buffer buffer))
		     (if (< xml-rpc-debug 1)
			 (kill-buffer buffer))
		     result))))))))


(defun xml-rpc-clean (l)
  (cond
   ((listp l)
    (let ((remain l)
	  elem
	  (result nil))
      (while l
					; iterate
	(setq elem (car l)
	      l (cdr l))
					; test the head
	(cond
					; a string, so clean it.
	 ((stringp elem)
	  (let ((tmp (xml-rpc-clean-string elem)))
	    (if tmp
		(setq result (append result (list tmp)))
	      result)))
					; a list, so recurse.
	 ((listp elem)
	  (setq result (append result (list (xml-rpc-clean elem)))))

					; everthing else, as is.
	 (t
	  (setq result (append result (list elem))))))
      result))

   ((stringp l)			  ; will returning nil be acceptable ?
    elem)

   (t
    l)))

(defun xml-rpc-request-process-buffer (xml-buffer)
  "Process buffer XML-BUFFER."
  (unwind-protect
      (save-excursion
	(set-buffer xml-buffer)
	(when (fboundp 'url-uncompress)
	  (url-uncompress))
	(goto-char (point-min))
	(search-forward-regexp "<\\?xml" nil t)
	(move-to-column 0)
	;; Gather the results
	(let* ((status url-http-response-status)
	       (result (cond
			;; A probable XML response
			((looking-at "<\\?xml ")
			 (xml-rpc-clean (xml-parse-region (point-min) (point-max))))
			  
			;; No HTTP status returned
			((not status)
			 (let ((errstart
				(search-forward "\n---- Error was: ----\n")))
			   (and errstart
				(buffer-substring errstart (point-max)))))

			;; Maybe they just gave us an the XML w/o PI?
			((search-forward "<methodResponse>" nil t)
			 (xml-rpc-clean (xml-parse-region (match-beginning 0)
							  (point-max))))

			;; Valid HTTP status
			(t
			 (int-to-string status)))))
	  result))))


(defun xml-rpc-request-callback-handler (callback-fun xml-buffer)
  "Marshall a callback function request to CALLBACK-FUN with the results \
handled from XML-BUFFER."
  (let ((xml-response (xml-rpc-request-process-buffer xml-buffer)))
    (if (< xml-rpc-debug 1)
	(kill-buffer xml-buffer))
    (funcall callback-fun (xml-rpc-xml-to-response xml-response))))
  

(defun xml-rpc-method-call-async (async-callback-func server-url method
						      &rest params)
  "Call an XML-RPC method asynchronously at SERVER-URL named METHOD with \
PARAMS as parameters. When the method returns, ASYNC-CALLBACK-FUNC will be \
called with the result as parameter."
  (let* ((m-name (if (stringp method)
		     method
		   (symbol-name method)))
	 (m-params (mapcar '(lambda (p)
			      `(param nil ,(car (xml-rpc-value-to-xml-list
						 p))))
			   (if async-callback-func
			       params
			     (car-safe params))))
	 (m-func-call `((methodCall nil (methodName nil ,m-name)
				    ,(append '(params nil) m-params)))))
    (if (> xml-rpc-debug 1)
	(print m-func-call (create-file-buffer "func-call")))
    (xml-rpc-request server-url m-func-call async-callback-func)))

(defun xml-rpc-method-call (server-url method &rest params)
  "Call an XML-RPC method at SERVER-URL named METHOD with PARAMS as \
parameters."
  (let ((response
	 (xml-rpc-method-call-async nil server-url method params)))
    (cond ((stringp response)
	   (list (cons nil (concat "URL/HTTP Error: " response))))
	  (t
	   (xml-rpc-xml-to-response response)))))

(eval-when-compile
  (unless (fboundp 'xml-print)
    (defun xml-debug-print (xml &optional indent-string)
      "Outputs the XML in the current buffer.
XML can be a tree or a list of nodes.
The first line is indented with the optional INDENT-STRING."
      (setq indent-string (or indent-string ""))
      (dolist (node xml)
	(xml-debug-print-internal node indent-string)))

    (defalias 'xml-print 'xml-debug-print)

    (defun xml-debug-print-internal (xml indent-string)
      "Outputs the XML tree in the current buffer.
The first line is indented with INDENT-STRING."
      (let ((tree xml)
	    attlist)
	(insert indent-string ?< (symbol-name (xml-node-name tree)))

	;;  output the attribute list
	(setq attlist (xml-node-attributes tree))
	(while attlist
	  (insert ?\  (symbol-name (caar attlist)) "=\"" (cdar attlist) ?\")
	  (setq attlist (cdr attlist)))

	(setq tree (xml-node-children tree))

	(if (null tree)
	    (insert ?/ ?>)
	  (insert ?>)

	  ;;  output the children
	  (dolist (node tree)
	    (cond
	     ((listp node)
	      (insert ?\n)
	      (xml-debug-print-internal node (concat indent-string "  ")))
	     ((stringp node) (insert node))
	     (t
	      (error "Invalid XML tree"))))

	  (when (not (and (null (cdr tree))
			  (stringp (car tree))))
	    (insert ?\n indent-string))
	  (insert ?< ?/ (symbol-name (xml-node-name xml)) ?>))))))
    
(provide 'xml-rpc)

;;; xml-rpc.el ends here
