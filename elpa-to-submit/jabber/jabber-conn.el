;; jabber-conn.el - Network transport functions

;; Copyright (C) 2005 - Georg Lehner - jorge@magma.com.ni
;; mostly inspired by Gnus.

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

;; A collection of functions, that hide the details of transmitting to
;; and fro a Jabber Server

(eval-when-compile (require 'cl))

;; Try two different TLS/SSL libraries, but don't fail if none available.
(or (ignore-errors (require 'tls))
    (ignore-errors (require 'ssl)))

;; TODO: Add custom flag, to not complain about plain-text passwords
;;       in encrypted connections
;;

;; This variable holds the connection, which is used for further
;; input/output to the server
(defvar *jabber-connection* nil
  "the process that does the actual connection")

(defgroup jabber-conn nil "Jabber Connection Settings"
  :group 'jabber)

(defcustom jabber-network-server nil
  "hostname or IP address of server to connect to, if different from `jabber-server'."
  :type '(radio (const :tag "Same as `jabber-server'" nil)
		(string :tag "Hostname or IP address"))
  :group 'jabber-conn)

(defcustom jabber-port nil
  "jabber port
The default depends on the connection type: 5222 for ordinary connections
and 5223 for SSL connections."
  :type '(choice (const :tag "Default" nil)
		 (integer :tag "Port number"))
  :group 'jabber-conn)

(defcustom jabber-connection-type 'network
  "Type of connection to the jabber server, ssl or network most likely."
  :type '(radio (const :tag "Encrypted connection, SSL" ssl)
		(const :tag "Standard TCP/IP connection" network))
  :group 'jabber-conn)

(defcustom jabber-connection-ssl-program nil
  "Program used for SSL/TLS connections.
nil means prefer gnutls but fall back to openssl.
'gnutls' means use gnutls (through `open-tls-stream').
'openssl means use openssl (through `open-ssl-stream')."
  :type '(choice (const :tag "Prefer gnutls, fall back to openssl" nil)
		 (const :tag "Use gnutls" gnutls)
		 (const :tag "Use openssl" openssl))
  :group 'jabber-conn)

(defvar jabber-connect-methods
  '((network jabber-network-connect jabber-network-send)
    (ssl jabber-ssl-connect jabber-ssl-send))
  "Alist of connection methods and functions.
First item is the symbol naming the method.
Second item is the connect function.
Third item is the send function.")

(defvar jabber-connect-function nil
  "function that connects to the jabber server")

(defvar jabber-conn-send-function nil
  "function that sends a line to the server")

(defun jabber-setup-connect-method ()
  (let ((entry (assq jabber-connection-type jabber-connect-methods)))
    (setq jabber-connect-function (nth 1 entry))
    (setq jabber-conn-send-function (nth 2 entry))))

;; Plain TCP/IP connection
(defun jabber-network-connect ()
    (let ((coding-system-for-read 'utf-8)
	  (coding-system-for-write 'utf-8))
      (setq *jabber-connection* 
	    (open-network-stream
	     "jabber"
	     jabber-process-buffer
	     (or jabber-network-server jabber-server)
	     (or jabber-port 5222)))))

(defun jabber-network-send (string)
  "Send a string via a plain TCP/IP connection to the Jabber Server."
  (process-send-string *jabber-connection* string))

;; SSL connection, we use openssl's s_client function for encryption
;; of the link
;; TODO: make this configurable
(defun jabber-ssl-connect ()
  "connect via OpenSSL or GnuTLS to a Jabber Server"
    (let ((coding-system-for-read 'utf-8)
	  (coding-system-for-write 'utf-8)
	  (connect-function
	   (cond
	    ((and (memq jabber-connection-ssl-program '(nil gnutls))
		  (fboundp 'open-tls-stream))
	     'open-tls-stream)
	    ((and (memq jabber-connection-ssl-program '(nil openssl))
		  (fboundp 'open-ssl-stream))
	     'open-ssl-stream)
	    (t
	     (error "Neither TLS nor SSL connect functions available")))))
      (setq *jabber-connection*
	    (funcall connect-function
		     "jabber"
		     jabber-process-buffer
		     (or jabber-network-server jabber-server)
		     (or jabber-port 5223)))))

(defun jabber-ssl-send (string)
  "Send a string via an SSL-encrypted connection to the Jabber Server,
   it seems we need to send a linefeed afterwards"
  (process-send-string *jabber-connection* string)
  (process-send-string *jabber-connection* "\n"))

(provide 'jabber-conn)
;; arch-tag: f95ec240-8cd3-11d9-9dbf-000a95c2fcd0
