;;; smallurl.el --- Tinify URLs

;; Copyright (C) 2009 Philip Jackson

;; Author: Philip Jackson <phil@shellarchive.co.uk>
;; Version: 0.2

;; This file is not currently part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program ; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Installation:
;;
;; To install put smallurl.el in your load-path and
;; (require 'smallurl) in your initialisation file.

;;; Usage:
;;
;; There are two interactive functions... with which you might want to
;; interact:
;;
;; smallurl-replace-at-point - replace the url at point with a tiny one.
;; smallurl                  - print and put into the kill ring the tiny
;;                             version of the url prompted for.
;;
;; Setting `smallurl-service' will let you choose a service.

;;; Inspired by, and code stolen from:
;;
;; http://www.emacswiki.org/emacs/TinyUrl

(require 'mm-url)

(defvar smallurl-service 'tinyurl
  "The service to use. One of 'tinyurl or 'trim.")

(defvar smallurl-services-map
  '((tinyurl . "http://tinyurl.com/api-create.php?url=")
    (trim    . "http://api.tr.im/api/trim_simple?url="))
  "Alist of tinyfy services.")

(defun smallurl-get (longurl)
  "Tinyfy LONGURL."
  (let ((api (cdr (assoc smallurl-service smallurl-services-map))))
    (unless api
      (error (concat
              "Invalid service try one of "
              (mapconcat '(lambda (x)
                           (symbol-name (car x)))
                         smallurl-services-map ", "))))
    (with-temp-buffer
      (mm-url-insert
       (concat api longurl))
      (buffer-substring (point-min) (point-at-eol)))))

;;;###autoload
(defun smallurl-replace-at-point ()
  "Replace the url at point with a tiny version."
  (interactive)
  (let* ((url-bounds (bounds-of-thing-at-point 'url))
         (url (thing-at-point 'url))
         (newurl (smallurl-get url)))
    (save-restriction
      (narrow-to-region (car url-bounds) (cdr url-bounds))
       (delete-region (point-min) (point-max))
      (insert newurl))
    newurl))

;;;###autoload
(defun smallurl ()
  "Print a tiny version of the url given at prompt. By defualt
will ask you for the url at point, if any."
  (interactive)
  (let ((url (thing-at-point 'url))
        (enable-recursive-minibuffers t)
        (val))
    (setq val (read-from-minibuffer
               (concat "Url"
                       (when url
                         (concat " (" url ")"))
                       ": ")))
    (let ((url (smallurl-get
                (cond
                  ((and (equal val "") url)
                   url)
                  ((> (length val) 0)
                   val)
                  (t
                   (error "No word to lookup"))))))
      (kill-new (message url)))))

(provide 'smallurl)
