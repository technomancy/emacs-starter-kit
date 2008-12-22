;;; cgi+.el --- Run CGI inside emacs

;; Copyright (C) 2007 Free Software Foundation, Inc.
;;
;; Author: Ye Wenbin <wenbinye@gmail.com>
;; Maintainer: Ye Wenbin <wenbinye@gmail.com>
;; Created: 15 Dec 2007
;; Version: 0.01
;; Keywords: 

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:
;; Just a toy. Maybe it can help you write html in lisp.

;; Put this file into your load-path and the following into your ~/.emacs:
;;   (require 'cgi+)
;;   (cgi-init)

;;; Code:

(eval-when-compile (require 'cl))

(require 'httpd)
(require 'cgi)

(defvar cgi-bin-dir "cgi-bin"
  "*Directory name for the cgi-bin")
(defvar cgi-request-handler nil
  "*Handler for cgi request.
Use `cgi-add-handler' to add new handler.")

(defvar cgi-path nil)
(defvar cgi-params nil)
(defvar cgi-content nil)

(defun cgi-handler (path cont)
  (let (res head-length)
    (setq cgi-path path
          cgi-content cont
          cgi-params (if (string-match "?" path)
                         (cgi-decode (substring path (match-end 0)))))
    (setq res
          (catch 'result
            (dolist (handler cgi-request-handler)
              (when (string-match (car handler) path)
                (throw 'result
                       (funcall (cdr handler)))))))
    (unless res
      (setq res (concat
                 "Content-type: text/html\n\n"
                 "<html><head><title>Unknown cgi request</title></head>\r\n"
                 "<body> <h1>Unknown cgi request</h1>\r\n"
                 "<pre>\r\n"
                 path "\n" cont
                 "\r\n</pre></body></html>\r\n")))
    (if (string-match "^Content-type: [^\n]+\n\n" res)
        (setq head-length (match-end 0))
      (setq head-length 25
            res (concat
                 "Content-type: text/html\n\n"
                 "<html><head><title>Bad cgi request</title></head>\r\n"
                 "<body> <h1>Bad cgi request</h1>\r\n"
                 "<pre>\r\n"
                 "path=" path "\r\n"
                 "cont=" cont
                 "\r\n</pre></body></html>\r\n")))
    (httpd-send 200 "OK"
                "Server: Emacs/httpd.el" httpd-endl
                "Connection: close" httpd-endl
                "MIME-Version: 1.0" httpd-endl
                "Content-Length: " (number-to-string (- (length res) head-length))
                httpd-endl)
    (httpd-send-data res)
    (setq cgi-path nil
          cgi-content nil
          cgi-params nil)
    t))

(defun cgi-add-handler (regexp handler &optional append)
  (add-to-list 'cgi-request-handler (cons regexp handler) append))

(defun cgi-param (key)
  (cdr (assoc key cgi-params)))

 
;; html generator
(defvar cgi-indent-level 2
  "*Indent level for output html")
(defvar cgi-valid-tags
  '(a base dir font form h1 h2 h3 h4 h5 h6 hr img input link menu ol
      p select table td textarea th ul abbrev acronym address array au
      b big blink blockquote body box br caption center cite code dd
      del dfn div dl dt em head html i ins isindex kbd lang li math meta
      nobr option over person pre q rev s samp small span strong sub
      sup title tr tt u var wbr)
  "*Valid html tags")

(defvar cgi-tag-default-attributes
  '((a :no-endl t)
    (pre :no-indent t))
  "*Default html tag attributes.
:no-endl and :no-indent are special attributes used for pretty output.
:no-endl means the open tag and close tag should apear in same line.
:no-indent means no extra newline and indentation will insert in the
tag(Affect the child tag also).
")

(defvar cgi-write-handler
  '((header . cgi-write-header)
    (start-html . cgi-write-start-html)
    (end-html . cgi-write-end-html)
    (noindent . cgi-write-noindent)
    (reindent . cgi-write-reindent)
    (endl . cgi-write-endl))
  "*Handler for tag in `cgi-write'")

(defvar cgi-indent-depth nil
  "Internal variable")
(defvar cgi-no-indent nil)

(defmacro cgi-write (&rest body)
  (declare (debug t))
  (let (handler)
    `(let ((cgi-indent-depth 0)
           cgi-no-indent)
       ,@(mapcar
          (lambda (tag)
            `(progn
               (cgi-write-tag ',tag)
               (cgi-write-endl)))
          body))))

(defun cgi-dispatch-args (args)
  (let (attr)
    (while (keywordp (car args))
      (push (car args) attr)
      (push (cadr args) attr)
      (setq args (cddr args)))
    (cons (nreverse attr) args)))

;; pretty indent
(defun cgi-make-endl (&optional inc)
  (if inc (setq cgi-indent-depth (+ cgi-indent-depth inc)))
  (or (> cgi-indent-depth 0) (setq cgi-indent-depth 0))
  (concat "\n" (make-string (* cgi-indent-level cgi-indent-depth) #x20)))

(defsubst cgi-write-noindent ()
  "Remove current indentation."
  (if (looking-back "^\\s-+")
      (delete-region (line-end-position 0) (point))))

(defsubst cgi-write-reindent (&optional inc)
  "Reinsert indentation according to adjusted depth."
  (cgi-write-noindent)
  (cgi-write-endl inc))

(defsubst cgi-write-endl (&optional inc)
  (insert (cgi-make-endl (if (listp inc) (car inc) inc))))

;; handlers
(defun cgi-write-header (&optional args)
  (insert "Content-Type: "
          (or (plist-get args :type)
              "text/html")
          "\n\n"))

(defun cgi-write-start-html (&optional args)
  (let (attr arg)
    (setq args (cgi-dispatch-args args)
          attr (car args)
          args (cdr args))
    (insert "<html>" (cgi-make-endl 1)
            "<head>" (cgi-make-endl 1))
    (when (setq arg (plist-get attr :charset))
      (cgi-write-tag `(meta :http-equiv "Content-Type"
                            :content ,(format "text/html; charset=%s" arg)))
      (cgi-write-endl))
    (when (setq arg (plist-get attr :author))
      (cgi-write-tag `(meta :name "author"
                            :content ,arg))
      (cgi-write-endl))
    (when args
      (cgi-write-tag `(title ,(car args))))
    (cgi-write-reindent -1)
    (insert "</head>" (cgi-make-endl)
            "<body>")
    (setq cgi-indent-depth (1+ cgi-indent-depth))))

(defun cgi-write-end-html ()
  (cgi-write-reindent -1)
  (insert "</body>" (cgi-make-endl -1)
          "</html>"))

(defun cgi-write-tag (tag)
  "Generate html for TAG.

The TAG can be one of the type:
 - string: the string will output directly
 - CAR in `cgi-write-handler': call the function in the CDR of the
   associate list with rest arguments
 - CAR in `cgi-valid-tags': recursively output the tag. The tag can
   have syntax like:
     (TAG-NAME :ATTR-NAME1 ATTR1
               ....
               :ATTR-NAMEn ATTRn
               SUBTAGS)
 - any valid form which return a string to output
"
  (cond
   ;; string
   ((stringp tag) (insert tag))
   ;; handler
   ((assoc (car tag) cgi-write-handler)
    (let ((handler (assoc (car tag) cgi-write-handler)))
      (if (cdr tag)
          (funcall (cdr handler) (cdr tag))
        (funcall (cdr handler)))))
   ;; html tag in `cgi-valid-tags'
   ((memq (car tag) cgi-valid-tags)
    (let ((name (car tag))
          (cgi-no-indent cgi-no-indent)
          attr subtags no-endl no-indent)
      (setq tag (cgi-dispatch-args (cdr tag))
            attr (nconc (car tag)
                        (cdr (assoc name cgi-tag-default-attributes)))
            no-endl (plist-get attr :no-endl)
            cgi-no-indent (or cgi-no-indent (plist-get attr :no-indent))
            subtags (cdr tag))
      (if cgi-no-indent (setq no-endl t))
      (if attr
          (let ((str ""))
            (while attr
              ;; :no-endl reserved for tag insert
              (unless (memq (car attr) (list :no-endl :no-indent))
                (setq str (concat str " " (substring (format "%s=\"%s\"" (car attr) (cadr attr)) 1))))
              (setq attr (cddr attr)))
            (setq attr str))
        (setq attr ""))
      (if (null subtags)
          (insert (format "<%S%s />" name attr))
        (insert (format "<%S%s>" name attr))
        (unless no-endl (insert (cgi-make-endl 1)))
        (while subtags
          (cgi-write-tag (car subtags))
          (setq subtags (cdr subtags)))
        (unless no-endl (insert (cgi-make-endl -1)))
        (insert (format "</%S>" name)))))
   ;; emacs lisp form
   (t (insert (eval tag)))))

 
;; An example
(defun cgi-cal ()
  (require 'calendar)
  (with-temp-buffer
    (cgi-write
     (header)
     (start-html "Emacs calendar")
     (h1 :no-endl t "Emacs calendar")
     (pre
      "\n"
      (let ((now (calendar-current-date)) month year)
        (setq month (cgi-param "month")
              year (cgi-param "year"))
        (and month (setq month (string-to-number month)))
        (and year (setq year (string-to-number year)))
        (or (and month (> month 0) (< month 13))
            (setq month (extract-calendar-month now)))
        (or year (setq year (extract-calendar-year now)))
        (with-temp-buffer
          (generate-calendar month year)
          (buffer-string)))
      (endl))
     (end-html))
    (buffer-string)))

(defun cgi-init ()
  (unless httpd-process (httpd-start))
  (httpd-add-handler (concat "^" cgi-bin-dir "/") 'cgi-handler)
  (cgi-add-handler "^cgi-bin/calendar" 'cgi-cal))

(provide 'cgi+)
;;; cgi+.el ends here
