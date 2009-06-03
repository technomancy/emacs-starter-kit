;;; notify.el --- notification frontend

;; Copyright (C) 2008  Mark A. Hershberger

;; Original Author: Mark A. Hershberger <mhersberger@intrahealth.org>
;; Keywords: extensions, convenience, lisp

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

;;; Commentary:

;; This provides a single function, notify, that will produce a notify
;; pop-up via DBus.

;;; Code:

(defvar notify-last '(0 0 0))

(defvar notify-defaults
  (list :app "GNU Emacs"
        :icon "/usr/share/icons/emacs22/emacs_48.png"
        :timeout 10000
        :urgency "low"
        :category "emacs.message"))

(defvar notify-id 0)

(defvar notify-delay '(0 5 0))

(defvar notify-last-notification '(0 5 0))

;; We could set up other notification methods like notify-via-shell or
;; notify-via-pointer
(defvar notify-method 'notify-via-dbus)

(defun notify-next-id ()
  "Return the next notification id."
  (setq notify-id (+ notify-id 1)))

(defun notify-via-dbus (title body params)
  "Send notification via DBus."
  (when (and (fboundp 'dbus-ping)
             (dbus-ping :session "org.freedesktop.Notifications"))
    (dbus-call-method :session "org.freedesktop.Notifications"
                      "/org/freedesktop/Notifications"
                      "org.freedesktop.Notifications" "Notify"
                      (get 'params :app)
                      (notify-next-id)
                      (get 'params :icon)
                      title
                      body
                      '(:array)
                      '(:array :signature "{sv}")
                      ':int32 (get 'params :timeout))))

(defun notify (title body &rest args)
  "Use pop-up notifications for events."
  (when (and
         (time-less-p notify-delay
                      (time-since notify-last-notification))
         (let ((params))
           (keywords-to-properties 'params args notify-defaults)
           (setq notify-last-notification (current-time))
           (funcall notify-method title body params)))))

(defun keywords-to-properties (symbol args &optional defaults)
  "Convert a list in the form (:keywordA valueA
                               :keywordB valueB ...)
to a list of propertys with the given values"
  (when (car-safe defaults)  ; probably need to avoid recursion
    (keywords-to-properties symbol defaults))
  (while args
      (let ((arg (car args)))
	(setq args (cdr args))
	(unless (symbolp arg)
	  (error "Junk in args %S" args))
	(let ((keyword arg)
	      (value (car args)))
	  (unless args
	    (error "Keyword %s is missing an argument" keyword))
	  (setq args (cdr args))
          (put symbol keyword value)))))

(provide 'notify)

;;; notify.el ends here
