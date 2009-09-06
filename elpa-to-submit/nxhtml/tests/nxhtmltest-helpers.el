;;; nxhtmltest-helpers.el --- Helper functions for testing
;;
;; Author: Lennart Borgman (lennart O borgman A gmail O com)
;; Created: 2008-07-08T19:10:54+0200 Tue
;; Version: 0.2
;; Last-Updated: 2008-09-01T01:13:15+0200 Sun
;; URL:
;; Keywords:
;; Compatibility:
;;
;; Features that might be required by this library:
;;
;;   `button', `help-fns', `help-mode', `view'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change log:
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(eval-when-compile (require 'cl))
(require 'ert2)

(defun nxhtmltest-mumamo-error-messages ()
  (ert-get-messages "^MU:MuMaMo error"))

(defun nxhtmltest-should-no-mumamo-errors ()
  (ert-should (not (nxhtmltest-mumamo-error-messages))))

(defun nxhtmltest-should-no-nxml-errors ()
  (ert-should (not (ert-get-messages "Internal nXML mode error"))))

(defun nxhtmltest-be-really-idle (seconds &optional prompt-mark)
  (unless prompt-mark (setq prompt-mark ""))
  (with-timeout (4 (message "<<<< %s - not really idle any more at %s"
                            prompt-mark
                            (format-time-string "%H:%M:%S")))
    (let ((prompt (format
                   ">>>> %s Starting beeing really idle %s seconds at %s"
                   prompt-mark
                   seconds
                   (format-time-string "%H:%M:%S ..."))))
      (message "%s" prompt)
      (read-minibuffer prompt)
      (redisplay))))

;;(nxhtmltest-be-really-idle 4 "HERE I AM!!")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Fontification methods

(defvar nxhtmltest-default-fontification-method nil)

(defun nxhtmltest-get-fontification-method ()
  "Ask user for default fontification method."
  (let* ((collection
          '(
            ("Fontify as usual (wait)" fontify-as-usual)
            ("Fontify by calling timer handlers" fontify-w-timer-handlers)
            ("Call fontify-buffer" fontify-buffer)
            ))
         (hist (mapcar (lambda (rec)
                         (car rec))
                       collection))
         (method-name (or t
                          (completing-read "Default fontification method: "
                                           collection nil t
                                           (car (nth 1 collection))
                                           'hist))))
    (setq nxhtmltest-default-fontification-method
          ;;(nth 1 (assoc method-name collection))
          'fontify-w-timer-handlers
          )))

(defun nxhtmltest-fontify-as-usual (seconds prompt-mark)
  (font-lock-mode 1)
  ;; This does not work now since I deleted the function below:
  (error "font-lock-wait not defined")
  ;;(font-lock-wait (nxhtmltest-be-really-idle seconds prompt-mark))
  )

(defun nxhtmltest-fontify-w-timers-handlers ()
    ;;(dolist (timer (copy-list timer-idle-list))
    (dolist (timer (copy-sequence timer-idle-list))
      (timer-event-handler timer))
    (redisplay t))

(defun nxhtmltest-fontify-buffer ()
  (font-lock-fontify-buffer)
  (redisplay t))

(defun nxhtmltest-fontify-default-way (seconds &optional pmark)
  ;;(assert (not font-lock-mode))
  (case nxhtmltest-default-fontification-method
    (fontify-as-usual         (nxhtmltest-fontify-as-usual seconds pmark))
    (fontify-w-timer-handlers (nxhtmltest-fontify-w-timers-handlers))
    (fontify-buffer           (nxhtmltest-fontify-buffer))
    (t (error "Unrecognized default fontification method: %s"
              nxhtmltest-default-fontification-method))))



(provide 'nxhtmltest-helpers)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; nxhtmltest-helpers.el ends here
