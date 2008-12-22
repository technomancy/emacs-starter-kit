;;; pause.el --- Take a break!
;;
;; Author: Lennart Borgman (lennart O borgman A gmail O com)
;; Created: 2008-01-19T02:51:39+0100 Sat
;; Version: 0.64
;; Last-Updated: 2008-06-15T11:55:21+0200 Sun
;; URL:
;; Keywords:
;; Compatibility:
;;
;; Features that might be required by this library:
;;
;;   None
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; If you are using Emacs then don't you need a little reminder to
;; take a pause? Add to your .emacs
;;
;;   (require 'pause)
;;
;; and do
;;
;;   M-x customize-group RET pause RET
;;
;; and set `pause-mode' to t.
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

(defgroup pause nil
  "Customize your health personal Emacs health saver!"
  :group 'emacs)

(defcustom pause-after-minutes 15
  "Pause after this number of minutes."
  :type 'integer
  :group 'pause)

(defcustom pause-text-color "DarkGoldenrod1"
  "Color of text in pause window."
  :type 'color
  :group 'pause)

(defcustom pause-prompt1-color "DarkOrange1"
  "Background color of first pause prompt."
  :type 'color
  :group 'pause)

(defcustom pause-prompt2-color "GreenYellow"
  "Background color of second pause prompt."
  :type 'color
  :group 'pause)

(defcustom pause-message-color "yellow"
  "Background color of pause messages."
  :type 'color
  :group 'pause)

(defvar pause-timer nil)
(defvar pause-idle-timer nil)

(defun pause-dont-save-me ()
  (when (timerp pause-timer) (cancel-timer pause-timer)))

(defun pause-start-timer (sec)
  (when (timerp pause-idle-timer) (cancel-timer pause-idle-timer))
  (setq pause-idle-timer nil)
  (when (timerp pause-timer) (cancel-timer pause-timer))
  (setq pause-timer (run-with-timer sec nil 'pause-pre-break)))

(defun pause-one-minute ()
  "Give you another minute ..."
  (pause-start-timer 60)
  (message (propertize " OK, I will come back in a minute! -- greatings from pause "
                       'face (list :background pause-message-color))))

(defun pause-save-me ()
  (pause-start-timer (* 60 pause-after-minutes))
  (message (propertize " OK, I will save you again in %d minutes! -- greatings from pause "
                       'face (list :background pause-message-color))
           pause-after-minutes))

(defun pause-ask-user ()
  (if (or isearch-mode
          (active-minibuffer-window))
      (pause-start-timer 10)
    (let* ((map (copy-keymap minibuffer-local-map))
           (minibuffer-local-map map)
           (use-dialog-box nil)
           (minibuffer-prompt-properties
            (copy-sequence minibuffer-prompt-properties))
           (msg1
            (concat
             " :-) Sorry to disturb you!\n\n"
             " Do you want me to take a break now? ... "))
           (msg2
            (concat
             " :-) Take a break now, then come back later and answer!\n\n"
             " Do you want me to save you again? That is my job ... ")))
      ;;(define-key map [(control ?g)] 'ignore)
      (plist-put minibuffer-prompt-properties 'face
                 (list :background pause-prompt1-color))
      (if (yes-or-no-p msg1)
          (progn
            (plist-put minibuffer-prompt-properties 'face
                       (list :background pause-prompt2-color))
            (y-or-n-p msg2))
        'one-minute))))

(defvar pause-idle-delay 15)

(defun pause-pre-break ()
  (setq pause-timer nil)
  (condition-case err
      (if pause-idle-delay
          (setq pause-idle-timer (run-with-idle-timer pause-idle-delay nil 'pause-break-in-timer))
        (setq pause-idle-timer (run-with-timer 0 nil 'pause-break-in-timer)))
    (error
     (lwarn 'pause-pre-break
            :error "%s" (error-message-string err)))))

(defvar pause-break-frmcfg nil)
;;(make-variable-frame-local 'pause-break-frmcfg)

(defvar pause-break-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [(control meta shift ?p)] 'pause-break-mode-exit)
    map))

(defvar pause-break-buffer nil)

(define-derived-mode pause-break-mode nil "Pause"
  "Mode used during pause.

\\[pause-break-mode-exit]"
  (set (make-local-variable 'buffer-read-only) t))

(defun pause-break-mode-exit ()
  (interactive)
  (kill-buffer pause-break-buffer)
  (set-frame-configuration pause-break-frmcfg)
  (pause-save-me))

(defun pause-break ()
  (pause-cancel-timer)
  (setq pause-break-frmcfg (current-frame-configuration))
  (dolist (frm (frame-list))
    (with-selected-frame frm
      (delete-other-windows)
      (setq pause-break-buffer
            (switch-to-buffer (get-buffer-create "* P A U S E *")))
      (when (= 0 (buffer-size))
        (insert (propertize "\n\tHi there,\n\n\tYou are worth a PAUSE!\n"
                            'face (list 'bold
                                        :height 2.0
                                        :foreground pause-text-color)))
        (pause-break-mode)
        (let ((inhibit-read-only t))
          (insert "\n\nTo exit use ")
          (where-is 'pause-break-mode-exit t)))))
  (top-level))

(defun pause-cancel-timer ()
  (when (timerp pause-idle-timer) (cancel-timer pause-idle-timer))
  (setq pause-idle-timer nil))

(defun pause-break-in-timer ()
  (pause-cancel-timer)
  (if (or (active-minibuffer-window)
          edebug-active)
      (let ((pause-idle-delay 5))
        (pause-pre-break))
    (let ((there-was-an-error nil))
      (message "calling break in timer")
      (condition-case err
          (pause-break)
        (error
         (message "pause-break-in-timer: %s" (error-message-string err))
         (setq there-was-an-error t)))
      (when there-was-an-error
        (condition-case err
            (progn
              (select-frame last-event-frame)
              (let ((pause-idle-delay nil))
                (pause-pre-break)))
          (error
           (lwarn 'pause-break-in-timer2 :error "%s" (error-message-string err))
           ))))))

(define-minor-mode pause-mode
  "This minor mode tries to make you take a break!
To customize it see:

 `pause-after-minutes'
 `pause-text-color'
 `pause-prompt1-color'
 `pause-prompt2-color'
 `pause-message-color'
"
  :global t
  :group 'pause
  (if pause-mode
      (pause-save-me)
    (pause-dont-save-me)))

(provide 'pause)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; pause.el ends here
