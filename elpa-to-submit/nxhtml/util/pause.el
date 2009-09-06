;;; pause.el --- Take a break!
;;
;; Author: Lennart Borgman (lennart O borgman A gmail O com)
;; Created: 2008-01-19 Sat
(defconst pause:version "0.64");; Version:
;; Last-Updated: 2009-08-04 Tue
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
  (message (propertize " OK, I will come back in a minute! -- greatings from pause"
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
      (save-match-data ;; runs in timer
        (if pause-idle-delay
            (setq pause-idle-timer (run-with-idle-timer pause-idle-delay nil 'pause-break-in-timer))
          (setq pause-idle-timer (run-with-idle-timer 5 nil 'pause-break-in-timer))))
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
  (set (make-local-variable 'buffer-read-only) t)
  ;;(set (make-local-variable 'cursor-type) nil)
  ;; Fix-me: workaround for emacs bug
  (run-with-idle-timer 0 nil 'pause-hide-cursor)
  )

(defun pause-kill-buffer ()
  ;; runs in timer, save-match-data
  (when (buffer-live-p pause-break-buffer) (kill-buffer pause-break-buffer)))

(defvar pause-break-was-in-minibuffer nil)
(defun pause-break-mode-exit-2 ()
  ;;(message "pause-break-mode-exit-2, active-minibuffer-window=%s" (active-minibuffer-window))
  (if (active-minibuffer-window)
      (setq pause-break-was-in-minibuffer t)
    (unless pause-break-was-in-minibuffer
      ;;(message "pause-break-mode-exit-2 exit state")
      (dolist (win (get-buffer-window-list pause-break-buffer nil t))
        (set-window-margins win 0 0))
      (remove-hook 'window-configuration-change-hook 'pause-break-mode-exit-2)
      ;;(when (buffer-live-p pause-break-buffer) (kill-buffer pause-break-buffer))
      ;; Fix-me: This is a work around for an emacs crash
      (run-with-idle-timer 0 nil 'pause-kill-buffer)
      (pause-save-me))
    (setq pause-break-was-in-minibuffer nil)
    ))

(defun pause-break-mode-exit ()
  (interactive)
  (when (buffer-live-p pause-break-buffer)
    (kill-buffer pause-break-buffer))
  (set-frame-configuration pause-break-frmcfg)
  (pause-save-me))

(defcustom pause-break-text
  (concat "\n\tHi there,"
          "\n\tYou are worth a PAUSE!"
          "\n\nTry some mindfulness:"
          "\n\t- Look around and observe."
          "\n\t- Listen."
          "\n\t- Feel your body.")
  "Text to show during pause."
  :type 'integer
  :group 'pause)

(defvar pause-default-img-dir
  (let* ((this-file (or load-file-name
                       buffer-file-name))
         (this-dir (file-name-directory this-file)))
    (expand-file-name "../etc/img/pause/" this-dir)))

(defcustom pause-img-dir pause-default-img-dir
  "Image directory for pause."
  :type 'directory
  :group 'pause)

(defun pause-insert-img ()
  (let* ((inhibit-read-only t)
        img
        src
        (slice '(0 0 200 300))
        (imgs (directory-files pause-img-dir nil nil t))
        skip
        )
    (setq imgs (delete nil
                       (mapcar (lambda (d)
                                 (unless (file-directory-p d) d))
                               imgs)))
    ;;(message "imgs=%s" imgs)
    (setq skip (random (length imgs)))
    (while (> skip 0)
      (setq skip (1- skip))
      (setq imgs (cdr imgs)))
    (setq src (expand-file-name (car imgs) pause-img-dir))
    (if (file-exists-p src)
        (condition-case err
            (setq img (create-image src nil nil
                                    :relief 1
                                    ;;:margin inlimg-margins
                                    ))
          (error (setq img (error-message-string err))))
      (setq img (concat "Image not found: " src)))
    (if (stringp img)
        (insert img)
      (insert-image img nil 'left-margin slice)
      )
    ;;(message "After insert img=%s" img)
    ))

(defun pause-hide-cursor ()
  ;; runs in timer, save-match-data
  (with-current-buffer pause-break-buffer
    (set (make-local-variable 'cursor-type) nil)))

(defun pause-add-to-conf-hook ()
  ;; runs in timer, save-match-data
  (add-hook 'window-configuration-change-hook 'pause-break-mode-exit-2))

(defun pause-break ()
  ;;(message "pause-break")
  (pause-cancel-timer)
  (setq pause-break-frmcfg (current-frame-configuration))
  (dolist (frm (frame-list))
    (with-selected-frame frm
      (delete-other-windows)
      (setq pause-break-buffer
            (switch-to-buffer (get-buffer-create "* P A U S E *")))
      (set-window-margins (selected-window) 25 0)
      (when (= 0 (buffer-size))
        (pause-break-mode)
        (pause-insert-img)
        ;;(insert (propertize "\n\tHi there,\n\n\tYou are worth a PAUSE!\n"
        (let ((inhibit-read-only t))
          (insert (propertize pause-break-text
                              'face (list 'bold
                                          :height 1.5
                                          :foreground pause-text-color)))
          (insert (propertize "\n\nTo exit switch buffer\n"
                              'face (list :foreground "lawn green")))
          ;;(where-is 'pause-break-mode-exit t)
          )
        (goto-char 1)
        )))
  (run-with-idle-timer 0 nil 'pause-add-to-conf-hook)
  (setq pause-break-was-in-minibuffer (active-minibuffer-window))
  ;;(message "pause-break-was-in-minibuffer before top-level=%s" pause-break-was-in-minibuffer)
  (top-level)
  )

(defun pause-cancel-timer ()
  (when (timerp pause-idle-timer) (cancel-timer pause-idle-timer))
  (setq pause-idle-timer nil))

(defun pause-break-in-timer ()
  (save-match-data ;; runs in timer
    (pause-cancel-timer)
    (if (or (active-minibuffer-window)
            (and (boundp 'edebug-active)
                 edebug-active))
        (let ((pause-idle-delay 5))
          (pause-pre-break))
      (let ((there-was-an-error nil))
        ;;(message "calling break in timer")
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
             )))))))

;;;###autoload
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
