;;; hl-needed.el --- Turn on highlighting of line and column when needed
;;
;; Author: Lennart Borgman (lennart O borgman A gmail O com)
;; Created: Fri Nov 30 21:19:18 2007
;; Version: 0.59
;; Last-Updated: 2008-01-21T01:47:44+0100 Mon
;; URL: http://www.emacswiki.org/cgi-bin/wiki/hl-needed.el
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
;; This is yet another highlight line and/or column idea.  The idea is
;; to try to show line and column only when it is probably most
;; needed.  See `hl-needed-mode' for more info.
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

(require 'hl-line)
(require 'vline nil t)

(defcustom hl-needed-always nil
  "Highlight always."
  :type 'boolean
  :group 'hl-needed)

(defcustom hl-needed-mark-line t
  "Highlight line."
  :type 'boolean
  :group 'hl-needed)

(defcustom hl-needed-mark-column t
  "Highlight column."
  :type 'boolean
  :group 'hl-needed)

(defcustom hl-needed-in-readonly-buffers nil
  "Do not highlight in read-only buffers unless non-nil."
  :type 'boolean
  :group 'hl-needed)

(defcustom hl-needed-not-in-modes
  '(wab-compilation-mode
    custom-mode)
  "List of modes where highlighting should not be done."
  :type '(repeat function)
  :group 'hl-needed)

(defcustom hl-needed-idle-time 30
  "Highligh current line and/or column if Emacs is idle for more seconds.
If nil do not turn on `hl-line-mode' when Emacs is idle."
  :type '(choice (const :tag "Don't turn on when Emacs is idle" nil)
                 (integer :tag "Turn on after (seconds)"))
  :group 'hl-needed)

(defcustom hl-needed-on-mouse t
  "Highlight current line and/or column on clicks."
  :type 'boolean
  :group 'hl-needed)

(defcustom hl-needed-on-new-window t
  "Highlight current line and/or column on new window selection."
  :type 'boolean
  :group 'hl-needed)

(defcustom hl-needed-on-new-buffer t
  "Highlight current line and/or column on new buffer selection."
  :type 'boolean
  :group 'hl-needed)

(defcustom hl-needed-on-config-change t
  "Highlight current line and/or column on window conf change."
  :type 'boolean
  :group 'hl-needed)

(defcustom hl-needed-on-scrolling t
  "Highlight current line and/or column after scrolling."
  :type 'boolean
  :group 'hl-needed)

(defcustom hl-needed-flash 0.8
  "Turn off highlighting after this number of second.
Highlighting is turned off only if it was turned on because of
some change. It will not turned off because Emacs was idle.

The default time is choosen to not disturb too much. I believe
human short attention may often be of this time. \(Compare eye
contact time.)"
  :type 'boolean
  :group 'hl-needed)

(defcustom hl-needed-currently-fun 'hl-needed-currently
  "Function that checks if highlighting should be done.
The function should return nil if not needed and non-nil
otherwise."
  :type 'function
  :group 'hl-need)

(defvar hl-needed-timer nil)
(defvar hl-needed-flash-timer nil)
(defvar hl-needed-window nil)
(defvar hl-needed-buffer nil)
(defvar hl-needed-window-start nil)

(defun hl-needed-show ()
  "Highlight current line and/or column now."
  (interactive)
  (when (called-interactively-p)
    (setq hl-needed-flash-this nil)
    (unless hl-needed-mode
      (message "Use hl-needed-hide to remove highlighting")))
  (hl-needed-hide)
  (unless (active-minibuffer-window)
    (unless hl-line-mode
      (when hl-needed-mark-line
        (let ((hl-line-mode t)
              (hl-line-sticky-flag nil))
          (hl-line-highlight))))
    (unless vline-mode
      (when hl-needed-mark-column
        (when (featurep 'vline)
          (let ((vline-style 'face)
                (vline-face hl-line-face)
                (vline-current-window-only t))
            (vline-show)))))))

(defun hl-needed-hide ()
  (interactive)
  (unless hl-line-mode
    (hl-line-unhighlight))
  (when (featurep 'vline)
    (unless vline-mode
      (vline-clear))))

(defun hl-needed-cancel-timer ()
  (when (timerp hl-needed-timer) (cancel-timer hl-needed-timer))
  (setq hl-needed-timer nil))

(defun hl-needed-start-timer ()
  (hl-needed-cancel-timer)
  (setq hl-needed-timer
        (run-with-idle-timer hl-needed-idle-time
                             nil 'hl-needed-show-in-timer)))

(defun hl-needed-show-in-timer ()
  "Turn on with special error handling.
Erros may go unnoticed in timers.  This should prevent it."
  (condition-case err
      (save-match-data ;; runs in timer
        (hl-needed-show))
    (error
     (lwarn 'hl-needed-show
            :error "%s" (error-message-string err)))))

(defun hl-needed-hide-in-timer ()
  "Turn off with special error handling.
Erros may go unnoticed in timers.  This should prevent it."
  (condition-case err
      (unless hl-needed-always
        (hl-needed-hide))
    (error
     (lwarn 'hl-needed-hide
            :error "%s" (error-message-string err)))))

(defun hl-needed-currently ()
  "Check if `hl-line-mode' is needed in buffer."
  ;; Check for change of buffer and window
  (if hl-needed-always
      t
    (unless (or (memq major-mode hl-needed-not-in-modes)
                isearch-mode
                (and buffer-read-only
                     (not hl-needed-in-readonly-buffers)))
      (or (and hl-needed-on-new-window
               (not (eq hl-needed-window (selected-window))))
          ;;(progn (message "here1") nil)
          (and hl-needed-on-new-buffer
               (not (eq hl-needed-buffer (current-buffer))))
          ;;(progn (message "here2") nil)
          (and hl-needed-on-config-change
               hl-needed-config-change)
          ;;(progn (message "here3") nil)
          (and hl-needed-on-mouse
               (listp last-input-event)
               (memq (car last-input-event) '(mouse-1 mouse-2 mouse-3)))
          ;;(progn (message "here4") nil)
          (and hl-needed-on-scrolling
               (and (not (eq hl-needed-window-start (window-start)))
                    (< 1
                       (abs
                        (- (line-number-at-pos hl-needed-window-start)
                           (line-number-at-pos (window-start)))))))))))

(defun hl-needed-cancel-flash-timer ()
    (when (timerp hl-needed-flash-timer) (cancel-timer hl-needed-flash-timer))
    (setq hl-needed-flash-timer nil))

(defvar hl-needed-flash-this nil)

(defun hl-needed-maybe-flash-timer ()
  (when (and hl-needed-flash-this
             (not hl-needed-always))
    (hl-needed-cancel-flash-timer)
    (setq hl-needed-flash-timer (run-with-timer hl-needed-flash nil 'hl-needed-hide-in-timer))))

(defun hl-needed-check ()
  ;; Cancel `hl-line-mode' and timer
  (unless (active-minibuffer-window)
    (if (funcall hl-needed-currently-fun)
        (progn
          ;;(message "HERE last-command=%s, this-command=%s, last-command-event=%s" last-command this-command last-command-event)
          ;; Some time calc for things that pause to show us where we are:
          (let* ((time-pre hl-needed-pre-command-time)
                (time-now (current-time))
                (pre (+ (nth 1 time-pre) (* 0.0000001 (nth 2 time-pre))))
                (now (+ (nth 1 time-now) (* 0.0000001 (nth 2 time-now)))))
            (if (< 1 (- now pre)) ;; Fix-me: option?
                nil ;; Don't show anything here, it just disturbs
              (hl-needed-show)
              (hl-needed-maybe-flash-timer))))
      ;; Submit an idle timer that can turn highlighting on.
      (hl-needed-start-timer)
      ))
    (setq hl-needed-config-change nil)
    (unless (active-minibuffer-window)
      (setq hl-needed-window (selected-window))
      (setq hl-needed-buffer (current-buffer))
      (setq hl-needed-window-start (window-start))))

(defvar hl-needed-pre-command-time (current-time))

(defvar hl-needed-after-active-minibuffer nil)

(defun hl-needed-pre-command ()
  ;;(message "active-minibuffer-window=%s" (active-minibuffer-window))
  (setq hl-needed-after-active-minibuffer (active-minibuffer-window))
  (condition-case err
      (progn
        (hl-needed-cancel-timer)
        (hl-needed-cancel-flash-timer)
        (hl-needed-hide)
        (setq hl-needed-flash-this hl-needed-flash)
        (setq hl-needed-pre-command-time (current-time)))
    (error
     (message "hl-needed-pre-command error: %s" err))))

(defun hl-needed-post-command ()
  (condition-case err
      (if (eq last-command 'keyboard-quit)
          (hl-needed-hide)
        (hl-needed-check))
    (error
     (message "hl-needed-post-command error: %s" err))))

(defvar hl-needed-minibuffer-active nil)

(defvar hl-needed-config-change nil)
(defun hl-needed-config-change ()
  (condition-case err
      (if (active-minibuffer-window)
          (setq hl-needed-minibuffer-active t)
        ;; Changing buffer in the echo area is a config change. Catch this:
        (setq hl-needed-config-change (not hl-needed-after-active-minibuffer))
        (setq hl-needed-after-active-minibuffer nil)
        (setq hl-needed-minibuffer-active nil))
    (error
     (message "hl-needed-config-change error: %s" err))))

(defvar hl-needed-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [(control ?c) ?+] 'hl-needed-show)
    map))

(define-minor-mode hl-needed-mode
  "Try to highlight current line and column when needed.
This can operate in some different ways:

- Highlighting can be on always, see `hl-needed-always'.

Or, it can be turned on depending on some conditions.  In this
case it highlighting is turned off before each command and turned
it on again in the current window when either:

- A new window was selected, see `hl-needed-on-new-window'.
- A new buffer was selected, see `hl-needed-on-new-buffer'.
- Window configuration was changed, see `hl-needed-on-config-change'.
- Buffer was scrolled see `hl-needed-on-scrolling'.
- A window was clicked with the mouse, see `hl-needed-on-mouse'.

In this case highlighting may be turned off again, normally after
a short delay, see `hl-needed-flash'.

If either highlighting was not turned on or was turned off again
it will be turned on when

- Emacs has been idle for `hl-needed-idle-time' seconds.

See also `hl-needed-not-in-modes' and `hl-needed-currently-fun'.

Note 1: For columns to be highlighted vline.el must be available.

Note 2: This mode depends on `hl-line-mode' and `vline-mode' and
tries to cooperate with them. If you turn on either of these that
overrides the variables for turning on the respective
highlighting here."
  :global t
  :group 'hl-needed
  :keymap hl-needed-mode-map
  (if hl-needed-mode
      (progn
        ;;(unless (memq major-mode hl-needed-not-in-modes) (setq hl-needed-window t))
        (add-hook 'post-command-hook 'hl-needed-post-command)
        (add-hook 'pre-command-hook 'hl-needed-pre-command)
        (add-hook 'window-configuration-change-hook 'hl-needed-config-change)
        (hl-needed-show)
        )
    (remove-hook 'post-command-hook 'hl-needed-post-command)
    (remove-hook 'pre-command-hook 'hl-needed-pre-command)
    (remove-hook 'window-configuration-change-hook 'hl-needed-config-change)
    (hl-needed-cancel-timer)
    (hl-needed-cancel-flash-timer)
    (hl-needed-hide)))

(provide 'hl-needed)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; hl-needed.el ends here
