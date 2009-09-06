;;; ert2.el --- Additions to ert.el
;;
;; Author: Lennart Borgman (lennart O borgman A gmail O com)
;; Created: 2008-09-02T11:46:03+0200 Tue
;; Version:
;; Last-Updated: 2009-01-06 Tue
;; URL:
;; Keywords:
;; Compatibility:
;;
;; Features that might be required by this library:
;;
;;   Cannot open load file: ert2.
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

(let* ((this-dir
        (file-name-directory (if load-file-name load-file-name buffer-file-name)))
       ;;(load-path (copy-list load-path)))
       (load-path (copy-sequence load-path)))
  (add-to-list 'load-path this-dir)
  (require 'ert))


(defvar ert-temp-test-buffer-test nil)
(make-variable-buffer-local 'ert-temp-test-buffer-test)
(put 'ert-temp-test-buffer-test 'permanent-local t)

(defvar ert-temp-test-buffer-file nil)
(make-variable-buffer-local 'ert-temp-test-buffer-file)
(put 'ert-temp-test-buffer-file 'permanent-local t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Test buffers

(defvar ert-failed-tests-temp-buffers nil)

(defvar ert-list-failed-buffers-name "*Ert Failed Test Buffers*")

(defun ert-kill-temp-test-buffers ()
  "Delete test buffers from unsuccessful tests."
  (interactive)
  (let ((failed (get-buffer ert-list-failed-buffers-name)))
    (when failed (kill-buffer failed)))
  (dolist (buf ert-failed-tests-temp-buffers)
    (when (buffer-live-p buf)
      (kill-buffer buf)))
  (setq ert-failed-tests-temp-buffers nil))

(defun ert-list-temp-test-buffers ()
  "List test buffers from unsuccessful tests."
  (interactive)
  (setq ert-failed-tests-temp-buffers
        (delq nil
              (mapcar (lambda (buf)
                        (when (buffer-live-p buf)
                          buf))
                      ert-failed-tests-temp-buffers)))
  (let ((ert-buffer (get-buffer "*ert*"))
        (buffers ert-failed-tests-temp-buffers))
    (when ert-buffer (setq buffers (cons ert-buffer buffers)))
    (switch-to-buffer
     (let ((Buffer-menu-buffer+size-width 40))
       (list-buffers-noselect nil buffers)))
    (rename-buffer ert-list-failed-buffers-name t))
  (unless ert-failed-tests-temp-buffers
    (message "No test buffers from unsuccessful tests")))

(defvar ert-temp-test-buffer-minor-mode-map
  (let ((map (make-sparse-keymap)))
    ;; Add menu bar entries for test buffer and test function
    (define-key map [(control ?c) ?? ?t] 'ert-temp-test-buffer-go-test)
    (define-key map [(control ?c) ?? ?f] 'ert-temp-test-buffer-go-file)
    map))
(defun ert-temp-test-buffer-go-test ()
  (interactive)
  (ert-find-test-other-window ert-temp-test-buffer-test))
(defun ert-temp-test-buffer-go-file ()
  (interactive)
  (find-file-other-window ert-temp-test-buffer-file))

(define-minor-mode ert-temp-test-buffer-minor-mode
  "Helpers for those buffers ..."
  )
(put 'ert-temp-test-buffer-minor-mode 'permanent-local t)

;; Fix-me: doc
(defvar ert-test-files-root nil)
(defun ert-get-test-file-name (file-name)
  (unless ert-test-files-root
    (error "Please set ert-test-files-root for your tests"))
  (unless (file-directory-p ert-test-files-root)
    (error "Can't find directory %s" ert-test-files-root))
  (expand-file-name file-name ert-test-files-root))

(defmacro* ert-with-temp-buffer-include-file (file-name-form &body body)
  "Insert FILE-NAME-FORM in a temporary buffer and eval BODY.
If success then delete the temporary buffer, otherwise keep it.

To access these temporary test buffers use
- `ert-list-temp-test-buffers': list them
- `ert-kill-temp-test-buffers': delete them"
  (declare (indent 1) (debug t))
  (let ((file-name (gensym "file-name-")))
    `(let* ((,file-name (ert-get-test-file-name ,file-name-form))
            (mode-line-buffer-identification (list (propertize "%b" 'face 'highlight)))
            ;; Give the buffer a name that allows us to switch to it
            ;; quickly when debugging a failure.
            (temp-buf
             (generate-new-buffer
              (format "%s" (ert-this-test)))))
       (unless (file-readable-p ,file-name)
         (if (file-exists-p ,file-name)
             (error "Can't read %s" ,file-name)
           (error "Can't find %s" ,file-name)))
       (message "Testing with file %s" ,file-name)
       (setq ert-failed-tests-temp-buffers (cons temp-buf ert-failed-tests-temp-buffers))
       (with-current-buffer temp-buf
         (ert-temp-test-buffer-minor-mode 1)
         (setq ert-temp-test-buffer-file ,file-name)
         (setq ert-temp-test-buffer-test (ert-this-test))
         ;; Avoid global font lock
         (let ((font-lock-global-modes nil))
           ;; Turn off font lock in buffer
           (font-lock-mode -1)
           (when (> emacs-major-version 22)
             (assert (not font-lock-mode) t "%s %s" "in ert-with-temp-buffer-include-file"))
           (insert-file-contents ,file-name)
           (save-window-excursion
             ;; Switch to buffer so it will show immediately when
             ;; debugging a failure.
             (switch-to-buffer-other-window (current-buffer))
             ,@body)
           ;; Fix-me: move to success list?
           (kill-buffer temp-buf))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Simulate commands

(defvar ert-simulate-command-delay nil)

(defvar ert-simulate-command-post-hook nil
  "Normal hook to be run at end of `ert-simulate-command'.")

;; Fix-me: use this in all tests where applicable.
(defun ert-simulate-command (command run-idle-timers)
  ;; Fix-me: run-idle-timers - use seconds
  ;; Fix-me: add unread-events
  "Simulate calling command COMMAND as in Emacs command loop.
If RUN-IDLE-TIMERS is non-nil then run the idle timers after
calling everything involved with the command.

COMMAND should be a list where the car is the command symbol and
the rest are arguments to the command.

NOTE: Since the command is not called by `call-interactively'
test for `called-interactively' in the command will fail.

Return the value of calling the command, ie

  (apply (car COMMAND) (cdr COMMAND)).

Run the hook `ert-simulate-command-post-hook' at the very end."

  (message "command=%s" command)
  (ert-should (listp command))
  (ert-should (commandp (car command)))
  (ert-should (not unread-command-events))
  (let (return-value
        (font-lock-mode t))
    ;; For the order of things here see command_loop_1 in keyboard.c
    ;;
    ;; The command loop will reset the command related variables so
    ;; there is no reason to let bind them. They are set here however
    ;; to be able to test several commands in a row and how they
    ;; affect each other.
    (setq deactivate-mark nil)
    (setq this-original-command (car command))
    ;; remap through active keymaps
    (setq this-command (or (command-remapping this-original-command)
                           this-original-command))
    (run-hooks 'pre-command-hook)
    (setq return-value (apply (car command) (cdr command))) ;; <-----
    (message "post-command-hook=%s" post-command-hook)
    (run-hooks 'post-command-hook)
    (when deferred-action-list
      (run-hooks 'deferred_action_function))
    (setq real-last-command (car command))
    (setq last-repeatable-command real-last-command)
    (setq last-command this-command)
    (when (and deactivate-mark transient-mark-mode) (deactivate-mark))
    ;;(message "ert-simulate-command.before idle-timers, point=%s" (point))
    (when run-idle-timers
      ;;(dolist (timer (copy-list timer-idle-list))
      (dolist (timer (copy-sequence timer-idle-list))
        (timer-event-handler timer)
        ;;(message "   after timer=%s, point=%s" timer (point))
        )
      (redisplay t))
    ;;(message "ert-simulate-command.after  idle-timers, point=%s" (point))
    (when ert-simulate-command-delay
      ;; Show user
      ;;(message "After M-x %s" command)
      (let ((old-buffer-name (buffer-name)))
        (rename-buffer (propertize (format "After M-x %s" (car command))
                                   'face 'highlight)
                       t)
        (sit-for ert-simulate-command-delay)
        (rename-buffer old-buffer-name)))
    (ert-should (not unread-command-events))
    (run-hooks 'ert-simulate-command-post-hook)
    return-value))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Misc

(defun ert-this-test ()
  "Return current `ert-deftest' function."
  (elt test 1))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Self tests

(provide 'ert2)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ert2.el ends here
