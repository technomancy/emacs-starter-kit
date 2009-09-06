;;; udev.el --- Helper functions for updating from dev sources
;;
;; Author: Lennart Borgman (lennart O borgman A gmail O com)
;; Created: 2008-08-24
(defconst udev:version "0.5");; Version:
;; Last-Updated: 2009-01-06 Tue
;; URL:
;; Keywords:
;; Compatibility:
;;
;; Features that might be required by this library:
;;
  ;; `cus-edit', `cus-face', `cus-load', `cus-start', `wid-edit'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; When you want to fetch and install sources from a repository you
;; may have to call several async processes and wait for the answer
;; before calling the next function.  These functions may help you with
;; this.
;;
;; See `udev-call-first-step' for more information.  Or look in the
;; file udev-cedet.el for examples.
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

(require 'cus-edit)

;;; Control/log buffer

(defvar udev-log-buffer nil
  "Log buffer pointer for sentinel function.")
(make-variable-buffer-local 'udev-log-buffer)

(defvar udev-is-log-buffer nil
  "This is t if this is an udev log/control buffer.")
(make-variable-buffer-local 'udev-is-log-buffer)

(defun udev-check-is-log-buffer (buffer)
  "Check that BUFFER is an udev log/control buffer."
  (with-current-buffer buffer
    (unless udev-is-log-buffer
      (error "Internal error, not a log buffer: %s" buffer))))

(defvar udev-this-chain nil)
(make-variable-buffer-local 'udev-this-chain)

(defvar udev-last-error nil
  "Error found during last step.")
(make-variable-buffer-local 'udev-last-error)

(defun udev-set-last-error (log-buffer msg)
  (with-current-buffer log-buffer
    (setq udev-last-error msg)))

;;; Chain utils

(defun udev-chain (log-buffer)
  "Return value of `udev-this-chain' in buffer LOG-BUFFER."
  (udev-check-is-log-buffer log-buffer)
  (with-current-buffer log-buffer
    udev-this-chain))

(defun udev-this-step (log-buffer)
  "Return current function to call from LOG-BUFFER."
  (let ((this-chain (udev-chain log-buffer)))
    (caar this-chain)))

(defun udev-goto-next-step (log-buffer)
  "Set next function as current in LOG-BUFFER."
  (let* ((this-chain (udev-chain log-buffer))
        (this-step (car this-chain)))
    (setcar this-chain (cdr this-step))))

(defun udev-num-steps (log-buffer)
  "Return number of steps."
  (length (nth 2 (udev-chain log-buffer))))

(defun udev-step-num (log-buffer)
  "Return current step number."
  (let ((this-chain (udev-chain log-buffer)))
    (when this-chain
      (1+ (- (udev-num-steps log-buffer)
             (length (car this-chain)))))))

(defun udev-finish-function (log-buffer)
  "Return setup function to be called when finished."
  (nth 3 (udev-chain log-buffer)))


(defvar udev-control-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map button-buffer-map)
    map))

(define-derived-mode udev-control-mode nil
  "Udev-Src"
  "Mode for udev control buffer."
  (setq show-trailing-whitespace nil)
  (setq buffer-read-only t)
  (nxhtml-minor-mode 1))

;;; Calling steps

;;;###autoload
(defun udev-call-first-step (log-buffer steps header finish-fun)
  "Set up and call first step.
Set up buffer LOG-BUFFER to be used for log messages and
controling of the execution of the functions in list STEPS which
are executed one after another.

Write HEADER at the end of LOG-BUFFER.

Call first step.

If FINISH-FUN non-nil it should be a function. This is called
after last step with LOG-BUFFER as parameter."
  ;;(dolist (step steps) (unless (functionp step) (error "Not a known function: %s" step)))
  (switch-to-buffer log-buffer)
  (udev-control-mode)
  (setq udev-is-log-buffer t)
  (let ((this-chain
         (cons nil
               (cons log-buffer
                     (cons (copy-tree steps)
                           (cons finish-fun nil))))))
    (setcar this-chain (caddr this-chain))
    (setq udev-this-chain this-chain))
  (assert (eq (car steps) (udev-this-step log-buffer)) t)
  (assert (eq finish-fun (udev-finish-function log-buffer)) t)
  (widen)
  (goto-char (point-max))
  (let ((inhibit-read-only t))
    (unless (= (point) (point-min)) (insert "\n\n"))
    (insert header))
  (udev-call-this-step log-buffer nil)
  (current-buffer))

(defvar udev-step-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map [(control ?c) ?r] 'udev-rerun-this-step)
    (define-key map [(control ?c) ?c] 'udev-continue-from-this-step)
    (define-key map [(control ?c) ?s] 'udev-goto-this-step-source)
    map))

(defun udev-step-at-point ()
  (get-text-property (point) 'udev-step))

(defun udev-rerun-this-step ()
  "Rerun this step."
  (interactive)
  (let ((this-step (udev-step-at-point)))
    (udev-call-this-step (current-buffer) this-step)))

(defun udev-continue-from-this-step ()
  "Continue from this step."
  (interactive)
  (let ((this-step (udev-step-at-point)))
    (goto-char (point-max))
    (let ((inhibit-read-only t))
      (insert (format "\n\nContinuing from %s..." this-step)))
    (udev-call-this-step (current-buffer) this-step)))

(defun udev-goto-this-step-source ()
  "Find source function for this step."
  (interactive)
  (let ((this-step (udev-step-at-point)))
    (find-function-other-window this-step)))

(defun udev-call-this-step (log-buffer this-step)
  "Call the current function in LOG-BUFFER.
If this function returns a buffer and the buffer has a process
then change the process sentinel to `udev-compilation-sentinel'.
Otherwise continue to call the next function.

Also put a log message in in LOG-BUFFER with a link to the buffer
returned above if any."
  (setq this-step (or this-step (udev-this-step log-buffer)))
  (with-current-buffer log-buffer
    (setq udev-last-error nil)
    (widen)
    (goto-char (point-max))
    (let* ((inhibit-read-only t)
           here
           buf
           proc)
      (if (not this-step)
          (let ((finish-fun (udev-finish-function log-buffer)))
            (insert (propertize "\nFinished\n" 'face 'compilation-info))
            (when finish-fun
              (funcall finish-fun log-buffer)))
        (insert (format "\nStep %s(%s): "
                        (udev-step-num log-buffer)
                        (udev-num-steps log-buffer)))
        (setq here (point))
        (insert (pp-to-string this-step))
        (setq buf (funcall this-step log-buffer))
        (when (bufferp buf)
          (make-text-button here (point)
                            'udev-step this-step
                            'keymap udev-step-keymap
                            'buffer buf
                            'help-echo "Push RET to see log buffer, <APPS> for other actions"
                            'action (lambda (btn)
                                      (display-buffer
                                       (button-get btn 'buffer))))
          (setq proc (get-buffer-process buf)))
        ;; Setup for next step
        (if (and proc
                 (not udev-last-error))
            (progn
              (with-current-buffer buf
                ;; Make a copy here for the sentinel function.
                (setq udev-log-buffer log-buffer)
                (setq udev-orig-sentinel (process-sentinel proc))
                (set-process-sentinel proc 'udev-compilation-sentinel)))
          ;;(message "proc is nil")
          (if udev-last-error
              (insert " "
                      (propertize udev-last-error 'face 'compilation-error))
            (udev-call-next-step log-buffer 0 nil)))))))

(defun udev-call-next-step (log-buffer prev-exit-status exit-status-buffer)
  "Go to next step in LOG-BUFFER and call `udev-call-this-step'.
However if PREV-EXIT-STATUS \(which is the exit status from the
previous step) is not 0 and there is in EXIT-STATUS-BUFFER no
`udev-continue-on-error-function' then stop and insert an error
message in LOG-BUFFER."
  (with-current-buffer log-buffer
    (let ((inhibit-read-only t))
      (widen)
      (goto-char (point-max))
      (insert " ")
      (if (or (= 0 prev-exit-status)
              (with-current-buffer exit-status-buffer
                (when udev-continue-on-error-function
                  (funcall udev-continue-on-error-function exit-status-buffer))))
          (progn
            (insert
             (if (= 0 prev-exit-status)
                 (propertize "Ok" 'face 'compilation-info)
               (propertize "Warning, check next step" 'face 'compilation-warning)))
            (udev-goto-next-step log-buffer)
            (udev-call-this-step log-buffer nil))
        (insert (propertize "Error" 'face 'compilation-error))))))


;;; Sentinel

(defvar udev-orig-sentinel nil
  "Old sentinel function remembered by `udev-call-this-step'.")
(make-variable-buffer-local 'udev-orig-sentinel)

(defun udev-compilation-sentinel (proc msg)
  "Sentinel to use for processes started by `udev-call-this-step'.
Check for error messages and call next step.  PROC and MSG have
the same meaning as for `compilation-sentinel'."
  ;;(message "udev-compilation-sentinel proc=%s msg=%s" proc msg)
  (let ((buf (process-buffer proc))
        (exit-status (process-exit-status proc)))
    (with-current-buffer buf
      (when udev-orig-sentinel
        (funcall udev-orig-sentinel proc msg))
      (when (and (eq 'exit (process-status proc))
                 (= 0 exit-status))
        ;; Check for errors
        (let ((here (point))
              (err-point 1)
              (has-error nil))
          (widen)
          (goto-char (point-min))
          (setq has-error
                (catch 'found-error
                  (while err-point
                    (setq err-point
                          (next-single-property-change err-point 'face))
                    (when err-point
                      (let ((face (get-text-property err-point 'face)))
                        (when (or (and (listp face)
                                       (memq 'compilation-error face))
                                  (eq 'compilation-error face))
                          (throw 'found-error t)))))))
          (when has-error
            (setq exit-status 1)
            (goto-char (point-max))
            (let ((inhibit-read-only t))
              (insert (propertize "There were errors" 'font-lock-face 'compilation-error)))
            (udev-set-compilation-end-message buf 'exit (cons "has errors" 1)))
          (goto-char here)
          ))
      (unless (member proc compilation-in-progress)
        (udev-call-next-step udev-log-buffer exit-status (current-buffer))))))

(defun udev-set-compilation-end-message (buffer process-status status)
  "Change the message shown after compilation.
This is similar to `compilation-end-message' and BUFFER,
PROCESS-STATUS and STATUS have the same meaning as there."
  (with-current-buffer buffer
    (setq mode-line-process
          (let ((out-string (format ":%s [%s]" process-status (cdr status)))
                (msg (format "%s %s" mode-name
                             (replace-regexp-in-string "\n?$" "" (car status)))))
            (message "%s" msg)
            (propertize out-string
                        'help-echo msg 'face (if (> (cdr status) 0)
                                                 'compilation-error
                                               'compilation-info))))))

(defvar udev-continue-on-error-function nil
  "One-time helper to resolve exit status error problem.
This can be used for example after calling `cvs diff' which
returns error exit status if there is a difference - even though
there does not have to be an error.")
(make-variable-buffer-local 'udev-continue-on-error-function)


;;; Convenience functions

(defun udev-buffer-name (fmt log-buffer mode)
  "Return a name for compilation buffer.
Use format string FMT and buffer LOG-BUFFER, but ignoring MODE."
  (format fmt (when (buffer-live-p log-buffer)
                (udev-this-step log-buffer))))

(defvar udev-this-dir
  (let ((this-file (or load-file-name (buffer-file-name))))
    (file-name-directory this-file)))

(defun udev-batch-compile (emacs-args defdir name-function)
  "Compile elisp code in an inferior Emacs.
Start Emacs with

  emacs -Q -batch EMACS-ARGS

in the default directory DEFDIR.

Set the buffer name for the inferior process with NAME-FUNCTION
by giving this to `compilation-start'."
  (let ((default-directory (file-name-as-directory defdir))
        (this-emacs (ourcomments-find-emacs)))
    (compilation-start
     (concat this-emacs " -Q -batch " emacs-args)
     'compilation-mode
     name-function)))

;;; Convenience functions for CVS

(defun udev-fetch-cvs-diff (defdir name-function)
  "Fetch cvs diff in directory DEFDIR.
Put the diff in file 'your-patches.diff' in DEFDIR.
Give inferior buffer name with NAME-FUNCTION."
  (let ((default-directory (file-name-as-directory defdir)))
    (with-current-buffer
        (compilation-start
         (concat "cvs diff -b -u > " (shell-quote-argument "your-patches.diff"))
         'compilation-mode
         name-function)
      (setq udev-continue-on-error-function 'udev-cvs-diff-continue)
      (current-buffer))))

(defun udev-cvs-diff-continue (cvs-diff-buffer)
  "Return non-nil if it is ok to continue.
Check the output from the `cvs diff' command in buffer
CVS-DIFF-BUFFER.

The cvs command exits with a failure status if there is a
difference, which means that it is hard to know whether there was
an error or just a difference.  This function tries to find out."
  (with-current-buffer cvs-diff-buffer
    (let ((here (point))
          (ret t))
      (goto-char (point-min))
      (when (search-forward "cvs [diff aborted]" nil t) (setq ret nil))
      (goto-char (point-min))
      (when (search-forward "merge conflict" nil t) (setq ret t))
      ;; From cvs co command:
      ;; rcsmerge: warning: conflicts during merge
      (goto-char (point-min))
      (when (search-forward "conflicts during merge" nil t) (setq ret t))
      ;; cvs checkout: conflicts found in emacs/lisp/startup.el
      (goto-char (point-min))
      (when (search-forward "conflicts found in" nil t) (setq ret t))
      (goto-char here)
      ret)))

(defun udev-check-cvs-diff (diff-file log-buffer)
  "Check cvs diff output in file DIFF-FILE for merge conflicts.
Return buffer containing DIFF-FILE."
  (let ((buf (find-buffer-visiting diff-file)))
    ;; Kill buffer to avoid question about revert.
    (when buf (kill-buffer buf))
    (setq buf (find-file-noselect diff-file))
    (with-current-buffer buf
      (widen)
      (let ((here (point)))
        (goto-char (point-min))
        ;; Fix-me: Better pattern:
        (if (search-forward "<<<<<<<" nil t)
            ;; Merge conflict
            (with-current-buffer log-buffer
              (let ((inhibit-read-only t))
                (setq udev-last-error "Error: merge conflict")))
          (goto-char here))))
      buf))

;;(setq compilation-scroll-output t)
;;(add-to-list 'compilation-error-regexp-alist 'cvs)
;;(setq compilation-error-regexp-alist (delq 'cvs compilation-error-regexp-alist))

;;; Misc

(defun udev-send-buffer-process (str)
  (interactive "sString to send to process: ")
  (let* ((procs (process-list))
         (proc (catch 'found
                 (dolist (p procs)
                   (when (eq (process-buffer p) (current-buffer))
                     (throw 'found p))))))
    (unless proc (error "Can't find process in buffer"))
    ;;(message "str=%s" str)
    ;;(message "proc=%s" proc)
    (process-send-string proc (concat str "\n"))
  ))


(provide 'udev)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; udev.el ends here
