;;; mozadd.el --- Additional functionality for MozRepl
;;
;; Author: Lennart Borgman (lennart O borgman A gmail O com)
;; Created: 2009-07-22 Wed
(defconst mozadd:version "0.2") ;; Version:
;; Last-Updated: 2009-08-04 Tue
;; URL:
;; Keywords:
;; Compatibility:
;;
;; Features that might be required by this library:
;;
  ;; `cc-cmds', `cc-defs', `cc-engine', `cc-vars', `comint', `json',
  ;; `moz', `regexp-opt', `ring'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; Live tracking of editing changes, see
;;   `mozadd-mirror-mode'
;;   `mozadd-refresh-edited-on-save-mode'
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
;; published by the Free Software Foundation; either version 3, or
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

(require 'moz)
(require 'json)

(defun mozadd-warning (format-string &rest args)
  (let ((str (apply 'format format-string args)))
    (message "%s" (propertize str 'face 'secondary-selection))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Refresh Firefox after save etc

;; Partly after an idea on EmacsWiki

(defconst mozadd-edited-buffer nil)

;;;###autoload
(define-minor-mode mozadd-refresh-edited-on-save-mode
  "Refresh mozadd edited file in Firefox when saving file.
The mozadd edited file is the file in the last buffer visited in
`mozadd-mirror-mode'.

You can use this for example when you edit CSS files.

The mozadd edited file must be shown in Firefox and visible."
  :lighter "MozRefresh"
  (if mozadd-refresh-edited-on-save-mode
      (add-hook 'after-save-hook 'mozadd-queue-reload-mozilla-edited-file nil t)
    (remove-hook 'after-save-hook 'mozadd-queue-reload-mozilla-edited-file t)))
(put 'mozadd-refresh-edited-on-save-mode 'permanent-local t)

;;;###autoload
(define-globalized-minor-mode global-mozadd-refresh-edited-on-save-mode
  mozadd-refresh-edited-on-save-mode
  (lambda ()
    (when (or (derived-mode-p 'css-mode)
              (mozadd-html-buffer-file-p))
      (mozadd-refresh-edited-on-save-mode 1))))

(defun mozadd-queue-reload-mozilla-edited-file ()
  "Reload edited file."
  (when (buffer-live-p mozadd-edited-buffer)
    (if (buffer-modified-p mozadd-edited-buffer)
        (mozadd-warning "Mozadd: Edited buffer %s is not saved, can't reload browser."
                          (buffer-name mozadd-edited-buffer))
      (mozadd-add-queue-get-mirror-location)
      (mozadd-add-task-1 'mozadd-send-refresh-edited-to-mozilla))))

(defun mozadd-send-refresh-edited-to-mozilla ()
  "Update the remote mozrepl instance"
  (with-current-buffer mozadd-edited-buffer
    (if (not (mozadd-edited-file-is-shown))
        (mozadd-warning "Mozadd: Edited buffer %s is not shown, can't reload browser."
                          (buffer-name mozadd-edited-buffer))
      (comint-send-string (inferior-moz-process)
                          "setTimeout(BrowserReload(), \"1000\");")))
  (mozadd-exec-next))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Mirror html buffer in Firefox

;; Partly after an idea on
;; http://people.internetconnection.net/2009/02/interactive-html-development-in-emacs/

;; Fun, it kind of works, but is perhaps totally useless .... - slow
;; and maybe scrolling... - but the file I am testing with have 3000
;; lines...

;; Fix-me: How do you get the currently shown page in Firefox?

(defun mozadd-perhaps-start ()
  "Start if MozRepl if not running. Return message if not ok."
  (unless (buffer-live-p inferior-moz-buffer)
    (condition-case err
        (progn
          (inferior-moz-start-process)
          nil)
      (error (error-message-string err)))))

(defvar mozadd-mirror-location nil)
(make-variable-buffer-local 'mozadd-mirror-location)
(put 'mozadd-mirror-location 'permanent-local t)

(defvar mozadd-initial-mirror-location nil)
(make-variable-buffer-local 'mozadd-initial-mirror-location)
(put 'mozadd-initial-mirror-location 'permanent-local t)

;;(mozadd-get-comint-string-part "\"hi\" there")
(defun mozadd-get-comint-string-part (comint-output)
  (save-match-data
    (if (string-match "^\".*?\"" comint-output)
        (match-string 0 comint-output)
      comint-output)))

(defun mozadd-get-initial-mirror-location (comint-output)
  ;;(message "mozadd-get-initial-mirror-location %S" comint-output)
  (with-current-buffer mozadd-edited-buffer
    (setq mozadd-initial-mirror-location (mozadd-get-comint-string-part comint-output)))
  (mozadd-exec-next)
  comint-output)

(defun mozadd-get-mirror-location (comint-output)
  ;;(message "mozadd-get-mirror-location %S" comint-output)
  (with-current-buffer mozadd-edited-buffer
    (setq mozadd-mirror-location (mozadd-get-comint-string-part comint-output)))
  (mozadd-exec-next)
  comint-output)

(defun mozadd-add-queue-get-mirror-location ()
  (mozadd-add-task "content.location.href" 'mozadd-get-mirror-location))

(defun mozadd-skip-output-until-prompt (comint-output)
  ;;(message "mozadd-skip-output-until-prompt %S" comint-output)
  (if (not (string-match-p "\\(\\w+\\)> $" comint-output))
      ""
    ;;(message "done  recieve %s" (current-time-string))
    (mozadd-exec-next)
    comint-output
    ""
    ))

(defun mozadd-queue-send-buffer-content-to-mozilla (buffer)
  (mozadd-add-queue-get-mirror-location)
  (setq mozadd-edited-buffer buffer)
  (mozadd-add-task-1 'mozadd-send-buffer-content-to-mozilla))

(defun mozadd-edited-file-is-shown ()
  (with-current-buffer mozadd-edited-buffer
    (string= mozadd-mirror-location mozadd-initial-mirror-location)))

(defvar mozadd-xml-path-outline-style "2px solid red")
(defun mozadd-send-buffer-content-to-mozilla ()
  "Update the remote mozrepl instance"
  (with-current-buffer mozadd-edited-buffer
    (if (mozadd-edited-file-is-shown)
        (mozadd-requeue-me-as-task
         (concat "content.document.body.innerHTML="
                 (json-encode
                  (save-restriction
                    (widen)
                    (let ((where-points nil)
                          (str "")
                          (p1 (point-min))
                          p2)
                      ;; If nxml-where-mode is on add corresponding outline style.
                      (when (and (boundp 'nxml-where-mode) nxml-where-mode)
                        (mapc (lambda (ovl)
                                (when (overlay-get ovl 'nxml-where)
                                  (when (/= ?/ (1+ (char-after (overlay-start ovl))))
                                    (push (1- (overlay-end ovl)) where-points))))
                              (overlays-in (point-min) (point-max)))
                        (setq where-points (sort where-points '<)))
                      (dolist (p2 where-points)
                        (setq str (concat str
                                          (buffer-substring-no-properties p1
                                                                          p2)))
                        (setq str (concat str
                                          " style=\"outline: "
                                          mozadd-xml-path-outline-style
                                          "\""))
                        (setq p1 p2)
                        )
                      (setq str (concat str
                                        (buffer-substring-no-properties p1
                                                                        (point-max))))
                      str))
                  )
                 ";")
         'mozadd-skip-output-until-prompt)
      (mozadd-skip-current-task))
    ;; Timer to avoid looping
    (run-with-idle-timer 0 nil 'mozadd-maybe-exec-next)
    ))

(defconst mozadd-current-task nil)
(defconst mozadd-task-queue nil)
;;(mozadd-add-task "content.location.href" 'mozadd-get-initial-mirror-location)
;;(mozadd-add-task "hi" 1)
;;(mozadd-add-task "hm" 2)
(defun mozadd-clear-exec-queue ()
  (setq mozadd-current-task nil)
  (setq mozadd-task-queue nil)
  (when (buffer-live-p inferior-moz-buffer)
    (with-current-buffer inferior-moz-buffer
      (dolist (fun (buffer-local-value 'comint-preoutput-filter-functions (current-buffer)))
        (remove-hook 'comint-preoutput-filter-functions fun t)))))

(defun mozadd-add-task (input task)
  (mozadd-add-task-1 (list input task)))

(defun mozadd-add-task-1 (task)
  (setq mozadd-task-queue (cons task mozadd-task-queue))
  (setq mozadd-task-queue (reverse mozadd-task-queue))
  ;;(message "add-task: mozadd-task-queue=%S, current=%s" mozadd-task-queue mozadd-current-task)
  (mozadd-maybe-exec-next))

(defun mozadd-maybe-exec-next ()
  ;;(message "mozadd-maybe-exec-next, current=%s" mozadd-current-task)
  (unless mozadd-current-task
    (mozadd-exec-next)))

(defun mozadd-exec-next ()
  (when mozadd-current-task
    (let* ((old-task mozadd-current-task) ;;(pop mozadd-task-queue))
           (old-filter (when (listp old-task) (nth 1 old-task))))
      (when (and old-filter (buffer-live-p inferior-moz-buffer))
        (with-current-buffer inferior-moz-buffer
          (remove-hook 'comint-preoutput-filter-functions old-filter t)))))
  (setq mozadd-current-task nil)
  (when mozadd-task-queue
    (let* ((this  (pop mozadd-task-queue))
           (input (when (listp this) (nth 0 this)))
           (task  (when (listp this) (nth 1 this)))
           )
      (setq mozadd-current-task this)
      ;;(message "EXEC: %s" this)
      (if (not (listp this))
          (funcall this)
        (when (buffer-live-p inferior-moz-buffer)
          (with-current-buffer inferior-moz-buffer
            (add-hook 'comint-preoutput-filter-functions task nil t)))
        (comint-send-string (inferior-moz-process) input)))))

(defun mozadd-skip-current-task ()
  ;;(message "mozadd-skip-current-task")
  ;;(pop mozadd-task-queue)
  (setq mozadd-current-task nil))

(defun mozadd-requeue-me-as-task (input task)
  (mozadd-skip-current-task)
  ;;(message "mozadd-requeue-me-as-task %S %S" input task)
  (setq mozadd-task-queue (cons (list input task) mozadd-task-queue)))

(defcustom mozadd-browseable-file-extensions
  '("html" "htm" "xhtml")
  "File extensions possibly viewable in a web browser."
  :type '(repeat (string :tag "File extension (without leading dot)"))
  :group 'mozadd)

(defun mozadd-html-buffer-file-p ()
  "Return non-nil if buffer file is viewable in a web browser."
  (when (buffer-file-name)
    (member (file-name-extension (buffer-file-name))
            mozadd-browseable-file-extensions)))

;;;###autoload
(define-minor-mode mozadd-mirror-mode
  "Mirror content of current file buffer immediately in Firefox.
When you turn on this mode the file will be opened in Firefox.
Every change you make in the buffer will trigger a redraw in
Firefox - regardless of if you save the file or not.

For the mirroring to work the edited file must be shown in
Firefox and visible.

If `nxml-where-mode' is on the marks will also be shown in
Firefox as CSS outline style.  You can customize the style
through the option `mozadd-xml-path-outline-style'.

See also `mozadd-refresh-edited-on-save-mode'."
  nil
  :lighter " MozMirror"
  :group 'mozadd
  (if mozadd-mirror-mode
      (unless (catch 'ok
                (unless (mozadd-html-buffer-file-p)
                  (mozadd-warning "You can only mirror html file buffers")
                  (throw 'ok nil))
                (when (buffer-modified-p)
                  (mozadd-warning "Please save buffer first")
                  (throw 'ok nil))
                (let ((msg (mozadd-perhaps-start)))
                  (when msg
                    (mozadd-warning msg)
                    (throw 'ok nil)))
                (mozadd-clear-exec-queue)
                (setq mozadd-edited-buffer (current-buffer))
                (mozadd-add-task (concat "content.location.href = "
                                         "\"file:///" (buffer-file-name) "\";")
                                 'mozadd-get-initial-mirror-location)
                (add-hook 'after-change-functions 'mozadd-update-mozilla t t)
                (add-hook 'nxhtml-where-hook 'mozadd-update-mozilla t t)
                (add-hook 'post-command-hook 'mozadd-edited-buffer-post-command)
                t)
        (setq mozadd-mirror-mode nil))
    (setq mozadd-edited-buffer nil)
    (remove-hook 'post-command-hook 'mozadd-edited-buffer-post-command)
    (remove-hook 'nxhtml-where-hook 'mozadd-update-mozilla t)
    (remove-hook 'after-change-functions 'mozadd-update-mozilla t)))
(put 'mozadd-mirror-mode 'permanent-local t)

;;;###autoload
(define-globalized-minor-mode global-mozadd-mirror-mode mozadd-mirror-mode
  (lambda ()
    (when (mozadd-html-buffer-file-p)
      (mozadd-mirror-mode 1))))

(defun mozadd-edited-buffer-post-command ()
  "Check if we are in a new edited buffer."
  (when mozadd-mirror-mode
    (setq mozadd-edited-buffer (current-buffer))))


(defvar mozadd-buffer-content-to-mozilla-timer nil)

(defun mozadd-update-mozilla (&rest ignored)
  (when (timerp mozadd-buffer-content-to-mozilla-timer)
    (cancel-timer mozadd-buffer-content-to-mozilla-timer))
  (setq mozadd-buffer-content-to-mozilla-timer
        (run-with-idle-timer 1 nil 'mozadd-queue-send-buffer-content-to-mozilla (current-buffer))))
(put 'mozadd-update-mozilla 'permanent-local-hook t)


(provide 'mozadd)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; mozadd.el ends here
