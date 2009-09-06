;;; sex-mode.el --- Shell EXecute mode / Send to EXternal program
;;
;; Author: Lennart Borgman (lennart O borgman A gmail O com)
;; Created: 2008-06-01T18:41:50+0200 Sun
(defconst sex-mode:version "0.71")
;; Last-Updated: 2009-01-06 Tue
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
;; Open urls belonging to other programs with those programs. To
;; enable this turn on the global minor mode `sex-mode'.
;;
;; If you for example open a .pdf file with C-x C-f it can be opened
;; by the .pdf application you have set your computer to use. (Or, if
;; that such settings are not possible on your OS, with the
;; application you have choosen here.)
;;
;; There is also a defmacro `sex-with-temporary-apps' that you can use
;; for example with `find-file' to open files in external
;; applications.
;;
;; The functions used to open files in external applications are
;; borrowed from `org-mode'.  There is some small differences:
;;
;; - There is an extra variable here `sex-file-apps' that is checked
;;   before the corresponding lists in `org-mode'.
;;
;; - In `org-mode' any file that is not found in the lists (and is not
;;   remote or a directory) is sent to an external application. This
;;   would create trouble when used here in a file handler so the
;;   logic is the reverse here: Any file that is not found in the
;;   lists is opened inside Emacs. (Actually I think that might be a
;;   good default in `org-mode' too, but I am not sure.)
;;
;; - Because of the above I have to guess which function is the one
;;   that sends a file to an external application.
;;
;; (Currently the integration with org.el is not the best code wise.
;; We hope to improve that soon.)
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change log:
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

;;(org-open-file "c:/EmacsW32/nxhtml/nxhtml/doc/nxhtml-changes.html")
(eval-when-compile (require 'cl))
(eval-when-compile (require 'org))
(eval-when-compile (require 'mailcap))

(defcustom sex-file-apps
  '(
    ("html" . emacs)
    ("pdf"  . default)
    ("wnk"  . default)
    )
  "Application for opening a file.
See `sex-get-file-open-cmd'."
  :group 'sex
  :type '(repeat
	  (cons (choice :value ""
			(string :tag "Extension")
			(const :tag "Default for unrecognized files" t)
			(const :tag "Remote file" remote)
			(const :tag "Links to a directory" directory))
		(choice :value ""
			(const :tag "Visit with Emacs" emacs)
			(const :tag "Use system default" default)
			(string :tag "Command")
			(sexp :tag "Lisp form")))))

;;(sex-get-apps)

(defvar sex-with-temporary-file-apps nil)

(defun sex-get-apps ()
  (or sex-with-temporary-file-apps
      (append sex-file-apps org-file-apps (org-default-apps))))

;; (sex-get-file-open-cmd "temp.el")
;; (sex-get-file-open-cmd "http://some.where/temp.el")
;; (sex-get-file-open-cmd "temp.c")
;; (sex-get-file-open-cmd "temp.pdf")
;; (sex-get-file-open-cmd "temp.doc")
;; (sex-get-file-open-cmd "/ftp:temp.doc")
;; (sex-get-file-open-cmd "http://some.host/temp.doc")
;; (sex-get-file-open-cmd "http://some.host/temp.html")

(defun sex-get-file-open-cmd (path)
  "Get action for opening file.
Construct a key from PATH:
- If PATH specifies a location on a remote system then set key to
  'remote.
- If PATH is a directory set key to 'directory.
- Otherwise use the file extension of PATH as key.

Search with this key against the combined association list of
`sex-file-apps', `org-file-apps' and `org-default-apps'.  The
first matching entry is used.

If cdr of this entry is 'default then search again with key equal
to t for the default action for the operating system you are on
\(or your own default action if you have defined one in the
variables above).

Return the cdr of the found entry.

If no entry was found return `emacs' for opening inside Emacs."
  (let* ((apps (sex-get-apps))
         (key (if (org-file-remote-p path)
                  'remote
                (if (file-directory-p path)
                    'directory
                  (let ((ext (file-name-extension path)))
                    (if (and t ext)
                        ;; t should be a check for case insensitive
                        ;; file names ... - how do you do that?
                        (downcase ext)
                      ext)))))
         (cmd (or (cdr (assoc key apps))
                  'emacs)))
    (when (eq cmd 'default)
      (setq cmd (or (cdr (assoc t apps))
                    'emacs)))
    (when (eq cmd 'mailcap)
      (require 'mailcap)
      (mailcap-parse-mailcaps)
      (let* ((mime-type (mailcap-extension-to-mime (or key "")))
	     (command (mailcap-mime-info mime-type)))
	(if (stringp command)
	    (setq cmd command)
	  (setq cmd 'emacs))))
    ;;(message "cmd=%s" cmd)
    cmd))

(defgroup sex nil
  "Customization group for `sex-mode'."
  :group 'external)

;;(setq sex-handle-urls t)
(defcustom sex-handle-urls nil
  "When non-nil `sex-mode' also handles urls.
Turn on `url-handler-mode' when turning on `sex-mode' if this is
non-nil.  Open urls in a web browser."
  :type 'boolean
  :group 'sex)

;; (setq sex-keep-dummy-buffer nil)
;; (setq sex-keep-dummy-buffer 'visible)
;; (setq sex-keep-dummy-buffer 'burried)
(defcustom sex-keep-dummy-buffer 'visible
  "Keep dummy buffer after opening file.
When opening a file with the shell a dummy buffer is created in
Emacs in `sex-file-mode' and an external program is called to
handle the file. How this dummy buffer is handled is governed by
this variable."
  :type '(choice (const :tag "Visible" visible)
                 (const :tag "Burried" burried)
                 (const :tag "Do not keep it" nil))
  :group 'sex)

(defcustom sex-reopen-on-buffer-entry nil
  "If non-nil send file to shell again on buffer entry."
  :type 'boolean
  :group 'sex)

(defun sex-post-command ()
  "Run post command in `sex-file-mode' buffers.
If `sex-reopen-on-buffer-entry' is non-nil then send the buffer
file to system again."
  (when sex-reopen-on-buffer-entry
    (if (and (boundp 'url-handler-regexp)
             (string-match url-handler-regexp buffer-file-name))
        (sex-browse-url buffer-file-name)
      (sex-handle-by-external buffer-file-name))
    (bury-buffer)))

(defun sex-browse-url (url)
  "Ask a web browser to open URL."
  (condition-case err
      (list (browse-url url) "Opened URL in web browser")
    (error (list nil (error-message-string err)))))

(defun sex-url-insert-file-contents (url &optional visit beg end replace)
  (sex-generic-insert-file-contents
   'sex-browse-url
   (concat "This dummy buffer is used just for opening a URL.\n"
           "To open the URL again click here:\n\n  ")
   (concat "Tried to open URL in web browser, "
           "but it failed with message\n\n  ")
   url visit beg end replace))

(defun sex-file-insert-file-contents (url &optional visit beg end replace)
  ;;(message "sex-file-insert-file-contents %s %s %s %s %s" url visit beg end replace)
  (sex-generic-insert-file-contents
   'sex-handle-by-external
   (concat "This dummy buffer is used just for opening a file.\n"
           "The file itself was sent to system for opening.\n\n"
           "To open the file again click here:\n\n  ")
   (concat "Tried to send file"
           " to system but it failed with message\n\n  ")
   url visit beg end replace))

(defun sex-write-file-function ()
  (set-buffer-modified-p nil)
  (error "Can't write this to file, it is just a dummy buffer"))

(defun sex-generic-insert-file-contents (insert-fun
                                         success-header
                                         fail-header
                                         url &optional visit beg end replace)
  (let ((window-config (current-window-configuration)))
    (unless (= 0 (buffer-size))
      (error "Buffer must be empty"))
    (set (make-local-variable 'write-file-functions)
         '(sex-write-file-function))
    (let* ((name url)
           ;;(result (sex-browse-url name))
           (result (funcall insert-fun name))
           (success (nth 0 result))
           (msg     (nth 1 result)))
      (setq buffer-file-name name)
      (if success
          (progn
            (insert success-header)
            (sex-setup-restore-window-config window-config)
            (message "%s" msg))
        (insert (propertize "Error: " 'face 'font-lock-warning-face)
                fail-header msg
                "\n\nTo try again click here:\n\n  "))
      (save-excursion
        (insert-text-button
         buffer-file-name
         'insert-fun insert-fun
         'action (lambda (button)
                   ;;(sex-browse-url buffer-file-name)
                   (funcall (button-get button 'insert-fun) buffer-file-name)
                   ))))))

(defun sex-file-handler (operation &rest args)
  "Handler for `insert-file-contents'."
  ;;(message "\noperation=%s, args=%s" operation args)
  (let ((done nil)
        (ftype 'emacs))
    ;; Always open files inside Emacs if the file opening request came
    ;; through Emacs client. Here is a primitive test if we are called
    ;; from outside, client-record is bound in `server-visit-files'
    ;; ...
    (when (not (boundp 'client-record))
      (let* ((filename (car args))
             (insert-handling (sex-get-file-open-cmd filename)))
        ;;(message "insert-handling=%s" insert-handling)
        (when insert-handling
          (setq ftype insert-handling))
        ;;(message "ftype=%s, filename=%s" ftype filename)
        ))
    (unless (eq ftype 'emacs)
      ;;(message "using sex-file-insert-file-contents for %s" args)
      (apply 'sex-file-insert-file-contents args)
      (setq done t))
    ;; Handle any operation we don't know about.
    (unless done
      ;;(message "fallback for operation=%s, args=%s" operation args)
      (let ((inhibit-file-name-handlers
             (cons 'sex-file-handler
                   (and (eq inhibit-file-name-operation operation)
                        inhibit-file-name-handlers)))
            (inhibit-file-name-operation operation))
        (apply operation args)))))
;; Note: Because of a bug in Emacs we must restrict the use of this
;; file handler to only 'insert-file-contents. (We should of course
;; anyway do that.)
(put 'sex-file-handler 'operations '(insert-file-contents))

(defun sex-setup-restore-window-config (window-config)
  (when (not (eq sex-keep-dummy-buffer 'visible))
    (run-with-idle-timer 0 nil
                         'sex-restore-window-config
                         (selected-frame)
                         window-config
                         (unless sex-keep-dummy-buffer
                           (current-buffer)))))

(defun sex-restore-window-config (frame win-config buffer)
  (save-match-data ;; runs in timer
    (with-selected-frame frame
      (set-window-configuration win-config))
    (when buffer (kill-buffer buffer))))

(defun sex-handle-by-external (&optional file)
  "Give file FILE to external program.
Return a list:

  (SUCCESS MESSAGE)

where SUCCESS is non-nil if operation succeeded and MESSAGE is an
informational message."
  (unless file (setq file buffer-file-name))
  (let ((cmd (sex-get-file-open-cmd file)))
    (assert (not (eq cmd 'emacs)))
    (cond
     ((and (stringp cmd) (not (string-match "^\\s-*$" cmd)))
      ;; Remove quotes around the file name - we'll use shell-quote-argument.
      (while (string-match "['\"]%s['\"]" cmd)
	(setq cmd (replace-match "%s" t t cmd)))
      (while (string-match "%s" cmd)
	(setq cmd (replace-match
		   (save-match-data
		     (shell-quote-argument
		      (convert-standard-filename file)))
		   t t cmd)))
      (save-window-excursion
	(start-process-shell-command cmd nil cmd)
	;;(and (boundp 'org-wait) (numberp org-wait) (sit-for org-wait))
	)
      (list t (format "Opened %s in external application" file)))
     ((consp cmd)
      (let ((file (convert-standard-filename file)))
	(eval cmd))
      (list t (format "Opened %s in external application" file)))
     (t (list nil (format "Don't know how to handle %s" file))))
    ))


(define-derived-mode sex-file-mode nil
  "External"
  "Mode for files opened in external programs."
  (add-hook 'post-command-hook 'sex-post-command nil t)
  (set-keymap-parent (current-local-map) button-buffer-map)
  (set-buffer-modified-p nil)
  (setq buffer-read-only t))


(defvar sex-old-url-insert-file-contents nil)
(defvar sex-old-url-handler-mode nil)

;;;###autoload
(define-minor-mode sex-mode
  "Open certain files in external programs.
See `sex-get-file-open-cmd' for how to determine which files to
open by external applications.  Note that this selection is
nearly the same as in `org-mode'.  The main difference is that
the fallback always is to open a file in Emacs. \(This is
necessary to avoid to disturb many of Emacs operations.)

This affects all functions that opens files, like `find-file',
`find-file-noselect' etc.

However it does not affect files opened through Emacs client.

Urls can also be handled, see `sex-handle-urls'.

When opening a file with the shell a \(temporary) dummy buffer is
created in Emacs with major mode `sex-file-mode' and an external
program is called to handle the file.  How this dummy buffer is
handled is governed by `sex-keep-dummy-buffer'."

  ;; On MS Windows `w32-shell-execute' is called to open files in an
  ;; external application. Be aware that this may run scripts if the
  ;; script file extension is not blocked in `sex-open-alist'.
  nil
  :group 'sex
  :global t
  ;; fix-me: better list handling
  (if sex-mode
      (progn
        (require 'org)
        (dolist (rec (sex-get-apps))
          (let* ((ext (car rec))
                 (app (cdr rec))
                 (patt (when (and (stringp ext)
                                  (not (eq app 'emacs)))
                         (concat "\\." ext "\\'"))))
            (unless patt
              (when (eq ext t)
                (setq patt (concat ".*\\'"))))
            (when patt
              (unless (eq ext t)
                (add-to-list 'auto-mode-alist (cons patt 'sex-file-mode)))
              (add-to-list 'file-name-handler-alist
                           (cons patt 'sex-file-handler) t))))
        (setq sex-old-url-insert-file-contents
              (get 'insert-file-contents 'url-file-handlers))
        (setq sex-old-url-handler-mode url-handler-mode)
        (when sex-handle-urls
          ;;(message "req url, before")
          (require 'url-handlers)
          ;;(message "req url, after")
          (put 'insert-file-contents 'url-file-handlers
               'sex-url-insert-file-contents)
          (unless url-handler-mode
            (url-handler-mode 1)
            ;;(message "after url-handler-mode 1")
            )))
    ;; Remove from the lists:
    ;;(let ((handler-list (copy-list file-name-handler-alist)))
    (let ((handler-list (copy-sequence file-name-handler-alist)))
      (dolist (handler handler-list)
        (when (eq 'sex-file-handler (cdr handler))
          (setq file-name-handler-alist
                (delete handler file-name-handler-alist)))))
    ;;(let ((mode-alist (copy-list auto-mode-alist)))
    (let ((mode-alist (copy-sequence auto-mode-alist)))
      (dolist (auto-mode mode-alist)
        (when (eq 'sex-file-mode (cdr auto-mode))
          (setq auto-mode-alist
                (delete auto-mode auto-mode-alist)))))
    (put 'insert-file-contents 'url-file-handlers
         sex-old-url-insert-file-contents)
    (unless sex-old-url-handler-mode (url-handler-mode 0))))

(defmacro sex-with-temporary-apps (open-alist &rest body)
  "Run BODY with `sex-mode' on.
If OPEN-ALIST is not t it replaces the list normally used by
`sex-get-file-open-cmd'."
  (declare (indent 1) (debug t))
  `(let ((old-sex-mode sex-mode)
         (sex-with-temporary-file-apps
          (if (eq ,open-alist t)
              nil
            ,open-alist)))
     (when sex-mode (sex-mode -1))
     (sex-mode 1)
     ,@body
     (setq sex-with-temporary-file-apps nil)
     (unless old-sex-mode (sex-mode -1))))

;; (with-sex t (find-file "c:/emacs-lisp/gimp-mode-v1.40/gimpmode.pdf"))
;; (with-sex nil (find-file "c:/emacs-lisp/gimp-mode-v1.40/gimpmode.pdf"))

(provide 'sex-mode)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; sex-mode.el ends here
