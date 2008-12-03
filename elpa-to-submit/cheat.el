;; cheat.el
;; Time-stamp: <2007-08-22 10:00:04 sjs>
;;
;; Copyright (c) 2007 Sami Samhuri <sami.samhuri@gmail.com>
;;
;; See http://sami.samhuri.net/2007/08/10/cheat-from-emacs for updates.
;;
;; License
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
;;
;;
;; Provide a handy interface to cheat.
;; See http://cheat.errtheblog.com for details on cheat itself.
;;
;; sjs 2007.08.21
;;  * Cache the list of cheat sheets, update it once a day (configurable).
;;  * Strictly complete cheat sheet names.
;;
;; TODO: make sure all functions are namespaced under cheat-
;;


(defvar *cheat-host* "cheat.errtheblog.com")
(defvar *cheat-port* "80")
(defvar *cheat-uri* (concat *cheat-host* ":" *cheat-port*))

(defvar *cheat-directory* "~/.cheat")
(defvar *cheat-sheets-cache-file* (concat *cheat-directory* "/sheets"))

(defvar *cheat-last-sheet* nil
  "Name of the most recently viewed cheat sheet.")

(defvar *cheat-sheet-history* nil
  "List of the most recently viewed cheat sheets.")

(defconst +seconds-per-day+ 86400)

(defvar *cheat-cache-ttl* +seconds-per-day+
  "The minimum age of a stale cache file, in seconds.")


;;; interactive functions

;;;###autoload
(defun cheat (name &optional silent)
  "Show the specified cheat sheet.

If SILENT is non-nil then do not print any output, but return it
as a string instead."
  (interactive (list (cheat-read-sheet-name)))
  (if silent
      (cheat-command-silent name)
      (cheat-command name)))

(defun cheat-sheets ()
  "List all cheat sheets."
  (interactive)
  (cheat-command "sheets"))

(defun cheat-recent ()
  "Show recently added cheat sheets."
  (interactive)
  (cheat-command "recent"))

(defun cheat-clear-cache ()
  "Clear the local cheat cache, located in ~/.cheat."
  (interactive)
  (cheat-command "--clear-cache")
  (make-directory *cheat-directory*))

(defun cheat-versions (name)
  "Version history of the specified cheat sheet."
  (interactive (list (cheat-read-sheet-name)))
  (cheat-command name "--versions"))

(defun cheat-diff (name version)
  "Show the diff between the given version and the current version of the named
  cheat.
If VERSION is of the form m:n then show the diff between versions m and n."
  (interactive (list (cheat-read-sheet-name)
                     (read-string "Cheat version(s): ")))
  (cheat-command name "--diff" version))

(defun cheat-add-current-buffer (name)
  "Add a new cheat with the specified name and the current buffer as the body."
  (interactive "sCheat name: \n")
  (post-cheat name (buffer-string) t)
    (if (interactive-p)
        (print (concat "Cheat added (" name ")"))))

(defun cheat-edit (name)
  "Fetch the named cheat and open a buffer containing its body.
The cheat can be saved with `cheat-save-current-buffer'."
  (interactive (list (cheat-read-sheet-name)))
  (cheat-clear-cache name) ; make sure we're working with the latest version
  (switch-to-buffer (get-buffer-create (cheat->buffer name)))
  (insert (cheat-body name))
  (if (interactive-p)
      (print "Run `cheat-save-current-buffer' when you're done editing.")))

(defun cheat-save-current-buffer ()
  "Save the current buffer using the buffer name for the title and the contents
  as the body."
  (interactive)
  (let ((name (buffer->cheat (buffer-name (current-buffer)))))
    (post-cheat name (buffer-string))
    ;; TODO check for errors and kill the buffer on success
    (if (interactive-p)
        (print (concat "Cheat saved (" name ")")))
    (cheat-clear-cache name)
    (cheat name)))


;;; helpers

;; this is from rails-lib.el in the emacs-rails package
(defun string-join (separator strings)
  "Join all STRINGS using SEPARATOR."
  (mapconcat 'identity strings separator))

(defun blank (thing)
  "Return T if THING is nil or an empty string, otherwise nil."
  (or (null thing)
      (and (stringp thing)
           (= 0 (length thing)))))

(defun cheat-command (&rest rest)
  "Run the cheat command with the given arguments, display the output."
  (interactive "sArguments for cheat: \n")
  (let* ((cmd (string-join " " rest)) 
         (buffer (get-buffer-create 
                  (concat "*Cheat: " cmd "*"))))
      (shell-command (concat "cheat " cmd) buffer)))

(defun cheat-command-to-string (&rest rest)
  "Run the cheat command with the given arguments and return the output as a
  string.  Display nothing."
  (shell-command-to-string (concat "cheat " (string-join " " rest))))

(defalias 'cheat-command-silent 'cheat-command-to-string)

(defun cheat-read-sheet-name (&optional prompt)
  "Get the name of an existing cheat sheet, prompting with completion and
  history.

The name of the sheet read is stored in *cheat-last-sheet* unless it was blank."
  (let* ((default (when (blank prompt) *cheat-last-sheet*))
         (prompt (or prompt
                     (if (not (blank default))
                         (concat "Cheat name (default: " default "): ")
                         "Cheat name: ")))
         (name (completing-read prompt
                                (cheat-sheets-list t)
                                nil
                                t
                                nil
                                '*cheat-sheet-history*
                                default)))
    (when (not (blank name))
      (setq *cheat-last-sheet* name))
    name))

(defun cheat-sheets-list (&optional fetch-if-missing-or-stale)
  "Get a list of all cheat sheets.

Return the cached list in *cheat-sheets-cache-file* if it's
readable and `cheat-cache-stale-p' returns nil.

When there is no cache or a stale cache, and
FETCH-IF-MISSING-OR-STALE is non-nil, cache the list and then
return it.

Otherwise return nil."
  (cond ((and (file-readable-p *cheat-sheets-cache-file*)
              (not (cheat-cache-stale-p)))
         (save-excursion
           (let* ((buffer (find-file *cheat-sheets-cache-file*))
                  (sheets (split-string (buffer-string))))
             (kill-buffer buffer)
             sheets)))
        (fetch-if-missing-or-stale
         (cheat-cache-list)
         (cheat-sheets-list))
        (t nil)))

(defun cheat-fetch-list ()
  "Fetch a fresh list of all cheat sheets."
  (nthcdr 3 (split-string (cheat-command-to-string "sheets"))))

(defun cheat-cache-list ()
  "Cache the list of cheat sheets in *cheat-sheets-cache-file*.  Return the
  list."
  (when (not (file-exists-p *cheat-directory*))
    (make-directory *cheat-directory*))
  (save-excursion
    (let ((buffer (find-file *cheat-sheets-cache-file*))
          (sheets (cheat-fetch-list)))
      (insert (string-join "\n" sheets))
      (basic-save-buffer)
      (kill-buffer buffer)
      sheets)))

(defun cheat-cache-stale-p ()
  "Non-nil if the cache in *cheat-sheets-cache-file* is more than
  *cheat-cache-ttl* seconds old.q

If the cache file does not exist then it is considered stale.

Also see `cheat-cache-sheets'."
  (or (null (file-exists-p *cheat-sheets-cache-file*))
      (let* ((now (float-time (current-time)))
             (last-mod (float-time (sixth (file-attributes
             *cheat-sheets-cache-file*))))
             (age (- now last-mod)))
        (> age *cheat-cache-ttl*))))

(defun cheat-body (name)
  "Call out to Ruby to load the YAML and return just the body."
  (shell-command-to-string
   (concat "ruby -ryaml -e '"
           "puts YAML.load_file(File.expand_path(\"~/.cheat/"
           name ".yml\")).to_a[0][-1]'")))

(defun url-http-post (url args)
  "Send ARGS to URL as a POST request."
  (let ((url-request-method "POST")
        (url-request-extra-headers
         '(("Content-Type" . "application/x-www-form-urlencoded")))
        (url-request-data
         (concat (mapconcat (lambda (arg)
                              (concat (url-hexify-string (car arg))
                                      "="
                                      (url-hexify-string (cdr arg))))
                            args
                            "&")
                 "\r\n")))
    ;; `kill-url-buffer'      to discard the result
    ;; `switch-to-url-buffer' to view the results (debugging).
    (url-retrieve url 'kill-url-buffer)))

(defun kill-url-buffer (status)
  "Kill the buffer returned by `url-retrieve'."
  (kill-buffer (current-buffer)))

(defun switch-to-url-buffer (status)
  "Switch to the buffer returned by `url-retreive'.
    The buffer contains the raw HTTP response sent by the server."
  (switch-to-buffer (current-buffer)))

(defun post-cheat (title body &optional new)
  (let ((uri (concat "http://" *cheat-uri* "/w/" (if new "" title))))
    (url-http-post uri `(("sheet_title" . ,title)
                         ("sheet_body"  . ,body)
                         ("from_gem"    . "1")))))

(defun buffer->cheat (name)
  (substring name 7 (- (length name) 1)))

(defun cheat->buffer (name)
  (concat "*cheat-" name "*"))

(provide 'cheat)