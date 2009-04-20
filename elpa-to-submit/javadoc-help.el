;;; javadoc-help.el --- Javadoc-Help.  Look up Java class on online javadocs in browser.
;;
;; Copyright (C) 2008 William W. Wong
;;
;; Author: William W. Wong <williamw520(AT)yahoo(DOT)com>
;; Created: February, 2008
;; Version: 1.0
;; Keywords: javadoc, help, lookup, java

;; This file is not part of GNU Emacs.

;;; License
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License version 2 as
;; published by the Free Software Foundation.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.
;;

;;; Commentary:
;;
;;  Javadoc-Help is an add-on module for Emacs that let you search a class
;;  through multiple online and local javadocs quickly, and view the found
;;  class documentation in the system web browser.
;;

;;; Overview:
;;
;;  - Look up the javadoc of a Java class.
;;  - Search through multiple javadocs.
;;  - Search through online or local javadoc.
;;  - Search can be the class name, package name, partial name, or regex.
;;  - Support multiple search results.
;;  - Display the doc for the class, the package, or the main page.
;;

;;; Installation:
;;
;;  Copy the javadoc-help.el file to your load-path directory.
;;  It's usually at ~/elisp/.  It's set in your .emacs like this:
;;    (setq load-path (append (list (expand-file-name "~/elisp")) load-path))
;;
;;  Next add the following to your .emacs startup file.
;;    (require 'javadoc-help)
;;
;;  or add the autoloads for the public command functions.
;;    (autoload 'javadoc-lookup       "javadoc-help" "Look up Java class in Javadoc."   t)
;;    (autoload 'javadoc-help         "javadoc-help" "Open up the Javadoc-help menu."   t)
;;    (autoload 'javadoc-set-predefined-urls  "javadoc-help" "Set pre-defined urls."    t)
;;

;;; Configuration:
;;
;;  Assign the commands to some keys in your .emacs file.
;;
;;  Examples below assign a set of keys to the javadoc-help functions.
;;    (global-set-key [(f1)]          'javadoc-lookup)  ; F1 to lookup
;;    (global-set-key [(shift f1)]    'javadoc-help)    ; Shift-F1 to bring up menu
;;
;;  Javadoc-help uses browse-url to launch the system web browser.  Make sure
;;  it's working for your platform.  Try it out with, M-x browse-url.  Usually
;;  browse-url defaults to the OS default browser.  Some the OS default browser
;;  might not be set up.  Use 'M-x customize-option' browse-url-browser-function
;;  to pick a specific browser, (like setting Firefox as the browser to use).
;;

;;; Usage:
;;
;;  Set up the javadocs by going to the Javadoc-help menu.  You can then add
;;  a url-based javadoc or a local file-based javadoc using the 'u' or 'f'
;;  command key.  The javadoc urls should be pointing to the main index 
;;  directory of the javadoc, which containing the allclasses-frame.html file.
;;  For example, http://commons.apache.org/lang/api-release/, /opt/jsee/docs/api/, or
;;  c:/jdk/docs/api/.  The entered javadocs are saved persistently.
;;
;;  After adding the javadoc url, try the 'o' command key to open the main
;;  index page of the javadoc in the browser.
;;
;;  To look up the javadoc for a class, invoke the javadoc-lookup (F1) command.
;;  Type in the class name to look up.  The name near the cursor in the current
;;  buffer is automatically used as the initial input.  The search term can be
;;  a partial class name, a package name, or it can be a regex.  For example,
;;    Connection
;;    String
;;    .*lang.*String
;;    java.io
;;
;;  The lookup might produce multiple matches.  The *Javadoc-Search-Result* 
;;  window offers a number of commands to view the class, the package, or the
;;  main javadoc page.
;;
;;  The search term history is accessable via the up/down arrows during input.
;;
;;  If you prefer setting up the javadoc urls in your .emacs file, you can call
;;  javadoc-set-predefined-urls in .emacs to set up the pre-defined javadoc urls.
;;  e.g.
;;    (javadoc-set-predefined-urls '("c:/jdk/docs/api" "/opt/jsee/docs/api"))
;;

;;; Acknowledgements:
;;
;;

;;; History:
;;
;;  2008/02/29 William Wong
;;      Daily usage and general testing done.
;;      Version 1.0
;;
;;  2008/02/20 William Wong
;;      Major features completed.
;;
;;  2008/02/09 William Wong
;;      Start project.
;;      Version 0.0
;;

;;;
;;  Todo:
;;
;;
;;


;;; Code:
;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Public section
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; User Configuration Variables
;;
;;  Customize the options by:
;;   'M-x customize-option' javadoc-help-setting-file
;;   'M-x customize-option' javadoc-help-cache-dir
;;

(defgroup javadoc-help nil
  "Look up Java class on online Javadocs in web browser."
  :link '(emacs-library-link :tag "Source Lisp File" "javadoc-help.el")
  :group 'editing
  :prefix "javadoc-")

(defcustom javadoc-help-setting-file (expand-file-name "~/.javadoc-help")
  "*Filename to store Javadoc-Help's settings."
  :type 'string
  :group 'javadoc-help)

(defcustom javadoc-help-cache-dir (expand-file-name "~/.javadoc-cache")
  "*Directory name to cache the downloaded Javadoc files."
  :type 'string
  :group 'javadoc-help)


;;; User callable functions

(defun javadoc-lookup ()
  "Look up Java class in Javadoc."
  (interactive)
  (let* ((initial-search-term (thing-at-point 'java-identifier))
         (search-term (jdh-get-search-input initial-search-term))
         match-list
         single-result-url)
    (when (and search-term 
               (> (length search-term) 0))
      (setq match-list (jdh-search-in-files search-term))
      (if (null match-list)
          (message (format "%s not found" search-term))
        ; Present the multi-result to user, or launch single-result-url in browser.
        (setq single-result-url (jdh-select-result match-list))
        (when single-result-url
          (message (format "Single matched item.  Launching web browser on %s" single-result-url))
          (browse-url single-result-url)))
        )
    )
  )

(defun javadoc-help ()
  "Bring up the Javadoc Help Menu to edit the Javadoc URLs."
  (interactive)
  (switch-to-buffer (get-buffer-create jdh--jmenu-buffer))
  (jdh-jmenu-redraw)
  (goto-char (point-min))
  (forward-line jdh--jmenu-table-offset)
  (jdh-jmenu-mode)
  )

(defun javadoc-set-predefined-urls (url-list)
  "Set the list of the pre-defined urls."
  (if (not (listp url-list))
      (message "Predefined url-list is not passed in as a list.")
    (setq *jdh-predefined-urls* url-list))
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Private section
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; Program global variables:

(defvar *jdh-javadocs* ()
  "List of javadoc records.  The list is (javadoc1 javadoc2 ...) where each javadoc is (url attr1 attr2 ...)"
  )

(defvar *jdh-predefined-urls* ()
  "List of javadoc urls."
  )

(defvar *jdh-url-input-history* ()
  "List of URL input history."
  )

(defvar *jdh-dir-input-history* nil
  "List of file input history."
  )

(defvar *jdh-search-input-history* ()
  "List of URL input history."
  )

(defvar *jdh-matched-search* ()
  "The list of matched search term."
  )


;;; Constants
(defconst jdh--file-allclasses  "allclasses-frame.html")
(defconst jdh--jmenu-buffer     "*Javadoc-Help*")
(defconst jdh--jmenu-mode-name  "*Javadoc-Help*")
(defconst jdh--smenu-buffer     "*Javadoc-Search-Result*")
(defconst jdh--smenu-mode-name  "*Javadoc-Search-Result*")
(defconst jdh--temp-buffer      "*javadoc-help-temp-buffer*")
(defconst jdh--file-magic       "WJDH")
(defconst jdh--file-version     1)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Javadoc url record functions

(defun jdh-javadoc-new (url refreshed enabled predefined)
  "Construct a Javadoc url record, saving its javadoc-url in the list."
  ; record [url refreshed enabled predefined reserved reserved reserved reserved reserved reserved]
  (list url refreshed enabled predefined nil nil nil nil nil nil)
  )

(defun jdh-javadoc-url (javadoc)
  "Return the url part of a javadoc record."
  (car javadoc)
  )

(defun jdh-javadoc-set-url (javadoc url)
  "Set the url of a javadoc"
  (setcar javadoc url)
  )

(defun jdh-javadoc-refreshed (javadoc)
  "Return the url part of a javadoc record."
  (car (nthcdr 1 javadoc))
  )

(defun jdh-javadoc-set-refreshed (javadoc refreshed)
  "Set the refreshed field of a javadoc"
  (if (> (length javadoc) 1)
      (setcar (nthcdr 1 javadoc) refreshed)
    (setcdr javadoc (cons refreshed nil)))
  )

(defun jdh-javadoc-enabled (javadoc)
  "Return the url part of a javadoc record."
  (car (nthcdr 2 javadoc))
  )

(defun jdh-javadoc-set-enabled (javadoc enabled)
  "Set the enabled field of a javadoc"
  (if (> (length javadoc) 2)
      (setcar (nthcdr 2 javadoc) enabled)
    (setcdr (nthcdr 1 javadoc) (cons enabled nil)))
  )

(defun jdh-javadoc-predefined (javadoc)
  "Return the url part of a javadoc record."
  (car (nthcdr 3 javadoc))
  )

(defun jdh-javadoc-set-predefined (javadoc predefined)
  "Set the predefined field of a javadoc"
  (if (> (length javadoc) 3)
      (setcar (nthcdr 3 javadoc) predefined)
    (setcdr (nthcdr 2 javadoc) (cons predefined nil)))
  )

(defun jdh-javadocs-add (javadoc)
  "Add a javadoc to the javadoc list"
  ;; Remove existing duplicate javadoc.
  (let* ((url (jdh-javadoc-url javadoc))
         (existing-index (jdh-javadocs-find-by-url url)))
    (if existing-index
        (jdh-javadocs-remove-at existing-index))
    )
  (setq *jdh-javadocs* (append *jdh-javadocs* (list javadoc)))
  )

(defun jdh-javadocs-get (index)
  "Get the javadoc by index from the javadoc list."
  (if index
      (nth index *jdh-javadocs*)
    nil)
  )

(defun jdh-javadocs-find-by-url (url)
  "Find a javadoc index by url from the javadoc list."
  (let ((index 0)
        (found-index nil))
    (mapc
     (lambda (url-record)
       (if (and (null found-index)
                (equal url (jdh-javadoc-url url-record)))
           (setq found-index index))
       (setq index (1+ index)))
     *jdh-javadocs*)
    found-index)
  )

(defun jdh-javadocs-remove (javadoc)
  "Remove a javadoc from the javadoc list."
  (setq *jdh-javadocs* (remove javadoc *jdh-javadocs*))
  (jdh-javadocs-save)
  )

(defun jdh-javadocs-remove-at (index)
  "Remove a javadoc by index from the javadoc list."
  (let ((javadoc (jdh-javadocs-get index)))
    (setq *jdh-javadocs* (remove javadoc *jdh-javadocs*)))
  (jdh-javadocs-save)
  )

(defun jdh-javadoc-to-str (javadoc)
  "Convert a javadoc to a string."
  (let ((url (jdh-javadoc-url javadoc))
        (attrs ()))
    (if (not (jdh-javadoc-enabled javadoc))
        (setq attrs (push "disabled" attrs)))
    (if (not (jdh-javadoc-refreshed javadoc))
        (setq attrs (push "refresh needed" attrs)))
    (if (jdh-javadoc-predefined javadoc)
        (setq attrs (push "predefined" attrs)))
    (if attrs
        (format "%-60s  [%s]" url (mapconcat 'identity attrs ", "))
      url)
    )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Search helper functions

(defun jdh-get-search-input (initial-search-term)
  "Get the search term input from user."
  (interactive)
  (let* ((input-str (read-from-minibuffer 
                     "Search Name: "
                     initial-search-term
                     nil
                     nil
                     '*jdh-search-input-history*
                     initial-search-term)))
    (string-trim input-str)
    )
  )

(defun jdh-search-in-files (search-term)
  "Search the search term in the javadoc files."
  (let ((allclasses-files (jdh-get-allclasses-files))
        (match-list nil))
    (if (and search-term 
             (> (length search-term) 0))
        (mapc (lambda (file-record)
                (when file-record
                  (let* ((url-file-base (car file-record))
                         (pathname (cdr file-record))
                         (found-list (jdh-search-file search-term url-file-base pathname))
                         )
                    (setq match-list (append match-list found-list))
                    )))
              allclasses-files
              ))
    match-list
    )
  )

(defun jdh-search-file (search-term url-file-base file-path)
  "Search the search term in a javadoc file."
  (save-window-excursion
    (set-buffer (get-buffer-create jdh--temp-buffer))
    (let ((match-list nil)
          (search-re-expr (concat "<A HREF=.*" search-term ".*\\.html"))
          found-filename)
      (insert-file-contents file-path nil nil nil t)    ; load file
      (goto-char (point-min))
      ; Search for all occurrences of the search-term
      (while (re-search-forward search-re-expr (point-max) t)
        (progn
          (beginning-of-line)
          ; Extract the found filename between the quotes
          (re-search-forward "<A HREF=\"\\([^\"]*\\)\"")
          (setq found-filename (buffer-substring (match-beginning 1) (match-end 1)))
          (push (cons url-file-base found-filename) match-list)
          ))
      (reverse match-list)))
  )

(defun jdh-get-allclasses-files ()
  "Convert the list in *jdh-javadocs* to a list of allclasses files."
  (mapcar (lambda (javadoc)
            (let* ((url-file-base (jdh-javadoc-url javadoc))
                   (allclasses-file (jdh-get-allclasses-local-file url-file-base)))
              (if (and (jdh-javadoc-enabled javadoc)
                       (file-readable-p allclasses-file))
                  (cons url-file-base allclasses-file)
                nil))
              )
          *jdh-javadocs*)
  )

(defun jdh-get-allclasses-url-file (url-file-base)
  "Convert the url-file-base to an allclasses url."
  (concat-path url-file-base jdh--file-allclasses)
  )

(defun jdh-get-allclasses-local-file (url-file-base)
  "Convert the url-file-base to an allclasses file."
  (let ((allclasses-file (jdh-get-allclasses-url-file url-file-base)))
    (if (jdh-http-p url-file-base)
        (concat javadoc-help-cache-dir "/" (jdh-convert-url-to-file allclasses-file))
      allclasses-file))
  )

(defun jdh-convert-url-to-file (url)
  "Convert a url to a local filename, with escape char."
  (let* ((url0 (replace-regexp-in-string "/" "-" url))
         (url1 (replace-regexp-in-string "[^[:alnum:]\\._\\-]" "_" url0))
         (url2 (md5 url))
         (len (min (length url1) 24)))
    (concat (substring url1 0 len) url2))
  )

(defun jdh-select-result (match-list)
  "Select single result item automatically or present the result list to the user."
  (if (= (length match-list) 1)
      (jdh-get-match-url (car match-list))
    (jdh-present-matches match-list)
    nil)
  )

(defun jdh-present-matches (match-list)
  "Presetnt the result of the class search to let the user pick one to browse."
  (interactive)
  (setq *jdh-matched-search* match-list)
  (switch-to-buffer-other-window (get-buffer-create jdh--smenu-buffer))
  (jdh-smenu-redraw)
  (goto-char (point-min))
  (forward-line jdh--smenu-table-offset)
  (jdh-smenu-mode)
  )

(defun jdh-get-match-url (match-item)
  "Extract the normalized url from a matched item."
  (let* ((url-file-base (car match-item))
         (file-path (cdr match-item))
         (url (concat-path url-file-base file-path)))
    (jdh-normalize-url url))
  )

(defun jdh-get-match-base-url (match-item)
  "Extract the base url from a matched item."
  (car match-item)
  )

(defun jdh-normalize-url (url)
  "Normalize local file url."
  (cond ((jdh-http-p url) url)
        ((string-match "^/" url) (concat "file://" url))
        (t (concat "file:///" url)))
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Thing-at-point helper functions

; Set up bound searching method for (thing-at-point 'java-identifier)
(require 'thingatpt)
(put 'java-identifier 'bounds-of-thing-at-point 
     (lambda ()
       "Determine where a Java identifier begins and ends for (thing-at-point 'java-identifier)"
       (save-excursion
         (skip-chars-backward "[a-zA-Z0-9_\\$]")            ; move back through $_ALPHANUM
         (if (looking-at "[a-zA-Z_\\$][a-zA-Z0-9_\\$]+")    ; match ahead through $_ALPHANUM
             (cons (point) (match-end 0))
           nil))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; *Javadoc-Help* menu functions

(defconst jdh--jmenu-table-offset 7)

(defun jdh-jmenu-redraw ()
  "Redraw the javadoc-help javadocs in the buffer named `*Javadoc-Help*'."
  (save-excursion
    (save-window-excursion
      (setq inhibit-read-only t)
      (erase-buffer)
      (insert (concat jdh--jmenu-buffer "\n\n"))
      (insert "Javadoc URL Management.\n")
      (insert "Command: u, f, o, r, e, q, ctrl-d, shift-d, shift-x.  Press '?' for help.\n\n")
      (insert "% Javadoc URL\n")
      (insert "- -------------------------------------------------------------------------\n")
      (mapc
       (lambda (javadoc)
         (insert (format "  %s\n" (jdh-javadoc-to-str javadoc)))
         )
       *jdh-javadocs*)
      ))
  )

(defun jdh-jmenu-redraw-at (&optional index)
  "Redraw the jmenu screen and preserve the cursor's position."
  (let* ((current-index (jdh-jmenu-get-javadoc-index))
         (index-to-advance (if (null index) current-index index)))
    (jdh-jmenu-redraw)
    (goto-char (point-min))
    (forward-line (+ jdh--jmenu-table-offset index-to-advance)))
  )

(defun jdh-jmenu-get-javadoc-index ()
  "Return a javadoc index under the cursor.  Index might be out of range."
  (1- (- (line-number-at-pos) jdh--jmenu-table-offset))
  )

(defun jdh-jmenu-valid-javadoc ()
  "Check whether the cursor is on a valid javadoc"
  (jdh-javadocs-get (jdh-jmenu-get-javadoc-index))
  )

(defun jdh-refresh-url (url-file-base)
  "Refresh and download the allclasses-file from the online javadoc url."
  (if (not (file-accessible-directory-p javadoc-help-cache-dir))
      (make-directory javadoc-help-cache-dir))
  (when (jdh-http-p url-file-base)
    (let* ((allclasses-url  (jdh-get-allclasses-url-file url-file-base))
           (allclasses-file (jdh-get-allclasses-local-file url-file-base)))
      (jdh-download-url allclasses-url allclasses-file)))
  )

(defun jdh-download-url (url file-to-save)
  "Download the content of a url to a file"
  (let ((buffer (url-retrieve-synchronously url))
        (status nil))
    (unwind-protect
        (with-current-buffer buffer
          (save-excursion
            (url-http-parse-response)
            (goto-char url-http-end-of-headers)
            (if (or (< url-http-response-status 200)
                    (>= url-http-response-status 300))
                (error (format "Fail to download %s.  Http Response code: %d" url url-http-response-status))
              (write-region (point) (point-max) file-to-save)
              (setq status t))
            )
          )
      (kill-buffer buffer))
    status)
  )

(defun jdh-jmenu-add-url ()
  "Add Javadoc URL."
  (interactive)
  (let* ((default-url (car *jdh-url-input-history*))
         (prompt-str (format "Javadoc URL (http://sample/javadoc/): "))
         (input-str (read-from-minibuffer 
                 prompt-str
		 nil
		 nil
		 nil
		 '*jdh-url-input-history*
		 default-url)))
    (setq input-str (jdh-jmenu-parse-input input-str))
    (if (not (and input-str
                  (jdh-refresh-url input-str)))
        (message (format "Failed to refresh %s" input-str))
      (jdh-javadocs-add (jdh-javadoc-new input-str t t nil))
      (jdh-javadocs-save)
      (jdh-jmenu-redraw-at)
      (message (format "Javadoc url %s added" input-str)))
    )
  )

(defun jdh-jmenu-add-directory ()
  "Add Javadoc directory."
  (interactive)
  (let* ((directory-name (jdh-jmenu-input-directory))
         allclasses-file)
    (when directory-name
      ;; and-check api file, add file record
      (setq allclasses-file (jdh-get-allclasses-local-file directory-name))
      (if (not (file-readable-p allclasses-file))
          (message (format "%s is not readable.  Make sure the directory is correct." allclasses-file))
        (jdh-javadocs-add (jdh-javadoc-new directory-name t t nil))
        (jdh-javadocs-save)
        (jdh-jmenu-redraw-at)
        (message (format "%s added." directory-name))
        )
      ))
  )

(defun jdh-jmenu-input-directory ()
  "Input Javadoc directory."
  (interactive)
  (let* ((default-dir *jdh-dir-input-history*)
         (prompt-str "Javadoc file: ")
         (input-str (if (null default-dir)
                        (read-file-name prompt-str)
                      (read-file-name prompt-str default-dir default-dir nil))))
    (setq input-str (jdh-jmenu-parse-input input-str))
    (if (null input-str)
        nil
      (setq *jdh-dir-input-history* input-str)
      (if (file-directory-p input-str)
          input-str
        (message (format "%s is not a directory." input-str))
        nil)))
  )

(defun jdh-jmenu-parse-input (input-url)
  "Do any parsing on the input url/directory."
  (setq input-url (string-trim input-url))
  (if (equal input-url "")
      nil
    (let ((suffix-pos (string-match "index.html$" input-url)))
      (if suffix-pos
          (setq input-url (substring input-url 0 suffix-pos))))
    input-url)
  )

(defun jdh-jmenu-advance-cursor ()
  (forward-line 1)
  (when (null (jdh-jmenu-valid-javadoc))
    (goto-char (point-min))
    (forward-line jdh--jmenu-table-offset))
  )

(defun jdh-jmenu-refresh-javadoc ()
  "Refresh the javadoc url from the source."
  (interactive)
  (let ((javadoc (jdh-jmenu-valid-javadoc)))
    (if (null javadoc)
        (message "No valid javadoc selected.")
      (let ((base-url (jdh-javadoc-url javadoc)))
        (message (format "Refreshing %s" base-url))
        (if (jdh-http-p base-url)
          (if (not (jdh-refresh-url base-url))
              (message (format "Failed to refresh %s" base-url))
            (message (format "Refreshed %s" base-url)))
          (message (format "Refreshed local directory %s" base-url)))
        (jdh-javadoc-set-refreshed javadoc t)
        (jdh-javadocs-save)
        (jdh-jmenu-redraw-at)
        (message (format "Refreshed %s" base-url)))
      ))
  )

(defun jdh-jmenu-enable-javadoc ()
  "Enable/disable the javadoc url."
  (interactive)
  (let ((javadoc (jdh-jmenu-valid-javadoc)))
    (if (null javadoc)
        (message "No valid javadoc selected.")
      (jdh-javadoc-set-enabled javadoc (not (jdh-javadoc-enabled javadoc)))
      (jdh-jmenu-redraw-at)
      (jdh-javadocs-save)
      (message (format "%s" (if (jdh-javadoc-enabled javadoc) "Enabled" "Disabled")))
      ))
  )

(defun jdh-jmenu-open-url ()
  "Open the main page of the javadoc under cursor in the system web browser."
  (interactive)
  (let ((javadoc (jdh-jmenu-valid-javadoc)))
    (if (null javadoc)
        (message "No valid javadoc selected.")
      (let* ((base-url (jdh-javadoc-url javadoc))
             (main-url (jdh-normalize-url(concat-path base-url "index.html"))))
;       (jdh-close-buffer)
        (message (format "Launching web browser on %s" main-url))
        (browse-url main-url))))
  )

(defun jdh-jmenu-mark-char (mark-char)
  "Set a mark char on the javadoc line at cursor."
  (when (jdh-jmenu-valid-javadoc)
    (setq inhibit-read-only t)
    (beginning-of-line)
    (delete-char 1)
    (insert mark-char)
    (jdh-jmenu-advance-cursor))
  )

(defun jdh-jmenu-mark-delete ()
  "Mark the javadoc at cursor for delete."
  (interactive)
  (let ((javadoc (jdh-jmenu-valid-javadoc)))
    (if (and javadoc (jdh-javadoc-predefined javadoc))
        (message "Cannot delete predefined javadoc.  Use javadoc-set-predefined-urls() to change predefined urls.")
      (jdh-jmenu-mark-char "D")))
  )

(defun jdh-jmenu-unmark-delete ()
  "Unmark the javadoc at cursor from deletion."
  (interactive)
  (jdh-jmenu-mark-char " ")
  )

(defun jdh-jmenu-commit-deletions ()
  "Commit deletion on the marked javadocs."
  (interactive)
  (goto-char (point-min))
  (forward-line jdh--jmenu-table-offset)
  (let ((items-to-delete (list)))
    (dotimes (i (length *jdh-javadocs*))
      (beginning-of-line)
      (if (looking-at "D")
          (push i items-to-delete))
      (forward-line 1))
    (dolist (index items-to-delete)
      (let ((javadoc (jdh-javadocs-get index)))
        (setq *jdh-javadocs* (remove javadoc *jdh-javadocs*))))
    )
  (jdh-jmenu-redraw)
  (forward-line jdh--jmenu-table-offset)
  (jdh-javadocs-save)
  (message "Committed deletion")
  )

(defvar *jdh-jmenu-mode-map* nil)
(progn
  (setq *jdh-jmenu-mode-map* (make-keymap))
  (suppress-keymap *jdh-jmenu-mode-map* t)
  (define-key *jdh-jmenu-mode-map* "u"        'jdh-jmenu-add-url)
  (define-key *jdh-jmenu-mode-map* "f"        'jdh-jmenu-add-directory)
  (define-key *jdh-jmenu-mode-map* "o"        'jdh-jmenu-open-url)
  (define-key *jdh-jmenu-mode-map* "r"        'jdh-jmenu-refresh-javadoc)
  (define-key *jdh-jmenu-mode-map* "e"        'jdh-jmenu-enable-javadoc)
  (define-key *jdh-jmenu-mode-map* "\C-d"     'jdh-jmenu-mark-delete)
  (define-key *jdh-jmenu-mode-map* "\S-d"     'jdh-jmenu-unmark-delete)
  (define-key *jdh-jmenu-mode-map* "\S-x"     'jdh-jmenu-commit-deletions)
  (define-key *jdh-jmenu-mode-map* "n"        'next-line)
  (define-key *jdh-jmenu-mode-map* " "        'next-line)
  (define-key *jdh-jmenu-mode-map* "p"        'previous-line)
  (define-key *jdh-jmenu-mode-map* "q"        'jdh-close-buffer)
  (define-key *jdh-jmenu-mode-map* "\C-g"     'jdh-close-buffer)
  (define-key *jdh-jmenu-mode-map* "?"        'describe-mode)
  )

(defun jdh-jmenu-mode ()
  "Major mode for listing and editing the list of javadoc-help javadocs.
The following commands are available.
\\<*jdh-jmenu-mode-map*>
\\[jdh-jmenu-add-url] -- add a new online javadoc url. e.g. http://commons.apache.org/lang/api/
\\[jdh-jmenu-add-directory] -- add a new local javadoc directory. e.g. c:/jdk/docs/api, or /opt/jdk/docs/api
\\[jdh-jmenu-open-url] -- launch the javadoc URL in the system web browser.
\\[jdh-jmenu-refresh-javadoc] -- refresh the javadoc url from the source.
\\[jdh-jmenu-enable-javadoc] -- enable/disable the javadoc url for searching.
\\[jdh-jmenu-mark-delete] -- mark this javadoc to be deleted.
\\[jdh-jmenu-unmark-delete] -- unmark the javadoc from deletion.
\\[jdh-jmenu-commit-deletions] -- delete javadocs marked with `\\[jdh-jmenu-mark-delete]'.
\\[next-line] -- move to the next line
\\[previous-line] -- move to the previous line
\\[jdh-close-buffer] -- close the *Javadoc-Help* window
"
  (kill-all-local-variables)
  (use-local-map *jdh-jmenu-mode-map*)
  (setq truncate-lines t)
  (setq buffer-read-only t)
  (setq major-mode 'jdh-jmenu-mode)
  (setq mode-name jdh--jmenu-mode-name)
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; *Javadoc-Search-Result* menu functions

(defconst jdh--smenu-table-offset 7)

(defun jdh-smenu-redraw ()
  "Redraw the javadoc-help javadocs in the buffer named `*Javadoc-Search-Result*'."
  (save-excursion
    (save-window-excursion
      (setq inhibit-read-only t)
      (erase-buffer)
      (insert (concat jdh--smenu-buffer "\n\n"))
      (insert "Javadoc search matches.  Select one for browsing.\n")
      (insert "Command: [enter], o, v, m, p, q.  Press '?' for help.\n\n")
      (insert " Javadoc Matches\n")
      (insert " -------------------------------------------------------------------------\n")
      (mapc
       (lambda (matched-item)
         (insert (format " %s\n" (jdh-matched-to-str matched-item)))
         )
       *jdh-matched-search*)
        ))
  )

(defun jdh-matched-to-str (matched-item)
  (let ((url-file-base (car matched-item))
        (file-path (cdr matched-item)))
    (format "%-60s  [%s]" (jdh-file-to-class file-path) url-file-base))
  )

(defun jdh-file-to-class (file-path)
  "pkg1/pkg2/class3.html => pkg1.pkg2.class3"
  (let ((class-path (replace-regexp-in-string "\\.html$" "" file-path)))
    (replace-regexp-in-string "/" "." class-path))
  )

(defun jdh-smenu-get-javadoc-index ()
  "Return a javadoc index under the cursor.  Index might be out of range."
  (1- (- (line-number-at-pos) jdh--smenu-table-offset))
  )

(defun jdh-smenu-valid-javadoc ()
  "Check whether the cursor is on a valid javadoc"
  (let ((index (jdh-smenu-get-javadoc-index)))
    (cond ((< index 0) nil)
          ((>= index (length *jdh-matched-search*)) nil)
          (t (nth index *jdh-matched-search*))))
  )

(defun jdh-smenu-advance-cursor ()
  (forward-line 1)
  (when (null (jdh-smenu-valid-javadoc))
    (goto-char (point-min))
    (forward-line jdh--smenu-table-offset))
  )

(defun jdh-smenu-launch-url (close-buffer)
  "Open the javadoc under cursor in the system web browser."
  (let ((matched-item (jdh-smenu-valid-javadoc)))
    (if (null matched-item)
        (message "No valid javadoc selected.")
      (let ((url (jdh-get-match-url matched-item)))
        (when close-buffer
          (jdh-close-buffer))
        (message (format "Launching web browser on %s" url))
        (browse-url url)
        )))
  )

(defun jdh-smenu-open-url ()
  "Open the javadoc under cursor in the system web browser."
  (interactive)
  (jdh-smenu-launch-url t)
  )

(defun jdh-smenu-visit-url ()
  "Visit the javadoc under cursor in the system web browser."
  (interactive)
  (jdh-smenu-launch-url nil)
  )

(defun jdh-smenu-open-main ()
  "Open the javadoc under cursor in the system web browser."
  (interactive)
  (let ((matched-item (jdh-smenu-valid-javadoc)))
    (if (null matched-item)
        (message "No valid javadoc selected.")
      (let* ((base-url (jdh-get-match-base-url matched-item))
             (main-url (jdh-normalize-url(concat-path base-url "index.html"))))
        (message (format "Launching web browser on %s" main-url))
        (browse-url main-url)
        )))
  )

(defun jdh-smenu-open-parent ()
  "Open the parent package of the javadoc under cursor in the browser."
  (interactive)
  (let ((matched-item (jdh-smenu-valid-javadoc)))
    (if (null matched-item)
        (message "No valid javadoc selected.")
      (let* ((url (jdh-get-match-url matched-item))
             (parent-url (path-parent url))
             (package-url (concat-path parent-url "package-summary.html")))
        (message (format "Launching web browser on %s" parent-url))
        (browse-url package-url)
        )))
  )

(defun path-parent (path)
  (let ((continue t)
        (pos)
        (previous-pos 0))
    (while continue
      (setq pos (string-match "/" path (1+ previous-pos)))
      (if pos
          (setq previous-pos pos)
        (setq continue nil)))
    (substring path 0 previous-pos))
  )

(defvar *jdh-smenu-mode-map* nil)
(progn
  (setq *jdh-smenu-mode-map* (make-keymap))
  (suppress-keymap *jdh-smenu-mode-map* t)
  (define-key *jdh-smenu-mode-map* "q"        'jdh-close-buffer)
  (define-key *jdh-smenu-mode-map* "\C-g"     'jdh-close-buffer)
  (define-key *jdh-smenu-mode-map* "o"        'jdh-smenu-open-url)
  (define-key *jdh-smenu-mode-map* "\C-m"     'jdh-smenu-open-url)
  (define-key *jdh-smenu-mode-map* "v"        'jdh-smenu-visit-url)
  (define-key *jdh-smenu-mode-map* "m"        'jdh-smenu-open-main)
  (define-key *jdh-smenu-mode-map* "p"        'jdh-smenu-open-parent)
  (define-key *jdh-smenu-mode-map* "n"        'next-line)
  (define-key *jdh-smenu-mode-map* " "        'next-line)
  (define-key *jdh-smenu-mode-map* "?"        'describe-mode)
  )

(defun jdh-smenu-mode ()
  "Major mode for selecting a javadoc to browse.
The following commands are available.
\\<*jdh-smenu-mode-map*>
\\[jdh-smenu-open-url] -- Open the selected javadoc in the system web browser.
\\[jdh-smenu-visit-url] -- Open the selected javadoc in the browser.  Don't close search result buffer.
\\[jdh-smenu-open-main] -- Open the main javadoc index page in the browser.
\\[jdh-smenu-open-parent] -- Open the parent package javadoc page in the browser.
\\[next-line] -- move to the next line
\\[jdh-close-buffer] -- close the *Javadoc-Search-Result* window
"
  (kill-all-local-variables)
  (use-local-map *jdh-smenu-mode-map*)
  (setq truncate-lines t)
  (setq buffer-read-only t)
  (setq major-mode 'jdh-smenu-mode)
  (setq mode-name jdh--smenu-mode-name)
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Javadoc saving and restoring

(defun jdh-javadocs-save ()
  "Save the javadocs to file."
  (let ((data-alist 
         (list 
          (cons 'magic-number jdh--file-magic)
          (cons 'version jdh--file-version)
          (cons 'timestamp (current-time))
          (cons '*jdh-javadocs* *jdh-javadocs*)
          (cons '*jdh-url-input-history* *jdh-url-input-history*)
          (cons '*jdh-dir-input-history* *jdh-dir-input-history*)
          (cons '*jdh-search-input-history* *jdh-search-input-history*))
         ))
    (jdh-javadocs-save-file data-alist javadoc-help-setting-file))
  )

(defun jdh-javadocs-restore ()
  "Load the javadocs from file."
  (let ((data-alist (jdh-javadocs-load-file javadoc-help-setting-file)))
    (when (equal jdh--file-magic (cdr (assoc 'magic-number data-alist)))
      (setq *jdh-javadocs* (cdr (assoc '*jdh-javadocs* data-alist)))
      (setq *jdh-url-input-history* (cdr (assoc '*jdh-url-input-history* data-alist)))
      (setq *jdh-dir-input-history* (cdr (assoc '*jdh-dir-input-history* data-alist)))
      (setq *jdh-search-input-history* (cdr (assoc '*jdh-search-input-history* data-alist)))
      ))
  )

(defun jdh-javadocs-load-file (file)
  "Load the javadoc-list from file."
  (when (and file
             (file-readable-p file))
    (let ((loading-buffer (find-file-noselect file))
          (javadoc-list))
      (setq javadoc-list (with-current-buffer loading-buffer
                        (goto-char (point-min))
                        (read (current-buffer))))
      (kill-buffer loading-buffer)
      javadoc-list))
  )

(defun jdh-javadocs-save-file (data-alist file)
  "Save the data-alist to file."
  (when (and file
             (file-writable-p file))
    (let ((writing-buffer (find-file-noselect file)))
      (with-current-buffer writing-buffer
        (erase-buffer)
        (insert ";; javadoc-help.el saved javadocs.  Do not edit this file.\n")
        (prin1 data-alist (current-buffer))
        (insert "\n")
        (save-buffer))
      (kill-buffer writing-buffer)))
  )

(defun jdh-process-predefined-urls (url-list)
  "Process the list of the pre-defined urls."
  ; Trim the urls
  (setq url-list (mapcar 
                  (lambda (url)
                    (string-trim url))
                  url-list))
  ; Add the predefined url as javadoc
  (mapc
   (lambda (url)
     (let* ((index (jdh-javadocs-find-by-url url))
            (javadoc (jdh-javadocs-get index)))
       (if javadoc
           (jdh-javadoc-set-predefined javadoc t)           ; Just mark the existing javadoc as predefined.
         (jdh-javadocs-add (jdh-javadoc-new url nil t t))   ; Add the predefined javadoc
         )))
   url-list)
  ; Remove any existing predefined javadoc that are not in the new predefined url-list.
  (mapc
   (lambda (javadoc)
     (let ((url (jdh-javadoc-url javadoc)))
       (if (and (jdh-javadoc-predefined javadoc)
                (not (member url url-list)))
           (jdh-javadocs-remove javadoc)))
     )
   *jdh-javadocs*)
  (jdh-javadocs-save)
  )

;; Load from setting file on start up.
(add-hook' after-init-hook
           (lambda ()
             (jdh-javadocs-restore)
             (jdh-process-predefined-urls *jdh-predefined-urls*)
             ))

;; Save to setting file on exit.
(add-hook 'kill-emacs-hook 'jdh-javadocs-save)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Util functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(unless (fboundp 'line-number-at-pos)
  (defun line-number-at-pos (&optional pos)
    "Return (narrowed) buffer line number at position POS.  If POS is nil, use current buffer location."
    (let ((opoint (or pos (point))) start)
      (save-excursion
        (goto-char (point-min))
        (setq start (point))
        (goto-char opoint)
        (forward-line 0)
        (1+ (count-lines start (point))))))
  )

(defun jdh-close-buffer ()
  "Make closing buffer work for both Emacs and XEmacs"
  (interactive)
  (if (not (one-window-p))
      (delete-window)
    (if (fboundp 'quit-window)
        (quit-window)
      (bury-buffer)))
  )

(defun string-ltrim (str)
  (let ((trim-pos (string-match "\\s +$" str)))
    (if trim-pos
        (substring str 0 trim-pos)
      str))
  )

(defun string-rtrim (str)
  (let ((trim-pos (string-match "[^ \t]+" str)))
    (if trim-pos
        (substring str trim-pos)
      str))
  )

(defun string-trim (str)
  (string-rtrim (string-ltrim str))
  )

(defun concat-path (base-path sub-path)
  (cond ((= (length base-path) 0) sub-path)
        ((= (length sub-path) 0)  base-path)
        ((equal (substring base-path (1- (length base-path))) "/") (concat base-path sub-path))
        (t (concat base-path "/" sub-path)))
  )

(defun jdh-http-p (url)
  (eq (string-match "^[Hh][Tt][Tt][Pp][Ss]?://" url) 0)
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Testing and debugging
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Testing/debugging with C-xC-e
(if nil
    (progn
      (global-set-key [(f1)]        'javadoc-lookup)
      (global-set-key [(meta f1)]   'javadoc-help)
      (global-set-key [(shift f1)]  'javadoc-help)
      (global-set-key "\C-x\M-t"    'jdh-debug-test)
      *jdh-javadocs*
      (jdh-javadocs-load-file javadoc-help-setting-file)
      (jdh-javadocs-save-file *jdh-javadocs* javadoc-help-setting-file)
      (jdh-javadocs-save)
      (jdh-javadocs-restore)
      (jdh-get-allclasses-files)
      )

(defun jdh-debug-test ()
  (interactive)
  (skip-chars-b$_ackward "[a-zA-Z0-9_\\$]")
  (message (format "%d " (point)))
  )

(defun jdh-debug-test ()
  (interactive)
  (skip-chars-backward "[[:alnum:]_\\$]")
  (message (format "%d " (point)))
  (if (looking-at "[[:alpha:]_\\$][[:alnum:]_\\$]+")
      (message (format "%d %d" (point) (match-end 0)))
    (message "none"))
  )

)

;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(provide 'javadoc-help)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; javadoc-help.el ends here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

