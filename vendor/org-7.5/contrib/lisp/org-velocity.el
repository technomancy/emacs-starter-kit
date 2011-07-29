;;; org-velocity.el --- something like Notational Velocity for Org.

;; Copyright (C) 2010 Paul M. Rodriguez

;; Author: Paul M. Rodriguez <paulmrodriguez@gmail.com>
;; Created: 2010-05-05
;; Version: 2.3

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation version 2.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; For a copy of the GNU General Public License, search the Internet,
;; or write to the Free Software Foundation, Inc., 59 Temple Place,
;; Suite 330, Boston, MA 02111-1307 USA

;;; Commentary:
;; Org-Velocity.el implements an interface for Org inspired by the
;; minimalist notetaking program Notational Velocity.  The idea is to
;; allow you to maintain, amass and access brief notes on many
;; subjects with minimal fuss.

;; It can be used in two ways: to store and access notes from any
;; buffer a universal bucket file; or as a method for navigating any
;; Org file.

;; The name of the bucket-file (`org-velocity-bucket') and whether to
;; always use it (`org-velocity-always-use-bucket-file') are set
;; through Customize.  If the bucket file is set but not always to be
;; used, then calling Org-Velocity outside of Org-mode uses the bucket
;; file; calling it in Org mode uses the current buffer.  If no bucket
;; file is set then Org-Velocity only works when called from Org.
;; Even if the bucket file is always to be used, calling
;; `org-velocity-read' with an argument will use the current file.

;; The interface, unlike its inspiration, is not incremental.
;; Org-Velocity prompts for search terms in the usual way; if the user
;; has customized `org-velocity-use-completion', completion is offered
;; on the headings in the target file.  If the search multiple times
;; in the target file, a buffer containing a buttonized list of the
;; headings where it occurs is displayed.  Results beyond what can be
;; indexed are discarded.  After clicking on a heading, or typing a
;; character associated with it, the user is taken to the heading.
;; (Typing 0 forces a new heading to be created.)  If
;; `org-velocity-edit-indirectly' is so set, the heading and its
;; subtree are displayed in an indirect buffer.  Otherwise the user is
;; simply taken to the proper buffer and position.

;; If the user simply hits RET at the prompt, without making a choice,
;; then the search is restored for editing.  A blank search quits.
;; This method of selection is obviously not as slick as the original,
;; but probably more useful for a keyboard-driven interface.

;; If the search does not occur in the file the user is offered a
;; choice to create a new heading named with the search.  Org-Velocity
;; will use `org-capture' or `org-remember' if they are loaded,
;; preferring `org-capture'.  Otherwise the user is simply taken to a
;; new heading at the end of the file.

;; Thanks to Richard Riley, Carsten Dominik, Bastien Guerry, and Jeff
;; Horn for their suggestions.

;;; Usage:
;; (require 'org-velocity)
;; (setq org-velocity-bucket (concat org-directory "/bucket.org"))
;; (global-set-key (kbd "C-c v") 'org-velocity-read)

;;; Code:
(require 'org)
(require 'button)
(require 'electric)
(eval-when-compile (require 'cl))

(defgroup org-velocity nil
  "Notational Velocity-style interface for Org."
  :tag "Org-Velocity"
  :group 'outlines
  :group 'hypermedia)

(defcustom org-velocity-bucket ""
  "Where is the bucket file?"
  :group 'org-velocity
  :type 'file)

(defcustom org-velocity-always-use-bucket nil
  "Use bucket file even when called from an Org buffer?"
  :group 'org-velocity
  :type 'boolean)

(defcustom org-velocity-use-completion nil
  "Complete on heading names?"
  :group 'org-velocity
  :type 'boolean)

(defcustom org-velocity-edit-indirectly t
  "Edit entries in an indirect buffer or just visit the file?"
  :group 'org-velocity
  :type 'boolean)

(defcustom org-velocity-search-method 'phrase
  "Match on whole phrase, any word, or all words?"
  :group 'org-velocity
  :type '(choice
	  (const :tag "Match whole phrase" phrase)
	  (const :tag "Match any word" any)
	  (const :tag "Match all words" all)))

(defcustom org-velocity-create-method 'capture
  "Prefer `org-capture', `org-remember', or neither?"
  :group 'org-velocity
  :type '(choice
	  (const :tag "Prefer capture > remember > default." capture)
	  (const :tag "Prefer remember > default." remember)
	  (const :tag "Edit in buffer." buffer)))

(defcustom org-velocity-allow-regexps nil
  "Allow searches to use regular expressions?"
  :group 'org-velocity
  :type 'boolean)

(defcustom org-velocity-remember-templates
  '(("Velocity entry"
     ?v
     "* %:search\n\n%i%?"
     nil
     bottom))
  "Use these templates with `org-remember'.
Meanwhile `org-default-notes-file' is bound to `org-velocity-use-file'.
The keyword :search inserts the current search.
See the documentation for `org-remember-templates'."
  :group 'org-velocity
  :type (or (get 'org-remember-templates 'custom-type) 'list))

(defcustom org-velocity-capture-templates
  '(("v"
     "Velocity entry"
     entry
     (file "")
     "* %:search\n\n%i%?"))
  "Use these template with `org-capture'.
Meanwhile `org-default-notes-file' is bound to `org-velocity-use-file'.
The keyword :search inserts the current search.
See the documentation for `org-capture-templates'."
  :group 'org-velocity
  :type (or (get 'org-capture-templates 'custom-type) 'list))

(defstruct (org-velocity-heading
	    (:constructor org-velocity-make-heading)
	    (:type list))
  (marker (point-marker))
  (name (substring-no-properties
	 (org-get-heading))))

(defconst org-velocity-index
  (eval-when-compile
   (nconc (number-sequence 49 57) 	;numbers
	  (number-sequence 97 122)	;lowercase letters
	  (number-sequence 65 90)))	;uppercase letters
  "List of chars for indexing results.")

(defconst org-velocity-display-buffer-name "*Velocity headings*")

(defvar org-velocity-search nil
  "Variable to bind to current search.")

(defsubst org-velocity-buffer-file-name (&optional buffer)
  "Return the name of the file BUFFER saves to.
Same as function `buffer-file-name' unless BUFFER is an
indirect buffer."
  (buffer-file-name
   (or (buffer-base-buffer buffer)
       buffer)))

(defun org-velocity-use-file ()
  "Return the proper file for Org-Velocity to search.
If `org-velocity-always-use-bucket' is t, use bucket file; complain
if missing.  Otherwise if this is an Org file, use it."
  (or
   ;; In remember and capture buffers the target should be used.
   (and org-remember-mode org-default-notes-file)
   (let ((org-velocity-bucket
	  (and org-velocity-bucket (expand-file-name org-velocity-bucket))))
     (if org-velocity-always-use-bucket
	 (or org-velocity-bucket (error "Bucket required but not defined"))
       (if (and (eq major-mode 'org-mode)
		(org-velocity-buffer-file-name))
	   (org-velocity-buffer-file-name)
	 (or org-velocity-bucket
	     (error "No bucket and not an Org file")))))))

(defsubst org-velocity-display-buffer ()
  "Return the proper buffer for Org-Velocity to display in."
  (get-buffer-create org-velocity-display-buffer-name))

(defsubst org-velocity-bucket-buffer ()
  "Return proper buffer for bucket operations."
  (find-file-noselect (org-velocity-use-file)))

(defun org-velocity-quote (search)
  "Quote SEARCH as a regexp if `org-velocity-allow-regexps' is non-nil.
Acts like `regexp-quote' on a string, `regexp-opt' on a list."
  (if org-velocity-allow-regexps
      search
    (if (listp search)
	(regexp-opt search)
      (regexp-quote search))))

(defun org-velocity-nearest-heading (position)
  "Return last heading at POSITION.
If there is no last heading, return nil."
  (save-excursion
    (goto-char position)
    (unless (org-before-first-heading-p)
      (org-back-to-heading)
      (org-velocity-make-heading))))

(defun org-velocity-make-button-action (heading)
  "Return a form to visit HEADING."
  `(lambda (button)
    (run-hooks 'mouse-leave-buffer-hook) ;turn off temporary modes
    (if org-velocity-edit-indirectly
	(org-velocity-edit-entry ',heading)
      (progn
	(message "%s" ,(org-velocity-heading-name heading))
	(switch-to-buffer (marker-buffer
			   ,(org-velocity-heading-marker heading)))
	(goto-char (marker-position
		    ,(org-velocity-heading-marker heading)))))))

(defun org-velocity-edit-entry (heading)
  "Edit entry at HEADING in an indirect buffer."
  (let ((buffer (make-indirect-buffer
		 (marker-buffer (org-velocity-heading-marker heading))
		 (generate-new-buffer-name
		  (org-velocity-heading-name heading)))))
    (with-current-buffer buffer
      (let ((org-inhibit-startup t))
	(org-mode))
      (goto-char (marker-position (org-velocity-heading-marker heading)))
      (narrow-to-region (point)
			(save-excursion
			  (org-end-of-subtree)
			  (point)))
      (goto-char (point-min))
      (add-hook 'org-ctrl-c-ctrl-c-hook 'org-velocity-dismiss nil t))
    (pop-to-buffer buffer)
    (set (make-local-variable 'header-line-format)
	 (format "%s Use C-c C-c to finish."
		 (abbreviate-file-name
		  (buffer-file-name
		   (marker-buffer
		    (org-velocity-heading-marker heading))))))))

(defun org-velocity-dismiss ()
  "Save current entry and close indirect buffer."
  (progn
    (save-buffer)
    (kill-buffer)))

(defun org-velocity-buttonize (heading)
  "Insert HEADING as a text button."
  (insert (format "#%c " (nth (1- (line-number-at-pos))
			      org-velocity-index)))
  (let ((action (org-velocity-make-button-action heading)))
   (insert-text-button
    (org-velocity-heading-name heading)
    'action action))
  (newline))

(defun org-velocity-remember ()
  "Use `org-remember' to record a note."
  (let ((org-remember-templates
	 org-velocity-remember-templates))
    (call-interactively 'org-remember)))

(defun org-velocity-capture ()
  "Use `org-capture' to record a note."
  (let ((org-capture-templates
	 org-velocity-capture-templates))
    (if (fboundp 'org-capture) ;; quiet compiler
	(call-interactively 'org-capture))))

(defun org-velocity-insert-heading (heading)
  "Add a new heading named HEADING."
  (with-current-buffer (org-velocity-bucket-buffer)
    (goto-char (point-max))
    (newline)
    (org-insert-heading) (insert heading)
    (newline)
    (goto-char (point-max))))

(defun org-velocity-create-heading ()
  "Add and visit a new heading."
  (org-store-link nil)
  (destructuring-bind (&key search initial
			    &allow-other-keys)
      org-store-link-plist
    (org-velocity-insert-heading search)
    (switch-to-buffer (org-velocity-bucket-buffer))
    (insert (or initial ""))))

(defun org-velocity-all-search (search)
  "Return entries containing all words in SEARCH."
  (when (file-exists-p (org-velocity-use-file))
   (save-excursion
     (delq nil
	   (let ((keywords
		  (mapcar 'org-velocity-quote
			  (split-string search)))
		 (case-fold-search t))
	     (org-map-entries
	      (lambda ()
		(if (loop with limit = (save-excursion
					 (org-end-of-subtree)
					 (point))
			  for word in keywords
			  always (save-excursion
				   (re-search-forward word limit t)))
		    (org-velocity-nearest-heading
		     (match-beginning 0))))))))))

(defun org-velocity-generic-search (search)
  "Return entries containing SEARCH."
  (save-excursion
    (delq nil
	  (nreverse
	   (let (matches (case-fold-search t))
	     (goto-char (point-min))
	     (while (re-search-forward search
				       (point-max) t)
	       (push (org-velocity-nearest-heading (match-beginning 0))
		     matches)
	       (outline-next-heading))
	     matches)))))

(defsubst org-velocity-phrase-search (search)
  "Return entries containing SEARCH as a phrase."
  (org-velocity-generic-search (org-velocity-quote search)))

(defsubst org-velocity-any-search (search)
  "Return entries containing any word in SEARCH."
  (org-velocity-generic-search (org-velocity-quote (split-string search))))

(defun org-velocity-present (headings)
  "Buttonize HEADINGS in `org-velocity-display-buffer'."
  (and (listp headings) (delete-dups headings))
  (let ((cdr (nthcdr
	      (1- (length org-velocity-index))
	      headings)))
    (and (consp cdr) (setcdr cdr nil)))
  (with-current-buffer (org-velocity-display-buffer)
    (mapc
     'org-velocity-buttonize
     headings)
    (goto-char (point-min))))

(defun org-velocity-create-1 ()
  "Create a new heading.
The possible methods are `org-velocity-capture',
`org-velocity-remember', or `org-velocity-create-heading', in
that order.  Which is preferred is determined by
`org-velocity-create-method'."
  (funcall
   (ecase org-velocity-create-method
     (capture (or (and (featurep 'org-capture) 'org-velocity-capture)
		  (and (featurep 'org-remember) 'org-velocity-remember)
		  'org-velocity-create-heading))
     (remember (or (and (featurep 'org-remember) 'org-velocity-remember)
		   'org-velocity-create-heading))
     (buffer 'org-velocity-create-heading))))

(defun org-velocity-store-link ()
  "Function for `org-store-link-functions'."
  (if org-velocity-search
   (org-store-link-props
    :search org-velocity-search)))

(add-hook 'org-store-link-functions 'org-velocity-store-link)

(defun org-velocity-create (search &optional ask)
  "Create new heading named SEARCH.
If ASK is non-nil, ask first."
  (when (or (null ask)
	    (y-or-n-p "No match found, create? "))
    (let ((org-velocity-search search)
	  (org-default-notes-file (org-velocity-use-file))
	  ;; save a stored link
	  (org-store-link-plist))
      (org-velocity-create-1))
    search))

(defun org-velocity-get-matches (search)
  "Return matches for SEARCH in current bucket.
Use method specified by `org-velocity-search-method'."
  (with-current-buffer (org-velocity-bucket-buffer)
   (case org-velocity-search-method
     ('phrase (org-velocity-phrase-search search))
     ('any    (org-velocity-any-search search))
     ('all    (org-velocity-all-search search)))))

(defun org-velocity-engine (search)
  "Display a list of headings where SEARCH occurs."
  (with-current-buffer (org-velocity-display-buffer)
    (erase-buffer)
    (setq cursor-type nil))
  (unless (or
	   (not (stringp search))
	   (string-equal "" search))	;exit on empty string
    (case
	(with-current-buffer (org-velocity-bucket-buffer)
	  (save-excursion
	    (let ((matches (org-velocity-get-matches search)))
	      (org-velocity-present matches)
	      (cond ((zerop (length matches)) 'new)
		    ((= (length matches) 1)   'follow)
		    ((> (length matches) 1)   'prompt)))))
      ('prompt (progn
		 (Electric-pop-up-window (org-velocity-display-buffer))
		 (let ((hint (org-velocity-electric-follow-hint)))
		   (if hint
		       (case hint
			 (edit (org-velocity-read nil search))
			 (new (org-velocity-create search))
			 (otherwise (org-velocity-activate-button hint)))))))
      ('new (unless (org-velocity-create search t)
	      (org-velocity-read nil search)))
      ('follow (if (y-or-n-p "One match, follow? ")
		   (progn
		     (set-buffer (org-velocity-display-buffer))
		     (goto-char (point-min))
		     (button-activate (next-button (point))))
		 (org-velocity-read nil search))))))

(defun org-velocity-position (item list)
  "Return first position of ITEM in LIST."
  (loop for elt in list
	for i from 0
	if (equal elt item)
	return i))

(defun org-velocity-activate-button (char)
  "Go to button on line number associated with CHAR in `org-velocity-index'."
  (goto-char (point-min))
  (forward-line (org-velocity-position char org-velocity-index))
  (goto-char
   (button-start
    (next-button (point))))
  (message "%s" (button-label (button-at (point))))
  (button-activate (button-at (point))))

(defun org-velocity-electric-undefined ()
  "Complain about an undefined key."
  (interactive)
  (message "%s"
	   (substitute-command-keys
	    "\\[org-velocity-electric-new] for new entry, \\[org-velocity-electric-edit] to edit search, \\[scroll-up] to scroll."))
  (sit-for 4))

(defun org-velocity-electric-follow (ev)
  "Follow a hint indexed by keyboard event EV."
  (interactive (list last-command-event))
  (if (not (> (org-velocity-position ev org-velocity-index)
	   (1- (count-lines (point-min) (point-max)))))
      (throw 'org-velocity-select ev)
    (call-interactively 'org-velocity-electric-undefined)))

(defun org-velocity-electric-click (ev)
  "Follow hint indexed by a mouse event EV."
  (interactive "e")
  (throw 'org-velocity-select
	 (nth (1- (count-lines
		   (point-min)
		   (posn-point (event-start ev))))
	      org-velocity-index)))

(defun org-velocity-electric-edit ()
  "Edit the search string."
  (interactive)
  (throw 'org-velocity-select 'edit))

(defun org-velocity-electric-new ()
  "Force a new entry."
  (interactive)
  (throw 'org-velocity-select 'new))

(defvar org-velocity-electric-map
  (let ((map (make-sparse-keymap)))
    (define-key map [t] 'org-velocity-electric-undefined)    (loop for c in org-velocity-index
	  do (define-key map (char-to-string c) 'org-velocity-electric-follow))
    (define-key map "0" 'org-velocity-electric-new)
    (define-key map [tab] 'scroll-up)
    (define-key map [return] 'org-velocity-electric-edit)
    (define-key map [mouse-1] 'org-velocity-electric-click)
    (define-key map [mouse-2] 'org-velocity-electric-click)
    (define-key map [escape escape escape] 'keyboard-quit)
    (define-key map "\C-h" 'help-command)
    map))

(defun org-velocity-electric-follow-hint ()
  "Read index of button electrically."
  (with-current-buffer (org-velocity-display-buffer)
    (use-local-map org-velocity-electric-map)
    (catch 'org-velocity-select
      (Electric-command-loop 'org-velocity-select
			     "Follow: "))))

(defun org-velocity-read-with-completion (prompt)
  "Like `completing-read' on entries with PROMPT.
Use `minibuffer-local-filename-completion-map'."
  (let ((minibuffer-local-completion-map
	 minibuffer-local-filename-completion-map))
    (completing-read
     prompt
     (mapcar 'substring-no-properties
	     (org-map-entries 'org-get-heading)))))

(defun org-velocity-read-string (prompt &optional initial-input)
  "Read string with PROMPT followed by INITIAL-INPUT."
  ;; The use of initial inputs to the minibuffer is deprecated (see
  ;; `read-from-minibuffer'), but in this case it is the user-friendly
  ;; thing to do.
  (minibuffer-with-setup-hook
      (lexical-let ((initial-input initial-input))
	(lambda ()
	  (and initial-input (insert initial-input))
	  (goto-char (point-max))))
    (if (and org-velocity-use-completion
	     ;; map-entries complains for nonexistent files
	     (file-exists-p (org-velocity-use-file)))
	(org-velocity-read-with-completion prompt)
      (read-string prompt))))

(defun org-velocity-read (arg &optional search)
  "Read a search string SEARCH for Org-Velocity interface.
This means that a buffer will display all headings where SEARCH
occurs, where one can be selected by a mouse click or by typing
its index.  If SEARCH does not occur, then a new heading may be
created named SEARCH.

If `org-velocity-bucket' is defined and
`org-velocity-always-use-bucket' is non-nil, then the bucket file
will be used; otherwise, this will work when called in any Org
file.  Calling with ARG forces current file."
  (interactive "P")
  (let ((org-velocity-always-use-bucket
	 (if arg nil org-velocity-always-use-bucket)))
    ;; complain if inappropriate
    (assert (org-velocity-use-file))
    (unwind-protect
	(org-velocity-engine
	 (org-velocity-read-string "Velocity search: " search))
      (progn
	(kill-buffer (org-velocity-display-buffer))
	(delete-other-windows)))))

(provide 'org-velocity)
;;; org-velocity.el ends here
