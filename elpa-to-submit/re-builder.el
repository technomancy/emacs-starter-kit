;;; re-builder.el --- building Regexps with visual feedback

;; Copyright (C) 1999, 2000, 2001, 2002, 2004 Free Software Foundation, Inc.

;; Author: Detlev Zundel <dzu@gnu.org>
;; Keywords: matching, lisp, tools

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; When I have to come up with regular expressions that are more
;; complex than simple string matchers, especially if they contain sub
;; expressions, I find myself spending quite some time in the
;; `development cycle'.  `re-builder' aims to shorten this time span
;; so I can get on with the more interesting bits.

;; With it you can have immediate visual feedback about how well the
;; regexp behaves to your expectations on the intended data.

;; When called up `re-builder' attaches itself to the current buffer
;; which becomes its target buffer, where all the matching is done.
;; The active window is split so you have a view on the data while
;; authoring the RE.  If the edited expression is valid the matches in
;; the target buffer are marked automatically with colored overlays
;; (for non-color displays see below) giving you feedback over the
;; extents of the matched (sub) expressions.  The (non-)validity is
;; shown only in the modeline without throwing the errors at you.  If
;; you want to know the reason why RE Builder considers it as invalid
;; call `reb-force-update' ("\C-c\C-u") which should reveal the error.

;; The target buffer can be changed with `reb-change-target-buffer'
;; ("\C-c\C-b").  Changing the target buffer automatically removes
;; the overlays from the old buffer and displays the new one in the
;; target window.

;; The `re-builder' keeps the focus while updating the matches in the
;; target buffer so corrections are easy to incorporate.  If you are
;; satisfied with the result you can paste the RE to the kill-ring
;; with `reb-copy' ("\C-c\C-w"), quit the `re-builder' ("\C-c\C-q")
;; and use it wherever you need it.

;; As the automatic updates can take some time on large buffers, they
;; can be limited by `reb-auto-match-limit' so that they should not
;; have a negative impact on the editing.  Setting it to nil disables
;; the auto update limit.  Setting it to 0 disables auto updates.
;; Forcing an update overrides this limit allowing an easy way to see
;; all matches.

;; Currently `re-builder' understands six different forms of input,
;; namely `lisp-re', `perl', `read', `rx', `sregex', and `string'
;; syntax.
;; Read syntax and string syntax are both delimited by `"'s and behave
;; according to their name.  With the `string' syntax there's no need
;; to escape the backslashes and double quotes simplifying the editing
;; somewhat.  The other three allow editing of symbolic regular
;; expressions supported by the packages of the same name.  (`lisp-re'
;; is a package by me and its support may go away as it is nearly the
;; same as the `sregex' package in Emacs)

;; Note that the `sregex,' `rx' and `lisp-re' syntaxes will only be
;; available in XEmacs if you've installed them yourself.

;; Editing symbolic expressions is done through a major mode derived
;; from `emacs-lisp-mode' so you'll get all the good stuff like
;; automatic indentation and font-locking etc.

;; When editing a symbolic regular expression, only the first
;; expression in the RE Builder buffer is considered, which helps
;; limiting the extent of the expression like the `"'s do for the text
;; modes.  For the `sregex' syntax the function `sregex' is applied to
;; the evaluated expression read.  So you can use quoted arguments
;; with something like '("findme") or you can construct arguments to
;; your hearts delight with a valid ELisp expression.  (The compiled
;; string form will be copied by `reb-copy')  If you want to take
;; a glance at the corresponding string you can temporarily change the
;; input syntax.

;; Changing the input syntax is transparent (for the obvious exception
;; non-symbolic -> symbolic) so you can change your mind as often as
;; you like.

;; There is also a shortcut function for toggling the
;; `case-fold-search' variable in the target buffer with an immediate
;; update.


;; Q: But what if my display cannot show colored overlays?
;; A: Then the cursor will flash around the matched text making it stand
;;    out.

;; Q: But how can I then make out the sub-expressions?
;; A: That's where the `sub-expression mode' comes in.  In it only the
;;    digit keys are assigned to perform an update that will flash the
;;    corresponding subexp only.


;;; Code:

;; Extent/overlay compatibility
(if (featurep 'xemacs)
    ;; (S)XEmacs
    (progn
      (defalias 'reb-delete-overlay 'delete-extent)
      (defalias 'reb-make-overlay 'make-extent)
      (defalias 'reb-overlay-put 'set-extent-property))
  ;; GNU/Emacs
  (defalias 'reb-delete-overlay 'delete-overlay)
  (defalias 'reb-make-overlay 'make-overlay)
  (defalias 'reb-overlay-put 'overlay-put))

;; User customizable variables
(defgroup re-builder nil
  "Options for the RE Builder."
  :group 'lisp
  :prefix "reb-")

(defcustom reb-blink-delay 0.5
  "*Seconds to blink cursor for next/previous match in RE Builder."
  :group 're-builder
  :type 'number)

(defcustom reb-mode-hook nil
  "*Hooks to run on entering RE Builder mode."
  :group 're-builder
  :type 'hook)

(defcustom reb-re-syntax 'read
  "*Syntax for the REs in the RE Builder.

Can either be `lisp-re', `perl', `read', `rx', `sregex', or `string'."
  :group 're-builder
  :type '(choice (const :tag "Read syntax" read)
		 (const :tag "String syntax" string)
		 (const :tag "`sregex' syntax" sregex)
		 (const :tag "`lisp-re' syntax" lisp-re)
		 (const :tag "`rx' syntax" rx)
		 (const :tag "`perl' syntax" perl)
		 (value: string)))

(defcustom reb-auto-match-limit 200
  "*Positive integer limiting the matches for RE Builder auto updates.
Set it to nil if you don't want limits here."
  :group 're-builder
  :type '(restricted-sexp :match-alternatives
			  (integerp 'nil)))

; Produced by
; (Hsv2ColoredTextSpectrum 0 359 36 0.2155 0.3000 1.0000 1.0000 1.0000 1.0000)
; from http://www.emacswiki.org/cgi-bin/emacs/hsv2rgb.el
; Hue Satur Value RGB Sample
;   0 0.216 1.000 #ffc8c8
;  36 0.216 1.000 #ffe9c8
;  72 0.216 1.000 #f4ffc8
; 108 0.216 1.000 #d3ffc8
; 144 0.216 1.000 #c8ffde
; 180 0.216 1.000 #c8ffff
; 216 0.216 1.000 #c8deff
; 252 0.216 1.000 #d3c8ff
; 288 0.216 1.000 #f4c8ff
; 324 0.216 1.000 #ffc8e9
(defface reb-match-0
  '((((class color) (background light))
     (:background "#ffc8c8"))
    (((class color) (background dark))
     (:background "steelblue4"))
    (t
     :inverse-video t))
  "Used for displaying the whole match."
  :group 're-builder)

(defface reb-match-1
  '((((class color) (background light))
     (:background "#ffe9c8"))
    (((class color) (background dark))
     (:background "blue3"))
    (t
     :inverse-video t))
  "Used for displaying the first matching subexpression."
  :group 're-builder)

(defface reb-match-2
  '((((class color) (background light))
     (:background "#f4ffc8"))
    (((class color) (background dark))
     (:background "chartreuse4"))
    (t
     :inverse-video t))
  "Used for displaying the second matching subexpression."
  :group 're-builder)

(defface reb-match-3
  '((((class color) (background light))
     (:background "#d3ffc8"))
    (((class color) (background dark))
     (:background "sienna4"))
    (t
     :inverse-video t))
  "Used for displaying the third matching subexpression."
  :group 're-builder)

(defface reb-match-4
  '((((class color) (background light))
     (:background "#c8ffde"))
    (((class color) (background dark))
     (:background "sienna4"))
    (t
     :inverse-video t))
  "Used for displaying the third matching subexpression."
  :group 're-builder)

(defface reb-match-5
  '((((class color) (background light))
     (:background "#c8ffff"))
    (((class color) (background dark))
     (:background "sienna4"))
    (t
     :inverse-video t))
  "Used for displaying the third matching subexpression."
  :group 're-builder)

(defface reb-match-6
  '((((class color) (background light))
     (:background "#c8deff"))
    (((class color) (background dark))
     (:background "sienna4"))
    (t
     :inverse-video t))
  "Used for displaying the third matching subexpression."
  :group 're-builder)

(defface reb-match-7
  '((((class color) (background light))
     (:background "#d3c8ff"))
    (((class color) (background dark))
     (:background "sienna4"))
    (t
     :inverse-video t))
  "Used for displaying the third matching subexpression."
  :group 're-builder)

(defface reb-match-8
  '((((class color) (background light))
     (:background "#f4c8ff"))
    (((class color) (background dark))
     (:background "sienna4"))
    (t
     :inverse-video t))
  "Used for displaying the third matching subexpression."
  :group 're-builder)

(defface reb-match-9
  '((((class color) (background light))
     (:background "#ffc8e9"))
    (((class color) (background dark))
     (:background "sienna4"))
    (t
     :inverse-video t))
  "Used for displaying the third matching subexpression."
  :group 're-builder)

;; Internal variables below
(defvar reb-mode nil
  "Enables the RE Builder minor mode.")

(defvar reb-target-buffer nil
  "Buffer to which the RE is applied to.")

(defvar reb-target-window nil
  "Window to which the RE is applied to.")

(defvar reb-regexp nil
  "Last regexp used by RE Builder.")

(defvar reb-regexp-src nil
  "Last regexp used by RE Builder before processing it.
Except for Lisp syntax this is the same as `reb-regexp'.")

(defvar reb-overlays nil
  "List of overlays of the RE Builder.")

(defvar reb-window-config nil
  "Old window configuration.")

(defvar reb-subexp-mode nil
  "Indicates whether sub-exp mode is active.")

(defvar reb-subexp-displayed nil
  "Indicates which sub-exp is active.")

(defvar reb-mode-string ""
  "String in mode line for additional info.")

(defvar reb-valid-string ""
  "String in mode line showing validity of RE.")

(defvar reb-perl-match-vector nil
  "Last match data for perl syntax used by RE Builder.")

(defvar reb-perl-match-index nil
  "Last match data index for perl syntax used by RE Builder.")

(make-variable-buffer-local 'reb-overlays)
(make-variable-buffer-local 'reb-regexp)
(make-variable-buffer-local 'reb-regexp-src)
(make-variable-buffer-local 'reb-perl-match-vector)
(make-variable-buffer-local 'reb-perl-match-index)

(defconst reb-buffer "*RE-Builder*"
  "Buffer to use for the RE Builder.")

;; Define the local "\C-c" keymap
(defvar reb-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-c\C-c" 'reb-toggle-case)
    (define-key map "\C-c\C-q" 'reb-quit)
    (define-key map "\C-c\C-w" 'reb-copy)
    (define-key map "\C-c\C-s" 'reb-next-match)
    (define-key map "\C-c\C-r" 'reb-prev-match)
    (define-key map "\C-c\C-i" 'reb-change-syntax)
    (define-key map "\C-c\C-e" 'reb-enter-subexp-mode)
    (define-key map "\C-c\C-b" 'reb-change-target-buffer)
    (define-key map "\C-c\C-u" 'reb-force-update)
    map)
  "Keymap used by the RE Builder.")

(defun reb-mode ()
  "Major mode for interactively building Regular Expressions.
\\{reb-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'reb-mode
        mode-name "RE Builder")
  (use-local-map reb-mode-map)
  (reb-mode-common)
  (run-hooks 'reb-mode-hook))

(define-derived-mode reb-lisp-mode
  emacs-lisp-mode "RE Builder Lisp"
  "Major mode for interactively building symbolic Regular Expressions."
  (cond ((eq reb-re-syntax 'lisp-re)	; Pull in packages
	 (require 'lisp-re))		; as needed
	((eq reb-re-syntax 'sregex)	; sregex is not autoloaded
	 (require 'sregex))		; right now..
	((eq reb-re-syntax 'rx)		; rx-to-string is autoloaded
	 (require 'rx)))		; require rx anyway
  (reb-mode-common))

;; Use the same "\C-c" keymap as `reb-mode' and use font-locking from
;; `emacs-lisp-mode'
(define-key reb-lisp-mode-map "\C-c"
  (lookup-key reb-mode-map "\C-c"))

(defvar reb-subexp-mode-map
  (let ((m (make-keymap)))
    (suppress-keymap m)
    ;; Again share the "\C-c" keymap for the commands
    (define-key m "\C-c" (lookup-key reb-mode-map "\C-c"))
    (define-key m "q" 'reb-quit-subexp-mode)
    (dotimes (digit 10)
      (define-key m (int-to-string digit) 'reb-display-subexp))
    m)
  "Keymap used by the RE Builder for the subexpression mode.")

(defun reb-mode-common ()
  "Setup functions common to functions `reb-mode' and `reb-mode-lisp'."

  (setq	reb-mode-string  ""
	reb-valid-string ""
	mode-line-buffer-identification
	                 '(25 . ("%b" reb-mode-string reb-valid-string)))
  (reb-update-modestring)
  (make-local-variable 'after-change-functions)
  (add-hook 'after-change-functions
	    'reb-auto-update)
  ;; At least make the overlays go away if the buffer is killed
  (make-local-variable 'reb-kill-buffer)
  (add-hook 'kill-buffer-hook 'reb-kill-buffer)
  (reb-auto-update nil nil nil))


;; Handy macro for doing things in other windows
(defmacro reb-with-current-window (window &rest body)
  "With WINDOW selected evaluate BODY forms and reselect previous window."

  (let ((oldwindow (make-symbol "*oldwindow*")))
    `(let ((,oldwindow (selected-window)))
       (select-window ,window)
       (unwind-protect
	   (progn
	     ,@body)
	 (select-window ,oldwindow)))))
(put 'reb-with-current-window 'lisp-indent-function 0)

(defun reb-color-display-p ()
  "Return t if display is capable of displaying colors."
  (eq 'color
      ;; emacs/xemacs compatibility
      (if (fboundp 'frame-parameter)
	  (frame-parameter (selected-frame) 'display-type)
	(device-class (frame-device (selected-frame))))))

(defsubst reb-lisp-syntax-p ()
  "Return non-nil if RE Builder uses a Lisp syntax."
  (memq reb-re-syntax '(lisp-re sregex rx)))

(defsubst reb-perl-syntax-p ()
  "Return non-nil if RE Builder uses a Perl syntax."
  (equal reb-re-syntax 'perl))

(defmacro reb-target-binding (symbol)
  "Return binding for SYMBOL in the RE Builder target buffer."
  `(with-current-buffer reb-target-buffer ,symbol))

;;; This is to help people find this in Apropos.
;;;###autoload
(defun regexp-builder ()
  "Alias for `re-builder': Construct a regexp interactively."
  (interactive)
  (re-builder))

;;;###autoload
(defun re-builder ()
  "Construct a regexp interactively."
  (interactive)

  (if (and (string= (buffer-name) reb-buffer)
           (memq major-mode '(reb-mode reb-lisp-mode)))
      (message "Already in the RE Builder")
    (if reb-target-buffer
        (reb-delete-overlays))
    (setq reb-target-buffer (current-buffer)
          reb-target-window (selected-window)
          reb-window-config (current-window-configuration))
    (select-window (split-window (selected-window) (- (window-height) 4)))
    (switch-to-buffer (get-buffer-create reb-buffer))
    (erase-buffer)
    (reb-insert-regexp)
    (goto-char (+ 2 (point-min)))
    (cond
     ((reb-lisp-syntax-p)
      (reb-lisp-mode))
     (t (reb-mode)))))

(defun reb-change-target-buffer (buf)
  "Change the target buffer and display it in the target window."
  (interactive "bSet target buffer to: ")

  (let ((buffer (get-buffer buf)))
    (if (not buffer)
        (error "No such buffer")
      (reb-delete-overlays)
      (setq reb-target-buffer buffer)
      (reb-do-update
       (if reb-subexp-mode reb-subexp-displayed nil))
      (reb-update-modestring))))

(defun reb-force-update ()
  "Force an update in the RE Builder target window without a match limit."
  (interactive)

  (let ((reb-auto-match-limit nil))
    (reb-update-overlays
     (if reb-subexp-mode reb-subexp-displayed nil))))

(defun reb-quit ()
  "Quit the RE Builder mode."
  (interactive)

  (setq reb-subexp-mode nil
	reb-subexp-displayed nil)
  (reb-delete-overlays)
  (bury-buffer)
  (set-window-configuration reb-window-config))

(defun reb-next-match ()
  "Go to next match in the RE Builder target window."
  (interactive)

  (reb-assert-buffer-in-window)
  (reb-with-current-window
    reb-target-window
    (if (not (reb-re-search-forward reb-regexp (point-max) t))
	(message "No more matches.")
      (reb-show-subexp
       (or (and reb-subexp-mode reb-subexp-displayed) 0)
       t))))

(defun reb-prev-match ()
  "Go to previous match in the RE Builder target window."
  (interactive)

  (reb-assert-buffer-in-window)
  (reb-with-current-window reb-target-window
    (goto-char (1- (point)))
    (if (not (reb-re-search-backward reb-regexp (point-min) t))
	(message "No more matches.")
      (reb-show-subexp
       (or (and reb-subexp-mode reb-subexp-displayed) 0)
       t))))

(defun reb-toggle-case ()
  "Toggle case sensitivity of searches for RE Builder target buffer."
  (interactive)

  (with-current-buffer reb-target-buffer
    (setq case-fold-search (not case-fold-search)))
  (reb-update-modestring)
  (reb-auto-update nil nil nil t))

(defun reb-copy ()
  "Copy current RE into the kill ring for later insertion."
  (interactive)

  (reb-update-regexp)
  (let* ((kill (reb-target-binding reb-regexp)))
    (unless (eq reb-re-syntax 'string)
      (setq kill (with-output-to-string (prin1 kill))))
    (kill-new kill)
    (message "%s regexp %s copied to kill-ring"
	     (symbol-name reb-re-syntax) kill)))

;; The subexpression mode is not electric because the number of
;; matches should be seen rather than a prompt.
(defun reb-enter-subexp-mode ()
  "Enter the subexpression mode in the RE Builder."
  (interactive)
  (setq reb-subexp-mode t)
  (reb-update-modestring)
  (use-local-map reb-subexp-mode-map)
  (message "`0'-`9' to display subexpressions, `q' to quit subexp mode."))

(defun reb-show-subexp (subexp &optional pause)
  "Visually show limit of subexpression SUBEXP of recent search.
On color displays this just puts point to the end of the expression as
the match should already be marked by an overlay.
On other displays jump to the beginning and the end of it.
If the optional PAUSE is non-nil then pause at the end in any case."
  (reb-with-current-window reb-target-window
    (if (not (reb-color-display-p))
	(progn (goto-char (match-beginning subexp))
	       (sit-for reb-blink-delay)))
    (goto-char (match-end subexp))
    (if (or (not (reb-color-display-p)) pause)
	(sit-for reb-blink-delay))))

(defun reb-quit-subexp-mode ()
  "Quit the subexpression mode in the RE Builder."
  (interactive)
  (setq reb-subexp-mode nil
	reb-subexp-displayed nil)
  (reb-update-modestring)
  (use-local-map reb-mode-map)
  (reb-do-update))

(defun reb-change-syntax (&optional syntax)
  "Change the syntax used by the RE Builder.
Optional argument SYNTAX must be specified if called non-interactively."
  (interactive
   (list (intern
	  (completing-read "Select syntax: "
			   (mapcar (lambda (el) (cons (symbol-name el) 1))
				   ;; APA TODO: Gotta be a way to
				   ;; avoid duplication of choices
				   ;; from `reb-re-syntax' here.
				   '(read string lisp-re sregex rx perl))
			   nil t (symbol-name reb-re-syntax)))))

  (if (memq syntax '(read string lisp-re sregex rx perl))
      (let ((buffer (get-buffer reb-buffer)))
	(setq reb-re-syntax syntax)
	(if buffer
	    (with-current-buffer buffer
	      (erase-buffer)
	      (reb-insert-regexp)
	      (goto-char (+ 2 (point-min)))
	      (cond ((reb-lisp-syntax-p)
		     (reb-lisp-mode))
		    (t (reb-mode))))))
    (error "Invalid syntax: %s" syntax)))


;; Non-interactive functions below
(defun reb-do-update (&optional subexp)
  "Update matches in the RE Builder target window.
If SUBEXP is non-nil mark only the corresponding sub-expressions."

  (reb-assert-buffer-in-window)
  (reb-update-regexp)
  (reb-update-overlays subexp))

(defun reb-auto-update (beg end lenold &optional force)
  "Called from `after-update-functions' to update the display.
BEG, END and LENOLD are passed in from the hook.
An actual update is only done if the regexp has changed or if the
optional fourth argument FORCE is non-nil."
  (let ((prev-valid reb-valid-string)
	(new-valid
	 (condition-case nil
	     (progn
	       (if (or (reb-update-regexp) force)
		   (progn
		     (reb-assert-buffer-in-window)
		     (reb-do-update)))
	       "")
	   (error " *invalid*"))))
    (setq reb-valid-string new-valid)
    (force-mode-line-update)

    ;; Through the caching of the re a change invalidating the syntax
    ;; for symbolic expressions will not delete the overlays so we
    ;; catch it here
    (if (and (reb-lisp-syntax-p)
	     (not (string= prev-valid new-valid))
	     (string= prev-valid ""))
	(reb-delete-overlays))))

(defun reb-delete-overlays ()
  "Delete all RE Builder overlays in the `reb-target-buffer' buffer."
  (if (buffer-live-p reb-target-buffer)
      (with-current-buffer reb-target-buffer
	(mapcar 'reb-delete-overlay reb-overlays)
	(setq reb-overlays nil
	      reb-perl-match-vector nil
	      reb-perl-match-index nil))))

(defun reb-assert-buffer-in-window ()
  "Assert that `reb-target-buffer' is displayed in `reb-target-window'."

  (if (not (eq reb-target-buffer (window-buffer reb-target-window)))
      (set-window-buffer reb-target-window reb-target-buffer)))

(defun reb-update-modestring ()
  "Update the variable `reb-mode-string' displayed in the mode line."
  (setq reb-mode-string
	(concat
	 (if reb-subexp-mode
             (format " (subexp %s)" (or reb-subexp-displayed "-"))
	   "")
	 (if (not (reb-target-binding case-fold-search))
	     " Case"
	   "")
	 " "
	 (symbol-name reb-re-syntax)))
  (force-mode-line-update))

(defun reb-display-subexp (&optional subexp)
  "Highlight only subexpression SUBEXP in the RE Builder."
  (interactive)

  (setq reb-subexp-displayed
	(or subexp (string-to-int (format "%c" last-command-char))))
  (reb-update-modestring)
  (reb-do-update reb-subexp-displayed))

(defun reb-kill-buffer ()
  "When the RE Builder buffer is killed make sure no overlays stay around."

  (if (member major-mode '(reb-mode reb-lisp-mode))
      (reb-delete-overlays)))


;; The next functions are the interface between the regexp and
;; its textual representation in the RE Builder buffer.
;; They are the only functions concerned with the actual syntax
;; being used.
(defun reb-read-regexp ()
  "Read current RE."
  (save-excursion
    (cond ((eq reb-re-syntax 'read)
	   (goto-char (point-min))
	   (read (current-buffer)))
	  ((member reb-re-syntax '(perl string))
	   (goto-char (point-min))
	   (re-search-forward "\"")
	   (let ((beg (point)))
	     (goto-char (point-max))
	     (re-search-backward "\"")
	     (buffer-substring-no-properties beg (point))))
	  ((reb-lisp-syntax-p)
	   (buffer-string)))))

(defun reb-empty-regexp ()
  "Return empty RE for current syntax."
  (cond ((reb-lisp-syntax-p) "'()")
	(t "")))

(defun reb-insert-regexp ()
  "Insert current RE."

  (let ((re (or (reb-target-binding reb-regexp)
		(reb-empty-regexp))))
    (cond ((eq reb-re-syntax 'read)
	   (print re (current-buffer)))
	  ((member reb-re-syntax '(perl string))
	   (insert "\n\"" re "\""))
	  ;; For the Lisp syntax we need the "source" of the regexp
	  ((reb-lisp-syntax-p)
	   (insert (or (reb-target-binding reb-regexp-src)
		       (reb-empty-regexp)))))))

(defun reb-cook-regexp (re)
  "Return RE after processing it according to `reb-re-syntax'."
  (cond ((eq reb-re-syntax 'lisp-re)
	 (lre-compile-string (eval (car (read-from-string re)))))
	((eq reb-re-syntax 'sregex)
	 (apply 'sregex (eval (car (read-from-string re)))))
	((eq reb-re-syntax 'rx)
	 (rx-to-string (eval (car (read-from-string re)))))
	(t re)))

(defun reb-update-regexp ()
  "Update the regexp for the target buffer.
Return t if the (cooked) expression changed."
  (let* ((re-src (reb-read-regexp))
	 (re (reb-cook-regexp re-src)))
    (with-current-buffer reb-target-buffer
      (let ((oldre reb-regexp))
	(prog1
	    (not (string= oldre re))
	  (setq reb-regexp re)
	  ;; Only update the source re for the lisp formats
	  (if (reb-lisp-syntax-p)
	      (setq reb-regexp-src re-src)))))))


;; And now the real core of the whole thing
(defun reb-count-subexps (re)
  "Return number of sub-expressions in the regexp RE."

  (let ((i 0) (beg 0))
    ;; Remove character classes first, to avoid false subexp matches,
    ;; then count beginnings of subexp grouping operators.
    ;; Even before that remove escaped character class operators to
    ;; simplify character class removal.
    (let ((subexp-start-regexp (if (reb-perl-syntax-p) "(" "\\\\("))
	  ;; escaped-char-class-regexp an char-class-regexp do not
	  ;; differ between perl and lisp regexps.
	  (escaped-char-class-regexp "\\\\[][]")
	  (char-class-regexp "\\[^?.[^]]*\\]"))
      (setq re (replace-regexp-in-string escaped-char-class-regexp "" re))
      (setq re (replace-regexp-in-string char-class-regexp "" re))
      (while (string-match subexp-start-regexp re beg)
	(setq i (1+ i)
	      beg (match-end 0)))
      i)))


(defun reb-update-overlays (&optional subexp)
  "Switch to `reb-target-buffer' and mark all matches of `reb-regexp'.
If SUBEXP is non-nil mark only the corresponding sub-expressions."

  (let* ((re (reb-target-binding reb-regexp))
	 (subexps (reb-count-subexps re))
	 (matches 0)
	 (submatches 0)
	 firstmatch)
    (save-excursion
      (set-buffer reb-target-buffer)
      (reb-delete-overlays)
      (goto-char (point-min))
      (while (and (reb-re-search-forward re (point-max) t)
		  (or (not reb-auto-match-limit)
		      (< matches reb-auto-match-limit)))
	(if (= 0 (length (match-string 0)))
	    (error "Empty regular expression!"))
	(let ((i 0))
	  (setq matches (1+ matches))
	  (while (<= i subexps)
	    (if (and (or (not subexp) (= subexp i))
		     (match-beginning i))
		(let ((overlay (reb-make-overlay (match-beginning i)
						 (match-end i)))
		      (face-name (format "reb-match-%d" i)))
		  (if (not firstmatch)
		      ;; Use INTEGERS argument for reb-perl-syntax-p.
		      (setq firstmatch (match-data (reb-perl-syntax-p))))
		  (setq reb-overlays (cons overlay reb-overlays)
			submatches (1+ submatches))
		  (reb-overlay-put
		   overlay 'face
		   (or (intern-soft face-name)
		       (error "Too many subexpressions - face `%s' not defined"
			      face-name )))
		  (reb-overlay-put overlay 'priority i)))
	    (setq i (1+ i))))))
    (let ((count (if subexp submatches matches)))
      (message "%s %smatch%s%s"
	       (if (= 0 count) "No" (int-to-string count))
	       (if subexp "subexpression " "")
	       (if (= 1 count) "" "es")
	       (if (and reb-auto-match-limit
			(= reb-auto-match-limit count))
		   " (limit reached)" "")))
    (if firstmatch
	(progn (store-match-data firstmatch)
	       (reb-show-subexp (or subexp 0))))))

(defconst reb-perl-program
  "
use strict;

# Adrian Aichner <adrian@xemacs.org>, The XEmacs Project, 2005-08-23.
# Perl script returning match data to be used by re-builder.el

my $re = qr{%s}%s;
my @matches;

# Read in whole data to count positions relative to begin of data
# stream.
# Shouldn't be a problem for typical use of re-builder.el.
# re-builder.el should emit warning on huge target buffers.
undef $/;

sub main {
    while (<DATA>) {
        my $re_comment = $re;
        $re_comment =~ s/\\n/\\n ;; /g;
        printf \"[\\n ;; perl (v%%vd) one-based match-data for qr{$re_comment}\", $^V;
        while (m{$re}g) {
            print \"\\n [\";
            for (my $i = 0; $i < scalar (@-); $i++) {
                printf(\"(%%d %%d)\", pos() - $+[0] + $-[$i] + 1, pos() - $+[0] + $+[$i] + 1);
            }
            print \"]\";
        }
        print \"]\\n\";
    }
}

main();

__DATA__
%s"
  "Perl program control string to pass to `format'.

The control-string contains three %s sequences to receive the regular
expression string, the regular expression flags to pass on the perl's
qr{RE}FLAGS; opeartor, and finally the buffer content to match
against.
")

(defun reb-re-search-forward (regexp &optional limit noerror count buffer)
  (cond
   ((reb-perl-syntax-p)
    (if (not (reb-target-binding reb-perl-match-vector))
	(let ((program-buffer (get-buffer-create " *reb-perl-program*"))
	      (match-buffer (get-buffer-create " *reb-perl-match-data*")))
	  (unwind-protect
	      (progn
		(with-current-buffer
		    program-buffer
		  (erase-buffer)
		  (insert
		   (format reb-perl-program
			   (reb-target-binding reb-regexp)
			   (concat
			    (if (reb-target-binding case-fold-search) "i" "")
			    ;; use extended perl regexp syntax?
			    "x")
			   (with-current-buffer reb-target-buffer (buffer-string))))
		  (let ((tmpfile "/tmp/.reb"))
		    (write-region 1 (point-max) tmpfile nil 'nodisp)
		    (call-process "perl"
				  tmpfile
				  ;;(list (get-buffer " *reb-perl-program*))
				  match-buffer)))
		(with-current-buffer reb-target-buffer
		  (setq reb-perl-match-vector
			(read (with-current-buffer match-buffer (buffer-string ))))))
	    (kill-buffer program-buffer)
	    (kill-buffer match-buffer))))
    (let* ((vector (reb-target-binding reb-perl-match-vector))
	   (index (reb-target-binding reb-perl-match-index))
	   (max-index (1- (length vector))))
      ;; Make sure to preserve previous match data on failing search!
      (if (not index)
	  (setq index 0)
	(incf index))
      (when (<= index max-index)
	(with-current-buffer reb-target-buffer
	  (setq reb-perl-match-index index))
	(store-match-data
	 (apply 'append
		(append
		 (elt
		  vector
		  (reb-target-binding index))
		 nil)))
	(with-current-buffer reb-target-buffer
	  (goto-char
	   (elt
	    (apply 'append
		   (append
		    (elt
		     vector
		     ;; match data for current match index:
		     (reb-target-binding index))
		    nil))
	    ;; index to end of subexp 0 (end of whole match):
	    1))))))
   (t (re-search-forward regexp limit noerror count ))))

(defun reb-re-search-backward (regexp &optional limit noerror count buffer)
  (cond
   ((reb-perl-syntax-p)
    (let* ((vector (reb-target-binding reb-perl-match-vector))
	   (index (reb-target-binding reb-perl-match-index))
	   (max-index (1- (length vector))))
      ;; Make sure to preserve previous match data on failing search!
      (if (> index 0)
	(decf index))
      (when (>= index 0)
	(with-current-buffer reb-target-buffer
	  (setq reb-perl-match-index index))
	(store-match-data
	 (apply 'append
		(append
		 (elt
		  vector
		  (reb-target-binding index))
		 nil)))
	(with-current-buffer reb-target-buffer
	  (goto-char
	   (elt
	    (apply 'append
		   (append
		    (elt
		     vector
		     ;; match data for current match index:
		     (reb-target-binding index))
		    nil))
	    ;; index to end of subexp 0 (end of whole match):
	    1))))))
   (t (re-search-backward regexp limit noerror count ))))

(provide 're-builder)

;;; arch-tag: 5c5515ac-4085-4524-a421-033f44f032e7
;;; re-builder.el ends here