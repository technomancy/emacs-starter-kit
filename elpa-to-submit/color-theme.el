;;; color-theme.el --- install color themes

;; Copyright (C) 1999, 2000  Jonadab the Unsightly One <jonadab@bright.net>
;; Copyright (C) 2000, 2001, 2002, 2003  Alex Schroeder <alex@gnu.org>

;; Version: 6.5.2
;; Keywords: faces
;; Author: Jonadab the Unsightly One <jonadab@bright.net>
;; Maintainer: Alex Schroeder <alex@gnu.org>
;; URL: http://www.emacswiki.org/cgi-bin/wiki.pl?ColorTheme

;; This file is not part of GNU Emacs.

;; This is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 2, or (at your option) any later
;; version.
;;
;; This is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
;; for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
;; MA 02111-1307, USA.

;;; Commentary:

;; Sharing your current color setup:
;;
;; Use `color-theme-submit'.  If you have already invested time in
;; customizing Emacs faces, please consider sharing your current setup.
;; Make sure that color-theme.el is in your `load-path'.  Type M-x
;; load-library RET color-theme RET to load all the functions.  Type M-x
;; color-theme-submit RET and mail the result to the maintainer of this
;; package (see above for mail addres).
;;
;; If you want to make sure that all your customization was exported,
;; type M-x list-faces-display RET to get a list of all faces currently
;; defined.  This is the list of faces that `color-theme-print' uses.

;; Installing a color theme:
;;
;; Make sure that color-theme.el is in your `load-path'.  Type M-x
;; load-library RET color-theme RET to load all the functions.
;;
;; The main function to call is color-theme-select.  Type M-x
;; color-theme-select RET.  That creates a Color Theme Selection
;; buffer.  Press RET or `i' on a color theme to install it for the
;; rest of your session.
;;
;; If you want to install the color theme as soon as Emacs is started
;; up, read the description of the theme you like and remember the
;; name of the color theme function.  Press `d' on a color theme in
;; the Color Theme Selection buffer to read the description.  Assuming
;; you like the Gnome2 theme, you'll find that the function to use is
;; called `color-theme-gnome2'.  Add the following to the end of your
;; .emacs (removing the leading `;;').
;;
;; (require 'color-theme)
;; (color-theme-gnome2)

;; Changing menu colors:
;;
;; In Emacs 21 on X, you can set the menu colors and font using the
;; menu face.  Example for your .emacs file:
;;
;;   (set-face-font 'menu "7x14")
;;   (set-face-foreground 'menu "white").
;;
;; If are using X, you can set the menu foreground and background using
;; a resource file, usually .Xdefaults or .Xresources.  Usually
;; .Xdefaults is used when you start your session using a display
;; manager such as xdm or gdm.  .Xresources is usually used when you
;; start X directly via a shell script such as startx.  If you set
;; Emacs*Background and Emacs*Foreground in such a resource file, the
;; foreground and background of Emacs including the menu will be set.
;; If your .emacs then loads a color theme, the foreground and
;; background are changed -- with the exception of the menu.  There is
;; no way to manipulate the menu foreground and background color from
;; elisp.  You can also set more specific menu resources for Emacs in
;; the resource file.  Here is a sample entry for your resource file:
;;
;;   Emacs*Background:		DarkSlateGray
;;   Emacs*Foreground:		wheat

;; Creating your own color theme:
;;
;; Use M-x customize-face and customize the faces.  Make sure to "Set
;; for Current Session" -- you don't want to save these using custom!
;; When you are done, call M-x color-theme-print to produce the elisp
;; code required to recreate your theme.  Better yet, use M-x
;; color-theme-submit to mail it to the maintainer.  That way it will be
;; added to future versions of color-theme.el.
;;
;; For more information on the elisp format of a color theme, start with
;; the documentation of `color-theme-install' using C-h f
;; color-theme-install.
;;
;; When your color theme is just a variation of an existing color theme,
;; take a look at `color-theme-robin-hood' in order to see an example of
;; how to do it.  Essentially you want to call all the parent color
;; themes before installing your changes.  For all but the first parent
;; color theme, you need to make sure that `color-theme-is-cumulative'
;; is bound to t.  If you don't do that, users that set
;; `color-theme-is-cumulative' to nil will only install your changes
;; without the parent color themes.

;; Making a color theme work for both Emacs and XEmacs:
;;
;; Once you have printed the color-theme, you can make sure it looks
;; similar in both Emacs and XEmacs by running
;; `color-theme-analyze-defun' on the printed theme.  This function
;; will check for missing faces for the other editor...

;;; Thanks

;; Deepak Goel  <deego@glue.umd.edu>
;; S. Pokrovsky <pok@nbsp.nsk.su> for ideas and discussion.
;; Gordon Messmer <gordon@dragonsdawn.net> for ideas and discussion.
;; Sriram Karra <karra@cs.utah.edu> for the color-theme-submit stuff.
;; Olgierd `Kingsajz' Ziolko <kingsajz@rpg.pl> for the spec-filter idea.
;; All the users that contributed their color themes.

;;; Bugs:

;; Emacs 20.7: Some faces are created using copy-face; these faces are
;; not printed correctly using M-x color-theme-print.  They will have
;; (nil) in their spec.  M-x customize-face has the same problem.
;; Example:
;; (copy-face 'bold 'new-bold)
;; (color-theme-spec 'bold)
;;   => (bold ((t (:bold t))))
;; (color-theme-spec 'new-bold)
;;   => (new-bold ((t (nil))))
;;
;; XEmacs 21.1: Some faces are defined using a certain font instead of
;; of the correct attribute.  They will have (nil) in their spec.
;; M-x customize-face has the same problem.
;; Example:
;; (color-theme-spec 'bold)
;;   => (bold ((t (nil))))
;;
;; XEmacs 21.2 and up, Emacs 21: Not compatible with the custom-theme
;; mode.  It should be easy to transform the color-theme source into
;; custom-theme source, however.
;;
;; If you are running XEmacs, then only foreground and background color
;; of the default face and only the background color of the text-cursor
;; face will used.  This is due to the fact that these three pieces of
;; information are stored as frame parameters in Emacs.
;;
;; If you are running XEmacs, variables cannot have a frame-local
;; binding.  Therefore, if color-theme-is-global is set to nil, the
;; variable settings in a color theme are ignored.
;;
;; Using Emacs and a non-nil value for color-theme-is-global will
;; install a new color theme for all frames.  Using XEmacs and a non-nil
;; value for color-theme-is-global will install a new color theme only
;; on those frames that are not using a local color theme.
;;
;; If your system does not define the color names used, you will get the
;; error "undefined color".  See the output of `list-colors-display' for
;; a list of colors defined on your display.
;;
;; The :box, :height, and other new attributes will be honored in Emacs
;; 21, but when you print such a color-theme on Emacs 20 or XEmacs 21,
;; the information will get lost.  So don't do that.  Furthermore,
;; customizing these faces may end up showing you a lisp expression
;; instead of the real widgets on Emacs 20 or XEmacs 21 because these
;; attributes are not understood.
;;
;; :inverse-video handling differs in Emacs and XEmacs.  We therefore do
;; away with it.  When printing a color-theme, the inverse-video
;; attribute should be handled correctly without ever appearing in color
;; themes.  For maintenance, the following might be usefull for
;; query-replace-regexp.
;; :background "\([^"]*\)"\(.*\):foreground "\([^"]*\)"\(.*\) :inverse-video t
;; :background "\3"\2:foreground "\1"\4
;;
;; In XEmacs 21.1, some of the face tests don't work.  Example:
;; (custom-face-bold 'bold) returns nil on my system.  A bug report was
;; submitted.
;;
;; Emacs 20 users will loose with new color themes, because these will
;; set the colors of the default face only, leaving frame background
;; untouched.  In Emacs 20, the colors of the default face and of the
;; frame could be changed independently.  In Emacs 21, this is no longer
;; true.  New color themes will not be made backwards compatible.
;;
;; This release was superficially tested with Emacs 21.2 and XEmacs 21.4.



;;; Code:

(require 'cl); set-difference is a function...

;; for custom-face-attributes-get or face-custom-attributes-get
(require 'cus-face)
(require 'wid-edit); for widget-apply stuff in cus-face.el

(defconst color-theme-maintainer-address "alex@gnu.org"
  "Address used by `submit-color-theme'.")

;; Emacs / XEmacs compatibility and workaround layer

(cond ((and (facep 'tool-bar)
	    (not (facep 'toolbar)))
       (put 'toolbar 'face-alias 'tool-bar))
      ((and (facep 'toolbar)
	    (not (facep 'tool-bar)))
       (put 'tool-bar 'face-alias 'toolbar)))

(defvar color-theme-xemacs-p (string-match "XEmacs" emacs-version)
  "Non-nil if running XEmacs.")

;; face-attr-construct has a problem in Emacs 20.7 and older when
;; dealing with inverse-video faces.  Here is a short test to check
;; wether you are affected.

;; (set-background-color "wheat")
;; (set-foreground-color "black")
;; (setq a (make-face 'a-face))
;; (face-spec-set a '((t (:background "white" :foreground "black" :inverse-video t))))
;; (face-attr-construct a)
;;     => (:background "black" :inverse-video t)

;; The expected response is the original specification:
;;     => (:background "white" :foreground "black" :inverse-video t)

;; That's why we depend on cus-face.el functionality.

(cond ((fboundp 'custom-face-attributes-get)
       (defalias 'color-theme-face-attr-construct
	 'custom-face-attributes-get))
      ((fboundp 'face-custom-attributes-get)
       (defalias 'color-theme-face-attr-construct
	 'face-custom-attributes-get))
      (t
       (defun color-theme-face-attr-construct (&rest ignore)
	 (error "Unable to construct face attributes"))))

(defun color-theme-alist (plist)
  "Transform PLIST into an alist if it is a plist and return it.
If the first element of PLIST is a cons cell, we just return PLIST,
assuming PLIST to be an alist.  If the first element of plist is not a
symbol, this is an error: We cannot distinguish a plist from an ordinary
list, but a list that doesn't start with a symbol is certainly no plist
and no alist.

This is used to make sure `default-frame-alist' really is an alist and not
a plist.  In XEmacs, the alist is deprecated; a plist is used instead."
  (cond ((consp (car plist))
	 plist)
	((not (symbolp (car plist)))
	 (error "Wrong type argument: plist, %S" plist))
	(t
	 (plist-to-alist plist)))); XEmacs only

;; Customization

(defgroup color-theme nil
  "Color Themes for Emacs.
A color theme consists of frame parameter settings, variable settings,
and face definitions."
  :version "20.6"
  :group 'faces)

(defcustom color-theme-legal-frame-parameters "\\(color\\|mode\\)$"
  "Regexp that matches frame parameter names.
Only frame parameter names that match this regexp can be changed as part
of a color theme."
  :type '(choice (const :tag "Colors only" "\\(color\\|mode\\)$")
		 (const :tag "Colors, fonts, and size"
			"\\(color\\|mode\\|font\\|height\\|width\\)$")
		 (regexp :tag "Custom regexp"))
  :group 'color-theme
  :link '(info-link "(elisp)Window Frame Parameters"))

(defcustom color-theme-legal-variables "\\(color\\|face\\)$"
  "Regexp that matches variable names.
Only variables that match this regexp can be changed as part of a color
theme.  In addition to matching this name, the variables have to be user
variables (see function `user-variable-p')."
  :type 'regexp
  :group 'color-theme)

(defcustom color-theme-illegal-faces "^w3-"
  "Regexp that matches face names forbidden in themes.
The default setting \"^w3-\" excludes w3 faces since these
are created dynamically."
  :type 'regexp
  :group 'color-theme
  :link '(info-link "(elisp)Faces for Font Lock")
  :link '(info-link "(elisp)Standard Faces"))

(defcustom color-theme-illegal-default-attributes '(:family :height :width)
  "A list of face properties to be ignored when installing faces.
This prevents Emacs from doing terrible things to your display just because
a theme author likes weird fonts."
  :type '(repeat symbol)
  :group 'color-theme)

(defcustom color-theme-is-global t
  "*Determines wether a color theme is installed on all frames or not.
If non-nil, color themes will be installed for all frames.
If nil, color themes will be installed for the selected frame only.

A possible use for this variable is dynamic binding. Here is a larger
example to put in your ~/.emacs; it will make the Blue Sea color theme
the default used for the first frame, and it will create two additional
frames with different color themes.

setup:
    \(require 'color-theme)
    ;; set default color theme
    \(color-theme-blue-sea)
    ;; create some frames with different color themes
    \(let ((color-theme-is-global nil))
      \(select-frame (make-frame))
      \(color-theme-gnome2)
      \(select-frame (make-frame))
      \(color-theme-standard))

Please note that using XEmacs and and a nil value for
color-theme-is-global will ignore any variable settings for the color
theme, since XEmacs doesn't have frame-local variable bindings.

Also note that using Emacs and a non-nil value for color-theme-is-global
will install a new color theme for all frames.  Using XEmacs and a
non-nil value for color-theme-is-global will install a new color theme
only on those frames that are not using a local color theme."
  :type 'boolean
  :group 'color-theme)

(defcustom color-theme-is-cumulative t
  "*Determines wether new color themes are installed on top of each other.
If non-nil, installing a color theme will undo all settings made by
previous color themes."
  :type 'boolean
  :group 'color-theme)

(defcustom color-theme-mode-hook nil
  "Hook for color-theme-mode."
  :type 'hook
  :group 'color-theme)

(defvar color-theme-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'color-theme-install-at-point)
    (define-key map (kbd "c") 'list-colors-display)
    (define-key map (kbd "d") 'color-theme-describe)
    (define-key map (kbd "f") 'list-faces-display)
    (define-key map (kbd "i") 'color-theme-install-at-point)
    (define-key map (kbd "l") 'color-theme-install-at-point-for-current-frame)
    (define-key map (kbd "p") 'color-theme-print)
    (define-key map (kbd "q") 'bury-buffer)
    (define-key map (kbd "?") 'color-theme-describe)
    (if color-theme-xemacs-p
	(define-key map (kbd "<button2>") 'color-theme-install-at-mouse)
      (define-key map (kbd "<mouse-2>") 'color-theme-install-at-mouse))
    map)
  "Mode map used for the buffer created by `color-theme-select'.")

(defvar color-theme-buffer-name "*Color Theme Selection*"
  "Name of the color theme selection buffer.")

(defvar color-theme-original-frame-alist nil
  "nil until one of the color themes has been installed.")

(defvar color-theme-history nil
  "List of color-themes called, in reverse order")

(defcustom color-theme-history-max-length nil
  "Max length of history to maintain.
Two other values are acceptable: t means no limit, and
nil means that no history is maintained."
  :type '(choice (const :tag "No history" nil)
		 (const :tag "Unlimited length" t)
		 integer)
  :group 'color-theme)

(defvar color-theme-counter 0
  "Counter for every addition to `color-theme-history'.
This counts how many themes were installed, regardless
of `color-theme-history-max-length'.")

(defun color-theme-add-to-history (name)
  "Add color-theme NAME to `color-theme-history'."
  (setq color-theme-history
	(cons (list name color-theme-is-cumulative)
	      color-theme-history)
	color-theme-counter (+ 1 color-theme-counter))
  ;; Truncate the list if necessary.
  (when (and (integerp color-theme-history-max-length)
	     (>= (length color-theme-history)
		 color-theme-history-max-length))
    (setcdr (nthcdr (1- color-theme-history-max-length)
		    color-theme-history)
	    nil)))

;; (let ((l '(1 2 3 4 5)))
;;   (setcdr (nthcdr 2 l) nil)
;;   l)



;; List of color themes used to create the *Color Theme Selection*
;; buffer.

(defvar color-themes nil
  "List of color themes.

Each THEME is itself a three element list (FUNC NAME MAINTAINER &optional LIBRARY).

FUNC is a color theme function which does the setup.  The function
FUNC may call `color-theme-install'.  The color theme function may be
interactive.

NAME is the name of the theme and MAINTAINER is the name and/or email of
the maintainer of the theme.

If LIBRARY is non-nil, the color theme will be considered a library and
may not be shown in the default menu.

If you defined your own color theme and want to add it to this list,
use something like this:

  (add-to-list 'color-themes '(color-theme-gnome2 \"Gnome2\" \"Alex\"))")

;;; Functions

(defun color-theme-backup-original-values ()
  "Back up the original `default-frame-alist'.
The values are stored in `color-theme-original-frame-alist' on
startup."
  (if (null color-theme-original-frame-alist)
      (setq color-theme-original-frame-alist
	    (color-theme-filter (frame-parameters (selected-frame))
				color-theme-legal-frame-parameters))))
(add-hook 'after-init-hook 'color-theme-backup-original-values)

(defun color-theme-select (&optional arg)
  "Displays a special buffer for selecting and installing a color theme.
With optional prefix ARG, this buffer will include color theme libraries
as well.  A color theme library is in itself not complete, it must be
used as part of another color theme to be useful.  Thus, color theme
libraries are mainly useful for color theme authors."
  (interactive "P")
  (switch-to-buffer (get-buffer-create color-theme-buffer-name))
  (setq buffer-read-only nil)
  (erase-buffer)
  ;; recreate the snapshot if necessary
  (when (or (not (assq 'color-theme-snapshot color-themes))
	    (not (commandp 'color-theme-snapshot)))
    (fset 'color-theme-snapshot (color-theme-make-snapshot))
    (setq color-themes (delq (assq 'color-theme-snapshot color-themes)
			     color-themes)
	  color-themes (delq (assq 'bury-buffer color-themes)
			     color-themes)
	  color-themes (append '((color-theme-snapshot
				  "[Reset]" "Undo changes, if possible.")
				 (bury-buffer
				  "[Quit]" "Bury this buffer."))
			     color-themes)))
  (dolist (theme color-themes)
    (let ((func (nth 0 theme))
	  (name (nth 1 theme))
	  (author (nth 2 theme))
	  (library (nth 3 theme))
	  (desc))
      (when (or (not library) arg)
	(setq desc (format "%-23s %s" 
			   (if library (concat name " [lib]") name)
			   author))
	(put-text-property 0 (length desc) 'color-theme func desc)
	(put-text-property 0 (length name) 'face 'bold desc)
	(put-text-property 0 (length name) 'mouse-face 'highlight desc)
	(insert desc)
	(newline))))
  (beginning-of-buffer)
  (setq buffer-read-only t)
  (set-buffer-modified-p nil)
  (color-theme-mode))

(require 'easymenu)
(easy-menu-add-item nil '("tools") "--")
(easy-menu-add-item  nil '("tools")
  ["Color Themes" color-theme-select t])

(defun color-theme-mode ()
  "Major mode to select and install color themes.

Use \\[color-theme-install-at-point] to install a color theme on all frames.
Use \\[color-theme-install-at-point-for-current-frame] to install a color theme for the current frame only.

The changes are applied on top of your current setup.  This is a
feature.

Some of the themes should be considered extensions to the standard color
theme: they modify only a limited number of faces and variables.  To
verify the final look of a color theme, install the standard color
theme, then install the other color theme.  This is a feature. It allows
you to mix several color themes.

Use \\[color-theme-describe] to read more about the color theme function at point.
If you want to install the color theme permanently, put the call to the
color theme function into your ~/.emacs:

    \(require 'color-theme)
    \(color-theme-gnome2)

If you worry about the size of color-theme.el: You are right.  Use
\\[color-theme-print] to print the current color theme and save the resulting buffer
as ~/.emacs-color-theme.  Now you can install only this specific color
theme in your .emacs:

    \(load-file \"~/.emacs-color-theme\")
    \(my-color-theme)

The Emacs menu is not affected by color themes within Emacs.  Depending
on the toolkit you used to compile Emacs, you might have to set specific
X ressources.  See the info manual for more information.  Here is an
example ~/.Xdefaults fragment:

    emacs*Background: DarkSlateGray
    emacs*Foreground: wheat

\\{color-theme-mode-map}

The color themes are listed in `color-themes', which see."
  (kill-all-local-variables)
  (setq major-mode 'color-theme-mode)
  (setq mode-name "Color Themes")
  (use-local-map color-theme-mode-map)
  (when (functionp 'goto-address); Emacs
    (goto-address))
  (run-hooks 'color-theme-mode-hook))

;;; Commands in Color Theme Selection mode

(defun color-theme-describe ()
  "Describe color theme listed at point.
This shows the documentation of the value of text-property color-theme
at point.  The text-property color-theme should be a color theme
function.  See `color-themes'."
  (interactive)
  (describe-function (get-text-property (point) 'color-theme)))

(defun color-theme-install-at-mouse (event)
  "Install color theme clicked upon using the mouse.
First argument EVENT is used to set point.  Then
`color-theme-install-at-point' is called."
  (interactive "e")
  (save-excursion
    (mouse-set-point event)
    (color-theme-install-at-point)))

(defun color-theme-install-at-point ()
  "Install color theme at point.
This calls the value of the text-property `color-theme' at point.
The text-property `color-theme' should be a color theme function.
See `color-themes'."
  (interactive)
  (let ((func (get-text-property (point) 'color-theme)))
    ;; install theme
    (if func
	(funcall func))
    ;; If goto-address is being used, remove all overlays in the current
    ;; buffer and run it again.  The face used for the mail addresses in
    ;; the the color theme selection buffer is based on the variable
    ;; goto-address-mail-face.  Changes in that variable will not affect
    ;; existing overlays, however, thereby confusing users.
    (when (functionp 'goto-address); Emacs
      (dolist (o (overlays-in (point-min) (point-max)))
	(delete-overlay o))
      (goto-address))))

(defun color-theme-install-at-point-for-current-frame ()
  "Install color theme at point for current frame only.
Binds `color-theme-is-global' to nil and calls
`color-theme-install-at-point'."
  (interactive)
  (let ((color-theme-is-global nil))
    (color-theme-install-at-point)))



;; Taking a snapshot of the current color theme and pretty printing it.

(defun color-theme-filter (old-list regexp &optional exclude)
  "Filter OLD-LIST.
The resulting list will be newly allocated and contains only elements
with names matching REGEXP.  OLD-LIST may be a list or an alist.  If you
want to filter a plist, use `color-theme-alist' to convert your plist to
an alist, first.

If the optional argument EXCLUDE is non-nil, then the sense is
reversed: only non-matching elements will be retained."
  (let (elem new-list)
    (dolist (elem old-list)
      (setq name (symbol-name (if (listp elem) (car elem) elem)))
      (when (or (and (not exclude)
		     (string-match regexp name))
		(and exclude
		     (not (string-match regexp name))))
	;; Now make sure that if elem is a cons cell, and the cdr of
	;; that cons cell is a string, then we need a *new* string in
	;; the new list.  Having a new cons cell is of no use because
	;; modify-frame-parameters will modify this string, thus
	;; modifying our color theme functions!
	(when (and (consp elem)
		   (stringp (cdr elem)))
	  (setq elem (cons (car elem)
			   (copy-sequence (cdr elem)))))
	;; Now store elem
	(setq new-list (cons elem new-list))))
    new-list))

(defun color-theme-spec-filter (spec)
  "Filter the attributes in SPEC.
This makes sure that SPEC has the form ((t (PLIST ...))).
Only properties not in `color-theme-illegal-default-attributes'
are included in the SPEC returned."
  (let ((props (cadar spec))
	result prop val)
    (while props
      (setq prop (nth 0 props)
	    val (nth 1 props)
	    props (nthcdr 2 props))
      (unless (memq prop color-theme-illegal-default-attributes)
	(setq result (cons val (cons prop result)))))
    `((t ,(nreverse result)))))

;; (color-theme-spec-filter '((t (:background "blue3"))))
;; (color-theme-spec-filter '((t (:stipple nil :background "Black" :foreground "SteelBlue" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :width semi-condensed :family "misc-fixed"))))

(defun color-theme-plist-delete (plist prop)
  "Delete property PROP from property list PLIST by side effect.
This modifies PLIST."
  ;; deal with prop at the start
  (while (eq (car plist) prop)
    (setq plist (cddr plist)))
  ;; deal with empty plist
  (when plist
    (let ((lastcell (cdr plist))
	  (l (cddr plist)))
      (while l
	(if (eq (car l) prop)
	    (progn
	      (setq l (cddr l))
	      (setcdr lastcell l))
	  (setq lastcell (cdr l)
		l (cddr l))))))
  plist)

;; (color-theme-plist-delete '(a b c d e f g h) 'a)
;; (color-theme-plist-delete '(a b c d e f g h) 'b)
;; (color-theme-plist-delete '(a b c d e f g h) 'c)
;; (color-theme-plist-delete '(a b c d e f g h) 'g)
;; (color-theme-plist-delete '(a b c d c d e f g h) 'c)
;; (color-theme-plist-delete '(a b c d e f c d g h) 'c)

(if (or (featurep 'xemacs)
	(< emacs-major-version 21))
    (defalias 'color-theme-spec-compat 'identity)
  (defun color-theme-spec-compat (spec)
    "Filter the attributes in SPEC such that is is never invalid.
Example: Eventhough :bold works in Emacs, it is not recognized by
`customize-face' -- and then the face is uncustomizable.  This
function replaces a :bold attribute with the corresponding :weight
attribute, if there is no :weight, or deletes it.  This undoes the
doings of `color-theme-spec-canonical-font', more or less."
    (let ((props (cadar spec)))
      (when (plist-member props :bold)
	(setq props (color-theme-plist-delete props :bold))
	(unless (plist-member props :weight)
	  (setq props (plist-put props :weight 'bold))))
      (when (plist-member props :italic)
	(setq props (color-theme-plist-delete props :italic))
	(unless (plist-member props :slant)
	  (setq props (plist-put props :slant 'italic))))
      `((t ,props)))))

;; (color-theme-spec-compat '((t (:foreground "blue" :bold t))))
;; (color-theme-spec-compat '((t (:bold t :foreground "blue" :weight extra-bold))))
;; (color-theme-spec-compat '((t (:italic t :foreground "blue"))))
;; (color-theme-spec-compat '((t (:slant oblique :italic t :foreground "blue"))))

(defun color-theme-spec-canonical-font (atts)
  "Add :bold and :italic attributes if necessary."
  ;; add these to the front of atts -- this will keept the old value for
  ;; customize-face in Emacs 21.
  (when (and (memq (plist-get atts :weight)
		   '(ultra-bold extra-bold bold semi-bold))
	     (not (plist-get atts :bold)))
    (setq atts (cons :bold (cons t atts))))
  (when (and (not (memq (plist-get atts :slant)
			'(normal nil)))
	     (not (plist-get atts :italic)))
    (setq atts (cons :italic (cons t atts))))
  atts)
;; (color-theme-spec-canonical-font (color-theme-face-attr-construct 'bold (selected-frame)))
;; (defface foo '((t (:weight extra-bold))) "foo")
;; (color-theme-spec-canonical-font (color-theme-face-attr-construct 'foo (selected-frame)))
;; (face-spec-set 'foo '((t (:weight extra-bold))) nil)
;; (face-spec-set 'foo '((t (:bold t))) nil)
;; (face-spec-set 'foo '((t (:bold t :weight extra-bold))) nil)

;; Handle :height according to NEWS file for Emacs 21
(defun color-theme-spec-resolve-height (old new)
  "Return the new height given OLD and NEW height.
OLD is the current setting, NEW is the setting inherited from."
  (cond ((not old)
	 new)
	((integerp old)
	 old)
	((and (floatp old)
	      (integerp new))
	 (round (* old new)))
	((and (floatp old)
	      (floatp new))
	 (* old new))
	((and (functionp old)
	      (integerp new))
	 (round (funcall old new)))
	((and (functionp old)
	      (float new))
	 `(lambda (f) (* (funcall ,old f) ,new)))
	((and (functionp old)
	      (functionp new))
	 `(lambda (f) (* (funcall ,old (funcall ,new f)))))
	(t
	 (error "Illegal :height attributes: %S or %S" old new))))
;; (color-theme-spec-resolve-height 12 1.2)
;; (color-theme-spec-resolve-height 1.2 1.2)
;; (color-theme-spec-resolve-height 1.2 12)
;; (color-theme-spec-resolve-height 1.2 'foo)
;; (color-theme-spec-resolve-height (lambda (f) (* 2 f)) 5)
;; (color-theme-spec-resolve-height (lambda (f) (* 2 f)) 2.0)
;; the following lambda is the result from the above calculation
;; (color-theme-spec-resolve-height (lambda (f) (* (funcall (lambda (f) (* 2 f)) f) 2.0)) 5)

(defun color-theme-spec-resolve-inheritance (atts)
  "Resolve all occurences of the :inherit attribute."
  (let ((face (plist-get atts :inherit)))
    ;; From the Emacs 21 NEWS file: "Attributes from inherited faces are
    ;; merged into the face like an underlying face would be." --
    ;; therefore properties of the inherited face only add missing
    ;; attributes.
    (when face
      ;; remove :inherit face from atts -- this assumes only one
      ;; :inherit attribute.
      (setq atts (delq ':inherit (delq face atts)))
      (let ((more-atts (color-theme-spec-resolve-inheritance
			(color-theme-face-attr-construct
			 face (selected-frame))))
	    att val)
	(while more-atts
	  (setq att (car more-atts)
		val (cadr more-atts)
		more-atts (cddr more-atts))
	  ;; Color-theme assumes that no value is ever 'unspecified.
	  (cond ((eq att ':height); cumulative effect!
		 (setq atts (plist-put atts 
				       ':height 
				       (color-theme-spec-resolve-height
					(plist-get atts att) 
					val))))
		;; Default: Only put if it has not been specified before.
		((not (plist-get atts att))
		 (setq atts (cons att (cons val atts))))
		  
))))
    atts))
;; (color-theme-spec-resolve-inheritance '(:bold t))
;; (color-theme-spec-resolve-inheritance '(:bold t :foreground "blue"))
;; (color-theme-face-attr-construct 'font-lock-comment-face (selected-frame))
;; (color-theme-spec-resolve-inheritance '(:bold t :inherit font-lock-comment-face))
;; (color-theme-spec-resolve-inheritance '(:bold t :foreground "red" :inherit font-lock-comment-face))
;; (color-theme-face-attr-construct 'Info-title-2-face (selected-frame))
;; (color-theme-face-attr-construct 'Info-title-3-face (selected-frame))
;; (color-theme-face-attr-construct 'Info-title-4-face (selected-frame))
;; (color-theme-spec-resolve-inheritance '(:inherit Info-title-2-face))

;; The :inverse-video attribute causes Emacs to swap foreground and
;; background colors, XEmacs does not.  Therefore, if anybody chooses
;; the inverse-video attribute, we 1. swap the colors ourselves in Emacs
;; and 2. we remove the inverse-video attribute in Emacs and XEmacs.
;; Inverse-video is only useful on a monochrome tty.
(defun color-theme-spec-maybe-invert (atts)
  "Remove the :inverse-video attribute from ATTS.
If ATTS contains :inverse-video t, remove it and swap foreground and
background color.  Return ATTS."
  (let ((inv (plist-get atts ':inverse-video)))
    (if inv
	(let (result att)
	  (while atts
	    (setq att (car atts)
		  atts (cdr atts))
	    (cond ((and (eq att :foreground) (not color-theme-xemacs-p))
		   (setq result (cons :background result)))
		  ((and (eq att :background) (not color-theme-xemacs-p))
		   (setq result (cons :foreground result)))
		  ((eq att :inverse-video)
		   (setq atts (cdr atts))); this prevents using dolist
		  (t
		   (setq result (cons att result)))))
	  (nreverse result))
      ;; else
      atts)))
;; (color-theme-spec-maybe-invert '(:bold t))
;; (color-theme-spec-maybe-invert '(:foreground "blue"))
;; (color-theme-spec-maybe-invert '(:background "red"))
;; (color-theme-spec-maybe-invert '(:inverse-video t))
;; (color-theme-spec-maybe-invert '(:inverse-video t :foreground "red"))
;; (color-theme-spec-maybe-invert '(:inverse-video t :background "red"))
;; (color-theme-spec-maybe-invert '(:inverse-video t :background "red" :foreground "blue" :bold t))
;; (color-theme-spec-maybe-invert '(:inverse-video nil :background "red" :foreground "blue" :bold t))

(defun color-theme-spec (face)
  "Return a list for FACE which has the form (FACE SPEC).
See `defface' for the format of SPEC.  In this case we use only one
DISPLAY, t, and determine ATTS using `color-theme-face-attr-construct'.
If ATTS is nil, (nil) is used  instead.

If ATTS contains :inverse-video t, we remove it and swap foreground and
background color using `color-theme-spec-maybe-invert'.  We do this
because :inverse-video is handled differently in Emacs and XEmacs.  We
will loose on a tty without colors, because in that situation,
:inverse-video means something."
  (let ((atts
	 (color-theme-spec-canonical-font
	  (color-theme-spec-maybe-invert
	   (color-theme-spec-resolve-inheritance
	    (color-theme-face-attr-construct face (selected-frame)))))))
    (if atts
	`(,face ((t ,atts)))
      `(,face ((t (nil)))))))

(defun color-theme-get-params ()
  "Return a list of frame parameter settings usable in a color theme.
Such an alist may be installed by `color-theme-install-frame-params'.  The
frame parameters returned must match `color-theme-legal-frame-parameters'."
  (let ((params (color-theme-filter (frame-parameters (selected-frame))
				    color-theme-legal-frame-parameters)))
    (sort params (lambda (a b) (string< (symbol-name (car a))
					(symbol-name (car b)))))))

(defun color-theme-get-vars ()
  "Return a list of variable settings usable in a color theme.
Such an alist may be installed by `color-theme-install-variables'.
The variable names must match `color-theme-legal-variables', and the
variable must be a user variable according to `user-variable-p'."
  (let ((vars)
	(val))
    (mapatoms (lambda (v)
		(and (boundp v)
		     (user-variable-p v)
		     (string-match color-theme-legal-variables
				   (symbol-name v))
		     (setq val (eval v))
		     (add-to-list 'vars (cons v val)))))
    (sort vars (lambda (a b) (string< (car a) (car b))))))

(defun color-theme-print-alist (alist)
  "Print ALIST."
  (insert "\n     " (if alist "(" "nil"))
  (dolist (elem alist)
    (when (= (preceding-char) ?\))
      (insert "\n      "))
    (prin1 elem (current-buffer)))
  (when (= (preceding-char) ?\)) (insert ")")))

(defun color-theme-get-faces ()
  "Return a list of faces usable in a color theme.
Such an alist may be installed by `color-theme-install-faces'.  The
faces returned must not match `color-theme-illegal-faces'."
  (let ((faces (color-theme-filter (face-list) color-theme-illegal-faces t)))
    ;; default face must come first according to comments in
    ;; custom-save-faces, the rest is to be sorted by name
    (cons 'default (sort (delq 'default faces) 'string-lessp))))

(defun color-theme-get-face-definitions ()
  "Return face settings usable in a color-theme."
  (let ((faces (color-theme-get-faces)))
    (mapcar 'color-theme-spec faces)))

(defun color-theme-print-faces (faces)
  "Print face settings for all faces returned by `color-theme-get-faces'."
  (when faces
    (insert "\n     "))
  (dolist (face faces)
    (when (= (preceding-char) ?\))
      (insert "\n     "))
    (prin1 face (current-buffer))))

(defun color-theme-reset-faces ()
  "Reset face settings for all faces returned by `color-theme-get-faces'."
  (let ((faces (color-theme-get-faces))
	(face) (spec) (entry)
	(frame (if color-theme-is-global nil (selected-frame))))
    (while faces
      (setq entry (color-theme-spec (car faces)))
      (setq face (nth 0 entry))
      (setq spec '((t (nil))))
      (setq faces (cdr faces))
      (if (functionp 'face-spec-reset-face)
	  (face-spec-reset-face face frame)
	(face-spec-set face spec frame)
	(if color-theme-is-global
	    (put face 'face-defface-spec spec))))))

(defun color-theme-print-theme (func doc params vars faces)
  "Print a theme into the current buffer.
FUNC is the function name, DOC the doc string, PARAMS the
frame parameters, VARS the variable bindings, and FACES
the list of faces and their specs."
  (insert "(defun " (symbol-name func) " ()\n"
	  "  \"" doc "\"\n"
	  "  (interactive)\n"
	  "  (color-theme-install\n"
	  "   '(" (symbol-name func))
  ;; alist of frame parameters
  (color-theme-print-alist params)
  ;; alist of variables
  (color-theme-print-alist vars)
  ;; remaining elements of snapshot: face specs
  (color-theme-print-faces faces)
  (insert ")))")
  (goto-char (point-min)))

(defun color-theme-print (&optional buf)
  "Print the current color theme function.

You can contribute this function to <URL:news:gnu.emacs.sources> or
paste it into your .emacs file and call it.  That should recreate all
the settings necessary for your color theme.

Example:

    \(require 'color-theme)
    \(defun my-color-theme ()
      \"Color theme by Alex Schroeder, created 2000-05-17.\"
      \(interactive)
      \(color-theme-install
       '(...
	 ...
	 ...)))
    \(my-color-theme)

If you want to use a specific color theme function, you can call the
color theme function in your .emacs directly.

Example:

    \(require 'color-theme)
    \(color-theme-gnome2)"
  (interactive)
  (message "Pretty printing current color theme function...")
  (switch-to-buffer (if buf
			buf
		      (get-buffer-create "*Color Theme*")))
  (unless buf
    (setq buffer-read-only nil)
    (erase-buffer))
  ;; insert defun
  (color-theme-print-theme 'my-color-theme
			   (concat "Color theme by "
				   (if (string= "" user-full-name)
				       (user-login-name)
				     user-full-name)
				   ", created " (format-time-string "%Y-%m-%d") ".")
			   (color-theme-get-params)
			   (color-theme-get-vars)
			   (mapcar 'color-theme-spec (color-theme-get-faces)))
  (unless buf
    (emacs-lisp-mode))
  (goto-char (point-min))
  (message "Pretty printing current color theme function... done"))

(defun color-theme-analyze-find-theme (code)
  "Find the sexpr that calls `color-theme-install'."
  (let (theme)
    (while (and (not theme) code)
      (when (eq (car code) 'color-theme-install)
	(setq theme code))
      (when (listp (car code))
	(setq theme (color-theme-analyze-find-theme (car code))))
      (setq code (cdr code)))
    theme))

;; (equal (color-theme-analyze-find-theme
;; 	'(defun color-theme-blue-eshell ()
;; 	   "Color theme for eshell faces only."
;; 	   (color-theme-install
;; 	    '(color-theme-blue-eshell
;; 	      nil
;; 	      (eshell-ls-archive-face ((t (:bold t :foreground "IndianRed"))))
;; 	      (eshell-ls-backup-face ((t (:foreground "Grey"))))))))
;;        '(color-theme-install
;; 	 (quote
;; 	  (color-theme-blue-eshell
;; 	   nil
;; 	   (eshell-ls-archive-face ((t (:bold t :foreground "IndianRed"))))
;; 	   (eshell-ls-backup-face ((t (:foreground "Grey")))))))))

(defun color-theme-analyze-add-face (a b regexp faces)
  "If only one of A or B are in FACES, the other is added, and FACES is returned.
If REGEXP is given, this is only done if faces contains a match for regexps."
  (when (or (not regexp)
	    (catch 'found
	      (dolist (face faces)
		(when (string-match regexp (symbol-name (car face)))
		  (throw 'found t)))))
    (let ((face-a (assoc a faces))
	  (face-b (assoc b faces)))
      (if (and face-a (not face-b))
	  (setq faces (cons (list b (nth 1 face-a))
			    faces))
	(if (and (not face-a) face-b)
	    (setq faces (cons (list a (nth 1 face-b))
			      faces))))))
  faces)

;; (equal (color-theme-analyze-add-face
;; 	'blue 'violet nil
;; 	'((blue ((t (:foreground "blue"))))
;; 	  (bold ((t (:bold t))))))
;;        '((violet ((t (:foreground "blue"))))
;; 	 (blue ((t (:foreground "blue"))))
;; 	 (bold ((t (:bold t))))))
;; (equal (color-theme-analyze-add-face
;; 	'violet 'blue nil
;; 	'((blue ((t (:foreground "blue"))))
;; 	  (bold ((t (:bold t))))))
;;        '((violet ((t (:foreground "blue"))))
;; 	 (blue ((t (:foreground "blue"))))
;; 	 (bold ((t (:bold t))))))
;; (equal (color-theme-analyze-add-face
;; 	'violet 'blue "foo"
;; 	'((blue ((t (:foreground "blue"))))
;; 	  (bold ((t (:bold t))))))
;;        '((blue ((t (:foreground "blue"))))
;; 	 (bold ((t (:bold t))))))
;; (equal (color-theme-analyze-add-face
;; 	'violet 'blue "blue"
;; 	'((blue ((t (:foreground "blue"))))
;; 	  (bold ((t (:bold t))))))
;;        '((violet ((t (:foreground "blue"))))
;; 	 (blue ((t (:foreground "blue"))))
;; 	 (bold ((t (:bold t))))))

(defun color-theme-analyze-add-faces (faces)
  "Add missing faces to FACES and return it."
  ;; The most important thing is to add missing faces for the other
  ;; editor.  These are the most important faces to check.  The
  ;; following rules list two faces, A and B.  If either of the two is
  ;; part of the theme, the other must be, too.  The optional third
  ;; argument specifies a regexp.  Only if an existing face name
  ;; matches this regexp, is the rule applied.
  (let ((rules '((font-lock-builtin-face font-lock-reference-face)
		 (font-lock-doc-face font-lock-doc-string-face)
		 (font-lock-constant-face font-lock-preprocessor-face)
		 ;; In Emacs 21 `modeline' is just an alias for
		 ;; `mode-line'.  I recommend the use of
		 ;; `modeline' until further notice.
		 (modeline mode-line)
		 (modeline modeline-buffer-id)
		 (modeline modeline-mousable)
		 (modeline modeline-mousable-minor-mode)
		 (region primary-selection)
		 (region zmacs-region)
		 (font-lock-string-face dired-face-boring "^dired")
		 (font-lock-function-name-face dired-face-directory "^dired")
		 (default dired-face-executable "^dired")
		 (font-lock-warning-face dired-face-flagged "^dired")
		 (font-lock-warning-face dired-face-marked "^dired")
		 (default dired-face-permissions "^dired")
		 (default dired-face-setuid "^dired")
		 (default dired-face-socket "^dired")
		 (font-lock-keyword-face dired-face-symlink "^dired")
		 (tool-bar menu))))
    (dolist (rule rules)
      (setq faces (color-theme-analyze-add-face
		   (nth 0 rule) (nth 1 rule) (nth 2 rule) faces))))
  ;; The `fringe' face defines what the left and right borders of the
  ;; frame look like in Emacs 21.  To give them default fore- and
  ;; background colors, use (fringe ((t (nil)))) in your color theme.
  ;; Usually it makes more sense to choose a color slightly lighter or
  ;; darker from the default background.
  (unless (assoc 'fringe faces)
    (setq faces (cons '(fringe ((t (nil)))) faces)))
  ;; The tool-bar should not be part of the frame-parameters, since it
  ;; should not appear or disappear depending on the color theme.  The
  ;; apppearance of the toolbar, however, can be changed by the color
  ;; theme.  For Emacs 21, use the `tool-bar' face.  The easiest way
  ;; to do this is to give it the default fore- and background colors.
  ;; This can be achieved using (tool-bar ((t (nil)))) in the theme.
  ;; Usually it makes more sense, however, to provide the same colors
  ;; as used in the `menu' face, and to specify a :box attribute.  In
  ;; order to alleviate potential Emacs/XEmacs incompatibilities,
  ;; `toolbar' will be defined as an alias for `tool-bar' if it does
  ;; not exist, and vice-versa.  This is done eventhough the face
  ;; `toolbar' seems to have no effect on XEmacs.  If you look at
  ;; XEmacs lisp/faces.el, however, you will find that it is in fact
  ;; referenced for XPM stuff.
  (unless (assoc 'tool-bar faces)
    (setq faces (cons '(tool-bar ((t (nil)))) faces)))
  ;; Move the default face back to the front, and sort the rest.
  (unless (eq (caar faces) 'default)
    (let ((face (assoc 'default faces)))
      (setq faces (cons face
			(sort (delete face faces)
			      (lambda (a b)
				(string-lessp (car a) (car b))))))))
  faces)

(defun color-theme-analyze-remove-heights (faces)
  "Remove :height property where it is an integer and return FACES."
  ;; I don't recommend making font sizes part of a color theme.  Most
  ;; users would be surprised to see their font sizes change when they
  ;; install a color-theme.  Therefore, remove all :height attributes
  ;; if the value is an integer.  If the value is a float, this is ok
  ;; -- the value is relative to the default height.  One notable
  ;; exceptions is for a color-theme created for visually impaired
  ;; people.  These *must* use a larger font in order to be usable.
  (let (result)
    (dolist (face faces)
      (let ((props (cadar (nth 1 face))))
	(if (and (plist-member props :height)
		 (integerp (plist-get props :height)))
	    (setq props (color-theme-plist-delete props :height)
		  result (cons (list (car face) `((t ,props)))
			       result))
	  (setq result (cons face result)))))
    (nreverse result)))

;; (equal (color-theme-analyze-remove-heights
;; 	'((blue ((t (:foreground "blue" :height 2))))
;; 	  (bold ((t (:bold t :height 1.0))))))
;;        '((blue ((t (:foreground "blue"))))
;; 	 (bold ((t (:bold t :height 1.0))))))

(defun color-theme-analyze-defun ()
  "Once you have a color-theme printed, check for missing faces.
This is used by maintainers who receive a color-theme submission
and want to make sure it follows the guidelines by the color-theme
author."
  ;; The support for :foreground and :background attributes works for
  ;; Emacs 20 and 21 as well as for XEmacs.  :inverse-video is taken
  ;; care of while printing color themes.
  (interactive)
  ;; Parse the stuff and find the call to color-theme-install
  (save-excursion
    (save-restriction
      (narrow-to-defun)
      ;; define the function
      (eval-defun nil)
      (goto-char (point-min))
      (let* ((code (read (current-buffer)))
	     (theme (color-theme-canonic
		     (eval
		      (cadr
		       (color-theme-analyze-find-theme
			code)))))
	     (func (color-theme-function theme))
	     (doc (documentation func t))
	     (variables (color-theme-variables theme))
	     (faces (color-theme-faces theme))
	     (params (color-theme-frame-params theme)))
	(setq faces (color-theme-analyze-remove-heights
		     (color-theme-analyze-add-faces faces)))
	;; Remove any variable bindings of faces that point to their
	;; symbol?  Perhaps not, because another theme might want to
	;; change this, so it is important to be able to reset them.
	;; 	(let (result)
	;; 	  (dolist (var variables)
	;; 	    (unless (eq (car var) (cdr var))
	;; 	      (setq result (cons var result))))
	;; 	  (setq variables (nreverse result)))
	;; Now modify the theme directly.
	(setq theme (color-theme-analyze-find-theme code))
	(setcdr (cadadr theme) (list params variables faces))
	(message "Pretty printing analysed color theme function...")
	(with-current-buffer (get-buffer-create "*Color Theme*")
	  (setq buffer-read-only nil)
	  (erase-buffer)
	  ;; insert defun
	  (color-theme-print-theme func doc params variables faces)
	  (emacs-lisp-mode))
	(message "Pretty printing analysed color theme function... done")
	(ediff-buffers (current-buffer)
		       (get-buffer "*Color Theme*"))))))

;;; Creating a snapshot of the current color theme

(defun color-theme-snapshot nil)

(defun color-theme-make-snapshot ()
  "Return the definition of the current color-theme.
The function returned will recreate the color-theme in use at the moment."
  (eval `(lambda ()
	   "The color theme in use when the selection buffer was created.
\\[color-theme-select] creates the color theme selection buffer.  At the
same time, this snapshot is created as a very simple undo mechanism.
The snapshot is created via `color-theme-snapshot'."
	   (interactive)
	   (color-theme-install
	    '(color-theme-snapshot
	      ;; alist of frame parameters
	      ,(color-theme-get-params)
	      ;; alist of variables
	      ,(color-theme-get-vars)
	      ;; remaining elements of snapshot: face specs
	      ,@(color-theme-get-face-definitions))))))



;;; Handling the various parts of a color theme install

(defvar color-theme-frame-param-frobbing-rules
  '((foreground-color default foreground)
    (background-color default background))
  "List of rules to use when frobbing faces based on frame parameters.
This is only necessary for XEmacs, because in Emacs 21 changing the
frame paramters automatically affects the relevant faces.")

(defun color-theme-frob-faces (params)
  "Change certain faces according to PARAMS.
This uses `color-theme-frame-param-frobbing-rules'."
  (dolist (rule color-theme-frame-param-frobbing-rules)
    (let* ((param (nth 0 rule))
	   (face (nth 1 rule))
	   (prop (nth 2 rule))
	   (val (cdr (assq param params)))
	   (frame (if color-theme-is-global nil (selected-frame))))
      (when val
	(set-face-property face prop val frame)))))

(defun color-theme-alist-reduce (old-list)
  "Reduce OLD-LIST.
The resulting list will be newly allocated and will not contain any elements
with duplicate cars.  This will speed the installation of new themes by
only installing unique attributes."
  (let (new-list)
    (dolist (elem old-list)
      (when (not (assq (car elem) new-list))
	(setq new-list (cons elem new-list))))
    new-list))

(defun color-theme-install-frame-params (params)
  "Change frame parameters using alist PARAMETERS.

If `color-theme-is-global' is non-nil, all frames are modified using
`modify-frame-parameters' and the PARAMETERS are prepended to
`default-frame-alist'.  The value of `initial-frame-alist' is not
modified.  If `color-theme-is-global' is nil, only the selected frame is
modified.  If `color-theme-is-cumulative' is nil, the frame parameters
are restored from `color-theme-original-frame-alist'.

If the current frame parameters have a parameter `minibuffer' with
value `only', then the frame parameters are not installed, since this
indicates a dedicated minibuffer frame.

Called from `color-theme-install'."
  (setq params (color-theme-filter
		params color-theme-legal-frame-parameters))
  ;; We have a new list in params now, therefore we may use
  ;; destructive nconc.
  (if color-theme-is-global
      (let ((frames (frame-list)))
	(if (or color-theme-is-cumulative
		(null color-theme-original-frame-alist))
	    (setq default-frame-alist
		  (append params (color-theme-alist default-frame-alist))
		  minibuffer-frame-alist
		  (append params (color-theme-alist minibuffer-frame-alist)))
	  (setq default-frame-alist
		(append params color-theme-original-frame-alist)
		minibuffer-frame-alist
		(append params (color-theme-alist minibuffer-frame-alist))))
	(setq default-frame-alist
	      (color-theme-alist-reduce default-frame-alist)
	      minibuffer-frame-alist
	      (color-theme-alist-reduce minibuffer-frame-alist))
	(dolist (frame frames)
	  (let ((params (if (eq 'only (cdr (assq 'minibuffer (frame-parameters frame))))
			    minibuffer-frame-alist
			  default-frame-alist)))
	    (condition-case var
		(modify-frame-parameters frame params)
	      (error (message "Error using params %S: %S" params var))))))
    (condition-case var
	(modify-frame-parameters (selected-frame) params)
      (error (message "Error using params %S: %S" params var))))
  (when color-theme-xemacs-p
    (color-theme-frob-faces params)))

;; (setq default-frame-alist (cons '(height . 30) default-frame-alist))

(defun color-theme-install-variables (vars)
  "Change variables using alist VARS.
All variables matching `color-theme-legal-variables' are set.

If `color-theme-is-global' and `color-theme-xemacs-p' are nil, variables
are made frame-local before setting them.  Variables are set using `set'
in either case.  This may lead to problems if changing the variable
requires the usage of the function specified with the :set tag in
defcustom declarations.

Called from `color-theme-install'."
  (let ((vars (color-theme-filter vars color-theme-legal-variables)))
    (dolist (var vars)
      (if (or color-theme-is-global color-theme-xemacs-p)
	  (set (car var) (cdr var))
	(make-variable-frame-local (car var))
	(modify-frame-parameters (selected-frame) (list var))))))

(defun color-theme-install-faces (faces)
  "Change faces using FACES.

Change faces for all frames and create any faces listed in FACES which
don't exist.  The modified faces will be marked as \"unchanged from
its standard setting\".  This is OK, since the changes made by
installing a color theme should never by saved in .emacs by
customization code.

FACES should be a list where each entry has the form:

  (FACE SPEC)

See `defface' for the format of SPEC.

If `color-theme-is-global' is non-nil, faces are modified on all frames
using `face-spec-set'.  If `color-theme-is-global' is nil, faces are
only modified on the selected frame.  Non-existing faces are created
using `make-empty-face' in either case.  If `color-theme-is-cumulative'
is nil, all faces are reset before installing the new faces.

Called from `color-theme-install'."
  ;; clear all previous faces
  (when (not color-theme-is-cumulative)
    (color-theme-reset-faces))
  ;; install new faces
  (let ((faces (color-theme-filter faces color-theme-illegal-faces t))
	(frame (if color-theme-is-global nil (selected-frame))))
    (dolist (entry faces)
      (let ((face (nth 0 entry))
	    (spec (nth 1 entry)))
	(or (facep face)
	    (make-empty-face face))
	;; remove weird properties from the default face only
	(when (eq face 'default)
	  (setq spec (color-theme-spec-filter spec)))
	;; Emacs/XEmacs customization issues: filter out :bold when
	;; the spec contains :weight, etc, such that the spec remains
	;; "valid" for custom.
	(setq spec (color-theme-spec-compat spec))
	;; using a spec of ((t (nil))) to reset a face doesn't work
	;; in Emacs 21, we use the new function face-spec-reset-face
	;; instead
	(if (and (functionp 'face-spec-reset-face)
		 (equal spec '((t (nil)))))
	    (face-spec-reset-face face frame)
	  (condition-case var
	      (progn
		(face-spec-set face spec frame)
		(if color-theme-is-global
		    (put face 'face-defface-spec spec)))
	    (error (message "Error using spec %S: %S" spec var))))))))

;; `custom-set-faces' is unusable here because it doesn't allow to set
;; the faces for one frame only.

;; Emacs `face-spec-set': If FRAME is nil, the face is created and
;; marked as a customized face.  This is achieved by setting the
;; `face-defface-spec' property.  If we don't, new frames will not be
;; created using the face we installed because `face-spec-set' is
;; broken: If given a FRAME of nil, it will not set the default faces;
;; instead it will walk through all the frames and set modify the faces.
;; If we do set a property (`saved-face' or `face-defface-spec'),
;; `make-frame' will correctly use the faces we defined with our color
;; theme.  If we used the property `saved-face',
;; `customize-save-customized' will save all the faces installed as part
;; of a color-theme in .emacs.  That's why we use the
;; `face-defface-spec' property.



;;; Theme accessor functions, canonicalization, merging, comparing

(defun color-theme-canonic (theme)
  "Return the canonic form of THEME.
This deals with all the backwards compatibility stuff."
  (let (function frame-params variables faces)
    (when (functionp (car theme))
      (setq function (car theme)
	    theme (cdr theme)))
    (setq frame-params (car theme)
	  theme (cdr theme))
    ;; optional variable defintions (for backwards compatibility)
    (when (listp (caar theme))
      (setq variables (car theme)
	    theme (cdr theme)))
    ;; face definitions
    (setq faces theme)
    (list function frame-params variables faces)))

(defun color-theme-function (theme)
  "Return function used to create THEME."
  (nth 0 theme))

(defun color-theme-frame-params (theme)
  "Return frame-parameters defined by THEME."
  (nth 1 theme))

(defun color-theme-variables (theme)
  "Return variables set by THEME."
  (nth 2 theme))

(defun color-theme-faces (theme)
  "Return faces defined by THEME."
  (nth 3 theme))

(defun color-theme-merge-alists (&rest alists)
  "Merges all the alist arguments into one alist.
Only the first instance of every key will be part of the resulting
alist.  Membership will be tested using `assq'."
  (let (result)
    (dolist (l alists)
      (dolist (entry l)
	(unless (assq (car entry) result)
	  (setq result (cons entry result)))))
    (nreverse result)))
;; (color-theme-merge-alists '((a . 1) (b . 2)))
;; (color-theme-merge-alists '((a . 1) (b . 2) (a . 3)))
;; (color-theme-merge-alists '((a . 1) (b . 2)) '((a . 3)))
;; (color-theme-merge-alists '((a . 1) (b . 2)) '((c . 3)))
;; (color-theme-merge-alists '((a . 1) (b . 2)) '((c . 3) (d . 4)))
;; (color-theme-merge-alists '((a . 1) (b . 2)) '((c . 3) (d . 4) (b . 5)))

(defun color-theme-compare (theme-a theme-b)
  "Compare two color themes.
This will print the differences between installing THEME-A and
installing THEME-B.  Note that the order is important: If a face is
defined in THEME-A and not in THEME-B, then this will not show up as a
difference, because there is no reset before installing THEME-B.  If a
face is defined in THEME-B and not in THEME-A, then this will show up as
a difference."
  (interactive
   (list
    (intern
     (completing-read "Theme A: "
		      (mapcar (lambda (i) (list (symbol-name (car i))))
			      color-themes)
		      (lambda (i) (string-match "color-theme" (car i)))))
    (intern
     (completing-read "Theme B: "
		      (mapcar (lambda (i) (list (symbol-name (car i))))
			      color-themes)
		      (lambda (i) (string-match "color-theme" (car i)))))))
  ;; install the themes in a new frame and get the definitions
  (let ((color-theme-is-global nil))
    (select-frame (make-frame))
    (funcall theme-a)
    (setq theme-a (list theme-a
			(color-theme-get-params)
			(color-theme-get-vars)
			(color-theme-get-face-definitions)))
    (funcall theme-b)
    (setq theme-b (list theme-b
			(color-theme-get-params)
			(color-theme-get-vars)
			(color-theme-get-face-definitions)))
    (delete-frame))
  (let ((params (set-difference
		 (color-theme-frame-params theme-b)
		 (color-theme-frame-params theme-a)
		 :test 'equal))
	(vars (set-difference
	       (color-theme-variables theme-b)
	       (color-theme-variables theme-a)
	       :test 'equal))
	(faces (set-difference
		(color-theme-faces theme-b)
		(color-theme-faces theme-a)
		:test 'equal)))
    (list 'diff
	  params
	  vars
	  faces)))



;;; Installing a color theme

(defun color-theme-install (theme)
  "Install a color theme defined by frame parameters, variables and faces.

The theme is installed for all present and future frames; any missing
faces are created.  See `color-theme-install-faces'.

THEME is a color theme definition.  See below for more information.

If you want to install a color theme from your .emacs, use the output
generated by `color-theme-print'.  This produces color theme function
which you can copy to your .emacs.

A color theme definition is a list:
\([FUNCTION] FRAME-PARAMETERS VARIABLE-SETTINGS FACE-DEFINITIONS)

FUNCTION is the color theme function which called `color-theme-install'.
This is no longer used.  There was a time when this package supported
automatic factoring of color themes.  This has been abandoned.

FRAME-PARAMETERS is an alist of frame parameters.  These are installed
with `color-theme-install-frame-params'.  These are installed last such
that any changes to the default face can be changed by the frame
parameters.

VARIABLE-DEFINITIONS is an alist of variable settings.  These are
installed with `color-theme-install-variables'.

FACE-DEFINITIONS is an alist of face definitions.  These are installed
with `color-theme-install-faces'.

If `color-theme-is-cumulative' is nil, a color theme will undo face and
frame-parameter settings of previous color themes."
  (setq theme (color-theme-canonic theme))
  (color-theme-install-variables (color-theme-variables theme))
  (color-theme-install-faces (color-theme-faces theme))
  ;; frame parameters override faces
  (color-theme-install-frame-params (color-theme-frame-params theme))
  (when color-theme-history-max-length
    (color-theme-add-to-history
     (car theme))))




(provide 'color-theme)

;;; color-theme.el ends here

(defun color-theme-standard ()
  "Emacs default colors.
If you are missing standard faces in this theme, please notify the maintainer."
  (interactive)
  ;; Note that some of the things that make up a color theme are
  ;; actually variable settings!
  (color-theme-install
   '(color-theme-standard
     ((foreground-color . "black")
      (background-color . "white")
      (mouse-color . "black")
      (cursor-color . "black")
      (border-color . "black")
      (background-mode . light))
     ((Man-overstrike-face . bold)
      (Man-underline-face . underline)
      (apropos-keybinding-face . underline)
      (apropos-label-face . italic)
      (apropos-match-face . secondary-selection)
      (apropos-property-face . bold-italic)
      (apropos-symbol-face . bold)
      (goto-address-mail-face . italic)
      (goto-address-mail-mouse-face . secondary-selection)
      (goto-address-url-face . bold)
      (goto-address-url-mouse-face . highlight)
      (help-highlight-face . underline)
      (list-matching-lines-face . bold)
      (view-highlight-face . highlight))
     (default ((t (nil))))
     (bold ((t (:bold t))))
     (bold-italic ((t (:bold t :italic t))))
     (calendar-today-face ((t (:underline t))))
     (cperl-array-face ((t (:foreground "Blue" :background "lightyellow2" :bold t))))
     (cperl-hash-face ((t (:foreground "Red" :background "lightyellow2" :bold t :italic t))))
     (cperl-nonoverridable-face ((t (:foreground "chartreuse3"))))
     (custom-button-face ((t (nil))))
     (custom-changed-face ((t (:foreground "white" :background "blue"))))
     (custom-documentation-face ((t (nil))))
     (custom-face-tag-face ((t (:underline t))))
     (custom-group-tag-face ((t (:foreground "blue" :underline t))))
     (custom-group-tag-face-1 ((t (:foreground "red" :underline t))))
     (custom-invalid-face ((t (:foreground "yellow" :background "red"))))
     (custom-modified-face ((t (:foreground "white" :background "blue"))))
     (custom-rogue-face ((t (:foreground "pink" :background "black"))))
     (custom-saved-face ((t (:underline t))))
     (custom-set-face ((t (:foreground "blue" :background "white"))))
     (custom-state-face ((t (:foreground "dark green"))))
     (custom-variable-button-face ((t (:bold t :underline t))))
     (custom-variable-tag-face ((t (:foreground "blue" :underline t))))
     (diary-face ((t (:foreground "red"))))
     (ediff-current-diff-face-A ((t (:foreground "firebrick" :background "pale green"))))
     (ediff-current-diff-face-Ancestor ((t (:foreground "Black" :background "VioletRed"))))
     (ediff-current-diff-face-B ((t (:foreground "DarkOrchid" :background "Yellow"))))
     (ediff-current-diff-face-C ((t (:foreground "Navy" :background "Pink"))))
     (ediff-even-diff-face-A ((t (:foreground "Black" :background "light grey"))))
     (ediff-even-diff-face-Ancestor ((t (:foreground "White" :background "Grey"))))
     (ediff-even-diff-face-B ((t (:foreground "White" :background "Grey"))))
     (ediff-even-diff-face-C ((t (:foreground "Black" :background "light grey"))))
     (ediff-fine-diff-face-A ((t (:foreground "Navy" :background "sky blue"))))
     (ediff-fine-diff-face-Ancestor ((t (:foreground "Black" :background "Green"))))
     (ediff-fine-diff-face-B ((t (:foreground "Black" :background "cyan"))))
     (ediff-fine-diff-face-C ((t (:foreground "Black" :background "Turquoise"))))
     (ediff-odd-diff-face-A ((t (:foreground "White" :background "Grey"))))
     (ediff-odd-diff-face-Ancestor ((t (:foreground "Black" :background "light grey"))))
     (ediff-odd-diff-face-B ((t (:foreground "Black" :background "light grey"))))
     (ediff-odd-diff-face-C ((t (:foreground "White" :background "Grey"))))
     (eshell-ls-archive-face ((t (:foreground "Orchid" :bold t))))
     (eshell-ls-backup-face ((t (:foreground "OrangeRed"))))
     (eshell-ls-clutter-face ((t (:foreground "OrangeRed" :bold t))))
     (eshell-ls-directory-face ((t (:foreground "Blue" :bold t))))
     (eshell-ls-executable-face ((t (:foreground "ForestGreen" :bold t))))
     (eshell-ls-missing-face ((t (:foreground "Red" :bold t))))
     (eshell-ls-product-face ((t (:foreground "OrangeRed"))))
     (eshell-ls-readonly-face ((t (:foreground "Brown"))))
     (eshell-ls-special-face ((t (:foreground "Magenta" :bold t))))
     (eshell-ls-symlink-face ((t (:foreground "DarkCyan" :bold t))))
     (eshell-ls-unreadable-face ((t (:foreground "Grey30"))))
     (eshell-prompt-face ((t (:foreground "Red" :bold t))))
     (eshell-test-failed-face ((t (:foreground "OrangeRed" :bold t))))
     (eshell-test-ok-face ((t (:foreground "Green" :bold t))))
     (minibuffer-prompt ((t (:foreground "Blue"))))
     (ido-first-match ((t (:foreground "Black" :bold t))))
     (ido-only-match ((t (:foreground "Black"))))
     (excerpt ((t (:italic t))))
     (fixed ((t (:bold t))))
     (flyspell-duplicate-face ((t (:foreground "Gold3" :bold t :underline t))))
     (flyspell-incorrect-face ((t (:foreground "OrangeRed" :bold t :underline t))))
     (font-lock-builtin-face ((t (:foreground "Orchid"))))
     (font-lock-comment-face ((t (:foreground "Firebrick"))))
     (font-lock-constant-face ((t (:foreground "CadetBlue"))))
     (font-lock-function-name-face ((t (:foreground "Blue"))))
     (font-lock-keyword-face ((t (:foreground "Purple"))))
     (font-lock-string-face ((t (:foreground "RosyBrown"))))
     (font-lock-type-face ((t (:foreground "ForestGreen"))))
     (font-lock-variable-name-face ((t (:foreground "DarkGoldenrod"))))
     (font-lock-warning-face ((t (:foreground "Red" :bold t))))
     (fringe ((t (:background "grey95"))))
     (gnus-cite-attribution-face ((t (:italic t))))
     (gnus-cite-face-1 ((t (:foreground "MidnightBlue"))))
     (gnus-cite-face-10 ((t (:foreground "medium purple"))))
     (gnus-cite-face-11 ((t (:foreground "turquoise"))))
     (gnus-cite-face-2 ((t (:foreground "firebrick"))))
     (gnus-cite-face-3 ((t (:foreground "dark green"))))
     (gnus-cite-face-4 ((t (:foreground "OrangeRed"))))
     (gnus-cite-face-5 ((t (:foreground "dark khaki"))))
     (gnus-cite-face-6 ((t (:foreground "dark violet"))))
     (gnus-cite-face-7 ((t (:foreground "SteelBlue4"))))
     (gnus-cite-face-8 ((t (:foreground "magenta"))))
     (gnus-cite-face-9 ((t (:foreground "violet"))))
     (gnus-emphasis-bold ((t (:bold t))))
     (gnus-emphasis-bold-italic ((t (:bold t :italic t))))
     (gnus-emphasis-italic ((t (:italic t))))
     (gnus-emphasis-underline ((t (:underline t))))
     (gnus-emphasis-underline-bold ((t (:bold t :underline t))))
     (gnus-emphasis-underline-bold-italic ((t (:bold t :italic t :underline t))))
     (gnus-emphasis-underline-italic ((t (:italic t :underline t))))
     (gnus-group-mail-1-empty-face ((t (:foreground "DeepPink3"))))
     (gnus-group-mail-1-face ((t (:foreground "DeepPink3" :bold t))))
     (gnus-group-mail-2-empty-face ((t (:foreground "HotPink3"))))
     (gnus-group-mail-2-face ((t (:foreground "HotPink3" :bold t))))
     (gnus-group-mail-3-empty-face ((t (:foreground "magenta4"))))
     (gnus-group-mail-3-face ((t (:foreground "magenta4" :bold t))))
     (gnus-group-mail-low-empty-face ((t (:foreground "DeepPink4"))))
     (gnus-group-mail-low-face ((t (:foreground "DeepPink4" :bold t))))
     (gnus-group-news-1-empty-face ((t (:foreground "ForestGreen"))))
     (gnus-group-news-1-face ((t (:foreground "ForestGreen" :bold t))))
     (gnus-group-news-2-empty-face ((t (:foreground "CadetBlue4"))))
     (gnus-group-news-2-face ((t (:foreground "CadetBlue4" :bold t))))
     (gnus-group-news-3-empty-face ((t (nil))))
     (gnus-group-news-3-face ((t (:bold t))))
     (gnus-group-news-low-empty-face ((t (:foreground "DarkGreen"))))
     (gnus-group-news-low-face ((t (:foreground "DarkGreen" :bold t))))
     (gnus-header-content-face ((t (:foreground "indianred4" :italic t))))
     (gnus-header-from-face ((t (:foreground "red3"))))
     (gnus-header-name-face ((t (:foreground "maroon"))))
     (gnus-header-newsgroups-face ((t (:foreground "MidnightBlue" :italic t))))
     (gnus-header-subject-face ((t (:foreground "red4"))))
     (gnus-signature-face ((t (:italic t))))
     (gnus-splash-face ((t (:foreground "ForestGreen"))))
     (gnus-summary-cancelled-face ((t (:foreground "yellow" :background "black"))))
     (gnus-summary-high-ancient-face ((t (:foreground "RoyalBlue" :bold t))))
     (gnus-summary-high-read-face ((t (:foreground "DarkGreen" :bold t))))
     (gnus-summary-high-ticked-face ((t (:foreground "firebrick" :bold t))))
     (gnus-summary-high-unread-face ((t (:bold t))))
     (gnus-summary-low-ancient-face ((t (:foreground "RoyalBlue" :italic t))))
     (gnus-summary-low-read-face ((t (:foreground "DarkGreen" :italic t))))
     (gnus-summary-low-ticked-face ((t (:foreground "firebrick" :italic t))))
     (gnus-summary-low-unread-face ((t (:italic t))))
     (gnus-summary-normal-ancient-face ((t (:foreground "RoyalBlue"))))
     (gnus-summary-normal-read-face ((t (:foreground "DarkGreen"))))
     (gnus-summary-normal-ticked-face ((t (:foreground "firebrick"))))
     (gnus-summary-normal-unread-face ((t (nil))))
     (gnus-summary-selected-face ((t (:underline t))))
     (highlight ((t (:background "darkseagreen2"))))
     (highlight-changes-delete-face ((t (:foreground "red" :underline t))))
     (highlight-changes-face ((t (:foreground "red"))))
     (highline-face ((t (:background "paleturquoise"))))
     (holiday-face ((t (:background "pink"))))
     (info-menu-5 ((t (:underline t))))
     (info-node ((t (:bold t :italic t))))
     (info-xref ((t (:bold t))))
     (italic ((t (:italic t))))
     (makefile-space-face ((t (:background "hotpink"))))
     (message-cited-text-face ((t (:foreground "red"))))
     (message-header-cc-face ((t (:foreground "MidnightBlue"))))
     (message-header-name-face ((t (:foreground "cornflower blue"))))
     (message-header-newsgroups-face ((t (:foreground "blue4" :bold t :italic t))))
     (message-header-other-face ((t (:foreground "steel blue"))))
     (message-header-subject-face ((t (:foreground "navy blue" :bold t))))
     (message-header-to-face ((t (:foreground "MidnightBlue" :bold t))))
     (message-header-xheader-face ((t (:foreground "blue"))))
     (message-separator-face ((t (:foreground "brown"))))
     (mode-line ((t (:foreground "Black" :background "#dddddd"
                                 :box (:color "#888888" :line-width 1)))))
     (mode-line-inactive ((t (:background "#aaaaaa" :foreground "Black"
                                          :box (:color "#888888" :line-width 1)))))
     (mode-line-buffer-id ((t (:foreground "Black"))))
     (mode-line-mousable ((t (:foreground "white" :background "black"))))
     (mode-line-mousable-minor-mode ((t (:foreground "white" :background "black"))))
     (region ((t (:background "gray"))))
     (secondary-selection ((t (:background "paleturquoise"))))
     (show-paren-match-face ((t (:background "turquoise"))))
     (show-paren-mismatch-face ((t (:foreground "white" :background "purple"))))
     (speedbar-button-face ((t (:foreground "green4"))))
     (speedbar-directory-face ((t (:foreground "blue4"))))
     (speedbar-file-face ((t (:foreground "cyan4"))))
     (speedbar-highlight-face ((t (:background "green"))))
     (speedbar-selected-face ((t (:foreground "red" :underline t))))
     (speedbar-tag-face ((t (:foreground "brown"))))
     (term-black ((t (:foreground "black"))))
     (term-blackbg ((t (:background "black"))))
     (term-blue ((t (:foreground "blue"))))
     (term-bluebg ((t (:background "blue"))))
     (term-bold ((t (:bold t))))
     (term-cyan ((t (:foreground "cyan"))))
     (term-cyanbg ((t (:background "cyan"))))
     (term-default-bg ((t (nil))))
     (term-default-bg-inv ((t (nil))))
     (term-default-fg ((t (nil))))
     (term-default-fg-inv ((t (nil))))
     (term-green ((t (:foreground "green"))))
     (term-greenbg ((t (:background "green"))))
     (term-invisible ((t (nil))))
     (term-invisible-inv ((t (nil))))
     (term-magenta ((t (:foreground "magenta"))))
     (term-magentabg ((t (:background "magenta"))))
     (term-red ((t (:foreground "red"))))
     (term-redbg ((t (:background "red"))))
     (term-underline ((t (:underline t))))
     (term-white ((t (:foreground "white"))))
     (term-whitebg ((t (:background "white"))))
     (term-yellow ((t (:foreground "yellow"))))
     (term-yellowbg ((t (:background "yellow"))))
     (underline ((t (:underline t))))
     (vcursor ((t (:foreground "blue" :background "cyan" :underline t))))
     (vhdl-font-lock-attribute-face ((t (:foreground "Orchid"))))
     (vhdl-font-lock-directive-face ((t (:foreground "CadetBlue"))))
     (vhdl-font-lock-enumvalue-face ((t (:foreground "Gold4"))))
     (vhdl-font-lock-function-face ((t (:foreground "Orchid4"))))
     (vhdl-font-lock-prompt-face ((t (:foreground "Red" :bold t))))
     (vhdl-font-lock-reserved-words-face ((t (:foreground "Orange" :bold t))))
     (vhdl-font-lock-translate-off-face ((t (:background "LightGray"))))
     (vhdl-speedbar-architecture-face ((t (:foreground "Blue"))))
     (vhdl-speedbar-architecture-selected-face ((t (:foreground "Blue" :underline t))))
     (vhdl-speedbar-configuration-face ((t (:foreground "DarkGoldenrod"))))
     (vhdl-speedbar-configuration-selected-face ((t (:foreground "DarkGoldenrod" :underline t))))
     (vhdl-speedbar-entity-face ((t (:foreground "ForestGreen"))))
     (vhdl-speedbar-entity-selected-face ((t (:foreground "ForestGreen" :underline t))))
     (vhdl-speedbar-instantiation-face ((t (:foreground "Brown"))))
     (vhdl-speedbar-instantiation-selected-face ((t (:foreground "Brown" :underline t))))
     (vhdl-speedbar-package-face ((t (:foreground "Grey50"))))
     (vhdl-speedbar-package-selected-face ((t (:foreground "Grey50" :underline t))))
     (viper-minibuffer-emacs-face ((t (:foreground "Black" :background "darkseagreen2"))))
     (viper-minibuffer-insert-face ((t (:foreground "Black" :background "pink"))))
     (viper-minibuffer-vi-face ((t (:foreground "DarkGreen" :background "grey"))))
     (viper-replace-overlay-face ((t (:foreground "Black" :background "darkseagreen2"))))
     (viper-search-face ((t (:foreground "Black" :background "khaki"))))
     (widget-button-face ((t (:bold t))))
     (widget-button-pressed-face ((t (:foreground "red"))))
     (widget-documentation-face ((t (:foreground "dark green"))))
     (widget-field-face ((t (:background "gray85"))))
     (widget-inactive-face ((t (:foreground "dim gray"))))
     (widget-single-line-field-face ((t (:background "gray85")))))))
