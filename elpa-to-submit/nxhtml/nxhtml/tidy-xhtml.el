;;; tidy-xhtml.el --- Interface to the HTML Tidy program

;; Copyright (C) 2001, 2002, 2003, 2006, 2007 by Free Software
;; Foundation, Inc.

;; Emacs Lisp Archive Entry
;; Ancestors filename: tidy.el
;; Author: Kahlil (Kal) HODGSON <dorge@tpg.com.au>
;;     and Lennart Borgman
;;         <lennart dot borgman dot 073 dot student dot lu dot se>
;; Original X-URL: http://www.emacswiki.org/elisp/tidy.el
;; Last-Updated: 2008-03-09T13:10:06+0100 Sun
(defconst tidy-xhtml:version "2.25")
;; Keywords: languages

;; This file is NOT part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;;; Commentary:

;; Provides a simple interface to the HTML Tidy program -- a free
;; utility that can fix common errors in your mark-up and clean up
;; sloppy editing automatically. See
;;
;;       <http://tidy.sourceforge.net/>
;;
;; for more details.  This package provides the following functions:
;;
;;       `tidy-buffer',
;;       `tidy-region',
;;       `tidy-tree',
;;       `tidy-html-site',
;;       `tidy-parse-config-file',
;;       `tidy-save-settings',
;;       `tidy-describe-options',
;;       `tidy-show-xhtml-options',
;;       `tidy-set-xhtml-options',
;;
;; These can be invoked interactively (using M-x) or via the menu-bar.
;; The function `tidy-buffer' sends the current buffer to HTML Tidy,
;; replacing the existing contents with a "tidied" version.  If
;; `tidy-buffer' is given a prefix argument, tidy operates on the
;; current region, ignoring mark-up outside <BODY>...</BODY> tags
;; (useful for writhing cgi scripts in Pearl).  Warnings and errors
;; are presented in a compilation buffer to facilitate tracking down
;; necessary changes (e.g. C-x ` is bound to `next-error').
;;
;; This package also provides menu-bar support for setting Tidy's many
;; options, and includes support for Tidy configuration files.  The
;; function `tidy-parse-config-file' will synchronise options
;; displayed in the menu-bar with the settings in `tidy-config-file'.
;; This is normally called by the load-hook for your HTML editing mode
;; (see installation instructions below).  The function
;; `tidy-save-settings' will save the current option settings to your
;; `tidy-config-file'.  Finally `tidy-describe-options' allows you to
;; browse the documentation strings associated with each option.

;;;

;;;; Installation:

;; This package assumes you have and up-to-date HTML Tidy program
;; installed on your system.  See the URL above for instructions on
;; how to do this.  To set up this support package, first place the
;; "tidy.el" file somewhere in your `load-path' and open it in Emacs.
;; Byte-compile and load this package using the command
;;
;; M-x emacs-lisp-byte-compile-and-load <RET>
;;
;; Next customise the variables `tidy-config-file', `tidy-temp-dir'
;; `tidy-shell-program', `tidy-menu-lock' and `tidy-menu-x-position'
;;
;; M-x customize-group <RET> tidy <RET>
;;
;; Now add the following autoloads to your ".emacs.el" file:
;;
;; (autoload 'tidy-buffer "tidy" "Run Tidy HTML parser on current buffer" t)
;; (autoload 'tidy-parse-config-file "tidy" "Parse the `tidy-config-file'" t)
;; (autoload 'tidy-save-settings "tidy" "Save settings to `tidy-config-file'" t)
;; (autoload 'tidy-build-menu  "tidy" "Install an options menu for HTML Tidy." t)
;;
;; If you use html-mode to edit HTML files then add something like
;; this as well

;; (defun my-html-mode-hook () "Customize my html-mode."
;;   (tidy-build-menu html-mode-map)
;;   (local-set-key [(control c) (control c)] 'tidy-buffer)
;;   (setq sgml-validate-command "tidy"))
;;
;; (add-hook 'html-mode-hook 'my-html-mode-hook)

;; This will set up a "tidy" menu in the menu bar and bind the key
;; sequence "C-c C-c" to `tidy-buffer' in html-mode (normally bound to
;; `validate-buffer').
;;
;; For other modes (like html-helper-mode) simple change the variables
;; `html-mode-hook' and `html-mode-map' to whatever is appropriate e.g.

;; (defun my-html-mode-hook () "Customize my html-helper-mode."
;;   (tidy-build-menu html-helper-mode-map)
;;   (local-set-key [(control c) (control c)] 'tidy-buffer)
;;   (setq sgml-validate-command "tidy"))
;;
;; (add-hook 'html-helper-mode-hook 'my-html-mode-hook)

;; Finally, restart Emacs and open an HTML file to test-drive the tidy
;; package. For people new to HTML tidy check that the option "markup"
;; under the "Input/Output" sub menu is set. You can read the
;; documentation on this option via the menu item "Describe Options".
;;
;; Enjoy!

;;;; New Features:
;;
;; 0. Now compatible with CVS version of Tidy as at 22 May 2003
;; 1. Improved menu support to facillitate incorporting new options
;; 2. Menu lock option makes menu stick when toggling options.
;; 3. Now runs on XEmacs!!
;; 4. Uses error file rather than std-error to retrieve errors (this
;;    fixes some odd pop up behaviour)
;; 5. minor bug fix (empty config files)
;; 6. handle buffer modified query in error buffer better
;; 7. make it impossible to mark the error buffer as modified
;; 8. Added the variable `tidy-temp-directory'.
;; 9. Bugfix in tidy-buffer: call find-file-noselect with NOWARN
;; 10. Removes ^M on w32.
;; 11. Changed defcustom types to 'file and 'directory.
;; 12. Added `tidy-set-xhtml-options'.
;; 13. Tried to handle encodings.
;; 14. Added the function `tidy-region'.
;; 15. Added ediff support.
;; 16. Added `tidy-tree'.
;; 17. Added `tidy-html-site'.

;;;; ToDo:
;;
;; 1. Automatically set "char-encoding" according to the buffer encoding
;; 2. Should check value of HTML_TIDY environment variable.


;;;; Bugs:

;; Requires a version of HTML Tidy that understands the "-f"
;; "-config" "--show-body-only" command line options e.g. source-forge
;; pre-release.
;;
;; There may be a bug with setting doctypes.  I don't use this feature
;; yet and, well, don't really know how its supposed to work:-)
;;
;; Care with character encodings!!


;;; History:

;; 2006-05-09: New features 10-17 above.
;;             - Lennart Borgman
;; 2006-05-24: Fixed some errors spotted by Andreas Roethler.


;;;; Credits

;; This code was inspired by an Emacs "tip" suggested by Pete Gelbman.
;;
;; Thanks to Hans-Michael Stahl for comments regarding XEmacs
;; compatibility.
;;
;; Thanks to Thomas Baumann for bugfix's in `tidy-parse-config-file'
;; and `tidy-buffer'.
;;
;; Thanks to Chris Lott for comments regarding installation and menu
;; display
;;
;; Thanks to Jeroen Baekelandt for noting a problem with ange-ftp and
;; inspiring `tidy-temp-directory'.

;;;; Code:

;;;;; Forward references (stuff which must come first)

(eval-when-compile (require 'cl))
(eval-when-compile (require 'ediff))
(eval-when-compile (require 'mumamo nil t))
(eval-when-compile
  (add-to-list 'load-path default-directory))
(eval-when-compile (require 'html-site))
(require 'easymenu) ;; This makes menus so much easier!
(require 'compile)  ;; To make the error buffer more sexy
(require 'cus-edit) ;; Just for face custom-button
(require 'help-mode)

;; The following two are functions so that the same compiled code will
;; work in both situations (time cost is negligible)

(defsubst tidy-xemacs-p ()
  "Return t iff we are running XEmacs this session."
  (not (null (string-match "^XEmacs.*" (emacs-version)))))

(defsubst tidy-windows-p ()
  "Return t iff we are running on a Windows system."
  (memq system-type '(emx win32 w32 mswindows ms-dos windows-nt)))

;; function definitions

;; XEmacs
(defalias 'tidy-x-event-function          'event-function)
(defalias 'tidy-x-event-object            'event-object)
(defalias 'tidy-x-find-menu-item          'find-menu-item)
(defalias 'tidy-x-get-popup-menu-response 'get-popup-menu-response)
(defalias 'tidy-x-make-event              'make-event)
(defalias 'tidy-x-misc-user-event-p       'misc-user-event-p)

;;;;; User Variables

(defgroup tidy nil
  "Provides a simple interface to the HTML Tidy program -- a free
utility that can fix common errors in your mark-up and clean up
sloppy editing automatically. See

      <http://tidy.sourceforge.net/>

for more details.  This package provides the following functions:

      `tidy-buffer',
      `tidy-parse-config-file',
      `tidy-save-settings', and
      `tidy-describe-options',

These can be invoked interactively (using M-x) or via the menu-bar.
The function `tidy-buffer' sends the current buffer to HTML Tidy,
replacing the existing contents with a \"tidied\" version.  If
`tidy-buffer' is given a prefix argument, tidy operates on the
current region, ignoring mark-up outside <BODY>...</BODY> tags
\(useful for writhing cgi scripts in Pearl).  Warnings and errors
are presented in a compilation buffer to facilitate tracking down
necessary changes (e.g. C-x ` is bound to `next-error').

This package also provides menu-bar support for setting Tidy's many
options, and includes support for Tidy configuration files.  The
function `tidy-parse-config-file' will synchronise options
displayed in the menu-bar with the settings in `tidy-config-file'.
This is normally called by the load-hook for your HTML editing mode
\(see installation instructions below).  The function
`tidy-save-settings' will save the current option settings to your
`tidy-config-file'.  Finally `tidy-describe-options' allows you to
browse the documentation strings associated with each option.
"
  :group 'nxhtml
  :group 'hypermedia)

;; (defcustom tidy-use-ediff nil
;;   "If non-nil call ediff in `tidy-buffer' instead of replacing."
;;   :group 'tidy
;;   :type 'boolean)

(defvar tidy-warnings 0)
(defvar tidy-errors 0)
(defvar tidy-message nil)

;;(defvar tidy-batch-buffer nil)
(defvar tidy-batch-last-file nil)

(defvar tidy-default-config-file "~/.tidyrc")

(defvar tidy-config-file-parsed nil)

(defcustom tidy-config-file tidy-default-config-file
  "Path to your default tidy configuration file.

This is used by `tidy-parse-config-file' to synchronise Tidy's behaviour
inside Emacs with its behaviour outside, and by `tidy-save-settings' to
set your configuration file from within Emacs.  If you never want this to
happen, set `tidy-config-file' to \"\"."
  :group 'tidy
  :type 'file
  :set (lambda (symbol value)
         (set-default symbol value)
         (if (file-readable-p value)
             ;; Just set the default values here:
             ;;(tidy-parse-config-file)
             ;; Just tell we need to parse:
             (setq tidy-config-file-parsed nil)
           (if (file-exists-p value)
               (lwarn '(tidy-config-file)
                      :warning "Tidy config file not readable: %s" value)
             (unless (string= value tidy-default-config-file)
               (lwarn '(tidy-config-file)
                      :warning "Tidy config file not found: %s" value))))))


(defcustom tidy-shell-program "tidy"
  "The HTML program."
  :group 'tidy
  :type '(choice (file :must-match t)
                 (string :tag "File name (searched for in path): "))
  :set (lambda (symbol value)
         (set-default symbol value)
         (unless (string= value "tidy")
           (or (file-executable-p value)
               (executable-find value)
               (lwarn '(tidy-shell-program)
                      :error "Tidy program not found: %s" value)))))

(defcustom tidy-temp-directory temporary-file-directory
  "Directory where tidy places its temp files.  The default is the
current directory which works fine unless you are operating on remote
files via `ange-ftp' and its ilk, in which case it will try to place
the temp files on the remote server (and will probably fail).  If this
is the case try setting this variable to something like \"/tmp/\" or
\"/var/tmp/\"."
  :group 'tidy
  :type 'directory
  :set-after '(temporary-file-directory))

(defcustom tidy-menu-lock t
  " *Non-nil means menu is locked (i.e. doesn't pop down) when
selecting toggle and radio options.

See also `tidy-menu-x-position'."
  :type 'boolean
  :group 'tidy)

(defcustom tidy-menu-x-position 211
  "Specify menu position height in pixels.

This variable is used to set the horizontal position of the locked
menu, so don't forget to adjust it if menu position is not ok.

See also `tidy-menu-lock'."
  :type 'integer
  :group 'tidy)

;;;;; Local Variables

(defvar tidy-debug  nil
  "If t then we rebuild everything on reload. Useful for debugging.")

;;(eval-when-compile (setq tidy-debug t))

(defun tidy-toggle-debug ()
  "Toggle value of tidy-debug."
  (interactive)
  (message "tidy-debug is %s" (setq tidy-debug (not tidy-debug))))

;; (defun tidy-boolean-option-value (symbol)
;;   "Return t when the symbol's value is \"yes\"."
;;   (let ((name (symbol-name symbol)))
;;     (assert (string= "tidy-" (substring name 0 5)))
;;     (setq name (substring name 5))
;;     (let ((entry (assoc name tidy-options-alist)))
;;       (assert (string= "Boolean" (nth 2 entry)))))
;;   (when (symbol-value symbol)
;;     (string= (symbol-value symbol))))

(defvar tidy-options-alist nil
  "An alist containing all valid tidy options.
Each element is a list of the form
    (NAME, SUB-MENU, VALUE-TYPE, DEFAULT-VALUE, DOC-STRING).
This is used to automatically construct variables and a menu bar.
To add new or modify exiting options simply modify this list.")

;; Fix-me: built the options list dynamically, point to
;; http://tidy.sourceforge.net/docs/quickref.html for help
(defun tidy-build-options-alist ()
  (when (and tidy-shell-program
             (executable-find tidy-shell-program))
    (let ((outbuf (get-buffer-create "* Tidy options *")))
      (call-process tidy-shell-program
                    nil     ;; No input
                    outbuf  ;; Output here
                    nil     ;; Do not display
                    "-help-config")
      (switch-to-buffer outbuf))))

(when (or (null tidy-options-alist) tidy-debug)
  (setq tidy-options-alist
        '(
          ("add-xml-decl" "Fix Markup" "Boolean" "no"
           "
Type: Boolean
Default: no
Example: y/n, yes/no, t/f, true/false, 1/0

This option specifies if Tidy should add the XML declaration when
outputting XML or XHTML. Note that if the input already includes an <?xml
... ?> declaration then this option will be ignored.")

;;        ("add-xml-pi" "Fix Markup" "Boolean" "no"
;;         "
;; Type: Boolean
;; Default: no
;; Example: y/n, yes/no, t/f, true/false, 1/0

;; This option is the same as the add-xml-decl option.")

          ("add-xml-space" "Fix Markup" "Boolean" "no"
           "
Type: Boolean
Default: no
Example: y/n, yes/no, t/f, true/false, 1/0

This option specifies if Tidy should add xml:space=\"preserve\" to elements
such as <PRE>, <STYLE> and <SCRIPT> when generating XML. This is needed if
the whitespace in such elements is to be parsed appropriately without
having access to the DTD.")

          ("alt-text" "Fix Markup" "String" ""
           "
Type: String
Default: -none-

This option specifies the default \"alt=\" text Tidy uses for <IMG>
attributes. This feature is dangerous as it suppresses further
accessibility warnings. You are responsible for making your documents
accessible to people who can not see the images!")

          ("ascii-chars" "Fix Markup" "Boolean" "yes"
           "
Type: Boolean
Default: yes
Example: y/n, yes/no, t/f, true/false, 1/0

Can be used to modify behavior of -c (--clean yes) option.
Defaults to \"yes\" when using -c. Set to \"no\" to prevent
converting &emdash;, &rdquo;, and other named character entities
to their ascii equivalents.")

          ("assume-xml-procins" "Fix Markup" "Boolean" "no"
           "
Type: Boolean
Default: no
Example: y/n, yes/no, t/f, true/false, 1/0

This option specifies if Tidy should change the parsing of processing
instructions to require ?> as the terminator rather than >. This option is
automatically set if the input is in XML.")

          ("bare" "Fix Markup" "Boolean" "no"
           "
Type: Boolean
Default: no
Example: y/n, yes/no, t/f, true/false, 1/0

This option specifies if Tidy should strip Microsoft specific HTML from
Word 2000 documents, and output spaces rather than non-breaking spaces
where they exist in the input.")

          ("break-before-br" "Fix Markup" "Boolean" "no"
           "
Type: Boolean
Default: no
Example: y/n, yes/no, t/f, true/false, 1/0

This option specifies if Tidy should output a line break before each <BR>
element.")

          ("char-encoding" "Encoding" "Encoding" "ascii"
           "
Type: Encoding
Default: ascii
Example: ascii, latin1, raw, utf8, iso2022, mac, win1252

This option specifies the character encoding Tidy uses for both the input
and output. Possible values are: ascii, latin1, raw, utf8, iso2022, mac,
win1252. For ascii, Tidy will accept Latin-1 (ISO-8859-1) character
values, but will use entities for all characters whose value > 127. For
raw, Tidy will output values above 127 without translating them into
entities. For latin1, characters above 255 will be written as
entities. For utf8, Tidy assumes that both input and output is encoded as
UTF-8. You can use iso2022 for files encoded using the ISO-2022 family of
encodings e.g. ISO-2022-JP. For mac and win1252, Tidy will accept vendor
specific character values, but will use entities for all characters whose
value > 127.")

          ("clean" "Fix Markup" "Boolean" "no"
           "
Type: Boolean
Default: no
Example: y/n, yes/no, t/f, true/false, 1/0

This option specifies if Tidy should strip out surplus presentational tags
and attributes replacing them by style rules and structural markup as
appropriate. It works well on the HTML saved by Microsoft Office products.")

          ("doctype" "Fix Markup" "DocType" "auto"
           "
Type: DocType
Default: auto
Example: auto, omit, strict, loose, transitional, user specified fpi \(string\)

This option specifies the DOCTYPE declaration generated by Tidy. If set to
\"omit\" the output won't contain a DOCTYPE declaration. If set to \"auto\"
\(the default\) Tidy will use an educated guess based upon the contents of
the document. If set to \"strict\", Tidy will set the DOCTYPE to the strict
DTD. If set to \"loose\", the DOCTYPE is set to the loose \(transitional\)
DTD. Alternatively, you can supply a string for the formal public
identifier \(FPI\). For example:

      doctype: \"-//ACME//DTD HTML 3.14159//EN\"

If you specify the FPI for an XHTML document, Tidy will set
the system identifier to the empty string. Tidy leaves the DOCTYPE for
generic XML documents unchanged.")

          ("drop-empty-paras" "Fix Markup" "Boolean" "yes"
           "
Type: Boolean
Default: yes
Example: y/n, yes/no, t/f, true/false, 1/0

This option specifies if Tidy should discard empty paragraphs. If set to
no, empty paragraphs are replaced by a pair of <BR> elements as HTML4
precludes empty paragraphs.")

          ("drop-font-tags" "Fix Markup" "Boolean" "no"
           "
Type: Boolean
Default: no
Example: y/n, yes/no, t/f, true/false, 1/0

This option specifies if Tidy should discard <FONT> and <CENTER> tags
rather than creating the corresponding style rules, but only if the clean
option is also set to yes.")

          ("drop-proprietary-attributes" "Fix Markup" "Boolean" "no"
           "
Type: Boolean
Default: no
Example: y/n, yes/no, t/f, true/false, 1/0

This option specifies if Tidy should strip out proprietary attributes,
such as MS data binding attributes.")

          ("enclose-block-text" "Fix Markup" "Boolean" "no"
           "
Type: Boolean
Default: no
Example: y/n, yes/no, t/f, true/false, 1/0

This option specifies if Tidy should insert a <P> element to enclose any
text it finds in any element that allows mixed content for HTML
transitional but not HTML strict.")

          ("enclose-text" "Fix Markup" "Boolean" "no"
           "
Type: Boolean
Default: no
Example: y/n, yes/no, t/f, true/false, 1/0

This option specifies if Tidy should enclose any text it finds in the body
element within a <P> element. This is useful when you want to take
existing HTML and use it with a style sheet.")

;;        ("error-file" "Omit" "String" "-none-"
;;         "
;; Type: String
;; Default: -none-

;; This option specifies the error file Tidy uses for errors and
;; warnings. Normally errors and warnings are output to \"stderr\".

;; This is option is ignored in Emacs.")

          ("escape-cdata" "Fix Markup" "Boolean" "no"
           "
Type: Boolean
Default: no
Example: y/n, yes/no, t/f, true/false, 1/0

This option specifies if Tidy should convert <![CDATA[]]> sections to
normal text.")

          ("fix-backslash" "Fix Markup" "Boolean" "yes"
           "
Type: Boolean
Default: yes
Example: y/n, yes/no, t/f, true/false, 1/0

This option specifies if Tidy should replace backslash characters \"\\\" in
URLs by forward slashes \"/\".")

          ("fix-bad-comments" "Fix Markup" "Boolean" "yes"
           "
Type: Boolean
Default: yes
Example: y/n, yes/no, t/f, true/false, 1/0

This option specifies if Tidy should replace unexpected hyphens with \"=\"
characters when it comes across adjacent hyphens. The default is yes. This
option is provided for users of Cold Fusion which uses the comment syntax:
<!--- --->")

          ("fix-uri" "Fix Markup" "Boolean" "yes"
           "
Type: Boolean
Default: yes
Example: y/n, yes/no, t/f, true/false, 1/0

This option specifies if Tidy should check attribute values that carry
URIsfor illegal characters and if such are found, escape them as HTML 4
recommends.")

          ("force-output" "Input/Output" "Boolean" "no"
           "
Type: Boolean
Default: no
Example: y/n, yes/no, t/f, true/false, 1/0

This option specifies if Tidy should produce output even if errors are
encountered. Use this option with care - if Tidy reports an error,
this means Tidy was not able to, or is not sure how to, fix the error,
so the resulting output may not reflect your intention.")

;;        ("gnu-emacs" "Omit" "Boolean" "no"
;;         "
;; Type: Boolean
;; Default: no
;; Example: y/n, yes/no, t/f, true/false, 1/0

;; This option specifies if Tidy should change the format for reporting
;; errors and warnings to a format that is more easily parsed by GNU
;; Emacs.

;; This option is automatically set in Emacs."  )

          ("hide-comments" "Fix Markup" "Boolean" "no"
           "
Type: Boolean
Default: no
Example: y/n, yes/no, t/f, true/false, 1/0

This option specifies if Tidy should print out comments.")

          ("hide-endtags" "Fix Markup" "Boolean" "no"
           "
Type: Boolean
Default: no
Example: y/n, yes/no, t/f, true/false, 1/0

This option specifies if Tidy should omit optional end-tags when
generating the pretty printed markup. This option is ignored if you are
outputting to XML.")

          ("indent" "Indentation" "AutoBool" "no"
           "
Type: AutoBool
Default: no
Example: auto, y/n, yes/no, t/f, true/false, 1/0

This option specifies if Tidy should indent block-level tags.  If set to
\"auto\", this option causes Tidy to decide whether or not to indent the
content of tags such as TITLE, H1-H6, LI, TD, TD, or P depending on whether
or not the content includes a block-level element. You are advised to avoid
setting indent to yes as this can expose layout bugs in some browsers.")

          ("indent-attributes" "Indentation" "Boolean" "no"
           "
Type: Boolean
Default: no
Example: y/n, yes/no, t/f, true/false, 1/0

This option specifies if Tidy should begin each attribute on a new line.")

          ("indent-cdata" "Indent" "Boolean" "no"
           "
Type: Boolean
Default: no
Example: y/n, yes/no, t/f, true/false, 1/0

This option specifies if Tidy should indent <![CDATA[]]> sections.")

          ("indent-spaces" "Indentation" "Integer" "2"
           "
Type: Integer
Default: 2
Example: 0, 1, 2, ...

This option specifies the number of spaces Tidy uses to indent content,
when indentation is enabled.")

          ("input-encoding" "Encoding" "Encoding" "latin1"
           "
Type: Encoding
Default: ascii
Example: ascii, latin1, raw, utf8, iso2022, mac, win1252

This option specifies the character encoding Tidy uses for the input. See
char-encoding for more info.")

          ("input-xml" "Input/Output" "Boolean" "no"
           "
Type: Boolean
Default: no
Example: y/n, yes/no, t/f, true/false, 1/0

This option specifies if Tidy should use the XML parser rather than the
error correcting HTML parser.")

          ("join-classes" "Fix Markup" "Boolean" "no"
           "
Type: Boolean
Default: no
Example: y/n, yes/no, t/f, true/false, 1/0

This option specifies if Tidy should combine class names to generate a
single new class name, if multiple class assignments are detected on an
element.")

          ("join-styles" "Fix Markup" "Boolean" "yes"
           "
Type: Boolean
Default: yes
Example: y/n, yes/no, t/f, true/false, 1/0

This option specifies if Tidy should combine styles to generate a single
new style, if multiple style values are detected on an element.")

          ("keep-time" "Preference" "Boolean" "no"
           "
Type: Boolean
Default: no
Example: y/n, yes/no, t/f, true/false, 1/0

This option specifies if Tidy should alter the last modified time for
files it writes back to. The default is no, which allows you to tidy files
without affecting which ones will be uploaded to a Web server when using a
tool such as 'SiteCopy'. Note that this feature may not work on some
platforms.")

          ("literal-attributes" "Preference" "Boolean" "no"
           "
Type: Boolean
Default: no
Example: y/n, yes/no, t/f, true/false, 1/0

This option specifies if Tidy should ensure that whitespace characters
within attribute values are passed through unchanged.")

          ("logical-emphasis" "Fix Markup" "Boolean" "no"
           "
Type: Boolean
Default: no
Example: y/n, yes/no, t/f, true/false, 1/0

This option specifies if Tidy should replace any occurrence of <I> by <EM>
and any occurrence of <B> by <STRONG>. In both cases, the attributes are
preserved unchanged. This option can be set independently of the clean and
drop-font-tags options.")

          ("lower-literals" "Preference" "Boolean" "yes"
           "
Type: Boolean
Default: yes
Example: y/n, yes/no, t/f, true/false, 1/0

This option specifies if Tidy should convert the value of an attribute
that takes a list of predefined values to lower case. This is required for
XHTML documents.")

          ("markup" "Input/Output" "Boolean" "yes"
           "
Type: Boolean
Default: yes
Example: y/n, yes/no, t/f, true/false, 1/0

This option specifies if Tidy should generate a pretty printed version of
the markup. Note that Tidy won't generate a pretty printed version if it
finds significant errors (see force-output).")

          ("ncr" "Preference" "Boolean" "yes"
           "
Type: Boolean
Default: yes
Example: y/n, yes/no, t/f, true/false, 1/0

This option specifies if Tidy should allow numeric character references.")

          ("newline" "Encoding" "Encoding" "LF"
           "
Type: Encoding
Default: LF
Example: LF, CRLF, CR

Line ending style. \(Only used in batch operations here.\)")

          ("new-blocklevel-tags" "Tags" "Tag names" ""
           "
Type: Tag names
Default: -none-
Example: tagX, tagY, ...

This option specifies new block-level tags. This option takes a space or
comma separated list of tag names. Unless you declare new tags, Tidy will
refuse to generate a tidied file if the input includes previously unknown
tags. Note you can't change the content model for elements such as
<TABLE>, <UL>, <OL> and <DL>.")

          ("new-empty-tags" "Tags" "Tag names" ""
           "
Type: Tag names
Default: -none-
Example: tagX, tagY, ...

This option specifies new empty inline tags. This option takes a space or
comma separated list of tag names. Unless you declare new tags, Tidy will
refuse to generate a tidied file if the input includes previously unknown
tags. Remember to also declare empty tags as either inline or blocklevel.")

          ("new-inline-tags" "Tags" "Tag names" ""
           "
Type: Tag names
Default: -none-
Example: tagX, tagY, ...

This option specifies new non-empty inline tags. This option takes a space
or comma separated list of tag names. Unless you declare new tags, Tidy
will refuse to generate a tidied file if the input includes previously
unknown tags.")

          ("new-pre-tags" "Tags" "Tag names" ""
           "
Type: Tag names
Default: -none-
Example: tagX, tagY, ...

This option specifies new tags that are to be processed in exactly the
same way as HTML's <PRE> element. This option takes a space or comma
separated list of tag names. Unless you declare new tags, Tidy will refuse
to generate a tidied file if the input includes previously unknown
tags. Note you can not as yet add new CDATA elements (similar to
<SCRIPT>).")

          ("numeric-entities" "Preference" "Boolean" "no"
           "
Type: Boolean
Default: no
Example: y/n, yes/no, t/f, true/false, 1/0

This option specifies if Tidy should output entities other than the
built-in HTML entities \(&amp;, &lt;, &gt; and &quot;\) in the numeric
rather than the named entity form.")

          ("output-bom" "Encoding" "AutoBool" "auto"
           "
Type: AutoBool
Default: auto
Example: auto, y/n, yes/no, t/f, true/false, 1/0

This option specifies if Tidy should write a Unicode Byte Order Mark
character (BOM; also known as Zero Width No-Break Space; has value of
U+FEFF) to the beginning of the output; only for UTF-8 and UTF-16 output
encodings. If set to \"auto\", this option causes Tidy to write a BOM to the
output only if a BOM was present at the beginning of the input. A BOM is
always written for XML/XHTML output using UTF-16 output encodings.")

          ("output-encoding" "Encoding" "Encoding" "ascii"
           "
Type: Encoding
Default: ascii
Example: ascii, latin1, raw, utf8, iso2022, mac, win1252

This option specifies the character encoding Tidy uses for the output. See
char-encoding for more info. May only be different from input-encoding for
Latin encodings (ascii, latin1, mac, win1252).")

          ("output-xhtml" "Input/Output" "Boolean" "no"
           "
Type: Boolean
Default: no
Example: y/n, yes/no, t/f, true/false, 1/0

This option specifies if Tidy should generate pretty printed output,
writing it as extensible HTML. This option causes Tidy to set the DOCTYPE
and default namespace as appropriate to XHTML. If a DOCTYPE or namespace
is given they will checked for consistency with the content of the
document. In the case of an inconsistency, the corrected values will
appear in the output. For XHTML, entities can be written as named or
numeric entities according to the setting of the \"numeric-entities\"
option.  The original case of tags and attributes will be preserved,
regardless of other options.")

          ("output-xml" "Input/Output" "Boolean" "no"
           "
Type: Boolean
Default: no
Example: y/n, yes/no, t/f, true/false, 1/0

This option specifies if Tidy should pretty print output, writing it as
well-formed XML. Any entities not defined in XML 1.0 will be written as
numeric entities to allow them to be parsed by a XML parser. The original
case of tags and attributes will be preserved, regardless of other
options.")

          ("quiet" "Input/Output" "Boolean" "no"
           "
Type: Boolean
Default: no
Example: y/n, yes/no, t/f, true/false, 1/0

This option specifies if Tidy should output the summary of the numbers of
errors and warnings, or the welcome or informational messages.")

          ("quote-ampersand" "Preference" "Boolean" "yes"
           "
Type: Boolean
Default: yes
Example: y/n, yes/no, t/f, true/false, 1/0

This option specifies if Tidy should output unadorned & characters as
&amp;.")

          ("quote-marks" "Preference"  "Boolean" "no"
           "
Type: Boolean
Default: no
Example: y/n, yes/no, t/f, true/false, 1/0

This option specifies if Tidy should output \" characters as &quot; as is
preferred by some editing environments. The apostrophe character \' is
written out as &#39; since many web browsers don't yet support &apos;.")

          ("quote-nbsp" "Preference" "Boolean" "yes"
           "
Type: Boolean
Default: yes
Example: y/n, yes/no, t/f, true/false, 1/0

This option specifies if Tidy should output non-breaking space characters
as entities, rather than as the Unicode character value 160 (decimal).")

          ("raw" "Omit" "Boolean" "no"
           "
Type: Boolean
Default: no
Example: y/n, yes/no, t/f, true/false, 1/0 char-encoding

Currently not used, but this option would be the same as the
char-encoding: raw option.")

          ("repeated-attributes" "Fix Markup" ("keep-first" "keep-last") "keep-last"
           "
Type: -
Default: keep-last
Example: keep-first, keep-last

This option specifies if Tidy should keep the first or last attribute, if
an attribute is repeated, e.g. has two align attributes.")


          ("replace-color" "Fix Markup" "Boolean" "no"
           "
Type: Boolean
Default: no
Example: y/n, yes/no, t/f, true/false, 1/0

This option specifies if Tidy should replace numeric values in color
attributes by HTML/XHTML color names where defined, e.g. replace
\"#ffffff\" with \"white\"."  )

          ("slide-style" "Omit" "String"
           "
Type: Name
Default: -none-
split Currently not used.")

;;        ("show-body-only" "Omit" "Boolean" "no"
;;         "
;; Type: Boolean
;; Default: no
;; Example: y/n, yes/no, t/f, true/false, 1/0

;; This option specifies if Tidy should print only the contents of the body
;; tag as an HTML fragment. Useful for incorporating existing whole pages as
;; a portion of another page.

;; Emacs overrides this option.")

          ("show-errors" "Input/Output" "Integer" "6"
           "
Type: Integer
Default: 6
Example: 0, 1, 2, ...

This option specifies the number Tidy uses to determine if further errors
should be shown. If set to 0, then no errors are shown.")

          ("show-warnings" "Input/Output" "Boolean" "yes"
           "
Type: Boolean
Default: yes
Example: y/n, yes/no, t/f, true/false, 1/0

This option specifies if Tidy should suppress warnings. This can be useful
when a few errors are hidden in a flurry of warnings.")

          ("slide-style" "Omit" "String" ""
           "
Type: Name
Default: -none-

Currently not used.")

          ("split" "Omit" "Boolean" "no"
            "
Type: Boolean
Default: no
Example: y/n, yes/no, t/f, true/false, 1/0

This option specifies if Tidy should create a sequence of slides from
the input, splitting the markup prior to each successive <H2>. The
slides are written to \"slide001.html\", \"slide002.html\" etc.

There is currently no Emacs support for this option.")

          ("tab-size" "Indentation" "Integer" "4"
           "
Type: Integer
Default: 4
Example: 0, 1, 2, ...

This option specifies the number of columns that Tidy uses between
successive tab stops. It is used to map tabs to spaces when reading the
input. Tidy never outputs tabs.")

          ("tidy-mark" "Preference" "Boolean" "yes"
           "
Type: Boolean
Default: yes
Example: y/n, yes/no, t/f, true/false, 1/0

This option specifies if Tidy should add a meta element to the document
head to indicate that the document has been tidied. Tidy won't add a meta
element if one is already present.")

          ("uppercase-attributes" "Preference" "Boolean" "no"
           "
Type: Boolean
Default: no
Example: y/n, yes/no, t/f, true/false, 1/0

This option specifies if Tidy should output attribute names in upper
case. The default is no, which results in lower case attribute names,
except for XML input, where the original case is preserved.")

          ("uppercase-tags" "Preference" "Boolean" "no"
           "
Type: Boolean
Default: no
Example: y/n, yes/no, t/f, true/false, 1/0

This option specifies if Tidy should output tag names in upper case. The
default is no, which results in lower case tag names, except for XML
input, where the original case is preserved.")

          ("word-2000" "Fix Markup" "Boolean" "no"
           "
Type: Boolean
Default: no
Example: y/n, yes/no, t/f, true/false, 1/0

This option specifies if Tidy should go to great pains to strip out all
the surplus stuff Microsoft Word 2000 inserts when you save Word documents
as \"Web pages\".  Doesn't handle embedded images or VML.")

          ("wrap" "Line Wrapping" "Integer" "68"
           "
Type: Integer
Default: 68
Example: 0, 1, 2, ...

This option specifies the right margin Tidy uses for line wrapping. Tidy
tries to wrap lines so that they do not exceed this length. Set wrap to
zero if you want to disable line wrapping.")

          ("wrap-asp" "Line Wrapping" "Boolean" "yes"
           "
Type: Boolean
Default: yes
Example: y/n, yes/no, t/f, true/false, 1/0

This option specifies if Tidy should line wrap text contained within ASP
pseudo elements, which look like: <% ... %>.")

          ("wrap-attributes" "Line Wrapping" "Boolean" "no"
           "
Type: Boolean
Default: no
Example: y/n, yes/no, t/f, true/false, 1/0

This option specifies if Tidy should line wrap attribute values, for
easier editing. This option can be set independently of
wrap-script-literals.")

          ("wrap-jste" "Line Wrapping" "Boolean" "yes"
           "
Type: Boolean
Default: yes
Example: y/n, yes/no, t/f, true/false, 1/0

This option specifies if Tidy should line wrap text contained within JSTE
pseudo elements, which look like: <# ... #>.")

          ("wrap-php" "Line Wrapping" "Boolean" "yes"
           "
Type: Boolean
Default: yes
Example: y/n, yes/no, t/f, true/false, 1/0

This option specifies if Tidy should line wrap text contained within PHP
pseudo elements, which look like: <?php ... ?>.")

          ("wrap-script-literals" "Line Wrapping" "Boolean" "no"
           "
Type: Boolean
Default: no
Example: y/n, yes/no, t/f, true/false, 1/0

This option specifies if Tidy should line wrap string literals that appear
in script attributes. Tidy wraps long script string literals by inserting
a backslash character before the line break.")

          ("wrap-sections" "Line Wrapping" "Boolean" "yes"
           "
Type: Boolean
Default: yes
Example: y/n, yes/no, t/f, true/false, 1/0

This option specifies if Tidy should line wrap text contained within
<![... ]> section tags.")

;;        ("write-back" "Omit" "Boolean" "no"
;;         "
;; Type: Boolean
;; Default: no
;; Example: y/n, yes/no, t/f, true/false, 1/0

;; This option specifies if Tidy should write back the tidied markup to
;; the same file it read from. You are advised to keep copies of
;; important files before tidying them, as on rare occasions the result
;; may not be what you expect.

;; This option is ignored by Emacs.")
          ))
  )

;;;;; Create a variable for each option in `tidy-options-alist'"

;; these variables are made buffer local so that different buffers can
;; use a different set of options

(let ((options-alist tidy-options-alist)
      option name symbol docstring)

  (while (setq option (car options-alist))
    (setq name      (nth 0 option)
          docstring (nth 4 option)
          symbol    (intern (concat "tidy-" name)))
    ;; create the variable on the fly
    (put symbol 'variable-documentation docstring)
    (make-variable-buffer-local symbol)
    (set symbol nil) ;;default)
    (setq options-alist (cdr options-alist))
    )
  )


;;;;; Quick options setting

(defvar tidy-xhtml-values
  '(
    (add-xml-decl "yes")
    (add-xml-space "no")
    (doctype "auto")
    (escape-cdata "no")
    (fix-backslash "yes")
    (fix-bad-comments "yes")
    (fix-uri "yes")
    (indent "yes")
    (indent-cdata "yes")
    (indent-spaces "2")
    (join-classes "yes")
    (join-styles "yes")
    (lower-literals "yes")
    (newline "LF")
    (output-xhtml "yes")
    (output-xml "no")
    (quote-ampersand "yes")
    (quote-nbsp "yes")
    (tidy-mark "no")
    (uppercase-attributes "no")
    (uppercase-tags "no")))

(defun tidy-xhtml-options-ok ()
  (let ((ok t))
    (dolist (optval tidy-xhtml-values)
      (let* ((opt (car optval))
             (val (cadr optval))
             (nam (symbol-name opt))
             (ent (assoc nam tidy-options-alist))
             (def (nth 3 ent))
             (sym (intern (concat "tidy-" nam))))
        (when (equal val def)
          (setq val nil))
        (unless (equal val (symbol-value sym))
          (setq ok nil))))
    ok))

(defun tidy-show-xhtml-options ()
  "List the settings set by `tidy-set-xhtml-options'."
  (interactive)
  (with-output-to-temp-buffer (help-buffer)
    (with-current-buffer (help-buffer)
      (help-setup-xref (list #'tidy-show-xhtml-settings) (interactive-p))
      (let ((inhibit-read-only t)
            (point (point))
            s)
        (insert "Values that will be set by `tidy-set-xhtml-options'.  ")
        (setq s "Green")
        (put-text-property 0 (length s)
                           'face '(:foreground "green")
                           s)
        (insert s " indicates that the local value in the current buffer")
        (insert " is the value that would be set, ")
        (setq s "red")
        (put-text-property 0 (length s)
                           'face '(:foreground "red")
                           s)
        (insert s " indicates it is not.\n\n")
        (fill-region point (point))
        (dolist (optval tidy-xhtml-values)
          (let* ((opt (car optval))
                 (val (cadr optval))
                 (nam (symbol-name opt))
                 (ent (assoc nam tidy-options-alist))
                 (def (nth 3 ent))
                 (cur (symbol-value (intern (concat "tidy-" nam))))
                 (show (copy-seq val))
                )
            (unless cur (setq cur def))
            (put-text-property 0 (length show)
                               'face
                               (if (equal val cur)
                                   '(:foreground "green")
                                 'face '(:foreground "red"))
                               show)
            (insert (format "%25s => %s\n" opt show))))))
    (print-help-return-message)))

(defun tidy-set-xhtml-options (&optional only-current-buffer)
  "Set option necessary to convert to XHTML.
To get a list of this settings use `tidy-show-xhtml-options'.

Note that the option variables are buffer local. The default
variable values are always set. If ONLY-CURRENT-BUFFER is nil set
the buffer local variables in all buffers."
  (interactive (list
                (not (y-or-n-p "Set XHTML options in all buffers? "))))
  (let ((buffers (if (not only-current-buffer)
                     (buffer-list)
                   (list (current-buffer)))))
    (dolist (buffer buffers)
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (dolist (optval tidy-xhtml-values)
            (let* ((opt (car optval))
                   (val (cadr optval))
                   (nam (symbol-name opt))
                   (ent (assoc nam tidy-options-alist))
                   (def (nth 3 ent))
                   (symbol (intern (concat "tidy-" nam))))
              (when (equal val def)
                (setq val nil))
              (set-default symbol val) ;; The least overhead I think
              (unless (equal val (symbol-value symbol))
                (set symbol val)))))))))


;;;;; Menu Lock (adapted from printing.el)

;; quite compiler
(eval-when-compile
  (progn
         (or (boundp 'current-menubar)
             (defvar current-menubar nil))
         (or (fboundp 'tidy-menu-position)
             (defun  tidy-menu-position () ""))
         (or (fboundp 'tidy-menu-lock)
             (defun tidy-menu-lock (entry state path) ""))
         (or (fboundp 'tidy-menu-lookup)
             (defun tidy-menu-lookup (path) ""))
         ))

;; always define these
(defvar tidy-menu nil "Menu used by tidy.")
(defvar tidy-menu-position nil)
(defvar tidy-menu-state nil)

(cond
 ((tidy-xemacs-p)
  ;; XEmacs
  (defun tidy-menu-position ()
    (tidy-x-make-event
     'button-release
     (list 'button 1
           'x tidy-menu-x-position
           'y -5
           )))

  ;; XEmacs
  (defun tidy-menu-lock (entry state path)
    (when (and (not (interactive-p)) tidy-menu-lock)
      (or (and tidy-menu-position (eq state tidy-menu-state))
          (setq tidy-menu-position (tidy-menu-position)
                tidy-menu-state    state))
      (let* ((menu   (tidy-menu-lookup path))
             (result (tidy-x-get-popup-menu-response menu tidy-menu-position)))
        (and (tidy-x-misc-user-event-p result)
             (funcall (tidy-x-event-function result)
                      (tidy-x-event-object result))))
      (setq tidy-menu-position nil)))

  ;; XEmacs
  (defun tidy-menu-lookup (path)
    (car (tidy-x-find-menu-item current-menubar (cons "Tidy" path)))))

 (t
    ;; GNU Emacs
  (defun tidy-menu-position ()
    (let ()
      (list (list tidy-menu-x-position '-5)
            (selected-frame))))         ; frame

  ;; GNU Emacs
;;   (defun tidy-menu-lock-old (entry state path)
;;     (when (and (not (interactive-p)) tidy-menu-lock)
;;       (or (and tidy-menu-position (eq state tidy-menu-state))
;;        (setq tidy-menu-position (tidy-menu-position )
;;              tidy-menu-state    state))
;;       (let* ((menu   (tidy-menu-lookup path))
;;           (result (x-popup-menu tidy-menu-position menu)))
;;      (and result
;;           (let ((command (lookup-key menu (vconcat result))))
;;             (if (fboundp command)
;;                 (funcall command)
;;               (eval command)))))
;;       (setq tidy-menu-position nil)))
  (defun tidy-menu-lock (entry state path)
    (when (and (not (interactive-p)) tidy-menu-lock)
      (or (and tidy-menu-position (eq state tidy-menu-state))
          (setq tidy-menu-position (tidy-menu-position )
                tidy-menu-state    state))
      ;;(popup-menu tidy-menu tidy-menu-position)))
      (run-with-idle-timer 1 nil 'popup-menu tidy-menu tidy-menu-position)))

  ;; GNU Emacs
  (defun tidy-menu-lookup (dummy)
    (lookup-key (current-local-map) [menu-bar Tidy])))
 )

;;;;; Define classes of menu items

(defun tidy-set (var-sym value mess entry state &optional path)
  "Set the value of the symbol VAR-SYM to VALUE giving a message
derived from VALUE and MESS.  Pass on menu data to `tidy-menu-lock'."
  (set var-sym value)
  (message "%s is %s" mess value)
  (tidy-menu-lock entry state path))

(defun tidy-boolean-entry (symbol name type default menu)
  "Returns a menu entry that allows us to toggle the value of SYMBOL.
SYMBOL refers to the option called NAME which has default value
DEFAULT.  TYPE should always have the value \"Boolean\".  MENU refers
to the sub-menu that this item belongs to and POSITION its position in
that list."
  (cond ((equal default "no")
         (list (vector name
                       (list 'tidy-set (list 'quote symbol)
                             (list 'if symbol 'nil "yes")
                             name
                             (list 'quote menu)
                             '(quote toggle)
                             )
                       :style 'toggle
                       :selected (list 'if symbol 't 'nil))))

        ((equal default "yes")
         (list (vector name (list 'tidy-set (list 'quote symbol)
                                  (list 'if symbol 'nil "no")
                                  name
                                  (list 'quote menu)
                                  '(quote toggle)
                                  )
                       :style 'toggle
                       :selected (list 'if symbol 'nil 't))))))

(defun tidy-list-entry (symbol name type default menu)
"Returns a menu entry that allows us to set via a radio button the
value of SYMBOL.  SYMBOL refers to the option called NAME which has
default value DEFAULT.  TYPE should be a list of the possible
values. MENU refers to the sub-menu that this item belongs to and
POSITION its position in that list."
  (let (value element)
      (while (setq value (car type))
        (if (equal value default)
            (setq element
                  (append element
                          (list
                           (vector
                            (concat name " is \"" value "\"")
                            (list 'tidy-set (list 'quote symbol)
                                  (list 'if symbol 'nil value)
                                  name
                                  (list 'quote menu)
                                  '(quote toggle)
                                  )
                            :style 'radio
                            :selected (list 'if symbol 'nil 't)
                            ))))
          (setq element
                (append element
                        (list
                         (vector
                          (concat name " is \"" value "\"")

                          (list 'tidy-set (list 'quote symbol)
                                (list 'if symbol 'nil value)
                                name
                                (list 'quote menu)
                                '(quote toggle)
                                )

                          :style 'radio
                          :selected (list
                                     'if (list 'string-equal symbol value)
                                     't 'nil)
                         )))))
        (setq type (cdr type)))
        element))

(defun tidy-set-string (symbol name default)
  "Set the value of SYMBOL identified by name to VALUE,
unless VALUE equals DEFAULT, in which case we set it to nil."
  (interactive)
  (let* ((last-value (symbol-value symbol))
         (new-value
          (if (tidy-xemacs-p)
              (read-string (format "Set %s to: " name)
                           (or last-value default) nil) ;; no default arg
            (read-string (format "Set %s to: " name)
                         (or last-value default) nil default))))
    (set symbol (if (equal new-value default) nil new-value))))

(defun tidy-set-integer (symbol name default)
  "Set the value of SYMBOL identified by name to VALUE,
unless VALUE = DEFAULT, in which case we set it to nil."
  (interactive)
  (let* ((last-value (symbol-value symbol))
         ;; careful to interpret the string as a number
         (new-value
          (string-to-number
           (if (tidy-xemacs-p)
               (read-string (format "Set %s to: " name)
                (or last-value default) nil)
             (read-string (format "Set %s to: " name)
                (or last-value default) nil default)
             ))))
         (set symbol (if (= new-value (string-to-number default)) nil
                       (number-to-string new-value)))))


(defun tidy-string-entry (symbol name type default menu)
  "Returns a menu entry that allows us to set the value of SYMBOL.
SYMBOL refers to the option called NAME which has default value
DEFAULT.  TYPE should always be one of \"String\" \"Tags\", or
\"DocType\".  MENU and POSITION are not used in this case."

  (list (vector (concat "set " name)
                (list 'tidy-set-string
                      (list 'quote symbol)
                      name default))))

(defun tidy-integer-entry (symbol name type default menu)
"Returns a menu entry that allows us to set the value of SYMBOL.
SYMBOL refers to the option called NAME which has default value
DEFAULT.  TYPE should always have the value \"Integer\". MENU and
POSITION are not used in this case. "
  (list (vector (concat "set " name)
                (list 'tidy-set-integer
                      (list 'quote symbol)
                      name default))))

(defun tidy-exe-found ()
  (when tidy-shell-program
    ;;(file-executable-p (car (split-string tidy-shell-program)))))
    (or (file-executable-p tidy-shell-program)
        (executable-find tidy-shell-program))))


(defvar tidy-top-menu nil
  "The first part of the menu.")
(when (or (null tidy-top-menu) tidy-debug)
  (setq tidy-top-menu
        '("Tidy"
          ["Tidy Buffer" tidy-buffer
           :active (tidy-exe-found)]

;;        ["Tidy Region" tidy-region
;;         :active (and (mark)
;;                         (tidy-exe-found))
;;         :keys "C-u \\[tidy-buffer]"]

          ["Tidy Directory Tree" tidy-tree
           :active (tidy-exe-found)]

          ["Tidy Site" tidy-tree
           :active (and (featurep 'html-site)
                        (tidy-exe-found))]

          "----------------------------------------------"

          ["Customize Tidy" (customize-group-other-window 'tidy)]

;;           ["Use Ediff" (lambda () (interactive)
;;                          (setq tidy-use-ediff (not tidy-use-ediff)))
;;            :style toggle
;;            :selected tidy-use-ediff
;;            ]

;;        ["Load Settings" tidy-parse-config-file
;;         :active (and tidy-config-file (file-exists-p tidy-config-file))]

;;        ["Load Settings All Buffers" (lambda () (interactive)
;;                                          (tidy-parse-config-file t))
;;         :active (and tidy-config-file (file-readable-p tidy-config-file))]

          ["Save Settings" tidy-save-settings
           :active (and tidy-config-file (file-readable-p tidy-config-file))]

          "----------------------------------------------"

          )))

(defvar tidy-newline-menu
  '("Set newline"
    ["LF"     (tidy-set 'tidy-newline
                        nil
                        "newline"
                        'newline
                        'toggle)
     :style radio
     :selected (if (null tidy-newline) t nil)]

    ["CRLF"   (tidy-set 'tidy-newline
                        "CRLF"
                        "newline"
                        'newline
                        'toggle)
     :style radio
     :selected (if (equal tidy-newline "CRLF") t nil)]

    ["CR"     (tidy-set 'tidy-newline
                        "CR"
                        "newline"
                        'newline
                        'toggle)
     :style radio
     :selected (if (equal tidy-newline "CRLF") t nil)]
    ))

(defvar tidy-doctype-menu nil "The second to last sub-menu.")
(when (or (null tidy-doctype-menu) tidy-debug)
  (setq  tidy-doctype-menu
         '("Set doctype" ;; ==>

           ["auto"   (tidy-set 'tidy-doctype
                               nil
                               "doctype"
                               'doctype
                               'toggle)
            :style radio
            :selected (if (null tidy-doctype)  t nil)]

           ["omit"   (tidy-set 'tidy-doctype
                               "omit"
                               "doctype"
                               'doctype
                               'toggle)
            :style radio
            :selected (if (equal tidy-doctype "omit") t nil)]

           ["strict" (tidy-set 'tidy-doctype
                               "strict"
                               "doctype"
                               'doctype
                               'toggle)
            :style radio
            :selected (if (equal tidy-doctype "strict") t nil)]

           ["loose"  (tidy-set 'tidy-doctype
                               "loose"
                               "doctype"
                               'doctype
                               'toggle)
            :style radio
            :selected (if (equal tidy-doctype "loose") t nil)]

           ["transitional"  (tidy-set 'tidy-doctype
                                      "transitional"
                                      "doctype"
                                      'doctype
                                      'toggle)
            :style radio
            :selected (if (equal tidy-doctype "transitional") t nil)]

           ["fpi" (null nil) ;; stub function
            :style radio
            :selected (if (or (null tidy-doctype)
                              (equal tidy-doctype "omit")
                              (equal tidy-doctype "strict")
                              (equal tidy-doctype "loose"))
                          nil t) ]

           ["reset fpi" (tidy-set-string 'tidy-doctype "doctype" "" "")]
           )))

(defconst tidy-emacs-encoding-lbl "Use Emacs' encoding")

(defun tidy-create-encoding-menu (label encoding-what msg-what)
  (list label ;; ==>
        (vector tidy-emacs-encoding-lbl   (list 'tidy-set (list 'quote encoding-what)
                                tidy-emacs-encoding-lbl ;(list 'tidy-get-buffer-encoding)
                                msg-what
                                ''encoding
                                ''toggle)
                :style 'radio
                :selected (list 'if (list 'equal encoding-what tidy-emacs-encoding-lbl) t nil)
                )

        "----------------------------------------------"

        (vector "ascii"   (list 'tidy-set (list 'quote encoding-what)
                                nil
                                msg-what
                                ''encoding
                                ''toggle)
                :style 'radio
                :selected (list 'if (list 'null encoding-what) t nil) ;; default
                )

        (vector "raw"     (list 'tidy-set (list 'quote encoding-what)
                                "raw"
                                msg-what
                                ''encoding
                                ''toggle)
                :style 'radio
                :selected (list 'if (list 'equal encoding-what "raw") t nil))

        (vector "latin1"  (list 'tidy-set (list 'quote encoding-what)
                                "latin1"
                                msg-what
                                ''encoding
                                ''toggle)
                :style 'radio
                :selected (list 'if (list 'equal encoding-what "latin1") t nil))

        (vector "utf8"    (list 'tidy-set (list 'quote encoding-what)
                                "utf8"
                                msg-what
                                ''encoding
                                ''toggle)
                :style 'radio
                :selected (list 'if (list 'equal encoding-what "utf8") t nil))

        (vector "iso2022" (list 'tidy-set (list 'quote encoding-what)
                                "iso2022"
                                msg-what
                                ''encoding
                                ''toggle)
                :style 'radio
                :selected (list 'if (list 'equal encoding-what "iso2022") t nil))

        (vector "mac" (list 'tidy-set (list 'quote encoding-what)
                            "mac"
                            msg-what
                            ''encoding
                            ''toggle)
                :style 'radio
                :selected (list 'if (list 'equal encoding-what "mac") t nil))

        (vector "win1252" (list 'tidy-set (list 'quote encoding-what)
                                "win1252"
                                msg-what
                                ''encoding
                                ''toggle)
                :style 'radio
                :selected (list 'if (list 'equal encoding-what "win1252") t nil))

        ))
;; (defvar tidy-char-encoding-menu nil "The last sub-menu.")
;; (when (or (null tidy-char-encoding-menu) tidy-debug)
;;   (setq tidy-char-encoding-menu
;;         (tidy-create-encoding-menu
;;          "Set char-encoding" 'tidy-char-encoding "char-encoding")))
;; (defvar tidy-input-encoding-menu nil "The last sub-menu.")
;; (when (or (null tidy-input-encoding-menu) tidy-debug)
;;   (setq tidy-input-encoding-menu
;;         (tidy-create-encoding-menu
;;          "Set input-encoding" 'tidy-input-encoding "input-encoding")))
(defvar tidy-output-encoding-menu nil "The last sub-menu.")
(when (or (null tidy-output-encoding-menu) tidy-debug)
  (setq tidy-output-encoding-menu
        (tidy-create-encoding-menu
         "Set output-encoding" 'tidy-output-encoding "output-encoding")))

;;;;; Create a menu item for each option that has a valid sub-menu
;; field

(when (or (null tidy-menu) tidy-debug)
  (let ((options-alist       tidy-options-alist)

        ;; sub menus are divided into two parts with list type options
        ;; coming first, followed by the rest

        markup-menu-bool     markup-menu-set
        line-wrap-menu-bool  line-wrap-menu-set
        preference-menu-bool preference-menu-set
        indent-menu-bool     indent-menu-set
        io-menu-bool         io-menu-set
        tags-menu-bool       tags-menu-set

        name sub-menu type default symbol entry entry-function option)

    (while (setq option (car options-alist))
      (setq name      (nth 0 option)
            sub-menu  (nth 1 option)
            type      (nth 2 option)
            default   (nth 3 option)
            symbol    (intern (concat "tidy-" name))
            entry     nil)

      (cond ((equal type "Boolean")
             (setq entry-function 'tidy-boolean-entry))

            ((equal type "AutoBool")
             (setq entry-function 'tidy-list-entry)
             (setq type '("auto" "yes" "no")))

            ((equal type "DocType")
             (setq entry '())) ;; handled below

            ((equal type "Tag names")
             (setq entry-function 'tidy-string-entry))

            ((equal type "String")
             (setq entry-function 'tidy-string-entry))

            ((equal type "Integer")
             (setq entry-function 'tidy-integer-entry))

            ((equal type "Encoding")
             (setq entry '()));; handled below

            ((listp type)
             (setq entry-function 'tidy-list-entry))
            (t
             (error (concat "Tidy: unhandled value type " type " for " name))))

      (cond ((equal sub-menu "Fix Markup")
             (setq entry (funcall
                          entry-function
                          symbol
                          name
                          type
                          default
                          'markup))

             (if (or (equal type "Boolean") (equal type "AutoBool") (listp type))
                 (setq markup-menu-bool (append markup-menu-bool entry))
               (setq markup-menu-set (append markup-menu-set entry))))

            ((equal sub-menu "Indentation")
             (setq entry (funcall
                          entry-function
                          symbol
                          name
                          type
                          default
                          'indent))

             (if (or (equal type "Boolean") (equal type "AutoBool") (listp type))
                 (setq indent-menu-bool (append indent-menu-bool entry))
               (setq indent-menu-set (append indent-menu-set entry))))

            ((equal sub-menu "Line Wrapping")
             (setq entry (funcall
                          entry-function
                          symbol
                          name
                          type
                          default
                          'line-wrap))

             (if (or (equal type "Boolean") (equal type "AutoBool") (listp type))
                 (setq line-wrap-menu-bool (append line-wrap-menu-bool entry))
               (setq line-wrap-menu-set (append line-wrap-menu-set entry))))

            ((equal sub-menu "Input/Output")
             (setq entry (funcall
                          entry-function
                          symbol
                          name
                          type
                          default
                          'io))

             (if (or (equal type "Boolean") (equal type "AutoBool") (listp type))
                 (setq io-menu-bool (append io-menu-bool entry))
               (setq io-menu-set (append io-menu-set entry))))

            ((equal sub-menu "Preference")
             (setq entry (funcall
                          entry-function
                          symbol
                          name
                          type
                          default
                          'preference))

             (if (or (equal type "Boolean") (equal type "AutoBool") (listp type))
                 (setq preference-menu-bool (append preference-menu-bool entry))
               (setq preference-menu-set (append preference-menu-set entry))))

            ((equal sub-menu "Tags")
             (setq entry (funcall
                          entry-function
                          symbol
                          name
                          type
                          default
                          'tags))

             (if (or (equal type "Boolean") (equal type "AutoBool"))
             (setq tags-menu-bool (append tags-menu-bool entry))
             (setq tags-menu-set (append tags-menu-set entry))))
            (t)) ;; we simple omit all other menus

      (setq options-alist (cdr options-alist)))

  (setq tidy-menu (append
                   tidy-top-menu
                   (list
                    (list "Quick Options Settings"
                          (vector "Set Options for XHTML"
                                  'tidy-set-xhtml-options
                                  :style 'toggle
                                  :selected '(tidy-xhtml-options-ok)
                                  )
                          (vector "Show Options for XHTML"
                                  'tidy-show-xhtml-options
                                  )
                          ))
                   (list (append (list "Advanced")


;;        "----------------------------------------------"

;;        ["Menu Lock" (tidy-set 'tidy-menu-lock
;;                               (if tidy-menu-lock nil t)
;;                               "Menu Lock"
;;                               'top
;;                               'toggle)
;;         :style toggle
;;         :selected (if tidy-menu-lock t nil)
;;         ]
;;                                  (vector "Menu Lock"
;;                                          'tidy-menu-lock
;;                                          :style 'toggle
;;                                          :selected '(if tidy-menu-lock t nil)
;;                                          )
                                 (list (vector "Menu Lock"
                                               '(tidy-set 'tidy-menu-lock
                                                          (if tidy-menu-lock nil t)
                                                          "Menu Lock"
                                                          'top
                                                          'toggle
                                                          )
                                               :style 'toggle
                                               :selected '(if tidy-menu-lock t nil)
                                               ))
                                 (list (list "-------"))

                                 (list (append (list "Fix Markup")
                                               markup-menu-bool
                                               markup-menu-set))
                                 (list (append  (list "Line Wrapping")
                                                line-wrap-menu-bool
                                                line-wrap-menu-set))
                                 (list (append (list "Preference")
                                               preference-menu-bool
                                               preference-menu-set))
                                 (list (append (list "Indentation")
                                               indent-menu-bool
                                               indent-menu-set))
                                 (list (append (list "Input/Output")
                                              io-menu-bool
                                              io-menu-set))
                                 (list (append (list "Tags")
                                               tags-menu-bool
                                               tags-menu-set))
                                 (list tidy-doctype-menu)
                                 (list tidy-output-encoding-menu)
                                 (list tidy-newline-menu)
                                 ))
                   '(["Describe Options" tidy-describe-options t])
                   (list (list "-------"))
                   '(["Tidy Home Page"
                      (lambda ()
                        "Open Tidy home page in your web browser."
                        (interactive)
                        (browse-url "http://tidy.sourceforge.net/"))
                      t])
                   ))
  )
)

(defvar tidy-menu-symbol nil)
;;(tidy-build-menu (&optional map)
;;;###autoload
(defun tidy-build-menu (&optional map)
  "Set up the tidy menu in MAP.
Used to set up a Tidy menu in your favourite mode."
  (interactive) ;; for debugging
  (unless tidy-menu-symbol
    (unless tidy-config-file-parsed
      (tidy-parse-config-file)
      (setq tidy-config-file-parsed t))
    ;;(or map (setq map (current-local-map)))
    (easy-menu-remove tidy-menu)
    (easy-menu-define tidy-menu-symbol map "Menu for Tidy" tidy-menu)
    (setq tidy-menu-symbol (delete "Tidy" tidy-menu-symbol))
    (easy-menu-add tidy-menu map))
  t)

;;;;; Option description support

;; quiet FSF Emacs and XEmacs compilers
(eval-when-compile
  (progn (or (fboundp 'event-point) (defun event-point (dummy) ""))
         (or (fboundp 'posn-point)  (defun posn-point  (dummy) ""))
         (or (fboundp 'event-start) (defun event-start (dummy) ""))))

(defun tidy-describe-this-option-mouse (click)
  (interactive "e")
  (let ((p (if (tidy-xemacs-p)
               (event-point click)
             (posn-point (event-start click)))))
    (tidy-describe-this-option p)))

(defun tidy-describe-this-option (&optional point)
  "Describe variable associated with the text at point."
  (interactive (list (point)))

  (let* ((variable (get-text-property
                    point
                    'tidy-variable))
         value
         buffer) ;; reuse the help buffer
    (when variable
      (with-output-to-temp-buffer (help-buffer)
        (help-setup-xref (list #'tidy-describe-this-option point) (interactive-p))
        (with-current-buffer (help-buffer)
          (setq value (symbol-value variable))
          (insert (substring (symbol-name variable) 5) ;; clip the `tidy-' prefix
                  " is set to ")
          (if value (insert value) (insert "set to the default value"))
          (insert "\n\n" (documentation-property variable 'variable-documentation))
          (local-set-key [(q)] 'tidy-quit-describe-options)
          (print-help-return-message))))))

(defun tidy-quit-describe-options ()
  "Rid thyself of any display associated with Tidy's options."
  (interactive)
  (bury-buffer (get-buffer "*tidy-options*"))
  (delete-windows-on (get-buffer "*tidy-options*"))
  (bury-buffer (get-buffer "*Help*"))
  (delete-windows-on (get-buffer "*Help*")))

;; nicked this from cal-desk-calendar.el:-)
(defun tidy-current-line ()
  "Get the current line number (in the buffer) of point."
  ;;(interactive)
  (save-restriction
    (widen)
    (save-excursion
      (beginning-of-line)
      (1+ (count-lines 1 (point))))))

(defun tidy-describe-options ()
  "Interactively access documentation strings for `tidy-' variables."
  (interactive)
  (let ((buffer (get-buffer "*tidy-options*")))
    (if buffer (pop-to-buffer buffer)
      ;; else build it from scratch
      (setq buffer (get-buffer-create "*tidy-options*"))
      (let* ((start 0)
            (end 0)
            name
            (count 0)
            (option-alist tidy-options-alist)
            (column2a (+ (length "drop-proprietary-attributes") 3))
            (column2b (/ (window-width) 3))
            (column2 (if (> column2a column2b) column2a column2b))
            (column3 (* 2 column2))
            (start-line 0)
            (third-length 0)
            (two-third-length 0))

        (set-buffer buffer)

        (setq buffer-read-only nil)
        (delete-region (point-min) (point-max)) ;; empty the buffer

        ;; set up local bindings
        (if (tidy-xemacs-p)
            (local-set-key [(button2)] 'tidy-describe-this-option-mouse)
          (local-set-key [(mouse-2)] 'tidy-describe-this-option-mouse))

        (local-set-key "\r" 'tidy-describe-this-option)
        (local-set-key [(q)] 'tidy-quit-describe-options)

        (insert "Press RET over option to see its description.  "
                "Type \"q\" to quit." "\n\n")

        (setq start-line (tidy-current-line))
        (setq third-length (1+ (/ (length option-alist) 3) ))
        (setq two-third-length (1- (* 2 third-length)))

        (while (setq name (car (car-safe option-alist)))
          (setq option-alist (cdr option-alist))
          (setq count (+ count 1))

          (cond
           ((< count third-length)     ;; 0 <= count < third-length
            (setq start (point))
                (insert name)
                (setq end (point))
                (insert "\n"))
           ((< count two-third-length) ;; third-length <= count < two-third-length
            (if (= count third-length)
                (goto-line start-line)
              (forward-line 1))
            (end-of-line)
            (setq start (point))
            (indent-to-column column2)
            (setq end (point))
            (put-text-property start end 'mouse-face 'default)
            (setq start (point))
            (insert name)
            (setq end (point)))
           (t                          ;; two-third-length <= count < length
            (if (= count two-third-length)
                (goto-line start-line)
              (forward-line 1))
            (end-of-line)
            (setq start (point))
            (indent-to-column column3)
            (setq end (point))
            (put-text-property start end 'mouse-face 'default)
            (setq start (point))
            (insert name)
            (setq end (point))))

          ;; make the strings funky
          (put-text-property start end 'mouse-face 'highlight)
          (put-text-property start end 'tidy-variable (intern (concat "tidy-" name)))
          )
        (setq buffer-read-only t)
        ;;(beginning-of-buffer)
        (goto-char (point-min))
        (pop-to-buffer buffer)
        ))))

;;;;; Configuration file support

(defun tidy-parse-config-file (&optional all-buffers)
  "Parse `tidy-config-file' and set variables accordingly.
If `tidy-config-file' is nil or \"\" do nothing. If the file does
not exist just give a message.

Note that the option variables are buffer local. The default
variable values are always set. If ALL-BUFFERS is non-nil set the
buffer local variables in all buffers."
  (interactive (list
                (y-or-n-p "Set Tidy config file values in all buffers? ")))
  (tidy-set-xhtml-options all-buffers)
  (when (and tidy-config-file
             (not (string= "" tidy-config-file)))
    (if (not (file-exists-p tidy-config-file))
        (unless (string= tidy-config-file tidy-default-config-file)
          (message "Could not find Tidy config file \"%s\"." tidy-config-file))
      (message "Parsing config file...")
      (let ((html-buffer (current-buffer))
            (config-buffer (find-file-noselect tidy-config-file t))
            config-variables)
        (save-excursion
          (set-buffer config-buffer)
          (goto-char (point-min)) ;; unnecessary but pedantic

          ;; delete all comments
          (while (re-search-forward "//.*\n" nil t)
            (replace-match "" nil nil))

          (goto-char (point-min))
          (while (re-search-forward "\\([a-z,-]+\\):\\s-*\\(.*\\)\\s-*" nil t)
            ;; set the variable
            ;; Thanks to Thomas Baumann for this bugfix
            (let ((variable (concat "tidy-" (match-string 1)))
                  (value (match-string 2)))
              ;;(set-default (intern variable value))
              (set-default (intern variable) value)
              (setq config-variables
                    (cons (cons variable value) config-variables))
              (save-excursion
                (set-buffer html-buffer)
                (set (intern variable) value))))

          (set-buffer-modified-p nil) ;; don't save changes
          (kill-buffer config-buffer))
        (when all-buffers
          (dolist (buffer (buffer-list))
            (when (buffer-live-p buffer)
              (with-current-buffer buffer
                (dolist (optval config-variables)
                  (let* ((opt (car optval))
                         (val (cdr optval))
                         (sym (intern opt)))
                    (set sym val))))))))
      (message "Parsing config file...done")
      )))

(defun tidy-save-settings (&optional config-file)
  "Query saving the current settings to your `tidy-config-file'.
The local values in the current buffer will be saved."
  (interactive)
  (or config-file (setq config-file tidy-config-file))
  (when config-file

      ;; should check for locks!
      (if (or (not (interactive-p))
              (y-or-n-p "Save settings to your tidy configuration file? "))

          (let ((buffer (find-file-noselect config-file t))
                (option-alist tidy-options-alist)
                (outer-buffer (current-buffer))
                option name symbol value)
            (save-excursion
              (set-buffer buffer)
              (delete-region (point-min) (point-max)) ;; clear the buffer

              ;; need this line so that config file is always non empty
              (insert "// HTML Tidy configuration file \n")
              (while (setq option (car option-alist))
                (setq option-alist (cdr option-alist))
                (setq name      (nth 0 option)
                      symbol    (intern (concat "tidy-" name)))
                (save-excursion ;; this is a local variable
                  (set-buffer outer-buffer)
                  (setq value (symbol-value symbol)))
                (when (string= value tidy-emacs-encoding-lbl)
                  (setq value (tidy-get-buffer-encoding)))
                (when value ;; nil iff default
                  (insert (car option) ": " value "\n")))

              (save-buffer)
              ;;(basic-save-buffer)
              (kill-buffer buffer)
              )))))


;;;;; Main user function

(eval-when-compile (defvar tidy-markup nil ""))

(defun tidy-set-buffer-unmodified (dummy1 dummy2 dumm3)
  "Used to prevent error buffer form being marked as modified."
  (set-buffer-modified-p nil))

;; See http://www.mhonarc.org/MHonArc/doc/resources/charsetaliases.html
(defconst tidy-encodings-mime-charset-list
  '(
    ;; ("raw") ;; Same as ascii??
    ("ascii"      . "us-ascii")
    ("latin0"     . "iso-8859-15")
    ("latin1"     . "iso-8859-1")
    ("iso2022"    . "iso-2022-jp") ;; Correct? There are several iso-2022-..
    ("utf8"       . "utf-8")
    ("mac"        . "macintosh")
    ("win1252"    . "windows-1252")
    ("ibm858"     . "cp850")
    ("utf16le"    . "utf-16-le")
    ("utf16be"    . "utf-16-be")
    ("utf16"      . "utf-16")
    ("big5"       . "big5")
    ("shiftjis"   . "shift_jis")
    )
  "Encoding names used by Tidy and Emacs.
First column is Tidy's name, second Emacs' name."
  )

(defun tidy-get-buffer-encoding ()
  "Get Tidy's name for value of `buffer-file-coding-system'."
  (tidy-get-tidy-encoding buffer-file-coding-system))

(defun tidy-get-tidy-encoding (emacs-coding-system)
  (let ((encoding (rassoc
                   (symbol-name
                    (coding-system-get emacs-coding-system 'mime-charset))
                   tidy-encodings-mime-charset-list)))
    (if encoding
        (setq encoding (car encoding))
      (setq encoding "raw"))
    encoding))

(defun tidy-temp-config-file ()
  (expand-file-name "temp-tidy-config"
                    tidy-temp-directory))

(defconst tidy-output-buf-name "Tidy (X)HTML Output")

(defvar tidy-tidied-buffer nil)
(make-variable-buffer-local 'tidy-tidied-buffer)
(put 'tidy-tidied-buffer 'permanent-local t)

(defun tidy-check-is-tidied (orig-buf tidy-buf)
  (with-current-buffer tidy-buf
    (unless tidy-tidied-buffer
      (error "%s is not a tidy output buffer" tidy-buf))
    (unless (eq orig-buf tidy-tidied-buffer)
      (error "Buffer does not contain tidied %s" orig-buf))))

(defconst tidy-control-buffer-name "Tidy Control Buffer")

(defun tidy-buffer ()
  "Run the HTML Tidy program on the current buffer.
Show the errors in a buffer with buttons to:

- show the original buffer
- show the tidied output
- copy tidied to original
- run ediff

This buffer also contains any error and warning messages and they
link to the original source code.

If the buffer to tidy contains a fictive XHTML validation header
\(see `nxhtml-validation-header-mode') then the corresponding
header is added before running tidy. This header is removed again
after tidying, together with additions tidy might have done at
the end of the buffer.

You may tidy part of the buffer, either by narrowing the buffer
or by selecting a region and having it visibly marked (`cua-mode'
etc). A fictive XHTML validation header will apply as
above. However if there is no such header then when you tidy part
of the buffer still a hopefully suitable header is added before
calling tidy."
;; Fix-me: copy back parts outside visible region
  (interactive)
  (message "starting tidy-buffer")
  (let* ((is-narrowed (buffer-narrowed-p))
         (validation-header (when (boundp 'rngalt-validation-header)
                              (let ((header (nth 2 rngalt-validation-header)))
                                (when header (concat header "\n")))))
         (region-restricted (and mark-active
                                 transient-mark-mode
                                 (or (< (point-min) (region-beginning))
                                     (< (region-end) (point-max)))))
         (partial (or validation-header
                      is-narrowed
                      region-restricted))
         (start (if region-restricted (region-beginning) (point-min)))
         (end   (if region-restricted (region-end) (point-max)))
         (region-header (when region-restricted
                          (when (< (point-min) (region-beginning))
                            (buffer-substring-no-properties
                             (point-min) (region-beginning)))))
         (region-footer (when region-restricted
                          (when (< (region-end) (point-max))
                            (buffer-substring-no-properties
                             (region-end) (point-max)))))
         (tidy-beg-mark "<!-- TIDY BEGIN MARK 142505535 -->\n")
         (tidy-end-mark "<!-- TIDY END MARK 143345187 -->")
         ;;(whole-file t) ;(and (= start 1) (= end   (1+ (buffer-size)))))
         (filename buffer-file-name)
         ;;(orig-dir (file-name-directory filename))
         (orig-buffer (current-buffer))
         (work-buffer (get-buffer-create "* Tidy Temporary Work Buffer *"))
         (line-header-offset nil)

         ;; Gasp! We have to use temp files here because the command
         ;; line would likely get too long!

         (error-buf-name tidy-control-buffer-name)

         (error-file (expand-file-name error-buf-name
                                       tidy-temp-directory))

         (error-buffer (get-buffer-create error-buf-name))

         (output-buffer (get-buffer-create tidy-output-buf-name))

         (config-file (tidy-temp-config-file))

         ;;(eol (coding-system-eol-type buffer-file-coding-system))
         ;;(encoding (coding-system-get buffer-file-coding-system 'mime-charset))
         ;;(errors 0)
         ;;(warnings 0)
         (tidy-message "")
         (seg-error nil)
         ;;(use-ediff tidy-use-ediff)

         (want-mumamo nil)
         )

;;     (when (and use-ediff
;;                (not tidy-show-warnings)) ;; default "yes" hence inverted logic
;;       (setq use-ediff (y-or-n-p "Warning can not be shown when using ediff. Still use ediff? ")))
    (unless buffer-file-name
      (error "Can't tidy buffer with no file name"))

    (when (buffer-modified-p orig-buffer)
      (error "Can't tidy buffer because it is modified"))

    (with-current-buffer error-buffer
      (setq buffer-read-only nil)
      (erase-buffer)
      (setq tidy-tidied-buffer orig-buffer))

    ;; OK do the tidy
;;     (message "coding-system: %s, %s, %s"
;;              (find-operation-coding-system
;;               'call-process-region start end command)
;;              coding-system-for-write
;;              buffer-file-coding-system)
    (let* ((coding-system-for-write buffer-file-coding-system)
           (tidy-input-encoding (tidy-get-tidy-encoding coding-system-for-write)))

      (let ((output-mode (if (not (featurep 'mumamo))
                             major-mode
                           (if mumamo-multi-major-mode
                               mumamo-multi-major-mode
                             major-mode))))
        (with-current-buffer output-buffer
          (erase-buffer)
          ;;(when (and (fboundp 'mumamo-mode) mumamo-mode) (setq want-mumamo t) (mumamo-mode 0))
          (funcall output-mode)
          (set (make-local-variable 'coding-system-for-read) coding-system-for-write)))

      (let ((tidy-output-encoding tidy-output-encoding))
        (unless tidy-output-encoding
          (setq tidy-output-encoding tidy-input-encoding))
        ;;(message "tidy-input-enc=%s, tidy-output-enc=%s" tidy-input-encoding tidy-output-encoding)
        (tidy-save-settings config-file)
        )

      ;; Tidy does not replace the xml declaration so it must be removed:
                                        ;(setq tidy-add-xml-decl "no")
      (let (;(orig-min (point-min)) (orig-max (point-max))
            )
;;         (when region-restricted
;;           ;; We have a visible region
;;           (setq orig-min (region-beginning))
;;           (setq orig-max (region-end)))
        (with-current-buffer work-buffer
          (erase-buffer)
          (when validation-header
            (insert validation-header))
          (when partial
            ;; Have to insert things to make tidy insert at correctt
            ;; position. A bit of guessing here to keep it simple.
            (let ((p "\n<p>TIDY</p>\n")
                  (b "\n<body>\n")
                  (he "\n<head><title></title></head>\n")
                  (ht (concat
                       "<?xml version=\"1.0\" encoding=\"utf-8\"?>\n"
                       "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.1//EN\"\n"
                       "\"http://www.w3.org/TR/xhtml11/DTD/xhtml11.dtd\">\n"
                       "<html xmlns=\"http://www.w3.org/1999/xhtml\">\n")))
              (goto-char (point-min))
              (if (re-search-forward "<body[^>]*>" nil t)
                  (insert p)
                (if (search-forward "</head>" nil t)
                    (insert b p)
                  (if (re-search-forward "<html[^>]*>" nil t)
                      (insert he b p)
                    (insert ht he b p))))
              (goto-char (point-max))
              (insert "\n" tidy-beg-mark)))
          (setq line-header-offset (line-number-at-pos))
          (insert-buffer-substring orig-buffer start end) ;orig-min orig-max)
          (when partial (insert "\n" tidy-end-mark "\n"))
          (let ((args
                 (list
                  ;;start end
                  (point-min) (point-max)
                  tidy-shell-program
                  nil
                  output-buffer
                  t
                  "-config" config-file
                  "--error-file" error-file
                  "--write-back" "no"
                  ;;(if (not whole-file) "--show-body-only" "--show-body-only")
                  ;;(if (not whole-file) "yes" "no")
                  "--show-body-only" "no"
                  "--gnu-emacs" "yes"
                  "--gnu-emacs-file" (file-name-nondirectory filename)
                  ))
                (default-directory (file-name-directory filename))
                )
            (apply 'call-process-region args)))
        ))


    ;; Since XEmacs can't grab the std error stream we use an error file
    ;;(setq error-buffer (find-file-noselect error-file t))
    (with-current-buffer error-buffer
      (setq tidy-tidied-buffer orig-buffer)
      (insert-file-contents error-file)
      ;; Change the line numbers if a header was inserted
      (when line-header-offset
        (goto-char (point-min))
        (while (re-search-forward ":\\([0-9]+\\):[0-9]+" nil t)
          (let ((line (1+ (- (string-to-number (match-string-no-properties 1))
                             line-header-offset))))
            (replace-match (number-to-string line) nil nil nil 1))))
        )

    ;; avoid leaving these guys lying around
    (if (file-exists-p error-file)  (delete-file error-file))
    ;;(if (file-exists-p config-file) (delete-file config-file))

    ;; scan the buffer for error strings
    (save-excursion
      (set-buffer error-buffer)
      ;;(local-set-key [tab] 'tidy-errbuf-forward)
      (goto-char (point-min))
      (insert "\n")
      (make-local-variable 'widget-button-face)
      (setq widget-button-face custom-button)
      (set (make-local-variable 'widget-push-button-prefix) "")
      (set (make-local-variable 'widget-push-button-suffix) "")
      (set (make-local-variable 'widget-link-prefix) "")
      (set (make-local-variable 'widget-link-suffix) "")
      (widget-create 'push-button
                     :tag " Show Source "
                     :keymap (make-sparse-keymap)
                     :arg-orig orig-buffer
                     :action (lambda (widget &optional event)
                               (let ((orig-buf (widget-get widget :arg-orig))
                                     (curr-win (selected-window)))
                                 (switch-to-buffer-other-window orig-buf)
                                 (select-window curr-win))))
      (insert " ")
      (widget-create 'push-button
                     :tag " Show Tidied "
                     :keymap (make-sparse-keymap)
                     :arg-tidy output-buffer
                     :arg-orig orig-buffer
                     :action (lambda (widget &optional event)
                               (let ((tidy-buf (widget-get widget :arg-tidy))
                                     (orig-buf (widget-get widget :arg-orig))
                                     (curr-win (selected-window)))
                                 (tidy-check-is-tidied orig-buf tidy-buf)
                                 (switch-to-buffer-other-window tidy-buf)
                                 (select-window curr-win))))


      (insert " ")
      (widget-create 'push-button
                     :tag " Use Tidied "
                     :keymap (make-sparse-keymap)
                     :arg-tidy output-buffer
                     :arg-orig orig-buffer
                     :action (lambda (widget &optional event)
                               (message "Copying ...")
                               (let* ((orig-buf (widget-get widget :arg-orig))
                                      (tidy-buf (widget-get widget :arg-tidy))
                                      (orig-buf-str
                                       (save-restriction
                                         (with-current-buffer orig-buf
                                           (widen)
                                           (buffer-substring-no-properties (point-min) (point-max)))))
                                      (tidy-buf-str
                                       (save-restriction
                                         (with-current-buffer tidy-buf
                                           (widen)
                                           (buffer-substring-no-properties (point-min) (point-max)))))
                                      )
                                 (tidy-check-is-tidied orig-buf tidy-buf)
                                 (kill-buffer (current-buffer))
                                 (kill-buffer tidy-buf)
                                 (if (string= orig-buf-str tidy-buf-str)
                                     (message "Original buffer's and tidied buffer's contents are equal")
                                   (with-current-buffer orig-buf
                                     (erase-buffer)
                                     (insert tidy-buf-str)
                                     (goto-char (point-min))
                                     (delete-window (selected-window))
                                     (switch-to-buffer orig-buf)
                                     (message "Copied to %s" orig-buf))))))
      (insert " ")
      (widget-create 'push-button
                     :tag " Ediff "
                     :keymap (make-sparse-keymap)
                     :arg-tidy output-buffer
                     :arg-orig orig-buffer
                     :action (lambda (widget &optional event)
                               (require 'ediff)
                               (let ((orig-buf (widget-get widget :arg-orig))
                                     (tidy-buf (widget-get widget :arg-tidy))
                                     ;; Fix-me: How should ediff-actual-options be set?
                                     (old-ediff-actual-diff-options (default-value 'ediff-actual-diff-options))
                                     (new-ediff-actual-diff-options " -a -b -w "))
                                 (with-current-buffer orig-buf (setq ediff-actual-diff-options " -a -b -w "))
                                 (with-current-buffer tidy-buf (setq ediff-actual-diff-options " -a -b -w "))
                                 (tidy-check-is-tidied orig-buf tidy-buf)
                                 (set-default 'ediff-actual-diff-options new-ediff-actual-diff-options)
                                 (tidy-ediff-buffers orig-buf tidy-buf)
                                 (set-default 'ediff-actual-diff-options old-ediff-actual-diff-options)
                                 )))
      ;;(widget-setup)

      (insert "\n\n")
      (when (re-search-forward (concat
                                "\\([0-9]+\\) warnings?, "
                                "\\([0-9]+\\) errors? were found!")
                               nil t)
        (setq tidy-warnings (string-to-number (match-string 1)))
        (setq tidy-errors (string-to-number (match-string 2)))
        (setq tidy-message (match-string 0)))

      (goto-char (point-min))
      ;;(while (re-search-forward "stdin:" nil t) (replace-match (concat filename ":")))
      (wab-compilation-mode)
      (set-buffer-modified-p nil)
      (goto-char (point-min))
      ;; Fix-me: How should this be run? Some hook for compilation I
      ;; guess, but what is the name of it?
      ;; (wab-forward)
      )


    (when (buffer-live-p output-buffer)
      ;; Catch segmentation violations
      ;; Sometimes get this when editing files from Macs
      ;; See the function at the bottom of the file
      (save-excursion
        (set-buffer output-buffer)
        (goto-char (point-min))
        (let ((case-fold-search t))
          (if (looking-at "Segmentation") ;; might work with XEmacs
              (setq seg-error t))))
      ;; Fix-me: add parts outside region
      (when partial
        (with-current-buffer output-buffer
          (goto-char (point-min))
          (when (search-forward tidy-beg-mark nil t)
            (delete-region (point-min) (point)))
          (when region-header (insert region-header))
          (when (search-forward tidy-end-mark nil t)
            (backward-char (length tidy-end-mark))
            (delete-region (point) (point-max)))
          (when region-footer (insert region-footer))))
      )

    (unless (or (> tidy-errors 0) seg-error)
      ;; Do not know if the window stuff is needed?
      (let* ((window (get-buffer-window (current-buffer)))
             (top (window-start window)))

        (unless tidy-markup ;; default is "yes" hence inverted logic
          (when (eq system-type 'windows-nt)
            (tidy-remove-ctrl-m output-buffer))
          (with-current-buffer output-buffer
            (setq tidy-tidied-buffer orig-buffer)
            (delete-trailing-whitespace)
            (indent-region (point-min) (point-max))
            (goto-char (point-min))))

        ;; Try not to move the window too much when we tidy the whole buffer
        (set-window-start window top)))

    (switch-to-buffer-other-window error-buffer)

    (if seg-error
        (message (concat "Tidy: Segmentation violation!!!"
                         "  Check your character encoding."))
      (message "%s" tidy-message))))

(defun tidy-after-ediff ()
  (run-with-idle-timer 0 nil 'remove-hook 'ediff-quit-hook 'tidy-after-ediff)
  ;;(lwarn 't :warning "cb=%s, %s, %s" (current-buffer) ediff-buffer-A ediff-buffer-B)
  (let ((sw (selected-window))
        nw)
    (select-window ediff-window-B)
    (setq nw (split-window))
    (set-window-buffer nw (get-buffer-create tidy-control-buffer-name))
    (select-window sw))
  nil)

(defun tidy-ediff-buffers (buffer-a buffer-b &optional startup-hooks job-name)
  (add-hook 'ediff-quit-hook 'tidy-after-ediff)
  (ediff-buffers buffer-a buffer-b startup-hooks job-name))

;; http://sf.net/tracker/index.php?func=detail&aid=1425219&group_id=27659&atid=390963
(defun tidy-remove-ctrl-m (buffer)
  (with-current-buffer buffer
    (goto-char (point-min))
    (let ((control-m (char-to-string ?\r)))
      (while (search-forward control-m nil t)
        (replace-match "" nil t)))))

(defvar tidy-html-files-re "\.x?html?$")
(defun tidy-is-html-file (filename)
  (string-match tidy-html-files-re filename))

(defun tidy-contains (dir file)
  (let ((d (file-name-as-directory dir)))
    (when (< (length d) (length file))
      (string= d (substring file 0 (length d))))))



(defvar tidy-tree-files nil)
(defun tidy-tree-next ()
  (let ((next-file (car tidy-tree-files))
        file-buffer
        ;;(tidy-use-ediff nil)
        )
    (if (not next-file)
        ;;(setq tidy-batch-buffer nil)
        nil
      (setq tidy-tree-files (cdr tidy-tree-files))
      (if (file-directory-p next-file)
          (error "Uh?")
        (tidy-batch next-file)))))

(defun tidy-tree (root)
  "Run Tidy on all files in the directory ROOT.
The files are first opened in Emacs and then `tidy-buffer' is
called."
  (interactive "DDirectory tree: ")
  (unless (file-directory-p root)
    (error "tidy-tree called with non-directory arg: %s" root))
  (setq tidy-tree-files (html-site-get-sub-files root html-site-files-re))
  (dolist (f tidy-tree-files)
    (let ((b (get-file-buffer f)))
      (when (and b
                 (buffer-modified-p b))
        (unless
            (y-or-n-p (format "Modified buffer %s must be saved. Save it and continue? "
                              (buffer-name b)))
          (error "Modified buffers prevent run with Tidy"))
        (with-current-buffer b
          (basic-save-buffer)))))
  (setq tidy-batch-last-file nil)
  (tidy-tree-next))

(defun tidy-html-site ()
  "Tidy the whole tree in the current site."
  ;; Fix-me: document html-site better.
  (interactive)
  (unless (featurep 'html-site)
    (error "html-site is not loaded"))
  (html-site-current-ensure-site-defined)
  (tidy-tree (html-site-current-site-dir)))

(defun tidy-batch-sentinel (process event)
  (with-current-buffer (process-buffer process)
    (let ((inhibit-read-only t))
      (insert "PROCESS-EVENT: " event "\n")))
  (when (eq (process-status process) 'exit)
    (when tidy-batch-last-file
      (let ((b (get-file-buffer tidy-batch-last-file)))
        (when b
          (with-current-buffer b
            (save-excursion
              (widen)
              (let ((old (buffer-substring-no-properties (point-min) (point-max)))
                    (new (with-temp-buffer
                           ;;(insert-file tidy-batch-last-file)
                           (insert-file-contents tidy-batch-last-file)
                           (buffer-substring-no-properties (point-min) (point-max))))
                    )
                (unless (string= old new)
                  (erase-buffer)
                  (insert new))))))))
    (tidy-tree-next)))

(defun tidy-batch-output-filter (proc string)
  (display-buffer (process-buffer proc))
  (with-current-buffer (process-buffer proc)
    (let ((moving (= (point) (process-mark proc))))
      (save-excursion
        ;; Insert the text, advancing the process marker.
        (goto-char (process-mark proc))
        (let ((inhibit-read-only t))
          ;; http://sf.net/tracker/index.php?func=detail&aid=1425219&group_id=27659&atid=390963
          (setq string (replace-regexp-in-string "\r$" "" string))
          (insert string))
        (set-marker (process-mark proc) (point)))
      (if moving (goto-char (process-mark proc))))))

(defun tidy-batch (filename)
  (interactive (list buffer-file-name)) ;; For testing
  (setq tidy-batch-last-file filename)
  (let* (;;(filename buffer-file-name)
         (config-file (tidy-temp-config-file))
         (command (list tidy-shell-program
                        ;; load configuration file first so that
                        ;; options are overridden by command line

                        "-config" config-file
                        ;;"--error-file" error-file
                        "--write-back" "yes"
                        ;;"--show-body-only" "no"
                        "--gnu-emacs" "yes"
                        "--gnu-emacs-file" filename
                        filename
                        ))
         (procbuf (noshell-procbuf-setup "subprocess for Tidy"))
         (start (with-current-buffer procbuf (point)))
         proc
         ;; This does not work at the moment (2006-05-16):
         (coding-system-for-read 'undecided-dos)
         (coding-system-for-write 'undecided-dos)
         )
    ;;(setq tidy-batch-buffer procbuf)
    (tidy-save-settings config-file)
    (unwind-protect
          (setq proc (apply 'noshell-procbuf-run procbuf command))
      (with-current-buffer procbuf
        (set-process-sentinel proc 'tidy-batch-sentinel)
        ;;(set-process-coding-system 'dos)
        (set-process-filter proc 'tidy-batch-output-filter)
        (let ((win (get-buffer-window procbuf)))
          (when win
            (set-window-point win (point-max))
            ))
        ))))

;;;}}} +

;;;}}}

;; (with-temp-buffer
;;   (tidy-parse-config-file))

(defun wab-compilation-button-at (pos)
  (let ((old (point))
        ret)
    (goto-char pos)
    (setq ret (eq 'compilation-button-map
                  (get-char-property pos 'keymap)))
    (goto-char old)
    ret))

(defun wab-click (&optional event)
  "Do the action that is tighed to the button."
  (interactive (list last-input-event))
  (if event (posn-set-point (event-end event)))
  (let ((button (get-char-property (point) 'button))
        done)
    (condition-case nil
        (progn
          (compile-goto-error)
          (setq done t))
      (error nil))
    (unless done
      (when button
        (if (widget-at)
            ;;(widget-apply-action button event)
            (widget-apply-action button)
          (push-button))))))

(defvar wab-errors-supress
  '("No more buttons"
    "Moved past last error"
    "No buttons or fields found"
    "Moved back before first error"
    ))

(defun wab-fb-errmsg (err)
  (let ((s (error-message-string err)))
    (unless (member s wab-errors-supress)
      (message "%s" err)
      (signal (car err) (cdr err)))))

(defun wab-fb-helper (forward function check-at args)
  (let (ret
        (len (1+ (- (point-max) (point-min))))
        (old (point)))
    (condition-case err
        (progn
          (apply function args)
          (when (funcall check-at (point))
            (setq ret (point))))
      (error (wab-fb-errmsg err)))
    (unless ret
      (if forward
          (goto-char (point-min))
        (goto-char (point-max)))
      (condition-case err
          (progn
            (apply function args)
            (when (funcall check-at (point))
              (setq ret (point))))
        (error (message "%s" (error-message-string err)))))
    (goto-char old)
    (when (and ret (= ret old))
      (if forward
          (setq ret (+ ret len))
        (setq ret (- ret len))))
    ;;(message "function=%s, ret=%s" function ret)
    ret))
(defvar wab-button-list
  '(
    (compilation-previous-error (1) compilation-next-error (1) wab-compilation-button-at)
    (backward-button (1) forward-button (1) button-at)
    (widget-backward (1) widget-forward (1) widget-at)
    ))
(defun wab-fb (forward)
  ;;(message "==================")
  (let* ((pos-list (mapcar (lambda (p)
                             (let ((prev-fun (nth 0 p))
                                   (prev-arg (nth 1 p))
                                   (next-fun (nth 2 p))
                                   (next-arg (nth 3 p))
                                   (test-fun (nth 4 p)))
                               (if forward
                                   (wab-fb-helper t next-fun test-fun next-arg)
                                 (wab-fb-helper nil prev-fun test-fun prev-arg))))
                           wab-button-list))
         (len (1+ (- (point-max) (point-min))))
         (defpos (if forward (* 2 len) (- len)))
         (newpos defpos)
         (here (point)))
    ;;(message "pos-list=%s" pos-list)
    (mapc (lambda (p) (when (and p
                                (if forward
                                    (progn
                                      (when (< p here)
                                        (setq p (+ p len)))
                                      (< p newpos))
                                  (when (> p here)
                                    (setq p (- p len)))
                                  (> p newpos)))
                       (setq newpos p)))
          pos-list)
    (if (= newpos defpos)
        (setq newpos here)
      (setq newpos (mod newpos len)))
    (goto-char newpos)))

(defun wab-backward ()
  "Go to next button or error link."
  (interactive)
  (wab-fb nil))

(defun wab-forward ()
  "Go to previous button or error link."
  (interactive)
  (wab-fb t))

(define-compilation-mode wab-compilation-mode "WAB Compilation"
  "Mode for tidy control buffer."
  )
(define-key wab-compilation-mode-map [tab]         'wab-forward)
(define-key wab-compilation-mode-map [(shift tab)] 'wab-backward)
(define-key wab-compilation-mode-map [backtab]     'wab-backward)
(define-key wab-compilation-mode-map "\r"          'wab-click)
(define-key wab-compilation-mode-map [mouse-1]     'wab-click)
(define-key wab-compilation-mode-map [mouse-2]     'wab-click)

(defvar tidy-menu-mode-map
  (let ((map (make-sparse-keymap)))
    ;; This did not work:
    ;;(define-key map [menu-bar tidy-menu] (list 'menu-item "Tidy" '(lambda () (interactive) tidy-menu-symbol)))
    map))

(define-minor-mode tidy-menu-mode
  "This mode just adds Tidy to the menu bar."
  nil
  nil
  nil
  (when tidy-menu-mode
    (define-key tidy-menu-mode-map [menu-bar tidy-menu]
      (list 'menu-item "Tidy" tidy-menu-symbol))))

(provide 'tidy-xhtml)

;;; tidy-xhtml.el ends here
