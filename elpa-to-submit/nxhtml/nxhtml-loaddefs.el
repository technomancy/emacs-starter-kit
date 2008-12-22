;; Autoloads for nXthml
;;
;; This file should be updated by `nxhtmlmaint-get-file-autoloads',
;; `nxhtmlmaint-get-dir-autoloads' or `nxhtmlmaint-get-all-autoloads'.

;;;### (autoloads (html-site-query-replace html-site-rgrep html-site-find-file
;;;;;;  html-site-dired-current html-site-set-site html-site-buffer-or-dired-file-name)
;;;;;;  "../nxhtml/html-site" "nxhtml/html-site.el" (18658 47798))
;;; Generated autoloads from nxhtml/html-site.el

(autoload 'html-site-buffer-or-dired-file-name "../nxhtml/html-site" "\
Return buffer file name or file pointed to in dired.

\(fn)" nil nil)

(autoload 'html-site-set-site "../nxhtml/html-site" "\
Not documented

\(fn NAME)" t nil)

(autoload 'html-site-dired-current "../nxhtml/html-site" "\
Open `dired' in current site top directory.

\(fn)" t nil)

(autoload 'html-site-find-file "../nxhtml/html-site" "\
Find file in current site.

\(fn)" t nil)

(autoload 'html-site-rgrep "../nxhtml/html-site" "\
Search current site's files with `rgrep'.
See `rgrep' for the arguments REGEXP and FILES.

\(fn REGEXP FILES)" t nil)

(autoload 'html-site-query-replace "../nxhtml/html-site" "\
Query replace in current site's files.

\(fn FROM TO FILE-REGEXP DELIMITED)" t nil)

;;;***

;;;### (autoloads (inlimg-toggle-img-display inlimg-mode) "../nxhtml/inlimg"
;;;;;;  "nxhtml/inlimg.el" (18658 47798))
;;; Generated autoloads from nxhtml/inlimg.el

(autoload 'inlimg-mode "../nxhtml/inlimg" "\
Display <img ...> images inline.
Images are displayed below the <img ...> tag using the margins in
`inlimg-margins'.  The whole image or a slice of it may be
displayed, see `inlimg-slice'.

See also the command `inlimg-toggle-img-display'.

\(fn &optional ARG)" t nil)

(autoload 'inlimg-toggle-img-display "../nxhtml/inlimg" "\
Toggle display of img image at point POINT.
See also the command `inlimg-mode'.

\(fn POINT)" t nil)

;;;***

;;;### (autoloads (nxhtml-validation-header-mode nxhtml-mode) "../nxhtml/nxhtml"
;;;;;;  "nxhtml/nxhtml.el" (18735 15066))
;;; Generated autoloads from nxhtml/nxhtml.el

(autoload 'nxhtml-mode "../nxhtml/nxhtml" "\
Major mode for editing XHTML documents.
It is based on `nxml-mode' and adds some features that are useful
when editing XHTML files.\\<nxhtml-mode-map>

To see an overview in html format do \\[nxhtml-overview].

* Note: Please observe that when loading nXhtml some file
  associations are done, see `nxhtml-auto-mode-alist'.

The nXhtml menu is added by this mode (or actually the minor
mode `nxhtml-minor-mode') and gives quick access and an overview
of some other important features. These includes:

- multiple major modes, see `define-mumamo-multi-major-mode'
- easy uploading and viewing of files, see for example
  `html-upl-upload-file'
- validation in XHTML part for php etc, see
  `nxhtml-validation-header-mode' (you probably also want to know about
  `nxhtml-toggle-visible-warnings' for this!)
- converting of html to xhtml, see `tidy-buffer'

The XML menu contains functionality added by `nxml-mode' (on
which this major mode is based).  There is also a popup menu
added to the [apps] key.

The most important features are probably completion and
validation, which is inherited from `nxml-mode' with some small
addtions.  In very many situation you can use completion. To
access it type \\[nxml-complete]. Completion has been enhanced in
the following way:

- If region is active and visible then completion will surround the
  region with the chosen tag's start and end tag.  However only the
  starting point is checked for validity. If something is wrong after
  insertion you will however immediately see it if you have validation
  on.
- It can in some cases give assistance with attribute values.
- Completion can be customized, see the menus XHTML - Completion:
  * You can use a menu popup style completion.
  * You can have alternatives grouped.
  * You can get a short help text shown for each alternative.
- There does not have to be a '<' before point for tag name
  completion. (`nxml-mode' requires a '<' before point for tag name
  completion.)
- Completes xml version and encoding.
- Completes in an empty buffer, ie inserts a skeleton.

Some smaller, useful, but easy-to-miss features:

* Following links. The href and src attribute names are
  underlined and a special keymap is bound to
  them:\\<mlinks-mode-map>

    \\[mlinks-backward-link], \\[mlinks-forward-link] Move
        between underlined href/src attributes

    \\[mlinks-goto], Mouse-1 Follow link inside Emacs
        (if possible)

  It is even a little bit quicker when the links are in an active
  state (marked with the face `isearch'):\\<mlinks-active-hilight-keymap>

    \\[mlinks-backward-link], \\[mlinks-forward-link] Move
        between underlined href/src attributes
    \\[mlinks-goto], Mouse-1  Follow link inside Emacs (if possible)

  If the link is not into a file that you can edit (a mailto link
  for example) you will be prompted for an alternative action.

* Creating links. To make it easier to create links to id/name
  attribute in different files there are two special
  functions:\\<nxhtml-mode-map>

    \\[nxhtml-save-link-to-here] copy link to id/name (you must
        be in the tag to get the link)
    \\[nxhtml-paste-link-as-a-tag] paste this as an a-tag.

Here are all key bindings in nxhtml-mode itself:

\\{nxhtml-mode-map}

The minor mode `nxhtml-minor-mode' adds some bindings:

\\{nxhtml-minor-mode-map}

Notice that other minor mode key bindings may also be active, as
well as emulation modes. Do \\[describe-bindings] to get a list
of all active key bindings. Also, *VERY IMPORTANT*, if mumamo is
used in the buffer each mumamo chunk has a different major mode
with different key bindings. You can however still see all
bindings with \\[describe-bindings], but you have to do that with
point in the mumamo chunk you want to know the key bindings in.

---------
* Note: Some of the features supported by this mode are optional
  and available only if other Emacs modules are found.  Use
  \\[nxhtml-features-check] to get a list of these optional
  features and modules needed. You should however have no problem
  with this if you have followed the installation instructions
  for nXhtml.

\(fn)" t nil)

(autoload 'nxhtml-validation-header-mode "../nxhtml/nxhtml" "\
If on use a Fictive XHTML Validation Header for the buffer.
See `nxhtml-set-validation-header' for information about Fictive XHTML Validation Headers.

This mode may be turned on automatically in two ways:
- If you try to do completion of a XHTML tag or attribute then
  `nxthml-mode' may ask you if you want to turn this mode on if
  needed.
- You can also choose to have it turned on automatically whenever
  mumamo is used, see `nxhtml-validation-header-if-mumamo' for
  further information.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads (nxhtml-report-bug) "../nxhtml/nxhtml-bug" "nxhtml/nxhtml-bug.el"
;;;;;;  (18654 33920))
;;; Generated autoloads from nxhtml/nxhtml-bug.el

(autoload 'nxhtml-report-bug "../nxhtml/nxhtml-bug" "\
Report a bug in nXhtml.

\(fn)" t nil)

;;;***

;;;### (autoloads (nxhtml-global-minor-mode nxhtml-minor-mode) "../nxhtml/nxhtml-menu"
;;;;;;  "nxhtml/nxhtml-menu.el" (18671 42692))
;;; Generated autoloads from nxhtml/nxhtml-menu.el

(autoload 'nxhtml-minor-mode "../nxhtml/nxhtml-menu" "\
Minor mode to turn on some key and menu bindings.
See `nxhtml-mode' for more information.

\(fn &optional ARG)" t nil)

(defvar nxhtml-global-minor-mode nil "\
Non-nil if Nxhtml-Global minor mode is enabled.
See the command `nxhtml-global-minor-mode' for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `nxhtml-global-minor-mode'.")

(custom-autoload 'nxhtml-global-minor-mode "../nxhtml/nxhtml-menu" nil)

(autoload 'nxhtml-global-minor-mode "../nxhtml/nxhtml-menu" "\
Toggle Nxhtml minor mode in every possible buffer.
With prefix ARG, turn Nxhtml-Global minor mode on if and only if ARG is positive.
Nxhtml minor mode is enabled in all buffers where `nxhtml-maybe-turn-on-minor-mode' would do it.
See `nxhtml-minor-mode' for more information on Nxhtml minor mode.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads (mako-nxhtml-mumamo-mode asp-nxhtml-mumamo-mode
;;;;;;  eruby-nxhtml-mumamo-mode jsp-nxhtml-mumamo-mode smarty-nxhtml-mumamo-mode
;;;;;;  mjt-nxhtml-mumamo-mode genshi-nxhtml-mumamo-mode django-nxhtml-mumamo-mode
;;;;;;  embperl-nxhtml-mumamo-mode nxhtml-mumamo-mode) "../nxhtml/nxhtml-mumamo"
;;;;;;  "nxhtml/nxhtml-mumamo.el" (18764 14404))
;;; Generated autoloads from nxhtml/nxhtml-mumamo.el

(autoload 'nxhtml-mumamo-mode "../nxhtml/nxhtml-mumamo" "\
Turn on multiple major modes for (X)HTML with main mode `nxhtml-mode'.
This covers inlined style and javascript and PHP." t)

(autoload 'embperl-nxhtml-mumamo-mode "../nxhtml/nxhtml-mumamo" "\
Turn on multiple major modes for Embperl files with main mode `nxhtml-mode'.
This also covers inlined style and javascript." t)

(autoload 'django-nxhtml-mumamo-mode "../nxhtml/nxhtml-mumamo" "\
Turn on multiple major modes for Django with main mode `nxhtml-mode'.
This also covers inlined style and javascript." t)

(autoload 'genshi-nxhtml-mumamo-mode "../nxhtml/nxhtml-mumamo" "\
Turn on multiple major modes for Genshi with main mode `nxhtml-mode'.
This also covers inlined style and javascript." t)

(autoload 'mjt-nxhtml-mumamo-mode "../nxhtml/nxhtml-mumamo" "\
Turn on multiple major modes for MJT with main mode `nxhtml-mode'.
This also covers inlined style and javascript." t)

(autoload 'smarty-nxhtml-mumamo-mode "../nxhtml/nxhtml-mumamo" "\
Turn on multiple major modes for Smarty with main mode `nxhtml-mode'.
This also covers inlined style and javascript." t)

(autoload 'jsp-nxhtml-mumamo-mode "../nxhtml/nxhtml-mumamo" "\
Turn on multiple major modes for JSP with main mode `nxhtml-mode'.
This also covers inlined style and javascript." t)

(autoload 'eruby-nxhtml-mumamo-mode "../nxhtml/nxhtml-mumamo" "\
Turn on multiple major modes for eRuby with main mode `nxhtml-mode'.
This also covers inlined style and javascript." t)

(autoload 'asp-nxhtml-mumamo-mode "../nxhtml/nxhtml-mumamo" "\
Turn on multiple major modes for ASP with main mode `nxhtml-mode'.
This also covers inlined style and javascript." t)

(autoload 'mako-nxhtml-mumamo-mode "../nxhtml/nxhtml-mumamo" "\
Turn on multiple major modes for Mako with main mode `nxhtml-mode'.
This also covers inlined style and javascript." t)

;;;***

;;;### (autoloads (nxml-where-global-mode nxml-where-mode) "../nxhtml/nxml-where"
;;;;;;  "nxhtml/nxml-where.el" (18658 47798))
;;; Generated autoloads from nxhtml/nxml-where.el

(autoload 'nxml-where-mode "../nxhtml/nxml-where" "\
Shows path in mode line.

\(fn &optional ARG)" t nil)

(defvar nxml-where-global-mode nil "\
Non-nil if Nxml-Where-Global mode is enabled.
See the command `nxml-where-global-mode' for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `nxml-where-global-mode'.")

(custom-autoload 'nxml-where-global-mode "../nxhtml/nxml-where" nil)

(autoload 'nxml-where-global-mode "../nxhtml/nxml-where" "\
Toggle Nxml-Where mode in every possible buffer.
With prefix ARG, turn Nxml-Where-Global mode on if and only if ARG is positive.
Nxml-Where mode is enabled in all buffers where `nxml-where-turn-on-in-nxml-child' would do it.
See `nxml-where-mode' for more information on Nxml-Where mode.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads (csharp-mode) "../related/csharp-mode" "related/csharp-mode.el"
;;;;;;  (18283 4168))
;;; Generated autoloads from related/csharp-mode.el

(autoload 'csharp-mode "../related/csharp-mode" "\
Major mode for editing C# (pronounced \"see sharp\") code.
This is a simple example of a separate mode derived from CC Mode to
support a language with syntax similar to C/C++/ObjC/Java/IDL/Pike.

The hook `c-mode-common-hook' is run with no args at mode
initialization, then `csharp-mode-hook'.

Key bindings:
\\{csharp-mode-map}

\(fn)" t nil)

;;;***

;;;### (autoloads (css-mode) "../related/css-mode" "related/css-mode.el"
;;;;;;  (18248 47980))
;;; Generated autoloads from related/css-mode.el
 (add-to-list 'auto-mode-alist '("\\.css\\'" . css-mode))

(autoload 'css-mode "../related/css-mode" "\
Major mode to edit Cascading Style Sheets.

\(fn)" t nil)

;;;***

;;;### (autoloads (django-mode) "../related/django" "related/django.el"
;;;;;;  (18654 33920))
;;; Generated autoloads from related/django.el

(autoload 'django-mode "../related/django" "\
Simple Django mode for use with mumamo.
This mode only provides syntax highlighting.

\(fn)" t nil)

;;;***

;;;### (autoloads (javascript-mode) "../related/javascript" "related/javascript.el"
;;;;;;  (18418 45624))
;;; Generated autoloads from related/javascript.el

(autoload 'javascript-mode "../related/javascript" "\
Major mode for editing JavaScript source text.

Key bindings:

\\{javascript-mode-map}

\(fn)" t nil)

;;;***

;;;### (autoloads (javascript-mode) "../related/javascript-mozlab"
;;;;;;  "related/javascript-mozlab.el" (18562 39536))
;;; Generated autoloads from related/javascript-mozlab.el

(autoload 'javascript-mode "../related/javascript-mozlab" "\
Major mode for editing JavaScript source text.

Key bindings:

\\{javascript-mode-map}

\(fn)" t nil)

;;;***

;;;### (autoloads (js2-mode) "../related/js2" "related/js2.el" (18624
;;;;;;  30036))
;;; Generated autoloads from related/js2.el
 (add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

(autoload 'js2-mode "../related/js2" "\
Major mode for editing JavaScript code.

\(fn)" t nil)

;;;***

;;;### (autoloads (js2-mode) "../related/js2-mumamo" "related/js2-mumamo.el"
;;;;;;  (18496 38616))
;;; Generated autoloads from related/js2-mumamo.el
 (add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

(autoload 'js2-mode "../related/js2-mumamo" "\
Major mode for editing JavaScript code.

\(fn)" t nil)

;;;***

;;;### (autoloads (js2-mode) "../related/js2-new" "related/js2-new.el"
;;;;;;  (18455 47788))
;;; Generated autoloads from related/js2-new.el
 (add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

(autoload 'js2-mode "../related/js2-new" "\
Major mode for editing JavaScript code.

\(fn)" t nil)

;;;***

;;;### (autoloads (inferior-moz-mode moz-minor-mode) "../related/moz"
;;;;;;  "related/moz.el" (18654 33920))
;;; Generated autoloads from related/moz.el

(autoload 'moz-minor-mode "../related/moz" "\
MozRepl minor mode for interaction with Firefox.
With no argument, this command toggles the mode.
Non-null prefix argument turns on the mode.
Null prefix argument turns off the mode.

When this minor mode is enabled, some commands become available
to send current code area (as understood by c-mark-function) or
region or buffer to an inferior MozRepl process (which will be
started as needed).

The following keys are bound in this minor mode:

\\{moz-minor-mode-map}

\(fn &optional ARG)" t nil)

(autoload 'inferior-moz-mode "../related/moz" "\
Major mode for interacting with Firefox via MozRepl.

\(fn)" t nil)

;;;***

;;;### (autoloads (php-mode php-file-patterns) "../related/php-mode"
;;;;;;  "related/php-mode.el" (18735 15066))
;;; Generated autoloads from related/php-mode.el

(defvar php-file-patterns '("\\.php[s34]?\\'" "\\.phtml\\'" "\\.inc\\'") "\
List of file patterns for which to automatically invoke `php-mode'.")

(custom-autoload 'php-file-patterns "../related/php-mode" nil)

(autoload 'php-mode "../related/php-mode" "\
Major mode for editing PHP code.

\\{php-mode-map}

\(fn)" t nil)

;;;***

;;;### (autoloads (php-mode php-file-patterns) "../related/php-mode-2008-10-23"
;;;;;;  "related/php-mode-2008-10-23.el" (18688 64648))
;;; Generated autoloads from related/php-mode-2008-10-23.el

(defvar php-file-patterns '("\\.php[s34]?\\'" "\\.phtml\\'" "\\.inc\\'") "\
List of file patterns for which to automatically invoke `php-mode'.")

(custom-autoload 'php-file-patterns "../related/php-mode-2008-10-23" nil)

(autoload 'php-mode "../related/php-mode-2008-10-23" "\
Major mode for editing PHP code.

\\{php-mode-map}

\(fn)" t nil)

;;;***

;;;### (autoloads (smarty-mode) "../related/smarty-mode" "related/smarty-mode.el"
;;;;;;  (18317 23898))
;;; Generated autoloads from related/smarty-mode.el

(autoload 'smarty-mode "../related/smarty-mode" "\
Mode for editing php smarty files.
Smarty Mode is a GNU XEmacs major mode for editing Smarty templates.

1 Introduction
**************

Smarty-Mode is a mode allowing easy edit of Smarty templates:
highlight, templates, navigation into source files...



Features (new features in bold) :

   * Completion

   * Customizable

   * Highlight

   * Menu

   * Stuttering

   * Templates
        - Built-in Functions

        - User Functions

        - Variable Modifiers

        - Plugin (Functions)
             * Smarty Formtool

             * Smarty Paginate

             * Smarty Validate

        - Plugin (Variable Modifiers)
             * AlternativeDateModifierPlugin

             * B2Smilies

             * BBCodePlugin

        - Fonctions Non-Smarty



This manual describes Smarty Mode version 0.0.4.

3 Customization
***************

This chapter describes the differents parameters and functions that
you can change to customize Smarty Mode.  To do that, open a Smarty
file, click on the Smarty menu and choose Options then Browse
Options....

3.1 Parameters
==============

3.1.1 Mode
----------

Smarty Mode has 2 modes allowing to simplify the writing of Smarty
templates. You can enable/disable each mode individually.

`smarty-electric-mode'
     Type: boolean
     Default value: `t'
     Description: If `t'; enable automatic generation of template.
     If `nil'; template generators can still be invoked through key
     bindings and menu. Is indicated in the modeline by \"/e\" after
     the mode name and can be toggled by `smarty-electric-mode'.

`smarty-stutter-mode'
     Type: boolean
     Default value: `t'
     Description: If `t'; enable the stuttering. Is indicated in the
     modeline by \"/s\" after the mode name and can be toggled by
     `smarty-stutter-mode'.

3.1.2 Menu
----------

Smarty Mode has also 1 menu that you can enable/disable. The menu
Sources is specific to each Smarty files opened.

`smarty-source-file-menu'
     Type: boolean
     Default value: `t'
     Description: If `t'; the Sources menu is enabled. This menu
     contains the list of Smarty file located in the current
     directory. The Sources menu scans the directory when a file is
     opened.

3.1.3 Menu
----------

`smarty-highlight-plugin-functions'
     Type: boolean
     Default value: `t'
     Description: If `t'; the functions described in the smarty
     plugins are highlighted.

3.1.4 Templates
---------------

3.1.4.1 Header
..............

`smarty-file-header'
     Type: string
     Default value: `\"\"'
     Description: String or file to insert as file header. If the
     string specifies an existing file name the contents of the file
     is inserted; otherwise the string itself is inserted as file
     header.
     Type `C-j' for newlines.
     The follonwing keywords are supported:
     <filename>: replaced by the file name.
     <author>: replaced by the user name and email address.
     <login>: replaced by `user-login-name'.
     <company>: replaced by `smarty-company-name' content.
     <date>: replaced by the current date.
     <year>: replaced by the current year.
     <copyright>: replaced by `smarty-copyright-string' content.
     <cursor>: final cursor position.

`smarty-file-footer'
     Type: string
     Default value: `\"\"'
     Description: String or file to insert as file footer.  See
     `smarty-file-header'

`smarty-company-name'
     Type: string
     Default value: `\"\"'
     Description: Name of the company to insert in file header.

`smarty-copyright-string'
     Type: string
     Default value: `\"\"'
     Description: Coryright string to insert in file header.

`smarty-date-format'
     Type: string
     Default value: `\"%Y-%m-%d\"'
     Description: Date format.

`smarty-modify-date-prefix-string'
     Type: string
     Default value: `\"\"'
     Description: Prefix string of modification date in Smarty file
     header.

`smarty-modify-date-on-saving'
     Type: bool
     Default value: `nil'
     Description: If `t'; update the modification date when the
     buffer is saved.

3.1.5 Miscellaneous
-------------------

`smarty-left-delimiter'
     Type: string
     Default value: `\"\"'
     Description: Left escaping delimiter for Smarty templates.

`smarty-right-delimiter'
     Type: string
     Default value: `\"\"'
     Description: Right escaping delimiter for Smarty templates.

`smarty-intelligent-tab'
     Type: bool
     Default value: `t'
     Description: If `t'; TAB does indentation; completion and insert
     tabulations. If `nil'; TAB does only indentation.

`smarty-word-completion-in-minibuffer'
     Type: bool
     Default value: `t'
     Description: If `t'; enable completion in the minibuffer.

`smarty-word-completion-case-sensitive'
     Type: bool
     Default value: `nil'
     Description: If `t'; completion is case sensitive.

3.2 Functions
=============

3.2.1 Mode
----------

`smarty-electric-mode'
     Menu: Smarty -> Options -> Mode -> Electric Mode
     Keybinding: `C-c C-m C-e'
     Description: This functions is used to enable/disable the
     electric mode.

`smarty-stutter-mode'
     Menu: Smarty -> Options -> Mode -> Stutter Mode
     Keybinding: `C-c C-m C-s'
     Description: This function is used to enable/disable the stutter
     mode.

4 Menus
*******

There are 2 menus: Smarty and Sources. All theses menus can be
accessed from the menubar or from the right click. This chapter
describes each menus.

4.1 Smarty
==========

This is the main menu of Smarty Mode. It allows an easy access to the
main features of the Smarty Mode: Templates (see *Note Templates::)
and Options (see *Note Customization::).

This menu contains also 3 functions that are discussed in the next
part.

4.1.1 Functions
---------------
\\<smarty-mode-map>
`smarty-show-messages'
     Menu: Smarty -> Show Messages
     Keybinding: `C-c M-m' \\[smarty-show-messages]
     Description: This function opens the *Messages* buffer to
     display previous error messages.

`smarty-doc-mode'
     Menu: Smarty -> Smarty Mode Documentation
     Keybinding: `C-c C-h' \\[smarty-doc-mode]
     Description: This function opens the *Help* buffer and prints in
     it the Smarty Mode documentation.

`smarty-version'
     Menu: Smarty -> Version
     Keybinding: `C-c C-v'
     Description: This function displays in the minibuffer the
     current Smarty Mode version with the timestamp.

4.2 Sources
===========

The Sources menu shows the Smarty files in the current directory. If
you add or delete a file in the current directory, you need to
refresh the menu.

4.2.1 Customization
-------------------

`smarty-source-file-menu'
     Type: boolean
     Default value: `t'
     Description: If `t'; the Sources menu is enabled. This menu
     contains the list of Smarty file located in the current
     directory. The Sources menu scans the directory when a file is
     opened.

4.2.2 Functions
---------------

`smarty-add-source-files-menu'
     Menu: Sources -> *Rescan*
     Keybinding: `C-c C-s C-u'
     Description: This function is used to refresh the Sources menu.

5 Stuttering
************

The stutter mode is a mode that affects a function to a key. For
example, when you use the `ENTER' key, the associated function will
create a new line and indent it.

5.1 Customization
=================

`smarty-stutter-mode'
     Type: boolean
     Default value: `t'
     Description: If `t'; enable the stuttering. Is indicated in the
     modeline by \"/s\" after the mode name and can be toggled by
     `smarty-stutter-mode'.

5.2 Functions
=============

`SPACE'
     If in comment, indent the comment and add new line if necessary.
     In other case, add a space.

`('
     If the previous character is a `(', the `((' will be replaced by
     `['.
     If the previous character is a `[', the `[(' will be replaced by
     `{'.
     In other case, insert a `('.

`)'
     If the previous character is a `)', the `))' will be replaced by
     `]'.
     If the previous character is a `]', the `])' will be replaced by
     `}'.
     In other case, insert a `)'.

6 Templates
***********

In the Smarty Mode, the Smarty functions (like if, while, for, fopen,
fclose) are predefined in functions called \"Templates\".

Each template can be invoked by the function name or by using the
<SPACE> key after the Smarty function name in the buffer (Note, using
`M-<SPACE>' disable the template).

A template can be aborted by using the `C-g' or by lefting empty the
tempate prompt (in the minibuffer).

6.1 Customization
=================

`smarty-electric-mode'
     Type: boolean
     Default value: `t'
     Description: If `t'; enable automatic generation of template.
     If `nil'; template generators can still be invoked through key
     bindings and menu. Is indicated in the modeline by \"/e\" after
     the mode name and can be toggled by `smarty-electric-mode'.

For a complete description of the template customizable variables,
see *Note Cu01-Pa01-Template::

6.2 Functions
=============

6.2.1 Smarty Functions
----------------------

For Smarty functions, see PDF or HTML documentation.

6.2.2 Non-Smarty Functions
--------------------------

`smarty-template-header'
     Menu: Smarty -> Templates -> Insert Header
     Keybinding: `C-c C-t C-h'
     Description: This function is used to insert a header in the
     current buffer.

`smarty-template-footer'
     Menu: Smarty -> Templates -> Insert Footer
     Keybinding: `C-c C-t C-f'
     Description: This function is used to insert a footer in the
     current buffer.

`smarty-template-insert-date'
     Menu: Smarty -> Templates -> Insert Date
     Keybinding: `C-c C-t C-d i'
     Description: This function is used to insert the date in the
     current buffer.

`smarty-template-modify'
     Menu: Smarty -> Templates -> Modify Date
     Keybinding: `C-c C-t C-d m'
     Description: This function is used to modify the last
     modification date in the current buffer.

7 Bugs, Help
************

   * To report bugs: Bugtracker
     (http://bugtracker.morinie.fr/lisp/set_project.php?project_id=2)

   * To obtain help you can post on the dedicated forum: Forum
     (http://forum.morinie.fr/lisp/)

8 Key bindings
**************

\\{smarty-mode-map}

\(fn)" t nil)

;;;***

;;;### (autoloads (smarty-mode) "../related/smarty-mode-vdebout"
;;;;;;  "related/smarty-mode-vdebout.el" (18295 31440))
;;; Generated autoloads from related/smarty-mode-vdebout.el

(autoload 'smarty-mode "../related/smarty-mode-vdebout" "\
Smarty Mode
***********

Smarty Mode is a GNU XEmacs major mode for editing Smarty templates.

1 Introduction
**************

Smarty-Mode is a mode allowing easy edit of Smarty templates:
highlight, templates, navigation into source files...



Features (new features in bold) :

   * Completion

   * Customizable

   * Highlight

   * Menu

   * Stuttering

   * Templates
        - Built-in Functions

        - User Functions

        - Variable Modifiers

        - Plugin (Functions)
             * Smarty Formtool

             * Smarty Paginate

             * Smarty Validate

        - Plugin (Variable Modifiers)
             * AlternativeDateModifierPlugin

             * B2Smilies

             * BBCodePlugin

        - Fonctions Non-Smarty



This manual describes Smarty Mode version 0.0.4.

2 Installation
**************

2.1 Requirements
================

Smarty Mode is a XEmacs major mode that needs the following
software/packages:

   * XEmacs (http://www.xemacs.org/).

   * `font-lock' mode generaly installed with XEmacs.

   * `assoc' mode generaly installed with XEmacs.

   * `easymenu' mode generaly installed with XEmacs.

   * `hippie-exp' mode generaly installed with XEmacs.

Before continuing, you must be sure to have all this packages
installed.

2.2 Download
============

Two internet address to download Smarty Mode :

   * Principal: Smarty-Mode 0.0.4
     (http://deboutv.free.fr/lisp/smarty/download/smarty-0.0.4.tar.gz)
     (http://deboutv.free.fr/lisp/smarty/)

   * Secondary: Smarty-Mode 0.0.4
     (http://www.morinie.fr/lisp/smarty/download/smarty-0.0.4.tar.gz)
     (http://www.morinie.fr/lisp/smarty/)

   * Old releases: Smarty-Mode
     (http://deboutv.free.fr/lisp/smarty/download.php)
     (http://deboutv.free.fr/lisp/smarty/)

2.3 Installation
================

2.3.1 Installation
------------------

To install Smarty Mode you need to choose an installation directory
\(for example `/usr/local/share/lisp' or `c:lisp'). The administrator
must have the write rights on this directory.

With your favorite unzip software, unzip the archive in the
installation directory.

Example:
     cd /usr/local/share/lisp
     tar zxvf smarty-0.0.4.tar.gz
Now you have a `smarty' directory in the installation directory. This
directory contains 2 files `smarty-mode.el' and `smarty-mode.elc' and
another directory `docs' containing the documentation.

You need to configure XEmacs. open you initialization file `init.el'
\(open the file or start XEmacs then choose the Options menu and Edit
Init File). Add the following lines (the installation directory in
this example is `/usr/local/share/lisp') :

     (setq load-path
           (append (list \"/usr/local/share/lisp/\") load-path))
     (autoload 'smarty-mode \"smarty-mode\" \"Smarty Mode\" t)

2.3.2 Update
------------

The update is easy. You need to unzip the archive in the installation
directory to remove the old release.

Example:
     cd /usr/local/share/lisp
     rm -rf smarty
     tar zxvf smarty-0.0.4.tar.gz

2.4 Invoke Smarty-Mode
======================

You have two possibilities to invoke the Smarty Mode.

   - Manually: At each file opening you need to launch Smarty Mode
     with the following command:

     `M-x smarty-mode'

   - Automatically: Add the following linesin your initialization
     file `init.el' :

          (setq auto-mode-alist
                (append
                 '((\"\\.tpl$\" . smarty-mode))
          	 auto-mode-alist))


3 Customization
***************

This chapter describes the differents parameters and functions that
you can change to customize Smarty Mode.  To do that, open a Smarty
file, click on the Smarty menu and choose Options then Browse
Options....

3.1 Parameters
==============

3.1.1 Mode
----------

Smarty Mode has 2 modes allowing to simplify the writing of Smarty
templates. You can enable/disable each mode individually.

`smarty-electric-mode'
     Type: boolean
     Default value: `t'
     Description: If `t'; enable automatic generation of template.
     If `nil'; template generators can still be invoked through key
     bindings and menu. Is indicated in the modeline by \"/e\" after
     the mode name and can be toggled by `smarty-electric-mode'.

`smarty-stutter-mode'
     Type: boolean
     Default value: `t'
     Description: If `t'; enable the stuttering. Is indicated in the
     modeline by \"/s\" after the mode name and can be toggled by
     `smarty-stutter-mode'.

3.1.2 Menu
----------

Smarty Mode has also 1 menu that you can enable/disable. The menu
Sources is specific to each Smarty files opened.

`smarty-source-file-menu'
     Type: boolean
     Default value: `t'
     Description: If `t'; the Sources menu is enabled. This menu
     contains the list of Smarty file located in the current
     directory. The Sources menu scans the directory when a file is
     opened.

3.1.3 Menu
----------

`smarty-highlight-plugin-functions'
     Type: boolean
     Default value: `t'
     Description: If `t'; the functions described in the smarty
     plugins are highlighted.

3.1.4 Templates
---------------

3.1.4.1 Header
..............

`smarty-file-header'
     Type: string
     Default value: `\"\"'
     Description: String or file to insert as file header. If the
     string specifies an existing file name the contents of the file
     is inserted; otherwise the string itself is inserted as file
     header.
     Type `C-j' for newlines.
     The follonwing keywords are supported:
     <filename>: replaced by the file name.
     <author>: replaced by the user name and email address.
     <login>: replaced by `user-login-name'.
     <company>: replaced by `smarty-company-name' content.
     <date>: replaced by the current date.
     <year>: replaced by the current year.
     <copyright>: replaced by `smarty-copyright-string' content.
     <cursor>: final cursor position.

`smarty-file-footer'
     Type: string
     Default value: `\"\"'
     Description: String or file to insert as file footer.  See
     `smarty-file-header'

`smarty-company-name'
     Type: string
     Default value: `\"\"'
     Description: Name of the company to insert in file header.

`smarty-copyright-string'
     Type: string
     Default value: `\"\"'
     Description: Coryright string to insert in file header.

`smarty-date-format'
     Type: string
     Default value: `\"%Y-%m-%d\"'
     Description: Date format.

`smarty-modify-date-prefix-string'
     Type: string
     Default value: `\"\"'
     Description: Prefix string of modification date in Smarty file
     header.

`smarty-modify-date-on-saving'
     Type: bool
     Default value: `nil'
     Description: If `t'; update the modification date when the
     buffer is saved.

3.1.5 Miscellaneous
-------------------

`smarty-left-delimiter'
     Type: string
     Default value: `\"\"'
     Description: Left escaping delimiter for Smarty templates.

`smarty-right-delimiter'
     Type: string
     Default value: `\"\"'
     Description: Right escaping delimiter for Smarty templates.

`smarty-intelligent-tab'
     Type: bool
     Default value: `t'
     Description: If `t'; TAB does indentation; completion and insert
     tabulations. If `nil'; TAB does only indentation.

`smarty-word-completion-in-minibuffer'
     Type: bool
     Default value: `t'
     Description: If `t'; enable completion in the minibuffer.

`smarty-word-completion-case-sensitive'
     Type: bool
     Default value: `nil'
     Description: If `t'; completion is case sensitive.

3.2 Functions
=============

3.2.1 Mode
----------

`smarty-electric-mode'
     Menu: Smarty -> Options -> Mode -> Electric Mode
     Keybinding: `C-c C-m C-e'
     Description: This functions is used to enable/disable the
     electric mode.

`smarty-stutter-mode'
     Menu: Smarty -> Options -> Mode -> Stutter Mode
     Keybinding: `C-c C-m C-s'
     Description: This function is used to enable/disable the stutter
     mode.

4 Menus
*******

There are 2 menus: Smarty and Sources. All theses menus can be
accessed from the menubar or from the right click. This chapter
describes each menus.

4.1 Smarty
==========

This is the main menu of Smarty Mode. It allows an easy access to the
main features of the Smarty Mode: Templates (see *Note Templates::)
and Options (see *Note Customization::).

This menu contains also 3 functions that are discussed in the next
part.

4.1.1 Functions
---------------

`smarty-show-messages'
     Menu: Smarty -> Show Messages
     Keybinding: `C-c M-m'
     Description: This function opens the *Messages* buffer to
     display previous error messages.

`smarty-doc-mode'
     Menu: Smarty -> Smarty Mode Documentation
     Keybinding: `C-c C-h'
     Description: This function opens the *Help* buffer and prints in
     it the Smarty Mode documentation.

`smarty-version'
     Menu: Smarty -> Version
     Keybinding: `C-c C-v'
     Description: This function displays in the minibuffer the
     current Smarty Mode version with the timestamp.

4.2 Sources
===========

The Sources menu shows the Smarty files in the current directory. If
you add or delete a file in the current directory, you need to
refresh the menu.

4.2.1 Customization
-------------------

`smarty-source-file-menu'
     Type: boolean
     Default value: `t'
     Description: If `t'; the Sources menu is enabled. This menu
     contains the list of Smarty file located in the current
     directory. The Sources menu scans the directory when a file is
     opened.

4.2.2 Functions
---------------

`smarty-add-source-files-menu'
     Menu: Sources -> *Rescan*
     Keybinding: `C-c C-s C-u'
     Description: This function is used to refresh the Sources menu.

5 Stuttering
************

The stutter mode is a mode that affects a function to a key. For
example, when you use the `ENTER' key, the associated function will
create a new line and indent it.

5.1 Customization
=================

`smarty-stutter-mode'
     Type: boolean
     Default value: `t'
     Description: If `t'; enable the stuttering. Is indicated in the
     modeline by \"/s\" after the mode name and can be toggled by
     `smarty-stutter-mode'.

5.2 Functions
=============

`SPACE'
     If in comment, indent the comment and add new line if necessary.
     In other case, add a space.

`('
     If the previous character is a `(', the `((' will be replaced by
     `['.
     If the previous character is a `[', the `[(' will be replaced by
     `{'.
     In other case, insert a `('.

`)'
     If the previous character is a `)', the `))' will be replaced by
     `]'.
     If the previous character is a `]', the `])' will be replaced by
     `}'.
     In other case, insert a `)'.

6 Templates
***********

In the Smarty Mode, the Smarty functions (like if, while, for, fopen,
fclose) are predefined in functions called \"Templates\".

Each template can be invoked by the function name or by using the
<SPACE> key after the Smarty function name in the buffer (Note, using
`M-<SPACE>' disable the template).

A template can be aborted by using the `C-g' or by lefting empty the
tempate prompt (in the minibuffer).

6.1 Customization
=================

`smarty-electric-mode'
     Type: boolean
     Default value: `t'
     Description: If `t'; enable automatic generation of template.
     If `nil'; template generators can still be invoked through key
     bindings and menu. Is indicated in the modeline by \"/e\" after
     the mode name and can be toggled by `smarty-electric-mode'.

For a complete description of the template customizable variables,
see *Note Cu01-Pa01-Template::

6.2 Functions
=============

6.2.1 Smarty Functions
----------------------

For Smarty functions, see PDF or HTML documentation.

6.2.2 Non-Smarty Functions
--------------------------

`smarty-template-header'
     Menu: Smarty -> Templates -> Insert Header
     Keybinding: `C-c C-t C-h'
     Description: This function is used to insert a header in the
     current buffer.

`smarty-template-footer'
     Menu: Smarty -> Templates -> Insert Footer
     Keybinding: `C-c C-t C-f'
     Description: This function is used to insert a footer in the
     current buffer.

`smarty-template-insert-date'
     Menu: Smarty -> Templates -> Insert Date
     Keybinding: `C-c C-t C-d i'
     Description: This function is used to insert the date in the
     current buffer.

`smarty-template-modify'
     Menu: Smarty -> Templates -> Modify Date
     Keybinding: `C-c C-t C-d m'
     Description: This function is used to modify the last
     modification date in the current buffer.

7 Bugs, Help
************

   * To report bugs: Bugtracker
     (http://bugtracker.morinie.fr/lisp/set_project.php?project_id=2)

   * To obtain help you can post on the dedicated forum: Forum
     (http://forum.morinie.fr/lisp/)

8 Key bindings
**************

\\{smarty-mode-map}

\(fn)" t nil)

;;;***

;;;### (autoloads (tt-mode) "../related/tt-mode" "related/tt-mode.el"
;;;;;;  (18603 15792))
;;; Generated autoloads from related/tt-mode.el

(autoload 'tt-mode "../related/tt-mode" "\
Major mode for editing Template Toolkit files.

\(fn)" t nil)

;;;***

;;;### (autoloads (wikipedia-draft-buffer wikipedia-draft-page wikipedia-draft
;;;;;;  wikipedia-mode) "../related/wikipedia-mode" "related/wikipedia-mode.el"
;;;;;;  (18401 44650))
;;; Generated autoloads from related/wikipedia-mode.el

(autoload 'wikipedia-mode "../related/wikipedia-mode" "\
Major mode for editing wikimedia style wikis.
Major mode for editing articles written in the markup language
used by Wikipedia, the free on-line
encyclopedia (see URL `http://www.wikipedia.org').

There are several ways to use wikipedia-mode:

- You can simply cut and paste articles between Emacs and your
  web browser's text box.
- If you are using Firefox you can use the It's All Text add-on
  for Firefox.
- You can use MozEx, a Mozilla/Firefox web browser extension that
  allows you to call Emacs from a text
  box (see URL `http://mozex.mozdev.org/').
- Another way is to use the PERL script ee-helper, which allows
  you to up and download wiki texts.

Wikipedia articles are usually unfilled: newline characters are not
used for breaking paragraphs into lines. Unfortunately, Emacs does not
handle word wrapping yet. As a workaround, wikipedia-mode turns on
longlines-mode automatically. In case something goes wrong, the
following commands may come in handy:

\\[wikipedia-fill-article] fills the buffer.
\\[wikipedia-unfill-article] unfills the buffer.
Be warned that function can be dead  slow, better use wikipedia-unfill-paragraph-or-region.
\\[wikipedia-unfill-paragraph-or-region] unfills the paragraph
\\[wikipedia-unfill-paragraph-simple] doehe same but simpler.



The following commands put in markup structures.

\\[wikipedia-insert-bold-italic] bold+italic
\\[wikipedia-insert-bold] bold text
\\[wikipedia-insert-italics] italics
\\[wikipedia-insert-nowiki] no wiki markup
\\[wikipedia-insert-link-wiki] inserts a link

The following commands are also defined:
\\[wikipedia-insert-user] inserts user name
\\[wikipedia-insert-signature] inserts ~~~~
\\[wikipedia-insert-enumerate] inserts enumerate type structures
\\[wikipedia-insert-itemize] inserts itemize type structures
\\[wikipedia-insert-hline] inserts a hline

The draft functionality
\\[wikipedia-draft]
\\[wikipedia-draft-region]
\\[wikipedia-draft-view-draft]
\\[wikipedia-draft-page]
\\[wikipedia-draft-buffer]

Replying and sending functionality
\\[wikipedia-reply-at-point-simple]
\\[wikipedia-draft-reply]


The register functionality
\\[wikipedia-copy-page-to-register]
\\[defun wikipedia-insert-page-to-register]


Some simple editing commands.
\\[wikipedia-enhance-indent]
\\[wikipedia-yank-prefix]
\\[wikipedia-unfill-paragraph-or-region]



\\[wikipedia-terminate-paragraph]     starts a new list item or paragraph in a context-aware manner.

\(fn)" t nil)

(autoload 'wikipedia-draft "../related/wikipedia-mode" "\
Open a temporary buffer in wikipedia mode for editing an
 wikipedia draft, which an arbitrary piece of data. After
 finishing the editing either use \\[wikipedia-draft-buffer] to
 send the data into the wikipedia-draft-data-file, or send the
 buffer using `wikipedia-draft-send-to-mozex' and insert it later
 into a wikipedia article.

\(fn)" t nil)

(autoload 'wikipedia-draft-page "../related/wikipedia-mode" "\
Not documented

\(fn)" t nil)

(autoload 'wikipedia-draft-buffer "../related/wikipedia-mode" "\
Wikipedia-draft-buffer sends the contents of the current (temporary)
buffer to the wikipedia-draft-buffer, see the variable
wikipedia-draft-data-file.

\(fn)" t nil)

(defvar wikipedia-draft-send-archive t "\
*Archive the reply.")

;;;***

;;;### (autoloads (ert-run-tests-interactively ert-deftest) "../tests/ert"
;;;;;;  "tests/ert.el" (18614 1898))
;;; Generated autoloads from tests/ert.el

(autoload 'ert-deftest "../tests/ert" "\
Define NAME (a symbol) as a test.

\(fn NAME () [:documentation DOCSTRING] [:expected-result TYPE] BODY...)" nil (quote macro))

(autoload 'ert-run-tests-interactively "../tests/ert" "\
Run the tests specified by SELECTOR and display the results in a buffer.

\(fn SELECTOR &optional OUTPUT-BUFFER-NAME MESSAGE-FN)" t nil)

;;;***

;;;### (autoloads (nxhtmltest-run-Q) "../tests/nxhtmltest-Q" "tests/nxhtmltest-Q.el"
;;;;;;  (18762 63688))
;;; Generated autoloads from tests/nxhtmltest-Q.el

(autoload 'nxhtmltest-run-Q "../tests/nxhtmltest-Q" "\
Run all tests defined for nXhtml in fresh Emacs.
See `nxhtmltest-run' for more information about the tests.

\(fn)" t nil)

;;;***

;;;### (autoloads (nxhtmltest-run nxhtmltest-run-indent) "../tests/nxhtmltest-suites"
;;;;;;  "tests/nxhtmltest-suites.el" (18764 7444))
;;; Generated autoloads from tests/nxhtmltest-suites.el

(autoload 'nxhtmltest-run-indent "../tests/nxhtmltest-suites" "\
Run indentation tests.

\(fn)" t nil)

(autoload 'nxhtmltest-run "../tests/nxhtmltest-suites" "\
Run all tests defined for nXhtml.
Currently there are only tests using ert.el defined.

Note that it is currently expected that the following tests will
fail (they corresponds to known errors in nXhtml/Emacs):

  `nxhtml-ert-nxhtml-changes-jump-back-10549'
  `nxhtml-ert-nxhtml-changes-jump-back-7014'

\(fn)" t nil)

;;;***

;;;### (autoloads (as-external-mode as-external-for-wiki as-external-for-mail
;;;;;;  as-external-for-xhtml) "as-external" "util/as-external.el"
;;;;;;  (18686 30854))
;;; Generated autoloads from util/as-external.el

(autoload 'as-external-for-xhtml "as-external" "\
Setup for Firefox addon It's All Text to edit XHTML.
It's All Text is a Firefox add-on for editing textareas with an
external editor.
See URL `https://addons.mozilla.org/en-US/firefox/addon/4125'.

In this case Emacs is used to edit textarea fields on a web page.
The text will most often be part of a web page later, like on a
blog.  Therefore turn on these:

- `nxhtml-mumamo-mode' since some XHTML tags may be allowed.
- `nxhtml-validation-header-mode' since it is not a full page.
- `wrap-to-fill-column-mode' to see what you are writing.
- `html-write-mode' to see it even better.

Also bypass the question for line end conversion when using
emacsw32-eol.

\(fn)" t nil)

(autoload 'as-external-for-mail "as-external" "\
Setup for Firefox addon It's All Text to edit mail.

- `text-mode' since some XHTML tags may be allowed.
- `wrap-to-fill-column-mode' to see what you are writing.
- `as-external-mail-comment-mode' for commenting/uncommenting.

See also `as-external-mode'.

\(fn)" t nil)

(autoload 'as-external-for-wiki "as-external" "\
Setup for Firefox addon It's All Text to edit MediaWikis.

\(fn)" t nil)

(defvar as-external-mode nil "\
Non-nil if As-External mode is enabled.
See the command `as-external-mode' for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `as-external-mode'.")

(custom-autoload 'as-external-mode "as-external" nil)

(autoload 'as-external-mode "as-external" "\
If non-nil check for if Emacs is used as external editor.
When Emacs is used as an external editor for example to edit text
areas on a web page viewed with Firefox this library tries to
help to setup the buffer in a useful way. It may for example set
major and minor modes for the buffer.

See `as-external-alist' for more information.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads (freemind-to-org-mode freemind-from-org-sparse-tree
;;;;;;  freemind-from-org-mode freemind-from-org-mode-node freemind-show)
;;;;;;  "freemind" "util/freemind.el" (18659 17016))
;;; Generated autoloads from util/freemind.el

(autoload 'freemind-show "freemind" "\
Show file MM-FILE in Freemind.

\(fn MM-FILE)" t nil)

(autoload 'freemind-from-org-mode-node "freemind" "\
Convert node at line NODE-LINE to the FreeMind file MM-FILE.

\(fn NODE-LINE MM-FILE)" t nil)

(autoload 'freemind-from-org-mode "freemind" "\
Convert the `org-mode' file ORG-FILE to the FreeMind file MM-FILE.

\(fn ORG-FILE MM-FILE)" t nil)

(autoload 'freemind-from-org-sparse-tree "freemind" "\
Convert visible part of buffer ORG-BUFFER to FreeMind file MM-FILE.

\(fn ORG-BUFFER MM-FILE)" t nil)

(autoload 'freemind-to-org-mode "freemind" "\
Convert FreeMind file MM-FILE to `org-mode' file ORG-FILE.

\(fn MM-FILE ORG-FILE)" t nil)

;;;***

;;;### (autoloads (gimp-can-edit gimp-edit-buffer gimp-edit-file)
;;;;;;  "gimp" "util/gimp.el" (18662 2982))
;;; Generated autoloads from util/gimp.el

(autoload 'gimp-edit-file "gimp" "\
Edit IMAGE-FILE with GIMP.

\(fn IMAGE-FILE)" t nil)

(autoload 'gimp-edit-buffer "gimp" "\
Edit image file in current buffer with GIMP.

\(fn)" t nil)

(autoload 'gimp-can-edit "gimp" "\
Not documented

\(fn FILE-NAME)" nil nil)

;;;***

;;;### (autoloads (html-write-mode) "html-write" "util/html-write.el"
;;;;;;  (18692 35034))
;;; Generated autoloads from util/html-write.el

(autoload 'html-write-mode "html-write" "\
Minor mode for convenient display of some HTML tags.
When this mode is on a tag in `html-write-tag-list' is displayed as
the inner text of the tag with a face corresponding to the tag.
By default for example <i>...</i> is displayed as italic and
<a>...</a> is displayed as an underlined clickable link.

Only non-nested tags are hidden.  The idea is just that it should
be easier to read and write, not that it should look as html
rendered text.

See the customization group `html-write' for more information about
faces.

IMPORTANT: Most commands you use works also on the text that is
hidden.  The movement commands is an exception, but as soon as
you edit the buffer you may also change the hidden parts.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads (majmodpri-apply-priorities majmodpri-sort-lists
;;;;;;  majmodpri) "majmodpri" "util/majmodpri.el" (18654 33920))
;;; Generated autoloads from util/majmodpri.el

(let ((loads (get 'majmodpri 'custom-loads))) (if (member '"majmodpri" loads) nil (put 'majmodpri 'custom-loads (cons '"majmodpri" loads))))

(autoload 'majmodpri-sort-lists "majmodpri" "\
Sort the list used when selecting major mode.
Only sort those lists choosen in `majmodpri-lists-to-sort'.
Sort according to priorities in `majmodpri-mode-priorities'.
Keep the old order in the list otherwise.

The lists can be sorted when loading elisp libraries, see
`majmodpri-sort-after-load'.

See also `majmodpri-apply-priorities'.

\(fn)" t nil)

(autoload 'majmodpri-apply-priorities "majmodpri" "\
Apply major mode priorities.
First run `majmodpri-sort-lists' and then if CHANGE-MODES is
non-nil apply to existing file buffers.  If interactive ask
before applying.

\(fn CHANGE-MODES)" t nil)

;;;***

;;;### (autoloads (mlinks-mode) "mlinks" "util/mlinks.el" (18685
;;;;;;  3334))
;;; Generated autoloads from util/mlinks.el

(autoload 'mlinks-mode "mlinks" "\
Recognizes certain parts of a buffer as hyperlinks.
The hyperlinks are created in different ways for different major
modes with the help of the functions in the list
`mlinks-mode-functions'.

The hyperlinks can be hilighted when point is over them.  Use
`mlinks-toggle-hilight' to toggle this feature for the current
buffer.

All keybindings in this mode are by default done under the prefi§x
key

  C-c RET

which is supposed to be a kind of mnemonic for link (alluding to
the RET key commonly used in web browser to follow a link).
\(Unfortunately this breaks the rules in info node `Key Binding
Conventions'.) Below are the key bindings defined by this mode:

\\{mlinks-mode-map}

For some major modes `mlinks-backward-link' and
`mlinks-forward-link' will take you to the previous/next link.
By default the link moved to will be active, see
`mlinks-active-links'.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads (mako-html-mumamo-mode org-mumamo-mode asp-html-mumamo-mode
;;;;;;  noweb2-mumamo-mode csound-sgml-mumamo-mode laszlo-nxml-mumamo-mode
;;;;;;  metapost-mumamo-mode cperl-mumamo-mode perl-mumamo-mode eruby-html-mumamo-mode
;;;;;;  eruby-mumamo-mode jsp-html-mumamo-mode smarty-html-mumamo-mode
;;;;;;  mjt-html-mumamo-mode genshi-html-mumamo-mode django-html-mumamo-mode
;;;;;;  embperl-html-mumamo-mode nxml-mumamo-mode html-mumamo-mode)
;;;;;;  "mumamo-fun" "util/mumamo-fun.el" (18764 14316))
;;; Generated autoloads from util/mumamo-fun.el

(autoload 'html-mumamo-mode "mumamo-fun" "\
Turn on multiple major modes for (X)HTML with main mode `html-mode'.
This covers inlined style and javascript and PHP." t)

(autoload 'nxml-mumamo-mode "mumamo-fun" "\
Turn on multiple major modes for (X)HTML with main mode `nxml-mode'.
This covers inlined style and javascript and PHP." t)

(autoload 'embperl-html-mumamo-mode "mumamo-fun" "\
Turn on multiple major modes for Embperl files with main mode `html-mode'.
This also covers inlined style and javascript." t)

(autoload 'django-html-mumamo-mode "mumamo-fun" "\
Turn on multiple major modes for Django with main mode `html-mode'.
This also covers inlined style and javascript." t)

(autoload 'genshi-html-mumamo-mode "mumamo-fun" "\
Turn on multiple major modes for Genshi with main mode `html-mode'.
This also covers inlined style and javascript.

Note: You will currently get fontification errors if you use
python chunks

  {% python ... %}

The reason is that the chunk routines currently do not know when
to just look for the } or %} endings.  However this should not
affect your editing normally." t)

(autoload 'mjt-html-mumamo-mode "mumamo-fun" "\
Turn on multiple major modes for MJT with main mode `html-mode'.
This also covers inlined style and javascript." t)

(autoload 'smarty-html-mumamo-mode "mumamo-fun" "\
Turn on multiple major modes for Smarty with main mode `html-mode'.
This also covers inlined style and javascript." t)

(autoload 'jsp-html-mumamo-mode "mumamo-fun" "\
Turn on multiple major modes for JSP with main mode `html-mode'.
This also covers inlined style and javascript." t)

(autoload 'eruby-mumamo-mode "mumamo-fun" "\
Turn on multiple major mode for eRuby with unspecified main mode.
Current major-mode will be used as the main major mode." t)

(autoload 'eruby-html-mumamo-mode "mumamo-fun" "\
Turn on multiple major modes for eRuby with main mode `html-mode'.
This also covers inlined style and javascript." t)

(autoload 'perl-mumamo-mode "mumamo-fun" "\
Turn on multiple major modes for Perl Here Document." t)

(autoload 'cperl-mumamo-mode "mumamo-fun" "\
Turn on multiple major modes for Perl Here Document." t)

(autoload 'metapost-mumamo-mode "mumamo-fun" "\
Turn on multiple major modes for MetaPost." t)

(autoload 'laszlo-nxml-mumamo-mode "mumamo-fun" "\
Turn on multiple major modes for OpenLaszlo." t)

(autoload 'csound-sgml-mumamo-mode "mumamo-fun" "\
Turn on mutiple major modes for CSound orc/sco Modes." t)

(autoload 'noweb2-mumamo-mode "mumamo-fun" "\
Multi major mode for noweb files." t)

(autoload 'asp-html-mumamo-mode "mumamo-fun" "\
Turn on multiple major modes for ASP with main mode `html-mode'.
This also covers inlined style and javascript." t)

(autoload 'org-mumamo-mode "mumamo-fun" "\
Turn on multiple major modes for `org-mode' files with main mode `org-mode'.
Unfortunately this only allows `html-mode' (not `nxhtml-mode') in
sub chunks." t)

(autoload 'mako-html-mumamo-mode "mumamo-fun" "\
Turn on multiple major modes for Mako with main mode `html-mode'.
This also covers inlined style and javascript." t)

;;;***

;;;### (autoloads (info-open-file grep-query-replace emacs-Q-nxhtml
;;;;;;  emacs-Q emacs--debug-init emacs-buffer-file emacs ourcomments-ido-buffer-raise-frame
;;;;;;  ourcomments-ido-buffer-other-frame ourcomments-ido-buffer-other-window
;;;;;;  describe-symbol describe-defstruct describe-custom-group
;;;;;;  describe-command ourcomments-ediff-files find-emacs-other-file
;;;;;;  better-fringes-mode wrap-to-fill-column-mode wrap-to-fill-left-marg-modes
;;;;;;  wrap-to-fill-left-marg describe-key-and-map-briefly ourcomments-move-end-of-line
;;;;;;  ourcomments-move-beginning-of-line major-modep multi-major-modep
;;;;;;  major-or-multi-majorp unfill-individual-paragraphs unfill-region
;;;;;;  unfill-paragraph define-toggle popup-menu-at-point) "ourcomments-util"
;;;;;;  "util/ourcomments-util.el" (18764 17429))
;;; Generated autoloads from util/ourcomments-util.el

(autoload 'popup-menu-at-point "ourcomments-util" "\
Popup the given menu at point.
This is similar to `popup-menu' and MENU and PREFIX has the same
meaning as there.  The position for the popup is however where
the window point is.

\(fn MENU &optional PREFIX)" nil nil)

(autoload 'define-toggle "ourcomments-util" "\
Declare SYMBOL as a customizable variable with a toggle function.
The purpose of this macro is to define a defcustom and a toggle
function suitable for use in a menu.

The arguments have the same meaning as for `defcustom' with these
restrictions:

- The :type keyword cannot be used.  Type is always 'boolean.
- VALUE must be t or nil.

DOC and ARGS are just passed to `defcustom'.

A `defcustom' named SYMBOL with doc-string DOC and a function
named SYMBOL-toggle is defined.  The function toggles the value
of SYMBOL.  It takes no parameters.

To create a menu item something similar to this can be used:

    (define-key map [SYMBOL]
      (list 'menu-item \"Toggle nice SYMBOL\"
            'SYMBOL-toggle
            :button '(:toggle . SYMBOL)))

\(fn SYMBOL VALUE DOC &rest ARGS)" nil (quote macro))

(autoload 'unfill-paragraph "ourcomments-util" "\
Unfill the current paragraph.

\(fn)" t nil)

(autoload 'unfill-region "ourcomments-util" "\
Unfill the current region.

\(fn)" t nil)

(autoload 'unfill-individual-paragraphs "ourcomments-util" "\
Unfill individual paragraphs in the current region.

\(fn)" t nil)

(autoload 'major-or-multi-majorp "ourcomments-util" "\
Not documented

\(fn VALUE)" nil nil)

(autoload 'multi-major-modep "ourcomments-util" "\
Return t if VALUE is a multi major mode function.

\(fn VALUE)" nil nil)

(autoload 'major-modep "ourcomments-util" "\
Return t if VALUE is a major mode function.

\(fn VALUE)" nil nil)

(define-widget 'major-mode-function 'function "\
A major mode lisp function." :complete-function (lambda nil (interactive) (lisp-complete-symbol (quote major-or-multi-majorp))) :prompt-match (quote major-or-multi-majorp) :prompt-history (quote widget-function-prompt-value-history) :match-alternatives (quote (major-or-multi-majorp)) :validate (lambda (widget) (unless (major-or-multi-majorp (widget-value widget)) (widget-put widget :error (format "Invalid function: %S" (widget-value widget))) widget)) :value (quote fundamental-mode) :tag "Major mode function")

(autoload 'ourcomments-move-beginning-of-line "ourcomments-util" "\
Move point to beginning of line or indentation.
See `beginning-of-line' for ARG.

If `physical-line-mode' is on then the visual line beginning is
first tried.

\(fn ARG)" t nil)

(autoload 'ourcomments-move-end-of-line "ourcomments-util" "\
Move point to end of line or indentation.
See `end-of-line' for ARG.

If `physical-line-mode' is on then the visual line ending is
first tried.

\(fn ARG)" t nil)

(autoload 'describe-key-and-map-briefly "ourcomments-util" "\
Try to print names of keymap from which KEY fetch its definition.
Look in current active keymaps and find keymap variables with the
same value as the keymap where KEY is bound.  Print a message
with those keymap variable names.  Return a list with the keymap
variable symbols.

When called interactively prompt for KEY.

INSERT and UNTRANSLATED should normall be nil (and I am not sure
what they will do ;-).

\(fn &optional KEY INSERT UNTRANSLATED)" t nil)

(defvar wrap-to-fill-left-marg nil "\
Left margin handling for `wrap-to-fill-column-mode'.
Used by `wrap-to-fill-column-mode'. If nil then center the
display columns. Otherwise it should be a number which will be
the left margin.")

(custom-autoload 'wrap-to-fill-left-marg "ourcomments-util" t)

(defvar wrap-to-fill-left-marg-modes '(text-mode fundamental-mode) "\
Major modes where `wrap-to-fill-left-margin' may be nil.")

(custom-autoload 'wrap-to-fill-left-marg-modes "ourcomments-util" t)

(autoload 'wrap-to-fill-column-mode "ourcomments-util" "\
Use `fill-column' display columns in buffer windows.
By default the display columns are centered, but see the option
`wrap-to-fill-left-marg'.

Note 1: When turning this on `visual-line-mode' is also turned on. This
is not reset when turning off this mode.

Note 2: The text property `wrap-prefix' is set by this mode to
indent continuation lines.  This is not recorded in the undo
list.

Key bindings added by this minor mode:

\\{wrap-to-fill-column-mode-map}

\(fn &optional ARG)" t nil)

(defvar better-fringes-mode nil "\
Non-nil if Better-Fringes mode is enabled.
See the command `better-fringes-mode' for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `better-fringes-mode'.")

(custom-autoload 'better-fringes-mode "ourcomments-util" nil)

(autoload 'better-fringes-mode "ourcomments-util" "\
Choose another fringe bitmap color and bottom angle.

\(fn &optional ARG)" t nil)

(autoload 'find-emacs-other-file "ourcomments-util" "\
Find corresponding file to source or installed elisp file.
If you have checked out and compiled Emacs yourself you may have
Emacs lisp files in two places, the checked out source tree and
the installed Emacs tree.  If buffer contains an Emacs elisp file
in one of these places then find the corresponding elisp file in
the other place. Return the file name of this file.

When DISPLAY-FILE is non-nil display this file in other window
and go to the same line number as in the current buffer.

\(fn DISPLAY-FILE)" t nil)

(autoload 'ourcomments-ediff-files "ourcomments-util" "\
In directory DEF-DIR run `ediff-files' on files FILE-A and FILE-B.
The purpose of this function is to make it eaiser to start
`ediff-files' from a shell through Emacs Client.

This is used in EmacsW32 in the file ediff.cmd.

\(fn DEF-DIR FILE-A FILE-B)" nil nil)

(autoload 'describe-command "ourcomments-util" "\
Like `describe-function', but prompts only for interactive commands.

\(fn COMMAND)" t nil)

(autoload 'describe-custom-group "ourcomments-util" "\
Describe customization group SYMBOL.

\(fn SYMBOL)" t nil)

(autoload 'describe-defstruct "ourcomments-util" "\
Not documented

\(fn SYMBOL)" t nil)

(autoload 'describe-symbol "ourcomments-util" "\
Show information about SYMBOL.
Show SYMBOL plist and whether is is a variable or/and a
function.

\(fn SYMBOL)" t nil)

(autoload 'ourcomments-ido-buffer-other-window "ourcomments-util" "\
Show buffer in other window.

\(fn)" t nil)

(autoload 'ourcomments-ido-buffer-other-frame "ourcomments-util" "\
Show buffer in other frame.

\(fn)" t nil)

(autoload 'ourcomments-ido-buffer-raise-frame "ourcomments-util" "\
Raise frame showing buffer.

\(fn)" t nil)

(autoload 'emacs "ourcomments-util" "\
Start a new Emacs.

\(fn)" t nil)

(autoload 'emacs-buffer-file "ourcomments-util" "\
Start a new Emacs showing current buffer file.
If there is no buffer file start with `dired'.

\(fn)" t nil)

(autoload 'emacs--debug-init "ourcomments-util" "\
Not documented

\(fn)" t nil)

(autoload 'emacs-Q "ourcomments-util" "\
Start new Emacs without any customization whatsoever.

\(fn)" t nil)

(autoload 'emacs-Q-nxhtml "ourcomments-util" "\
Start new Emacs with -Q and load nXhtml.

\(fn)" t nil)

(autoload 'grep-query-replace "ourcomments-util" "\
Do `query-replace-regexp' of FROM with TO, on all files in *grep*.
Third arg DELIMITED (prefix arg) means replace only word-delimited matches.
If you exit (\\[keyboard-quit], RET or q), you can resume the query replace
with the command \\[tags-loop-continue].

\(fn FROM TO &optional DELIMITED)" t nil)

(autoload 'info-open-file "ourcomments-util" "\
Open an info file in `Info-mode'.

\(fn INFO-FILE)" t nil)

;;;***

;;;### (autoloads (rnc-mode) "rnc-mode" "util/rnc-mode.el" (18654
;;;;;;  33922))
;;; Generated autoloads from util/rnc-mode.el

(autoload 'rnc-mode "rnc-mode" "\
Major mode for editing RELAX NG Compact Syntax schemas.
\\{rnc-mode-map}

\(fn)" t nil)

;;;***

;;;### (autoloads (tabkey2-mode) "tabkey2" "util/tabkey2.el" (18662
;;;;;;  2982))
;;; Generated autoloads from util/tabkey2.el

(defvar tabkey2-mode nil "\
Non-nil if Tabkey2 mode is enabled.
See the command `tabkey2-mode' for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `tabkey2-mode'.")

(custom-autoload 'tabkey2-mode "tabkey2" nil)

(autoload 'tabkey2-mode "tabkey2" "\
More fun with Tab key number two (completion etc).
This global minor mode by default binds Tab in a way that let you
do completion with Tab in all buffers (where it is possible).

The Tab key is easy to type on your keyboard.  Then why not use
it for completion, something that is very useful?  Shells usually
use Tab for completion so many are used to it.  This was the idea
of Smart Tabs and this is a generalization of that idea.

However in Emacs the Tab key is usually used for indentation.
The idea here is that if Tab has been pressed once for
indentation, then as long as point stays further Tab keys might
as well do completion.

So you kind of do Tab-Tab for first completion (and then just
Tab for further completions as long as point is not moved).

And there is even kind of Tab-Tab-Tab completion: If completion
fails the next completion function will be the one you try with
next Tab. (You get some notification of this, of course.)

See `tabkey2-first' for more information about usage.

Note: If you do not want the Tab-Tab behaviour above, but still
want an easy way to reach the available completion functions,
then you can instead of turning on tabkey2-mode enter this in
your .emacs:

 (global-set-key [f8] 'tabkey2-cycle-completion-functions)

After hitting f8 you will then be in the same state as after the
first in tabkey2-mode.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads (tyda-lookup-word) "tyda" "util/tyda.el" (18659
;;;;;;  17016))
;;; Generated autoloads from util/tyda.el

(autoload 'tyda-lookup-word "tyda" "\
Look up word WORD at URL `http://tyda.se/'.
This site translates between English and Swedish.  The site will
be opened in your webbrowser with WORD looked up.

\(fn WORD)" t nil)

;;;***

;;;### (autoloads (udev-cedet-update) "udev-cedet" "util/udev-cedet.el"
;;;;;;  (18654 33922))
;;; Generated autoloads from util/udev-cedet.el

(autoload 'udev-cedet-update "udev-cedet" "\
Fetch and install CEDET from the devel sources.
To determine where to store the sources see `udev-cedet-dir'.
For how to start CEDET see `udev-cedet-load-cedet'.

\(fn)" t nil)

;;;***

;;;### (autoloads (udev-ecb-update) "udev-ecb" "util/udev-ecb.el"
;;;;;;  (18654 33922))
;;; Generated autoloads from util/udev-ecb.el

(autoload 'udev-ecb-update "udev-ecb" "\
Fetch and install ECB from the devel sources.
To determine where to store the sources see `udev-ecb-dir'.
For how to start ECB see `udev-ecb-load-ecb'.

\(fn)" t nil)

;;;***

;;;### (autoloads (udev-rinari-update) "udev-rinari" "util/udev-rinari.el"
;;;;;;  (18654 44936))
;;; Generated autoloads from util/udev-rinari.el

(autoload 'udev-rinari-update "udev-rinari" "\
Fetch and install Rinari from the devel sources.
To determine where to store the sources and how to start rinari
see `udev-rinari-dir' and `udev-rinari-load-rinari'.

\(fn)" t nil)

;;;***

;;;### (autoloads (viper-tutorial) "viper-tut" "util/viper-tut.el"
;;;;;;  (18685 3334))
;;; Generated autoloads from util/viper-tut.el

(autoload 'viper-tutorial "viper-tut" "\
Run a tutorial for Viper.
If any of the standard Viper key bindings that are used in the
tutorial have been changed then an explanatory note about this is
shown in the beginning of the tutorial buffer.

When the tutorial buffer is killed the content and point position
in the buffer is saved so that the tutorial may be resumed
later.

\(fn PART &optional DONT-ASK-FOR-REVERT)" t nil)

;;;***

;;;### (autoloads (winsize-save-window-configuration winsize-balance-siblings
;;;;;;  resize-windows) "winsize" "util/winsize.el" (18761 37866))
;;; Generated autoloads from util/winsize.el

(autoload 'resize-windows "winsize" "\
Start window resizing.
During resizing a window is selected.  You can move its
borders. In the default configuration the arrow keys moves the
right or bottom border if they are there. To move the opposite
border use S-arrowkeys.

You can also do other window operations, like splitting, deleting
and balancing the sizes.  The keybindings below describes the key
bindings during resizing:\\<winsize-keymap>

  `balance-windows'                      \\[balance-windows]
  `winsize-balance-siblings'             \\[winsize-balance-siblings]
  `fit-window-to-buffer'                 \\[fit-window-to-buffer]
  `shrink-window-if-larger-than-buffer'  \\[shrink-window-if-larger-than-buffer]

  `winsav-rotate'                        \\[winsav-rotate]

  `winsize-move-border-up'      \\[winsize-move-border-up]
  `winsize-move-border-down'    \\[winsize-move-border-down]
  `winsize-move-border-left'    \\[winsize-move-border-left]
  `winsize-move-border-right'   \\[winsize-move-border-right]

  `winsize-to-border-or-window-left'    \\[winsize-to-border-or-window-left]
  `winsize-to-border-or-window-up'      \\[winsize-to-border-or-window-up]
  `winsize-to-border-or-window-right'   \\[winsize-to-border-or-window-right]
  `winsize-to-border-or-window-down'    \\[winsize-to-border-or-window-down]

   Note that you can also use your normal keys for
   `forward-char', `backward-char', `next-line', `previous-line'
   and what you have on HOME and END to move in the windows. That
   might sometimes be necessary to directly select a
   window. (You may however also use `other-window' or click
   with the mouse, see below.)

  `delete-window'                \\[delete-window]
  `delete-other-windows'         \\[delete-other-windows]
  `split-window-vertically'      \\[split-window-vertically]
  `split-window-horizontally'    \\[split-window-horizontally]
  `other-window'                 \\[other-window]

  `winsize-save-window-configuration'       \\[winsize-save-window-configuration]
  `winsize-next-window-configuration'       \\[winsize-next-window-configuration]
  `winsize-previous-window-configuration'   \\[winsize-previous-window-configuration]

  `mouse-set-point'   \\[mouse-set-point]

  `winsize-quit'               \\[winsize-quit]
  `winsize-stop-go-back'       \\[winsize-stop-go-back]
  `winsize-stop'               \\[winsize-stop]
  `winsize-stop-and-execute'   \\[winsize-stop-and-execute]

  `winsize-help'          \\[winsize-help]
  `describe-key'          \\[describe-key]
  `describe-key-briefly'  \\[describe-key-briefly]
  (All the normal help keys work, and at least those above will
  play well with resizing.)

Nearly all other keys exits window resizing and they are also
executed.  However, the key sequences in `winsize-let-me-use' and
dito for commands there are also executed without exiting
resizing.

The colors of the modelines are changed to those given in
`winsize-mode-line-colors' to indicate that you are resizing
windows.  To make this indication more prominent the text in the
selected window is marked with the face hold in the variable
`winsize-selected-window-face'.

The option `juris-way' decides how the borders to move are
selected. If this option is non-nil then the right or bottom
border are the ones that are moved with the arrow keys and the
opposite border with shift arrow keys.

If `juris-way' is nil then the following apply:

As you select other borders or move to new a window the mouse
pointer is moved inside the selected window to show which borders
are beeing moved. The mouse jumps a little bit to make its
position more visible. You can turn this off by customizing
`winsize-make-mouse-prominent'.

Which borders initially are choosen are controlled by the
variable `winsize-autoselect-borders'.

** Example: Border selection, movements and windows.

  Suppose you have a frame divided into windows like in the
  figure below.  If window B is selected when you start resizing
  then (with default settings) the borders marked with 'v' and
  'h' will be the ones that the arrow keys moves. To indicate
  this the mouse pointer is placed in the right lower corner of
  the selected window B.

    +----------+-----------+--------+
    |          |           v        |
    |          |           v        |
    |    A     |    _B_    v        |
    |          |           v        |
    |          |           v        |
    |          |         x v        |
    +hhhhhhhhhhhhhhhhhhhhhhhhhhhhhhh+
    |                    |          |
    |                    |          |
    |                    |          |
    |                    |          |
    |                    |          |
    |                    |          |
    +----------+---------+----------+

  Now if you press M-<left> then the picture below shows what has
  happened. Note that the selected vertical border is now the one
  between A and B. The mouse pointer has moved to the
  corresponding corner in the window B, which is still selected.

    +----------+-----------+--------+
    |          v           |        |
    |          v           |        |
    |    A     v    _B_    |        |
    |          v           |        |
    |          v           |        |
    |          v x         |        |
    +hhhhhhhhhhhhhhhhhhhhhhhhhhhhhhh+
    |                    |          |
    |                    |          |
    |                    |          |
    |                    |          |
    |                    |          |
    |                    |          |
    +----------+---------+----------+

  Press M-<left> once again. This gives this picture:

    +----------+-----------+--------+
    |          v           |        |
    |          v           |        |
    |   _A_    v     B     |        |
    |          v           |        |
    |          v           |        |
    |        x v           |        |
    +hhhhhhhhhhhhhhhhhhhhhhhhhhhhhhh+
    |                    |          |
    |                    |          |
    |                    |          |
    |                    |          |
    |                    |          |
    |                    |          |
    +----------+---------+----------+

  Note that the window A is now selected. However there is no
  border that could be moved to the left of this window (which
  would otherwise be chosen now) so the border between A and B is
  still the one that <left> and <right> moves. The mouse has
  moved to A.

  If we now delete window A the new situation will look like
  this:

    +----------+-----------+--------+
    |                      |        |
    |                      |        |
    |         _B_          |        |
    |                      |        |
    |                      |        |
    |                    x |        |
    +hhhhhhhhhhhhhhhhhhhhhhhhhhhhhhh+
    |                    |          |
    |                    |          |
    |                    |          |
    |                    |          |
    |                    |          |
    |                    |          |
    +----------+---------+----------+



>>>> testing stuff >>>>
`help-mode-hook'
`temp-buffer-show-function'
`view-exit-action'
<<<<<<<<<<<<<<<<<<<<<<<

\(fn)" t nil)

(autoload 'winsize-balance-siblings "winsize" "\
Make current window siblings the same height or width.
It works the same way as `balance-windows', but only for the
current window and its siblings.

\(fn)" t nil)

(autoload 'winsize-save-window-configuration "winsize" "\
Not documented

\(fn)" t nil)

;;;***

;;;### (autoloads nil nil ("autostart.el" "etc/schema/schema-path-patch.el"
;;;;;;  "nxhtml/html-chklnk.el" "nxhtml/html-imenu.el" "nxhtml/html-move.el"
;;;;;;  "nxhtml/html-pagetoc.el" "nxhtml/html-quote.el" "nxhtml/html-toc.el"
;;;;;;  "nxhtml/html-upl.el" "nxhtml/html-wtoc.el" "nxhtml/nxhtml-anything-1.el"
;;;;;;  "nxhtml/nxhtml-autoload.el" "nxhtml/nxhtml-js.el" "nxhtml/nxhtml-strval.el"
;;;;;;  "nxhtml/outline-magic.el" "nxhtml/rngalt.el" "nxhtml/test-c-m.el"
;;;;;;  "nxhtml/test-cust.el" "nxhtml/tests.el" "nxhtml/tidy-xhtml.el"
;;;;;;  "nxhtml/wtest.el" "nxhtml/xhtml-help.el" "related/cgi+.el"
;;;;;;  "related/find-recursive-orig.el" "related/find-recursive.el"
;;;;;;  "related/flymake-helpers.el" "related/flymake-js.el" "related/flymake-keys.el"
;;;;;;  "related/flymake-php.el" "related/flymu.el" "related/fold-dwim.el"
;;;;;;  "related/js2-font-lock-new.el" "related/js2-font-lock.el"
;;;;;;  "related/php-imenu.el" "related/php-mode-1.5.0.el" "related/snippet.el"
;;;;;;  "tests/angus77-setup-jde.el" "tests/elunit.el" "tests/emacstest-suites.el"
;;;;;;  "tests/ert-080729.el" "tests/ert2.el" "tests/inemacs/bug1013.el"
;;;;;;  "tests/nxhtmltest-helpers.el" "tests/temp-test.el" "util/appmenu-fold.el"
;;;;;;  "util/appmenu.el" "util/buffer-bg.el" "util/bw-rnc-load/schema-find.el"
;;;;;;  "util/chart.el" "util/color-test-sent.el" "util/color-test.el"
;;;;;;  "util/custsets.el" "util/ecb-batch-compile.el" "util/elunit.el"
;;;;;;  "util/ffip.el" "util/fmode.el" "util/from-osxkeys.el" "util/fupd.el"
;;;;;;  "util/half-qwerty.el" "util/hexcolor.el" "util/hfy-test.el"
;;;;;;  "util/hfyview.el" "util/hl-needed.el" "util/htmlfontify.21.el"
;;;;;;  "util/htmlfontify.el" "util/mumamo-aspnet.el" "util/mumamo-test.el"
;;;;;;  "util/mumamo-trace.el" "util/mumamo.el" "util/new-key-seq-widget.el"
;;;;;;  "util/nxml-mode-os-additions.el" "util/ocr-user.el" "util/org-panel.el"
;;;;;;  "util/pause.el" "util/physical-line.el" "util/popcmp.el"
;;;;;;  "util/rebind.el" "util/rxi.el" "util/search-form.el" "util/sex-mode.el"
;;;;;;  "util/udev-nxhtml.el" "util/udev.el" "util/useful-commands.el"
;;;;;;  "util/vline.el" "util/whelp.el" "util/winsav.el" "util/zen-mode.el")
;;;;;;  (18766 53631 796000))

;;;***

;;;### (autoloads (nxhtmlmaint-get-all-autoloads) "../nxhtml-maintenance"
;;;;;;  "nxhtml-maintenance.el" (18766 50614))
;;; Generated autoloads from nxhtml-maintenance.el

(autoload 'nxhtmlmaint-get-all-autoloads "../nxhtml-maintenance" "\
Not documented

\(fn)" t nil)

;;;***
