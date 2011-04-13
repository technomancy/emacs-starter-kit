;;; vimpulse.el --- emulates Vim's most useful features -*- coding: utf-8 -*-

;; Copyright (C) 2007 Brad Beveridge
;; Copyright (C) 2007, 2009 Alessandro Piras
;; Copyright (C) 2008 Frank Fischer
;; Copyright (C) 2009 Jason Spiro <http://www.jspiro.com/>
;; Copyright (C) 2010 Vegard Øye
;; Copyright (C) 2010 Štěpán Němec
;;
;; Author: Brad Beveridge et al.
;; Maintainer: Vegard Øye <vegard_oye at hotmail.com>
;;      Please send bug reports to the mailing list (see below).
;; Created: 23 Aug 2007
;; Version: 0.5
;; Keywords: emulations, viper
;; Human-Keywords: vim, visual-mode, rsi, ergonomics, emacs pinky
;; URL: http://www.emacswiki.org/emacs/vimpulse.el
;; Gitorious project: http://gitorious.org/vimpulse
;;      For the latest developmental version, clone the repo with:
;;      git clone git://gitorious.org/vimpulse/vimpulse.git
;; Mailing list: <implementations-list at lists.ourproject.org>
;;      Subscribe: http://tinyurl.com/implementations-list
;;      Newsgroup: nntp://news.gmane.org/gmane.emacs.vim-emulation
;;      Archives: http://dir.gmane.org/gmane.emacs.vim-emulation
;;      You don't have to subscribe. We usually reply within a few
;;      days and CC our replies back to you.
;; Related: viper.el, viper-in-more-modes.el
;;
;; Thanks to our old maintainers:
;;      Alessandro Piras
;;      Jason Spiro
;; We'll miss you as maintainers :)
;;
;; This file is not part of GNU Emacs.

;;; Commentary:

;; Vimpulse emulates Vim's most popular features, like Visual mode
;; and text objects. Vimpulse is a set of modifications to Viper, the
;; standard library that emulates vi. Vimpulse is not a minor mode;
;; as soon as it is loaded, Viper will start working in a more
;; Vim-like way.
;;
;; Vimpulse is under active development. It works quite well with
;; GNU Emacs 22.3 and 23.2, as well as XEmacs 21.4.22. Patches and
;; feature requests are welcome (see also the file CONTRIBUTE in the
;; repository).

;;; Installation:

;; If you checked out from Git, run `make' to produce
;; vimpulse-big.el. If on Windows, you can run compile.bat. Then:
;;
;; 1. Copy vimpulse.el (or vimpulse-big.el) to somewhere in your
;;    `load-path'.
;;
;; 2. Add the following to your init file:
;;
;;        (require 'vimpulse)
;;
;;    If you use Windows, see
;;    http://www.gnu.org/software/emacs/windows/faq3.html
;;
;; The rest is optional:
;;
;; 3. For linear undo/redo and undo branches, install
;;    undo-tree.el: http://www.emacswiki.org/emacs/UndoTree
;;
;;    Just place it in `load-path', and Vimpulse will load it
;;    automatically. On XEmacs, you can use redo.el instead.
;;
;; Vimpulse automatically enables Viper. You can temporarily disable
;; Viper (and Vimpulse) with the "C-z" key.

;;; Usage:

;; To use Visual mode, press "v" in vi (command) mode. Then use the
;; motion commands to expand the selection. Press "d" to delete, "c"
;; to change, "r" to replace, or "y" to copy. You can use "p" to
;; paste. For Line selection, press "V" instead of "v"; then you can
;; copy and paste whole lines. For Block selection, press "C-v"; now
;; you can copy and paste the selected rectangle. In Block selection,
;; you may use "I" or "A" to insert or append text before or after the
;; selection on each line.
;;
;; Other features:
;;
;; Vimpulse supports text objects: "daw", "daW", "das", "dap", "dab",
;; "daB", "da(", "da[", "da{", "da<", "da"", "da'", as well as "diw",
;; "diW", "dis", etc. To change an object: "caw", "cas", etc. To yank
;; it: "yaw", "yas", etc. To select it: "vaw", "vas", etc.
;;
;; The extended documentation is still in its early stages, but you
;; can view drafts at: http://gitorious.org/vimpulse/pages/Home
;;
;; The documentation that comes with Vim -- which is online at
;; http://vimdoc.sf.net/ -- may also be helpful.
;;
;; Tips:
;;
;; - Vimpulse makes "C-r" run Redo in command mode, but you can
;;   still get reverse isearch by pressing "C-s" and then "C-r".
;;
;; - To change the color of search, add something like the following
;;   to .emacs:
;;
;;       (set-face-foreground isearch nil)
;;       (set-face-background isearch "lightgoldenrod2")
;;
;; - To change the color of Visual mode (`zmacs-region' in XEmacs):
;;
;;       (set-face-background 'region "blue")
;;
;; For more tips, see: http://gitorious.org/vimpulse/pages/Tips

;;; News:

;; Version 0.5 [2010-09-12]
;;  [vegard_oye at hotmail.com:]
;;  - [NEW] "/" and "?" use isearch. Matches are highlighted while
;;    typing; also, isearch shortcuts apply, like "M-c" to toggle
;;    case-sensitivity. Type "C-h k /" for more details.
;;  - [NEW] :undolist or :ul shows the undo history as a tree. This
;;    uses undo-tree.el, which replaces redo.el for undo/redo
;;    (see installation instructions).
;;  - [NEW] Keybinding functions: `vimpulse-global-set-key',
;;    `vimpulse-local-set-key', `vimpulse-define-key'. State bindings
;;    can now be assigned to both minor and major Emacs modes,
;;    which is useful for writing extensions.
;;  - [NEW] Keys: "gi", "g0", "g$", "]P", "]p".
;;  - [NEW] Lower-case marks are buffer-local -- thanks, Štěpán Němec.
;;  - [NEW] If `viper-auto-indent' is t, then "RET" extends the
;;    comment prefix to the next line (with `comment-indent-new-line'
;;    from newcomment.el).
;;  - [NEW] "C-w" has its own prefix map, `vimpulse-window-map'.
;;  - [FIX] Replace mode's appearance is now more similar to Vim's.
;;  - [FIX] Byte compilation errors.
;;  - [FIX] Various bugs submitted to the mailing list --
;;    thanks, everyone.
;;  -  Operator commands are now defined with the
;;    `vimpulse-define-operator' macro.
;;  - To pacify the compiler, all variables are initially defined
;;    in one place.
;;  - For readability and consistency, "Yoda conditions" are
;;    universally banned: e.g., (= var 0), not (= 0 var).
;;  - "Change Log" is renamed to "News".
;;
;; Version 0.4 [2010-04-26]
;;  [vegard_oye at hotmail.com:]
;;  - [NEW] Operator-Pending mode: the cursor's appearance
;;    changes temporarily after "y", "d", "c", etc.
;;  - [NEW] Motion type system: one can change how a motion is
;;    "interpreted" with "v", "V" and "C-v". For example, "dvj" will
;;    delete a characterwise range (the default is linewise).
;;  - [NEW] Keys: "gq", "gu", "gU", "g~", "g?".
;;  - [NEW] Keybinding functions: `vimpulse-omap' and
;;    `vimpulse-omap-local'.
;;  - [FIX] Vimpulse's text objects handle whitespace
;;    more like Vim's.
;;  - [FIX] Various bugs submitted to the mailing list --
;;    thanks, everyone.
;;  - The code for applying an "operator" like "d" to a "motion"
;;    like "w" is completely rewritten. Operators are simple
;;    to define (with `vimpulse-range'), and can be applied
;;    to regular Emacs movement commands as well.
;;  - The text objects have been redefined in terms of the new
;;    framework. They are implemented as selection commands;
;;    see the `vimpulse-define-text-object' macro for details.
;;  - The code for adding Viper states is generalized.
;;    Both Visual mode and Operator-Pending mode are
;;    defined with the `vimpulse-define-state' macro.
;;  - The comments use a more conventional format: ;;;; for major
;;    headings (one per file), ;;; for subsections (within each file),
;;    ;; for individual pieces of code and ; for trailing comments.
;;    This is easier to maintain and complies with section D.7 of
;;    the GNU Emacs Lisp Reference Manual.
;;
;; Version 0.3.1 [2010-03-09]
;;  [vegard_oye at hotmail.com:]
;;  - [NEW] Emacs-compatible Visual selection.
;;    It is now a Viper state proper, with a user map
;;    and a major mode extension map.
;;    [NEW] Visual keys: "u", "U", "~", ">", "<", "J", "O", "gv" --
;;    thanks, Frank Fischer.
;;  - [NEW] Movement keys: "C-o", "C-i", "C-w hjkl", "gb", "gd", "+", "_".
;;  - [NEW] Keybinding functions: `vimpulse-map',
;;    `vimpulse-imap' and `vimpulse-vmap'.
;;  - [NEW] Backspace in Replace mode restores text.
;;  - [NEW] Basic vi navigation in help buffers.
;;  - [NEW] Vimpulse has its own customization group.
;;  - [FIX] Improved text objects support, including Visual mode.
;;  - [FIX] Various bugs listed at EmacsWiki or submitted to the
;;    mailing list or bug tracker -- thanks.
;;  - All Vimpulse bindings are now in `viper-vi-basic-map',
;;    leaving `viper-vi-global-user-map' for the user.
;;    The same is true of Visual mode.
;;  - Easier installation. rect-mark.el is no longer needed,
;;    nor is cl.el.
;;  - All tabs are replaced by spaces.
;;  - The file encoding is UTF-8.
;;  [laynor at gmail.com:]
;;  - Added some small fixes, and promoted the experimental stuff to
;;    stable, as it seems to work well and not loading it caused
;;    problems.
;;
;; Version 0.3.0 [2009-07-03]
;;  [laynor at gmail.com:]
;;  - [NEW] Register support on text object commands.
;;  - [NEW] Issuing ":" in Visual mode has a behavior closer
;;    to Vim's.
;;  [jasonspiro3 at gmail.com:]
;;  - [FIX] The Enter key now does what it should do -- insert a
;;    newline -- even when longlines-mode is on.
;;  - Comment changes.
;;
;; Version 0.2.6.9 [2009-06-24]
;;  [laynor at gmail.com:]
;; - [FIX & NEW] Text objects support fixed and integrated with Viper.
;;   Now count works (e.g., you can do "3caw" and it works correctly),
;;   and it's possible to repeat the commands with ".".
;;
;; Version 0.2.6.8 [2009-06-22]
;;  [laynor at gmail.com:]
;; - [NEW] Text object support: paren blocks, sentences, word, Words,
;;   quoted expressions, paragraphs. Delete and change commands.
;;   Example commands: "diw", "ci(", "das", etc.
;; - [FIX] It's now possible to exit Visual mode by pressing
;;   "ESC" or "^[".
;;
;; Version 0.2.6.7
;;  [jasonspiro3 at gmail.com:]
;;  - No code changes.
;;  - Fixed up "thanks" section below to mention Mieszko
;;    <sillyfox at yahoo.com>'s full name. He wrote a small patch
;;    which was included long ago. I must have forgotten to include it
;;    in the changelog.
;;
;; Version 0.2.6.6
;;  [laynor at gmail.com:]
;; - Fixed pasting in Visual mode, works like in Vim now
;;   (experimental, see point 6 of installation instructions).
;;
;; Version 0.2.6.5
;;  [laynor at gmail.com:]
;; - Fixed some major suckage with the change command. Still alpha,
;;   comments welcome. To use it, see the installation instructions,
;;   point 6 (it's still experimental).
;; - Cleaned namespace, hope there are no hidden bugs.
;; - Fixed loading on Emacs snapshot.
;;
;; Version 0.2.6.4
;;  [laynor at gmail.com:]
;;  - This can probably be considered a major release.
;;  - [FIX & NEW] Rewritten Visual mode, "v" and "V" variants (no
;;    changes to Visual Block still). It does not use the region like
;;    before: highlighting is done through overlays, and the region is
;;    set inside the command code before calling the Viper commands.
;;    "=" in Visual mode calls `vimpulse-visual-indent-command'. The
;;    Visual mode (apart from Block mode) looks and feels like Vim.
;;  - [NEW] Enhanced paren-matching. Moving the cursor on a closing
;;    paren in Normal mode now highlights the opening paren.
;;  - [NEW] Pressing "RET" in Insert mode automatically indents
;;    the new line.
;;  - [NEW] "^[" works.
;;  - [FIX] "a<ESC>" leaves the cursor in the same location as it was
;;    before (it advanced the cursor 1 character before --
;;    `viper-exit-insert-state's fault).
;;  - [FIX] "cW" doesn't suck anymore at the end of a line.
;;
;; Version 0.2.6.3:
;;  [frank.fischer at s2001.tu-chemnitz.de:]
;;  - Support more Visual Block mode features: insert, append, delete,
;;    yank, change.
;;  - Change some Vimpulse and Viper functions to handle Block mode
;;    properly.
;;  - Update documentation to reflect Visual Block mode.
;;  - The "=" key in Visual mode calls `indent-region'.
;;
;; Version 0.2.6.2:
;;  [jasonspiro3 at gmail.com:]
;;  - Improved XEmacs compatibility.
;;  - Small documentation improvements.
;;
;; Version 0.2.6.1:
;;  [jasonspiro3 at gmail.com:]
;;  - Removed duplicate definition of `vimpulse-detect-mark-deactivate'
;;    and duplicate `add-hook' call to add the hook. I must have added
;;    the extra copies by accident when doing my last big merge; now
;;    they are gone.
;;
;; Version 0.2.6.0:
;;  [jasonspiro3 at gmail.com:]
;;  - Merged a patch for the function that powers "*" and "#". Based on
;;    Ryoichi's patch and a cleaned-up version of Weihua's patch --
;;    thanks. Now "*" and "#" will search for entire symbol at point,
;;    including underscores, not just word at point.
;;  - TODO addition.
;;
;; Version 0.2.5.1:
;;  [jasonspiro3 at gmail.com:]
;;  - Redefined viper-adjust-undo to do nothing. This way, in Insert
;;    mode, typing then moving the cursor then typing more counts as
;;    two separately undoable actions instead of one. Thanks to Weihua
;;    JIANG and to max_ from IRC #emacs for the idea.
;;  - Small extra TODO.
;;
;; Version 0.2.5.0:
;;  [jasonspiro3 at gmail.com:]
;;  I've ignored my local changes for too long. Here they are:
;;  - Added keybindings from a Usenet post by Samuel Padgett.
;;  - Made change ("cw", etc.) commands work more like Vim (my code).
;;  - I removed (setq ex-cycle-other-window nil); although it is very
;;    useful, it merely works around a problem with Viper. I plan to
;;    discuss it with the Viper maintainer instead.
;;  - Other changes and bugfixes from various people.
;;
;; Version 0.2.0.3:
;;  [jasonspiro3 at gmail.com:]
;;  - Added Brad's `viper-jump-to-tag-at-point'.
;;
;; Version 0.2.0.2:
;;  [jasonspiro3 at gmail.com:]
;;  - Small "C-w" keys and doc fixes.
;;
;; Version 0.2.0.1:
;;  [cppjavaperl:]
;;  - Added support for Visual Block mode (i.e., rectangle selection).
;;  - Made "C-p" look for matches PRIOR to the cursor and added "C-n"
;;    binding to look for matches BEFORE the cursor. This works more
;;    like Vim does.
;;  [jasonspiro3 at gmail.com:]
;;  - Since Vimpulse has no website, I added a prominent pointer at
;;    the top to the installation instructions.
;;
;; Version 0.2.0.0: Brad merged in several changes, including:
;;  - Exit Visual mode when the mark deactivates.
;;  - Changed the window manipulation to be global.
;;  - Added "gf" (goto file at point).
;;  - Added "\C-]" and "\C-t", tag jump & pop.
;;  - Added a helper function for defining keys.
;;  - Commented out `show-paren-function', what is it meant to do?
;;
;; Version 0.1.0.1: No code changes. Small documentation changes,
;; including updates on moving-left bug.
;;
;; Version 0.1: Initial release.

;;; Acknowledgements:

;; Special thanks to Brad Beveridge, the original author of Vimpulse.
;;
;; Thanks to:
;;
;;      cppjavaperl <cppjavaperl at yahoo.com>
;;      Fabian Brännström <f.braennstroem at gmx.de>
;;      Frank Fischer <frank.fischer at s2001.tu-chemnitz.de>
;;      John <jn at ngedit.com>
;;      John J Foerch <jjfoerch at earthlink.net>
;;      José Alfredo Romero L. <escherdragon at gmail.com>
;;      Mieszko <sillyfox at yahoo.com>
;;      rhinoryan
;;      Rick Sladkey, author of rect-mark.el
;;      Ryoichi Kanetaka <ryoichi.kanetaka at gmail.com>
;;      Samuel Padgett
;;      Štěpán Němec <stepnem at gmail.com>
;;      Stephen Bach <stephen at sjbach.com>
;;      Stian S.
;;      Tim Harper <timcharper at gmail.com>
;;      Toby Cubitt
;;      Wang Xin
;;      Weihua Jiang <weihua.jiang at gmail.com>
;;
;; and all the other people who have sent in bug reports and feedback.
;; Also, thanks to Michael Kifer and Viper's contributors.
;;
;; We love patches. Would you like to see your name here?
;; Please send code and/or documentation patches to the maintainer.
;; Ideas, comments, and test results are appreciated too.

;;; Bugs:

;; (We would appreciate it very much if you report bugs.)
;;
;; Known bugs:
;;
;; - Undo has problems in XEmacs.

;;; Development and documentation TODOs:

;; - Make sure I have added all stuff in Brad's Viper additions and
;;   from my collection, then start documenting already. Once there
;;   are even the simplest of docs (a nice keymap), people will have a
;;   far easier time using Vimpulse and so I bet more will contribute.
;;
;; - Folding. This should be implemented as a separate Lisp library
;;   usable for even non-Viper users. Which foldmethods to do first?
;;   I personally only use foldmethod=marker, and even that rarely.
;;
;; - i_C-(I forgot the letter) should do (copy-from-above-command 1)
;;   from misc.el.
;;
;; - Add :set spell / :set nospell that uses flyspell-mode.
;;
;; - Add support for tabs.el, a tabs mode that works sensibly (get it
;;   from Emacs Lisp List).
;;     - Minimum needed: :tabedit, :tabnext, :tabprevious.
;;     - Since I'm emulating Vim, emulate its tab pages feature. So a
;;       tab page should be able to hold one or more buffers.
;;
;; - Add Customize option to let users stop "C-r" from being Redo?
;;
;; - Copy more features from Brad's work in darcs and from vimpact
;;   into Vimpulse.
;;
;; - Doc: look in Google chat log, find description of one-char-off
;;   bug, see if it applies to this or to the not-yet-released
;;   viper-x, and if to this, mention under Bugs.
;;
;; - Doc: list all new keys (and maybe all differences from Viper) in
;;   Usage section.
;;
;; - Doc: describe all new keys in Usage section; can look at Vim
;;   manual for ideas.
;;
;; - Modify how tramp works so it also automatically handles URLs
;;   typed in the netrw syntax, e.g., http:// etc. But first ask tramp
;;   upstream if they could please make those changes themselves.
;;
;; - Improve "CTRL-O" for jumping back in the jumplist and "CTRL-I" for
;;   jumping forwards (for undoing one "CTRL-O"). The global mark ring
;;   is not what I want. I wonder if Emacs' tags functionality allows
;;   a jumplist. I wonder if Viper does tags like nvi does.
;;     - Try code.google.com/p/ejumplist/source/browse/trunk/jumplist.el
;;
;; - On my PC (I run Ubuntu), if you start plain Vim and then press
;;   "CTRL-O" many times, it starts opening recently opened files. Is
;;   that useful? Should Vimpulse have persistent jump table
;;   functionality like that, and if so, should it use recentf or
;;   Vim's .viminfo file or some tag functionality in Emacs? How will
;;   it interact with the fact that in Emacs, it's not traditional to
;;   suddenly close files without warning?
;;
;; - Make sentence movement work like in Vim. I wonder if this can be
;;   done by setting Viper options.
;;     - In Vim, according to :help sentence, end of sentence is:
;;         - ".", "?", or "!"
;;         - then (optionally) one or more """, "'", ")", and "]"
;;           characters
;;         - then a newline, space, or tab.
;;         - A paragraph or section boundary is also a sentence
;;           boundary, but I bet Viper handles that, and if it doesn't,
;;           it should.
;;             - A paragraph begins after each truly empty line (no
;;               whitespace chars on it) or after certain col-1 nroff
;;               macros. A sentence begins after a form feed (^L), or
;;               certain nroff macros, in column 1.
;;             - The characters "{" and "}" sometimes affect paragraph
;;               definitions. See :help paragraph.
;;     - In Viper, on the other hand, I bet sentences are like in vi,
;;       where tabs aren't whitespace, and you need at least two spaces
;;       after the punctuation mark.
;;
;; - Try to get Vimpulse included with upstream Viper; also, ideally,
;;   if you pressed "v" in Viper, Viper would offer to load Vimpulse.
;;   (Likely to happen? Consider that Michael Kifer, the Viper
;;   maintainer, told me he doesn't need Vim keys. Then again, maybe I
;;   could convince him that it's worth it to ship Vim keys, for other
;;   people's benefit. Also, consider that some of the code (like
;;   Operator-Pending mode) addresses problems mentioned in viper.el.)
;;
;; - E-mail ridip <rdp at inthefaith.net> and ask him for his Vimpulse
;;   contribs and his DVORAK stuff.
;;
;; - E-mail to Tromey for upload into ELPA? We'd have to redo this
;;   when a new major version comes out. Or maybe we should just
;;   contribute some auto-ELPA-management code. By the way, should we
;;   move Vimpulse into CVS somewhere?
;;
;; - Maybe merge all feature requests that anyone has ever sent into a
;;   "Feature requests" section here.

;;; Development plans:

;; The design plan for Vimpulse is for it to only emulate features
;; that are in Vim. Therefore, other features do not belong in
;; Vimpulse unless one can get the Vim people to implement those
;; features too.
;;
;; At the same time, Vimpulse should strive for customizability and
;; extensibility, so that the user can modify it just as easily as the
;; rest of Emacs.

;;; Undecided development questions:

;; - In Vimpulse, like in real Vim, "C-r" only does Redo in vi (command)
;;   mode; in Insert mode it does something else. (In Vimpulse that
;;   "something else" is reverse isearch.) Should it do reverse
;;   isearch in Insert mode too?
;;
;; - With some delete commands, Viper shows a message like "Deleted 50
;;   characters" in the minibuffer. Is that annoying?
;;     - Update 1 month later: I hardly notice the message.
;;     - Dear users: Do you think I should disable the message?
;;
;; - I want to allow buffer-switching without using the "C-x" key, since
;;   "C-x b RET" an extremely large amount of times per day is
;;   uncomfortable for my right pinky, which presses RET. There's
;;   already :b, which seems to just invoke `switch-to-buffer'. Is this
;;   right? Is it bad if I make Vimpulse emulate set autowrite=on
;;   then write new multi-buffer code? What should the code's user
;;   interface be like? I really should switch back to Vim for a day,
;;   learn more about how it deals with multiple buffers at once (and
;;   maybe also with tab pages) and emulate whatever of Vim's is most
;;   convenient. What do you think of all the above?
;;     - Update: IIRC, :set hidden lets you switch buffers w/o saving
;;     - Update from Sebastien Rocca Serra: :set wildmenu plus
;;       tab-completion makes :b very pleasant to use when you have
;;       50+ buffers open. Wildmenu is almost like iswitchb or ido.
;;     - I wonder how well that stuff works with just a few buffers open.
;;
;; - Simulate Vim's set virtualedit=onemore option to make "C-x C-e"
;;   possible without first advancing to next line?
;;
;; - Would it be bad to edit users' .viminfo files without asking
;;   permission, or should some variable have to be customized on to do
;;   such a thing?
;;
;; - Is there any need to implement Vim's new
;;   [count]dk-can-go-past-top-of-file-without-error functionality (to
;;   me, no need) or any related functionality?
;;
;; - What to do about XEmacs? It doesn't ship with woman. I wonder
;;   if woman is in some XEmacs package?

;;; License:

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of the
;; License, or any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
;; 02111-1307, USA.

;;; Code:

(defconst vimpulse-version "0.5"
  "The current version of Vimpulse")

(defun vimpulse-version ()
  (interactive)
  (message "Vimpulse version is %s" vimpulse-version))

;; load Viper
(defvar viper-mode t)
(defvar viper-inhibit-startup-message t)
(defvar viper-expert-level 5)
(defvar viper-want-ctl-h-help t)
(defvar viper-search-wrap-around t)
(require 'viper)

;; load undo-tree.el if available, with redo.el as fall-back
(unless (featurep 'undo-tree)
  (condition-case nil
      (require 'undo-tree)
    (error (condition-case nil
               (require 'redo)
             (error nil)))))
(and (fboundp 'global-undo-tree-mode)
     (global-undo-tree-mode 1))

;;; Customization group for Vimpulse

(defgroup vimpulse nil
  "Vim emulation within Emacs."
  :group  'emulations
  :link   '(custom-group-link "viper")
  :prefix 'vimpulse-)

(defcustom vimpulse-want-change-state nil
  "Whether commands like \"cw\" invoke Replace state, vi-like.
The default is to delete the text and enter Insert state,
like in Vim."
  :group 'vimpulse
  :type  'boolean)

(defcustom vimpulse-want-change-undo t
  "Whether commands like \"cw\" are undone in a single step.
On by default."
  :group 'vimpulse
  :type  'boolean)

(defcustom vimpulse-want-C-u-like-Vim nil
  "Whether C-u scrolls like in Vim, off by default."
  :group 'vimpulse
  :type  'boolean)

(defcustom vimpulse-want-C-i-like-Vim t
  "Whether C-i jumps forward like in Vim, on by default."
  :group 'vimpulse
  :type  'boolean)

(defcustom vimpulse-want-quit-like-Vim t
  "Whether :q quits the editor like in Vim, on by default."
  :group 'vimpulse
  :type  'boolean)

(defcustom vimpulse-enhanced-paren-matching t
  "Enhanced matching of parentheses, on by default."
  :group 'vimpulse
  :type  'boolean)

(defcustom vimpulse-want-operator-pending-cursor t
  "Whether the cursor changes in Operator-Pending mode, on by default."
  :group 'vimpulse
  :type  'boolean)

(defcustom vimpulse-visual-block-untabify nil
  "Whether Block mode may change tabs to spaces for fine movement.
Off by default."
  :type  'boolean
  :group 'vimpulse-visual)

(defcustom vimpulse-incremental-search t
  "Use isearch for / and ?, on by default.")

(defcustom vimpulse-flash-delay 2
  "Number of seconds to flash search matches.")

(defvar vimpulse-flash-timer nil
  "Timer for flashing search results.")

(defcustom vimpulse-want-vi-keys-in-apropos t
  "Whether to use vi keys in Apropos mode, on by default."
  :group 'vimpulse
  :type  'boolean)

(defcustom vimpulse-want-vi-keys-in-buffmenu t
  "Whether to use vi keys in Buffer menu, on by default."
  :group 'vimpulse
  :type  'boolean)

(defcustom vimpulse-want-vi-keys-in-dired t
  "Whether to use vi keys in Dired mode, on by default."
  :group 'vimpulse
  :type  'boolean)

(defcustom vimpulse-want-vi-keys-in-Info t
  "Whether to use vi keys in Info mode, on by default."
  :group 'vimpulse
  :type  'boolean)

(defcustom vimpulse-want-vi-keys-in-help t
  "Whether to use vi keys in Help mode, on by default."
  :group 'vimpulse
  :type  'boolean)

(defcustom vimpulse-fold-level 0
  "Default fold level."
  :type  'integer
  :group 'vimpulse)

;; the secrets discovered from untold diggings among
;; the ruins of Customize code
(defun vimpulse-custom-value-p (symbol)
  "Non-nil if SYMBOL has a customized value."
  (or (get symbol 'customized-value)
      (get symbol 'customized-face)
      (get symbol 'saved-value)))

(defmacro vimpulse-setq-custom (sym val &rest body)
  "Set the customized value of SYM to VAL."
  `(progn
     (prog1 (setq ,sym ,val)            ; return VAL
       (when (get ',sym 'custom-autoload)
         (custom-load-symbol ',sym))
       (put ',sym 'customized-value (list (custom-quote ,val))))
     ,(when body
        `(vimpulse-setq-custom ,@body))))

(defmacro vimpulse-setq-custom-default (symbol value &rest body)
  "Set the customized default value of SYMBOL to VALUE."
  `(progn
     (prog1 ,value                      ; return VALUE
       (when (get ',symbol 'custom-autoload)
         (custom-load-symbol ',symbol))
       (put ',symbol 'standard-value (list (custom-quote ,value))))
     ,(when body
        `(vimpulse-setq-custom-default ,@body))))

(defmacro vimpulse-setq (sym val &rest body)
  "Set SYM to VAL, defaults included, unless SYM is customized.
SYM is unquoted. Returns VAL."
  `(progn
     (cond
      ;; customized value: just set custom standard value
      ((vimpulse-custom-value-p ',sym)
       (vimpulse-setq-custom-default ,sym ,val))
      ;; customized variable: set custom and regular values
      ((custom-variable-p ',sym)
       (vimpulse-setq-custom-default ,sym ,val)
       (vimpulse-setq-custom ,sym ,val)
       (setq-default ,sym ,val)
       (setq ,sym ,val))
      ;; regular variable; set default and local values
      (t
       (setq-default ,sym ,val)
       (setq ,sym ,val)))
     ,@(when body
         `((vimpulse-setq ,@body)))))

;;; Declare and/or initialize variables

(defvar dabbrev--last-abbrev-location)
(defvar dabbrev--last-abbreviation)
(defvar dabbrev--last-direction)
(defvar dabbrev--last-expansion)
(defvar dabbrev--last-expansion-location)
(defvar isearch-forward)
(defvar isearch-lazy-highlight-end)
(defvar isearch-lazy-highlight-last-string)
(defvar isearch-lazy-highlight-start)
(defvar isearch-lazy-highlight-wrapped)
(defvar isearch-regexp)
(defvar isearch-string)
(defvar killed-rectangle nil)           ; rect.el
(defvar show-paren-delay)
(defvar undo-tree-visualizer-map)
(defvar woman-use-own-frame)
(defvar woman-use-topic-at-point)

(defvar ex-token-alist)                 ; viper-ex.el
(defvar viper-mode-string)

(defvar vimpulse-viper-movement-cmds
  '(viper-backward-Word viper-backward-char viper-backward-paragraph
    viper-backward-sentence viper-backward-word
    viper-beginning-of-line viper-command-argument
    viper-digit-argument viper-end-of-Word viper-end-of-word
    viper-exec-mapped-kbd-macro viper-find-char-backward
    viper-find-char-forward viper-forward-Word viper-forward-char
    viper-forward-paragraph viper-forward-sentence viper-forward-word
    viper-goto-char-backward viper-goto-char-forward viper-goto-eol
    viper-goto-line viper-line-to-bottom viper-line-to-middle
    viper-line-to-top viper-next-line viper-previous-line
    viper-scroll-down-one viper-scroll-down viper-scroll-up
    viper-scroll-up-one viper-window-bottom viper-window-middle
    viper-window-top vimpulse-end-of-previous-word
    vimpulse-goto-first-line vimpulse-goto-definition
    vimpulse-goto-line vimpulse-search-backward-for-symbol-at-point
    vimpulse-search-forward-for-symbol-at-point vimpulse-jump-backward
    vimpulse-jump-forward vimpulse-visual-toggle-char
    vimpulse-visual-toggle-line vimpulse-visual-toggle-block)
  "List of Viper/Vimpulse movement commands.")

(defvar vimpulse-core-movement-cmds
  '(viper-backward-char
    viper-next-line
    viper-previous-line
    viper-forward-char
    viper-ex)
  "List of Viper \"core\" movement commands.
These should be present in every mode, to avoid confusion.")

(defvar vimpulse-mark-list nil
  "List of mark positions to jump to with `vimpulse-jump-forward'.
They are stored as markers, the current position first:

    (car vimpulse-mark-list)  = current position (last popped)
    (cdr vimpulse-mark-list)  = future positions (previously popped)
    (cadr vimpulse-mark-list) = next position (to jump to)

In other words, a sort of \"reverse mark ring\": marks that are
popped off the mark ring, are collected here.")

(viper-deflocalvar vimpulse-local-marks-alist nil
  "Association list of local marks.
Entries have the form (CHAR (FILE . POS)), where POS is a marker
or a character position.")

(defvar vimpulse-global-marks-alist nil
  "Association list of global marks.
Entries have the form (CHAR (FILE . POS)), where POS is a marker
or a character position.")

(viper-deflocalvar vimpulse-replace-alist nil
  "Alist of characters overwritten in Replace mode.
Used by `vimpulse-replace-backspace' to restore text.
The format is (POS . CHAR).")

(viper-deflocalvar vimpulse-exit-point nil
  "Like `viper-insert-point', but when exiting Insert mode.")

(defvar vimpulse-last-command-event nil
  "Value for overwriting `last-command-event'.
Used by `vimpulse-careful-pre-hook'.")

(defvar vimpulse-careful-alist nil
  "Key bindings for which `vimpulse-careful-pre-hook' is active.
That is, `last-command-event' and `read-char' work differently
for these bindings. The format is (KEY-VECTOR . COMMAND).")

(defvar vimpulse-careful-map (make-sparse-keymap)
  "Keymap of bindings overwritten by `vimpulse-map' et al.")

(defvar vimpulse-paren-overlay-open nil
  "Overlay used to highlight the opening paren.")

(defvar vimpulse-paren-overlay-close nil
  "Overlay used to highlight the closing paren.")

(defvar vimpulse-operators nil
  "Operator commands defined with `vimpulse-define-operator'.")

(viper-deflocalvar vimpulse-inhibit-operator nil
  "Inhibit current operator.
If an operator calls a motion and the motion sets this variable
to t, the operator code is not executed.")

(defvar vimpulse-operator-basic-map)

(defvar vimpulse-operator-remap-map (make-sparse-keymap))

(defvar vimpulse-operator-remap-alist nil
  "Association list of command remappings in Operator-Pending mode.")

(defvar vimpulse-this-operator nil
  "Current operator.
In general, motions and operators are orthogonal, with some exceptions:
\"cw\" and \"dw\" work on slightly different ranges, for example.
Motions can check this variable if they need to know what
operator receives their range. See also `vimpulse-this-motion'.")

(defvar vimpulse-this-motion nil
  "Current motion.
In general, motions and operators are orthogonal, with some exceptions:
\"cc\" may indent the current line while \"cw\" may not, for example.
Operators may check this variable if they need to know what
motion produced the current range. See also `vimpulse-this-operator'.")

(defvar vimpulse-this-count nil
  "Current count (operator count times motion count).")

(defvar vimpulse-this-motion-type nil
  "Current motion type.
May be `block', `line', `inclusive', `exclusive' or nil.")

(defvar vimpulse-last-motion-type nil
  "Last repeated range type.
May be `block', `line', `inclusive', `exclusive' or nil.")

(defvar vimpulse-last-operator nil
  "Last repeated operator.
Used by `vimpulse-operator-repeat'.")

(defvar vimpulse-last-motion nil
  "Last repeated motion.
Used by `vimpulse-operator-repeat'.")

(defvar vimpulse-visual-basic-map)

(defvar vimpulse-visual-remap-alist nil
  "Association list of command remappings in Visual mode.")

(put 'vimpulse-visual-basic-map
     'remap-alist 'vimpulse-visual-remap-alist)

(viper-deflocalvar vimpulse-visual-mode nil
  "Current Visual mode: may be nil, `char', `line' or `block'.")

(viper-deflocalvar vimpulse-visual-global-vars nil
  "List of variables that were global.")

(viper-deflocalvar vimpulse-visual-local-vars
  '(cua-mode
    mark-active
    transient-mark-mode
    zmacs-regions)
  "System variables that are reset for each Visual session.")

(viper-deflocalvar vimpulse-visual-vars-alist nil
  "Alist of old variable values.")

(viper-deflocalvar vimpulse-visual-last nil
  "Last active Visual mode.
May be `char', `line', `block' or nil.")

(viper-deflocalvar vimpulse-visual-previous-state 'viper-state
  "Previous state before enabling Visual mode.
This lets us revert to Emacs state in non-vi buffers.")

(viper-deflocalvar vimpulse-visual-region-expanded nil
  "Whether region is expanded to the Visual selection.")

(viper-deflocalvar vimpulse-visual-point nil
  "Last expanded `point' in Visual mode.")

(viper-deflocalvar vimpulse-visual-mark nil
  "Last expanded `mark' in Visual mode.")

(viper-deflocalvar vimpulse-visual-overlay nil
  "Overlay for Visual selection.
In XEmacs, this is an extent.")

(viper-deflocalvar vimpulse-visual-block-overlays nil
  "Overlays for Visual Block selection.")

(viper-deflocalvar vimpulse-visual-whitespace-overlay nil
  "Overlay encompassing text inserted into the buffer
to make Block selection at least one column wide.")

(viper-deflocalvar vimpulse-undo-list-pointer nil
  "Everything up to this mark is united in the undo-list.")

(defvar vimpulse-visual-height nil
  "Height of last Visual selection.")

(defvar vimpulse-visual-width nil
  "Width of last Visual selection.")

(defvar vimpulse-visual-insert-coords nil
  "List with (I-COM UL-POS COL NLINES), where
I-COM is the insert command (?i, ?a, ?I or ?A),
UL-POS is the position of the upper left corner of the region,
COL is the column of insertion, and
NLINES is the number of lines in the region.")

(defvar vimpulse-movement-cmds
  '(backward-char backward-list backward-paragraph backward-sentence
    backward-sexp backward-up-list backward-word beginning-of-buffer
    beginning-of-defun beginning-of-line beginning-of-visual-line
    cua-cancel digit-argument down-list end-of-buffer end-of-defun
    end-of-line end-of-visual-line exchange-point-and-mark
    forward-char forward-list forward-paragraph forward-sentence
    forward-sexp forward-word keyboard-quit mouse-drag-region
    mouse-save-then-kill mouse-set-point mouse-set-region
    move-beginning-of-line move-end-of-line next-line previous-line
    scroll-down scroll-up undo universal-argument up-list
    vimpulse-end-of-previous-word vimpulse-goto-definition
    vimpulse-goto-first-line vimpulse-goto-line
    vimpulse-visual-block-rotate vimpulse-visual-exchange-corners
    vimpulse-visual-reselect vimpulse-visual-restore
    vimpulse-visual-toggle-block vimpulse-visual-toggle-line
    vimpulse-visual-toggle-char viper-backward-Word
    viper-backward-char viper-backward-paragraph
    viper-backward-sentence viper-backward-word
    viper-beginning-of-line viper-digit-argument viper-end-of-Word
    viper-end-of-word viper-exec-mapped-kbd-macro
    viper-find-char-backward viper-find-char-forward
    viper-forward-Word viper-forward-char viper-forward-paragraph
    viper-forward-sentence viper-forward-word viper-goto-char-backward
    viper-goto-char-forward viper-goto-eol viper-goto-line
    viper-insert viper-intercept-ESC-key viper-line-to-bottom
    viper-line-to-middle viper-line-to-top viper-next-line
    viper-paren-match viper-previous-line viper-search-Next
    viper-search-backward viper-search-forward viper-search-next
    viper-window-bottom viper-window-middle viper-window-top)
  "List of commands that move point.
If listed here, the region is not expanded to the
Visual selection before the command is executed.")

(defvar vimpulse-newline-cmds
  '(cua-copy-region cua-cut-region cua-delete-region delete-region
    exchange-point-and-mark execute-extended-command kill-region
    kill-ring-save vimpulse-Put-and-indent vimpulse-put-and-indent
    vimpulse-visual-exchange-corners viper-Put-back viper-put-back)
  "Non-operator commands needing trailing newline in Visual Line mode.
In most cases, it's more useful not to include this newline in
the region acted on.")

(defvar vimpulse-search-prompt nil
  "String to use for vi-like searching.")

(defvar vimpulse-auxiliary-modes nil
  "List of Emacs modes with state bindings.
The topmost modes have the highest priority.")

(defvar vimpulse-auxiliary-modes-alist
  '((vi-state . viper-vi-auxiliary-modes)
    (insert-state . viper-insert-auxiliary-modes)
    (replace-state . viper-replace-auxiliary-modes)
    (emacs-state . viper-emacs-auxiliary-modes)))

(defvar viper-vi-auxiliary-modes nil)
(defvar viper-insert-auxiliary-modes nil)
(defvar viper-replace-auxiliary-modes nil)
(defvar viper-emacs-auxiliary-modes nil)

;;; Carefully set Viper/woman variables

(defun vimpulse-configure-variables ()
  "Set various variables, unless customized."
  ;; can backspace past start of insert/line
  (vimpulse-setq viper-ex-style-editing nil)
  ;; don't create new frame for manpages
  (vimpulse-setq woman-use-own-frame nil)
  ;; don't prompt upon K key (manpage display)
  (vimpulse-setq woman-use-topic-at-point t)
  ;; no start-up message
  (vimpulse-setq viper-inhibit-startup-message t)
  ;; Viper expert level 5
  (vimpulse-setq viper-expert-level 5)
  ;; make cursor color consistent
  (vimpulse-setq viper-insert-state-cursor-color
                 viper-vi-state-cursor-color)
  ;; cursor moves backwards when exiting Insert state
  (vimpulse-setq viper-ESC-moves-cursor-back t)
  ;; not in Vim: C-h is indispensable in Emacs
  (vimpulse-setq viper-want-ctl-h-help t)
  ;; refresh Viper settings
  (viper-change-state-to-vi))

(if (and (boundp 'after-init-time) after-init-time)
    (vimpulse-configure-variables)
  (add-hook 'after-init-hook 'vimpulse-configure-variables))

;;;; Redefinitions of some of Viper's functions

(defalias 'viper-digit-argument 'digit-argument)

;; Ensure that counts are always echoed immediately, since they might
;; alter the command's behavior profoundly (e.g., 5i repeats the
;; insertion four times).
(defadvice digit-argument (around echo-keystrokes activate)
  "Echo keystrokes immediately."
  (setq echo-keystrokes 0.01)
  ad-do-it)

(defadvice ensure-overriding-map-is-bound (after echo-keystrokes activate)
  "Echo keystrokes immediately."
  (setq echo-keystrokes 0.01))

(defvar vimpulse-saved-echo-keystrokes echo-keystrokes)

(defadvice restore-overriding-map (after echo-keystrokes activate)
  "Restore `echo-keystrokes'."
  (setq echo-keystrokes vimpulse-saved-echo-keystrokes))

(defadvice viper-change
  (around vimpulse-want-change-state activate)
  "Disable Replace state if `vimpulse-want-change-state' is nil."
  (cond
   (vimpulse-want-change-state
    ad-do-it)
   (t
    ;; we don't want Viper's Replace mode when changing text;
    ;; just delete and enter Insert state
    (setq viper-began-as-replace t)
    (kill-region beg end)
    (goto-char beg)
    (viper-change-state-to-insert))))

(defun vimpulse-set-replace-cursor-type ()
  "Display a horizontal bar cursor."
  (unless (featurep 'xemacs)
    (setq cursor-type '(hbar . 4))))

(set-face-foreground viper-replace-overlay-face nil)
(set-face-background viper-replace-overlay-face nil)

(unless (featurep 'xemacs)
  (setq viper-replace-overlay-cursor-color
        viper-vi-state-cursor-color)
  (add-hook 'viper-replace-state-hook
            'vimpulse-set-replace-cursor-type)
  (remove-hook 'viper-replace-state-hook
               'viper-restore-cursor-type))

;;; Marks

;; the following makes lowercase marks buffer-local
(defun vimpulse-mark-point ()
  "Set Vimpulse mark at point."
  (interactive)
  (let ((char (read-char)))
    (cond
     ;; local marks
     ((and (<= ?a char) (<= char ?z))
      (vimpulse-mark char))
     ;; global marks
     ((and (<= ?A char) (<= char ?Z))
      (vimpulse-mark char t))
     ;; < > . , ^
     (t
      (add-to-list 'unread-command-events char)
      (viper-mark-point)))))

(defun vimpulse-mark (char &optional global)
  "Set mark CHAR at point.
Mark is buffer-local unless GLOBAL."
  (let* ((marks-alist (if global
                          'vimpulse-global-marks-alist
                        'vimpulse-local-marks-alist))
         (mark  (assq char (symbol-value marks-alist)))
         (value (cons buffer-file-name (point-marker))))
    (if mark
        (setcdr mark value)
      (set marks-alist (cons (cons char value)
                             (symbol-value marks-alist)))))
  (add-hook 'kill-buffer-hook 'vimpulse-mark-swap-out nil t))

(defun vimpulse-mark-swap-out ()
  "Cf. `register-swap-out'."
  (and buffer-file-name
       (dolist (marks-alist '(vimpulse-local-marks-alist
                              vimpulse-global-marks-alist))
         (dolist (elt (symbol-value marks-alist))
           (and (markerp (cddr elt))
                (eq (marker-buffer (cddr elt)) (current-buffer))
                (setcdr (cdr elt) (marker-position (cddr elt))))))))

(defun vimpulse-get-mark (char)
  (or (cdr (assq char (if (< char ?Z)
                          vimpulse-global-marks-alist
                        vimpulse-local-marks-alist)))
      (error "No such mark: %c" char)))

(defun vimpulse-goto-mark (arg)
  "Go to mark."
  (interactive "P")
  (let ((char (read-char))
        (com (viper-getcom arg)))
    (vimpulse-goto-mark-subr char com nil)))

(defun vimpulse-goto-mark-and-skip-white (arg)
  "Go to mark and skip to first non-white character on line."
  (interactive "P")
  (let ((char (read-char))
        (com (viper-getCom arg)))
    (vimpulse-goto-mark-subr char com t)))

(defun vimpulse-goto-mark-subr (char com skip-white)
  (cond
   ((viper-valid-register char '(letter Letter))
    (let* ((buff (current-buffer))
           (pos (vimpulse-get-mark char))
           (file (car pos))
           (marker (cdr pos)))
      (if (and file (equal buffer-file-name file))
          (goto-char marker)
        (if (null file)
            (if (marker-buffer marker)
                (progn (switch-to-buffer (marker-buffer marker))
                       (goto-char marker))
              (error "Cannot jump to non-existent buffer"))
          (and (or (find-buffer-visiting file)
                   (y-or-n-p (format "Visit file %s again? " file)))
               (find-file file)
               (goto-char marker))))
      (when com
        (viper-move-marker-locally 'viper-com-point (point)))
      (if (and (viper-same-line (point) viper-last-jump)
               (= (point) viper-last-jump-ignore))
          (push-mark viper-last-jump t)
        (push-mark nil t))
      (setq viper-last-jump (point-marker))
      (when skip-white
        (back-to-indentation)
        (setq viper-last-jump-ignore (point)))
      (when com
        (if (equal buff (current-buffer))
            (viper-execute-com (if skip-white
                                   'viper-goto-mark-and-skip-white
                                 'viper-goto-mark)
                               nil com)
          (switch-to-buffer buff)
          (goto-char viper-com-point)
          (viper-change-state-to-vi)
          (error "Viper bell")))))
   ((and (not skip-white) (viper= char ?`))
    (when com
      (viper-move-marker-locally 'viper-com-point (point)))
    (when (and (viper-same-line (point) viper-last-jump)
               (= (point) viper-last-jump-ignore))
      (goto-char viper-last-jump))
    (when (null (mark t))
      (error "Mark is not set in this buffer"))
    (when (= (point) (mark t))
      (pop-mark))
    (push-mark (prog1 (point)
                 (goto-char (or (mark t) (point)))) t)
    (setq viper-last-jump (point-marker)
          viper-last-jump-ignore 0)
    (when com
      (viper-execute-com 'viper-goto-mark nil com)))
   ((and skip-white (viper= char ?'))
    (when com
      (viper-move-marker-locally 'viper-com-point (point)))
    (when (and (viper-same-line (point) viper-last-jump)
               (= (point) viper-last-jump-ignore))
      (goto-char viper-last-jump))
    (when (= (point) (mark t))
      (pop-mark))
    (push-mark (prog1 (point)
                 (goto-char (or (mark t) (point)))) t)
    (setq viper-last-jump (point))
    (back-to-indentation)
    (setq viper-last-jump-ignore (point))
    (when com
      (viper-execute-com 'viper-goto-mark-and-skip-white nil com)))
   (t
    (error viper-InvalidTextmarker char))))

;;; Code for adding extra states

;; state index variables: for keeping track of which modes
;; belong to which states, et cetera
(defvar vimpulse-state-vars-alist
  '((vi-state
     (id . viper-vi-state-id)
     (auxiliary-modes . viper-vi-auxiliary-modes)
     (change-func . viper-change-state-to-vi)
     (basic-mode . viper-vi-basic-minor-mode)
     (basic-map . viper-vi-basic-map)
     (diehard-mode . viper-vi-diehard-minor-mode)
     (diehard-map . viper-vi-diehard-map)
     (modifier-mode . viper-vi-state-modifier-minor-mode)
     (modifier-alist . viper-vi-state-modifier-alist)
     (kbd-mode . viper-vi-kbd-minor-mode)
     (kbd-map . viper-vi-kbd-map)
     (global-user-mode . viper-vi-global-user-minor-mode)
     (global-user-map . viper-vi-global-user-map)
     (local-user-mode . viper-vi-local-user-minor-mode)
     (local-user-map . viper-vi-local-user-map)
     (need-local-map . viper-need-new-vi-local-map)
     (intercept-mode . viper-vi-intercept-minor-mode)
     (intercept-map . viper-vi-intercept-map))
    (insert-state
     (id . viper-insert-state-id)
     (auxiliary-modes . viper-insert-auxiliary-modes)
     (change-func . viper-change-state-to-insert)
     (basic-mode . viper-insert-basic-minor-mode)
     (basic-map . viper-insert-basic-map)
     (diehard-mode . viper-insert-diehard-minor-mode)
     (diehard-map . viper-insert-diehard-map)
     (modifier-mode . viper-insert-state-modifier-minor-mode)
     (modifier-alist . viper-insert-state-modifier-alist)
     (kbd-mode . viper-insert-kbd-minor-mode)
     (kbd-map . viper-insert-kbd-map)
     (global-user-mode . viper-insert-global-user-minor-mode)
     (global-user-map . viper-insert-global-user-map)
     (local-user-mode . viper-insert-local-user-minor-mode)
     (local-user-map . viper-insert-local-user-map)
     (need-local-map . viper-need-new-insert-local-map)
     (intercept-mode . viper-insert-intercept-minor-mode)
     (intercept-map . viper-insert-intercept-map))
    (replace-state
     (auxiliary-modes . viper-replace-auxiliary-modes)
     (id . viper-replace-state-id)
     (change-func . viper-change-state-to-replace)
     (basic-mode . viper-replace-minor-mode)
     (basic-map . viper-replace-map))
    (emacs-state
     (id . viper-emacs-state-id)
     (auxiliary-modes . viper-emacs-auxiliary-modes)
     (change-func . viper-change-state-to-emacs)
     (modifier-mode . viper-emacs-state-modifier-minor-mode)
     (modifier-alist . viper-emacs-state-modifier-alist)
     (kbd-mode . viper-emacs-kbd-minor-mode)
     (kbd-map . viper-emacs-kbd-map)
     (global-user-mode . viper-emacs-global-user-minor-mode)
     (global-user-map . viper-emacs-global-user-map)
     (local-user-mode . viper-emacs-local-user-minor-mode)
     (local-user-map . viper-emacs-local-user-map)
     (need-local-map . viper-need-new-emacs-local-map)
     (intercept-mode . viper-emacs-intercept-minor-mode)
     (intercept-map . viper-emacs-intercept-map)))
  "Alist of Vimpulse state variables.
Entries have the form (STATE . ((VAR-TYPE . VAR) ...)).
For example, the basic state keymap has the VAR-TYPE `basic-map'.")

(defvar vimpulse-state-modes-alist
  '((vi-state
     (viper-vi-intercept-minor-mode . t)
     (viper-vi-minibuffer-minor-mode . (viper-is-in-minibuffer))
     (viper-vi-local-user-minor-mode . t)
     (viper-vi-auxiliary-modes . t)
     (viper-vi-global-user-minor-mode . t)
     (viper-vi-kbd-minor-mode . (not (viper-is-in-minibuffer)))
     (viper-vi-state-modifier-minor-mode . t)
     (viper-vi-diehard-minor-mode
      . (not (or viper-want-emacs-keys-in-vi
                 (viper-is-in-minibuffer))))
     (viper-vi-basic-minor-mode . t))
    (insert-state
     (viper-insert-intercept-minor-mode . t)
     (viper-replace-minor-mode . (eq state 'replace-state))
     (viper-insert-minibuffer-minor-mode . (viper-is-in-minibuffer))
     (viper-insert-local-user-minor-mode . t)
     (viper-insert-auxiliary-modes . t)
     (viper-insert-global-user-minor-mode . t)
     (viper-insert-kbd-minor-mode . (not (viper-is-in-minibuffer)))
     (viper-insert-state-modifier-minor-mode . t)
     (viper-insert-diehard-minor-mode
      . (not (or viper-want-emacs-keys-in-insert
                 (viper-is-in-minibuffer))))
     (viper-insert-basic-minor-mode . t))
    (replace-state
     (viper-insert-intercept-minor-mode . t)
     (viper-replace-minor-mode . (eq state 'replace-state))
     (viper-replace-auxiliary-modes . t)
     (viper-insert-minibuffer-minor-mode . (viper-is-in-minibuffer))
     (viper-insert-local-user-minor-mode . t)
     (viper-insert-auxiliary-modes . t)
     (viper-insert-global-user-minor-mode . t)
     (viper-insert-kbd-minor-mode . (not (viper-is-in-minibuffer)))
     (viper-insert-state-modifier-minor-mode . t)
     (viper-insert-diehard-minor-mode
      . (not (or viper-want-emacs-keys-in-insert
                 (viper-is-in-minibuffer))))
     (viper-insert-basic-minor-mode . t))
    (emacs-state
     (viper-emacs-intercept-minor-mode . t)
     (viper-emacs-local-user-minor-mode . t)
     (viper-emacs-auxiliary-modes . t)
     (viper-emacs-global-user-minor-mode . t)
     (viper-emacs-kbd-minor-mode . (not (viper-is-in-minibuffer)))
     (viper-emacs-state-modifier-minor-mode . t)))
  "Alist of Vimpulse state mode toggling.
Entries have the form (STATE . ((MODE . EXPR) ...)), where STATE
is the name of a state, MODE is a mode associated with STATE and
EXPR is an expression with which to enable or disable MODE.
The first modes get the highest priority.")

(defvar vimpulse-state-maps-alist
  '((viper-vi-intercept-minor-mode . viper-vi-intercept-map)
    (viper-vi-minibuffer-minor-mode . viper-minibuffer-map)
    (viper-vi-local-user-minor-mode . viper-vi-local-user-map)
    (viper-vi-global-user-minor-mode . viper-vi-global-user-map)
    (viper-vi-kbd-minor-mode . viper-vi-kbd-map)
    (viper-vi-state-modifier-minor-mode
     . (if (keymapp (cdr (assoc major-mode viper-vi-state-modifier-alist)))
           (cdr (assoc major-mode viper-vi-state-modifier-alist))))
    (viper-vi-diehard-minor-mode . viper-vi-diehard-map)
    (viper-vi-basic-minor-mode . viper-vi-basic-map)
    (viper-insert-intercept-minor-mode . viper-insert-intercept-map)
    (viper-insert-minibuffer-minor-mode . viper-minibuffer-map)
    (viper-insert-local-user-minor-mode . viper-insert-local-user-map)
    (viper-insert-global-user-minor-mode . viper-insert-global-user-map)
    (viper-insert-kbd-minor-mode . viper-insert-kbd-map)
    (viper-insert-state-modifier-minor-mode
     . (if (keymapp (cdr (assoc major-mode viper-insert-state-modifier-alist)))
           (cdr (assoc major-mode viper-insert-state-modifier-alist))))
    (viper-insert-diehard-minor-mode . viper-insert-diehard-map)
    (viper-insert-basic-minor-mode . viper-insert-basic-map)
    (viper-replace-minor-mode . viper-replace-map)
    (viper-emacs-intercept-minor-mode . viper-emacs-intercept-map)
    (viper-emacs-local-user-minor-mode . viper-emacs-local-user-map)
    (viper-emacs-global-user-minor-mode . viper-emacs-global-user-map)
    (viper-emacs-kbd-minor-mode . viper-emacs-kbd-map)
    (viper-emacs-state-modifier-minor-mode
     . (if (keymapp (cdr (assoc major-mode viper-emacs-state-modifier-alist)))
           (cdr (assoc major-mode viper-emacs-state-modifier-alist)))))
  "Alist of Vimpulse modes and keymaps.
Entries have the form (MODE . MAP-EXPR), where MAP-EXPR is an
expression for determining the keymap of MODE.")

;; state-changing code: this uses the variables above
(defun vimpulse-normalize-minor-mode-map-alist ()
  "Normalize state keymaps."
  (let (local-user-mode map mode modes)
    ;; refresh `viper--intercept-key-maps'
    (setq viper--intercept-key-maps nil)
    (dolist (mode vimpulse-state-vars-alist)
      (add-to-list 'viper--intercept-key-maps
                   (cons (cdr (assq 'intercept-mode mode))
                         (eval (cdr (assq 'intercept-map mode)))) t))
    ;; refresh `viper--key-maps'
    (setq viper--key-maps (vimpulse-make-keymap-alist))
    ;; make `minor-mode-map-alist' buffer-local in older Emacs versions
    ;; lacking `emulation-mode-map-alists'
    (unless (and (fboundp 'add-to-ordered-list)
                 (boundp 'emulation-mode-map-alists))
      (set (make-local-variable 'minor-mode-map-alist)
           (viper-append-filter-alist
            (append viper--intercept-key-maps viper--key-maps)
            minor-mode-map-alist)))))

(defalias 'viper-normalize-minor-mode-map-alist 'vimpulse-normalize-minor-mode-map-alist)

(defun vimpulse-normalize-auxiliary-modes ()
  "Normalize `vimpulse-auxiliary-modes'.
Order the modes on the basis of `minor-mode-map-alist'
and remove duplicates."
  (let ((temp vimpulse-auxiliary-modes) result)
    (dolist (mode minor-mode-map-alist)
      (setq mode (car mode))
      (when (memq mode temp)
        (setq temp (delq mode temp))
        (add-to-list 'result mode t 'eq)))
    (dolist (mode temp)
      (add-to-list 'result mode t 'eq))
    (setq vimpulse-auxiliary-modes result)))

(defun vimpulse-make-toggle-alist (&optional state &rest excluded-states)
  "Make toggle alist for STATE (current if not specified)."
  (let (mode result toggle)
    (setq state (or state viper-current-state 'vi-state))
    (unless (memq state excluded-states)
      (dolist (entry (cdr (assq state vimpulse-state-modes-alist)))
        (setq toggle (cdr entry)
              entry  (car entry))
        (mapc
         (lambda (var)
           (unless (assq (car var) result)
             (if toggle
                 (add-to-list 'result var t)
               (add-to-list 'result (cons (car var) nil)))))
         (cond
          ;; state reference
          ((assq entry vimpulse-state-modes-alist)
           (apply 'vimpulse-make-toggle-alist entry state excluded-states))
          ;; auxiliary modes
          ((rassq entry vimpulse-auxiliary-modes-alist)
           (let (aux result)
             (setq entry (symbol-value entry))
             (dolist (mode vimpulse-auxiliary-modes)
               (when (and (boundp mode)
                          (symbol-value mode)
                          (assq mode entry))
                 (setq aux (cdr (assq mode entry)))
                 (unless (assq aux result)
                   (add-to-list 'result (cons aux toggle) t))))
             (when (memq major-mode vimpulse-auxiliary-modes)
               (setq aux (cdr (assq major-mode entry)))
               (unless (assq aux result)
                 (add-to-list 'result (cons aux toggle) t)))
             result))
          ;; regular mode
          (t
           (unless (assq entry result)
             (list (cons entry toggle))))))))
    result))

(defun vimpulse-make-keymap-alist (&optional state)
  "Make keymap alist for STATE (current if not specified)."
  (let (result map)
    (setq state (or state viper-current-state 'vi-state)
          result (mapcar (lambda (entry)
                           (cons (car entry)
                                 (eval (cdr (assq (car entry)
                                                  vimpulse-state-maps-alist)))))
                         (vimpulse-make-toggle-alist state)))
    (dolist (entry vimpulse-state-modes-alist)
      (dolist (mode (cdr entry))
        (setq mode (car mode))
        (unless (or (assq mode result)
                    (assq mode vimpulse-state-modes-alist)
                    (rassq mode vimpulse-auxiliary-modes-alist))
          (add-to-list 'result
                       (cons mode
                             (eval (cdr (assq mode vimpulse-state-maps-alist))))
                       t))))
    result))

(defadvice viper-refresh-mode-line (after vimpulse-states activate)
  "Refresh mode line tag for Vimpulse states."
  (let ((id (assq viper-current-state vimpulse-state-vars-alist)))
    (setq id (eval (cdr (assq 'id (cdr id)))))
    (when id
      (set (make-local-variable 'viper-mode-string) id)
      (force-mode-line-update))))

(defadvice viper-set-mode-vars-for (after vimpulse-states activate)
  "Toggle Vimpulse state modes."
  (let (enable disable)
    ;; determine which modes to enable
    (setq enable (vimpulse-make-toggle-alist state))
    ;; determine which modes to disable
    (dolist (entry vimpulse-state-modes-alist)
      (dolist (mode (mapcar 'car (cdr entry)))
        (unless (or (assq mode enable)
                    (assq mode vimpulse-state-modes-alist)
                    (rassq mode vimpulse-auxiliary-modes-alist))
          (add-to-list 'disable mode t))))
    (dolist (entry vimpulse-auxiliary-modes-alist)
      (dolist (aux (mapcar 'cdr (symbol-value (cdr entry))))
        (unless (assq aux enable)
          (add-to-list 'disable aux t))))
    ;; enable modes
    (dolist (entry enable)
      (when (boundp (car entry))
        (set (car entry) (eval (cdr entry)))))
    ;; disable modes
    (dolist (entry disable)
      (when (boundp entry)
        (set entry nil)))))

(defadvice viper-change-state (before vimpulse-states activate)
  "Update `viper-insert-point'."
  (let (mark-active)
    (unless (mark t)
      (push-mark nil t nil)))
  (when (and (eq new-state 'insert-state)
             (not (memq viper-current-state '(vi-state emacs-state))))
    (viper-move-marker-locally 'viper-insert-point (point))))

(defun vimpulse-modifier-map (state &optional mode)
  "Return the current major mode modifier map for STATE.
If none, return the empty keymap (`viper-empty-keymap')."
  (setq mode (or mode major-mode))
  (setq state (assq state vimpulse-state-vars-alist))
  (setq state (eval (cdr (assq 'modifier-alist (cdr state)))))
  (if (keymapp (cdr (assoc mode state)))
      (cdr (assoc mode state))
    (copy-keymap viper-empty-keymap)))

(defun vimpulse-modify-major-mode (mode state keymap)
  "Modify key bindings in a major-mode in a Viper state using a keymap.

If the default for a major mode is emacs-state, then
modifications to this major mode may not take effect until the
buffer switches state to Vi, Insert or Emacs. If this happens,
add `viper-change-state-to-emacs' to this major mode's hook.
If no such hook exists, you may have to put an advice on the
function that invokes the major mode. See `viper-set-hooks'
for hints.

The above needs not to be done for major modes that come up in
Vi or Insert state by default."
  (let (alist elt)
    (setq alist (cdr (assq state vimpulse-state-vars-alist)))
    (setq alist (cdr (assq 'modifier-alist alist)))
    (if (setq elt (assoc mode (eval alist)))
        (set alist (delq elt (eval alist))))
    (set alist (cons (cons mode keymap) (eval alist)))
    (viper-normalize-minor-mode-map-alist)
    (viper-set-mode-vars-for viper-current-state)))

(defalias 'viper-modify-major-mode 'vimpulse-modify-major-mode)

(defun vimpulse-add-local-keys (state alist)
  "Override some vi-state or insert-state bindings in the current buffer.
The effect is seen in the current buffer only.
Useful for customizing  mailer buffers, gnus, etc.
STATE is 'vi-state, 'insert-state, or 'emacs-state
ALIST is of the form ((key . func) (key . func) ...)
Normally, this would be called from a hook to a major mode or
on a per buffer basis.
Usage:
      (viper-add-local-keys state '((key-str . func) (key-str . func)...))"
  (let (local-user-map need-local-map)
    (setq local-user-map (cdr (assq state vimpulse-state-vars-alist)))
    (when local-user-map
      (setq need-local-map
            (cdr (assq 'need-local-map local-user-map)))
      (setq local-user-map
            (cdr (assq 'local-user-map local-user-map)))
      (when (symbol-value need-local-map)
        (set local-user-map (make-sparse-keymap))
        (set need-local-map nil))
      (viper-modify-keymap (symbol-value local-user-map) alist)
      (viper-normalize-minor-mode-map-alist)
      (viper-set-mode-vars-for viper-current-state))))

(defalias 'viper-add-local-keys 'vimpulse-add-local-keys)

(eval-and-compile
  (defun vimpulse-unquote (exp)
    "Return EXP unquoted."
    (if (and (listp exp)
             (eq (car exp) 'quote))
        (eval exp)
      exp)))

;; Macro for defining new Viper states. This saves us the trouble of
;; defining and indexing all those minor modes manually.
(defmacro vimpulse-define-state (state doc &rest body)
  "Define a new Viper state STATE.
DOC is a general description and shows up in all docstrings.
Then follows one or more optional keywords:

:id ID                  Mode line indicator.
:hook LIST              Hooks run before changing to STATE.
:change-func FUNC       Function to change to STATE.
:basic-mode MODE        Basic minor mode for STATE.
:basic-map MAP          Keymap of :basic-mode.
:diehard-mode MODE      Minor mode for when Viper wants to be vi.
:diehard-map MAP        Keymap of :diehard-mode.
:modifier-mode MODE     Minor mode for modifying major modes.
:modifier-alist LIST    Keymap alist for :modifier-mode.
:kbd-mode MODE          Minor mode for Ex command macros.
:kbd-map MAP            Keymap of :kbd-mode.
:global-user-mode MODE  Minor mode for global user bindings.
:global-user-map MAP    Keymap of :global-user-mode.
:local-user-mode MODE   Minor mode for local user bindings.
:local-user-map MAP     Keymap of :local-user-mode.
:need-local-map VAR     Buffer-local variable for :local-user-mode.
:intercept-mode         Minor mode for vital Viper bindings.
:intercept-map          Keymap of :intercept-mode.
:enable LIST            List of other modes enabled by STATE.
:prefix PREFIX          Variable prefix, default \"vimpulse-\".
:advice TYPE            Toggle advice type, default `after'.

It is not necessary to specify all of these; the minor modes are
created automatically unless one provides an existing mode. The
only keyword one should really specify is :id, the mode line tag.
For example:

    (vimpulse-define-state test
      \"A simple test state.\"
      :id \"<T> \")

The basic keymap of this state will then be
`vimpulse-test-basic-map', and so on.

Following the keywords is optional code to be executed each time
the state is enabled or disabled. This is stored in a `defadvice'
of `viper-change-state'. :advice specifies the advice type
\(default `after'). The advice runs :hook before completing."
  (declare (debug (&define name stringp
                           [&rest [keywordp sexp]]
                           def-body))
           (indent defun))
  (let (advice auxiliary-modes basic-map basic-mode change-func
        diehard-map diehard-mode enable global-user-map
        global-user-mode hook id intercept-map intercept-mode kbd-map
        kbd-mode keyword local-user-map local-user-mode modifier-alist
        modifier-mode name name-string need-local-map prefix
        prefixed-name-string state-name state-name-string)
    ;; collect keywords
    (while (keywordp (setq keyword (car body)))
      (setq body (cdr body))
      (cond
       ((eq keyword :prefix)
        (setq prefix (vimpulse-unquote (pop body))))
       ((eq keyword :enable)
        (setq enable (vimpulse-unquote (pop body))))
       ((eq keyword :advice)
        (setq advice (vimpulse-unquote (pop body))))
       ((memq keyword '(:state-id :id))
        (setq id (vimpulse-unquote (pop body))))
       ((memq keyword '(:state-hook :hook))
        (setq hook (vimpulse-unquote (pop body))))
       ((memq keyword '(:change-func :change))
        (setq change-func (vimpulse-unquote (pop body))))
       ((memq keyword '(:basic-mode :basic-minor-mode))
        (setq basic-mode (vimpulse-unquote (pop body))))
       ((eq keyword :basic-map)
        (setq basic-map (vimpulse-unquote (pop body))))
       ((memq keyword '(:local-user-mode
                        :local-mode
                        :local-user-minor-mode))
        (setq local-user-mode (vimpulse-unquote (pop body))))
       ((memq keyword '(:local-user-map :local-map))
        (setq local-user-map (vimpulse-unquote (pop body))))
       ((memq keyword '(:need-new-local-map
                        :need-local-map
                        :need-map))
        (setq need-local-map (vimpulse-unquote (pop body))))
       ((memq keyword '(:global-user-mode
                        :global-mode
                        :global-user-minor-mode))
        (setq global-user-mode (vimpulse-unquote (pop body))))
       ((memq keyword '(:global-user-map :global-map))
        (setq global-user-map (vimpulse-unquote (pop body))))
       ((memq keyword '(:state-modifier-minor-mode
                        :state-modifier-mode
                        :modifier-minor-mode
                        :modifier-mode))
        (setq modifier-mode (vimpulse-unquote (pop body))))
       ((memq keyword '(:state-modifier-alist :modifier-alist))
        (setq modifier-alist (vimpulse-unquote (pop body))))
       ((memq keyword '(:diehard-mode :diehard-minor-mode))
        (setq diehard-mode (vimpulse-unquote (pop body))))
       ((eq keyword :diehard-map)
        (setq diehard-map (vimpulse-unquote (pop body))))
       ((memq keyword '(:kbd-mode :kbd-minor-mode))
        (setq kbd-mode (vimpulse-unquote (pop body))))
       ((eq keyword :kbd-map)
        (setq kbd-map (vimpulse-unquote (pop body))))
       ((memq keyword '(:intercept-mode :intercept-minor-mode))
        (setq intercept-mode (vimpulse-unquote (pop body))))
       ((eq keyword :intercept-map)
        (setq intercept-map (vimpulse-unquote (pop body))))
       (t
        (pop body))))
    ;; set up the state name etc.
    (setq name-string (replace-regexp-in-string
                       "-state$" "" (symbol-name state)))
    (setq name (intern name-string))
    (setq state-name-string (concat name-string "-state"))
    (setq state-name (intern state-name-string))
    (when (and prefix (symbolp prefix))
      (setq prefix (symbol-name prefix)))
    (setq prefix (or prefix "vimpulse-"))
    (setq prefix (concat (replace-regexp-in-string
                          "-$" "" prefix) "-"))
    (setq prefixed-name-string (concat prefix name-string))
    (setq advice (or advice 'after))
    (setq auxiliary-modes (intern (concat prefixed-name-string
                                          "-auxiliary-modes")))
    (unless (and change-func (symbolp change-func))
      (setq change-func
            (intern (concat prefix "change-state-to-" name-string))))
    ;; macro expansion
    `(progn
       ;; define change function
       (defun ,change-func ()
         ,(format "Change Viper state to %s." state-name)
         (viper-change-state ',state-name))
       ;; define state variables etc.
       (let* ((advice ',advice)
              (auxiliary-modes ',auxiliary-modes)
              (change-func ',change-func)
              (doc ',doc)
              (enable ',enable)
              (name ',name)
              (name-string ',name-string)
              (prefix ',prefix)
              (prefixed-name-string ',prefixed-name-string)
              (state-name ',state-name)
              (state-name-string ',state-name-string)
              (basic-map (vimpulse-define-symbol
                          ',basic-map (concat prefixed-name-string
                                              "-basic-map")
                          (make-sparse-keymap) 'keymapp
                          (format "The basic %s keymap.\n\n%s"
                                  state-name doc)))
              (basic-mode (vimpulse-define-symbol
                           ',basic-mode
                           (concat prefixed-name-string
                                   "-basic-minor-mode")
                           nil nil
                           (format "Basic minor mode for %s.\n\n%s"
                                   state-name doc) t))
              (diehard-map (vimpulse-define-symbol
                            ',diehard-map
                            (concat prefixed-name-string
                                    "-diehard-map")
                            (make-sparse-keymap) 'keymapp
                            (format "This keymap is in use when the \
user asks Viper to simulate vi very closely.
This happens when `viper-expert-level' is 1 or 2.  \
See `viper-set-expert-level'.\n\n%s" doc)))
              (diehard-mode (vimpulse-define-symbol
                             ',diehard-mode
                             (concat prefixed-name-string
                                     "-diehard-minor-mode")
                             nil nil
                             (format "This minor mode is in effect \
when the user wants Viper to be vi.\n\n%s" doc) t))
              (global-user-map (vimpulse-define-symbol
                                ',global-user-map
                                (concat prefixed-name-string
                                        "-global-user-map")
                                (make-sparse-keymap) 'keymapp
                                (format "Auxiliary map for global \
user-defined keybindings in %s.\n\n%s" state-name doc)))
              (global-user-mode (vimpulse-define-symbol
                                 ',global-user-mode
                                 (concat prefixed-name-string
                                         "-global-user-minor-mode")
                                 nil nil
                                 (format "Auxiliary minor mode for \
global user-defined bindings in %s.\n\n%s" state-name doc) t))
              (hook (vimpulse-define-symbol
                     ',hook (concat prefixed-name-string
                                    "-state-hook")
                     nil 'listp
                     (format "*Hooks run just before the switch to %s \
is completed.\n\n%s" state-name doc)))
              (id (vimpulse-define-symbol
                   ',id (concat prefixed-name-string "-state-id")
                   (format "<%s> " (upcase name-string)) 'stringp
                   (format "Mode line tag indicating %s.\n\n%s"
                           state-name doc)))
              (intercept-map (vimpulse-define-symbol
                              ',intercept-map
                              (concat prefixed-name-string
                                      "-intercept-map")
                              viper-vi-intercept-map 'keymapp
                              (format "Keymap for binding Viper's \
vital keys.\n\n%s" doc)))
              (intercept-mode (vimpulse-define-symbol
                               ',intercept-mode
                               (concat prefixed-name-string
                                       "-intercept-minor-mode")
                               nil nil
                               (format "Mode for binding Viper's \
vital keys.\n\n%s" doc)))
              (kbd-map (vimpulse-define-symbol
                        ',kbd-map
                        (concat prefixed-name-string "-kbd-map")
                        (make-sparse-keymap) 'keymapp
                        (format "This keymap keeps keyboard macros \
defined via the :map command.\n\n%s" doc)))
              (kbd-mode (vimpulse-define-symbol
                         ',kbd-mode
                         (concat prefixed-name-string
                                 "-kbd-minor-mode")
                         nil nil
                         (format "Minor mode for Ex command macros \
in Vi state.
The corresponding keymap stores key bindings of Vi macros defined with
the Ex command :map.\n\n%s" doc) t))
              (local-user-map (vimpulse-define-symbol
                               ',local-user-map
                               (concat prefixed-name-string
                                       "-local-user-map")
                               (make-sparse-keymap) 'keymapp
                               (format "Auxiliary map for per-buffer \
user-defined keybindings in %s.\n\n%s" state-name doc) t))
              (local-user-mode (vimpulse-define-symbol
                                ',local-user-mode
                                (concat prefixed-name-string
                                        "-local-user-minor-mode")
                                nil nil
                                (format "Auxiliary minor mode for \
user-defined local bindings in %s.\n\n%s" state-name doc) t))
              (modifier-alist (vimpulse-define-symbol
                               ',modifier-alist
                               (concat prefixed-name-string
                                       "-state-modifier-alist")
                               nil 'listp))
              (modifier-mode (vimpulse-define-symbol
                              ',modifier-mode
                              (concat prefixed-name-string
                                      "-state-modifier-minor-mode")
                              nil nil
                              (format "Minor mode used to make major \
mode-specific modifications to %s.\n\n%s" state-name doc) t))
              (need-local-map (vimpulse-define-symbol
                               ',need-local-map
                               (concat prefix "need-new-"
                                       name-string "-local-map")
                               t (lambda (val) (eq val t)) nil t))
              enable-modes-alist enable-states-alist
              modes-alist vars-alist)
         (put need-local-map 'permanent-local t)
         (defvar ,auxiliary-modes nil)
         (add-to-list 'vimpulse-auxiliary-modes-alist
                      (cons ',state-name ',auxiliary-modes) t)
         ;; remove old index entries
         (dolist (entry (list basic-mode
                              diehard-mode
                              modifier-mode
                              kbd-mode
                              global-user-mode
                              local-user-mode
                              intercept-mode))
           (setq vimpulse-state-maps-alist
                 (assq-delete-all entry vimpulse-state-maps-alist)))
         (setq vimpulse-state-modes-alist
               (assq-delete-all state-name vimpulse-state-modes-alist))
         (setq vimpulse-state-vars-alist
               (assq-delete-all state-name vimpulse-state-vars-alist))
         ;; index keymaps
         (add-to-list 'vimpulse-state-maps-alist
                      (cons basic-mode basic-map))
         (add-to-list 'vimpulse-state-maps-alist
                      (cons diehard-mode diehard-map))
         (add-to-list 'vimpulse-state-maps-alist
                      (cons modifier-mode
                            `(if (keymapp
                                  (cdr (assoc major-mode
                                              ,modifier-alist)))
                                 (cdr (assoc major-mode
                                             ,modifier-alist)))))
         (add-to-list 'vimpulse-state-maps-alist
                      (cons kbd-mode kbd-map))
         (add-to-list 'vimpulse-state-maps-alist
                      (cons global-user-mode global-user-map))
         (add-to-list 'vimpulse-state-maps-alist
                      (cons local-user-mode local-user-map))
         (add-to-list 'vimpulse-state-maps-alist
                      (cons intercept-mode intercept-map))
         ;; index minor mode toggling: first, sort lists from symbols
         ;; in :enable
         (unless (listp enable)
           (setq enable (list enable)))
         (dolist (entry enable)
           (let ((mode entry) (val t))
             (when (listp entry)
               (setq mode (car entry)
                     val  (cadr entry)))
             (when (and mode (symbolp mode))
               (add-to-list 'enable-modes-alist (cons mode val) t))))
         ;; then add the state's own modes to the front if they're not
         ;; already there
         (dolist (mode (list (cons basic-mode t)
                             (cons diehard-mode
                                   '(not (or viper-want-emacs-keys-in-vi
                                             (viper-is-in-minibuffer))))
                             (cons modifier-mode t)
                             (cons kbd-mode '(not (viper-is-in-minibuffer)))
                             (cons global-user-mode t)
                             (cons auxiliary-modes t)
                             (cons local-user-mode t)
                             (cons intercept-mode t)))
           (unless (assq (car mode) enable-modes-alist)
             (add-to-list 'enable-modes-alist mode)))
         ;; add the result to `vimpulse-state-modes-alist'
         (add-to-list 'vimpulse-state-modes-alist
                      (cons state-name enable-modes-alist) t)
         (viper-normalize-minor-mode-map-alist)
         ;; index state variables
         (setq vars-alist
               (list (cons 'id id)
                     (cons 'hook hook)
                     (cons 'auxiliary-modes auxiliary-modes)
                     (cons 'change-func change-func)
                     (cons 'basic-mode basic-mode)
                     (cons 'basic-map basic-map)
                     (cons 'diehard-mode diehard-mode)
                     (cons 'diehard-map diehard-map)
                     (cons 'modifier-mode modifier-mode)
                     (cons 'modifier-alist modifier-alist)
                     (cons 'kbd-mode kbd-mode)
                     (cons 'kbd-map kbd-map)
                     (cons 'global-user-mode global-user-mode)
                     (cons 'global-user-map global-user-map)
                     (cons 'local-user-mode local-user-mode)
                     (cons 'local-user-map local-user-map)
                     (cons 'need-local-map need-local-map)
                     (cons 'intercept-mode intercept-mode)
                     (cons 'intercept-map intercept-map)))
         (add-to-list 'vimpulse-state-vars-alist
                      (cons state-name vars-alist) t)
         ;; make toggle-advice
         (eval `(defadvice viper-change-state (,advice ,state-name activate)
                  ,(format "Toggle %s." state-name)
                  ,',@body
                  (when (eq new-state ',state-name)
                    (run-hooks ',hook))))
         ',state))))

(when (fboundp 'font-lock-add-keywords)
  (font-lock-add-keywords
   'emacs-lisp-mode
   '(("(\\(vimpulse-define-state\\)\\>[ \f\t\n\r\v]*\\(\\sw+\\)?"
      (1 font-lock-keyword-face)
      (2 font-lock-function-name-face nil t)))))

(defun vimpulse-define-symbol
  (sym-or-val varname varval &optional val-p doc local)
  "Accept a symbol or a value and define a variable for it.
If SYM-OR-VAL is a symbol, set that symbol's value to VARVAL.
If SYM-OR-VAL is a value, set VARNAME's value to SYM-OR-VAL.
VAL-P checks whether SYM-OR-VAL's value is \"valid\", in which
case it is kept; otherwise we default to VARVAL. DOC is the
docstring for the defined variable. If LOCAL is non-nil,
create a buffer-local variable. Returns the result."
  (cond
   ((and sym-or-val (symbolp sym-or-val)) ; nil is a symbol
    (setq varname sym-or-val))
   ((or (not val-p) (funcall val-p sym-or-val))
    (setq varval sym-or-val)))
  (when (stringp varname)
    (setq varname (intern varname)))
  (unless (and (boundp varname) val-p
               (funcall val-p (eval varname)))
    (eval `(defvar ,varname (quote ,varval) ,doc))
    (set varname varval)
    (when local
      (make-variable-buffer-local varname)))
  varname)

;;; Viper bugs (should be forwarded to Michael Kifer)

;; `viper-deflocalvar's definition lacks a `declare' statement,
;; so Emacs tends to mess up the indentation. Luckily, the
;; relevant symbol properties can be set up with `put'.
;; TODO: E-mail Michael Kifer about updating the definition.
(put 'viper-deflocalvar 'lisp-indent-function 'defun)
(put 'viper-loop 'lisp-indent-function 'defun)
(put 'viper-deflocalvar 'function-documentation
     "Define VAR as a buffer-local variable.
DEFAULT-VALUE is the default value and DOCUMENTATION is the
docstring. The variable becomes buffer-local whenever set.")

(when (fboundp 'font-lock-add-keywords)
  (font-lock-add-keywords
   'emacs-lisp-mode
   '(("(\\(viper-deflocalvar\\)\\>[ \f\t\n\r\v]*\\(\\sw+\\)?"
      (1 font-lock-keyword-face)
      (2 font-lock-variable-name-face nil t))
     ("(\\(viper-loop\\)\\>" 1 font-lock-keyword-face))))

;; search bug: `viper-search' flashes twice when search wraps
(defun vimpulse-search
  (string forward arg
          &optional no-offset init-point fail-if-not-found dont-flash)
  (if (not (equal string ""))
      (let ((val (viper-p-val arg))
            (com (viper-getcom arg))
            (offset (not no-offset))
            (start-point (or init-point (point))))
        (viper-deactivate-mark)
        ;; smartcase searching: upper-case chars disable case folding
        (when search-upper-case
          (setq case-fold-search
                (isearch-no-upper-case-p
                 viper-s-string viper-re-search)))
        (if forward
            (condition-case nil
                (progn
                  (if offset (viper-forward-char-carefully))
                  (if viper-re-search
                      (progn
                        (re-search-forward string nil nil val)
                        (re-search-backward string))
                    (search-forward string nil nil val)
                    (search-backward string))
                  (if (not (equal (point) start-point))
                      (push-mark start-point t)))
              (search-failed
               (if (and (not fail-if-not-found)
                        viper-search-wrap-around)
                   (progn
                     (message "Search wrapped around BOTTOM of buffer")
                     (goto-char (point-min))
                     (viper-search string forward (cons 1 com)
                                   t start-point 'fail)
                     (setq dont-flash t)
                     ;; don't wait in macros
                     (or executing-kbd-macro
                         (memq viper-intermediate-command
                               '(viper-repeat
                                 viper-digit-argument
                                 viper-command-argument))
                         (sit-for 2))
                     ;; delete the wrap-around message
                     (message ""))
                 (goto-char start-point)
                 (error "`%s': %s not found"
                        string
                        (if viper-re-search "Pattern" "String")))))
          ;; backward
          (condition-case nil
              (progn
                (if viper-re-search
                    (re-search-backward string nil nil val)
                  (search-backward string nil nil val))
                (if (not (equal (point) start-point))
                    (push-mark start-point t)))
            (search-failed
             (if (and (not fail-if-not-found) viper-search-wrap-around)
                 (progn
                   (message "Search wrapped around TOP of buffer")
                   (goto-char (point-max))
                   (viper-search string forward (cons 1 com)
                                 t start-point 'fail)
                   (setq dont-flash t)
                   ;; don't wait in macros
                   (or executing-kbd-macro
                       (memq viper-intermediate-command
                             '(viper-repeat
                               viper-digit-argument
                               viper-command-argument))
                       (sit-for 2))
                   ;; delete the wrap-around message
                   (message ""))
               (goto-char start-point)
               (error "`%s': %s not found"
                      string
                      (if viper-re-search "Pattern" "String"))))))
        ;; pull up or down if at top/bottom of window
        (viper-adjust-window)
        ;; highlight the result of search;
        ;; don't wait and don't highlight in macros
        (or dont-flash
            executing-kbd-macro
            (memq viper-intermediate-command
                  '(viper-repeat
                    viper-digit-argument
                    viper-command-argument))
            (viper-flash-search-pattern)))))

(defalias 'viper-search 'vimpulse-search)

;; e/E bug: on a single-letter word, ce may change two words
(defun vimpulse-end-of-word-kernel ()
  (when (viper-looking-at-separator)
    (viper-skip-all-separators-forward))
  (cond
   ((viper-looking-at-alpha)
    (viper-skip-alpha-forward "_"))
   ((not (viper-looking-at-alphasep))
    (viper-skip-nonalphasep-forward))))

(defun vimpulse-end-of-word (arg &optional careful)
  "Move point to end of current word."
  (interactive "P")
  (viper-leave-region-active)
  (let ((val (viper-p-val arg))
        (com (viper-getcom arg)))
    (cond
     (com
      (viper-move-marker-locally 'viper-com-point (point))
      (when (and (not (viper-looking-at-alpha))
                 (not (viper-looking-at-alphasep)))
        (setq val (1+ val))))
     ((viper-end-of-word-p)
      (setq val (1+ val))))
    (viper-loop val (viper-end-of-word-kernel))
    (if com
        (viper-execute-com 'viper-end-of-word val com)
      (viper-backward-char-carefully))))

(defun vimpulse-end-of-Word (arg)
  "Forward to end of word delimited by white character."
  (interactive "P")
  (viper-leave-region-active)
  (let ((val (viper-p-val arg))
        (com (viper-getcom arg)))
    (cond
     (com
      (viper-move-marker-locally 'viper-com-point (point)))
     ((save-excursion
        (viper-forward-char-carefully)
        (or (eolp) (memq (char-syntax (char-after)) '(?\  ?- nil))))
      (setq val (1+ val))))
    (viper-loop val
      (viper-end-of-word-kernel)
      (viper-skip-nonseparators 'forward))
    (if com
        (viper-execute-com 'viper-end-of-Word val com)
      (viper-backward-char-carefully))))

(defalias 'viper-end-of-word-kernel 'vimpulse-end-of-word-kernel)
(defalias 'viper-end-of-word 'vimpulse-end-of-word)
(defalias 'viper-end-of-Word 'vimpulse-end-of-Word)

;;;; General utility code used by all of Vimpulse;
;;;; may be useful to the end user

;;; Autogenerated vi bindings

(defun vimpulse-augment-keymap
  (map augment-alist &optional replace)
  "Augment MAP with bindings from AUGMENT-ALIST.
If REPLACE is non-nil, bindings in MAP may be overwritten.
AUGMENT-ALIST has the format ((KEY . DEF) ...),
where KEY and DEF are passed to `define-key'."
  (let (key def num)
    (dolist (binding augment-alist)
      (setq key (car binding)
            def (cdr binding)
            num (lookup-key map key))
      (cond
       (replace
        (when (numberp num)
          (define-key map (vimpulse-truncate key num) nil))
        (define-key map key def))
       (t
        (when (numberp num)
          (setq num (lookup-key map (vimpulse-truncate key num))))
        (unless num
          (define-key map key def)))))))

(defun vimpulse-add-vi-bindings (map cmds &optional replace filter)
  "Add vi bindings for CMDS to MAP.
Add forcefully if REPLACE is t. Don't add keys matching FILTER,
which is a list of key vectors."
  (let ((bindings (apply 'vimpulse-get-vi-bindings cmds)))
    (unless filter
      (when (and (boundp 'viper-want-ctl-h-help)
                 viper-want-ctl-h-help)
        (add-to-list 'filter [?\C-h]))
      (unless (and (boundp 'vimpulse-want-C-u-like-Vim)
                   vimpulse-want-C-u-like-Vim)
        (add-to-list 'filter [?\C-u])))
    (dolist (key filter)
      (setq bindings (assq-delete-all key bindings)))
    (vimpulse-augment-keymap map bindings replace)))

(defun vimpulse-get-bindings (cmd &rest maps)
  "Return assocation list of bindings for CMD in MAPS."
  (let (keys bindings)
    (setq maps (or maps '(nil)))
    (dolist (map maps bindings)
      (unless (keymapp map)
        (setq map (eval map)))
      (setq keys (where-is-internal cmd map))
      (dolist (key keys)
        (unless (assq key bindings)
          (add-to-list 'bindings (cons key cmd) t))))))

(defun vimpulse-get-vi-bindings (&rest cmds)
  "Return assocation list of vi bindings for CMDS."
  (let (bindings)
    (dolist (cmd cmds bindings)
      (dolist (binding (apply 'vimpulse-get-bindings cmd
                              '(viper-vi-intercept-map
                                viper-vi-local-user-map
                                viper-vi-global-user-map
                                viper-vi-kbd-map
                                viper-vi-diehard-map
                                viper-vi-basic-map)))
        (unless (assq (car binding) bindings)
          (add-to-list 'bindings binding t))))))

(defun vimpulse-add-movement-cmds (map &optional replace)
  "Add Viper/Vimpulse movement commands to MAP.
The commands are taken from `vimpulse-viper-movement-cmds' and looked
up in vi keymaps. If REPLACE is non-nil, may overwrite bindings
in MAP."
  (vimpulse-add-vi-bindings map vimpulse-viper-movement-cmds replace))

;; the default for this function is to replace rather than augment,
;; as core navigation should be present everywhere
(defun vimpulse-add-core-movement-cmds (map &optional augment)
  "Add \"core\" movement commands to MAP, forcefully.
The commands are taken from `vimpulse-core-movement-cmds'.
If AUGMENT is non-nil, don't overwrite bindings in MAP."
  (vimpulse-add-vi-bindings map
                            vimpulse-core-movement-cmds
                            (not augment)))

(defun vimpulse-inhibit-cmds (map cmds &optional replace)
  "Remap CMDS to `viper-nil' in MAP.
REPLACE is passed to `vimpulse-augment-keymap'."
  (vimpulse-augment-keymap
   map (mapcar (lambda (cmd)
                 (cons `[remap ,cmd] 'viper-nil))
               cmds) replace))

(defun vimpulse-inhibit-movement-cmds (map &optional replace)
  "Remap Viper movement commands to `viper-nil' in MAP.
The commands are taken from `vimpulse-viper-movement-cmds'.
If REPLACE is non-nil, may overwrite bindings in MAP."
  (vimpulse-inhibit-cmds map vimpulse-viper-movement-cmds replace))

(defun vimpulse-inhibit-other-movement-cmds (map &optional replace)
  "Remap non-core Viper movement commands to `viper-nil' in MAP.
The commands are taken from `vimpulse-viper-movement-cmds'.
If REPLACE is non-nil, may overwrite bindings in MAP."
  (let ((cmds vimpulse-viper-movement-cmds))
    ;; remove core movement commands
    (dolist (cmd vimpulse-core-movement-cmds)
      (setq cmds (delq cmd cmds)))
    (vimpulse-inhibit-cmds map cmds replace)))

(defun vimpulse-inhibit-destructive-cmds (map &optional replace)
  "Remap destructive Viper commands to `viper-nil' in MAP."
  (let ((cmds '(viper-Append
                viper-Insert
                viper-append
                viper-change-to-eol
                viper-command-argument
                viper-insert
                viper-kill-line
                viper-substitute
                viper-substitute-line
                vimpulse-change
                vimpulse-delete
                vimpulse-visual-append
                vimpulse-visual-insert)))
    (vimpulse-inhibit-cmds map cmds replace)))

(defmacro vimpulse-remap (keymap from to)
  "Remap FROM to TO in KEYMAP.
For XEmacs compatibility, KEYMAP should have a `remap-alist'
property referring to a variable used for storing a \"remap
association list\"."
  (if (featurep 'xemacs)
      `(let ((remap-alist (get ',keymap 'remap-alist))
             (from ,from) (to ,to))
         (when remap-alist
           (add-to-list remap-alist (cons from to))))
    `(let ((keymap ,keymap) (from ,from) (to ,to))
       (define-key keymap `[remap ,from] to))))

(defun vimpulse-vi-remap (from to &optional keymap)
  "Remap FROM to TO in vi (command) state.
If KEYMAP is specified, take the keys that FROM is bound to
in vi state and bind them to TO in KEYMAP."
  (if keymap
      (vimpulse-augment-keymap
       keymap
       (mapcar (lambda (binding)
                 (cons (car binding) to))
               (vimpulse-get-vi-bindings from)))
    (define-key viper-vi-basic-map `[remap ,from] to)))

;;; States

(defmacro vimpulse-with-state (state &rest body)
  "Execute BODY with Viper state STATE, then restore previous state."
  (declare (indent defun))
  `(let ((new-viper-state ,state)
         (old-viper-state viper-current-state))
     (unwind-protect
         (progn
           (viper-set-mode-vars-for new-viper-state)
           (let ((viper-current-state new-viper-state))
             (viper-normalize-minor-mode-map-alist)
             ,@body))
       (viper-set-mode-vars-for old-viper-state)
       (viper-normalize-minor-mode-map-alist))))

(when (fboundp 'font-lock-add-keywords)
  (font-lock-add-keywords
   'emacs-lisp-mode
   '(("(\\(vimpulse-with-state\\)\\>" 1 font-lock-keyword-face))))

;;; Vector tools

(defun vimpulse-truncate (vector length &optional offset)
  "Return a copy of VECTOR truncated to LENGTH.
If LENGTH is negative, skip last elements of VECTOR.
If OFFSET is specified, skip first elements of VECTOR."
  ;; if LENGTH is too large, trim it
  (when (> length (length vector))
    (setq length (length vector)))
  ;; if LENGTH is negative, convert it to the positive equivalent
  (when (< length 0)
    (setq length (max 0 (+ (length vector) length))))
  (if offset
      (setq length (- length offset))
    (setq offset 0))
  (let ((result (make-vector length t)))
    (dotimes (idx length result)
      (aset result idx (aref vector (+ idx offset))))))

;; This is useful for deriving a "standard" key-sequence from
;; `this-command-keys', to be looked up in `vimpulse-careful-alist'.
(defun vimpulse-strip-prefix (key-sequence &optional string)
  "Strip any prefix argument keypresses from KEY-SEQUENCE.
If STRING is t, output a string if possible.
Otherwise output a vector."
  (let* ((offset 0)
         (temp-sequence (vconcat key-sequence))
         (key (aref temp-sequence offset))
         (length (length temp-sequence))
         temp-string)
    ;; if XEmacs, get rid of the event object type
    (and (featurep 'xemacs) (eventp key)
         (setq key (event-to-character key nil t)))
    ;; any keys bound to `universal-argument', `digit-argument' or
    ;; `negative-argument' or bound in `universal-argument-map'
    ;; are considered prefix keys
    (while (and (or (memq (key-binding (vector key) t)
                          '(universal-argument
                            digit-argument
                            negative-argument))
                    (lookup-key universal-argument-map
                                (vector key)))
                (setq offset (1+ offset))
                (< offset length))
      (setq key (aref temp-sequence offset))
      (and (featurep 'xemacs) (eventp key)
           (setq key (event-to-character key nil t))))
    (if (and string
             ;; string conversion is impossible if the vector
             ;; contains a non-numerical element
             (not (memq nil (mapcar 'integerp (append temp-sequence nil)))))
        (concat (vimpulse-truncate temp-sequence length offset))
      (vimpulse-truncate temp-sequence length offset))))

;; GNU Emacs 22 lacks `characterp'
(unless (fboundp 'characterp)
  (defalias 'characterp 'integerp))

;;; Movement

(defun vimpulse-move-to-column (column &optional dir force)
  "Move point to column COLUMN in the current line.
Places point at left of the tab character (at the right
if DIR is non-nil) and returns point.
If `vimpulse-visual-block-untabify' is non-nil, then
tabs are changed to spaces. (FORCE untabifies regardless.)"
  (interactive "p")
  (if (or vimpulse-visual-block-untabify force)
      (move-to-column column t)
    (move-to-column column)
    (when (or (not dir) (and (numberp dir) (< dir 1)))
      (when (> (current-column) column)
        (unless (bolp)
          (backward-char)))))
  (point))

(defmacro vimpulse-limit (start end &rest body)
  "Eval BODY, but limit point to buffer-positions START and END.
Both may be nil. Returns position."
  (declare (indent 2))
  `(let ((start (or ,start (point-min)))
         (end   (or ,end   (point-max))))
     (when (> start end)
       (setq start (prog1 end
                     (setq end start))))
     (save-restriction
       (narrow-to-region start end)
       ,@body
       (point))))

(defmacro vimpulse-skip (dir bounds &rest body)
  "Eval BODY, but limit point to BOUNDS in DIR direction.
Returns position."
  (declare (indent 2))
  `(let ((dir ,dir) (bounds ,bounds) start end)
     (setq dir (if (and (numberp dir) (< dir 0)) -1 1))
     (dolist (bound bounds)
       (unless (numberp bound)
         (setq bounds (delq bound bounds))))
     (when bounds
       (if (< dir 0)
           (setq start (apply 'min bounds))
         (setq end (apply 'max bounds))))
     (vimpulse-limit start end ,@body)))

(defun vimpulse-skip-regexp (regexp dir &rest bounds)
  "Move point in DIR direction based on REGEXP and BOUNDS.
REGEXP is passed to `looking-at' or `looking-back'.
If DIR is positive, move forwards to the end of the regexp match,
but not beyond any buffer positions listed in BOUNDS.
If DIR is negative, move backwards to the beginning of the match.
Returns the new position."
  (setq dir (if (and (numberp dir) (< dir 0)) -1 1))
  (setq regexp (or regexp ""))
  (vimpulse-skip dir bounds
    (if (< dir 0)
        (when (looking-back regexp nil t)
          (goto-char (match-beginning 0)))
      (when (looking-at regexp)
        (goto-char (match-end 0))))))

;; XEmacs only has `looking-at'
(unless (fboundp 'looking-back)
  (defun looking-back (regexp &optional limit greedy)
    "Return t if text before point matches regular expression REGEXP."
    (let ((start (point))
          (pos
           (save-excursion
             (and (re-search-backward
                   (concat "\\(?:" regexp "\\)\\=") limit t)
                  (point)))))
      (if (and greedy pos)
          (save-restriction
            (narrow-to-region (point-min) start)
            (while (and (> pos (point-min))
                        (save-excursion
                          (goto-char pos)
                          (backward-char 1)
                          (looking-at
                           (concat "\\(?:" regexp "\\)\\'"))))
              (setq pos (1- pos)))
            (save-excursion
              (goto-char pos)
              (looking-at (concat "\\(?:"  regexp "\\)\\'")))))
      (not (null pos)))))

(defun vimpulse-backward-up-list (&optional arg)
  "Like `backward-up-list', but breaks out of strings."
  (interactive "p")
  (let ((orig (point)))
    (setq arg (or arg 1))
    (while (progn
             (condition-case
                 nil (backward-up-list arg)
               (error nil))
             (when (eq (point) orig)
               (backward-char)
               (setq orig (point)))))))

;;; Region

;; GNU Emacs 22 lacks `region-active-p'
(unless (fboundp 'region-active-p)
  (defun region-active-p ()
    (and transient-mark-mode mark-active)))

(defun vimpulse-region-face ()
  "Return face of region."
  (if (featurep 'xemacs) 'zmacs-region 'region))

(defun vimpulse-deactivate-region (&optional now)
  "Deactivate region, respecting Emacs version."
  (cond
   ((and (boundp 'cua-mode) cua-mode
         (fboundp 'cua--deactivate))
    (cua--deactivate now))
   ((featurep 'xemacs)
    (let ((zmacs-region-active-p t))
      (zmacs-deactivate-region)))
   (now
    (setq mark-active nil))
   (t
    (setq deactivate-mark t))))

(defun vimpulse-activate-region (&optional pos)
  "Activate mark if there is one. Otherwise set mark at point.
If POS if specified, set mark at POS instead."
  (setq pos (or pos (mark t) (point)))
  (cond
   ((and (boundp 'cua-mode) cua-mode)
    (let ((opoint (point))
          (oldmsg (current-message))
          message-log-max
          cua-toggle-set-mark)
      (goto-char (or pos (mark t) (point)))
      (unwind-protect
          (and (fboundp 'cua-set-mark)
               (cua-set-mark))
        (if oldmsg (message "%s" oldmsg)
          (message nil)))
      (goto-char opoint)))
   (t
    (let (this-command)
      (push-mark pos t t)))))

(defun vimpulse-set-region (beg end &optional widen dir)
  "Set Emacs region to BEG and END.
Preserves the order of point and mark, unless specified by DIR:
a positive number means mark goes before or is equal to point,
a negative number means point goes before mark. If WIDEN is
non-nil, only modifies region if it does not already encompass
BEG and END. Returns nil if region is unchanged."
  (cond
   (widen
    (vimpulse-set-region
     (min beg end (or (region-beginning) (point)))
     (max beg end (or (region-end) (point)))
     nil dir))
   (t
    (unless (region-active-p)
      (vimpulse-activate-region))
    (let* ((oldpoint (point))
           (oldmark  (or (mark t) oldpoint))
           (newmark  (min beg end))
           (newpoint (max beg end)))
      (when (or (and (numberp dir) (< dir 0))
                (and (not (numberp dir))
                     (< oldpoint oldmark)))
        (setq newpoint (prog1 newmark
                         (setq newmark newpoint))))
      (unless (or (and (numberp dir)
                       (= (min newpoint newmark)
                          (min oldpoint oldmark))
                       (= (max newpoint newmark)
                          (max oldpoint oldmark)))
                  (and (= newpoint oldpoint)
                       (= newmark  oldmark)))
        (set-mark newmark)
        (goto-char newpoint))))))

;;; Overlays (extents in XEmacs)

(eval-and-compile
  (cond
   ((featurep 'xemacs)                  ; XEmacs
    (defalias 'vimpulse-delete-overlay 'delete-extent)
    (defalias 'vimpulse-overlays-at 'extents-at))
   (t                                   ; GNU Emacs
    (defalias 'vimpulse-delete-overlay 'delete-overlay)
    (defalias 'vimpulse-overlays-at 'overlays-at))))

;; `viper-make-overlay' doesn't handle FRONT-ADVANCE
;; and REAR-ADVANCE properly in XEmacs
(defun vimpulse-make-overlay
  (beg end &optional buffer front-advance rear-advance)
  "Create a new overlay with range BEG to END in BUFFER.
In XEmacs, create an extent."
  (cond
   ((featurep 'xemacs)
    (let ((extent (make-extent beg end buffer)))
      (set-extent-property extent 'start-open front-advance)
      (set-extent-property extent 'end-closed rear-advance)
      (set-extent-property extent 'detachable nil)
      extent))
   (t
    (make-overlay beg end buffer front-advance rear-advance))))

(defun vimpulse-overlay-before-string (overlay string &optional face)
  "Set the `before-string' property of OVERLAY to STRING.
In XEmacs, change the `begin-glyph' property."
  (cond
   ((featurep 'xemacs)
    (setq face (or face (get-text-property 0 'face string)))
    (when (and string (not (glyphp string)))
      (setq string (make-glyph string)))
    (when face
      (set-glyph-face string face))
    (set-extent-begin-glyph overlay string))
   (t
    (viper-overlay-put overlay 'before-string string))))

(defun vimpulse-overlay-after-string (overlay string &optional face)
  "Set the `after-string' property of OVERLAY to STRING.
In XEmacs, change the `end-glyph' property."
  (cond
   ((featurep 'xemacs)
    (setq face (or face (get-text-property 0 'face string)))
    (when (and string (not (glyphp string)))
      (setq string (make-glyph string)))
    (when face
      (set-glyph-face string face))
    (set-extent-end-glyph overlay string))
   (t
    (viper-overlay-put overlay 'after-string string))))

;;; Undo

(defun vimpulse-refresh-undo-step ()
  "Refresh `buffer-undo-list' entries for current undo step.
Undo boundaries until `vimpulse-undo-list-pointer' are removed
to make the entries undoable as a single action.
See `vimpulse-start-undo-step'."
  (setq buffer-undo-list
        (vimpulse-filter-undo-boundaries buffer-undo-list
                                         vimpulse-undo-list-pointer)))

(defun vimpulse-filter-undo-boundaries (undo-list &optional pointer)
  "Filter undo boundaries from beginning of UNDO-LIST, until POINTER.
A boundary is a nil element, typically inserted by `undo-boundary'.
Return the filtered list."
  (cond
   ((null undo-list)
    nil)
   ((not (listp undo-list))
    undo-list)
   ((eq undo-list pointer)
    undo-list)
   ((null (car undo-list))
    (vimpulse-filter-undo-boundaries (cdr undo-list) pointer))
   (t
    (cons (car undo-list)
          (vimpulse-filter-undo-boundaries (cdr undo-list) pointer)))))

(defun vimpulse-start-undo-step ()
  "Start a single undo step.
End the step with `vimpulse-end-undo-step'.
All intermediate buffer modifications will be undoable as a
single action."
  (when (listp buffer-undo-list)
    (unless (null (car buffer-undo-list))
      (add-to-list 'buffer-undo-list nil))
    (setq vimpulse-undo-list-pointer buffer-undo-list)
    ;; Continually refresh the undo entries for the step,
    ;; ensuring proper synchronization between `buffer-undo-list'
    ;; and `buffer-undo-tree'
    (add-hook 'post-command-hook 'vimpulse-refresh-undo-step nil t)))

(defun vimpulse-end-undo-step ()
  "End a single undo step.
The step must have been started with `vimpulse-start-undo-step'.
All intermediate buffer modifications will be undoable as a
single action."
  (when (memq 'vimpulse-refresh-undo-step post-command-hook)
    (vimpulse-refresh-undo-step)
    (undo-boundary)
    (remove-hook 'post-command-hook 'vimpulse-refresh-undo-step t)))

(defmacro vimpulse-single-undo (&rest body)
  "Execute BODY as a single undo step."
  `(unwind-protect
       (progn
         (vimpulse-start-single-undo)
         ,@body)
     (vimpulse-end-single-undo)))

;;; Motion type system

(defun vimpulse-range-p (object)
  "Return t if OBJECT is a pure range (BEG END)."
  (and (listp object)
       (eq (length object) 2)
       (numberp (car object))
       (numberp (cadr object))))

(defun vimpulse-motion-range-p (object)
  "Return t if OBJECT is a motion range (TYPE BEG END)."
  (and (listp object)
       (symbolp (car object))
       (vimpulse-range-p (cdr object))))

(defun vimpulse-motion-range (object)
  "Return the range part of OBJECT."
  (cond
   ((vimpulse-motion-range-p object)
    (cdr object))
   ((vimpulse-range-p object)
    object)
   (t
    (list (point) (point)))))

(defun vimpulse-range-beginning (range)
  "Return the beginning of RANGE."
  (apply 'min (vimpulse-motion-range range)))

(defun vimpulse-range-end (range)
  "Return the end of RANGE."
  (apply 'max (vimpulse-motion-range range)))

(defun vimpulse-range-height (range &optional normalized)
  "Return height of RANGE. Normalize unless NORMALIZED.

A block range has height and width, a line range
has only height, and a character range has only width."
  (let* ((range (if normalized range
                  (vimpulse-normalize-motion-range range)))
         (beg (vimpulse-range-beginning range))
         (end (vimpulse-range-end range))
         (type (vimpulse-motion-type range)))
    (cond
     ((eq type 'block)
      (count-lines beg
                   (save-excursion
                     (goto-char end)
                     (if (and (bolp) (not (eobp)))
                         (1+ end)
                       end))))
     ((eq type 'line)
      (count-lines beg end))
     (t
      nil))))

(defun vimpulse-range-width (range &optional normalized)
  "Return width of RANGE. Normalize unless NORMALIZED.

A block range has height and width, a line range
has only height, and a character range has only width."
  (let* ((range (if normalized range
                  (vimpulse-normalize-motion-range range)))
         (beg (vimpulse-range-beginning range))
         (end (vimpulse-range-end range))
         (type (vimpulse-motion-type range)))
    (cond
     ((eq type 'block)
      (abs (- (save-excursion
                (goto-char end)
                (current-column))
              (save-excursion
                (goto-char beg)
                (current-column)))))
     ((eq type 'line)
      nil)
     (t
      (abs (- end beg))))))

(defun vimpulse-motion-type (object &optional raw)
  "Return motion type of OBJECT.
The type is one of `exclusive', `inclusive', `line' and `block'.
Defaults to `exclusive' unless RAW is specified."
  (let ((type (cond
               ((symbolp object)
                (get object 'motion-type))
               ((vimpulse-motion-range-p object)
                (car object)))))
    (if raw
        type
      (or type 'exclusive))))

(defun vimpulse-make-motion-range (beg end &optional type normalize)
  "Return motion range (TYPE BEG END).
If NORMALIZE is non-nil, normalize the range with
`vimpulse-normalize-motion-range'."
  (let* ((range (list (min beg end) (max beg end)))
         (type (or type 'exclusive)))
    (if normalize
        (vimpulse-normalize-motion-range range type)
      (cons type range))))

;; This implements section 1 of motion.txt (Vim Reference Manual)
(defun vimpulse-normalize-motion-range (range &optional type)
  "Normalize the beginning and end of a motion range (TYPE FROM TO).
Returns the normalized range.

Usually, a motion range should be normalized only once, as
information is lost in the process: an unnormalized motion range
has the form (TYPE FROM TO), while a normalized motion range has
the form (TYPE BEG END).

See also `vimpulse-block-range', `vimpulse-line-range',
`vimpulse-inclusive-range' and `vimpulse-exclusive-range'."
  (let* ((type (or type (vimpulse-motion-type range)))
         (range (vimpulse-motion-range range))
         (from (car range))
         (to   (cadr range)))
    (cond
     ((memq type '(blockwise block))
      (vimpulse-block-range from to))
     ((memq type '(linewise line))
      (vimpulse-line-range from to))
     ((eq type 'inclusive)
      (vimpulse-inclusive-range from to))
     (t
      (vimpulse-exclusive-range from to t)))))

;; Ranges returned by these functions have the form (TYPE BEG END)
;; where TYPE is one of `inclusive', `exclusive', `line' or `block'
;; and BEG and END are the buffer positions.
(defun vimpulse-block-range (from to)
  "Return a blockwise motion range delimited by FROM and TO.
Like `vimpulse-inclusive-range', but for rectangles:
the last column is included."
  (let* ((beg (min from to))
         (end (max from to))
         (beg-col (save-excursion
                    (goto-char beg)
                    (current-column)))
         (end-col (save-excursion
                    (goto-char end)
                    (current-column))))
    (save-excursion
      (cond
       ((= beg-col end-col)
        (goto-char end)
        (cond
         ((eolp)
          (goto-char beg)
          (if (eolp)
              (vimpulse-make-motion-range beg end 'block)
            (vimpulse-make-motion-range (1+ beg) end 'block)))
         (t
          (vimpulse-make-motion-range beg (1+ end) 'block))))
       ((< beg-col end-col)
        (goto-char end)
        (if (eolp)
            (vimpulse-make-motion-range beg end 'block)
          (vimpulse-make-motion-range beg (1+ end) 'block)))
       (t
        (goto-char beg)
        (if (eolp)
            (vimpulse-make-motion-range beg end 'block)
          (vimpulse-make-motion-range (1+ beg) end 'block)))))))

(defun vimpulse-line-range (from to)
  "Return a linewise motion range delimited by FROM and TO."
  (let* ((beg (min from to))
         (end (max from to)))
    (vimpulse-make-motion-range
     (save-excursion
       (goto-char beg)
       (line-beginning-position))
     (save-excursion
       (goto-char end)
       (line-beginning-position 2))
     'line)))

(defun vimpulse-inclusive-range (from to)
  "Return an inclusive motion range delimited by FROM and TO.
That is, the last character is included."
  (let* ((beg (min from to))
         (end (max from to)))
    (save-excursion
      (goto-char end)
      (unless (eobp)
        (setq end (1+ end)))
      (vimpulse-make-motion-range beg end 'inclusive))))

(defun vimpulse-exclusive-range (from to &optional normalize)
  "Return an exclusive motion range delimited by FROM and TO.
However, if NORMALIZE is t and the end of the range is at the
beginning of a line, a different type of range is returned:

  * If the start of the motion is at or before the first
    non-blank in the line, the motion becomes `line' (normalized).

  * Otherwise, the end of the motion is moved to the end of the
    previous line and the motion becomes `inclusive' (normalized)."
  (let* ((beg (min from to))
         (end (max from to)))
    (save-excursion
      (cond
       ((and normalize
             (/= beg end)
             (progn
               (goto-char end)
               (bolp)))
        (viper-backward-char-carefully)
        (setq end (max beg (point)))
        (cond
         ((save-excursion
            (goto-char beg)
            (looking-back "^[ \f\t\v]*"))
          (vimpulse-make-motion-range beg end 'line t))
         (t
          (vimpulse-make-motion-range beg end 'inclusive))))
       (t
        (vimpulse-make-motion-range beg end 'exclusive))))))

;;;; Keybindings

;;; C-u

(unless vimpulse-want-C-u-like-Vim
  (define-key viper-vi-basic-map "\C-u" 'universal-argument))

;;; vi (command) mode keys

(define-key viper-vi-basic-map "c" 'vimpulse-change)
(define-key viper-vi-basic-map "d" 'vimpulse-delete)
(define-key viper-vi-basic-map "g" nil) ; delete `viper-nil' binding
(define-key viper-vi-basic-map "g?" 'vimpulse-rot13)
(define-key viper-vi-basic-map "gU" 'vimpulse-upcase)
(define-key viper-vi-basic-map "gb" 'vimpulse-end-of-previous-word)
(define-key viper-vi-basic-map "gd" 'vimpulse-goto-definition)
(define-key viper-vi-basic-map "gf" 'find-file-at-point)
(define-key viper-vi-basic-map "gg" 'vimpulse-goto-first-line)
(define-key viper-vi-basic-map "gh" 'backward-char)
(define-key viper-vi-basic-map "gi" 'vimpulse-resume-insert)
(define-key viper-vi-basic-map "gj" 'next-line)
(define-key viper-vi-basic-map "gk" 'previous-line)
(define-key viper-vi-basic-map "gl" 'forward-char)
(define-key viper-vi-basic-map "gq" 'vimpulse-fill)
(define-key viper-vi-basic-map "gu" 'vimpulse-downcase)
(define-key viper-vi-basic-map "gw" 'vimpulse-fill)
(define-key viper-vi-basic-map "g~" 'vimpulse-invert-case)
(define-key viper-vi-basic-map "g0" 'vimpulse-beginning-of-visual-line)
(define-key viper-vi-basic-map "g$" 'vimpulse-end-of-visual-line)
(define-key viper-vi-basic-map "J" 'vimpulse-join)
(define-key viper-vi-basic-map "K" 'woman)
(define-key viper-vi-basic-map "m" 'vimpulse-mark-point)
(define-key viper-vi-basic-map "`" 'vimpulse-goto-mark)
(define-key viper-vi-basic-map "'" 'vimpulse-goto-mark-and-skip-white)
(define-key viper-vi-basic-map "r" 'vimpulse-replace)
(define-key viper-vi-basic-map "y" 'vimpulse-yank)
(define-key viper-vi-basic-map "zb" 'viper-line-to-bottom)
(define-key viper-vi-basic-map "zh" 'scroll-right)
(define-key viper-vi-basic-map "zl" 'scroll-left)
(define-key viper-vi-basic-map "zt" 'viper-line-to-top)
(define-key viper-vi-basic-map "zz" 'viper-line-to-middle)
(define-key viper-vi-basic-map "\C-]" 'vimpulse-jump-to-tag-at-point)
(define-key viper-vi-basic-map "\C-t" 'pop-tag-mark)
(define-key viper-vi-basic-map "]" nil) ; delete `viper-ket-function' binding
(define-key viper-vi-basic-map "]P" 'vimpulse-Put-and-indent)
(define-key viper-vi-basic-map "]p" 'vimpulse-put-and-indent)
(define-key viper-vi-basic-map "=" 'vimpulse-indent)
(define-key viper-vi-basic-map "+" 'vimpulse-previous-line-skip-white)
(define-key viper-vi-basic-map "_" 'vimpulse-next-line-skip-white)
(define-key viper-vi-basic-map "#" 'vimpulse-search-backward-for-symbol-at-point)
(define-key viper-vi-basic-map "*" 'vimpulse-search-forward-for-symbol-at-point)
(define-key viper-vi-basic-map "<" 'vimpulse-shift-left)
(define-key viper-vi-basic-map ">" 'vimpulse-shift-right)
(define-key viper-vi-basic-map "~" 'vimpulse-invert-char)
(define-key viper-vi-basic-map "\"" 'vimpulse-read-register)
(define-key viper-vi-basic-map "/" 'vimpulse-search-forward)
(define-key viper-vi-basic-map "?" 'vimpulse-search-backward)
(define-key viper-vi-kbd-map "/" nil)

;; Visual bindings
(define-key viper-vi-basic-map "v" 'vimpulse-visual-toggle-char)
(define-key viper-vi-basic-map "V" 'vimpulse-visual-toggle-line)
(define-key viper-vi-basic-map "\C-v" 'vimpulse-visual-toggle-block)
(define-key viper-vi-basic-map "gv" 'vimpulse-visual-restore)

;; map undo and redo
(define-key viper-vi-basic-map "u" 'undo)
(cond
 ((fboundp 'undo-tree-redo)
  (define-key viper-vi-basic-map "\C-r" 'undo-tree-redo))
 ((fboundp 'redo)
  (define-key viper-vi-basic-map "\C-r" 'redo)))

;; window manipulation
(defvar vimpulse-window-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-w" 'other-window)
    (define-key map "w" 'other-window)
    (define-key map "o" 'delete-other-windows)
    (define-key map "c" 'delete-window)
    (define-key map "s" 'split-window-vertically)
    (define-key map "v" 'split-window-horizontally)
    (when (fboundp 'windmove-left)
      (define-key map "h" 'windmove-left)
      (define-key map "j" 'windmove-down)
      (define-key map "k" 'windmove-up)
      (define-key map "l" 'windmove-right))
    map)
  "Keymap for window-related commands.
Equivalent to Vim's C-w prefix.")

(define-key viper-vi-basic-map "\C-w" vimpulse-window-map)

;;; Insert mode keys

;; Vim-like completion keys
(define-key viper-insert-basic-map "\C-p" 'vimpulse-abbrev-expand-before)
(define-key viper-insert-basic-map "\C-n" 'vimpulse-abbrev-expand-after)
(define-key viper-insert-basic-map "\C-x\C-p" 'vimpulse-expand-line)
(define-key viper-insert-basic-map "\C-x\C-n" 'vimpulse-expand-line)
(define-key viper-insert-basic-map [delete] 'delete-char) ; <delete> key
;; make ^[ work
(define-key viper-insert-basic-map (kbd "ESC") 'viper-exit-insert-state)

;;; Registers

(defun vimpulse-store-in-register (register start end)
  "Store text from START to END in REGISTER."
  (cond
   ((viper-valid-register register '(Letter))
    (viper-append-to-register
     (downcase register) start end))
   (t
    (copy-to-register register start end))))

(defun vimpulse-store-in-current-register (start end)
  "Store text from START to END in current register, if any.
  Resets `viper-use-register'."
  (when viper-use-register
    (vimpulse-store-in-register viper-use-register start end)
    (setq viper-use-register nil)))

(defun vimpulse-read-register (&optional register command)
  "Use COMMAND with REGISTER.
  If called interactively, read REGISTER and COMMAND from keyboard."
  (interactive)
  (setq register (or register (read-char)))
  (when (viper-valid-register register)
    (setq command (or command (key-binding (read-key-sequence nil))))
    (when (commandp command)
      (let ((this-command command)
            (viper-use-register register))
        (call-interactively command)))))

;;; g0, g$

(defun vimpulse-beginning-of-visual-line (arg)
  "Go to beginning of `visual-line-mode' line."
  (interactive "p")
  (if (and (boundp 'visual-line-mode) visual-line-mode)
      (beginning-of-visual-line arg)
    ;; `move-beginning-of-line' instead of `beginning-of-line'
    ;; handles longlines-mode properly
    (move-beginning-of-line arg)))

(defun vimpulse-end-of-visual-line (arg)
  "Go to end of `visual-line-mode' line."
  (interactive "p")
  (if (and (boundp 'visual-line-mode) visual-line-mode)
      (end-of-visual-line arg)
    ;; `move-end-of-line' instead of `end-of-line'
    ;; handles longlines-mode properly
    (move-end-of-line arg))
  (unless (bolp)
    (backward-char)))

;;; gg

(defun vimpulse-goto-first-line (arg)
  "Go to ARGth line (first line by default)."
  (interactive "P")
  (let ((val (viper-P-val arg))
        (com (viper-getCom arg)))
    (when (eq com ?c) (setq com ?C))
    (viper-move-marker-locally 'viper-com-point (point))
    (viper-deactivate-mark)
    (push-mark nil t)
    (cond
     ((null val)
      (goto-char (point-min)))
     (t
      (viper-goto-line val)))
    (when com
      (viper-execute-com 'vimpulse-goto-line val com))))

;;; gb

(defun vimpulse-beginning-of-Word-p ()
  (save-excursion
    (or (bobp)
        (when (viper-looking-at-alpha)
          (backward-char)
          (not (viper-looking-at-alpha))))))

(defun vimpulse-end-of-previous-word (arg)
  "Move point to end of previous word."
  (interactive "P")
  (viper-leave-region-active)
  (let ((val (viper-p-val arg))
        (com (viper-getcom arg)))
    (when com
      (viper-move-marker-locally 'viper-com-point (point)))
    (unless (vimpulse-beginning-of-Word-p)
      (viper-backward-Word 1))
    (viper-backward-Word val)
    (viper-end-of-Word '(1 . ?r))
    (unless com
      (backward-char))
    (when com
      (viper-execute-com 'viper-end-of-word val com))))

;;; gd

(defun vimpulse-goto-definition ()
  "Go to definition or first occurrence of symbol under cursor."
  (interactive)
  (let ((str (vimpulse-search-string (point) 'symbol))
        ientry ipos)
    (cond
     ((string= str "")
      (error "No string under cursor"))
     ;; if imenu is available, try it
     ((or (fboundp 'imenu--make-index-alist)
          (load "imenu" t))
      (setq ientry
            (condition-case nil
                (and (fboundp 'imenu--make-index-alist)
                     (imenu--make-index-alist))
              (error nil)))
      (setq ientry (assoc str ientry))
      (setq ipos (cdr ientry))
      (unless (markerp ipos)
        (setq ipos (cadr ientry)))
      (cond
       ;; imenu found a position, so go there and
       ;; highlight the occurrence
       ((and (markerp ipos)
             (eq (marker-buffer ipos) (current-buffer)))
        (vimpulse-search-for-symbol nil ipos str))
       ;; imenu failed, so just go to first occurrence in buffer
       (t
        (vimpulse-search-for-symbol nil (point-min)))))
     ;; no imenu, so just go to first occurrence in buffer
     (t
      (vimpulse-search-for-symbol nil (point-min))))))

(defun vimpulse-jump-to-tag-at-point ()
  (interactive)
  (let ((tag (thing-at-point 'word)))
    (find-tag tag)))

;;; gi

(defun vimpulse-resume-insert (arg)
  "Insert at previous insert position."
  (interactive "P")
  (when (markerp vimpulse-exit-point)
    (goto-char vimpulse-exit-point))
  (viper-insert arg))

;;; +, _

(defun vimpulse-previous-line-skip-white (&optional arg)
  "Go ARG lines backward and to the first non-blank character."
  (interactive "P")
  (let ((val (viper-p-val arg))
        (com (viper-getcom arg)))
    (when com
      (viper-move-marker-locally 'viper-com-point (point)))
    (forward-line (- val))
    (back-to-indentation)
    (when com
      (viper-execute-com 'vimpulse-previous-line-nonblank val com))))

(defun vimpulse-next-line-skip-white (&optional arg)
  "Go ARG lines forward and to the first non-blank character."
  (interactive "P")
  (let ((val (viper-p-val arg))
        (com (viper-getcom arg)))
    (when com
      (viper-move-marker-locally 'viper-com-point (point)))
    (forward-line val)
    (back-to-indentation)
    (when com
      (viper-execute-com 'vimpulse-next-line-nonblank val com))))

;;; *, #

(defun vimpulse-search-string (&optional pos thing backward regexp)
  "Find something to search for near POS or point.
THING is a `thing-at-point', default `symbol'.
BACKWARD, if t, specifies reverse direction.
REGEXP, if t, means the string is `regexp-quote'd.
Returns the empty string if nothing is found."
  (save-excursion
    (setq pos (or pos (point))
          thing (or thing 'symbol))
    (goto-char pos)
    (let ((str (thing-at-point thing)))
      ;; if there's nothing under point, go forwards
      ;; (or backwards) to find it
      (while (and (not str) (or (and backward (not (bobp)))
                                (and (not backward) (not (eobp)))))
        (if backward (backward-char) (forward-char))
        (setq str (thing-at-point 'symbol)))
      (setq str (or str ""))
      ;; no text properties, thank you very much
      (set-text-properties 0 (length str) nil str)
      (when regexp
        (setq str (regexp-quote str)))
      str)))

(defun vimpulse-search-for-symbol (&optional backward pos search)
  "Search forwards or backwards for the symbol under point.
If BACKWARD is t, search in the reverse direction.
SEARCH is a regular expression to use for searching instead of
the symbol under point; it is wrapped in \"\\\\_<\" and \"\\\\_>\".
POS specifies an alternative position to search from. Note that
if POS is specified and at the beginning of a match, that match
is highlighted rather than skipped past."
  (setq search (or search (vimpulse-search-string
                           (point) 'symbol backward t)))
  (cond
   ((string= search "")
    (error "No string under cursor"))
   (t
    (setq viper-s-string  (concat "\\_<" search "\\_>")
          viper-s-forward (not backward))
    (cond
     (pos
      (unless (region-active-p)
        (push-mark nil t))
      (goto-char pos)
      (cond
       ((looking-at search)
        (save-excursion
          (search-forward search))
        (viper-flash-search-pattern))
       (t
        (viper-search viper-s-string (not backward) 1)
        (unless (region-active-p)
          (pop-mark)))))
     (t
      (viper-search viper-s-string (not backward) 1))))))

(defun vimpulse-search-forward-for-symbol-at-point ()
  (interactive)
  (cond
   ((and (stringp viper-s-string)
         (not (string= viper-s-string ""))
         (memq last-command
               '(vimpulse-search-backward-for-symbol-at-point
                 vimpulse-search-forward-for-symbol-at-point)))
    (setq viper-s-forward t)
    (viper-search-next 1))
   (t
    (vimpulse-search-for-symbol))))

(defun vimpulse-search-backward-for-symbol-at-point ()
  (interactive)
  (cond
   ((and (stringp viper-s-string)
         (not (string= viper-s-string ""))
         (memq last-command
               '(vimpulse-search-backward-for-symbol-at-point
                 vimpulse-search-forward-for-symbol-at-point)))
    (setq viper-s-forward nil)
    (viper-search-next 1))
   (t
    (vimpulse-search-for-symbol t))))

;;; Auto-indent

(defun vimpulse-autoindent ()
  "Auto Indentation, Vim-style."
  (interactive)
  (let ((col (current-indentation)))
    (when abbrev-mode
      (expand-abbrev))
    (if viper-preserve-indent
        (setq viper-preserve-indent nil)
      (setq viper-current-indent col))
    ;; don't leave whitespace lines around
    (if (memq last-command
              '(viper-autoindent
                viper-open-line viper-Open-line
                viper-replace-state-exit-cmd))
        (indent-to-left-margin))
    (when viper-auto-indent
      (setq viper-cted t)
      (if (and viper-electric-mode
               (not (memq major-mode
                          '(fundamental-mode
                            text-mode
                            paragraph-indent-text-mode))))
          (if (fboundp 'comment-indent-new-line)
              (comment-indent-new-line)
            (newline-and-indent))
        (newline)
        (indent-to col)))))

(defun vimpulse-Put-and-indent (&optional arg)
  "Put before point/line and indent to current line.
Doesn't indent with a prefix argument."
  (interactive "P")
  (viper-Put-back nil)
  (unless arg
    (indent-region (region-beginning) (region-end))))

(defun vimpulse-put-and-indent (&optional arg)
  "Put after point/line and indent to current line.
Doesn't indent with a prefix argument."
  (interactive "P")
  (viper-put-back nil)
  (unless arg
    (indent-region (region-beginning) (region-end))))

(defalias 'viper-autoindent 'vimpulse-autoindent)

;;; C-o, C-i

(defadvice set-mark (after vimpulse activate)
  "Clear `vimpulse-mark-list'."
  (mapc (lambda (marker)
          (set-marker marker nil))
        vimpulse-mark-list)
  (setq vimpulse-mark-list nil))

(defadvice push-mark (after vimpulse activate)
  "Clear `vimpulse-mark-list'."
  (mapc (lambda (marker)
          (set-marker marker nil))
        vimpulse-mark-list)
  (setq vimpulse-mark-list nil))

(defun vimpulse-jump-backward (arg)
  "Go to older position in jump list.
To go the other way, press \\[vimpulse-jump-forward]."
  (interactive "p")
  (let ((current-pos (make-marker)) i)
    (unless vimpulse-mark-list
      (move-marker current-pos (point))
      (add-to-list 'vimpulse-mark-list current-pos))
    (dotimes (arg arg)
      (setq current-pos (make-marker))
      ;; skip past duplicate entries in the mark ring
      (setq i (length mark-ring))
      (while (progn (move-marker current-pos (point))
                    (let (vimpulse-mark-list)
                      ;; protect `vimpulse-mark-list'
                      (set-mark-command 0))
                    (setq i (1- i))
                    (and (= (point) current-pos) (> i 0))))
      ;; Already there?
      (move-marker current-pos (point))
      (unless (= (car vimpulse-mark-list) current-pos)
        (setq vimpulse-mark-list
              (cons current-pos vimpulse-mark-list))))))

(defun vimpulse-remove-minibuffer-marks ()
  "Remove minibuffer marks from `global-mark-ring'."
  (let (result)
    (dolist (mark global-mark-ring)
      (unless (and (marker-buffer mark)
                   (minibufferp (marker-buffer mark)))
        (add-to-list 'result mark t)))
    (setq global-mark-ring result)))

(defun vimpulse-jump-forward (arg)
  "Go to newer position in jump list.
To go the other way, press \\[vimpulse-jump-backward]."
  (interactive "p")
  (let (current-pos next-pos)
    (dotimes (arg arg)
      (setq current-pos (car vimpulse-mark-list)
            next-pos (cadr vimpulse-mark-list))
      (when next-pos
        ;; protect `vimpulse-mark-list'
        (let (vimpulse-mark-list)
          (push-mark current-pos t nil))
        (unless (eq (marker-buffer next-pos) (current-buffer))
          (switch-to-buffer (marker-buffer next-pos)))
        (goto-char next-pos)
        (setq vimpulse-mark-list (cdr vimpulse-mark-list))))))

(when vimpulse-want-C-i-like-Vim
  (define-key viper-vi-basic-map "\C-i" 'vimpulse-jump-forward))
(define-key viper-vi-basic-map "\C-o" 'vimpulse-jump-backward)

;;; Replace backspace

(defcustom vimpulse-backspace-restore t
  "Whether Backspace restores the original text in Replace mode.
On by default."
  :group 'vimpulse
  :type  'boolean)

(defun vimpulse-replace-pre-command ()
  "Remember the character under point."
  (cond
   (viper-replace-minor-mode
    (unless (assq (point) vimpulse-replace-alist)
      (add-to-list 'vimpulse-replace-alist
                   (cons (point) (char-after)))))
   ;; if not in Replace mode, remove itself
   (t
    (remove-hook 'pre-command-hook 'vimpulse-replace-pre-command))))

(add-hook 'viper-replace-state-hook
          (lambda ()
            (setq vimpulse-replace-alist nil)
            (vimpulse-replace-pre-command)
            (add-hook 'pre-command-hook
                      'vimpulse-replace-pre-command)))

(defun vimpulse-replace-backspace ()
  "Restore character under cursor.
If `vimpulse-backspace-restore' is nil,
call `viper-del-backward-char-in-replace' instead."
  (interactive)
  (cond
   (vimpulse-backspace-restore
    (backward-char)
    (let ((oldchar (cdr (assq (point) vimpulse-replace-alist))))
      (when oldchar
        (save-excursion
          (delete-char 1)
          (insert oldchar)))))
   (t
    (viper-del-backward-char-in-replace))))

(defadvice viper-adjust-keys-for (after vimpulse activate)
  "Map <backspace> to `vimpulse-replace-backspace' in Replace mode."
  (define-key viper-replace-map [backspace] 'vimpulse-replace-backspace))

(defun vimpulse-abbrev-expand-before ()
  "Expand to the nearest preceding word.
Search forwards if a match isn't found."
  (interactive)
  (if (minibufferp)
      (minibuffer-complete)
    (dabbrev-expand nil)))

;; Getting dabbrev to search forwards first and then backwards
;; is tricky, because (dabbrev-expand -1) just fails when it
;; doesn't find a following match.
(defun vimpulse-abbrev-expand-after ()
  "Expand to the nearest following word.
Search backwards if a match isn't found."
  (interactive)
  ;; back up global variables
  (let ((abbrev (and (boundp 'dabbrev--last-abbreviation)
                     dabbrev--last-abbreviation))
        (abbrev-loc (and (boundp 'dabbrev--last-abbrev-location)
                         dabbrev--last-abbrev-location))
        (expansion (and (boundp 'dabbrev--last-expansion)
                        dabbrev--last-expansion))
        (expansion-loc (and (boundp 'dabbrev--last-expansion-location)
                            dabbrev--last-expansion-location)))
    ;; expand in same direction as previously, initially forward
    (if (minibufferp)
        (minibuffer-complete)
      (condition-case nil
          (if (eq last-command this-command)
              (dabbrev-expand nil)
            (setq dabbrev--last-direction -1)
            (dabbrev-expand -1))
        ;; restore dabbrev variables if version < 23.2
        (error (progn
                 (when (version< emacs-version "23.2")
                   (setq dabbrev--last-abbreviation abbrev
                         dabbrev--last-abbrev-location abbrev-loc
                         dabbrev--last-expansion expansion
                         dabbrev--last-expansion-location expansion-loc))
                 (setq dabbrev--last-direction 1)
                 (dabbrev-expand nil) nil))))))

(defun vimpulse-expand-line (&optional arg)
  "Expand a whole line."
  (interactive "P")
  (let ((hippie-expand-try-functions-list
         '(try-expand-line
           try-expand-line-all-buffers)))
    (hippie-expand arg)))

;;;; Modal keybinding functions

;; Functions for making state bindings (modal bindings).
;;
;; Global state bindings (seen in all buffers):
;;     `vimpulse-global-set-key', `vimpulse-map', `vimpulse-imap',
;;     `vimpulse-vmap', `vimpulse-omap'
;; Buffer-local state bindings (seen only in the current buffer):
;;     `vimpulse-local-set-key', `vimpulse-map-local',
;;     `vimpulse-imap-local', `vimpulse-vmap-local',
;;     `vimpulse-omap-local'
;; State bindings for Emacs modes (minor and major):
;;     `vimpulse-define-key'
;;
;; `vimpulse-map' etc. mimic Vim's :map, :imap, :vmap and :omap
;; commands; `vimpulse-map-local' etc. are similar, but buffer-local.
;;
;; `vimpulse-define-key' associates state bindings with an Emacs mode,
;; and is useful for grouping bindings:
;;
;;     (define-minor-mode foo-mode)
;;     (vimpulse-define-key 'foo-mode 'vi-state "a" 'bar)
;;     (vimpulse-define-key 'foo-mode 'visual-state "b" 'baz)
;;
;; These bindings are only seen in buffers where foo-mode is enabled.
;; You can also augment existing modes, of course.
;;
;; Vimpulse provides "careful" bindings, which are bindings that can
;; be safely stacked on top of pre-existing bindings. They can be made
;; with `vimpulse-make-careful-binding' (see its docstring for more
;; details), or by passing a non-nil value for the CAREFUL argument of
;; `vimpulse-global-set-key', `vimpulse-local-set-key' or
;; `vimpulse-define-key'. `vimpulse-map' etc. are always careful.

;;; Advice

;; For XEmacs, construct a wrap-around advice of the current command
;; shadowing the read-only command loop variables with a
;; `let' binding.
(defmacro vimpulse-advice-command (command)
  "Define an around advice for COMMAND to shadow `last-command-event'.
XEmacs does not allow us to change its command loop variables
directly, but shadowing them with a `let' binding works."
  `(defadvice ,command (around vimpulse-careful activate)
     "Shadow `last-command-event' with a `let' binding."
     (cond
      (vimpulse-last-command-event
       (let* ((last-command-event
               (character-to-event vimpulse-last-command-event))
              (last-command-char vimpulse-last-command-event)
              (last-input-event last-command-event)
              (last-input-char last-command-char))
         ad-do-it))
      (t
       ad-do-it))))

;;; General functions

(defun vimpulse-careful-check (key-sequence)
  "Return t if KEY-SEQUENCE defaults to `this-command',
but only for bindings listed in `vimpulse-careful-alist'."
  (let ((temp-sequence (vimpulse-strip-prefix key-sequence)))
    (setq temp-sequence (vimpulse-truncate temp-sequence -1))
    (and this-command                     ; may be nil
         (not (key-binding key-sequence)) ; only default bindings
         (eq (cdr (assoc temp-sequence vimpulse-careful-alist))
             this-command))))

(defun vimpulse-careful-remove (key-vector &optional recursive)
  "Delete entry with KEY-VECTOR from `vimpulse-careful-alist'.
If RECURSIVE is non-nil, also delete entries whose key-vectors
start with KEY-VECTOR."
  (if recursive
      (dolist (entry vimpulse-careful-alist)
        (when (equal (vimpulse-truncate (car entry)
                                        (length key-vector))
                     key-vector)
          (setq vimpulse-careful-alist
                (delq entry vimpulse-careful-alist))))
    (setq vimpulse-careful-alist
          (assq-delete-all key-vector vimpulse-careful-alist))))

(defun vimpulse-xemacs-def-binding
  (keymap key def &optional careful-binding define-func)
  "Make a default binding in XEmacs. If CAREFUL-BINDING is
non-nil, advice DEF by means of `vimpulse-advice-command'."
  (let ((temp-sequence (vconcat key))
        (submap (lookup-key keymap key)))
    (unless define-func (setq define-func 'define-key))
    (and careful-binding (commandp def)
         (eval `(vimpulse-advice-command ,def)))
    (and (> (length temp-sequence) 1)
         (eq (aref temp-sequence (1- (length temp-sequence))) t)
         (setq temp-sequence (vimpulse-truncate temp-sequence -1)))
    ;; from http://tracker.xemacs.org/XEmacs/its/msg2021
    (unless (keymapp submap)
      (setq submap (make-sparse-keymap)))
    (when (fboundp 'set-keymap-default-binding)
      (set-keymap-default-binding submap def))
    (funcall define-func keymap temp-sequence submap)))

(defun vimpulse-default-binding
  (keymap key def &optional careful-binding define-func)
  "Make a default binding in GNU Emacs or XEmacs,
whichever is appropriate. If CAREFUL-BINDING is non-nil,
the binding is listed in `vimpulse-careful-alist'."
  (let ((temp-sequence (vconcat key)))
    (unless define-func (setq define-func 'define-key))
    (cond
     ((featurep 'xemacs)
      (vimpulse-xemacs-def-binding
       keymap temp-sequence def careful-binding define-func))
     (t
      (unless (eq (aref temp-sequence (1- (length temp-sequence))) t)
        (setq temp-sequence (vconcat temp-sequence [t])))
      (funcall define-func keymap temp-sequence def)))
    (when careful-binding
      (add-to-list 'vimpulse-careful-alist
                   (cons (vimpulse-truncate temp-sequence -1) def)))
    def))

;;; Hook run before each command

;; If the current command is a default key binding made by
;; `vimpulse-make-careful-binding', we need to unread the last input
;; events and change some command loop variables to give the command
;; the impression of its "old" binding.
(defun vimpulse-careful-pre-hook ()
  "Update `vimpulse-last-command-event' and `unread-command-events'.
If the current key-sequence defaults to a shorter key-sequence,
the difference is stored in these two variables, to be passed on
via the `last-command-event' variable and the `read-char'
functions, respectively."
  (setq vimpulse-last-command-event nil)
  (let ((key-sequence (vconcat (this-command-keys))))
    ;; if XEmacs, get rid of the event object type
    (when (featurep 'xemacs)
      (setq key-sequence (events-to-keys key-sequence)))
    (while (and (> (length key-sequence) 1)
                (vimpulse-careful-check key-sequence))
      ;; unread last event
      (setq vimpulse-last-command-event
            (elt key-sequence (1- (length key-sequence))))
      (when (featurep 'xemacs)
        (setq vimpulse-last-command-event
              (character-to-event vimpulse-last-command-event)))
      (add-to-list 'unread-command-events vimpulse-last-command-event)
      ;; change command loop variables
      (setq vimpulse-last-command-event
            (elt key-sequence (1- (1- (length key-sequence)))))
      (unless (featurep 'xemacs)      ; if XEmacs, do this with advice
        (with-no-warnings
          (setq last-command-event vimpulse-last-command-event
                last-command-char  vimpulse-last-command-event
                last-input-event   vimpulse-last-command-event
                last-input-char    vimpulse-last-command-event)))
      (setq key-sequence
            (vimpulse-truncate key-sequence -1)))))

;;; Hook run after each command

;; this merely ensures `vimpulse-last-command-event' is reset
(defun vimpulse-careful-post-hook ()
  "Erase `vimpulse-last-command-event'."
  (setq vimpulse-last-command-event nil))

(add-hook 'pre-command-hook  'vimpulse-careful-pre-hook)
(add-hook 'post-command-hook 'vimpulse-careful-post-hook)

;;; Modal binding functions

(defun vimpulse-make-careful-binding
  (keymap key def &optional dont-list define-func)
  "Carefully bind KEY to DEF in KEYMAP.
\"Carefully\" means that if a subset of the key sequence is already
bound, a default binding is made so that the new binding won't
overwrite the old. E.g., if we want to carefully bind \"A B C\" to
`foo', and \"A B\" is already bound to `bar', the end result is

    \"A B C\"   => `foo'
    \"A B <t>\" => `bar'

Thus, \"A B C\" runs `foo', while \"A B\" + any key other than \"C\"
defaults to `bar'. To remove a careful binding, bind it to nil.

The default binding gets listed in `vimpulse-careful-alist', so
that, with regard to command loop variables, it appears exactly
the same as the binding it replaced. In the example above, `bar'
will see a `last-command-event' of \"B\", and will automatically
obtain the last key by calling `read-char'. To make a regular
default binding, use DONT-LIST. DEFINE-FUNC specifies a function
to be used in place of `define-key'.

NOTE: If the original binding \"A B\" is not stored in KEYMAP,
but in some other map which is active only in a certain
state (say, Insert mode), this function can detect that binding
only if called in the same state. The functions `vimpulse-map',
`vimpulse-imap' and `vimpulse-vmap' take care of this."
  (let (key-vector temp-sequence current-binding previous-binding)
    ;; For each subset of KEY-VECTOR (stored in `temp-sequence'), check
    ;; the binding (stored in `current-binding'); if it isn't bound,
    ;; use `previous-binding'.
    (setq define-func (or define-func 'define-key))
    (setq key-vector (if (stringp key)
                         (read-kbd-macro key t)
                       key))
    (cond
     ;; nil unbinds the key-sequence
     ((not def)
      (funcall define-func keymap key-vector def)
      (while (and (> (length key-vector) 1)
                  (not (lookup-key keymap key-vector)))
        (vimpulse-careful-remove key-vector t)
        (setq key-vector (vimpulse-truncate key-vector -1))))
     ;; `undefined' also unbinds, but less forcefully
     ((eq def 'undefined)
      (if (keymapp (lookup-key keymap key-vector))
          (vimpulse-default-binding keymap key-vector nil t define-func)
        (funcall define-func keymap key-vector def))
      (vimpulse-careful-remove key-vector))
     ;; regular binding: convert previous bindings to default bindings
     (t
      (dotimes (i (1- (length key-vector)))
        (setq temp-sequence (vimpulse-truncate key-vector (1+ i)))
        (setq current-binding (lookup-key keymap temp-sequence t))
        (when (or (numberp current-binding) (not current-binding))
          (setq current-binding
                (or (key-binding temp-sequence t) previous-binding)))
        (setq previous-binding current-binding)
        ;; If `current-binding' is a keymap, do nothing, since our
        ;; careful binding can happily exist as part of that keymap.
        ;; However, if `current-binding' is a command, we need to make
        ;; room for the careful binding by creating a default binding.
        (unless (keymapp current-binding)
          (setq temp-sequence (vconcat temp-sequence [t]))
          (setq current-binding (lookup-key keymap temp-sequence t))
          (when (or (numberp current-binding) (not current-binding))
            (setq current-binding
                  (or (key-binding temp-sequence t) previous-binding))
            (define-key keymap
              (vimpulse-truncate temp-sequence -1) nil)
            (vimpulse-default-binding
             keymap temp-sequence current-binding
             (not dont-list) define-func))
          (setq previous-binding current-binding)))
      ;; Defaults are taken care of; we may now bind the key.
      ;; If a longer binding starting with KEY-VECTOR exists,
      ;; make a default binding so it's not overwritten.
      (if (keymapp (lookup-key keymap key-vector))
          (vimpulse-default-binding
           keymap key-vector def (not dont-list) define-func)
        (funcall define-func keymap key def))))))

(define-minor-mode vimpulse-careful-minor-mode
  "Minor mode of bindings overwritten by `vimpulse-map' et al."
  :keymap vimpulse-careful-map
  (dolist (entry vimpulse-careful-alist)
    (unless (lookup-key vimpulse-careful-map (car entry))
      (define-key vimpulse-careful-map (car entry) (cdr entry))))
  (when vimpulse-careful-minor-mode
    (viper-normalize-minor-mode-map-alist)))

(add-to-list 'vimpulse-state-maps-alist
             (cons 'vimpulse-careful-minor-mode 'vimpulse-careful-map))

(defun vimpulse-define-key (mode state key def &optional careful)
  "Modally bind KEY to DEF in STATE for MODE.
MODE is an Emacs mode (minor or major), while STATE is one of
`vi-state', `insert-state', `visual-state' or `operator-state'.
For example:

    (vimpulse-define-key 'text-mode 'vi-state \"a\" 'foo)
    (vimpulse-define-key 'visual-line-mode 'visual-state \"b\" 'bar)

If CAREFUL is non-nil, make a careful binding with
`vimpulse-make-careful-binding'."
  (let* ((entry (cdr (assq state vimpulse-auxiliary-modes-alist)))
         (aux   (cdr (assq mode (symbol-value entry))))
         (map   (eval (cdr (assq aux vimpulse-state-maps-alist)))))
    ;; if no auxiliary mode exists, create one
    (unless (keymapp map)
      (setq aux (intern (format "vimpulse-%s-%s" state mode))
            map (intern (format "vimpulse-%s-%s-map" state mode)))
      (eval `(viper-deflocalvar ,aux nil
               ,(format "Auxiliary %s mode for `%s'." state mode)))
      (eval `(defvar ,map (make-sparse-keymap)
               ,(format "Auxiliary %s keymap for `%s'." state mode)))
      (when (fboundp mode)
        (eval `(defadvice ,mode (after vimpulse-modal activate)
                 (when viper-mode
                   (viper-normalize-minor-mode-map-alist)
                   (viper-set-mode-vars-for viper-current-state)))))
      (add-to-list 'vimpulse-state-maps-alist (cons aux map) t)
      (add-to-list entry (cons mode aux) t)
      (add-to-list 'vimpulse-auxiliary-modes mode)
      (vimpulse-normalize-auxiliary-modes)
      (setq map (eval map)))
    ;; define key
    (if careful
        (vimpulse-with-state state
          (vimpulse-make-careful-binding map key def))
      (define-key map key def))))

;; This modifies the major mode extension keymap, i.e., it's
;; a reusable front-end to `viper-modify-major-mode'.
;; (By itself, `viper-modify-major-mode' discards the previous keymap.)
;; Don't use this; use `vimpulse-define-key' instead.
(defun vimpulse-define-major-key (mode state key def &optional careful)
  "Modally bind KEY to DEF in STATE for major mode MODE.
STATE is one of `vi-state', `insert-state', `visual-state' or
`operator-state'. If CAREFUL is non-nil, make a careful binding
with `vimpulse-make-careful-binding'."
  (let ((modifier-map (vimpulse-modifier-map state mode)))
    (if careful
        (vimpulse-with-state state
          (vimpulse-make-careful-binding modifier-map key def))
      (define-key modifier-map key def))
    (viper-modify-major-mode mode state modifier-map)
    def))

(defalias 'vimpulse-define-minor-key 'vimpulse-define-key)

(defun vimpulse-global-set-key (state key def &optional careful)
  "Modally bind KEY to DEF in STATE.
STATE is one of `vi-state', `insert-state', `visual-state' or `operator-state'.
If CAREFUL is non-nil, don't overwrite previous bindings."
  (let* ((map (cdr (assq state vimpulse-state-vars-alist)))
         (global-user-map (eval (cdr (assq 'global-user-map map)))))
    (if careful
        (vimpulse-with-state state
          (vimpulse-make-careful-binding global-user-map key def))
      (define-key global-user-map key def))))

(defun vimpulse-local-set-key (state key def)
  "Modally bind KEY to DEF in STATE, locally.
STATE is one of `vi-state', `insert-state', `visual-state' or `operator-state'."
  (viper-add-local-keys state `((,key . ,def)))
  def)

(defun vimpulse-map-state (state key def &optional modes)
  "Modally bind KEY to DEF in STATE.
Don't use this function directly; see `vimpulse-map',
`vimpulse-imap', `vimpulse-vmap' and `vimpulse-omap' instead."
  (let* ((map (cdr (assq state vimpulse-state-vars-alist)))
         (basic-map (eval (cdr (assq 'basic-map map)))))
    (if modes
        (dolist (mode modes)
          (if (eq mode t)
              (vimpulse-global-set-key 'vi-state key def t)
            (vimpulse-define-major-key mode 'vi-state key def t)))
      (vimpulse-with-state state
        (vimpulse-make-careful-binding basic-map key def)))))

(defalias 'vimpulse-map-state-local 'vimpulse-local-set-key)

(defun vimpulse-map (key def &rest modes)
  "Modally bind KEY to DEF in vi (command) state.
The syntax is the same as that of `global-set-key', e.g.,

    (vimpulse-map \"abc\" 'abc-command)

The optional MODES argument specifies which major modes the
binding is seen in:

    (vimpulse-map \"abc\" 'abc-command 'lisp-mode 'text-mode)

Otherwise, the binding is universal, but has lower priority.
Pass t to MODES to create an universal binding with presedence
over mode-specific bindings."
  (vimpulse-map-state 'vi-state key def modes))

(defun vimpulse-imap (key def &rest modes)
  "Modally bind KEY to DEF in Insert state.
The syntax is the same as that of `global-set-key', e.g.,

    (vimpulse-imap \"abc\" 'abc-command)

The optional MODES argument specifies which major modes the
binding is seen in:

    (vimpulse-imap \"abc\" 'abc-command 'lisp-mode 'text-mode)

Otherwise, the binding is universal, but has lower priority.
Pass t to MODES to create an universal binding with presedence
over mode-specific bindings."
  (vimpulse-map-state 'insert-state key def modes))

(defun vimpulse-vmap (key def &rest modes)
  "Modally bind KEY to DEF in the Visual state.
The syntax is the same as that of `global-set-key', e.g.,

    (vimpulse-vmap \"abc\" 'abc-command)

The optional MODES argument specifies which major modes the
binding is seen in:

    (vimpulse-vmap \"abc\" 'abc-command 'lisp-mode 'text-mode)

Otherwise, the binding is universal, but has lower priority.
Pass t to MODES to create an universal binding with presedence
over mode-specific bindings."
  (vimpulse-map-state 'visual-state key def modes))

(defun vimpulse-omap (key def &rest modes)
  "Modally bind KEY to DEF in the Operator-Pending state.
The syntax is the same as that of `global-set-key', e.g.,

    (vimpulse-omap \"abc\" 'abc-command)

The optional MODES argument specifies which major modes the
binding is seen in:

    (vimpulse-omap \"abc\" 'abc-command 'lisp-mode 'text-mode)

Otherwise, the binding is universal, but has lower priority.
Pass t to MODES to create an universal binding with presedence
over mode-specific bindings."
  (vimpulse-map-state 'operator-state key def modes))

(defun vimpulse-map! (key def &rest modes)
  "Bind KEY to DEF in vi (command) state and the Visual state.
To bind in Insert state, use `vimpulse-imap'."
  (vimpulse-map key def modes)
  (vimpulse-vmap key def modes))

(defun vimpulse-map-local (key def)
  "Make a buffer-local binding of KEY to DEF in vi (command) state.
The syntax is the same as that of `local-set-key', e.g.,

    (vimpulse-map-local \"abc\" 'abc-command)

You would typically use this in a mode hook. To make a global
binding, use `vimpulse-map'."
  (vimpulse-map-state-local 'vi-state key def))

(defun vimpulse-imap-local (key def)
  "Make a buffer-local binding of KEY to DEF in Insert state.
The syntax is the same as that of `local-set-key', e.g.,

    (vimpulse-imap-local \"abc\" 'abc-command)

You would typically use this in a mode hook. To make a global
binding, use `vimpulse-imap'."
  (vimpulse-map-state-local 'insert-state key def))

(defun vimpulse-vmap-local (key def)
  "Make a buffer-local binding of KEY to DEF in the Visual state.
The syntax is the same as that of `local-set-key', e.g.,

    (vimpulse-vmap-local \"abc\" 'abc-command)

You would typically use this in a mode hook. To make a global
binding, use `vimpulse-vmap'."
  (vimpulse-map-state-local 'visual-state key def))

(defun vimpulse-omap-local (key def)
  "Make a buffer-local binding of KEY to DEF in the Operator-Pending state.
The syntax is the same as that of `local-set-key', e.g.,

    (vimpulse-omap-local \"abc\" 'abc-command)

You would typically use this in a mode hook. To make a global
binding, use `vimpulse-omap'."
  (vimpulse-map-state-local 'visual-state key def))

;;;; Ex commands

(defvar vimpulse-extra-ex-commands
  '(("b" "buffer")
    ("bdelete" (vimpulse-kill-current-buffer))
    ("bnext" "next")
    ("clo" "close")
    ("close" (delete-window))
    ("on" "only")
    ("only" (delete-other-windows))
    ("split" (split-window))
    ("syntax" (global-font-lock-mode))
    ;; Emacs and Vim use inverted naming conventions for splits
    ("vsplit" (split-window-horizontally)))
  "Extra Ex commands, added to `ex-token-alist' when Vimpulse loads.")

(when vimpulse-want-quit-like-Vim
  (add-to-list 'vimpulse-extra-ex-commands
               '("quit" (save-buffers-kill-emacs))))

(defun vimpulse-kill-current-buffer ()
  "Kill the current buffer."
  (interactive)
  (kill-buffer nil))

(dolist (entry vimpulse-extra-ex-commands)
  (setq ex-token-alist
        (delete (assoc (car entry) ex-token-alist) ex-token-alist))
  (push entry ex-token-alist))

;;;; Paren matching

;; When highlighting matching parentheses, Emacs matches the closing
;; parenthesis before the cursor, instead of under it (like in Vim).
;; This code provides an alternate parenthesis matching function
;; used when Viper is in vi (command) mode, so that the parenthesis
;; under the cursor is matched. This makes it possible to visually
;; inspect a closing parenthesis at the end of the line.
;;
;; In Insert mode, Emacs' scheme is deemed best and kept as is.
;;
;; This code is LOADED BY DEFAULT.
;; To avoid loading it, set `vimpulse-enhanced-paren-matching' to nil
;; in your .emacs before loading Vimpulse.

;; load and enable paren.el if available
(unless (featurep 'paren)
  (condition-case nil
      (require 'paren)
    (error nil)))
(and (fboundp 'show-paren-mode)
     (not (vimpulse-custom-value-p 'show-paren-mode))
     ;; fast paren-matching
     (vimpulse-setq show-paren-delay 0)
     (show-paren-mode 1))

(defun vimpulse-paren-open-p (&optional pos)
  "Return t if the character at point (or POS) is an opening paren."
  (setq pos (or pos (point)))
  (let ((class (syntax-after pos)))
    (when class
      (setq class (syntax-class class))
      (= class 4))))

(defun vimpulse-paren-close-p (&optional pos)
  "Return t if the character at point (or POS) is an closing paren."
  (setq pos (or pos (point)))
  (let ((class (syntax-after pos)))
    (when class
      (setq class (syntax-class class))
      (= class 5))))

(defun vimpulse-paren-match (&optional pos)
  "Return the position of possible matching paren at point (or POS).
If not a paren, return `not-a-paren'. If not found, return nil."
  (setq pos (or pos (point)))
  (condition-case nil
      (cond
       ((vimpulse-paren-open-p pos)
        (1- (scan-sexps pos 1)))
       ((vimpulse-paren-close-p pos)
        (scan-sexps (1+ pos) -1))
       (t
        'not-a-paren))
    (error nil)))

(defun vimpulse-paren-match-p (pos1 pos2)
  "Return t if POS1 and POS2 are matching characters.
Checks the characters at position POS1 and POS2 and returns t
if they are matching characters (in a paren-match meaning),
nil otherwise."
  (let ((class1 (car (syntax-after pos1)))
        (match1 (cdr (syntax-after pos1)))
        (class2 (car (syntax-after pos2)))
        (match2 (cdr (syntax-after pos2))))
    (or (eq match1 (char-after pos2))
        (eq match2 (char-after pos1))
        (eq match1 match2))))

(defun vimpulse-paren-highlight (face &optional pos)
  "Highlight the paren at point (or POS) with FACE."
  (setq pos (or pos (point)))
  (let ((ovl (if (vimpulse-paren-open-p pos)
                 vimpulse-paren-overlay-open
               vimpulse-paren-overlay-close)))
    (viper-overlay-put ovl 'face face)
    (viper-move-overlay ovl pos (1+ pos))))

;; FIXME: this description sucks
(defun vimpulse-paren-highlight-pair (&optional pos)
  "Highlight paren pair.
Highlights the paren at point (or POS) and eventual matching
or mismatched paren."
  (setq pos (or pos (point)))
  (let ((match (vimpulse-paren-match pos)))
    (cond
     ((not match)
      (vimpulse-paren-highlight 'show-paren-mismatch pos))
     ((eq match 'not-a-paren)
      (vimpulse-delete-overlay vimpulse-paren-overlay-open)
      (vimpulse-delete-overlay vimpulse-paren-overlay-close))
     ((/= pos (vimpulse-paren-match match))
      (vimpulse-paren-highlight 'show-paren-mismatch pos))
     ((vimpulse-paren-match-p pos match)
      (vimpulse-paren-highlight 'show-paren-match pos)
      (vimpulse-paren-highlight 'show-paren-match match))
     (t
      (vimpulse-paren-highlight 'show-paren-mismatch pos)
      (vimpulse-paren-highlight 'show-paren-mismatch match)))))

(defadvice show-paren-function (around vimpulse-paren activate)
  "Use custom highlighting if `vimpulse-enhanced-paren-matching' is t."
  ;; define overlays if they don't exist
  (cond
   (vimpulse-enhanced-paren-matching
    (unless (viper-overlay-live-p vimpulse-paren-overlay-open)
      (setq vimpulse-paren-overlay-open
            (viper-make-overlay (point) (point) nil t nil)
            vimpulse-paren-overlay-close
            (viper-make-overlay (point) (point) nil t nil))
      (vimpulse-delete-overlay vimpulse-paren-overlay-open)
      (vimpulse-delete-overlay vimpulse-paren-overlay-close))
    (cond
     ;; Viper not in Insert, Replace or Emacs state
     ((and (not (eq viper-current-state 'insert-state))
           (not (eq viper-current-state 'replace-state))
           (not (eq viper-current-state 'emacs-state))
           show-paren-mode viper-mode)
      ;; safely delete the overlays used by `show-paren-function'
      ;; and call our custom function instead
      (and (viper-overlay-live-p show-paren-overlay)
           (vimpulse-delete-overlay show-paren-overlay))
      (and (viper-overlay-live-p show-paren-overlay-1)
           (vimpulse-delete-overlay show-paren-overlay-1))
      (vimpulse-paren-highlight-pair))
     ;; Viper in Insert mode
     (t
      ;; delete the overlays used by our custom function
      (vimpulse-delete-overlay vimpulse-paren-overlay-open)
      (vimpulse-delete-overlay vimpulse-paren-overlay-close)
      ad-do-it)))
   (t
    ad-do-it)))

;;;; Operator-Pending mode

;; This provides a framework for combining "motions" and "operators".
;; A motion is any command moving point. An operator is a command
;; acting on the text moved over by a motion.
;;
;; Defining operator commands is similar to defining commands acting
;; on the region. That is, both must have two arguments, BEG and END,
;; and an `interactive' specification that stores the relevant range
;; in those arguments. The `vimpulse-define-operator' macro takes care
;; of this. (If you like, you can also convert any region command to
;; an operator with `vimpulse-convert-to-operator'.)
;;
;; When an operator command is run in vi state, it queries the user
;; for a motion and determines the resulting range to store in BEG and
;; END. In Visual mode, it skips the querying and uses the selection
;; boundaries.
;;
;; While a motion is read from the keyboard, a temporary Viper state,
;; Operator-Pending mode, is entered. This state inherits bindings
;; from the regular vi state, but it may also define its own, for
;; instance text objects. Text objects are like motions, but define a
;; starting point as well as an ending point. They are implemented
;; simply as selection commands.
;;
;; As in Vim, a motion may specify a motion type, such as `line',
;; stored in the `motion-type' symbol property:
;;
;;   * `line': the motion range is extended to whole lines.
;;   * `inclusive': the ending character is included.
;;   * `exclusive' (default): the ending character is excluded.
;;
;; For example, (put 'foo 'motion-type 'line) gives `foo' a type of
;; `line'. If unspecified, the motion is considered `exclusive'.
;; You can override the type with v, V and C-v: for instance,
;; dvj will delete an exclusive range rather than a linewise.
;;
;; The benefit of a dedicated state when an "operator" is "pending" is
;; code separation. In the original scheme, every Viper motion had to
;; manually do the work of deleting/changing/yanking the text moved
;; over, making that action repeatable, etc. The new framework handles
;; everything automatically and orthogonally, enabling the use of
;; plain Emacs movement commands (like S-exp navigation) as motions.
;;
;; A smattering of compatibility macros ensure that certain Viper
;; motions are repeated correctly. In the long run, Viper's motions
;; should be rewritten; I'll have to contact Michael Kifer and hear
;; what he thinks about this. For what it's worth, the following code
;; addresses "TODO item #1" in viper.el.

(vimpulse-define-state operator
  "Operator-pending mode is when an operator is pending,
awaiting a motion (after \"d\", \"y\", \"c\", etc.)."
  :id "<OP> "
  :hook '(vimpulse-set-operator-cursor-type)
  :enable '(vimpulse-operator-remap-minor-mode
            (viper-vi-kbd-minor-mode nil)
            vi-state vimpulse-careful-minor-mode)
  (cond
   ((eq viper-current-state 'operator-state)
    (vimpulse-careful-minor-mode 1))
   (t
    (vimpulse-careful-minor-mode -1))))

;; This is a short-lived state, only used for calculating
;; motion ranges. If anything goes wrong and we enter the
;; command loop, exit to vi state immediately.
(defun vimpulse-operator-exit-hook ()
  "Exit Operator-Pending mode."
  (when (eq viper-current-state 'operator-state)
    (save-excursion (viper-change-state-to-vi))))

(add-hook 'pre-command-hook 'vimpulse-operator-exit-hook)
(add-hook 'post-command-hook 'vimpulse-operator-exit-hook)

;; We place all remap bindings in a keymap of their own.
;; This enables Visual mode only to inherit text object
;; bindings from Operator-Pending mode, not any remapping.
(define-minor-mode vimpulse-operator-remap-minor-mode
  "Minor mode of bindings overwritten by `vimpulse-map' et al."
  :keymap vimpulse-operator-remap-map)

(put 'vimpulse-operator-remap-map
     'remap-alist 'vimpulse-operator-remap-alist)

(defun vimpulse-operator-remap (from to)
  "Remap FROM to TO in Operator-Pending mode."
  (vimpulse-remap vimpulse-operator-remap-map from to))

(defun vimpulse-operator-remapping (cmd)
  "Return Operator-Pending remapping for CMD."
  (if (featurep 'xemacs)
      (or (cdr (assq cmd vimpulse-operator-remap-alist)) cmd)
    (or (command-remapping cmd) cmd)))

(when (featurep 'xemacs)
  ;; XEmacs shows the tag before the modes, so truncate it to a
  ;; constant length to avoid excessive flickering
  (setq vimpulse-operator-state-id "<OP>") ; 4 characters
  ;; XEmacs lacks a horizontal bar cursor option
  (setq vimpulse-want-operator-pending-cursor nil))

(defun vimpulse-set-operator-cursor-type ()
  "Change cursor appearance in Operator-Pending mode."
  (when vimpulse-want-operator-pending-cursor
    (vimpulse-half-height-cursor)))

;; The half-height "Operator-Pending cursor" cannot be specified
;; as a static `cursor-type' value, since its height depends on
;; the current font size; a function is needed.
(defun vimpulse-half-height-cursor ()
  "Change cursor to a half-height box.
\(This is really just a thick horizontal bar.)"
  (unless (featurep 'xemacs)
    (condition-case nil
        (let (height)
          ;; make `window-line-height' reliable
          (redisplay)
          (setq height (window-line-height))
          (setq height (+ (nth 0 height) (nth 3 height)))
          ;; cut cursor height in half
          (setq height (/ height 2))
          (setq cursor-type (cons 'hbar height))
          ;; ensure the cursor is redisplayed
          (force-window-update (selected-window))
          (redisplay))
      (error nil))))

(defmacro vimpulse-define-operator (operator args &rest body)
  "Define an operator command OPERATOR.
ARGS is the argument list, which must contain at least two
arguments: the beginning and end of the range. It is followed by
an optional docstring and optional keywords:

:repeat BOOLEAN         Let \\[viper-repeat] repeat the command (default).
:move-point BOOLEAN     Move to beg. of range in vi state (default).
:whole-lines BOOLEAN    Extend the range to include whole lines.
:keep-visual BOOLEN     Don't disable Visual selection.
:motion MOTION          Predefined motion to use in vi state.
:keys KEYS              A key or a list of keys to bind the command to.
:map MAP                Keymap to bind :keys in, defaults to
                        `viper-vi-basic-map'.

The keywords are followed by the operator's body. Thus, a simple
example may look like:

    (vimpulse-define-operator test (beg end)
      \"Test operator.\"
      :repeat nil
      :whole-lines t
      (delete-region beg end))

When this command is called interactively, a motion is read from
the keyboard and the resulting range is stored in BEG and END.
In Visual mode, the beginning and end of the selection are used.
The command then proceeds to do whatever it wants to do on the
text between those buffer positions, like delete it in this case.

If :motion is specified, the operator will use that motion
instead of reading one from the keyboard. This has no effect
in Visual mode."
  (declare (indent defun))
  (let ((repeat t) (move-point t)
        (map 'viper-vi-basic-map)
        beg end doc keep-visual keys keyword motion whole-lines)
    ;; collect BEG and END arguments
    (setq beg (or (pop args) 'beg)
          end (or (pop args) 'end))
    ;; collect docstring, if any
    (when (stringp (car body))
      (setq doc (pop body)))
    ;; collect keywords
    (while (keywordp (setq keyword (car body)))
      (setq body (cdr body))
      (cond
       ((eq keyword :repeat)
        (setq repeat (vimpulse-unquote (pop body))))
       ((eq keyword :move-point)
        (setq move-point (vimpulse-unquote (pop body))))
       ((eq keyword :whole-lines)
        (setq whole-lines (vimpulse-unquote (pop body))))
       ((eq keyword :keep-visual)
        (setq keep-visual (vimpulse-unquote (pop body))))
       ((eq keyword :motion)
        (setq motion (vimpulse-unquote (pop body))))
       ((eq keyword :map)
        (setq map (vimpulse-unquote (pop body))))
       ((eq keyword :keys)
        (setq keys (vimpulse-unquote (pop body))))
       (t
        (pop body))))
    (unless (listp keys)
      (setq keys (list keys)))
    ;; macro expansion: define key bindings and define command
    `(progn
       (add-to-list 'vimpulse-operators ',operator)
       (dolist (key ',keys)
         (define-key ,map key ',operator))
       (defun ,operator (,beg ,end ,@args)
         ,doc
         (interactive
          (vimpulse-range
           ,(not repeat) ,(not move-point)
           ,whole-lines ,keep-visual ',motion))
         (if (and vimpulse-inhibit-operator
                  (called-interactively-p 'any))
             (setq vimpulse-inhibit-operator nil)
           ,@body)))))

(when (fboundp 'font-lock-add-keywords)
  (font-lock-add-keywords
   'emacs-lisp-mode
   '(("(\\(vimpulse-define-operator\\)\\>[ \f\t\n\r\v]*\\(\\sw+\\)?"
      (1 font-lock-keyword-face)
      (2 font-lock-function-name-face nil t)))))

(defun vimpulse-range
  (&optional no-repeat dont-move-point whole-lines keep-visual custom-motion)
  "Read a motion and return a range (BEG END).
Belongs in the `interactive' form of a command. Don't use this
function directly; see `vimpulse-define-operator' instead."
  (let ((range (list (point) (point)))
        (type-alist '((vimpulse-visual-toggle-char . inclusive)
                      (vimpulse-visual-toggle-line . line)
                      (vimpulse-visual-toggle-block . block)))
        (type (when whole-lines 'line))
        ;; For restoration of the echo area. We bind `message-log-max' to nil
        ;; to prevent `oldmsg' from messing up the *Messages* buffer.
        (oldmsg (current-message))
        message-log-max)
    (setq vimpulse-this-motion-type nil
          vimpulse-this-count nil
          vimpulse-this-motion nil
          vimpulse-this-operator this-command
          vimpulse-inhibit-operator nil)
    (cond
     ;; if text is selected, use selection boundaries as range
     ((or vimpulse-visual-mode (region-active-p))
      (when (and whole-lines
                 (not (eq vimpulse-visual-mode 'line)))
        (vimpulse-visual-activate 'line)
        (vimpulse-set-visual-dimensions))
      ;; determine range and go to beginning
      (setq range (vimpulse-visual-range))
      (setq vimpulse-this-motion-type (vimpulse-motion-type range)
            range (vimpulse-motion-range range))
      (setq vimpulse-this-motion 'vimpulse-visual-reselect)
      (if keep-visual
          (if (eq vimpulse-visual-mode 'line)
              (vimpulse-visual-restore)
            (vimpulse-visual-contract-region))
        (if (eq vimpulse-this-motion-type 'block)
            (vimpulse-visual-block-rotate
             'upper-left
             (vimpulse-range-beginning range)
             (vimpulse-range-end range))
          (goto-char (vimpulse-range-beginning range))
          (set-mark  (vimpulse-range-end range)))
        ;; disable selection
        (if (and vimpulse-visual-mode
                 (fboundp 'vimpulse-visual-mode))
            (vimpulse-visual-mode -1)
          (vimpulse-deactivate-region))))
     ;; Not in Visual mode: use CUSTOM-MOTION if specified,
     ;; or read motion and return motion range.
     (t
      (if custom-motion
          (setq vimpulse-this-motion custom-motion)
        (vimpulse-change-state-to-operator)
        (while (progn
                 (setq vimpulse-this-motion
                       (vimpulse-keypress-parser t))
                 (setq vimpulse-this-count
                       (if vimpulse-this-count
                           (if (numberp (cadr vimpulse-this-motion))
                               (string-to-number
                                (concat (number-to-string
                                         vimpulse-this-count)
                                        (number-to-string
                                         (cadr vimpulse-this-motion))))
                             vimpulse-this-count)
                         (cadr vimpulse-this-motion))
                       vimpulse-this-motion
                       (car vimpulse-this-motion))
                 (when (assq vimpulse-this-motion type-alist)
                   (setq type (cdr (assq vimpulse-this-motion
                                         type-alist))))))
        ;; motion reading done: restore the echo area
        (if oldmsg (message "%s" oldmsg)
          (message nil))
        ;; with doubled operator ("gqgq" or "gqq"), set motion to current line
        (if (or (eq vimpulse-this-motion vimpulse-this-operator)
                (member (vimpulse-strip-prefix (this-command-keys) t)
                        '("g??" "gUU" "gqq" "guu" "gww" "g~~")))
            (setq vimpulse-this-motion 'vimpulse-line)
          (setq vimpulse-this-motion
                (vimpulse-operator-remapping vimpulse-this-motion))))
      (cond
       ;; quit if motion reading failed
       ((or (not vimpulse-this-motion)
            (memq vimpulse-this-motion '(viper-nil keyboard-quit))
            (vimpulse-operator-cmd-p vimpulse-this-motion))
        (save-excursion (viper-change-state-to-vi))
        (setq quit-flag t))
       (t
        ;; multiply operator count and motion count together
        (when (or current-prefix-arg vimpulse-this-count)
          (setq vimpulse-this-count
                (* (prefix-numeric-value current-prefix-arg)
                   (prefix-numeric-value vimpulse-this-count))))
        ;; determine type to use for type conversion
        (when (and (eq type 'inclusive)
                   (memq (vimpulse-motion-type vimpulse-this-motion)
                         '(line inclusive)))
          (setq type 'exclusive))
        (setq range (vimpulse-calculate-motion-range
                     vimpulse-this-count vimpulse-this-motion type))
        (setq vimpulse-this-motion-type (vimpulse-motion-type range)
              range (vimpulse-motion-range range))
        (unless dont-move-point
          (goto-char (vimpulse-range-beginning range))
          (when (and viper-auto-indent
                     (looking-back "^[ \f\t\v]*"))
            (back-to-indentation)))
        (save-excursion (viper-change-state-to-vi))))))
    ;; set up repeat
    (unless no-repeat
      (setq vimpulse-last-operator vimpulse-this-operator
            vimpulse-last-motion vimpulse-this-motion
            vimpulse-last-motion-type
            (when type vimpulse-this-motion-type))
      (viper-set-destructive-command
       (list 'vimpulse-operator-repeat
             vimpulse-this-count nil viper-use-register nil nil)))
    range))

(defun vimpulse-calculate-motion-range (count motion &optional type refresh)
  "Derive motion range (TYPE BEG END) from MOTION and COUNT.
MOTION can move point or select some text (a text object).
TYPE may specify the motion type for normalizing the resulting
range. If REFRESH is t, this function changes point,
`viper-com-point' and `vimpulse-this-motion-type'."
  (cond
   ;; REFRESH is nil, so bind global variables
   ((not refresh)
    (let ((opoint   (point))
          (omark    (mark t))
          (omactive (and (boundp 'mark-active) mark-active))
          (obuffer  (current-buffer))
          viper-com-point vimpulse-this-motion-type)
      (unwind-protect (vimpulse-calculate-motion-range count motion type t)
        ;; restore point and mark like `save-excursion',
        ;; but only if the motion hasn't disabled the operator
        (unless vimpulse-inhibit-operator
          (set-buffer obuffer)
          (let (mark-active) (set-mark omark))
          (and (boundp 'mark-active) (setq mark-active omactive))
          (goto-char opoint)))))
   (t
    (let ((current-prefix-arg count)
          (viper-intermediate-command 'viper-command-argument)
          (viper-current-state 'operator-state)
          (vimpulse-operator-basic-minor-mode t)
          (motion-type (vimpulse-motion-type motion t))
          (already-selection (or vimpulse-visual-mode
                                 (region-active-p)))
          (range (list 'exclusive (point) (point)))
          vimpulse-visual-vars-alist)
      (setq vimpulse-this-motion-type
            (or type motion-type 'exclusive))
      (viper-move-marker-locally 'viper-com-point (point))
      ;; enable Transient Mark mode so we can reliably
      ;; detect selection commands
      (vimpulse-transient-mark)
      ;; Whatever happens next, we must restore Transient Mark mode
      ;; to its original state afterwards!
      (unwind-protect
          ;; `vimpulse-visual-vars-alist' is used for restoring,
          ;; so protect it
          (let (vimpulse-visual-vars-alist)
            (if (commandp motion)
                (call-interactively motion)
              (funcall motion count))
            (cond
             ;; if text has been selected (i.e., it's a text object),
             ;; return the selection
             ((and (not already-selection)
                   (or vimpulse-visual-mode (region-active-p)))
              (setq range (vimpulse-visual-range))
              (cond
               ((and motion-type (not (eq (car range) motion-type)))
                (setcar range motion-type))
               ((and type (not (eq (car range) type)))
                (setcar range type)
                (setq range (vimpulse-normalize-motion-range range))))
              ;; deactivate Visual mode/region
              (if (and vimpulse-visual-mode
                       (fboundp 'vimpulse-visual-mode))
                  (vimpulse-visual-mode -1)
                (vimpulse-deactivate-region)))
             ;; otherwise, range is defined by `viper-com-point'
             ;; and point (Viper type motion)
             (t
              (setq range (vimpulse-make-motion-range
                           (marker-position viper-com-point)
                           (point)
                           (or type vimpulse-this-motion-type) t)))))
        (vimpulse-transient-restore))
      range))))

;; A keypress parser of some kind is unavoidable because we need to
;; know what we are executing beforehand (like when multiplying the
;; counts in "2d3w"). We try to avoid hard-coding where possible by
;; inspecting commands rather than the keys themselves.
(defun vimpulse-keypress-parser (&optional no-remap)
  "Read from keyboard and build a command description.
Returns (CMD COUNT), where COUNT is the numeric prefix argument
of CMD. Both COUNT and CMD may be nil."
  (let ((inhibit-quit t)
        (echo-keystrokes 0.01)
        char digit keys cmd count)
    (while (progn
             ;; read a keypress, respecting Emacs version,
             ;; and convert it to ASCII representation
             (if (featurep 'xemacs)
                 (setq char (event-to-character
                             (next-command-event) nil t))
               (setq char (read-event))
               (when (symbolp char)
                 (setq char (or (get char 'ascii-character) char))))
             ;; this trick from simple.el's `digit-argument'
             ;; converts keystrokes like C-0 and C-M-1 to digits
             (if (or (characterp char) (integerp char))
                 (setq digit (- (logand char ?\177) ?0))
               (setq digit nil))
             (if (keymapp cmd)
                 (setq keys (vconcat keys (vector char)))
               (setq keys (vector char)))
             (if no-remap              ; XEmacs doesn't have remapping
                 (setq cmd (key-binding keys t))
               (setq cmd (key-binding keys t t)))
             ;; this `cond' form determines whether
             ;; the reading loop will continue
             (cond
              ;; if calling itself ("cc"), return current command
              ((eq (vimpulse-strip-prefix
                    (vconcat (this-command-keys))) keys)
               (setq cmd this-command)
               nil)
              ;; if CMD is a keymap, we need to read more
              ((keymapp cmd)
               t)
              ;; numeric prefix argument
              ((or (memq cmd '(viper-digit-argument digit-argument))
                   ;; the 0 key runs `viper-beginning-of-line',
                   ;; so ignore it unless preceded by other digits
                   (and (eq (length keys) 1)
                        (not (keymapp cmd))
                        count
                        ;; probably overkill: only 0 bound this way
                        (memq digit '(0 1 2 3 4 5 6 7 8 9))))
               ;; store digits in a string, which is easily converted
               ;; to a number afterwards
               (setq count (concat (or count "")
                                   (number-to-string digit)))
               t)
              ;; catch middle digits like "da2w"
              ((and (not cmd)
                    (> (length keys) 1)
                    (memq digit '(0 1 2 3 4 5 6 7 8 9)))
               (setq count (concat (or count "")
                                   (number-to-string digit)))
               ;; remove the digit from the key sequence
               ;; so we can see if the previous one goes anywhere
               (setq keys (vimpulse-truncate keys -1))
               (setq cmd (key-binding keys))
               t)
              ;; We might as well accept negative numbers using
              ;; Emacs' C--. Best of both worlds, right?
              ((eq cmd 'negative-argument)
               (unless count
                 (setq count "-")))
              ;; user pressed C-g, so return nil for CMD
              ((eq cmd 'keyboard-quit)
               (setq cmd nil))
              ;; we are done, exit the `while' loop
              (t
               nil))))
    ;; determine COUNT
    (when (stringp count)
      (if (string= count "-")
          (setq count nil)
        (setq count (string-to-number count))))
    ;; return command description
    (list cmd count)))

(defmacro vimpulse-with-operator-message (beg end template &rest body)
  "Echo an operator message after executing BODY.
BEG and END specify the text range acted upon.
TEMPLATE is a string like \"Deleted <N>\", where <N>
is substituted with the amount of characters or lines,
which is determined before executing BODY. The range type
is read from `vimpulse-this-motion-type'.

This macro respects `viper-change-notification-threshold'."
  (declare (indent defun))
  `(let* ((range (vimpulse-make-motion-range ,beg ,end vimpulse-this-motion-type))
          (height (vimpulse-range-height range t))
          (width  (vimpulse-range-width range t))
          (template (replace-regexp-in-string
                     "<N>"
                     (apply 'format
                            (cond
                             ((eq vimpulse-this-motion-type 'block)
                              (list "%s row%s and %s column%s"
                                    height
                                    (if (/= (abs height) 1) "s" "")
                                    width
                                    (if (/= (abs width) 1) "s" "")))
                             ((eq vimpulse-this-motion-type 'line)
                              (list "%s line%s"
                                    height
                                    (if (/= (abs height) 1) "s" "")))
                             (t
                              (list "%s character%s"
                                    width
                                    (if (/= (abs width) 1) "s" "")))))
                     ,template)))
     (prog1 (progn ,@body)
       (when (and template
                  (not (viper-is-in-minibuffer))
                  (or (> (or height width)
                         viper-change-notification-threshold)
                      (> (or width height)
                         viper-change-notification-threshold)))
         (message "%s" template)))))

(when (fboundp 'font-lock-add-keywords)
  (font-lock-add-keywords
   'emacs-lisp-mode
   '(("(\\(vimpulse-with-operator-message\\)\\>" 1 font-lock-keyword-face))))

;; utility macro for converting region commands to operators
(defmacro vimpulse-convert-to-operator (region-cmd &rest args)
  "Convert a region command to an operator command.
Defines a new command with the name REGION-CMD-operator.
ARGS is passed to `vimpulse-range'."
  `(vimpulse-define-operator
     ,(intern (concat (symbol-name region-cmd) "-operator"))
     (beg end)
     ,(format "Operator-wrapper for `%s'.\n\n%s"
              region-cmd (documentation region-cmd t))
     (,region-cmd beg end)))

(defun vimpulse-operator-cmd-p (cmd)
  "Return t if CMD is an operator command."
  (and (memq cmd vimpulse-operators) t))

;;; Repeat an operator/motion combination

;; this is used in `viper-d-com' (read by `viper-repeat')
(defun vimpulse-operator-repeat (arg)
  "Repeat an operator-motion combination.
ARG is a list of the form (COUNT . COM).
COM is discarded."
  (let ((val (viper-P-val arg)))
    (cond
     ((region-active-p)
      (funcall vimpulse-last-operator
               (region-beginning) (region-end)))
     (t
      (vimpulse-operator-apply
       vimpulse-last-operator vimpulse-last-motion val
       vimpulse-last-motion-type)))))

(defun vimpulse-operator-apply (operator motion count &optional type)
  "Apply OPERATOR on MOTION. COUNT is the motion count.
TYPE is the motion type."
  (let ((vimpulse-this-operator operator)
        (vimpulse-this-motion motion)
        (vimpulse-this-motion-type (or type vimpulse-this-motion-type))
        vimpulse-inhibit-operator beg end range)
    (setq range (vimpulse-calculate-motion-range count motion type)
          beg   (vimpulse-range-beginning range)
          end   (vimpulse-range-end range)
          vimpulse-this-motion-type (vimpulse-motion-type range))
    (unless vimpulse-inhibit-operator
      (funcall operator beg end))))

;;; Operators

;; yank, delete, change
(vimpulse-define-operator vimpulse-yank (beg end)
  "Yank text from BEG to END."
  :repeat nil
  :move-point nil
  (let (last-command)
    (vimpulse-with-operator-message beg end "Saved <N>"
      (cond
       ((eq vimpulse-this-motion-type 'block)
        (setq killed-rectangle (extract-rectangle beg end))
        ;; associate the rectangle with the last entry in the kill-ring
        (unless kill-ring
          (copy-region-as-kill beg end))
        (put 'killed-rectangle 'previous-kill (current-kill 0))
        (vimpulse-visual-block-rotate 'upper-left beg end))
       (t
        (vimpulse-store-in-current-register beg end)
        (copy-region-as-kill beg end)
        (unless (eq vimpulse-this-motion-type 'line)
          (goto-char beg))
        (when (and (eolp) (not (bolp)))
          (backward-char)))))))

(vimpulse-define-operator vimpulse-delete (beg end &optional dont-save)
  "Delete text from BEG to END.
If DONT-SAVE is non-nil, don't store the deleted text on `kill-ring'."
  (let (last-command)
    (vimpulse-with-operator-message beg end "Deleted <N>"
      (cond
       (dont-save
        (if (eq vimpulse-this-motion-type 'block)
            (delete-rectangle beg end)
          (delete-region beg end)))
       ((eq vimpulse-this-motion-type 'block)
        (let ((orig (make-marker)))
          ;; associate the rectangle with the last entry in the kill-ring
          (viper-move-marker-locally
           'orig (vimpulse-visual-block-position 'upper-left beg end))
          (unless kill-ring
            (copy-region-as-kill beg end))
          (kill-rectangle beg end)
          (put 'killed-rectangle 'previous-kill (current-kill 0))
          (goto-char orig)
          (set-marker orig nil)))
       (t
        (vimpulse-store-in-current-register beg end)
        (kill-region beg end)
        (when (and (eolp) (not (bolp)))
          (backward-char)))))))

(vimpulse-define-operator vimpulse-change (beg end &optional dont-save)
  "Change text from BEG to END.
If DONT-SAVE is non-nil, don't store the deleted text on `kill-ring'."
  (when vimpulse-want-change-undo
    (vimpulse-start-undo-step))
  (cond
   ((eq vimpulse-this-motion-type 'block)
    (vimpulse-delete beg end dont-save)
    (goto-char
     (vimpulse-visual-create-coords
      'block ?i
      (min vimpulse-visual-point vimpulse-visual-mark)
      (1+ (max vimpulse-visual-point vimpulse-visual-mark))))
    (viper-insert nil))
   ((eq viper-intermediate-command 'viper-repeat)
    (if dont-save
        (delete-region beg end)
      (kill-region beg end))
    (goto-char beg)
    (when (eq vimpulse-this-motion-type 'line)
      (save-excursion (newline))
      (when viper-auto-indent
        (indent-according-to-mode)))
    (viper-yank-last-insertion))
   ((eq vimpulse-this-motion-type 'line)
    (setq viper-began-as-replace t)
    (if dont-save
        (delete-region beg end)
      (vimpulse-store-in-current-register beg end)
      (kill-region beg end))
    (save-excursion (newline))
    (when viper-auto-indent
      (indent-according-to-mode))
    (viper-change-state-to-insert))
   (t
    (if dont-save
        (progn
          (delete-region beg end)
          (viper-change-state-to-insert))
      (vimpulse-store-in-current-register beg end)
      (viper-change beg end)))))

;; r, J, =, >, <
(vimpulse-define-operator vimpulse-replace (beg end)
  "Replace all selected characters with ARG."
  :move-point nil
  :keep-visual t
  :motion 'forward-char
  (let (endpos length visual-p)
    (setq endpos (max beg (1- end)))
    (unless (and (eq viper-intermediate-command 'viper-repeat)
                 viper-d-char)
      (unwind-protect
          (progn
            (vimpulse-set-replace-cursor-type)
            (save-excursion
              (viper-special-read-and-insert-char))
            (setq viper-d-char (char-after))
            (delete-char 1))
        (viper-restore-cursor-type)
        (when vimpulse-visual-mode
          (vimpulse-visual-mode -1)
          (setq endpos beg))))
    (cond
     ((eq vimpulse-this-motion-type 'block)
      (setq length (abs (- (save-excursion
                             (goto-char beg)
                             (current-column))
                           (save-excursion
                             (goto-char end)
                             (current-column)))))
      (vimpulse-apply-on-block
       (lambda (beg end)
         (goto-char beg)
         (delete-region beg end)
         (insert (make-string length viper-d-char)))
       beg end))
     (t
      (goto-char beg)
      (while (< (point) end)
        (if (looking-at "\n")
            (forward-char)
          (delete-char 1)
          (insert-char viper-d-char 1)))
      (goto-char endpos)))))

(vimpulse-define-operator vimpulse-join (beg end)
  "Join the selected lines."
  :whole-lines t
  :motion 'vimpulse-line
  (let ((num (count-lines beg end)))
    (unless (> num 2)
      (setq num 2))
    (viper-join-lines num)))

(vimpulse-define-operator vimpulse-indent (beg end)
  "Indent text according to mode."
  :repeat nil
  :whole-lines t
  (indent-region beg end nil)
  (when viper-auto-indent
    (back-to-indentation)))

(vimpulse-define-operator vimpulse-shift-left (beg end)
  "Shift all selected lines to the left."
  (let ((nlines (count-lines beg end)))
    (viper-next-line (cons (1- nlines) ?<))))

(vimpulse-define-operator vimpulse-shift-right (beg end)
  "Shift all selected lines to the right."
  (let ((nlines (count-lines beg end)))
    (viper-next-line (cons (1- nlines) ?>))))

;; gq, gu, gU
(vimpulse-define-operator vimpulse-fill (beg end)
  "Fill text."
  :repeat nil
  :move-point nil
  (setq end (save-excursion
              (goto-char end)
              (skip-chars-backward " ")
              (point)))
  (save-excursion
    (fill-region beg end)))

(vimpulse-define-operator vimpulse-downcase (beg end)
  "Convert text to lower case."
  (if (eq vimpulse-this-motion-type 'block)
      (vimpulse-apply-on-block 'downcase-region beg end)
    (downcase-region beg end))
  (when (and viper-auto-indent
             (looking-back "^[ \f\t\v]*"))
    (back-to-indentation)))

(vimpulse-define-operator vimpulse-upcase (beg end)
  "Convert text to upper case."
  (if (eq vimpulse-this-motion-type 'block)
      (vimpulse-apply-on-block 'upcase-region beg end)
    (upcase-region beg end)
    (when (and viper-auto-indent
               (looking-back "^[ \f\t\v]*"))
      (back-to-indentation))))

(vimpulse-define-operator vimpulse-invert-case (beg end)
  "Convert text to inverted case."
  (let (char)
    (save-excursion
      (cond
       ((eq vimpulse-this-motion-type 'block)
        (let (vimpulse-this-motion-type)
          (vimpulse-apply-on-block 'vimpulse-invert-case beg end)))
       (t
        (goto-char beg)
        (while (< beg end)
          (setq char (following-char))
          (delete-char 1 nil)
          (if (eq (upcase char) char)
              (insert-char (downcase char) 1)
            (insert-char (upcase char) 1))
          (setq beg (1+ beg))))))
    (when (and viper-auto-indent
               (looking-back "^[ \f\t\v]*"))
      (back-to-indentation))))

(vimpulse-define-operator vimpulse-invert-char (beg end)
  "Invert case of character."
  :keep-visual t
  :motion 'forward-char
  (vimpulse-invert-case beg end)
  (cond
   (vimpulse-visual-mode
    (goto-char beg)
    (vimpulse-visual-mode -1))
   (t
    (goto-char end))))

(vimpulse-define-operator vimpulse-rot13 (beg end)
  "ROT13 encrypt text."
  (rot13-region beg end))

;;; Compatibility code allowing old-style Viper motions to work

;; Postpone operator execution by disabling `viper-execute-com'.
;; In the old scheme, the operator was executed inside the motion
;; (by a call to this function), rather than after it; the following
;; advice removes this behavior. However, some motions, like f and /,
;; need to access `viper-d-com' for negative count and command-keys
;; while repeating, so certain parts must be carefully retained.
(defadvice viper-execute-com (around vimpulse-operator activate)
  "Disable in Operator-Pending mode."
  (cond
   ((eq viper-current-state 'operator-state)
    ;; ?r is Viper's "dummy operator", associated with
    ;; `viper-exec-dummy' in `viper-exec-array'
    (setq com ?r)
    ad-do-it
    ;; while repeating, put needed values in `viper-d-com'
    (unless (or (eq this-command 'viper-repeat)
                (eq viper-intermediate-command 'viper-repeat))
      (unless viper-d-com
        (setq viper-d-com (list nil nil nil nil nil nil)))
      (unless (eq vimpulse-this-motion
                  (vimpulse-operator-remapping m-com))
        (setq vimpulse-this-motion (vimpulse-operator-remapping m-com))
        (setcar (nthcdr 2 viper-d-com) com))
      (setq vimpulse-this-count val)
      (setcar (nthcdr 5 viper-d-com)
              (viper-array-to-string
               (if (arrayp viper-this-command-keys)
                   viper-this-command-keys
                 (this-command-keys))))))
   (t
    ad-do-it)))

;; this separates the operator-pending part of a Viper motion from the
;; rest, defining a new command called vimpulse-operator-MOTION
(defmacro vimpulse-operator-map-define
  (viper-motion &optional type &rest body)
  "Define a new command for the Operator-Pending part of VIPER-MOTION.
The new command is named VIMPULSE-OPERATOR-MOTION and has motion
type TYPE. A custom function body may be specified via BODY."
  (declare (indent 2))
  `(let* ((viper-motion ',viper-motion)
          (type ,type)
          (body ',body)
          (motion-name (symbol-name viper-motion))
          (docstring (documentation viper-motion t)))
     (setq type (or type (vimpulse-motion-type viper-motion)))
     (unless (memq type '(inclusive line block))
       (setq type 'exclusive))
     (setq motion-name (replace-regexp-in-string
                        "^viper-\\\|^vimpulse-" "" motion-name))
     (setq motion-name
           (concat "vimpulse-operator-" motion-name))
     (setq motion-name (intern motion-name))
     (add-to-list 'vimpulse-movement-cmds motion-name)
     (vimpulse-operator-remap viper-motion motion-name)
     (eval `(defun ,motion-name (arg)
              ,(format "Operator-pending %s part of `%s'.\n\n%s"
                       type viper-motion (or docstring ""))
              ,@(if body body
                  `((interactive "P")
                    (let (com com-alist)
                      (setq com-alist
                            '((vimpulse-change . ?c)
                              (vimpulse-delete . ?d)
                              (vimpulse-yank . ?y)))
                      (setq com
                            (or (cdr (assq vimpulse-this-operator
                                           com-alist))
                                ?r))
                      (,viper-motion (if (region-active-p)
                                         arg
                                       (cons arg com)))
                      ,(unless (eq type 'exclusive)
                         '(viper-backward-char-carefully)))))))
     (put motion-name 'motion-type type)
     `(quote ,motion-name)))

;; d%: when point is before the parenthetical expression,
;; include it in the resulting range
(vimpulse-operator-map-define viper-paren-match 'inclusive
  (interactive "P")
  (let ((orig (point)))
    (viper-paren-match arg)
    (viper-move-marker-locally 'viper-com-point orig)
    (when (integerp arg)
      (setq vimpulse-this-motion-type 'line))))

;; Viper quirk: cw only deletes a single character when at whitespace,
;; dw deletes all of it. Use the latter behavior in both cases.
(vimpulse-operator-map-define viper-forward-word 'exclusive
  (interactive "P")
  (let ((com-alist '((vimpulse-change . ?c)
                     (vimpulse-delete . ?d)
                     (vimpulse-yank . ?y))) com)
    (if (looking-at "[[:space:]]")
        (setq com ?d)
      (setq com (or (cdr (assq vimpulse-this-operator com-alist)) ?r)))
    (viper-forward-word (if (region-active-p)
                            arg
                          (cons arg com)))))

(vimpulse-operator-map-define viper-forward-Word 'exclusive
  (interactive "P")
  (let ((com-alist '((vimpulse-change . ?c)
                     (vimpulse-delete . ?d)
                     (vimpulse-yank . ?y))) com)
    (if (looking-at "[[:space:]]")
        (setq com ?d)
      (setq com (or (cdr (assq vimpulse-this-operator com-alist)) ?r)))
    (viper-forward-Word (if (region-active-p)
                            arg
                          (cons arg com)))))

;;; remap non-motion commands to `viper-nil'
(vimpulse-operator-remap 'undo 'viper-nil)
(vimpulse-operator-remap 'undo-tree-redo 'viper-nil)
(vimpulse-operator-remap 'redo 'viper-nil)
(vimpulse-operator-remap 'vimpulse-put-and-indent 'viper-nil)
(vimpulse-operator-remap 'vimpulse-Put-and-indent 'viper-nil)
(vimpulse-operator-remap 'viper-Put-back 'viper-nil)
(vimpulse-operator-remap 'viper-put-back 'viper-nil)
(vimpulse-operator-remap 'viper-delete-backward-char 'viper-nil)
(vimpulse-operator-remap 'viper-delete-char 'viper-nil)
(vimpulse-operator-remap 'viper-insert 'viper-nil)
(vimpulse-operator-remap 'viper-intercept-ESC-key 'viper-nil)
(vimpulse-operator-remap 'viper-line-to-bottom 'viper-nil)
(vimpulse-operator-remap 'viper-line-to-middle 'viper-nil)
(vimpulse-operator-remap 'viper-line-to-top 'viper-nil)
(vimpulse-operator-remap 'viper-repeat 'viper-nil)
(vimpulse-operator-remap 'viper-substitute 'viper-nil)

;; these motions need wrapper functions to repeat correctly
(vimpulse-operator-map-define viper-end-of-Word 'inclusive)
(vimpulse-operator-map-define viper-end-of-word 'inclusive)
(vimpulse-operator-map-define viper-find-char-backward 'exclusive)
(vimpulse-operator-map-define viper-find-char-forward 'inclusive)
(vimpulse-operator-map-define viper-forward-char 'inclusive)
(vimpulse-operator-map-define viper-goto-char-backward 'exclusive)
(vimpulse-operator-map-define viper-goto-char-forward 'inclusive)
(vimpulse-operator-map-define viper-search-backward 'exclusive)
(vimpulse-operator-map-define viper-search-forward 'exclusive)

;; set up motion types for remaining Viper motions
(put 'vimpulse-goto-first-line 'motion-type 'line)
(put 'vimpulse-goto-mark-and-skip-white 'motion-type 'line)
(put 'vimpulse-end-of-visual-line 'motion-type 'inclusive)
(put 'viper-backward-Word 'motion-type 'exclusive)
(put 'viper-backward-char 'motion-type 'exclusive)
(put 'viper-backward-paragraph 'motion-type 'exclusive)
(put 'viper-backward-sentence 'motion-type 'exclusive)
(put 'viper-backward-word 'motion-type 'exclusive)
(put 'viper-beginning-of-line 'motion-type 'exclusive)
(put 'viper-forward-paragraph 'motion-type 'exclusive)
(put 'viper-forward-sentence 'motion-type 'exclusive)
(put 'viper-goto-eol 'motion-type 'inclusive)
(put 'viper-goto-line 'motion-type 'line)
(put 'viper-goto-mark 'motion-type 'exclusive)
(put 'viper-goto-mark-and-skip-white 'motion-type 'line)
(put 'viper-next-line 'motion-type 'line)
(put 'viper-previous-line 'motion-type 'line)
(put 'viper-search-Next 'motion-type 'exclusive)
(put 'viper-search-next 'motion-type 'exclusive)
(put 'viper-window-bottom 'motion-type 'line)
(put 'viper-window-middle 'motion-type 'line)
(put 'viper-window-top 'motion-type 'line)
(put 'next-line 'motion-type 'line)
(put 'previous-line 'motion-type 'line)

;;;; Text objects support

;; The following code implements support for text objects and commands
;; like diw, daw, ciw, caw. Currently, the most common objects are
;; supported:
;;
;;   - bracket-blocks: b B { [ ( < > ) ] }
;;   - sentences: s
;;   - paragraphs: p
;;   - quoted expressions: " and '
;;   - words: w and W
;;
;; Vimpulse's text objects are fairly close to Vim's, and are based on
;; Viper's movement commands. More objects are easily added with
;; `vimpulse-define-text-object'.

(defmacro vimpulse-define-text-object (object args &rest body)
  "Define a text object OBJECT.
ARGS is the argument list, which must contain at least one argument:
the count. It is followed by an optional docstring and optional
keywords:

:keys KEYS      A key or a list of keys to bind the command to.
:map MAP        Keymap to bind :keys in, defaults to
                `vimpulse-operator-basic-map'.
:type TYPE      The object's motion type.

The keywords are followed by the object's body, which must return
a pure range (BEG END) or a motion range (TYPE BEG END). Thus,
a simple example may look somewhat like:

    (vimpulse-define-text-object test (arg)
      \"Test object.\"
      :keys \"t\"
      (list 'exclusive (point) (+ arg (point))))

Here, the count is stored in ARG. Note that the body must be able
to handle a negative value, which specifies reverse direction."
  (declare (indent defun))
  (let ((map 'vimpulse-operator-basic-map)
        count doc keys keyword type)
    ;; collect COUNT argument
    (setq args (or args (list 'arg))
          count (car args))
    ;; collect docstring, if any
    (when (stringp (car body))
      (setq doc  (car body)
            body (cdr body)))
    ;; collect keywords
    (while (keywordp (setq keyword (car body)))
      (setq body (cdr body))
      (cond
       ((eq keyword :keys)
        (setq keys (vimpulse-unquote (pop body))))
       ((eq keyword :map)
        (setq map (vimpulse-unquote (pop body))))
       ((eq keyword :type)
        (setq type (vimpulse-unquote (pop body))))
       (t
        (pop body))))
    (unless (listp keys)
      (setq keys (list keys)))
    (when type
      (setq type `(',type)))
    ;; macro expansion: define key bindings, set motion type
    ;; and define command
    `(progn
       (dolist (key ',keys)
         (define-key ,map key ',object))
       ,(when type
          `(put ',object 'motion-type ,@type))
       (defun ,object ,args
         ,doc
         (interactive "p")
         (let ((,count (if (numberp ,count) ,count 1))
               range)
           (cond
            ((region-active-p)
             (when (< (point) (mark t))
               (setq ,count (- ,count)))
             (when (memq vimpulse-visual-mode '(line block))
               (vimpulse-visual-activate 'char))
             (when (and vimpulse-visual-mode
                        (not vimpulse-visual-region-expanded))
               (vimpulse-visual-expand-region))
             (setq range (progn ,@body))
             (unless (vimpulse-mark-range range t ,@type)
               ;; Are we stuck (unchanged region)?
               ;; Move forward and try again.
               (viper-forward-char-carefully (if (< ,count 0) -1 1))
               (setq range (progn ,@body))
               (vimpulse-mark-range range t ,@type)))
            (t
             (setq range (progn ,@body))
             (vimpulse-mark-range range nil ,@type))))))))

(when (fboundp 'font-lock-add-keywords)
  (font-lock-add-keywords
   'emacs-lisp-mode
   '(("(\\(vimpulse-define-text-object\\)\\>[ \f\t\n\r\v]*\\(\\sw+\\)?"
      (1 font-lock-keyword-face)
      (2 font-lock-function-name-face nil t)))))

(defun vimpulse-mark-range (range &optional widen type)
  "Mark RANGE, which has the form (BEG END) or (TYPE BEG END).
If WIDEN is non-nil, expands existing region. If the TYPE
argument is specified, it overrides the type of RANGE."
  (let* ((type  (or type (vimpulse-motion-type range)))
         (range (vimpulse-motion-range range))
         (beg (vimpulse-range-beginning range))
         (end (vimpulse-range-end range)))
    (cond
     ((eq type 'exclusive)
      (if vimpulse-visual-mode
          (vimpulse-visual-select beg end widen)
        (vimpulse-set-region beg end widen)))
     (t
      (when vimpulse-visual-mode
        (unless (memq type '(line block))
          (setq type 'char))
        (unless (eq type vimpulse-visual-mode)
          (vimpulse-visual-activate type)))
      (vimpulse-visual-select beg end widen)))))

;;; Text object range functions

;; word-like expressions (words, sentences, paragraphs)
(defun vimpulse-object-range
  (count backward-func forward-func &optional type)
  "Return a text object range (TYPE BEG END).
BACKWARD-FUNC moves point to the object's beginning,
FORWARD-FUNC moves to its end. Schematically,

\(vimpulse-object-range <num> <beg-of-object> <end-of-object>)

COUNT is the number of objects. If positive, go forwards and
then backwards; if negative, go backwards and then forwards.

The type of the object (`exclusive', `inclusive' or `line')
may be specified with TYPE. Otherwise, the type is inferred
from the motion types of BACKWARD-FUNC and FORWARD-FUNC."
  (let ((types '(exclusive inclusive line block))
        beg end forward-range backward-range
        viper-com-point
        vimpulse-visual-vars-alist
        vimpulse-this-motion
        vimpulse-this-motion-type)
    (save-excursion
      (setq count (or (if (eq count 0) 1 count) 1))
      (if (< count 0)
          (setq backward-range
                (vimpulse-calculate-motion-range
                 (abs count) backward-func type t)
                forward-range
                (vimpulse-calculate-motion-range
                 (abs count) forward-func type t))
        (setq forward-range
              (vimpulse-calculate-motion-range
               (abs count) forward-func type t)
              backward-range
              (vimpulse-calculate-motion-range
               (abs count) backward-func type t)))
      (setq beg (apply 'min (vimpulse-motion-range backward-range))
            end (apply 'max (vimpulse-motion-range forward-range)))
      (unless type
        (setq type 'exclusive)
        (dolist (elt types)
          (when (or (eq elt (vimpulse-motion-type backward-range))
                    (eq elt (vimpulse-motion-type forward-range)))
            (setq type elt))))
      (list type beg end))))

(defun vimpulse-an-object-range
  (count backward-func forward-func &optional include-newlines regexp)
  "Return a text object range (BEG END) with whitespace.
Unless INCLUDE-NEWLINES is t, whitespace inclusion is restricted
to the line(s) the object is on. REGEXP is a regular expression
for matching whitespace; the default is \"[ \\f\\t\\n\\r\\v]+\".
See `vimpulse-object-range' for more details."
  (let (range beg end line-beg line-end mark-active-p)
    (save-excursion
      (setq count (or (if (eq count 0) 1 count) 1))
      (setq regexp (or regexp "[ \f\t\n\r\v]+"))
      (setq range (vimpulse-motion-range
                   (vimpulse-object-range
                    count backward-func forward-func)))
      ;; let `end' be the boundary furthest from point,
      ;; based on the direction we are going
      (if (< count 0)
          (setq beg (cadr range)
                end (car range))
        (setq beg (car range)
              end (cadr range)))
      ;; if INCLUDE-NEWLINES is nil, never move past
      ;; the line boundaries of the text object
      (unless include-newlines
        (setq line-beg (line-beginning-position)
              line-end (line-end-position))
        (when (> (* count beg)
                 (max (* count line-beg) (* count line-end)))
          (setq count (- count))
          (setq range (vimpulse-motion-range
                       (vimpulse-object-range
                        count backward-func forward-func)))
          (if (< count 0)
              (setq beg (cadr range)
                    end (car range))
            (setq beg (car range)
                  end (cadr range))))
        (setq line-beg (save-excursion
                         (goto-char (min beg end))
                         (line-beginning-position))
              line-end (save-excursion
                         (goto-char (max beg end))
                         (line-end-position))))
      ;; Generally only include whitespace at one side (but see below).
      ;; If we are before the object, include leading whitespace;
      ;; if we are inside the object, include trailing whitespace.
      ;; If trailing whitespace inclusion fails, include leading.
      (setq count (if (< count 0) -1 1))
      (when (or (< (* count (point)) (* count beg))
                (eq end (setq end (save-excursion
                                    (goto-char end)
                                    (vimpulse-skip-regexp
                                     regexp count line-beg line-end)))))
        (setq beg (save-excursion
                    (goto-char beg)
                    (if (and (not include-newlines)
                             (looking-back "^[ \t]*"))
                        beg
                      (vimpulse-skip-regexp
                       regexp (- count) line-beg line-end))))
        ;; Before/after adjustment for whole lines: if the object is
        ;; followed by a blank line, include that as trailing
        ;; whitespace and subtract a line from the leading whitespace.
        (when include-newlines
          (goto-char end)
          (forward-line count)
          (when (looking-at "[ \t]*$")
            (setq end (line-beginning-position))
            (goto-char beg)
            (when (looking-at "[ \t]*$")
              (forward-line count)
              (setq beg (line-beginning-position))))))
      ;; return the range
      (list (min beg end) (max beg end)))))

(defun vimpulse-inner-object-range
  (count backward-func forward-func)
  "Return a text object range (BEG END) including point.
If point is outside the object, it is included in the range.
To include whitespace, use `vimpulse-an-object-range'.
See `vimpulse-object-range' for more details."
  (let (range beg end line-beg line-end)
    (setq count (or (if (eq count 0) 1 count) 1))
    (setq range (vimpulse-motion-range
                 (vimpulse-object-range
                  count backward-func forward-func)))
    (setq beg (car range)
          end (cadr range))
    (setq line-beg (line-beginning-position)
          line-end (line-end-position))
    (when (> (min (* count beg) (* count end))
             (max (* count line-beg) (* count line-end)))
      (setq count (- count))
      (setq range (vimpulse-motion-range
                   (vimpulse-object-range
                    count backward-func forward-func))
            beg (car range)
            end (cadr range)))
    ;; return the range, including point
    (list (min beg (point)) (max end (point)))))

;; parenthetical expressions
(defun vimpulse-paren-range (count &optional open close include-parentheses)
  "Return a parenthetical expression range (BEG END).
The type of parentheses may be specified with OPEN and CLOSE,
which must be characters. INCLUDE-PARENTHESES specifies
whether to include the parentheses in the range."
  (let ((beg (point)) (end (point))
        line-beg line-end)
    (setq count (if (eq count 0) 1 (abs count)))
    (save-excursion
      (setq open  (if (characterp open)
                      (regexp-quote (string open)) "")
            close (if (characterp close)
                      (regexp-quote (string close)) ""))
      (when (and (not (string= open ""))
                 (looking-at open))
        (forward-char))
      ;; find opening and closing paren with Emacs' S-exp facilities
      (while (progn
               (vimpulse-backward-up-list 1)
               (not (when (looking-at open)
                      (when (save-excursion
                              (forward-sexp)
                              (when (looking-back close)
                                (setq end (point))))
                        (if (>= count 0)
                            (setq beg (point))
                          (setq count (1- count)) nil))))))
      (if include-parentheses
          (list beg end)
        (setq beg (prog1 (min (1+ beg) end)
                    (setq end (max (1- end) beg))))
        (if (<= (count-lines beg end) 1)
            (list beg end)
          ;; multi-line inner range: select whole lines
          (goto-char beg)
          (when (looking-at "[ \f\t\n\r\v]*$")
            (forward-line)
            ;; Include indentation?
            (if (and viper-auto-indent
                     (not (eq vimpulse-this-operator
                              'vimpulse-delete)))
                (back-to-indentation)
              (beginning-of-line))
            (setq beg (point)))
          (goto-char end)
          (when (and (looking-back "^[ \f\t\n\r\v]*")
                     (not (eq vimpulse-this-operator
                              'vimpulse-delete)))
            (setq end (line-end-position 0))
            (goto-char end))
          (list (min beg end) (max beg end)))))))

;; quoted expressions
(defun vimpulse-quote-range (count &optional quote include-quotes)
  "Return a quoted expression range (BEG END).
QUOTE is a quote character (default ?\\\"). INCLUDE-QUOTES
specifies whether to include the quote marks in the range."
  (let ((beg (point)) (end (point))
        regexp)
    (save-excursion
      (setq count (if (eq count 0) 1 (abs count)))
      (setq quote (or quote ?\"))
      (setq quote (if (characterp quote)
                      (regexp-quote (string quote)) "")
            regexp (concat "\\([^\\\\]\\|^\\)" quote))
      (when (and (not (string= quote ""))
                 (looking-at quote))
        (forward-char))
      ;; search forward for a closing quote
      (while (and (> count 0)
                  (re-search-forward regexp nil t))
        (setq count (1- count))
        (setq end (point))
        ;; find the matching opening quote
        (condition-case nil
            (progn
              (setq beg (scan-sexps end -1))
              ;; Emacs' S-exp logic doesn't work in text mode
              (save-excursion
                (goto-char beg)
                (unless (looking-at quote)
                  (re-search-backward regexp)
                  (unless (looking-at quote)
                    (forward-char))
                  (setq beg (point)))))
          ;; Finding the opening quote failed. Maybe we're already at
          ;; the opening quote and should look for the closing instead?
          (error (condition-case nil
                     (progn
                       (viper-backward-char-carefully)
                       (setq beg (point))
                       (setq end (scan-sexps beg 1))
                       (unless (looking-back quote)
                         (re-search-forward regexp)
                         (unless (looking-back quote)
                           (backward-char))
                         (setq end (point))))
                   (error (setq end beg))))))
      (if include-quotes
          (list beg end)
        (list (min (1+ beg) end) (max (1- end) beg))))))

;;; Text object definitions

(vimpulse-define-text-object vimpulse-line (arg)
  "Select ARG lines."
  :type 'line
  (setq arg (1- arg))
  (vimpulse-line-range
   (point)
   (save-excursion
     (when (> arg 0)
       (viper-next-line-carefully arg))
     (point))))

(vimpulse-define-text-object vimpulse-a-word (arg)
  "Select a word."
  :keys "aw"
  (vimpulse-an-object-range
   arg
   (lambda (arg)
     (vimpulse-limit (line-beginning-position) (line-end-position)
       (viper-backward-word (cons arg ?r))))
   (lambda (arg)
     (vimpulse-limit (line-beginning-position) (line-end-position)
       (viper-end-of-word (cons arg ?r))))))

(vimpulse-define-text-object vimpulse-inner-word (arg)
  "Select inner word."
  :keys "iw"
  (vimpulse-inner-object-range
   arg
   (lambda (arg)
     (vimpulse-limit (line-beginning-position) (line-end-position)
       (viper-backward-word (cons arg ?r))))
   (lambda (arg)
     (vimpulse-limit (line-beginning-position) (line-end-position)
       (backward-char)
       (viper-end-of-word (cons arg ?r))))))

(vimpulse-define-text-object vimpulse-a-Word (arg)
  "Select a Word."
  :keys "aW"
  (vimpulse-an-object-range
   arg
   (lambda (arg)
     (vimpulse-limit (line-beginning-position) (line-end-position)
       (viper-backward-Word (cons arg ?r))))
   (lambda (arg)
     (vimpulse-limit (line-beginning-position) (line-end-position)
       (viper-end-of-Word (cons arg ?r))))))

(vimpulse-define-text-object vimpulse-inner-Word (arg)
  "Select inner Word."
  :keys "iW"
  (vimpulse-inner-object-range
   arg
   (lambda (arg)
     (vimpulse-limit (line-beginning-position) (line-end-position)
       (viper-backward-Word (cons arg ?r))))
   (lambda (arg)
     (vimpulse-limit (line-beginning-position) (line-end-position)
       (viper-end-of-Word (cons arg ?r))))))

(vimpulse-define-text-object vimpulse-a-sentence (arg)
  "Select a sentence."
  :keys "as"
  (vimpulse-an-object-range
   arg
   (lambda (arg)
     (viper-backward-sentence arg)
     (vimpulse-skip-regexp "[ \f\t\n\r\v]+" 1))
   (lambda (arg)
     (viper-forward-sentence arg)
     (vimpulse-skip-regexp "[ \f\t\n\r\v]+" -1))))

(vimpulse-define-text-object vimpulse-inner-sentence (arg)
  "Select inner sentence."
  :keys "is"
  (vimpulse-inner-object-range
   arg
   (lambda (arg)
     (viper-backward-sentence arg)
     (vimpulse-skip-regexp "[ \f\t\n\r\v]+" 1))
   (lambda (arg)
     (viper-forward-sentence arg)
     (vimpulse-skip-regexp "[ \f\t\n\r\v]+" -1))))

(vimpulse-define-text-object vimpulse-a-paragraph (arg)
  "Select a paragraph."
  :keys "ap"
  (vimpulse-an-object-range
   arg
   (lambda (arg)
     (vimpulse-skip-regexp "[ \f\t\n\r\v]+" -1)
     (viper-backward-paragraph arg)
     (vimpulse-skip-regexp "[ \f\t\n\r\v]+" 1))
   (lambda (arg)
     (vimpulse-skip-regexp "[ \f\t\n\r\v]+" 1)
     (viper-forward-paragraph arg)
     (vimpulse-skip-regexp "[ \f\t\n\r\v]+" -1)) t))

(vimpulse-define-text-object vimpulse-inner-paragraph (arg)
  "Select inner paragraph."
  :keys "ip"
  (vimpulse-inner-object-range
   arg
   (lambda (arg)
     (vimpulse-skip-regexp "[ \f\t\n\r\v]+" -1)
     (viper-backward-paragraph arg)
     (vimpulse-skip-regexp "[ \f\t\n\r\v]+" 1))
   (lambda (arg)
     (vimpulse-skip-regexp "[ \f\t\n\r\v]+" 1)
     (viper-forward-paragraph arg)
     (vimpulse-skip-regexp "[ \f\t\n\r\v]+" -1))))

(vimpulse-define-text-object vimpulse-a-paren (arg)
  "Select a parenthesis."
  :keys '("ab" "a(" "a)")
  (vimpulse-paren-range arg ?\( nil t))

(vimpulse-define-text-object vimpulse-inner-paren (arg)
  "Select inner parenthesis."
  :keys '("ib" "i(" "i)")
  (vimpulse-paren-range arg ?\())

(vimpulse-define-text-object vimpulse-a-bracket (arg)
  "Select a square bracket."
  :keys '("a[" "a]")
  (vimpulse-paren-range arg ?\[ nil t))

(vimpulse-define-text-object vimpulse-inner-bracket (arg)
  "Select inner square bracket."
  :keys '("i[" "i]")
  (vimpulse-paren-range arg ?\[))

(vimpulse-define-text-object vimpulse-a-curly (arg)
  "Select a curly bracket (\"brace\")."
  :keys '("aB" "a{" "a}")
  (vimpulse-paren-range arg ?{ nil t))

(vimpulse-define-text-object vimpulse-inner-curly (arg)
  "Select inner curly bracket (\"brace\")."
  :keys '("iB" "i{" "i}")
  (vimpulse-paren-range arg ?{))

(vimpulse-define-text-object vimpulse-an-angle (arg)
  "Select an angle bracket."
  :keys '("a<" "a>")
  (vimpulse-paren-range arg ?< nil t))

(vimpulse-define-text-object vimpulse-inner-angle (arg)
  "Select inner angle bracket."
  :keys '("i<" "i>")
  (vimpulse-paren-range arg ?<))

(vimpulse-define-text-object vimpulse-a-single-quote (arg)
  "Select a single-quoted expression."
  :keys "a'"
  (vimpulse-quote-range arg ?' t))

(vimpulse-define-text-object vimpulse-inner-single-quote (arg)
  "Select inner single-quoted expression."
  :keys "i'"
  (vimpulse-quote-range arg ?'))

(vimpulse-define-text-object vimpulse-a-double-quote (arg)
  "Select a double-quoted expression."
  :keys "a\""
  (vimpulse-quote-range arg ?\" t))

(vimpulse-define-text-object vimpulse-inner-double-quote (arg)
  "Select inner double-quoted expression."
  :keys "i\""
  (vimpulse-quote-range arg ?\"))

;;;; Visual mode

;; Visual mode is defined as another Viper state, just like vi state,
;; Insert state, Replace state etc. It inherits keybindings from
;; vi state (movement), but defines some bindings of its own
;; on top of that.
;;
;; Text selection in Emacs and Vim differs subtly by that in Vim, the
;; character under the cursor is always included in the selection,
;; while Emacs' region excludes it when point follows mark. Vimpulse
;; solves the problem by "translating" a Visual selection to the
;; equivalent Emacs region when a command is about to be executed.
;; Likewise, a Line selection is translated to an Emacs region of
;; whole lines.
;;
;; This is pretty transparent, except that we don't wish to do any
;; translating when the user is just moving around in the buffer.
;; To that end, the variable `vimpulse-movement-cmds' lists all of
;; Viper's movement commands, so that translation can be postponed
;; until the user executes a non-movement command.
;;
;; Block selections are rectangle compatible. This means Emacs'
;; rectangular commands are applicable on the selection, and you can
;; write your own utilities using the rect.el library. Alternatively,
;; use the `vimpulse-apply-on-block' function.

(vimpulse-define-state visual
  "Visual mode is a flexible and easy way to select text.
To use Visual mode, press v in vi (command) mode. Then use the
motion commands to expand the selection. Press d to delete, c to
change, r to replace, or y to copy. You can use p to paste.
For Line selection, press V instead of v; then you can copy and
paste whole lines. For Block selection, press C-v; now you can
copy and paste the selected rectangle. In Block selection, you
may use I or A to insert or append text before or after the
selection on each line."
  :id "<VIS> "
  :basic-minor-mode 'vimpulse-visual-mode
  :enable '((vimpulse-visual-mode (or vimpulse-visual-mode t))
            (vimpulse-operator-remap-minor-mode nil)
            operator-state
            vi-state)
  (cond
   ((eq new-state 'visual-state)
    (unless (memq vimpulse-visual-mode '(char line block))
      (vimpulse-visual-mode 1)))
   (t
    (vimpulse-visual-mode -1))))

(defgroup vimpulse-visual nil
  "Visual mode for Viper."
  :prefix "vimpulse-visual-"
  :group  'vimpulse)

;; Visual mode comprises three "submodes": characterwise, linewise
;; and blockwise selection. We implement this by setting the mode
;; variable `vimpulse-visual-mode' to either `char', `line'
;; or `block'.
(define-minor-mode vimpulse-visual-mode
  "Toggles Visual mode in Viper."
  :initial-value nil
  :keymap vimpulse-visual-basic-map
  :group 'vimpulse-visual
  (cond
   (vimpulse-visual-mode
    (unless (memq vimpulse-visual-mode '(char line block))
      (vimpulse-visual-activate 'char)))
   (t
    ;; This is executed when we do (vimpulse-visual-mode -1).
    ;; It must run without error even if Visual mode is not active.
    (vimpulse-visual-highlight -1)
    ;; clean up local variables
    (dolist (var vimpulse-visual-local-vars)
      (when (assq var vimpulse-visual-vars-alist)
        (set var (cdr (assq var vimpulse-visual-vars-alist))))
      (when (memq var vimpulse-visual-global-vars)
        (kill-local-variable var)))
    (setq vimpulse-visual-region-expanded nil)
    ;; deactivate mark
    (when vimpulse-visual-vars-alist
      (vimpulse-deactivate-mark t))
    (vimpulse-transient-restore)
    (kill-local-variable 'vimpulse-visual-vars-alist)
    (kill-local-variable 'vimpulse-visual-global-vars)
    ;; if Viper state is not already changed,
    ;; change it to vi (command) state
    (when (eq viper-current-state 'visual-state)
      (cond
       ((eq vimpulse-visual-previous-state 'emacs-state)
        (viper-change-state-to-emacs))
       (t
        (save-excursion (viper-change-state-to-vi)))))
    (kill-local-variable 'vimpulse-visual-previous-state))))

;;; Activation

(eval-and-compile
  (defalias 'viper-deactivate-mark 'vimpulse-deactivate-mark)
  (defalias 'vimpulse-activate-mark 'vimpulse-activate-region))

(defun vimpulse-visual-activate (&optional mode)
  "Activate Visual mode. MODE is `char', `line' or `block'.
May also be used to change the Visual mode."
  (unless (memq vimpulse-visual-mode '(char line block))
    ;; we are activating Visual mode for the first time
    (kill-local-variable 'vimpulse-visual-vars-alist)
    (kill-local-variable 'vimpulse-visual-global-vars)
    (setq vimpulse-visual-previous-state viper-current-state)
    ;; make global variables buffer-local
    (setq vimpulse-visual-vars-alist nil)
    (vimpulse-visual-block-cleanup-whitespace)
    (vimpulse-transient-remember)
    (dolist (var vimpulse-visual-local-vars)
      (when (and (boundp var)
                 (not (assq var vimpulse-visual-vars-alist)))
        ;; remember old value
        (add-to-list 'vimpulse-visual-vars-alist
                     (cons var (eval var))))
      (unless (assoc var (buffer-local-variables))
        (make-local-variable var)
        (add-to-list 'vimpulse-visual-global-vars var)))
    (setq vimpulse-visual-region-expanded nil)
    ;; re-add hooks in case they were cleared
    (add-hook 'pre-command-hook 'vimpulse-visual-pre-command)
    (add-hook 'post-command-hook 'vimpulse-visual-post-command)
    (if (featurep 'xemacs)
        (add-hook 'zmacs-deactivate-region-hook
                  'vimpulse-visual-deactivate-hook)
      (add-hook 'deactivate-mark-hook 'vimpulse-visual-deactivate-hook))
    ;; activate mark at point
    (cond
     ((eq mode 'block)
      (set-mark (point))
      (vimpulse-deactivate-mark t)     ; `set-mark' activates the mark
      (vimpulse-transient-mark -1))
     (t
      (vimpulse-transient-mark 1)
      ;; convert active Emacs region to Visual selection, if any
      (cond
       ((region-active-p)
        (vimpulse-visual-contract-region
         (not viper-ESC-moves-cursor-back)))
       (t
        (vimpulse-activate-mark (point))))
      (vimpulse-visual-highlight))))
  ;; set the Visual mode
  (setq mode (or mode 'char))
  (setq vimpulse-visual-mode mode
        vimpulse-visual-last mode)
  (viper-change-state 'visual-state)
  (viper-restore-cursor-type)           ; use vi cursor
  ;; reactivate mark
  (cond
   ((eq mode 'block)
    (vimpulse-deactivate-mark t)
    (vimpulse-transient-mark -1))
   (t
    (vimpulse-transient-mark 1)
    (vimpulse-activate-mark)))
  (vimpulse-set-visual-dimensions))

(defun vimpulse-visual-toggle (mode)
  "Enable Visual MODE if this is not the current mode.
Otherwise disable Visual mode."
  (if (eq mode vimpulse-visual-mode)
      (vimpulse-visual-mode -1)
    (vimpulse-visual-activate mode)))

(defun vimpulse-visual-activate-char ()
  "Enable Visual Character selection."
  (interactive)
  (let (message-log-max)
    (vimpulse-visual-activate 'char)
    (message "-- VISUAL --")))

(defun vimpulse-visual-activate-line ()
  "Enable Visual Line selection."
  (interactive)
  (let (message-log-max)
    (vimpulse-visual-activate 'line)
    (message "-- VISUAL LINE --")))

(defun vimpulse-visual-activate-block ()
  "Enable Visual Block selection."
  (interactive)
  (let (message-log-max)
    (vimpulse-visual-activate 'block)
    (message "-- VISUAL BLOCK --")))

(defun vimpulse-visual-toggle-char ()
  "Toggle Visual Character selection."
  (interactive)
  (let (message-log-max)
    (vimpulse-visual-toggle 'char)
    (when vimpulse-visual-mode
      (message "-- VISUAL --"))))

(defun vimpulse-visual-toggle-line ()
  "Toggle Visual Line selection."
  (interactive)
  (let (message-log-max)
    (vimpulse-visual-toggle 'line)
    (when vimpulse-visual-mode
      (message "-- VISUAL LINE --"))))

(defun vimpulse-visual-toggle-block ()
  "Toggle Visual Block selection."
  (interactive)
  (let (message-log-max)
    (vimpulse-visual-toggle 'block)
    (when vimpulse-visual-mode
      (message "-- VISUAL BLOCK --"))))

;;; Visualization

(defun vimpulse-deactivate-mark (&optional now)
  "Don't deactivate mark in Visual mode."
  (cond
   ((and vimpulse-visual-mode
         (not (eq vimpulse-visual-mode 'block)))
    nil)
   (t
    (vimpulse-deactivate-region now))))

(defun vimpulse-transient-mark (&optional arg)
  "Enable Transient Mark mode (and Cua mode) if not already enabled.
Enable forcefully with positive ARG. Disable with negative ARG.
Saves the previous state of Transient Mark mode in
`vimpulse-visual-vars-alist', so it can be restored with
`vimpulse-transient-restore'."
  (setq deactivate-mark nil)
  (and (boundp 'mark-active)
       (setq mark-active (region-active-p)))
  (let (deactivate-mark)
    (cond
     ;; disable Transient Mark/Cua
     ((and (integerp arg) (< arg 1))
      (and (fboundp 'cua-mode)
           cua-mode
           (cua-mode -1))
      (and (fboundp 'transient-mark-mode)
           transient-mark-mode
           (transient-mark-mode -1))
      (and (boundp 'zmacs-regions)
           (setq zmacs-regions nil)))
     ;; enable Transient Mark/Cua
     (t
      (vimpulse-transient-remember)
      (cond
       ((and (fboundp 'cua-mode)
             (vimpulse-visual-before (eq cua-mode t))
             (or (not cua-mode) (numberp arg)))
        (cua-mode 1))
       ((and (fboundp 'transient-mark-mode)
             (or (not transient-mark-mode) (numberp arg)))
        (transient-mark-mode 1))
       ((and (boundp 'zmacs-regions)
             (or (not zmacs-regions) (numberp arg)))
        (setq zmacs-regions t)))))))

(defun vimpulse-transient-remember ()
  "Remember Transient Mark mode state in `vimpulse-visual-vars-alist'."
  (when (and (boundp 'transient-mark-mode)
             (not (assq 'transient-mark-mode
                        vimpulse-visual-vars-alist)))
    (add-to-list 'vimpulse-visual-vars-alist
                 (cons 'transient-mark-mode
                       (when (eq transient-mark-mode t)
                         transient-mark-mode))))
  (when (and (boundp 'cua-mode)
             (not (assq 'cua-mode vimpulse-visual-vars-alist)))
    (add-to-list 'vimpulse-visual-vars-alist
                 (cons 'cua-mode cua-mode))))

(defun vimpulse-transient-restore ()
  "Restore Transient Mark mode to what is was before Visual mode.
 Also restores Cua mode."
  (when vimpulse-visual-vars-alist
    (when (boundp 'transient-mark-mode)
      (if (vimpulse-visual-before transient-mark-mode)
          (transient-mark-mode 1)
        (transient-mark-mode -1)))
    (when (boundp 'cua-mode)
      ;; prevent Cua mode from setting `deactivate-mark' to t
      (let (deactivate-mark)
        (if (vimpulse-visual-before cua-mode)
            (cua-mode 1)
          (cua-mode -1))))
    (when (boundp 'zmacs-regions)
      (let ((oldval (vimpulse-visual-before zmacs-regions)))
        (setq zmacs-regions oldval)))))

;; should be replaced with something more readable,
;; like (vimpulse-visual-historical-value 'transient-mark-mode)
(defmacro vimpulse-visual-before (&rest body)
  "Evaluate BODY with original system values from before Visual mode.
This is based on `vimpulse-visual-vars-alist'."
  ;; this needs to be expanded at runtime, obviously
  `(eval `(let ,(mapcar (lambda (elt)
                          `(,(car elt) (quote ,(cdr elt))))
                        vimpulse-visual-vars-alist)
            ,',@body)))

(defun vimpulse-visual-beginning (&optional mode force)
  "Return beginning of Visual selection.
See `vimpulse-visual-range'."
  (vimpulse-range-beginning (vimpulse-visual-range mode force)))

(defun vimpulse-visual-end (&optional mode force)
  "Return end of Visual selection.
See `vimpulse-visual-range'."
  (vimpulse-range-end (vimpulse-visual-range mode force)))

(defun vimpulse-visual-range (&optional mode force)
  "Return a Visual motion range (TYPE BEG END).
TYPE is the Visual mode.

The range depends on `point', `mark' and `vimpulse-visual-mode'.
The Visual mode may be specified explicitly with MODE, which must
be one of `char', `line' and `block'.

In Character mode, returns region plus one character.
In Line mode, returns region as whole lines.
In Block mode, returns rectangle plus one column.

If the Visual selection is already translated to Emacs' region,
returns the region as-is. This can be overridden with FORCE.

See also `vimpulse-visual-beginning' and `vimpulse-visual-end'."
  (let ((mark  (or (mark t) 1))
        (point (point)))
    (setq mode (or mode vimpulse-visual-mode))
    (unless (memq mode '(line block))
      (setq mode (if vimpulse-visual-mode 'inclusive 'exclusive)))
    (cond
     ((and (not force)
           (or (not vimpulse-visual-mode)
               vimpulse-visual-region-expanded))
      (vimpulse-make-motion-range mark point mode))
     ((eq mode 'block)
      (vimpulse-block-range mark point))
     ((eq mode 'line)
      (vimpulse-line-range mark point))
     (t
      (vimpulse-inclusive-range mark point)))))

(defun vimpulse-visual-select (beg end &optional widen)
  "Visually select text inclusively from BEG to END.
Return nil if selection is unchanged. If WIDEN is non-nil, only
modify selection if it does not already encompass BEG and END.

Under the hood, this function changes Emacs' `point' and `mark'.
The boundaries of the Visual selection are deduced from these and
the current Visual mode via `vimpulse-visual-beginning' and
`vimpulse-visual-end'."
  (cond
   ;; in Visual mode, protect the value of `mark-active'
   (vimpulse-visual-mode
    (let (mark-active)
      (vimpulse-set-region
       (min beg end)
       (if vimpulse-visual-region-expanded
           (max beg end)
         (max (min beg end) (1- (max beg end))))
       widen)))
   (t
    (vimpulse-set-region
     (min beg end) (max beg end) widen))))

;;; Functions for Visual selection <=> Emacs region transformation

;; In Vim, Visual-mode selection always includes the character position under
;; the cursor (i.e., "at point" or "following point" in Emacs-speak), so the
;; former is invariably larger than the latter -- thus "expand" and "contract".
(defun vimpulse-visual-expand-region (&optional mode no-trailing-newline)
  "Transform the current Emacs region to the equivalent Visual selection.
If NO-TRAILING-NEWLINE is t and the selection ends with a newline,
exclude that newline from the region.
Cf. `vimpulse-visual-contract-region' for the reverse operation."
  (let* ((range (vimpulse-visual-range mode))
         (type  (vimpulse-motion-type range))
         (beg   (vimpulse-range-beginning range))
         (end   (vimpulse-range-end range))
         mark-active)
    (when no-trailing-newline
      (save-excursion
        (goto-char end)
        (when (and (bolp) (not (bobp)))
          (setq range (vimpulse-make-motion-range
                       beg (max beg (1- (point))) type)))))
    (setq vimpulse-visual-region-expanded t)
    (vimpulse-mark-range range)))

(defun vimpulse-visual-contract-region (&optional keep-point)
  "Transform the current Visual selection to the equivalent Emacs region.
If KEEP-POINT is t, do not move point (transformation may be incomplete
if mark < point).
Return nil if selection is unchanged.
Cf. `vimpulse-visual-expand-region' for the reverse operation."
  (let ((opoint (point)) (omark (mark t)))
    (setq vimpulse-visual-region-expanded nil)
    (vimpulse-visual-select (region-beginning) (region-end))
    ;; KEEP-POINT?
    (when keep-point
      (goto-char opoint))
    ;; Was selection changed?
    (not (and (= (point)  opoint)
              (= (mark t) omark)))))

;; While there is a one-to-one relationship between Vim-like, "inclusive"
;; selections and Emacs-like, "exclusive" regions, line selection is a
;; one-way operation -- multiple selections can produce the same number
;; of lines. Line "contraction" is therefore based on memory.
(defun vimpulse-visual-restore ()
  "Restore previous selection.
This selects a specific range of text in the buffer.
See also `vimpulse-visual-reselect'."
  (interactive)
  (setq vimpulse-visual-region-expanded nil)
  (let ((last vimpulse-visual-last))
    (cond
     ;; if no previous selection, try a quick C-x C-x
     ((or (not vimpulse-visual-point)
          (not vimpulse-visual-mark))
      (vimpulse-activate-mark nil)
      (vimpulse-visual-mode 1))
     (t
      (unless vimpulse-visual-mode
        ;; protect the previous values of `vimpulse-visual-mark'
        ;; and `vimpulse-visual-point'
        (let (vimpulse-visual-mark vimpulse-visual-point)
          (cond
           ((eq last 'line)
            (vimpulse-visual-activate-line))
           ((eq last 'block)
            (vimpulse-visual-activate-block))
           (t                           ; char
            (vimpulse-visual-activate-char)))))
      (set-mark vimpulse-visual-mark)
      (goto-char vimpulse-visual-point)
      (unless (save-excursion
                (goto-char (max vimpulse-visual-mark
                                vimpulse-visual-point))
                (bolp))
        (vimpulse-visual-contract-region))
      (vimpulse-visual-highlight)))))

(defun vimpulse-visual-reselect (&optional mode height width pos)
  "Create a Visual MODE selection of dimensions HEIGHT and WIDTH.
When called interactively, uses dimensions of previous selection.
If specified, selects about POS; otherwise selects about point.
See also `vimpulse-visual-restore'."
  (interactive)
  (when pos
    (goto-char pos))
  (setq mode (or mode vimpulse-visual-mode vimpulse-visual-last)
        height (or height vimpulse-visual-height 1)
        width (or width vimpulse-visual-width 1))
  (unless vimpulse-visual-mode
    (vimpulse-visual-activate mode))
  (cond
   ((eq mode 'block)
    (viper-next-line-carefully (1- height))
    (setq width (+ (1- width) (current-column)))
    (vimpulse-move-to-column width)
    (setq height (count-lines (vimpulse-visual-beginning mode)
                              (vimpulse-visual-end mode)))
    (while (and (not (eq (current-column) width))
                (> height 1))
      (viper-next-line-carefully -1)
      (setq height (1- height))
      (move-to-column width)))
   ((eq mode 'line)
    (viper-next-line-carefully (1- height)))
   (t                                   ; char
    (viper-forward-char-carefully (1- width)))))

(defun vimpulse-set-visual-markers (&optional point mark)
  "Refresh `vimpulse-visual-point' and `vimpulse-visual-mark'."
  (setq mark  (vimpulse-visual-beginning 'char)
        point (vimpulse-visual-end 'char))
  (when (< (point) (mark t))
    (setq mark (prog1 point
                 (setq point mark))))
  (viper-move-marker-locally 'vimpulse-visual-point point)
  (viper-move-marker-locally 'vimpulse-visual-mark  mark)
  (set-marker-insertion-type vimpulse-visual-point
                             (<= point mark))
  (set-marker-insertion-type vimpulse-visual-mark
                             (> point mark)))

(defun vimpulse-set-visual-dimensions (&optional beg end mode)
  "Refresh `vimpulse-visual-height' and `vimpulse-visual-width'."
  (vimpulse-set-visual-markers beg end)
  (let* ((range (vimpulse-visual-range mode))
         (beg   (or beg (vimpulse-range-beginning range)))
         (end   (or end (vimpulse-range-end range)))
         (type  (vimpulse-motion-type range))
         (range (vimpulse-make-motion-range beg end type)))
    (setq vimpulse-visual-height (vimpulse-range-height range t)
          vimpulse-visual-width  (vimpulse-range-width range t))))

(defun vimpulse-visual-highlight (&optional arg)
  "Highlight Visual selection, depending on region and Visual mode.
With negative ARG, removes highlighting."
  (cond
   ((and (numberp arg) (< arg 1))
    (when (viper-overlay-live-p vimpulse-visual-overlay)
      (vimpulse-delete-overlay vimpulse-visual-overlay))
    (mapc 'vimpulse-delete-overlay vimpulse-visual-block-overlays)
    (setq vimpulse-visual-block-overlays nil)
    ;; clean up unreferenced overlays
    (dolist (overlay (vimpulse-overlays-at (point)))
      (when (eq (viper-overlay-get overlay 'face) (vimpulse-region-face))
        (vimpulse-delete-overlay overlay))))
   ((eq vimpulse-visual-mode 'block)
    ;; remove any char/line highlighting
    (when (viper-overlay-live-p vimpulse-visual-overlay)
      (vimpulse-delete-overlay vimpulse-visual-overlay))
    ;; block highlighting isn't perfect
    (condition-case nil
        (vimpulse-visual-highlight-block
         (vimpulse-visual-beginning)
         (vimpulse-visual-end))
      (error nil)))
   (vimpulse-visual-mode                ; char or line
    (let ((beg (vimpulse-visual-beginning))
          (end (vimpulse-visual-end)))
      ;; remove any block highlighting
      (mapc 'vimpulse-delete-overlay vimpulse-visual-block-overlays)
      (setq vimpulse-visual-block-overlays nil)
      ;; reuse overlay if possible
      (if (viper-overlay-live-p vimpulse-visual-overlay)
          (viper-move-overlay vimpulse-visual-overlay beg end)
        (setq vimpulse-visual-overlay
              (vimpulse-make-overlay beg end nil t))
        (viper-overlay-put vimpulse-visual-overlay
                           'face (vimpulse-region-face))
        (viper-overlay-put vimpulse-visual-overlay
                           'priority 99))))))

(defun vimpulse-visual-highlight-block (beg end)
  "Highlight rectangular region from BEG to END.
We do this by putting an overlay on each line within the
rectangle. Each overlay extends across all the columns of the
rectangle. We try to reuse overlays where possible because this
is more efficient and results in less flicker.

Adapted from: `rm-highlight-rectangle' in rect-mark.el."
  (let ((opoint (point))                ; remember point
        (omark  (mark t))               ; remember mark
        (old vimpulse-visual-block-overlays)
        beg-col end-col new nlines overlay window-beg window-end)
    ;; Calculate the rectangular region represented by BEG and END,
    ;; but put BEG in the north-west corner and END in the south-east
    ;; corner if not already there.
    (save-excursion
      (setq beg-col (save-excursion (goto-char beg)
                                    (current-column))
            end-col (save-excursion (goto-char end)
                                    (current-column)))
      (when (>= beg-col end-col)
        (if (= beg-col end-col)
            (setq end-col (1+ end-col))
          (setq beg-col (prog1 end-col
                          (setq end-col beg-col))))
        (setq beg (save-excursion (goto-char beg)
                                  (vimpulse-move-to-column beg-col)
                                  (point))
              end (save-excursion (goto-char end)
                                  (vimpulse-move-to-column end-col 1)
                                  (point))))
      ;; force a redisplay so we can do reliable
      ;; windows BEG/END calculations
      (sit-for 0)
      (setq window-beg (max (window-start) beg)
            window-end (min (window-end) (1+ end))
            nlines (count-lines window-beg
                                (min window-end (point-max))))
      ;; iterate over those lines of the rectangle which are
      ;; visible in the currently selected window
      (goto-char window-beg)
      (dotimes (i nlines)
        (let (row-beg row-end bstring astring)
          ;; beginning of row
          (vimpulse-move-to-column beg-col)
          (when (< (current-column) beg-col)
            ;; prepend overlay with virtual spaces if we are unable to
            ;; move directly to the first column
            (setq bstring
                  (propertize
                   (make-string
                    (- beg-col (current-column)) ?\ )
                   'face
                   (or (get-text-property (1- (point)) 'face)
                       'default))))
          (setq row-beg (point))
          ;; end of row
          (vimpulse-move-to-column end-col)
          (when (< (current-column) end-col)
            ;; append overlay with virtual spaces if we are unable to
            ;; move directly to the last column
            (setq astring
                  (propertize
                   (make-string
                    (if (= (point) row-beg)
                        (- end-col beg-col)
                      (- end-col (current-column)))
                    ?\ ) 'face (vimpulse-region-face)))
            ;; place cursor on one of the virtual spaces
            ;; (only works in GNU Emacs)
            (if (= opoint row-beg)
                (put-text-property
                 0 (min (length astring) 1)
                 'cursor t astring)
              (put-text-property
               (max 0 (1- (length astring))) (length astring)
               'cursor t astring)))
          (setq row-end (min (point) (line-end-position)))
          ;; XEmacs bug: zero-length extents display
          ;; end-glyph before start-glyph
          (and (featurep 'xemacs)
               bstring astring
               (= row-beg row-end)
               (setq bstring (prog1 astring
                               (setq astring bstring))))
          ;; trim old leading overlays
          (while (and old
                      (setq overlay (car old))
                      (< (viper-overlay-start overlay) row-beg)
                      (/= (viper-overlay-end overlay) row-end))
            (vimpulse-delete-overlay overlay)
            (setq old (cdr old)))
          ;; reuse an overlay if possible, otherwise create one
          (cond
           ((and old (setq overlay (car old))
                 (or (= (viper-overlay-start overlay) row-beg)
                     (= (viper-overlay-end overlay) row-end)))
            (viper-move-overlay overlay row-beg row-end)
            (vimpulse-overlay-before-string overlay bstring)
            (vimpulse-overlay-after-string overlay astring)
            (setq new (cons overlay new)
                  old (cdr old)))
           (t
            (setq overlay (vimpulse-make-overlay row-beg row-end))
            (vimpulse-overlay-before-string overlay bstring)
            (vimpulse-overlay-after-string overlay astring)
            (viper-overlay-put overlay 'face (vimpulse-region-face))
            (viper-overlay-put overlay 'priority 99)
            (setq new (cons overlay new)))))
        (forward-line 1))
      ;; trim old trailing overlays
      (mapc 'vimpulse-delete-overlay old)
      (setq vimpulse-visual-block-overlays (nreverse new)))))

(defun vimpulse-visual-pre-command ()
  "Run before each command in Visual mode."
  (when vimpulse-visual-mode
    ;; refresh Visual restore markers and marks
    (vimpulse-set-visual-dimensions)
    (cond
     ;; movement command: don't expand region
     ((vimpulse-movement-cmd-p this-command)
      (setq vimpulse-visual-region-expanded nil))
     (t
      ;; add whitespace if necessary for making a rectangle
      (and (eq vimpulse-visual-mode 'block)
           (vimpulse-visual-block-add-whitespace))
      (vimpulse-visual-expand-region
       ;; if in Line mode, don't include trailing newline
       ;; unless the command has real need of it
       nil (and (eq vimpulse-visual-mode 'line)
                (not (vimpulse-needs-newline-p this-command))))))))

(defun vimpulse-visual-post-command ()
  "Run after each command in Visual mode."
  (cond
   (vimpulse-visual-mode
    ;; quitting: exit to vi (command) mode
    (cond
     (quit-flag                         ; C-g
      (vimpulse-visual-mode -1))
     ((eq this-command 'keyboard-quit)
      (vimpulse-visual-mode -1))
     ((and (not (region-active-p))
           (not (eq vimpulse-visual-mode 'block)))
      (vimpulse-visual-mode -1))
     ;; region was expanded, so contract it
     (vimpulse-visual-region-expanded
      (when (eq vimpulse-visual-mode 'block)
        (vimpulse-visual-block-cleanup-whitespace))
      (if (eq vimpulse-visual-mode 'line)
          (vimpulse-visual-restore)
        (vimpulse-visual-contract-region))
      (vimpulse-visual-highlight))
     (t
      (vimpulse-visual-highlight))))
   ;; Not in the Visual state, but maybe the mark
   ;; was activated in vi (command) state?
   ((and (region-active-p)
         (eq viper-current-state 'vi-state)
         (if (boundp 'deactivate-mark) (not deactivate-mark) t))
    (vimpulse-visual-mode 1))))

(defun vimpulse-visual-deactivate-hook ()
  "Hook run when mark is deactivated in Visual mode."
  (when vimpulse-visual-mode
    (and (not (region-active-p))
         (not (vimpulse-movement-cmd-p this-command))
         (vimpulse-visual-mode -1))))

(add-hook 'pre-command-hook 'vimpulse-visual-pre-command)
(add-hook 'post-command-hook 'vimpulse-visual-post-command)
(if (featurep 'xemacs)
    (add-hook 'zmacs-deactivate-region-hook
              'vimpulse-visual-deactivate-hook)
  (add-hook 'deactivate-mark-hook 'vimpulse-visual-deactivate-hook))

;; advise viper-intercept-ESC-key to exit Visual mode with ESC
(defadvice viper-intercept-ESC-key
  (around vimpulse-ESC-exit-visual-mode activate)
  "Exit Visual mode with ESC."
  (let ((viper-ESC-moves-cursor-back (unless (region-active-p)
                                       viper-ESC-moves-cursor-back))
        deactivate-mark)
    (if (and vimpulse-visual-mode
             (not (input-pending-p)))
        (vimpulse-visual-mode -1)
      ad-do-it)))

(defadvice viper-Put-back (around vimpulse-visual activate)
  "Delete selection before pasting in Visual mode."
  (let (inserted-text replaced-text mode)
    (setq yank-window-start (window-start))
    (cond
     (vimpulse-visual-mode
      (setq mode vimpulse-visual-mode)
      (unless (eq mode 'block)
        ;; add replaced text to the kill-ring before the current kill
        (setq inserted-text (current-kill 0))
        (setq replaced-text
              (buffer-substring (region-beginning) (region-end)))
        (kill-new replaced-text t)
        (kill-new inserted-text))
      (vimpulse-delete (region-beginning) (region-end) t)
      (when (and (eq mode 'char)
                 (not (bolp))
                 (viper-end-with-a-newline-p inserted-text))
        (newline))
      (when (and (eq mode 'line)
                 (not (viper-end-with-a-newline-p inserted-text)))
        (save-excursion (newline))))
     ((region-active-p)
      (delete-region (region-beginning) (region-end))))
    (if (and killed-rectangle
             kill-ring
             (eq (get 'killed-rectangle 'previous-kill)
                 (current-kill 0)))
        (save-excursion
          (yank-rectangle))
      ad-do-it)
    (when vimpulse-visual-mode
      (vimpulse-visual-mode -1))))

(defadvice viper-put-back (around vimpulse-visual activate)
  "Delete selection before pasting in Visual mode."
  (setq yank-window-start (window-start))
  (cond
   (vimpulse-visual-mode
    (viper-Put-back arg))
   ((region-active-p)
    (viper-Put-back arg))
   ((and killed-rectangle
         kill-ring
         (eq (get 'killed-rectangle 'previous-kill)
             (current-kill 0)))
    (unless (eolp)
      (viper-forward-char-carefully))
    (save-excursion
      (yank-rectangle)))
   (t
    ad-do-it))
  (when vimpulse-visual-mode
    (vimpulse-visual-mode -1)))

;; Viper's larger movement commands use the mark to store the previous
;; position, which is fine and useful when the mark isn't active. When
;; it is, however, it has the effect of remaking the region.
(defadvice push-mark (around vimpulse-visual-mode activate)
  (unless (and vimpulse-visual-mode
               ;; Note: if you really need to call `push-mark'
               ;; in proximity with these commands (e.g., in a hook),
               ;; do (let (this-command) (push-mark)).
               (memq this-command
                     '(vimpulse-goto-first-line
                       vimpulse-goto-line
                       viper-backward-paragraph
                       viper-backward-sentence
                       viper-forward-paragraph
                       viper-forward-sentence
                       viper-goto-line
                       viper-search-next
                       viper-search-Next
                       viper-window-bottom
                       viper-window-middle
                       viper-window-top)))
    ad-do-it))

;; block selection disables Transient Mark mode
(defadvice deactivate-mark (after vimpulse-visual activate)
  "Deactivate Visual Block mode."
  (when (eq vimpulse-visual-mode 'block)
    (vimpulse-visual-mode -1)))

(defmacro vimpulse-visual-mouse-advice (cmd)
  "Advise mouse command CMD to enable Visual mode."
  `(defadvice ,cmd (around vimpulse-visual activate)
     "Enable Visual mode in vi (command) state."
     (let ((w (posn-window (event-start (ad-get-arg 0)))))
       (cond
        ;; if Visual mode is enabled in the window clicked in,
        ;; adjust region afterwards
        ((with-selected-window w
           vimpulse-visual-mode)
         (vimpulse-visual-highlight -1)
         ad-do-it
         (when (eq (selected-window) w)
           (vimpulse-visual-contract-region t)
           (vimpulse-visual-highlight)))
        ;; otherwise, if in vi (command) state, enable Visual mode
        ((with-selected-window w
           (eq viper-current-state 'vi-state))
         ad-do-it
         (when (eq (selected-window) w)
           (cond
            (vimpulse-visual-mode
             (vimpulse-visual-contract-region t))
            ((region-active-p)
             (vimpulse-visual-mode 1)
             (setq vimpulse-visual-region-expanded nil)
             (vimpulse-visual-contract-region t)))))
        (t
         ad-do-it)))))

(vimpulse-visual-mouse-advice mouse-drag-region)
(vimpulse-visual-mouse-advice mouse-save-then-kill)

(defadvice mouse-show-mark (before vimpulse-visual activate)
  "Refresh highlighting of Visual selection."
  (when vimpulse-visual-mode
    (vimpulse-visual-highlight)))

(defun vimpulse-movement-cmd-p (command)
  "Whether COMMAND is a \"movement\" command.
That is, whether it is listed in `vimpulse-movement-cmds'."
  ;; we use `member' rather than `memq' to allow lambdas
  (member command vimpulse-movement-cmds))

(defun vimpulse-needs-newline-p (command)
  "Whether COMMAND needs trailing newline in Visual Line mode.
In most cases (say, when wrapping the selection in a skeleton),
it is more useful to exclude the last newline from the region."
  (or (member command vimpulse-newline-cmds)
      (vimpulse-operator-cmd-p command)))

(defun vimpulse-visual-remap (from to)
  "Remap FROM to TO in Visual mode."
  (vimpulse-remap vimpulse-visual-basic-map from to))

;;; Ex

(defun vimpulse-visual-ex (arg)
  "Call `viper-ex' on region."
  (interactive "p")
  (viper-ex arg))

;;; Insert/append

(defun vimpulse-visual-insert (beg end &optional arg)
  "Enter Insert state at beginning of Visual selection."
  (interactive "r\nP")
  (let (deactivate-mark)
    (cond
     ((eq vimpulse-visual-mode 'block)
      (vimpulse-visual-block-rotate 'upper-left beg end)
      (setq beg (vimpulse-visual-beginning)
            end (vimpulse-visual-end))
      (vimpulse-visual-mode -1)
      (goto-char
       (vimpulse-visual-create-coords 'block ?i beg end))
      (viper-insert arg))
     (t
      (vimpulse-visual-mode -1)
      (push-mark end t t)
      (goto-char beg)
      (viper-insert arg))
     (t
      (error "Not in Visual mode")))))

(defun vimpulse-visual-append (beg end &optional arg)
  "Enter Insert state at end of Visual selection."
  (interactive "r\nP")
  (let (deactivate-mark)
    (cond
     ((eq vimpulse-visual-mode 'block)
      (vimpulse-visual-block-rotate 'upper-left beg end)
      (setq beg (vimpulse-visual-beginning)
            end (vimpulse-visual-end))
      (setq vimpulse-visual-whitespace-overlay nil)
      (vimpulse-visual-mode -1)
      (goto-char
       (vimpulse-visual-create-coords 'block ?a beg end))
      (viper-append arg))
     (t
      (vimpulse-visual-mode -1)
      (push-mark beg t t)
      (goto-char end)
      (viper-insert arg))
     (t
      (error "Not in Visual mode")))))

;;; Block selection

(defun vimpulse-apply-on-block (func &optional beg end &rest args)
  "Call FUNC for each line of Visual Block selection.
The selection may be specified explicitly with BEG and END.
FUNC must take at least two arguments, the beginning and end of
each line. Extra arguments to FUNC may be passed via ARGS."
  (let (beg-col end-col)
    (save-excursion
      (setq beg (or beg (vimpulse-visual-beginning))
            end (or end (vimpulse-visual-end)))
      ;; ensure BEG < END
      (setq beg (prog1 (min beg end)
                  (setq end (max beg end))))
      ;; calculate columns
      (goto-char end)
      (setq end-col (current-column))
      (goto-char beg)
      (setq beg-col (current-column))
      ;; ensure BEG-COL < END-COL
      (when (> beg-col end-col)
        (setq beg-col (prog1 end-col
                        (setq end-col beg-col)))
        (setq end (save-excursion
                    (goto-char end)
                    (move-to-column end-col)
                    (point))))
      ;; apply FUNC on each line
      (while (< (point) end)
        (apply func
               (save-excursion
                 (move-to-column beg-col)
                 (point))
               (save-excursion
                 (move-to-column end-col)
                 (point))
               args)
        (forward-line 1)))))

(defun vimpulse-visual-block-position (corner &optional beg end)
  "Return position of Visual Block CORNER.
CORNER may be one of `upper-left', `upper-right', `lower-left'
and `lower-right', or a clockwise number from 0 to 3:

        0---1        upper-left +---+ upper-right
        |   |                   |   |
        3---2        lower-left +---+ lower-right

The rectangle is defined by mark and point, or BEG and END
if specified. The CORNER values `upper', `left', `lower'
and `right' return one of the defining corners.

        upper P---+                    +---M upper
         left |   | lower        lower |   | right
              +---M right         left P---+

Corners 0 and 3 are returned by their left side, corners 1 and 2
by their right side. To place point in one of the corners, use
`vimpulse-visual-block-rotate'.

To go the other way, use `vimpulse-visual-block-corner'."
  (save-excursion
    (setq beg (or beg (vimpulse-visual-beginning 'block))
          end (or end (vimpulse-visual-end 'block)))
    (when (> beg end) (setq beg (prog1 end (setq end beg))))
    (let ((beg-col (progn (goto-char beg)
                          (current-column)))
          (end-col (progn (goto-char end)
                          (current-column)))
          (upper beg) (left beg) (lower end) (right end)
          (upper-left 0) (upper-right 1)
          (lower-left 3) (lower-right 2))
      (when (> beg-col end-col)
        (setq beg-col (prog1 end-col
                        (setq end-col beg-col)))
        (setq left (prog1 right
                     (setq right left))))
      (if (memq corner '(upper left lower right))
          (eval corner)
        (setq corner (mod (eval corner) 4))
        (if (memq corner '(0 1))
            (goto-char beg)
          (goto-char end))
        (if (memq corner '(0 3))
            (vimpulse-move-to-column beg-col)
          (vimpulse-move-to-column end-col))
        (point)))))

(defun vimpulse-visual-block-corner (&optional symbolic pos)
  "Return the current Visual Block corner as a number from 0 to 3.
Corners are numbered clockwise, starting with the upper-left corner.
Return as one of `upper-left', `upper-right', `lower-left' and
`lower-right' if SYMBOLIC is non-nil.

        0---1        upper-left +---+ upper-right
        |   |                   |   |
        3---2        lower-left +---+ lower-right

Specify POS to compare that position, rather than point,
against the corners. The result can be passed to functions
like `vimpulse-visual-block-position' and
`vimpulse-visual-block-rotate'."
  (let ((upper-left 0)
        (upper-right 1)
        (lower-left 3)
        (lower-right 2)
        corner)
    (setq pos (or pos (point)))
    (or (dolist (i '(upper-left lower-left) corner)
          (when (eq (vimpulse-visual-block-position i) pos)
            (setq corner i)))
        (progn
          (unless vimpulse-visual-region-expanded
            (setq pos (1+ pos)))
          (dolist (i '(upper-right lower-right) corner)
            (when (eq (vimpulse-visual-block-position i) pos)
              (setq corner i)))))
    (if symbolic
        corner
      (eval corner))))

(defun vimpulse-visual-block-rotate (corner &optional beg end)
  "In Visual Block selection, rotate point and mark clockwise.
When called non-interactively, CORNER specifies the corner to
place point in; mark is placed in the opposite corner.

        0---1        upper-left +---+ upper-right
        |   |                   |   |
        3---2        lower-left +---+ lower-right

Corners are numbered clockwise from 0. For better readability,
you may use the symbolic values `upper-left', `upper-right',
`lower-left' and `lower-right'.

This function updates `vimpulse-visual-point' and
`vimpulse-visual-mark' so that \\[vimpulse-visual-restore]
restores the selection with the same rotation."
  (interactive
   (list (if (< (prefix-numeric-value current-prefix-arg) 0)
             (1- (vimpulse-visual-block-corner))
           (1+ (vimpulse-visual-block-corner)))))
  (let ((upper-left 0) (upper-right 1) (lower-left 3) (lower-right 2)
        newmark newpoint newmark-marker newpoint-marker mark-active)
    (setq corner (mod (eval corner) 4))
    (setq newpoint (vimpulse-visual-block-position corner beg end))
    (setq newmark (vimpulse-visual-block-position
                   (mod (+ 2 corner) 4) beg end))
    (if (memq corner '(0 3))
        (setq newmark-marker (1- newmark)
              newpoint-marker newpoint)
      (setq newpoint-marker (1- newpoint)
            newmark-marker newmark))
    (unless vimpulse-visual-region-expanded
      (setq newpoint newpoint-marker
            newmark  newmark-marker))
    (set-mark newmark)
    (goto-char newpoint)
    (vimpulse-set-visual-dimensions beg end 'block)))

(defun vimpulse-visual-exchange-corners ()
  "Rearrange corners in Visual Block mode.

        M---+          +---M
        |   |    =>    |   |
        +---P          P---+

For example, if mark is in the upper left corner and point
in the lower right (see fig.), this function puts mark in
the upper right corner and point in the lower left."
  (interactive)
  (cond
   ((memq vimpulse-visual-mode '(char line))
    (exchange-point-and-mark))
   ((eq vimpulse-visual-mode 'block)
    (let ((mark-col (save-excursion
                      (goto-char (mark t))
                      (forward-char)
                      (1- (current-column))))
          (point-col (current-column)))
      (set-mark (save-excursion
                  (goto-char (mark t))
                  (vimpulse-move-to-column
                   point-col (< (current-column) point-col))
                  (point)))
      (vimpulse-move-to-column
       mark-col (< (current-column) mark-col))
      (and (eolp) (not (bolp)) (backward-char))))
   (t
    (error "Not in Visual mode"))))

;; Insert whitespace into buffer to handle zero-width rectangles.
;; This isn't ideal and should be replaced with something else.
(defun vimpulse-visual-block-add-whitespace ()
  "Ensure rectangle is at least one column wide.
If the Block selection starts and ends on blank lines, the
resulting rectangle has width zero even if intermediate lines
contain characters. This function inserts a space after mark
so that a one-column rectangle can be made. The position of the
space is stored in `vimpulse-visual-whitespace-overlay' so it can be
removed afterwards with `vimpulse-visual-block-cleanup-whitespace'."
  (save-excursion
    (when (and (eq vimpulse-visual-mode 'block)
               (/= (vimpulse-visual-beginning)
                   (vimpulse-visual-end))
               (save-excursion
                 (goto-char (vimpulse-visual-beginning))
                 (and (bolp) (eolp)))
               (save-excursion
                 (goto-char (vimpulse-visual-end))
                 (and (bolp) (eolp))))
      (goto-char (mark t))
      (insert " ")
      (setq vimpulse-visual-whitespace-overlay
            (vimpulse-make-overlay (mark t) (1+ (mark t))
                                   nil t nil)))))

(defun vimpulse-visual-block-cleanup-whitespace ()
  "Clean up whitespace inserted by `vimpulse-visual-block-add-whitespace'."
  (when (viper-overlay-live-p vimpulse-visual-whitespace-overlay)
    (when (= (- (viper-overlay-end
                 vimpulse-visual-whitespace-overlay)
                (viper-overlay-start
                 vimpulse-visual-whitespace-overlay))
             1)
      (delete-region
       (viper-overlay-start vimpulse-visual-whitespace-overlay)
       (viper-overlay-end   vimpulse-visual-whitespace-overlay)))
    (vimpulse-delete-overlay vimpulse-visual-whitespace-overlay)
    (setq vimpulse-visual-whitespace-overlay nil)))

(defun vimpulse-visual-create-coords
  (mode i-com upper-left lower-right)
  "Update the list of block insert coordinates with current rectangle.
I-COM should be ?c, ?i, ?a, ?I or ?A; the column for the
insertion will be chosen according to this command.
Returns the insertion point."
  (setq vimpulse-visual-insert-coords nil)
  (let ((nlines (count-lines upper-left lower-right))
        (col 0))                 ; for ?I and ?A, trivial: column is 0
    (when (memq i-com '(?a ?c ?i))
      ;; for ?i and ?a, choose the left (the right) rectangle column
      (let ((beg-col (save-excursion
                       (goto-char upper-left)
                       (current-column)))
            (end-col (save-excursion
                       (goto-char lower-right)
                       (current-column))))
        ;; decide if we use the left or right column
        (setq col (max 0 (if (memq i-com '(?c ?i))
                             beg-col
                           (1- end-col))))))
    ;; save the information
    (setq vimpulse-visual-insert-coords
          (list mode i-com upper-left col nlines))
    (save-excursion
      (goto-char upper-left)
      (vimpulse-move-to-column col)
      (point))))

;; Redefinitions of Viper functions to handle Visual block selection,
;; that is, the "update all lines when we hit ESC" part.
;; This function is not in viper-functions-redefinitions.el
;; because its code is closely related to Visual mode.
(defun vimpulse-exit-insert-state ()
  (interactive)
  (viper-move-marker-locally 'vimpulse-exit-point (point))
  (viper-change-state-to-vi)
  (when vimpulse-visual-insert-coords
    ;; get the saved info about the Visual selection
    (let ((mode   (nth 0 vimpulse-visual-insert-coords))
          (i-com  (nth 1 vimpulse-visual-insert-coords))
          (pos    (nth 2 vimpulse-visual-insert-coords))
          (col    (nth 3 vimpulse-visual-insert-coords))
          (nlines (nth 4 vimpulse-visual-insert-coords)))
      (goto-char pos)
      (save-excursion
        (dotimes (i (1- nlines))
          (forward-line 1)
          (let ((cur-col (vimpulse-move-to-column col)))
            ;; if we are in Block mode, this line, but do not hit the
            ;; correct column, we check if we should convert tabs
            ;; and/or append spaces
            (if (and (eq mode 'block)
                     (or (/= col cur-col) ; wrong column or
                         (eolp)))         ; end of line
                (cond ((> cur-col col)    ; we are inside a tab
                       (move-to-column (1+ col) t) ; convert to spaces
                       (move-to-column col t) ; this is needed for ?a
                       (viper-repeat nil))
                      ((and (>= col cur-col) ; we are behind the end
                            (eq i-com ?a))   ; and I-COM is ?a
                       (move-to-column (1+ col) t) ; append spaces
                       (viper-repeat nil)))
              (viper-repeat nil)))))
      (setq vimpulse-visual-insert-coords nil)))
  ;; update undo-list
  (vimpulse-end-undo-step))

(defalias 'viper-exit-insert-state 'vimpulse-exit-insert-state)

(defadvice viper-goto-eol (after vimpulse-visual activate)
  "Move to end of line in Visual mode."
  (when vimpulse-visual-mode
    (end-of-line 1)))

(defadvice viper-forward-char (around vimpulse-activate activate)
  "Move to end of line in Visual mode."
  (cond
   (vimpulse-visual-mode
    (let ((val (viper-p-val arg)))
      (forward-char val)
      (when (and viper-ex-style-motion (bolp))
        (backward-char))))
   (t
    ad-do-it)))

;;; Key bindings

(define-key vimpulse-visual-basic-map "v" 'vimpulse-visual-toggle-char)
(define-key vimpulse-visual-basic-map "V" 'vimpulse-visual-toggle-line)
(define-key vimpulse-visual-basic-map "\C-v" 'vimpulse-visual-toggle-block)
(define-key vimpulse-visual-basic-map "x" 'vimpulse-delete)
(define-key vimpulse-visual-basic-map "D" 'vimpulse-delete)
(define-key vimpulse-visual-basic-map "Y" 'vimpulse-yank)
(define-key vimpulse-visual-basic-map "R" 'vimpulse-change)
(define-key vimpulse-visual-basic-map "C" 'vimpulse-change)
(define-key vimpulse-visual-basic-map "s" 'vimpulse-change)
(define-key vimpulse-visual-basic-map "S" 'vimpulse-change)
(define-key vimpulse-visual-basic-map "o" 'exchange-point-and-mark)
(define-key vimpulse-visual-basic-map "O" 'vimpulse-visual-exchange-corners)
(define-key vimpulse-visual-basic-map "I" 'vimpulse-visual-insert)
(define-key vimpulse-visual-basic-map "A" 'vimpulse-visual-append)
(define-key vimpulse-visual-basic-map "U" 'vimpulse-upcase)
(define-key vimpulse-visual-basic-map "u" 'vimpulse-downcase)
(define-key vimpulse-visual-basic-map ":" 'vimpulse-visual-ex)
;; keys that have no effect in Visual mode
(vimpulse-visual-remap 'viper-repeat 'viper-nil)

;;;; This code integrates Viper with the outside world

;;; undo-tree.el

(when (and (boundp 'undo-tree-visualizer-map)
           (fboundp 'undo-tree-visualizer-quit))

  (defun vimpulse-undo-quit ()
    "Quit the undo-tree visualizer and delete window."
    (interactive)
    (let ((w (selected-window)))
      (undo-tree-visualizer-quit)
      (when (eq (selected-window) w)
        (delete-window))))

  (add-to-list 'viper-vi-state-mode-list 'undo-tree-visualizer-mode)

  (let ((map undo-tree-visualizer-map))
    (vimpulse-add-core-movement-cmds map)
    (vimpulse-inhibit-destructive-cmds map)
    (vimpulse-inhibit-other-movement-cmds map)

    (define-key map [remap viper-backward-char] 'undo-tree-visualize-switch-branch-left)
    (define-key map [remap viper-forward-char] 'undo-tree-visualize-switch-branch-right)
    (define-key map [remap viper-next-line] 'undo-tree-visualize-redo)
    (define-key map [remap viper-previous-line] 'undo-tree-visualize-undo)
    (define-key map [remap undo-tree-visualizer-scroll-left] 'viper-scroll-up)
    (define-key map [remap undo-tree-visualizer-scroll-left] 'viper-scroll-up-one)
    (define-key map [remap undo-tree-visualizer-scroll-right] 'viper-scroll-down)
    (define-key map [remap undo-tree-visualizer-scroll-right] 'viper-scroll-down-one)
    (define-key map [remap viper-intercept-ESC-key] 'vimpulse-undo-quit)
    (define-key map [remap undo-tree-visualizer-quit] 'vimpulse-undo-quit)
    (define-key map [remap viper-next-line-at-bol] 'vimpulse-undo-quit)

    (viper-modify-major-mode 'undo-tree-visualizer-mode 'vi-state map)

    (add-to-list 'ex-token-alist '("undolist" (undo-tree-visualize)))
    (add-to-list 'ex-token-alist '("ulist" (undo-tree-visualize)))))

;;; Isearch

(defadvice isearch-message-prefix (around vimpulse-search activate)
  "Use vi prefix if appropriate."
  (if vimpulse-search-prompt
      (setq ad-return-value vimpulse-search-prompt)
    ad-do-it))

(defadvice isearch-delete-char (around vimpulse-search activate)
  "Exit search if no search string."
  (if (and vimpulse-search-prompt
           (string= isearch-string ""))
      (isearch-exit)
    ad-do-it))

(defadvice isearch-update-ring (after vimpulse-search activate)
  "Update `viper-s-string'."
  (when (eq viper-re-search regexp)
    (setq viper-s-string string)))

(defadvice isearch-lazy-highlight-search (around vimpulse-search activate)
  "Deactivate `viper-search-wrap-around'."
  (let (viper-search-wrap-around)
    ad-do-it))

(defadvice viper-search (after vimpulse-search activate)
  "Update isearch history."
  (isearch-update-ring string viper-re-search))

;; if `viper-search-wrap-around' is t, we want the search to wrap
(defun vimpulse-search-fun-function ()
  "Return a wrapping search function.
Based on `viper-re-search' and `viper-s-forward'."
  `(lambda (regexp &optional bound noerror count)
     (let ((orig (point))
           (search-fun (if isearch-regexp
                           (if isearch-forward
                               're-search-forward
                             're-search-backward)
                         (if isearch-forward
                             'search-forward
                           'search-backward)))
           retval)
       (setq retval (funcall search-fun regexp bound t count))
       (when (and (not retval) viper-search-wrap-around)
         (goto-char (if isearch-forward (point-min) (point-max)))
         (setq retval (funcall search-fun regexp bound t count))
         (unless retval
           (goto-char orig)))
       retval)))

(defun vimpulse-search-backward (arg)
  "Search backward for user-entered text.
Searches for regular expression if `viper-re-search' is t."
  (interactive "P")
  (let ((vimpulse-search-prompt "?")
        (lazy-highlight-initial-delay 0)
        (orig (point))
        (isearch-mode-map isearch-mode-map)
        (isearch-search-fun-function 'vimpulse-search-fun-function)
        (oldmsg (current-message))
        message-log-max
        search-nonincremental-instead)
    (vimpulse-vi-remap 'viper-intercept-ESC-key
                       'isearch-exit
                       isearch-mode-map)
    (setq viper-s-forward nil)
    (isearch-backward viper-re-search)
    (when (and (eq orig (point))
               (not (string= isearch-string "")))
      (isearch-repeat-backward)
      (isearch-exit))
    (if oldmsg (message "%s" oldmsg)
      (message nil))
    (unless (string= isearch-string "")
      (vimpulse-flash-search-pattern t))
    (setq vimpulse-this-motion 'viper-search-next)))

(put 'vimpulse-search-backward 'function-documentation
     (format "Search backward for user-entered text.
Searches for regular expression if `viper-re-search' is t.

%s" (if (and (fboundp 'isearch-forward)
             (documentation 'isearch-forward))
        (format "Below is the documentation string for `isearch-forward',
which lists available keys:

%s" (documentation 'isearch-forward)))))

(defun vimpulse-search-forward (arg)
  "Search forward for user-entered text.
Searches for regular expression if `viper-re-search' is t."
  (interactive "P")
  (let ((vimpulse-search-prompt "/")
        (orig (point))
        (isearch-mode-map isearch-mode-map)
        (isearch-search-fun-function 'vimpulse-search-fun-function)
        (oldmsg (current-message))
        message-log-max
        search-nonincremental-instead)
    (vimpulse-vi-remap 'viper-intercept-ESC-key
                       'isearch-exit
                       isearch-mode-map)
    (setq viper-s-forward t)
    (isearch-forward viper-re-search)
    (and isearch-other-end (goto-char isearch-other-end))
    (when (and (eq orig (point))
               (not (string= isearch-string "")))
      (isearch-repeat-forward)
      (isearch-exit))
    (and isearch-other-end (goto-char isearch-other-end))
    (if oldmsg (message "%s" oldmsg)
      (message nil))
    (unless (string= isearch-string "")
      (vimpulse-flash-search-pattern t))
    (setq vimpulse-this-motion 'viper-search-next)))

(put 'vimpulse-search-forward 'function-documentation
     (format "Search forward for user-entered text.
Searches for regular expression if `viper-re-search' is t.

%s" (if (and (fboundp 'isearch-forward)
             (documentation 'isearch-forward))
        (format "Below is the documentation string for `isearch-forward',
which lists available keys:

%s" (documentation 'isearch-forward)))))

(defun vimpulse-flash-search-pattern (&optional only-current)
  "Flash search matches for duration of `vimpulse-flash-delay'."
  (let ((lazy-highlight-initial-delay 0)
        (isearch-search-fun-function 'vimpulse-search-fun-function)
        (isearch-case-fold-search case-fold-search)
        (disable (lambda (&optional arg) (vimpulse-flash-hook t))))
    (when vimpulse-flash-timer
      (if (fboundp 'disable-timeout)
          (disable-timeout vimpulse-flash-timer)
        (cancel-timer vimpulse-flash-timer)))
    (when (viper-has-face-support-p)
      (isearch-highlight (match-beginning 0) (match-end 0))
      (unless only-current
        (setq isearch-string viper-s-string
              isearch-forward viper-s-forward
              isearch-regexp viper-re-search
              isearch-lazy-highlight-wrapped nil
              isearch-lazy-highlight-start (point)
              isearch-lazy-highlight-end (point))
        (and (fboundp 'isearch-lazy-highlight-new-loop)
             (isearch-lazy-highlight-new-loop))
        (unless (and (boundp 'isearch-lazy-highlight-overlays)
                     isearch-lazy-highlight-overlays)
          (and (fboundp 'isearch-lazy-highlight-update)
               (isearch-lazy-highlight-update))))
      (add-hook 'pre-command-hook 'vimpulse-flash-hook)
      (setq vimpulse-flash-timer
            (if (fboundp 'run-at-time)
                (add-timeout vimpulse-flash-delay disable nil)
              (run-at-time vimpulse-flash-delay nil disable))))))

(defun vimpulse-flash-hook (&optional force)
  "Disable hightlighting if `this-command' is not search.
Disable anyway if FORCE is t."
  (when (or force
            ;; to avoid flicker, don't disable highlighting if the
            ;; next command is also a search command
            (not (memq this-command
                       '(viper-exec-mapped-kbd-macro
                         viper-search
                         viper-search-backward
                         viper-search-forward
                         viper-search-next
                         viper-search-Next
                         vimpulse-search-backward
                         vimpulse-search-forward
                         vimpulse-search-backward-for-symbol-at-point
                         vimpulse-search-forward-for-symbol-at-point))))
    (isearch-dehighlight)
    (setq isearch-lazy-highlight-last-string nil)
    (and (fboundp 'isearch-highlight-all-cleanup)
         (isearch-highlight-all-cleanup))
    (and (fboundp 'lazy-highlight-cleanup)
         (lazy-highlight-cleanup t))
    (when vimpulse-flash-timer
      (cancel-timer vimpulse-flash-timer)))
  (remove-hook 'pre-command-hook 'vimpulse-flash-hook))

(when vimpulse-incremental-search
  (defvaralias 'viper-case-fold-search 'case-fold-search)
  (defalias 'viper-search-backward 'vimpulse-search-backward)
  (defalias 'viper-search-forward 'vimpulse-search-forward)
  (defalias 'viper-flash-search-pattern 'vimpulse-flash-search-pattern))

;;; Add vi navigation to help buffers

;; Apropos
(eval-after-load 'apropos
  '(when vimpulse-want-vi-keys-in-apropos
     (add-to-list 'viper-vi-state-mode-list 'apropos-mode)
     (let ((map apropos-mode-map))
       (vimpulse-add-core-movement-cmds map)
       (vimpulse-inhibit-destructive-cmds map)
       (viper-modify-major-mode 'apropos-mode 'vi-state map))))

;; Buffer-menu
(eval-after-load "buff-menu"
  '(when vimpulse-want-vi-keys-in-buffmenu
     (setq viper-emacs-state-mode-list
           (delq 'Buffer-menu-mode viper-emacs-state-mode-list))
     (add-to-list 'viper-vi-state-mode-list 'Buffer-menu-mode)
     (let ((map Buffer-menu-mode-map))
       (vimpulse-add-core-movement-cmds map)
       (vimpulse-inhibit-destructive-cmds map)
       (viper-modify-major-mode 'Buffer-menu-mode 'vi-state map))))

;; Dired
(eval-after-load 'dired
  '(when vimpulse-want-vi-keys-in-dired
     (setq viper-emacs-state-mode-list
           (delq 'dired-mode viper-emacs-state-mode-list))
     (add-to-list 'viper-vi-state-mode-list 'dired-mode)
     (let ((map dired-mode-map))
       (vimpulse-add-core-movement-cmds map)
       (vimpulse-inhibit-destructive-cmds map)
       (viper-modify-major-mode 'dired-mode 'vi-state map))))

;; Info
(eval-after-load 'info
  '(when vimpulse-want-vi-keys-in-Info
     (setq viper-emacs-state-mode-list
           (delq 'Info-mode viper-emacs-state-mode-list))
     (add-to-list 'viper-vi-state-mode-list 'Info-mode)
     (let ((map Info-mode-map))
       (vimpulse-add-core-movement-cmds map)
       (vimpulse-inhibit-destructive-cmds map)
       (define-key map "\C-t" 'Info-history-back) ; l
       (define-key map "\C-o" 'Info-history-back)
       (define-key map (kbd "\M-h") 'Info-help) ; h
       (define-key map " " 'Info-scroll-up)
       (define-key map "\C-]" 'Info-follow-nearest-node)
       (define-key map [backspace] 'Info-scroll-down)
       (viper-modify-major-mode 'Info-mode 'vi-state map))))

;; Help
(eval-after-load 'help-mode
  '(when vimpulse-want-vi-keys-in-help
     (setq viper-emacs-state-mode-list
           (delq 'help-mode viper-emacs-state-mode-list))
     (add-to-list 'viper-vi-state-mode-list 'help-mode)
     (let ((map help-mode-map))
       (vimpulse-add-core-movement-cmds map)
       (vimpulse-inhibit-destructive-cmds map)
       (define-key map "q" 'View-quit)
       (viper-modify-major-mode 'help-mode 'vi-state map))))

;; Slime
(eval-after-load 'slime
  '(defadvice slime-popup-buffer-mode (after vimpulse activate)
     (when slime-popup-buffer-mode
       (viper-add-local-keys
        'vi-state '(([?q] . slime-popup-buffer-quit-function))))))

;;; ElDoc

(eval-after-load 'eldoc
  '(apply 'eldoc-add-command
          (append vimpulse-viper-movement-cmds
                  vimpulse-core-movement-cmds)))

;;; Folding

(eval-after-load 'hideshow
  '(progn
     (defun vimpulse-za ()
       (interactive)
       (hs-toggle-hiding)
       (hs-hide-level vimpulse-fold-level))
     (defun vimpulse-hs-setup ()
       (define-key viper-vi-basic-map "za" 'vimpulse-za)
       (define-key viper-vi-basic-map "zm" 'hs-hide-all)
       (define-key viper-vi-basic-map "zr" 'hs-show-all)
       (define-key viper-vi-basic-map "zo" 'hs-show-block)
       (define-key viper-vi-basic-map "zc" 'hs-hide-block))
     (add-hook 'hs-minor-mode-hook 'vimpulse-hs-setup)))

(provide 'vimpulse)

;;; vimpulse.el ends here
