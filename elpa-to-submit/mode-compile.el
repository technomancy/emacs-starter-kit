;;; mode-compile.el ---  Smart command for compiling files
;;                       according to major-mode.
;;
;;   Copyright (c) 1994 - 2006 heddy Boubaker C.E.N.A.
;;
;;   Author: Heddy Boubaker <heddy.Boubaker@cena.fr>
;;   Maintainer: Heddy Boubaker <heddy.Boubaker@cena.fr>
;;   Created: June 1994
;;   Last modified: 2006/12/01 13:52:47
;;   Version: 2.29
;;   Keywords: compile, compilation, modes, languages
;;   Tested for:
;;     XEmacs (Lucid GNU Emacs) >= 19.10
;;     Must work with FSF GNU Emacs > 19.31 ;-)
;;     Do not work anymore for Emacses <= 18
;;   Ftp access:
;;    archive.cis.ohio-state.edu:pub/gnu/emacs/elisp-archive/misc/mode-compile.el.Z
;;   WWW access:
;;    <URL http://www.tls.cena.fr/~boubaker/Emacs/>
;;
;; LCD Archive Entry:
;; mode-compile|Heddy Boubaker|boubaker@cena.fr|
;; Smart command for compiling files according to major-mode and more.|
;; 2003/04/01 13:52:47|2.28|~/misc/mode-compile.el.Z|
;;
;;; This file is NOT part of GNU Emacs but the same permissions apply.
;;
;; GNU Emacs is free software; you can redistribute  it and/or modify it under
;; the terms  of  the GNU General   Public License as   published  by the Free
;; Software Foundation;    either version 2,   or (at  your option)  any later
;; version.
;;
;; GNU Emacs  is distributed in  the hope that it will  be useful, but WITHOUT
;; ANY  WARRANTY; without  even the  implied   warranty of MERCHANTABILITY  or
;; FITNESS  FOR A PARTICULAR PURPOSE.  See  the GNU General Public License for
;; more details.
;;
;; You  should have received a  copy of the GNU  General  Public License along
;; with GNU Emacs; see the  file COPYING.  If  not, write to the Free Software
;; Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.
;;
;; @ Purpose:
;; ==========
;;
;;  Provide `mode-compile' function as a replacement  for the use of `compile'
;;  command which  is  very dumb  for  creating it's  compilation command (use
;;  "make  -k" by default).  `mode-compile'  is  a layer  above `compile'; Its
;;  purpose is mainly   to  build a  smart  compile-command for  `compile'  to
;;  execute  it. This   compile-command is   built   according  to number   of
;;  parameters:
;;   - the major-mode.
;;   - presence or not of a makefile in current directory.
;;   - the buffer-file-name and extension.
;;   - what is in the current buffer (`main' function,"#!/path/shell", ...).
;;   - and more ... (see Commentary section below).
;;  Most  of  these  parameters  are  higly customizable  throught Emacs  Lisp
;;  variables  (to be  set  in your  .emacs  or  through Customization  menu).
;;  Running  mode-compile after   an  universal-argument (C-u)  allows  remote
;;  compilations, user is  prompted  for a host name   to run the  compilation
;;  command on.   Another  function  provided  is  `mode-compile-kill'   which
;;  terminate a running compilation session launched by `mode-compile'.
;;
;; @ Installation:
;; ===============
;;
;;   Byte compile this file (*) somewhere in your `load-path' and add in
;;  your .emacs:
;;  (autoload 'mode-compile "mode-compile"
;;   "Command to compile current buffer file based on the major mode" t)
;;  (global-set-key "\C-cc" 'mode-compile)
;;  (autoload 'mode-compile-kill "mode-compile"
;;   "Command to kill a compilation launched by `mode-compile'" t)
;;  (global-set-key "\C-ck" 'mode-compile-kill)
;;
;;  By  default mode-compile is  very verbose  and  waits a  few seconds (1 by
;;  default) after  each message for   the user to have  time  to read it. You
;;  could      set           variables      `mode-compile-expert-p'        and
;;  `mode-compile-reading-time'   to  change  this    behaviour.  On X-Windows
;;  systems  setting the variable  `mode-compile-other-frame-p'  will create a
;;  new frame and launch the compilation command in it.
;;
;;  (*) Don't take care of messages:
;;        ** reference to free variable efs-remote-shell-file-name
;;      This is perfectly normal ;-}. But if you know a way to avoid it let me
;;      know.
;;
;; @ Bug Reports:
;; ==============
;;
;;   To   report a  bug  please  use function `mode-compile-submit-bug-report'
;;   Please note that this bug-report facility uses Barry Warsaw's reporter.el
;;   which is part of GNU Emacs v19 and bundled with many  other packages.  If
;;   needed, you can obtain a copy of reporter.el at the elisp-archive.
;;
;; @ Documentation:
;; ================
;;
;;  This section will explain how the `compile-command' are built according to
;;  the `major-mode' and how to  customize it.  The major modes `mode-compile'
;;  currently known are:
;;   - c-mode, c++-mode, makefile-mode, dired-mode, ada-mode, emacs-lisp-mode,
;;     lisp-interaction-mode, sh-mode, csh-mode, fundamental-mode,  text-mode,
;;     indented-text-mode     compilation-mode,  fortran-mode,    c?perl-mode,
;;     zsh-mode java-mode, tcl-mode, python-mode, ruby-mode
;;  For other modes a default behaviour is provided.
;;
;;  When    running    `mode-compile'  or     `mode-compile-kill'   the  hooks
;;  `mode-compile-(before|after)-(compile|kill)-hook'  are   executed.     The
;;  current   buffer   could   be      automaticaly    saved    if    variable
;;  `mode-compile-always-save-buffer-p' is  set  to   `t'.  ALL  the  modified
;;  buffers could  be automaticaly saved if variable `mode-compile-save-all-p'
;;  is set to `t'.
;;
;; @@ fundamental-mode, text-mode, indented-text-mode & UNKNOWN MODES:
;;    *** THIS IS TOO THE DEFAULT BEHAVIOR FOR UNKNOWN MODES ***
;;    Try to guess what the file is by:
;;   - 1st looking at it's name and extension (see variable
;;     `mode-compile-filename-regexp-alist').
;;   - 2nd looking at string "#!/path/shell" at first line to extract shell
;;     to run the script with (see variable `mode-compile-shell-alist').
;;   - 3rd looking at a makefile in current directory.
;;   - then calling `compile' with the last compile command which is
;;     asked to be edited by user ...
;;   The `kill-compile' command is then bound dynamically (buffer-local).
;;
;; @@ compilation-mode:
;;    Call `compile' with the last compile command.
;;
;; @@ makefile-mode:
;;    The makefile is run with make throught `compile' (user is prompted
;;   for        the        rule         to      run,      see         variable
;;   `mode-compile-prefered-default-makerule'  to  see how  a   default choice
;;   could be selected).
;;
;; @@ emacs-lisp-mode, lisp-interaction-mode:
;;    If the buffer is a .el file byte-compile it to produce a .elc file,
;;   else  just  byte-compile  the   buffer  (this   don't  use  `compile' but
;;   `byte-compile').
;;
;; @@ dired-mode:
;;   Find a makefile   in   the directory and  run    make with  it   (like in
;;   makefile-mode),  else try  to byte-recompile  all .el   files olders than
;;   their associated  .elc  files (unlike  `byte-recompile-directory' this is
;;   not  recursive),  finally if  no  .el  files  are present ask compilation
;;   command to  user by  calling  `default-compile'.  To  find  a  makefile a
;;   regexp is provided which name is `mode-compile-makefile-regexp'.
;;
;; @@ sh-mode, csh-mode, zsh-mode:
;;    Run "[cz]?sh" with debugging arguments as specified in
;;   `[cz]?sh-dbg-flags' on the currently edited file.
;;
;; @@ c?perl-mode:
;;    Run  file with "perl   -w"  (can step   throught errors  with  compile's
;;    `next-error' command).
;;
;; @@ tcl-mode:
;;    Run     file  with "wish"  (can     step  throught errors with compile's
;;    `next-error' command).
;;
;; @@ c-mode, c++-mode:
;;   First it try to see if there is a makefile in the directory, makefiles to
;;   look for are  specified  by the variable  `mode-compile-makefile-regexp'.
;;   If yes two cases could happen: there  is only one  makefile so use it, or
;;   there is more  than one (sometimes when  you need to  write portable soft
;;   you  could have  some makefiles by  system:  SunOs.make, HP.make ...), in
;;   that case prompt to  user for choice  (with smart completion).   Once the
;;   makefile has been selected  it extract the rules from  it and ask to user
;;   to   choose  a   rule  to  make  (with  smart  completion,  see  variable
;;   `mode-compile-prefered- default-makerule'  to  see  how  a default choice
;;   could be selected).
;;
;;   There are some cases where no makefiles are  presents (YES I KNOW this is
;;   bad  practice but you  sometimes have no  needs  to write a Makefile). In
;;   that case the  function try  to build  the most intelligent   compilation
;;   command by using the favourite  user C/C++ compiler: value of environment
;;   variable "CC"  or "CXX"  or  first found,   in  the PATH,   of  compilers
;;   specified in variable  `cc-compilers-list' or `c++-compilers-list'.  Then
;;   it look for the  varenv "CFLAGS" of  "CXXFLAGS" to append to the compiler
;;   command,        find            the     file         to          compile:
;;   <name-of-the-file-to-compiled>.(c|cc|C|cpp)  (see    *)   and   ask   for
;;   confirmation.  If  you  really trust mode-compile   will build  the right
;;   command  and  want to  bypass confirmation  you  could  set the  variable
;;   `mode-compile-never-edit-command-p' to t.
;;
;;   (*) How to find <name-of-the-file-to-compiled>:
;;    In both case the command try to guess which file has to be compiled:
;;   It's a trivial choice when current buffer file is a
;;   .(c|C|cc|cpp... -any file with extension specified in
;;   `cc-source-file-ext-list' or `c++-source-file-ext-list') file but
;;   when it's a .(h|H|hh) file what to do?  The variable
;;   `cc-companion-file-regexp' or `c++-companion-file-regexp' specify
;;   how to find a .(c|C|cc|cpp...) file from a .(h|H|hh...); This is
;;   done by appending .(c|C|cc|cpp) to
;;   <filename-without-matching-regexp>.  In c-mode with default value
;;   it produce:
;;      file.h, file_[Pp].h -> file.c
;;      I sometimes use files _p.h to indicate that the file is a private header
;;      file for a .c file.
;;   In c++-mode with default value it produce:
;;      file.hh, file_[Pp].hh -> file.cc
;;      I sometimes use files _p.cc to indicate that the file is a private header
;;      file for a .cc file.
;;   The output of compilation will be a
;;   <name-of-the-file-to-compiled>.o file if no `main' function is
;;   found inside or a <name-of-the-file-to-compiled> EXECUTABLE file
;;   if `main' function found.
;;
;; @@ ada-mode:
;;   Same  as c/c++-mode but  run Ada compiler on the  Ada file.  There are no
;;   companion file and no way to find a main function in Ada.
;;
;; @@ fortran-mode:
;;    Same as c-mode but run Fortran compiler on .[Ff](or)? files.
;;
;; @@ java-mode:
;;    Same as c-mode but call "javac" without the -o option on .java files
;;
;; @@  python-mode:
;;    Run file with "python" (can step throught errors with compile's
;;    `next-error' command).
;;
;; @@  ruby-mode:
;;    Run file with "ruby" (can step throught errors with compile's
;;    `next-error' command).
;;
;; @@  message-mode:
;;    Run `message-send'.
;;
;; @ WhatsNew:
;; ===========
;;
;;  Support for ruby-mode
;;
;; @ Contributors/Helpers:
;; =======================
;;
;;   Adrian Aichner <aichner@ecf.teradyne.com>
;;   "William A. Perkins" <wa_perkins@pnl.gov>
;;   Bin Mu <mubin@DerivaTech.COM>
;;   Gael MARZIOU <Gael_Marziou@grenoble.hp.com>
;;   Christian Motschke <motschke@prosun.first.gmd.de>
;;   boris <boris@cs.rochester.edu>
;;   Edward Hartnett <ejh@larry.gsfc.nasa.gov>.
;;   Hartmut MANZ <manz@intes-stuttgart.de>.
;;   Henry Guillaume <henryg@tusc.com.au>.
;;   Ian Young <imy@wcl-rs.bham.ac.uk>
;;   Ilya Zakharevich <ilya@math.ohio-state.edu>.
;;   Kevin Broadey <KevinB@bartley.demon.co.uk>.
;;   Lawrence R. Dodd <dodd@roebling.poly.edu>.
;;   Martin Jost <asictest@ztivax.zfe.siemens.de>.
;;   Michael Welsh Duggan <md5i+@andrew.cmu.edu>.
;;   Rolf EBERT <rolf@gundog.lbl.gov>.
;;   Scott Hofmann <scotth@visix.com>.
;;   Stefan Schoef <Stefan.Schoef@arbi.informatik.uni-oldenburg.de>.
;;   John W. Harwell <ccjohnh@showme.missouri.edu> - JWH.
;;   Charles L.G. Comstock <dgtized@gmail.com> - CLGC
;;
;; @ ToDo:
;; =======
;;
;;   Extending this to some others programming languages (modes).
;;   Writting an Info documentation.
;;   Contributors are greatly accepted (send me diffs and don't forget to
;;   update documentation and all comments too please).
;;   Maybe Using ange-ftp parse .netrc utilities for remote host and
;;   user infos.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; @ Requirements ;;;

;; mode-compile is not a replacement for compile
;; it is just a layer above it.
(require 'compile)
;;; For Emacs-Lisp files compilations
(require 'byte-compile "bytecomp")
;;; For easy macros
(require 'backquote)
(require 'cl)
(load-library "cl-macs")
;; Pretty print elisp
(require 'pp)
;;; Setting obsolete vars
(eval-and-compile
  (condition-case ()
      (require 'obsolete)
    (error nil))
  (if (and (featurep 'obsolete) (fboundp 'define-obsolete-variable-alias))
      nil ;; We've got what we needed
    (defmacro define-obsolete-variable-alias (old new)
      (` (progn (defalias (, old) (, new))
                (put (, old) 'byte-obsolete-variable (, new)))
         ))))
;;; For using custom - stolen from w3-cus.el -
(eval-and-compile
  (condition-case ()
      (require 'custom)
    (error nil))
  (if (and (featurep 'custom) (fboundp 'custom-declare-variable))
      nil ;; We've got what we needed
    ;; We have the old custom-library, hack around it!
    (defmacro defgroup (&rest args)
      nil)
    (defmacro defcustom (var value doc &rest args)
      (` (defvar (, var) (, value) (, doc))))))

;; Custom groups
(defgroup compilation nil
  "Compilations from within Emacs variables."
  :link '(url-link :tag "Author's Emacs Page"
                   "http://www.tls.cena.fr/~boubaker/Emacs/")
  :group 'tools
  :group 'development)
(defgroup compilation-lang nil
  "Language specific compilation options."
  :group 'languages
  :group 'compilation)
(defgroup compilation-script nil
  "Scripts compilation options."
  :group 'compilation)
(defgroup compilation-elisp nil
  "Emacs developpement compilation options."
  :group 'lisp
  :group 'compilation)

;; @ User variables ;;;
;; @@ Common variables to mode-compile for all modes ;;;

(defcustom mode-compile-modes-alist
  '((c-mode                . (cc-compile        kill-compilation))
    (java-mode             . (java-compile      kill-compilation))
    (c++-mode              . (c++-compile       kill-compilation))
    (ada-mode              . (ada-compile       kill-compilation))
    (fortran-mode          . (f77-compile       kill-compilation))
    (dired-mode            . (dired-compile     kill-compilation))
    (emacs-lisp-mode       . (elisp-compile     keyboard-quit)) ; I'm SURE IT'S NOT the best way
    (lisp-interaction-mode . (elisp-compile     keyboard-quit)) ; to kill a byte-compilation.
    (makefile-mode         . (makefile-compile  kill-compilation))
    (sh-mode               . (sh-compile        kill-compilation))
    (csh-mode              . (csh-compile       kill-compilation))
    (zsh-mode              . (zsh-compile       kill-compilation))
    (perl-mode             . (perl-compile      kill-compilation))
    (cperl-mode            . (perl-compile      kill-compilation))
    (tcl-mode              . (tcl-compile       kill-compilation)) ; JWH
    (python-mode           . (python-compile    kill-compilation)) ; BM
    (ruby-mode             . (ruby-compile      kill-compilation)) ; CLGC
    ;(message-mode          . (message-compile   kill-compilation))
    (fundamental-mode      . (guess-compile     nil)) ; bound dynamically
    (text-mode             . (guess-compile     nil)) ; itou
    (indented-text-mode    . (guess-compile     nil)) ; itou
    (compilation-mode      . (default-compile   kill-compilation)))
  "Assoc list of compile/kill functions for some known modes.

Each element look like (MODE . (COMPILE-FUNCTION KILL-FUNCTION))
 `mode-compile' will call COMPILE-FUNCTION and `mode-compile-kill'
 KILL-FUNCTION if current major-mode is MODE.

If you want to add or modify a COMPILE-FUNCTION and it's associated
KILL-FUNCTION for MODE and don't want to hack `mode-compile' you could
do the following (it exists however a more subtle method for
modifying, this is left as an exercice for the reader :-):
 (defun my-mode-compile() ...)
 (defun my-mode-compile-kill() ...)
 (setq mode-compile-modes-alist
       (append '((my-mode . (my-mode-compile my-mode-compile-kill)))
               mode-compile-modes-alist))"
  :type '(repeat
          (cons :tag "Association: mode/compilation functions"
                (function :tag "Mode")
                (list :tag "Compilation functions"
                      (choice :tag "Function to run the compilation"
                              (function-item :tag "Default" :value default-compile)
                              (function-item :tag "Guess" :value guess-compile)
                              (function-item :tag "Emacs lisp byte compilation" :value elisp-compile)
                              (function :tag "Your choice, take care..."))
                      (choice :tag "Function to kill a running compilation"
                              (function-item :tag "Just kill" :value kill-compilation)
                              (const :tag "Nothing -- use with guess-compile --" :value nil)
                              (function-item :tag "To use with elisp-compile" :value keyboard-quit)
                              ;; This item could not be selected due to a custom (hum) feature ...
                              (function :tag "Your choice, take care..." :value nil)))))
  :group 'compilation)

(defcustom mode-compile-filename-regexp-alist
  ;; These could be in auto-mode-alist. But if you are like me
  ;; and don't like these modes (prefear to edit these kind of
  ;; files in text-mode) this is a nice way to compile them
  ;; without to be bored with their associated modes.
  '((mode-compile-makefile-regexp . makefile-mode)
    ("\\.sh$"                     . sh-mode)
    ("\\.csh$"                    . csh-mode)
    ("\\.zsh$"                    . zsh-mode))
  "Assoc list of major-modes for some filenames regexp.

Each element look like (REGEXP . MODE) This variable is really similar
to `auto-mode-alist' in the fact that it associate a MODE to a REGEXP
matching a filename. The only differences is that you are not obliged
to have the specified MODE available to use it (`guess-compile' use
it), the MODE is only a pointer to an assoq in
`mode-compile-modes-alist' to get the COMPILE-FUNCTION and the
KILL-FUNCTION. The REGEXP could be a form wich evaluate to a string.

To add a new filename regexp do the following:
 (setq mode-compile-filename-regexp-alist
       (append '((my-filename-regexp . some-mode-mode-compile-know)
               mode-compile-modes-alist))"
  :type '(repeat
          (cons :tag "Association: filename/mode"
           (choice :tag "Filename regexp match"
            (regexp :tag "Regexp as a string")
            (symbol :tag "Variable containing the regexp")
            (sexp :tag "Form wich evaluate to a string"))
           ;; I need to bind dynamicaly this with const, ideas??
           ;;`(choice
           ;; ,@(mapcar (lambda (x) `(const ,(car x))) mode-compile-modes-alist))))
           (function :tag "Mode to use -- should be a valid assoq in mode-compile-modes-alist --")))
  :group 'compilation)

(defcustom mode-compile-shell-alist
  '(("sh"     .  sh-mode)
    ("csh"    .  csh-mode)
    ("zsh"    .  zsh-mode)
    ("perl"   .  perl-mode)
    ("tcl"    .   tcl-mode) ; JWH
    ("python" . python-mode) ; BM
    ("ruby"   . ruby-mode)) ; CLGC
  "Assoc list of compile function for some known shells.

Each element look like (SHELL . MODE) This variable look like
`auto-mode-alist' in the fact that it associate a MODE to a name; A
SHELL name here. The main difference is that you are not obliged to
have the specified MODE available to use it (`guess-compile' use it),
the MODE is only a pointer to an assoq in `mode-compile-modes-alist'
to get the COMPILE-FUNCTION and the KILL-FUNCTION.

To add a new shell do the following:
 (setq mode-compile-filename-shell-alist
       (append '((my-shell-name . some-mode-mode-compile-know)
               mode-compile-modes-alist))"
  :type '(repeat
          (cons :tag "Association: shell name/mode"
           (string :tag "Shell name")
           ;; I need to bind dynamicaly this with const, ideas??
           ;;`(choice
           ;; ,@(mapcar (lambda (x) `(const ,(car x))) mode-compile-modes-alist))))
           (function :tag "Mode to use -- should be a valid assoq in mode-compile-modes-alist --")))
  :group 'compilation)

;;;###autoload
(defcustom mode-compile-make-program "make"
  "*The `make' program used to process makefiles.

If you have GNU make installed with name \"gmake\" use it."
  :type 'string
  :group 'compilation)

(defcustom mode-compile-makefile-regexp
  "\\(^[Mm]akefile\\|.*\\.[mM][aA]?[kK][eE]?\\.?.*$\\)"
  "Regexp matching 'could be' makefiles filenames."
  :type 'regexp
  :group 'compilation)

(defcustom mode-compile-makefile-backups-regexp
  "\\(\\(~\\|\\.[bB][aA][cC]?[kK]\\)$\\)\\|\\(\\(^\\|/\\)[.,][^/]+$\\)"
  "Regexp to find if a Makefile is a backup or not"
  :type 'regexp
  :group 'compilation)

;;;###autoload
(defcustom mode-compile-ignore-makefile-backups t
  "*Tell mode compile to ignore makefiles backup files when selecting the Makefile to use."
  :type 'boolean
  :group 'compilation)

;;;###autoload
(defvar mode-compile-default-make-options "-k"
  "Default options to give to `make'.")
;;;###autoload
(defcustom mode-compile-make-options (eval mode-compile-default-make-options)
  "*Options to give to `make'.
This could be any form evaluating to a string.

Some people asked me a way to modify the make options everytime a
compilation command is launched, do that:
 (defun my-mode-compile-ask-make-options()
   \"*Hook called by mode-compile, asking for make options.\"
   (interactive)
   (read-string \"Make options: \"
                mode-compile-default-make-options))
 (setq mode-compile-make-options
           'my-mode-compile-ask-make-options)"
  :type '(choice
          string
          (sexp :tag "Form evaluating to a string"))
  :group 'compilation)

;;;###autoload
(defcustom mode-compile-prefered-default-makerule 'none
  "*Default makerule you would like to see in minibuffer as a default choice
when selecting the make rule to build.

Possible values are:
'none    -- let mode-compile deciding for you.
'all     -- try hard to show you the \"all\" rule.
'default -- try hard to show you the \"default\" rule.
'file    -- try to show you the name of the file which will be
            result of compilation.
The 'none action is taken as default is something fail."
  :type '(radio :tag "Symbol"
                (const :tag "None - Let mode compile made the choice" :value none)
                (const :tag "All - Show the \"all\" rule" :value all)
                (const :tag "Default - Show the \"default\" rule" :value default)
                (const :tag "File - Show the \"result file name\" rule" :value file))
  :group 'compilation)

;;;###autoload
(defcustom mode-compile-ignore-makerule-regexp nil
  "*Makefile rules which must be ignored when building completion list.

For example if you want to remove all `files rules' set
it to: \"\\\\.\\\\([aoc]\\\\|s[ao][.0-9]*\\\\)\". "
  :type '(choice (const :tag "none" :value nil)
                 (const :tag "The `all files' rule" :value "\\.\\([aoc]\\|s[ao][.0-9]*\\)")
                 regexp)
  :group 'compilation)

;;;###autoload
(defcustom mode-compile-save-all-p nil
  "*Non-nil means save ALL the modified buffers without asking
before launching compilation command."
  :type 'boolean
  :group 'compilation)

;;;###autoload
(defcustom mode-compile-always-save-buffer-p nil
  "*Non-nil means save the current buffer without asking
before launching compilation command."
  :type 'boolean
  :group 'compilation)

;;;###autoload
(defcustom mode-compile-never-edit-command-p nil
  "*Non-nil means never ask to user to edit the compile command."
  :type 'boolean
  :group 'compilation)

;; @@ Compilation in other frame vars ;;;
(defgroup compilation-frame nil
  "Compile in another frame variables."
  :group 'frames
  :group 'compilation)

;;;###autoload
(defcustom mode-compile-other-frame-p nil
  "*Non-nil means compile in another frame.

A new Emacs FRAME is created and the compilation command is executed
in this other frame.  To specify the frame parameters see also
variable `mode-compile-frame-parameters-alist'."
  :type 'boolean
  :group 'compilation-frame)

(defcustom mode-compile-other-frame-name "COMPILATION"
  "Name of mode-compile's other frame.

This name could be used in your .Xdefault or .Xresources file as:
Emacs.MODE-COMPILE-OTHER-FRAME-NAME.resource_to_be_set: ..."
  :type 'string
  :group 'compilation-frame)

(defconst mode-compile-default-frame-parameters
  (list
   (cons 'name   mode-compile-other-frame-name)
   (cons 'width  85)  ; columns
   (cons 'height 30)) ; lines
   "Default parameters for mode-compile's other frame.")

(defvar mode-compile-frame-parameters-alist
  (purecopy mode-compile-default-frame-parameters)
  "Parameters for the new Compilation Screen created
if variable `mode-compile-other-frame-p' is non nil..

See also variable `mode-compile-default-frame-parameters' and
`mode-compile-other-frame-name'.

For informations about Screen/Frame parameters see:
- Info, Nodes: Lispref::Screen::Screen Parameters
- GNU Emacs Lisp Reference Manual, chapter 26 p375: Frames.")

;; @@ Hooks ;;;

;;;###autoload
(defcustom mode-compile-before-compile-hook nil
  "Hook to be run before compile command is executed
when `mode-compile' is invoked."
  :type 'hook
  :group 'compilation)

;;;###autoload
(defcustom mode-compile-after-compile-hook nil
  "Hook to be run after compile command is executed
when `mode-compile' is invoked."
  :type 'hook
  :group 'compilation)

;;;###autoload
(defcustom mode-compile-before-kill-hook nil
  "Hook to be run before killing compile command is executed
when `mode-compile-kill' is invoked."
  :type 'hook
  :group 'compilation)

;;;###autoload
(defcustom mode-compile-after-kill-hook nil
  "Hook to be run after killing compile command is executed
when `mode-compile-kill' is invoked."
  :type 'hook
  :group 'compilation)

;; @@ System dependencies ;;;

(defvar mode-compile-exe-file-ext
  (cond
   ((memq system-type '(ms-dos emx windows-95 windows-98 windows-nt)) ".exe")
   (t ""))
  "*Extension of executable files (with dot included)")

(defvar mode-compile-dir-separator-char
  (cond
   ;; MSDOSish file systems
   ((memq system-type '(ms-dos emx windows-95 windows-98 windows-nt)) "\\")
   ;; Unixish file systems
   (t "/"))
  "*Separator char between directories")

;; @@ Facilities variables ;;;

;;;###autoload
(defvar mode-compile-choosen-compiler nil
  "*Global variable containing the name of the compiler
which will be used for compiling without makefile.

 Could be used in combination with
 (cc|c++|ada|f77)-default-compiler-options
to automaticaly choose the compiler specific options.

example:
 (defun my-compiler-get-options()
   (cond
    ((string= mode-compile-choosen-compiler \"gcc\")
      \"-Wall -pedantic-errors\")
    ((string= mode-compile-choosen-compiler \"cc\")
      \"cc options whatever they are...\")
    (t
     (message \"Don't know this compiler: %s\" mode-compile-choosen-compiler)
     (read-string
      (format \"Options for %s compiler: \" mode-compile-choosen-compiler)))))

  (setq cc-default-compiler-options 'my-compiler-get-options)")

;; @@ User level ;;;

;;;###autoload
(defcustom mode-compile-expert-p nil
  "*Non nil means `mode-compile' will not speaks too much.

See also variable variable mode-compile-reading-time."
  :type 'boolean
  :group 'compilation)

;;;###autoload
(defcustom mode-compile-reading-time 1
  "*Seconds to wait in verbose mode after printing a message.

In verbose mode mode-compile print too much messages that it is
allmost impossible to read them. Just setting this delay leave you the
time to read all the messages. If you don't want any delay set it to
`0'.

See also function sit-for."
  :type 'integer
  :group 'compilation)


;; @@ Remote compilation vars ;;;
(defgroup compilation-remote nil
  "Remote compilations options."
  :group 'compilation)

(defcustom mode-compile-remote-hosts-alist '()
  "Alist of favourites hosts names and the username
to use to log on (HOSTNAME . USERNAME).

If USERNAME is a function it will be called with HOSTNAME as argument
and should return an USERNAME string (for example you could use
something like efs-get-user - not tested -), if it is nil the function
user-login-name will be used."
  :type '(repeat
          (cons
           (string :tag "Hostname")
           (choice
            (const :tag "We'll use \'user-login-name" :value nil)
            (string :tag "Username")
            (function :tag "Function which return USERNAME given a HOSTNAME" :value efs-get-user))))
  :group 'compilation-remote)

(defcustom mode-compile-remote-execute-command "rsh"
  "The shell command used to run a command remotely.
\"rsh\" is the only choice I know but I'm far to know everything...

 This variable is set automaticaly with the value of
remote-shell-program or efs-remote-shell-file-name at load time."
  :type 'string
  :group 'compilation)
(eval-when 'load
  (cond
   ((not (string= mode-compile-remote-execute-command "rsh"))
    ;; user changed default
    nil)
   ;; Try to not multiply definitions of the same stuff
   ;; in too many emacs lisp packages ...
   ((and (boundp 'remote-shell-program) remote-shell-program)
    (setq mode-compile-remote-execute-command remote-shell-program))
   ((and (boundp 'efs-remote-shell-file-name) efs-remote-shell-file-name)
    (setq mode-compile-remote-execute-command efs-remote-shell-file-name))
   ))

(defcustom mode-compile-remote-execute-set-host-arg ""
  "Argument To set the remote host name to the
mode-compile-remote-execute-command,

None is required for \"rsh\"."
  :type 'string
  :group 'compilation-remote)

(defcustom mode-compile-remote-execute-set-command-arg ""
  "Argument to set the command to be run remotely to the
mode-compile-remote-execute-command.

None is required for \"rsh\"."
  :type 'string
  :group 'compilation-remote)

(defcustom mode-compile-remote-execute-set-username-arg "-l"
  "Argument to set the username under which we will log on
on the remote host, to give to mode-compile-remote-execute-command."
  :type 'string
  :group 'compilation-remote)

(defcustom mode-compile-remote-execute-misc-args ""
  "Misc additionnals arguments to give to the
mode-compile-remote-execute-command."
  :type 'string
  :group 'compilation-remote)



;; @@ c-mode compile variables ;;;
(defgroup compile-c nil
  "C Compilation options."
  :group 'c
  :group 'compilation-lang)

(defcustom cc-compilers-list '( "gcc" "c89" "acc" "cc" )
  "List of user's favourites C compilers in order of preferencies."
  :type '(repeat (string :tag "C Compiler name"))
  :group 'compile-c)

(defcustom cc-companion-file-regexp "\\(_[Pp]\\)?\\.[pP]?h"
  "Regexp to find associated .c file from a .h."
  :type 'regexp
  :group 'compile-c)

(defcustom cc-default-compiler "cc"
  "*Default C compiler to use when everything else fails.

This could be any form evaluating to a string, so you could map it to
a function asking you interactively to choose the compiler.

example:
 (defun my-choose-compiler()
   (read-string \"C compiler: \"))
 (setq cc-compilers-list '()
       cc-default-compiler 'my-choose-compiler)"
  :type '(choice string function)
  :group 'compile-c)

(defcustom cc-compiler-varenv "CC"
  "Varenv indicating the C compiler to use."
  :type 'string
  :group 'compile-c)

(defcustom cc-cflags-varenv "CFLAGS"
  "Varenv indicating the C compiler flags to use."
  :type 'string
  :group 'compile-c)

(defcustom cc-source-ext-list '( "c" )
  "Extensions for C compileable source files."
  :type '(repeat string)
  :group 'compile-c)

(defcustom cc-headers-ext-list '( "h" )
  "Extensions for C headers source files."
  :type '(repeat string)
  :group 'compile-c)

(defcustom cc-default-compiler-options "-g"
  "*Default options to give to the C compiler.

This could be any form evaluating to a string.
See `mode-compile-choosen-compiler' variable."
  :type '(choice
          string
          (sexp :tag "Form evaluating to a string"))
  :group 'compile-c)

(defcustom cc-source-file-ext-regexp "\\.c"
  "Regexp to find, from it's name, if a C file is compileable."
  :type 'string
  :group 'compile-c)

(defcustom cc-build-output-args t
  "Build output-args for c-mode."
  :type 'boolean
  :group 'compile-c)

(defcustom cc-object-file-ext "o"
  "Extension of objects file (result of compilation)
in c mode."
  :type 'string
  :group 'compile-c)



;; @@ java-mode compile variables ;;;
(defgroup compile-java nil
  "Java compilation options."
  :group 'compilation-lang)

(defcustom java-compilers-list '( "javac" )
  "List of user's favourites java compilers in order of preferencies."
  :type '(repeat (string :tag "Java Compiler name"))
  :group 'compile-java)

(defcustom java-companion-file-regexp ""
  "Regexp to find associated compileable Java companion file.

This is useless in Java because there do not exists uncompileable files."
  :type 'regexp
  :group 'compile-java)

(defcustom java-default-compiler "javac"
  "*Default C compiler to use when everything else fails.

This could be any form evaluating to a string, so you could map it to
a function asking you interactively to choose the compiler.

example:
 (defun my-choose-compiler()
   (read-string \"Java compiler: \"))
 (setq java-default-compiler 'my-choose-compiler)."
  :type '(choice string function)
  :group 'compile-java)

(defcustom java-compiler-varenv "JAVAC"
  "Varenv indicating the C compiler to use."
  :type 'string
  :group 'compile-java)

(defcustom java-cflags-varenv "JAVAC_FLAGS"
  "Varenv indicating the C compiler flags to use."
  :type 'string
  :group 'compile-java)

(defcustom java-source-ext-list '( "java" )
  "Extensions for Java compileable source files."
  :type '(repeat string)
  :group 'compile-java)

(defcustom java-headers-ext-list '( "java" )
  "Extensions for Java source files."
  :type '(repeat string)
  :group 'compile-java)

(defcustom java-default-compiler-options "-O"
  "*Default options to give to the Java compiler.

This could be any form evaluating to a string.  See
`mode-compile-choosen-compiler' variable."
  :type '(choice
          string
          (sexp :tag "Form evaluating to a string"))
  :group 'compile-java)

(defcustom java-source-file-ext-regexp "\\.java"
  "Regexp to find, from it's name, if a Java file is compileable."
  :type 'regexp
  :group 'compile-java)

(defcustom java-build-output-args nil
  "Dont build output-args for Java-mode."
  :type 'boolean
  :group 'compile-java)

(defcustom java-object-file-ext "class"
  "Extension of objects file (result of compilation)
in java mode."
  :type 'string
  :group 'compile-java)



;; @@ c++-mode compile variables ;;;
(defgroup compile-c++ nil
  "C++ compilation options"
  :group 'compilation-lang)

(defcustom c++-compilers-list '( "g++" "gcc" "CC" )
  "List of user's favourites C++ compilers in order of preferencies."
  :type '(repeat (string :tag "C++ Compiler name"))
  :group 'compile-c++)

(defcustom c++-companion-file-regexp "\\(_[Pp]\\)?\\.\\([pP]?[Hh][Hh]?\\|[Hh]\\+\\+?\\)"
  "Regexp to find associated compileable C++ companion file
from a header file."
  :type 'regexp
  :group 'compile-c++)

(defcustom c++-default-compiler "CC"
  "*Default C++ compiler to use when everything else fails..

This could be any form evaluating to a string, so you could map it to
a function asking you interactively to choose the compiler.

example:
 (defun my-choose-compiler()
   (read-string \"C++ compiler: \"))
 (setq c++-default-compiler 'my-choose-compiler)"
  :type '(choice string function)
  :group 'compile-c++)

(defcustom c++-compiler-varenv "CXX"
  "Varenv indicating the C++ compiler to use."
  :type 'string
  :group 'compile-c++)

(defcustom c++-cflags-varenv "CXXFLAGS"
  "Varenv indicating the C++ compiler flags to use."
  :type 'string
  :group 'compile-c++)

(defcustom c++-source-ext-list '( "cc" "C" "CC" "cpp" "cxx" "c++" "c+" )
  "Extensions for C++ compileable source files."
  :type '(repeat string)
  :group 'compile-c++)

(defcustom c++-headers-ext-list '( "H" "hh" "HH" "h++" "h+" "h" "hpp" "hxx" )
  "Extensions for C++ headers source files."
  :type '(repeat string)
  :group 'compile-c++)

(defcustom c++-default-compiler-options "-g"
  "*Default options to give to the C++ compiler.
This could be any form evaluating to a string.  See
`mode-compile-choosen-compiler' variable."
  :type '(choice
          string
          (sexp :tag "Form evaluating to a string"))
  :group 'compile-c++)

(defcustom c++-source-file-ext-regexp "\\.\\(cc\\|CC?\\|c\\+\\+?\\|cpp\\|cxx\\)"
  "Regexp to find, from it's name, if a C++ file is compileable."
  :type 'regexp
  :group 'compile-c++)

(defcustom c++-build-output-args t
  "Build output-args for c++-mode."
  :type 'boolean
  :group 'compile-c++)

(defcustom c++-object-file-ext "o"
  "Extension of objects file (result of compilation)
in c++ mode."
  :type 'string
  :group 'compile-c++)



;; @@ ada-mode compile variables ;;;
(defgroup compile-ada nil
  "Ada compilation options"
  :group 'compilation-lang)

(defcustom ada-compilers-list
  '( "gcc" "gnat" "ada" )
  "List of user's favourites Ada compilers in order of preferencies."
  :type '(repeat (string :tag "Ada Compiler name"))
  :group 'compile-ada)

(defcustom ada-companion-file-regexp ""
  "Regexp to find associated compileable Ada companion file from a spec file.

This is useless in Ada because there do not exists uncompileable files."
  :type 'regexp
  :group 'compile-ada)

(defcustom ada-default-compiler "ada"
  "*Default Ada compiler to use when everything else fails.

This could be any form evaluating to a string, so you could map it to
a function asking you interactively to choose the compiler.

example:
 (defun my-choose-compiler()
   (read-string \"Ada compiler: \"))
 (setq ada-default-compiler 'my-choose-compiler)"
  :type '(choice string function)
  :group 'compile-ada)

(defcustom ada-compiler-varenv "ADA"
  "Varenv indicating the Ada compiler to use."
  :type 'string
  :group 'compile-ada)

(defcustom ada-aflags-varenv "AFLAGS"
  "Varenv indicating the Ada compiler flags to use."
  :type 'string
  :group 'compile-ada)

(defcustom ada-source-ext-list '( "ads" "adb" "ada" "a" )
  "Extensions for Ada compileable source files."
  :type '(repeat string)
  :group 'compile-ada)

(defcustom ada-headers-ext-list '( "ads" "ada" "a" )
  "Extensions for Ada spec source files."
  :type '(repeat string)
  :group 'compile-ada)

(defcustom ada-default-compiler-options "-g"
  "*Default options to give to the Ada compiler.

This could be any form evaluating to a string.  See
`mode-compile-choosen-compiler' variable."
  :type '(choice
          string
          (sexp :tag "Form evaluating to a string"))
  :group 'compile-ada)

(defcustom ada-source-file-ext-regexp "\\.\\(ad[abs]\\|a\\)"
  "Regexp to find, from it's name, if an Ada file is compileable.

This is useless in Ada because there do not exists uncompileable files."
  :type 'regexp
  :group 'compile-ada)

(defcustom ada-build-output-args t
  "Build output-args for ada-mode."
  :type 'boolean
  :group 'compile-ada)

(defcustom ada-object-file-ext "o"
  "Extension of objects file (result of compilation)
in ada mode."
  :type 'string
  :group 'compile-ada)



;; @@ fortran-mode compile variables ;;;
(defgroup compile-fortran nil
  "Fortran compilation options"
  :group 'compilation-lang)

(defcustom f77-compilers-list '( "f77" "fc" )
  "List of user's favourite Fortran compilers in order of preferencies."
  :type '(repeat (string :tag "C Compiler name"))
  :group 'compile-fortran)

(defcustom f77-companion-file-regexp "\\(_[Pp]\\)?\\.[pP]?inc"
  "Regexp to find associated .f file from a .inc."
  :type 'regexp
  :group 'compile-fortran)

(defcustom f77-default-compiler "f77"
  "*Default fortran compiler to use when everything else fails..

This could be any form evaluating to a string, so you could map it to
a function asking you interactively to choose the compiler.

example:
 (defun my-choose-compiler()
   (read-string \"Fortran compiler: \"))
 (setq f77-default-compiler 'my-choose-compiler)"
  :type '(choice string function)
  :group 'compile-fortran)

(defcustom f77-compiler-varenv "F77"
  "Varenv indicating the fortran compiler to use."
  :type 'string
  :group 'compile-fortran)

(defcustom f77-cflags-varenv "FCOPTS"
  "Varenv indicating the fortran compiler flags to use."
  :type 'string
  :group 'compile-fortran)

(defcustom f77-source-ext-list '( "f" "F" "for" "For" )
  "Extensions for fortran compileable source files."
  :type '(repeat string)
  :group 'compile-fortran)

(defcustom f77-headers-ext-list '( "inc" "h")
  "Extensions for fortran include files."
  :type '(repeat string)
  :group 'compile-fortran)

(defcustom f77-default-compiler-options "-w66 -a"
  "*Default options to give to the fortran compiler.

This could be any form evaluating to a string.  See
`mode-compile-choosen-compiler' variable."
  :type '(choice
          string
          (sexp :tag "Form evaluating to a string"))
  :group 'compile-fortran)

(defcustom f77-source-file-ext-regexp "\\.\\([Ff]\\|for\\)"
  "Regexp to find, from it's name, if a fortran file is compileable."
  :type 'regexp
  :group 'compile-fortran)

(defcustom f77-build-output-args t
  "Build output-args for f77-mode."
  :type 'boolean
  :group 'compile-fortran)

(defcustom f77-object-file-ext "o"
  "Extension of objects file (result of compilation)
in Fortran mode."
  :type 'string
  :group 'compile-fortran)



;; @@ sh-mode compile variables ;;;
(defgroup compile-sh nil
  "Sh (Bourne Shell scripts) compilation options"
  :group 'compilation-script)

(defcustom sh-command "sh"
  "Command to run sh scripts"
  :type 'string
  :group 'compile-sh)

(defcustom sh-dbg-flags "-fvx"
  "*Flags to give to sh for debugging a Bourne Shell script.

The -f flag must always be present."
  :type 'string
  :group 'compile-sh)

(defvar sh-compilation-error-regexp-alist nil
  ;; I'd never seen a Bourne shell returning file+line where a syntax
  ;; error occured.
  "Alist that specifies how to match errors in sh output.

See variable compilation-error-regexp-alist for more details.")


;; @@ csh-mode compile variables ;;;
(defgroup compile-csh nil
  "Csh (C Shell) compilation options"
  :group 'compilation-script)

(defcustom csh-command "csh"
  "Command to run csh scripts"
  :type 'string
  :group 'compile-csh)

(defcustom csh-dbg-flags "-fVX"
  "*Flags to give to csh for debugging a C Shell script.

The -f flag must always be present."
  :type 'string
  :group 'compile-csh)

(defvar csh-compilation-error-regexp-alist nil
  ;; I'd never seen a C shell returning file+line where a syntax
  ;; error occured.
  "Alist that specifies how to match errors in csh output.

See variable compilation-error-regexp-alist for more details.")


;; @@ zsh-mode compile variables ;;;
(defgroup compile-zsh nil
  "Zsh (Z Shell scripts) compilation options"
  :group 'compilation-script)

(defcustom zsh-command "zsh"
  "Command to run zsh scripts"
  :type 'string
  :group 'compile-zsh)

(defcustom zsh-dbg-flags "-nCvx"
  "*Flags to give to zsh for debugging a Z Shell script."
  :type 'string
  :group 'compile-zsh)

(defvar zsh-compilation-error-regexp-alist nil
  ;; I'd never seen a Z shell returning file+line where a syntax
  ;; error occured.
  "Alist that specifies how to match errors in csh output.

See variable compilation-error-regexp-alist for more details.")


;; @@ tcl-mode compile variables - JWH ;;;
(defgroup compile-tcl nil
  "Tcl compilation options"
  :group 'compilation-script)

(defcustom tcl-command "wish"
  "Command to run tcl scripts"
  :type 'string
  :group 'compile-tcl)

(defcustom tcl-dbg-flags ""
  "*Flags to give to tcl -- none."
  :type 'string
  :group 'compile-tcl)

(defvar tcl-compilation-error-regexp-alist
  ;; TK  (file "/directory-path/filename.tcl" line XY)
  '(
    ("file \"\\([^ ]+\\)\" line \\([0-9]+\\)[)]" 1 2)
    )
  "Alist that specifies how to match errors in tcl output.

See variable compilation-error-regexp-alist for more details.")


;; @@ python-mode compile variables - BM ;;;
(defgroup compile-python nil
  "Python compilation options"
  :group 'compilation-script)

(defcustom python-command "python"
  "Command to run python scripts"
  :type 'string
  :group 'compile-python)

(defcustom python-dbg-flags ""
  "*Flags to give to python -- none."
  :type 'string
  :group 'compile-python)

(defvar python-compilation-error-regexp-alist
  ;; TK  (file "/directory-path/filename.tcl" line XY in ZZZ)
  '(
    ("File \"\\([^ ]+\\)\", line \\([0-9]+\\).*" 1 2)
    )
  "Alist that specifies how to match errors in python output.

See variable compilation-error-regexp-alist for more details.")


;; @@ perl-mode compile variables ;;;
(defgroup compile-perl nil
  "Perl compilation options"
  :group 'compilation-script)

(defcustom perl-command "perl"
  "Command to run perl."
  :type 'string
  :group 'compile-perl)

(defcustom perl-dbg-flags "-w"
  "*Flags to give to perl for debugging a Perl script."
  :type 'string
  :group 'compile-perl)

(defvar perl-compilation-error-regexp-alist
  ;; Contributed by Martin Jost
  '(
    ;; PERL 4
    ("in file \\([^ ]+\\) at line \\([0-9]+\\).*" 1 2)
    ;; PERL 5   Blubber at FILE line XY, <XY> line ab.
    ("at \\([^ ]+\\) line \\([0-9]+\\)," 1 2)
    ;; PERL 5   Blubber at FILE line XY.
    ("at \\([^ ]+\\) line \\([0-9]+\\)." 1 2)
    )
  ;; This look like a paranoiac regexp: could anybody find a better one? (which WORK).
  ;;'(("^[^\n]* \\(file\\|at\\) \\([^ \t\n]+\\) [^\n]*line \\([0-9]+\\)[\\.,]" 2 3))
  "Alist that specifies how to match errors in perl output.

See variable compilation-error-regexp-alist for more details.")


;; @@ ruby-mode compile variables ;;;
(defgroup compile-ruby nil
  "Ruby compilation options"
  :group 'compilation-script)

(defcustom ruby-command "ruby" 
  "Command to run ruby" 
  :type 'string 
  :group 'compile-ruby)

(defcustom ruby-dbg-flags "-w" 
  "Flags to give ruby for catching warnings" 
  :type 'string 
  :group 'compile-ruby)

(defvar ruby-compilation-error-regexp-alist
  '(
    ;; Unit Tests
    ("test[a-zA-Z0-9_]*([A-Z][a-zA-Z0-9_]*) \\[\\(.*\\):\\([0-9]+\\)\\]:" 1 2)
    ;; Errors and Warnings
    ("\\(.*?\\)\\([0-9A-Za-z_./\:-]+\\.rb\\):\\([0-9]+\\)" 2 3))
  "Alist that specifies how to match errors in ruby output.

See variable compilation-error-regexp-alist for more details.")


;; @@ emacs lisp compile variables ;;;

;;;###autoload
(defcustom emacs-lisp-byte-compile-dir-interactive-p t
  "*Non-nil means when byte-compiling a directory ask for each file
needing to be recompiled or not."
  :type 'boolean
  :group 'compilation-elisp)
(define-obsolete-variable-alias
  'mode-compile-byte-compile-dir-interactive-p
  'emacs-lisp-byte-compile-dir-interactive-p)

(defcustom emacs-lisp-sources-regexp
  (cond
   ((boundp 'emacs-lisp-file-regexp)
    emacs-lisp-file-regexp)
   (t
    "\\.el$"))
  "Regexp to find emacs lisp sources files."
  :type 'regexp
  :group 'compilation-elisp)

(defcustom emacs-lisp-bytecomp-ext "c"
  "Extension added to byte-compiled emacs sources files."
  :type 'string
  :group 'compilation-elisp)


;; @@ Misc declarations ;;;

;;;###autoload
(defconst mode-compile-version "2.28"
  "Current version of mode-compile package.

mode-compile.el,v 2.28 2003/04/01 13:52:47 boubaker Exp
Please send bugs-fixes/contributions/comments to boubaker@cena.fr")

(defconst mode-compile-help-address "heddy.Boubaker@cena.fr"
  "E-Mail address of mode-compile maintainer.")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; @ No user modifiable stuff below this line ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Save old compile function. In case someone will bound
;; mode-compile on 'compile.
(or (fboundp 'mc--compile-sav)
    (if (fboundp 'compile)
        (progn
          (fset 'mc--compile-sav (symbol-function 'compile))
          (put 'compile 'compile-saved-on-mc--compile-sav t))
      (error "`compile' function not known to be defined...")))

;; @@ Internals variables and constants ;;;

;; Mode specific
(defvar mc--comp-lst          nil) ; c-mode,c++-mode,ada-mode,fortran-mode
(defvar mc--def-comp          nil) ; itou
(defvar mc--compfile-regexp   nil) ; itou
(defvar mc--comp-varenv       nil) ; itou
(defvar mc--comp-options      nil) ; itou
(defvar mc--cflags-varenv     nil) ; itou
(defvar mc--source-ext-lst    nil) ; itou
(defvar mc--head-ext-lst      nil) ; itou
(defvar mc--source-ext-regexp nil) ; itou
(defvar mc--build-op-args     nil) ; itou
(defvar mc--outfile-ext       nil) ; itou

;; remote stuff
(defvar mc--efs-path-list       nil)
(defvar mc--remote-host         nil)
(defvar mc--remote-host-history nil)
(defvar mc--remote-username     nil)
(defvar mc--remote-command      nil)
(defvar mc--remote-pathname     nil)

;; Frames/Windows stuff
(defvar mc--other-frame nil)
(defvar mc--ws (or
                (and (fboundp 'console-type) (console-type))
                (and (fboundp 'device-type)  (device-type))
                window-system))

(defvar mc--compile-command nil)
;; Compile command used when no makefile has been found.
;; This variable is buffer local to keep history for read-string.
;; Unfortunately not a real history, keep only memory of
;; the last compile command used.
(make-variable-buffer-local 'mc--compile-command)

(defvar mc--kill-compile nil)
;; kill-compile command bound dynamically by `guess-compile'.
(make-variable-buffer-local 'mc--kill-compile)

(defvar mc--selected-makefile nil)
(defvar mc--selected-makefile-history nil)
;; User selected makefile among the list, to run make with.
;; This variable is buffer local to keep history for completing-read
;; Unfortunately not a real history, keep only memory of
;; the last makefile used.
(make-variable-buffer-local 'mc--selected-makefile)

(defvar mc--selected-makerule nil)
(defvar mc--selected-makerule-history nil)
;; User selected make rule to rebuild.
;; This variable is buffer local to keep history for completing-read
;; Unfortunately not a real history, keep only memory of
;; the last makerule used.
(make-variable-buffer-local 'mc--selected-makerule)

(defconst mc--find-C-main-regexp
  "^[ \t]*\\(int\\|void\\)?[ \t\n]*main[ \t\n]*\(+" )
;; Regexp to find the main() function in a C/C++ file

(defconst mc--makefile-rules-regexp
  "^\n*\\([^.$ \t#\n][^$ \t#\n:]*\\)[ \t]*:")
;; Regexp to extract makefiles rules.
;; But only those not containing references to $(VARIABLES)
;; and not starting with `.'

(defvar mc--makefile-rules nil)
;; List of all rules extracted from makefile
(make-variable-buffer-local 'mc--makefile-rules)

(defvar mc--mkfl-buffer-tick nil)
;; Tick counter for the buffer at the time of the rules extraction.
(make-variable-buffer-local 'mc--mkfl-buffer-tick)

(defvar mc--shell-args nil)
;; Shell arguments for the script to debug.
;; This variable is buffer local to keep history for read-string.
;; Unfortunately not a real history, keep only memory of
;; the last shell arguments used.
(make-variable-buffer-local 'mc--shell-args)

;; nil in GNU FSF Emacs, >= 0 in GNU Lucid Emacs/XEmacs
(defconst mc--lucid-emacs-p (or (string-match "Lucid"  emacs-version)
                                (string-match "XEmacs" emacs-version)))

;; @@ Internals functions and macros ;;;

(if (not (fboundp 'defsubst))
    ;; Emacs 18
    (fset 'defsubst (symbol-function 'defun)))

(defun mc--compile (compile-command)
  ;; Call compile with the compile command
  ;; but append the remote-command before
  (if (null mc--remote-command)
      ;; local compile
      (mc--compile-sav compile-command)
    ;; remote compile
    (let ((thisdir (expand-file-name (or default-directory "~"))))
      (mc--compile-sav
       (concat
        ;; The command to lauch remote commands
        mc--remote-command
        ;; Change to this buffer directory ...
        "'( cd " thisdir " ; "
        ;; then run the compile command
        compile-command " )'")))))

(defsubst mc--msg (msg &rest args)
  ;; Print MSG with ARGS and wait to let time to user
  ;; to read the message in minibuffer.
  (cond ((not mode-compile-expert-p)
         (apply 'message (concat "mode-compile: " msg) args)
         (sit-for mode-compile-reading-time))))

(cond
 ;; Check availaibles frames functions
 ((fboundp 'make-frame)
  ;; GNU Emacs
  (fset 'mc--make-frame         (symbol-function 'make-frame))
  (fset 'mc--select-frame       (symbol-function 'select-frame))
  (fset 'mc--frame-live-p       (symbol-function 'frame-live-p))
  (fset 'mc--make-frame-visible (symbol-function 'make-frame-visible))
  (fset 'mc--raise-frame        (symbol-function 'raise-frame)))
 ((fboundp 'make-screen)
  ;; XEmacs
  (fset 'mc--make-frame         (symbol-function 'make-screen))
  (fset 'mc--select-frame       (symbol-function 'select-screen))
  (fset 'mc--frame-live-p       (symbol-function 'screen-live-p))
  (fset 'mc--make-frame-visible (symbol-function 'make-screen-visible))
  (fset 'mc--raise-frame        (symbol-function 'raise-screen)))
 ((fboundp 'new-screen)
  ;; Lucid Emacs/obsolete
  (fset 'mc--make-frame         (symbol-function 'new-screen))
  (fset 'mc--select-frame       (symbol-function 'select-screen))
  (fset 'mc--frame-live-p       (symbol-function 'screen-live-p))
  (fset 'mc--make-frame-visible (symbol-function 'make-screen-visible))
  (fset 'mc--raise-frame        (symbol-function 'raise-screen))))

(defsubst mc--funcall (command &rest params)
  ;; Run command with params in another frame or not:
  ;; only if user ask for it and if window system is X
  ;; (maybe test window-system is set will be enought?).
  (cond ((and (eq mc--ws 'x)
              mode-compile-other-frame-p)
         ;; switch to another frame
         (mc--msg "Switching to another frame to compile...")
         (let ((buffer   (current-buffer))
               (win-attr (or mode-compile-frame-parameters-alist
                             mode-compile-default-frame-parameters))
               (frame   (cond ((fboundp 'mc--frame-live-p)
                                (if (mc--frame-live-p mc--other-frame)
                                    mc--other-frame
                                  nil))
                               (t
                                (mc--msg "Don't know how to check frame existence.")
                                nil))))
           (cond ((fboundp 'mc--make-frame)
                  (mc--select-frame (or frame
                                         (setq mc--other-frame
                                               (mc--make-frame win-attr))))
                  ;; I really don't understand why the 3 following
                  ;; are necessary (raise-frame must be enought?).
                  (mc--make-frame-visible mc--other-frame)
                  (mc--raise-frame        mc--other-frame)
                  (switch-to-buffer        buffer))
                 (t
                  (mc--msg "Don't know how to create a new frame."))))))
  ;; Just run the command with it's parameters
  (apply command params))

(defun mc--byte-compile-buffer()
  (if (fboundp 'byte-compile-buffer) (byte-compile-buffer)
    ;; No byte-compile-buffer
    ;; Save current-buffer in a temporary file and byte-compile it.
    (let ((tmp-file (concat (or (getenv "TMPDIR")
                                (concat mode-compile-dir-separator-char "tmp"))
                            mode-compile-dir-separator-char (make-temp-name "mc--"))))
      (save-restriction
        (widen)
        (write-region (point-min) (point-max) tmp-file)
        (condition-case err
            (byte-compile-file tmp-file)
          ;; handler
          (error (mc--msg "Failing to byte-compile %s, #error %s"
                          (buffer-name) err)))
        (delete-file tmp-file)
        (let ((elc-file (concat tmp-file emacs-lisp-bytecomp-ext)))
          (if (file-writable-p elc-file)
              (condition-case err
                  (delete-file elc-file)
                ;; handler
                (error (mc--msg "Failing to delete %s, #error %s"
                                elc-file err)))))
        (message nil))))) ; to clean minibuffer

(fset 'mc--member
      (if (fboundp 'member)
          (symbol-function 'member)
        ;; No member function
        (function
         (lambda (elt list)
           (catch 'elt-is-member
             (while list
               (if (equal elt (car list))
                   (throw 'elt-is-member list))
               (setq list (cdr list))))))))

(fset 'mc--run-hooks
      (if (fboundp 'run-hooks)
          (symbol-function 'run-hooks)
        ;; No run-hooks
        (function
         (lambda (hooklist)
           (mapcar '(lambda (x)
                      ;; report an error if x not a function
                      (funcall x))
                   hooklist)))))

(defsubst mc--read-string (prompt &optional initial-contents)
  ;; On Lucid Emacs I use compile-history as 3rd argument but
  ;; no history is possible with GNU emacs.
  (if mc--lucid-emacs-p
      (read-string prompt initial-contents 'compile-history)
    (read-string prompt initial-contents)))

(defmacro mc--eval (sym &optional arg)
  ;; Evaluate symbol
  (` (cond
      ((and (symbolp (, sym))
            (fboundp (, sym)))
       (funcall (, sym) (, arg)))
      (t
       (eval (, sym))))))

(defmacro mc--common-completion (alist)
  ;; Return the greatest common string for all
  ;; possible completions in alist.
  (` (try-completion "" (, alist))))

(defun mc--byte-recompile-files (files)
  ;; Byte recompile all FILES which are older than their
  ;; .elc files in the current directory
  (let ((tmp-fl files))
    (while (car-safe tmp-fl)
      (let* ((el-file  (car tmp-fl))
             (elc-file (concat el-file emacs-lisp-bytecomp-ext)))
        (mc--msg "Checking file %s ..." el-file)
        ;; is el-file newer than elc-file (if exists)
        (if (and (file-newer-than-file-p el-file elc-file)
                 (or (not emacs-lisp-byte-compile-dir-interactive-p)
                     (y-or-n-p (format "byte-recompile file %s? " el-file))))
            (condition-case err
                (byte-compile-file el-file)
              ;; handler
              (error (mc--msg "Failing to byte-compile %s, #error %s"
                              el-file err))))
        (setq tmp-fl (cdr-safe tmp-fl))))
    (mc--msg "All files processed")))

(defun mc--which (file)
  ;; Find an executable FILE in exec-path
  (if (not (stringp file))
      (error "mc--which: nil FILE arg"))
  (if mc--lucid-emacs-p
      ;; Some emacses don't have locate-file some have...
      ;; Lucid have it in standard, some others (GNU) have it
      ;; (add-on pkg) but the syntax is not always consistent...
;      (locate-file file exec-path nil 1)
      (locate-file file exec-path mode-compile-exe-file-ext 1)
    (let ((tmp-p-lst  exec-path)
          (found      nil)
          (file-found nil))
      (while (and (car-safe tmp-p-lst)
                  (not (setq found
                             (file-executable-p
                              (setq file-found
                                    (concat (car tmp-p-lst)
                                            mode-compile-dir-separator-char
                                            file
                                            mode-compile-exe-file-ext))))))
        (setq tmp-p-lst (cdr-safe tmp-p-lst)))
      (if found file-found nil))))

(defun mc--find-compiler ()
  ;; Find user's favourite mode compiler
  (mc--msg "Searching for your favourite %s compiler ..." mode-name)
  (let ((tmp-comp-lst mc--comp-lst)
        (compiler     nil))
    (or (getenv mc--comp-varenv)
        (progn
          (while (and tmp-comp-lst
                      (not (setq compiler
                                 (mc--which (car tmp-comp-lst)))))
            (setq tmp-comp-lst (cdr tmp-comp-lst)))
          (file-name-nondirectory (or compiler (mc--eval mc--def-comp)))))))

(defun mc--find-to-compile-file (&optional fname)
  ;; Find the name of the file to compile.
  (let ((file-name (or fname
                       (buffer-file-name)
                       (error "Compilation abort: Buffer %s has no filename."
                              (buffer-name))))
        (assoc-file nil)
        (found      nil)
        (pos        0))
    (cond
     ((string-match mc--source-ext-regexp file-name)
      ;; buffer is a compileable file
      (file-name-nondirectory file-name))

     ((setq pos (string-match mc--compfile-regexp file-name))
      ;; Buffer is not a compileable file, try to find associated
      ;; compileable file.
      (let ((tmp-ext-lst mc--source-ext-lst))
        (mc--msg "Looking for a compileable companion file for %s..."
                 (file-name-nondirectory file-name))
        (while (and tmp-ext-lst
                    (not (setq found
                               (file-readable-p
                                (setq assoc-file
                                      (concat
                                       (substring file-name 0 pos)
                                       "." (car tmp-ext-lst)))))))
          (setq tmp-ext-lst (cdr tmp-ext-lst))))
      (if found
          ;; A compileable companion source file found
          (file-name-nondirectory assoc-file)
        ;; No compileable companion source file found
        (mc--msg "Couldn't find any compileable companion file for %s ..."
                 (file-name-nondirectory file-name))
        nil))

     (t
      ;; Buffer has an unknown file extension
      ;; Could I be more cool?
      (error "Compilation abort: Don't know how to compile %s."
             (file-name-nondirectory file-name))))))

(defun mc--guess-compile-result-fname (infile)
  ;; Try to guess if outfile will be an object file or
  ;; an executable file by grepping for `main()' in INFILE.
  (let ((base-fname
         (substring infile 0
                    (string-match mc--source-ext-regexp infile))))
    (save-excursion
      ;; Create a temporary buffer containing infile contents
      (set-buffer (find-file-noselect infile))
      (save-excursion
        (save-restriction
          (widen)
          (goto-char (point-min))
          ;; Grep into tmp buffer for main function
          ;; THIS WILL NOT WORK FOR PROGRAMMING LANGAGES
          ;; WHICH ARE NOT C DIALECTS:
          ;; In non C-ish modes I hope this regexp will never be found :-(
          (if (re-search-forward mc--find-C-main-regexp (point-max) t)
              (concat base-fname mode-compile-exe-file-ext)
            (concat base-fname "." mc--outfile-ext)))))))

(defun mc--build-output-args (infile)
  ;; Build output arguments for compile command by scanning INFILE.
  (mc--msg "Looking into %s to build compile command ..." infile)
  (let ((out-file (mc--guess-compile-result-fname infile)))
    (concat (if (string-match
                 (concat "\\." mc--outfile-ext "$")
                 out-file)
                ;; outfile will be an object file
                " -c "
              ;; outfile will be an executable file
              " ")
            infile " -o " out-file )))

(defun mc--set-remote-cmd (remote-host &optional username pathname)
  ;; Check validity of remote-host or ask one to user
  ;; Then build the first part of the remote command
  (if (stringp remote-host)
      ;; Arg is the remote host name
      (let ((host-infos (assoc remote-host
                               mode-compile-remote-hosts-alist)))
        (setq mc--remote-host
              remote-host)
        (setq mc--remote-username
              (or username
                  (let ((usrnam (cdr host-infos)))
                    (if usrnam
                        (cond
                         ((stringp usrnam)
                          usrnam)
                         ((functionp usrnam)
                          ;; usrnam is a function call it with hostname arg
                          (funcall usrnam mc--remote-host))
                         (t
                          ;; What's that??
                          (mc--msg "%s is not a valid option using user-login-name" (pp-to-string usrnam))
                          (user-login-name)))
                      (user-login-name)))))
        (setq mc--remote-pathname pathname)
        ;; Add host to the user's list
        (or host-infos
            (setq mode-compile-remote-hosts-alist
                  (append (list (list remote-host))
                          mode-compile-remote-hosts-alist)))
        ;; Prepare the command
        (setq mc--remote-command
              (concat
               ;; "rsh host -l username"
               mode-compile-remote-execute-command           " "
               mode-compile-remote-execute-set-host-arg      " "
               mc--remote-host                               " "
               mode-compile-remote-execute-set-username-arg  " "
               mc--remote-username                           " "
               mode-compile-remote-execute-misc-args         " "
               mode-compile-remote-execute-set-command-arg   " "
                )))
    ;; Arg is: Ask to user then check
    (let ((rhost (completing-read
                  "Remote host to compile to: "
                  mode-compile-remote-hosts-alist
                  nil nil
                  ;; Initial contents
                  (or mc--remote-host
                      (car-safe (car-safe mode-compile-remote-hosts-alist)))
                  mc--remote-host-history)))
      (or (string= rhost "")
          (mc--set-remote-cmd rhost)))))

;(defmacro mc--makefile-test-p (makefile)
;  (` (and (, makefile)
;          (not (string-equal     (, makefile) ""))
;          (not (file-directory-p (, makefile)))
;          (file-readable-p       (, makefile)))))
(defun mc--makefile-test-p (makefile)
  (cond
   ((or (not makefile)
        (string-equal makefile ""))
    (mc--msg "Empty makefile selection")
    nil)
   ((file-directory-p makefile)
    (mc--msg "Makefile selection %s is a directory !!" makefile)
    nil)
   ((not (file-readable-p makefile))
    (mc--msg "Makefile %s unreadable" makefile)
    nil)
   (t)))


(if (not (fboundp 'buffer-modified-tick))
    (fset 'buffer-modified-tick
          ;; allways indicate modified
          (function
           (lambda()
             (if mc--mkfl-buffer-tick
                 (+ mc--mkfl-buffer-tick 1)
               1)))))

(defun  mc--get-makefile-rules (makefile)
  ;; Try to find if makefile's buffer has been modified
  ;; since last rule extraction
  (if (or (not mc--mkfl-buffer-tick)
          (not (equal mc--mkfl-buffer-tick
                      (buffer-modified-tick))))
      (save-excursion
        (save-restriction
          (widen)
          (goto-char (point-min))
          (setq mc--mkfl-buffer-tick (buffer-modified-tick))
          (setq mc--makefile-rules   nil)
          (mc--msg "Extracting rules from %s ..." makefile)
          ;; Grep into tmp buffer for makefile rules
          (while (re-search-forward mc--makefile-rules-regexp nil t)
            (let ((rule (buffer-substring
                         (match-beginning 1)
                         (match-end       1))))
              ;; add rule to list if it don't match the ignore regexp
              ;; and if not allready in rules list.
              (if (and
                   (or (not mode-compile-ignore-makerule-regexp)
                       (not (string-match
                             mode-compile-ignore-makerule-regexp
                             rule)))
                   (not (mc--member rule mc--makefile-rules)))
                  (setq mc--makefile-rules
                        (append mc--makefile-rules
                                (list rule))))))))
    (mc--msg "Rules had already been extracted from %s ..." makefile))
  ;; Always add an empty rule to allow `default' choice.
  (append mc--makefile-rules '([])))

(defun mc--makerule-completion (alist outfile &optional pref)
  ;; Return the makerule completion according to the prefered
  ;; default makerule
  (let ((preference (or pref
                        mode-compile-prefered-default-makerule)))
    (mc--msg "Prefered makerule choice is '%s" preference)
    (cond
     ((eq preference 'none)
      ;; Just show max common completion string to user
      (or (mc--common-completion alist) ""))

     ((eq preference 'all)
      ;; Find the all rule or 'none
      (if (assoc "all" alist) "all"
        (mc--makerule-completion alist outfile 'none)))

     ((eq preference 'file)
      ;; The out file is prefered or 'none
      (or outfile (mc--makerule-completion alist outfile 'none)))

     ((eq preference 'default)
      ;; Find the default rule or ""
      (if (assoc "default" alist) "default" ""))

     (t
      ;; Invalid preference return 'none
      (mc--msg "Invalid `mode-compile-prefered-default-makerule': '%s"
               mode-compile-prefered-default-makerule)
      (mc--makerule-completion alist outfile 'none)))))

(defun mc--choose-makefile-rule (makefile &optional outfile)
  ;; Choose the makefile rule and set it makefile local
  (save-excursion
    ;; Switch to makefile buffer
    (set-buffer (find-file-noselect makefile))
    (setq mc--selected-makerule
          ;; Add the name of the out file to the makefile
          ;; rules list if not allready in.
          (let* ((mk-rules-alist (mc--get-makefile-rules makefile))
                 (choices        (mapcar '(lambda (x) (list x))
                                         (if (or (not outfile)
                                                 (mc--member outfile
                                                             mk-rules-alist))
                                             mk-rules-alist
                                           (append mk-rules-alist
                                                   (list outfile))))))
            (completing-read
             (if mode-compile-expert-p
                 "Make rule: "
               "Using `make', enter rule to rebuild ([TAB] to complete): ")
             choices
             nil nil
             ;; initial contents
             (or mc--selected-makerule
                 (mc--makerule-completion choices outfile
                                          (if outfile 'file)))
             mc--selected-makerule-history
             )))))

(defmacro mc--cleanup-makefile-list (makefile-list)
  ;; Remove unusable and/or backups makefiles from list
  (` (let ((newlist))
       (mapcar
        '(lambda (x)
           (if (and (mc--makefile-test-p x)
                    (or (not mode-compile-ignore-makefile-backups)
                        (not (string-match
                              mode-compile-makefile-backups-regexp
                              x))))
               (setq newlist (cons x newlist))
             (mc--msg "Removing makefile \"%s\" from completion list"
                      x)))
        (, makefile-list))
       newlist)))

(defun mc--makefile-to-use (&optional directory)
  ;; Find the makefile to use in the current directory
  (let ((makefile-list (mc--cleanup-makefile-list
                        (directory-files
                         ;; I do not use the 5th parameter in Lucid Emacs
                         ;; to be compatible with GNU Emacs which accept
                         ;; only 4 parameters - no NOSORT -.
                         (or directory default-directory)
                         nil mode-compile-makefile-regexp t))))
    (cond
     ((not makefile-list)
      ;; No makefile found
      nil)

     ((and (not (cdr-safe makefile-list))
           (mc--makefile-test-p (car makefile-list)))
      ;; Only one valid makefile
      (car makefile-list))

     (t
      ;; Many makefiles in directory ask user to select one
      (let ((choices  (mapcar
                       '(lambda (x) (list x))
                       makefile-list))
            (makefile nil))
        (while
            ;; While the makefile do not pass the test.
            (not (mc--makefile-test-p
                  (setq makefile
                        (completing-read
                         (if mode-compile-expert-p
                             "Makefile: "
                           "Using `make', select makefile to use ([TAB] to complete): ")
                         choices
                         nil t
                         ;; initial contents
                         (or mc--selected-makefile
                             (mc--common-completion choices))
                         mc--selected-makefile-history
                         )))))
        makefile)))))

(defun mc--set-command (&optional file)
  ;; Return a compilation command, built according to the existence
  ;; of a makefile or not, to compile FILE .
  (setq completion-ignore-case nil) ; let completion be case sensitive
  (let ((to-compile-fname (or file (mc--find-to-compile-file))))
    (if (setq mc--selected-makefile (mc--makefile-to-use))
        (progn
          ;; A makefile found in the directory:
          ;; using make to compile
          (let ((out-fname (if to-compile-fname
                               (mc--guess-compile-result-fname
                                to-compile-fname)
                             nil)))
            ;; build make command by asking rule to user
            (concat mode-compile-make-program " "
                    (or (mc--eval mode-compile-make-options) "")
                    " -f " mc--selected-makefile " "
                    (mc--choose-makefile-rule
                     mc--selected-makefile out-fname))))
      ;; else
      ;; No makefile: build compile command asking  for confirmation to user.
      ;; Should this be replaced by the creation of a makefile (and then
      ;; running it) as rms proposed me?
      (or mc--compile-command
          (setq mc--compile-command
                (concat (setq mode-compile-choosen-compiler
                              (mc--find-compiler)) " "
                        (or (getenv mc--cflags-varenv)
                            (mc--eval mc--comp-options))
                        (if to-compile-fname
                            (if mc--build-op-args
				(mc--build-output-args to-compile-fname)
			      (concat " " to-compile-fname)
			      )
                          " "))))
      (if (not mode-compile-never-edit-command-p)
          (setq mc--compile-command
                (mc--read-string
                 (if mode-compile-expert-p
                     "Compile command: "
                   (if to-compile-fname
                       (format "Edit command to compile %s: "
                               to-compile-fname)
                     "Edit compile command: " ))
                 mc--compile-command))
        mc--compile-command))))

(defun mc--shell-compile (shell dbgflags &optional errors-regexp-alist)
  ;; Run SHELL with debug flags DBGFLAGS on current-buffer
  (let* ((shcmd   (or (mc--which shell)
                      (error "Compilation abort: command %s not found" shell)))
         (shfile  (or mc--remote-pathname (buffer-file-name)
                      (error "Compilation abort: Buffer %s has no filename"
                             (buffer-name))))
         (run-cmd (concat shcmd " " dbgflags " " shfile " "
                          (setq mc--shell-args
                                (read-string (if mode-compile-expert-p
                                                 "Argv: "
                                               (format "Arguments to %s %s script: "
                                                       shfile shell))
                                             mc--shell-args)))))
    ;; Modify compilation-error-regexp-alist if needed
    (if errors-regexp-alist
        (progn
          ;; Set compilation-error-regexp-alist from compile
          (or (listp errors-regexp-alist)
              (error "Compilation abort: In mc--shell-compile errors-regexp-alist not a list."))
          ;; Add new regexp alist to compilation-error-regexp-alist
          (mapcar '(lambda(x)
                     (if (mc--member x compilation-error-regexp-alist) nil
                       (setq compilation-error-regexp-alist
                             (append (list x)
                                     compilation-error-regexp-alist))))
                  errors-regexp-alist)))
    ;; Run compile with run-cmd
    (mc--compile run-cmd)))

(defmacro mc--assq-get-fcomp (asq)
  ;; Return compile-function associated to ASQ
  (` (let* ((mode  (cdr  (, asq)))
            (massq (assq mode mode-compile-modes-alist)))
       (if massq (car-safe (cdr massq))))))

(defmacro mc--assq-get-fkill (asq)
  ;; Return kill-compile-function associated to ASQ
  (` (let* ((mode  (cdr  (, asq)))
            (massq (assq mode mode-compile-modes-alist)))
       (if massq (car-safe (cdr-safe (cdr massq)))))))

(defun mc--lookin-for-shell ()
  ;; Look into current-buffer to see if it is a shell script
  ;; and return function to compile it or nil.
  (mc--msg "Looking if buffer %s is a shell script..." (buffer-name))
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (if (looking-at "#![ \t]*/\\([^ \t\n]+/\\)\\([^ \t\n]+\\)")
          (let* ((shell-name (buffer-substring (match-beginning 2)
                                               (match-end       2)))
                 (shell-assq (assoc shell-name mode-compile-shell-alist)))
            (if shell-assq
                (progn
                  (mc--msg "Buffer is a %s script" shell-name)
                  (setq mc--kill-compile (mc--assq-get-fkill shell-assq))
                  (mc--assq-get-fcomp shell-assq))
              nil))))))

(defun mc--lookat-name ()
  ;; Lookat buffer file name to see if it can return a function
  ;; to compile it or nil.
  (mc--msg "Trying to guess compile command from buffer %s file name..."
           (buffer-name))
  (let ((fname (buffer-file-name)))
    (if (not fname) nil
      ;; try regexp from mode-compile-filename-regexp-alist
      (let ((tmp-al mode-compile-filename-regexp-alist)
            (found  nil))
        (while (and tmp-al (car tmp-al) (not found))
          ;; evaluate to string
          (let ((regxp (mc--eval (car (car tmp-al)))))
            (if (string-match regxp fname)
                (setq found (car tmp-al)))
            (setq tmp-al (cdr tmp-al))))
        (if (not found)
            nil
          (mc--msg "File %s matches regexp %s" fname (car found))
          (setq mc--kill-compile (mc--assq-get-fkill found))
          (mc--assq-get-fcomp found))))))


;; @ mode specific functions ;;;

(defun cc-compile ()
  "Run `compile' with a dynamically built command for `c-mode'.

The command is built depending of the existence of a makefile (which could
be specified by changing value of variable mode-compile-makefile-regexp) in
the current directory or not.
If no makefile is found try to run a C compiler on the file or it's companion.

See also variables:
 -- cc-compilers-list
 -- cc-default-compiler
 -- cc-companion-file-regexp
 -- cc-compiler-varenv
 -- cc-cflags-varenv
 -- cc-source-ext-list
 -- cc-headers-ext-list
 -- cc-source-file-ext-regexp"
  (setq
   mc--comp-lst          cc-compilers-list
   mc--def-comp          cc-default-compiler
   mc--compfile-regexp   cc-companion-file-regexp
   mc--comp-varenv       cc-compiler-varenv
   mc--comp-options      cc-default-compiler-options
   mc--cflags-varenv     cc-cflags-varenv
   mc--source-ext-lst    cc-source-ext-list
   mc--head-ext-lst      cc-headers-ext-list
   mc--source-ext-regexp cc-source-file-ext-regexp
   mc--build-op-args     cc-build-output-args
   mc--outfile-ext       cc-object-file-ext
   )
  (mc--compile (mc--set-command)))

(defun java-compile ()
  "Run `compile' with a dynamically built command for `java-mode'.

The command is built depending of the existence of a makefile (which could
be specified by changing value of variable mode-compile-makefile-regexp) in
the current directory or not.
If no makefile is found try to run a Java compiler on the file or it's
companion.

See also variables:
 -- java-compilers-list
 -- java-default-compiler
 -- java-companion-file-regexp
 -- java-compiler-varenv
 -- java-cflags-varenv
 -- java-source-ext-list
 -- java-headers-ext-list
 -- java-source-file-ext-regexp"
  (setq
   mc--comp-lst          java-compilers-list
   mc--def-comp          java-default-compiler
   mc--compfile-regexp   java-companion-file-regexp
   mc--comp-varenv       java-compiler-varenv
   mc--comp-options      java-default-compiler-options
   mc--cflags-varenv     java-cflags-varenv
   mc--source-ext-lst    java-source-ext-list
   mc--head-ext-lst      java-headers-ext-list
   mc--source-ext-regexp java-source-file-ext-regexp
   mc--build-op-args     java-build-output-args
   mc--outfile-ext       java-object-file-ext
   )
  (mc--compile (mc--set-command)))

(defun c++-compile ()
  "Run `compile' with a dynamically built command for `c++-mode'.

The command is built depending of the existence of a makefile (which could
be specified by changing value of variable mode-compile-makefile-regexp) in
the current directory or not.
If no makefile is found try to run a C++ compiler on the file or it's companion.

See also variables:
 -- c++-compilers-list
 -- c++-default-compiler
 -- c++-companion-file-regexp
 -- c++-compiler-varenv
 -- c++-cflags-varenv
 -- c++-source-ext-list
 -- c++-headers-ext-list
 -- c++-source-file-ext-regexp"
  (setq
   mc--comp-lst          c++-compilers-list
   mc--def-comp          c++-default-compiler
   mc--compfile-regexp   c++-companion-file-regexp
   mc--comp-varenv       c++-compiler-varenv
   mc--comp-options      c++-default-compiler-options
   mc--cflags-varenv     c++-cflags-varenv
   mc--source-ext-lst    c++-source-ext-list
   mc--head-ext-lst      c++-headers-ext-list
   mc--source-ext-regexp c++-source-file-ext-regexp
   mc--build-op-args     c++-build-output-args
   mc--outfile-ext       c++-object-file-ext
   )
  (mc--compile (mc--set-command)))


(defun ada-compile ()
  "Run `compile' with a dynamically built command for `ada-mode'.

The command is built depending of the existence of a makefile (which could
be specified by changing value of variable mode-compile-makefile-regexp) in
the current directory or not.
If no makefile is found try to run an Ada compiler on the file.

See also variables:
 -- ada-compilers-list
 -- ada-default-compiler
 -- ada-companion-file-regexp
 -- ada-compiler-varenv
 -- ada-aflags-varenv
 -- ada-source-ext-list
 -- ada-headers-ext-list
 -- ada-source-file-ext-regexp)"
  (setq
   mc--comp-lst          ada-compilers-list
   mc--def-comp          ada-default-compiler
   mc--compfile-regexp   ada-companion-file-regexp
   mc--comp-varenv       ada-compiler-varenv
   mc--comp-options      ada-default-compiler-options
   mc--cflags-varenv     ada-aflags-varenv
   mc--source-ext-lst    ada-source-ext-list
   mc--head-ext-lst      ada-headers-ext-list
   mc--source-ext-regexp ada-source-file-ext-regexp
   mc--build-op-args     ada-build-output-args
   mc--outfile-ext       ada-object-file-ext
   )
  (mc--compile (mc--set-command)))


(defun f77-compile ()
  "Run `compile' with a dynamically built command for `fortran-mode'.

The command is built depending of the existence of a makefile (which could
be specified by changing value of variable mode-compile-makefile-regexp) in
the current directory or not.
If no makefile is found try to run a Fortran compiler on the file or it's companion..

See also variables:
 -- f77-compilers-list
 -- f77-default-compiler
 -- f77-companion-file-regexp
 -- f77-compiler-varenv
 -- f77-cflags-varenv
 -- f77-source-ext-list
 -- f77-headers-ext-list
 -- f77-source-file-ext-regexp)"
  (setq
   mc--comp-lst          f77-compilers-list
   mc--def-comp          f77-default-compiler
   mc--compfile-regexp   f77-companion-file-regexp
   mc--comp-varenv       f77-compiler-varenv
   mc--cflags-varenv     f77-cflags-varenv
   mc--comp-options      f77-default-compiler-options
   mc--source-ext-lst    f77-source-ext-list
   mc--head-ext-lst      f77-headers-ext-list
   mc--source-ext-regexp f77-source-file-ext-regexp
   mc--build-op-args     f77-build-output-args
   mc--outfile-ext       f77-object-file-ext
   )
  (mc--compile (mc--set-command)))


(defun elisp-compile ()
  "Run `byte-compile' on the current Emacs lisp buffer.
For `emacs-lisp-mode' and `lisp-interaction-mode'.

Produce a `.elc' file if possible or `byte-compile' only the buffer."
  (let ((comp-file (or (buffer-file-name) "")))
    (if (string-match emacs-lisp-sources-regexp comp-file)
        (progn
          (mc--msg "Byte compiling file %s ..." comp-file)
          (byte-compile-file comp-file))
      (mc--msg "Byte compiling buffer %s #No .elc produced ..." (buffer-name))
      (mc--byte-compile-buffer))))


(defun makefile-compile (&optional makefile)
  "Run `make' on the current-buffer (`makefile-mode').

The user is prompted for a selection of make rules to build."
  (let ((mkfile (or makefile (buffer-file-name)
                    (error
                     "Compilation abort: buffer %s has no file name"
                     (buffer-name)))))
    (setq mc--selected-makefile mkfile)
    (setq mc--compile-command
          (concat mode-compile-make-program " "
                  (or (mc--eval mode-compile-make-options) "")
                  " -f " mkfile " "
                  (mc--choose-makefile-rule mkfile))))
    (mc--compile mc--compile-command))


(defun dired-compile ()
  "Run `make' if a Makefile is present in current directory (`dired-mode').

The user is prompted for a selection of a makefile to choose if many
matching `mode-compile-makefile-regexp' are present in the directory and
for the make rules to build. If directory contain no makefile the function
try to find if there are some un-byte-compiled .el files and recompile them
if needed.
Ask for the complete `compile-command' if no makefile and no .el files found."
  (let ((makefile (mc--makefile-to-use)))
    (if makefile
        ;; Makefile exists compile with it
        (makefile-compile makefile)
      ;; No makefile found look for some .el files
      (mc--msg "No makefile found, looking for .el files ...")
      (let ((el-files (directory-files
                       default-directory nil emacs-lisp-sources-regexp)))
        (if el-files
            ;; Some .el files found byte-recompile them
            (mc--byte-recompile-files el-files)
          ;; No .el files ask compile command to user
          (mc--msg "No .el files found in directory %s" default-directory)
          (default-compile))))))


(defun sh-compile ()
  "Run `sh-command' (Bourne Shell) with `sh-dbg-flags' on current-buffer (`sh-mode').

User is prompted for arguments to run his sh program with.
If you want to step throught errors set the variable `sh-compilation-error-regexp-alist'
to a value understandable by compile's `next-error'.
See variables compilation-error-regexp-alist or sh-compilation-error-regexp-alist."
  (mc--shell-compile sh-command sh-dbg-flags sh-compilation-error-regexp-alist))


(defun csh-compile ()
  "Run `csh-command' (C Shell) with `csh-dbg-flags' on current-buffer (`csh-mode').

User is prompted for arguments to run his csh program with.
If you want to step throught errors set the variable `csh-compilation-error-regexp-alist'
to a value understandable by compile's `next-error'.
See variables compilation-error-regexp-alist or csh-compilation-error-regexp-alist."
  (mc--shell-compile csh-command csh-dbg-flags csh-compilation-error-regexp-alist))


(defun zsh-compile ()
  "Run `zsh-command' (Z Shell) with `zsh-dbg-flags' on current-buffer (`zsh-mode').

User is prompted for arguments to run his zsh program with.
If you want to step throught errors set the variable `zsh-compilation-error-regexp-alist'
to a value understandable by compile's `next-error'.
See variables compilation-error-regexp-alist or zsh-compilation-error-regexp-alist."
  (mc--shell-compile zsh-command zsh-dbg-flags zsh-compilation-error-regexp-alist))


(defun perl-compile ()
  "Run Perl with `perl-dbg-flags' on current-buffer (`perl-mode').

User is prompted for arguments to run his perl program with.
If you want to step throught errors set the variable `perl-compilation-error-regexp-alist'
to a value understandable by compile's `next-error'.
See variables compilation-error-regexp-alist or perl-compilation-error-regexp-alist."
  (mc--shell-compile perl-command perl-dbg-flags perl-compilation-error-regexp-alist))


(defun tcl-compile ()
  ;; JWH
  "Run `tcl-command' with `tcl-dbg-flags' on current-buffer (`tcl-mode').

User is prompted for arguments to run his Tcl/Tk program with.
If you want to step throught errors set the variable `tcl-compilation-error-regexp-alist'
to a value understandable by compile's `next-error'.
See variables compilation-error-regexp-alist or tcl-compilation-error-regexp-alist."
  (mc--shell-compile tcl-command tcl-dbg-flags tcl-compilation-error-regexp-alist))


(defun python-compile ()
  ;; BM
  "Run `python-command' with `python-dbg-flags' on current-buffer (`python-mode').

User is prompted for arguments to run his Python program with.
If you want to step throught errors set the variable `python-compilation-error-regexp-alist'
to a value understandable by compile's `next-error'.
See variables compilation-error-regexp-alist or python-compilation-error-regexp-alist."
  (mc--shell-compile python-command python-dbg-flags python-compilation-error-regexp-alist))

(defun ruby-compile ()
  ;; CLGC
  "Run `ruby-command' with `ruby-dbg-flags' on current-buffer (`ruby-mode').

User is prompted for arguments to run their ruby program with.
If you want to step throught errors set the variable `ruby-compilation-error-regexp-alist'
to a value understandable by compile's `next-error'.
See variables compilation-error-regexp-alist or ruby-compilation-error-regexp-alist."
  (mc--shell-compile ruby-command ruby-dbg-flags ruby-compilation-error-regexp-alist))

(defun default-compile ()
  "Default function invoked by `mode-compile' (\\[mode-compile])
when everything else failed.

Ask to user to edit `compile-command' and run `compile' (\\[compile]) with it."
  (setq mc--compile-command
        (mc--read-string
         (if mode-compile-expert-p
             "Compile command: "
           (format "Edit command to compile %s : " (buffer-name)))
         (or mc--compile-command compile-command)))
  (mc--compile mc--compile-command))

(defvar mc--makefile) ;; Just to avoid compiler warning
(defun guess-compile ()
  "Try to guess how to compile current-buffer.

When the compile command could not be extrapolated from major-mode this function
is called which try to guess from number of parameters which command to build.
The steps to guess which command to use to compile are:
  1st : Look into the file to check if it is a shell script
        See variable mode-compile-shell-alist
  2nd : Try to guess from the file name
        See variable mode-compile-filename-regexp-alist
  3rd : Look for a makefile in the current directory
        See variable mode-compile-makefile-regexp
  Last: Give up and ask user for the command to use
        See function default-compile"
  (mc--msg "Trying to guess how to compile buffer %s ..." (buffer-name))
  (let ((mc--makefile))
    (funcall
     (or
      ;; step 1
      (mc--lookin-for-shell)
      ;; step 2
      (mc--lookat-name)
      ;; step 3
      (progn
        (mc--msg "Looking for a makefile in current directory...")
        (if (setq mc--makefile (mc--makefile-to-use
                                (and (buffer-file-name)
                                     (file-name-directory (buffer-file-name)))))
            (progn
              (setq mc--kill-compile 'kill-compilation)
              ;; Byte-compiling says `makefile' is not referenced.
              '(lambda () (makefile-compile mc--makefile)))))
      ;; step 4
      (progn
        (mc--msg "Don't know how to compile %s, giving up..."
                 (buffer-name))
        (setq mc--kill-compile 'kill-compilation)
        'default-compile)))))


;; @ user accessible/exported function ;;;

;; get reporter-submit-bug-report when byte-compiling
(and (fboundp 'eval-when-compile)
     (eval-when-compile (require 'reporter)))

;;;###autoload
(defun mode-compile-submit-bug-report ()
  "*Submit via mail a bug report on mode-compile v2.27."
  (interactive)
  (and
   (y-or-n-p "Do you REALLY want to submit a report on mode-compile? ")
   (require 'reporter)
   (reporter-submit-bug-report
    mode-compile-help-address
    (concat "mode-compile " mode-compile-version)
    (list
     ;; Interesting mode-compile variables
     'mode-compile-modes-alist
     'mode-compile-filename-regexp-alist
     'mode-compile-shell-alist
     'mode-compile-makefile-regexp
     'mode-compile-make-program
     'mode-compile-default-make-options
     'mode-compile-make-options
     'mode-compile-reading-time
     'mode-compile-expert-p
     'mode-compile-never-edit-command-p
     'mode-compile-save-all-p
     'mode-compile-always-save-buffer-p
     'mode-compile-before-compile-hook
     'mode-compile-after-compile-hook
     'mode-compile-before-kill-hook
     'mode-compile-after-kill-hook
     'mode-compile-other-frame-p
     'mode-compile-other-frame-name
     'mode-compile-frame-parameters-alist
     'mode-compile-prefered-default-makerule
     'emacs-lisp-byte-compile-dir-interactive-p
     ;; others variables
     'features
     'compilation-error-regexp-alist
     'compile-command
     )
    nil
    nil
    "Dear Heddy,")))


;;;###autoload
(defun mode-compile (&optional remote-host)
  "*Compile the file in the current buffer with a dynamically built command.

The command is built according to the current major mode the function
was invoked from.

Running this command preceded by universal-argument (\\[universal-argument])
allows remote compilation, the user is prompted for a host name to run the
compilation command on.

Currently know how to compile in:
 `c-mode' ,              -- function cc-compile.
 `java-mode' ,           -- function java-compile.
 `c++-mode',             -- function c++-compile.
 `ada-mode',             -- function ada-compile.
 `fortran-mode',         -- function f77-compile.
 `emacs-lisp-mode'       -- function elisp-compile.
 `lisp-interaction-mode' -- function elisp-compile.
 `makefile-mode'         -- function makefile-compile.
 `dired-mode'            -- function dired-compile.
 `sh-mode'               -- function sh-compile.
 `csh-mode'              -- function csh-compile.
 `zsh-mode'              -- function zsh-compile.
 `perl-mode'             -- function perl-compile.
 `cperl-mode'            -- function perl-compile.
 `tcl-mode'              -- function tcl-compile.
 `python-mode'           -- function python-compile.
 `ruby-mode'             -- function ruby-compile.
 `fundamental-mode'      -- function guess-compile.
 `text-mode'             -- function guess-compile.
 `indented-text-mode'    -- function guess-compile.
 `compilation-mode'      -- function default-compile.
 The function `guess-compile' is called when mode is unknown.

The variable `mode-compile-modes-alist' contain description of known
modes.  The hooks variables `mode-compile-before-compile-hook' and
`mode-compile-after-compile-hook' are run just before and after
invoking the compile command of the mode.

Use the command `mode-compile-kill' (\\[mode-compile-kill]) to abort a
running compilation.

Bound on \\[mode-compile]."
  (interactive "P")
  ;; reinit
  (setq
   mc--efs-path-list  nil
   mc--remote-command nil)
  (if remote-host
      ;; Remote compilation asked
      ;; prepare remote command
      (mc--set-remote-cmd remote-host)
    ;; Not asked but check buffer-file-name to see
    ;; if it is not an efs file
    (setq mc--efs-path-list (and (fboundp 'efs-ftp-path)
                                 (buffer-file-name)
                                 (efs-ftp-path (buffer-file-name))))
    (and mc--efs-path-list (mc--set-remote-cmd
                            (car mc--efs-path-list)
                            (nth 1 mc--efs-path-list)
                            (nth 2 mc--efs-path-list))))
  (if (and mode-compile-always-save-buffer-p
           (buffer-file-name))
      ;; save-buffer allready check if buffer had been modified
      (save-buffer))
  (if mode-compile-save-all-p (save-some-buffers t))
  ;; Check if compile-command set as local variable
  (if (and
       (boundp 'compile-command)
       (local-variable-p 'compile-command (current-buffer))
       compile-command ; not null
       (if mc--compile-command
           (equal compile-command mc--compile-command)
         t)
       )
      ;; Just ask user and go
      (progn
        (mc--run-hooks 'mode-compile-before-compile-hook)
        (default-compile)
        (mc--run-hooks 'mode-compile-after-compile-hook)
        )
    ;; Here is the real work
    (let ((mode-elem (assq major-mode mode-compile-modes-alist)))
      (if mode-elem
          ;; known mode
          (progn
            (mc--msg (substitute-command-keys
                      "Compiling in %s mode ... \\[mode-compile-kill] to kill.")
                     mode-name)
            (mc--run-hooks 'mode-compile-before-compile-hook)
            ;; mc--funcall can launch the compilation
            ;; in another frame.
            (mc--funcall   (car (cdr mode-elem)))
            (mc--run-hooks 'mode-compile-after-compile-hook))
        ;; unknown mode: try to guess
        (mc--msg (substitute-command-keys
                  "Don't know how to compile in %s mode, guessing... \\[mode-compile-kill] to kill.")
                 mode-name)
        (mc--run-hooks 'mode-compile-before-compile-hook)
        ;; mc--funcall can launch the compilation
        ;; in another frame.
        (mc--funcall   'guess-compile)
        (mc--run-hooks 'mode-compile-after-compile-hook)))))

(provide 'mode-compile)


;;;###autoload
(defun mode-compile-kill()
  "*Kill the running compilation launched by `mode-compile' (\\[mode-compile]) \
command.

The compilation command is killed according to the current major mode
the function was invoked from.

Currently know how to kill compilations from:
 `c-mode' ,              -- function kill-compilation.
 `java-mode' ,           -- function kill-compilation.
 `c++-mode' ,            -- function kill-compilation.
 `ada-mode' ,            -- function kill-compilation.
 `fortran-mode' ,        -- function kill-compilation.
 `emacs-lisp-mode'       -- function keyboard-quit.
 `lisp-interaction-mode' -- function keyboard-quit.
 `makefile-mode'         -- function kill-compilation.
 `dired-mode'            -- function kill-compilation.
 `sh-mode'               -- function kill-compilation.
 `csh-mode'              -- function kill-compilation.
 `zsh-mode'              -- function kill-compilation.
 `perl-mode'             -- function kill-compilation.
 `cperl-mode'            -- function kill-compilation.
 `tcl-mode'              -- function kill-compilation.
 `python-mode'           -- function kill-compilation.
 `ruby-mode'             -- function kill-compilation.
 `fundamental-mode'      -- Bound dynamically.
 `text-mode'             -- Bound dynamically.
 `indented-text-mode'    -- Bound dynamically.
 `compilation-mode'      -- function kill-compilation.

The variable `mode-compile-modes-alist' contain description of ALL
known modes.  The hooks variables `mode-compile-before-kill-hook' and
`mode-compile-after-kill-hook' are run just before and after invoking
the kill compile command of the mode.

Bound on \\[mode-compile-kill]."
  (interactive)
  (let ((mode-elem (assq major-mode mode-compile-modes-alist)))
    (if mode-elem
        ;; known mode
        (progn
          (mc--run-hooks 'mode-compile-before-kill-hook)
          (mc--msg "Killing compilation in %s mode..." mode-name)
          (let ((killfun (or (car-safe (cdr (cdr mode-elem)))
                             mc--kill-compile
                             nil)))
            (if killfun
                ;; I don't call mc--funcall here caus' we don't need
                ;; to switch to another frame to kill a compilation.
                (funcall killfun)
              (mc--msg "Unable to kill compilation in %s mode..." mode-name))
            (mc--run-hooks 'mode-compile-after-kill-hook)))
      ;; unknown mode
      (mc--msg "Don't know how to kill compilation in %s mode"
               mode-name))))

(provide 'mode-compile-kill)



;;; Local variables:
;;; outline-regexp: ";; @+"
;;; eval: (outline-minor-mode 1)
;;; End:

;;; mode-compile.el ends here
