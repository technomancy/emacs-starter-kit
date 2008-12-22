;;; mumamo.el --- Multiple major modes in a buffer
;;
;; Author: Lennart Borgman (lennart O borgman A gmail O com)
;; Maintainer:
;; Created: Fri Mar 09 18:15:25 2007
(defconst mumamo:version "0.88") ;;Version:
;; Last-Updated: 2008-09-23T19:09:11+0200 Tue
;; URL: http://OurComments.org/Emacs/Emacs.html
;; Keywords:
;; Compatibility:
;;
;; Features that might be required by this library:
;;
;;   `backquote', `bytecomp', `cl', `flyspell', `ispell',
;;   `sgml-mode'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;;; Commentary:
;;
;; In some cases you may find that it is quite hard to write one major
;; mode that does everything for the type of file you want to handle.
;; That is the case for example for a PHP file where there comes
;; useful major modes with Emacs for the html parts, and where you can
;; get a major mode for PHP from other sources (see EmacsWiki for
;; Aaron Hawleys php-mode.el, or the very similar version that comes
;; with nXhtml).
;;
;; Using one major mode for the HTML part and another for the PHP part
;; sounds like a good solution.  But this means you want to use (at
;; least) two major modes in the same buffer.
;;
;; This file implements just that, support for MUltiple MAjor MOdes
;; (mumamo) in a buffer.
;;
;;
;;;; Usage:
;;
;; The multiple major mode support is turned on by calling special
;; functions which are used nearly the same way as major modes.  See
;; `mumamo-defined-turn-on-functions' for more information about those
;; functions.
;;
;; Each such function defines how to take care of a certain mix of
;; major functions in the buffer. We call them "multi major modes".
;;
;; You may call those functions directly (like you can with major mode
;; functions) or you may use them in for example `auto-mode-alist'.
;;
;; You can load mumamo in your .emacs with
;;
;;   (require 'mumamo-fun)
;;
;; or you can generate an autoload file from mumamo-fun.el
;;
;; Note that no multi major mode functions are defined in this file.
;; Together with this file comes the file mumamo-fun.el that defines
;; some such functions.  All those functions defined in that file are
;; marked for autoload.
;;
;;
;;
;; Thanks to Stefan Monnier for beeing a good and knowledgeable
;; speaking partner for some difficult parts while I was trying to
;; develop this.
;;
;; Thanks to RMS for giving me support and ideas about the programming
;; interface.  That simplified the code and usage quite a lot.
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;;; How to add support for a new mix of major modes
;;
;; This is done by creating a new function using
;; `define-mumamo-multi-major-mode'.  See that function for more
;; information.
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;;; Information for major mode authors
;;
;; There are a few special requirements on major modes to make them
;; work with mumamo:
;;
;; - fontification-functions should be '(jit-lock-function). However
;;   nxml-mode derivates can work too, see the code for more info.
;;
;; - narrowing should be respected during fontification and
;;   indentation when font-lock-dont-widen is non-nil.
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;;; Information for minor mode authors
;;
;; Some minor modes are written to be specific for the file edited in
;; the buffer and some are written to be specific for a major
;; modes.  Others are emulating another editor.  Those are probably
;; global, but might still have buffer local values.
;;
;; Those minor modes that are not meant to be specific for a major
;; mode should probably survive changing major mode in the
;; buffer.  That is mostly not the case in Emacs today.
;;
;; There are (at least) two type of values for those minor modes that
;; sometimes should survive changing major mode: buffer local
;; variables and functions added locally to hooks.
;;
;; * Some buffer local variables are really that - buffer local. Other
;;   are really meant not for the buffer but for the major mode or
;;   some minor mode that is local to the buffer.
;;
;;   If the buffer local variable is meant for the buffer then it is
;;   easy to make them survive changing major mode: just add
;;
;;    (put 'VARIABLE 'permanent-local t)
;;
;;   to those variables.  That will work regardless of the way major
;;   mode is changed.
;;
;;   If one only wants the variables to survive the major mode change
;;   that is done when moving between chunks with different major
;;   modes then something different must be used.  To make a variable
;;   survive this, but not a major mode change for the whole buffer,
;;   call any the function `mumamo-make-variable-buffer-permanent':
;;
;;     (mumamo-make-variable-buffer-permanent 'VARIABLE)
;;
;; * For functions entered to local hooks use this
;;
;;     (put 'FUNSYM 'permanent-local-hook t)
;;     (add-hook 'HOOKSYM 'FUNSYM nil t)
;;
;;   where HOOKSYM is the hook and FUNSYM is the function.
;;
;; * Some functions that are run in `change-major-mode' and dito
;;   after- must be avoided when mumamo changes major mode.  The
;;   functions to avoid should be listed in
;;
;;     `mumamo-change-major-mode-no-nos'
;;     `mumamo-after-change-major-mode-no-nos'
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;;; Comments on code etc:
;;
;; This is yet another way to try to get different major modes for
;; different chunks of a buffer to work.  (I borrowed the term "chunk"
;; here from multi-mode.el.)  I am aware of two main previous elisp
;; packages that tries to do this, multi-mode.el and mmm-mode.el.
;; (See http://www.emacswiki.org/cgi-bin/wiki/MultipleModes where
;; there are also some other packages mentioned.)  The solutions in
;; those are a bit different from the approach here.
;;
;; The idea of doing it the way mumamo does it is of course based on a
;; hope that switching major mode when moving between chunks should be
;; quick.  I found that it took from 0 - 62 000 ms, typically 0 - 16
;; 000 ms on a 3ghz cpu.  However unfortunately this is not the whole
;; truth.  It could take longer time, depending on what is run in the
;; hooks: The major mode specific hook, `after-change-major-mode-hook'
;; and `change-major-mode-hook'.
;;
;; Because it currently may take long enough time switching major mode
;; when moving between chunks to disturb smooth moving around in the
;; buffer I have added a way to let the major mode switching be done
;; after moving when Emacs is idle.  This is currently the default, but
;; see the custom variable `mumamo-set-major-mode-delay'.
;;
;; Since the intention is to set up the new major mode the same way as
;; it should have been done if this was a major mode for the whole
;; buffer these hooks must be run.  However if this idea is developed
;; further some of the things done in these hooks (like switching on
;; minor modes) could perhaps be streamlined so that switching minor
;; modes off and then on again could be avoided.  In fact there is
;; already tools for this in mumamo.el, see the section below named
;; "Information for minor mode authors".
;;
;; Another problem is that the major modes must use
;; `font-lock-fontify-region-function'.  Currently the only major
;; modes I know that does not do this are `nxml-mode' and its
;; derivatives.
;;
;; The indentation is currently working rather ok, but with the price
;; that buffer modified is sometimes set even though there are no
;; actual changes.  That seems a bit unnecessary and it could be
;; avoided if the indentation functions for the the various major
;; modes were rewritten so that you could get the indentation that
;; would be done instead of actually doing the indentation.  (Or
;; mumamo could do this better, but I do not know how right now.)
;;
;; See also "Known bugs and problems etc" below.
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;;; Known bugs:
;;
;; - See the various FIX-ME for possible bugs.  See also below.
;;
;;
;;;; Known problems and ideas:
;;
;; - There is no way in Emacs to tell a mode not to change
;;   fontification when changing to or from that mode.
;;
;; - The dividing into chunk is not that simple as I first thought.  I
;;   have not gone through the logic of this very carefully.  Perhaps
;;   that is needed.  The current logic is mainly in
;;   `mumamo-get-chunk-at' and `mumamo-find-possible-chunk'.  (Some
;;   other routines tries to behave like `mumamo-find-possible-chunk'
;;   too: `mumamo-chunk-attr=' and `mumamo-easy-make-chunk-fun'.)
;;
;; - One idea that I currently have not used is to check outer major
;;   mode while dividing into chunks.  This could probably be done
;;   with a small change to `mumamo-create-chunk-values-at'.  However
;;   `syntax-ppss' etc must also be handled a bit differently.
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;;; Change log:
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;;; Code:

(eval-when-compile (require 'cl))
(eval-when-compile (require 'flyspell))
(eval-when-compile (require 'sgml-mode)) ;; For sgml-xml-mode
;; For `define-globalized-minor-mode-with-on-off':
;;(require 'ourcomments-util)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Debugging etc

;; (defun dbg-smarty-err ()
;; ;;   (insert "}{")

;; ;;   (insert "}{")
;; ;;   (backward-char)
;; ;;   (backward-char)
;; ;;   (search-backward "}")

;;   ;; This gives an error rather often, but not always:
;;   (delete-char 3)
;;   (search-backward "}")
;;   )

;; (defun dbg-smarty-err2 ()
;;   (forward-char 5)
;;   (insert "}{")
;;   ;; Start in nxhtml part and make sure the insertion is in smarty
;;   ;; part.  Gives reliably an error if moved backward so point stay in
;;   ;; the new nxhtml-mode part, otherwise not.
;;   ;;
;;   ;; Eh, no.  If chunk family is changed and reset there is no more an
;;   ;; error.
;;   ;;
;;   ;; Seems to be some race condition, but I am unable to understand
;;   ;; how.  I believed that nxml always left in a reliable state.  Is
;;   ;; this a state problem in mumamo or nxml? I am unable to make it
;;   ;; happen again now.
;;   ;;
;;   ;; I saw one very strange thing: The error message got inserted in
;;   ;; the .phps buffer once.  How could this happen? Is this an Emacs
;;   ;; bug? Can't see how this could happen since it is the message
;;   ;; function that outputs the message.  A w32 race condition? Are
;;   ;; people aware that the message queue runs in parallell? (I have
;;   ;; tried to ask on the devel list, but got no answer at that time.)
;;   (backward-char 2)
;;   )


(defvar msgtrc-buffer
  "*Messages*"
  ;;"*trace-output*"
  "Doc"
  )

(defun msgtrc (format-string &rest args)
  ;;(apply 'message format-string args)
  (with-current-buffer msgtrc-buffer
    (goto-char (point-max))
    (insert (apply 'format format-string args))
    (insert "\n")
    )
  )

(defmacro mumamo-msgfntfy (format-string &rest args)
  "Give some messages during fontification.
This macro should just do nothing during normal use.  However if
there are any problems you can uncomment one of the lines in this
macro and recompile/reeval mumamo.el to get those messages.

You have to search the code to see where you will get them.  All
uses are in this file.

FORMAT-STRING and ARGS have the same meaning as for the function
`message'."
  ;;(list 'apply (list 'quote 'msgtrc) format-string (append '(list) args))
  ;;(list 'apply (list 'quote 'message) format-string (append '(list) args))
  ;;(list 'apply (list 'quote 'message) (list 'concat "%s: " format-string)
  ;;   (list 'get-internal-run-time) (append '(list) args))
  )
;;(mumamo-msgfntfy "my-format=%s" (get-internal-run-time))

(defmacro mumamo-msgindent (format-string &rest args)
  "Give some messages during indentation.
This macro should just do nothing during normal use.  However if
there are any problems you can uncomment one of the lines in this
macro and recompile/reeval mumamo.el to get those messages.

You have to search the code to see where you will get them.  All
uses are in this file.

FORMAT-STRING and ARGS have the same meaning as for the function
`message'."
  ;;(list 'apply (list 'quote 'msgtrc) format-string (append '(list) args))
  ;;(list 'apply (list 'quote 'message) format-string (append '(list) args))
  ;;(list 'apply (list 'quote 'message) (list 'concat "%s: " format-string)
  ;;   (list 'get-internal-run-time) (append '(list) args))
  )
(defvar mumamo-display-error-lwarn nil
  "Set to t to call `lwarn' on fontification errors.
If this is t then `*Warnings*' buffer will popup on fontification
errors.")
(defvar mumamo-display-error-stop nil
  "Set to t to stop fontification on errors.")

(defun mumamo-message-with-face (msg face)
  "Put MSG with face FACE in *Message* buffer."
  (let ((start (+ (with-current-buffer msgtrc-buffer
                    (point-max))
                  1))
        ;; This is for the echo area:
        (msg-with-face (propertize (format "%s" msg)
                                   'face face)))

    (msgtrc "%s" msg-with-face)
    ;; This is for the buffer:
    (with-current-buffer msgtrc-buffer
      (goto-char (point-max))
      (backward-char)
      (put-text-property start (point)
                         'face face))))

;;(run-with-idle-timer 1 nil 'mumamo-show-report-message)
(defun mumamo-show-report-message ()
  "Tell the user there is a long error message."
  (mumamo-message-with-face
   "MuMaMo error, please look in the *Message* buffer"
    'highlight))

;; This code can't be used now because `debugger' is currently not
;; useable in timers. I keep it here since I hope someone will make it
;; possible in the future.
;;
;; (defmacro mumamo-get-backtrace-if-error (bodyform)
;;   "Evaluate BODYFORM, return a list with error message and backtrace.
;; If there is an error in BODYFORM then return a list with the
;; error message and the backtrace as a string.  Otherwise return
;; nil."
;;   `(let* ((debugger
;;            (lambda (&rest debugger-args)
;;              (let ((debugger-ret (with-output-to-string (backtrace))))
;;                ;; I believe we must put the result in a buffer,
;;                ;; otherwise `condition-case' might erase it:
;;                (with-current-buffer (get-buffer-create "TEMP GET BACKTRACE")
;;                  (erase-buffer)
;;                  (insert debugger-ret)))))
;;           (debug-on-error t)
;;           (debug-on-signal t))
;;      (mumamo-condition-case err
;;          (progn
;;            ,bodyform
;;            nil)
;;        (error
;;         (let* ((errmsg (error-message-string err))
;;                (dbg1-ret
;;                 (with-current-buffer
;;                     (get-buffer "TEMP GET BACKTRACE") (buffer-string)))
;;                ;; Remove lines from this routine:
;;                (debugger-lines (split-string dbg1-ret "\n"))
;;               (dbg-ret (mapconcat 'identity (nthcdr 6 debugger-lines) "\n"))
;;                )
;;           (list errmsg (concat errmsg "\n" dbg-ret)))))))

;;(mumamo-display-error 'test-lwarn-type "testing 1=%s, 2=%s" "one" 'two)
(defun mumamo-display-error (lwarn-type format-string &rest args)
  "Display a message plus traceback in the *Message* buffer.
Use this for errors that happen during fontification or when
running a timer.

LWARN-TYPE is used as the type argument to `lwarn' if warnings
are displayed.  FORMAT-STRING and ARGS are used as the
corresponding arguments to `message' and `lwarn'.

All the output from this function in the *Message* buffer is
displayed with the highlight face.  After the message printed by
`message' is traceback from where this function was called.
Note: There is no error generated, just a traceback that is put
in *Message* as above.

Display an error message using `message' and colorize it using
the `highlight' face to make it more prominent.  Add a backtrace
colored with the `highlight' face to the buffer *Message*.  Then
display the error message once again after this so that the user
can see it.

If `mumamo-display-error-lwarn' is non-nil, indicate the error by
calling `lwarn'.  This will display the `*Warnings*' buffer and
thus makes it much more easy to spot that there was an error.

If `mumamo-display-error-stop' is non-nil raise an error that may
stop fontification."

  ;; Warnings are sometimes disturbning, make it optional:
  (when mumamo-display-error-lwarn
    (apply 'lwarn lwarn-type :error format-string args))

  (let ((format-string2 (concat "%s: " format-string))
        (bt (with-output-to-string (backtrace))))

    (mumamo-message-with-face
     (concat
      (apply 'format format-string2 lwarn-type args)
      "\n"
      (format "** In buffer %s\n" (current-buffer))
      bt)
     'highlight)

    ;; Output message once again so the user can see it:
    (apply 'message format-string2 lwarn-type args)
    ;; But ... there might be more messages so wait until things has
    ;; calmed down and then show a message telling that there was an
    ;; error and that there is more information in the *Messages*
    ;; buffer.
    (run-with-idle-timer 1 nil 'mumamo-show-report-message)

    ;; Stop fontifying:
    (when mumamo-display-error-stop
      ;;(font-lock-mode -1)
      (setq font-lock-mode nil)
      (when (timerp jit-lock-context-timer)
        (cancel-timer jit-lock-context-timer))
      (when (timerp jit-lock-defer-timer)
        (cancel-timer jit-lock-defer-timer))
      (apply 'error format-string2 lwarn-type args))))


(defun mumamo-debug-to-backtrace (&rest debugger-args)
  "This function should give a backtrace during fontification errors.
The variable `debugger' should then be this function.  See the
function `debug' for an explanation of DEBUGGER-ARGS.

Fix-me: Can't use this function yet since the display routines
uses safe_eval and safe_call."
  (mumamo-display-error 'mumamo-debug-to-backtrace
                        "%s"
                        (nth 1 debugger-args)))

;; (defun my-test-err3 ()
;;   (interactive)
;;   (let ((debugger 'mumamo-debug-to-backtrace)
;;         (debug-on-error t))
;;     (my-err)
;;     ))
;;(my-test-err3()

;;(set-default 'mumamo-use-condition-case nil)
;;(set-default 'mumamo-use-condition-case t)
(defvar mumamo-use-condition-case t)
(make-variable-buffer-local 'mumamo-use-condition-case)
(put 'mumamo-use-condition-case 'permanent-local t)

(defvar mumamo-debugger 'mumamo-debug-to-backtrace)
(make-variable-buffer-local 'mumamo-debugger)
(put 'mumamo-debugger 'permanent-local t)

(defmacro mumamo-condition-case (var body-form &rest handlers)
  "Like `condition-case', but optional.
If `mumamo-use-condition-case' is non-nil then do

  (condition-case VAR
      BODY-FORM
    HANDLERS).

Otherwise just evaluate BODY-FORM."
  (declare (indent 2) (debug t))
  `(if (not mumamo-use-condition-case)
       (let* ((debugger (or mumamo-debugger 'debug))
              (debug-on-error (if debugger t debug-on-error)))
         ,body-form)
    (condition-case ,var
        ,body-form
      ,@handlers)))

;; (defun my-test-err4 ()
;;   (interactive)
;;   (mumamo-condition-case err
;;       (my-errx)
;;     (arith-error (message "here"))
;;     (error (message "%s, %s" err (error-message-string err)))
;;     ))

(defvar mumamo-warned-once nil)
(make-variable-buffer-local 'mumamo-warned-once)
(put 'mumamo-warned-once 'permanent-local t)

; (append '(0 1) '(a b))
(defun mumamo-warn-once (type message &rest args)
  "Warn only once with TYPE, MESSAGE and ARGS.
If the same problem happens again then do not warn again."
  (let ((msgrec (append (list type message) args)))
    (unless (member msgrec mumamo-warned-once)
      (setq mumamo-warned-once
            (cons msgrec mumamo-warned-once))
      ;;(apply 'lwarn type :warning message args)
      (apply 'message (format "%s: %s" type message) args)
      )))

(defun mumamo-add-help-tabs ()
  "Add key bindings for moving between buttons.
Add bindings similar to those in `help-mode' for moving between
text buttons."
  (local-set-key [tab] 'forward-button)
  (local-set-key [(meta tab)]  'backward-button)
  (local-set-key [(shift tab)] 'backward-button)
  (local-set-key [backtab]     'backward-button))

(defun mumamo-insert-describe-button (symbol type)
  "Insert a text button that describes SYMBOL of type TYPE."
  (let ((func `(lambda (btn)
                 (funcall ',type ',symbol))))
    (mumamo-add-help-tabs)
    (insert-text-button
     (symbol-name symbol)
     :type 'help-function
     'face 'link
     'action func)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Custom group

(defgroup mumamo nil
  "Customization group for multiple major modes in a buffer."
  :group 'editing
  :group 'languages
  :group 'sgml
  :group 'nxhtml
  )

(defface mumamo-border-face
  '((t (:inherit font-lock-preprocessor-face :bold t :italic t)))
  "Face for marking borders."
  :group 'mumamo)


;;(setq mumamo-set-major-mode-delay -1)
;;(setq mumamo-set-major-mode-delay 5)
(defcustom mumamo-set-major-mode-delay idle-update-delay
  "Delay this number of seconds before setting major mode.
When point enters a region where the major mode should be
different than the current major mode, wait until Emacs has been
idle this number of seconds before switching major mode.

If negative switch major mode immediately.

Ideally the switching of major mode should occur immediately when
entering a region.  However this can make movements a bit unsmooth
for some major modes on a slow computer.  Therefore on a slow
computer use a short delay.

If you have a fast computer and want to use mode specific
movement commands then set this variable to -1.

I tried to measure the time for switching major mode in mumamo.
For most major modes it took 0 ms, but for `nxml-mode' and its
derivate it took 20 ms on a 3GHz CPU."
  :type 'number
  :group 'mumamo)

(defface mumamo-background-chunk-submode
  '((((class color) (min-colors 88) (background dark))
     ;;:background "blue3")
     :background "dark green")
    (((class color) (min-colors 88) (background light))
     ;;:background "lightgoldenrod2")
     :background "azure")
    (((class color) (min-colors 16) (background dark))
     :background "blue3")
    (((class color) (min-colors 16) (background light))
     :background "azure")
    (((class color) (min-colors 8))
     :background "blue")
    (((type tty) (class mono))
     :inverse-video t)
    (t :background "gray"))
  "Background colors for chunks in major mode.
You should only specify :background here, otherwise it will
interfere with syntax highlighting."
  :group 'mumamo)

(defcustom mumamo-background-chunk-major 'mumamo-background-chunk-major
  "Background colors for chunks in major mode.
Pointer to face with background color.

If you do not want any special background color use the face named
default."
  :type 'face
  :group 'mumamo)

(defface mumamo-background-chunk-major
  '((((class color) (min-colors 88) (background dark))
     ;;:background "blue3")
     :background "midnight blue")
    (((class color) (min-colors 88) (background light))
     ;;:background "lightgoldenrod2")
     :background "cornsilk")
    (((class color) (min-colors 16) (background dark))
     :background "blue4")
    (((class color) (min-colors 16) (background light))
     :background "cornsilk")
    (((class color) (min-colors 8))
     :background "blue")
    (((type tty) (class mono))
     :inverse-video t)
    (t :background "gray"))
  "Background colors for chunks in sub modes.
You should only specify :background here, otherwise it will
interfere with syntax highlighting."
  :group 'mumamo)

(defcustom mumamo-background-chunk-submode 'mumamo-background-chunk-submode
  "Background colors for chunks in sub modes.
Pointer to face with background color.

If you do not want any special background color use the face named
default."
  :type 'face
  :group 'mumamo)

(defcustom mumamo-chunk-coloring 'both-colored
  "What chunks to color."
  :type '(choice (const :tag "Color only submode chunks" submode-colored)
                 (const :tag "No coloring of chunks" no-chunks-colored)
                 (const :tag "Color both submode and main major mode chunks"
                        both-colored))
  :group 'mumamo)

(defcustom mumamo-submode-indent-offset 2
  "Indentation of submode relative main major mode.
If this is nil then no special indent is made when entering a
submode.

See also `mumamo-submode-indent-offset-0'."
  :type '(choice integer
                 (const :tag "No special"))
  :group 'mumamo)

(defcustom mumamo-submode-indent-offset-0 0
  "Indentation of submode at column 0.
This value overrides `mumamo-submode-indent-offset' when the main
major mode above has indentation 0."
  :type '(choice integer
                 (const :tag "No special"))
  :group 'mumamo)

(defcustom mumamo-major-mode-indent-specials
  '(
    (php-mode (use-widen))
    (nxhtml-mode ((use-widen (html-mumamo-mode nxhtml-mumamo-mode))))
    (html-mode ((use-widen (html-mumamo-mode nxhtml-mumamo-mode))))
    )
  "Major mode specials to use during indentation."
  :type '(repeat
          (list (symbol :tag "Major mode symbol")
                (set
                 (const :tag "Widen buffer during indentation" use-widen))))
  :group 'mumamo)

(defcustom mumamo-check-chunk-major-same nil
  "Check if main major mode is the same as normal mode."
  :type 'boolean
  :group 'mumamo)

;; (customize-option 'mumamo-major-modes)
;;(require 'django)
(defcustom mumamo-major-modes
  '(
    (asp-js-mode
     javascript-mode)
    (asp-vb-mode
     visual-basic-mode)
    ;;(css-mode fundamental-mode)
    (javascript-mode
     javascript-mode
     js2-fl-mode
     ecmascript-mode)
    (java-mode
     jde-mode
     java-mode)
    )
  "Alist for conversion of chunk major mode specifier to major mode.
Each entry has the form

  \(MAJOR-SPEC MAJORMODE ...)

where the symbol MAJOR-SPEC specifies the code type and should
match the value returned from `mumamo-find-possible-chunk'.  The
MAJORMODE symbols are major modes that can be used for editing
that code type.  The first available MAJORMODE is the one that is
used.

The MAJOR-SPEC symbols are used by the chunk definitions in
`define-mumamo-multi-major-mode'.

The major modes are not specified directly in the chunk
definitions. Instead a chunk definition contains a symbol that is
looked up in this list to find the chunk's major
mode.

The reason for doing it this way is to make it possible to use
new major modes with existing multi major modes. If for example
someone writes a new CSS mode that could easily be used instead
of the current one in `html-mumamo-mode'.

Lookup in this list is done by `mumamo-major-mode-from-modespec'."
  :type '(alist
          :key-type (symbol :tag "Symbol for major mode spec in chunk")
          :value-type (repeat (choice
                               (command :tag "Major mode")
                               (symbol :tag "Major mode (not yet loaded)")))
          )
  :group 'mumamo)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; JIT lock functions

(defun mumamo-jit-lock-function (start)
  "This function is added to `fontification-functions' by mumamo.
START is a parameter given to functions in that hook."
  (mumamo-msgfntfy "mumamo-jit-lock-function %s, ff=%s, just-changed=%s" start (get-text-property start 'fontified) mumamo-just-changed-major)
  (if mumamo-just-changed-major
      (setq mumamo-just-changed-major nil))
  (jit-lock-function start))

(defun mumamo-jit-lock-register (fun &optional contextual)
  "Replacement for `jit-lock-register'.
Avoids refontification, otherwise same.  FUN and CONTEXTUAL has
the some meaning as there."
  (add-hook 'jit-lock-functions fun nil t)
  (when (and contextual jit-lock-contextually)
    (set (make-local-variable 'jit-lock-contextually) t))

  ;;(jit-lock-mode t)
  ;;
  ;; Replace this with the code below from jit-lock-mode t part:
  (setq jit-lock-mode t)

  ;; Mark the buffer for refontification.
  ;; This is what we want to avoid in mumamo:
  ;;(jit-lock-refontify)

  ;; Install an idle timer for stealth fontification.
  (when (and jit-lock-stealth-time (null jit-lock-stealth-timer))
    (setq jit-lock-stealth-timer
          (run-with-idle-timer jit-lock-stealth-time t
                               'jit-lock-stealth-fontify)))

  ;; Create, but do not activate, the idle timer for repeated
  ;; stealth fontification.
  (when (and jit-lock-stealth-time (null jit-lock-stealth-repeat-timer))
    (setq jit-lock-stealth-repeat-timer (timer-create))
    (timer-set-function jit-lock-stealth-repeat-timer
                        'jit-lock-stealth-fontify '(t)))

  ;; Init deferred fontification timer.
  (when (and jit-lock-defer-time (null jit-lock-defer-timer))
    (setq jit-lock-defer-timer
          (run-with-idle-timer jit-lock-defer-time t
                               'jit-lock-deferred-fontify)))

  ;; Initialize contextual fontification if requested.
  (when (eq jit-lock-contextually t)
    (unless jit-lock-context-timer
      (setq jit-lock-context-timer
            (run-with-idle-timer jit-lock-context-time t
                                 'jit-lock-context-fontify)))
    (setq jit-lock-context-unfontify-pos
          (or jit-lock-context-unfontify-pos (point-max))))

  ;; Setup our hooks.
  ;;(add-hook 'after-change-functions 'jit-lock-after-change t t)
  ;;(add-hook 'after-change-functions 'mumamo-jit-lock-after-change t t)
  (add-hook 'after-change-functions 'mumamo-after-change t t)
  ;; Set up fontification to call jit:
  (let ((ff (reverse fontification-functions)))
    (mapc (lambda (f)
            ;;(unless (eq f 'jit-lock-function)
            (remove-hook 'fontification-functions f t))
          ;;)
          ff))
  (add-hook 'fontification-functions 'mumamo-jit-lock-function nil t)
  )

;; From jit-lock.el:
(defmacro mumamo-jit-with-buffer-unmodified (&rest body)
  "Eval BODY, preserving the current buffer's modified state."
  (declare (debug t))
  (let ((modified (make-symbol "modified")))
    `(let ((,modified (buffer-modified-p)))
       (unwind-protect
           (progn ,@body)
         (unless ,modified
           (restore-buffer-modified-p nil))))))

(defmacro mumamo-with-buffer-prepared-for-jit-lock (&rest body)
  "Execute BODY in current buffer, overriding several variables.
Preserves the `buffer-modified-p' state of the current buffer."
  (declare (debug t))
  `(mumamo-jit-with-buffer-unmodified
    (let ((buffer-undo-list t)
          (inhibit-read-only t)
          (inhibit-point-motion-hooks t)
          (inhibit-modification-hooks t)
          deactivate-mark
          buffer-file-name
          buffer-file-truename)
      ,@body)))

;; Fix-me: integrate this with fontify-region!
(defvar mumamo-find-chunks-timer nil)
(make-variable-buffer-local 'mumamo-find-chunks-timer)
(put 'mumamo-find-chunks-timer 'permanent-local t)

(defvar mumamo-find-chunk-delay idle-update-delay)
(make-variable-buffer-local 'mumamo-find-chunks-timer)
(put 'mumamo-find-chunks-timer 'permanent-local t)

(defun mumamo-stop-find-chunks-timer ()
  (when (and mumamo-find-chunks-timer
             (timerp mumamo-find-chunks-timer))
    (cancel-timer mumamo-find-chunks-timer))
  (setq mumamo-find-chunks-timer nil))

(defun mumamo-start-find-chunks-timer ()
  ;;(message "TIMER mumamo-start-find-chunks-timer ()")
  (mumamo-stop-find-chunks-timer)
  (setq mumamo-find-chunks-timer
        (run-with-idle-timer mumamo-find-chunk-delay nil
                             'mumamo-find-chunks-in-timer (current-buffer))))

(defun mumamo-find-chunks-in-timer (buffer)
  (mumamo-msgfntfy "mumamo-find-chunks-in-timer")
  (condition-case err
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          ;;(mumamo-find-chunks nil 'until-input)))
          (mumamo-find-chunks nil)))
    (error (message "mumamo-find-chunks error: %s" err))))

;; (defvar mumamo-end-last-chunk-pos nil)
;; (make-variable-buffer-local 'mumamo-end-last-chunk-pos)
;; (put 'mumamo-end-last-chunk-pos 'permanent-local t)
(defvar mumamo-last-chunk nil)
(make-variable-buffer-local 'mumamo-last-chunk)
(put 'mumamo-last-chunk 'permanent-local t)

(defvar mumamo-last-chunk-change-pos nil)
(make-variable-buffer-local 'mumamo-last-chunk-change-pos)
(put 'mumamo-last-chunk-change-pos 'permanent-local t)

(defvar mumamo-last-chunk-change-pos-list nil)
(make-variable-buffer-local 'mumamo-last-chunk-change-pos-list)
(put 'mumamo-last-chunk-change-pos-list 'permanent-local t)

(defvar mumamo-old-chunks nil)
(make-variable-buffer-local 'mumamo-old-chunks)
(put 'mumamo-old-chunks 'permanent-local t)

;; Fix-me: maybe this belongs to contextual fontification? Eh,
;; no. Unfortunately there is not way to make that handle more than
;; multiple lines.
(defun mumamo-find-chunks (end) ;; min max)
  "Find or create chunks from last known chunk.
Ie, start from the end of `mumamo-last-chunk' if this is non-nil,
otherwise 1.

If END is nil then continue till end of buffer or until any input
is available.  In this case the return value is undefined.

Otherwise END must be a position in the buffer.  Return the
mumamo chunk containing the position.  If `mumamo-last-chunk'
ends before END then create chunks upto END.

If MIN and MAX are non-nil then do not mark for refontification
in this part of the buffer."
  (mumamo-msgfntfy "!!!!!!!!!!!!!!!!!!!mumamo-find-chunks %s, last-chunk-change=%s, mumamo-last-chunk=%s" end mumamo-last-chunk-change-pos mumamo-last-chunk)
  ;;(message "!!!!!!!!!!!!!!!!!!!mumamo-find-chunks %s, last-chunk-change=%s, mumamo-last-chunk=%s" end mumamo-last-chunk-change-pos mumamo-last-chunk)
  (mumamo-stop-find-chunks-timer)
  ;; If `mumamo-last-chunk-change-pos' is an integer then it was set
  ;; by `mumamo-find-chunk-after-change'.  We need to investigate
  ;; chunks after this position.
  (when (integerp mumamo-last-chunk-change-pos)
    (let* ((this-chunk (mumamo-get-existing-chunk-at
                       mumamo-last-chunk-change-pos))
           ;; Check if near border
           (in-border (when this-chunk
                        (>= (mumamo-chunk-syntax-min this-chunk)
                            mumamo-last-chunk-change-pos))))
      (when this-chunk
        (setq mumamo-last-chunk (overlay-get this-chunk 'mumamo-prev-chunk))
        (when in-border
          (setq mumamo-last-chunk
                (overlay-get mumamo-last-chunk 'mumamo-prev-chunk))))
      (setq mumamo-last-chunk-change-pos nil)))
  (unless (and (overlayp mumamo-last-chunk) (overlay-buffer mumamo-last-chunk))
    (setq mumamo-last-chunk nil))
  (let ((ok-pos (if mumamo-last-chunk (overlay-end mumamo-last-chunk) 1))
        (end (or end (point-max)))
        narpos
        this-values
        this-chunk
        prev-chunk
        first-change-pos
        interrupted
        (point-max (1+ (buffer-size)))
        (here (point)))
    (if (> ok-pos end)
        (progn
          (setq this-chunk (mumamo-get-existing-chunk-at end))
          (unless this-chunk (error "Could not find chunk though ok-pos=%s > end=%s" ok-pos end))
          this-chunk)
      (save-restriction
        (widen)
        (mumamo-save-buffer-state nil
          (setq this-chunk mumamo-last-chunk)
          (while (and (or (not end)
                          (<= ok-pos end))
                      (< ok-pos (point-max))
                      (not (setq interrupted (and (not end)
                                                  (input-pending-p)))))
            ;; Narrow to speed up. However the chunk divider may be
            ;; before ok-pos here. Assume that the marker is not
            ;; longer than 200 chars. fix-me.
            (setq narpos (max (- ok-pos 200) 1))
            (widen)
            (narrow-to-region narpos point-max)
            ;;(message "narrow-to-region %s %s, ok-pos=%s, end=%s, this-chunk=%s" narpos point-max ok-pos end this-chunk)
            (setq prev-chunk this-chunk)
            (setq this-chunk nil)
            (setq this-values (mumamo-create-chunk-values-at ok-pos))
            (setq ok-pos (or (mumamo-chunk-value-max this-values) ;;(overlay-end this-chunk)
                             (point-max)))
            (mumamo-msgfntfy "!!!new ok-pos=%s, this-values=%s" ok-pos this-values)
            ;;(message "!!!new ok-pos=%s, this-values=%s" ok-pos this-values)
            ;; With the new organization all chunks are created here.
            (let ((old-chunk (if (= 1 (mumamo-chunk-value-min this-values))
                                 (mumamo-get-existing-chunk-at 1)
                               (when (and prev-chunk
                                          (overlayp prev-chunk)
                                          (overlay-buffer prev-chunk))
                                 (overlay-get prev-chunk 'mumamo-next-chunk))))
                  next-old-chunk)
              (when old-chunk
                (mumamo-msgfntfy "old-chunk=%s" old-chunk)
                ;;(message "old-chunk=%s" old-chunk)
                (if (mumamo-chunk-equal-chunk-values old-chunk this-values)
                    (progn
                      ;;(message "setting this-chunk to old-chunk")
                      (setq this-chunk old-chunk))
                  ;; Fix-me: delete just until next old chunk fits again
                  (while old-chunk
                    (setq next-old-chunk
                          (overlay-get old-chunk 'mumamo-next-chunk))
                    ;;(message "deleting old-chunk %s" old-chunk)
                    (delete-overlay old-chunk)
                    (setq old-chunk next-old-chunk))
                  (when end (mumamo-start-find-chunks-timer))
                  ))
              (unless this-chunk
                ;; Create chunk and chunk links
                ;; Fix-me: Mark chunk borders here??? Won't work
                ;; well with font lock turn on/off.
                (setq this-chunk (mumamo-create-chunk-from-chunk-values this-values))
                ;;(message "created this-chunk=%s" this-chunk)
                (when prev-chunk
                  (overlay-put prev-chunk 'mumamo-next-chunk this-chunk))
                (overlay-put this-chunk 'mumamo-prev-chunk prev-chunk)
                (unless first-change-pos
                  (setq first-change-pos (mumamo-chunk-value-min this-values)))
                ;; Fix-me: Mark old chunks for refontification, but
                ;; only where fontified=t. This must be sent back to
                ;; fontify-region in some way. Add a new text prop for
                ;; this? Eh, or just use fontified ...

                ;; Fix-me: Maybe better do what jit-lock-after-change
                ;; does, but just for the current chunks?
;;;                 (let (jit-lock-start jit-lock-end)
;;;                   (while (and (prog1
;;;                                   (setq old-chunk (car mumamo-old-chunks))
;;;                                 (setq mumamo-old-chunks (cdr mumamo-old-chunks)))
;;;                               (< (mumamo-chunk-value-min old-chunk)
;;;                                  (mumamo-chunk-value-max this-chunk)))
;;;                     (if (eq (mumamo-chunk-value-major this-chunk)
;;;                             (mumamo-chunk-value-major old-chunk))
;;;                         ;; Fix-me: extend?  Fix-me: Should we really
;;;                         ;; mark for refontification here? this is before
;;;                         ;; fontification per major mode is done, but
;;;                         ;; fontified=t ...  So we must use another
;;;                         ;; property here to tell what is not needed to
;;;                         ;; refontify afaics.  (put-text-property min max
;;;                         ;; 'fontified nil)
;;;                         ;;
;;;                         ;; Mark for refontification if major mode was
;;;                         ;; changed, but not between MIN and MAX which
;;;                         ;; are the values from `mumamo-fontify-region'
;;;                         ;; because that region is beeing fontified right
;;;                         ;; after this and `fontified' prop is already
;;;                         ;; set to t.
;;;                         ;;
;;;                         (progn
;;;                           ;; Border at start of old chunk
;;;                           (setq jit-lock-start (mumamo-chunk-value-min old-chunk))
;;;                           (setq jit-lock-end (mumamo-chunk-value-syntax-min old-chunk))
;;;                           ;; Fix-me: make this a function - eh..., or move it ... - eh, no
;;;                           (when (and (> jit-lock-end jit-lock-start)
;;;                                      (not (or (< max jit-lock-start)
;;;                                               (> min jit-lock-end))))
;;;                             (when (> min jit-lock-start) (setq jit-lock-start min))
;;;                             (when (< max jit-lock-end)   (setq jit-lock-end   max))
;;;                             (run-hooks jit-lock-after-change-extend-region-functions)
;;;                             )
;;;                           ;; Border at end of old chunk
;;;                           )
;;;                       ;; Whole old chunk (within the range we are
;;;                       ;; interested in here of course)
;;;                       (mumamo-mark-for-refontification (mumamo-chunk-value-min old-chunk))
;;;                       )))
                ))
            ;; Cache ppss syntax
            ;;(syntax-ppss (1+ (mumamo-chunk-syntax-min this-chunk)))
            ;;(setq mumamo-end-last-chunk-pos ok-pos)
            )))
      (when (or interrupted
                (and mumamo-last-chunk
                     (overlayp mumamo-last-chunk)
                     (< (overlay-end mumamo-last-chunk) (point-max))))
        ;; Fix-me
        ;;(mumamo-start-find-chunks-timer)
        )
      (when first-change-pos
;;;         (mumamo-with-buffer-prepared-for-jit-lock
;;;          (put-text-property first-change-pos (point) 'fontified nil))
        (setq jit-lock-context-unfontify-pos
              (min jit-lock-context-unfontify-pos first-change-pos)))
      (goto-char here)
      (mumamo-msgfntfy "!!!!mumamo-find-chunks, this-chunk=%s, this-values=%s" this-chunk this-values)
      ;;(message "!!!!mumamo-find-chunks, this-chunk=%s, this-values=%s" this-chunk this-values)
      this-chunk)))

(defun mumamo-find-chunk-after-change (min)
  (setq mumamo-last-chunk-change-pos
        (if mumamo-last-chunk-change-pos
            (min min mumamo-last-chunk-change-pos)
          min)))

(defun mumamo-after-change (min max old-len)
  "Everything that needs to be done in mumamo after a change.
This is run in the `after-change-functions' hook.  For MIN, MAX
and OLD-LEN see that variable."
  (mumamo-find-chunk-after-change min)
  (mumamo-jit-lock-after-change min max old-len))

(defun mumamo-jit-lock-after-change (min max old-len)
  "Replacement for `jit-lock-after-change'.
Does the nearly the same thing as that function, but takes
care of that there might be different major modes at MIN and MAX.
It also marks for refontification only in the current mumamo chunk.

OLD-LEN is the pre-change length.

Jit-lock after change functions is organized this way:

`jit-lock-after-change' (doc: Mark the rest of the buffer as not
fontified after a change) is added locally to the hook
`after-change-functions'.  This function runs
`jit-lock-after-change-extend-region-functions'."
  (when (and jit-lock-mode (not memory-full))
    (mumamo-msgfntfy "mumamo-jit-lock-after-change %s %s %s" min max old-len)
    ;; Why is this nil?:
    (mumamo-msgfntfy "  mumamo-jit-lock-after-change: font-lock-extend-after-change-region-function=%s" font-lock-extend-after-change-region-function)
    (let* ((ovl-min (mumamo-get-existing-chunk-at min))
           (ovl-max (when (or (not ovl-min)
                              (< (overlay-end ovl-min) max))
                      (mumamo-get-existing-chunk-at max)))
           (major-min (when ovl-min (mumamo-chunk-major-mode ovl-min)))
           (major-max (when ovl-max (mumamo-chunk-major-mode ovl-max)))
           (r-min nil)
           (r-max nil)
           (new-min min)
           (new-max max))
      (if (and major-min (eq major-min major-max))
          (setq r-min
                (when major-min
                  (mumamo-jit-lock-after-change-1 min max old-len major-min)))
        (setq r-min
              (when major-min
                (mumamo-jit-lock-after-change-1 min max old-len major-min)))
        (setq r-max
              (when major-max
                (mumamo-jit-lock-after-change-1 min max old-len major-max))))
      (mumamo-msgfntfy "mumamo-jit-lock-after-change r-min,max=%s,%s major-min,max=%s,%s" r-min r-max major-min major-max)
      (when r-min
        (setq new-min (min new-min (car r-min)))
        (setq new-max (max new-max (cdr r-min))))
      (when r-max
        (setq new-min (min new-min (car r-max)))
        (setq new-max (max new-max (cdr r-max))))
      (setq new-min (max new-min (point-min)))
      (setq new-max (min new-max (point-max)))
      ;; Make sure we change at least one char (in case of deletions).
      (setq new-max (min (max new-max (1+ new-min)) (point-max)))
      (mumamo-msgfntfy "mumamo-jit-lock-after-change new-min,max=%s,%s" new-min new-max)
      (mumamo-mark-for-refontification new-min new-max)

      ;; Mark the change for deferred contextual refontification.
      (when jit-lock-context-unfontify-pos
        (setq jit-lock-context-unfontify-pos
              ;; Here we use `start' because nothing guarantees that the
              ;; text between start and end will be otherwise refontified:
              ;; usually it will be refontified by virtue of being
              ;; displayed, but if it's outside of any displayed area in the
              ;; buffer, only jit-lock-context-* will re-fontify it.
              (min jit-lock-context-unfontify-pos new-min))
        (mumamo-msgfntfy "mumamo-jit-lock-after-change.unfontify-pos=%s" jit-lock-context-unfontify-pos)
        ))))
              ;;(min jit-lock-context-unfontify-pos jit-lock-start))))))
;;(put 'mumamo-jit-lock-after-change 'permanent-local-hook t)
(put 'mumamo-after-change 'permanent-local-hook t)

(defun mumamo-jit-lock-after-change-1 (min max old-len major)
  "Extend the region the same way jit-lock does it.
This function tries to extend the region between MIN and MAX the
same way jit-lock does it after a change.  OLD-LEN is the
pre-change length.

The extending of the region is done as if MAJOR was the major
mode."
  (mumamo-with-major-mode-fontification major
    `(progn
       (let ((jit-lock-start ,min)
             (jit-lock-end   ,max))
         ;;(mumamo-msgfntfy "mumamo-mumamo-jit-lock-after-change-1 jlacer=%s" ,jit-lock-after-change-extend-region-functions)
         (mumamo-with-buffer-prepared-for-jit-lock
          ;;(font-lock-extend-jit-lock-region-after-change ,min ,max ,old-len)
          (run-hook-with-args 'jit-lock-after-change-extend-region-functions min max old-len)
          ;;(setq jit-lock-end (min (max jit-lock-end (1+ min)) (point-max)))

;;;           ;; Just run the buffer local function:
;;;           (dolist (extend-fun jit-lock-after-change-extend-region-functions)
;;;             (when (fboundp extend-fun)
;;;               (funcall extend-fun ,min ,max ,old-len)))
          )
         (setq min jit-lock-start)
         (setq max jit-lock-end)
         (syntax-ppss-flush-cache min)
         )))
  (cons min max))

(defun mumamo-mark-chunk ()
  "Mark chunk and move point to beginning of chunk."
  (interactive)
  (let ((chunk (mumamo-get-existing-chunk-at (point))))
    (unless chunk (error "There is no MuMaMo chunk here."))
    (goto-char (overlay-start chunk))
    (push-mark (overlay-end chunk) t t)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Font lock functions

;; Borrowed from font-lock.el
(defmacro mumamo-save-buffer-state (varlist &rest body)
  "Bind variables according to VARLIST and eval BODY restoring buffer state.
Do not record undo information during evaluation of BODY."
  (declare (indent 1) (debug let))
  (let ((modified (make-symbol "modified")))
    `(let* ,(append varlist
                    `((,modified (buffer-modified-p))
                      (buffer-undo-list t)
                      (inhibit-read-only t)
                      (inhibit-point-motion-hooks t)
                      (inhibit-modification-hooks t)
                      deactivate-mark
                      buffer-file-name
                      buffer-file-truename))
       (progn
         ,@body)
       (unless ,modified
         (restore-buffer-modified-p nil)))))

(defun mumamo-mark-for-refontification (min max)
  "Mark region between MIN and MAX for refontification."
  (mumamo-msgfntfy "mumamo-mark-for-refontification A min,max=%s,%s point-min,max=%s,%s modified=%s" min max (point-min) (point-max) (buffer-modified-p) )
  (assert (<= min max))
  (when (< min max)
    (save-restriction
      (widen)
      (mumamo-msgfntfy "mumamo-mark-for-refontification B min,max=%s,%s point-min,max=%s,%s modified=%s" min max (point-min) (point-max) (buffer-modified-p) )
      ;;(mumamo-save-buffer-state nil
      (mumamo-with-buffer-prepared-for-jit-lock
        (put-text-property min max 'fontified nil)
        ))))


(defvar mumamo-internal-major-modes-alist nil
  "Alist with info for different major modes.
Internal use only.  This is automatically set up by
`mumamo-get-major-mode-setup'.")
(setq mumamo-internal-major-modes-alist nil)
(put 'mumamo-internal-major-modes-alist 'permanent-local t)

(defvar mumamo-ppss-last-chunk nil
  "Internal variable used to avoid unnecessary flushing.")
(defvar mumamo-ppss-last-major nil
  "Internal variable used to avoid unnecessary flushing.")

;;(mumamo-get-major-mode-substitute 'nxhtml-mode 'fontification)
;;(mumamo-get-major-mode-substitute 'nxhtml-mode 'indentation)
;;(mumamo-get-major-mode-substitute 'css-mode 'fontification)
;;(mumamo-get-major-mode-substitute 'css-mode 'indentation)
;; (assq 'nxml-mode mumamo-major-mode-substitute)
(defconst mumamo-major-mode-substitute
  '(
    (nxhtml-mode (html-mode nxhtml-mode))
    ;;(nxhtml-mode (html-mode))
    (nxhtml-genshi-mode (html-mode nxhtml-mode))
    (nxhtml-mjt-mode (html-mode nxhtml-mode))
    (nxml-mode (sgml-mode))
    )
  "Major modes substitute to use for fontification and indentation.
The entries in this list has either of the formats

  \(MAJOR (FONT-MODE INDENT-MODE))
  \(MAJOR (FONT-MODE))

where major is the major mode in a mumamo chunk and FONT-MODE is
the major mode for fontification of that chunk and INDENT-MODE is
dito for indentation.  In the second form the same mode is used
for indentation as for fontification.")

;;(mumamo-get-major-mode-substitute 'nxhtml-mode 'indentation)
;;(mumamo-get-major-mode-substitute 'nxhtml-mode 'fontification)
(defun mumamo-get-major-mode-substitute (major for-what)
  "For major mode MAJOR return major mode to use for FOR-WHAT.
FOR-WHAT can be either 'fontification or indentation.

mumamo must handle fontification and indentation for `major-mode'
by using other major mode if the functions for this in
`major-mode' are not compatible with mumamo.  This functions
looks in the table `mumamo-major-mode-substitute' for get major
mode to use."
  ;;(when (eq for-what 'indentation) (message "subst.major=%s" major))
  (let ((m (assq major mumamo-major-mode-substitute))
        ret-major)
    (if (not m)
        (setq ret-major major)
      (setq m (nth 1 m))
      (setq ret-major
            (cond
             ((eq for-what 'fontification)
              (nth 0 m))
             ((eq for-what 'indentation)
              (nth 1 m))
             (t
              (mumamo-display-error 'mumamo-get-major-mode-substitute
                                    "Bad parameter, for-what=%s" for-what))))
      (unless ret-major (setq ret-major major)))
    (unless (commandp ret-major) (setq ret-major 'mumamo-bad-mode))
    ;;(when (eq for-what 'indentation) (message "ret.ind=%s, major=%s, m=%s" ret major m))
    ret-major))

(defmacro mumamo-with-major-mode-setup (major for-what &rest body)
  "Run code with some local variables set as in specified major mode.
Set variables as needed for major mode MAJOR when doing FOR-WHAT
and then run BODY using `with-syntax-table'.

FOR-WHAT is used to choose another major mode than MAJOR in
certain cases.  It should be 'fontification or 'indentation.

Note: We must let-bind the variables here instead of make them buffer
local since they otherwise could be wrong at \(point) in top
level \(ie user interaction level)."
  (declare (indent 2) (debug t))
  `(let ((need-major-mode (mumamo-get-major-mode-substitute ,major ,for-what)))
     (mumamo-msgfntfy "mumamo-with-major-mode-setup %s => %s, modified=%s" ,major need-major-mode (buffer-modified-p))
     (mumamo-msgfntfy "mumamo-with-major-mode-setup <<<<<<<<<< body=%S\n>>>>>>>>>>" '(progn ,@body))
     (let ((major-mode need-major-mode)
           (evaled-set-mode (mumamo-get-major-mode-setup need-major-mode)))
         ;;(message ">>>>>> before %s" evaled-set-mode)
         ;;(message ">>>>>> before %s, body=%s" evaled-set-mode (list ,@body))
         (funcall evaled-set-mode
                  (list 'progn
                        ,@body))
         ;;(mumamo-msgfntfy "<<<<<< after evaled-set-mode modified=%s" (buffer-modified-p))
         )))

(defmacro mumamo-with-major-mode-fontification (major &rest body)
  "With fontification variables set as in another major mode do things.
This is used during font locking and indentation.  The variables
affecting those are set as they are in major mode MAJOR.

See the code in `mumamo-fetch-major-mode-setup' for exactly which
local variables that are set."
  (declare (indent 1) (debug t))
  `(mumamo-with-major-mode-setup ,major 'fontification
     ,@body))

(defmacro mumamo-with-major-mode-indentation (major &rest body)
  "With indentation variables set as in another major mode do things.
Same as `mumamo-with-major-mode-fontification' but for
indentation.  See that function for some notes about MAJOR and
BODY."
  (declare (indent 1) (debug t))
  `(mumamo-with-major-mode-setup ,major 'indentation ,@body))

(defun mumamo-assert-fontified-t (start end)
  "Assert that the region start to end has 'fontified t."
  (let ((start-ok (get-text-property start 'fontified))
        (first-not-ok
         (next-single-property-change (1+ start) 'fontified nil end)))
    (when (not start-ok)
      (message "==== mumamo-assert-fontified-t %s-%s start not ok" start end))
    (when (not (= first-not-ok end))
      (message "==== mumamo-assert-fontified-t %s-%s first not ok=%s"  start end first-not-ok))))

;; Keep this separate for easier debugging.
(defun mumamo-do-fontify (start end verbose chunk-syntax-min chunk-syntax-max chunk-major)
  "Fontify region between START and END.
If VERBOSE is non-nil then print status messages during
fontification.

CHUNK-SYNTAX-MIN, CHUNK-SYNTAX-MAX and CHUNK-MAJOR are the
chunk's min point, max point and major mode.

During fontification narrow the buffer to the chunk to make
syntactic fontification work.  If chunks starts or end with \"
then the first respective last char then exclude those chars from
from the narrowed part, since otherwise the syntactic
fontification can't find out where strings start and stop.

Note that this function is run under
`mumamo-with-major-mode-fontification'.

This function takes care of `font-lock-dont-widen' and
`font-lock-extend-region-functions'.  Normally
`font-lock-default-fontify-region' does this, but that function
is not called when mumamo is used!

PS: `font-lock-fontify-syntactically-region' is the main function
that does syntactic fontification."
  (mumamo-msgfntfy "mumamo-do-fontify <<<<<<< %s %s %s %s %s %s" start end verbose chunk-syntax-min chunk-syntax-max chunk-major)
  ;;(mumamo-assert-fontified-t start end)
  (mumamo-condition-case err
      (let* ((font-lock-dont-widen t)
             (font-lock-extend-region-functions
              ;; nil
              font-lock-extend-region-functions
              )
             ;; Extend like in `font-lock-default-fontify-region':
             (funs font-lock-extend-region-functions)
             (font-lock-beg start)
             (font-lock-end end))
        (while funs
          (setq funs (if (or (not (funcall (car funs)))
                             (eq funs font-lock-extend-region-functions))
                         (cdr funs)
                       ;; If there's been a change, we should go through
                       ;; the list again since this new position may
                       ;; warrant a different answer from one of the fun
                       ;; we've already seen.
                       font-lock-extend-region-functions)))
        ;; But we must restrict to the chunk here:
        (let ((new-start (max chunk-syntax-min font-lock-beg))
              (new-end (min chunk-syntax-max font-lock-end)))
          (mumamo-msgfntfy "  mumamo-do-fontify %s %s, chunk-syntax-min,max=%s,%s, new: %s %s" start end chunk-syntax-min chunk-syntax-max new-start new-end)
          ;; A new condition-case just to catch errors easier:
          (mumamo-condition-case err
              (save-restriction
                (narrow-to-region chunk-syntax-min chunk-syntax-max)
                ;; Now call font-lock-fontify-region again with
                ;; the chunk font lock parameters:
                ;;(message "(font-lock-fontify-region %s %s)" new-start new-end)
                (setq font-lock-syntactically-fontified (1- new-start))
                (font-lock-fontify-region new-start new-end verbose))
            (error
             (mumamo-display-error 'mumamo-do-fontify-2
                                   "mumamo-do-fontify m=%s, s/e=%s/%s syn-min/max=%s/%s: %s"
                                   chunk-major
                                   start end
                                   chunk-syntax-min chunk-syntax-max
                                   (error-message-string err)))
            )))
    (error
     (mumamo-display-error 'mumamo-do-fontify
                           "mumamo-do-fontify m=%s, s=%s, e=%s: %s"
                           chunk-major start end (error-message-string err)))
    )
  (mumamo-msgfntfy "mumamo-do-fontify exit >>>>>>> %s %s %s %s %s %s" start end verbose chunk-syntax-min chunk-syntax-max major)
  )

(defun mumamo-do-unfontify (start end)
  "Unfontify region between START and END."
  (mumamo-condition-case err
      (font-lock-unfontify-region start end)
    (error
     (mumamo-display-error 'mumamo-do-unfontify "%s"
                           (error-message-string err)))))

(defun mumamo-fontify-region-with (start end verbose major chunk-syntax-min chunk-syntax-max)
  "Fontify from START to END.
If VERBOSE is non-nil then print status messages during
fontification.

Do the fontification as in major mode MAJOR.

Narrow to region CHUNK-SYNTAX-MIN and CHUNK-SYNTAX-MAX during
fontification."
  ;; The text property 'fontified is always t here due to the way
  ;; jit-lock works!

  (mumamo-msgfntfy "mumamo-fontify-region-with %s %s %s %s, ff=%s" start end verbose major (get-text-property start 'fontified))
  ;;(mumamo-assert-fontified-t start end)
  (mumamo-condition-case err
      (progn
        (mumamo-with-major-mode-fontification major
          `(mumamo-do-fontify ,start ,end ,verbose ,chunk-syntax-min ,chunk-syntax-max major))
        )
    (error
     (mumamo-display-error 'mumamo-fontify-region-with "%s"
                           (error-message-string err)))))

(defun mumamo-unfontify-region-with (start end major)
  "Unfontify from START to END as in major mode MAJOR."
  (mumamo-msgfntfy "mumamo-unfontify-region-with %s %s %s, ff=%s" start end major (get-text-property start 'fontified))
  (mumamo-with-major-mode-fontification major
    `(mumamo-do-unfontify ,start ,end)))


(defun mumamo-unfontify-buffer ()
  "Unfontify buffer.
This function is called when the minor mode function
`font-lock-mode' is turned off. \(It is the value of
`font-lock-unfontify-uffer-function')."
  ;;(message "BACKTRACE: %s" (with-output-to-string (backtrace)))
  (when mumamo-multi-major-mode
    (save-excursion
      (save-restriction
        (widen)
        (let ((ovls (overlays-in (point-min) (point-max)))
              (main-major (mumamo-main-major-mode)))
          (dolist (o ovls)
            (let ((major (mumamo-chunk-major-mode o)))
              (when major
                (unless (eq major main-major)
                  (mumamo-unfontify-chunk o))
                (mumamo-msgfntfy "delete-overlay 1")
                (delete-overlay o)
                ))))
        (mumamo-unfontify-region-with (point-min) (point-max)
                                      (mumamo-main-major-mode))))))


(defun mumamo-fontify-buffer ()
  "For `font-lock-fontify-buffer-function' call.
Not sure when this normally is done.  However some functions call
this to ensure that the whole buffer is fontified."
  (mumamo-msgfntfy "===> mumamo-fontify-buffer-function called")
  ;;(font-lock-default-fontify-buffer)
  (unless mumamo-set-major-running
    ;; This function is normally not called, but when new patterns
    ;; have been added by hi-lock it will be called.  In this case we
    ;; need to make buffer local fontification variables:
    (set (make-local-variable 'mumamo-internal-major-modes-alist) nil)
    (jit-lock-refontify)))


(defun mumamo-unfontify-chunk (chunk) ; &optional start end)
  "Unfontify mumamo chunk CHUNK."
  (let ((major (mumamo-chunk-major-mode chunk))
        ;;(start (overlay-start chunk))
        ;;(end   (overlay-end   chunk))
        (syntax-min (mumamo-chunk-syntax-min chunk))
        (syntax-max (mumamo-chunk-syntax-max chunk))
        (font-lock-dont-widen t))
    (save-restriction
      (narrow-to-region syntax-min syntax-max)
      (mumamo-unfontify-region-with syntax-min syntax-max major))))

(defvar mumamo-just-changed-major nil
  "Avoid refontification when switching major mode.
Set to t by `mumamo-set-major'.  Checked and reset to nil by
`mumamo-jit-lock-function'.")
(make-variable-buffer-local 'mumamo-just-changed-major)

(defun mumamo-fontify-region (start end &optional verbose)
  "Fontify between START and END.
Take the major mode chunks into account while doing this.

If VERBOSE do the verbously.

The value of `font-lock-fontify-region-function' when
mumamo is used is this function."
  (mumamo-msgfntfy "++++++ mumamo-fontify-regionX %s %s %s, skip=%s" start end verbose mumamo-just-changed-major)
  ;;(mumamo-assert-fontified-t start end)
  ;; If someone else tries to fontify the buffer ...
  (if (and mumamo-just-changed-major
           ;; The above variable is reset in `post-command-hook' so
           ;; check if we are in a recursive search.  (Note: There are
           ;; other situation when this can occur.  It might be best to
           ;; remove this test later, or make it optional.)
           ;;
           ;; skip the test for now:
           nil
           (= 0 (recursion-depth)))
      (mumamo-display-error 'mumamo-fontify-region
                            "Just changed major, should not happen")
    (mumamo-condition-case err
        (let ((debugger 'mumamo-debug-to-backtrace)
              (debug-on-error t))
          (mumamo-fontify-region-1 start end verbose))
      (error
       (mumamo-display-error 'mumamo-fontify-region "%s"
                             (error-message-string err))))))

(defconst mumamo-dbg-pretend-fontified nil
  "Set this to t to be able to debug more easily.
This is for debugging `mumamo-fontify-region-1' more easily by
just calling it.  It will make that function believe that the text
has a non-nil 'fontified property.")

(defun mumamo-exc-mode (chunk)
  "Return sub major mode for CHUNK.
If chunk is a main major mode chunk return nil, otherwise return
the major mode for the chunk."
  (let ((major (mumamo-chunk-major-mode chunk)))
    (unless (eq major (mumamo-main-major-mode))
      major)))

;; (let ((l '(1 2))) (setcar (nthcdr 1 l) 10) l)
;; setters
(defsubst mumamo-chunk-value-set-min (chunk-values min)
  "In CHUNK-VALUES set min value to MIN.
CHUNK-VALUES should have the format return by
`mumamo-create-chunk-values-at'."
  (setcar (nthcdr 0 chunk-values) min))
(defsubst mumamo-chunk-value-set-max (chunk-values max)
  "In CHUNK-VALUES set max value to MAX.
See also `mumamo-chunk-value-set-min'."
  (setcar (nthcdr 1 chunk-values) max))
(defsubst mumamo-chunk-value-set-syntax-min (chunk-values min)
  "In CHUNK-VALUES set min syntax diff value to MIN.
See also `mumamo-chunk-value-set-min'."
  (setcar (nthcdr 3 chunk-values) min))
(defsubst mumamo-chunk-value-set-syntax-max (chunk-values max)
  "In CHUNK-VALUES set max syntax diff value to MAX.
See also `mumamo-chunk-value-set-min'."
  (setcar (nthcdr 3 chunk-values) max))
;; getters
(defsubst mumamo-chunk-value-min    (chunk-values)
  "Get min value from CHUNK-VALUES.
See also `mumamo-chunk-value-set-min'."
  (nth 0 chunk-values))
(defsubst mumamo-chunk-value-max    (chunk-values)
  "Get max value from CHUNK-VALUES.
See also `mumamo-chunk-value-set-min'."
  (nth 1 chunk-values))
(defsubst mumamo-chunk-value-major  (chunk-values)
  "Get major value from CHUNK-VALUES.
See also `mumamo-chunk-value-set-min'."
  (nth 2 chunk-values))
(defsubst mumamo-chunk-value-syntax-min    (chunk-values)
  "Get min syntax diff value from CHUNK-VALUES.
See also `mumamo-chunk-value-set-min'."
  (nth 3 chunk-values))
(defsubst mumamo-chunk-value-syntax-max    (chunk-values)
  "Get max syntax diff value from CHUNK-VALUES.
See also `mumamo-chunk-value-set-min'."
  (nth 4 chunk-values))
(defsubst mumamo-chunk-value-parseable-by    (chunk-values)
  "Get parseable-by from CHUNK-VALUES.
See also `mumamo-chunk-value-set-min'.
For parseable-by see `mumamo-find-possible-chunk'."
  (nth 5 chunk-values))
(defsubst mumamo-chunk-prev-chunk (chunk-values)
  "Get prevous chunk from CHUNK-VALUES.
See also `mumamo-chunk-value-set-min'."
  (nth 6 chunk-values))

;; (defun mumamo-adjust-old-chunks (new-chunk-values start end)
;;   "Adjust old chunks to NEW-CHUNK-VALUES.
;; This must be run whenever adding a new chunk.

;; If START and END is non-nil then mark ranges outside of NEW-CHUNK
;; for refontification."
;;   (let* ((new-start      (mumamo-chunk-value-min   new-chunk-values))
;;          (new-end        (mumamo-chunk-value-max   new-chunk-values))
;;          (new-syntax-min (mumamo-chunk-value-syntax-min new-chunk-values))
;;          (new-major      (mumamo-chunk-value-major new-chunk-values))
;;          (new-is-closed new-end)
;;          (new-real-end (or new-end (point-max)))
;; ;;;          (old-chunks (delq nil
;; ;;;                            (mapcar
;; ;;;                             (lambda (ovl)
;; ;;;                               (when (mumamo-chunk-major-mode ovl)
;; ;;;                                 ovl))
;; ;;;                             (overlays-in new-start new-real-end))))
;;          old-chunks)
;;     ;;(setq old-chunks (delq nil old-chunks))
;;     (dolist (ovl (overlays-in new-start new-real-end))
;;       (when (mumamo-chunk-major-mode ovl)
;;         (setq old-chunks (cons ovl old-chunks))))
;;     (when (and start (< new-start start))
;;       (let ((must-refontify t))
;;         (dolist (old old-chunks)
;;           (when (and (= (overlay-start old) new-start)
;;                      (>= (overlay-end old) start)
;;                      (eq new-major (mumamo-chunk-major-mode old)))
;;             (setq must-refontify nil)))
;;         (when must-refontify
;;           ;;(mumamo-mark-for-refontification new-start (- start 1))
;;           )))
;;     (when (and end new-real-end (> new-real-end end))
;;       (mumamo-msgfntfy "(when (and end new-real-end (> new-real-end end))")
;;       (mumamo-mark-for-refontification (+ end 0) new-real-end))
;;     (dolist (old old-chunks)
;;       (let ((old-start (overlay-start old))
;;             (old-end   (overlay-end   old))
;;             (old-syntax-max (overlay-get old 'syntax-max))
;;             (old-major (mumamo-chunk-major-mode old)))
;;       (assert old)
;;       (assert old-major)
;;       ;; Fix-me: I forget to refontify something here.
;;       ;;
;;       ;; Fix-me: Clean up among the overlay removal.
;;       ;;
;;       (when (< old-start new-start)
;;         (mumamo-msgfntfy "(move-overlay %s %s %s" old old-start (1- new-start))
;;         (move-overlay old old-start (1- new-start)))
;;       (when (and (<= new-start old-start)
;;                  (<= old-end new-real-end))
;;         (mumamo-msgfntfy "delete-overlay 2")
;;         (delete-overlay old))
;;       ;;(when (< old-start new-start))
;;       ;; There is nothing to do in this case since the conditions
;;       ;; for fontifying have not changed in this region.
;;       (when (< new-real-end old-end)
;;         (mumamo-msgfntfy "new-is-closed=%s" new-is-closed)
;;         (if new-is-closed
;;             (progn
;;               (mumamo-msgfntfy "delete-overlay 3")
;;               (delete-overlay old)
;;               (mumamo-mark-for-refontification (+ new-real-end 1) old-end))
;;           (if (eq (mumamo-chunk-major-mode old) new-major)
;;               (progn
;;                 ;;(move-overlay new-chunk new-start old-end)
;;                 ;; Fix-me: Is this really correct????
;;                 (mumamo-chunk-value-set-min        new-chunk-values new-start)
;;                 (mumamo-chunk-value-set-max        new-chunk-values old-end)
;;                 (mumamo-chunk-value-set-syntax-min new-chunk-values new-syntax-min)
;;                 (mumamo-chunk-value-set-syntax-max new-chunk-values old-syntax-max)
;;                 (mumamo-msgfntfy "delete-overlay 4")
;;                 (delete-overlay old))
;;             (mumamo-msgfntfy "delete-overlay 5")
;;             (delete-overlay old)
;;             (mumamo-mark-for-refontification (+ new-real-end 1) old-end))))))))

(defvar mumamo-chunks-to-remove nil
  "Internal.  Chunk overlays marked for removal.")
(make-variable-buffer-local 'mumamo-chunks-to-remove)

(defun mumamo-flush-chunk-syntax (chunk chunk-min chunk-max)
  "Flush syntax cache for chunk CHUNK."
  ;; syntax-ppss-flush-cache
  (overlay-put chunk 'syntax-ppss-last  nil)
  (overlay-put chunk 'syntax-ppss-cache nil)
  (overlay-put chunk 'syntax-ppss-stats nil)
  (mumamo-save-buffer-state nil
    (remove-text-properties chunk-min chunk-max '(syntax-table nil))))

;; Fix-me: If I open nxhtml-changes.html and then go to the bottom of
;; the file at once syntax-ppss seems to be upset. It is however cured
;; by doing some change above the region that is badly fontified.
(defun mumamo-fontify-region-1 (start end verbose)
  "Fontify region between START and END.
If VERBOSE is non-nil then print status messages during
fontification.

This is called from `mumamo-fontify-region' which is the value of
`font-lock-fontify-region-function' when mumamo is used.  \(This
means that it ties into the normal font lock framework in Emacs.)

Note: The purpose of extracting this function from
`mumamo-fontify-region' \(which is the only place where it is
called) is to make debugging easier.  Edebug will without this
function just step over the `condition-case' in
`mumamo-fontify-region'.

The fontification is done in steps:

- First a mumamo chunk is found or created at the start of the
  region with `mumamo-get-chunk-at'.
- Then this chunk is fontified according to the major mode for
  that chunk.
- If the chunk did not encompass the whole region then this
  procedure is repeated with the rest of the region.

If some mumamo chunk in the region between START and END has been
marked for removal \(for example by `mumamo-jit-lock-after-change') then
they are removed by this function.

For some main major modes \(see `define-mumamo-multi-major-mode') the
main major modes is first used to fontify the whole region.  This
is because otherwise the fontification routines for that mode may
have trouble finding the correct starting state in a chunk.

Special care has been taken for chunks that are strings, ie
surrounded by \"...\" since they are fontified a bit special in
most major modes."
  ;; Fix-me: unfontifying should be done using the correct syntax table etc.
  ;; Fix-me: refontify when new chunk
  (save-match-data
    (let* ((old-point (point))
           (here start)
           (main-major (mumamo-main-major-mode))
           (fontified-t ;;(or mumamo-dbg-pretend-fontified
            ;;    (get-text-property here 'fontified))
            t)
           after-change-functions ;; Fix-me: tested adding this to avoid looping
           (first-new-ovl nil)
           (last-new-ovl nil)
           (chunk-at-start-1 (mumamo-get-existing-chunk-at start)))
      (when chunk-at-start-1
        (unless (= start (1- (overlay-end chunk-at-start-1)))
          (setq chunk-at-start-1 nil)))
      (while (and fontified-t
                  (< here end))
        (mumamo-msgfntfy "mumamo-fontify-region-1 heree 1, here=%s, end=%s" here end)
        ;;(mumamo-assert-fontified-t here end)
        ;;(mumamo-assert-fontified-t start end)
        ;; Check where new chunks should be, adjust old chunks as
        ;; necessary.  Refontify inside end-start and outside of
        ;; start-end mark for refontification when major-mode has
        ;; changed or there was no old chunk.
        ;;
        ;; Fix-me: Join chunks!
        (let* (;;(chunk (mumamo-get-existing-chunk-at here))
               ;;(old-chunk chunk)
               (chunk (mumamo-find-chunks here))
               (chunk-min (when chunk (overlay-start chunk)))
               (chunk-max (when chunk (overlay-end chunk)))
               (chunk-min-1 (when chunk (if (> chunk-min (point-min)) (1- chunk-min) (point-min))))
               (chunk-max-1 (when chunk (if (< chunk-max (point-max)) (1+ chunk-max) (point-max))))
               (chunk-min-face (when chunk (get-text-property chunk-min-1 'face)))
               (chunk-max-face (when chunk (get-text-property chunk-max-1 'face)))
               (chunk-major (when chunk (mumamo-chunk-major-mode chunk)))
               ;;(chunk-values (mumamo-find-chunk-values-at here))
               ;;(cv-min          (mumamo-chunk-value-min          chunk-values))
               ;;(cv-max          (mumamo-chunk-value-max          chunk-values))
               ;;(cv-is-closed    cv-max) ;(mumamo-chunk-value-is-closed    chunk-values))
               ;;(cv-major-sub    (mumamo-chunk-value-major  chunk-values))
               max                    ; (min chunk-max end))
               ;;prev-major
               ;;prev-chunk
               ;;(need-new-chunk nil)
               )
          ;;(unless cv-max (setq cv-max end))
          ;;(unless old-chunk
          ;;  (mumamo-adjust-old-chunks chunk-values nil end)
          ;;  (setq chunk (mumamo-create-chunk-from-chunk-values chunk-values)))
          ;;(when old-chunk
          ;;  ;; Compare with cv:
          ;;  (unless (mumamo-chunk-equal-chunk-values old-chunk chunk-values)
          ;;    (setq need-new-chunk t)))

          ;;(when need-new-chunk
          ;;  ;; Fix-me: unfontify somewhere here
          ;;  (mumamo-adjust-old-chunks chunk-values start end)
          ;;  (setq chunk (mumamo-create-chunk-from-chunk-values chunk-values)))

          (setq chunk-min (when chunk (overlay-start chunk)))
          (setq chunk-max (when chunk (overlay-end chunk)))
          (setq chunk-min-1
                (when chunk
                  (if (> chunk-min (point-min)) (1- chunk-min) (point-min)))) ;chunk-min
          (setq chunk-max-1
                (when chunk
                  (if (< chunk-max (point-max)) (1+ chunk-max) (point-max)))) ;chunk-max
          (setq chunk-min-face
                (when chunk (get-text-property chunk-min-1 'face)))
          (setq chunk-max-face
                (when chunk (get-text-property chunk-max-1 'face)))
          (setq chunk-major (when chunk (mumamo-chunk-major-mode chunk)))

          (if (and first-new-ovl (overlay-buffer first-new-ovl))
              (setq last-new-ovl chunk)
            (setq last-new-ovl chunk)
            (setq first-new-ovl chunk))
          ;;(mumamo-assert-fontified-t chunk-min chunk-max)

          (setq max (min chunk-max end))

          (assert chunk) (assert (overlay-buffer chunk)) (assert chunk-min)
          (assert chunk-max) (assert chunk-major)
          ;; Fix-me: The next assertion sometimes fails.  Could it be
          ;; that this loop is continuing even after a change in the
          ;; buffer? How do I stop that? When?:
          ;;(assert (or (= here start) (= here chunk-min)) nil "h=%s, s=%s, cm=%s-%s, e=%s, chunk-major=%s" here start chunk-min chunk-max end chunk-major)
          ;;(assert (not (eq prev-major chunk-major)))
          ;;(when prev-chunk
          ;;  (assert (= (overlay-end prev-chunk) (overlay-start chunk))))

          ;; Fontify
          (let ((syntax-min (mumamo-chunk-syntax-min chunk))
                (syntax-max (mumamo-chunk-syntax-max chunk))
                (chunk-min (overlay-start chunk))
                (chunk-max (overlay-end chunk)))
            (when (<= here syntax-min)
              (mumamo-flush-chunk-syntax chunk chunk-min chunk-max))
            (when (and (<= here syntax-min)
                       (< chunk-min syntax-min))
              (put-text-property chunk-min syntax-min
                                 'face 'mumamo-border-face))
            (when (and (<= chunk-max max)
                       (< syntax-max chunk-max))
              (put-text-property syntax-max chunk-max
                                 'face 'mumamo-border-face))
            (mumamo-fontify-region-with here max verbose chunk-major
                                        syntax-min syntax-max))

          ;;(setq prev-major chunk-major)
          ;;(setq prev-chunk chunk)
          (setq here max)
          (setq fontified-t (or mumamo-dbg-pretend-fontified
                                (get-text-property here 'fontified)))
          ))
      (goto-char old-point)
      (mumamo-msgfntfy "b first-new-ovl=%s last-new-ovl=%s" first-new-ovl last-new-ovl)
      (unless fontified-t
        ;; Fix-me: I am not sure what to do here.  Probably just
        ;; refontify the rest between start and end.  But does not
        ;; this lead to unnecessary refontification?
        (mumamo-msgfntfy "not sure, here=%s, end=%s" here end)
        (unless (= here (point-max))
          (mumamo-mark-for-refontification here end)))
      ;; Check if more should be refontified due to major mode
      ;; changes.  Compare with old overlays.
      (mumamo-msgfntfy "c first-new-ovl=%s last-new-ovl=%s" first-new-ovl last-new-ovl)
      (let ((ovl-start (min start (overlay-start first-new-ovl)))
            (ovl-end   (max end   (overlay-end   last-new-ovl)))
            (first-new-major (overlay-get first-new-ovl 'mumamo-major))
            (last-new-major  (overlay-get last-new-ovl  'mumamo-major)))
        (mumamo-msgfntfy "*** mumamo-fontify-region-1: here 3 ovl-start=%s,end=%s, start=%s, chunks-to-remove=%s" ovl-start ovl-end start mumamo-chunks-to-remove)
        (when (< ovl-start start)
          ;; Check all old overlays in this region
          (dolist (old-o mumamo-chunks-to-remove)
            (when (overlay-buffer old-o)
              (let ((old-start (overlay-start old-o))
                    (old-end   (overlay-end   old-o))
                    min-refont
                    max-refont)
                (mumamo-msgfntfy "*** mumamo-fontify-region-1: here 3a old-start=%s,end=%s" old-start old-end)
                ;; The trick here is writing this in a manner so that
                ;; you do not have to use paper and pencil to check it:
                (when (< ovl-start old-end)
                  (setq max-refont (min ovl-start old-start)))
                (when (< old-start start)
                  (setq min-refont (max start old-end)))
                (and min-refont
                     max-refont
                     (< min-refont max-refont)
                     (not (eq first-new-major
                              (overlay-get old-o 'mumamo-old-major-mode)))
                     (progn
                       (mumamo-msgfntfy "min-refont/max=%s/%s" min-refont max-refont)
                       (mumamo-mark-for-refontification min-refont max-refont)
                       )
                     )))))
        (mumamo-msgfntfy "*** mumamo-fontify-region-1: here 4")
        (when (< end ovl-end)
          ;; Check all old overlays in this region
          (dolist (old-o mumamo-chunks-to-remove)
            (when (overlay-buffer old-o)
              (let ((old-start (overlay-start old-o))
                    (old-end   (overlay-end   old-o))
                    min-refont
                    max-refont)
                (when (< end old-end)
                  (setq max-refont (min ovl-end old-end)))
                (when (< old-start ovl-end)
                  (setq min-refont (max end old-start)))
                (and min-refont
                     max-refont
                     (< min-refont max-refont)
                     (not (eq last-new-major
                              (overlay-get old-o 'mumamo-old-major-mode)))
                     (progn
                       (mumamo-msgfntfy "2 min-refont/max=%s/%s" min-refont max-refont)
                       (mumamo-mark-for-refontification min-refont
                                                        max-refont))))))))
      ;;(mumamo-remove-old-overlays)
      )))

(defun mumamo-remove-old-overlays ()
  "Remove mumamo overlays marked for removal from the buffer."
  (while mumamo-chunks-to-remove
    (let ((ovl (car mumamo-chunks-to-remove)))
      (setq mumamo-chunks-to-remove (cdr mumamo-chunks-to-remove))
      ;;(unless (overlay-get ovl 'mumamo-old-major-mode) (error "Chunk overlay was not marked for removal"))
      (overlay-put ovl 'mumamo-old-major-mode nil)
      (mumamo-msgfntfy "delete-overlay 6")
      (delete-overlay ovl))))


(defvar mumamo-known-buffer-local-fontifications
  '(
    font-lock-mode-hook
    ;;
    hexcolor-mode
    hi-lock-mode
    hi-lock-file-patterns
    hi-lock-interactive-patterns
    ))

(defconst mumamo-irrelevant-buffer-local-vars
  '(
    ;; This list was fetched with
    ;; emacs-Q, fundamental-mode
    after-change-functions
    ;;auto-composition-function
    ;;auto-composition-mode
    ;;auto-composition-mode-major-mode
    buffer-auto-save-file-format
    buffer-auto-save-file-name
    buffer-backed-up
    buffer-display-count
    buffer-display-time
    buffer-file-format
    buffer-file-name
    buffer-file-truename
    buffer-invisibility-spec
    buffer-read-only
    buffer-saved-size
    buffer-undo-list
    change-major-mode-hook
    ;;char-property-alias-alist
    cursor-type
    default-directory
    delay-mode-hooks
    enable-multibyte-characters
    ;;font-lock-mode
    ;;font-lock-mode-major-mode
    ;;major-mode
    mark-active
    mark-ring
    mode-name
    point-before-scroll
    ;; Handled by font lock etc
    font-lock-defaults
    font-lock-fontified
    font-lock-keywords
    font-lock-keywords-only
    font-lock-keywords-case-fold-search
    font-lock-mode
    font-lock-mode-major-mode
    font-lock-set-defaults
    font-lock-syntax-table
    font-lock-beginning-of-syntax-function
    fontification-functions
    jit-lock-context-unfontify-pos
    jit-lock-mode
    ;; Mumamo
    font-lock-fontify-buffer-function
    jit-lock-contextually
    jit-lock-functions
    ;; More symbols from visual inspection
    before-change-functions
    delayed-mode-hooks
    imenu-case-fold-search
    imenu-generic-expression
    isearch-mode
    line-move-ignore-invisible
    local-abbrev-table
    ;;syntax-ppss-last
    ;;syntax-ppss-cache

    ;; Cua
    cua--explicit-region-start
    ;; Viper
    viper--intercept-key-maps
    viper--key-maps
    viper-ALPHA-char-class
    viper-current-state
    viper-emacs-global-user-minor-mode
    viper-emacs-intercept-minor-mode
    viper-emacs-kbd-minor-mode
    viper-emacs-local-user-minor-mode
    viper-emacs-state-modifier-minor-mode
    viper-insert-basic-minor-mode
    viper-insert-diehard-minor-mode
    viper-insert-global-user-minor-mode
    viper-insert-intercept-minor-mode
    viper-insert-kbd-minor-mode
    viper-insert-local-user-minor-mode
    viper-insert-minibuffer-minor-mode
    viper-insert-point
    viper-insert-state-modifier-minor-mode
    viper-intermediate-command
    viper-last-posn-while-in-insert-state
    viper-minibuffer-current-face
    viper-mode-string
    viper-non-word-characters
    viper-replace-minor-mode
    viper-replace-overlay
    viper-undo-functions
    viper-undo-needs-adjustment
    viper-vi-basic-minor-mode
    viper-vi-diehard-minor-mode
    viper-vi-global-user-minor-mode
    viper-vi-intercept-minor-mode
    viper-vi-kbd-minor-mode
    viper-vi-local-user-minor-mode
    viper-vi-minibuffer-minor-mode
    viper-vi-state-modifier-minor-mode
    ;; hs minor mode
    hs-adjust-block-beginning
    hs-block-start-mdata-select
    hs-block-start-regexp
    hs-c-start-regexp
    hs-forward-sexp-func
    hs-minor-mode
    ;; Imenu
    imenu-case-fold-search
    imenu-generic-expression
    ;; Fix-me: add more here
    ))

(defun mumamo-get-relevant-buffer-local-vars ()
  "Get list of buffer local variables to save.
Like `buffer-local-variables', but remove variables that are
known to not be necessary to save for fontification, indentation
or filling \(or that can even disturb things)."
  (let (var-vals)
    (dolist (vv (buffer-local-variables))
      (unless (or (not (listp vv))
                  (memq (car vv) mumamo-irrelevant-buffer-local-vars)
                  (let* ((sym (car vv))
                         (val (symbol-value sym)))
                    (or (markerp val)
                        (overlayp val))))
        (let ((ent (list (car vv) (custom-quote (cdr vv)))))
          (setq var-vals (cons ent var-vals)))))
    ;; Sorting is for debugging/testing
    (setq var-vals (sort var-vals
                         (lambda (a b)
                           (string< (symbol-name (car a))
                                    (symbol-name (car b))))))
    var-vals))

(defvar mumamo-major-modes-local-maps nil
  "An alist with major mode and local map.
An entry in the list looks like

  \(MAJOR-MODE LOCAL-KEYMAP)")


;; RMS had the following idea:
;;
;; Suppose we add a Lisp primitive to bind a set of variables under
;; the control of an alist.  Would it be possible to eliminate these
;; helper functions and use that primitive instead?
;;
;;;     But wouldn't it be better to test this version first? There is
;;;     no hurry, this version works and someone might find that there
;;;     is a better way to do this than with helper functions.
;;
;; OK with me, as long as this point doesn't get forgotten.
(defun mumamo-fetch-major-mode-setup (major)
  "Return a helper function to do fontification etc like in major mode MAJOR.
Fetch the variables affecting font locking, indentation and
filling by calling the major mode MAJOR in a temporary buffer.

Make a function with one parameter BODY which is elisp code to
eval.  The function should let bind the variables above, sets the
syntax table temporarily to the one used by the major mode
\(using the mode symbol name to find it) and then evaluates body.

Name this function mumamo-eval-in-MAJOR.  Put the code for this
function in the property `mumamo-defun' on this function symbol.


** Some notes about background etc.

The function made here is used in `mumamo-with-major-mode-setup'.
The code in the function parameter BODY is typically involved in
fontification, indentation or filling.

The main reasons for doing it this way is:

- It is faster and than setting the major mode directly.
- It does not affect buffer local variables."
  ;; (info "(elisp) Other Font Lock Variables")
  ;; (info "(elisp) Syntactic Font Lock)
  (let ((func-sym (intern (concat "mumamo-eval-in-" (symbol-name major))))
        temp-buf-name
        temp-buf)
    ;; font-lock-mode can't be turned on in buffers whose names start
    ;; with a char with white space syntax.  Temp buffer names are
    ;; such and it is not possible to change name of a temp buffer.
    (setq temp-buf-name (concat "mumamo-fetch-major-mode-setup-" (symbol-name major)))
    (setq temp-buf (get-buffer temp-buf-name))
    (when temp-buf (kill-buffer temp-buf))
    (setq temp-buf (get-buffer-create temp-buf-name))
    (with-current-buffer temp-buf

      (mumamo-msgfntfy "mumamo-fetch-major-mode-setup %s" major)
      (let ((mumamo-fetching-major t))
        (funcall major)
        )

      (mumamo-msgfntfy ">>> mumamo-fetch-major-mode-setup A font-lock-mode=%s" font-lock-mode)
      (font-lock-mode 1)
      (mumamo-msgfntfy "<<< mumamo-fetch-major-mode-setup B font-lock-mode=%s" font-lock-mode)
      (mumamo-msgfntfy "mumamo-fetch-major-mode-setup: fetching jit-lock-after-change-extend-region-functions A=%s" jit-lock-after-change-extend-region-functions)

      ;; Fix-me: Make this some kind of hook. I think this has better
      ;; wait until mumamo is merged with Emacs. Some changes to
      ;; hi-lock could simplify this. Also there is little use in
      ;; setting this now before negative priorities can be used for
      ;; the background chunks since hi-lock uses background colors
      ;; and those will be invisible since the chunk overlays also
      ;; uses background colors.
;;;       (when (boundp 'hi-lock-interactive-patterns)
;;;         (dolist (hi hi-lock-interactive-patterns)
;;;           (font-lock-add-keywords nil (list hi) t)))
;;;      (run-hooks 'font-lock-mode-hook)

      (font-lock-set-defaults)

      (add-to-list 'mumamo-major-modes-local-maps
                   (let ((local-map (current-local-map)))
                     (cons major-mode (if local-map
                                          (copy-keymap local-map)
                                        'no-local-map))))

      (mumamo-msgfntfy "mumamo-fetch-major-mode-setup: fetching jit-lock-after-change-extend-region-functions B=%s" jit-lock-after-change-extend-region-functions)
      (let* ((syntax-sym (intern-soft (concat (symbol-name major) "-syntax-table")))
             (fetch-func-definition
              `(defun ,func-sym (body)))
             (fetch-func-definition-let
                 ;; Be XML compliant:
                 (list
                  (list 'sgml-xml-mode
                   (when (mumamo-derived-from-mode ',major 'sgml-mode) t))

                  ;; We need to copy the variables that we need and
                  ;; that are not automatically buffer local, but
                  ;; could be it. Arguably it is a bug if they are not
                  ;; buffer local though we have to adapt.

                  ;; From cc-mode.el:
                  (list 'indent-line-function (custom-quote indent-line-function))
                  (list 'indent-region-function (custom-quote indent-region-function))
                  (list 'normal-auto-fill-function (custom-quote normal-auto-fill-function))
                  (list 'comment-start (custom-quote comment-start))
                  (list 'comment-end (custom-quote comment-end))
                  (list 'comment-start-skip (custom-quote comment-start-skip))
                  (list 'comment-end-skip (custom-quote comment-end-skip))
                  (list 'comment-multi-line (custom-quote comment-multi-line))
                  (list 'comment-line-break-function (custom-quote comment-line-break-function))
                  (list 'paragraph-start (custom-quote paragraph-start))
                  (list 'paragraph-separate (custom-quote paragraph-separate))
                  (list 'paragraph-ignore-fill-prefix (custom-quote paragraph-ignore-fill-prefix))
                  (list 'adaptive-fill-mode (custom-quote adaptive-fill-mode))
                  (list 'adaptive-fill-regexp (custom-quote adaptive-fill-regexp))

                 ;;; Try doing the font lock things last, keywords really last
                  (list 'font-lock-multiline (custom-quote font-lock-multiline))
                  (list 'font-lock-extend-after-change-region-function (custom-quote font-lock-extend-after-change-region-function))
                  (list 'font-lock-extend-region-functions (custom-quote font-lock-extend-region-functions))
                  (list 'font-lock-comment-start-skip (custom-quote font-lock-comment-start-skip))
                  (list 'font-lock-comment-end-skip (custom-quote font-lock-comment-end-skip))
                  (list 'font-lock-syntactic-keywords (custom-quote font-lock-syntactic-keywords))

                  (list 'font-lock-set-defaults) ; whether we have set up defaults.

                  ;; Set from font-lock-defaults normally:
                  (list 'font-lock-defaults (custom-quote (copy-tree font-lock-defaults)))
                  ;; Syntactic Font Lock
                  ;;(set 'font-lock-syntax-table (custom-quote font-lock-syntax-table))
                  (list 'font-lock-beginning-of-syntax-function (custom-quote font-lock-beginning-of-syntax-function))
                  (list 'font-lock-syntactic-face-function (custom-quote font-lock-syntactic-face-function))

                  ;; Other Font Lock Variables
                  (list 'font-lock-mark-block-function (custom-quote font-lock-mark-block-function))
                  (list 'font-lock-extra-managed-props (custom-quote font-lock-extra-managed-props))
                  ;; This value is fetched from font-lock:
                  (list 'font-lock-fontify-buffer-function (custom-quote font-lock-fontify-buffer-function))
                  (list 'font-lock-unfontify-buffer-function (custom-quote font-lock-unfontify-buffer-function))
                  (list 'font-lock-fontify-region-function (custom-quote font-lock-fontify-region-function))
                  (list 'font-lock-unfontify-region-function (custom-quote font-lock-unfontify-region-function))

                  ;; Jit Lock Variables
                  (list 'jit-lock-after-change-extend-region-functions (custom-quote jit-lock-after-change-extend-region-functions))

                  ;;(list 'syntax-table (custom-quote (copy-syntax-table (syntax-table))))
                  ;;(list 'mumamo-original-syntax-begin-function (custom-quote syntax-begin-function))
                  (list 'syntax-begin-function (custom-quote syntax-begin-function))
                  (list 'fill-paragraph-function (custom-quote fill-paragraph-function))
                  (list 'fill-forward-paragraph-function
                        (when (boundp 'fill-forward-paragraph-function)
                          (custom-quote fill-forward-paragraph-function)))

                  ;; newcomment
                  (list 'comment-use-global-state (custom-quote (when (boundp 'comment-use-global-state) comment-use-global-state)))

                  ;; parsing sexps
                  (list 'multibyte-syntax-as-symbol (custom-quote multibyte-syntax-as-symbol))
                  (list 'parse-sexp-ignore-comments (custom-quote parse-sexp-ignore-comments))
                  (list 'parse-sexp-lookup-properties (custom-quote parse-sexp-lookup-properties))
                  ;; fix-me: does not the next line work?
                  (list 'forward-sexp-function (custom-quote forward-sexp-function))
                  ))
             (relevant-buffer-locals (mumamo-get-relevant-buffer-local-vars))
                 )
        ;;(append '(1 2) '(3 4) '((eval body)))
        (mumamo-msgfntfy "===========> before setq fetch-func-definition %s" func-sym)
        ;; Avoid doublets
        (dolist (fetched fetch-func-definition-let)
          (let ((fvar (car fetched)))
            (setq relevant-buffer-locals (assq-delete-all fvar relevant-buffer-locals))))
        (setq fetch-func-definition
              (append fetch-func-definition
                      (list
                       (list 'let
                             (append
                              fetch-func-definition-let
                              relevant-buffer-locals
                              )
                             (list 'with-syntax-table
                                   ;;(list 'if syntax-sym syntax-sym
                                   (if syntax-sym
                                       syntax-sym
                                     (list 'standard-syntax-table)
                                     );;'syntax-table
                                   ;; I can't see why it should be
                                   ;; needed to call
                                   ;; `font-lock-set-defaults' now:
;;;                                    '(let ((t1 (get-internal-run-time))
;;;                                           t2)
;;;                                       (setq font-lock-set-defaults nil)
;;;                                       (font-lock-set-defaults)
;;;                                       (setq t2 (get-internal-run-time))
;;;                                       (mumamo-msgfntfy "font-lock-set-defaults runtime=%s, %s" t1 t2)
;;;                                       t2
;;;                                       )

;;;                                 '(message "eval body: syntax-table=html ? %s, not flsyn-tab=%s"
;;;                                       (equal
;;;                                             (syntax-table)
;;;                                             html-mode-syntax-table)
;;;                                       (not font-lock-syntax-table))
;;;                                 '(message "  syntax-table=html ? %s" (equal (syntax-table) html-mode-syntax-table))
;;;                                 '(message "  sgml-font-lock-syntactic-keywords=%S" sgml-font-lock-syntactic-keywords)
;;;                                 '(message "  font-lock-syntactic-keywords=%S" font-lock-syntactic-keywords)
;;;                                 '(message "  font-lock-syntactic-keywords=html? %s"
;;;                                           (or (equal font-lock-syntactic-keywords 'sgml-font-lock-syntactic-keywords)
;;;                                               (equal (nth 1 font-lock-syntactic-keywords) sgml-font-lock-syntactic-keywords)))
;;;                                 '(message "  multibyte-syntax-as-symbol=%s parse-sexp-ignore-comments=%s parse-sexp-lookup-properties=%s"
;;;                                           multibyte-syntax-as-symbol
;;;                                           parse-sexp-ignore-comments
;;;                                           parse-sexp-lookup-properties)

                                (list 'eval 'body))))))
        (eval fetch-func-definition)
        (byte-compile fetch-func-definition)
        (mumamo-msgfntfy "===========> after eval")
        ;; Silence the byte compiler:
        (let ((major-syntax-table))
          (byte-compile func-sym))
        (mumamo-msgfntfy "===========> after byte-compile")
        (put func-sym 'mumamo-defun fetch-func-definition)
        ))
    (kill-buffer temp-buf)
    func-sym))


(defun mumamo-bad-mode ()
  "MuMaMo replacement for a major mode that could not be loaded."
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'mumamo-bad-mode)
  (setq mode-name
        (propertize "Mumamo Bad Mode"
                    'face 'font-lock-warning-face)))

;;(mumamo-get-major-mode-setup 'css-mode)
;;(mumamo-get-major-mode-setup 'fundamental-mode)
(defun mumamo-get-major-mode-setup (use-major)
  "Return function for evaluating code in major mode USE-MAJOR.
Fix-me: This doc string is wrong, old:

Get local variable values for major mode USE-MAJOR.  These
variables are used for indentation and fontification.  The
variables are returned in a list with the same format as
`mumamo-fetch-major-mode-setup'.

The list of local variable values which is returned by this
function is cached in `mumamo-internal-major-modes-alist'. This
avoids calling the major mode USE-MAJOR for each chunk during
fontification and speeds up fontification significantly."
  ;; Fix-me: Problems here can cause mumamo to loop badly when this
  ;; function is called over and over again. To avoid this add a
  ;; temporary entry using mumamo-bad-mode while trying to fetch the
  ;; correct mode.

  ;;(assq 'mumamo-bad-mode mumamo-internal-major-modes-alist)
  (let ((use-major-entry (assq use-major mumamo-internal-major-modes-alist))
        bad-mode-entry
        dummy-entry)
    (unless use-major-entry
      ;; Get mumamo-bad-mode entry and add a dummy entry based on
      ;; this to avoid looping.
      (setq bad-mode-entry
            (assq 'mumamo-bad-mode mumamo-internal-major-modes-alist))
      (unless bad-mode-entry
        ;; Assume it is safe to get the mumamo-bad-mode entry ;-)
        (add-to-list 'mumamo-internal-major-modes-alist
                     (list 'mumamo-bad-mode
                           (mumamo-fetch-major-mode-setup
                            'mumamo-bad-mode)))
        (setq bad-mode-entry
              (assq 'mumamo-bad-mode mumamo-internal-major-modes-alist)))
      (setq dummy-entry (list use-major (cadr bad-mode-entry)))
      ;; Before fetching setup add the dummy entry and then
      ;; immediately remove it.
      (add-to-list 'mumamo-internal-major-modes-alist dummy-entry)
      (setq use-major-entry (list use-major
                                  (mumamo-fetch-major-mode-setup
                                   use-major)))
      (setq mumamo-internal-major-modes-alist
            (delete dummy-entry
                    mumamo-internal-major-modes-alist))
      (add-to-list 'mumamo-internal-major-modes-alist use-major-entry)
      ))

  (cadr (or (assq use-major mumamo-internal-major-modes-alist)
            (assq use-major
                  (add-to-list 'mumamo-internal-major-modes-alist
                               (list use-major
                                     (mumamo-fetch-major-mode-setup
                                      use-major)))))))

;; Fix-me: This is to drastic since after-change-functions are run
;; immediately after a change.  It breaks indentation for
;; example.  Change this to not remove the chunks but just mark them
;; for maybe removing.  They should still be used until new chunks are
;; created by fontification.
(defun mumamo-remove-chunk-overlays (min max)
  "Mark chunk overlays in MIN to MAX as old.
Return as a cons region covered by those overlays if greater than
MIN to MAX, otherwise MIN to MAX."
  (mumamo-msgfntfy "mumamo-remove-chunk-overlays %s %s, modified=%s" min max (buffer-modified-p))
  ;;(mumamo-assert-fontified-t min max)
  (let ((min-min min)
        (max-max max)
        (did-remove nil))
    (dolist (o (overlays-in min max))
      (when (mumamo-chunk-major-mode o)
        (when (< max (overlay-end o))
          (setq max-max (overlay-end o)))
        (when (> min (overlay-start o))
          (setq min-min (overlay-start o)))
        ;; Save the old major mode so that we can compare with it:
        (overlay-put o 'mumamo-old-major-mode (mumamo-chunk-major-mode o))
        (overlay-put o 'mumamo-major-mode nil)
        ;; Fix-me: There must be something wrong. If we remove the
        ;; overlay there is no need to save it.
        (mumamo-msgfntfy "delete-overlay 7")
        (delete-overlay o)
        (setq did-remove t)
        (setq mumamo-chunks-to-remove (cons o mumamo-chunks-to-remove))))
    (mumamo-msgfntfy "  exit mumamo-remove-chunk-overlays %s %s, modified=%s" min max (buffer-modified-p))
    ;;(mumamo-assert-fontified-t min max)
    (when did-remove (cons min-min max-max))))

(defun mumamo-remove-all-chunk-overlays ()
  "Remove all CHUNK overlays from the current buffer."
  (save-restriction
    (widen)
    (mumamo-remove-chunk-overlays (point-min) (point-max))
    (mumamo-remove-old-overlays)))





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Creating and accessing chunks

;; (defun mumamo-find-chunk-values-at (pos)
;;   "Return a list of values for creating a chunk at POS after last chunk.
;; Like `mumamo-create-chunk-values-at, but first create chunks
;; before POS."
;;   (mumamo-msgfntfy "********* computing new-way and old-way")
;;   (let ((new-way (mumamo-find-chunks pos 'last-as-values))
;;         ;; Fix-me: remove
;;         (old-way (mumamo-create-chunk-values-at pos)))
;;     (mumamo-msgfntfy "new-way=%s" new-way)
;;     (mumamo-msgfntfy "old-way=%s" old-way)
;;     (setcar (nthcdr 6 old-way) (nth 6 new-way))
;;     (unless (equal new-way old-way)
;;       (message "ERROR: new-way /= old-way, pos=%s => %s /= %s" pos new-way old-way))
;;     new-way)
;;   )

(defun mumamo-create-chunk-values-at (pos)
  "Return a list of values to be used to create a chunk at POS."
  (assert mumamo-current-chunk-family)
  (let* ((chunk-info (cdr mumamo-current-chunk-family))
         (chunk-fns    (cadr chunk-info))
         (here (point))
         min
         max
         border-min
         border-max
         parseable
         (max-found nil)
         major-sub)
    ;; Fix-me: maybe assume previous chunk is trustworthy if it ends
    ;; before pos. But check first at which pos this function is
    ;; called relative to old chunks.

;;;     (unless t ;(overlays-at pos)
;;;       (let* ((ovl-pos pos)
;;;              (prev-end (previous-overlay-change ovl-pos))
;;;              ;; fix-me: start vs end
;;;              (prev-chunk (when prev-end
;;;                            (mumamo-get-existing-chunk-at prev-end))))
;;;         (when prev-chunk
;;;           (unless (= prev-end (overlay-end prev-chunk))
;;;             (setq prev-chunk nil)))
;;;         ;; fix-me: use ovl-pos to find chunk? That would make ppss work I believe.
;;;         (while prev-chunk
;;;           (if (mumamo-chunk-major-mode prev-chunk)
;;;               (progn
;;;                 (setq min (1+ (overlay-end prev-chunk)))
;;;                 (setq prev-chunk nil))
;;;             (setq ovl-pos (1- (overlay-start prev-chunk)))
;;;             (if (< ovl-pos (point-min))
;;;                 (setq prev-chunk nil)
;;;               (setq prev-end (previous-overlay-change ovl-pos))
;;;               (setq prev-chunk (when prev-end (mumamo-get-existing-chunk-at prev-end)))
;;;               )))))
    (dolist (fn chunk-fns)
      (let* (
             (r (funcall fn pos (point-min) (point-max)))
             (rmin       (nth 0 r))
             (rmax       (nth 1 r))
             (rmajor-sub (nth 2 r))
             (rborder    (nth 3 r))
             (rparseable (nth 4 r))
             (rborder-min (when rborder (nth 0 rborder)))
             (rborder-max (when rborder (nth 1 rborder)))
             (rmax-found rmax))
        (mumamo-msgfntfy "  fn=%s, r=%s" fn r)
        (unless rmin (setq rmin (point-min)))
        (unless rmax (setq rmax (point-max)))
        ;; Do not allow zero length chunks
        (unless (and (> rmin 1) (= rmin rmax))
          ;; comparision have to be done differently if we are in an
          ;; exception part or not.  since we are doing this from top to
          ;; bottom the rules are:
          ;;
          ;; - exception parts always outrules non-exception part.  when
          ;;   in exception part the min start point should be used.
          ;; - when in non-exception part the max start point and the
          ;;   min end point should be used.
          ;;
          ;; check if first run:

          ;; Fix-me: there is some bug here when borders are not
          ;; included and are not 0 width.
          (if (not min)
              (progn
                (setq min rmin)
                (setq border-min rborder-min)
                (setq max rmax)
                (setq border-max rborder-max)
                (setq max-found rmax-found)
                (setq parseable rparseable)
                (setq major-sub rmajor-sub))
            (if rmajor-sub
                (if major-sub
                    (when (or (not min)
                              (< rmin min))
                      (setq min rmin)
                      (setq border-min rborder-min)
                      (setq max rmax)
                      (setq border-max rborder-max)
                      (when rmax-found (setq max-found t))
                      (setq parseable rparseable)
                      (setq major-sub rmajor-sub))
                  (setq min rmin)
                  (setq border-min rborder-min)
                  (setq max rmax)
                  (setq border-max rborder-max)
                  (when rmax-found (setq max-found t))
                  (setq parseable rparseable)
                  (setq major-sub rmajor-sub))
              (unless major-sub
                (when (< min rmin)
                  (setq min rmin)
                  (setq border-min rborder-min))
                (when (< rmax max)
                  (setq max-found rmax-found)
                  (setq max rmax)
                  (setq border-max rborder-max))
                ))))
        (mumamo-msgfntfy "min/max=%s/%s border=%s/%s pos=%s" min max border-min border-max pos)
        ;; check!
        (when (and min max)
          (assert (<= min pos) t)
          (assert (<= pos max) t)
          (when border-min
            (assert (< min border-min) t)
            (assert (<= border-min max) t))
          (when border-max
            (assert (<= min border-max) t)
            (assert (< border-max max) t)))))
    ;;(list min (when max-found max) major-sub syntax-min syntax-max)
    (goto-char here)
    (list min (when max-found max) major-sub border-min border-max parseable)
    ))

(defun mumamo-define-no-mode (mode-sym)
  (let ((mumamo-repl4 (intern (format "mumamo-4-%s" mode-sym)))
        (lighter (format "No %s" mode-sym))
        (doc (format "MuMaMo replacement for %s which was not found."
                     mode-sym)))
    (if (commandp mumamo-repl4)
        mumamo-repl4
      (eval `(defun ,mumamo-repl4 ()
               ,doc
               (interactive)
               (kill-all-local-variables)
               (setq major-mode ',mumamo-repl4)
               (setq mode-name
                     (propertize ,lighter
                                 'face 'font-lock-warning-face)))))))
;;(mumamo-define-no-mode 'my-ownB-mode)

(defun mumamo-major-mode-from-modespec (major-spec)
  "Translate MAJOR-SPEC to a major mode.
Translate MAJOR-SPEC used in chunk definitions of multi major
modes to a major mode.

See `mumamo-major-modes' for an explanation."
  (let ((modes (cdr (assq major-spec mumamo-major-modes)))
        (mode 'mumamo-bad-mode))
    (setq mode
          (catch 'mode
            (dolist (m modes)
              (when (functionp m)
                (let ((def (symbol-function m)))
                  (when (and (listp def)
                             (eq 'autoload (car def)))
                    (mumamo-condition-case err
                        (load (nth 1 def))
                      (error (setq m nil)))))
                (when m (throw 'mode m))))
            nil))
    (unless mode
      (if (functionp major-spec)
          ;; As a last resort allow spec to be a major mode too:
          (setq mode major-spec)
        (if modes
            (mumamo-warn-once '(mumamo-major-mode-from-modespec)
                              "Couldn't find an available major mode for specification %s,\n  alternatives are:\n    %s"
                              major-spec modes)
          (mumamo-warn-once '(mumamo-major-mode-from-modespec)
                 "Couldn't find an available major mode for spec %s"
                 major-spec))
        ;;(setq mode 'fundamental-mode)
        (setq mode (mumamo-define-no-mode major-spec))
        ))
    (mumamo-msgfntfy " mumamo-major-mode-from-modespec %s => %s" major-spec mode)
    mode))
;(mumamo-major-mode-from-modespec 'ruby-mode)

;; (defun mumamo-ceqcv ()
;;   (mumamo-chunk-equal-chunk-values (mumamo-get-existing-chunk-at (point))
;;                                    (mumamo-create-chunk-values-at (point))))

(defun mumamo-chunk-equal-chunk-values (chunk chunk-values)
  "Return non-nil if CHUNK corresponds to CHUNK-VALUES."
  ;;(message "mumamo-chunk-equal-chunk-values, c=%s, v=%s" chunk chunk-values)
  (let* ((chunk-min (overlay-start chunk))
         (chunk-max (overlay-end chunk))
         (chunk-syntax-min-d    (overlay-get chunk 'syntax-min-d))
         (chunk-syntax-max-d    (overlay-get chunk 'syntax-max-d))
         (chunk-syntax-min    (when (and chunk-syntax-min-d chunk-min) (+ chunk-min chunk-syntax-min-d)))
         (chunk-syntax-max    (when (and chunk-syntax-max-d chunk-max) (- chunk-max chunk-syntax-max-d)))
         (chunk-major-sub     (overlay-get chunk 'mumamo-major-mode))
         (chunk-parseable-by (overlay-get chunk 'mumamo-parseable-by))
         (val-syntax-min (mumamo-chunk-value-syntax-min chunk-values))
         (val-syntax-max (mumamo-chunk-value-syntax-max chunk-values))
         (val-major-sub (let ((mode-spec (mumamo-chunk-value-major  chunk-values)))
                          (if mode-spec
                              (mumamo-major-mode-from-modespec mode-spec)
                            (mumamo-main-major-mode))))
         )
    (and (eq chunk-min (or (mumamo-chunk-value-min chunk-values)
                           (point-min)))
         (eq chunk-max (or (mumamo-chunk-value-max chunk-values)
                           (point-max)))
         (eq chunk-syntax-min val-syntax-min)
         (eq chunk-syntax-max val-syntax-max)
         (eq chunk-major-sub val-major-sub)
         (equal chunk-parseable-by (mumamo-chunk-value-parseable-by  chunk-values))
         ;;(or (message " equal") t)
         )))

(defun mumamo-create-chunk-from-chunk-values (chunk-values)
  "Create a chunk from CHUNK-VALUES and return it.
CHUNK-VALUES should be in the format returned by
`mumamo-create-chunk-values-at'."
  (mumamo-msgfntfy "mumamo-create-chunk-from-chunk-values %s" chunk-values)
  ;; Fix-me: Move adjusting of old chunks to here since it must always
  ;; be done.
  (let* ((min           (mumamo-chunk-value-min    chunk-values))
         (max           (mumamo-chunk-value-max    chunk-values))
         (syntax-min    (mumamo-chunk-value-syntax-min chunk-values))
         (syntax-max    (mumamo-chunk-value-syntax-max chunk-values))
         (major-sub     (mumamo-chunk-value-major  chunk-values))
         (parseable-by  (mumamo-chunk-value-parseable-by  chunk-values))
         ;;(prev-chunk    (mumamo-chunk-prev-chunk   chunk-values))
         (major-normal (mumamo-main-major-mode))
         (max-found    (when max t))
         (prev-chunk   (mumamo-get-existing-chunk-at (1- min)))
         (prev-major   (and prev-chunk
                            (mumamo-chunk-major-mode prev-chunk)))
         (prev-same    (and prev-chunk
                            (eq prev-major
                                (if major-sub
                                    major-sub
                                  major-normal))))
         ;;(prev-min      (when prev-same (overlay-start prev-chunk)))
         chunk-ovl)
    (assert major-normal)
    ;; remove all old chunk overlays between min and max
    ;; Fix-me: Must keep track of those to know how much to refontify:
    (unless max-found  (setq max (point-max)))
    ;;(mumamo-remove-chunk-overlays min max)
    ;;(message "min-max=%s-%s, prev-chunk=%s, prev-major=%s, prev-same=%s, major-sub=%s" min max prev-chunk prev-major prev-same major-sub)
    (setq chunk-ovl (make-overlay min max))
    (overlay-put chunk-ovl 'mumamo-prev-chunk prev-chunk)
    (overlay-put chunk-ovl 'mumamo-is-closed max-found)
    ;; Make syntax border width positive integers:
    (overlay-put chunk-ovl 'syntax-min-d (when syntax-min (- syntax-min min)))
    (overlay-put chunk-ovl 'syntax-max-d (when syntax-max (- max syntax-max)))
;;;     (when (and (= min (point-min))
;;;                (= max (point-max)))
;;;       ;; Fix-me: I believe this is not needed any more and it creates
;;;       ;; trouble for files starting in a sub mode, for example php
;;;       ;; files.  They will get the wrong major mode in the
;;;       ;; chunk.
;;;       ;;
;;;       ;;(setq major-sub nil)
;;;       )
    ;; Get syntax-begin-function for syntax-ppss:
    (let* ((major (if major-sub major-sub major-normal))
           (syntax-begin-function
            (mumamo-with-major-mode-fontification major
              ;; Do like in syntax.el:
              '(if syntax-begin-function
                   (progn
                     syntax-begin-function)
                 (when (and (not syntax-begin-function)
                            ;; fix-me: How to handle boundp here?
                            (boundp 'font-lock-beginning-of-syntax-function)
                            font-lock-beginning-of-syntax-function)
                   font-lock-beginning-of-syntax-function)))))
      (mumamo-msgfntfy "Got syntax-begin-function, modified=%s" (buffer-modified-p))
      (overlay-put chunk-ovl 'syntax-begin-function syntax-begin-function))

    (if major-sub
        (let ((major-sub-to-use (mumamo-major-mode-from-modespec major-sub)))
          (remove-list-of-text-properties min max '(category))
          (overlay-put chunk-ovl 'mumamo-major-mode major-sub-to-use)
          (overlay-put chunk-ovl 'mumamo-parseable-by parseable-by)
          (overlay-put chunk-ovl
                       'face
                       (when (memq mumamo-chunk-coloring
                                   '(submode-colored both-colored))
                         mumamo-background-chunk-submode)))
      (overlay-put chunk-ovl 'mumamo-major-mode major-normal)
      (overlay-put chunk-ovl
                   'face
                   (when (memq mumamo-chunk-coloring '(both-colored))
                     mumamo-background-chunk-major)))
    (assert (mumamo-chunk-major-mode chunk-ovl))
    (unless (mumamo-valid-nxml-chunk chunk-ovl)
      (rng-clear-overlays min max))
    (when (and (not parseable-by)
               (not major-sub)
               nil ;; Fix-me: I can't understand the next line...
               (mumamo-derived-from-mode major-normal 'nxml-mode))
      (setq parseable-by '(nxml-mode)))
    (mumamo-with-buffer-prepared-for-jit-lock
     (put-text-property min max 'mumamo-parseable-by parseable-by))
    (mumamo-msgfntfy "after put mumamo-parseable-by, modified=%s" (buffer-modified-p))
    (unless (memq 'nxml-mode parseable-by)
      (mumamo-with-buffer-prepared-for-jit-lock
       (remove-text-properties min max '(category rng-error))))
    (let ((ovls (overlays-in min max)))
      (dolist (ovl ovls)
        (let ((ctg (overlay-get ovl 'category)))
          ;;(message "ctg=%s" ctg)
          (when (memq ctg '(nxml-dependent rng-dependent rng-error))
            (mumamo-msgfntfy "delete-overlay 8")
            (delete-overlay ovl))
          )))
    (mumamo-msgfntfy "leaving mumamo-create-chunk-from-chunk-values modified=%s" (buffer-modified-p))
    ;;(setq mumamo-end-last-chunk-pos (overlay-end chunk-ovl))
    (setq mumamo-last-chunk chunk-ovl)
    chunk-ovl))

(defun mumamo-create-chunk-at (pos)
  "Create and return a new chunk at POS.
There must not be an old chunk there.  Mark for refontification."
  (assert (not (mumamo-get-existing-chunk-at pos)))
;;;   (let ((new-chunk-values (mumamo-find-chunk-values-at pos))
;;;         (new-chunk))
;;;     (mumamo-msgfntfy "mumamo-create-chunk-at %s, ncv=%s" pos new-chunk-values)
;;;     (mumamo-adjust-old-chunks new-chunk-values nil nil)
;;;     (setq new-chunk (mumamo-create-chunk-from-chunk-values new-chunk-values nil))
;;;     (mumamo-mark-for-refontification (overlay-start new-chunk) (overlay-end new-chunk))
;;;     new-chunk))
  (mumamo-find-chunks pos))

(defun mumamo-get-existing-chunk-at (pos)
  "Return existing chunk at POS if any."
  (let ((chunk-ovl))
    (when (= pos (point-max))
      (setq pos (1- pos)))
    (dolist (o (overlays-at pos))
      (unless chunk-ovl
        (when ;;(mumamo-chunk-major-mode o)
            (and (overlay-get o 'mumamo-major-mode)
                 (not (overlay-get o 'mumamo-is-old)))
          (setq chunk-ovl o))))
    chunk-ovl))

(defun mumamo-get-chunk-save-buffer-state (pos)
  "Return chunk overlay at POS. Preserve state."
  (let (chunk)
    (mumamo-save-buffer-state nil
      (setq chunk (mumamo-get-chunk-at pos)))
    chunk))

(defun mumamo-get-chunk-at (pos)
  "Return chunk overlay at POS.
Create it if it does not exist.  How to do this is governed by
`mumamo-current-chunk-family'.

A mumamo chunk is an Emacs overlay with some properties telling
how mumamo should handle the chunk during fontification,
indentation etc."
  (let ((chunk-ovl (mumamo-get-existing-chunk-at pos)))
    (if chunk-ovl
        ;;(mumamo-msgfntfy "existing %s %s" pos chunk-ovl)
        (unless (and (<= (overlay-start chunk-ovl) pos)
                     (<= pos (overlay-end chunk-ovl)))
          (error "Mumamo-get-chunk-at: start=%s, pos=%s, end=%s"
                   (overlay-start chunk-ovl) pos (overlay-end chunk-ovl)))
      (setq chunk-ovl (mumamo-create-chunk-at pos)))
    chunk-ovl))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Chunk and chunk family properties

(defsubst mumamo-chunk-major-mode (chunk)
  "Get major mode specified in CHUNK."
  ;;(assert chunk)
  ;;(assert (overlay-buffer chunk))
  (if chunk
      (overlay-get chunk 'mumamo-major-mode)
    major-mode))

(defsubst mumamo-chunk-syntax-min (chunk)
  "Get min syntactically safe point inside mumamo chunk CHUNK.
Syntax here refer to the syntax handled by `syntax-ppss' etc."
;;  (overlay-start chunk))
  (+ (overlay-start chunk)
     (or (overlay-get chunk 'syntax-min-d)
         0)))

(defsubst mumamo-chunk-syntax-max (chunk)
  "Get max point where syntax is consistent inside mumamo chunk CHUNK.
See `mumamo-chunk-syntax-min'."
  (- (overlay-end chunk)
     (or (overlay-get chunk 'syntax-max-d)
         0)))

(defun mumamo-syntax-maybe-completable (pnt)
  "Return non-nil if at point PNT non-printable characters may occur.
This just considers existing chunks."
  (let ((chunk (mumamo-get-existing-chunk-at pnt)))
    (if (not chunk)
        t
      (and (> pnt (1+ (mumamo-chunk-syntax-min chunk)))
           (< pnt (1- (mumamo-chunk-syntax-max chunk)))))))

(defvar mumamo-current-chunk-family nil
  "The currently used chunk family.")
(make-variable-buffer-local 'mumamo-current-chunk-family)
(put 'mumamo-current-chunk-family 'permanent-local t)

;; (defvar mumamo-main-major-mode nil)
;; (make-variable-buffer-local 'mumamo-main-major-mode)
;; (put 'mumamo-main-major-mode 'permanent-local t)

(defun mumamo-main-major-mode ()
  "Return major mode used when there are no chunks."
  (cadr mumamo-current-chunk-family))
;;;   (let ((main (cadr mumamo-current-chunk-family)))
;;;     (if main
;;;         main
;;;       mumamo-main-major-mode)))

;; (defun mumamo-unset-chunk-family ()
;;   "Set chunk family to nil, ie undecided."
;;   (interactive)
;;   (setq mumamo-current-chunk-family nil))

;; (defun mumamo-define-chunks (chunk-family)
;;   "Set the CHUNK-FAMILY used to divide the buffer."
;;   (setq mumamo-current-chunk-family chunk-family))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; General chunk search routines

;; search start forward

;;(defun mumamo-search-fw-exc-start-str (pos max marker)
(defun mumamo-chunk-start-fw-str (pos max marker)
  "General chunk function helper.
A chunk function helper like this can be used in
`mumamo-find-possible-chunk' to find the borders of a chunk.
There are several functions like this that comes with mumamo.
Their names tell what they do.  Lets look at the parts of the
name of this function:

  mumamo-chunk: All this helper functions begins so
  -start-: Search for the start of a chunk
  -fw-: Search forward
  -str: Search for a string

Instead of '-start-' there could be '-end-', ie end.
Instead of '-fw-' there could be '-bw-', ie backward.
Instead of '-str' there could be '-re', ie regular expression.

There could also be a '-inc' at the end of the name.  If the name
ends with this then the markers should be included in the chunks,
otherwise not.

The argument POS means where to start the search.  MAX means how
far to search (when searching backwards the argument is called
'min' instead).  MARKER is a string or regular expression (see
the name) to search for."
  (assert (stringp marker))
  (goto-char (- pos (length marker)))
  (search-forward marker max t))

(defun mumamo-chunk-start-fw-re (pos max marker)
  "General chunk function helper.
See `mumamo-chunk-start-fw-str' for more information and the
meaning of POS, MAX and MARKER."
  (assert (stringp marker))
  (goto-char (- pos (length marker)))
  (re-search-forward marker max t))

(defun mumamo-chunk-start-fw-str-inc (pos max marker)
  "General chunk function helper.
See `mumamo-chunk-start-fw-str' for more information and the
meaning of POS, MAX and MARKER."
  (assert (stringp marker))
  (goto-char pos)
  (let ((start (search-forward marker max t)))
    (when start (setq start (- start (length marker))))))

;; search start backward

(defun mumamo-chunk-start-bw-str (pos min marker)
  "General chunk function helper.
See `mumamo-chunk-start-fw-str' for more information and the
meaning of POS, MIN and MARKER."
  (assert (stringp marker))
  (let (start-in)
    (goto-char pos)
    (setq start-in (search-backward marker min t))
    (when start-in
      ;; do not include the marker
      (setq start-in (+ start-in (length marker))))
    start-in))

(defun mumamo-chunk-start-bw-re (pos min marker)
  "General chunk function helper.
See `mumamo-chunk-start-fw-str' for more information and the
meaning of POS, MIN and MARKER."
  (assert (stringp marker))
  (let (start-in)
    (goto-char pos)
    (setq start-in (re-search-backward marker min t))
    (when start-in
      ;; do not include the marker
      (setq start-in (match-end 0)))
    start-in))

(defun mumamo-chunk-start-bw-str-inc (pos min marker)
  "General chunk function helper.
See `mumamo-chunk-start-fw-str' for more information and the
meaning of POS, MIN and MARKER."
  (assert (stringp marker))
  (goto-char (+ pos (length marker)))
  (search-backward marker min t))

;; search end forward

(defun mumamo-chunk-end-fw-str (pos max marker)
  "General chunk function helper.
See `mumamo-chunk-start-fw-str' for more information and the
meaning of POS, MAX and MARKER."
  (assert (stringp marker))
  (goto-char (1+ pos)) ;; 1+ cause otherwise ?> is at point
  (let (end-in)
    (setq end-in (search-forward marker max t))
    (when end-in
      ;; do not include the marker
      (setq end-in (- end-in (length marker))))
    end-in))

(defun mumamo-chunk-end-fw-re (pos max marker)
  "General chunk function helper.
See `mumamo-chunk-start-fw-str' for more information and the
meaning of POS, MAX and MARKER."
  (assert (stringp marker))
  (goto-char (1+ pos)) ;; 1+ cause otherwise ?> is at point
  (let (end-in)
    (setq end-in (re-search-forward marker max t))
    (when end-in
      ;; do not include the marker
      (setq end-in (match-beginning 0)))
    end-in))

(defun mumamo-chunk-end-fw-str-inc (pos max marker)
  "General chunk function helper.
See `mumamo-chunk-start-fw-str' for more information and the
meaning of POS, MAX and MARKER."
  (assert (stringp marker))
  ;;(goto-char (1+ pos)) ;; 1+ cause otherwise ?> is at point
  (goto-char (1+ (- pos (length marker))))
  (search-forward marker max t))

;; search end backward

(defun mumamo-chunk-end-bw-str (pos min marker)
  "General chunk function helper.
See `mumamo-chunk-start-fw-str' for more information and the
meaning of POS, MIN and MARKER."
  (assert (stringp marker))
  (goto-char (+ pos (length marker)))
  (search-backward marker min t))

(defun mumamo-chunk-end-bw-re (pos min marker)
  "General chunk function helper.
See `mumamo-chunk-start-fw-str' for more information and the
meaning of POS, MIN and MARKER."
  (assert (stringp marker))
  (goto-char (+ pos (length marker)))
  (re-search-backward marker min t))

(defun mumamo-chunk-end-bw-str-inc (pos min marker)
  "General chunk function helper.
See `mumamo-chunk-start-fw-str' for more information and the
meaning of POS, MIN and MARKER."
  (assert (stringp marker))
  (goto-char pos)
  (let ((end (search-backward marker min t)))
    (when end (setq end (+ end (length marker))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; General chunk routines

;; (defvar mumamo-known-chunk-start nil "Internal use only!.")

(defconst mumamo-string-syntax-table
  (let ((tbl (copy-syntax-table)))
    (modify-syntax-entry ?\" "\"" tbl)
    (modify-syntax-entry ?\' "\"" tbl)
    tbl)
    "Just for \"..\" and '...'.")

;; "..." '...' "..'.." '.."..'
(defun mumamo-guess-in-string (pos)
  (let ((here (point))
        (inhibit-field-text-motion t)
        line-beg
        parsed
        str-char
        str-pos)
    (goto-char pos)
    (setq line-beg (line-beginning-position))
    (setq parsed (with-syntax-table mumamo-string-syntax-table
                   (parse-partial-sexp line-beg pos)))
    (setq str-char (nth 3 parsed))
    (when str-char
      (skip-chars-backward (string ?^ str-char))
      (setq str-pos (point)))
    (goto-char here)
    str-pos))

;;; The main generic chunk routine

;; Fix-me: This routine has some difficulties. One of the more
;; problematic things is that chunk borders may depend on the
;; surrounding chunks syntax. Patterns that possibly could be chunk
;; borders might instead be parts of comments or strings in cases
;; where they should not be valid borders there.
(defun mumamo-find-possible-chunk (pos
                                   min max
                                   bw-exc-start-fun
                                   bw-exc-end-fun
                                   fw-exc-start-fun
                                   fw-exc-end-fun
                                   &optional find-borders-fun)
  "Return list describing a possible chunk that includes POS.
No notice is taken about existing chunks and no chunks are
created.  The description returned is for the smallest possible
chunk which is delimited by the function parameters.

POS must be between MIN and MAX.

The function BW-EXC-START-FUN takes two parameters, POS and
MIN.  It should search backward from POS, bound by MIN, for
exception start and return a cons or a list:

  \(FOUND-POS . EXCEPTION-MODE)
  \(FOUND-POS EXCEPTION-MODE PARSEABLE-BY)

Here FOUND-POS is the start of the chunk. EXCEPTION-MODE is the
major mode specifier for this chunk. \(Note that this specifier
is translated to a major mode through `mumamo-major-modes'.)

PARSEABLE-BY is a list of parsers that can handle the chunk
beside the one that may be used by the chunks major
mode. Currently only the XML parser in `nxml-mode' is
recognized. In this list it should be the symbol `nxml-mode'.

The functions BW-EXC-END-FUN, FW-EXC-START-FUN and FW-EXC-END-FUN
should search for exception start or end, forward resp backward.
Those three should return just the start respectively the end of
the chunk.

For all four functions the position returned should be nil if
search fails.


Return as a list with values

  \(START END EXCEPTION-MODE BORDERS PARSEABLE-BY)

The bounds START and END are where the exception starts or stop.
Either of them may be nil, in which case this is equivalent to
`point-min' respectively `point-max'.

If EXCEPTION-MODE is non-nil that is the submode for this
range.  Otherwise the main major mode should be used for this
chunk.

BORDERS is the return value of the optional FIND-BORDERS-FUN
which takes three parameters, START, END and EXCEPTION-MODE in
the return values above. BORDERS may be nil and otherwise has
this format:

  \(START-BORDER END-BORDER EXCEPTION-MODE)

START-BORDER and END-BORDER may be nil. Otherwise they should be
the position where the border ends respectively start at the
corresponding end of the chunk.

PARSEABLE-BY is a list of major modes with parsers that can parse
the chunk.

----
* Note: This routine is used by to create new members for chunk
families.  If you want to add a new chunk family you could most
often do that by writing functions for this routine.  Please see
the many examples in mumamo-fun.el for how this can be done. See
also `mumamo-quick-static-chunk'."
  ;;\(START END EXCEPTION-MODE END-OF-EXCEPTION POS)
  (mumamo-msgfntfy "\nmumamo-find-possible-chunk %s %s %s %s %s\n%s %s %s %s %s" pos min max (point-min) (point-max) bw-exc-start-fun bw-exc-end-fun fw-exc-start-fun fw-exc-end-fun find-borders-fun)
  ;;(message "\nmumamo-find-possible-chunk %s %s %s %s %s\n%s %s %s %s %s" pos min max (point-min) (point-max) bw-exc-start-fun bw-exc-end-fun fw-exc-start-fun fw-exc-end-fun find-borders-fun)
  ;;(message "\nmumamo-find-possible-chunk %s %s %s %s %s" pos min max (point-min) (point-max))
  ;;(message "\nmumamo-find-possible-chunk.debugger=%s" debugger)
  (setq err nil)
  (mumamo-condition-case err
      (progn
        (assert (and (<= min pos) (<= pos max))
                nil
                "mumamo-chunk: min=%s, pos=%s, max=%s, bt=%S"
                min pos max (with-output-to-string (backtrace)))
        ;; "in" refers to "in exception" and "out" is then in main
        ;; major mode.
        (let (start-in-cons
              exc-mode
              parseable-by
              start-in start-out
              end-in end-out
              start end
              ;;end-of-exception
              wants-end-type
              found-valid-end
              (main-major (mumamo-main-major-mode))
              borders
              border-beg
              border-end)
          ;;;; find start of range
          ;;
          ;; start normal
          ;;
          ;;(message "here a1, bw-exc-end-fun=(%s %s %s) debugger=%s" bw-exc-end-fun pos min debugger)
          (setq start-out (funcall bw-exc-end-fun pos min))
          ;;(message "here a1b, start-out=%s debugger=%s" start-out debugger)
          (when start-out
            (assert (<= start-out pos))
            (assert (<= min start-out)))
          (when start-out (setq min start-out)) ;; minimize next search bw
          ;; start exception
          ;;(message "start exception, bw-exc-start-fun=%s debugger=%s" bw-exc-start-fun debugger)
          (setq start-in-cons (funcall bw-exc-start-fun pos min))
          ;;(message "start-in-cons=%s" start-in-cons)
          ;;(message "after start exception, bw-exc-start-fun=%s" bw-exc-start-fun)
          (setq start-in (car start-in-cons))
          ;;(message "start-in=%s" start-in)
          (when start-in
            ;;(message "here in a2, start-in=%s, pos=%s" start-in pos)
            (assert (<= start-in pos) t)
            ;;(message "here in b")
            (assert (<= min start-in) t)
            ;;(message "here in c")
            )
          ;; compare
          ;;(message "compare")
          (cond
           ((and start-in start-out)
            (if (< start-in start-out)
                (setq start start-out)
              (setq exc-mode (cdr start-in-cons))
              (setq start start-in)))
           (start-in
            (setq exc-mode (cdr start-in-cons))
            (setq start start-in))
           (start-out
            (setq start start-out))
           ;; Fix-me: I am not sure about this, I do not remember why
           ;; I had this test. -1 just disables it.
           ;;((= min 1)
           ((= min -1)
            (setq start-in 1)
            (setq start 1)
            (setq exc-mode nil)))
          (when (and exc-mode
                     (listp exc-mode))
            (setq parseable-by (cadr exc-mode))
            (setq exc-mode (car exc-mode)))
          ;;;; find end of range
          ;;
          ;; what end type is acceptable?  three possible values: nil means
          ;; any end type, the other values are 'end-normal and
          ;; 'end-exception.
          ;;(message "find end of range")
          (while (not found-valid-end)
            (when start
              (if exc-mode
                  (setq wants-end-type 'end-exception)
                (setq wants-end-type 'end-normal)))
            ;; end exception
            (when (or (not wants-end-type)
                      (eq wants-end-type 'end-exception))
              (setq max end-in) ;; minimize next search fw
              (setq end-in (funcall fw-exc-end-fun pos max)))
            ;; end normal
            (when (or (not wants-end-type)
                      (eq wants-end-type 'end-normal))
              ;; 1+ is for zero length chunks (that will never be created)
              (setq end-out (funcall fw-exc-start-fun (1+ pos) max)))
            ;;(message "=========================== fw-exc-start-fun=%s end-out=%s end-in=%s" fw-exc-start-fun end-out end-in)
            ;; compare
            (cond
             ((and end-in end-out)
              (if (> end-in end-out)
                  (setq end end-out)
                ;;(setq end-of-exception t)
                (setq end end-in)))
             (end-in
              ;;(setq end-of-exception t)
              (setq end end-in))
             (end-out
              (setq end end-out)))
            ;; borders
            (when find-borders-fun
              (let ((start-border (when start (unless (and (= 1 start)
                                                           (not exc-mode))
                                                start)))
                    (end-border (when end (unless (and (= (point-max) end)
                                                       (not exc-mode))
                                            end))))
                (setq borders (funcall find-borders-fun start-border end-border exc-mode))))
            ;; check
            (setq border-beg (nth 0 borders))
            (setq border-end (nth 1 borders))
            (when start
              (assert (<= start pos))
              (when border-beg
                (assert (<= start border-beg))))
            (when end
              ;;(message "start=%s, wants-end-type =%s" start wants-end-type)
;;;               (message "pos=%s min=%s max=%s bw-exc-start-fun=%s bw-exc-end-fun=%s fw-exc-start-fun=%s fw-exc-end-fun=%s find-borders-fun=%s"
;;;                        pos min max
;;;                        bw-exc-start-fun
;;;                        bw-exc-end-fun
;;;                        fw-exc-start-fun
;;;                        fw-exc-end-fun
;;;                        find-borders-fun)
              (assert (<= pos end) t)
              (when border-end
                (assert (<= border-end end))))
            (if (not end)
                (setq found-valid-end t)
              (let ((syntax-start (if border-beg border-beg
                                    (if start start min)))
                    (syntax-end (if border-end border-end end))
                    (major (if exc-mode exc-mode main-major)))
                (mumamo-msgfntfy "point-min/max=%s/%s, border-beg=%s, border-end=%s, start/end/min=%s/%s/%s" (point-min) (point-max) border-beg border-end start end min)
                (setq found-valid-end
                      ;;(mumamo-end-chunk-is-valid syntax-start syntax-end major)
                      t)
                (mumamo-msgfntfy "after setq found-valid-end=%s" found-valid-end)
                (unless found-valid-end
                  (setq end nil)
                  (setq end-in (point-max))
                  (setq pos (1+ syntax-end)))
                )))
          ;;(list start end exc-mode end-of-exception pos)
;;;           (message " return (%s %s %s %s)\n   %s %s %s\n   %s %s %s\n   %s %s" start end exc-mode borders
;;;                    pos min max
;;;                    bw-exc-start-fun
;;;                    bw-exc-end-fun
;;;                    fw-exc-start-fun
;;;                    fw-exc-end-fun
;;;                    find-borders-fun)
          (mumamo-msgfntfy "start/end=%s/%s borders=%s, exc-mode=%s" start end borders exc-mode)
          ;;(message "start/end=%s/%s borders=%s, exc-mode=%s" start end borders exc-mode)
          ;; This is just totally wrong in some pieces and a desperate
          ;; try after seeing the problems with wp-app.php around line
          ;; 1120.  Maybe this can be used when cutting chunks from
          ;; top to bottom however.
          (when nil ;end
            (let ((here (point))
                  end-line-beg
                  end-in-string
                  start-in-string
                  (start-border (or (nth 0 borders) start))
                  (end-border   (or (nth 1 borders) end)))
              ;; Check if in string
              ;; Fix-me: add comments about why and examples + tests
              ;; Fix-me: must loop to find good borders ....
              (when end
                ;; Fix-me: more careful positions for guess
                (setq end-in-string
                      (mumamo-guess-in-string
                       ;;(+ end 2)
                       (1+ end-border)
                       ))
                (when end-in-string
                  (when start
                    (setq start-in-string
                          (mumamo-guess-in-string
                           ;;(- start 2)
                           (1- start-border)
                           )))
                  (if (not start-in-string)
                      (setq end nil)
                    (if exc-mode
                        (if (and start-in-string end-in-string)
                            ;; If both are in a string and on the same line then
                            ;; guess this is actually borders, otherwise not.
                            (unless (= start-in-string end-in-string)
                              (setq start nil)
                              (setq end nil))
                          (when start-in-string (setq start nil))
                          (when end-in-string (setq end nil)))
                      ;; Fix-me: ???
                      (when start-in-string (setq start nil))
                      ))
                  (unless (or start end)
                    (setq exc-mode nil)
                    (setq borders nil)
                    (setq parseable-by nil))))))
          
          (when (or start end exc-mode borders parseable-by)
            (mumamo-msgfntfy "--- mumamo-find-possible-chunk %s" (list start end exc-mode borders parseable-by))
            (list start end exc-mode borders parseable-by))))
    (error
     (mumamo-display-error 'mumamo-chunk "%s"
                           (error-message-string err)))))

;; Fix-me: This should check if the new chunk should be
;; parsed or not
;; (defsubst mumamo-chunk-nxml-parseable (chunk)
;;   (eq (mumamo-main-major-mode)
;;       (mumamo-chunk-major-mode xml-chunk)))

(defun mumamo-valid-nxml-point (pos)
  "Return non-nil if point should be in valid XML chunk."
  (memq 'nxml-mode (get-text-property pos 'mumamo-parseable-by)))

(defun mumamo-valid-nxml-chunk (chunk)
  "Return t if chunk CHUNK should be valid XML."
  (when chunk
    (let ((major-mode (mumamo-chunk-major-mode chunk))
          (parseable-by (overlay-get chunk 'mumamo-parseable-by)))
      (or (derived-mode-p 'nxml-mode)
          (memq 'nxml-mode parseable-by)))))

;; A good test case for the use of this is the troublesome code in the
;; first line of xml-as-string.php in nxml/nxhtml/bug-tests. Currently
;; this test code is however splitted and it looks like the code below
;; can't handle the line above if the line looks like below. The ?> is
;; still thought to be a border.  Does this mean that ' is not treated
;; as a string separator?
;;
;; <?php header("Content-type:application/xml; charset=utf-8"); echo '<?xml version="1.0" encoding="utf-8"?>'; ?>
(defun mumamo-end-chunk-is-valid (syntax-start syntax-end major)
  "Return t if SYNTAX-END of chunk is not in a string or comment.
Use MAJOR mode when testing this and assume that sexp syntax is
nil at SYNTAX-START."
  ;; Fix-me: This can't always detect html comments: <!--
  ;; ... -->. Could this be solved by RMS suggestion with a
  ;; function/defmacro that binds variables to their global values?
  (mumamo-msgfntfy "point-min,max=%s,%s syntax-start,end=%s,%s, major=%s" (point-min) (point-max) syntax-start syntax-end major)
  (assert (and syntax-start syntax-end) t)
  (save-restriction
    (widen)
    (mumamo-with-major-mode-fontification major
      `(progn
         (let (ppss ret)
           ;; fix-me: Use main major mode, and `syntax-ppss'. Change the
           ;; defadvice of this to make that possible.
           (setq ppss (parse-partial-sexp ,syntax-start (+ ,syntax-end 0)))
           ;; If inside a string or comment then the end marker is
           ;; invalid:
           (if (or (nth 3 ppss)
                   (nth 4 ppss))
               (progn
                 ;;(message "invalid end, syntax-end =%s" syntax-end)
                 nil
                 t)
             ;;(message "valid end, syntax-end =%s" syntax-end)
             t))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Easy chunk defining

(defun mumamo-quick-static-chunk (pos min max
                                  begin-mark end-mark inc mode
                                  mark-is-border)
  "Quick way to make a chunk function with static dividers.
Here is an example of how to use it:

  (defun mumamo-chunk-embperl-<- (pos min max)
    \"Find [- ... -], return range and perl-mode.\"
    (mumamo-quick-static-chunk pos min max \"[-\" \"-]\" nil 'perl-mode))

As you can see POS, MIN and MAX comes from argument of the
function you define.

BEGIN-MARK should be a string that begins the chunk.
END-MARK should be a string that ends the chunk.
If INC is non-nil then the dividers are included in the chunks.
MODE should be the major mode for the chunk.

If MARK-IS-BORDER is non-nil then the marks are made borders."

;;See also `mumamo-easy-make-chunk-fun'."
  (mumamo-msgfntfy "quick.pos=%s min,max=%s,%s begin-mark/end=%s/%s mark-is-border=%s" pos min max begin-mark end-mark mark-is-border)
  (let ((search-bw-exc-start
         (lambda (pos min)
           (let ((exc-start
                  (if inc
                      (mumamo-chunk-start-bw-str-inc pos min begin-mark)
                    (mumamo-chunk-start-bw-str pos min begin-mark))))
             (when (and exc-start
                        (<= exc-start pos))
               (cons exc-start mode)))))
        (search-bw-exc-end
         (lambda (pos min)
           (if inc
               (mumamo-chunk-end-bw-str-inc pos min end-mark)
             (mumamo-chunk-end-bw-str pos min end-mark))))
        (search-fw-exc-start
         (lambda (pos max)
           (if inc
               (mumamo-chunk-start-fw-str-inc pos max begin-mark)
             (mumamo-chunk-start-fw-str pos max begin-mark))))
        (search-fw-exc-end
         (lambda (pos max)
           (if inc
               (mumamo-chunk-end-fw-str-inc pos max end-mark)
             (mumamo-chunk-end-fw-str pos max end-mark))))
        (find-borders
         (when mark-is-border
           (lambda (start end exc-mode)
             (let ((start-border)
                   (end-border))
               (if (and inc exc-mode)
                   (progn
                     (when start
                       (setq start-border
                             (+ start (length begin-mark))))
                     (when end
                       (setq end-border
                             (- end (length end-mark)))))
                 (if (and (not inc) (not exc-mode))
                     (progn
                       (when start
                         (setq start-border
                               (+ start (length end-mark))))
                       (when end
                         (setq end-border
                               (- end (length begin-mark)))))))
               (when (or start-border end-border)
                 (mumamo-msgfntfy "quick.start-border/end=%s/%s, start/end=%s/%s exc-mode=%s" start-border end-border start end exc-mode)
                 (list start-border end-border)))))))
    (mumamo-find-possible-chunk pos min max
                                search-bw-exc-start
                                search-bw-exc-end
                                search-fw-exc-start
                                search-fw-exc-end
                                find-borders)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Changing the major mode that the user sees

(defvar mumamo-unread-command-events-timer nil)
(make-variable-buffer-local 'mumamo-unread-command-events-timer)

(defun mumamo-unread-command-events (command-keys new-major old-last-command)
  "Sync new keymaps after changing major mode in a timer.
Also tell new major mode.

COMMAND-KEYS is the keys entered after last command and the call
to `mumamo-idle-set-major-mode' \(which is done in an idle
timer).  Those keys are added to `unread-command-events' so they
can be used in the new keymaps.  They should be in the format
returned by

  \(listify-key-sequence (this-command-keys-vector))

NEW-MAJOR mode is the new major mode.

OLD-LAST-COMMAND is the value of `last-command' after switching
major mode.  \(This is cleared by the function `top-level' so
this function will not see it since it is run in a timer.)"
  (mumamo-condition-case err
      (progn
        ;; last-command seems to be cleared by top-level so set it
        ;; back here.
        (unless last-command
          (setq last-command old-last-command))
        (when (< 0 (length command-keys))
          ;;(setq last-command-char nil) ;; For `viper-command-argument'
          (setq unread-command-events (append command-keys nil)))
        (message "Switched to %s" new-major))
    (error
     (let ((mumamo-display-error-lwarn t))
       (mumamo-display-error 'mumamo-unread-command-events "err=%s" err)))))

(defvar mumamo-idle-set-major-mode-timer nil)
(make-variable-buffer-local 'mumamo-idle-set-major-mode-timer)
(put 'mumamo-idle-set-major-mode-timer 'permanent-local t)

(defun mumamotemp-pre-command ()
  (message "mumamotemp-pre 1: modified=%s %s" (buffer-modified-p) (current-buffer)))
(defun mumamotemp-post-command ()
  (message "mumamotemp-post 1: modified=%s %s" (buffer-modified-p) (current-buffer)))
(put 'mumamotemp-pre-command 'permanent-local-hook t)
(put 'mumamotemp-post-command 'permanent-local-hook t)
(defun mumamotemp-start ()
  (add-hook 'post-command-hook 'mumamotemp-post-command nil t)
  (add-hook 'pre-command-hook 'mumamotemp-pre-command nil t))

(defun mumamo-idle-set-major-mode (buffer window)
  "Set major mode from mumamo chunk when Emacs is idle.
Do this in window WINDOW if and only if current buffer is BUFFER.

See the variable `mumamo-set-major-mode-delay' for an
explanation."
  (mumamo-msgfntfy "mumamo-idle-set-major-mode b=%s, window=%s" buffer window)
  (with-selected-window window
    ;; According to Stefan Monnier we need to set the buffer too.
    (with-current-buffer (window-buffer)
      (when (eq buffer (current-buffer))
        (mumamo-condition-case err
            (let* ((ovl (mumamo-get-chunk-at (point)))
                   (major (mumamo-chunk-major-mode ovl))
                   (modified (buffer-modified-p)))
              (unless (eq major major-mode)
                (mumamo-set-major major)
                ;; Fix-me: This is a bug workaround. Possibly in Emacs.
                (when (and (buffer-modified-p)
                           (not modified))
                  (set-buffer-modified-p nil))
                ;; sync keymap
                (when (timerp mumamo-unread-command-events-timer)
                  (cancel-timer mumamo-unread-command-events-timer))
                (when unread-command-events
                  ;; Save unread keys before calling `top-level' which
                  ;; will clear them.
                  (setq mumamo-unread-command-events-timer
                        (run-with-idle-timer
                         0 nil
                         'mumamo-unread-command-events
                         unread-command-events
                         major last-command))
                  (top-level)
                  )))
          (error
           (mumamo-display-error 'mumamo-idle-set-major-mode
                                 "cb=%s, err=%s" (current-buffer) err)))))))

(defun mumamo-request-idle-set-major-mode ()
  "Setup to change major mode from chunk when Emacs is idle."
  (when (timerp mumamo-idle-set-major-mode-timer)
    (cancel-timer mumamo-idle-set-major-mode-timer))
  (setq mumamo-idle-set-major-mode-timer
        (run-with-idle-timer
         mumamo-set-major-mode-delay
         nil
         ;;'mumamo-idle-set-major-mode (mumamo-get-chunk-at (point)))))
         'mumamo-idle-set-major-mode (current-buffer) (selected-window))))

(defvar mumamo-done-first-set-major nil)
(make-variable-buffer-local 'mumamo-done-first-set-major)
(put 'mumamo-done-first-set-major 'permanent-local t)

(defvar mumamo-multi-major-mode nil
  "The function that handles multiple major modes.
If this is nil then multiple major modes in the buffer is not
handled by mumamo.

Set by functions defined by `define-mumamo-multi-major-mode'.")
(make-variable-buffer-local 'mumamo-multi-major-mode)
(put 'mumamo-multi-major-mode 'permanent-local t)

;; Fix-me: Add a property to the symbol instead (like in CUA).
(defvar mumamo-safe-commands-in-wrong-major
  '(forward-char
    viper-forward-char
    backward-char
    viper-backward-char
    next-line
    viper-next-line
    previous-line
    viper-previous-line
    scroll-down
    cua-scroll-down
    scroll-up
    cua-scroll-up
    move-beginning-of-line
    move-end-of-line
    nonincremental-search-forward
    nonincremental-search-backward
    mumamo-backward-chunk
    mumamo-forward-chunk
    ;; Fix-me: add more
    )
  )

(defun mumamo-set-major-pre-command ()
  "Change major mode if necessary before a command.
When the key sequence that invoked the command is in current
local map and major mode is not the major mode for the current
mumamo chunk then set major mode to that for the chunk."
  ;;(message "enter mumamo-set-major-pre-command")
  (mumamo-condition-case err
      (let* ((ovl (mumamo-get-chunk-at (point)))
             (major (mumamo-chunk-major-mode ovl))
             (found-this (lookup-key (current-local-map) (this-command-keys-vector)))
             )
;;;         (message "pre-commend: major=%s, major-mode=%s, lookup M-TAB=%s, binding=%s, found-this=%s"
;;;                  major major-mode (lookup-key (current-local-map) [(meta tab)])
;;;                  (key-binding [(meta tab)] t)
;;;                  found-this)
        (if (not major)
            (lwarn '(mumamo-set-major-pre-command)
                   :error "major=%s" major)
          (when (and (not (eq major-mode major))
                     ;;(lookup-key (current-local-map) (this-command-keys-vector))
                     (not (memq this-command mumamo-safe-commands-in-wrong-major))
                     )
            (mumamo-set-major major)
            ;; Unread the last command key sequence
            (setq unread-command-events
                  (append (listify-key-sequence (this-command-keys-vector))
                          unread-command-events))
            ;; Some commands, like `viper-command-argument' need to
            ;; know the last command, so tell them.
            (setq this-command (lambda ()
                                 (interactive)
                                 (setq this-command last-command)))
            )))
    (error
     (mumamo-display-error 'mumamo-set-major-pre-command
                           "%s" (error-message-string err)))))

(defun mumamo-fetch-local-map (major)
  "Fetch local keymap for major mode MAJOR.
Do that by turning on the major mode in a new buffer.  Add the
keymap to `mumamo-major-modes-local-maps'.

Return the fetched local map."
  (let (temp-buf-name
        temp-buf
        local-map)
    (setq temp-buf-name (concat "mumamo-fetch-major-mode-local-"
                                (symbol-name major)))
    (setq temp-buf (get-buffer temp-buf-name))
    (when temp-buf (kill-buffer temp-buf))
    (setq temp-buf (get-buffer-create temp-buf-name))
    (with-current-buffer temp-buf
      (let ((mumamo-fetching-major t))
        (funcall major))
      (setq local-map (current-local-map))
      (when local-map (setq local-map (copy-keymap (current-local-map))))
      (add-to-list 'mumamo-major-modes-local-maps
                   (cons major-mode local-map)))
    (kill-buffer temp-buf)
    local-map))

;; (setq mumamo-set-major-mode-delay 10)
(defun mumamo-set-major-post-command ()
  "Change major mode if necessary after a command.
If the major mode for chunk at `window-point' differ from current
major mode then change major mode to that for the chunk.  If
however `mumamo-set-major-mode-delay' is greater than 0 just
request a change of major mode when Emacs is idle that long.

See the variable above for an explanation why a delay might be
needed \(and is the default).
"
  ;;(message "mumamo-set-major-post-command here")
  (let* ((ovl (mumamo-get-chunk-at (point)))
         (major (mumamo-chunk-major-mode ovl))
         (in-pre-hook (memq 'mumamo-set-major-pre-command pre-command-hook)))
    (if (not major)
        (lwarn '(mumamo-set-major-post-command)
               :error "major=%s" major)
      (unless (and mumamo-done-first-set-major
                   (eq major-mode major)
                   (not in-pre-hook))
        ;;(message "mumamo-set-major-post-command here done=%s\nsurvive=%s" mumamo-done-first-set-major mumamo-survive)
        (if mumamo-done-first-set-major
            (if (<= 0 mumamo-set-major-mode-delay)
                ;; Window point has been moved to a new chunk with a
                ;; new major mode.  Major mode will not be changed
                ;; directly, but in an idle timer.  To avoid that the
                ;; user get the wrong key bindings for the new chunk
                ;; fetch the local map directly and apply that.
                (let* ((map-rec (assoc major mumamo-major-modes-local-maps))
                       (map (cdr map-rec)))
                  (unless map
                    (setq map (mumamo-fetch-local-map major)))
                  (unless (eq map 'no-local-map)
                    (use-local-map map))
                  (add-hook 'pre-command-hook
                            'mumamo-set-major-pre-command nil t)
                  (mumamo-request-idle-set-major-mode))
              (mumamo-set-major major)
              (message "Switched to %s" major-mode))
          (mumamo-set-major major))))))

(defun mumamo-post-command-1 (&optional no-debug)
  (unless no-debug (setq debug-on-error t))
  (mumamo-msgfntfy "mumamo-post-command-1: font-lock-mode=%s" font-lock-mode)
  (if font-lock-mode
      (mumamo-set-major-post-command)
    ;;(mumamo-on-font-lock-off)
    ))

(defun mumamo-post-command ()
  "Run this in `post-command-hook'.
Change major mode if necessary."
  (mumamo-msgfntfy "mumamo-post-command")
  (when mumamo-multi-major-mode
    (mumamo-condition-case err
        (mumamo-post-command-1 t)
      (error
       (mumamo-msgfntfy "mumamo-post-command %s" (error-message-string err))
       ;; Warnings are to disturbing when run in post-command-hook,
       ;; but this message is important so show it with an highlight.
       (message
        (propertize
         "%s\n- Please try M-: (mumamo-post-command-1) to see what happened."
         'face 'highlight)
        (error-message-string err))))))

(defvar mumamo-set-major-running nil
  "Internal use.  Handling of mumamo turn off.")

(defun mumamo-change-major-function ()
  "Function added to `change-major-mode-hook'.
Remove mumamo when changing to a new major mode if the change is
not done because point was to a new chunk."
  (unless mumamo-set-major-running
    (mumamo-turn-off-actions)))

(defun mumamo-derived-from-mode (major from-mode)
  "Return t if major mode MAJOR is derived from FROM-MODE."
  (let ((major-mode major))
    (derived-mode-p from-mode)))

;; This is the new version of add-hook. For its origin see
;; http://lists.gnu.org/archive/html/emacs-devel/2007-12/msg00169.html
;;
;;(unless (> emacs-major-version 22)
(defvar mumamo-test-add-hook nil
  "Internal use.")
(unless (and t
             (let ((has-it nil))
               ;;(add-hook 'mumamo-test-add-hook 'mumamo-jit-lock-after-change nil t)
               (add-hook 'mumamo-test-add-hook 'mumamo-after-change nil t)
               (setq has-it (eq 'permanent-local-hook
                                (get 'mumamo-test-add-hook 'permanent-local)))
               has-it))
  (defun add-hook (hook function &optional append local)
    "Add to the value of HOOK the function FUNCTION.
FUNCTION is not added if already present.
FUNCTION is added (if necessary) at the beginning of the hook list
unless the optional argument APPEND is non-nil, in which case
FUNCTION is added at the end.

The optional fourth argument, LOCAL, if non-nil, says to modify
the hook's buffer-local value rather than its default value.
This makes the hook buffer-local if needed, and it makes t a member
of the buffer-local value.  That acts as a flag to run the hook
functions in the default value as well as in the local value.

HOOK should be a symbol, and FUNCTION may be any valid function.  If
HOOK is void, it is first set to nil.  If HOOK's value is a single
function, it is changed to a list of functions."
    (or (boundp hook) (set hook nil))
    (or (default-boundp hook) (set-default hook nil))
    (if local (unless (local-variable-if-set-p hook)
                (set (make-local-variable hook) (list t)))
      ;; Detect the case where make-local-variable was used on a hook
      ;; and do what we used to do.
      (unless (and (consp (symbol-value hook)) (memq t (symbol-value hook)))
        (setq local t)))
    (let ((hook-value (if local (symbol-value hook) (default-value hook))))
      ;; If the hook value is a single function, turn it into a list.
      (when (or (not (listp hook-value)) (eq (car hook-value) 'lambda))
        (setq hook-value (list hook-value)))
      ;; Do the actual addition if necessary
      (unless (member function hook-value)
        (setq hook-value
              (if append
                  (append hook-value (list function))
                (cons function hook-value))))
      ;; Set the actual variable
      (if local
          (progn
            ;; If HOOK isn't a permanent local,
            ;; but FUNCTION wants to survive a change of modes,
            ;; mark HOOK as partially permanent.
            (and (symbolp function)
                 (get function 'permanent-local-hook)
                 (not (get hook 'permanent-local))
                 (put hook 'permanent-local 'permanent-local-hook))
            (set hook hook-value))
        (set-default hook hook-value))))
  )


(defvar mumamo-survive-hooks
  '(
;;     activate-mark-hook after-change-functions after-save-hook
;;     before-save-functions auto-save-hook before-revert-hook
;;     buffer-access-fontify-functions calendar-load-hook
;;     command-line-functions compilation-finish-function
;;     deactivate-mark-hook find-file-hook
;;     find-file-not-found-functions first-change-hook
;;     kbd-macro-termination-hook kill-buffer-hook
;;     kill-buffer-query-functions menu-bar-update-hook
;;     post-command-hook pre-abbrev-expand-hook pre-command-hook
;;     write-contents-functions write-file-functions
;;     write-region-annotate-functions
;;     c-special-indent-hook
    ))

;;
;; Emulation modes
;;
;; These variables should have 'permanant-local t set in their
;; packages IMO, but now they do not have that.
(eval-after-load 'viper-cmd
  (progn
    (put 'viper-after-change-functions 'permanent-local t)
    (put 'viper-before-change-functions 'permanent-local t)
    ))
(eval-after-load 'viper
  (progn
    (put 'viper-post-command-hooks 'permanent-local t)
    (put 'viper-pre-command-hooks 'permanent-local t)
    ;;minor-mode-map-alist
    ;; viper-mode-string -- is already buffer local, globally void
    (put 'viper-mode-string 'permanent-local t)
    ))
    ;;viper-tut--part
(eval-after-load 'viper-init
  (progn
    (put 'viper-d-com 'permanent-local t)
    (put 'viper-last-insertion 'permanent-local t)
    (put 'viper-command-ring 'permanent-local t)
    (put 'viper-vi-intercept-minor-mode 'permanent-local t)
    (put 'viper-vi-basic-minor-mode 'permanent-local t)
    (put 'viper-vi-local-user-minor-mode 'permanent-local t)
    (put 'viper-vi-global-user-minor-mode 'permanent-local t)
    (put 'viper-vi-state-modifier-minor-mode 'permanent-local t)
    (put 'viper-vi-diehard-minor-mode 'permanent-local t)
    (put 'viper-vi-kbd-minor-mode 'permanent-local t)
    (put 'viper-insert-intercept-minor-mode 'permanent-local t)
    (put 'viper-insert-basic-minor-mode 'permanent-local t)
    (put 'viper-insert-local-user-minor-mode 'permanent-local t)
    (put 'viper-insert-global-user-minor-mode 'permanent-local t)
    (put 'viper-insert-state-modifier-minor-mode 'permanent-local t)
    (put 'viper-insert-diehard-minor-mode 'permanent-local t)
    (put 'viper-insert-kbd-minor-mode 'permanent-local t)
    (put 'viper-replace-minor-mode 'permanent-local t)
    (put 'viper-emacs-intercept-minor-mode 'permanent-local t)
    (put 'viper-emacs-local-user-minor-mode 'permanent-local t)
    (put 'viper-emacs-global-user-minor-mode 'permanent-local t)
    (put 'viper-emacs-kbd-minor-mode 'permanent-local t)
    (put 'viper-emacs-state-modifier-minor-mode 'permanent-local t)
    (put 'viper-vi-minibuffer-minor-mode 'permanent-local t)
    (put 'viper-insert-minibuffer-minor-mode 'permanent-local t)
    (put 'viper-automatic-iso-accents 'permanent-local t)
    (put 'viper-special-input-method 'permanent-local t)
    (put 'viper-intermediate-command 'permanent-local t)
    ;; already local: viper-undo-needs-adjustment
    (put 'viper-began-as-replace 'permanent-local t)
    ;; already local: viper-replace-overlay
    ;; already local: viper-last-posn-in-replace-region
    ;; already local: viper-last-posn-while-in-insert-state
    ;; already local: viper-sitting-in-replace
    (put 'viper-replace-chars-to-delete 'permanent-local t)
    (put 'viper-replace-region-chars-deleted 'permanent-local t)
    (put 'viper-current-state 'permanent-local t)
    (put 'viper-cted 'permanent-local t)
    (put 'viper-current-indent 'permanent-local t)
    (put 'viper-preserve-indent 'permanent-local t)
    (put 'viper-auto-indent 'permanent-local t)
    (put 'viper-electric-mode 'permanent-local t)
    ;; already local: viper-insert-point
    ;; already local: viper-pre-command-point
    (put 'viper-com-point 'permanent-local t)
    (put 'viper-ex-style-motion 'permanent-local t)
    (put 'viper-ex-style-editing 'permanent-local t)
    (put 'viper-ESC-moves-cursor-back 'permanent-local t)
    (put 'viper-delete-backwards-in-replace 'permanent-local t)
    ;; already local: viper-related-files-and-buffers-ring
    (put 'viper-local-search-start-marker 'permanent-local t)
    (put 'viper-search-overlay 'permanent-local t)
    (put 'viper-last-jump 'permanent-local t)
    (put 'viper-last-jump-ignore 'permanent-local t)
    (put 'viper-minibuffer-current-face 'permanent-local t)
    ;; already local: viper-minibuffer-overlay
    (put 'viper-command-ring 'permanent-local t)
    (put 'viper-last-insertion 'permanent-local t)
    ))
(eval-after-load 'viper-keym
  (progn
    ;; already local: viper-vi-local-user-map
    ;; already local: viper-insert-local-user-map
    ;; already local: viper-emacs-local-user-map
    (put 'viper--key-maps 'permanent-local t)
    (put 'viper--intercept-key-maps 'permanent-local t)
    ;; already local: viper-need-new-vi-local-map
    ;; already local: viper-need-new-insert-local-map
    ;; already local: viper-need-new-emacs-local-map
    ))
(eval-after-load 'viper-mous
  (progn
    (put 'viper-mouse-click-search-noerror 'permanent-local t)
    (put 'viper-mouse-click-search-limit 'permanent-local t)
    ))
(eval-after-load 'viper-util
  (progn
    (put 'viper-syntax-preference 'permanent-local t)
    (put 'viper-non-word-characters 'permanent-local t)
    (put 'viper-ALPHA-char-class 'permanent-local t)
    ))

(eval-after-load 'cua-base
  (progn
    (put 'cua-inhibit-cua-keys 'permanent-local t)
    (put 'cua--explicit-region-start 'permanent-local t)
    (put 'cua--status-string 'permanent-local t)
    ))
;; This is for the defvar in ido.el:
(eval-after-load 'ido
  (progn
    (put 'cua-inhibit-cua-keys 'permanent-local t)
    ))
(eval-after-load 'cua-rect
  (progn
    (put 'cua--rectangle 'permanent-local t)
    (put 'cua--rectangle-overlays 'permanent-local t)
    ))
(eval-after-load 'edt
  (progn
    (put 'edt-select-mode 'permanent-local t)
    ))
(eval-after-load 'tpu-edt
  (progn
    (put 'tpu-newline-and-indent-p 'permanent-local t)
    (put 'tpu-newline-and-indent-string 'permanent-local t)
    (put 'tpu-saved-delete-func 'permanent-local t)
    (put 'tpu-buffer-local-map 'permanent-local t)
    (put 'tpu-mark-flag 'permanent-local t)
    ))
(eval-after-load 'vi
  (progn
    (put 'vi-add-to-mode-line 'permanent-local t)
    ;;Warning (mumamo-survive): Not a local variable: vi-scroll-amount
    ;;Warning (mumamo-survive): Not a local variable: vi-shift-width
    ;;Warning (mumamo-survive): Not a local variable: vi-ins-point
    ;;Warning (mumamo-survive): Not a local variable: vi-ins-length
    ;;Warning (mumamo-survive): Not a local variable: vi-ins-repetition
    ;;Warning (mumamo-survive): Not a local variable: vi-ins-overwrt-p
    ;;Warning (mumamo-survive): Not a local variable: vi-ins-prefix-code
    ;;Warning (mumamo-survive): Not a local variable: vi-last-change-command
    ;;Warning (mumamo-survive): Not a local variable: vi-last-shell-command
    ;;Warning (mumamo-survive): Not a local variable: vi-last-find-char
    ;;Warning (mumamo-survive): Not a local variable: vi-mark-alist
    ;;Warning (mumamo-survive): Not a local variable: vi-insert-state
    ;;Warning (mumamo-survive): Not a local variable: vi-mode-old-local-map
    ;;Warning (mumamo-survive): Not a local variable: vi-mode-old-mode-name
    ;;Warning (mumamo-survive): Not a local variable: vi-mode-old-major-mode
    ;;Warning (mumamo-survive): Not a local variable: vi-mode-old-case-fold
    ;;
    ))
(eval-after-load 'vi
  (progn
    (put 'vip-emacs-local-map 'permanent-local t)
    (put 'vip-insert-local-map 'permanent-local t)
    (put 'vip-insert-point 'permanent-local t)
    (put 'vip-com-point 'permanent-local t)
    (put 'vip-current-mode 'permanent-local t)
    (put 'vip-emacs-mode-line-buffer-identification 'permanent-local t)
    (put 'vip-current-major-mode 'permanent-local t)
    ))

(eval-after-load 'hi-lock
  (progn
    (put 'hi-lock-mode 'permanent-local t)
    ))

;;
;; Minor modes that are not major mode specific
;;

(put 'visual-line-mode 'permanent-local t)

(eval-after-load 'flymake
  (progn
    ;; hook functions:
    (put 'flymake-after-change-function 'permanent-local-hook t)
    (put 'flymake-after-save-hook 'permanent-local-hook t)
    (put 'flymake-kill-buffer-hook 'permanent-local-hook t)
    ;; hooks:
;;;     (put 'after-change-functions 'permanent-local 'permanent-local-hook)
;;;     (put 'after-save-hook 'permanent-local 'permanent-local-hook)
;;;     (put 'kill-buffer-hook 'permanent-local 'permanent-local-hook)
    ;; vars:
    (put 'flymake-mode 'permanent-local t)
    (put 'flymake-is-running 'permanent-local t)
    (put 'flymake-timer 'permanent-local t)
    (put 'flymake-last-change-time 'permanent-local t)
    (put 'flymake-check-start-time 'permanent-local t)
    (put 'flymake-check-was-interrupted 'permanent-local t)
    (put 'flymake-err-info 'permanent-local t)
    (put 'flymake-new-err-info 'permanent-local t)
    (put 'flymake-output-residual 'permanent-local t)
    (put 'flymake-mode-line 'permanent-local t)
    (put 'flymake-mode-line-e-w 'permanent-local t)
    (put 'flymake-mode-line-status 'permanent-local t)
    (put 'flymake-temp-source-file-name 'permanent-local t)
    (put 'flymake-master-file-name 'permanent-local t)
    (put 'flymake-temp-master-file-name 'permanent-local t)
    (put 'flymake-base-dir 'permanent-local t)))

;; (eval-after-load 'imenu
;;   (progn
;;     ;; Fix-me: imenu is only useful for main major mode.  The menu
;;     ;; disappears in sub chunks because it is tighed to
;;     ;; local-map.  Don't know what to do about that.  I do not
;;     ;; understand the reason for binding it to local-map, but I
;;     ;; suspect the intent is to have different menu items for
;;     ;; different modes.  Could not that be achieved by deleting the
;;     ;; menu and creating it again when changing major mode? (That must
;;     ;; be implemented in imenu.el of course.)
;;     ;;
;;     ;; hook functions:
;; ;;;     (put 'imenu-update-menubar 'permanent-local-hook t)
;;     ;; hooks:
;;     (put 'menu-bar-update-hook 'permanent-local 'permanent-local-hook)
;;     ;; vars:
;;     (put 'imenu-generic-expression 'permanent-local t)
;;     (put 'imenu-create-index-function 'permanent-local t)
;;     (put 'imenu-prev-index-position-function 'permanent-local t)
;;     (put 'imenu-extract-index-name-function 'permanent-local t)
;;     (put 'imenu-name-lookup-function 'permanent-local t)
;;     (put 'imenu-default-goto-function 'permanent-local t)
;;     (put 'imenu--index-alist 'permanent-local t)
;;     (put 'imenu--last-menubar-index-alist 'permanent-local t)
;;     (put 'imenu-syntax-alist 'permanent-local t)
;;     (put 'imenu-case-fold-search 'permanent-local t)
;;     (put 'imenu-menubar-modified-tick 'permanent-local t)
;;     ))

(eval-after-load 'longlines
  (progn
    ;; Fix-me: take care of longlines-mode-off
    (put 'longlines-mode 'permanent-local t)
    (put 'longlines-wrap-beg 'permanent-local t)
    (put 'longlines-wrap-end 'permanent-local t)
    (put 'longlines-wrap-point 'permanent-local t)
    (put 'longlines-showing 'permanent-local t)
    (put 'longlines-decoded 'permanent-local t)
    ;;
    (put 'longlines-after-change-function 'permanent-local-hook t)
    (put 'longlines-after-revert-hook 'permanent-local-hook t)
    (put 'longlines-before-revert-hook 'permanent-local-hook t)
    (put 'longlines-decode-buffer 'permanent-local-hook t)
    (put 'longlines-decode-region 'permanent-local-hook t)
    (put 'longlines-mode-off 'permanent-local-hook t)
    (put 'longlines-post-command-function 'permanent-local-hook t)
    (put 'longlines-window-change-function 'permanent-local-hook t)
    ;;(put 'mail-indent-citation 'permanent-local-hook t)
    ))


;; Fix-me: Rails, many problematic things:

;;; Fix-me: No idea about these, where are they used?? Add them to
;;; mumamo-survive?:
;; predictive-main-dict
;; predictive-prog-mode-main-dict
;; predictive-use-auto-learn-cache
;; predictive-dict-autosave-on-kill-buffer
(eval-after-load 'inf-ruby
  (progn
    (put 'inferior-ruby-first-prompt-pattern 'permanent-local t)
    (put 'inferior-ruby-prompt-pattern 'permanent-local t)
    ))

;;; These are for the output buffer (no problems):
;; font-lock-keywords-only
;; font-lock-defaults -- always buffer local
;; scroll-margin
;; scroll-preserve-screen-position

(eval-after-load 'rails-script
  (progn
    (put 'rails-script:run-after-stop-hook 'permanent-local t)
    (put 'rails-script:show-buffer-hook 'permanent-local t)
    (put 'rails-script:output-mode-ret-value 'permanent-local t)
    ))

;;; No problems I believe (it is in output buffer):
;; compilation-error-regexp-alist-alist
;; compilation-error-regexp-alist

;;; Fix-me: This is in the minor mode, what to do? Looks like it
;;; should have 'permanent-local t - in this case.  I have added it to
;;; mumamo-survive for now.
;; tags-file-name

(eval-after-load 'rails
  (progn
    (put 'rails-primary-switch-func 'permanent-local t)
    (put 'rails-secondary-switch-func 'permanent-local t)
    ))



(defvar mumamo-survive
  '(
    buffer-file-name
    tags-file-name
    nxhtml-minor-mode
    ;; Fix-me: adding rng timers here stops Emacs from looping after
    ;; indenting in ind-0-error.php, but I have no clue why. Hm. This
    ;; problem is gone, but I forgot why.
    rng-c-current-token ;;rng-cmpct.el:132:(make-variable-buffer-local 'rng-c-current-token)
    rng-c-escape-positions ;;rng-cmpct.el:341:(make-variable-buffer-local 'rng-c-escape-positions)
    rng-c-file-name ;;rng-cmpct.el:344:(make-variable-buffer-local 'rng-c-file-name)
    rng-current-schema-file-name ;;rng-loc.el:37:(make-variable-buffer-local 'rng-current-schema-file-name)
    rng-current-schema ;;rng-pttrn.el:71:(make-variable-buffer-local 'rng-current-schema)
    ;;rng-validate-timer is permanent-local t
    ;;rng-validate-timer ;;rng-valid.el:141:(make-variable-buffer-local 'rng-validate-timer)
    ;;rng-validate-quick-timer is permanent-local t
    ;;rng-validate-quick-timer ;;rng-valid.el:146:(make-variable-buffer-local 'rng-validate-quick-timer)
    rng-error-count ;;rng-valid.el:153:(make-variable-buffer-local 'rng-error-count)
    rng-message-overlay ;;rng-valid.el:158:(make-variable-buffer-local 'rng-message-overlay)
    rng-message-overlay-inhibit-point ;;rng-valid.el:165:(make-variable-buffer-local 'rng-message-overlay-inhibit-point)
    rng-message-overlay-current ;;rng-valid.el:169:(make-variable-buffer-local 'rng-message-overlay-current)
    rng-validate-up-to-date-end ;;rng-valid.el:188:(make-variable-buffer-local 'rng-validate-up-to-date-end)
    rng-conditional-up-to-date-start ;;rng-valid.el:199:(make-variable-buffer-local 'rng-conditional-up-to-date-start)
    rng-conditional-up-to-date-end ;;rng-valid.el:205:(make-variable-buffer-local 'rng-conditional-up-to-date-end)
    rng-validate-mode ;;rng-valid.el:212:(make-variable-buffer-local 'rng-validate-mode)
    rng-dtd ;;rng-valid.el:215:(make-variable-buffer-local 'rng-dtd)

    nxml-syntax-highlight-flag ;; For pre-Emacs nxml
    ;;nxml-ns-state - not buffer local currently
    nxml-prolog-regions ;;snxml-mode.el:362:(make-variable-buffer-local 'nxml-prolog-regions)
    nxml-last-fontify-end ;;dnxml-mode.el:367:(make-variable-buffer-local 'nxml-last-fontify-end)
    nxml-degraded ;;dnxml-mode.el:373:(make-variable-buffer-local 'nxml-degraded)
    nxml-char-ref-extra-display ;;ynxml-mode.el:397:(make-variable-buffer-local 'nxml-char-ref-extra-display)
    nxml-prolog-end ;;dnxml-rap.el:92:(make-variable-buffer-local 'nxml-prolog-end)
    nxml-scan-end ;;dnxml-rap.el:107:(make-variable-buffer-local 'nxml-scan-end)

;;;     longlines-mode
;;;     longlines-wrap-beg
;;;     longlines-wrap-end
;;;     longlines-wrap-point
;;;     longlines-showing
;;;     longlines-decoded
    buffer-invisibility-spec
    header-line-format

    line-move-visual ;;simple.el:4537:    (kill-local-variable 'line-move-visual)
    word-wrap ;;simple.el:4538:    (kill-local-variable 'word-wrap)
    truncate-lines ;;simple.el:4539:    (kill-local-variable 'truncate-lines)
    truncate-partial-width-windows ;;simple.el:4540:    (kill-local-variable 'truncate-partial-width-windows)
    fringe-indicator-alist ;;simple.el:4541:    (kill-local-variable 'fringe-indicator-alist)
    visual-line--saved-state ;;simple.el:4544:    (kill-local-variable 'visual-line--saved-state)))
    vis-mode-saved-buffer-invisibility-spec ;;simple.el:6237:    (kill-local-variable 'vis-mode-saved-buffer-invisibility-spec))
    )
  "Local variables to survive the change of major mode.")

(when nil
  (make-variable-buffer-local 'mumamo-survive-minor-modes)
  (put 'mumamo-survive-minor-modes 'permanent-local t)
  (defvar mumamo-survive-minor-modes nil
    "Hold local minor mode variables specific major modes.
  Those values are saved when leaving a chunk with a certain
  major mode and restored when entering a chunk with the same
  major mode again.

  The value of this variable is an associative list where the key
  is a list with

    \(MAJOR-MODE MINOR-MODE)

  and the value is a stored value for the minor mode.")
  )

(defun mumamo-make-variable-buffer-permanent (var)
  "Make buffer local value of VAR survive when moving point to a new chunk.
When point is moved between chunks in a multi major mode the
major mode will be changed.  This will by default kill all local
variables unless they have a non-nil `permanent-local' property
\(see info node `(elisp)Creating Buffer-Local').

If you do not want to put a `permanent-local' property on a
variable you can instead use this function to make variable VAR
survive chunk switches in all mumamo multi major mode buffers."

;; If you want it to survive chunk switches only in the current
;; buffer then use `mumamo-make-local-permanent' instead."
  (pushnew var (default-value 'mumamo-survive)))

;; ;; Fix-me: use local value
;; ;; Fix-me: delelete local value when exiting mumamo
;; (defun mumamo-make-local-permanent (var)
;;   "Make buffer local value of VAR survive when moving point to a new chunk.
;; This is for the current buffer only.
;; In most cases you almost certainly want to use
;; `mumamo-make-variable-buffer-permanent' instead."
;;   (pushnew var mumamo-survive))

(defvar mumamo-survive-done-by-me nil
  "Variables set by mumamo already.
Used to avoid unnecessary warnings if setting major mode fails.")

;; (mumamo-hook-p 'viper-pre-command-hooks)
;; (mumamo-hook-p 'viper-before-change-functions)
;; (mumamo-hook-p 'c-special-indent-hook)
(defun mumamo-hook-p (sym)
  "Try to detect if SYM is a hook variable.
Just check the name."
  (let ((name (symbol-name sym)))
    (or (string= "-hook" (substring name -5))
        (string= "-hooks" (substring name -6))
        (string= "-functions" (substring name -10)))))

(defvar mumamo-major-mode nil)
(make-variable-buffer-local 'mumamo-major-mode)
(put 'mumamo-major-mode 'permanent-local t)

(defvar mumamo-change-major-mode-no-nos
  '((font-lock-change-mode t)
    (longlines-mode-off t)
    global-font-lock-mode-cmhh
    (nxml-cleanup t)
    (turn-off-hideshow t))
  "Avoid running these in `change-major-mode-hook'.")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Remove things from hooks temporarily

;; Fix-me: This is a bit disorganized, could not decide which level I
;; wanted this on.

(defvar mumamo-after-change-major-mode-no-nos
  '(nxhtml-global-minor-mode-enable-in-buffers
    global-font-lock-mode-enable-in-buffers)
  "Avoid running these in `after-change-major-mode-hook'.")

(defvar mumamo-removed-from-hook nil)

(defun mumamo-remove-from-hook (hook remove)
  (let (did-remove
        removed)
    (dolist (rem remove)
      ;;(message "rem.rem=%s" rem)
      (setq did-remove nil)
      (if (listp rem)
          (when (memq (car rem) (symbol-value hook))
            (setq did-remove t)
            (remove-hook hook (car rem) t))
        (when (memq rem (symbol-value hook))
          (setq did-remove t)
          (remove-hook hook rem)))
      (when did-remove
        (setq removed (cons rem removed))))
    (setq mumamo-removed-from-hook
          (cons (cons hook removed)
                mumamo-removed-from-hook))))

(defun mumamo-addback-to-hooks ()
  ;;(message "mumamo-removed-from-hook=%s" mumamo-removed-from-hook)
  (dolist (rem-rec mumamo-removed-from-hook)
    (mumamo-addback-to-hook (car rem-rec) (cdr rem-rec))))

(defun mumamo-addback-to-hook (hook removed)
  ;;(message "addback: hook=%s, removed=%s" hook removed)
  (dolist (rem removed)
    ;;(message "add.rem=%s" rem)
    (if (listp rem)
        (add-hook hook (car rem) nil t)
      (add-hook hook rem))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Compare mumamo-irrelevant-buffer-local-vars
(defvar mumamo-buffer-locals-dont-set
  '(
    adaptive-fill-mode
    adaptive-fill-first-line-regexp
    adaptive-fill-regexp
    add-log-current-defun-header-regexp
    auto-composition-function
    auto-composition-mode
    auto-composition-mode-major-mode
    auto-fill-chars

    beginning-of-defun-function
    buffer-auto-save-file-format
    buffer-auto-save-file-name
    buffer-backed-up
    buffer-display-count
    buffer-display-time
    buffer-file-coding-system
    buffer-file-format
    buffer-file-name
    buffer-file-truename
    buffer-invisibility-spec
    buffer-read-only
    buffer-saved-size
    buffer-undo-list
    c++-template-syntax-table
    c-<-op-cont-regexp
    c-<>-multichar-token-regexp
    c->-op-cont-regexp
    c-after-suffixed-type-decl-key
    c-after-suffixed-type-maybe-decl-key
    c-anchored-cpp-prefix
    c-assignment-op-regexp
    c-at-vsemi-p-fn
    c-backslash-column
    c-backslash-max-column
    c-basic-offset
    c-before-font-lock-function
    c-block-comment-prefix
    c-block-comment-start-regexp
    c-block-prefix-charset
    c-block-stmt-1-key
    c-block-stmt-2-key
    c-brace-list-key
    c-cast-parens
    c-class-key
    c-cleanup-list
    c-colon-type-list-re
    c-comment-only-line-offset
    c-comment-prefix-regexp
    c-comment-start-regexp
    c-current-comment-prefix
    c-decl-block-key
    c-decl-hangon-key
    c-decl-prefix-or-start-re
    c-decl-prefix-re
    c-decl-start-re
    c-doc-comment-start-regexp
    c-doc-comment-style
    c-found-types
    c-get-state-before-change-function
    c-hanging-braces-alist
    c-hanging-colons-alist
    c-hanging-semi&comma-criteria
    c-identifier-key
    c-identifier-start
    c-identifier-syntax-modifications
    c-identifier-syntax-table
    c-indent-comment-alist
    c-indent-comments-syntactically-p
    c-indentation-style
    c-keywords-obarray
    c-keywords-regexp
    c-known-type-key
    c-label-kwds-regexp
    c-label-minimum-indentation
    c-label-prefix-re
    c-line-comment-starter
    c-literal-start-regexp
    c-multiline-string-start-char
    c-nonlabel-token-key
    c-nonsymbol-chars
    c-nonsymbol-token-regexp
    c-not-decl-init-keywords
    c-offsets-alist
    c-old-BOM
    c-old-EOM
    c-opt-<>-arglist-start
    c-opt-<>-arglist-start-in-paren
    c-opt-<>-sexp-key
    c-opt-asm-stmt-key
    c-opt-bitfield-key
    c-opt-block-decls-with-vars-key
    c-opt-block-stmt-key
    c-opt-cpp-macro-define-id
    c-opt-cpp-macro-define-start
    c-opt-cpp-prefix
    c-opt-cpp-start
    c-opt-extra-label-key
    c-opt-friend-key
    c-opt-identifier-concat-key
    c-opt-inexpr-brace-list-key
    c-opt-method-key
    c-opt-op-identifier-prefix
    c-opt-postfix-decl-spec-key
    c-opt-type-component-key
    c-opt-type-concat-key
    c-opt-type-modifier-key
    c-opt-type-suffix-key
    c-other-decl-block-key
    c-other-decl-block-key-in-symbols-alist
    c-overloadable-operators-regexp
    c-paragraph-separate
    c-paragraph-start
    c-paren-stmt-key
    c-prefix-spec-kwds-re
    c-primary-expr-regexp
    c-primitive-type-key
    c-recognize-<>-arglists
    c-recognize-colon-labels
    c-recognize-knr-p
    c-recognize-paren-inexpr-blocks
    c-recognize-paren-inits
    c-recognize-typeless-decls
    c-regular-keywords-regexp
    c-simple-stmt-key
    c-special-brace-lists
    c-special-indent-hook
    c-specifier-key
    c-stmt-delim-chars
    c-stmt-delim-chars-with-comma
    c-string-escaped-newlines
    c-symbol-key
    c-symbol-start
    c-syntactic-eol
    c-syntactic-ws-end
    c-syntactic-ws-start
    c-type-decl-end-used
    c-type-decl-prefix-key
    c-type-decl-suffix-key
    c-type-prefix-key
    c-vsemi-status-unknown-p-fn

    case-fold-search
    comment-end
    comment-end-skip
    comment-indent-function
    comment-line-break-function
    comment-multi-line
    comment-start
    comment-start-skip
    cursor-type

    default-directory
    defun-prompt-regexp
    delay-mode-hooks

    enable-multibyte-characters
    end-of-defun-function

    fill-paragraph-function
    font-lock-beginning-of-syntax-function
    font-lock-defaults
    font-lock-extend-after-change-region-function
    font-lock-extend-region-functions
    font-lock-fontified
    font-lock-fontify-buffer-function
    font-lock-fontify-region-function
    font-lock-keywords
    font-lock-keywords-only
    font-lock-keywords-case-fold-search
    font-lock-mode
    font-lock-mode-hook
    font-lock-mode-major-mode
    font-lock-multiline
    font-lock-set-defaults
    font-lock-syntactic-keywords
    font-lock-syntactically-fontified
    font-lock-syntax-table
    font-lock-unfontify-buffer-function
    font-lock-unfontify-region-function
    fontification-functions
    forward-sexp-function

    indent-line-function
    indent-region-function
    imenu--index-alist
    imenu--last-menubar-index-alist
    imenu-create-index-function
    imenu-menubar-modified-tick
    isearch-mode

    jit-lock-after-change-extend-region-functions
    jit-lock-context-unfontify-pos
    jit-lock-contextually
    jit-lock-functions
    jit-lock-mode

    line-move-ignore-invisible
    local-abbrev-table

    major-mode
    mark-active
    ;;mark-ring
    mode-line-process
    mode-name

    normal-auto-fill-function
    nxhtml-minor-mode-major-mode

    open-paren-in-column-0-is-defun-start
    outline-level
    outline-regexp

    paragraph-ignore-fill-prefix
    paragraph-separate
    paragraph-start
    parse-sexp-ignore-comments
    parse-sexp-lookup-properties
    php-mode-pear-hook
    point-before-scroll

    ;; More symbols from visual inspection
    ;;before-change-functions
    ;;delayed-mode-hooks
    ;;imenu-case-fold-search
    ;;imenu-generic-expression
    rngalt-completing-read-tag
    rngalt-completing-read-attribute-name
    rngalt-completing-read-attribute-value
    rngalt-complete-first-try
    rngalt-complete-last-try
    rngalt-complete-tag-hooks

    syntax-begin-function
    )
  )

(defun mumamo-save-most-buffer-locals (major)
  "Save some local variables for major mode MAJOR.
This should be called before switching to a new chunks major
mode."
  ;;(message "mumamo-save-most-buffer-locals %s %s" major (current-buffer))
  (let ((locals (buffer-local-variables)))
    (setq locals (mapcar (lambda (local)
                           (unless
                               (or (memq (car local) mumamo-buffer-locals-dont-set)
                                   (memq (car local) mumamo-survive)
                                   (get (car local) 'permanent-local))
                             local))
                         locals))
    (setq locals (delq nil locals))
    (setq locals (sort locals (lambda (sym-a sym-b)
                                (string< (symbol-name (car sym-a))
                                         (symbol-name (car sym-b))))))
    (setq mumamo-buffer-locals-per-major
          (assq-delete-all major mumamo-buffer-locals-per-major))
    (setq mumamo-buffer-locals-per-major
          (cons (cons major-mode locals)
                mumamo-buffer-locals-per-major))))

;; (benchmark 1000 '(mumamo-save-most-buffer-locals major-mode))
;; (benchmark 1000 '(mumamo-restore-most-buffer-locals major-mode))
(defvar mumamo-restore-most-buffer-locals-in-hook-major nil)
(defun mumamo-restore-most-buffer-locals-in-hook ()
  (mumamo-restore-most-buffer-locals
   mumamo-restore-most-buffer-locals-in-hook-major)
  (setq mumamo-restore-most-buffer-locals-in-hook-major nil))
(put 'mumamo-restore-most-buffer-locals-in-hook 'permanent-local-hook t)

(defun mumamo-restore-most-buffer-locals (major)
  "Restore some local variables for major mode MAJOR.
This should be called after switching to a new chunks major
mode."
  ;;(message "mumamo-restore-most-buffer-locals %s %s" major (current-buffer))
  (let ((locals (cdr (assq major mumamo-buffer-locals-per-major)))
        var
        perm)
    (dolist (rec locals)
      (setq var (car rec))
      (setq perm (get var 'permanent-local))
      (unless (or perm
                  (memq var mumamo-buffer-locals-dont-set))
        (set (make-local-variable var) (cdr rec))))))

;; (defun mumamo-testing-new ()
;;   (let ((locals (buffer-local-variables))
;;         var
;;         perm
;;         )
;;     (dolist (rec locals)
;;       (setq var (car rec))
;;       (setq perm (get var 'permanent-local))
;;       (unless (or perm
;;                   (memq var mumamo-buffer-locals-dont-set))
;;         (setq var (cdr rec))))
;;     ))
;; ;;(benchmark 1000 '(mumamo-testing-new))

(defvar mumamo-buffer-locals-per-major nil)
(make-variable-buffer-local 'mumamo-buffer-locals-per-major)
(put 'mumamo-buffer-locals-per-major 'permanent-local t)

(defun mumamo-get-hook-value (hook remove)
  "Return hook HOOK value with entries in REMOVE removed.
Remove also t. The value returned is a list of both local and
default values."
  (let ((value (append (symbol-value hook) (default-value hook) nil)))
    (dolist (rem remove)
      (setq value (delq rem value)))
    (delq t value)))

;; FIX-ME: Clean up the different ways of surviving variables during
;; change of major mode.
(defun mumamo-set-major (major)
  "Set major mode to MAJOR for mumamo."
  (mumamo-msgfntfy "mumamo-set-major %s, %s" major (current-buffer))
  ;;(message "mumamo-set-major %s, %s" major (current-buffer))
  (let ((start-time (get-internal-run-time))
        end-time
        used-time
        ;; Tell `mumamo-change-major-function':
        (mumamo-set-major-running major)
        ;; Fix-me: Take care of the new values added to these hooks!
        ;; That looks difficult. We may after this have changes to
        ;; both buffer local value and global value. The global
        ;; changes are in this variable, but the buffer local values
        ;; have been set once again.
        (change-major-mode-hook (mumamo-get-hook-value
                                 'change-major-mode-hook
                                 mumamo-change-major-mode-no-nos))
        (after-change-major-mode-hook (mumamo-get-hook-value
                                       'after-change-major-mode-hook
                                       mumamo-after-change-major-mode-no-nos))
        ;; Some major modes deactivates the mark, we do not want that:
        deactivate-mark
        ;; Font lock
        (font-lock-mode font-lock-mode)
        ;; We have to save and reset the cursor type, at least when
        ;; Viper is used
        (old-cursor-type cursor-type)
        ;; Protect last-command: fix-me: probably remove
;;;         (old-last-command last-command)
        (last-command last-command)
        ;; Fix-me: remove this
        (old-rng-schema-file (when (boundp 'rng-current-schema-file-name) rng-current-schema-file-name))
        saved-state
        )
    ;; We are not changing mode from font-lock's point of view, so do
    ;; not tell font-lock (let binding these hooks is probably not a
    ;; good choice since they may contain other stuff too):
    ;;(message "here 1")
    (setq mumamo-removed-from-hook nil)
    (mumamo-remove-from-hook 'change-major-mode-hook mumamo-change-major-mode-no-nos)
    ;;(message "change-major-mode-hook=%s" change-major-mode-hook)
    ;;(message "change-major-mode-hook glob=%s" (default-value 'change-major-mode-hook))
;;;     (remove-hook 'change-major-mode-hook 'font-lock-change-mode t)
;;;     (remove-hook 'change-major-mode-hook 'longlines-mode-off t)
;;;     (remove-hook 'change-major-mode-hook 'global-font-lock-mode-cmhh)
;;;     ;; Added somewhere at the beginning of April to nxml:
;;;     (remove-hook 'change-major-mode-hook 'nxml-cleanup t)
;;;     ;; We are not changing mode from hs-minor-mode's point of view:
;;;     (remove-hook 'change-major-mode-hook 'turn-off-hideshow t)

    (dolist (sym (reverse mumamo-survive))
      (when (boundp sym)
        (when (and (get sym 'permanent-local)
                   (not (memq sym mumamo-survive-done-by-me))
                   (not (mumamo-hook-p sym)))
          (delq sym mumamo-survive)
          (lwarn 'mumamo-survive :warning
                 "Already 'permanent-local t: %s" sym))))
    ;; Fix-me: Implement alternative way since there are problems with
    ;; 'permanent-local right now. Copy the style used in
    ;; visual-line-mode.
;;;     (dolist (sym mumamo-survive)
;;;       (add-to-list 'mumamo-survive-done-by-me sym)
;;;       (put sym 'permanent-local t))
    (dolist (var mumamo-survive)
      (if (local-variable-p var)
          (push (cons var (symbol-value var))
                saved-state)))

    ;; For all hooks that probably can have buffer local values, go
    ;; through the buffer local values and look for a permanent-local
    ;; property on each function.  Remove those functions that does not
    ;; have it.  Then make the buffer local value of the hook survive
    ;; by putting a permanent-local property on it.
    (unless (> emacs-major-version 22)
      (dolist (hk mumamo-survive-hooks)
        (put hk 'permanent-local t)
        (when (local-variable-p hk)
          (let ((hkv (copy-sequence (symbol-value hk))))
            (dolist (v hkv)
              (unless (or (eq v t)
                          (get v 'permanent-local-hook))
                (remove-hook hk v t)
                ))))))

    (run-hooks 'mumamo-change-major-mode-hook)

    (setq mumamo-major-mode major)


    ;; Save local variables before switching major
    (mumamo-save-most-buffer-locals major-mode)
    ;; Restore local variables after switching, but do it in the
    ;; greatest ancestor's mode hook (see `run-mode-hooks'):
    (let (ancestor-hook-sym
          parent-hook-sym
          (parent major)
          ;;(restore-fun (lambda () (mumamo-restore-most-buffer-locals major)))
          )
      ;; We want the greatest ancestor's mode hook:
      (setq parent-hook-sym (intern-soft (concat (symbol-name parent) "-hook")))
      (when parent-hook-sym (setq ancestor-hook-sym parent-hook-sym))
      (while (get parent 'derived-mode-parent)
        (setq parent (get parent 'derived-mode-parent))
        (setq parent-hook-sym (intern-soft (concat (symbol-name parent) "-hook")))
        (when parent-hook-sym (setq ancestor-hook-sym parent-hook-sym)))
      (when ancestor-hook-sym
        ;; Put first in local hook to run it first:
        (setq mumamo-restore-most-buffer-locals-in-hook-major major)
        (add-hook ancestor-hook-sym
                  ;;restore-fun
                  'mumamo-restore-most-buffer-locals-in-hook
                  nil t))
      (funcall major) ;; <-----------------------------------------------
      (if (not ancestor-hook-sym)
          (mumamo-restore-most-buffer-locals major)
        (remove-hook ancestor-hook-sym
                     ;;restore-fun
                     'mumamo-restore-most-buffer-locals-in-hook
                     t)))

    (setq mode-name (concat (format-mode-line mode-name)
                            (save-match-data
                              (replace-regexp-in-string
                               "-mumamo-mode$" ""
                               (format "/%s" mumamo-multi-major-mode)))))

    ;;(mumamo-restore-most-buffer-locals major)

    (dolist (hk mumamo-survive-hooks) (put hk 'permanent-local nil))

;;     (when (and (featurep 'flymake)
;;                flymake-mode)
;;       (add-hook 'after-change-functions 'flymake-after-change-function nil t)
;;       (add-hook 'after-save-hook 'flymake-after-save-hook nil t)
;;       (add-hook 'kill-buffer-hook 'flymake-kill-buffer-hook nil t))


;;;     (dolist (sym mumamo-survive)
;;;       (when (boundp sym)
;;;         (put sym 'permanent-local nil)))
    (dolist (saved saved-state)
      (set (make-local-variable (car saved)) (cdr saved)))
    ;;(message "here 2")
    ;;(mumamo-addback-to-hook 'change-major-mode-hook mumamo-change-major-mode-no-nos)
    (mumamo-addback-to-hooks)
    (when (and (featurep 'mlinks)
               mlinks-mode)
      (add-hook 'after-change-functions 'mlinks-after-change t t))

    (setq cursor-type old-cursor-type)
;;;     (unless (eq last-command old-last-command)
;;;       (lwarn 'mumamo-set-major :error
;;;              "last-command 3=%s, old-last-command" last-command old-last-command)
;;;       (setq last-command old-last-command))
    (run-hooks 'mumamo-after-change-major-mode-hook)

    (when (derived-mode-p 'nxml-mode)
      (when (and old-rng-schema-file
                 (not (string= old-rng-schema-file rng-current-schema-file-name)))
        (let ((rng-schema-change-hook nil)) ;(list 'rng-alidate-clear)))
          (condition-case err
              (progn
                (rng-set-schema-file-1 old-rng-schema-file)
                (rng-what-schema))
            (nxml-file-parse-error
             (nxml-display-file-parse-error err)))
	 (when rng-validate-mode
           ;; Fix-me: Change rng-validate variables so that this is
           ;; not necessary any more.
           (rng-validate-mode 0)
           (rng-validate-mode 1))
          )))
    ;; The nxml-parser should not die:
    (when (mumamo-derived-from-mode (mumamo-main-major-mode) 'nxml-mode)
      (add-hook 'after-change-functions 'rng-after-change-function nil t)
      (add-hook 'after-change-functions 'nxml-after-change nil t)
      ;; Added these for Emacs 22:
      (unless nxml-prolog-end (setq nxml-prolog-end 1))
      (unless nxml-scan-end (setq nxml-scan-end (copy-marker 1))))

;;;     (when (and global-font-lock-mode
;;;                font-lock-global-modes
;;;                font-lock-mode)
;;;     (when global-font-lock-mode
;;;       (add-hook 'change-major-mode-hook 'global-font-lock-mode-cmhh))
;;;     (add-hook 'change-major-mode-hook 'font-lock-change-mode nil t)
;;;     (when (and (fboundp 'longlines-mode-off)
;;;                longlines-mode)
;;;       (add-hook 'change-major-mode-hook 'longlines-mode-off nil t))

    (mumamo-set-fontification-functions)

    ;; If user has used M-x flyspell-mode then we need to correct it:
    ;; Fix-me: This is inflexible. Need flyspell to cooperate.
    (when (featurep 'flyspell)
      (setq flyspell-generic-check-word-predicate 'mumamo-flyspell-verify))

    (if mumamo-done-first-set-major
        (setq mumamo-just-changed-major t)
      (mumamo-msgfntfy "mumamo-set-major: ----- removing 'fontified")
      ;; Set up to fontify buffer
      (mumamo-save-buffer-state nil
        (remove-text-properties (point-min) (point-max) '(fontified)))
      (setq mumamo-done-first-set-major t))

    (remove-hook 'pre-command-hook 'mumamo-set-major-pre-command t)

    ;; Timing, on a 3ghz cpu:
    ;;
    ;;   used-time=(0 0 0), major-mode=css-mode
    ;;   used-time=(0 0 0), major-mode=ecmascript-mode
    ;;   used-time=(0 0 0), major-mode=html-mode
    ;;   used-time=(0 0 203000), major-mode=nxhtml-mode
    ;;
    ;; After some changes 2007-04-25:
    ;;
    ;;   used-time=(0 0 15000), major-mode=nxhtml-mode
    ;;
    ;; which is 15 ms.  That seems acceptable though I am not sure
    ;; everything is correct when switching to nxhtml-mode yet.  I
    ;; will have to wait for bug reports ;-)
    ;;
    ;; The delay is clearly noticeable and disturbing IMO unless you
    ;; change major mode in an idle timer.
    ;;
    ;;(setq end-time (get-internal-run-time))
    ;;(setq used-time (time-subtract end-time start-time))
    ))

(defun mumamo-setup-local-fontification-vars ()
  "Set up buffer local variables for mumamo style fontification."
  (make-local-variable 'font-lock-fontify-region-function)
  (setq font-lock-fontify-region-function 'mumamo-fontify-region)

  ;; Like font-lock-turn-on-thing-lock:
  (make-local-variable 'font-lock-fontify-buffer-function)
  (setq font-lock-fontify-buffer-function 'jit-lock-refontify)
  (setq font-lock-fontify-buffer-function 'mumamo-fontify-buffer)
  ;; Don't fontify eagerly (and don't abort if the buffer is large).
  (set (make-local-variable 'font-lock-fontified) t)

  (make-local-variable 'font-lock-unfontify-buffer-function)
  (setq font-lock-unfontify-buffer-function 'mumamo-unfontify-buffer)


  (make-local-variable 'indent-line-function)
  (setq indent-line-function 'mumamo-indent-line-function)

  (set (make-local-variable 'fill-paragraph-function) 'mumamo-fill-paragraph-function)

  (make-local-variable 'indent-region-function)
  (setq indent-region-function 'mumamo-indent-region-function)

  ;;(set (make-local-variable 'syntax-begin-function) 'mumamo-beginning-of-syntax)

  ;; FIX-ME: Not sure about this one, but it looks like it must be
  ;; set:
  (make-local-variable 'jit-lock-contextually)
  (setq jit-lock-contextually t)
  )



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Turning on/off multi major modes

(defun mumamo-set-fontification-functions ()
  "Let mumamo take over fontification.
This is run after changing major mode so that jit-lock will get
the major mode specific values.  \(There are currently no such
values.)"
  ;; Give the jit machinery a starting point:
  (mumamo-jit-lock-register 'font-lock-fontify-region t)
  ;; Set the functions that font-lock should use:
  (mumamo-setup-local-fontification-vars)
  ;; Need some hook modifications to keep things together too:
  (add-hook 'change-major-mode-hook 'mumamo-change-major-function nil t)
  (add-hook 'post-command-hook 'mumamo-post-command nil t)
  (remove-hook 'change-major-mode-hook 'nxml-change-mode t)
  (remove-hook 'change-major-mode-hook 'nxhtml-change-mode t)
  )

(defun mumamo-initialize-state ()
  "Initialize some mumamo state variables."
  (setq mumamo-done-first-set-major nil)
  (setq mumamo-just-changed-major nil))

(defun mumamo-turn-on-actions (old-major-mode)
  "Do what is necessary to turn on mumamo.
Turn on minor mode function `font-lock-mode'.
Set up for mumamo style fontification.
Create a mumamo chunk at point.
Run `mumamo-turn-on-hook'.

OLD-MAJOR-MODE is used for the main major mode if the main major
mode in the chunk family is nil."
  ;;(unless font-lock-mode (font-lock-mode 1))
  (mumamo-msgfntfy "mumamo-turn-on-actions")
  (unless mumamo-current-chunk-family
    ;;(mumamo-select-chunk-family)
    (error "Internal error: Chunk family is not set")
    )
  (if (not mumamo-current-chunk-family)
      (progn
        (lwarn '(mumamo) :warning
               "Could not turn on mumamo because chunk family was not set\n\tin buffer %s."
               (current-buffer))
        (with-current-buffer "*Warnings*"
          (insert "\tFor more information see `")
          (mumamo-insert-describe-button 'define-mumamo-multi-major-mode 'describe-function)
          (insert "'.\n")))
    ;; Load major mode:
    (let ((main-major-mode (mumamo-main-major-mode)))
      (unless main-major-mode
        (setcar (cdr mumamo-current-chunk-family) old-major-mode)
        (setq main-major-mode (mumamo-main-major-mode)))
      ;;(with-temp-buffer (funcall main-major-mode))
      (setq mumamo-major-mode main-major-mode)
      (when (boundp 'nxml-syntax-highlight-flag)
        (when (mumamo-derived-from-mode main-major-mode 'nxml-mode)
          (set (make-local-variable 'nxml-syntax-highlight-flag) nil))))
    ;; Init fontification
    (mumamo-initialize-state)
    (mumamo-set-fontification-functions)
    (mumamo-save-buffer-state nil
      (remove-list-of-text-properties (point-min) (point-max)
                                      (list 'fontified)))
    ;; For validation header etc:
    (require 'rngalt nil t)
    (when (featurep 'rngalt)
      (setq rngalt-major-mode (mumamo-main-major-mode))
      (rngalt-update-validation-header-overlay))
    (when (featurep 'rng-valid)
      ;;(setq rng-get-major-mode-chunk-function 'mumamo-get-existing-chunk-at)
      (setq rng-get-major-mode-chunk-function 'mumamo-get-chunk-at)
      (setq rng-valid-nxml-major-mode-chunk-function 'mumamo-valid-nxml-chunk)
      (setq rng-end-major-mode-chunk-function 'overlay-end))
    ;;(mumamo-set-major-post-command)
    ;;(add-hook 'change-major-mode-hook 'mumamo-change-major-function nil t)
    (when (boundp 'flyspell-generic-check-word-predicate)
      (setq flyspell-generic-check-word-predicate 'mumamo-flyspell-verify))
    (run-hooks 'mumamo-turn-on-hook)
    (mumamo-get-chunk-save-buffer-state (point))
    ;;(message "(benchmark 1 '(mumamo-find-chunks))") (benchmark 1 '(mumamo-find-chunks nil nil))
    ;;(setq mumamo-end-last-chunk-pos nil)
    ;;(mumamo-find-chunks nil 'until-input)
    (mumamo-find-chunks nil)
    ))

;; (defun mumamo-on-font-lock-off ()
;;   "The reverse of `mumamo-turn-on-actions'."
;;   (let ((mumamo-main-major-mode (mumamo-main-major-mode)))
;;     (mumamo-turn-off-actions)
;;     ;; Turning off `font-lock-mode' also turns off `mumamo-mode'.  It is
;;     ;; quite tricky to not turn on `font-lock-mode' again in case we got
;;     ;; here because it was turned off.  We must first remove the cmhh
;;     ;; function and then also run the internal font lock turn off.
;;     (let* ((flm  font-lock-mode)
;;            (flgm global-font-lock-mode)
;;            (remove-cmhh (and (not flm) flgm)))
;;       ;; If remove-cmhh is non-nil then we got here because
;;       ;; `font-lock-mode' was beeing turned off in the buffer, but
;;       ;; `global-font-lock-mode' is still on.
;;       (when remove-cmhh
;;         (remove-hook 'change-major-mode-hook 'global-font-lock-mode-cmhh))

;;       (if mumamo-main-major-mode
;;           (funcall mumamo-main-major-mode)
;;         (fundamental-mode))

;;       (unless flm
;;         (setq font-lock-mode nil)
;;         (font-lock-mode-internal nil))
;;       (when remove-cmhh
;;         (add-hook 'change-major-mode-hook 'global-font-lock-mode-cmhh)))))

(defun mumamo-turn-off-actions ()
  "The reverse of `mumamo-turn-on-actions'."
  (mumamo-msgfntfy "mumamo-turn-off-actions")
  (when (fboundp 'nxhtml-validation-header-mode)
    (nxhtml-validation-header-mode -1))
  (when (mumamo-derived-from-mode
         (nth 1 mumamo-current-chunk-family) 'nxml-mode)
    (when (fboundp 'nxml-change-mode)
      (nxml-change-mode)))
  (when (and (boundp 'rng-validate-mode)
             rng-validate-mode)
    (rng-validate-mode 0))
  (when (featurep 'rng-valid)
    (setq rng-get-major-mode-chunk-function nil)
    (setq rng-valid-nxml-major-mode-chunk-function nil)
    (setq rng-end-major-mode-chunk-function nil)
    )
  (when (boundp 'rngalt-major-mode)
    (setq rngalt-major-mode nil))
  (remove-hook 'change-major-mode-hook 'mumamo-change-major-function t)
  ;;(mumamo-unfontify-chunks)
  ;;(remove-hook 'after-change-functions 'mumamo-jit-lock-after-change t)
  (remove-hook 'after-change-functions 'mumamo-after-change t)
  (remove-hook 'post-command-hook 'mumamo-post-command t)
  ;;(remove-hook 'c-special-indent-hook 'mumamo-c-special-indent t)
  (mumamo-remove-all-chunk-overlays)
  (save-restriction
    (widen)
    (mumamo-save-buffer-state nil
      (set-text-properties (point-min) (point-max) nil)))
  (setq mumamo-current-chunk-family nil)
  (setq mumamo-major-mode nil)
  (setq mumamo-multi-major-mode nil) ;; for minor-mode-map-alist
  (setq mumamo-multi-major-mode nil)
  (when (fboundp 'rng-cancel-timers) (rng-cancel-timers))
  )

(defvar mumamo-turn-on-hook nil
  "Normal hook run after turning on `mumamo-mode'.")
(put 'mumamo-turn-on-hook 'permanent-local t)

(defvar mumamo-change-major-mode-hook nil
  "Normal hook run before internal change of major mode.")
(put 'mumamo-change-major-mode-hook 'permanent-local t)

(defvar mumamo-after-change-major-mode-hook nil
  "Normal hook run after internal change of major mode.")
(put 'mumamo-after-change-major-mode-hook 'permanent-local t)




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Defining multi major modes

(defvar mumamo-defined-turn-on-functions nil
  "List of functions defined for turning on mumamo.
Those functions should be called instead of calling a major mode
function when you want to use multiple major modes in a buffer.
They may be added to for example `auto-mode-alist' to
automatically have the major mode support turned on when opening
a file.

Each of these functions defines how to mix certain major modes in
a buffer.

All functions defined by `define-mumamo-multi-major-mode' are
added to this list.  See this function for a general description
of how the functions work.

If you want to quickly define a new mix of major modes you can
use `mumamo-quick-static-chunk'.")

(defun mumamo-describe-chunks (chunks)
  "Return text describing CHUNKS."
  (let* ((desc
          (concat "Main major mode: `" (symbol-name (nth 1 chunks)) "'\n"
                  "\nFunctions for dividing into submodes:\n")))
    (dolist (divider (nth 2 chunks))
      (setq desc
            (concat
             desc
             "\n`" (symbol-name divider)
             "'\n   "
             (let ((doc (documentation divider t)))
               (if (not doc)
                   "(Not documented)"
                 (substring doc 0 (string-match "\n" doc)))))))
    (setq desc
          (concat
           desc
           "\n\n(Note that the functions for dividing into chunks returns\n"
           "a major mode specifier which may be translated into a major mode\n"
           "by `mumamo-main-major-mode'.)\n"))
    desc))

(defun mumamo-add-multi-keymap (toggle keymap)
  "Add TOGGLE and KEYMAP to `minor-mode-map-alist'.
This is used to add a keymap to multi major modes since the local
keymap is occupied by the major modes.

It is also used to add the `mumamo-map' keymap to every buffer
with a multi major mode."
  ;; Copied from add-minor-mode
  ;; Add the map to the minor-mode-map-alist.
  (when keymap
    (let ((existing (assq toggle minor-mode-map-alist))
          (after t))
      (if existing
	  (setcdr existing keymap)
	(let ((tail minor-mode-map-alist) found)
	  (while (and tail (not found))
	    (if (eq after (caar tail))
		(setq found tail)
	      (setq tail (cdr tail))))
	  (if found
	      (let ((rest (cdr found)))
		(setcdr found nil)
		(nconc found (list (cons toggle keymap)) rest))
	    (setq minor-mode-map-alist (cons (cons toggle keymap)
					     minor-mode-map-alist))))))))

(defvar mumamo-map
  (let ((map (make-sparse-keymap)))
    (define-key map [(control meta prior)] 'mumamo-backward-chunk)
    (define-key map [(control meta next)]  'mumamo-forward-chunk)
    ;; Use mumamo-indent-line-function:
    ;;(define-key map [tab] 'indent-for-tab-command)
    map)
  "Keymap that is active in all mumamo buffers.
It has the some priority as minor mode maps.")
;;(make-variable-buffer-local 'mumamo-map)
(put 'mumamo-map 'permanent-local t)

(mumamo-add-multi-keymap 'mumamo-multi-major-mode mumamo-map)

;; fix-me: tell no sub-chunks in sub-chunks
(defmacro define-mumamo-multi-major-mode (fun-sym spec-doc chunks)
  "Define a function that turn on support for multiple major modes.
Define a function FUN-SYM that set up to divide the current
buffer into chunks with different major modes.

The documentation string for FUN-SYM should contain the special
documentation in the string SPEC-DOC, general documentation for
functions of this type and information about CHUNKS.

The new function will use the definitions in CHUNKS to make the
dividing of the buffer.

The function FUN-SYM can be used to setup a buffer instead of a
major mode function:

- The function FUN-SYM can be called instead of calling a major
  mode function when you want to use multiple major modes in a
  buffer.

- The defined function can be used instead of a major mode
  function in for example `auto-mode-alist'.

- As the very last thing FUN-SYM will run the hook FUN-SYM-hook,
  just as major modes do.

- There is also a general hook, `mumamo-turn-on-hook', which is
  run when turning on mumamo with any of these functions.  This
  is run right before the hook specific to any of the functions
  above that turns on the multiple major mode support.

- The multi major mode FUN-SYM has a keymap named FUN-SYM-map.
  This overrides the major modes' keymaps since it is handled as
  a minor mode keymap.

- There is also a special mumamo keymap, `mumamo-map' that is
  active in every buffer with a multi major mode.  This is also
  handled as a minor mode keymap and therefor overrides the major
  modes' keymaps.

- However when this support for multiple major mode is on the
  buffer is divided into chunks, each with its own major mode.

- The chunks are fontified according the major mode assigned to
  them for that.

- Indenting is also done according to the major mode assigned to
  them for that.

- The actual major mode used in the buffer is changed to the one
  in the chunk when moving point between these chunks.

- When major mode is changed the hooks for the new major mode,
  `after-change-major-mode-hook' and `change-major-mode-hook' are
  run.

- There will be an alias for FUN-SYM called mumamo-alias-FUN-SYM.
  This can be used to check whic multi major modes have been
  defined.

** A little bit more technical description:

The dividing of a buffer into chunks is done during fontification
by `mumamo-get-chunk-at'.

The name of the function is saved in in the buffer local variable
`mumamo-multi-major-mode' when the function is called.

All functions defined by this macro is added to the list
`mumamo-defined-turn-on-functions'.

Basically Mumamo handles only major modes that uses jit-lock.
However as a special effort also `nxml-mode' and derivatives
thereof are handled.  Since it seems impossible to me to restrict
those major modes fontification to only a chunk without changing
`nxml-mode' the fontification is instead done by
`html-mode'/`sgml-mode' for chunks using `nxml-mode' and its
derivates.

CHUNKS is a list where each entry have the format

  \(CHUNK-DEF-NAME MAIN-MAJOR-MODE SUBMODE-CHUNK-FUNCTIONS)

CHUNK-DEF-NAME is the key name by which the entry is recognized.
MAIN-MAJOR-MODE is the major mode used when there is no chunks.
If this is nil then `major-mode' before turning on this mode will
be used.

SUBMODE-CHUNK-FUNCTIONS is a list of the functions that does the
chunk division of the buffer.  They are tried in the order they
appear here during the chunk division process.

If you want to write new functions for chunk divisions then
please see `mumamo-find-possible-chunk'.  You can perhaps also
use `mumamo-quick-static-chunk' which is more easy-to-use
alternative.  See also the file mumamo-fun.el where there are
many routines for chunk division.

When you write those new functions you may want to use some of
the functions for testing chunks:

 `mumamo-test-create-chunk-at'  `mumamo-test-create-chunks-at-all'
 `mumamo-test-easy-make'        `mumamo-test-fontify-region'

These are in the file mumamo-test.el."
  ;;(let ((c (if (symbolp chunks) (symbol-value chunks) chunks))) (message "c=%S" c))
  (let* (;;(mumamo-describe-chunks (make-symbol "mumamo-describe-chunks"))
         (turn-on-fun (if (symbolp fun-sym)
                          fun-sym
                        (error "Parameter FUN-SYM must be a symbol")))
         (turn-on-fun-alias (intern (concat "mumamo-alias-" (symbol-name fun-sym))))
         ;; Backward compatibility nXhtml v 1.60
         (turn-on-fun-old (when (string= (substring (symbol-name fun-sym) -5)
                                         "-mode")
                            (intern (substring (symbol-name fun-sym) 0 -5))))
         (turn-on-hook (intern (concat (symbol-name turn-on-fun) "-hook")))
         (turn-on-map  (intern (concat (symbol-name turn-on-fun) "-map")))
         (turn-on-hook-doc (concat "Hook run at the very end of `"
                                   (symbol-name turn-on-fun) "'."))
         (chunks2 (if (symbolp chunks)
                      (symbol-value chunks)
                    chunks))
         (docstring
          (concat
           spec-doc
           "

This function is called a multi major mode.  The main use for it
is in `auto-mode-alist' to have Emacs do this setup whenever you
open a file named in a certain way.  \(You can of course call
this function directly yourself too.)

It sets up for multiple mode in the following way:

"
           ;; Fix-me: During byte compilation the next line is not
           ;; expanded as I thought because the functions in CHUNK is
           ;; not defined. How do I fix this?
           (funcall 'mumamo-describe-chunks chunks2)
           "

At the very end this multi major mode function runs first the hook
`mumamo-turn-on-hook' and then `" (symbol-name turn-on-hook) "'.

There is a keymap specific to this multi major mode, but it is
not returned by `current-local-map' which returns the chunk's
major mode's local keymap.

The keymap is named `" (symbol-name turn-on-map) "'.

This major mode has an alias `mumamo-alias-"
(symbol-name turn-on-fun) "'.
For more information see `define-mumamo-multi-major-mode'."
           )))
    `(progn
       (add-to-list 'mumamo-defined-turn-on-functions (cons (car ',chunks2) ',turn-on-fun))
       (defvar ,turn-on-hook nil ,turn-on-hook-doc)
       (defvar ,turn-on-map (make-sparse-keymap)
         ,(concat "Keymap for multi major mode function `"
                  (symbol-name turn-on-fun) "'"))
       (defvar ,turn-on-fun nil)
       (make-variable-buffer-local ',turn-on-fun)
       (put ',turn-on-fun 'permanent-local t)
       (defun ,turn-on-fun nil ,docstring
         (interactive)
         (let ((old-major-mode (or mumamo-major-mode
                                   major-mode)))
           (kill-all-local-variables)
           (run-hooks 'change-major-mode-hook)
           (setq mumamo-multi-major-mode ',turn-on-fun)
           (setq ,turn-on-fun t)
           (mumamo-add-multi-keymap ',turn-on-fun ,turn-on-map)
           (setq mumamo-current-chunk-family (copy-tree ',chunks2))
           (mumamo-turn-on-actions old-major-mode)
           (run-hooks ',turn-on-hook)))
       (defalias ',turn-on-fun-alias ',turn-on-fun)
       (when (intern-soft ',turn-on-fun-old)
         (defalias ',turn-on-fun-old ',turn-on-fun))
       )))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Indenting, filling, moving etc

;; FIX-ME: Indentation in perl here doc indents the ending mark which
;; corrupts the perl here doc.

(defun mumamo-indent-line-function ()
  "Function to indent the current line.
This is the buffer local value of `indent-line-function' when
mumamo is used."
  (let ((here (point-marker))
        (before-text (<= (current-column) (current-indentation))))
    (mumamo-indent-line-function-1 nil nil)
    ;; If the marker was in the indentation part strange things happen
    ;; if we try to go back to the marker, at least in php-mode parts.
    (if before-text
        (back-to-indentation)
      (goto-char here))))

;; Fix-me: return chunks instead?
;;(defun mumamo-indent-line-major-modes ()
(defun mumamo-indent-line-chunks ()
  "Return major modes for indenting current line.
A list with major mode at beginning and dito at the end of line
is returned."
  ;; Fix-me: must take markers into account to when a submode includes
  ;; the markers.
  (save-restriction
    (widen)
    (let* ((lb-pos (line-beginning-position))
           (le-pos (line-end-position))
           (pos1 (if (> lb-pos (point-min))
                     (1- lb-pos)
                   (point-min)))
           (pos2 lb-pos)
           (pos3 le-pos)
           (pos4 (if (< le-pos (point-max))
                     (1+ le-pos)
                   (point-max)))
           (ovl1 (mumamo-get-chunk-save-buffer-state pos1))
           (ovl2 (if (>= (overlay-end ovl1) pos2)
                     ovl1
                   (mumamo-get-chunk-save-buffer-state pos2)))
           (ovl3 (mumamo-get-chunk-save-buffer-state pos3))
           (ovl4 (if (<= pos4 (overlay-end ovl3))
                   ovl3
                 (mumamo-get-chunk-save-buffer-state pos4)))
         )
      (list ovl1 ovl2 ovl3 ovl4))))

;; Fix-me: need to back up past comments in for example <style> /* comment */
;; fix-me: clean up
(put 'mumamo-error-ind-0 'error-conditions '(error mumamo-error-ind-0))
(put 'mumamo-error-ind-0 'error-message "indentation 0 in sub chunk")
;; Fix-me: error indenting in xml-as-string at <?\n?>
(defun mumamo-indent-line-function-1 (prev-line-chunks
                                      last-main-major-indent)
  "Indent current line.
When doing that care must be taken if this line's major modes at
the start and end are different from previous line major modes.
The latter may be known through the parameter PREV-LINE-CHUNKS.

Also the indentation of the last previous main major line may be
necessary to know.  This may be known through the parameter
LAST-MAIN-MAJOR-INDENT.

If the two parameters above are nil then this function will
search backwards in the buffer to try to determine their values.

The following rules are used when indenting:

- If the major modes are the same in this and the previous line
  then indentation is done using that mode.

- Otherwise if going into a submode indentation is increased by
  `mumamo-submode-indent-offset'.

- When going out of a submode indentation is reset to
  LAST-MAIN-MAJOR-INDENT.

- When going from one submode to another the new submode's
  indentation will be relative LAST-MAIN-MAJOR-INDENT."
  (let* ((this-line-chunks (mumamo-indent-line-chunks))
         (this-line-major0 (mumamo-chunk-major-mode (nth 0 this-line-chunks)))
         (this-line-major1 (mumamo-chunk-major-mode (nth 1 this-line-chunks)))
         (this-line-major2 (mumamo-chunk-major-mode (nth 2 this-line-chunks)))
         (this-line-major3 (mumamo-chunk-major-mode (nth 3 this-line-chunks)))
         prev-line-major0
         prev-line-major1
         prev-line-major2
         prev-line-major3
         this-line-indent-major
         major-indent-line-function
         (main-major (mumamo-main-major-mode))
         (old-indent (current-indentation))
         (entering-submode nil)
         (leaving-submode nil)
         want-indent ;; The indentation we desire
         got-indent
         (here-on-line (point-marker))
         this-pending-undo-list)
    (unless prev-line-chunks
      (save-excursion
        (goto-char (line-beginning-position 1))
        (skip-chars-backward "\n\t ")
        (goto-char (line-beginning-position 1))
        (setq prev-line-chunks (mumamo-indent-line-chunks))
        ))
    (setq prev-line-major0 (mumamo-chunk-major-mode (nth 0 prev-line-chunks)))
    (setq prev-line-major1 (mumamo-chunk-major-mode (nth 1 prev-line-chunks)))
    (setq prev-line-major2 (mumamo-chunk-major-mode (nth 2 prev-line-chunks)))
    (setq prev-line-major3 (mumamo-chunk-major-mode (nth 3 prev-line-chunks)))
    (mumamo-msgindent "mumamo-indent-line-function-1 L%s last=%s\n  this0=%s  %s  %s  %s\n  prev0=%s  %s  %s  %s"
                      (line-number-at-pos)
                      last-main-major-indent
                      this-line-major0 this-line-major1 this-line-major2 this-line-major3
                      prev-line-major0 prev-line-major1 prev-line-major2 prev-line-major3
                      )
    (setq entering-submode
          (or
           ;; Going from main to sub
           (and (eq prev-line-major1 main-major)
                (not (eq this-line-major1 main-major))
                (not (eq this-line-major2 main-major))
                (let ((this-chunk-1 (nth 1 this-line-chunks)))
                  (not (= (line-beginning-position) (overlay-start this-chunk-1))))
                )
           ;; Going from sub to sub
           (and (not (eq prev-line-major1 main-major))
                (not (eq this-line-major1 main-major))
                (not (eq prev-line-major1
                         this-line-major1
                         )))))
    (setq leaving-submode
          (and (not (eq prev-line-major2 main-major))
               (eq this-line-major2 main-major)
               ))
    ;; Fix-me: indentation of <?\n?>
    (assert (not (and leaving-submode entering-submode)) t)
    ;; Fix-me: indentation
    ;;(error "leaving=%s, entering=%s this0,1,2,3=%s,%s,%s,%s" leaving-submode entering-submode this-line-major0 this-line-major1 this-line-major2 this-line-major3)
    (when (or leaving-submode entering-submode)
      (unless last-main-major-indent
        (save-excursion
          (while (not last-main-major-indent)
            (if (bobp)
                (setq last-main-major-indent 0)
              (goto-char (line-beginning-position 0))
              (when (eq main-major
                        (mumamo-chunk-major-mode
                         (car
                          (mumamo-indent-line-chunks)))
                        )
                (skip-chars-forward " \t")
                (if (eolp)
                    (setq last-main-major-indent 0)
                  (setq last-main-major-indent (current-column)))))))))
    (mumamo-msgindent "  leaving-submode=%s, entering-submode=%s" leaving-submode entering-submode)
    (cond
     (leaving-submode
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;;;;; First line after submode
      (mumamo-msgindent "  leaving last-main-major-indent=%s" last-main-major-indent)
      (setq want-indent last-main-major-indent))
     (entering-submode
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;;;;; First line in submode
      (setq this-line-indent-major this-line-major0)
      ;;(when (and prev-line-major0 (not (eq this-line-major0 prev-line-major0))) (setq this-line-indent-major prev-line-major0))
      (mumamo-msgindent "  this-line-indent-major=%s, major-mode=%s this0=%s" this-line-indent-major major-mode this-line-major0)
      (mumamo-msgindent "  mumamo-submode-indent-offset=%s" mumamo-submode-indent-offset)
      (unless (eq this-line-indent-major major-mode) (mumamo-set-major this-line-indent-major))
      (setq want-indent (+ last-main-major-indent
                           (if (= 0 last-main-major-indent)
                               (if mumamo-submode-indent-offset-0
                                   mumamo-submode-indent-offset-0
                                 -1000)
                             (if mumamo-submode-indent-offset
                                 mumamo-submode-indent-offset
                               -1000))))
      (unless (< 0 want-indent) (setq want-indent nil))
      (when (and want-indent (mumamo-indent-use-widen major-mode))
        ;; In this case only use want-indent if it is bigger than the
        ;; indentation calling indent-line-function would give.
        (condition-case nil
            (atomic-change-group
              (mumamo-call-indent-line)
              (when (> want-indent (current-indentation))
                (signal 'mumamo-error-ind-0 nil))
              (setq want-indent nil))
          (mumamo-error-ind-0)))
      (unless want-indent
        (mumamo-call-indent-line))
      (mumamo-msgindent "  enter sub.want-indent=%s, curr=%s, last-main=%s" want-indent (current-indentation)
                        last-main-major-indent)
      ;;(unless (> want-indent (current-indentation)) (setq want-indent nil))
      )
     (t
      ;; We have to change major mode, because we know nothing
      ;; about the requirements of the indent-line-function:
      ;; Fix-me: This may be cured by RMS suggestion to
      ;; temporarily set all variables back to global values?
      (setq this-line-indent-major this-line-major0)
      (mumamo-msgindent "  this-line-indent-major=%s" this-line-indent-major)
      (unless (eq this-line-indent-major major-mode) (mumamo-set-major this-line-indent-major))
      ;; Use the major mode at the beginning of since a sub chunk may
      ;; start at start of line.
      (if (eq this-line-major1 main-major)
          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          ;;;;; In main major mode
          ;;
          ;; Fix-me: Take care of the case when all the text is in a
          ;; sub chunk. In that case use the same indentation as if
          ;; the code all belongs to the surrounding major mode.
          (progn
            (mumamo-msgindent "  In main major mode")
            (mumamo-call-indent-line)
            (setq last-main-major-indent (current-indentation)))
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;;;;; In sub major mode
        ;;
        ;; Get the indentation the major mode alone would use:
        ;;(setq got-indent (mumamo-get-major-mode-indent-column))
        ;; Since this line has another major mode than the
        ;; previous line we instead want to indent relative to
        ;; that line in a way decided in mumamo:
        (mumamo-msgindent "  In sub major mode")
        (let ((chunk (mumamo-get-chunk-save-buffer-state (point)))
              (font-lock-dont-widen t)
              ind-zero
              (here (point))
              ind-on-first-sub-line)
          (save-restriction
            (narrow-to-region (mumamo-chunk-syntax-min chunk)
                              (mumamo-chunk-syntax-max chunk))
            (save-restriction
              (condition-case nil
                  (atomic-change-group
                    (mumamo-call-indent-line)
                    (when (= 0 (current-indentation))
                      (setq ind-zero t)
                      ;; It is maybe ok if indentation on first sub
                      ;; line is 0 so check that:
                      (goto-char (point-min))
                      (setq ind-on-first-sub-line (current-indentation))
                      (goto-char here)
                      (widen)
                      (signal 'mumamo-error-ind-0 nil)))
                (mumamo-error-ind-0)))
            ;; Unfortunately the indentation can sometimes get 0
            ;; here even though it is clear it should not be 0. This
            ;; happens when there are only comments or empty lines
            ;; above.
            ;;
            ;; See c:/test/erik-lilja-index.php for an example.
            (when ind-zero ;(and t (= 0 (current-indentation)))
              (save-excursion
                (setq want-indent 0)
                (unless (= 0 ind-on-first-sub-line)
                  (while (and (= 0 want-indent)
                              (/= (point) (point-min)))
                    (beginning-of-line 0)
                    (setq want-indent (current-indentation)))
                  ;; Now if want-indent is still 0 we need to look further above
                  (when (= 0 want-indent)
                    (widen)
                    (while (and (= 0 want-indent)
                                (/= (point) (point-min)))
                      (beginning-of-line 0)
                      (setq want-indent (current-indentation)))
                    ;; If we got to the main major mode we need to add
                    ;; the special submode offset:
                    (let* ((ovl (mumamo-get-chunk-save-buffer-state (point)))
                           (major (mumamo-chunk-major-mode ovl)))
                      (when (eq major main-major)
                        (setq want-indent (+ want-indent
                                             (if (= 0 want-indent)
                                                 mumamo-submode-indent-offset-0
                                               mumamo-submode-indent-offset)))))))))
            )))))
    (when want-indent
      (indent-line-to want-indent))
    (goto-char here-on-line)
    ;;(message "exit: %s" (list this-line-chunks last-main-major-indent))
    (list this-line-chunks last-main-major-indent)))

;; Fix-me: use this for first line in a submode
(defun mumamo-indent-use-widen (major-mode)
  (let* ((specials (cadr (assoc major-mode mumamo-major-mode-indent-specials)))
         (use-widen (memq 'use-widen specials))
         (use-widen-maybe (assq 'use-widen specials)))
    (or use-widen
        (memq mumamo-multi-major-mode (cadr use-widen-maybe)))))
;;(mumamo-indent-use-widen 'php-mode)
;;(mumamo-indent-use-widen 'nxhtml-mode)
;;(mumamo-indent-use-widen 'html-mode)

;; Fix-me: remove
(defun mumamo-indent-special-or-default (default-indent)
  "Indent to DEFAULT-INDENT unless a special indent can be done."
  (mumamo-with-major-mode-indentation major-mode
    `(progn
         (if (mumamo-indent-use-widen major-mode)
             (save-restriction
               (widen)
               (mumamo-msgindent "=> special-or-default did widen, %s" major-mode)
               (funcall indent-line-function))
           (indent-to-column default-indent)))))

(defun mumamo-call-indent-line ()
  "Call the relevant `indent-line-function'."
  (mumamo-with-major-mode-indentation major-mode
    `(save-restriction
       (when (mumamo-indent-use-widen major-mode)
         (mumamo-msgindent "=> indent-line did widen")
         (widen))
       (funcall indent-line-function))))

(defun mumamo-indent-region-function (start end)
  "Indent the region between START and END."
  (save-excursion
    (setq end (copy-marker end))
    (goto-char start)
    (let ((old-point -1)
          prev-line-major
          last-main-major-indent)
      (while (and (< (point) end)
                  (/= old-point (point)))
        ;;(message "mumamo-indent-region-function, point=%s" (point))
        (or (and (bolp) (eolp))
            (let ((ret (mumamo-indent-line-function-1
                        prev-line-major
                        last-main-major-indent)))
              (setq prev-line-major        (nth 0 ret))
              (setq last-main-major-indent (nth 1 ret))))
        (setq old-point (point))
        (forward-line 1)))
    (message "Ready indenting region")))


(defun mumamo-fill-forward-paragraph-function(&optional arg)
  "Function to move over paragraphs used by filling code.
This is the buffer local value of
`fill-forward-paragraph-function' when mumamo is used."
  ;; fix-me: Do this chunk by chunk
  ;; Fix-me: use this (but only in v 23)
  (let* ((ovl (mumamo-get-chunk-save-buffer-state (point)))
         (major (mumamo-chunk-major-mode ovl)))
    (mumamo-with-major-mode-fontification major
      fill-forward-paragraph-function)))

(defun mumamo-fill-paragraph-function(&optional justify region)
  "Function to fill the current paragraph.
This is the buffer local value of `fill-paragraph-function' when
mumamo is used."
  (let* ((ovl (mumamo-get-chunk-save-buffer-state (point)))
         (major (mumamo-chunk-major-mode ovl))
         ;;(main-major (mumamo-main-major-mode))
         )
    (save-restriction
      (narrow-to-region (mumamo-chunk-syntax-min ovl)
                        (mumamo-chunk-syntax-max ovl))
      (mumamo-with-major-mode-fontification major
        ;;`(let ((fill-paragraph-function ,mumamo-original-fill-paragraph-function))
        `(fill-paragraph ,justify ,region)))))

(defun mumamo-forward-chunk ()
  "Move forward to next chunk."
  (interactive)
  (let* ((chunk (mumamo-get-chunk-save-buffer-state (point)))
         (end-pos (overlay-end chunk)))
    (goto-char (min end-pos
                    (point-max)))))

(defun mumamo-backward-chunk ()
  "Move backward to previous chunk."
  (interactive)
  (let* ((chunk (mumamo-get-chunk-save-buffer-state (point)))
         (start-pos (overlay-start chunk)))
    (goto-char (max (1- start-pos)
                    (point-min)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Spell checking

(defun mumamo-flyspell-verify ()
  "Function used for `flyspell-generic-check-word-predicate'."
  (let* ((chunk (when mumamo-multi-major-mode
                  (mumamo-get-existing-chunk-at (point))))
         (chunk-major (when chunk (mumamo-chunk-major-mode chunk)))
         (mode-predicate (when chunk-major
                           (let ((predicate (get chunk-major
                                                 'flyspell-mode-predicate)))
                             (if predicate
                                 predicate
                               (if (mumamo-derived-from-mode chunk-major
                                                             'text-mode)
                                   nil
                                 'flyspell-generic-progmode-verify)))))
         )
    (if mode-predicate
        ;; Fix-me: (run-hooks 'flyspell-prog-mode-hook)
        (funcall mode-predicate)
      t)))

(eval-after-load 'rng-match
;;;   (defun rng-match-init-buffer ()
;;;     (make-local-variable 'rng-compile-table)
;;;     (make-local-variable 'rng-ipattern-table)
;;;     (make-local-variable 'rng-last-ipattern-index))
  (progn
    (put 'rng-compile-table 'permanent-local t)
    (put 'rng-ipattern-table 'permanent-local t)
    (put 'rng-last-ipattern-index 'permanent-local t)
    ))

(eval-after-load 'flyspell
  (progn
    (put 'flyspell-mode 'permanent-local t)

    (put 'flyspell-generic-check-word-predicate 'permanent-local t)

    (put 'flyspell-casechars-cache 'permanent-local t)
    (put 'flyspell-ispell-casechars-cache 'permanent-local t)

    (put 'flyspell-not-casechars-cache 'permanent-local t)
    (put 'flyspell-ispell-not-casechars-cache 'permanent-local t)

    (put 'flyspell-auto-correct-pos 'permanent-local t)
    (put 'flyspell-auto-correct-region 'permanent-local t)
    (put 'flyspell-auto-correct-ring 'permanent-local t)
    (put 'flyspell-auto-correct-word 'permanent-local t)

    (put 'flyspell-consider-dash-as-word-delimiter-flag 'permanent-local t)

    (put 'flyspell-dash-dictionary 'permanent-local t)

    (put 'flyspell-dash-local-dictionary 'permanent-local t)

    (put 'flyspell-word-cache-start 'permanent-local t)
    (put 'flyspell-word-cache-end 'permanent-local t)
    (put 'flyspell-word-cache-word 'permanent-local t)
    (put 'flyspell-word-cache-result 'permanent-local t)

    (put 'flyspell-word-cache-start 'permanent-local t)


    (put 'flyspell-kill-ispell-hook 'permanent-local-hook t)
    (put 'flyspell-post-command-hook 'permanent-local-hook t)
    (put 'flyspell-pre-command-hook 'permanent-local-hook t)
    (put 'flyspell-after-change-function 'permanent-local-hook t)
    (put 'flyspell-hack-local-variables-hook 'permanent-local-hook t)
    (put 'flyspell-auto-correct-previous-hook 'permanent-local-hook t)

    (when mumamo-multi-major-mode
      (when (featurep 'flyspell)
        (setq flyspell-generic-check-word-predicate 'mumamo-flyspell-verify)))
    ))

(defun flyspell-mumamo-mode ()
  "Turn on function `flyspell-mode' for multi major modes."
  (interactive)
  (require 'flyspell)
  (setq flyspell-generic-check-word-predicate 'mumamo-flyspell-verify)
  (flyspell-mode 1)
  ;;(run-hooks 'flyspell-prog-mode-hook)
  )

(eval-after-load 'sgml-mode
  (progn
    (put 'sgml-tag-face-alist 'permanent-local t)
    (put 'sgml-display-text   'permanent-local t)
    (put 'sgml-tag-alist 'permanent-local t)
    (put 'sgml-face-tag-alist 'permanent-local t)
    (put 'sgml-tag-help 'permanent-local t)
    ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; New versions of syntax-ppss functions, temporary written as defadvice.

(defadvice syntax-ppss-flush-cache (around
                                    mumamo-advice-syntax-ppss-flush-cache
                                    activate
                                    compile
                                    )
  "Support for mumamo.
See the defadvice for `syntax-ppss' for an explanation."
  (let ((pos (ad-get-arg 0)))
    (let* ((chunk-at-pos (when (and (boundp 'mumamo-multi-major-mode) mumamo-multi-major-mode) (mumamo-get-existing-chunk-at pos))))
      (if chunk-at-pos
          (let* ((syntax-ppss-last  (overlay-get chunk-at-pos 'syntax-ppss-last))
                 (syntax-ppss-cache (overlay-get chunk-at-pos 'syntax-ppss-cache)))
            (setq ad-return-value ad-do-it)
            (overlay-put chunk-at-pos 'syntax-ppss-last syntax-ppss-last)
            (overlay-put chunk-at-pos 'syntax-ppss-cache syntax-ppss-cache)
            )
        (setq ad-return-value ad-do-it)))))

(defvar mumamo-syntax-chunk-at-pos nil
  "Internal use.")
(make-variable-buffer-local 'mumamo-syntax-chunk-at-pos)

;; Fix-me: Is this really needed?
;; See http://lists.gnu.org/archive/html/emacs-devel/2008-04/msg00374.html
(defadvice syntax-ppss-stats (around
                              mumamo-advice-syntax-ppss-stats
                              activate
                              compile
                              )
  "Support for mumamo.
See the defadvice for `syntax-ppss' for an explanation."
  (if mumamo-syntax-chunk-at-pos
      (let* ((syntax-ppss-stats
              (overlay-get mumamo-syntax-chunk-at-pos 'syntax-ppss-stats)))
        (setq ad-return-value ad-do-it)
        (overlay-put mumamo-syntax-chunk-at-pos 'syntax-ppss-stats syntax-ppss-stats)
        )
    (setq ad-return-value ad-do-it)))

(defvar mumamo-syntax-ppss-major nil)

;; FIX-ME: There is a problem with " in xhtml files, especially after
;; syntax="...".  Looks like it is the " entry in
;; `sgml-font-lock-syntactic-keywords' that is jumping in!  Dumping
;; things in `font-lock-apply-syntactic-highlight' seems to show that.
;;
;; (I have put in some dump code in my patched version of
;; Emacs+EmacsW32 there for that.  This is commented out by default
;; and it will only work for the file nxhtml-changes.html which is big
;; enough for the problem to occur.  It happens at point 1109.)
;;
;; It is this piece of code where the problem arise:
;;
;;   (if (prog1
;;           (zerop (car (syntax-ppss (match-beginning 0))))
;;         (goto-char (match-end 0)))
;;       .)
;;
;;
;; It comes from `sgml-font-lock-syntactic-keywords' in sgml-mode.el
;; and is supposed to protect from " that is not inside a tag.
;; However in this case for the second " in syntax="..." `syntax-ppss'
;; returns 0 as the first element in its return value.  That happen
;; even though `major-mode' is correctly `html-mode'.  It leads to
;; that the property 'syntax with the value (1) is added to the "
;; after the css-mode chunk in syntax="...".  The problem persists
;; even if the chunk has `fundamental-mode' instead of `css-mode'.
;;
;; Bypassing the cache for `syntax-pss' by calling
;; `parse-partial-sexp' directly instead of doing ad-do-it (see
;; by-pass-chache in the code below) solves the problem for now.  It
;; does not feel like the right solution however.
;;
;; One way of temporary solving the problem is perhaps to modify
;; `mumamo-chunk-attr=' to make "" borders, but I am not sure that it
;; works and it is the wrong solution.
(defadvice syntax-ppss (around
                        mumamo-advice-syntax-ppss
                        activate
                        compile
                        )
  "Support for mumamo chunks.
For each chunk store as properties of the chunk the parse state
that is normally hold in `syntax-ppss-last' and
`syntax-ppss-cache'.

Compute the beginning parse state for a chunk this way:

- If the chunk major mode is the same as the main major mode for
  the multi major mode then parse from the beginning of the file
  to the beginning of the chunk using the main major mode.  While
  doing that jump over chunks that do not belong to the main
  major mode and cache the state at the end and beginning of the
  the main major mode chunks.

FIX-ME: implement above.  Solution?:
 (parse-partial-sexp syntax-min (1+ syntax-max) nil nil state-at-syntax-min)
Put this at next chunk's beginning.

- Otherwise set the state at the beginning of the chunk to nil.

Do here also other necessary adjustments for this."
  (let ((pos (ad-get-arg 0)))
    (unless pos (setq pos (point)))
    (let* ((chunk-at-pos (when (and (boundp 'mumamo-multi-major-mode) mumamo-multi-major-mode) (mumamo-get-existing-chunk-at pos)))
           (dump2 (and (boundp 'dump-quote-hunt)
                      dump-quote-hunt
                      (boundp 'start)
                      ;;(= 1109 start)
                      )))
      ;;(setq dump2 t)
      (setq mumamo-syntax-chunk-at-pos chunk-at-pos)
      (when dump2 (msgtrc "\npos=%s point-min=%s mumamo-syntax-ppss.chunk-at-pos=%s" pos (point-min) chunk-at-pos))
      (if chunk-at-pos
          (let* ((chunk-syntax-min (mumamo-chunk-syntax-min chunk-at-pos))
                 (chunk-major (mumamo-chunk-major-mode chunk-at-pos))
                 (syntax-ppss-last  (overlay-get chunk-at-pos 'syntax-ppss-last))
                 (syntax-ppss-cache (overlay-get chunk-at-pos 'syntax-ppss-cache))
                 (syntax-ppss-last-min  (overlay-get chunk-at-pos 'syntax-ppss-last-min))
                 (syntax-ppss-cache-min (list syntax-ppss-last-min))
                 ;; This must be fetch the same way as in syntax-ppss:
                 (syntax-begin-function (overlay-get chunk-at-pos 'syntax-begin-function))
                 (syntax-ppss-max-span (if chunk-syntax-min
                                           (/ (- pos chunk-syntax-min -2) 2)
                                         syntax-ppss-max-span))
                 (syntax-ppss-stats (let ((stats (overlay-get chunk-at-pos 'syntax-ppss-stats)))
                                      (if stats
                                          stats
                                        (default-value 'syntax-ppss-stats))))
                 (last-min-pos (or (car syntax-ppss-last-min)
                                   1))
                 )
            ;; If chunk has moved the cached values are invalid.
            (unless (= chunk-syntax-min last-min-pos)
              (setq syntax-ppss-last nil)
              (setq syntax-ppss-last-min nil)
              (setq syntax-ppss-cache nil)
              (setq syntax-ppss-cache-min nil)
              (setq syntax-ppss-stats (default-value 'syntax-ppss-stats)))
            (when dump2
              (msgtrc " get syntax-ppss-last-min=%s len=%s chunk=%s" syntax-ppss-last-min (length syntax-ppss-last-min) chunk-at-pos)
              (msgtrc " prop syntax-ppss-last-min=%s" (overlay-properties chunk-at-pos))
              (msgtrc " chunk-major=%s, %s, syntax-min=%s\n last-min=%s" chunk-major major-mode chunk-syntax-min syntax-ppss-last-min))
            ;;(setq dump2 nil)
            (when syntax-ppss-last-min
              (unless (car syntax-ppss-last-min)
                ;;(msgtrc "fix-me: emacs bug workaround, setting car of syntax-ppss-last-min")
                ;;(setcar syntax-ppss-last-min (1- chunk-syntax-min))
                (msgtrc "fix-me: emacs bug workaround, need new syntax-ppss-last-min because car is nil")
                (setq syntax-ppss-last-min nil)
                ))
            (unless syntax-ppss-last-min
              (setq syntax-ppss-last nil)
              (save-restriction
                (widen)
                (let* ((min-pos chunk-syntax-min)
                       (chunk-sub-major (mumamo-chunk-major-mode chunk-at-pos))
                       (main-major (mumamo-main-major-mode))
                       (is-main-mode-chunk (eq chunk-sub-major main-major)))
                  (when dump2 (msgtrc " min-pos=%s, is-main-mode-chunk=%s" min-pos is-main-mode-chunk))
                  (assert (and min-pos) t)
                  (setq syntax-ppss-last-min
                        (cons min-pos ;;(1- min-pos)
                              (if nil ;is-main-mode-chunk
                                  ;; Fix-me: previous chunks as a cache?
                                  (mumamo-with-major-mode-fontification main-major
                                    `(parse-partial-sexp 1 ,min-pos nil nil nil nil))
                                (parse-partial-sexp 1 1))))
                  (setq syntax-ppss-cache-min (list syntax-ppss-last-min))
                  (when dump2 (msgtrc " put syntax-ppss-last-min=%s len=%s chunk=%s" syntax-ppss-last-min (length syntax-ppss-last-min) chunk-at-pos))
                  (when dump2 (msgtrc " prop syntax-ppss-last-min=%s" (overlay-properties chunk-at-pos)))
                  (overlay-put chunk-at-pos 'syntax-ppss-last-min syntax-ppss-last-min)
                  (let ((test-syntax-ppss-last-min
                         (overlay-get chunk-at-pos 'syntax-ppss-last-min)))
                    (when dump2 (msgtrc " test syntax-ppss-last-min=%s len=%s" test-syntax-ppss-last-min (length test-syntax-ppss-last-min)))
                    (when dump2 (msgtrc " propt syntax-ppss-last-min=%s" (overlay-properties chunk-at-pos)))
                  ))))
            (when dump2 (msgtrc " here 0, syntax-ppss-last=%s" syntax-ppss-last))
            (unless syntax-ppss-last
              (setq syntax-ppss-last syntax-ppss-last-min)
              (setq syntax-ppss-cache syntax-ppss-cache-min))
            ;;(syntax-ppss pos)
            (when dump2 (msgtrc " at 1, syntax-ppss-last=%s" syntax-ppss-last))
            (when dump2 (msgtrc " at 1, syntax-ppss-cache=%s" syntax-ppss-cache))
            (let (ret-val
                  (by-pass-cache t)
                  (dump2 dump2))
              (if (not by-pass-cache)
                  (progn
                    (when dump2
                      (let ((old-ppss (cdr syntax-ppss-last))
                            (old-pos (car syntax-ppss-last)))
                        (assert (and old-pos pos) t)
                        (msgtrc "parse-partial-sexp=>%s" (parse-partial-sexp old-pos pos nil nil old-ppss))))
                    (let (dump2)
                      (setq ret-val ad-do-it)))
                (let ((old-ppss (cdr syntax-ppss-last))
                      (old-pos (car syntax-ppss-last)))
                  (when dump2
                    (msgtrc "Xparse-partial-sexp %s %s nil nil %s" old-pos pos old-ppss)
                    (let (dump2)
                      (msgtrc "ad-do-it=>%s" ad-do-it)))
                  (save-restriction
                    (widen)
                    (assert (and old-pos pos) t)
                    (when dump2
                      (msgtrc "parse-partial-sexp %s %s nil nil %s" old-pos pos old-ppss))
                    (setq ret-val (parse-partial-sexp old-pos pos nil nil old-ppss)))))
              (when dump2 (msgtrc " ==>ret-val=%s" ret-val))
              (setq ad-return-value ret-val))
            (overlay-put chunk-at-pos 'syntax-ppss-last syntax-ppss-last)
            (overlay-put chunk-at-pos 'syntax-ppss-cache syntax-ppss-cache)
            (overlay-put chunk-at-pos 'syntax-ppss-stats syntax-ppss-stats)
            )
        (setq ad-return-value ad-do-it)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; rng-valid.el support

(defvar rng-get-major-mode-chunk-function nil
  "Function to use to get major mode chunk.
It should take one argument, the position where to get the major
mode chunk.

This is to be set by multiple major mode frame works, like
mumamo.

See also `rng-valid-nxml-major-mode-chunk-function' and
`rng-end-major-mode-chunk-function'. Note that all three
variables must be set.")
(make-variable-buffer-local 'rng-get-major-mode-chunk-function)
(put 'rng-get-major-mode-chunk-function 'permanent-local t)

(defvar rng-valid-nxml-major-mode-chunk-function nil
  "Function to use to check if nxml can parse major mode chunk.
It should take one argument, the chunk.

For more info see also `rng-get-major-mode-chunk-function'.")
(make-variable-buffer-local 'rng-valid-nxml-major-mode-chunk-function)
(put 'rng-valid-nxml-major-mode-chunk-function 'permanent-local t)

(defvar rng-end-major-mode-chunk-function nil
  "Function to use to get the end of a major mode chunk.
It should take one argument, the chunk.

For more info see also `rng-get-major-mode-chunk-function'.")
(make-variable-buffer-local 'rng-end-major-mode-chunk-function)
(put 'rng-end-major-mode-chunk-function 'permanent-local t)


;; Fix-me: The solution in this defadvice is temporary. The defadvice
;; for rng-do-some-validation should be fixed instead.
(defadvice rng-mark-error (around
                           mumamo-advice-rng-mark-error
                           activate
                           compile
                           )
  (let* ((beg (ad-get-arg 1))
         (end (ad-get-arg 2))
         (ovls-beg-end (overlays-in beg end))
         (xml-parts nil)
         chunks
         )
    (dolist (ovl ovls-beg-end)
      (when (mumamo-chunk-major-mode ovl)
        (setq chunks (cons ovl chunks))))
    ;;(message "rng-mark-error advice, beg,end=%s,%s" beg end)
    ;;(message "chunks=%s" chunks)
    (if (not chunks)
        ad-do-it
      (dolist (chunk chunks)
        (when (mumamo-valid-nxml-chunk chunk)
          ;; rng-error
          (let ((part-beg (max (overlay-start chunk)
                               beg))
                (part-end (min (overlay-end chunk)
                               end)))
            ;;(message "   part-beg/end=%s/%s" part-beg part-end)
            (when (< part-beg part-end)
              (ad-set-arg 1 part-beg)
              (ad-set-arg 2 part-end)
              ad-do-it)))))))

(defadvice rng-do-some-validation-1 (around
                                     mumamo-advice-rng-do-some-validation-1
                                     activate
                                     compile
                                     )
  (if (not mumamo-multi-major-mode)
      ad-do-it
    (let (major-mode-chunk
          (point-max (1+ (buffer-size))) ;(save-restriction (widen) (point-max)))
          end-major-mode-chunk
          (limit (+ rng-validate-up-to-date-end
                    rng-validate-chunk-size))
          (remove-start rng-validate-up-to-date-end)
          (next-cache-point (+ (point) rng-state-cache-distance))
          (continue t)
          (xmltok-dtd rng-dtd)
          have-remaining-chars
          xmltok-type
          xmltok-start
          xmltok-name-colon
          xmltok-name-end
          xmltok-replacement
          xmltok-attributes
          xmltok-namespace-attributes
          xmltok-dependent-regions
          xmltok-errors
          )
      ;;(message "> > > > > enter rng-do-some-validation-1, continue-p-function=%s" continue-p-function)
      (setq have-remaining-chars (< (point) point-max))
      (when (and continue (= (point) 1))
        (let ((regions (xmltok-forward-prolog)))
          (rng-clear-overlays 1 (point))
          (while regions
            (when (eq (aref (car regions) 0) 'encoding-name)
              (rng-process-encoding-name (aref (car regions) 1)
                                         (aref (car regions) 2)))
            (setq regions (cdr regions))))
        (unless (equal rng-dtd xmltok-dtd)
          (rng-clear-conditional-region))
        (setq rng-dtd xmltok-dtd))
      (while continue
        ;; If mumamo (or something similar) is used then jump over parts
        ;; that can not be parsed by nxml-mode.
        (when (and rng-get-major-mode-chunk-function
                   rng-valid-nxml-major-mode-chunk-function
                   rng-end-major-mode-chunk-function)
          (let ((here (point))
                next-non-space-pos)
            (skip-chars-forward " \t\r\n")
            (setq next-non-space-pos (point))
            (goto-char here)
            ;;(message "here when, p=%s emmc=%s non-space=%s" (point) end-major-mode-chunk next-non-space-pos )
            (unless (and end-major-mode-chunk
                         ;; Remaining chars in this chunk?
                         (< next-non-space-pos end-major-mode-chunk))
              (setq end-major-mode-chunk nil)
              (setq major-mode-chunk (funcall rng-get-major-mode-chunk-function next-non-space-pos))
              (while (and major-mode-chunk
                          (not (funcall rng-valid-nxml-major-mode-chunk-function major-mode-chunk))
                          (< next-non-space-pos (point-max)))
                (let ((end-pos (funcall rng-end-major-mode-chunk-function major-mode-chunk)))
                  (goto-char (+ end-pos 0))
                  (setq major-mode-chunk (funcall rng-get-major-mode-chunk-function (point)))
                  ;;(message "---> here 3, point=%s, ep=%s, mm-chunk=%s" (point) end-pos major-mode-chunk)
                  )
                (setq next-non-space-pos (point))))
            ;; Stop parsing if we do not have a chunk here yet.
            ;;(message "major-mode-chunk=%s" major-mode-chunk)
            ;;(message "rng-valid-nxml-major-mode-chunk-function=%s" rng-valid-nxml-major-mode-chunk-function)
            (setq continue (and major-mode-chunk
                                (funcall rng-valid-nxml-major-mode-chunk-function major-mode-chunk)))
            ;;(unless continue (message "continue=nil, no major-mode-chunk"))
            (when continue
              ;;(message "  continue=t")
              (setq end-major-mode-chunk (funcall rng-end-major-mode-chunk-function major-mode-chunk)))))

        (when continue
          ;; Narrow since rng-forward will continue into next chunk
          ;; even if limit is at chunk end.
          (if t
              (progn
                ;;(message "before rng-forward, point=%s" (point))
                (setq have-remaining-chars (rng-forward end-major-mode-chunk))
                ;;(message "after  rng-forward, point=%s" (point))
                )
            ;; Fix-me: Validation does not work when narrowing because
            ;; some state variables values seems to be lost. Probably
            ;; looking at `rng-validate-prepare' will tell what to do.
            (save-restriction
              (when (and end-major-mode-chunk
                         (< (point-min) end-major-mode-chunk))
                (narrow-to-region (point-min) end-major-mode-chunk))
              (setq have-remaining-chars (rng-forward end-major-mode-chunk)))
            (unless (> end-major-mode-chunk (point))
              ;;(setq have-remaining-chars t)
              (goto-char end-major-mode-chunk))
            )
          ;;(message "end-major-mode-chunk=%s, rng-validate-up-to-date-end=%s" end-major-mode-chunk rng-validate-up-to-date-end)
          (setq have-remaining-chars (< (point) point-max))
          ;;(unless have-remaining-chars (message "*** here have-remaining-chars=%s, p=%s/%s" have-remaining-chars (point) point-max))
          (let ((pos (point)))
            (when end-major-mode-chunk
              ;; Fix-me: Seems like we need a new initialization (or why
              ;; do we otherwise hang without this?)
              (and (> limit end-major-mode-chunk) (setq limit end-major-mode-chunk)))
            (setq continue
                  (and have-remaining-chars
                       continue
                       (or (< pos limit)
                           (and continue-p-function
                                (funcall continue-p-function)
                                (setq limit (+ limit rng-validate-chunk-size))
                                t))))
            ;;(unless continue (message "continue=nil, why?: %s<%s, %s" pos limit (when continue-p-function (funcall continue-p-function))))
            (cond ((and rng-conditional-up-to-date-start
                        ;; > because we are getting the state from (1- pos)
                        (> pos rng-conditional-up-to-date-start)
                        (< pos rng-conditional-up-to-date-end)
                        (rng-state-matches-current (get-text-property (1- pos)
                                                                      'rng-state)))
                   (when (< remove-start (1- pos))
                     (rng-clear-cached-state remove-start (1- pos)))
                   ;; sync up with cached validation state
                   (setq continue nil)
                   ;; do this before settting rng-validate-up-to-date-end
                   ;; in case we get a quit
                   (rng-mark-xmltok-errors)
                   (rng-mark-xmltok-dependent-regions)
                   (setq rng-validate-up-to-date-end
                         (marker-position rng-conditional-up-to-date-end))
                   (rng-clear-conditional-region)
                   (setq have-remaining-chars
                         (< rng-validate-up-to-date-end point-max))
                   ;;(unless have-remaining-chars (message "have-remaining-chars=%s rng-validate-up-to-date-end=%s, point-max=%s" have-remaining-chars rng-validate-up-to-date-end point-max))
                   )
                  ((or (>= pos next-cache-point)
                       (not continue))
                   (setq next-cache-point (+ pos rng-state-cache-distance))
                   (rng-clear-cached-state remove-start pos)
                   (when have-remaining-chars
                     ;;(message "rng-cach-state (1- %s)" pos)
                     (rng-cache-state (1- pos)))
                   (setq remove-start pos)
                   (unless continue
                     ;; if we have just blank chars skip to the end
                     (when have-remaining-chars
                       (skip-chars-forward " \t\r\n")
                       (when (= (point) point-max)
                         (rng-clear-overlays pos (point))
                         (rng-clear-cached-state pos (point))
                         (setq have-remaining-chars nil)
                         ;;(message "have-remaining-chars => nil, cause (point) = point-max")
                         (setq pos (point))))
                     (when (not have-remaining-chars)
                       (rng-process-end-document))
                     (rng-mark-xmltok-errors)
                     (rng-mark-xmltok-dependent-regions)
                     (setq rng-validate-up-to-date-end pos)
                     (when rng-conditional-up-to-date-end
                       (cond ((<= rng-conditional-up-to-date-end pos)
                              (rng-clear-conditional-region))
                             ((< rng-conditional-up-to-date-start pos)
                              (set-marker rng-conditional-up-to-date-start
                                          pos))))))))))
      ;;(message "--- exit rng-do-some-validation-1, have-remaining-chars=%s" have-remaining-chars)
      (setq have-remaining-chars (< (point) point-max))
      have-remaining-chars)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; xmltok.el

;; This advice only prevents adding nxml/rng-valid errors in non-xml
;; chunks. Doing more seems like a very big job - unless Emacs gets a
;; narrow-to-multiple-regions function!
(defadvice xmltok-add-error (around
                             mumamo-advice-xmltok-add-error
                             activate
                             compile
                             )
  (if (not mumamo-multi-major-mode)
      ad-do-it
    (when (let* ((start (or start xmltok-start))
                 (end (or end (point)))
                 ;; Fix-me: this is too slow - or was that really the
                 ;; problem???
                 ;;(chunk (mumamo-get-existing-chunk-at (if start start end)))
                 )
            (mumamo-valid-nxml-point (if start start end))
            ;;(when chunk (mumamo-valid-nxml-chunk chunk))
            )
      ;;(message "xmltok-add-error start=%s, end=%s" start end)
      (setq xmltok-errors
            (cons (xmltok-make-error message
                                     (or start xmltok-start)
                                     (or end (point)))
                  xmltok-errors)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Maybe activate advices

;; Fix-me: This assumes there are no other advices on these functions.
(if t
    (progn
      (ad-activate 'syntax-ppss)
      (ad-activate 'syntax-ppss-flush-cache)
      (ad-activate 'syntax-ppss-stats)
      (ad-activate 'rng-do-some-validation-1)
      (ad-activate 'rng-mark-error)
      (ad-activate 'xmltok-add-error)
      )
  (ad-deactivate 'syntax-ppss)
  (ad-deactivate 'syntax-ppss-flush-cache)
  (ad-deactivate 'syntax-ppss-stats)
  (ad-deactivate 'rng-do-some-validation-1)
  (ad-deactivate 'rng-mark-error)
  (ad-deactivate 'xmltok-add-error)
  )

(font-lock-add-keywords
 'emacs-lisp-mode
 '(("\\<define-mumamo-multi-major-mode\\>" . font-lock-keyword-face)))

(provide 'mumamo)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; mumamo.el ends here
