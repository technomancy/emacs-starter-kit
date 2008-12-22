;;; winsize.el --- Interactive window structure editing
;;
;; Author: Lennart Borgman <lennart dot borgman at gmail dot com >
;; Maintainer:
;; Created: Wed Dec 07 15:35:09 2005
;; Version: 0.97
;; Lxast-Updated: Sun Nov 18 02:14:52 2007 (3600 +0100)
;; Keywords:
;; Compatibility:
;;
;; Fxeatures that might be required by this library:
;;
;;   None
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; This file contains functions for interactive resizing of Emacs
;; windows.  To use it put it in your `load-path' and add the following
;; to your .emacs:
;;
;;     (require 'winsize)
;;     (global-set-key [(control x) ?+] 'resize-windows)
;;
;; For more information see `resize-windows'.
;;
;; These functions are a slightly rewritten version of the second part
;; of the second part my proposal for a new `balance-windows' function
;; for Emacs 22.  The rewrite is mostly a restructure to more easily
;; add new functions.  All functions and variables have been renamed.
;; The file was originally named bw-interactive.el.
;;
;; New ideas for functionality have been to a large part adopted from
;; the Emacs Devel mailing list.  Probably most of them originated from
;; Drew Adams and Bastien.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change log:
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; TODO: Change mouse pointer shape during resizing.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:


;;; Keymap, interactive functions etc

(require 'winsav nil t)

(defvar winsize-keymap nil
  "Keymap used by `resize-windows'.")

(defun winsize-make-keymap (let-me-use)
  "Build the keymap that should be used by `winsize-keymap'."
  (let ((map (make-sparse-keymap "Window Resizing")))
    (when (featurep 'winsav)
      (define-key map [menu-bar bw rotate]
        '("Rotate window configuration" . winsav-rotate))
      (define-key map [menu-bar bw sep3] '(menu-item "--")))
    (define-key map [menu-bar bw]
      (cons "Resize" (make-sparse-keymap "second")))
    (define-key map [menu-bar bw save-config]
      '("Save window configuration" . winsize-save-window-configuration))
    (define-key map [menu-bar bw next-config]
      '("Next saved window configuration" . winsize-next-window-configuration))
    (define-key map [menu-bar bw prev-config]
      '("Previous saved window configuration" . winsize-previous-window-configuration))
    (define-key map [menu-bar bw sep2] '(menu-item "--"))
    (define-key map [menu-bar bw fit]
      '("Fit Window to Buffer" . fit-window-to-buffer))
    (define-key map [menu-bar bw shrink]
      '("Shrink Window to Buffer" . shrink-window-if-larger-than-buffer))
    (define-key map [menu-bar bw sep1] '(menu-item "--"))
    (define-key map [menu-bar bw siblings]
      '("Balance Window Siblings" . winsize-balance-siblings))
    (define-key map [menu-bar bw balance]
      '("Balance Windows" . balance-windows))

    (when (featurep 'winsav)
      (define-key map [?|] 'winsav-rotate))
    (define-key map [?+] 'balance-windows)
    (define-key map [?.] 'winsize-balance-siblings)
    (define-key map [?=] 'fit-window-to-buffer)
    (define-key map [?-] 'shrink-window-if-larger-than-buffer)

    (define-key map [(up)]    'winsize-move-border-up)
    (define-key map [(down)]  'winsize-move-border-down)
    (define-key map [(left)]  'winsize-move-border-left)
    (define-key map [(right)] 'winsize-move-border-right)

    (define-key map [(shift up)]    'winsize-move-other-border-up)
    (define-key map [(shift down)]  'winsize-move-other-border-down)
    (define-key map [(shift left)]  'winsize-move-other-border-left)
    (define-key map [(shift right)] 'winsize-move-other-border-right)

    (define-key map [(meta left)]   'winsize-to-border-or-window-left)
    (define-key map [(meta up)]     'winsize-to-border-or-window-up)
    (define-key map [(meta right)]  'winsize-to-border-or-window-right)
    (define-key map [(meta down)]   'winsize-to-border-or-window-down)

    (define-key map [?0] 'delete-window)
    (define-key map [?1] 'delete-other-windows)
    (define-key map [?2] 'split-window-vertically)
    (define-key map [?3] 'split-window-horizontally)
    (define-key map [?4] 'other-window)

    (define-key map [?!] 'winsize-save-window-configuration)
    (define-key map [?>] 'winsize-next-window-configuration)
    (define-key map [?<] 'winsize-previous-window-configuration)

    ;; Fix-me: These keys could also be set to nil
    (define-key map [mouse-1]                        'mouse-set-point)
    ;;(define-key map [down-mouse-1]                   'mouse-set-point)
    (define-key map [(mode-line) (down-mouse-1)]     'mouse-drag-mode-line)
    (define-key map [(vertical-line) (down-mouse-1)] 'mouse-drag-vertical-line)
    (define-key map [(vertical-scroll-bar) (mouse-1)] 'scroll-bar-toolkit-scroll)

    (define-key map [??] 'winsize-help)
    (define-key map [(control ?g)]     'winsize-quit)
    (define-key map [(control return)] 'winsize-stop-go-back)
    (define-key map [(return)]         'winsize-stop)
    (define-key map [t]                'winsize-stop-and-execute)

    (dolist (ks let-me-use)
      (if (and (not (vectorp ks))
               (not (stringp ks))
               (commandp ks))
          (let ((ks-list (where-is-internal ks)))
            (dolist (ks ks-list)
              (unless (lookup-key map ks)
                (define-key map ks nil))))
        (unless (lookup-key map ks)
          (define-key map ks nil))))

    (setq winsize-keymap map)))

(defun winsize-pre-command ()
  "Do this before every command.
Runs this in `pre-command-hook'.

Remember the currently used border sides for resizing. Also
remember position in message buffer to be able to see if next
command outputs some message.

For more information see `winsize-post-command'."
  (setq winsize-message-end (winsize-message-end))
  (setq winsize-border-hor (winsize-border-used-hor))
  (setq winsize-border-ver (winsize-border-used-ver)))

(defun winsize-post-command ()
  "Done after every command.
Run this in `post-command-hook'.

Check the border sides \(left/right, up/down) remembered in
`winsize-pre-command' and use the the same side if possible,
otherwise the opposite side if that is possible. \(This check is
of course not done if the last command changed the border side.)

The reason for selecting borders this way is to try to give the
user a coherent and easy picture of what is going on when
changing window or when window structure is changed.  \(Note that
the commands moving to another window or changing the window
structure does not have to belong to this package. Those commands
can therefore not select the border sides.)

Give the user feedback about selected window and borders.  Also
give a short help message unless last command gave some message."
  (unless juris-way
    (unless winsize-border-hor
      (winsize-select-initial-border-hor))
    (when winsize-border-hor
      (winsize-set-border winsize-border-hor t))
    (unless winsize-border-ver
     (winsize-select-initial-border-ver))
    (when winsize-border-ver
      (winsize-set-border winsize-border-ver t)))
  (winsize-tell-user))

;;;###autoload
(defun resize-windows ()
  "Start window resizing.
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
   window. \(You may however also use `other-window' or click
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
  then \(with default settings) the borders marked with 'v' and
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
  border that could be moved to the left of this window \(which
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
"
  (interactive)
  (setq winsize-resizing t)
  ;; Save old values:
  (unless winsize-old-mouse-avoidance-mode
    (setq winsize-old-mouse-avoidance-mode mouse-avoidance-mode))
  ;; Setup user feedback things:
  (mouse-avoidance-mode 'none)
  (winsize-set-mode-line-colors t)
  (winsize-create-short-help-message)
  (setq winsize-message-end (winsize-message-end))
  ;; Save config for exiting:
  (setq winsize-window-config-init (current-window-configuration))
  (setq winsize-window-at-entry (selected-window))
  (setq winsize-frame (selected-frame))
  ;; Setup keymap and command hooks etc:
  (winsize-setup-local-map)
  (winsize-add-command-hooks)
  (setq winsize-window-for-side-hor nil)
  (setq winsize-window-for-side-ver nil))


(defun winsize-setup-local-map ()
  "Setup an overriding keymap and use this during resizing.
Save current keymaps."
  ;; Fix-me: use copy-keymap for old?
  (unless winsize-old-overriding-terminal-local-map
    (setq winsize-old-overriding-terminal-local-map overriding-terminal-local-map))
  (setq overriding-terminal-local-map (copy-keymap winsize-keymap))
  (setq winsize-old-overriding-local-map-menu-flag overriding-local-map-menu-flag)
  (setq overriding-local-map-menu-flag t))

(defun winsize-restore-local-map ()
  "Restore keymaps saved by `winsize-setup-local-map'."
  (setq overriding-terminal-local-map winsize-old-overriding-terminal-local-map)
  (setq winsize-old-overriding-terminal-local-map nil)
  (setq overriding-local-map-menu-flag winsize-old-overriding-local-map-menu-flag)
  (setq winsize-old-overriding-local-map-menu-flag nil))


(defvar winsize-window-config-init nil
  "Hold window configuration from resizing start.")

(defvar winsize-window-config-help nil
  "Hold window configuration when help is shown.")

(defvar winsize-window-config-init-help nil
  "Hold window configuration from resizing start during help.")

(defun winsize-restore-after-help (buffer)
  "Restore window configuration after help.
Raise frame and reactivate resizing."
  (remove-hook 'temp-buffer-setup-hook 'winsize-help-mode-hook-function)
  (setq temp-buffer-show-function winsize-old-temp-buffer-show-function)
  ;; Get rid of the view exit action and the extra text in the help
  ;; buffer:
  (with-current-buffer (help-buffer)
    (setq view-exit-action winsize-old-view-exit-action)
    (setq winsize-old-view-exit-action nil)
    (let ((here (point-marker))
          (inhibit-read-only t))
      (goto-char (point-min))
      (forward-line 2)
      (delete-region (point-min) (point))
      (goto-char (point-max))
      (forward-line -2)
      (delete-region (point) (point-max))
      (goto-char here)))
  ;; Restart resizing, restoring window configurations:
  (when (select-frame winsize-help-frame)
    (raise-frame)
    (set-window-configuration winsize-window-config-help)
    (resize-windows)
    (setq winsize-window-config-init winsize-window-config-init-help)))

(defvar winsize-help-frame nil
  "The frame from which help was called.")

(defun winsize-help-mode-hook-function ()
  "Setup temp buffer show function to only run second step.
The first step, `winsize-temp-buffer-show-function', has already been run."
  (setq temp-buffer-show-function 'winsize-temp-buffer-show-function-1))

(defun winsize-temp-buffer-show-function (buffer)
  "First step of setup for showing help during resizing.
This step is run when showing help during resizing.

Save window configuration etc to be able to resume resizing. Stop
resizing. Delete other windows.

Run second step (`winsize-temp-buffer-show-function-1') and
arrange so that second step is run when following help links."
  (setq winsize-window-config-help (current-window-configuration))
  (setq winsize-window-config-init-help winsize-window-config-init)
  (setq winsize-help-frame (selected-frame))
  (winsize-stop)
  (delete-other-windows)
  (winsize-temp-buffer-show-function-1 buffer)
  (add-hook 'temp-buffer-setup-hook 'winsize-help-mode-hook-function))

(defun winsize-temp-buffer-show-function-1 (buffer)
  "Second step of setup for showing help during resizing.
This is run after the first step when accessing help during
resizing. It is also when following help links."
  (with-current-buffer buffer
    (let ((inhibit-read-only t)
          (buffer-read-only t) ;; It is reverted in `help-mode-finish'
          )
      (run-hooks 'temp-buffer-show-hook))
    (let ((here (point-marker))
          (str "*** Type q to return to window resizing ***"))
      (put-text-property 0 (length str) 'face 'highlight str)
      (goto-char (point-min))
      (insert str "\n\n")
      (goto-char (point-max))
      (insert "\n\n" str)
      (goto-char here)
      (setq buffer-read-only t))
    (unless winsize-old-view-exit-action
      (setq winsize-old-view-exit-action view-exit-action)
      (setq view-exit-action 'winsize-restore-after-help)))
  (set-window-buffer (selected-window) buffer)
  (message "Type q to return to window resizing"))

(defun winsize-help ()
  "Give help during resizing.
Save current window configuration and pause resizing."
  (interactive)
  (if pop-up-frames
      (progn
        (winsize-exit-resizing nil)
        (describe-function 'resize-windows))
    ;; Fix-me: move setup of view-exit-action etc here. Or was it
    ;; temp-buffer-show-function?
    ;; Setup help hooks etc:
    (unless (or winsize-old-temp-buffer-show-function
                ;; These things should not happen... :
                (eq temp-buffer-show-function 'winsize-temp-buffer-show-function)
                (eq temp-buffer-show-function 'winsize-temp-buffer-show-function-1))
      (setq winsize-old-temp-buffer-show-function temp-buffer-show-function))
    (setq temp-buffer-show-function 'winsize-temp-buffer-show-function)
    (with-output-to-temp-buffer (help-buffer)
      (with-current-buffer (help-buffer)
        (insert "resize-windows is ")
        (describe-function-1 'resize-windows)))))

(defun winsize-quit ()
  "Quit resing, restore window configuration at start."
  (interactive)
  (set-window-configuration winsize-window-config-init)
  (winsize-exit-resizing nil))

(defun winsize-stop-go-back ()
  "Exit window resizing.  Go back to the window started in."
  (interactive)
  (winsize-exit-resizing nil t))

(defun winsize-stop-and-execute ()
  "Exit window resizing and put last key on the input queue.
Select the window marked during resizing before putting back the
last key."
  ;; Fix-me: maybe replace this with a check of this-command in
  ;; post-command-hook instead?
  (interactive)
  (winsize-exit-resizing t))

(defun winsize-stop ()
  "Exit window resizing.
Select the window marked during resizing."
  (interactive)
  (winsize-exit-resizing nil))

;;;###autoload
(defun winsize-balance-siblings ()
  "Make current window siblings the same height or width.
It works the same way as `balance-windows', but only for the
current window and its siblings."
  (interactive)
  (balance-windows (selected-window)))

(defun winsize-to-border-or-window-left ()
  "Switch to border leftwards, maybe moving to next window.
If already at the left border, then move to left window, the same
way `windmove-left' does."
  (interactive) (winsize-switch-border 'left t))

(defun winsize-to-border-or-window-right ()
  "Switch to border rightwards, maybe moving to next window.
For more information see `winsize-to-border-or-window-left'."
  (interactive) (winsize-switch-border 'right t))

(defun winsize-to-border-or-window-up ()
  "Switch to border upwards, maybe moving to next window.
For more information see `winsize-to-border-or-window-left'."
  (interactive) (winsize-switch-border 'up t))

(defun winsize-to-border-or-window-down ()
  "Switch to border downwards, maybe moving to next window.
For more information see `winsize-to-border-or-window-left'."
  (interactive) (winsize-switch-border 'down t))


(defun winsize-move-border-left ()
  "Move border left, but select border first if not done."
  (interactive) (winsize-resize 'left nil))

(defun winsize-move-border-right ()
  "Move border right, but select border first if not done."
  (interactive) (winsize-resize 'right nil))

(defun winsize-move-border-up ()
  "Move border up, but select border first if not done."
  (interactive) (winsize-resize 'up nil))

(defun winsize-move-border-down ()
  "Move border down, but select border first if not done."
  (interactive) (winsize-resize 'down nil))


(defun winsize-move-other-border-left ()
  "Move border left, but select border first if not done."
  (interactive) (winsize-resize 'left t))

(defun winsize-move-other-border-right ()
  "Move border right, but select border first if not done."
  (interactive) (winsize-resize 'right t))

(defun winsize-move-other-border-up ()
  "Move border up, but select border first if not done."
  (interactive) (winsize-resize 'up t))

(defun winsize-move-other-border-down ()
  "Move border down, but select border first if not done."
  (interactive) (winsize-resize 'down t))


;;; Custom variables

(defcustom winsize-autoselect-borders t
  "Determines how borders are selected by default.
If nil hever select borders automatically (but keep them on the
same side while changing window).  If 'when-single select border
automatically if there is only one possible choice.  If t alwasy
select borders automatically if they are not selected."
  :type '(choice (const :tag "Always" t)
                 (const :tag "When only one possbility" when-single)
                 (const :tag "Never" nil))
  :group 'winsize)

(defcustom winsize-mode-line-colors (list t (list "green" "green4"))
  "Mode line colors used during resizing."
  :type '(list (boolean :tag "Enable mode line color changes during resizing")
               (list
                (color :tag "- Active window mode line color")
                (color :tag "- Inactive window mode line color")))
  :group 'winsize)

(defcustom winsize-mark-selected-window t
  "Mark selected window if non-nil."
  :type 'boolean
  :group 'winsize)

(defcustom winsize-make-mouse-prominent t
  "Try to make mouse more visible during resizing.
The mouse is positioned next to the borders that you can move.
It can however be hard to see if where it is.  Setting this to on
makes the mouse jump a few times."
  :type 'boolean
  :group 'winsize)

(defvar widget-command-prompt-value-history nil
  "History of input to `widget-function-prompt-value'.")

(define-widget 'command 'restricted-sexp
  "A Lisp function."
  :complete-function (lambda ()
                       (interactive)
                       (lisp-complete-symbol 'commandp))
  :prompt-value 'widget-field-prompt-value
  :prompt-internal 'widget-symbol-prompt-internal
  :prompt-match 'commandp
  :prompt-history 'widget-command-prompt-value-history
  :action 'widget-field-action
  :match-alternatives '(commandp)
  :validate (lambda (widget)
              (unless (commandp (widget-value widget))
                (widget-put widget :error (format "Invalid command: %S"
                                                  (widget-value widget)))
                widget))
  :value 'ignore
  :tag "Command")

(defcustom winsize-let-me-use '(next-line ;;[(control ?n)]
                                previous-line ;;[(control ?p)]
                                forward-char ;;[(control ?f)]
                                backward-char ;;[(control ?b)]
                                [(home)]
                                [(end)]
                                ;; Fix-me: replace this with something
                                ;; pulling in help-event-list:
                                [(f1)]
                                execute-extended-command
                                eval-expression)
  "Key sequences or commands that should not be overriden during resize.
The purpose is to make it easier to switch windows.  The functions
`windmove-left' etc depends on the position when chosing the
window to move to."
  :type '(repeat
          (choice
           ;; Note: key-sequence must be before command here, since
           ;; the key sequences seems to match command too.
           key-sequence command))
  :set (lambda (sym val)
         (set-default sym val)
         (winsize-make-keymap val))
  :group 'winsize)

(defcustom winsize-selected-window-face 'winsize-selected-window-face
  "Variable holding face for marking selected window.
This variable may be nil or a face symbol."
  :type '(choice (const :tag "Do not mark selected window" nil)
                 face)
  :group 'winsize)

(defface winsize-selected-window-face
  '((t (:inherit secondary-selection)))
  "Face for marking selected window."
  :group 'winsize)

;;; Internals

;; These variables all holds values to be reset when exiting resizing:
(defvar winsize-old-mode-line-bg nil)
(defvar winsize-old-mode-line-inactive-bg nil)
(defvar winsize-old-overriding-terminal-local-map nil)
(defvar winsize-old-overriding-local-map-menu-flag nil)
(defvar winsize-old-temp-buffer-show-function nil)
(defvar winsize-old-mouse-avoidance-mode nil
  "Hold the value of `mouse-avoidance-mode' at resizing start.")
(defvar winsize-old-view-exit-action nil)
(make-variable-buffer-local 'winsize-old-view-exit-action)


(defvar winsize-resizing nil
  "t during resizing, nil otherwise.")

(defvar winsize-window-at-entry nil
  "Window that was selected when `resize-windows' started.")

(defvar winsize-frame nil
  "Frame that `resize-windows' is operating on.")

(defun winsize-exit-resizing (put-back-last-event &optional stay)
  "Stop window resizing.
Put back mode line colors and keymaps that were changed.

Upon exit first select window.  If STAY is non-nil then select
the window which was selected when `resize-windows' was called,
otherwise select the last window used during resizing.  After
that, if PUT-BACK-LAST-EVENT is non-nil, put back the last input
event on the input queue."
  (setq winsize-resizing nil)
  ;; Reset user feedback things:
  (mouse-avoidance-mode winsize-old-mouse-avoidance-mode)
  (setq winsize-old-mouse-avoidance-mode nil)
  (winsize-set-mode-line-colors nil)
  (winsize-mark-selected-window nil)
  ;; Remove all hooks etc for help:
  (if (or (eq winsize-old-temp-buffer-show-function 'winsize-temp-buffer-show-function)
          (eq winsize-old-temp-buffer-show-function 'winsize-temp-buffer-show-function-1))
      (setq temp-buffer-show-function nil)
    (setq temp-buffer-show-function winsize-old-temp-buffer-show-function))
  (setq winsize-old-temp-buffer-show-function nil)
  (remove-hook 'help-mode-hook 'winsize-help-mode-hook-function)
  (remove-hook 'temp-buffer-setup-hook 'winsize-help-mode-hook-function)
  ;; Restore keymap and command hooks:
  (winsize-restore-local-map)
  (winsize-remove-command-hooks)
  ;; Exit:
  (when stay (select-window winsize-window-at-entry))
  (message "Exited window resizing")
  (when (and put-back-last-event)
    ;; Add this to the input queue again:
    (isearch-unread last-command-event)))

(defun winsize-add-command-hooks ()
  (add-hook 'pre-command-hook 'winsize-pre-command)
  (add-hook 'post-command-hook 'winsize-post-command))

(defun winsize-remove-command-hooks ()
  (remove-hook 'pre-command-hook 'winsize-pre-command)
  (remove-hook 'post-command-hook 'winsize-post-command))


;;; Borders

(defvar winsize-window-for-side-hor nil
  "Window used internally for resizing in vertical direction.")

(defvar winsize-window-for-side-ver nil
  "Window used internally for resizing in horizontal direction.")

(defvar winsize-border-hor nil
  "Use internally to remember border choice.
This is set by `winsize-pre-command' and checked by
`winsize-post-command', see the latter for more information.

The value should be either nil, 'left or 'right.")

(defvar winsize-border-ver nil
  "Use internally to remember border choice.
This is set by `winsize-pre-command' and checked by
`winsize-post-command', see the latter for more information.

The value should be either nil, 'up or 'down.")

(defun winsize-border-used-hor ()
  "Return the border side used for horizontal resizing."
  (let ((hor (when winsize-window-for-side-hor
               (if (eq (selected-window) winsize-window-for-side-hor)
                   'right
                 'left))))
    hor))

(defun winsize-border-used-ver ()
  "Return the border side used for vertical resizing."
  (let ((ver (when winsize-window-for-side-ver
               (if (eq (selected-window) winsize-window-for-side-ver)
                   'down
                 'up))))
    ver))

(defun winsize-switch-border (dir allow-windmove)
  "Switch border that is beeing resized.
Switch to border in direction DIR.  If ALLOW-WINDMOVE is non-nil
then change window if necessary, otherwise stay and do not change
border."
  (let* ((window-in-that-dir (windmove-find-other-window
                              dir nil (selected-window))))
    (when (window-minibuffer-p window-in-that-dir)
      (setq window-in-that-dir nil))
    (if juris-way
        (if (not window-in-that-dir)
            (message "No window in that direction")
          (windmove-do-window-select dir nil))
      (if (not window-in-that-dir)
          (message "No window or border in that direction")
        (let* ((is-hor (memq dir '(left right)))
               (border-used (if is-hor
                                (winsize-border-used-hor)
                              (winsize-border-used-ver)))
               (using-dir-border (eq dir border-used)))
          (if using-dir-border
              (when allow-windmove
                (setq winsize-window-for-side-hor nil)
                (setq winsize-window-for-side-ver nil)
                (windmove-do-window-select dir nil)
                (message "Moved to new window"))
            (winsize-select-border dir)
            (message "Switched to border %swards" dir)))))))


(defun winsize-select-initial-border-hor ()
  "Select a default border horizontally."
  (if juris-way
      (winsize-set-border 'right t)
    (let ((has-left  (winsize-window-beside (selected-window) 'left))
          (has-right (winsize-window-beside (selected-window) 'right)))
      (cond
       ((not winsize-autoselect-borders) t)
       ((eq winsize-autoselect-borders 'when-single)
        (when (= 1 (length (delq nil (list has-left has-right))))
          (winsize-select-border 'right)))
       (t
        (winsize-select-border 'right))))))

(defun winsize-select-initial-border-ver ()
  "Select a default border vertically."
  (if juris-way
      (winsize-set-border 'up t)
    (let ((has-up  (winsize-window-beside (selected-window) 'up))
          (has-down (winsize-window-beside (selected-window) 'down)))
      (cond
       ((not winsize-autoselect-borders) t)
       ((eq winsize-autoselect-borders 'when-single)
        (when (= 1 (length (delq nil (list has-up has-down))))
          (winsize-select-border 'up)))
       (t
        (winsize-select-border 'up))))))

(defun winsize-select-border (dir)
  "Select border to be set for resizing.
The actually setting is done in `post-command-hook'."
  (cond
   ((memq dir '(left right))
    (setq winsize-border-hor dir))
   ((memq dir '(up down))
    (setq winsize-border-ver dir))
   (t (error "Bad DIR=%s" dir))))

(defun winsize-set-border (dir allow-other-side)
  "Set border for resizing."
  (let ((window-beside (winsize-window-beside (selected-window) dir))
        (horizontal (memq dir '(left right))))
    (unless window-beside
      (when allow-other-side
        (setq dir (winsize-other-side dir))
        (setq window-beside
              (winsize-window-beside (selected-window) dir))))
    (if horizontal
        (progn
          (setq winsize-border-hor nil)
          (setq winsize-window-for-side-hor nil))
      (setq winsize-border-ver nil)
      (setq winsize-window-for-side-ver nil))
    (when window-beside
      (let ((window-for-side (if (memq dir '(right down))
                                 (selected-window)
                               window-beside)))
        (if horizontal
            (setq winsize-window-for-side-hor window-for-side)
          (setq winsize-window-for-side-ver window-for-side))))))

(defcustom juris-way t
  ""
  :type 'boolean
  :group 'winsize)

(defun winsize-resize (dir other-side)
  "Choose border to move.  Or if border is chosen move that border.
Used by `winsize-move-border-left' etc."
  (when juris-way
    (let ((bside (if (memq dir '(left right))
                     (if other-side 'left 'right)
                   (if other-side 'up 'down))))
      (winsize-set-border bside t)))
  (let* ((horizontal (memq dir '(left right)))
         (arg (if (memq dir '(left up)) -1 1))
         (window-for-side (if horizontal 'winsize-window-for-side-hor 'winsize-window-for-side-ver))
         (window-for-side-val (symbol-value window-for-side)))
    (if (not window-for-side-val)
        (winsize-select-border dir)
      (when (and winsize-resizing
                 (not (eq window-for-side-val 'checked)))
        (condition-case err
            (adjust-window-trailing-edge (symbol-value window-for-side) arg horizontal)
          (error (message "%s" (error-message-string err))))))))

(defun winsize-other-side (side)
  "Return other side for 'left etc, ie 'left => 'right."
  (cond
    ((eq side 'left) 'right)
    ((eq side 'right) 'left)
    ((eq side 'up) 'down)
    ((eq side 'down) 'up)
    (t (error "Invalid SIDE=%s" side))))

(defun winsize-window-beside (window side)
  "Return a window directly beside WINDOW at side SIDE.
That means one whose edge on SIDE is touching WINDOW.  SIDE
should be one of 'left, 'up, 'right and 'down."
  (require 'windmove)
  (let* ((windmove-wrap-around nil)
         (win (windmove-find-other-window side nil window)))
    (unless (window-minibuffer-p win)
      win)))


;;; Window configs

(defconst winsize-window-configuration-ring (make-ring 20)
  "Hold window configurations.")

(defun winsize-ring-rotate (ring forward)
  (when (< 1 (ring-length ring))
    (if forward
        (ring-insert ring (ring-remove ring nil))
      (ring-insert-at-beginning ring (ring-remove ring 0)))))

(defun winsize-ring-index (ring elem)
  (let ((memb (member elem (ring-elements ring))))
    (when memb
      (- (ring-length ring)
         (length memb)))))

(defun winsize-previous-window-configuration ()
  (interactive)
  (winsize-goto-window-configuration nil))

(defun winsize-next-window-configuration ()
  (interactive)
  (winsize-goto-window-configuration t))

(defun winsize-goto-window-configuration (forward)
  (let* ((curr-conf (current-window-configuration))
         (ring winsize-window-configuration-ring)
         (idx (winsize-ring-index ring curr-conf)))
    (if idx
        (progn
          (setq idx (if forward (1- idx) (1+ idx)))
          (set-window-configuration (ring-ref ring idx)))
      ;; Unfortunately idx often seems to be nil so we will have to
      ;; rotate the ring (or something similar).
      (winsize-ring-rotate ring forward)
      (set-window-configuration (ring-ref ring 0)))))

;;;###autoload
(defun winsize-save-window-configuration ()
  (interactive)
  (let* ((curr-conf (current-window-configuration))
         (ring winsize-window-configuration-ring))
    (if (winsize-ring-index ring curr-conf)
        (error "Current configuration was already stored")
      (ring-insert ring curr-conf)
      (message "Saved window config, use '<' or '>' to get it back"))))


;;; User feedback

(defun winsize-set-mode-line-colors (on)
  "Turn mode line colors on if ON is non-nil, otherwise off."
  (if on
      (progn
        (unless winsize-old-mode-line-inactive-bg
          (setq winsize-old-mode-line-inactive-bg (face-attribute 'mode-line-inactive :background)))
        (unless winsize-old-mode-line-bg
          (setq winsize-old-mode-line-bg (face-attribute 'mode-line :background)))
        (let* ((use-colors (car winsize-mode-line-colors))
               (colors (cadr winsize-mode-line-colors))
               (active-color (elt colors 0))
               (inactive-color (elt colors 1)))
          (when use-colors
            (set-face-attribute 'mode-line-inactive nil :background inactive-color)
            (set-face-attribute 'mode-line nil :background active-color))))
    (set-face-attribute 'mode-line-inactive nil :background winsize-old-mode-line-inactive-bg)
    (setq winsize-old-mode-line-inactive-bg nil)
    (set-face-attribute 'mode-line nil :background winsize-old-mode-line-bg)
    (setq winsize-old-mode-line-bg nil)))

(defvar winsize-short-help-message nil
  "Short help message shown in echo area.")

(defun winsize-create-short-help-message ()
  "Create short help message to show in echo area."
  (let ((msg ""))
    (mapc (lambda (rec)
            (let ((fun (elt rec 0))
                  (desc (elt rec 1))
                  (etc (elt rec 2)))
              (when (< 0 (length msg))
                (setq msg (concat msg ", ")))
              (setq msg (concat msg
                                desc
                                ":"
                                (key-description
                                 (where-is-internal fun winsize-keymap t))
                                (if etc " etc" "")))))
          '(
            (balance-windows "balance" nil)
            (winsize-move-border-left "resize" t)
            (winsize-to-border-or-window-left "border" nil)
            ))
    (setq msg (concat msg ", exit:RET, help:?"))
    (setq winsize-short-help-message msg)))

(defun winsize-move-mouse-to-resized ()
  "Move mouse to show which border(s) are beeing moved."
  (let* ((edges (window-edges (selected-window)))
         (L (nth 0 edges))
         (T (nth 1 edges))
         (R (nth 2 edges))
         (B (nth 3 edges))
         (x (/ (+ L R) 2))
         (y (/ (+ T B) 2)))
    (when (and winsize-window-for-side-hor
               (not (eq winsize-window-for-side-hor 'checked)))
      (setq x (if (eq (selected-window) winsize-window-for-side-hor) (- R 6) (+ L 2))))
    (when (and winsize-window-for-side-ver
               (not (eq winsize-window-for-side-ver 'checked)))
      (setq y (if (eq (selected-window) winsize-window-for-side-ver) (- B 2) (+ T 0))))
    (set-mouse-position (selected-frame) x y)))

(defvar winsize-selected-window-overlay nil)

(defun winsize-mark-selected-window (active)
  (when winsize-selected-window-overlay
    (delete-overlay winsize-selected-window-overlay)
    (setq winsize-selected-window-overlay nil))
  (when active
    (with-current-buffer (window-buffer (selected-window))
      (let ((ovl (make-overlay (point-min) (point-max))))
        (setq winsize-selected-window-overlay ovl)
        (overlay-put ovl 'window (selected-window))
        (overlay-put ovl 'pointer 'arrow)
        (overlay-put ovl 'priority 1000)
        (when winsize-selected-window-face
          (overlay-put ovl 'face winsize-selected-window-face))))))

(defvar winsize-message-end nil
  "Marker, maybe at end of message buffer.")

(defun winsize-message-end ()
  "Return a marker at the end of the message buffer."
  (with-current-buffer (get-buffer-create "*Messages*")
    (point-max-marker)))

(defvar winsize-move-mouse 1)

(defun winsize-move-mouse ()
  ;;(setq winsize-move-mouse (- winsize-move-mouse))
  (let* ((fxy (mouse-pixel-position))
         (f (car fxy))
         (x (cadr fxy))
         (y (cddr fxy))
         (m (mod winsize-move-mouse 2))
         (d (* (if (= 0 m) 1 -1) 1)))
    (set-mouse-pixel-position f (+ d x) (+ d y))
    (when (< 1 winsize-move-mouse)
      (setq winsize-move-mouse (1- winsize-move-mouse))
      (setq winsize-make-mouse-prominent-timer
            (run-with-timer 0.2 nil 'winsize-move-mouse)))))

(defvar winsize-make-mouse-prominent-timer nil)

(defun winsize-make-mouse-prominent-f (doit)
  (when (and winsize-make-mouse-prominent-timer
             (timerp winsize-make-mouse-prominent-timer))
    (cancel-timer winsize-make-mouse-prominent-timer))
  (when doit
    (setq winsize-move-mouse 3)
    (setq winsize-make-mouse-prominent-timer
          (run-with-idle-timer 0.1 nil 'winsize-move-mouse))))

(defun winsize-tell-user ()
  "Give the user feedback."
  (when winsize-mark-selected-window
    (winsize-mark-selected-window t))
  (unless juris-way
    (let ((move-mouse (not (member this-command
                                   '(mouse-drag-mode-line
                                     mouse-drag-vertical-line
                                     scroll-bar-toolkit-scroll)))))
      ;;(message "%s, move-mouse=%s" this-command move-mouse);(sit-for 2)
      (when move-mouse
        (winsize-move-mouse-to-resized))
      (when winsize-make-mouse-prominent
        (winsize-make-mouse-prominent-f move-mouse))))
  (when (= winsize-message-end (winsize-message-end))
    (message "%s" winsize-short-help-message)))


(provide 'winsize)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; winsize.el ends here
