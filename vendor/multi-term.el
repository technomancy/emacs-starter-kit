;;; multi-term.el --- Managing multiple terminal buffers in Emacs.

;; Author: Andy Stewart <lazycat.manatee@gmail.com>
;; Maintainer: ahei <ahei0802@gmail.com>
;; Copyright (C) 2008, 2009, Andy Stewart, all rights reserved.
;; Copyright (C) 2010, ahei, all rights reserved.
;; Created: <2008-09-19 23:02:42>
;; Version: 0.8.8
;; Last-Updated: <2010-05-13 00:40:24 Thursday by ahei>
;; URL: http://www.emacswiki.org/emacs/download/multi-term.el
;; Keywords: term, terminal, multiple buffer
;; Compatibility: GNU Emacs 23.2.1

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.

;; Features that might be required by this library:
;;
;;  `term' `cl' `advice'
;;

;;; Commentary:
;;
;; This package is for creating and managing multiple terminal buffers in Emacs.
;;
;; By default, term.el provides a great terminal emulator in Emacs.
;; But I have some troubles with term-mode:
;;
;; 1. term.el just provides commands `term' or `ansi-term'
;;    for creating a terminal buffer.
;;    And there is no special command to create or switch
;;    between multiple terminal buffers quickly.
;;
;; 2. By default, the keystrokes of term.el conflict with global-mode keystrokes,
;;    which makes it difficult for the user to integrate term.el with Emacs.
;;
;; 3. By default, executing *NIX command “exit” from term-mode,
;;    it will leave an unused buffer.
;;
;; 4. term.el won’t quit running sub-process when you kill terminal buffer forcibly.
;;
;; 5. Haven’t a dedicated window for debug program.
;;
;; And multi-term.el is enhanced with those features.
;;

;;; Installation:
;;
;; Copy multi-term.el to your load-path and add to your ~/.emacs
;;
;;  (require 'multi-term)
;;
;; And setup program that `multi-term' will need:
;;
;; (setq multi-term-program "/bin/bash")
;;
;;      or setup like me "/bin/zsh" ;)
;;
;; Below are the commands you can use:
;;
;;      `multi-term'                    Create a new term buffer.
;;      `multi-term-next'               Switch to next term buffer.
;;      `multi-term-prev'               Switch to previous term buffer.
;;      `multi-term-dedicated-open'     Open dedicated term window.
;;      `multi-term-dedicated-close'    Close dedicated term window.
;;      `multi-term-dedicated-toggle'   Toggle dedicated term window.
;;      `multi-term-dedicated-select'   Select dedicated term window.
;;
;; Tips:
;;
;;      You can type `C-u' before command `multi-term' or `multi-term-dedicated-open'
;;      then will prompt you shell name for creating terminal buffer.
;;

;;; Customize:
;;
;; `multi-term-program' default is nil, so when creating new term buffer,
;; send environment variable of `SHELL' (`ESHELL', `/bin/sh') to `make-term'.
;;
;; And you can set it to your liking, like me: ;-)
;;
;; (setq multi-term-program "/bin/zsh")
;;
;; `multi-term-default-dir' default is `~/', only use when current buffer
;; is not in a real directory.
;;
;; `multi-term-buffer-name' is the name of term buffer.
;;
;; `multi-term-scroll-show-maximum-output' controls how interpreter
;; output causes window to scroll.
;;
;; `multi-term-scroll-to-bottom-on-output' controls whether interpreter
;; output causes window to scroll.
;;
;; `multi-term-switch-after-close' try to switch other `multi-term' buffer
;; after close current one.
;; If you don't like this feature just set it with nil.
;;
;; `term-unbind-key-list' is a key list to unbind some keystroke.
;;
;; `term-bind-key-alist' is a key alist that binds some keystroke.
;; If you don't like default, modify it.
;;
;; `multi-term-dedicated-window-height' the height of a dedicated term window.
;;
;; `multi-term-dedicated-max-window-height' the max height limit that dedicated
;; window is allowed.
;;
;; `multi-term-dedicated-skip-other-window-p' whether skip dedicated term
;; window when use command `other-window' to cycle windows order.
;;
;; All of the above can be customize by:
;;      M-x customize-group RET multi-term RET
;;

;;; Change log:
;;
;; 2009/07/04
;;      * Add new option `multi-term-dedicated-select-after-open-p'.
;;
;; 2009/06/29
;;      * Fix regexp bug.
;;
;; 2009/04/21
;;      * Fix a bug that bring at `2009/03/28':
;;        It will kill sub-process in other multi-term buffer
;;        when we kill current multi-term buffer.
;;
;; 2009/03/29
;;      * Add new command `term-send-reverse-search-history'.
;;
;; 2009/03/28
;;      * Add new option `multi-term-switch-after-close'.
;;
;; 2009/02/18
;;      * Fix bug between ECB and `multi-term-dedicated-close'.
;;
;; 2009/02/05
;;      * Prompt user shell name when type `C-u' before command
;;        `multi-term' or `multi-term-dedicated-open'.
;;      * Fix doc.
;;
;; 2009/01/29
;;      * Use `term-quit-subjob' instead `term-interrupt-subjob'.
;;      * Fix doc.
;;
;; 2009/01/13
;;      * Rewrite advice for `pop-to-buffer' to avoid `pop-to-buffer' not effect
;;        when have many dedicated window in current frame.
;;      * Rewrite advice for `delete-other-windows' to avoid use common variable
;;        `delete-protected-window-list' and use `window-dedicated-p' instead.
;;        Remove variable `delete-protected-window-list' and function
;;        `multi-term-dedicated-match-protected-window-p'.
;;
;; 2009/01/06
;;      * Improve document.
;;
;; 2008/12/29
;;      * Remove option `multi-term-current-window-height' and
;;        function `multi-term-current-directory'.
;;      * Add some functions to make get dedicated term buffer,
;;        those functions is beginning with `multi-term-dedicated-'.
;;      * Modified advice `delete-window', make command `delete-window'
;;        and delete dedicated window, but will remember window height
;;        before deleted.
;;      * Don't remember dedicated window height if larger than max value.
;;      * Fix some bug with `delete-other-windows' and window configuration.
;;        And this bug exists with another extension `sr-speedbar'.
;;      * Add new variable `delete-protected-window-list' for protected
;;        special window that won't be deleted.
;;        This variable is common for any extension that use dedicated
;;        window.
;;      * Fix doc.
;;
;; 2008/12/21
;;      * Default bind `C-m' with `term-send-input'.
;;
;; 2008/12/10
;;      * Improve customize interface.
;;      * Setup customize automatically, don't need to user setup it up.
;;      * Add option `multi-term-try-create'.
;;      * Make function `multi-term-switch' accept offset argument.
;;      * Fix doc.
;;
;; 2008/10/22
;;      * Add variable `multi-term-current-window-height'.
;;      * Add variable `multi-term-buffer-name'.
;;      * Add variable `term-unbind-key-list'.
;;      * Add variable `term-rebind-key-alist'.
;;      * Move key setup and some extension from `term-extension.el'.
;;      * Create new function `multi-term-keystroke-setup'.
;;      * Fix doc.
;;
;; 2008/09/19
;;      * First released.
;;

;;; Acknowledgments:
;;
;;      Mark Triggs     <mst@dishevelled.net>
;;              For create multi-shell.el
;;      Aaron S. Hawley <aaron.s.hawley@gmail.com>
;;              For improve document.
;;

;;; Bug
;;
;;

;;; TODO
;;
;;
;;

;;; Require:
(require 'term)
(require 'cl)
(require 'advice)

;;; Code:

;;; Customize

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Customize ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defgroup multi-term nil
  "Multi term manager."
  :group 'term)

(defcustom multi-term-program nil
  "The program of term.
If this is nil, setup to environment variable of `SHELL'."
  :type 'string
  :group 'multi-term)

(defcustom multi-term-program-switches nil
  "The command-line switches to pass to the term program."
  :type 'string
  :group 'multi-term)

(defcustom multi-term-try-create t
  "Try to create a new term buffer when switch.

When use `multi-term-next' or `multi-term-prev', switch term buffer,
and try to create a new term buffer if no term buffers exist."
  :type 'boolean
  :group 'multi-shell)

(defcustom multi-term-default-dir "~/"
  "The default directory for terms if current directory doesn't exist."
  :type 'string
  :group 'multi-term)

(defcustom multi-term-buffer-name "terminal"
  "The buffer name of term buffer."
  :type 'string
  :group 'multi-term)

(defcustom multi-term-scroll-show-maximum-output nil
  "*Controls how interpreter output causes window to scroll.
If non-nil, then show the maximum output when the window is scrolled.

See variable `multi-term-scroll-to-bottom-on-output'."
  :type 'boolean
  :group 'multi-term)

(defcustom multi-term-scroll-to-bottom-on-output nil
  "*Controls whether interpreter output causes window to scroll.
If nil, then do not scroll.  If t or `all', scroll all windows showing buffer.
If `this', scroll only the selected window.
If `others', scroll only those that are not the selected window.

The default is nil.

See variable `multi-term-scroll-show-maximum-output'."
  :type 'boolean
  :group 'multi-term)

(defcustom multi-term-switch-after-close 'NEXT
  "Try to switch other `multi-term' buffer after close current one.
If this option is 'NEXT, switch to next `multi-term' buffer;
If this option is 'PREVIOUS, switch to previous `multi-term' buffer.
If this option is nil, don't switch other `multi-term' buffer."
  :type 'symbol
  :group 'multi-term)

(defcustom term-unbind-key-list
  '("C-z" "C-x" "C-c" "C-h" "C-y" "<ESC>")
  "The key list that will need to be unbind."
  :type 'list
  :group 'multi-term)

(defcustom term-bind-key-alist
  '(
    ("C-c C-c" . term-interrupt-subjob)
    ("C-p" . previous-line)
    ("C-n" . next-line)
    ("C-s" . isearch-forward)
    ("C-r" . isearch-backward)
    ("C-m" . term-send-raw)
    ("M-f" . term-send-forward-word)
    ("M-b" . term-send-backward-word)
    ("M-o" . term-send-backspace)
    ("M-p" . term-send-up)
    ("M-n" . term-send-down)
    ("M-M" . term-send-forward-kill-word)
    ("M-N" . term-send-backward-kill-word)
    ("M-r" . term-send-reverse-search-history)
    ("M-," . term-send-input)
    ("M-." . comint-dynamic-complete))
  "The key alist that will need to be bind.
If you do not like default setup, modify it, with (KEY . COMMAND) format."
  :type 'alist
  :group 'multi-term)

(defcustom multi-term-dedicated-window-height 14
  "The height of `multi-term' dedicated window."
  :type 'integer
  :group 'multi-term)

(defcustom multi-term-dedicated-max-window-height 30
  "The max height limit of `multi-term' dedicated window.
Default, when hide `multi-term' dedicated window, will remember
window height before hide, except height is larger than this.`"
  :type 'integer
  :group 'multi-term)

(defcustom multi-term-dedicated-skip-other-window-p nil
  "Default, can have `other-window' select window in cyclic ordering of windows.
In cases you don't want to select `multi-term' dedicated window, use `other-window'
and make `multi-term' dedicated window as a viewable sidebar.

So please turn on this option if you want to skip `multi-term' dedicated window with `other-window'.

Default is nil."
  :type 'boolean
  :set (lambda (symbol value)
         (set symbol value)
         (when (ad-advised-definition-p 'other-window)
           (multi-term-dedicated-handle-other-window-advice value)))
  :group 'multi-term)

(defcustom multi-term-dedicated-select-after-open-p nil
  "Default, multi-term won't focus terminal window after you open dedicated window.
Please make this option with t if you want focus terminal window.

Default is nil."
  :type 'boolean
  :group 'multi-term)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Constant ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defconst multi-term-dedicated-buffer-name "MULTI-TERM-DEDICATED"
  "The buffer name of dedicated `multi-term'.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Variable ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar multi-term-dedicated-window nil
  "The dedicated `multi-term' window.")

(defvar multi-term-dedicated-buffer nil
  "The dedicated `multi-term' buffer.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Interactive Functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;###autoload
(defun multi-term ()
  "Create new term buffer.
Will prompt you shell name when you type `C-u' before this command."
  (interactive)
  (let (term-buffer)
    ;; Set buffer.
    (setq term-buffer (multi-term-get-buffer current-prefix-arg))
    (set-buffer term-buffer)
    ;; Internal handle for `multi-term' buffer.
    (multi-term-internal)
    ;; Switch buffer
    (switch-to-buffer term-buffer)))

(defun multi-term-next (&optional offset)
  "Go to the next term buffer.
If OFFSET is `non-nil', will goto next term buffer with OFFSET."
  (interactive "P")
  (multi-term-switch 'NEXT (or offset 1)))

(defun multi-term-prev (&optional offset)
  "Go to the previous term buffer.
If OFFSET is `non-nil', will goto previous term buffer with OFFSET."
  (interactive "P")
  (multi-term-switch 'PREVIOUS (or offset 1)))

(defun multi-term-dedicated-open ()
  "Open dedicated `multi-term' window.
Will prompt you shell name when you type `C-u' before this command."
  (interactive)
  (if (not (multi-term-dedicated-exist-p))
      (let ((current-window (selected-window)))
        (if (multi-term-buffer-exist-p multi-term-dedicated-buffer)
            (unless (multi-term-window-exist-p multi-term-dedicated-window)
              (multi-term-dedicated-get-window))
          ;; Set buffer.
          (setq multi-term-dedicated-buffer (multi-term-get-buffer current-prefix-arg t))
          (set-buffer (multi-term-dedicated-get-buffer-name))
          ;; Get dedicate window.
          (multi-term-dedicated-get-window)
          ;; Whether skip `other-window'.
          (multi-term-dedicated-handle-other-window-advice multi-term-dedicated-skip-other-window-p)
          ;; Internal handle for `multi-term' buffer.
          (multi-term-internal))
        (set-window-buffer multi-term-dedicated-window (get-buffer (multi-term-dedicated-get-buffer-name)))
        (set-window-dedicated-p multi-term-dedicated-window t)
        ;; Select window.
        (select-window
         (if multi-term-dedicated-select-after-open-p
             ;; Focus dedicated terminal window if option `multi-term-dedicated-select-after-open-p' is enable.
             multi-term-dedicated-window
           ;; Otherwise focus current window.
           current-window)))
    (message "`multi-term' dedicated window has exist.")))

(defun multi-term-dedicated-close ()
  "Close dedicated `multi-term' window."
  (interactive)
  (if (multi-term-dedicated-exist-p)
      (let ((current-window (selected-window)))
        ;; Remember height.
        (multi-term-dedicated-select)
        (multi-term-dedicated-remember-window-height)
        ;; Close window.
        (if (and (require 'ecb nil t)
                 ecb-activated-window-configuration)
            ;; Toggle ECB window when ECB window activated.
            (progn
              (ecb-deactivate)
              (ecb-activate))
          ;; Otherwise delete dedicated window.
          (delete-window multi-term-dedicated-window)
          (if (multi-term-window-exist-p current-window)
              (select-window current-window))))
    (message "`multi-term' window is not exist.")))

(defun multi-term-dedicated-remember-window-height ()
  "Remember window height."
  (let ((win-height (multi-term-current-window-take-height)))
    (if (and (multi-term-dedicated-window-p) ;in `multi-term' window
             (> win-height 1)
             (<= win-height multi-term-dedicated-max-window-height))
        (setq multi-term-dedicated-window-height win-height))))

(defun multi-term-dedicated-toggle ()
  "Toggle dedicated `multi-term' window."
  (interactive)
  (if (multi-term-dedicated-exist-p)
      (multi-term-dedicated-close)
    (multi-term-dedicated-open)))

(defun multi-term-dedicated-select ()
  "Select the `multi-term' dedicated window."
  (interactive)
  (if (multi-term-dedicated-exist-p)
      (select-window multi-term-dedicated-window)
    (message "`multi-term' window is not exist.")))

(defun term-send-backward-kill-word ()
  "Backward kill word in term mode."
  (interactive)
  (term-send-raw-string "\C-w"))

(defun term-send-forward-kill-word ()
  "Kill word in term mode."
  (interactive)
  (term-send-raw-string "\ed"))

(defun term-send-backward-word ()
  "Move backward word in term mode."
  (interactive)
  (term-send-raw-string "\eb"))

(defun term-send-forward-word ()
  "Move forward word in term mode."
  (interactive)
  (term-send-raw-string "\ef"))

(defun term-send-reverse-search-history ()
  "Search history reverse."
  (interactive)
  (term-send-raw-string "\C-r"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Utilise Functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun multi-term-internal ()
  "Internal handle for `multi-term' buffer."
  ;; Add customize keystroke with `term-mode-hook'
  (remove-hook 'term-mode-hook 'multi-term-keystroke-setup)
  (add-hook 'term-mode-hook 'multi-term-keystroke-setup)
  ;; Load term mode
  (term-mode)
  (term-char-mode)
  ;; Handle term buffer close
  (multi-term-handle-close)
  ;; Handle `output' variable.
  (setq term-scroll-show-maximum-output multi-term-scroll-show-maximum-output
        term-scroll-to-bottom-on-output multi-term-scroll-to-bottom-on-output)
  ;; Add hook to be sure `term' quit subjob before buffer killed.
  (add-hook 'kill-buffer-hook 'multi-term-kill-buffer-hook))

(defun multi-term-get-buffer (&optional special-shell dedicated-window)
  "Get term buffer.
If option SPECIAL-SHELL is `non-nil', will use shell from user input.
If option DEDICATED-WINDOW is `non-nil' will create dedicated `multi-term' window ."
  (with-temp-buffer
    (let ((shell-name (or multi-term-program ;shell name
                          (getenv "SHELL")
                          (getenv "ESHELL")
                          "/bin/sh"))
          term-list-length              ;get length of term list
          index                         ;setup new term index
          term-name)                    ;term name
      (if dedicated-window
          (setq term-name multi-term-dedicated-buffer-name)
        ;; Compute index.
        (setq term-list-length (length (multi-term-list)))
        (setq index (if term-list-length (1+ term-list-length) 1))
        ;; switch to current local directory,
        ;; if in-existence, switch to `multi-term-default-dir'.
        (cd (or default-directory (expand-file-name multi-term-default-dir)))
        ;; adjust value N when max index of term buffer is less than length of term list
        (while (buffer-live-p (get-buffer (format "*%s<%s>*" multi-term-buffer-name index)))
          (setq index (1+ index)))
        (setq term-name (format "%s<%s>" multi-term-buffer-name index)))
      ;; Try get other shell name if `special-shell' is non-nil.
      (if special-shell
          (setq shell-name (read-from-minibuffer "Run program: " shell-name)))
      ;; Make term, details to see function `make-term' in `term.el'.
      (if multi-term-program-switches
          (make-term term-name shell-name nil multi-term-program-switches)
          (make-term term-name shell-name)))))


(defun multi-term-handle-close ()
  "Close current term buffer when `exit' from term buffer."
  (when (ignore-errors (get-buffer-process (current-buffer)))
    (set-process-sentinel (get-buffer-process (current-buffer))
                          (lambda (proc change)
                            (when (string-match "\\(finished\\|exited\\)" change)
                              (kill-buffer (process-buffer proc)))))))

(defun multi-term-kill-buffer-hook ()
  "Function that hook `kill-buffer-hook'."
  (when (eq major-mode 'term-mode)
    ;; Quit the current subjob
    ;; when have alive process with current term buffer.
    ;; Must do this job BEFORE `multi-term-switch-after-close' action.
    (when (term-check-proc (current-buffer))
      ;; Quit sub-process.
      (term-quit-subjob))
    ;; Remember dedicated window height.
    (multi-term-dedicated-remember-window-height)
    ;; Try to switch other multi-term buffer
    ;; when option `multi-term-switch-after-close' is non-nil.
    (when multi-term-switch-after-close
      (multi-term-switch-internal multi-term-switch-after-close 1))))

(defun multi-term-list ()
  "List term buffers presently active."
  ;; Autload command `remove-if-not'.
  (autoload 'remove-if-not "cl-seq")
  (sort
   (remove-if-not (lambda (b)
                    (setq case-fold-search t)
                    (string-match
                     (format "^\\\*%s<[0-9]+>\\\*$" multi-term-buffer-name)
                     (buffer-name b)))
                  (buffer-list))
   (lambda (a b)
     (< (string-to-number
         (cadr (split-string (buffer-name a) "[<>]")))
        (string-to-number
         (cadr (split-string (buffer-name b)  "[<>]")))))))

(defun multi-term-switch (direction offset)
  "Switch `multi-term' buffers.
If DIRECTION is `NEXT', switch to the next term.
If DIRECTION `PREVIOUS', switch to the previous term.
Option OFFSET for skip OFFSET number term buffer."
  (unless (multi-term-switch-internal direction offset)
    (if multi-term-try-create
        (progn
          (multi-term)
          (message "Create a new `multi-term' buffer."))
      (message "Haven't any `multi-term' buffer exist."))))

(defun multi-term-switch-internal (direction offset)
  "Internal `multi-term' buffers switch function.
If DIRECTION is `NEXT', switch to the next term.
If DIRECTION `PREVIOUS', switch to the previous term.
Option OFFSET for skip OFFSET number term buffer."
  (let (terms this-buffer)
    (setq terms (multi-term-list))
    (if (consp terms)
        (progn
          (setf (cdr (last terms)) terms)
          (setq this-buffer (position (current-buffer) (multi-term-list)))
          (if this-buffer
              (if (eql direction 'NEXT)
                  (switch-to-buffer (nth (+ this-buffer offset) terms))
                (switch-to-buffer (nth (+ (- (length (multi-term-list)) offset)
                                          this-buffer) terms)))
            (switch-to-buffer (car terms)))
          t)
      nil)))

(defun multi-term-keystroke-setup ()
  "Keystroke setup of `term-char-mode'.

By default, the key bindings of `term-char-mode' conflict with user's keystroke.
So this function unbinds some keys with `term-raw-map',
and binds some keystroke with `term-raw-map'."
  (let (bind-key bind-command)
    ;; Unbind base key that conflict with user's keys-tokes.
    (dolist (unbind-key term-unbind-key-list)
      (cond
       ((stringp unbind-key) (setq unbind-key (read-kbd-macro unbind-key)))
       ((vectorp unbind-key) nil)
       (t (signal 'wrong-type-argument (list 'array unbind-key))))
      (define-key term-raw-map unbind-key nil))
    ;; Add some i use keys.
    ;; If you don't like my keystroke,
    ;; just modified `term-bind-key-alist'
    (dolist (element term-bind-key-alist)
      (setq bind-key (car element))
      (setq bind-command (cdr element))
      (cond
       ((stringp bind-key) (setq bind-key (read-kbd-macro bind-key)))
       ((vectorp bind-key) nil)
       (t (signal 'wrong-type-argument (list 'array bind-key))))
      (define-key term-raw-map bind-key bind-command))))

(defun multi-term-dedicated-handle-other-window-advice (activate)
  "Handle advice for function `other-window'.
If ACTIVATE is `non-nil', will enable advice
`multi-term-dedicated-other-window-advice'.
Otherwise, disable it."
  (if activate
      (ad-enable-advice 'other-window 'after 'multi-term-dedicated-other-window-advice)
    (ad-disable-advice 'other-window 'after 'multi-term-dedicated-other-window-advice))
  (ad-activate 'other-window))

(defun multi-term-current-window-take-height (&optional window)
  "Return the height the `window' takes up.
Not the value of `window-height', it returns usable rows available for WINDOW.
If `window' is nil, get current window."
  (let ((edges (window-edges window)))
    (- (nth 3 edges) (nth 1 edges))))

(defun multi-term-dedicated-get-window ()
  "Get `multi-term' dedicated window."
  (setq multi-term-dedicated-window
        (split-window
         (selected-window)
         (- (multi-term-current-window-take-height) multi-term-dedicated-window-height))))

(defun multi-term-dedicated-get-buffer-name ()
  "Get the buffer name of `multi-term' dedicated window."
  (format "*%s*" multi-term-dedicated-buffer-name))

(defun multi-term-dedicated-exist-p ()
  "Return `non-nil' if `multi-term' dedicated window exist."
  (and (multi-term-buffer-exist-p multi-term-dedicated-buffer)
       (multi-term-window-exist-p multi-term-dedicated-window)))

(defun multi-term-window-exist-p (window)
  "Return `non-nil' if WINDOW exist.
Otherwise return nil."
  (and window (window-live-p window)))

(defun multi-term-buffer-exist-p (buffer)
  "Return `non-nil' if `BUFFER' exist.
Otherwise return nil."
  (and buffer (buffer-live-p buffer)))

(defun multi-term-dedicated-window-p ()
  "Return `non-nil' if current window is `multi-term' dedicated window.
Otherwise return nil."
  (equal (multi-term-dedicated-get-buffer-name) (buffer-name (window-buffer))))

(defun multi-term-window-dedicated-only-one-p ()
  "Only have one non-dedicated window."
  (interactive)
  (let ((window-number 0)
        (dedicated-window-number 0))
    (walk-windows
     (lambda (w)
       (with-selected-window w
         (incf window-number)
         (if (window-dedicated-p w)
             (incf dedicated-window-number)))))
    (if (and (> dedicated-window-number 0)
             (= (- window-number dedicated-window-number) 1))
        t nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Advice ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defadvice delete-other-windows (around multi-term-delete-other-window-advice activate)
  "This is advice to make `multi-term' avoid dedicated window deleted.
Dedicated window can't deleted by command `delete-other-windows'."
  (let ((multi-term-dedicated-active-p (multi-term-window-exist-p multi-term-dedicated-window)))
    (if multi-term-dedicated-active-p
        (let ((current-window (selected-window)))
          (dolist (win (window-list))
            (when (and (window-live-p win)
                       (not (eq current-window win))
                       (not (window-dedicated-p win)))
              (delete-window win))))
      ad-do-it)))

(defadvice delete-window (before multi-term-delete-window-advice activate)
  "Use `delete-window' delete `multi-term' dedicated window.
Have same effect as command `multi-term-dedicated-close'.
This advice to remember `multi-term' dedicated window height before deleting."
  ;; Remember window height before deleted.
  (multi-term-dedicated-remember-window-height))

(defadvice pop-to-buffer (before multi-term-pop-to-buffer-advice activate)
  "This advice fix the problem between `pop-to-buffer' and dedicated window.
By default, function `display-buffer' can't display buffer in selected window
if current window is `dedicated'.

So function `display-buffer' conflicts with `sr-speedbar' window, because
`sr-speedbar' window is a `dedicated' window.

That is to say, when current frame just have one `non-dedicated' window,
any functions that uses `display-buffer' can't split windows
to display buffer, even when the option `pop-up-windows' is enabled.

And the example function that can induce the problem is `pop-to-buffer'.

This advice will fix this problem when current frame just have one `non-dedicated' window."
  (when (and pop-up-windows                           ;`pop-up-windows' is enable
             (multi-term-window-dedicated-only-one-p) ;just have one `non-dedicated' window.
             (multi-term-window-exist-p multi-term-dedicated-window)
             (not (multi-term-dedicated-window-p))) ;not in `sr-speedbar' window
    (split-window-vertically)
    (windmove-down)))

(defadvice other-window (after multi-term-dedicated-other-window-advice)
  "Default, can use `other-window' select window in cyclic ordering of windows.
But sometimes we don't want to select `sr-speedbar' window,
but use `other-window' and just make `multi-term' dedicated
window as a viewable sidebar.

This advice can make `other-window' skip `multi-term' dedicated window."
  (let ((count (or (ad-get-arg 0) 1)))
    (when (and (multi-term-window-exist-p multi-term-dedicated-window)
               (eq multi-term-dedicated-window (selected-window)))
      (other-window count))))

(provide 'multi-term)

;; Local Variables:
;; time-stamp-line-limit: 10
;; time-stamp-start: "Last-Updated: <"
;; time-stamp-end: ">"
;; End:

;;; multi-term.el ends here

;;; LocalWords:  multi el dir sr Hawley eb ef cd
