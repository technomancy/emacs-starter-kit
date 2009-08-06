;;; yank-pop-summary.el --- yank-pop with summary buffer

;; Copyright (C) 2002 TSUCHIYA Masatoshi <tsuchiya@namazu.org>

;; Author: TSUCHIYA Masatoshi <tsuchiya@namazu.org>
;; Keywords: yank
;; Version: $Revision: 1.10 $

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, you can either send email to this
;; program's maintainer or write to: The Free Software Foundation,
;; Inc.; 59 Temple Place, Suite 330; Boston, MA 02111-1307, USA.


;;; Commentary:

;; This file provides commands which are similar to `yank-pop' but
;; display summary of `kill-ring'.

;; The latest version of this program can be downloaded from
;; http://namazu.org/~tsuchiya/elisp/yank-pop-summary.el.


;;; Install:

;; Put this file into load-path'ed directory, and byte compile it if
;; desired.  And put these expressions into your ~/.emacs.
;;
;;      (autoload 'yank-pop-forward "yank-pop-summary" nil t)
;;      (autoload 'yank-pop-backward "yank-pop-summary" nil t)
;;      (global-set-key "\M-y" 'yank-pop-forward)
;;      (global-set-key "\C-\M-y" 'yank-pop-backward)


;;; TODO:

;; 時々，無限ループになって Emacs ごと殺さないといけないようになるので，
;; pre-command-hook() を使わない形に再実装したい．おそらく，以下のよう
;; な形で read-event() と unread-command-events を組み合わせればうまく
;; いきそうだ．

;; (defun loop-test-read-event (keymap)
;;   (let* ((event (if (fboundp 'read-event)
;;                     (read-event)
;;                   (next-command-event))) ; XEmacs の場合
;;          (action (lookup-key keymap (vector event))))
;;     (or (if (keymapp action)
;;             (loop-test-read-event action)
;;           action)
;;         (progn
;;           (setq unread-command-events
;;                 (cons event unread-command-events))
;;           nil))))

;; (defun loop-test ()
;;   (interactive)
;;   (let ((echo-keystrokes 0)
;;         (keymap (make-sparse-keymap))
;;         action)
;;     (define-key keymap "\C-y" 'next)
;;     (define-key keymap "\C-\M-y" 'prev)
;;     (define-key keymap "\C-xo" 'other-window)
;;     (while (setq action (loop-test-read-event keymap))
;;       (message "%s" action)
;;       (cond
;;        ((eq action 'prev)
;;         (forward-line -1))
;;        ((eq action 'next)
;;         (forward-line 1))))))

;;; Code:

(eval-and-compile
  ;; Stuffs to keep compatibility between Emacsen.
  (if (featurep 'xemacs)
      (require 'overlay))
  (or (fboundp 'defgroup)
      (defmacro defgroup (symbol members doc &rest args) nil))
  (or (fboundp 'defcustom)
      (defmacro defcustom (symbol value doc &rest args)
	(list 'defvar symbol value doc)))
  ;; These macros, such as `when', `unless' and `dolist' are imported
  ;; from subr.el of Emacs-21.2.
  (or (fboundp 'when)
      (progn
	(defmacro when (cond &rest body)
	  "If COND yields non-nil, do BODY, else return nil."
	  (list 'if cond (cons 'progn body)))
	(put 'when 'edebug-form-spec '(form body))
	(put 'when 'lisp-indent-function 1)))
  (or (fboundp 'unless)
      (progn
	(defmacro unless (cond &rest body)
	  "If COND yields nil, do BODY, else return nil."
	  (cons 'if (cons cond (cons nil body))))
	(put 'unless 'edebug-form-spec '(form body))
	(put 'unless 'lisp-indent-function 1)))
  (or (and (fboundp 'dolist)
	   (dolist (var nil t)))
      (progn
	(defmacro dolist (spec &rest body)
	  "(dolist (VAR LIST [RESULT]) BODY...): loop over a list.
Evaluate BODY with VAR bound to each car from LIST, in turn.
Then evaluate RESULT to get return value, default nil."
	  (let ((temp (make-symbol "--dolist-temp--")))
	    (list 'let (list (list temp (nth 1 spec)) (car spec))
		  (list 'while temp
			(list 'setq (car spec) (list 'car temp))
			(cons 'progn
			      (append body
				      (list (list 'setq temp
						  (list 'cdr temp))))))
		  (if (cdr (cdr spec))
		      (cons 'progn
			    (cons (list 'setq (car spec) nil)
				  (cdr (cdr spec))))))))
	(put 'dolist 'edebug-form-spec '((symbolp form &rest form) &rest form))
	(put 'dolist 'lisp-indent-function 1)))
  (or (fboundp 'save-current-buffer)
      (progn
	(defmacro save-current-buffer (&rest body)
	  "Save the current buffer; execute BODY; restore the current buffer."
	  (let ((orig-buffer (make-symbol "orig-buffer")))
	    (` (let (((, orig-buffer) (current-buffer)))
		 (unwind-protect
		     (progn (,@ body))
		   (set-buffer (, orig-buffer)))))))
	(put 'save-current-buffer 'edebug-form-spec t)
	(put 'save-current-buffer 'lisp-indent-function 1)))
  (or (fboundp 'with-current-buffer)
      (progn
	(defmacro with-current-buffer (buffer &rest body)
	  "Execute the forms in BODY with BUFFER as the current buffer."
	  (` (save-current-buffer
	       (set-buffer (, buffer))
	       (,@ body))))
	(put 'with-current-buffer 'edebug-form-spec '(form body))
	(put 'with-current-buffer 'lisp-indent-function 1))))


(defgroup yank-pop-summary nil
  "Commands with summary of `kill-ring'"
  :group 'killing)

(defcustom yank-pop-summary-show-line-number t
  "*Non-nil means that line numbers are displayed in summary buffer."
  :type 'boolean
  :group 'yank-pop-summary)

(defcustom yank-pop-summary-escape-alist
  '(("\t" . "^I")
    ("\n" . "^J")
    ("\r" . "^M"))
  "*Alist of escape strings to create summary of `kill-ring'."
  :type '(repeat (cons (string :tag "From") (string :tag "To")))
  :group 'yank-pop-summary)

(defcustom yank-pop-summary-window-height 10
  "*Height of summary window to view `kill-ring'."
  :type 'integer
  :group 'yank-pop-summary)

(defcustom yank-pop-summary-window-offset 3
  "*Offset line relative to summary window."
  :type '(choice integer (const :tag "Center of window" nil))
  :group 'yank-pop-summary)

(defconst yank-pop/buffer-name " *yank-pop*")
(defconst yank-pop/arrow-string "=>")
(defconst yank-pop/valid-commands
  '(digit-argument
    negative-argument
    universal-argument
    universal-argument-other-key
    yank-pop-forward
    yank-pop-backward))

(defvar yank-pop/arrow-overlay nil)
(defvar yank-pop/window-configuration nil)
(make-variable-buffer-local 'yank-pop/arrow-overlay)
(make-variable-buffer-local 'yank-pop/window-configuration)

(defun yank-pop/make-arrow ()
  "Make an overlay arrow at this current point."
  (if yank-pop/arrow-overlay
      (move-overlay yank-pop/arrow-overlay
		    (point)
		    (+ (point) (length yank-pop/arrow-string))
		    (current-buffer))
    (setq yank-pop/arrow-overlay
	  (make-overlay (point)
			(+ (point) (length yank-pop/arrow-string))
			(current-buffer))))
  (overlay-put yank-pop/arrow-overlay 'invisible t)
  (overlay-put yank-pop/arrow-overlay
	       'before-string yank-pop/arrow-string))

(defun yank-pop/make-summary ()
  "Make the summary buffer to view kill-ring and return it."
  (with-current-buffer (get-buffer-create yank-pop/buffer-name)
    (buffer-disable-undo)
    (widen)
    (let ((i 1) (ring kill-ring) bol pos lines buffer-read-only)
      (erase-buffer)
      (while ring
	(setq bol (point))
	(insert (car ring))
	(setq lines (count-lines bol (point)))
	(goto-char bol)
	(insert (if yank-pop-summary-show-line-number
		    (format "%4d:%-2d " i lines)
		  (format "%4d: " i)))
	(setq pos (point))
	(dolist (pair yank-pop-summary-escape-alist)
	  (goto-char pos)
	  (while (search-forward (car pair) nil t)
	    (delete-char (- (length (car pair))))
	    (insert (cdr pair))))
	(goto-char (point-max))
	(insert "\n")
	(put-text-property bol (point) 'yank-pop ring)
	(setq i (1+ i)
	      ring (cdr ring)))
      (delete-char -1))
    (setq truncate-lines t
	  buffer-read-only t)
    (set-buffer-modified-p nil)
    (current-buffer)))

(defmacro yank-pop/current-position ()
  "Return the position points the current kill-ring entry."
  (` (text-property-any (point-min) (point-max)
			'yank-pop kill-ring-yank-pointer)))

(defun yank-pop/show-summary ()
  "Show the summary buffer and make it current for after operations.
When the summary buffer is not active, call `yank-pop/make-summary' to
create it."
  (let (buf win)
    (unless (and yank-pop/window-configuration
		 (setq buf (get-buffer yank-pop/buffer-name)
		       win (and buf (get-buffer-window buf)))
		 (set-buffer buf)
		 (select-window win))
      (setq yank-pop/window-configuration (current-window-configuration))
      (set-buffer (setq buf (yank-pop/make-summary)))
      (setq win (or (get-buffer-window buf)
		    (if (one-window-p)
			(split-window (selected-window)
				      (- (window-height)
					 yank-pop-summary-window-height))
		      (next-window))))
      (set-window-buffer win buf)
      (select-window win))))

(defun yank-pop/close-summary ()
  "Close the summary buffer and recover window configuration."
  (with-current-buffer yank-pop/buffer-name
    (when (overlayp yank-pop/arrow-overlay)
      (delete-overlay yank-pop/arrow-overlay)
      (setq yank-pop/arrow-overlay nil))
    (bury-buffer (current-buffer)))
  (when (window-configuration-p yank-pop/window-configuration)
    (set-window-configuration yank-pop/window-configuration))
  (setq yank-pop/window-configuration nil))

(defun yank-pop/pre-command ()
  (unless (memq this-command yank-pop/valid-commands)
    (remove-hook 'pre-command-hook 'yank-pop/pre-command)
    (setq this-command
	  (` (lambda ()
	       (interactive)
	       ;; Keep the value of `this-command' for functions refer
	       ;; its value.
	       (setq this-command (quote (, this-command)))
	       (if (or debug-on-error debug-on-quit)
		   (yank-pop/close-summary)
		 (condition-case nil
		     (yank-pop/close-summary)
		   (error (setq yank-pop/window-configuration nil))))
	       (call-interactively this-command))))))

(defun yank-pop-forward (arg)
  "Replace just-yanked stretch of killed text with a different stretch.
This command is quite similar to `yank-pop' but displays summary
buffer to view strings registered in `kill-ring'."
  (interactive "*p")
  (prog1 (yank-pop arg)
    (save-current-buffer
      (save-selected-window
	(yank-pop/show-summary)
	(goto-char (yank-pop/current-position))
	(yank-pop/make-arrow)
	(recenter yank-pop-summary-window-offset)))
    (add-hook 'pre-command-hook 'yank-pop/pre-command)))

(defun yank-pop-backward (arg)
  "Replace just-yanked stretch of killed text with a different stretch.
This command is quite similar to `yank-pop-forward' but its argument
is interpreted reversely."
  (interactive "*p")
  (yank-pop-forward (- arg)))

(provide 'yank-pop-summary)

;;; yank-pop-summary.el ends here
