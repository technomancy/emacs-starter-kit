;;; from-osxkeys.el ---
;;
;; Author: Lennart Borgman (lennart O borgman A gmail O com)
;; Created: 2008-06-18T16:32:32+0200 Wed
;; Version:
;; Last-Updated:
;; URL:
;; Keywords:
;; Compatibility:
;;
;; Features that might be required by this library:
;;
;;   None
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; This is extracted from
;; http://article.gmane.org/gmane.emacs.devel/90861
;; plus some additions.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change log:
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:



;; From: David Reitter <david.reitter <at> gmail.com>
;; Subject: Re: longlines-mode
;; Newsgroups: gmane.emacs.devel
;; Date: 2008-02-29 10:47:40 GMT (15 weeks, 4 days, 20 hours and 13 minutes ago)

;; On 28 Feb 2008, at 16:47, Stefan Monnier wrote:
;; >>
;; >> We have been using different code for these that does the right
;; >> thing. This was necessary primarily because of variable width
;; >> fonts. <down> is bound to visual-line-down', which moves point to
;; >> the next visual line and to the column that is closest on the screen
;; >> (pixel  wise), minimizing horizontal offset (in pixels). Other
;; >> functions provided are beginning-of-visual-line',
;; >> `end-of-visual-line', `kill-  visual-line', `kill-whole-visual-line'.
;; >
;; > Could you show us the corresponding code?

;; The code is below. This is straight out of Aquamacs (osxkeys.el) and
;; may or may not run as is.

;; Note that `visual-line-up' and friends use two different methods to
;; figure out the best position to move to because of a slowness with
;; outline-(minor-)mode. One of the methods (basically binary search) is
;; much faster when a lot of hidden text is present, but a bit slower in
;; all other cases.

;; We have `visual-line-down' bound to <down> (and so forth), with
;; special cases for minibuffers.

;; N.B. I do not take comments about non-conforming code formatting etc.
;; at this point.

(defun visual-col-at-point ()
  (- (point)
     (save-excursion
       (vertical-motion 0)
       (point))))
;; seems slower (in situations with very long lines)
;;(or (car (nth 6 (posn-at-point))) 0))

(defun visual-pixel-col-at-point ()
  (or
   (car-safe (pos-visible-in-window-p (point) nil 'partial))
   0))

(defvar visual-movement-temporary-goal-column nil)
(make-variable-buffer-local 'visual-movement-temporary-goal-column)

(defvar visual-previous-scroll-margin 'none)
(defun visual-restore-scroll-margin ()
  (if (integerp visual-previous-scroll-margin)
      (setq scroll-margin visual-previous-scroll-margin))
  (remove-hook 'pre-command-hook 'visual-restore-scroll-margin))

(defcustom visual-scroll-margin nil
  "Number of lines of margin at top and bottom of a window.
For visual scrolling with up and down keys, this value
applies instead of `scroll-margin' if it is non-nil.

The reason this variable exists is that clicks in the first and last
line of a window will set the cursor within the standard scroll-margin,
causing the buffer to scroll immediately. This is usually undesired.
In this case, set `scroll-margin' to zero and `visual-scroll-margin'
to the desired margin."
  :group 'Aquamacs)

;; (setq scroll-margin 0)
;; (setq visual-scroll-margin 5)
(defun visual-line-up (num-lines)
  "Move cursor vertically up NUM-LINES lines.
Interactively, vscroll tall lines if `auto-window-vscroll' is
enabled.  If there is no character in the target line exactly
over the current horizontal pixel position, the cursor is
positioned close to the character in that line at the same position,
or at the end of the line if it is not long enough.

The command C-x C-n can be used to create
a semipermanent goal column for this command.
Then instead of trying to move exactly vertically (or as close as
possible),
this command moves to the specified goal column (or as close as
possible).
The goal column is stored in the variable `goal-column', which is nil
when there is no goal column.

This function differs from `previous-line' as it moves vertically
in the visual sense. The result differs when variable-width font is
used or when characters of non-standard width (e.g. TABs) are used.

If you are thinking of using this in a Lisp program, consider using
`forward-line' with a negative argument instead.  It is usually easier
to use and more reliable (no dependence on goal column, etc.)."
  (interactive "p")
  (if (bobp) (signal 'beginning-of-buffer nil))
  (let ((pixel-col (visual-pixel-col-at-point))
	(visual-col (visual-col-at-point))
	(old-point (point))
	(end-of-old-line))

    ;; temporary binding of scroll-margin
    ;; cannot do this with a temporary let binding
    (setq visual-previous-scroll-margin scroll-margin)
    (if visual-scroll-margin
	(setq scroll-margin visual-scroll-margin))
    (add-hook 'pre-command-hook 'visual-restore-scroll-margin)

    (save-excursion
      (vertical-motion 1)	;; trying going one down, to left
      (setq end-of-old-line (point)))

    (vertical-motion 0)

    (let* ((beg-of-old-line
            ;; move right, but not further than to end of line
	    (prog1 (point)
	      (vertical-motion (- num-lines))))	    ;; one up again
	   (beg-of-new-line (point))
	   (rel-beg-of-old-line  (- beg-of-old-line (point) 1)))

      ;; handle track-eol...
      (if (and track-eol (= old-point (1- end-of-old-line))
	       ;; Don't count beg of empty line as end of line
	       ;; unless we just did explicit end-of-line.
	       (or (not (= old-point beg-of-old-line))
		   (eq last-command 'end-of-line)))
	  (setq visual-movement-temporary-goal-column 9999))

      ;; approximate positioning
      (if (and (or goal-column visual-movement-temporary-goal-column)
	       (memq last-command '(visual-line-up
				    visual-line-down
				    osxkeys-visual-line-up-in-buffers
				    osxkeys-visual-line-down-in-buffers))
	       (= old-point (1- end-of-old-line)))
	  ;; jumping from end of line

          (forward-char (min (or goal-column
                                 visual-movement-temporary-goal-column)
                             rel-beg-of-old-line))
	;; else, do complete positioning
	;; save original position
	(setq visual-movement-temporary-goal-column visual-col)
                                        ;	(forward-char (min visual-col rel-beg-of-old-line))

	;; this won't work because we don't have the
	;; absolute position, just the position within window
	;; (let ((p (pos-visible-in-window-p old-point nil 'p))
        ;; 	      (p2 (pos-visible-in-window-p beg-of-new-line nil 'p) ))
        ;; 	  (print (cons (car p) (cdr p2)))
        ;; 	  (posn-set-point (cons (car p) (cdr p2)))
        ;; 	  )
	(if (> (abs (- (point) beg-of-old-line)) 400)
	    ;; aq-find-position-at-pixel-col is much faster when
	    ;; large portions of hidden text are to be crossed.
	    ;; this can happen in outline-(minor-)mode for instance.
	    (goto-char (aq-find-position-at-pixel-col  pixel-col))
	  ;; approximate positioning
	  (forward-char (min visual-col rel-beg-of-old-line))
	  (if (>= (visual-pixel-col-at-point) pixel-col)
	      (progn
		(while (and
			(> (visual-pixel-col-at-point) pixel-col)
			(> (point) beg-of-new-line)) ;; do not cross line
		  (forward-char -1)))
	    (progn
	      (while (and
		      (< (visual-pixel-col-at-point) pixel-col)
		      (< (point) (1- beg-of-old-line))) ;; do not cross line
		(forward-char +1)))))

	))))

(defun visual-line-down (num-lines)
  "Move cursor vertically down NUM-LINES lines.
Interactively, vscroll tall lines if `auto-window-vscroll' is enabled.
If there is no character in the target line exactly under the current
column,
the cursor is positioned after the character in that line which spans
this
column, or at the end of the line if it is not long enough.
If there is no line in the buffer after this one, behavior depends on
the
value of `next-line-add-newlines'.  If non-nil, it inserts a newline
character
to create a line, and moves the cursor to that line.  Otherwise it
moves the
cursor to the end of the buffer.

The command C-x C-n can be used to create
a semipermanent goal column for this command.
Then instead of trying to move exactly vertically (or as close as
possible),
this command moves to the specified goal column (or as close as
possible).
The goal column is stored in the variable `goal-column', which is nil
when there is no goal column.

This function differs from `next-line' as it moves vertically
in the visual sense. The result differs when variable-width font is
used or when characters of non-standard width (e.g. TABs) are used.

If you are thinking of using this in a Lisp program, consider
using `forward-line' instead.  It is usually easier to use
and more reliable (no dependence on goal column, etc.)."
  (interactive "p")

  (if (and next-line-add-newlines (= num-lines 1))
      (if (save-excursion (end-of-line) (eobp))
          ;; When adding a newline, don't expand an abbrev.
          (let ((abbrev-mode nil))
            (end-of-line)
            (insert hard-newline)))
    (if (eobp) (signal 'end-of-buffer nil)))
  (let ((pixel-col (visual-pixel-col-at-point))
	(visual-col (visual-col-at-point))
	(old-point (point))
	(beg-of-line)
	(next-line-start)
	(rel-next-line-start))

    ;; temporary binding of scroll-margin
    ;; cannot do this with a temporary let binding
    (setq visual-previous-scroll-margin scroll-margin)
    (if visual-scroll-margin
	(setq scroll-margin visual-scroll-margin))
    (add-hook 'pre-command-hook 'visual-restore-scroll-margin)

    (vertical-motion num-lines) ;; down
    (save-excursion
      (setq beg-of-line (point))
      (vertical-motion +1) ;; down
      (setq next-line-start (point))
      (setq rel-next-line-start  (- (point) beg-of-line 1)))
    (unless (= beg-of-line (point-max))
      ;; handle track-eol...
      (if (and track-eol (= old-point (1- next-line-start))
	       ;; Don't count beg of empty line as end of line
	       ;; unless we just did explicit end-of-line.
	       (or (not (= 0 visual-col))
		   (eq last-command 'end-of-line)))
	  (setq visual-movement-temporary-goal-column 9999))
      ;; approximate positioning
      (if (and (or goal-column visual-movement-temporary-goal-column)
               (memq last-command '(visual-line-up
                                    visual-line-down
                                    osxkeys-visual-line-up-in-buffers
                                    osxkeys-visual-line-down-in-buffers))
               (= old-point (- beg-of-line 1))) ;; jumping from end of line

          (forward-char (min (or goal-column
                                 visual-movement-temporary-goal-column)
                             rel-next-line-start))
        ;; else, do complete positioning
        ;; save original position
        (setq visual-movement-temporary-goal-column visual-col)
        ;; aq-find-position-at-pixel-col is much faster when
        ;; large portions of hidden text are to be crossed.
        ;; this can happen in outline-(minor-)mode for instance.
        (if (> (abs (- old-point next-line-start)) 400)
            (goto-char (aq-find-position-at-pixel-col  pixel-col))
          (forward-char (min visual-col rel-next-line-start))
          (if (> (visual-pixel-col-at-point) pixel-col)
              (progn
                (while (and
                        (> (visual-pixel-col-at-point) pixel-col)
                        (> (point) beg-of-line)) ;; do not cross line
                  (forward-char -1)))
            (progn
              (while (and
                      (< (visual-pixel-col-at-point) pixel-col)
                      (< (point) (1- next-line-start))) ;; do not cross line
                (forward-char +1)))))))))

(defun aq-find-position-at-pixel-col  (pixel-col)

  (let ((beg-of-line) (end-of-line))
    (vertical-motion 1)	;; trying going one down, to left
    (setq end-of-line (point))
    (if (eq (point) (point-max)) (vertical-motion 0) (vertical-motion
                                                      -1))
    (setq beg-of-line (point))

    (let ((op (point)))
					; move to beg of line
      (vertical-motion 0) ;; trying going one down, to left
      (forward-char (/ pixel-col (frame-char-width)))

      (aq-find-position-at-pixel-col-recursive
       beg-of-line end-of-line pixel-col)

      (let* ((nearest-pos (point))
	     (smallest-distance
	      (abs (- pixel-col (visual-pixel-col-at-point)))))

	(let ((pdif (abs (- pixel-col
			    (progn (forward-char -1)
				   (visual-pixel-col-at-point))))))
	  (when (< pdif smallest-distance)
	    (setq nearest-pos (point))
	    (setq smallest-distance pdif)))

	(let ((pdif (abs (- pixel-col
			    (progn (forward-char 2)
				   (visual-pixel-col-at-point))))))
	  (when (< pdif smallest-distance)
	    (setq nearest-pos (point))
	    (setq smallest-distance pdif)))
	(goto-char nearest-pos))

      (point))))

(defun aq-find-position-at-pixel-col-recursive
  (beg-of-line end-of-line pixel-col)
                                        ; set it in the middle

  (if (eq beg-of-line end-of-line)
      (point)

    (let ((middle (+ beg-of-line (round (/ (- end-of-line beg-of-line)
                                           2)))))
      (if (or
           (eq middle (point)) ;; wouldn't change point any more
           (eq (visual-pixel-col-at-point) pixel-col))
          (point)
                                        ;(incf steps)
        (goto-char middle)
        (if (> (visual-pixel-col-at-point) pixel-col)
            (aq-find-position-at-pixel-col-recursive
             beg-of-line (point) pixel-col)
          (aq-find-position-at-pixel-col-recursive
           (point) end-of-line pixel-col))))))


(defun beginning-of-visual-line ()
  (interactive)
  (if (bobp)
      (signal 'beginning-of-buffer nil))
  (vertical-motion 0))

(defun end-of-visual-line ()
  (interactive)
  (if (eobp)
      (signal 'end-of-buffer nil))
  (let ((end-of-line (line-end-position)))
    (vertical-motion 1)
    (unless (or (eobp)
                ;;(< (point) end-of-line) ;; jumping over wrapped text
                )
      (backward-char 1))))

;; this code based on simple.el
(defun kill-visual-line (&optional arg)
  "Kill the rest of the visual line; if no nonblanks there, kill thru
newline.
With prefix argument, kill that many lines from point.
Negative arguments kill lines backward.
With zero argument, kills the text before point on hthe current line.

When calling from a program, nil means \"no arg\",
a number counts as a prefix arg.

To kill a whole line, when point is not at the beginning, type \
\\[beginning-of-line] \\[kill-line] \\[kill-line].

If `kill-whole-line' is non-nil, then this command kills the whole line
including its terminating newline, when used at the beginning of a line
with no argument.  As a consequence, you can always kill a whole line
by typing \\[beginning-of-line] \\[kill-line].

If you want to append the killed line to the last killed text,
use \\[append-next-kill] before \\[kill-line].

If the buffer is read-only, Emacs will beep and refrain from deleting
the line, but put the line in the kill ring anyway.  This means that
you can use this command to copy text from a read-only buffer.
\(If the variable `kill-read-only-ok' is non-nil, then this won't
even beep.)"
  (interactive "P")
  (kill-region (point)
	       ;; It is better to move point to the other end of the kill
	       ;; before killing.  That way, in a read-only buffer, point
	       ;; moves across the text that is copied to the kill ring.
	       ;; The choice has no effect on undo now that undo records
	       ;; the value of point from before the command was run.
	       (progn
		 (if arg
		     (vertical-motion (prefix-numeric-value arg))
		   (if (eobp)
		       (signal 'end-of-buffer nil))
		   (let ((end
			  (save-excursion
			    (end-of-visual-line) (point))))
		     (if (or (save-excursion
			       ;; If trailing whitespace is visible,
			       ;; don't treat it as nothing.
			       (unless show-trailing-whitespace
				 (skip-chars-forward " \t" end))
			       (= (point) end))
			     (and kill-whole-line (bolp)))
			 (visual-line-down 1)
		       (goto-char end))))
		 (point))))

(defun kill-whole-visual-line (&optional arg)
  "Kill current visual line.
With prefix arg, kill that many lines starting from the current line.
If arg is negative, kill backward.  Also kill the preceding newline.
\(This is meant to make \\[repeat] work well with negative arguments.\)
If arg is zero, kill current line but exclude the trailing newline."
  (interactive "p")
  (if (and (> arg 0) (eobp) (save-excursion (vertical-motion 0) (eobp)))
      (signal 'end-of-buffer nil))
  (if (and (< arg 0) (bobp) (save-excursion (vertical-motion 1) (bobp)))
      (signal 'beginning-of-buffer nil))
  (unless (eq last-command 'kill-region)
    (kill-new "")
    (setq last-command 'kill-region))
  (cond ((zerop arg)
	 ;; We need to kill in two steps, because the previous command
	 ;; could have been a kill command, in which case the text
	 ;; before point needs to be prepended to the current kill
	 ;; ring entry and the text after point appended.  Also, we
	 ;; need to use save-excursion to avoid copying the same text
	 ;; twice to the kill ring in read-only buffers.
	 (save-excursion
	   ;; delete in one go
	   (kill-region (progn (vertical-motion 0) (point))
			(progn (vertical-motion 1) (point)))
           ))
	((< arg 0)
	 (save-excursion
	   (kill-region (point) (progn (end-of-visual-line) (point))))
	 (kill-region (point)
		      (progn (vertical-motion (1+ arg))
			     (unless (bobp) (backward-char))
			     (point))))
	(t
	 (save-excursion
	   (kill-region (progn (vertical-motion 0) (point))
			(progn (vertical-motion arg) (point)))))))

(defun osxkeys-visual-line-up-in-buffers ()
  "Moves the cursor up one (visual) line.
If the `up' key would normally be bound to something else than
`previous-line' (as it is the case in minibuffers), the other binding
is called."
  (interactive)
  (let* (osx-key-mode  ;; turn off mode temporarily
	 (binding (key-binding [up])))
    (if (eq binding 'previous-line)
	(call-interactively (function visual-line-up))
      (call-interactively binding))))

(defun osxkeys-visual-line-down-in-buffers ()
  "Moves the cursor down one (visual) line.
If the `down' key would normally be bound to something else than
`next-line' (as it is the case in minibuffers), the other binding
is called."
  (interactive)
  (let* (osx-key-mode  ;; turn off mode temporarily
	 (binding (key-binding [down])))
    (if (eq binding 'next-line)
	(call-interactively (function visual-line-down))
      (call-interactively binding))))

;; mark functions for CUA
(dolist (cmd
	 '( beginning-of-visual-line
	    end-of-visual-line
	    visual-line-down visual-line-up
	    osxkeys-visual-line-up-in-buffers
	    osxkeys-visual-line-down-in-buffers
            smart-move-beginning
            smart-move-end))
  (put cmd 'CUA 'move))

(defalias 'original-kill-line 'kill-line "test")

(defun smart-move-beginning (arg)
  (interactive "p")
  (if (or (null arg) (/= arg 1))
      (if visual-line-mode
          (progn
            (visual-line-up (1- arg))
            (beginning-of-visual-line))
        (call-interactively 'move-beginning-of-line arg))
    (let ((here (point)))
      (if visual-line-mode
          (beginning-of-visual-line)
        (move-beginning-of-line))
      (when (= here (point))
        (if (= here (line-beginning-position))
            (skip-chars-forward " \t")
          (backward-char 1)
          (beginning-of-visual-line))))))

(defun smart-move-end (arg)
  (interactive "p")
  (if (or (null arg) (/= arg 1))
      (if visual-line-mode
          (progn
            (visual-line-down (1- arg))
            (beginning-of-visual-line))
        (call-interactively 'move-end-of-line arg))
    (let ((here (point)))
      (if visual-line-mode
          (end-of-visual-line)
        (move-end-of-line))
      (when (= here (point))
        (if (= here (line-end-position))
            (skip-chars-backward " \t")
          (forward-char 1)
          (end-of-visual-line))))))

(defvar visual-line-map
  (let ((map (make-sparse-keymap)))
    (define-key map [remap next-line] 'visual-line-down)
    (define-key map [remap previous-line] 'visual-line-up)
    (define-key map [remap kill-line] 'kill-visual-line)
    (define-key map [(control shift ?k)] 'original-kill-line)
    (define-key map [home] 'smart-move-beginning)
    (define-key map [end]  'smart-move-end)
    map))

(define-minor-mode visual-line-mode
  "Define key binding for visual line moves."
  :keymap visual-line-map
  :group 'convenience)

(defun maybe-turn-on-visual-line-mode ()
  (visual-line-mode 1))

(define-globalized-minor-mode global-visual-line-mode
  visual-line-mode maybe-turn-on-visual-line-mode
  :lighter " vl")

(provide 'from-osxkeys)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; from-osxkeys.el ends here

;;(defalias 'original-next-line 'next-line "test")
;;(defalias 'original-previous-line 'previous-line "test")
;;
;; (defun line-is-wrapped ()
;;   (let ((here (point))
;;         res)
;;     (vertical-motion 0)
;;     (setq res (/= (line-beginning-position) (point)))
;;     (unless res
;;       (let ((line-end-pos (line-end-position)))
;;         (vertical-motion 1)
;;         (setq res (/= line-end-pos (- (point) 1)))))
;;     (goto-char here)
;;     res))

;; (defun next-wrapped-line-or-paragraph (&optional arg try-vscroll)
;;   (interactive)
;;   (let ((bpos (line-beginning-position)))
;;     (visual-line-down 1)
;;     (if (= bpos (line-beginning-position))
;;         (next-line arg try-vscroll)
;;       (forward-paragraph arg))))

;; (defun previous-wrapped-line-or-paragraph (&optional arg try-vscroll)
;;   (interactive)
;;   (let ((bpos (line-beginning-position)))
;;     (visual-line-up 1)
;;     (if (= bpos (line-beginning-position))
;;         (previous-line arg try-vscroll)
;;       (backward-paragraph arg))))

