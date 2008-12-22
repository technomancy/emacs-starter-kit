;;; outline-magic.el --- outline mode extensions for Emacs

;; Copyright (C) 2002 Carsten Dominik <dominik@science.uva.nl>

;; Maintainer: Carsten Dominik <dominik@science.uva.nl>
;; Version: 0.9
;; Keywords: outlines

;; This file is not part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; This file implements extensions for outline(-minor)-mode.
;;
;; - VISIBILITY CYCLING: A *single* command to replace the many
;;   outline commands for showing and hiding parts of a document.
;;
;; - STRUCTURE EDITING: Promotion, demotion and transposition of subtrees.
;;
;; Installation
;; ============
;;
;; Byte-compile outline-magic.el, put it on the load path and copy the
;; following into .emacs (adapting keybindings to your own preferences)
;;
;; (add-hook 'outline-mode-hook
;;           (lambda ()
;;             (require 'outline-cycle)))
;;
;; (add-hook 'outline-minor-mode-hook
;;           (lambda ()
;;             (require 'outline-magic)
;;             (define-key outline-minor-mode-map [(f10)] 'outline-cycle)))
;;
;; Usage
;; =====
;;
;; Visibility cycling
;; ------------------
;;
;; The command `outline-cycle' changes the visibility of text and headings
;; in the buffer.  Instead of using many different commands to show and
;; hide buffer parts, `outline-cycle' cycles through the most important
;; states of an outline buffer.  In the major `outline-mode', it will be
;; bound to the TAB key.  In `outline-minor-mode', the user can choose a
;; different keybinding.  The action of the command depends on the current
;; cursor location:
;;
;; 1. When point is at the beginning of the buffer, `outline-cycle'
;;    cycles the entire buffer through 3 different states:
;;      - OVERVIEW: Only top-level headlines are shown.
;;      - CONTENTS: All headlines are shown, but no body text.
;;      - SHOW ALL: Everything is shown.
;;
;; 2. When point in a headline, `outline-cycle' cycles the subtree started
;;    by this line through the following states:
;;      - FOLDED:   Only the headline is shown.
;;      - CHILDREN: The headline and its direct children are shown.  From
;;                  this state, you can move to one of the children and
;;                  zoom in further.
;;      - SUBTREE:  The entire subtree under the heading is shown.
;;
;; 3. At other positions, `outline-cycle' jumps back to the current heading.
;;    It can also be configured to emulate TAB at those positions, see
;;    the option `outline-cycle-emulate-tab'.
;;
;; Structure editing
;; -----------------
;;
;; Four commands are provided for structure editing.  The commands work on
;; the current subtree (the current headline plus all inferior ones). In
;; addition to menu access, the commands are assigned to the four arrow
;; keys pressed with a modifier (META by default) in the following way:
;;
;;                                 move up
;;                                    ^
;;                        promote  <- | ->  demote
;;                                    v
;;                                move down
;;
;; Thus, M-left will promote a subtree, M-up will move it up
;; vertically throught the structure.  Configure the variable
;; `outline-structedit-modifiers' to use different modifier keys.
;;
;; Moving subtrees
;; - - - - - - - -
;; The commands `outline-move-subtree-up' and `outline-move-subtree-down'
;; move the entire current subtree (folded or not) past the next same-level
;; heading in the given direction.  The cursor moves with the subtree, so
;; these commands can be used to "drag" a subtree to the wanted position.
;; For example, `outline-move-subtree-down' applied with the cursor at the
;; beginning of the "* Level 1b" line will change the tree like this:
;;
;;   * Level 1a                         * Level 1a
;;   * Level 1b         ===\            * Level 1c
;;   ** Level 2b        ===/            * Level 1b
;;   * Level 1c                         ** Level 2b
;;
;; Promotion/Demotion
;; - - - - - - - - - -
;; The commands `outline-promote' and `outline-demote' change the current
;; subtree to a different outline level - i.e. the level of all headings in
;; the tree is decreased or increased.  For example, `outline-demote'
;; applied with the cursor at the beginning of the "* Level 1b" line will
;; change the tree like this:
;;
;;   * Level 1a                         * Level 1a
;;   * Level 1b         ===\            ** Level 1b
;;   ** Level 2b        ===/            *** Level 2
;;   * Level 1c                         * Level 1c
;;
;; The reverse operation is `outline-promote'.  Note that the scope of
;; "current subtree" may be changed after a promotion.  To change all
;; headlines in a region, use transient-mark-mode and apply the command to
;; the region.
;;
;; NOTE: Promotion/Demotion in complex outline setups
;; - - - - - - - - - - - - - - - - - - - - - - - - - -
;; Promotion/demotion works easily in a simple outline setup where the
;; indicator of headings is just a polymer of a single character (e.g. "*"
;; in the default outline mode).  It can also work in more complicated
;; setups.  For example, in LaTeX-mode, sections can be promoted to
;; chapters and vice versa.  However, the outline setup for the mode must
;; meet two requirements:
;;
;; 1. `outline-regexp' must match the full text which has to be changed
;;    during promotion/demotion.  E.g. for LaTeX, it must match "\chapter"
;;    and not just "\chap".  Major modes like latex-mode, AUCTeX's
;;    latex-mode and texinfo-mode do this correctly.
;;
;; 2. The variable `outline-promotion-headings' must contain a sorted list
;;    of headings as matched by `outline-regexp'.  Each of the headings in
;;    `outline-promotion-headings' must be matched by `outline-regexp'.
;;    `outline-regexp' may match additional things - those matches will be
;;    ignored by the promotion commands.  If a mode has multiple sets of
;;    sectioning commands (for example the texinfo-mode with
;;    chapter...subsubsection and unnumbered...unnumberedsubsubsec), the
;;    different sets can all be listed in the same list, but must be
;;    separated by nil elements to avoid "promotion" accross sets.
;;    Examples:
;;
;;    (add-hook 'latex-mode-hook      ; or 'LaTeX-mode-hook for AUCTeX
;;     (lambda ()
;;       (setq outline-promotion-headings
;;             '("\\chapter" "\\section" "\\subsection"
;;               "\\subsubsection" "\\paragraph" "\\subparagraph"))))
;;
;;    (add-hook 'texinfo-mode-hook
;;     (lambda ()
;;      (setq outline-promotion-headings
;;       '("@chapter" "@section" "@subsection" "@subsubsection" nil
;;         "@unnumbered" "@unnumberedsec" "@unnumberedsubsec"
;;                                       "@unnumberedsubsubsec" nil
;;         "@appendix" "@appendixsec" "@appendixsubsec"
;;                                         "@appendixsubsubsec" nil
;;         "@chapheading" "@heading" "@subheading" "@subsubheading"))))
;;
;;    If people find this useful enough, maybe the maintainers of the
;;    modes can be persuaded to set `outline-promotion-headings'
;;    already as part of the mode setup.
;;
;;  Compatibility:
;;  --------------
;;  outline-magic was developed to work with the new outline.el
;;  implementation which uses text properties instead of selective display.
;;  If you are using XEmacs which still has the old implementation, most
;;  commands will work fine.  However, structure editing commands will
;;  require all relevant headlines to be visible.
;;
;;  History
;;  -------
;;  - Before first header now works as at beginning of file
;;  - Two levels are shown for contents.
;;
;;; Code:

(require 'outline)

;;; Visibility cycling

(defcustom outline-cycle-emulate-tab nil
  "Where should `outline-cycle' emulate TAB.
nil    Never
white  Only in completely white lines
t      Everywhere except in headlines"
  :group 'outlines
  :type '(choice (const :tag "Never" nil)
		 (const :tag "Only in completely white lines" white)
		 (const :tag "Everywhere except in headlines" t)
		 ))

(defvar outline-promotion-headings nil
  "A sorted list of headings used for promotion/demotion commands.
Set this to a list of headings as they are matched by `outline-regexp',
top-level heading first.  If a mode or document needs several sets of
outline headings (for example numbered and unnumbered sections), list
them set by set, separated by a nil element.  See the example for
`texinfo-mode' in the file commentary.")
(make-variable-buffer-local 'outline-promotion-headings)

(defun outline-cycle (&optional arg)
  "Visibility cycling for outline(-minor)-mode.

- When point is at the beginning of the buffer, or when called with a
  C-u prefix argument, rotate the entire buffer through 3 states:
  1. OVERVIEW: Show only top-level headlines.
  2. CONTENTS: Show all headlines of all levels, but no body text.
  3. SHOW ALL: Show everything.

- When point is at the beginning of a headline, rotate the subtree started
  by this line through 3 different states:
  1. FOLDED:   Only the main headline is shown.
  2. CHILDREN: The main headline and the direct children are shown.  From
               this state, you can move to one of the children and
               zoom in further.
  3. SUBTREE:  Show the entire subtree, including body text.

- When point is not at the beginning of a headline, execute
  `indent-relative', like TAB normally does."
  (interactive "P")
  (setq deactivate-mark t)
  (cond

   ((equal arg '(4))
    ; Run `outline-cycle' as if at the top of the buffer.
    (save-excursion
      (goto-char (point-min))
      (outline-cycle nil)))

   (t
    (cond
     ((or (bobp) ;; Beginning of buffer: Global cycling
          (let ((here (point))
                (atbobp t))
            (condition-case err
                (progn
                  (outline-back-to-heading)
                  (setq atbobp nil))
              (error nil))
            atbobp))

      (cond
       ((eq last-command 'outline-cycle-overview)
	;; We just created the overview - now do table of contents
	;; This can be slow in very large buffers, so indicate action
	(message "CONTENTS...")
	(save-excursion
	  ;; Visit all headings and show their offspring
	  (goto-char (point-max))
	  (catch 'exit
	    (while (and (progn (condition-case nil
				   (outline-previous-visible-heading 1)
				 (error (goto-char (point-min))))
			       t)
			(looking-at outline-regexp))
	      (show-branches)
	      (if (bobp) (throw 'exit nil))))
	  (message "CONTENTS...done"))
	(setq this-command 'outline-cycle-toc))
       ((eq last-command 'outline-cycle-toc)
	;; We just showed the table of contents - now show everything
	(show-all)
	(message "SHOW ALL")
	(setq this-command 'outline-cycle-showall))
       (t
	;; Default action: go to overview
        ;; FIX-ME: variable sublevel here (for wikipedia for example):
	(hide-sublevels 2)
	(message "OVERVIEW")
	(setq this-command 'outline-cycle-overview))))

     ((save-excursion (beginning-of-line 1) (looking-at outline-regexp))
      ;; At a heading: rotate between three different views
      (outline-back-to-heading)
      (let ((goal-column 0) beg eoh eol eos)
	;; First, some boundaries
	(save-excursion
	  (outline-back-to-heading)           (setq beg (point))
	  (save-excursion (outline-next-line) (setq eol (point)))
	  (outline-end-of-heading)            (setq eoh (point))
	  (outline-end-of-subtree)            (setq eos (point)))
	;; Find out what to do next and set `this-command'
	(cond
	 ((= eos eoh)
	  ;; Nothing is hidden behind this heading
	  (message "EMPTY ENTRY"))
	 ((>= eol eos)
	  ;; Entire subtree is hidden in one line: open it
	  (show-entry)
	  (show-children)
	  (message "CHILDREN")
	  (setq this-command 'outline-cycle-children))
	 ((eq last-command 'outline-cycle-children)
	  ;; We just showed the children, now show everything.
	  (show-subtree)
	  (message "SUBTREE"))
	 (t
	  ;; Default action: hide the subtree.
	  (hide-subtree)
	  (message "FOLDED")))))

     ;; TAB emulation
     ((outline-cycle-emulate-tab)
      (indent-relative))

     (t
      ;; Not at a headline: Do indent-relative
      (outline-back-to-heading))))))

(defun outline-cycle-emulate-tab ()
  "Check if TAB should be emulated at the current position."
  ;; This is called after the check for point in a headline,
  ;; so we can assume we are not in a headline
  (if (and (eq outline-cycle-emulate-tab 'white)
	   (save-excursion
	     (beginning-of-line 1) (looking-at "[ \t]+$")))
      t
    outline-cycle-emulate-tab))

(defun outline-next-line ()
  "Forward line, but mover over invisible line ends.
Essentially a much simplified version of `next-line'."
  (interactive)
  (beginning-of-line 2)
  (while (and (not (eobp))
	      (get-char-property (1- (point)) 'invisible))
    (beginning-of-line 2)))

;;; Vertical tree motion

(defun outline-move-subtree-up (&optional arg)
  "Move the currrent subtree up past ARG headlines of the same level."
  (interactive "p")
  (outline-move-subtree-down (- arg)))

(defun outline-move-subtree-down (&optional arg)
  "Move the currrent subtree down past ARG headlines of the same level."
  (interactive "p")
  (let ((re (concat "^" outline-regexp))
	(movfunc (if (> arg 0) 'outline-get-next-sibling
		   'outline-get-last-sibling))
	(ins-point (make-marker))
	(cnt (abs arg))
	beg end txt)
    ;; Select the tree
    (outline-back-to-heading)
    (setq beg (point))
    (outline-end-of-subtree)
    (if (= (char-after) ?\n) (forward-char 1))
    (setq end (point))
    ;; Find insertion point, with error handling
    (goto-char beg)
    (while (> cnt 0)
      (or (funcall movfunc)
	  (progn (goto-char beg)
		 (error "Cannot move past superior level")))
      (setq cnt (1- cnt)))
    (if (> arg 0)
	;; Moving forward - still need to move over subtree
	(progn (outline-end-of-subtree)
	       (if (= (char-after) ?\n) (forward-char 1))))
    (move-marker ins-point (point))
    (setq txt (buffer-substring beg end))
    (delete-region beg end)
    (insert txt)
    (goto-char ins-point)
    (move-marker ins-point nil)))

;;; Promotion and Demotion

(defun outline-promote (&optional arg)
  "Decrease the level of an outline-structure by ARG levels.
When the region is active in transient-mark-mode, all headlines in the
region are changed.  Otherwise the current subtree is targeted. Note that
after each application of the command the scope of \"current subtree\"
may have changed."
  (interactive "p")
  (outline-change-level (- arg)))


(defun outline-demote (&optional arg)
  "Increase the level of an outline-structure by ARG levels.
When the region is active in transient-mark-mode, all headlines in the
region are changed.  Otherwise the current subtree is targeted. Note that
after each application of the command the scope of \"current subtree\"
may have changed."
  (interactive "p")
  (outline-change-level arg))

(defun outline-change-level (delta)
  "Workhorse for `outline-demote' and `outline-promote'."
  (let* ((headlist (outline-headings-list))
	 (atom (outline-headings-atom headlist))
	 (re (concat "^" outline-regexp))
	 (transmode (and transient-mark-mode mark-active))
	 beg end)

    ;; Find the boundaries for this operation
    (save-excursion
      (if transmode
	  (setq beg (min (point) (mark))
		end (max (point) (mark)))
	(outline-back-to-heading)
	(setq beg (point))
	(outline-end-of-heading)
	(outline-end-of-subtree)
	(setq end (point)))
      (setq beg (move-marker (make-marker) beg)
	    end (move-marker (make-marker) end))

      (let (head newhead level newlevel static)

	;; First a dry run to test if there is any trouble ahead.
	(goto-char beg)
	(while (re-search-forward re end t)
	  (outline-change-heading headlist delta atom 'test))

	;; Now really do replace the headings
	(goto-char beg)
	(while (re-search-forward re end t)
	  (outline-change-heading headlist delta atom))))))

(defun outline-headings-list ()
  "Return a list of relevant headings, either a user/mode defined
list, or an alist derived from scanning the buffer."
  (let (headlist)
    (cond
     (outline-promotion-headings
      ;; configured by the user or the mode
      (setq headlist outline-promotion-headings))

     ((and (eq major-mode 'outline-mode) (string= outline-regexp "[*\^L]+"))
      ;; default outline mode with original regexp
      ;; this need special treatment because of the \f in the regexp
      (setq headlist '(("*" . 1) ("**" . 2))))  ; will be extrapolated

     (t ;; Check if the buffer contains a complete set of headings
      (let ((re (concat "^" outline-regexp)) head level)
	(save-excursion
	  (goto-char (point-min))
	  (while (re-search-forward re nil t)
	    (save-excursion
	      (beginning-of-line 1)
	      (setq head (outline-cleanup-match (match-string 0))
		    level (funcall outline-level))
	      (add-to-list  'headlist (cons head level))))))
      ;; Check for uniqueness of levels in the list
      (let* ((hl headlist) entry level seen nonunique)
	(while (setq entry (car hl))
	  (setq hl (cdr hl)
		level (cdr entry))
	  (if (and (not (outline-static-level-p level))
		   (member level seen))
	      ;; We have two entries for the same level.
	      (add-to-list 'nonunique level))
	  (add-to-list 'seen level))
	(if nonunique
	    (error "Cannot promote/demote: non-unique headings at level %s\nYou may want to configure `outline-promotion-headings'."
		   (mapconcat 'int-to-string nonunique ","))))))
    ;; OK, return the list
    headlist))

(defun outline-change-heading (headlist delta atom &optional test)
  "Change heading just matched by `outline-regexp' by DELTA levels.
HEADLIST can be either an alist ((\"outline-match\" . level)...) or a
straight list like `outline-promotion-headings'. ATOM is a character
if all headlines are composed of a single character.
If TEST is non-nil, just prepare the change and error if there are problems.
TEST nil means, really replace old heading with new one."
  (let* ((head (outline-cleanup-match (match-string 0)))
	 (level (save-excursion
		  (beginning-of-line 1)
		  (funcall outline-level)))
	 (newhead  ; compute the new head
	  (cond
	   ((= delta 0) t)
	   ((outline-static-level-p level) t)
	   ((null headlist) nil)
	   ((consp (car headlist))
	    ;; The headlist is an association list
	    (or (car (rassoc (+ delta level) headlist))
		(and atom
		     (> (+ delta level) 0)
		     (make-string (+ delta level) atom))))
	   (t
	    ;; The headlist is a straight list - grab the correct element.
	    (let* ((l (length headlist))
		   (n1 (- l (length (member head headlist)))) ; index old
		   (n2 (+ delta n1)))                         ; index new
	      ;; Careful checking
	      (cond
	       ((= n1 l) nil)                ; head not found
	       ((< n2 0) nil)                ; newlevel too low
	       ((>= n2 l) nil)               ; newlevel too high
	       ((let* ((tail (nthcdr (min n1 n2) headlist))
		       (nilpos (- (length tail) (length (memq nil tail)))))
		  (< nilpos delta))          ; nil element between old and new
		nil)
	       (t (nth n2 headlist))))))))      ; OK, we have a match!
    (if (not newhead)
	(error "Cannot shift level %d heading \"%s\" to level %d"
	       level head (+ level delta)))
    (if (and (not test) (stringp newhead))
	(save-excursion
	  (beginning-of-line 1)
	  (or (looking-at (concat "[ \t]*\\(" (regexp-quote head) "\\)"))
	      (error "Please contact maintainer"))
	  (replace-match newhead t t nil 1)))))

(defun outline-headings-atom (headlist)
  "Use the list created by `outline-headings-list' and check if all
headings are polymers of a single character, e.g. \"*\".
If yes, return this character."
  (if (consp (car headlist))
      ;; this is an alist - it makes sense to check for atomic structure
      (let ((re (concat "\\`"
			(regexp-quote (substring (car (car headlist)) 0 1))
			"+\\'")))
	(if (not (delq nil (mapcar (lambda (x) (not (string-match re (car x))))
				   headlist)))
	    (string-to-char (car (car headlist)))))))

(defun outline-cleanup-match (s)
  "Remove text properties and start/end whitespace from a string."
  (set-text-properties 1 (length s) nil s)
  (save-match-data
    (if (string-match "^[ \t]+" s) (setq s (replace-match "" t t s)))
    (if (string-match "[ \t]+$" s) (setq s (replace-match "" t t s))))
  s)

(defun outline-static-level-p (level)
  "Test if a level should not be changed by level promotion/demotion."
  (>= level 1000))

;;; Key bindings

(defcustom outline-structedit-modifiers '(meta)
  "List of modifiers for outline structure editing with the arrow keys."
  :group 'outlines
  :type '(repeat symbol))

(define-key outline-mode-map [(tab)] 'outline-cycle)
(let ((keys '((left . outline-promote)
	      (right . outline-demote)
	      (up . outline-move-subtree-up)
	      (down . outline-move-subtree-down)))
      key)
  (while (setq key (pop keys))
    (apply 'define-key outline-mode-map
	   (list
	    (vector (append outline-structedit-modifiers (list (car key))))
	    (cdr key)))))

;;; Menu entries

(define-key outline-mode-menu-bar-map [headings outline-move-subtree-down]
  '("Move subtree down" . outline-move-subtree-down))
(define-key outline-mode-menu-bar-map [headings outline-move-subtree-up]
  '("Move subtree up" . outline-move-subtree-up))
(define-key outline-mode-menu-bar-map [headings outline-demote]
  '("Demote by 1 level" . outline-demote))
(define-key outline-mode-menu-bar-map [headings outline-promote]
  '("Promote by 1 level" . outline-promote))
(define-key outline-mode-menu-bar-map [show outline-cycle]
  '("Rotate visibility" . outline-cycle))
(define-key outline-mode-menu-bar-map [hide outline-cycle]
  '("Rotate visibility" . outline-cycle))

;;; Finish up

(provide 'outline-magic)

;;; outline-magic.el ends here
