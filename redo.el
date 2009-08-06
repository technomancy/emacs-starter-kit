;;; redo.el -- Redo/undo system for XEmacs

;; Copyright (C) 1985, 1986, 1987, 1993-1995 Free Software Foundation, Inc.
;; Copyright (C) 1995 Tinker Systems and INS Engineering Corp.
;; Copyright (C) 1997 Kyle E. Jones

;; Author: Kyle E. Jones, February 1997
;; Keywords: lisp, extensions

;; This file is part of XEmacs.

;; XEmacs is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; XEmacs is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with XEmacs; see the file COPYING.  If not, write to the Free
;; Software Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
;; 02111-1307, USA.

;;; Synched up with: Not in FSF.

;;; Commentary:

;; Derived partly from lisp/prim/simple.el in XEmacs.

;; Emacs' normal undo system allows you to undo an arbitrary
;; number of buffer changes.  These undos are recorded as ordinary
;; buffer changes themselves.  So when you break the chain of
;; undos by issuing some other command, you can then undo all
;; the undos.  The chain of recorded buffer modifications
;; therefore grows without bound, truncated only at garbage
;; collection time.
;;
;; The redo/undo system is different in two ways:
;;   1. The undo/redo command chain is only broken by a buffer
;;      modification.  You can move around the buffer or switch
;;      buffers and still come back and do more undos or redos.
;;   2. The `redo' command rescinds the most recent undo without
;;      recording the change as a _new_ buffer change.  It
;;      completely reverses the effect of the undo, which
;;      includes making the chain of buffer modification records
;;      shorter by one, to counteract the effect of the undo
;;      command making the record list longer by one.
;;
;; Installation:
;;
;; Save this file as redo.el, byte compile it and put the
;; resulting redo.elc file in a directory that is listed in
;; load-path.
;;
;; In your .emacs file, add
;;   (require 'redo)
;; and the system will be enabled.

;;; Code:

(provide 'redo)

(defvar redo-version "1.02"
  "Version number for the Redo package.")

(defvar last-buffer-undo-list nil
  "The head of buffer-undo-list at the last time an undo or redo was done.")
(make-variable-buffer-local 'last-buffer-undo-list)

(make-variable-buffer-local 'pending-undo-list)

;; Emacs 20 variable
(defvar undo-in-progress)

(defun redo (&optional count)
  "Redo the the most recent undo.
Prefix arg COUNT means redo the COUNT most recent undos.
If you have modified the buffer since the last redo or undo,
then you cannot redo any undos before then."
  (interactive "*p")
  (if (eq buffer-undo-list t)
      (error "No undo information in this buffer"))
  (if (eq last-buffer-undo-list nil)
      (error "No undos to redo"))
  (or (eq last-buffer-undo-list buffer-undo-list)
      ;; skip one undo boundary and all point setting commands up
      ;; until the next undo boundary and try again.
      (let ((p buffer-undo-list))
	(and (null (car-safe p)) (setq p (cdr-safe p)))
	(while (and p (integerp (car-safe p)))
	  (setq p (cdr-safe p)))
	(eq last-buffer-undo-list p))
      (error "Buffer modified since last undo/redo, cannot redo"))
  (and (or (eq buffer-undo-list pending-undo-list)
	   (eq (cdr buffer-undo-list) pending-undo-list))
       (error "No further undos to redo in this buffer"))
  (or (eq (selected-window) (minibuffer-window))
      (message "Redo..."))
  (let ((modified (buffer-modified-p))
	(undo-in-progress t)
	(recent-save (recent-auto-save-p))
	(old-undo-list buffer-undo-list)
	(p (cdr buffer-undo-list))
	(records-between 0))
    ;; count the number of undo records between the head of the
    ;; undo chain and the pointer to the next change.  Note that
    ;; by `record' we mean clumps of change records, not the
    ;; boundary records.  The number of records will always be a
    ;; multiple of 2, because an undo moves the pending pointer
    ;; forward one record and prepend a record to the head of the
    ;; chain.  Thus the separation always increases by two.  When
    ;; we decrease it we will decrease it by a multiple of 2
    ;; also.
    (while p
      (cond ((eq p pending-undo-list)
	     (setq p nil))
	    ((null (car p))
	     (setq records-between (1+ records-between))
	     (setq p (cdr p)))
	    (t
	     (setq p (cdr p)))))
    ;; we're off by one if pending pointer is nil, because there
    ;; was no boundary record in front of it to count.
    (and (null pending-undo-list)
	 (setq records-between (1+ records-between)))
    ;; don't allow the user to redo more undos than exist.
    ;; only half the records between the list head and the pending
    ;; pointer are undos that are a part of this command chain.
    (setq count (min (/ records-between 2) count)
	  p (primitive-undo (1+ count) buffer-undo-list))
    (if (eq p old-undo-list)
	nil ;; nothing happened
      ;; set buffer-undo-list to the new undo list.  if has been
      ;; shortened by `count' records.
      (setq buffer-undo-list p)
      ;; primitive-undo returns a list without a leading undo
      ;; boundary.  add one.
      (undo-boundary)
      ;; now move the pending pointer backward in the undo list
      ;; to reflect the redo.  sure would be nice if this list
      ;; were doubly linked, but no... so we have to run down the
      ;; list from the head and stop at the right place.
      (let ((n (- records-between count)))
	(setq p (cdr old-undo-list))
	(while (and p (> n 0))
	  (if (null (car p))
	      (setq n (1- n)))
	  (setq p (cdr p)))
	(setq pending-undo-list p)))
    (and modified (not (buffer-modified-p))
	 (delete-auto-save-file-if-necessary recent-save))
    (or (eq (selected-window) (minibuffer-window))
	(message "Redo!"))
    (setq last-buffer-undo-list buffer-undo-list)))

(defun undo (&optional arg)
  "Undo some previous changes.
Repeat this command to undo more changes.
A numeric argument serves as a repeat count."
  (interactive "*p")
  (let ((modified (buffer-modified-p))
	(recent-save (recent-auto-save-p)))
    (or (eq (selected-window) (minibuffer-window))
	(message "Undo..."))
    (or (eq last-buffer-undo-list buffer-undo-list)
	;; skip one undo boundary and all point setting commands up
	;; until the next undo boundary and try again.
	(let ((p buffer-undo-list))
	  (and (null (car-safe p)) (setq p (cdr-safe p)))
	  (while (and p (integerp (car-safe p)))
	    (setq p (cdr-safe p)))
	  (eq last-buffer-undo-list p))
	(progn (undo-start)
	       (undo-more 1)))
    (undo-more (or arg 1))
    ;; Don't specify a position in the undo record for the undo command.
    ;; Instead, undoing this should move point to where the change is.
    ;;
    ;;;; The old code for this was mad!  It deleted all set-point
    ;;;; references to the position from the whole undo list,
    ;;;; instead of just the cells from the beginning to the next
    ;;;; undo boundary.  This does what I think the other code
    ;;;; meant to do.
    (let ((list buffer-undo-list)
    	  (prev nil))
      (while (and list (not (null (car list))))
    	(if (integerp (car list))
    	    (if prev
    		(setcdr prev (cdr list))
    	      ;; impossible now, but maybe not in the future 
    	      (setq buffer-undo-list (cdr list))))
    	(setq prev list
    	      list (cdr list))))
    (and modified (not (buffer-modified-p))
	 (delete-auto-save-file-if-necessary recent-save)))
  (or (eq (selected-window) (minibuffer-window))
      (message "Undo!"))
  (setq last-buffer-undo-list buffer-undo-list))

;;; redo.el ends here
