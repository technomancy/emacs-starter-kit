;;; org-depend.el --- TODO dependencies for Org-mode
;; Copyright (C) 2008 Free Software Foundation, Inc.
;;
;; Author: Carsten Dominik <carsten at orgmode dot org>
;; Keywords: outlines, hypermedia, calendar, wp
;; Homepage: http://orgmode.org
;; Version: 0.08
;;
;; This file is not part of GNU Emacs.
;;
;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; WARNING: This file is just a PROOF OF CONCEPT, not a supported part
;;          of Org-mode.
;;
;; This is an example implementation of TODO dependencies in Org-mode.
;; It uses the new hooks in version 5.13 of Org-mode,
;; `org-trigger-hook' and `org-blocker-hook'.
;;
;; It implements the following:
;;
;; Triggering
;; ----------
;;
;; 1) If an entry contains a TRIGGER property that contains the string
;;    "chain-siblings(KEYWORD)", then switching that entry to DONE does
;;    do the following:
;;    - The sibling following this entry switched to todo-state KEYWORD.
;;    - The sibling also gets a TRIGGER property "chain-sibling(KEYWORD)",
;;      property, to make sure that, when *it* is DONE, the chain will
;;      continue.
;;
;; 2) If an entry contains a TRIGGER property that contains the string
;;    "chain-siblings-scheduled", then switching that entry to DONE does
;;    the following actions, similarly to "chain-siblings(KEYWORD)":
;;    - The sibling receives the same scheduled time as the entry
;;      marked as DONE (or, in the case, in which there is no scheduled
;;      time, the sibling does not get any either).
;;    - The sibling also gets the same TRIGGER property
;;      "chain-siblings-scheduled", so the chain can continue.
;;
;; 3) If the TRIGGER property contains any other words like
;;    XYZ(KEYWORD), these are treated as entry id's with keywords.  That
;;    means Org-mode will search for an entry with the ID property XYZ
;;    and switch that entry to KEYWORD as well.
;;
;; Blocking
;; --------
;;
;; 1) If an entry contains a BLOCKER property that contains the word
;;    "previous-sibling", the sibling above the current entry is
;;    checked when you try to mark it DONE.  If it is still in a TODO
;;    state, the current state change is blocked.
;;
;; 2) If the BLOCKER property contains any other words, these are
;;    treated as entry id's.  That means Org-mode will search for an
;;    entry with the ID property exactly equal to this word.  If any
;;    of these entries is not yet marked DONE, the current state change
;;    will be blocked.
;;
;; 3) Whenever a state change is blocked, an org-mark is pushed, so that
;;    you can find the offending entry with `C-c &'.
;;
;;; Example:
;;
;; When trying this example, make sure that the settings for TODO keywords
;; have been activated, i.e. include the following line and press C-c C-c
;; on the line before working with the example:
;;
;; #+TYP_TODO: TODO NEXT | DONE
;;
;; * TODO Win a million in Las Vegas
;;   The "third" TODO (see above) cannot become a TODO without this money.
;;
;;   :PROPERTIES:
;;     :ID: I-cannot-do-it-without-money
;;   :END:
;;
;; * Do this by doing a chain of TODO's
;; ** NEXT This is the first in this chain
;;    :PROPERTIES:
;;      :TRIGGER: chain-siblings(NEXT)
;;    :END:
;; 
;; ** This is the second in this chain
;;
;; ** This is the third in this chain
;;    :PROPERTIES:
;;      :BLOCKER: I-cannot-do-it-without-money
;;    :END:
;;
;; ** This is the forth in this chain
;;    When this is DONE, we will also trigger entry XYZ-is-my-id
;;   :PROPERTIES:
;;     :TRIGGER: XYZ-is-my-id(TODO)
;;   :END:
;;
;; ** This is the fifth in this chain
;; 
;; * Start writing report
;;   :PROPERTIES:
;;     :ID: XYZ-is-my-id
;;   :END:
;;
;;

(require 'org)

(defcustom org-depend-tag-blocked t
  "Whether to indicate blocked TODO items by a special tag."
  :group 'org
  :type 'boolean)

(defmacro org-depend-act-on-sibling (trigger-val &rest rest)
  "Perform a set of actions on the next sibling, if it exists,
copying the sibling spec TRIGGER-VAL to the next sibling."
  `(catch 'exit
     (save-excursion
       (goto-char pos)
       ;; find the sibling, exit if no more siblings
       (condition-case nil
           (outline-forward-same-level 1)
         (error (throw 'exit t)))
       ;; mark the sibling TODO
       ,@rest
       ;; make sure the sibling will continue the chain
       (org-entry-add-to-multivalued-property
        nil "TRIGGER" ,trigger-val))))

(defun org-depend-trigger-todo (change-plist)
  "Trigger new TODO entries after the current is switched to DONE.
This does two different kinds of triggers:

- If the current entry contains a TRIGGER property that contains
  \"chain-siblings(KEYWORD)\", it goes to the next sibling, marks it
  KEYWORD and also installs the \"chain-sibling\" trigger to continue
  the chain.
- If the current entry contains a TRIGGER property that contains
  \"chain-siblings-scheduled\", we go to the next sibling and copy
  the scheduled time from the current task, also installing the property
  in the sibling.
- Any other word (space-separated) like XYZ(KEYWORD) in the TRIGGER
  property is seen as an entry id.  Org-mode finds the entry with the
  corresponding ID property and switches it to the state TODO as well."

  ;; Get information from the plist
  (let* ((type (plist-get change-plist :type))
	       (pos (plist-get change-plist :position))
	 (from (plist-get change-plist :from))
	 (to (plist-get change-plist :to))
	 (org-log-done nil) ; IMPROTANT!: no logging during automatic trigger!
	 trigger triggers tr p1 kwd)
    (catch 'return
      (unless (eq type 'todo-state-change)
	;; We are only handling todo-state-change....
	(throw 'return t))
      (unless (and (member from org-not-done-keywords)
		   (member to org-done-keywords))
	;; This is not a change from TODO to DONE, ignore it
	(throw 'return t))

      ;; OK, we just switched from a TODO state to a DONE state
      ;; Lets see if this entry has a TRIGGER property.
      ;; If yes, split it up on whitespace.
      (setq trigger (org-entry-get pos "TRIGGER")
	    triggers (and trigger (org-split-string trigger "[ \t]+")))

      ;; Go through all the triggers
      (while (setq tr (pop triggers))
	(cond
	 ((string-match "\\`chain-siblings(\\(.*?\\))\\'" tr)
	  ;; This is a TODO chain of siblings
	  (setq kwd (match-string 1 tr))
          (org-depend-act-on-sibling (format "chain-siblings(%s)" kwd)
                                     (org-todo kwd)))

	 ((string-match "\\`\\(\\S-+\\)(\\(.*?\\))\\'" tr)
	  ;; This seems to be ENTRY_ID(KEYWORD)
	  (setq id (match-string 1 tr)
		kwd (match-string 2 tr)
		p1 (org-find-entry-with-id id))
	  (when p1
	    ;; there is an entry with this ID, mark it TODO
	    (save-excursion
	      (goto-char p1)
	      (org-todo kwd))))
         ((string-match "\\`chain-siblings-scheduled\\'" tr)
          (let ((time (org-get-scheduled-time pos)))
            (when time
              (org-depend-act-on-sibling
               "chain-siblings-scheduled"
               (org-schedule nil time))))))))))

(defun org-depend-block-todo (change-plist)
  "Block turning an entry into a TODO.
This checks for a BLOCKER property in an entry and checks
all the entries listed there.  If any of them is not done,
block changing the current entry into a TODO entry.  If the property contains
the word \"previous-sibling\", the sibling above the current entry is checked.
Any other words are treated as entry id's. If an entry exists with the
this ID property, that entry is also checked."
  ;; Get information from the plist
  (let* ((type (plist-get change-plist :type))
	       (pos (plist-get change-plist :position))
	 (from (plist-get change-plist :from))
	 (to (plist-get change-plist :to))
	 (org-log-done nil) ; IMPROTANT!: no logging during automatic trigger
	 blocker blockers bl p1
	 (proceed-p
	  (catch 'return
            ;; If this is not a todo state change, or if this entry is
            ;; DONE, do not block
            (when (or (not (eq type 'todo-state-change))
                      (member from (cons 'done org-done-keywords))
                      (member to (cons 'todo org-not-done-keywords))
                      (not to))
              (throw 'return t))

	    ;; OK, the plan is to switch from nothing to TODO
	    ;; Lets see if we will allow it.  Find the BLOCKER property
	    ;; and split it on whitespace.
	    (setq blocker (org-entry-get pos "BLOCKER")
		  blockers (and blocker (org-split-string blocker "[ \t]+")))
	    
	    ;; go through all the blockers
	    (while (setq bl (pop blockers))
	      (cond
	       ((equal bl "previous-sibling")
		;; the sibling is required to be DONE.
		(catch 'ignore
		  (save-excursion
		    (goto-char pos)
		    ;; find the older sibling, exit if no more siblings
		    (condition-case nil
			(outline-backward-same-level 1)
		      (error (throw 'ignore t)))
		    ;; Check if this entry is not yet done and block
		    (unless (org-entry-is-done-p)
		      ;; return nil, to indicate that we block the change!
		      (org-mark-ring-push)
		      (throw 'return nil)))))

	       ((setq p1 (org-find-entry-with-id bl))
		;; there is an entry with this ID, check it out
		(save-excursion
		  (goto-char p1)
		  (unless (org-entry-is-done-p)
		    ;; return nil, to indicate that we block the change!
		    (org-mark-ring-push)
		    (throw 'return nil))))))
	    t ; return t to indicate that we are not blocking
	    )))
    (when org-depend-tag-blocked
      (org-toggle-tag "blocked" (if proceed-p 'off 'on)))
    
    proceed-p))

(add-hook 'org-trigger-hook 'org-depend-trigger-todo)
(add-hook 'org-blocker-hook 'org-depend-block-todo)

(provide 'org-depend)

;;; org-depend.el ends here
