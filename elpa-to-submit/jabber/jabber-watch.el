;; jabber-watch.el - get notified when certain persons go online

;; Copyright (C) 2004 - Mathias Dahl
;; Copyright (C) 2004 - Magnus Henoch - mange@freemail.hu

;; This file is a part of jabber.el.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

(require 'jabber-util)

(defvar jabber-watch-alist nil
  "Alist of buddies for which an extra notification should be sent
when they come online, with comment strings as values.")

(defun jabber-presence-watch (who oldstatus newstatus
				  statustext proposed-alert)
  "Checks if one of your extra-important buddies comes online and
sends a message if that happens. The buddies are stored in
`jabber-watch-alist' and are added and removed by calling
`jabber-watch-add' and `jabber-watch-remove.'"
  ;; check that buddy was previously offline and now online
  (if (and (null oldstatus)
           (not (null newstatus)))
      (let ((entry (assq who jabber-watch-alist)))
	(when entry
	  ;; Give an intrusive message.  With a window system,
	  ;; that's easy.
	  (if window-system
	      (message-box "%s%s" proposed-alert
			   (if (cdr entry) (format ": %s" (cdr entry)) ""))
	    ;; Without a window system, yes-or-no-p should be
	    ;; sufficient.
	    (while (not
		    (yes-or-no-p (format "%s%s  Got that? " proposed-alert
					 (if (cdr entry) (format ": %s" (cdr entry)) ""))))))))))

(defun jabber-watch-add (buddy &optional comment)
  (interactive (list (jabber-read-jid-completing "Add buddy to watch list: ")
		     (read-string "Comment: ")))
  (unless (memq 'jabber-presence-watch jabber-presence-hooks)
    (error "jabber-presence-watch is not in jabber-presence-hooks"))
  (add-to-list 'jabber-watch-alist (cons
				    (jabber-jid-symbol buddy)
				    (and (not (zerop (length comment)))
					 comment))))

(defun jabber-watch-remove (buddy)
  (interactive
   (list (jabber-read-jid-completing "Remove buddy from watch list: "
				     (or (mapcar 'car jabber-watch-alist)
					 (error "Watch list is empty"))
				     t)))
  (setq jabber-watch-alist
        (delq (assq (jabber-jid-symbol buddy) jabber-watch-alist)
	      jabber-watch-alist)))

(provide 'jabber-watch)

;; arch-tag: c27299d8-019e-44b5-9529-d67b8682be23
