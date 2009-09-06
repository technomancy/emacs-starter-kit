;;; winsav.el --- Save and restore window structure
;;
;; Author: Lennart Borgman
;; Created: Sun Jan 14 2007
(defconst winsav:version "0.77") ;;Version: 0.77
;; Last-Updated: 2009-08-04 Tue
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
;; This library contains both user level commands and options and
;; functions for use in other elisp libraries.
;;
;;;; User level commands and options
;;
;; The user level commands and options are for saving frame, windows
;; and buffers between Emacs sessions.  To do that you can customize
;; the options `desktop-save-mode' and `winsav-save-mode' or put this
;; at the end of your .emacs:
;;
;;   (desktop-save-mode 1)
;;   (winsav-save-mode 1)
;;
;; You can also save configurations that you later switch between.
;; For more information see the command `winsav-save-mode'.
;;
;; (There is also a command in this library for rotating window
;; borders in a frame, `winsav-rotate'.  It is here just because the
;; needed support functions lives here.)
;;
;;
;;
;;;; Commands for other elisp libraries
;;
;; This library was orignally written to solve the problem of adding a
;; window to the left of some windows in a frame like the one below
;;
;; ___________
;; |    |    |
;; | 1  | 2  |
;; |____|____|
;; |         |
;; |    3    |
;; |_________|
;;
;; so that the window structure on the frame becomes
;;
;; ___________
;; |  |  |   |
;; |  | 1| 2 |
;; | B|__|___|
;; | A|      |
;; | R|  3   |
;; |__|______|
;;
;;
;; This problem can be solved by this library.  However the solution in
;; this library is a bit more general: You first copy the window
;; structure and then restore that into another window.  To do the
;; above you first copy the window structure in the first frame above
;; with `winsav-get-window-tree'.  Then you create windows like this:
;;
;; ___________
;; |  |      |
;; |  |      |
;; | B|      |
;; | A|      |
;; | R|      |
;; |__|______|
;;
;;
;; Finally you use `winsav-put-window-tree' to put the window
;; structure into the right window.  (Of course you could have put BAR
;; above, under etc.)
;;
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Bugs and limitations:
;;
;; Juanma Barranquero has pointed out there is a serious limitation in
;; this way of doing it when overlays with 'window properties are
;; used.  The problem is that any pointers to windows are made invalid
;; since they are deleted.  So in fact any code that relies on saved
;; pointers to windows will have problem if the window is one of those
;; that are involved here.
;;
;; To overcome this problem when doing something like inserting a BAR
;; window (see above) a new window has to be inserted in the existing
;; window tree on a frame in a way that is currently not supported in
;; Emacs.
;;
;; It would be nice to be have primitives to manipulate the window
;; tree more generally from elisp.  That requires implementation of
;; them at the C level of course.
;;
;; However it is probably much easier to implement it quite a bit less
;; general.  The concept of splitting is maybe then the right level to
;; search for primitives at.
;;
;; My conclusion is that it will take some time to find suitable
;; primitives for this.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change log:
;;
;; Version 0.72:
;;
;; - Format of window structure changed in Emacs 23. Adopted to that.
;; - Added save and restore of frame/window configurations between
;;   Emacs sessions.
;; - Added named winsav configurations for save and restore of frames,
;;   windows, buffers and files.
;;
;; Version 0.71:
;;
;; - Added rotation of window structure.
;;
;; Version 0.70:
;;
;; - Support for save and restore from file.
;;
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
;;; Code:


(eval-when-compile (require 'cl))
(eval-when-compile (require 'desktop))

;; (defun winsav-upper-left-window(&optional frame w)
;;   (let* ((tree (if w w (car (window-tree frame))))
;;          (is-split (not (windowp tree))))
;;     (if (not is-split)
;;         tree
;;       (winsav-upper-left-window frame (nth 2 tree)))))


(defcustom winsav-after-get-hook nil
  "Hook to run after at the end of `winsav-get-window-tree'.
The functions in this hook are called with one parameter which is
the same as the return value from the function above."
  :type 'hook
  :group 'winsav)

(defcustom winsav-after-put-hook nil
  "Hook to run after at the end of `winsav-put-window-tree'.
The functions in this hook are called with one parameter which is
a list where each element is a list \(old-win new-win) where
OLD-WIN are the window from `winsav-get-window-tree' and NEW-WIN
is the newly created corresponding window.  This list is the same
as the return value from the function above."
  :type 'hook
  :group 'winsav)

(defun winsav-get-window-tree(&optional frame)
  "Get window structure.
This returns an object with current windows with values, buffers,
points and the selected window.

FRAME is the frame to save structure from. If nil use selected.

At the very end of this function the hook `winsav-after-get' is
run."
  ;;(let* ((upper-left (winsav-upper-left-window frame))
  (let* ((upper-left (frame-first-window frame))
         (num -1)
         sel-num)
    (dolist (w (window-list frame nil upper-left))
      (setq num (1+ num))
      (when (eq w (selected-window))
        (setq sel-num num)))
    (let ((ret (list sel-num
                     (winsav-get-window-tree-1 frame nil))))
      (run-hook-with-args 'winsav-after-get-hook ret)
      ret)))

;; Fix-me: add window-hscroll
(defun winsav-get-window-tree-1(frame w)
  (let ((tree (if w w (car (window-tree frame)))))
    (if (windowp tree)
        ;; Note: Desktop is used for saving buffers.
        (with-current-buffer (window-buffer tree)
          (list (window-buffer tree)
                ;; buffer
                (buffer-name)
                (buffer-file-name)
                ;;buffer-read-only
                ;;(if mumamo-multi-major-mode mumamo-multi-major-mode major-mode)
                ;;minor-modes
                ;;buffer locals
                ;;(cons (+ 0 (mark-marker)) (mark-active))
                ;; window
                (window-point tree)
                (window-edges tree)
                (window-scroll-bars tree)
                (window-fringes tree)
                (window-margins tree)
                (window-hscroll tree)
                ;; misc
                (window-dedicated-p tree)
                (window-redisplay-end-trigger tree)
                (window-start tree)
                tree))
      (let* ((dir (nth 0 tree))
             (split (nth 1 tree))
             (wt (cddr tree))
             (wsubs (mapcar (lambda(wc)
                              (winsav-get-window-tree-1 nil wc))
                            wt)))
        (append (list dir split) wsubs)))))

;;;###autoload
(defun winsav-put-window-tree (saved-tree window &optional copy-win-ovl win-ovl-all-bufs)
  "Put window structure SAVED-TREE into WINDOW.
Restore a structure SAVED-TREE returned from
`winsav-get-window-tree' into window WINDOW.

If COPY-WIN-OVL is non-nil then overlays having a 'window
property pointing to one of the windows in SAVED-TREE where this
window still is shown will be copied to a new overlay with
'window property pointing to the corresponding new window.

If WIN-OVL-ALL-BUFS is non-nil then all buffers will be searched
for overlays with a 'window property of the kind above.

At the very end of this function the hook `winsav-after-put' is
run."
  (let* ((sel-num (nth 0 saved-tree))
         (tree    (nth 1 saved-tree))
         nsiz
         nh
         nw
         osiz
         oh
         ow
         scale-w
         scale-h
         first-win
         winsav-put-return)
    (unless (or (bufferp (car tree))
                (eq 'buffer (car tree)))
      (setq nsiz (window-edges window))
      (setq nh (- (nth 3 nsiz) (nth 1 nsiz)))
      (setq nw (- (nth 2 nsiz) (nth 0 nsiz)))
      (setq osiz (cadr tree))
      (setq oh (- (nth 3 osiz) (nth 1 osiz)))
      (setq ow (- (nth 2 osiz) (nth 0 osiz)))
      (setq scale-w (unless (= ow nw) (/ nw (float ow))))
      (setq scale-h (unless (= oh nh) (/ nh (float oh)))))
    (setq first-win (winsav-put-window-tree-1 tree window scale-w scale-h t 1))
    (select-window first-win)
    (when sel-num (other-window sel-num))
    (winsav-fix-win-ovl winsav-put-return copy-win-ovl win-ovl-all-bufs)
    (run-hook-with-args 'winsav-after-put-hook winsav-put-return)
    winsav-put-return))

(defun winsav-put-window-tree-1 (saved-tree window scale-w scale-h first-call level)
  "Helper for `winsav-put-window-tree'.
For the arguments SAVED-TREE and WINDOW see that function.

The arguments SCALE-W and SCALE-H are used to make the saved
window config fit into its new place.  FIRST-CALL is a state
variable telling if this is the first round.  LEVEL helps
debugging by tells how far down we are in the call chain."
  (if (or (bufferp (car saved-tree))
          ;;(not (car saved-tree))
          (eq 'buffer (car saved-tree))
          )
      (let ((buffer  (nth 0 saved-tree))
            ;; buffer
            (bufnam  (nth 1 saved-tree))
            (filnam  (nth 2 saved-tree))
            ;;(mark    (nth 3 saved-tree))
            ;; window
            (point   (nth 3 saved-tree))
            (edges   (nth 4 saved-tree))
            (scroll  (nth 5 saved-tree))
            (fringe  (nth 6 saved-tree))
            (margs   (nth 7 saved-tree))
            (hscroll (nth 8 saved-tree))
            (dedic   (nth 9 saved-tree))
            (trigger (nth 10 saved-tree))
            (start   (nth 11 saved-tree))
            (ovlwin  (nth 12 saved-tree))
            scr2
            (misbuf  " *Winsav information: Buffer is gone*"))
        (or (windowp ovlwin)
            (not ovlwin)
          (error "Parameter mismatch, ovlwin not window: %s" ovlwin))
        (when first-call
          (add-to-list 'winsav-put-return (list ovlwin window))
          (when (eq 'buffer buffer)
            (when filnam
              (setq buffer (winsav-find-file-noselect filnam)))
            (if (buffer-live-p buffer)
                (or (string= bufnam (buffer-name buffer))
                    (eq (string-to-char bufnam) 32) ;; Avoid system buffer names
                    (rename-buffer bufnam))
              (when (eq (string-to-char bufnam) 32)
                (setq bufnam " *Winsav dummy buffer*"))
              ;; Fix-me, this might need some tweaking: Don't restore
              ;; buffers without a file name and without
              ;; content. (desktop-mode will make that when
              ;; necessary.)  Just show the scratch buffer instead.
              (setq buffer (get-buffer bufnam))
              (unless (and buffer
                           (< 0 (buffer-size buffer)))
                (setq buffer (get-buffer-create "*scratch*")))))
          (set-window-buffer window buffer)
          (set-window-dedicated-p window dedic)
          ;; Strange incompatibility in scroll args:
          (setq scr2 (list (nth 0 scroll) (nth 2 scroll) (nth 3 scroll)))
          (apply 'set-window-scroll-bars (append (list window) scr2))
          (apply 'set-window-fringes (append (list window) fringe))
          (set-window-margins window (car margs) (cdr margs))
          (set-window-hscroll window hscroll)
          (set-window-redisplay-end-trigger window trigger))
        (let* ((nsiz (window-edges window))
               (nh (- (nth 3 nsiz) (nth 1 nsiz)))
               (nw (- (nth 2 nsiz) (nth 0 nsiz)))
               (osiz edges) ;(nth 2 saved-tree))
               (oh (- (nth 3 osiz) (nth 1 osiz)))
               (ow (- (nth 2 osiz) (nth 0 osiz)))
               (diff-w (- (if scale-w
                              (round (* scale-w ow))
                            ow)
                          nw))
               (diff-h (- (if scale-h
                              (round (* scale-h oh))
                            oh)
                          nh)))
          ;; Avoid rounding naggings:
          (when (> (abs diff-h) 1)
            (bw-adjust-window window diff-h nil))
          (when (> (abs diff-w) 1)
            (bw-adjust-window window diff-w t)))
        ;; Fix-me: there were some problems getting point correctly. Don't know why...
        (with-selected-window window
          (with-current-buffer (window-buffer window)
            (goto-char point))
          (set-window-point window point)
          ;;(unless (buffer-live-p buffer) (setq point 1) (setq start 1))
          (set-window-start window start)
          ;; Maybe point got off screen?
          (when (/= point (window-point window))
            (set-window-point window point)))
        window)
    (let* ((ver (car saved-tree))
           (wtree (list (cons window (caddr saved-tree))))
           (nwin window)
           pwin
           pdelta
           (first-win nwin))
      ;; First split to get it in correct order
      (when first-call
        (dolist (subtree (cdddr saved-tree))
          (setq pwin nwin)
          ;;(message "nwin edges=%s, ver=%s" (window-edges nwin) ver)
          (let ((split-err nil)
                (window-min-height 1)
                (window-min-width 1))
            (setq nwin (split-window nwin nil (not ver))))
          ;; Make the previous window as small as permitted to allow
          ;; splitting as many times as possible
          (setq pdelta (-
                        (if ver
                            window-min-height
                          window-min-width)
                        (if ver
                            (window-width pwin)
                          (window-height pwin))))
          ;;(message "pwin=%s, edges=%s, pdelta=%s, ver=%s" pwin (window-edges pwin) pdelta ver)
          ;; No reason to fail here:
          (condition-case err
              (adjust-window-trailing-edge pwin pdelta (not ver))
            (error
             ;;(message "awt=>%s" (error-message-string err))
             nil
             ))
          ;; Add to traverse
          (add-to-list 'wtree
                       (cons nwin subtree)
                       t)))
      ;; Now traverse. Sizing is a bit tricky, multiple runs have to
      ;; be done (as in balance-windows).
      (let (tried-sizes
            last-sizes
            (windows (window-list (selected-frame))))
        (while (not (member last-sizes tried-sizes))
          (when last-sizes (setq tried-sizes (cons last-sizes tried-sizes)))
          (setq last-sizes (mapcar (lambda (w)
                                     (window-edges w))
                                   windows))
          (dolist (wsub (reverse wtree))
            (select-window (car wsub))
            (winsav-put-window-tree-1 (cdr wsub) (selected-window)
                                      scale-w scale-h
                                      first-call
                                      (1+ level)
                                      ))
          (setq first-call nil)
          ))
      first-win)))

(defun winsav-fix-win-ovl(win-list copy-win-ovl win-ovl-all-bufs)
  (let ((oldwins (mapcar (lambda(elt)
                           (car elt))
                         win-list))
        ovlwin
        window)
    (let (buffers)
      (if win-ovl-all-bufs
          (setq buffers (buffer-list))
        (mapc (lambda(w)
                (when (window-live-p w)
                  (add-to-list 'buffers (window-buffer w))))
              oldwins))
      (dolist (buf buffers)
        (with-current-buffer buf
          (save-restriction
            (widen)
            (dolist (overlay (overlays-in (point-min) (point-max)))
              (when (setq ovlwin (car (memq (overlay-get overlay 'window) oldwins)))
                (setq window (cadr (assoc ovlwin win-list)))
                ;; If the old window is still alive then maybe copy
                ;; overlay, otherwise change the 'window prop. However
                ;; copy only if COPY-WIN-OVL is non-nil.
                (if (not (and (window-live-p ovlwin)
                              (window-frame ovlwin)))
                    (overlay-put overlay 'window window)
                  (when copy-win-ovl
                    (let* ((props (overlay-properties overlay))
                           (start (overlay-start overlay))
                           (end   (overlay-end   overlay))
                           ;; Fix-me: start and end marker props
                           (newovl (make-overlay start end)))
                      (while props
                        (let ((key (car props))
                              (val (cadr props)))
                          (setq props (cddr props))
                          (when (eq key 'window)
                            (setq val window))
                          (overlay-put newovl key val))))))))))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Window rotating and mirroring

(defun winsav-rotate (mirror transpose)
  "Rotate window configuration on selected frame.
MIRROR should be either 'mirror-left-right, 'mirror-top-bottom or
nil.  In the first case the window configuration is mirrored
vertically and in the second case horizontally.  If MIRROR is nil
the configuration is not mirrored.

If TRANSPOSE is non-nil then the window structure is transposed
along the diagonal from top left to bottom right (in analogy with
matrix transosition).

If called interactively MIRROR will is 'mirror-left-right by
default, but 'mirror-top-bottom if called with prefix.  TRANSPOSE
is t. This mean that the window configuration will be turned one
quarter clockwise (or counter clockwise with prefix)."
  (interactive (list
                (if current-prefix-arg
                    'mirror-left-right
                  'mirror-top-bottom)
                t))
  (let* ((wintree (winsav-get-window-tree))
         (tree (cadr wintree))
         (win-config (current-window-configuration)))
    ;;(winsav-log "old-wintree" wintree)
    (winsav-transform-1 tree mirror transpose)
    ;;(winsav-log "new-wintree" wintree)
    ;;
    ;; Fix-me: Stay in corresponding window. How?
    (delete-other-windows)
    (condition-case err
        (winsav-put-window-tree wintree (selected-window))
      (error
       (set-window-configuration win-config)
       (message "Can't rotate: %s" (error-message-string err))))
    ))

(defun winsav-transform-edges (edges)
  "Just rotate the arguments in EDGES to make them fit next function."
  (let ((le (nth 0 edges))
        (te (nth 1 edges))
        (re (nth 2 edges))
        (be (nth 3 edges)))
    (list te le be re)))

(defun winsav-transform-1 (tree mirror transpose)
  "Mirroring of the window tree TREE.
MIRROR could be 'mirror-top-bottom or 'mirror-left-right which I
think explain what it does here.  TRANSPOSE shifts the tree
between a horisontal and vertical tree."
  (let* ((vertical (nth 0 tree))
         (edges    (nth 1 tree))
         (subtrees (nthcdr 2 tree))
         )
    ;;(winsav-log "tree 1" tree)
    (when transpose
      (cond
       ((eq vertical nil)
        (setcar tree t))
       ((eq vertical t)
        (setcar tree nil))
       (t
        (error "Uh? vertical=%S" vertical))))
    (setcar (nthcdr 1 tree) (winsav-transform-edges edges))
    (dolist (subtree subtrees)
      (if (bufferp (car subtree))
          (when transpose
            (let ((edges    (nth 4 subtree)))
              ;;(winsav-log "subtree 1" subtree)
              (setcar (nthcdr 4 subtree) (winsav-transform-edges edges))
              ;;(winsav-log "subtree 2" subtree)
              ))
        (winsav-transform-1 subtree mirror transpose)))
    (when (case mirror
            ('mirror-top-bottom vertical)
            ('mirror-left-right (not vertical))
            (nil) ;; Don't mirror
            (t
             (error "Uh? mirror=%s" mirror)))
      (setcdr (nthcdr 1 tree) (reverse subtrees))
      )
    ))

(defun winsav-find-file-noselect (filename)
  "Read file FILENAME into a buffer and return the buffer.
Like `find-file-noselect', but if file is not find then creates a
buffer with a message about that."
  (let ((buf (find-file-noselect filename)))
    (unless buf
      (setq buf (generate-new-buffer filename))
      (with-current-buffer buf
        (insert "Winsav could not find the file " filename)
        (set-buffer-modified-p nil)))
    buf))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Session saving and restore etc

(defgroup winsav nil
  "Save frames and windows when you exit Emacs."
  :group 'frames)

;;;###autoload
(define-minor-mode winsav-save-mode
  "Toggle winsav configuration saving mode.
With numeric ARG, turn winsav saving on if ARG is positive, off
otherwise.

When this mode is turned on, winsav configurations are saved from
one session to another.  A winsav configuration consists of
frames, windows and visible buffers configurations plus
optionally buffers and files managed by the functions used by
option `desktop-save-mode'

By default this is integrated with `desktop-save-mode'.  If
`desktop-save-mode' is on and `winsav-handle-also-desktop' is
non-nil then save and restore also desktop.

See the command `winsav-switch-config' for more information and
other possibilities.

Note: If you want to avoid saving when you exit just turn off
this minor mode.

For information about what is saved and restored and how to save
and restore additional information see the function
`winsav-save-configuration'."
  :global t
  :group 'winsav)

(defun winsav-save-mode-on ()
  "Ensable option `winsav-save-mode'.  Provided for use in hooks."
  (winsav-save-mode 1))

(defun winsav-save-mode-off ()
  "Disable option `winsav-save-mode'.  Provided for use in hooks."
  (winsav-save-mode -1))

(defcustom winsav-save 'ask-if-new
  "Specifies whether the winsav config should be saved when it is killed.
A winsav config \(winsav frame configuration) is killed when the
user changes winsav directory or quits Emacs.

Possible values are:
   t             -- always save.
   ask           -- always ask.
   ask-if-new    -- ask if no winsav file exists, otherwise just save.
   ask-if-exists -- ask if winsav file exists, otherwise don't save.
   if-exists     -- save if winsav file exists, otherwise don't save.
   nil           -- never save.
The winsav config is never saved when the option `winsav-save-mode' is nil.
The variables `winsav-dirname' and `winsav-base-file-name'
determine where the winsav config is saved."
  :type
  '(choice
    (const :tag "Always save" t)
    (const :tag "Always ask" ask)
    (const :tag "Ask if winsav file is new, else do save" ask-if-new)
    (const :tag "Ask if winsav file exists, else don't save" ask-if-exists)
    (const :tag "Save if winsav file exists, else don't" if-exists)
    (const :tag "Never save" nil))
  :group 'winsav)

(defcustom winsav-handle-also-desktop t
  "If this is non-nil then desktop is also saved and restored.
See option `winsav-save-mode' for more information."
  :type 'boolean
  :group 'winsav)

(defcustom winsav-base-file-name
  (convert-standard-filename ".emacs.winsav")
  "Base name of file for Emacs winsav, excluding directory part.
The actual file name will have a system identifier added too."
  :type 'file
  :group 'winsav)

(defvar winsav-dirname nil
  "The directory in which the winsav file should be saved.")

(defun winsav-current-default-dir ()
  "Current winsav configuration directory."
  (or winsav-dirname "~/"))

;;(find-file (winsav-full-file-name))
(defun winsav-full-file-name (&optional dirname)
  "Return the full name of the winsav session file in DIRNAME.
DIRNAME omitted or nil means use `~'.

The file name consist of `winsav-base-file-name' with a system
identifier added.  This will be '-nw' for a terminal and '-' +
the value of `window-system' otherwise."
  ;; Fix-me: Different frames in different files? Can multi-tty be handled??
  (let* ((sys-id (if (not window-system)
                     "nw"
                   (format "%s" window-system)))
         (base-file (concat winsav-base-file-name "-" sys-id)))
    (expand-file-name base-file (or dirname
                                    (winsav-current-default-dir)))))



(defun winsav-serialize (obj)
  "Return a string with the printed representation of OBJ.
This should be possible to eval and get a similar object like OBJ
again."
  (prin1-to-string obj))

(defcustom winsav-before-save-configuration-hook nil
  "Hook called before saving frames.
Hook for writing elisp code at the beginning of a winsav
configuration file.  When this hook is called the current buffer
and point is where the code should be written.

This is a normal hook.  For more information see
`winsav-save-configuration'."
  :type 'hook
  :group 'winsav)

(defcustom winsav-after-save-configuration-hook nil
  "Hook called after saving frames.
Hook for writing elisp code at the end of a winsav configuration
file.  When this hook is called the current buffer and point is
where the code should be written.

This is a normal hook.  For more information see
`winsav-save-configuration'."
  :type 'hook
  :group 'winsav)

(defcustom winsav-after-save-frame-hook nil
  "Hook called when saving a frame after saving frame data.
Hook for writing elisp code in a winsav configuration file after
each frame creation.  When this hook is called code for restoring
a frame has been written and code that sets
`winsav-last-loaded-frame' to point to it.  Point is in the
configuration file buffer right after this.

This is a normal hook.  For more information see
`winsav-save-configuration'."
  :type 'hook
  :group 'winsav)

(defvar winsav-loaded-frames nil)
(defvar winsav-last-loaded-frame nil)

(defun winsav-restore-frame (frame-params
                             window-tree-params
                             use-minibuffer-frame
                             was-max)
  "Restore a frame with specified values.
If this is a minibuffer only frame then just apply the frame
parameters FRAME-PARAMS.  Otherwise create a new frame using
FRAME-PARAMS and set up windows and buffers according to
WINDOW-TREE-PARAMS.  Also, if USE-MINIBUFFER-FRAME let the new
frame have this minibuffer frame."
  (let* ((default-minibuffer-frame use-minibuffer-frame)
         (frame-name (cdr (assoc 'name frame-params)))
         (minibuffer-val (cdr (assoc 'minibuffer frame-params)))
         (minibuffer-only (eq 'only minibuffer-val))
         (mini-frames
          (delq nil (mapcar (lambda (frm)
                              (when (eq 'only (frame-parameter frm 'minibuffer))
                                frm))
                            (frame-list))))
         (frame-with-that-name
          (when (and frame-name minibuffer-only)
            (catch 'frame
              (dolist (frame (frame-list))
                (when (string= frame-name (frame-parameter frame 'name))
                  (throw 'frame frame))))))
         ;; If this is a minibuffer only frame then if it is already
         ;; there under a correct name then do not create it because
         ;; there might be variables pointing to it; just set the
         ;; parameters. Perhaps even better: if it is not already
         ;; there give an error - because it might be impossible to
         ;; set things up correctly then.
         (frame-with-that-name-has-mini
          (when frame-with-that-name
            (eq 'only
                (frame-parameter frame-with-that-name 'minibuffer))))
         (this-mini-frame (when minibuffer-only
                            (or frame-with-that-name
                                (and (= 1 (length mini-frames))
                                     (car mini-frames)))))
         (create-new
          (if minibuffer-only
              (if this-mini-frame ;frame-with-that-name-has-mini
                  nil
                (error "Winsav: Can't find minibuffer only frame with name %s"
                       frame-name))
            t))
         (this-frame (if create-new
                         (make-frame frame-params)
                       this-mini-frame))
         (win (frame-first-window this-frame)))
    ;;(message "create-new=%s, frame-with-that-name=%s" create-new frame-with-that-name)
    (if create-new
        (winsav-put-window-tree window-tree-params win)
      (modify-frame-parameters this-frame frame-params))
    (setq winsav-last-loaded-frame this-frame)
    (setq winsav-loaded-frames (cons this-frame winsav-loaded-frames))
    (when was-max (winsav-set-maximized-size this-frame))))

(defvar winsav-frame-parameters-to-save
  '(
    ;;explicit-name
    ;;name
    ;;parent-id
    ;;title
    alpha
    auto-lower
    auto-raise
    background-color
    background-mode
    border-color
    border-width
    buffer-predicate
    cursor-color
    cursor-type
    font
    font-backend
    foreground-color
    fullscreen
    icon-name
    icon-type
    icon-left
    icon-top
    internal-border-width
    left-fringe
    line-spacing
    menu-bar-lines
    modeline
    mouse-color
    right-fringe
    screen-gamma
    scroll-bar-width
    tool-bar-lines
    top left width height
    tty-color-mode ;; ??
    unsplittable
    user-position
    user-size
    vertical-scroll-bars
    visibility
    )
  "Parameters saved for frames by `winsav-save-configuration'.")

;;(winsav-set-restore-size nil)
(defun winsav-set-restore-size (frame)
  (when (fboundp 'w32-send-sys-command)
    (select-frame-set-input-focus frame)
    (w32-send-sys-command #xf120)
    ;; Note: sit-for must be used, not sleep-for. Using the latter
    ;; prevents the fetching of the new size (for some reason I do not
    ;; understand).
    (sit-for 0.5)
    t))

(defun winsav-set-maximized-size (frame)
  (when (fboundp 'w32-send-sys-command)
    (select-frame-set-input-focus frame)
    (w32-send-sys-command #xf030)
    t))

(defun winsav-save-frame (frame mb-frm-nr buffer)
  "Write into current buffer elisp code to recreate frame FRAME.
If MB-FRM-NR is a number then it is the order number of the frame
whose minibuffer should be used."
  (with-current-buffer buffer
    (let* ((start nil)
           (end nil)
           (obj (winsav-get-window-tree frame))
           (frm-size-now (cons (frame-pixel-height frame)
                               (frame-pixel-width frame)))
           (frm-size-rst (when (winsav-set-restore-size frame)
                           (cons (frame-pixel-height frame)
                                 (frame-pixel-width frame))))
           (was-max (and frm-size-rst
                         (not (equal frm-size-now frm-size-rst))))
           (frm-par (frame-parameters frame)))
      (setq frm-par
            (delq nil
                  (mapcar (lambda (elt)
                            (cond
                             ((memq (car elt) winsav-frame-parameters-to-save)
                              elt)
                             ((eq (car elt) 'minibuffer)
                              (let ((val (cdr elt)))
                                (if (not (windowp val))
                                    elt
                                  (if (eq (window-frame val) frame)
                                      nil
                                    (cons 'minibuffer nil)))))))
                          frm-par)))
      (insert "(winsav-restore-frame\n'"
              ;;make-frame-params
              (winsav-serialize frm-par))
      ;;window-tree-params
      (setq start (point))
      (insert "'" (winsav-serialize obj) "\n")
      (setq end (copy-marker (point) t))
      (replace-regexp (rx "#<buffer "
                          (1+ (not (any ">")))
                          (1+ ">")) ;; 1+ for indirect buffers ...
                      "buffer"
                      nil start end)
      (replace-regexp (rx "#<window "
                          (1+ (not (any ">")))
                          (1+ ">"))
                      "nil"
                      nil start end)
      (goto-char end)
      ;;use-minibuffer-frame
      (insert (if mb-frm-nr
                  (format "(nth %s (reverse winsav-loaded-frames))" mb-frm-nr)
                "nil")
              (if was-max " t " " nil ")
              ")\n\n")

      (insert "    ;; ---- before after-save-frame-hook ----\n")
      ;; (dolist (fun winsav-after-save-frame-hook)
      ;;   (funcall fun frame (current-buffer)))
      (run-hooks winsav-after-save-frame-hook)
      (insert "    ;; ---- after after-save-frame-hook  ----\n")

      ;;(insert "  )\n\n\n")
      )))

(defvar winsav-file-version "1"
  "Version number of winsav file format.
Written into the winsav file and used at winsav read to provide
backward compatibility.")


;; fix-me: This should be in desktop.el
(defun winsav-restore-indirect-file-buffer (file name)
  "Make indirect buffer from file buffer visiting file FILE.
Give it the name NAME."
  (let* ((fbuf (find-file-noselect file)))
    (when fbuf
      (make-indirect-buffer fbuf name))))
(defun winsav-save-indirect-buffers ()
  "Save information about indirect buffers.
Only file visiting buffers currently.  Clone the base buffers."
  (dolist (buf (buffer-list))
    (when (buffer-base-buffer buf)
      (let* ((base-buf (buffer-base-buffer buf))
             (file (buffer-file-name base-buf))
             )
        (when file
          (insert "(winsav-restore-indirect-file-buffer \""
                  file "\" \"" (buffer-name buf) "\")\n"))))))

;; Fix-me: test
;; (defun winsav-restore-minibuffer (frame-num frm-num win-num)
;;   (let* ((frame (nth (1- frame-num) winsav-loaded-frames))
;;          (mini-frm (nth (1- frm-num) winsav-loaded-frames))
;;          (mini-win (nth (1- win-num) (reverse (window-list mini-frm))))
;;          )
;;     (with-selected-frame frame
;;       (set-minibuffer-window mini-win))))

(defvar winsav-minibuffer-alist nil)
(defun winsav-save-minibuffers (sorted-frames)
  "Save information about minibuffer frames.
SORTED-FRAMES should be a list of all frames sorted using
`winsav-frame-sort-predicate'."
  (setq winsav-minibuffer-alist nil)
  (dolist (frame sorted-frames)
    (let* ((num-frames (length sorted-frames))
           (mini-win (minibuffer-window frame))
           (mini-frm (window-frame mini-win))
           (win-num (length
                     (memq mini-win
                           (window-list mini-frm t (frame-first-window mini-frm)))))
           (frm-num (- num-frames (length (memq mini-frm sorted-frames))))
           (frame-num (- num-frames (length (memq frame sorted-frames))))
           )
      (unless (and (eq mini-frm frame)
                   (= win-num 1))
        ;; Not the normal minibuffer window
        ;;(insert (format ";;(winsav-restore-minibuffer %s %s %s)\n"
        ;;(insert (format "'(%s %s)\n" frame-num frm-num)
        (setq winsav-minibuffer-alist (cons (list frame-num frm-num) winsav-minibuffer-alist))
        )))
  (insert "(setq winsav-minibuffer-alist '"
          (winsav-serialize winsav-minibuffer-alist)
          ")\n"))

(defun winsav-restore-dedicated-window (frame-num win-num dedicate-flag)
  "Set dedicated window flag.
On frame number FRAME-NUM in `winsav-loaded-frames' set the
dedicated flag on window number WIN-NUM to DEDICATE-FLAG."
  (let* ((frame (nth (1- frame-num) winsav-loaded-frames))
         (win (nth (1- win-num) (reverse (window-list frame t
                                                      (frame-first-window frame))))))
    (set-window-dedicated-p win dedicate-flag)))

(defun winsav-save-dedicated-windows (sorted-frames)
  "Save information about dedicated windows on frames in SORTED-FRAMES.
Write this to current buffer."
  (dolist (frame sorted-frames)
    (dolist (win (window-list frame))
      (when (window-dedicated-p win)
        (let ((frame-num (length (memq frame sorted-frames)))
              (win-num (length
                        (memq win
                              (window-list frame t (frame-first-window frame)))))
              (flag (window-dedicated-p win)))
          (insert (format "(winsav-restore-dedicated-window %s %s %S)\n" frame-num win-num flag))
          )))))

(defun winsav-restore-ecb (frame-num layout-ecb)
  "Restore ECB.
On frame number FRAME-NUM-ECB in `winsav-loaded-frames' restore
ECB layout LAYOUT-ECB."
  (when (boundp 'ecb-minor-mode)
    (let* ((frame (nth (1- frame-num) winsav-loaded-frames)))
      (select-frame frame)
      (unless (string= layout-ecb ecb-layout-name)
        (setq ecb-layout-name layout-ecb))
      (ecb-minor-mode 1))))

(defun winsav-save-ecb (frame-ecb layout-ecb sorted-frames)
  "Save information about ECB layout on frames in SORTED-FRAMES.
Write this in current buffer."
  (dolist (frame sorted-frames)
    (when (eq frame frame-ecb)
      (let ((frame-num (length (memq frame sorted-frames))))
        (insert (format "(winsav-restore-ecb %s %S)\n" frame-num layout-ecb))))))

;; (make-frame '((minibuffer)))
;; (sort (frame-list) 'winsav-frame-sort-predicate)
(defun winsav-frame-sort-predicate (a b)
  "Compare frame A and B for sorting.
Sort in the order frames can be created.  Frames without
minibuffers will come later."
  (let* ((a-mbw (minibuffer-window a))
         (a-mbw-frm (window-frame a-mbw))
         (b-mbw (minibuffer-window b))
         (b-mbw-frm (window-frame b-mbw))
         )
    ;;(message "a-mbw-frm=%s, b=%s" a-mbw-frm b)
    ;;(message "b-mbw-frm=%s, a=%s" a-mbw-frm b)
    (when (or (eq a-mbw-frm b)
              (not (eq b-mbw-frm b)))
      ;;(message "a > b")
      t
      )))

(defun winsav-can-read-config (config-version)
  "Return t we can read config file version CONFIG-VERSION."
  (when (<= config-version 1)
    t))

(defvar winsav-file-modtime nil)

;; Like desktop-save, fix-me
(defun winsav-save-configuration (&optional dirname release)
  "Write elisp code to recreate all frames.
Write into the file name computed by `winsav-full-file-name'
given the argument DIRNAME.

The information that is saved for each frame is its size and
position, the window configuration including buffers and the
parameters in `winsav-frame-parameters-to-save'.  If you want save
more information for frames you can do that in the hook
`winsav-after-save-frame-hook'.

See also the hook variables
`winsav-before-save-configuration-hook' and
`winsav-after-save-configuration-hook'.

Fix-me: RELEASE is not implemented."
  (let ((file (winsav-full-file-name dirname))
        start
        end
        (sorted-frames (sort (frame-list) 'winsav-frame-sort-predicate))
        (frm-nr 0)
        frame-ecb
        layout-ecb
        )
    ;; Recreating invisible frames hits Emacs bug 3859
    (setq sorted-frames
          (delq nil
                (mapcar (lambda (f)
                          (when (frame-parameter f 'visibility) f))
                        sorted-frames)))
    (when (and (boundp 'ecb-minor-mode) ecb-minor-mode)
      (when (frame-live-p ecb-frame)
        (setq layout-ecb ecb-layout-name)
        (setq frame-ecb ecb-frame))
      (ecb-minor-mode -1)
      (sit-for 0) ;; Fix-me: is this needed?
      )
    (with-temp-buffer
      ;;(erase-buffer)
      (insert
       ";; -*- mode: emacs-lisp; coding: utf-8; -*-\n"
       ";; --------------------------------------------------------------------------\n"
       ";; Winsav File for Emacs\n"
       ";; --------------------------------------------------------------------------\n"
       ";; Created " (current-time-string) "\n"
       ";; Winsav file format version " winsav-file-version "\n"
       ";; Emacs version " emacs-version "\n\n"
       "(if (not (winsav-can-read-config " winsav-file-version "))\n\n"
       "    (message \"Winsav: Can't read config file with version " winsav-file-version "\")\n")
      (insert ";; ---- indirect buffers ------------------------\n")
      (winsav-save-indirect-buffers)
      ;;(insert ";; ---- special minibuffers ------------------------\n")
      (winsav-save-minibuffers sorted-frames)
      (insert "(setq winsav-loaded-frames nil)\n")
      (insert ";; ---- before winsav-before-save-configuration-hook ------------------------\n")
      (run-hooks 'winsav-before-save-configuration-hook)
      (insert ";; ---- after winsav-before-save-configuration-hook  ------------------------\n\n")
      (dolist (frm sorted-frames)
        (let ((mb-frm-nr (cadr (assoc frm-nr winsav-minibuffer-alist)))
              ;;(mb-frm (when mb-frm-nr (nth mb-frm-nr sorted-frames)))
              )
          (winsav-save-frame frm mb-frm-nr (current-buffer))
          (setq frm-nr (1+ frm-nr))))
      (insert ";; ---- dedicated windows ------------------------\n")
      (winsav-save-dedicated-windows sorted-frames)
      (insert ";; ---- ECB --------------------------------------\n")
      (winsav-save-ecb frame-ecb layout-ecb sorted-frames)
      (insert "\n\n;; ---- before winsav-after-save-configuration-hook  ------------------------\n")
      (run-hooks 'winsav-after-save-configuration-hook)
      (insert ";; ---- after winsav-after-save-configuration-hook   ------------------------\n")
      (insert "\n)\n")
      (emacs-lisp-mode)
      (pp-buffer)
      (indent-region (point-min) (point-max))
      ;;(save-buffer 0) ;; No backups
      ;;(kill-buffer)

      ;;(with-current-buffer (find-file-noselect file)
      (let ((coding-system-for-write 'utf-8))
        (write-region (point-min) (point-max) file nil 'nomessage))
      (setq winsav-file-modtime (nth 5 (file-attributes file)))
      (setq winsav-dirname (file-name-as-directory (file-name-directory file)))
      )))

(defvar winsav-current-config-name nil)

;;(winsav-restore-configuration)
;;(winsav-full-file-name "~")
;; (defun winsav-restore-winsav-configuration ()
;;   )

;; Like desktop-read, fix-me
(defun winsav-restore-configuration (&optional dirname)
  "Restore frames from file in directory DIRNAME.
The file was probably written by `winsav-save-configuration'.
Delete the frames that were used before."
  ;;(message "winsav-restore-configuration %s" dirname)
  (let ((old-frames (sort (frame-list) 'winsav-frame-sort-predicate))
        (conf-file (winsav-full-file-name dirname)))
    ;;(message "winsav:conf-file=%s" conf-file)
    (if (or (not conf-file)
            (not (file-exists-p conf-file)))
        (progn
          (message (propertize "Winsav: No default configuration file found"
                               'face 'secondary-selection))
          t) ;; Ok
      (setq debug-on-error t) ;; fix-me
      (if (file-exists-p conf-file)
          (progn
            (load conf-file nil nil t)
            (setq winsav-file-modtime (nth 5 (file-attributes conf-file)))
            (setq winsav-dirname (file-name-as-directory (file-name-directory conf-file)))
            (when (< 0 (length winsav-loaded-frames))
              (dolist (old (reverse old-frames))
                (unless (eq 'only (frame-parameter old 'minibuffer))
                  (delete-frame old)))
              ;;(winsav-maximize-all-nearly-max-frames)
              )
            (message "Winsav: %s frame(s) restored" (length winsav-loaded-frames))
            t)
        ;; No winsav file found
        ;;(winsav-clear)
        (message "No winsav file: %s" conf-file)
        nil))))

;; (defcustom winsav-add-to-desktop nil
;;   "Set this to let desktop save and restore also winsav configurations."
;;   :type 'boolean
;;   :set (lambda (sym val)
;;          (set-default sym val)
;;          (if value
;;              (progn
;;                (add-hook 'desktop-after-read-hook 'winsav-restore-configuration)
;;                (add-hook 'desktop-save-hook 'winsav-save-configuration))
;;            (remove-hook 'desktop-after-read-hook 'winsav-restore-configuration)
;;            (remove-hook 'desktop-save-hook 'winsav-save-configuration)) )
;;   :group 'winsav)

(defun winsav-restore-configuration-protected (&optional dirname)
  "Like `winsav-restore-configuration' but protect for errors.
DIRNAME has the same meaning."
  (condition-case err
      (winsav-restore-configuration dirname)
    (error
     (message "winsav-restore-configuration: %s" err))))

(defun winsav-relative-~-or-full (dirname)
  (let* ((rel-dir (file-relative-name dirname
                                      (file-name-directory
                                       (winsav-full-file-name "~"))))
         (confname (if (string= ".." (substring rel-dir 0 2))
                       winsav-dirname
                     (if (string= rel-dir "./")
                         "(default)"
                       (concat "~/" rel-dir)))))
    confname))

(defun winsav-tell-configuration ()
  "Tell which winsav configuration that is used."
  (interactive)
  (save-match-data ;; runs in timer
    (let ((confname (if (not winsav-dirname)
                        "(none)"
                      (winsav-relative-~-or-full winsav-dirname))))
      (if t ;;(called-interactively-p)
          (message (propertize (format "Current winsav config is '%s'" confname)
                               'face 'secondary-selection))
        (save-window-excursion
          (delete-other-windows)
          (set-window-buffer (selected-window)
                             (get-buffer-create " *winsav*"))
          (with-current-buffer (window-buffer)
            (momentary-string-display
             (propertize
              (format "\n\n\n  Current winsav config is '%s'\n\n\n\n" confname)
              'face 'secondary-selection)
             (window-start)
             (kill-buffer))))))))

(defun winsav-tell-configuration-request ()
  "Start an idle timer to call `winsav-tell-configuration'."
  (run-with-idle-timer 1 nil 'winsav-tell-configuration))

(defun winsav-nearly-maximized (frame)
  "Return non-nil if size of frame FRAME is nearly full screen."
  (let* ((top (frame-parameter frame 'top))
         (left (frame-parameter frame 'left))
         (width (frame-pixel-width frame))
         (height (frame-pixel-height frame))
         (display-width (display-pixel-width))
         (display-height (display-pixel-height))
         (char-height (frame-char-height frame))
         (height-diff (- display-height height))
         (terminal-type (framep frame)))
    ;;(message "w=%s/%s, h=%s/%s, ch=%s, hd=%s" width display-width height display-height char-height height-diff)
    (cond
     ((eq 'w32 terminal-type)
      (and (equal top '(+ -4))
           (equal left '(+ -4))
           (= width display-width)
           (< height-diff (* 4 char-height))
           )))))

;; (defun winsav-maximize-nearly-maximized (frame)
;;   "Maximize frame FRAME if size is nearly full screen."
;;   (when (winsav-nearly-maximized frame)
;;     (let ((terminal-type (framep frame)))
;;       (cond
;;        ((eq 'w32 terminal-type)
;;         ;;(message "max %s" frame)
;;         ;;(select-frame-set-input-focus frame)
;;         (select-frame frame)
;;         (w32-send-sys-command 61488))))))

;;(winsav-maximize-all-nearly-max-frames)
;; (defun winsav-maximize-all-nearly-max-frames ()
;;   "Maximizes all frames whose size is nearly full screen."
;;   (let ((sel-frm (selected-frame)))
;;     (dolist (frame (frame-list))
;;       (winsav-maximize-nearly-maximized frame))
;;     (run-with-idle-timer 0 nil 'select-frame-set-input-focus sel-frm)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Startup and shut down

;; Run after desktop at startup so that desktop has loaded files and
;; buffers.
(defun winsav-after-init ()
  "Restore frames and windows.
Run this once after Emacs startup, after desktop in the
`after-init-hook'."
  ;; Turn off with --no-deskttop:
  (unless desktop-save-mode (winsav-save-mode -1))
  (when winsav-save-mode
    ;;(run-with-idle-timer 0.1 nil 'winsav-restore-configuration-protected)
    ;;(message "winsav-after-init")
    ;;(winsav-restore-configuration-protected)
    ;; In case of error make sure winsav-save-mode is turned off
    (setq inhibit-startup-screen t)
    (winsav-save-mode -1)
    (winsav-restore-configuration)
    (winsav-save-mode 1)
    ))

(add-hook 'after-init-hook 'winsav-after-init t)

(add-hook 'kill-emacs-hook 'winsav-kill)
;;(remove-hook 'kill-emacs-hook 'winsav-kill)

(defun winsav-kill ()
  "Save winsav frame configuration.
Run this before Emacs exits."
  ;; (when winsav-save-mode
  ;;   (let ((conf-dir (when winsav-current-config-name
  ;;                     (winsav-full-config-dir-name winsav-current-config-name))))
  ;;     (winsav-save-configuration conf-dir))))
  (when (and winsav-save-mode
             (let ((exists (file-exists-p (winsav-full-file-name))))
               (or (eq winsav-save t)
                   (and exists (memq winsav-save '(ask-if-new if-exists)))
                   (and
                    (or (memq winsav-save '(ask ask-if-new))
                        (and exists (eq winsav-save 'ask-if-exists)))
                    (y-or-n-p "Save winsav? ")))))
    (unless winsav-dirname
      ;; Fix-me: Since this can be a new user of winsav I think the
      ;; best thing to do here is to encourage the user to save in the
      ;; default directory since otherwise the winsav file will not be
      ;; loaded at startup. Desktop does not currently do that however
      ;; (report that!).
      (let* ((full-file (winsav-full-file-name))
             (default-directory (directory-file-name
                                 (file-name-directory full-file))))
        (setq winsav-dirname
              (file-name-as-directory
               (expand-file-name
                (read-directory-name "Directory for winsav file: " nil nil t))))))
    (condition-case err
	;;(winsav-save winsav-dirname t)
	(winsav-save-configuration winsav-dirname)
      (file-error
       (unless (yes-or-no-p
                (format "Error while saving winsav config: %s  Save anyway? "
                        (error-message-string err)))
	 (signal (car err) (cdr err))))))
  ;; If we own it, we don't anymore.
  ;;(when (eq (emacs-pid) (winsav-owner)) (winsav-release-lock))
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Switching configurations

(defun winsav-restore-full-config (dirname)
  "Restore the winsav configuration in directory DIRNAME.
If NAME is nil then restore the startup configuration."
  ;;(desktop-change-dir dirname)
  (when (and winsav-handle-also-desktop desktop-save-mode)
    (when (eq (emacs-pid) (desktop-owner)) (desktop-release-lock))
    (desktop-clear)
    (desktop-read dirname))
  (winsav-restore-configuration dirname)
  ;;(setq winsav-current-config-name name)
  (winsav-tell-configuration-request))

(defun winsav-full-config-dir-name (name)
  "Return full directory path where configuration NAME is stored."
  (let* ((base-dir (concat (winsav-full-file-name) ".d"))
         (conf-dir (expand-file-name name base-dir)))
    (setq conf-dir (file-name-as-directory conf-dir))
    ;;(message "conf-dir=%s" conf-dir)
    conf-dir))

;;;###autoload
(defun winsav-save-full-config (dirname)
  "Saved current winsav configuration in directory DIRNAME.
Then change to this configuration.

See also `winsav-switch-config'."
  (unless (file-name-absolute-p dirname)
    (error "Directory ame must be absolute: %s" dirname))
  (let* ((conf-dir (or dirname "~"))
         (old-conf-dir winsav-dirname))
    (mkdir conf-dir t)
    (winsav-save-configuration conf-dir)
    (when (and winsav-handle-also-desktop desktop-save-mode)
      (desktop-release-lock)
      (desktop-save conf-dir))
    ;;(unless (string= winsav-current-config-name name)
    (unless (string= old-conf-dir conf-dir)
      ;;(setq winsav-current-config-name name)
      (winsav-tell-configuration-request))))

;; Fix-me: remove named configurations, use just dir as desktop
(defun winsav-switch-to-default-config ()
  "Change to default winsav configuration.
See also `winsav-switch-config'."
  (interactive)
  (winsav-switch-config "~"))

;;;###autoload
(defun winsav-switch-config (dirname)
  "Change to winsav configuration in directory DIRNAME.
If DIRNAME is the current winsav configuration directory then
offer to save it or restore it from saved values.

Otherwise, before switching offer to save the current winsav
configuration.  Then finally switch to the new winsav
configuration, creating it if it does not exist.

If option `desktop-save-mode' is on then buffers and files are also
restored and saved the same way.

See also option `winsav-save-mode' and command
`winsav-tell-configuration'."
  (interactive
   (list
    (let ((default-directory (or winsav-dirname default-directory))
          (base-dir (concat (winsav-full-file-name) ".d"))
          new-dir)
      (mkdir base-dir t)
      (setq new-dir
            (read-directory-name "Winsav: Switch config directory: "))
      (when (string= "" new-dir) (setq new-dir nil))
      (or new-dir
          "~"))))
  (setq dirname (file-name-as-directory (expand-file-name dirname)))
  (catch 'stop
    (let ((conf-file (expand-file-name winsav-base-file-name dirname))
          config-exists)
      (if (file-exists-p conf-file)
          (setq config-exists t)
        (unless (y-or-n-p (format "%s was not found.  Create it? " conf-file))
          (throw 'stop nil)))
      (if (string= winsav-dirname dirname)
          (if (y-or-n-p "You are already using this configuration, restore it from saved values? ")
              (winsav-restore-full-config winsav-dirname)
            (when (y-or-n-p "You are already using this winsav configuration, save it? ")
              (winsav-save-full-config winsav-dirname)))
        (when (y-or-n-p
               (format "Save current config, %s,\n first before switching to %s? "
                       (if (and winsav-dirname
                                (not (string= winsav-dirname
                                              (file-name-directory (winsav-full-file-name "~")))))
                           winsav-dirname
                         "the startup config")
                       dirname))
          (winsav-save-full-config winsav-dirname))
        (if config-exists
            (winsav-restore-full-config dirname)
          (winsav-save-full-config dirname))))))




;;; Old things

;; (defun winsav-log-buffer ()
;;   (get-buffer-create "winsav log buffer"))

;; (defun winsav-log (mark obj)
;;   (with-current-buffer (winsav-log-buffer)
;;     (insert "=== " mark "===\n" (pp-to-string obj))))

;; (global-set-key [f2] 'winsav-test-get)
;; (global-set-key [f3] 'winsav-test-put)
;; (defvar winsav-saved-window-tree nil)

;; (defun winsav-test-get()
;;   (interactive)
;;   (setq winsav-saved-window-tree (winsav-get-window-tree)))

;; (defun winsav-test-put()
;;   (interactive)
;;   (let ((ret (winsav-put-window-tree winsav-saved-window-tree
;;                                      (selected-window))))
;;     ;;(message "ret=%s" ret)
;;     ))

;; (defun winsav-serialize-to-file (obj file)
;;   (with-current-buffer (find-file-noselect file)
;;     ;;(erase-buffer)
;;     (save-restriction
;;       (widen)
;;       (goto-char (point-max))
;;       (insert (winsav-serialize obj)
;;               "\n"))
;;     ;;(basic-save-buffer)
;;     ))

;;(global-set-key [f11] 'winsav-rotate)

;; (defun winsav-de-serialize-window-tree-from-file (file)
;;   (with-current-buffer (find-file-noselect file)
;;     (save-restriction
;;       (widen)
;;       (let ((start (point))
;;             (end nil))
;;         (forward-list)
;;         (setq end (point))
;;         ;;(goto-char (point-min))
;;         (winsav-de-serialize-window-tree (buffer-substring-no-properties start end))))))

;; (defun winsav-restore-from-file (file)
;;   (winsav-put-window-tree
;;    (winsav-de-serialize-window-tree-from-file file)
;;    (selected-window)))

;; (defun winsav-de-serialize-window-tree (str)
;;   (save-match-data
;;     (let ((read-str
;;            (replace-regexp-in-string (rx "#<buffer "
;;                                          (1+ (not (any ">")))
;;                                          ">")
;;                                      "buffer"
;;                                      str))
;;           obj-last
;;           obj
;;           last)
;;       (setq read-str
;;             (replace-regexp-in-string (rx "#<window "
;;                                           (1+ (not (any ">")))
;;                                           ">")
;;                                       "nil"
;;                                       read-str))
;;       (setq obj-last (read-from-string read-str))
;;       (setq obj (car obj-last))
;;       (setq last (cdr obj-last))
;;       ;; Fix me, maby check there are only spaces left (or trim them above...)
;;       obj)))

(provide 'winsav)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; winsav.el ends here
