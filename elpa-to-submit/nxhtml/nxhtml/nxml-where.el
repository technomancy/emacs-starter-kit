;;; nxml-where.el --- Show XML path
;;
;; Author: Lennart Borgman
;; Maintainer:
;; Created: Tue Dec 19 14:59:01 2006
(defconst nxml-where:version "0.52");; Version:
;; Lxast-Updated: Thu Mar 01 23:16:35 2007 (3600 +0100)
;; Keywords:
;; Compatibility:
;;
;; Features that might be required by this library:
;;
;;   `cl'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change log:
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

;; This buffer is for notes you don't want to save, and for Lisp evaluation.
;; If you want to create a file, visit that file with C-x C-f,
;; then enter the text in that file's own buffer.

(eval-when-compile (require 'cl))
(eval-when-compile (require 'mumamo))
(eval-when-compile
  (unless (featurep 'nxhtml-autostart)
    (let ((efn (expand-file-name "../autostart.el")))
      (load efn))
    (require 'nxml-mode)))

(defun nxml-where-error-message (format-string &rest args)
  (with-current-buffer (get-buffer-create "*Messages*")
    (let ((start (1+ (point-max))))
      (apply 'message format-string args)
      (goto-char (point-max))
      (backward-char)
      ;; fix-me: got some error here:
      ;;(put-text-property start (point) 'face 'highlight)
      )))

(defvar nxml-where-last-point nil
  "Where point was last time marking finished.
Ie we should not restart marking if point is still there and no
changes have occured.")
(make-variable-buffer-local 'nxml-where-last-point)
(put 'nxml-where-last-point 'permanent-local t)

(defvar nxml-where-last-finished nil
  "Non-nil then marking is finished.")
(make-variable-buffer-local 'nxml-where-last-finished)
(put 'nxml-where-last-finished 'permanent-local t)

(defvar nxml-where-last-added nil)
(make-variable-buffer-local 'nxml-where-last-added)
(put 'nxml-where-last-added 'permanent-local t)

(defvar nxml-where-path nil
  "The current where path.
This is a list where the records have the form

   \(START END TAG-STR OVERLAY)")
(make-variable-buffer-local 'nxml-where-path)
(put 'nxml-where-path 'permanent-local t)

(defvar nxml-where-new-path nil
  "The new where path.
This is a list where the records have the form

   \(START END TAG-STR OVERLAY)")
(make-variable-buffer-local 'nxml-where-new-path)
(put 'nxml-where-new-path 'permanent-local t)

(defvar nxml-where-once-update-timer nil)
(make-variable-buffer-local 'nxml-where-once-update-timer)
(put 'nxml-where-once-update-timer 'permanent-local t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Custom options

(defgroup nxml-where nil
  "Customization group for nxml-where."
  :group 'nxhtml
  :group 'nxml)

(define-toggle nxml-where-only-inner nil
  "Mark only inner-most tag."
  :set (lambda (sym val)
         (set-default sym val)
         (when (fboundp 'nxml-where-update-buffers)
           (nxml-where-update-buffers)))
  :group 'nxml-where)

(define-toggle nxml-where-header t
  "Show header with XML-path if non-nil."
  :set (lambda (sym val)
         (set-default sym val)
         (when (fboundp 'nxml-where-update-buffers)
           (nxml-where-update-buffers)))
  :group 'nxml-where)

(define-toggle nxml-where-tag+id t
  "Show tags + id in path if non-nil.
If nil show only tag names."
  :set (lambda (sym val)
         (set-default sym val)
         (when (fboundp 'nxml-where-update-buffers)
           (nxml-where-update-buffers)))
  :group 'nxml-where)

(define-toggle nxml-where-marks t
  "Show marks in buffer for XML-path if non-nil."
  :set (lambda (sym val)
         (set-default sym val)
         (when (fboundp 'nxml-where-update-buffers)
           (nxml-where-update-buffers)))
  :group 'nxml-where)

(define-toggle nxml-where-only-tags-with-id t
  "Show only tags with id in the header line."
  :set (lambda (sym val)
         (set-default sym val)
         (when (fboundp 'nxml-where-update-buffers)
           (nxml-where-update-buffers)))
  :group 'nxml-where)

(defface nxml-where-marking
  '((t (:inherit secondary-selection)))
  "The default face used for marking tags in path."
  :group 'nxml-where)

(defcustom nxml-where-marking 'nxml-where-marking
  "Variable pointing to the face used for marking tags in path."
  :type 'face
  :set (lambda (sym val)
         (set-default sym val)
         (when (fboundp 'nxml-where-update-buffers)
           (nxml-where-update-buffers)))
  :group 'nxml-where)

(defcustom nxml-where-header-attributes '("id" "name")
  "List of attributes `nxml-where-header' should display."
  :type '(repeat string)
  :set (lambda (sym val)
         (set-default sym val)
         (when (fboundp 'nxml-where-update-buffers)
           (nxml-where-update-buffers)))
  :group 'nxml-where)

(defcustom nxml-where-widen t
  "If non-nil and narrowed widen before getting XML path."
  :type 'boolean
  :group 'nxml-where)





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Modes

(defvar nxml-where-modes '(nxml-mode nxhtml-mode))

(defun nxml-where-is-nxml ()
  (or (derived-mode-p 'nxml-mode)
      (and (featurep 'mumamo)
           mumamo-multi-major-mode
           (let ((major-mode (mumamo-main-major-mode)))
             (derived-mode-p 'nxml-mode)))))

(defun nxml-where-setup-updating ()
  (nxml-where-clear-old-path 0 "setup")
  (setq nxml-where-last-added nil)
  (setq nxml-where-last-point nil)
  (when (and nxml-where-header
             (not nxml-where-only-inner))
    (setq header-line-format "Started nxml-where-mode ..."))
  ;;(nxml-where-restart-update)
  (add-hook 'post-command-hook 'nxml-where-restart-update nil t))

(defun nxml-where-mode-start ()
  ;;(message "START")
  (unless (nxml-where-is-nxml)
    (error "Can't display XML path since major mode is not nxml-mode child."))
  (add-hook 'after-change-major-mode-hook 'nxml-where-turn-off-unless-nxml nil t)
  (add-hook 'after-change-functions 'nxml-where-after-change nil t)
  (nxml-where-save-header-line-format)
  (nxml-where-setup-updating))

(defun nxml-where-mode-stop ()
  ;;(message "STOP")
  (remove-hook 'after-change-major-mode-hook 'nxml-where-turn-off-unless-nxml t)
  (remove-hook 'after-change-functions 'nxml-where-after-change t)
  (nxml-where-stop-updating)
  (nxml-where-unmark-forward-element)
  (nxml-where-restore-header-line-format)
  (nxml-where-clear-old-path 0 "stop"))

(defun nxml-where-turn-off-unless-nxml ()
  (unless (nxml-where-is-nxml)
    (nxml-where-mode-stop)))
(put 'nxml-where-turn-off-unless-nxml 'permanent-local-hook t)

;;;###autoload
(define-minor-mode nxml-where-mode
  "Shows path in mode line."
  :global nil
  :group 'nxml-where
  (if nxml-where-mode
      ;;Turn it on
      (nxml-where-mode-start)
    ;; Turn it off
    (nxml-where-mode-stop)
    ))
(put 'nxml-where-mode 'permanent-local t)

(defun nxml-where-turn-on-in-nxml-child ()
  "Turn on `nxml-where-mode' if possible.
This is possible if `major-mode' in the buffer is derived from
`nxml-mode'."
  (when (or (derived-mode-p 'nxml-mode)
            (and mumamo-multi-major-mode
                 (let ((major-mode (mumamo-main-major-mode)))
                   (derived-mode-p 'nxml-mode))))
    (unless nxml-where-mode
      (nxml-where-mode 1))))

;;;###autoload
(define-globalized-minor-mode nxml-where-global-mode nxml-where-mode
  nxml-where-turn-on-in-nxml-child
  :group 'nxml-where)
;; The problem with global minor modes:
(when (and nxml-where-global-mode
           (not (boundp 'define-global-minor-mode-bug)))
  (nxml-where-global-mode 1))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Auto updating

(defvar nxhtml-where-hook nil
  "Normal hook run when marking has changed.")

(defun nxml-where-start-update-in-timer (buffer)
  "First checks post command."
  ;;(message "nxml-where-start-update buffer=%s (bufferp buffer)=%s" buffer (bufferp buffer))
  (when (and (bufferp buffer)
             (buffer-live-p buffer))
    (with-current-buffer buffer
      (let ((here (point)))
        (save-match-data
          (condition-case err
              (progn
                ;;(unless nxml-where-marks (nxml-where-clear-old-path))
                (unless (and nxml-where-header
                             (not nxml-where-only-inner))
                  (setq header-line-format nil))
                (when (and nxml-where-mode
                           (or nxml-where-header nxml-where-marks))
                  (nxml-where-do-marking nil buffer)))
            (error
             (nxml-where-error-message
              "nxml-where-start-update-in-timer error: %s" err)))
          (goto-char here))))))

(defun nxml-where-continue-marking-in-timer (this-point buffer)
  "Continue unfinished marking after last restart.
Ie we have run at least once post command."
  ;;(message "continue-marking-in-timer %s %s" this-point buffer)
  (with-current-buffer buffer
    (let ((here (point)))
      (condition-case err
          (save-match-data ;; runs in timer
            (nxml-where-do-marking this-point buffer))
        (error
         (nxml-where-error-message
          "nxml-where-do-marking error: %s"
          err)))
      (goto-char here))))

(defun nxml-where-start-continue-in-timer (next-point buffer)
  ;;(message "start second")
  (condition-case err
      (setq nxml-where-once-update-timer
            (run-with-idle-timer idle-update-delay
                                 nil
                                 'nxml-where-continue-marking-in-timer
                                 next-point
                                 buffer))
    (error
     (nxml-where-error-message
      "nxml-where-start-second error %s" err))))

(defun nxml-where-restart-update ()
  "Restart update, runs in `post-command-hook'."
  ;;(message "restart-update")
  (condition-case err
      (save-match-data ;; runs in timer
        (unless (and nxml-where-last-point
                     (= nxml-where-last-point (point)))
          (setq nxml-where-last-point nil)
          (setq nxml-where-last-finished nil)
          (nxml-where-cancel-once)
          (setq nxml-where-once-update-timer
                (run-with-idle-timer
                 (* 0.2 idle-update-delay)
                 nil
                 'nxml-where-start-update-in-timer
                 (current-buffer)))))
    (error
     (nxml-where-error-message
      "%s" (error-message-string err)))))
(put 'nxml-where-restart-update 'permanent-local-hook t)

(defvar nxml-where-first-change-pos nil)
(make-variable-buffer-local 'nxml-where-first-change-pos)
(put 'nxml-where-first-change-pos 'permanent-local t)

(defun nxml-where-after-change (beg end len)
  (setq nxml-where-last-point nil)
  (setq nxml-where-first-change-pos
        (min beg
             (or nxml-where-first-change-pos
                 beg))))

(defun nxml-where-cancel-once ()
  (when (timerp nxml-where-once-update-timer)
    (cancel-timer nxml-where-once-update-timer)
    (setq nxml-where-once-update-timer nil)))

(defun nxml-where-update-buffers ()
  (when (boundp 'nxml-where-mode)
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (when nxml-where-mode
          (nxml-where-mode -1)
          (nxml-where-mode 1))))))

(defun nxml-where-stop-updating ()
  (remove-hook 'post-command-hook 'nxml-where-restart-update t))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Marking

(defconst nxml-where-get-id-pattern
  (rx-to-string
   `(and
     space
     ,(cons 'or nxml-where-header-attributes)
     (0+ space)
     ?=
     (0+ space)
     ?\"
     (0+ (not (any ?\")))
     ?\")
   t))

(defvar nxml-where-tag+id-pattern
  (rx ?<
      (submatch
       (1+ (char "-a-z0-9:"))
       )
      (0+ (1+ space)
          (1+ (any "a-z"))
          (0+ space)
          ?=
          (0+ space)
          ?\"
          (0+ (not (any ?\")))
          ?\"
          )
      (0+ space)
      (opt ?/)
      ?>))


(defvar nxml-where-forward-element nil)
(make-variable-buffer-local 'nxml-where-forward-element)
(put 'nxml-where-forward-element 'permanent-local t)

(defun nxml-where-unmark-forward-element ()
  "Unmark currently marked end tag."
  (when nxml-where-forward-element
    (let* ((ovl (nth 1 nxml-where-forward-element))
           (str (when ovl (buffer-substring-no-properties (overlay-start ovl) (overlay-end ovl)))))
      (when (overlayp ovl)
        ;;(message "unmark-forward-element:delete-overlay %s %s" str ovl)
        (delete-overlay ovl)))
    (setq nxml-where-forward-element nil)))

(defun nxml-where-mark-forward-element (start-tag)
  "Mark the end tag matching START-TAG."
  ;;(message "nxml-where-forward-element=%s" nxml-where-forward-element)
  (unless (and start-tag
               nxml-where-forward-element
               (nth 1 nxml-where-forward-element)
               (= (nth 0 nxml-where-forward-element)
                  start-tag))
    ;;(message "before unmark")
    (nxml-where-unmark-forward-element)
    ;;(message "after unmark")
    (when start-tag
      (let ((here (point))
            (end-of-narrow
             (progn
               (goto-char start-tag)
               (line-end-position 4)))
            start end ovl)
        ;; Fix-me: Narrow how much?
        (setq end-of-narrow (max (+ 4000 (window-end))
                                 end-of-narrow))
        (setq end-of-narrow (min (point-max)
                                 end-of-narrow))
        (save-restriction
          (narrow-to-region start-tag end-of-narrow)
          (condition-case err
              (progn
                (goto-char start-tag)
                (nxml-forward-element)
                (when (looking-back "</[a-z0-9]+>")
                  (setq start (match-beginning 0))
                  (setq end (point))
                  (setq ovl (make-overlay start end))
                  (overlay-put ovl 'nxml-where t)
                  (overlay-put ovl 'face nxml-where-marking)))
            (error
             (let ((msg (error-message-string err)))
               (unless (string= msg "Start-tag has no end-tag")
                 (message "nxml-where-mark-forw: %s" msg))))))
        (goto-char here)
        ;;(message "point 2 = %s" (point))
        (setq nxml-where-forward-element (list start-tag ovl))))))


(defun nxml-where-make-rec (tag-start tag-end tag-str buf)
  ;;(message "nxml-where-make-rec %s %s %s %s" tag-start tag-end tag-str buf)
  (let ((ovls (overlays-at tag-start))
        str)
    (dolist (ovl ovls)
      (when (overlay-get ovl 'nxml-where)
        (setq str (buffer-substring-no-properties (overlay-start ovl) (overlay-end ovl)))
        (message "==================================================")
        (nxml-where-error-message "old ovl=%s    %S" ovl str)
        (message "old: nxml-where-path=%s" nxml-where-path)
        (message "old: nxml-where-new-path=%s" nxml-where-new-path)
        )))
  (let ((ovl (when buf (make-overlay tag-start tag-end))))
    (when ovl
      (overlay-put ovl 'nxml-where t)
      (overlay-put ovl 'face nxml-where-marking))
    (list tag-start tag-end tag-str ovl)))

(defun nxml-where-delete-rec (rec from)
  (let* ((ovl (nth 3 rec))
         (str (when ovl
                (buffer-substring-no-properties (overlay-start ovl) (overlay-end ovl)))))
    (when (and ovl (overlay-buffer ovl))
      (assert (overlayp ovl) t)
      ;;(message "delete-rec:delete-overlay %s %s (%s)" str ovl from)
      (delete-overlay ovl)
      ;;(message "after delete=%s" ovl)
      )))


(defun nxml-where-clear-old-path (end-of-interest from)
  "Clear all marking below END-OF-INTEREST.
Update `nxml-where-path accordingly."
  (setq nxml-where-last-added nil)
  ;;(message "++++++ clear A: %s (%s)" end-of-interest from)
  (setq nxml-where-path (cons 'dummy nxml-where-path))
  (let ((path nxml-where-path))
    ;;(message "path 1=%s" path)
    (while (cdr path)
      ;;(message "path 2=%s" path)
      (when (> (nth 1 (cadr path)) end-of-interest)
        (dolist (p (cdr path))
          (nxml-where-delete-rec p "clear"))
        (setcdr path nil))
      (setq path (cdr path))))
  (setq nxml-where-path (cdr nxml-where-path)))

(defun nxml-where-clear-new-path ()
  (dolist (new nxml-where-new-path)
    (nxml-where-delete-rec new "clear new"))
  (setq nxml-where-new-path nil)
  ;;(message "clear B:nxml-where-path=%s" nxml-where-path)
  )


(defun nxml-where-update-where-path (tag-start tag-end tag-str buffer)
  "Update where path with given tag.
The tag is between TAG-START and TAG-END and the string to
display for it in the header-line is TAG-STR.  This is in buffer
BUFFER."
  ;; Delete old marks below tag-start:
  (nxml-where-clear-old-path (+ tag-end 0) (format "update-where-path, tag-start=%s" tag-start))
  ;; Is this now the same as the old value?
  (let ((last-old (last nxml-where-path))
        new-rec
        result)
    ;;(message "update: %s %s %s %s, last-old=%s" tag-start tag-end tag-str buffer last-old)
    (if (and last-old
             (= tag-start (nth 0 (car last-old)))
             (= tag-end   (nth 1 (car last-old))))
        (progn
          (setq result 'ready))
      (when nxml-where-only-inner
        ;;(message "last-old=%S, nwp=%S, nwnp=%S" last-old nxml-where-path nxml-where-new-path)
        (setq last-old (car (last nxml-where-path)))
        (when last-old
          (setq nxml-where-path nil)
          (nxml-where-delete-rec last-old "only-inner")))
      (setq new-rec (nxml-where-make-rec tag-start tag-end tag-str buffer))
      (setq nxml-where-last-added new-rec)
      (setq nxml-where-new-path (cons new-rec nxml-where-new-path))
      (setq result 'continue))
    result))

(defun nxml-where-do-marking (this-point buffer)
  "Do marking.
If THIS-POINT is nil then it is the first marking post command in
buffer BUFFER.  In that case start from current point, otherwise
from THIS-POINT.

Go up to previous tag.  Then check if this is the same tag where
we started last time and ran to completion.  If so just finish.

Otherwise check this tag.  If not ready after that then restart
this command with arg THIS-POINT set to right before this tag."
  ;;(message "****************nxml-where-do-marking %s %s, point=%s" this-point buffer (point))
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (save-restriction
        (when nxml-where-widen (widen))
        (let ((here (point))
              next-point
              (is-first (not this-point))
              (end-of-interest (if nxml-where-first-change-pos
                                   (min (point) nxml-where-first-change-pos)
                                 ;; Check for tag at point
                                 (catch 'eoi
                                   (let (ovl)
                                     (dolist (ovl (overlays-at (point)))
                                       (when (overlay-get ovl 'nxml-where)
                                         (throw 'eoi (overlay-end ovl)))))
                                   (point)))))
          ;; If on beginning of tag step forward one char.
          (unless (or (eobp)
                      this-point
                      (not (eq  ?< (char-after))))
            (forward-char))
          (when this-point (goto-char this-point))
          (setq next-point
                (catch 'cnext-point
                  (progn
                    (condition-case err
                        (nxml-backward-up-element)
                      (error
                       (if (equal err '(error "No parent element"))
                           (let (rec)
                             ;;(message "------------ No parent element")
                             (dolist (rec nxml-where-path)
                               (nxml-where-delete-rec rec "no parent"))
                             (setq nxml-where-path nil)
                             (throw 'cnext-point nil)) ;; <---- remember...
                         (nxml-where-error-message "nxml-where error: %S" err)
                         (throw 'cnext-point "uh?"))))
                    ;; Is this the first call
                    ;;(message ";; Is this the first call, %s" is-first)
                    (when is-first
                      (when (and nxml-where-path
                                 nxml-where-last-finished
                                 (= (point) (caar (last nxml-where-path))))
                        (throw 'cnext-point 'same-as-last))
                      ;;(setq nxml-where-new-path nil)
                      (setq nxml-where-last-added nil)
                      ;; Delete those parts we can't trust or don't
                      ;; need any more. Fix-me, Note: this is different
                      ;; dependent on if some buffer changes occured.
                      (nxml-where-clear-old-path end-of-interest (format "is-first,p=%s" (point)))
                      (nxml-where-clear-new-path))
                    ;;(message "looking-at")
                    (when (looking-at nxml-where-tag+id-pattern)
                      (let ((start (point))
                            (end (match-end 0))
                            (tag (match-string-no-properties 1))
                            (all (match-string-no-properties 0)))
                        (when nxml-where-tag+id
                          (when (string-match nxml-where-get-id-pattern all)
                            (setq tag (concat tag (match-string 0 all)))))
                        (setq tag (concat "<" tag ">"))
                        (when (or (eq 'ready
                                      (nxml-where-update-where-path start end tag t))
                                  nxml-where-only-inner)
                          ;;(message "throw 'cp nil")
                          (throw 'cnext-point nil))))
                    (throw 'cnext-point (max (1- (point)) (point-min))))))
          (goto-char here)
          (if next-point
              (cond
               ((stringp next-point) (message "%s" next-point) ;; Some error
                (when nxml-where-header (setq header-line-format next-point)))
               ((eq 'same-as-last next-point)
                nil)
               (t
                (unless nxml-where-only-inner
                  (setq nxml-where-once-update-timer
                        (run-with-timer (* 0.2 idle-update-delay)
                                        nil
                                        'nxml-where-start-continue-in-timer
                                        next-point (current-buffer))))))
            (if nxml-where-path
                (setcdr (last nxml-where-path) nxml-where-new-path)
              (setq nxml-where-path nxml-where-new-path))
            (setq nxml-where-new-path nil)
            ;;(message "nxml-where-path=%s" nxml-where-path)
            (nxml-where-mark-forward-element (caar (last nxml-where-path)))
            (setq nxml-where-last-finished t)
            (setq nxml-where-first-change-pos nil)
            (run-hooks 'nxhtml-where-hook)
            (setq nxml-where-last-point (point))
            (when (and nxml-where-header
                       (not nxml-where-only-inner))
              (nxml-where-insert-header))))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Header path

(defun nxml-where-insert-header ()
  (let ((path (mapcar (lambda (elt)
                        (nth 2 elt))
                      nxml-where-path)))
    (unless path
      (setq path (list (if (looking-at "[[:space:]]*\\'")
                           "(After last tag)"
                         "(Before first tag)"))))
    (if (null path)
        (setq path " *Error* ")
      ;; Throw away <html>
      (let* ((first (car path))
             (html "<html")
             (hlen (length html)))
        (when (and (> (length first) hlen)
                   (string= html (substring first 0 hlen)))
          (setq path (cdr path))))
      (unless path
        (setq path (list "(At html start)"))))
    (let* ((sp (substring (format "%s" path) 1 -1))
           (label " Path: ")
           (totlen (+ (length sp) (length label)))
           header)
      (when (> totlen (window-width))
        (setq sp (concat "... "
                         (substring sp (+ (- totlen (window-width))
                                          4)))))
      (setq header (concat label sp))
      (when nxml-where-header
        (setq header-line-format header)))))

(defvar nxml-where-saved-header-line-format nil)
(make-variable-buffer-local 'nxml-where-saved-header-line-format)
(put 'nxml-where-saved-header-line-format 'permanent-local t)

(defun nxml-where-save-header-line-format ()
  (unless nxml-where-saved-header-line-format
    (setq nxml-where-saved-header-line-format header-line-format)))

(defun nxml-where-restore-header-line-format ()
  (setq header-line-format nxml-where-saved-header-line-format))



(provide 'nxml-where)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; nxml-where.el ends here
