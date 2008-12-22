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

(eval-when-compile
  (require 'cl)
  (unless (featurep 'nxhtml-autostart)
    (let ((efn (expand-file-name "../autostart.el")))
      (load efn))
    (require 'nxml-mode)))

(defvar nxml-where-once-update-timer nil)
(make-variable-buffer-local 'nxml-where-once-update-timer)
(put 'nxml-where-once-update-timer 'permanent-local t)

(defun nxml-where-cancel-once ()
  (when (timerp nxml-where-once-update-timer)
    (cancel-timer nxml-where-once-update-timer)
    (setq nxml-where-once-update-timer nil)))

(defun nxml-where-start-second-in-timer (next-point buffer)
  ;;(message "start second")
  (condition-case err
      (setq nxml-where-once-update-timer
            (run-with-idle-timer idle-update-delay
                                 nil
                                 'nxml-where-do-marking-in-timer
                                 next-point
                                 buffer))
    (error
     (nxml-where-error-message
            "nxml-where-start-second error %s" err))))

(defun nxml-where-update-buffers ()
  (when (boundp 'nxml-where-mode)
    (dolist (buf (buffer-list))
      (when nxml-where-mode
        (nxml-where-mode -1)
        (nxml-where-mode 1)))))

(defun nxml-where-restart-update ()
  (condition-case err
      (unless (and nxml-where-last-point
                   (= nxml-where-last-point (point)))
        ;;(message "\n restart-update: this-command=%s, nxml-where-last-point=%s, point=%s" this-command nxml-where-last-point (point))
        (setq nxml-where-last-point nil)
        (setq nxml-where-last-finished nil)
        (nxml-where-cancel-once)
        (setq nxml-where-once-update-timer
              (run-with-idle-timer
               (* 0.2 idle-update-delay)
               nil
               'nxml-where-start-update-in-timer
               (current-buffer))))
    (error
     (nxml-where-error-message
      "%s" (error-message-string err)))))
(put 'nxml-where-restart-update 'permanent-local-hook t)

(defun nxml-where-stop-updating ()
  (remove-hook 'post-command-hook 'nxml-where-restart-update t))

(defun nxml-where-setup-updating ()
  (nxml-where-clear-path)
  (setq nxml-where-last-added nil)
  (setq nxml-where-last-point nil)
  (when (and nxml-where-header
             (not nxml-where-only-inner))
    (setq header-line-format "Started nxml-where-mode ..."))
  ;;(nxml-where-restart-update)
  (add-hook 'post-command-hook 'nxml-where-restart-update nil t))

(defgroup nxml-where nil
  "Customization group for nxml-where."
  :group 'nxhtml
  :group 'nxml)

(define-toggle nxml-where-tag+id t
  "Show tags + id in path if non-nil.
If nil show only tag names."
  :set (lambda (sym val)
         (set-default sym val)
         (nxml-where-update-buffers))
  :group 'nxml-where)

(define-toggle nxml-where-header t
  "Show header with XML-path if non-nil."
  :set (lambda (sym val)
         (set-default sym val)
         (nxml-where-update-buffers))
  :group 'nxml-where)

(define-toggle nxml-where-marks t
  "Show marks in buffer for XML-path if non-nil."
  :set (lambda (sym val)
         (set-default sym val)
         (nxml-where-update-buffers))
  :group 'nxml-where)

(define-toggle nxml-where-only-inner nil
  "Mark only inner-most tag."
  :set (lambda (sym val)
         (set-default sym val)
         (nxml-where-update-buffers))
  :group 'nxml-where)

(define-toggle nxml-where-only-tags-with-id t
  "Show only tags with id in the header line."
  :set (lambda (sym val)
         (set-default sym val)
         (nxml-where-update-buffers))
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
         (nxml-where-update-buffers))
  :group 'nxml-where)

(defcustom nxml-where-header-attributes '("id" "name")
  "List of attributes `nxml-where-header' should display."
  :type '(repeat string)
  :set (lambda (sym val)
         (set-default sym val)
         (nxml-where-update-buffers))
  :group 'nxml-where)

(defcustom nxml-where-widen t
  "If non-nil and narrowed widen before getting XML path."
  :type 'boolean
  :group 'nxml-where)


(defvar nxml-where-saved-header-line-format nil)
(make-variable-buffer-local 'nxml-where-saved-header-line-format)
(put 'nxml-where-saved-header-line-format 'permanent-local t)

(defun nxml-where-save-header-line-format ()
  (unless nxml-where-saved-header-line-format
    (setq nxml-where-saved-header-line-format header-line-format)))

(defun nxml-where-restore-header-line-format ()
  (setq header-line-format nxml-where-saved-header-line-format))

(defvar nxml-where-modes '(nxml-mode nxhtml-mode))

(defun nxml-where-is-nxml ()
  (or (derived-mode-p 'nxml-mode)
      (and (featurep 'mumamo)
           mumamo-multi-major-mode
           (let ((major-mode (mumamo-main-major-mode)))
             (derived-mode-p 'nxml-mode)))))

(defun nxml-where-mode-start ()
  ;;(message "START")
  (unless (nxml-where-is-nxml)
    (error "Can't display XML path since major mode is not nxml-mode child."))
  (add-hook 'after-change-major-mode-hook
            'nxml-where-turn-off-unless-nxml nil t)
  (nxml-where-save-header-line-format)
  (nxml-where-setup-updating))

(defun nxml-where-mode-stop ()
  ;;(message "STOP")
  (nxml-where-stop-updating)
  (nxml-where-unmark-forward-element)
  (nxml-where-restore-header-line-format)
  (nxml-where-clear-path))

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
  (when (derived-mode-p 'nxml-mode)
    (nxml-where-mode 1)))

;;;###autoload
(define-globalized-minor-mode nxml-where-global-mode nxml-where-mode
  nxml-where-turn-on-in-nxml-child
  :group 'nxml-where)
;; The problem with global minor modes:
(when (and nxml-where-global-mode
           (not (boundp 'define-global-minor-mode-bug)))
  (nxml-where-global-mode 1))

(defun nxml-where-start-update-in-timer (buffer)
  ;;(message "nxml-where-start-update buffer=%s (bufferp buffer)=%s" buffer (bufferp buffer))
  (condition-case err
      (when (and (bufferp buffer)
                 (buffer-live-p buffer))
        (with-current-buffer buffer
          (unless nxml-where-marks (nxml-where-clear-path))
          (unless nxml-where-header (setq header-line-format nil))
          (when (and nxml-where-mode
                     (or nxml-where-header nxml-where-marks))
            (nxml-where-do-marking nil buffer))))
    (error
     (nxml-where-error-message
      "nxml-where-start-update-in-timer error: %s" err))))

(defvar nxml-where-get-id-pattern
  (rx space
      (eval (cons 'or nxml-where-header-attributes))
      (0+ space)
      ?=
      (0+ space)
      ?\"
      (0+ (not (any ?\")))
      ?\"
      ))

(defvar nxml-where-tag+id-pattern
  ;;(insert ;; -------------------
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
      ?>)
  ;;) ;; -------------------
  )

;; (defun nxml-where-mark (start end)
;;   (let (ovl
;;         (ovls (overlays-at start)))
;;     (dolist (o ovls)
;;       (when (and (eq (overlay-get o 'face) nxml-where-marking)
;;                  (= start (overlay-start o))
;;                  (= end   (overlay-end o)))
;;         (setq ovl o)))
;;     (unless ovl
;;       (setq ovl (make-overlay start end))
;;       (overlay-put ovl 'face nxml-where-marking)
;;       (move-overlay ovl start end))
;;     ovl))

(defvar nxml-where-forward-element nil)
(make-variable-buffer-local 'nxml-where-forward-element)
(put 'nxml-where-forward-element 'permanent-local t)

(defun nxml-where-unmark-forward-element ()
  ;;(message "unmark-for %s" nxml-where-forward-element)
  (when nxml-where-forward-element
    (let ((ovl (nth 1 nxml-where-forward-element)))
      (when (overlayp ovl) (delete-overlay ovl)))
    (setq nxml-where-forward-element nil)))

(defun nxml-where-mark-forward-element (start-tag)
  ;;(message "point 0 = %s, start-tag=%s fe=%s" (point) start-tag nxml-where-forward-element)
  ;;(message "  path=%s" nxml-where-path)
  (unless (and nxml-where-forward-element
               (nth 1 nxml-where-forward-element)
               (= (nth 0 nxml-where-forward-element)
                  start-tag))
    ;;(message "h3re")
    (nxml-where-unmark-forward-element)
    (save-restriction
      ;;(message "point 1 = %s" (point))
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
        (narrow-to-region here end-of-narrow)
        (condition-case err
            (progn
              ;;(message "point X forw= %s" (point))
              (nxml-forward-element)
              (when (looking-back "</[a-z0-9]+>")
                (setq start (match-beginning 0))
                (setq end (point))
                (setq ovl (make-overlay start end))
                (overlay-put ovl 'face nxml-where-marking)))
          (error
           (let ((msg (error-message-string err)))
             (unless (string= msg "Start-tag has no end-tag")
               (message "nxml-where-mark-forw: %s" str)))))
        (widen)
        (goto-char here)
        ;;(message "point 2 = %s" (point))
        (setq nxml-where-forward-element (list start-tag ovl))))))

(defvar nxml-where-path nil)
(make-variable-buffer-local 'nxml-where-path)
(put 'nxml-where-path 'permanent-local t)

(defun nxml-where-make-rec (tag-start tag-end tag-str buf)
  ;;(message "nxml-where-make-rec %s %s %s %s" tag-start tag-end tag-str buf)
  (let ((ovls (overlays-at tag-start)))
    (dolist (ovl ovls)
      (when (equal (overlay-get ovl 'face) nxml-where-marking)
        (nxml-where-error-message "old ovl=%s" ovl))))
  (let ((ovl (when buf (make-overlay tag-start tag-end))))
    (when ovl (overlay-put ovl 'face nxml-where-marking))
    (list tag-start tag-end tag-str ovl)))

(defun nxml-where-delete-rec (rec)
  ;;(message "delete-overlay rec=%s" rec)
  (let ((ovl (nth 3 rec)))
    (when ovl
      (assert (overlayp ovl) t)
      ;;(message "delete-overlay %s" ovl)
      (delete-overlay ovl))))

(defvar nxml-where-last-point nil)
(make-variable-buffer-local 'nxml-where-last-point)
(put 'nxml-where-last-point 'permanent-local t)

(defvar nxml-where-last-finished nil)
(make-variable-buffer-local 'nxml-where-last-finished)
(put 'nxml-where-last-finished 'permanent-local t)

(defvar nxml-where-last-added nil)
(make-variable-buffer-local 'nxml-where-last-added)
(put 'nxml-where-last-added 'permanent-local t)

(defun nxml-where-clear-path ()
  (setq nxml-where-last-added nil)
  (nxml-where-update-where-path -1 nil nil nil))

(defun nxml-where-update-where-path (tag-start tag-end tag-str buffer)
  ;;(message "nxml-where-last-added=%s" nxml-where-last-added)
  ;;
  ;; Fix-me: Deletion is not handled correctly when moving quickly.
  (when nxml-where-only-inner
    (setq nxml-where-path
          (mapcar (lambda (rec)
                    (if (and (= tag-start (nth 0 rec))
                             (= tag-end   (nth 0 rec))
                             (string= tag-str (nth 2 rec)))
                        rec
                      (nxml-where-delete-rec rec)
                      nil))
                  nxml-where-path))
    (setq nxml-where-path (delq nil nxml-where-path)))
  (unless nxml-where-last-added
    ;; Trim off everything below tag-start.
    (let ((ptr nxml-where-path)
          prev-ptr
          old-rec
          tail)
      (while (and ptr
                  (<= (nth 0 (car ptr)) (1+ tag-start)))
        (setq prev-ptr ptr)
        (setq ptr (cdr ptr)))
      ;;(message "trimming tag-start=%s, tag-end=%s, tail ptr at=%s, nxml-where-last-added=%s" tag-start tag-end (caar ptr) nxml-where-last-added)
      (when ptr
        ;;(setq tail (cdr ptr))
          (setq tail ptr)
          (if prev-ptr
              (setcdr prev-ptr nil)
            (setq nxml-where-path nil)))
      ;; Clean up tail
      ;;(message "tail=%s" tail)
      (dolist (rec tail)
        (nxml-where-delete-rec rec))))
  ;; If tag-end is nil we are just clearing
  (when tag-end
    (let (result)
      ;; Check we are sane
      (if (or (not nxml-where-last-added)
              (< tag-start (nth 0 nxml-where-last-added)))
          (setq nxml-where-last-added t)
        ;;(message "nxml-where internal error: tag-start below last added, %s < %s" tag-start nxml-where-last-added)
        (setq nxml-where-last-added nil))
      (when nxml-where-last-added
        ;; Find out where to insert
        (let ((ptr nxml-where-path)
              prev-ptr
              old-rec
              new-rec)
          ;;(message "before while ptr=%s" ptr)
          (while (and ptr (< (nth 0 (car ptr)) tag-start))
            (setq prev-ptr ptr)
            (setq ptr (cdr ptr))
            )
          ;; Is this now the same as the old value?
          ;;(message "Same? ptr=%s" ptr)
          (if (and ptr
                   (= tag-start (nth 0 (car ptr)))
                   (= tag-end   (nth 1 (car ptr))))
              (progn
                ;;(message "vvvvvvvvvvvvvvvvv")
                ;;(message "ready tag-start=%s-%s tag-str=%s nxml-where-last-added=%s" tag-start tag-end tag-str nxml-where-last-added)
                (when (eq nxml-where-last-added t)
                  (setq nxml-where-last-added (car (last nxml-where-path))))
                ;; Clean up between here and last added. Unfortunately
                ;; we can't catch everything here. We need to walk up
                ;; the list to check, but that takes some time so do
                ;; not do that here.
                ;;
                ;; Fix-me: implement the walk up above.

                ;;(message "trimming 2 tag-start=%s, tag-end=%s, ptr at=%s, nxml-where-last-added=%s" tag-start tag-end (caar ptr) nxml-where-last-added)
                ;;(message "ptr 1=%s" ptr)
                (while (and ptr ;; Not needed, but just in case to prevent looping ...
                            (cdr ptr)
                            (not (eq nxml-where-last-added (cadr ptr))))
                  ;; Delete entry after ptr.
                  ;;(message "ptr=%s" ptr)
                  (setq old-rec (cadr ptr))
                  (nxml-where-delete-rec old-rec)
                  (setcdr ptr (cddr ptr))
                  )
                (setq nxml-where-last-finished t)
                (setq result 'ready))
            (setq new-rec (nxml-where-make-rec tag-start tag-end tag-str buffer))
            (setq nxml-where-last-added new-rec)
            (if (not prev-ptr)
                  (setq nxml-where-path (cons new-rec nxml-where-path))
                (setcdr prev-ptr (cons new-rec (cdr prev-ptr))))
            (setq result 'continue))))
      ;;(message "Mark forward path=%s" nxml-where-path)
      (nxml-where-mark-forward-element (caar (last nxml-where-path)))
      result)))

;;(nxhtmltest-nxml-where-adding)
;;(global-set-key [f9] 'nxhtmltest-nxml-where-adding)
(defun nxhtmltest-nxml-where-add1 (rec)
  (let* ((tag-start (nth 0 rec))
         (tag-end   (nth 1 rec))
         (tag-str   (nth 2 rec))
         (res
          (nxml-where-update-where-path tag-start tag-end tag-str nil)))
    (message "*** (%s %s %s) => %s, last=%s" tag-start tag-end tag-str res nxml-where-last-added)
    (message "nxml-where-path=%s" nxml-where-path)
    ))

(defun nxhtmltest-nxml-where-adding ()
  (interactive)
  (let ((nxml-where-path nil)
        (nxml-where-last-added nil)
        rec)
    (message "=====================")
    (assert (equal nxml-where-path nil) t)
    (nxhtmltest-nxml-where-add1 (setq rec '(50 55 "<sub>" nil)))
    (assert (equal nxml-where-path `(,rec)) t)
    (nxhtmltest-nxml-where-add1 (setq rec '(30 35 "<bla>" nil)))
    (assert (equal nxml-where-path `(,rec
                                     (50 55 "<sub>" nil))) t)
    (message "------ A") (setq nxml-where-last-added nil)
    (nxhtmltest-nxml-where-add1 (setq rec '(50 55 "<sub>" nil)))
    (assert (equal nxml-where-path `((30 35 "<bla>" nil)
                                     (50 55 "<sub>" nil))) t)
    (message "------ B") (setq nxml-where-last-added nil)
    (nxhtmltest-nxml-where-add1 (setq rec '(50 55 "<sub>")))
    (assert (equal nxml-where-path `((30 35 "<bla>" nil)
                                     (50 55 "<sub>" nil))) t)
    (nxhtmltest-nxml-where-add1 (setq rec '(30 35 "<bla>" nil)))
    (assert (equal nxml-where-path `((30 35 "<bla>" nil)
                                     (50 55 "<sub>" nil))) t)
    (message "------ C") (setq nxml-where-last-added nil)
    (nxhtmltest-nxml-where-add1 (setq rec '(40 45 "<bla>" nil)))
    (assert (equal nxml-where-path `((30 35 "<bla>" nil)
                                     ,rec)) t)
    (nxhtmltest-nxml-where-add1 (setq rec '(50 55 "<bla>" nil)))
    (assert (eq nxml-where-last-added nil) t)
    (message "---- Clear")
    (nxml-where-clear-path)
    (assert (eq nxml-where-path nil) t)
    ))

(defun nxml-where-do-marking-in-timer (this-point buffer)
  (condition-case err
      (nxml-where-do-marking this-point buffer)
    (error
     (nxml-where-error-message
      "nxml-where-do-marking error: %s"
      err))))

;; (nxml-where-error-message "%s" "something")
(defun nxml-where-error-message (format-string &rest args)
  (with-current-buffer (get-buffer-create "*Messages*")
    (let ((start (1+ (point-max))))
      (apply 'message format-string args)
      (goto-char (point-max))
      (backward-char)
      (put-text-property start (point)
                         'face 'highlight))))

(defun nxml-where-do-marking (this-point buffer)
  ;;(message "nxml-where-do-marking %s %s, point=%s" this-point buffer (point))
  (with-current-buffer buffer
    (save-restriction
      (when nxml-where-widen (widen))
      (let (ovl
            (here (point))
            next-point
            (is-first (not this-point))
            same-as-last)
        ;; If on beginning of tag step forward one char.
        (unless (or (eobp)
                    this-point
                    (not (eq  ?< (char-after))))
          (forward-char))
        (unless this-point (setq nxml-where-last-added nil))
        (unless this-point (setq this-point (point)))
        (goto-char this-point)
        (setq next-point
              (catch 'cnext-point
                ;; Are we ready?
                (condition-case err
                    (nxml-backward-up-element)
                  (error
                   (if (equal err '(error "No parent element"))
                       (throw 'cnext-point nil)
                     (nxml-where-error-message "nxml-where error: %S" err)
                     (throw 'cnext-point "uh?"))))
                ;; Is this the first call
                (when is-first
                  ;;(message "is-first=%s, (last nxml-where-path=%s" is-first (last nxml-where-path))
                  (setq same-as-last
                        (and nxml-where-path
                             nxml-where-last-finished
                             (= (point) (caar (last nxml-where-path)))))
                  (when same-as-last
                    (throw 'cnext-point 'same-as-last)))
                (save-match-data
                  (when (looking-at nxml-where-tag+id-pattern)
                    (let ((start (point))
                          (end (match-end 0))
                          (tag (match-string-no-properties 1))
                          (all (match-string-no-properties 0))
                          result)
                      (when nxml-where-tag+id
                        (when (string-match nxml-where-get-id-pattern all)
                          (setq tag (concat tag (match-string 0 all)))))
                      (setq tag (concat "<" tag ">"))
                      (setq result (nxml-where-update-where-path start end tag t))
                      (when (or (eq result 'ready)
                                nxml-where-only-inner)
                        (throw 'cnext-point nil))
                      ;;(message "here 2")
                      )))
                (throw 'cnext-point (max (1- (point)) (point-min)))))
                                        ;)
        (goto-char here)
        ;;(message "next-point=%s" next-point)
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
                                      'nxml-where-start-second-in-timer
                                      next-point (current-buffer))))))
          (setq nxml-where-last-point (point))
          (when (and nxml-where-header
                     (not nxml-where-only-inner))
            (nxml-where-write-header)))))))

(defun nxml-where-write-header ()
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


(provide 'nxml-where)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; nxml-where.el ends here
