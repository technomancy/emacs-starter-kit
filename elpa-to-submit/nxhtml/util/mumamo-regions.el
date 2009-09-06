;;; mumamo-regions.el --- user defined regions with mumamo
;;
;; Author: Lennart Borgman (lennart O borgman A gmail O com)
;; Created: 2009-05-31 Sun
;; Version: 0.5
;; Last-Updated: 2009-06-01 Mon
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
;; Add temporary mumamo chunks (called mumamo regions).  This are
;; added interactively from a highlighted region.
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
;; published by the Free Software Foundation; either version 3, or
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Internal side functions etc

(defvar mumamo-regions nil
  "List of active mumamo regions.  Internal use only.
The entries in this list should be like this

    \(OVL-DEF OVL-CHUNK)

where OVL-DEF is an overlay containing the definitions, ie `major-mode'.
OVL-CHUNK is the definitions set up temporarily for mumamo chunks.

The fontification functions in mumamo looks in this list, but the
chunk dividing functions defined by
`define-mumamo-multi-major-mode' does not.  The effect is that
the normal chunks exists regardless of what is in this list, but
fontification etc is overridden by what this list says.")
(make-variable-buffer-local 'mumamo-regions)
(put 'mumamo-regions 'permanent-local t)

(defun mumamo-add-region-1 (major start end buffer)
  "Add a mumamo region with major mode MAJOR from START to END.
Return the region.  The returned value can be used in
`mumamo-clear-region'.

START and END should be markers in the buffer BUFFER.  They may
also be nil in which case they extend the region to the buffer
boundaries."
  (unless mumamo-multi-major-mode
    (mumamo-temporary-multi-major))
  (or (not start)
      (markerp start)
      (eq (marker-buffer start) buffer)
      (error "Bad arg start: %s" start))
  (or (not end)
      (markerp end)
      (eq (marker-buffer end) buffer)
      (error "Bad arg end: %s" end))
  (let ((ovl (make-overlay start end)))
    (overlay-put ovl 'mumamo-region 'defined)
    (overlay-put ovl 'face 'mumamo-region)
    (mumamo-region-set-major ovl major)
    (setq mumamo-regions (cons (list ovl nil) mumamo-regions))
    (mumamo-mark-for-refontification (overlay-start ovl) (overlay-end ovl))
    (message "Added mumamo region from %d to %d" (+ 0 start) (+ 0 end))
    ovl))

(defun mumamo-clear-region-1 (region-entry)
  "Clear mumamo region REGION-ENTRY.
The entry must have been returned from `mumamo-add-region-1'."
  (let ((buffer (overlay-buffer (car region-entry)))
        (entry  (cdr region-entry)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (let ((ovl1 (car region-entry))
              (ovl2 (cadr region-entry)))
          (delete-overlay ovl1)
          (when ovl2
            (mumamo-mark-for-refontification (overlay-start ovl2) (overlay-end ovl2))
            (delete-overlay ovl2))
          (setq mumamo-regions (delete region-entry mumamo-regions)))))))

(defvar mumamo-region-priority 0)
(make-variable-buffer-local 'mumamo-region-priority)
(put 'mumamo-region-priority 'permanent-local t)

(defun mumamo-get-region-from-1 (point)
  "Return mumamo region values for POINT.
The return value is either mumamo chunk or a cons with
information about where regions starts to hide normal chunks.
Such a cons has the format \(BELOW . OVER) where each of them is
a position or nil."
  (when mumamo-regions
    (save-restriction
      (widen)
      (let* ((start nil)
             (end   nil)
             (major nil)
             hit-reg
             ret-val)
        (catch 'found-major
          (dolist (reg mumamo-regions)
            (assert (eq (overlay-get (car reg) 'mumamo-region) 'defined) t)
            (assert (or (not (cadr reg)) (overlayp (cadr reg))))
            (let* ((this-ovl (car reg))
                   (this-start (overlay-start this-ovl))
                   (this-end   (overlay-end this-ovl)))
               (when  (<= this-end point)
                 (setq start this-end))
               (when  (< point this-start)
                 (setq end this-start))
               (when (and (<= this-start point)
                          (< point this-end))
                 (setq major (overlay-get this-ovl 'mumamo-major-mode))
                 (setq start (max this-start (or start this-start)))
                 (setq end   (min this-end   (or end this-end)))
                 (setq hit-reg reg)
                 (throw 'found-major nil)))))
        (if major
            (progn
              (setq ret-val (nth 1 hit-reg))
              (when ret-val (assert (eq (overlay-get ret-val 'mumamo-region) 'used) t))
              (if ret-val
                  (move-overlay ret-val start end)
                (setq ret-val (make-overlay start end nil t nil)) ;; fix-me
                (setcar (cdr hit-reg) ret-val)
                (overlay-put ret-val 'mumamo-region 'used)
                (overlay-put ret-val 'priority ;; above normal chunks + chunks on chunks
                             (setq mumamo-region-priority (1+ mumamo-region-priority)))
                ;;(overlay-put ret-val 'face '(:background "chocolate")) ;; temporary
                (overlay-put ret-val 'mumamo-major-mode
                             (overlay-get (car hit-reg) 'mumamo-major-mode))))
          (setq ret-val (cons start end)))
        ;;(message "mumamo-get-region-from-1, point=%s ret-val=%s" point ret-val)
        ret-val))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; User side functions

(defun mumamo-temporary-multi-major ()
  "Turn on a temporary multi major mode from buffers current mode.
Define one if no one exists.  It will have no chunk dividing
routines.  It is meant mainly to be used with mumamo regions when
there is no mumamo multi major mode in the buffer and the user
wants to add a mumamo region \(which requires a multi major mode
to work)."
  (when mumamo-multi-major-mode
    (error "Mumamo is already active in buffer"))
  (let* ((temp-mode-name (concat "mumamo-1-"
                                 (symbol-name major-mode)))
         (temp-mode-sym (intern-soft temp-mode-name)))
    (unless temp-mode-sym
      (setq temp-mode-sym (intern temp-mode-name))
      (eval
       `(define-mumamo-multi-major-mode ,temp-mode-sym
          "Temporary multi major mode."
          ("Temporary" ,major-mode nil))))
    (funcall temp-mode-sym)))

(defface mumamo-region
  '((t (:background "white")))
  "Face for mumamo-region regions."
  :group 'mumamo)

;;;###autoload
(defun mumamo-add-region ()
  "Add a mumamo region.
Mumamo regions are like another layer of chunks above the normal chunks.
They does not affect the normal chunks, but they overrides them.

To create a mumamo region first select a visible region and then
call this function."
  (interactive)
  (if (not mark-active)
      (message (propertize "Please select a visible region first" 'face 'secondary-selection))
    (let ((beg (region-beginning))
          (end (region-end))
          (maj (mumamo-region-read-major)))
      (mumamo-add-region-1 maj (copy-marker beg) (copy-marker end) (current-buffer))
      (setq deactivate-mark t))))

;; (dolist (o (overlays-in (point-min) (point-max))) (delete-overlay o))
(defun mumamo-clear-all-regions ()
  "Clear all mumamo regions in buffer.
For information about mumamo regions see `mumamo-add-region'."
  (interactive)
  (while mumamo-regions
    (mumamo-clear-region-1 (car mumamo-regions))
    (setq mumamo-regions (cdr mumamo-regions)))
  (message "Cleared all mumamo regions"))

(defun mumamo-region-read-major ()
  "Prompt user for major mode.
Accept only single major mode, not mumamo multi major modes."
  (let ((major (read-command "Major mode: ")))
    (unless (major-modep major) (error "Not a major mode: %s" major))
    (when (mumamo-multi-major-modep major) (error "Multi major modes not allowed: %s" major))
    (when (let ((major-mode major))
            (derived-mode-p 'nxml-mode))
      (error "%s is based on nxml-mode and can't be used here" major))
    major))

(defun mumamo-region-at (point)
  "Return mumamo region at POINT."
   (let ((ovls (overlays-at (point))))
     (catch 'overlay
       (dolist (o ovls)
         (when (overlay-get o 'mumamo-region)
           (throw 'overlay o)))
       nil)))

(defun mumamo-region-set-major (ovl major)
  "Change major mode for mumamo region at point.
For information about mumamo regions see `mumamo-add-region'.

If run non-interactively then OVL should be a mumamo region and
MAJOR the major mode to set for that region."
  (interactive
   (list (or (mumamo-region-at (point))
             (error "There is no mumamo region at point"))
         (mumamo-region-read-major)))
  (overlay-put ovl 'mumamo-major-mode `(,major))
  (overlay-put ovl 'help-echo (format "Mumamo region, major mode `%s'" major)))

(defun mumamo-clear-region (ovl)
  "Clear the mumamo region at point.
For information about mumamo regions see `mumamo-add-region'.

If run non-interactively then OVL should be the mumamo region to
clear."
  (interactive
   (list (or (mumamo-region-at (point))
             (error "There is no mumamo region at point"))))
  (let ((region-entry (assoc ovl mumamo-regions)))
    (mumamo-clear-region-1 region-entry)))


(provide 'mumamo-regions)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; mumamo-regions.el ends here
