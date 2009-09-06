;;; vline.el --- show vertical line mode.

;; Copyright (C) 2002, 2008 by Taiki SUGAWARA <buzz.taiki@gmail.com>

;; Author: Taiki SUGAWARA <buzz.taiki@gmail.com>
;; Keywords: faces, editing, emulating
;; Version: 1.04
;; Time-stamp: <2008-10-22 12:49:08 UTC taiki>
;; URL: http://www.emacswiki.org/cgi-bin/wiki/vline.el

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Usage
;; put followings your .emacs
;;   (require 'vline)
;;
;; if you display a vertical line, type M-x vline-mode. `vline-mode' doesn't
;; effect other buffers, because it is a buffer local minor mode. if you hide
;; a vertical line, type M-x vline-mode again.
;;
;; if you display a vertical line in all buffers, type M-x vline-global-mode.
;;
;; `vline-style' provides a display style of vertical line. see `vline-style' docstring.

;;; Changes
;; 2008-10-22 taiki
;; fix coding-system problem.
;; - Added vline-multiwidth-space-list
;; - Use ucs code-point for japanese fullwidth space.
;;
;; 2008-01-22 taiki
;; applied patch from Lennart Borgman
;; - Added :group 'vline
;; - Added defcustom vline-current-window-only
;; - Added header items to simplify for users

;;; Code:

(defvar vline-overlay-table-size 200)
(defvar vline-overlay-table (make-vector vline-overlay-table-size nil))
(defvar vline-line-char ?|)
(defvar vline-multiwidth-space-list
  (list
   ?\t
   (decode-char 'ucs #x3000)		; japanese fullwidth space
   ))

(defcustom vline-style 'face
  "*This variable holds vertical line display style.
Available values are followings:
`face'      : use face.
`compose'   : use composit char.
`mixed'     : use face and composit char."
  :type '(radio
          (const face)
          (const compose)
          (const mixed))
  :group 'vline)


(defface vline
  '((t (:background "gray90")))
  "*A default face for vertical line highlighting."
  :group 'vline)

(defcustom vline-face 'vline
  "*A face for vertical line highlighting."
  :type 'face
  :group 'vline)

(defcustom vline-current-window-only nil
  "*If non-nil then show column in current window only.
If the buffer is shown in several windows then show column only
in the currently selected window."
  :type 'boolean
  :group 'vline)

;;;###autoload
(define-minor-mode vline-mode
  "Display vertical line mode."
  :global nil
  :lighter " VL"
  :group 'vline
  (if vline-mode
        (add-hook 'post-command-hook 'vline-post-command-hook nil t)
    (vline-clear)
    (remove-hook 'post-command-hook 'vline-post-command-hook t)))

;;;###autoload
(define-minor-mode vline-global-mode
  "Display vertical line mode as globally."
  :global t
  :lighter " VL"
  :group 'vline
  (if vline-global-mode
        (add-hook 'post-command-hook 'vline-global-post-command-hook)
    (vline-clear)
    (remove-hook 'post-command-hook 'vline-global-post-command-hook)))


(defun vline-post-command-hook ()
      (when (and vline-mode (not (minibufferp)))
    (vline-show)))

(defun vline-global-post-command-hook ()
  (when (and vline-global-mode (not (minibufferp)))
    (vline-show)))

(defun vline-clear ()
  (mapcar (lambda (ovr)
            (and ovr (delete-overlay ovr)))
          vline-overlay-table))

(defun vline-show (&optional point)
  (vline-clear)
  (save-excursion
    (if point
        (goto-char point)
      (setq point (point)))
    (let* ((column (current-column))
           (i 0)
           (compose-p (memq vline-style '(compose mixed)))
           (face-p (memq vline-style '(face mixed)))
           (line-char (if compose-p vline-line-char ? ))
           (line-str (make-string 1 line-char)))
      (when face-p
        (setq line-str (propertize line-str 'face vline-face)))
      (goto-char (window-start))
      (while (and (< i (1- (window-height)))
                  (< i (length vline-overlay-table))
                  (not (eobp)))
        (move-to-column column)
        ;; non-cursor line only (workaround of eol probrem.
        (unless (= (point) point)
          ;; if column over the cursor's column (when tab or wide char is appered.
          (when (> (current-column) column)
            (backward-char))
          (let ((ovr (aref vline-overlay-table i))
                ;; consider a newline, tab and wide char.
                (str (concat (make-string (- column (current-column)) ? )
                             line-str))
                (char (char-after)))
            ;; create overlay if not found.
            (unless ovr
              (setq ovr (make-overlay 0 0))
              (overlay-put ovr 'rear-nonsticky t)
              (aset vline-overlay-table i ovr))

            ;; initialize overlay.
            (overlay-put ovr 'face nil)
            (overlay-put ovr 'before-string nil)
            (overlay-put ovr 'after-string nil)
            (overlay-put ovr 'invisible nil)
            (overlay-put ovr 'window
                         (if vline-current-window-only
                             (selected-window)
                           nil))

            (cond
	     ;; multiwidth space
	     ((memq char vline-multiwidth-space-list)
              (setq str
                    (concat str
                            (make-string (- (save-excursion (forward-char)
                                                            (current-column))
                                            (current-column)
                                            (string-width str))
                                         ? )))
              (move-overlay ovr (point) (1+ (point)))
              (overlay-put ovr 'invisible t)
              (overlay-put ovr 'after-string str))
             ;; eol
             ((eolp)
              (move-overlay ovr (point) (point))
              (overlay-put ovr 'after-string str))
             (t
              (cond
               (compose-p
                (let (str)
                  (when char
                    (setq str (compose-chars
                               char
                               (cond ((= (char-width char) 1)
                                      '(tc . tc))
                                     ((= (current-column) column)
                                      '(tc . tr))
                                     (t
                                      '(tc . tl)))
                               line-char))
                    (when face-p
                      (setq str (propertize str 'face vline-face)))
                    (move-overlay ovr (point) (1+ (point)))
                    (overlay-put ovr 'invisible t)
                    (overlay-put ovr 'after-string str))))
               (face-p
                (move-overlay ovr (point) (1+ (point)))
                (overlay-put ovr 'face vline-face)))))))
        (setq i (1+ i))
        (forward-line)))))

(provide 'vline)

;;; vline.el ends here
