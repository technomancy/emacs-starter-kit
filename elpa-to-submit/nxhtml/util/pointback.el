;;; pointback.el --- Restore window points when returning to buffers

;; Copyright (C) 2009  Markus Triska

;; Author: Markus Triska <markus.triska@gmx.at>
;; Keywords: convenience

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; When you have two windows X and Y showing different sections of the
;; same buffer B, then switch to a different buffer in X, and then
;; show B in X again, the new point in X will be the same as in Y.
;; With pointback-mode, window points are preserved instead, and point
;; will be where it originally was in X for B when you return to B.

;; Use M-x pointback-mode RET to enable pointback-mode for a buffer.
;; Use M-x global-pointback-mode RET to enable it for all buffers.

;;; Code:

(require 'assoc)

(defconst pointback-version "0.2")

(defvar pointback-windows nil
  "Association list of windows to buffers and window points.")

(defun pointback-store-point ()
  "Save window point and start for the current buffer of the
selected window."
  (sit-for 0)                       ; redisplay to update window-start
  (let* ((buffers (cdr (assq (selected-window) pointback-windows)))
         (b (assq (current-buffer) buffers))
         (p (cons (point) (window-start))))
    (if b
        (setcdr b p)
      (let ((current (cons (current-buffer) p)))
        (aput 'pointback-windows (selected-window) (cons current buffers))))))

(defun pointback-restore ()
  "Restore previously stored window point for the selected window."
  (let* ((buffers (cdr (assq (selected-window) pointback-windows)))
         (b (assq (current-buffer) buffers))
         (p (cdr b)))
    (when b
      (goto-char (car p))
      (set-window-start (selected-window) (cdr p) t)))
  ;; delete dead windows from pointback-windows
  (dolist (w pointback-windows)
    (unless (window-live-p (car w))
      (adelete 'pointback-windows (car w))))
  ;; delete window points of dead buffers
  (dolist (w pointback-windows)
    (let (buffers)
      (dolist (b (cdr w))
        (when (buffer-live-p (car b))
          (push b buffers)))
      (aput 'pointback-windows (car w) buffers))))

;;;###autoload
(define-minor-mode pointback-mode
  "Restore previous window point when switching back to a buffer."
  :lighter ""
  (if pointback-mode
      (progn
        (add-hook 'post-command-hook 'pointback-store-point nil t)
        (add-hook 'window-configuration-change-hook
                  'pointback-restore nil t))
    (remove-hook 'post-command-hook 'pointback-store-point t)
    (remove-hook 'window-configuration-change-hook 'pointback-restore t)
    (setq pointback-windows nil)))

;;;###autoload
(define-globalized-minor-mode global-pointback-mode pointback-mode pointback-on)

(defun pointback-on ()
  (pointback-mode 1))

(provide 'pointback)
;;; pointback.el ends here
