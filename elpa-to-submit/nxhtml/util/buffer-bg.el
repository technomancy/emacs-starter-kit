;;; buffer-bg.el --- Changing background color of windows
;;
;; Author: Lennart Borgman (lennart O borgman A gmail O com)
;; Created: 2008-05-22T19:06:23+0200 Thu
;; Version: 0.5
;; Last-Updated: 2008-05-22T23:19:55+0200 Thu
;; URL: http://www.emacswiki.org/cgi-bin/wiki/buffer-bg.el
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
;; There is currently no way to change background colors of Emacs
;; windows. This library implements a workaround using overlays.
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
;; published by the Free Software Foundation; either version 2, or
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

(defvar buffer-bg-overlay nil)
(put 'buffer-bg-overlay 'permanent-local t)

(defun buffer-bg-set-color (color buffer)
  "Add an overlay with background color COLOR to buffer BUFFER.
If COLOR is nil remove previously added overlay."
  (interactive
   (let ((color (read-color "Background color (empty string to remove): " nil t)))
     (when (= 0 (length color))
       (setq color nil))
     (list color (current-buffer))
     ))
  (if (not color)
      (when buffer-bg-overlay
        (delete-overlay buffer-bg-overlay)
        (setq buffer-bg-overlay nil))
    (save-restriction
      (widen)
      (setq buffer-bg-overlay
            (make-overlay (point-min) (point-max) nil nil t))
      ;; Let the overlay have priority 0 which is the lowest.
      (overlay-put buffer-bg-overlay 'face (list :background color)))))


(provide 'buffer-bg)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; buffer-bg.el ends here
