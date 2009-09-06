;;; gpl.el --- Highlight and edit gpl color palettes

(defconst gpl:version "0.01")
;; Copyright (C) 2008  Niels Giesen

;; Author: Niels Giesen
;; Keywords: extensions, tools

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

;; GPL provides font-locking and has functions to edit the values
;; of colors (hue, saturation value, red, green and blue vals)
;; in-place in a simple, intuitive, and lightweight fashion. See the
;; documentation of `gpl-mode'.

;; The methods and keybindings used are roughly the same as in the new
;; css-color mode. I should maybe have abstracted both color notation
;; models better, but did not feel like it. With under 200 lines of
;; code, it did not seem worth the effort.

;; The css-color.el used is the one by Niels Giesen, at
;; `http://niels.kicks-ass.org/public/elisp/css-color.el'.

;; Installation:

;; Put this file in your load-path. Put a declaration such as

;; (autoload 'gpl-mode "gpl")
;; (add-to-list 'auto-mode-alist
;; 	     '("\\.gpl\\'" . gpl-mode))

;; In your initialization file (e.g. ~/.emacs) to make sure `gpl-mode'
;; is started anytime you open a *.gpl file, and gpl-mode is only
;; loaded when needed.

;;; Code:
(require 'css-color)

(defvar gpl-keywords
  '(("^[[:space:]]*\\([a-fA-F[:digit:]]\\{1,3\\}\\)[[:space:]]+\\([a-fA-F[:digit:]]\\{1,3\\}\\)[[:space:]]+\\([a-fA-F[:digit:]]\\{1,3\\}\\)"
     (0
      (let ((color (concat "#" (apply 'css-color-rgb-to-hex
				      (mapcar 'string-to-number
					      (list
					       (match-string-no-properties 1)
					       (match-string-no-properties 2)
					       (match-string-no-properties 3)))))))

	(put-text-property (match-beginning 0)
			   (match-end 0)
			   'keymap gpl-map)
	(put-text-property (match-beginning 0)
			   (match-end 0)
			   'face (list :background
				       color
				       :foreground
				       (css-color-foreground-color
					color))))))))

;;;###autoload
(define-derived-mode gpl-mode fundamental-mode "GPL"
  "Mode for font-locking and editing color palettes of the GPL format.

Such palettes are used and produced by free software applications
such as the GIMP, Inkscape, Scribus, Agave and on-line tools such
as http://colourlovers.com.

You can also use
URL `http://niels.kicks-ass.org/public/elisp/css-palette.el' to import
such palette into a css-file as hexadecimal color palette."
  (setq font-lock-defaults
	'((gpl-keywords)
	  t)))

(defvar gpl-map
  (let ((m (make-sparse-keymap)))
    (define-key m "=" 'gpl-up)
    (define-key m "-" 'gpl-down)
    (define-key m "h" 'gpl-hue-up)
    (define-key m "H" 'gpl-hue-down)
    (define-key m "v" 'gpl-value-up)
    (define-key m "V" 'gpl-value-down)
    (define-key m "s" 'gpl-saturation-up)
    (define-key m "S" 'gpl-saturation-down)
    m)
  "Mode map for `gpl-mode'")

(defun gpl-get-color-at-point ()
  (or (get-text-property (point) 'color)
      (apply 'css-color-rgb-to-hsv
	     (gpl-get-rgb-list-at-point))))

(defun gpl-get-rgb-list-at-point ()
  (mapcar 'string-to-number
	  (split-string
	   (buffer-substring-no-properties
	    (point-at-bol)
	    (+ 11 (point-at-bol))) "[[:space:]]+" t)))

(defun gpl-replcolor-at-p (fun increment)
  (let ((pos (point)))
    (beginning-of-line)
    (insert
     (funcall fun
	      (gpl-get-color-at-point)
	      increment))
    (delete-region (point) (+ (point) 11))
    (goto-char pos)))

(defun gpl-hsv-to-gimp-color (h s v)
  (propertize
   (apply 'format "%3d %3d %3d"
	  (css-color-hsv-to-rgb h s v))
   'keymap gpl-map
   'color (list h s v)))

(defun gpl-what-channel ()
  (/ (- (point) (point-at-bol)) 4))

(defun gpl-adjust-channel-at-p (incr)
  (interactive "p")
  (let ((pos (point))
	(channel (gpl-what-channel)))
    (beginning-of-line)
    (let ((rgb
	   (gpl-get-rgb-list-at-point)))
      (setf (nth channel rgb)
	    (css-color-within-bounds
	     (+ incr (nth channel rgb))
	     0 255))
      (delete-region (point) (+ 11 (point)))
      (insert
       (propertize
	(apply 'format "%3d %3d %3d" rgb)
	'keymap gpl-map
	'color nil)))
    (goto-char pos)))

(defun gpl-inchue (color incr)
  (destructuring-bind (h s v) color
    (gpl-hsv-to-gimp-color
     (+ incr h) s v)))

(defun gpl-incsat (color incr)
  (destructuring-bind (h s v) color
    (gpl-hsv-to-gimp-color
      h (css-color-within-bounds (+ incr s) 0 100) v)))

(defun gpl-incval (color incr)
  (destructuring-bind (h s v) color
    (gpl-hsv-to-gimp-color
     h s (css-color-within-bounds (+ incr v) 0 100))))

(defun gpl-adj-hue-at-p (increment)
  (interactive "p")
  (gpl-replcolor-at-p 'gpl-inchue increment))

(defun gpl-adj-saturation-at-p (increment)
  (interactive "p")
  (gpl-replcolor-at-p 'gpl-incsat increment))

(defun gpl-adj-value-at-p (increment)
  (interactive "p")
  (gpl-replcolor-at-p 'gpl-incval increment))

;; channels (r, g, b)
(defun gpl-up (val)
  (interactive "p")
  (gpl-adjust-channel-at-p val))

(defun gpl-down (val)
  (interactive "p")
  (gpl-adjust-channel-at-p (- val)))
;; hue
(defun gpl-hue-up (val)
  (interactive "p")
  (gpl-adj-hue-at-p val))

(defun gpl-hue-down (val)
  (interactive "p")
  (gpl-adj-hue-at-p (- val)))
;; saturation
(defun gpl-saturation-up (val)
  (interactive "p")
  (gpl-adj-saturation-at-p val))

(defun gpl-saturation-down (val)
  (interactive "p")
  (gpl-adj-saturation-at-p (- val)))
;; value
(defun gpl-value-up (val)
  (interactive "p")
  (gpl-adj-value-at-p val))

(defun gpl-value-down (val)
  (interactive "p")
  (gpl-adj-value-at-p (- val)))

(provide 'gpl)
;;; gpl.el ends here
