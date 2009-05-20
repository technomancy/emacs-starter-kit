;;; rcirc-color.el -- color nicks
;; Copyright 2005, 2006, 2007  Alex Schroeder

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of
;; the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE.  See the GNU General Public License for more details.

;; You should have received a copy of the GNU General Public
;; License along with this program; if not, write to the Free
;; Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
;; MA 02111-1307 USA

;;; Commentary:

;; Use /COLOR to list all colored nicks with their color
;; Use /COLOR NICK COLOR to color NICK using COLOR

;;; Code:

(require 'rcirc)

(defvar rcirc-colors
  (if (fboundp 'color-distance)
      (let ((min-distance (* 0.23 (color-distance "black" "white")))
            (bg (face-background 'default))
            (fg (face-foreground 'rcirc-my-nick))
            candidates)
        (dolist (item color-name-rgb-alist)
          (let ((color (car item)))
            (when (and (not (color-gray-p color))
                       (> (color-distance color bg) min-distance)
                       (> (color-distance color fg) min-distance))
              (setq candidates (cons color candidates)))))
        candidates)
    (delete (face-background 'default) (defined-colors)))
  "Colors to use for nicks in rcirc.
By default, all the non-grey colors that are very different from
the default background are candidates.  The minimum
color-distance is half the distance between black and red as
computed by `color-distance'.

To check out the list, evaluate (list-colors-display rcirc-colors).")

(defvar rcirc-color-mapping (make-hash-table :test 'equal)
  "Hash-map mapping nicks to color names.")

(defadvice rcirc-facify (before rcirc-facify-colors activate)
  "Add colors to other nicks based on `rcirc-colors'."
  (when (and (eq face 'rcirc-other-nick)
             (not (string= string "")))
    (let ((cell (gethash string rcirc-color-mapping)))
      (unless cell
        (setq cell (cons 'foreground-color
                         (elt rcirc-colors (random (length rcirc-colors)))))
        (puthash (substring-no-properties string) cell rcirc-color-mapping))
      (setq face (list cell)))))

(defun rcirc-markup-nick-colors (process sender response channel-buffer)
  (let ((target (with-current-buffer channel-buffer (or rcirc-target ""))))
    (with-syntax-table rcirc-nick-syntax-table
      (while (re-search-forward "\\w+" nil t)
        (let ((face (gethash (match-string-no-properties 0) rcirc-color-mapping)))
          (when face
            (rcirc-add-face (match-beginning 0) (match-end 0) face)))))))

(defun-rcirc-command color (args)
  "Change one of the nick colors."
  (interactive)
  (setq args (split-string args))
  (rcirc-do-color (car args) (cadr args) process target))

(defun rcirc-do-color (nick color process target)
  "Implement /COLOR."
  (if (not nick)
      (let (names)
        (maphash (lambda (key value)
                   (add-text-properties
                    0 (length key)
                    `(face (,value) help-echo ,(cdr value))
                    key)
                   (setq names (cons key names)))
                 rcirc-color-mapping)
        (rcirc-print process (rcirc-nick process) "NOTICE" target
                     (mapconcat 'identity names " ")))
    (unless color
      (error "Use what color?"))
    (puthash nick (cons 'foreground-color color) rcirc-color-mapping)))

(defadvice rcirc-handler-NICK (before rcirc-handler-NICK-colors activate)
  "Update colors in `rcirc-color-mapping'."
  (let* ((old-nick (rcirc-user-nick sender))
         (cell (gethash old-nick rcirc-color-mapping))
         (new-nick (car args)))
    ;; don't delete the old mapping
    (when cell
      (puthash new-nick cell rcirc-color-mapping))))

(provide 'rcirc-color)
;;; rcirc-color.el ends here