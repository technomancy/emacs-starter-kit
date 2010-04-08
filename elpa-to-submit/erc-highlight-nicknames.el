;;; erc-highlight-nicknames.el --- Highlights nicknames

;; Copyright (C) 2007  André Riemann
;; Copyright (C) 2008  Andy Stewart

;; Author: André Riemann  <andre.riemann@web.de>
;; Maintainer: André Riemann  <andre.riemann@web.de>
;; Created: 2007-09-25
;; Keywords: comm, faces

;; URL: http://www.emacswiki.org/cgi-bin/wiki/ErcHighlightNicknames
;; Compatibility: tested with
;;   * GNU Emacs 23.0 (Erc 5.2, 5.3)
;;   * XEmacs 21.4 (Erc 5.1) (briefly)
;; Version: 0.4.1
;; Last-Updated: 2008-12-07
;; By: Andy Stewart

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING. If not, write to the Free
;; Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
;; MA 02110-1301, USA.

;;; Commentary:
;; To activate this module, put this in your init file
;;     (require 'erc-highlight-nicknames)
;; and add highlight-nicknames to `erc-modules' by customizing it.

;; Or put this in your init file
;;     (and
;;      (load-library "erc-highlight-nicknames")
;;      (add-to-list 'erc-modules 'highlight-nicknames)
;;      (erc-update-modules))

;; Press
;;     M-x customize-face RET erc-highlight-nick-base-face RET
;; to customize the face that will be added before the color, if you want
;; for example all the nicks underlined.

;;; Change Log:
;; 2008-12-07  Andy Stewart
;;   * There was a bug that function `erc-highlight-nicknames' created
;;     many faces for the same nickname. It would create a new one when
;;     erc inserts text, and didn't care whether a face already existed
;;     for this nickname.
;;     Now I added a hash table `erc-highlight-face-table' to save the
;;     face for a nickname. Faces are now only created, if the nickname
;;     doesn't occur in the hash table.
;; 2007-12-24  andre-r
;;   * bug fixed in invert-color where color code contained spaces instead
;;     of leading zeros
;; 2007-12-12  andre-r
;;   * erc-highlight-nick-base-face is by default empty now,
;;     not inherited from `default'
;;   * erc-button-add-face instead of put-text-property
;;   * using `x-color-values' instead of `color-values' since XEmacs
;;     seams to only know the former
;; 2007-12-12  andre-r
;;   * built in XEmacs-compatible code for text properties by Dave Marquardt
;;     (works both in Emacs and XEmacs)
;;   * changed it a bit so that the face for the text property is derived
;;     from another face (hopefully it still works with XEmacs)
;;   * for that purpose created a new, customizable face
;; 2007-09-25  andre-r
;;   * inital release

;; This file is *NOT* part of GNU Emacs.

;;; Code:

(require 'erc)
(require 'erc-button)

(defface erc-highlight-nick-base-face
  '((t nil))
  "Base face used for highlighting nicks in erc. (Before the nick
color is added)"
  :group 'erc-faces)

(defvar erc-highlight-face-table
  (make-hash-table :test 'equal)
  "The hash table that contains unique erc nickname faces.")

(defun hexcolor-luminance (color)
  "Returns the luminance of color COLOR. COLOR is a string \(e.g.
\"#ffaa00\", \"blue\"\) `color-values' accepts. Luminance is a
value of 0.299 red + 0.587 green + 0.114 blue and is always
between 0 and 255."
  (let* ((values (x-color-values color))
         (r (car values))
         (g (car (cdr values)))
         (b (car (cdr (cdr values)))))
    (floor (+ (* 0.299 r) (* 0.587 g) (* 0.114 b)) 256)))

(defun invert-color (color)
  "Returns the inverted color of COLOR."
  (let* ((values (x-color-values color))
         (r (car values))
         (g (car (cdr values)))
         (b (car (cdr (cdr values)))))
    (format "#%04x%04x%04x"
            (- 65535 r) (- 65535 g) (- 65535 b))))

;;;###autoload
(defun erc-highlight-nicknames ()
  "Searches for nicknames and highlights them. Uses the first
twelve digits of the MD5 message digest of the nickname as
color (#rrrrggggbbbb)."
  (with-syntax-table erc-button-syntax-table
    (let (bounds word color new-nick-face)
      (goto-char (point-min))
      (while (re-search-forward "\\w+" nil t)
        (setq bounds (bounds-of-thing-at-point 'word))
        (setq word (buffer-substring-no-properties
                    (car bounds) (cdr bounds)))
        (when (erc-get-server-user word)
          (setq new-nick-face (gethash word erc-highlight-face-table))
          (unless new-nick-face
            (setq color (concat "#" (substring (md5 (downcase word)) 0 12)))
            (if (equal (cdr (assoc 'background-mode (frame-parameters))) 'dark)
                ;; if too dark for background
                (when (< (hexcolor-luminance color) 85)
                  (setq color (invert-color color)))
              ;; if to bright for background
              (when (> (hexcolor-luminance color) 170)
                (setq color (invert-color color))))
            (setq new-nick-face (make-symbol (concat "erc-highlight-nick-" word "-face")))
            (copy-face 'erc-highlight-nick-base-face new-nick-face)
            (set-face-foreground new-nick-face color)
            (puthash word new-nick-face erc-highlight-face-table))
          (erc-button-add-face (car bounds) (cdr bounds) new-nick-face))))))

(define-erc-module highlight-nicknames nil
  "Search through the buffer for nicknames, and highlight."
  ((add-hook 'erc-insert-modify-hook 'erc-highlight-nicknames t))
  ((remove-hook 'erc-insert-modify-hook 'erc-highlight-nicknames)))

(provide 'erc-highlight-nicknames)

;;; erc-highlight-nicknames.el ends here
