;;; rainbow-delimiters.el --- Color nested parentheses, brackets, and braces according to their depth.

;; Copyright (C) 2010-2011 Jeremy L. Rayman.
;; Author: Jeremy L. Rayman <jeremy.rayman@gmail.com>
;; Maintainer: Jeremy L. Rayman <jeremy.rayman@gmail.com>
;; Created: 2010-09-02
;; Version: 1.2.1
;; Keywords: faces, convenience, lisp, matching, tools
;; EmacsWiki: RainbowDelimiters
;; URL: http://www.emacswiki.org/emacs/rainbow-delimiters.el

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

;; This is a "rainbow parentheses" mode which includes support for
;; parens "()", brackets "[]", and braces "{}". It conveys nesting
;; depth by using a different face for each level.  It colors all
;; statements at a given level using the same color - if several
;; statements are all at the same nested depth, they will all be the
;; same color.
;;
;; Great care has been taken to make this mode FAST. You should see no
;; discernible change in scrolling or editing speed while using it,
;; even with delimiter-rich languages like Clojure, Lisp, and Scheme.
;;
;; The one exception is with extremely large nested data structures
;; having hundreds of delimiters; in that case there will be a brief
;; pause to colorize the structure the very first time it is displayed
;; on screen; from then on editing this structure will perform at full
;; speed.
;;
;; Default colors have been chosen with the philosophy that it's
;; better be less intrusive than to be more colorful. Color schemes
;; are always a matter of taste.  If you do take the time to design a
;; new color scheme, please post it on the EmacsWiki page!
;; URL: http://www.emacswiki.org/emacs/RainbowDelimiters

;;; Installation:

;; 1. Place rainbow-delimiters.el on your emacs load-path.
;;
;; 2. Compile the file (necessary for speed):
;; M-x byte-compile-file <location of rainbow-delimiters.el>
;;
;; 3. Add the following to your dot-emacs/init file:
;; (require 'rainbow-delimiters)
;;
;; 4. Add hooks for modes where you want it enabled, for example:
;; (add-hook 'clojure-mode-hook 'rainbow-delimiters-mode)
;;
;; - To activate rainbow-delimiters mode temporarily in a buffer:
;; M-x rainbow-delimiters-mode

;;; Customization:

;; To customize various options, including the color scheme:
;; M-x customize-group rainbow-delimiters
;;
;; color-theme.el users:
;; If you use the color-theme package, you can specify custom colors
;; by adding the appropriate faces to your theme.
;; - Faces take the form of:
;;   'rainbow-delimiters-depth-#-face' with # being the depth.
;;   Depth begins at 1, the outermost color.
;;   Faces exist for depths 1-12.
;; - The unmatched delimiter face is:
;;   'rainbow-delimiters-unmatched-delimiter-face'

;;; Change Log:

;; 1.0 - Initial release.
;; 1.1 - Stop tracking each delimiter's depth independently.
;;       This had lead to confusing results when viewing clojure
;;       code. Instead, just color based on current nesting inside
;;       all delimiters combined.
;;     - Added 'all-delimiters' faces to apply a color scheme to
;;       all delimiters at once. Other faces inherit from this group.
;; 1.1.1 - Change color scheme to a lighter, more subtle style.
;; 1.1.2: (2011-03-25)
;;  - Add an unmatched-delimiter face and correct problem with
;;    coloring of text following unmatched closing delims.
;; 1.2: (2011-03-28)
;;  - Unify delimiter faces: all delimiter types now use the same depth
;;    faces, of form 'rainbow-delimiters-depth-#-face'.
;; 1.2.1: (2011-03-29)
;;  - Conform to ELPA conventions.

;;; TODO:

;; - Add support for nested tags (XML, HTML)

;;; Issues:

;; - Rainbow-delimiters mode does not appear to change the color of
;;   delimiters when Org-mode is enabled.


;;; Code:

(eval-when-compile (require 'cl))


;;; Customize interface:

(defgroup rainbow-delimiters nil
  "Color nested parentheses, brackets, and braces according to their depth."
  :prefix "rainbow-delimiters-"
  :link '(url-link :tag "Website for rainbow-delimiters (EmacsWiki)"
                   "http://www.emacswiki.org/emacs/RainbowDelimiters")
  :group 'applications)

(defgroup rainbow-delimiters-faces nil
  "Faces for each nested depth. Used to color delimiter pairs.

Depths 1-12 are defined. Depth 1 is the outermost delimiter pair."
  :group 'rainbow-delimiters
  :link '(custom-group-link "rainbow-delimiters")
  :prefix 'rainbow-delimiters-faces-)


;;; Faces:

;; Unmatched delimiter face:
(defface rainbow-delimiters-unmatched-face
  '((t (:foreground "#88090B")))
  "Face to color unmatched closing delimiters with."
  :group 'rainbow-delimiters
  :group 'rainbow-delimiters-faces)


;; NOTE: The use of repetitious definitions for depth faces is temporary.
;; Once the emacs 24 color theme support comes in, this will be reevaluated.

;; Faces for colorizing delimiters at each level:
(defface rainbow-delimiters-depth-1-face
  '((t (:foreground "grey55")))
  "Rainbow-delimiters nested delimiter face, depth 1 - the outermost pair."
  :group 'rainbow-delimiters-faces)

(defface rainbow-delimiters-depth-2-face
  '((t (:foreground "#93a8c6")))
  "Rainbow-delimiters nested delimiter face, depth 2."
  :group 'rainbow-delimiters-faces)

(defface rainbow-delimiters-depth-3-face
  '((t (:foreground "#b0b1a3")))
  "Rainbow-delimiters nested delimiter face, depth 3."
  :group 'rainbow-delimiters-faces)

(defface rainbow-delimiters-depth-4-face
  '((t (:foreground "#97b098")))
  "Rainbow-delimiters nested delimiter face, depth 4."
  :group 'rainbow-delimiters-faces)

(defface rainbow-delimiters-depth-5-face
  '((t (:foreground "#aebed8")))
  "Rainbow-delimiters nested delimiter face, depth 5."
  :group 'rainbow-delimiters-faces)

(defface rainbow-delimiters-depth-6-face
  '((t (:foreground "#b0b0b3")))
  "Rainbow-delimiters nested delimiter face, depth 6."
  :group 'rainbow-delimiters-faces)

(defface rainbow-delimiters-depth-7-face
  '((t (:foreground "#90a890")))
  "Rainbow-delimiters nested delimiter face, depth 7."
  :group 'rainbow-delimiters-faces)

(defface rainbow-delimiters-depth-8-face
  '((t (:foreground "#a2b6da")))
  "Rainbow-delimiters nested delimiter face, depth 8."
  :group 'rainbow-delimiters-faces)

(defface rainbow-delimiters-depth-9-face
  '((t (:foreground "#9cb6ad")))
  "Rainbow-delimiters nested delimiter face, depth 9."
  :group 'rainbow-delimiters-faces)

;; Emacs doesn't sort face names by number correctly above 1-9; trick it into
;; proper sorting by prepending a _ before the faces with depths over 10.
(defface rainbow-delimiters-depth-_10-face
  '((t (:foreground "#83787e")))
  "Rainbow-delimiters nested delimiter face, depth 10."
  :group 'rainbow-delimiters-faces)

(defface rainbow-delimiters-depth-_11-face
  '((t (:foreground "#e1ddca")))
  "Rainbow-delimiters nested delimiter face, depth 11."
  :group 'rainbow-delimiters-faces)

(defface rainbow-delimiters-depth-_12-face
  '((t (:foreground "#e0c7c7")))
  "Rainbow-delimiters nested delimiter face, depth 12."
  :group 'rainbow-delimiters-faces)

;; Variable aliases for faces 10+:
;; We prepend an underline to numbers 10+ to force customize to sort correctly.
;; Here we define aliases without the underline for use everywhere else.
(put 'rainbow-delimiters-depth-10-face
     'face-alias
     'rainbow-delimiters-depth-_10-face)
(put 'rainbow-delimiters-depth-11-face
     'face-alias
     'rainbow-delimiters-depth-_11-face)
(put 'rainbow-delimiters-depth-12-face
     'face-alias
     'rainbow-delimiters-depth-_12-face)


;;; Face utility functions

;; inlining this function for speed:
;; see: http://www.gnu.org/s/emacs/manual/html_node/elisp/Compilation-Tips.html
;; this will cause problems with debugging. To debug, change defsubst -> defun.
(defsubst rainbow-delimiters-depth-face (depth)
  "Return face-name 'rainbow-delimiters-depth-DEPTH-face' as a string.

DEPTH is the number of nested levels deep for the delimiter being colorized.

Returns a face namestring the of form 'rainbow-delimiters-depth-DEPTH-face',
e.g. 'rainbow-delimiters-depth-1-face'."
  (concat "rainbow-delimiters-depth-" (number-to-string depth) "-face"))


;;; Nesting level

;; syntax-table: used with parse-partial-sexp for determining current depth.
(defvar rainbow-delimiters-delim-syntax-table
  (let ((table (copy-syntax-table emacs-lisp-mode-syntax-table)))
    (modify-syntax-entry ?\( "()  " table)
    (modify-syntax-entry ?\) ")(  " table)
    (modify-syntax-entry ?\[ "(]" table)
    (modify-syntax-entry ?\] ")[" table)
    (modify-syntax-entry ?\{ "(}" table)
    (modify-syntax-entry ?\} "){" table)
    table)
  "Syntax table for recognizing all supported delimiter types.")

(defun rainbow-delimiters-depth (point)
  "Return # of nested levels of parens, brackets, braces POINT is inside of."
  (save-excursion
      (beginning-of-defun)
      (let ((depth
             (with-syntax-table rainbow-delimiters-delim-syntax-table
               (car (parse-partial-sexp (point) point)))))
        (if (>= depth 0)
            depth
          0)))) ; ignore negative depths created by unmatched closing parens.


;;; Text properties

;; inlining this function for speed:
;; see: http://www.gnu.org/s/emacs/manual/html_node/elisp/Compilation-Tips.html
;; this will cause problems with debugging. To debug, change defsubst -> defun.
(defsubst rainbow-delimiters-propertize-delimiter (point depth)
  "Colorize delimiter at POINT according to DEPTH.

POINT is the point of character to propertize.
DEPTH is the nested delimiter depth at POINT, which determines the face to use.

Sets text properties:
`font-lock-face' to the corresponding delimiter face.
`rear-nonsticky' to prevent color from bleeding into subsequent characters typed by the user."
  (with-silent-modifications
    (let ((delim-face (if (<= depth 0)
                          "rainbow-delimiters-unmatched-face"
                        (rainbow-delimiters-depth-face depth))))
      ;; (when (eq depth -1) (message "Unmatched delimiter at char %s." point))
      (add-text-properties point (1+ point)
                           `(font-lock-face ,delim-face
                             rear-nonsticky t)))))


(defun rainbow-delimiters-unpropertize-delimiter (point)
  "Remove text properties set by rainbow-delimiters mode from char at POINT."
  (remove-text-properties point (1+ point)
                          '(font-lock-face nil
                            rear-nonsticky nil)))


(defun rainbow-delimiters-char-ineligible-p (point)
  "Return t if char at POINT should be skipped, e.g. if inside a comment.

Returns t if char at point meets one of the following conditions:
- Inside a string.
- Inside a comment.
- Is an escaped char, e.g. ?\)"
  (let ((parse-state (save-excursion
                       (beginning-of-defun)
                       ;; (point) is at beg-of-defun; point is the char location
                       (parse-partial-sexp (point) point))))
    (or
     (nth 3 parse-state)                ; inside string?
     (nth 4 parse-state)                ; inside comment?
     (and (eq (char-before point) ?\\)  ; escaped char, e.g. ?\) - not counted
          (and (not (eq (char-before (1- point)) ?\\)) ; special-case: ignore ?\\
               (eq (char-before (1- point)) ?\?))))))
;; standard char read syntax '?)' is not tested for because emacs manual states
;; that punctuation such as delimiters should _always_ use escaped '?\)' form.


;;; JIT-Lock functionality

;; Used to skip delimiter-by-delimiter `rainbow-delimiters-propertize-region'.
(defvar rainbow-delimiters-delim-regex "\\(\(\\|\)\\|\\[\\|\\]\\|\{\\|\}\\)"
  "Regex matching all opening and closing delimiters we intend to colorize.")

;; main function called by jit-lock:
(defun rainbow-delimiters-propertize-region (start end)
  "Colorize delimiters in region between START and END.

Used by jit-lock for dynamic highlighting."
  (save-excursion
    (goto-char start)
    ;; START can be anywhere in buffer; begin depth counts from values at START.
    (let ((depth (rainbow-delimiters-depth start)))
      (while (and (< (point) end)
                  (re-search-forward rainbow-delimiters-delim-regex end t))
        (backward-char) ; re-search-forward places point after delim; go back.
        (unless (rainbow-delimiters-char-ineligible-p (point))
          (let ((delim (char-after (point))))
            (cond ((eq ?\( delim)       ; (
                   (setq depth (1+ depth))
                   (rainbow-delimiters-propertize-delimiter (point)
                                                            depth))
                  ((eq ?\) delim)       ; )
                   (rainbow-delimiters-propertize-delimiter (point)
                                                            depth)
                   (setq depth (or (and (<= depth 0) 0) ; unmatched paren
                                   (1- depth))))
                  ((eq ?\[ delim)       ; [
                   (setq depth (1+ depth))
                   (rainbow-delimiters-propertize-delimiter (point)
                                                            depth))
                  ((eq ?\] delim)       ; ]
                   (rainbow-delimiters-propertize-delimiter (point)
                                                            depth)
                   (setq depth (or (and (<= depth 0) 0) ; unmatched bracket
                                   (1- depth))))
                  ((eq ?\{ delim)       ; {
                   (setq depth (1+ depth))
                   (rainbow-delimiters-propertize-delimiter (point)
                                                            depth))
                  ((eq ?\} delim)       ; }
                   (rainbow-delimiters-propertize-delimiter (point)
                                                            depth)
                   (setq depth (or (and (<= depth 0) 0) ; unmatched brace
                                   (1- depth)))))))
        ;; move past delimiter so re-search-forward doesn't pick it up again
        (forward-char)))))

(defun rainbow-delimiters-unpropertize-region (start end)
  "Remove mode faces from delimiters in region between START and END."
  (save-excursion
    (goto-char start)
    (while (and (< (point) end)
                (re-search-forward rainbow-delimiters-delim-regex end t))
      ;; re-search-forward places point 1 further than the delim matched:
      (rainbow-delimiters-unpropertize-delimiter (1- (point))))))


;;; Minor mode:

;;;###autoload
(define-minor-mode rainbow-delimiters-mode
  "Color nested parentheses, brackets, and braces according to their depth."
  nil "" nil ; No modeline lighter - it's already obvious when the mode is on.
  (if (not rainbow-delimiters-mode)
      (progn
        (jit-lock-unregister 'rainbow-delimiters-propertize-region)
        (rainbow-delimiters-unpropertize-region (point-min) (1- (point-max))))
    (jit-lock-register 'rainbow-delimiters-propertize-region t)))


(provide 'rainbow-delimiters)

;;; rainbow-delimiters.el ends here.
