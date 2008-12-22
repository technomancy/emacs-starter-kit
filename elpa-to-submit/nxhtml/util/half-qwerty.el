;;; half-qwerty.el --- Using half keyboard to type
;;
;; Author: Lennart Borgman (lennart O borgman A gmail O com)
;; Created: 2008-09-29T19:42:09+0200 Mon
;; Version:
;; Last-Updated:
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
;; Support for typing with one hand in Emacs using ideas from
;;
;;    http://half-qwerty.com/demo/
;;
;; This was written after a request from Corey Foote
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

(defcustom half-qwerty-langs
  '("SE")
  "doc"
  :type '(repeat (string :tag "Language"))
  :group 'half-qwerty)

(defcustom half-qwerty-hand 'left-hand
  "doc"
  :type '(choice (const left-hand)
                 (const right-hand))
  :group 'half-qwerty)

(defcustom half-qwerty-keypairs
  '(
    ("SE"
     (;; First row
      (?t ?y)
      (?r ?u)
      (?e ?i)
      (?w ?o)
      (?q ?p)
      ;; Right side leftovers in first
      ;;([(control ?\ ) ?a ?q] ?å)
      ;;([(control ?\ ) ?c ?c] ?:)
      ([(control ?\ ) ?v] ?^)
      ([(control ?\ ) ?s] ?~)
      ([(control ?\ ) ?w] ?¨)

      ;; Second row
      (?g ?h)
      (?f ?j)
      (?d ?k)
      (?s ?l)
      (?a ?ö)
      ;; Right side leftovers in second
      ;;([(control ?\ ) ?a ?a] ?ä)
      ([(control ?\ ) ?q] ?')
      ([(control ?\ ) ?x] ?*)

      ;; Third row
      (?b ?n)
      (?v ?m)
      (?c ?,)
      (?x ?.)
      (?z ?-)
      ;; Left side leftovers in third
      (?< [(control ?\ ) ?y])
      (?> [(control ?\ ) ?u])
      (?| [(control ?\ ) ?l])

      ;; Number row
      (?6 ?7)
      (?5 ?8)
      (?4 ?9)
      (?3 ?0)
      (?2 ?+)
      (?1 ?´)
      ;; Number row shifted
      (?& ?/)
      (?% ?()
      (?¤ ?))
      (?# ?=)
      (?\" ??)
      (?! ?`)
      ;; Number row AltGr
      (?$ ?\])
      (?£ ?\})
      (?@ ?\\)
      ;; Righ side leftovers for AltGr
      ([(control ?\ ) ?4] ?\])
      ([(control ?\ ) ?3] ?\})
      )
     )
    )
  ;;nil
  "doc"
  :type '(repeat (list string
                       (repeat (list
                                (choice character key-sequence)
                                (choice character key-sequence)
                                ))))
  :group 'half-qwerty)

(defvar half-qwerty-mode-map nil)

(defvar half-qwerty-old-input-decode-map nil)

(define-minor-mode half-qwerty-mode
  "Typing with one hand.
See URL `http://half-qwerty.com/demo/'.  Using this with Emacs
you however do not need a special keyboard.
Though we can not change for example SPACE+h directly.
There is no support for pressing them simultaneously.

Instead you have to type the after each other."
  global t
  :group 'half-qwerty
  (if half-qwerty-mode
      (progn
        (setq half-qwerty-old-input-decode-map (copy-keymap input-decode-map))
        (let ((map (copy-keymap input-decode-map))
              ;;(defcustom half-qwerty-langs
              (pairs (cadr (assoc "SE" half-qwerty-keypairs)))
              (space-map (make-sparse-keymap))
              (control-space-map (make-sparse-keymap))
              )
          (define-key map [?\ ] space-map)
          (define-key space-map [(control ?\ )] control-space-map)
          (dolist (pair pairs)
            (let ((left  (nth 0 pair))
                  (right (nth 1 pair))
                  )
              (if (eq half-qwerty-hand 'left-hand)
                  (unless (vectorp right)
                    (if (vectorp left)
                        (define-key map left (vector right))
                      (define-key space-map (vector left) (vector right))
                      )))))
          (setq input-decode-map (copy-keymap map))
          ))
    (setq input-decode-map (copy-keymap half-qwerty-old-input-decode-map))
    ))

;;(define-key input-decode-map (kbd ".") (kbd ":"))
;;(define-key input-decode-map (kbd "SPC") (kbd ":"))
;;(define-key input-decode-map [?\ ] (kbd ":"))
;;(define-key input-decode-map (vector ?\ ) (vector ?:))
;;(define-key input-decode-map (vector ?\ ?\ ) (vector ?\ ))
;;(define-key input-decode-map [?\. ?\. ] (vector ?\ ))
;;(define-key input-decode-map [?\. ?\. ] (vector ?\ ))
;;(define-key input-decode-map [?\.] (vector ?\ ))    
;;(define-key input-decode-map (kbd "SPC SPC") (vector ?\ ))
;;(define-key input-decode-map (kbd "SPC a") (vector ?\ ))
;; (defvar my-space-map (make-sparse-keymap))
;; (define-key my-space-map [?\ ] [?\ ])
;; (define-key my-space-map [?\:] [?\.])
;; (define-key input-decode-map [?\:] my-space-map)
;; (define-key input-decode-map [?\.] [?\:])
;;(setq input-decode-map (make-sparse-keymap))

(provide 'half-qwerty)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; half-qwerty.el ends here
