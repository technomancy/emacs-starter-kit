;;; flymake-keys.el ---
;;
;; Author: Lennart Borgman (lennart O borgman A gmail O com)
;; Created: Sun Dec 02 08:30:45 2007
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
;; Key bindings for flymake minor mode.
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

(require 'flymake)

;; (unless (boundp 'flymake-mode-map) (defvar flymake-mode-map (make-sparse-keymap)))
(defvar flymake-mode-map) ;; silence compiler

(define-key flymake-mode-map [(control ?c) ?! ?e] 'flymake-display-err-menu-for-current-line)
(define-key flymake-mode-map [(control ?c) ?! ?n] 'flymake-goto-next-error)
(define-key flymake-mode-map [(control ?c) ?! ?p] 'flymake-goto-prev-error)

(defun flymake-popup-menu (menu-data)
  (lwarn t :warning "menu-data=%s" menu-data)
  (let* (
         ;;(menu (flymake-make-emacs-menu menu-data))
         (menu (make-sparse-keymap))
         (add-alt (lambda (tg)
                    (define-key menu
                      (read (format "[popcmp-%s]" (replace-regexp-in-string " " "-" tg)))
                      (list 'menu-item
                           tg
                           `(lambda ()
                              (interactive)
                              (setq completion ,tg))))))
         )
    (dolist (m menu-data)
      (unless (listp m)
        (funcall add-alt m)))
    (popup-menu-at-point menu)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; flymake-keys.el ends here
