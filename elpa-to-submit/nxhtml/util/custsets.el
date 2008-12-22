;;; custsets.el --- Sets of named customizations
;;
;; Author: Lennart Borgman (lennart O borgman A gmail O com)
;; Created: 2008-03-25T00:17:06+0100 Mon
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
;; After an idea expressed by among other Stephen Turnbull on the
;; emacs devel list.
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

(defcustom custsets-sets
  '(
    ("Windows"
     (cua-mode t)
     )
    )
  "Sets of customizations."
  :group 'custsets)

(defun custsets-turn-on (set-name)
  (interactive "sCustomization set: ")
  (let ((set (assoc-string set-name custsets-sets t)))
    (unless set
      (error "Can't find customization set %s" set-name))
    (dolist (opt-rec (cdr set))
      (let* ((opt (car opt-rec))
             (val (cdr opt-rec))
             (saved-opt (get opt 'saved-value))
             (saved-val saved-opt) ;; fix-me
             (ask (if saved-opt
                      (format "You have currently customized %s to %s. Change this to %s? "
                              opt saved-opt val)
                    (format "Customize %s to %s? " opt val)))
             )
        (when (y-or-n-p ask)
          (customize-set-variable opt val)
          (customize-set-value opt val)
          (customize-mark-to-save opt))
        )
      )
    (custom-save-all)))


(provide 'custsets)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; custsets.el ends here
