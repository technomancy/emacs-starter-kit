;;; appmenu-fold.el --- Support form fold-dwim in AppMenu

;; Copyright (C) 2006, 2007 by Lennart Borgman

;; Author:  Lennart Borgman <lennart DOT borgman DOT 073 AT student DOT lu DOT se>
;; Created: Wed Jan 11 21:48:02 2006
(defconst appmenu-fold:version "0.51") ;; Version:
;; Last-Updated: Mon Jan 15 03:10:59 2007 (3600 +0100)
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
;;
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change log:
;;
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(require 'fold-dwim nil t)
(eval-when-compile (require 'appmenu))

(when (featurep 'fold-dwim)

  (defun appmenu-fold-no-hs-minor-mode ()
    t)
  (defun appmenu-fold-no-outline-minor-mode ()
    t)
  (defun appmenu-fold-setup ()
    "Adds some tweaks for using fold-dwim in AppMenu."
    (let ((fd-map (make-sparse-keymap)))
      (define-key fd-map [fold-dwim-toggle]
        (list 'menu-item "Fold Dwin Toggle" 'fold-dwim-toggle))
      (define-key fd-map [fold-dwim-hide-all]
        (list 'menu-item "Fold Dwin Hide All" 'fold-dwim-hide-all))
      (define-key fd-map [fold-dwim-show-all]
        (list 'menu-item "Fold Dwin Show All" 'fold-dwim-show-all))
      ;;(add-to-list 'appmenu-alist (cons t (cons "Folding" fd-map)))
      (appmenu-add 'appmenu-fold nil t "Folding" fd-map)
      )
;;;     (add-to-list 'appmenu-minor-modes-exclude
;;;                  '(hs-minor-mode appmenu-fold-no-hs-minor-mode))
;;;     (add-to-list 'appmenu-minor-modes-exclude
;;;                  '(outline-minor-mode appmenu-fold-no-outline-minor-mode)))
    )
  )

(provide 'appmenu-fold)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; appmenu-fold.el ends here
