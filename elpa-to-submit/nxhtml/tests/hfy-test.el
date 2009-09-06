;;; hfy-test.el --- Test for htmlfontify + hfyview
;;
;; Author: Lennart Borgman (lennart O borgman A gmail O com)
;; Created: 2008-10-17 Fri
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
;;
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
(require 'winsav)
(require 'emacsw32 nil t)
(require 'grep)

(defun hfy-test-setup-frame ()
  (find-library "htmlfontify")
  (occur "hfy-tmpfont-stack")
  (unless grep-template (grep-compute-defaults))
  (lgrep "hfy-tmpfont-stack" "*.el" ".")
  (list-faces-display)
  (list-colors-display)
  (describe-function 'describe-function)
  (delete-other-windows)

  (split-window-vertically)
  (split-window-vertically)
  (balance-windows)
  (split-window-vertically)
  (balance-windows)
  (split-window-vertically)
  (balance-windows)

  ;;(winsav-upper-left-window)
  (frame-first-window)
  (split-window-horizontally)
  ;;(winsav-upper-left-window)
  (frame-first-window)
  (switch-to-buffer "*scratch*")

  (select-window (next-window))
  (switch-to-buffer "*Help*")

  (select-window (next-window))
  (switch-to-buffer "*Faces*")
  (split-window-horizontally)

  (select-window (next-window))
  (switch-to-buffer "*Colors*")

  (select-window (next-window))
  (when (fboundp 'emacsw32-show-custstart)
    (emacsw32-show-custstart))

  (select-window (next-window))
  (info)

  (select-window (next-window))
  (split-window-horizontally)
  (switch-to-buffer "*grep*")

  (select-window (next-window))
  (switch-to-buffer "*Occur*")
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; hfy-test.el ends here
