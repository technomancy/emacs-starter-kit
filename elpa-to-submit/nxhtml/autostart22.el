;;; autostart22.el --- Example of autostart file for Emacs22
;;
;; Author: Lennart Borgman (lennart O borgman A gmail O com)
;; Created: 2009-01-01 Thu
;; Version:
;; Last-Updated: 2009-01-05 Mon
;; URL:
;; Keywords:
;; Compatibility:
;;
;; Features that might be required by this library:
;;
;;   This file is for Emacs 22 only.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; Change this file according to the path of your nxml-mode dir. If
;; you do not use nxml-mode then just use autostart.el.
;;
;; NOTICE: You need to enter the path to your nxml-mode installation
;; below.
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

(let ((debug-on-error t))
  (unless (= emacs-major-version 22)
    (error "This file is for Emacs 22 only"))

  (defalias 'Custom-mode 'custom-mode)

  (let* ((this-file (or load-file-name buffer-file-name))
         (this-dir (file-name-directory this-file))
         ;; FIX-ME: Download nXml (since it is not included in Emacs
         ;; 22) and place the path to rng-auto.el in your downloaded
         ;; nXml HERE:
         (rng-auto-file (or (locate-library "rng-auto.el")
                            (concat this-dir "../nxml-mode/rng-auto.el"))))
    (unless (file-exists-p rng-auto-file)
      (error "Can't find rng-auto.el, please edit %s" this-file))
    (load rng-auto-file)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; autostart22.el ends here
