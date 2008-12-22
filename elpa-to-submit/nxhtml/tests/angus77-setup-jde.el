;;; angus77-setup-jde.el ---
;;
;; Author: Lennart Borgman (lennart O borgman A gmail O com)
;; Created: 2008-08-20T16:57:35+0200 Wed
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

;; Question #42407 on nXhtml changed:
;; https://answers.launchpad.net/nxhtml/+question/42407

;; Angus77 posted a new comment:

(eval-when-compile (require 'cl))
(let (
      ;;(jde-lisp-dir "C:/jdee/jdee/trunk/jde/lisp/")
      (jde-lisp-dir "C:/jdee/jdee/branches/phil_lord/dimitre_liotev_new_build/jde/lisp")
      (cedet-root "c:/cedet/cedet/")
      (elib-dir "C:/DL/emacs/elib-1.0")
      )
  (assert (file-directory-p jde-lisp-dir) t)
  ;;(add-to-list 'load-path (expand-file-name "~/elisp/jde-2.3.5.1/lisp"))
  (add-to-list 'load-path jde-lisp-dir)
  ;;(add-to-list 'load-path (expand-file-name "~/elisp/cedet-1.0pre4/semantic"))
  (add-to-list 'load-path (expand-file-name "semantic" cedet-root))
  ;;(add-to-list 'load-path (expand-file-name "~/elisp/cedet-1.0pre4/speedbar"))
  (add-to-list 'load-path (expand-file-name "speedbar" cedet-root))
  ;;(add-to-list 'load-path (expand-file-name "~/elisp/elib"))
  (add-to-list 'load-path elib-dir)
  ;;(add-to-list 'load-path (expand-file-name "~/elisp/cedet-1.0pre4/eieio"))
  (add-to-list 'load-path (expand-file-name "eieio" cedet-root))
  ;;(add-to-list 'load-path (expand-file-name "~/elisp/cedet-1.0pre4/common"))
  (add-to-list 'load-path (expand-file-name "common" cedet-root)))

;; Initialize CEDET.
;;(load-file (expand-file-name "~/elisp/cedet-1.0pre4/common/cedet.el"))
(load-library "cedet.el")

(setq defer-loading-jde t)

(if defer-loading-jde
    (progn
      (autoload 'jde-mode "jde" "JDE mode." t)
      (setq auto-mode-alist
	    (append
	     '(("\\.java\\'" . jde-mode))
	     auto-mode-alist)))
  (require 'jde))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; angus77-setup-jde.el ends here
