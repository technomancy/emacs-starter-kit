;;; eshell-vc.el --- Put some VC info in your eshell prompt

;; Copyright (C) 2008 Phil Hagelberg

;; Author: Phil Hagelberg <technomancy@gmail.com>
;; URL: http://www.emacswiki.org/cgi-bin/wiki/EshellVCBranch
;; Version: 0.5
;; Created: 2008-12-17
;; Keywords: eshell vc shell
;; EmacsWiki: EshellVCBranch
;; Package-Requires: ((magit "1.0"))

;; This file is NOT part of GNU Emacs.

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; When this is required, the Eshell prompt should show the current
;; branch name. It will also will alter the prompt if there are
;; changes that haven't been checked in.

;; Currently this actually builds on magit functionality rather than
;; VC as it's not clear how to get this kind of info on a directory
;; rather than a file with VC.

;;; Code:

(defvar eshell-vc-dirty
  " ⚠ " "String to use for the prompt when there are uncomitted checkins.")

(defun eshell-vc-prompt ()
  "Return a prompt with VC branch and dirty state."
  (let ((branch (eshell/branch)))
    (propertize (concat (and branch (concat branch " "))
                        (eshell/pwd)
                        (cond ((= (user-uid) 0) " # ")
                              ((and branch (eshell/vc-dirty)) eshell-vc-dirty)
                              (t " $ ")))
                'face 'eshell-prompt)))

(defun eshell-vc-skip-prompt ()
  "Skip the prompt using text properties instead of a regex."
  (goto-char (next-property-change (point) nil (point-max))))

;; TODO: make this work with VC if possible rather than magit
(require 'magit)
(defalias 'eshell/branch 'magit-get-current-branch)
(defun eshell/vc-dirty () (not (magit-everything-clean-p)))

(setq eshell-prompt-function #'eshell-vc-prompt
      eshell-skip-prompt-function #'eshell-vc-skip-prompt)

(provide 'eshell-vc)
;;; eshell-vc.el ends here