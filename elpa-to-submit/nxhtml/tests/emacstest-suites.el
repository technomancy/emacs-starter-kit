;;; emacstest-suites.el --- Some unit tests for Emacs
;;
;; Author: Lennart Borgman (lennart O borgman A gmail O com)
;; Created: 2008-09-21T22:34:11+0200 Sun
;; Version:
;; Last-Updated: 2008-09-22T00:36:11+0200 Sun
;; URL:
;; Keywords:
;; Compatibility:
;;
;; Features that might be required by this library:
;;
;;   `button', `cl', `debug', `ert', `ert2', `ewoc', `find-func',
;;   `help-fns', `help-mode', `view'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; Unit tests for some Emacs bug reports.
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

(eval-when-compile (require 'cl))

(setq debug-on-error t)

(defvar emacstest-bin
  (file-name-directory (if load-file-name load-file-name buffer-file-name)))

(pushnew emacstest-bin load-path)


(require 'ert2)


(defvar emacstest-files-root
  (let* ((this-dir emacstest-bin)
         (root (expand-file-name "inemacs/" this-dir)))
    (unless (file-accessible-directory-p root)
          (error (if (file-exists-p root)
                     "Can't read files in test directory %s"
                   "Can't find test directory %s")
                 root))
    root))

(let ((distr-in "c:/EmacsW32/nxhtml/tests/inemacs/"))
  (when (file-directory-p distr-in)
    (setq emacstest-files-root distr-in)))

(defun emacstest-run ()
  "Run Emacs tests."
  (interactive)
  (setq message-log-max t)
  (setq ert-test-files-root emacstest-files-root)
  (let ((selector "emacs-"))
    (if noninteractive
        (ert-run-tests-batch selector)
      (ert-kill-temp-test-buffers)
      (ert-run-tests-interactively selector)
      (other-window 1)
      (ert-list-temp-test-buffers))))

(ert-deftest emacs-bug1013 ()
  "Emacs bug 1013.
See URL
`http://emacsbugs.donarmstrong.com/cgi-bin/bugreport.cgi?bug=1013'."
  (ert-with-temp-buffer-include-file "bug1013.el"
    (eval-buffer)))

(provide 'emacstest-suites)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; emacstest-suites.el ends here
