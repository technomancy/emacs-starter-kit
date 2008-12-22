;;; test-Q.el --- Run test from a fresh Emacs
;;
;; Author: Lennart Borgman (lennart O borgman A gmail O com)
;; Created: 2008-07-08T23:05:40+0200 Tue
;; Version: 0.1
;; Last-Updated: 2008-07-09T00:17:26+0200 Tue
;; URL:
;; Keywords:
;; Compatibility:
;;
;; Features that might be required by this library:
;;
;;   Required feature `test-Q' was not provided.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; Defines `nxhtmltest-Q'.
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


(defvar nxhtmltest-bin-Q
  (file-name-directory (if load-file-name load-file-name buffer-file-name)))

;;;###autoload
(defun nxhtmltest-run-Q ()
  "Run all tests defined for nXhtml in fresh Emacs.
See `nxhtmltest-run' for more information about the tests."
  (interactive)
  (let* ((test-el (expand-file-name "nxhtmltest-suites.el" nxhtmltest-bin-Q))
         (nxhtml-auto-start (expand-file-name "../autostart.el" nxhtmltest-bin-Q))
         (temp-eval-file (expand-file-name "temp-test.el" nxhtmltest-bin-Q))
         (temp-eval-buf (find-file-noselect temp-eval-file))
         (load-path load-path))
    ;;(load (expand-file-name "nxhtmltest-helpers" nxhtmltest-bin-Q))
    (add-to-list 'load-path nxhtmltest-bin-Q)
    (require 'nxhtmltest-helpers)
    (nxhtmltest-get-fontification-method)
    (with-current-buffer temp-eval-buf
      (erase-buffer)
      (insert "(setq debug-on-error t)\n"
              "(eval-when-compile (require 'cl))\n"
              "(delete-other-windows)\n"
              "(eval-after-load 'nxhtml '(setq nxhtml-skip-welcome t))\n"
              (format "(setq nxhtmltest-default-fontification-method '%s)\n"
                      nxhtmltest-default-fontification-method)
              ))
    (when (featurep 'ruby-mode)
      (with-current-buffer temp-eval-buf
        (insert "(pushnew \""
                (file-name-directory (locate-library "ruby-mode"))
                "\" load-path)")))
    (with-current-buffer temp-eval-buf
      (save-buffer))
    (unless (file-exists-p nxhtmltest-bin-Q)
      (error "Can't find directory %s" nxhtmltest-bin-Q))
    (unless (file-exists-p test-el)
      (error "Can't find file %s" test-el))
    (unless (file-exists-p nxhtml-auto-start)
      (error "Can't find file %s" nxhtml-auto-start))
    (message "nxhtmltest-bin-Q=%s" nxhtmltest-bin-Q)
    (message "nxhtml-auto-start=%s" nxhtml-auto-start)
    (setenv "nxhtmltest-run-Q" "run")
    (message "After setenv nxhtmltest-run-Q=%s" (getenv "nxhtmltest-run-Q"))
    (message "(ourcomments-find-emacs) => %s" (ourcomments-find-emacs))
    (call-process (ourcomments-find-emacs) nil 0 nil "-Q"
                  "-l" temp-eval-file
                  "-l" nxhtml-auto-start
                  "-l" test-el)
    (message "After call-process")
    (setenv "nxhtmltest-run-Q")
    (message "Starting new Emacs instance for test - it will be ready soon ...")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; nxhtmltest-Q.el ends here
