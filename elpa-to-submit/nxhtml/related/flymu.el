;;; flymu.el --- Flymake for mumamo-mode
;;
;; Author: Lennart Borgman (lennart O borgman A gmail O com)
;; Created: Sun Dec 02 14:52:32 2007
;; Version: 0.1
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
;; Flymake syntax checks for mumamo chunks.
;;
;; Not ready yet!!!
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

(defun flymu-make-major-mode-alist ()
  "Grab values from `flymake-allowed-file-name-masks'.
We need a list of major modes and the corresponding init and
cleanup functions for flymake. This functions creates such a list
from flymakes dito list for file names."
  (let ((allowed nil))
    (save-match-data
      (dolist (regexp-init flymake-allowed-file-name-masks)
        (let* ((regexp (car regexp-init))
               (init   (cdr regexp-init))
               ;; Make it as simple as possible. First see if the same
               ;; regexp is used:
               (mode (let ((m (cdr (assoc regexp auto-mode-alist))))
                       ;; Don't use this if it is complicated:
                       (when (commandp m) m)))
               (ext regexp))
          (unless mode
            ;; Try to make a simple file name, this could be made
            ;; better but I do not know if that would be meaningful:
            (setq ext (replace-regexp-in-string "\\\\\\." "." ext))
            (setq ext (replace-regexp-in-string "\\\\'" "" ext))
            (setq ext (replace-regexp-in-string "[\\$?+*]" "" ext))
            ;; Next compare the filename against the entries in
            ;; auto-mode-alist. The code is from `set-auto-mode'.
            (let ((name ext)
                  (done nil))
              (while name
                ;; Find first matching alist entry.
                (setq mode
                      (if (memq system-type '(vax-vms windows-nt cygwin))
                          ;; System is case-insensitive.
                          (let ((case-fold-search t))
                            (assoc-default name auto-mode-alist
                                           'string-match))
                        ;; System is case-sensitive.
                        (or
                         ;; First match case-sensitively.
                         (let ((case-fold-search nil))
                           (assoc-default name auto-mode-alist
                                          'string-match))
                         ;; Fallback to case-insensitive match.
                         (and auto-mode-case-fold
                              (let ((case-fold-search t))
                                (assoc-default name auto-mode-alist
                                               'string-match))))))
                (if (and mode
                         (consp mode)
                         (cadr mode))
                    (setq name (substring name 0 (match-beginning 0)))
                  (setq name)))))
          (when (and mode
                     ;; nxml-mode's do not need flymake:
                     (let ((major-mode mode))
                       (not (derived-mode-p 'nxml-mode))))
            (let ((rec (append (list mode) init)))
              (when (= (length rec) 2)
                (setq rec (append rec (list nil))))
              (add-to-list 'allowed rec))))))
    allowed))

(defcustom flymu-allowed-major-modes (flymu-make-major-mode-alist)
  "Major modes syntax checking is allowed for."
  :type '(repeat (list (function :tag "Major mode")
                       (function :tag "Init function")
                       (choice (const :tag "No cleanup function" nil)
                               (function :tag "Cleanup function"))))
  :set-after '(flymake-allowed-file-name-masks)
  :group 'flymu)

(defvar flymu-mumamo-chunk nil)
(make-variable-buffer-local 'flymo-mumamo-chunk)

;; Fix-me: What to check? When? Make flymu-mumamo-chunk a function
;; instead? Mark chunks for checking - let mumamo do that? Flymake
;; should be able to mark a chunk to, even if it is not a whole
;; line. What about line numbers?

;; Advice these functions:
(defadvice flymake-get-file-name-mode-and-masks
  (around flymu-get-file-name-mode-and-masks
          (file-name))
  "Make flymake init file selection according to mode."
  (if flymu-mumamo-chunk
      (let ((major (overlay-get ovl 'mumamo-major-mode))
            (rec (assq major flymu-allowed-major-modes)))
        (when rec
          (cdr rec)))
    ad-do-it))
(ad-activate 'flymake-get-file-name-mode-and-masks)

;;(defun flymake-save-buffer-in-file (file-name)
(defadvice flymake-save-buffer-in-file
  (around flymu-save-buffer-in-file
          (file-name))
  (if flymu-mumamo-chunk
      (let ((min (overlay-start flymu-mumamo-chunk))
            (max (overlay-end   flymu-mumamo-chunk)))
        (make-directory (file-name-directory file-name) 1)
        (write-region min max file-name nil 566)
        (flymake-log 3 "saved chunk %s:%s-%s in file %s" (buffer-name) min ma file-name))
    ad-do-it))

(provide 'flymu)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; flymu.el ends here
