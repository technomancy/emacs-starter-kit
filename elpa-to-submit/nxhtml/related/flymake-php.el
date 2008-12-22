;;; flymake-php.el ---
;;
;; Author: hosiawak
;; Created: Sun Dec 02 05:38:46 2007
;; Version: 0.4
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
;; This file provides basic setup for using `flymake-mode' with php
;; files.  To use this you must have php installed and it must be in
;; your path.
;;
;; Put this file in your Emacs `load-path' and then in .emacs
;;
;;    (require 'flymake-php)
;;
;; If you want to turn on `flymake-mode' automatically for php files
;; then set `flymake-php-on' through Emacs' custom:
;;
;;    M-x customize-option RET flymake-php-on RET
;;
;; This file is based on the ideas from the net, for example the nice
;; introduction at http://www.blik.it/category/php/.
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

;; Flymake PHP Extension

(require 'flymake)

(defcustom flymake-allowed-php-file-name-masks '(("\\.php[0-9]\\'" flymake-php-init)
                                                 ("\\.inc\\'" flymake-php-init)
                                                 ("\\.php\\'" flymake-php-init))
  "Filename extensions that switch on php syntax checks."
  :type '(repeat (list (regexp :tag "File name regexp")
                       (function :tag "Init function")
                       (choice (const :tag "No cleanup function" nil)
                               (function :tag "Cleanup function"))))
  :group 'flymake)

(defconst flymake-php-err-line-pattern-re '(
                                            ("\\(Parse\\|Fatal\\) error: \\(.*\\) in \\(.*\\) on line \\([0-9]+\\)" 3 4 nil 2)
                                            )
  "Regexp matching PHP error messages")

(defun flymake-php-init ()
  (let* ((temp-file       (flymake-init-create-temp-buffer-copy
                           'flymake-create-temp-inplace))
         (local-file  (file-relative-name
                       temp-file
                       (file-name-directory buffer-file-name))))
    ;; Fix-me: How do I make flymake check if the command is valid?
    (list "php" (list "-f" local-file "-l"))))

(defvar flymake-php-has-engine nil)

(defun flymake-php-has-engine ()
  "Check for the needed files."
  (if flymake-php-has-engine
      t
    (unless (executable-find "php")
      (error "Can not find php program"))
    (setq flymake-php-has-engine t)))

(defun flymake-php-turn-on ()
  (when buffer-file-name
    (flymake-php-has-engine)
    (unless flymake-mode
      (flymake-mode 1))))

(defcustom flymake-php-on nil
  "Turn on flymake for new php file buffers if non-nil."
  :type 'boolean
  :set (lambda (sym val)
         (set-default sym val)
         (if val
             (add-hook 'php-mode-hook 'flymake-php-turn-on)
           (remove-hook 'php-mode-hook 'flymake-php-turn-on)))
  :group 'flymake)

(defun flymake-php-load ()
  (dolist (mask flymake-allowed-php-file-name-masks)
    (add-to-list 'flymake-allowed-file-name-masks mask))
  (dolist (rec flymake-php-err-line-pattern-re)
    (add-to-list 'flymake-err-line-patterns rec)))

(eval-after-load 'php-mode
  (flymake-php-load))

(provide 'flymake-php)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; flymake-php.el ends here
