;;; company-predictive.el --- Use company-mode for predictive(-mode)
;;
;; Author: Lennart Borgman (lennart O borgman A gmail O com)
;; Created: 2009-07-16 Thu
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
;; published by the Free Software Foundation; either version 3, or
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


(require 'company)
(eval-when-compile (require 'cl))

(require 'predictive nil t)
(load "dict-english") ;; fix-me

;(syntax-table-p company-predictive-syntax-table)
(defvar company-predictive-syntax-table
  (let ((tbl (copy-syntax-table (standard-syntax-table))))
    (modify-syntax-entry ?\- "w" tbl)
    tbl)
  "Make character `-' have syntax word.")

;; (string (with-syntax-table company-predictive-syntax-table (char-syntax ?-)))
;; (string (char-syntax ?-))
(defun company-predictive-grab-word ()
  (with-syntax-table company-predictive-syntax-table
    (company-grab-word)))

;;;###autoload
(defun company-predictive (command &optional arg &rest ignored)
  "A predictive-like `company-mode' completion back-end."
  (interactive (list 'interactive))
  (case command
    ('interactive (company-begin-backend 'company-predictive))
    ('prefix (company-predictive-grab-word))
    ('candidates (predictive-complete arg))
    ('ignore-case t)
    ('sorted nil)
    ('duplicates t)))


(provide 'company-predictive)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; company-predictive.el ends here
