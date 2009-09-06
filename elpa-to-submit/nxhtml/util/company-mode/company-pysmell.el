;;; company-pysmell.el --- a company-mode completion back-end for pysmell.el
;;
;; Copyright (C) 2009 Nikolaj Schumacher
;;
;; This file is part of company 0.4.3.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(eval-when-compile (require 'cl))
(require 'pysmell nil t)

(defvar company-pysmell--available-p 'unknown)
(make-variable-buffer-local 'company-pysmell--available-p)

(defun company-pysmell--available-p ()
  (if (eq company-pysmell--available-p 'unknown)
      (setq company-pysmell--available-p
            (company-locate-dominating-file buffer-file-name "PYSMELLTAGS"))
    company-pysmell--available-p))

(defun company-pysmell--grab-symbol ()
  (let ((symbol (company-grab-symbol)))
    (when symbol
      (cons symbol
            (save-excursion
              (let ((pos (point)))
                (goto-char (- (point) (length symbol)))
                (while (eq (char-before) ?.)
                  (goto-char (1- (point)))
                  (skip-syntax-backward "w_"))
                (- pos (point))))))))

;;;###autoload
(defun company-pysmell (command &optional arg &rest ignored)
  "A `company-mode' completion back-end for pysmell.
This requires pysmell.el and pymacs.el."
  (interactive (list 'interactive))
  (case command
    ('interactive (company-begin-backend 'company-pysmell))
    ('prefix (and (derived-mode-p 'python-mode)
                  buffer-file-name
                  (not (company-in-string-or-comment))
                  (company-pysmell--available-p)
                  (company-pysmell--grab-symbol)))
    ('candidates (delete "" (pysmell-get-all-completions)))))

(provide 'company-pysmell)
;;; company-pysmell.el ends here
