;;; company-abbrev.el --- a company-mode completion back-end for abbrev
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

(require 'company)
(eval-when-compile (require 'cl))
(require 'abbrev)

(defun company-abbrev-insert (match)
  "Replace MATCH with the expanded abbrev."
  (expand-abbrev))

;;;###autoload
(defun company-abbrev (command &optional arg &rest ignored)
  "A `company-mode' completion back-end for abbrev."
  (interactive (list 'interactive))
  (case command
        ('interactive (company-begin-backend 'company-abbrev
                                             'company-abbrev-insert))
        ('prefix (company-grab-symbol))
        ('candidates (nconc
                      (delete "" (all-completions arg global-abbrev-table))
                      (delete "" (all-completions arg local-abbrev-table))))
        ('meta (abbrev-expansion arg))
        ('require-match t)))

(provide 'company-abbrev)
;;; company-abbrev.el ends here
