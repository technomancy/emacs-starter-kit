;;; company-etags.el --- a company-mode completion back-end for etags
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
(eval-when-compile (require 'etags))
(eval-when-compile (require 'cl))

(defcustom company-etags-use-main-table-list t
  "*Always search `tags-table-list' if set.
If this is disabled, `company-etags' will try to find the one table for each
buffer automatically."
  :group 'company-mode
  :type '(choice (const :tag "off" nil)
                 (const :tag "on" t)))

(defvar company-etags-modes '(c-mode objc-mode c++-mode java-mode jde-mode
                              pascal-mode perl-mode python-mode))

(defvar company-etags-buffer-table 'unknown)
(make-variable-buffer-local 'company-etags-buffer-table)

(defun company-etags-find-table ()
  (let ((file (company-locate-dominating-file (or buffer-file-name
                                                  default-directory)
                                              "TAGS")))
    (when file
      (list (expand-file-name file)))))

(defun company-etags-buffer-table ()
  (or (and company-etags-use-main-table-list tags-table-list)
      (if (eq company-etags-buffer-table 'unknown)
          (setq company-etags-buffer-table (company-etags-find-table))
        company-etags-buffer-table)))

;;;###autoload
(defun company-etags (command &optional arg &rest ignored)
  "A `company-mode' completion back-end for etags."
  (interactive (list 'interactive))
  (case command
    ('interactive (company-begin-backend 'company-etags))
    ('prefix (and (memq major-mode company-etags-modes)
                  (not (company-in-string-or-comment))
                  (require 'etags nil t)
                  (company-etags-buffer-table)
                  (or (company-grab-symbol) 'stop)))
    ('candidates (let ((tags-table-list (company-etags-buffer-table))
                       (completion-ignore-case nil))
                   (and (or tags-file-name tags-table-list)
                        (fboundp 'tags-completion-table)
                        tags-table-list
                        (all-completions arg (tags-completion-table)))))
    ('location (let ((tags-table-list (company-etags-buffer-table)))
                 (when (fboundp 'find-tag-noselect)
                   (let ((buffer (find-tag-noselect arg)))
                     (cons buffer (with-current-buffer buffer (point)))))))
    ('sorted t)))

(provide 'company-etags)
;;; company-etags.el ends here
