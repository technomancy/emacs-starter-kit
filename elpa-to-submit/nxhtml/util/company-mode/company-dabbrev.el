;;; company-dabbrev.el --- a dabbrev-like company-mode completion back-end
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

(defcustom company-dabbrev-other-buffers 'all
  "*Determines whether `company-dabbrev' should search other buffers.
If 'all, search all other buffers.  If t, search buffers with the same
major-mode.
See also `company-dabbrev-time-limit'."
  :group 'company
  :type '(choice (const :tag "Off" nil)
                 (const :tag "Same major mode" t)
                 (const :tag "All" all)))

(defcustom company-dabbrev-time-limit .5
  "*Determines how many seconds `company-dabbrev' should look for matches."
  :group 'company
  :type '(choice (const :tag "Off" nil)
                 (number :tag "Seconds")))

(defcustom company-dabbrev-char-regexp "\\sw"
  "*A regular expression matching the characters `company-dabbrev' looks for."
  :group 'company
  :type 'regexp)

(defmacro company-dabrev--time-limit-while (test start limit &rest body)
  (declare (indent 3) (debug t))
  `(let ((company-time-limit-while-counter 0))
     (catch 'done
       (while ,test
         ,@body
         (and ,limit
              (eq (incf company-time-limit-while-counter) 25)
              (setq company-time-limit-while-counter 0)
              (> (float-time (time-since ,start)) ,limit)
              (throw 'done 'company-time-out))))))

(defsubst company-dabbrev--make-regexp (prefix)
  (concat "\\<" (if (equal prefix "")
              company-dabbrev-char-regexp
            (regexp-quote prefix))
          "\\(" company-dabbrev-char-regexp "\\)*\\>"))

(defun company-dabbrev--search-buffer (regexp pos symbols start limit
                                       ignore-comments)
  (save-excursion
    (let (match)
      (goto-char (if pos (1- pos) (point-min)))
      ;; search before pos
      (company-dabrev--time-limit-while (re-search-backward regexp nil t)
          start limit
        (setq match (match-string-no-properties 0))
        (if (and ignore-comments (company-in-string-or-comment))
            (re-search-backward "\\s<\\|\\s!\\|\\s\"\\|\\s|" nil t)
          (push match symbols)))
      (goto-char (or pos (point-min)))
      ;; search after pos
      (company-dabrev--time-limit-while (re-search-forward regexp nil t)
          start limit
        (setq match (match-string-no-properties 0))
        (if (and ignore-comments (company-in-string-or-comment))
            (re-search-forward "\\s>\\|\\s!\\|\\s\"" nil t)
          (push match symbols)))
      symbols)))

(defun company-dabbrev--search (regexp &optional limit other-buffers
                                ignore-comments)
  (let* ((start (current-time))
         (symbols (company-dabbrev--search-buffer regexp (point) nil start limit
                                                  ignore-comments)))
    (when other-buffers
      (dolist (buffer (delq (current-buffer) (buffer-list)))
        (and (or (eq other-buffers 'all)
                 (eq (buffer-local-value 'major-mode buffer) major-mode))
             (with-current-buffer buffer
               (setq symbols
                     (company-dabbrev--search-buffer regexp nil symbols start
                                                     limit ignore-comments))))
        (and limit
             (> (float-time (time-since start)) limit)
             (return))))
    symbols))

;;;###autoload
(defun company-dabbrev (command &optional arg &rest ignored)
  "A dabbrev-like `company-mode' completion back-end."
  (interactive (list 'interactive))
  (case command
    ('interactive (company-begin-backend 'company-dabbrev))
    ('prefix (company-grab-word))
    ('candidates
     (mapcar 'downcase
             (company-dabbrev--search (company-dabbrev--make-regexp arg)
                                      company-dabbrev-time-limit
                                      company-dabbrev-other-buffers)))
    ('ignore-case t)
    ('duplicates t)))

(provide 'company-dabbrev)
;;; company-dabbrev.el ends here
