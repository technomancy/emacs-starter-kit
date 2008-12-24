;;; pcmpl-rake.el --- functions for completing Rake tasks

;; Copyright (C) 2007 Phil Hagelberg

;; Author: Phil Hagelberg <technomancy@gmail.com>
;; URL: http://www.emacswiki.org/cgi-bin/wiki/PcompleteRake
;; Version: 0.1
;; Created: 2007-12-02
;; Keywords: shell completion rake
;; EmacsWiki: PcompleteRake

;; This file is NOT part of GNU Emacs.

;; Last-Updated: Sun Dec 02 15:58:06 2007 PST
;; By: Phil Hagelberg
;; Update #: 1

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

;; Provides pcompletion for the `rake' command, which is basically
;; `make' implemented in Ruby.

;;; Code:

(require 'pcomplete)

;;;###autoload
(defun pcomplete/rake ()
  "Completion rules for the `ssh' command."
  (pcomplete-here (pcmpl-rake-tasks)))

(defun pcmpl-rake-tasks ()
   "Return a list of all the rake tasks defined in the current
projects.  I know this is a hack to put all the logic in the
exec-to-string command, but it works and seems fast"
   (delq nil (mapcar '(lambda(line)
			(if (string-match "rake \\([^ ]+\\)" line) (match-string 1 line)))
		     (split-string (shell-command-to-string "rake -T") "[\n]"))))

(provide 'pcmpl-rake)
;;; pcmpl-rake.el ends here