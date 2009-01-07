;;; -*- mode: emacs-lisp; indent-tabs-mode: nil -*-
;;; perl-find-library.el --- Functions for locating perl libraries on your system
;;
;;  Copyright (C) 2006 by Nelson Elhage
;;
;;  Author:     Nelson Elhage <nelhage@mit.edu>
;;  Filename:   perl-utils.el
;;  Created:    August 17 2006
;;  Keywords:   util perl
;;
;;  This program is free software; you can redistribute it and/or modify it
;;  under the terms of the GNU General Public License as published by the
;;  Free Software Foundation; either version 2 of the License, or (at your
;;  option) any later version.
;;
;;  This program is distributed in the hope that it will be useful, but
;;  WITHOUT ANY WARRANTY; without even the implied warranty of
;;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;  General Public License for more details.
;;
;;  You should have received a copy of the GNU General Public License along
;;  with this program; if not, write to the Free Software Foundation, Inc.,
;;  59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
;;
;;; Commentary:
;;
;;  The main functionality provided by this file is in the
;;  `perl-read-library' function, which reads the name of a perl
;;  module from the minibuffer with completion. We also provide
;;  `perl-library-path', which turns the name of a perl library into a
;;  path on the system.
;;
;;  We also expose a `perldoc' function, which is a wrapper around
;;  `cperl-perldoc' that uses `perl-read-library', as well as
;;  `perl-find-library', which reads the name of a perl library from
;;  the minibuffer and finds the file corresponding to it on disk.

(require 'cperl-mode)
(require 'cl)

;; Convenience function that really ought be part of emacs to start
;; with, damnit.
(defun replace-all (from to str)
  "Replace all instances of FROM with TO in STR, and return the
result"
  (while (string-match from str)
    (setq str (replace-match to t t str)))
  str)

(defvar *perl-lib-path* '() "Directories in which to find perl libraries in.")
(defvar *perl-libraries* '() "Cache of all known perl libraries on the system")

(defun perl-rebuild-lib-path ()
  "Compute *PERL-LIB-PATH* by invoking perl and printing the
value of @INC"
  (setq *perl-lib-path*
         (remove-if-not
          (lambda (dir) (and (string-match "/" dir ) (file-exists-p dir)))
          (car
           (read-from-string
            (shell-command-to-string "perl -e '$\"=\"\\\" \\\"\"; print \"(\\\"@INC\\\")\"'"))))))

(defun perl-find-all-libraries ()
  "Compute the value of *PERL-LIBRARIES* by searching in
*PERL-LIB-PATH* for files ending in .pod or .pm"
  (if (not *perl-lib-path*)
      (perl-rebuild-lib-path))
  (require 'find-lisp)
  (setq *perl-libraries*
        (mapcan
         (lambda (dir)
           (mapcar
            (lambda (file)
              (replace-all
               (rx (seq "." (| "pod" "pm" ) string-end))
               ""
               (replace-all "/" "::" (substring file (+ 1(length dir))))))
            (find-lisp-find-files dir (rx (seq "." (| "pod" "pm" ) string-end)))))
         *perl-lib-path*)))

;;;###autoload
(defun perldoc (library)
  "Invoke `cperl-perldoc' on LIBRARY, but do completion using *PERL-LIBRARIES*
when run interactively"
  (interactive
   (list (perl-read-library "Perldoc entry: ")))
  (cperl-perldoc library))

;;;###autoload
(defun perl-find-file (library)
  "Find a perl library by module name"
  (interactive
   (list (perl-read-library "Find perl library: ")))
  (find-file (perl-library-path library)))

(defun perl-library-path (library)
  "Returns the path to the perl library LIBRARY on disk."
  (let ((path (shell-command-to-string (concat "perldoc -l " library))))
    ;; Strip the trailing newline
    (and path
         (substring path 0 (- (length path) 1)))))

(defun perl-read-library (&optional prompt)
  "Read the name of a perl library from the minibuffer, with
completion."
  (if (not prompt)
      (setq prompt "Perl library: "))
  (if (not *perl-libraries*)
      (progn
        (message "Building completion list...")
        (perl-find-all-libraries)))
  (completing-read
   prompt
   *perl-libraries*))

(provide 'perl-find-library)
