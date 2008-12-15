;;; ri.el --- Ruby Documentation Lookup

;; Copyright (C) 2008 Phil Hagelberg

;; Author: Phil Hagelberg <technomancy@gmail.com>
;; Version: 0.3
;; Keywords: tools, documentation
;; Created: 2008-09-19
;; URL: http://www.emacswiki.org/cgi-bin/wiki/RiEl
;; EmacsWiki: RiEl

;; This file is NOT part of GNU Emacs.

;;; License:

;; This program is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;;; Commentary:

;; ri.el provides an Emacs frontend to Ruby's `ri' documentation
;; tool. It offers lookup and completion.

;; This version will load all completion targets the first time it's
;; invoked. This can be a significant startup time, but it will not
;; have to look up anything after that point. It also has no external
;; dependencies except for having `ri' on your path. If RDoc 2.x is
;; not installed, it will fall back to ri 1.x.

;;; TODO:

;; * Replace Const.method with Const::method on the fly
;; * Hyperlinked ri-follow command
;; * Font Lock
;; * Flex matching?

;;; Code:

(require 'thingatpt)

;; Borrow this functionality from ido.
(unless (functionp 'ido-find-common-substring)
  (require 'ido))

(defvar ri-mode-hook nil
  "Hooks to run when invoking ri-mode.")

(defvar ri-names nil
  "All RI completion targets.")

;;;###autoload
(defun ri (&optional ri-documented)
  "Look up Ruby documentation."
  (interactive)
  (setq ri-documented (or ri-documented (ri-completing-read)))
  (let ((ri-buffer-name (format "*ri %s*" ri-documented)))
    (if (get-buffer ri-buffer-name)
        (switch-to-buffer ri-buffer-name)
      (let ((ri-buffer (get-buffer-create ri-buffer-name)))
        (display-buffer ri-buffer)
        (with-current-buffer ri-buffer
          (erase-buffer)
          (insert (shell-command-to-string (format "ri %s"
                                                   ri-documented)))
          (goto-char (point-min))
          (ri-mode))))))

(defun ri-mode ()
  "Mode for viewing Ruby documentation."
  (buffer-disable-undo)
  (kill-all-local-variables)
  (local-set-key (kbd "q") 'quit-window)
  (local-set-key (kbd "RET") 'ri-follow)
  (setq mode-name "ri")
  (setq major-mode 'ri-mode)
  (setq buffer-read-only t)
  (run-hooks 'ri-mode-hook))

;;; Completion

(defun ri-completing-read ()
  "Read the name of a Ruby class, module, or method to look up."
  (setq ri-names (or ri-names (ri-names)))
  (completing-read "RI: " ri-names nil nil (ri-symbol-at-point)))

(defun ri-names ()
  "One-liner to make RI spit out every class, module, and method name."
  (let ((ri-output (shell-command-to-string "ruby -rubygems -e \"require 'rdoc/ri/reader'; require 'rdoc/ri/cache'; require 'rdoc/ri/paths'; puts RDoc::RI::Reader.new(RDoc::RI::Cache.new(RDoc::RI::Paths.path(true, true, true, true))).all_names.join(\\\"\n\\\")\"")))
    (split-string (if (string-match "no such file to load" ri-output)
                      ;; Fall back to ri 1.x
                      (shell-command-to-string "ri -l")
                    ri-output))))

(defun ri-symbol-at-point ()
  ;; TODO: make this smart about class/module at point
  (let ((ri-symbol (symbol-at-point)))
    (if (and ri-symbol (member ri-symbol ri-names))
        (symbol-name ri-symbol)
      "")))

(defun ri-query (string)
  "Passes the `command' to the `ri' subprocess."
  (with-current-buffer (process-buffer (ri-get-process))
    (erase-buffer)
    (process-send-string (ri-get-process) (concat string "\n"))
    (accept-process-output (ri-get-process) 3 0 t)
    (buffer-string)))

(provide 'ri)
;;; ri.el ends here