;;; nxml-mode-os-additions.el --- additional functions for nxml-mode

;; Copyright (C) 2004 by Oliver Steele

;; Author: Oliver Steele <steele@osteele.com>
;; Version: 1.0 (2004-08-08)
;; Homepage: http://osteele.com/sources/nxml-mode-os-additions.el
;; Keywords: XML

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of
;; the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE.  See the GNU General Public License for more details.

;; You should have received a copy of the GNU General Public
;; License along with this program; if not, write to the Free
;; Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
;; MA 02111-1307 USA

;;; Description:

;; nxml-mode-os-additions defines additional functions for using
;; James Clark's nxml-mode:
;; - reload the current buffer's schema
;; - edit the current buffer's schema

;;; Installation:
;;
;; To use nxml-mode-os-additions.el, put it in your load-path and add
;; the following to your .emacs:
;;
;;   (load-library "nxml-mode-os-additions")

;; Configuration:
;;
;; To make it easier to use, assign the commands to some keys.
;; Once nxml-mode has been loaded, you can define keys on nxml-mode-map.
;; The function rng-mode-os-additions-set-key-bindings illustrates
;; this.
;;
;; Alternatively, you can place the following in your .emacs:
;;  (add-hook 'nxml-mode-hook 'rng-mode-os-additions-set-key-bindings)

;;; ChangeLog:
;;
;; 2004-08-08 (version 1.0):
;;   * Initial public release

;; Added require rng-valid (Lennart Borgman)

;;; Code:

(require 'nxml-mode)
(require 'rng-valid)

(defun rng-mode-os-additions-set-key-bindings ()
  (define-key nxml-mode-map "\C-c\C-s\C-r" 'rng-reload-schema-file)
  ; move the rng-set-schema-file-and-validate to another key binding
  ;(define-key nxml-mode-map "\C-c\C-s\C-s" 'rng-set-schema-file-and-validate)
  (define-key nxml-mode-map "\C-c\C-sf" 'rng-find-schema-file)
  )

(defun rng-reload-schema-file ()
  "Reloads the current schema file."
  (interactive)
  (let ((schema-filename rng-current-schema-file-name))
    (when schema-filename
      (setq rng-current-schema (rng-load-schema schema-filename))
      (run-hooks 'rng-schema-change-hook)
      (message "Reloaded schema %s" schema-filename))
    (unless schema-filename
      (rng-set-schema-and-validate))))

;; Helper function for rng-find-schema-file*
(defun rng-apply-find-schema-file (fn)
  (let ((schema-filename rng-current-schema-file-name))
    (unless schema-filename
      (error "This file is not associated with a schema file."))
    (funcall fn schema-filename)))

(defun rng-find-schema-file ()
  "Edit the current schema file."
  (interactive)
  (rng-apply-find-schema-file 'find-file))

(defun rng-find-schema-file-other-frame ()
  "Edit the current schema in another frame."
  (interactive)
  (rng-apply-find-schema-file 'find-file-other-frame))

(defun rng-find-schema-file-other-window ()
  "Edit the current schema in another window."
  (interactive)
  (rng-apply-find-schema-file 'find-file-other-window))
