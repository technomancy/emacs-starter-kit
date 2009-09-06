(setq message-log-max t)
(setq debug-on-error t)
;;; autostart.el --- Load nxhtml
;;
;; Author: By: Lennart Borgman
;; Created: Fri Dec 15 2006
;; Version:
;; Last-Updated: 2009-04-30 Thu
;; Keywords:
;; Compatibility:
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
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(message "Nxml/Nxhtml Autostart.el loading ...")
(defconst nxhtml-load-time-start (float-time))

(defconst nxhtml-install-dir
  (file-name-directory (or load-file-name
                           (when (boundp 'bytecomp-filename) bytecomp-filename)
                           buffer-file-name))
  "Installation directory for nXhtml.")
;; (setq nxhtml-install-dir (file-name-directory
;;                           (or load-file-name
;;                               (when (boundp 'bytecomp-filename) bytecomp-filename)
;;                               buffer-file-name)))

;; (defun nxhtml-custom-load-and-get-value (symbol)
;;   (custom-load-symbol symbol)
;;   (symbol-value symbol))

(defun nxhtml-custom-autoload (symbol load &optional noset)
  "Like `custom-autoload', but also run :set for defcustoms etc."
  ;; Fix-me: is-boundp is currently always t because of the order in
  ;; loaddefs.
  (let* ((is-boundp (prog1 (boundp symbol)
                      (custom-autoload symbol load noset)))
         (standard (get symbol 'standard-value))
         (saved (get symbol 'saved-value))
         ;; Fix-me: property custom-set etc are not available
         (custom-set (get symbol 'custom-set))
         (custom-initialize (get symbol 'custom-initialize))
         (set (or custom-set 'custom-set-default))) ;; Fix-me: initialize
    ;;(message "nx:symbol = %s, standard/saved=%s/%s, custom-set=%s, boundp=%s,val=%s" symbol standard saved custom-set is-boundp (when is-boundp (symbol-value symbol)))
    (setq custom-set t) ;; Not available here
    (when (or custom-initialize
              (and saved
                   (not (equal (car saved) (symbol-value symbol)))
                   custom-set))
      ;;(message "nx:custom-load-symbol %s" symbol)
      (funcall set symbol (car saved))
      (custom-load-symbol symbol)
      )))

(defun nxhtml-list-loaded-features ()
  (interactive)
  (let ((buf (when (called-interactively-p)
               (get-buffer-create "*nXhtml loaded features*"))))
    (if buf
        (with-current-buffer buf (erase-buffer))
      (message "")
      (message "=== Loaded at nxhtml/autostart.el end:"))
    (dolist (feature '(
                       as-external
                       html-chklnk
                       html-imenu
                       html-move
                       html-pagetoc
                       html-quote
                       html-site
                       html-toc
                       html-upl
                       html-wtoc
                       inlimg
                       mumamo
                       nxhtml-bug
                       nxhtml-menu
                       nxhtml-mode
                       nxhtml-mumamo
                       nxhtml-strval
                       nxhtml
                       nxhtml-js
                       nxml-where
                       outline-magic
                       rngalt
                       tidy-xhtml
                       xhtml-help
                       ))
      (when (featurep feature)
        (if buf
            (with-current-buffer buf
              (insert (format "(feature '%s)=%s\n" feature (featurep feature))))
          (message "(feature '%s)=%s" feature (featurep feature)))))
    (if buf
        (display-buffer buf)
      (message ""))))

(unless (featurep 'nxhtml-autostart)
  ;; Provide the feature here to avoid loading looping on error.
  (provide 'nxhtml-autostart)

  (if (< emacs-major-version 23)
      (load (expand-file-name "autostart22" nxhtml-install-dir))
    ;; Check that the nxml-mode included with Emacs is used. There
    ;; has been some problems on Debian with this.
    (let ((nxml-mode-file (locate-library "nxml-mode"))
          (help-file      (locate-library "help")))
      (unless (string= (expand-file-name ".." help-file)
                       (expand-file-name "../.." nxml-mode-file))
        (error "Wrong nxml-mode=%s used, please use the one that comes with Emacs" nxml-mode-file))))

  (let* ((util-dir (file-name-as-directory (expand-file-name "util" nxhtml-install-dir)))
         (related-dir (file-name-as-directory (expand-file-name "related" nxhtml-install-dir)))
         (nxhtml-dir (file-name-as-directory (expand-file-name "nxhtml" nxhtml-install-dir)))
         (company-dir (file-name-as-directory (expand-file-name "util/company-mode" nxhtml-install-dir)))
         (tests-dir (file-name-as-directory (expand-file-name "tests" nxhtml-install-dir))))
    (add-to-list 'load-path nxhtml-dir)
    (add-to-list 'load-path related-dir)
    (add-to-list 'load-path util-dir)
    (add-to-list 'load-path nxhtml-install-dir)
    (add-to-list 'load-path company-dir)
    (add-to-list 'load-path tests-dir)

    (message "... nXhtml loading %.1f seconds elapsed ..." (- (float-time) nxhtml-load-time-start))

    ;; Autoloading etc

    ;; Fix-me: Why must as-external be loaded? Why doesn't it work in batch?
    ;;(unless noninteractive (require 'as-external))

    (load (expand-file-name "nxhtml-loaddefs" nxhtml-install-dir))
    (message "... nXhtml loading %.1f seconds elapsed ..." (- (float-time) nxhtml-load-time-start))

    ;; Turn on `nxhtml-global-minor-mode' unconditionally
    (nxhtml-global-minor-mode 1)
    (message "... nXhtml loading %.1f seconds elapsed ..." (- (float-time) nxhtml-load-time-start))

    ;; Patch the rnc include paths
    (when (fboundp 'nxml-mode)
      (load (expand-file-name "etc/schema/schema-path-patch"
                              nxhtml-install-dir))
      (rncpp-patch-xhtml-loader))
    (message "... nXhtml loading %.1f seconds elapsed ..." (- (float-time) nxhtml-load-time-start))

    ;; Load nXhtml
    (load (expand-file-name "nxhtml/nxhtml-autoload" nxhtml-install-dir)))
    (message "... nXhtml loading %.1f seconds elapsed ..." (- (float-time) nxhtml-load-time-start))

  ;; Tell what have been loaded of nXhtml:
  (nxhtml-list-loaded-features)

  ;; How long time did it all take?
  (message "Nxml/Nxhtml Autostart.el loaded in %.1f seconds" (- (float-time) nxhtml-load-time-start))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; autostart.el ends here
