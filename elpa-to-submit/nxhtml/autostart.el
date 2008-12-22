;;; autostart.el --- Load nxhtml
;;
;; Author: By: Lennart Borgman
;; Created: Fri Dec 15 10:22:41 2006
;; Version:
;; Last-Updated: 2008-03-06T23:49:43+0100 Thu
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

;; ;; In case an old Emacs 22 beta is used, ie mostly for Debian/Ubuntu
;; ;; at the moment. Suggested by Hadron Quark, thanks.
;; (unless (fboundp 'define-globalized-minor-mode)
;;   (defalias 'define-globalized-minor-mode 'define-global-minor-mode))

(defvar nxhtml-install-dir
  (file-name-directory (if load-file-name load-file-name buffer-file-name))
  "Installation directory for nXhtml.")

(unless (featurep 'nxhtml-autostart)
  ;; Provide the feature to avoid loading looping on error.
  (provide 'nxhtml-autostart)
  ;; Use the css-mode that comes with Emacs if there is one.
  ;; Fix-me: remove this loading later:
  (when (fboundp 'css-mode) (require 'css-mode))
  (let* ((util-dir (file-name-as-directory
                    (expand-file-name "util"
                                      nxhtml-install-dir)))
         (related-dir (file-name-as-directory
                       (expand-file-name "related"
                                         nxhtml-install-dir))))
    (add-to-list 'load-path util-dir)
    (add-to-list 'load-path related-dir)

    ;; Autoloading etc
    (require 'as-external)
    (load (expand-file-name "nxhtml-loaddefs.el" nxhtml-install-dir))
    ;; Use the nxml-mode that comes with Emacs if available:
    (unless (fboundp 'nxml-mode)
      (load (expand-file-name "nxml-mode-20041004/rng-auto"
                              nxhtml-install-dir)))
    ;; Patch the rnc include paths
    (load-file (expand-file-name "etc/schema/schema-path-patch.el"
                                 nxhtml-install-dir))
    (rncpp-patch-xhtml-loader)
    ;; Load nXhtml
    (load (expand-file-name "nxhtml/nxhtml-autoload"
                            nxhtml-install-dir))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; autostart.el ends here
