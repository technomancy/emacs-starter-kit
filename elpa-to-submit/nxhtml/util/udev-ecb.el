;;; udev-ecb.el --- Get ECB sources and set it up
;;
;; Author: Lennart Borgman (lennart O borgman A gmail O com)
;; Created: 2008-08-25T04:02:37+0200 Mon
(defconst udev-ecb:version "0.2");; Version:
;; Last-Updated:
;; URL:
;; Keywords:
;; Compatibility:
;;
;; Features that might be required by this library:
;;
;;   None
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
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:


(require 'udev)

(defgroup udev-ecb nil
  "Customization group for udev-ecb."
  :group 'nxhtml)

(defcustom udev-ecb-dir "~/ecb-cvs/"
  "Directory where to put CVS ECB sources."
  :type 'directory
  :group 'udev-ecb)

(defun udev-ecb-load-ecb ()
  (let ((semantic-found (locate-library "semantic"))
        (eieio-found (locate-library "eieio")))
    ;; Speedbar is included in Emacs now
    (unless semantic-found (message "Can't find semantic"))
    (unless eieio-found (message "Can't find eieio"))
    (if (not (and semantic-found eieio-found))
        (progn
          (message "Can't load ECB")
          (message "Try M-x udev-cedet-update first"))
      (let ((ecb-path (expand-file-name "ecb/" udev-ecb-dir)))
        (add-to-list 'load-path ecb-path)
        (require 'ecb nil t)))))

(defcustom udev-ecb-load-ecb nil
  "To load or not to load ECB..."
  :type 'boolean
  :require 'udev-ecb
  :set (lambda (sym val)
         (set-default sym val)
         (when val
           (udev-ecb-load-ecb)))
  ;; ecb-activate, ecb-customize-most-important to menu
  :group 'udev-ecb)

(defvar udev-ecb-steps
  '(udev-ecb-fetch
    udev-ecb-fetch-diff
    udev-ecb-check-diff
    udev-ecb-install
    ))

(defun udev-ecb-buffer-name (mode)
  "Return a name for current compilation buffer ignoring MODE."
  (udev-buffer-name " *Updating ECB %s*" udev-ecb-update-buffer mode))

(defvar udev-ecb-update-buffer nil)

(defun udev-ecb-check-cedet ()
  (unless (and (locate-library "semantic")
               (locate-library "eieio"))
    (if (not (y-or-n-p "CEDET must be installed first.  Do that now? "))
        (error "Can't install ECB without CEDET")
      (require 'udev-cedet)
      (udev-cedet-update))))

(defun udev-ecb-setup-when-finished (log-buffer)
  (require 'cus-edit)
  (let ((inhibit-read-only t))
    (with-current-buffer log-buffer
      (widen)
      (goto-char (point-max))
      (insert "\n\nYou must restart Emacs to load ECB properly.\n")
      (let ((load-ecb-saved-value (get 'udev-ecb-load-ecb 'saved-value))
            (here (point))
            )
        (if load-ecb-saved-value
            (insert "You have setup to load ECB the next time you start Emacs.\n\n")
          (insert (propertize "Warning:" 'face 'compilation-warning)
                  " You have not setup to load ECB the next time you start Emacs.\n\n"))
        (insert-button " Setup "
                       'face 'custom-button
                       'action (lambda (btn)
                                 (interactive)
                                 (customize-group-other-window 'udev-ecb)))
        (insert " Setup to load ECB from fetched sources when starting Emacs.")))))

;;;###autoload
(defun udev-ecb-update ()
  "Fetch and install ECB from the devel sources.
To determine where to store the sources see `udev-ecb-dir'.
For how to start ECB see `udev-ecb-load-ecb'."
  (interactive)
  (udev-ecb-check-cedet)
  (setq udev-ecb-update-buffer (get-buffer-create "*Update ECB*"))
  (udev-call-first-step udev-ecb-update-buffer udev-ecb-steps
                        "Starting updating ECB from development sources"
                        'udev-ecb-setup-when-finished))

(defun udev-ecb-fetch (log-buffer)
  "Fetch ECB sources (asynchronously)."
  (let ((default-directory (file-name-as-directory udev-ecb-dir)))
    (unless (file-directory-p default-directory)
      (make-directory default-directory))
    (with-current-buffer
        (compilation-start
         "cvs -z3 -d:pserver:anonymous@ecb.cvs.sourceforge.net:/cvsroot/ecb co -P ecb"
         'compilation-mode
         'udev-ecb-buffer-name)
      (current-buffer))))

(defun udev-ecb-cvs-dir ()
  "Return cvs root directory."
  (file-name-as-directory (expand-file-name "ecb" udev-ecb-dir)))

(defun udev-ecb-fetch-diff (log-buffer)
  "Fetch diff between local ECB sources and repository."
  (udev-fetch-cvs-diff (udev-ecb-cvs-dir) 'udev-ecb-buffer-name))

(defun udev-ecb-check-diff (log-buffer)
  "Check cvs diff output for merge conflicts."
  (udev-check-cvs-diff (expand-file-name "your-patches.diff"
                                          (udev-ecb-cvs-dir))
                        udev-ecb-update-buffer))

(defun udev-ecb-install (log-buffer)
  "Install the ECB sources just fetched.
Note that they will not be installed in current Emacs session."
  (udev-batch-compile "-l ecb-batch-compile.el"
                      udev-this-dir
                      'udev-ecb-buffer-name))

(provide 'udev-ecb)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; udev-ecb.el ends here
