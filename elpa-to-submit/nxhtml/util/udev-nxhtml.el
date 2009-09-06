;;; udev-nxhtml.el --- Get nXhtml development sources and set it up
;;
;; Author: Lennart Borgman (lennart O borgman A gmail O com)
;; Created: 2008-08-27T16:43:40+0200 Wed
(defconst udev-nxhtml:version "0.2");; Version:
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

(eval-when-compile (require 'udev))

(defvar udev-nxhtml-install-dir nil)

(defun udev-nxhtml-init (install-dir)
  "Check setup and inform user."
  (interactive (list nil))
  (switch-to-buffer " *nXhtml update initialization*")
  (udev-control-mode)
  (let ((bzr-prog (executable-find "bzr"))
        (inhibit-read-only t)
        here
        (bzr-org-url "http://bazaar-vcs.org/")
        (current-nxhtml-dir (when (boundp 'nxhtml-install-dir) nxhtml-install-dir))
        bzr-init-done
        (ok t)
        )
    (erase-buffer)
    ;;;;;;;;;;;;;;;;;;;;;;;
    ;; Say hallo
    (setq here (point))
    (insert "nXhtml update/installation

This is for update or install of nXhtml directly from the development repository at
Launchpad.")
    ;;;;;;;;;;;;;;;;;;;;;;;
    ;; Check bazaar
    (when (not bzr-prog)
      (setq ok nil)
      (insert "\n\nYou must however first install bazaar, see\n\n  ")
      (insert-text-button bzr-org-url
                          'action
                          `(lambda (btn) (interactive)
                             (browse-url ,bzr-org-url))))
    ;;;;;;;;;;;;;;;;;;;;;;;
    ;; Get installation dir
    (unless install-dir (setq install-dir current-nxhtml-dir))
    (when install-dir
      (unless (or (file-directory-p install-dir)
                  ;; Or parent:
                  (file-directory-p
                   (file-name-directory
                    (directory-file-name install-dir))))
        (setq ok nil)
        (insert (propertize
                 (format "\n\nCan't find %s or its parent"
                         install-dir)
                 'face 'compilation-error))))
    (when ok
      (unless install-dir
        (setq ok nil)
        (insert "\n\nCan't find your current nXhtml.
If you have nXhtml installed please load it and then run this command again.
Else click here and tell me where you want nXhtml to be installed:\n\n")
        (insert-text-button " Where? "
                            'face 'custom-button
                            'action
                            '(lambda (btn) (interactive)
                               (udev-nxhtml-init
                                (read-directory-name "Where do you want nXhtml: ")))))
      )
    ;;;;;;;;;;;;;;;;;;;;;;;
    ;; Install or update
    (when ok
      (setq bzr-init-done
            (file-directory-p (expand-file-name ".bzr"
                                                install-dir)))
      (if (not bzr-init-done)
          ;; fix-me
          (progn
            (insert "\n
Your current nXhtml dir has not synchronized with the repository
from the beginning.  Therefore a new directory with the name
'nxhtml' will be created.")
            (when (string= (file-name-nondirectory
                            (directory-file-name current-nxhtml-dir))
                           "nxhtml")
              (insert "
Your current nXhtml dir will be renamed by adding '-old' to the name."))
            (insert "\n\n")
            (fill-region here (point))
            (setq install-dir (file-name-as-directory current-nxhtml-dir))
            (insert-text-button " Get nXhtml "
                                'face 'custom-button
                                'action `(lambda (btn) (interactive)
                                           (udev-nxhtml-first-fetch ,install-dir)))
            (insert "  Get nXhtml from Launchpad")
            (setq here (point))
            )
        (insert "\n\nClick to pull new nXhtml files from Launchpad"
                "to your current nXhtml:\n\n  ")
        (insert-text-button " Update nXhtml "
                            'face 'custom-button
                            'action `(udev-nxhtml-pull-fetch ,install-dir))
        (setq here (point))))
    (fill-region here (point))
    ))

(defvar udev-nxhtml-update-buffer nil)

(defun udev-nxhtml-buffer-name (mode)
  "Return a name for current compilation buffer ignoring MODE."
  (udev-buffer-name "*Updating/installing nXhtml %s*" udev-nxhtml-update-buffer mode))

(defvar udev-nxhtml-first-fetch-steps
  '(
    udev-nxhtml-rename-old
    udev-nxhtml-bzr-branch
    ))

(defun udev-nxhtml-first-fetch (install-dir)
  "Fetch and install nXhtml from the devel sources.
To determine where to store the sources see `udev-nxhtml-dir'.
For how to start nxhtml see `udev-nxhtml-load-nxhtml'."
  (setq udev-nxhtml-install-dir install-dir)
  (setq udev-nxhtml-update-buffer
        (udev-call-first-step "*Update/Install nXhtml*"
                              (if (file-directory-p install-dir)
                                  udev-nxhtml-first-fetch-steps
                                (cdr udev-nxhtml-first-fetch-steps))
                              "Starting updating/installing nXhtml from development sources"
                              'udev-nxhtml-after-fetch-finished)))

(defun udev-nxhtml-rename-old (log-buffer)
  (with-current-buffer (get-buffer-create " *Rename old nXhtml*")
    (let* ((inhibit-read-only t)
           (file-name (file-name-nondirectory
                       (directory-file-name udev-nxhtml-install-dir)))
           (from-name udev-nxhtml-install-dir)
           (to-name (file-name-as-directory
                     (concat (directory-file-name udev-nxhtml-install-dir) "-old"))))
      (erase-buffer)
      ;; Do some reasonable tests.
      (insert (format "
Rename
  %s
to
  %s"
                      from-name
                      to-name))
      (insert (propertize "

Please notice that if something goes wrong you may have to rename
the directory back!\n\n"
                          'face 'compilation-warning))
      (display-buffer (current-buffer))
      (unless (string= "nxhtml" file-name)
        (error "Directory file name must be 'nxhtml': %s" file-name))
      (if (not (yes-or-no-p "Rename directory? "))
          (progn
            (goto-char (point-max))
            (insert (propertize "Can't continue if directory is not renamed"
                                'face 'compilation-warning))
            (udev-set-last-error log-buffer "User cancelled")
            ;;(error "Can't continue if directory is not renamed")
            )
        (goto-char (point-max))
        (condition-case err
            (progn
              (rename-file from-name to-name)
              (insert (propertize "Done" 'face 'compilation-info)))
          (error
           (insert (propertize
                    (format "Error: %s" (error-message-string err))
                    'face 'compilation-error))
           (error "%s" (error-message-string err))))))

    (current-buffer)))

(defun udev-nxhtml-bzr-branch (log-buffer)
  "Create a branch that we can use and update later."
  (let ((default-directory (file-name-as-directory
                            (file-name-directory
                             (directory-file-name udev-nxhtml-install-dir)))))
    (with-current-buffer
        (compilation-start
         "bzr branch lp:nxhtml"
         'compilation-mode
         'udev-nxhtml-buffer-name)
      (current-buffer))))

(defun udev-nxhtml-after-fetch-finished (log-buffer)
  (let ((inhibit-read-only t))
    (with-current-buffer log-buffer
      (widen)
      (goto-char (point-max))
      (if nxhtml-install-dir
          (insert "\n\nYou must restart Emacs to load the new nXhtml.")
        (insert (format "
Please add

  (load-file \"%s\")

to your .emacs and restart Emacs."
                        (expand-file-name "autostart.el"
                                          udev-nxhtml-install-dir))))
      (insert "\n\nTo see what is new look in ")
      (insert-text-button "nxhtml-changes.html"
                          'action
                          `(lambda (btn) (interactive)
                             (let* ((root ,udev-nxhtml-install-dir)
                                    (changes-file
                                     (expand-file-name
                                      "nxhtml/doc/nxhtml-changes.html"
                                      root)))
                            (browse-url changes-file))))
      )))

(defvar udev-nxhtml-update-steps
  '(
    udev-nxhtml-bzr-pull
    ))

(defun udev-nxhtml-pull-fetch (install-dir)
  "Update nXhtml from the devel sources."
  (setq udev-nxhtml-install-dir install-dir)
  (setq udev-nxhtml-update-buffer
        (udev-call-first-step "*Update/Install nXhtml*"
                              (if (file-directory-p install-dir)
                                  udev-nxhtml-first-fetch-steps
                                (cdr udev-nxhtml-first-fetch-steps))
                              "Starting updating/installing nXhtml from development sources"
                              'udev-nxhtml-after-fetch-finished)))

(defun udev-nxhtml-bzr-pull (log-buffer)
  "Update a branch we have."
  (let ((default-directory (file-name-as-directory udev-nxhtml-install-dir)))
    (with-current-buffer
        (compilation-start
         "bzr pull"
         'compilation-mode
         'udev-nxhtml-buffer-name)
      (current-buffer))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; udev-nxhtml.el ends here
