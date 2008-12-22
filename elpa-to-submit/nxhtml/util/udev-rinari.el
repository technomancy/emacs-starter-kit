;;; udev-rinari.el --- Get rinary sources and set it up
;;
;; Author: Lennart Borgman (lennart O borgman A gmail O com)
;; Created: 2008-08-24T22:32:21+0200 Sun
(defconst udev-rinari:version "0.2");; Version:
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

(defgroup udev-rinari nil
  "Customization group for udev-rinari."
  :group 'nxhtml)

(defcustom udev-rinari-dir "~/rinari-svn/"
  "Directory where to put SVN Rinari sources."
  :type 'directory
  :group 'udev-rinari)

(defcustom udev-rinari-load-rinari nil
  "To load or not to load Rinari..."
  :type '(choice (const :tag "Don't load Rinari" nil)
                 (const :tag "Load Rinari" t))
  :set (lambda (sym val)
         (set-default sym val)
         (when val
           (let* ((base-dir  (expand-file-name "svn/trunk/" udev-rinari-dir))
                  (rhtml-dir (expand-file-name "rhtml/" base-dir))
                  (test-dir  (expand-file-name "test/lisp/" base-dir)))
             (unless (file-directory-p base-dir)  (message "Can't find %s" base-dir))
             (unless (file-directory-p rhtml-dir) (message "Can't find %s" rhtml-dir))
             (unless (file-directory-p test-dir)  (message "Can't find %s" test-dir))
             (add-to-list 'load-path base-dir)
             (add-to-list 'load-path rhtml-dir)
             (add-to-list 'load-path test-dir))
           (require 'rinari)
           (require 'ruby-mode)))
  :group 'udev-rinari)

(defvar udev-rinari-steps
  '(udev-rinari-fetch
    udev-rinari-fetch-diff
    udev-rinari-check-diff
    ;;udev-rinari-install
    ))

(defun udev-rinari-buffer-name (mode)
  "Return a name for current compilation buffer ignoring MODE."
  (udev-buffer-name " *Updating Rinari %s*" udev-rinari-update-buffer mode))

(defvar udev-rinari-update-buffer nil)

(defun udev-rinari-check-conflicts ()
  "Check if Rinari and ruby-mode already loaded and from where.
Give an error if they are loaded from somewhere else than
`udev-rinari-dir' tree."
  (when (featurep 'rinari)
    (let ((old-dir (file-name-directory (car (load-history-filename-element (load-history-regexp "rinari")))))
          (new-dir (expand-file-name "svn/trunk/" udev-rinari-dir)))
      (unless (string= (file-truename old-dir)
                       (file-truename new-dir))
        (error "Rinari is already loaded from: %s" old-dir))))
  (when (featurep 'ruby-mode)
    (let ((old-dir (file-name-directory (car (load-history-filename-element (load-history-regexp "ruby-mode")))))
          (new-dir (expand-file-name "svn/trunk/test/lisp/" udev-rinari-dir)))
      (unless (string= (file-truename old-dir)
                       (file-truename new-dir))
        (error "Ruby-mode is already loaded from: %s" old-dir))))
  )

(defun udev-rinari-setup-when-finished (log-buffer)
  (let ((inhibit-read-only t))
    (with-current-buffer log-buffer
      (widen)
      (goto-char (point-max))
      (insert "\n\nYou must restart Emacs to load Rinari properly.\n")
      (let ((load-rinari-saved-value (get 'udev-rinari-load-rinari 'saved-value))
            (here (point))
            )
        (if load-rinari-saved-value
            (insert "You have setup to load Rinari the next time you start Emacs.\n\n")
          (insert (propertize "Warning:" 'face 'compilation-warning)
                  " You have not setup to load Rinari the next time you start Emacs.\n\n"))
        (insert-button " Setup "
                       'face 'custom-button
                       'action (lambda (btn)
                                 (interactive)
                                 (customize-group-other-window 'udev-rinari)))
        (insert " Setup to load Rinari from fetched sources when starting Emacs.")))))

;;;###autoload
(defun udev-rinari-update ()
  "Fetch and install Rinari from the devel sources.
To determine where to store the sources and how to start rinari
see `udev-rinari-dir' and `udev-rinari-load-rinari'."
  (interactive)
  (udev-rinari-check-conflicts)
  (setq udev-rinari-update-buffer (get-buffer-create "*Update Rinari*"))
  (udev-call-first-step udev-rinari-update-buffer udev-rinari-steps
                        "Starting updating Rinari from development sources"
                        'udev-rinari-setup-when-finished))

(defvar udev-rinari-fetch-buffer nil)

(defun udev-rinari-fetch (log-buffer)
  "Fetch Rinari from development sources."
  (let* ((default-directory (file-name-as-directory udev-rinari-dir)) ;; fix-me: for emacs bug
         )
    (unless (file-directory-p default-directory)
      (make-directory default-directory))
    (with-current-buffer
        (compilation-start
         "svn checkout http://rinari.rubyforge.org/svn/"
         'compilation-mode
         'udev-rinari-buffer-name)
      (setq udev-rinari-fetch-buffer (current-buffer)))))

(defvar udev-rinari-diff-file nil)

(defun udev-rinari-fetch-diff (log-buffer)
  "Fetch diff between local Rinari sources and dev repository."
  (let ((must-fetch-diff t))
    (setq udev-rinari-fetch-diff-buffer
          (when must-fetch-diff
            (let* ((default-directory (file-name-as-directory
                                       (expand-file-name "svn"
                                                         udev-rinari-dir))))
              (setq udev-rinari-diff-file (expand-file-name "../patches.diff"))
              (with-current-buffer
                  (compilation-start
                   (concat "svn diff > " (shell-quote-argument udev-rinari-diff-file))
                   'compilation-mode
                   'udev-rinari-buffer-name)
                (setq udev-continue-on-error-function 'udev-cvs-diff-continue)
                (current-buffer)))))))

(defvar udev-rinari-fetch-diff-buffer nil)

(defun udev-rinari-check-diff (log-buffer)
  "Check output from svn diff command for merge conflicts."
  ;; Fix-me: How can this be checked?
  (when udev-rinari-fetch-diff-buffer
    (let ((buf (find-buffer-visiting udev-rinari-diff-file)))
      (if buf
          (with-current-buffer buf (revert-buffer nil t))
        (setq buf (find-file-noselect udev-rinari-diff-file)))
      (with-current-buffer buf
        (widen)
        (goto-char (point-min))
        (if (search-forward "<<<<<<<" nil t)
            ;; Merge conflict
            (udev-call-next-step udev-rinari-update-buffer 1 nil)
          buf)))))

;; (defun udev-rinari-install ()
;;   "Install Rinari and ruby-mode for use."
;;   (if udev-rinari-load-rinari
;;       (message "Rinari should be loaded now")
;;     (when (y-or-n-p
;;            "You need to set udev-rinari-load-rinari.  Do that now? ")
;;       (customize-group-other-window 'udev-rinari)))
;;   nil)


(provide 'udev-rinari)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; udev-rinari.el ends here
