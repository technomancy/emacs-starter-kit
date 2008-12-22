;;; udev-cedet.el --- Get CEDET development sources and set it up
;;
;; Author: Lennart Borgman (lennart O borgman A gmail O com)
;; Created: 2008-08-22
(defconst udev-cedet:version "0.2") ;; Version:
;; Last-Updated: 2008-08-24T18:19:15+0200 Sun
;; URL:
;; Keywords:
;; Compatibility:
;;
;; Features that might be required by this library:
;;
;;   `cl', `udev'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; Fetch and install CEDET from the devel sources.
;;
;; See `udev-cedet-update' for more information.
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

(defgroup udev-cedet nil
  "Customization group for udev-cedet."
  :group 'nxhtml)

(defcustom udev-cedet-dir "~/cedet-cvs/"
  "Directory where to put CVS CEDET sources."
  :type 'directory
  :group 'udev-cedet)

(defun udev-cedet-load-cedet (must-be-fetched)
  (let ((cedet-el (expand-file-name "cedet/common/cedet.el"
                                    udev-cedet-dir)))
    ;;(message "cedet-el=%s, exists=%s, mbf=%s" cedet-el (file-exists-p cedet-el) must-be-fetched)
    (unless (featurep 'cedet)
      (when (file-exists-p cedet-el)
        (condition-case-no-debug err
            (load-file cedet-el)
          (error (message "%s" err))))
      (unless (featurep 'cedet)
        (when must-be-fetche
          (error "Could not load ecb???"))
        (when (y-or-n-p "Could not load CEDET, update from dev sources? ")
          (udev-cedet-update)
          (load-file cedet-el))))))

(defcustom udev-cedet-load-cedet nil
  "To load or not to load CEDET..."
  :type '(choice (const :tag "Don't load CEDET" nil)
                 (set :tag "Choose what to load"
                      (const :tag "EDE Project Management" ede)
                      (radio :tag "Choose parsing and completion features"
                             (const :tag "Minimum features (database+idle reparse)" min-parse)
                             (const :tag "Semantic navigator etc" code-helpers)
                             (const :tag "Intellisense etc" gaudy-code-helpers))
                      (const srecode))
                 (const :tag "Load whole CEDET" t))
  :require 'udev-cedet
  :set (lambda (sym val)
         (set-default sym val)
         (when val
           (udev-cedet-load-cedet nil)
           (when (featurep 'cedet)
             (let ((use-ede
                    (or (eq val t)
                        (memq 'ede val)))
                   (use-min-parse
                    (or (eq val t)
                        (memq 'min-parse val)))
                   (use-code-helpers
                    (or (eq val t)
                        (memq 'code-helpers val)))
                   (use-gaudy-code-helpers
                    (or (eq val t)
                        (memq 'gaudy-code-helpers val)))
                   )
                 (global-ede-mode (if use-ede 1 -1))
                 (when use-min-parse
                   (semantic-load-enable-minimum-features))
                 (when use-code-helpers
                   (semantic-load-enable-code-helpers))
                 (when use-gaudy-code-helpers
                   (semantic-load-enable-gaudy-code-helpers))
                 ))))
  :group 'udev-cedet)

;; (defun udev-cedet-fontify-marker (limit)
;;   (message "here 1 %s-%s" (point) limit)
;;   (when (= (point) (line-beginning-position))
;;     (when (eq (char-after) ?\ )
;;       (message "here 2")
;;       (put-text-property (point) (1+ (point))
;;                          'face 'highlight)
;;       )))

;; (define-derived-mode udev-cedet-compilation-mode compilation-mode
;;   "CVS command"
;;   "For cvs command output."
;;   ;;(font-lock-add-keywords nil '((udev-cedet-fontify-marker)))
;;   )

(defvar udev-cedet-steps
  '(udev-cedet-fetch
    udev-cedet-fetch-diff
    udev-cedet-check-diff
    udev-cedet-install
    ))

(defun udev-cedet-buffer-name (mode)
  "Return a name for current compilation buffer ignoring MODE."
  (udev-buffer-name " *Updating CEDET %s*" udev-cedet-update-buffer mode))

(defvar udev-cedet-update-buffer nil)

(defun udev-cedet-setup-when-finished (log-buffer)
  (require 'cus-edit)
  (let ((inhibit-read-only t))
    (with-current-buffer log-buffer
      (widen)
      (goto-char (point-max))
      (insert "\n\nYou must restart Emacs to load CEDET properly.\n")
      (let ((load-cedet-saved-value (get 'udev-cedet-load-cedet 'saved-value))
            (here (point))
            )
        (if load-cedet-saved-value
            (insert "You have setup to load CEDET the next time you start Emacs.\n\n")
          (insert (propertize "Warning:" 'face 'compilation-warning)
                  " You have not setup to load CEDET the next time you start Emacs.\n\n"))
        (insert-button " Setup "
                       'face 'custom-button
                       'action (lambda (btn)
                                 (interactive)
                                 (customize-group-other-window 'udev-cedet)))
        (insert " Setup to load CEDET from fetched sources when starting Emacs.")))))

;;;###autoload
(defun udev-cedet-update ()
  "Fetch and install CEDET from the devel sources.
To determine where to store the sources see `udev-cedet-dir'.
For how to start CEDET see `udev-cedet-load-cedet'."
  (interactive)
  (setq udev-cedet-update-buffer
        (udev-call-first-step "*Update CEDET*"
                              ;;udev-cedet-update-buffer
                              udev-cedet-steps
                              "Starting updating CEDET from development sources"
                              'udev-cedet-setup-when-finished)))

(defun udev-cedet-fetch (log-buffer)
  "Fetch CEDET sources (asynchronously)."
  (let ((default-directory (file-name-as-directory udev-cedet-dir)))
    (unless (file-directory-p default-directory)
      (make-directory default-directory))
    (with-current-buffer
        (compilation-start
         "cvs -z3 -d:pserver:anonymous@cedet.cvs.sourceforge.net:/cvsroot/cedet co -P cedet"
         'compilation-mode
         'udev-cedet-buffer-name)
      (current-buffer))))

(defun udev-cedet-cvs-dir ()
  "Return cvs root directory."
  (file-name-as-directory (expand-file-name "cedet" udev-cedet-dir)))

(defun udev-cedet-fetch-diff (log-buffer)
  "Fetch diff between local CEDET sources and repository."
  (udev-fetch-cvs-diff (udev-cedet-cvs-dir) 'udev-cedet-buffer-name))

(defun udev-cedet-check-diff (log-buffer)
  "Check cvs diff output for merge conflicts."
  (udev-check-cvs-diff (expand-file-name "your-patches.diff"
                                          (udev-cedet-cvs-dir))
                        udev-cedet-update-buffer))

(defun udev-cedet-install (log-buffer)
  "Install the CEDET sources just fetched.
Note that they will not be installed in current Emacs session."
  (udev-batch-compile "-l cedet-build.el -f cedet-build"
                      (udev-cedet-cvs-dir)
                      'udev-cedet-buffer-name))

(provide 'udev-cedet)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; udev-cedet.el ends here
