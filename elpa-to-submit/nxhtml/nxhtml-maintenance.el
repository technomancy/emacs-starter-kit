;;; nxhtml-maintenance.el --- Some maintenance helpers
;;
;; Author: Lennart Borgman (lennart O borgman A gmail O com)
;; Created: 2008-09-27T15:29:35+0200 Sat
;; Version:
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

(defvar nxhtmlmaint-dir
  (file-name-directory (if load-file-name load-file-name buffer-file-name))
  "Maintenance directory for nXhtml.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Autoload helpers

(defun nxhtmlmaint-autoloads-file ()
  (file-truename (expand-file-name "nxhtml-loaddefs.el" nxhtmlmaint-dir)))

(defun nxhtmlmaint-util-dir ()
  (file-truename (file-name-as-directory
                  (expand-file-name "util" nxhtmlmaint-dir))))

(defvar nxhtmlmaint-autoload-default-directory (nxhtmlmaint-util-dir))

(defun nxhtmlmaint-initialize-autoloads-file ()
  (with-current-buffer (find-file-noselect generated-autoload-file)
    (when (= 0 (buffer-size))
      (insert ";; Autoloads for nXthml
;;
;; This file should be updated by `nxhtmlmaint-get-file-autoloads',
;; `nxhtmlmaint-get-dir-autoloads' or `nxhtmlmaint-get-all-autoloads'.")
    (basic-save-buffer))))

(defun nxmtmlmaint-advice-autoload (on)
  (if on
      (progn
        (ad-activate 'autoload-file-load-name)
        (ad-activate 'make-autoload))
    (ad-deactivate 'autoload-file-load-name)
    (ad-deactivate 'make-autoload)))

(defun nxhtmlmaint-get-file-autoloads (file)
  (interactive (list (buffer-file-name)))
  (let* ((generated-autoload-file (nxhtmlmaint-autoloads-file))
         (emacs-lisp-mode-hook nil)
         (default-directory (nxhtmlmaint-util-dir)))
    (nxhtmlmaint-initialize-autoloads-file)
    ;; Get the autoloads using advice
    (nxmtmlmaint-advice-autoload t)
    (update-file-autoloads file nil)
    (nxmtmlmaint-advice-autoload nil)
    ;; Display
    (display-buffer (find-file-noselect generated-autoload-file))))

(defun nxhtmlmaint-get-dir-autoloads (dir)
  (interactive (list (or (when (buffer-file-name)
                           (file-name-directory (buffer-file-name)))
                         default-directory)))
  (let* ((generated-autoload-file (nxhtmlmaint-autoloads-file))
         (emacs-lisp-mode-hook nil)
         (auto-buf (find-file-noselect generated-autoload-file)))
    (nxhtmlmaint-initialize-autoloads-file)
    ;; Get the autoloads using advice
    (nxmtmlmaint-advice-autoload t)
    ;; Fix-me: Loop instead, some files must be avoided.
    (update-directory-autoloads dir)
    (nxmtmlmaint-advice-autoload nil)
    ;; Display
    (display-buffer (find-file-noselect generated-autoload-file))))

(defun nxhtmlmaint-get-tree-autoloads (root)
  (interactive (list (or (when (buffer-file-name)
                           (file-name-directory (buffer-file-name)))
                         default-directory)))
  (nxhtmlmaint-get-dir-autoloads root)
  (message "----- ROOT=%s" root)
  (let* ((files (directory-files root))
         (sub-dirs (mapcar (lambda (file)
                             (when (and (not (member file '("." "..")))
                                        (not (member file '("nxml-mode-20041004" "old")))
                                        (not (member file '("in")))
                                        (file-directory-p (expand-file-name file root)))
                               file))
                           files)))
    (setq sub-dirs (delq nil sub-dirs))
    (message "sub-dirs=%s" sub-dirs)
    (dolist (dir sub-dirs)
      (let ((full-dir (expand-file-name dir root)))
        (unless (string= full-dir nxhtmlmaint-dir)
          (nxhtmlmaint-get-tree-autoloads full-dir))))))

;;;###autoload
(defun nxhtmlmaint-get-all-autoloads ()
  (interactive)
  (let ((auto-buf (find-file-noselect (nxhtmlmaint-autoloads-file))))
    (with-current-buffer auto-buf
      (erase-buffer)
      (basic-save-buffer)))
  (nxhtmlmaint-get-tree-autoloads nxhtmlmaint-dir))

(defun nxhtmlmaint-autoload-file-load-name (file)
  (let ((name (if nxhtmlmaint-autoload-default-directory
                  (file-relative-name
                   file nxhtmlmaint-autoload-default-directory)
                (file-name-nondirectory file))))
    (if (string-match "\\.elc?\\(\\.\\|\\'\\)" name)
        (substring name 0 (match-beginning 0))
      name)))

(defadvice autoload-file-load-name (around
                                    nxhtmlmaint-advice-autoload-file-load-name
                                    ;;activate
                                    compile)
  (setq ad-return-value (nxhtmlmaint-autoload-file-load-name (ad-get-arg 0))))

(defun nxhtmlmaint-make-autoload (form file)
  ;;(message "form=%S" form)
  (if (or (not (listp form))
          (not (eq 'define-mumamo-multi-major-mode (car form))))
      ad-return-value
    (if ad-return-value
        ad-return-value
      ;; Fix-me: Maybe expand??
      (let ((name (nth 1 form))
            (doc  (nth 2 form)))
        `(autoload ',name ,file ,doc t)
        ))))

(defadvice make-autoload (after
                          nxhtmlmaint-advice-make-autoload
                          ;;activate
                          compile)
  (setq ad-return-value
        (nxhtmlmaint-make-autoload (ad-get-arg 0)
                                             (ad-get-arg 1))))

(defun generate-library-autoloads (library)
  "Insert at point autoloads for Emacs library LIBRARY.
  Works like `generate-file-autoloads', but for a library."
  (interactive
   (list (completing-read "Generate autoloads for library: "
                          'locate-file-completion
                          (cons load-path (get-load-suffixes)))))
  (let ((file (locate-library library)))
    (generate-file-autoloads file)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; nxhtml-maintenance.el ends here
