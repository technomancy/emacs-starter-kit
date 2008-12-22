;;; schema-path-patch.el --- Patch schema paths
;;
;; Author: Lennart Borgman (lennart O borgman A gmail O com)
;; Created: 2008-08-08T20:21:31+0200 Fri
;; Version: 0.2
;; Last-Updated: 2008-08-19T00:21:25+0200 Mon
;; URL:
;; Keywords:
;; Compatibility:
;;
;; Features that might be required by this library:
;;
;;   Cannot open load file: schema-path-patch.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; Schemas here may include parts from nxml and need to know the path.
;; This file can be used to patch the paths.
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

;;(eval-when-compile (require 'cl))

(defvar rncpp-this-dir
  (file-name-as-directory
   (file-name-directory
    (if load-file-name load-file-name buffer-file-name))))

(defun rncpp-get-nxml-schema-dir ()
  ;; First look for nxml-mode included with Emacs
  (let ((schema-dir (file-name-as-directory
                     (expand-file-name "schema" data-directory))))
    (unless (file-directory-p schema-dir)
      ;; This is an old nxml-mode, look for its schemas dir.
      (let ((nxml-mode-dir (file-name-as-directory
                            (file-name-directory (locate-library "nxml-mode")))))
        (setq schema-dir (file-name-as-directory
                          (expand-file-name "schema" nxml-mode-dir)))))
    (unless (file-directory-p schema-dir)
      (error "Can't find schema-dir=%s" schema-dir))
    schema-dir))

;; (defun rncpp-patch-file (file)
;;   (let ((schema-dir (rncpp-get-nxml-schema-dir))
;;         relative-inc-file
;;         absolute-inc-file
;;         (buf (find-file-noselect file))
;;         file-dir
;;         )
;;     (with-current-buffer buf
;;       (setq file-dir (file-name-directory buffer-file-name))
;;       (widen)
;;       (goto-char (point-min))
;;       (setq absolute-inc-file (expand-file-name "xhtml.rnc" schema-dir))
;;       (assert (file-exists-p absolute-inc-file) t)
;;       (setq relative-inc-file
;;             (file-relative-name absolute-inc-file file-dir))
;;       (assert (file-exists-p relative-inc-file) t)
;;       (when (re-search-forward "include \"\\(.*[^a-zA-Z0-9-]xhtml.rnc\\)\"" nil t)
;;         (replace-match relative-inc-file t t nil 1)
;;         (basic-save-buffer)
;;         (kill-buffer (current-buffer))
;;         (message "Patched %s" file)))))

;; Use xhtml-loader.rnc (an idea from Bryan Waite):
(defun rncpp-patch-xhtml-loader ()
  "Patch xhtml-loader.rnc so genshi and mjt rnc files works."
  ;;(interactive)
  (let* ((default-directory rncpp-this-dir)
         (loader-path (expand-file-name "xhtml-loader.rnc"))
         (loader-buf (find-buffer-visiting loader-path))
         (schema-dir (rncpp-get-nxml-schema-dir))
         (schema-relative-dir (file-relative-name schema-dir))
         (loader-string (concat "include \""
                                schema-relative-dir
                                "xhtml.rnc\"")))
    (when loader-buf (kill-buffer loader-buf))
    (setq loader-buf (find-file-noselect loader-path))
    (with-current-buffer loader-buf
      (unless (file-exists-p loader-path)
        (insert loader-string))
      ;; Test if correct
      (if (string= (buffer-substring-no-properties (point-min) (point-max))
                       loader-string)
          (message "xhtml-loader.rnc was ok")
        (message "Patching xhtml-loader.rnc")
        (delete-region (point-min) (point-max))
        (insert loader-string))
      (basic-save-buffer)
      (kill-buffer (current-buffer)))))

;; (defun rncpp-patch-files (&optional no-check-done)
;;   "Patch rnc files in this directory.
;; This will make the includes from here point to those that
;; `nxml-mode' use."
;;   (interactive (list t))
;;   (let ((default-directory rncpp-this-dir)
;;         (done nil)
;;         (done-mark "rncpp-patch-files.done"))
;;     (message "Patching rnc files path in %s" default-directory)
;;     (unless no-check-done
;;       (setq done (file-exists-p done-mark)))
;;     (if done
;;         (message "... patching was already done")
;;       (rncpp-patch-file "mjt.rnc")
;;       (rncpp-patch-file "qtmstr-xhtml.rnc")
;;       (with-current-buffer (find-file-noselect done-mark)
;;         (insert (format-time-string "%Y-%m%dT%T%z"))
;;         (basic-save-buffer)
;;         (kill-buffer (current-buffer)))
;;       (message "... ready patching"))))

;;(rncpp-patch-files)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; schema-path-patch.el ends here
