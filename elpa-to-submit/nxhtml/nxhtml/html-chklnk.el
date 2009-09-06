;;; html-chklnk.el --- Check links in local HTML sites

;; Copyright (C) 2005 by Lennart Borgman

;; Author:  Lennart Borgman <lennart DOT borgman DOT 073 AT student DOT lu DOT se>
;; Created: Wed Mar 15 14:46:17 2006
(defconst html-chklnk:version "0.2") ;; Version:
;; Last-Updated: Tue Apr 10 04:12:32 2007 (7200 +0200)
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

(eval-when-compile (add-to-list 'load-path default-directory load-path))
(eval-when-compile
  (when (> emacs-major-version 22)
    (let* ((load-path load-path)
           (this-file (or load-file-name
                          (when (boundp 'bytecomp-filename) bytecomp-filename)
                          buffer-file-name))
           (this-dir (file-name-directory this-file)))
      (add-to-list 'load-path (expand-file-name "../../lisp" this-dir))
      (require 'w32shell nil t))))


(eval-when-compile (require 'html-site))
(require 'compile)

(defgroup html-chklnk nil
  "Customization group for html-chklnk."
  :group 'nxhtml)

(defcustom html-chklnk-dir
  (file-name-as-directory
   (expand-file-name
    "html-chklnk"
    (file-name-directory
     (if load-file-name load-file-name buffer-file-name))))

  "Directory where the tools needed are located.
"
  :type 'directory
  :group 'html-chklnk)

(defun html-chklnk-check-site-links (start-file)
  "Check local file web site links.
Currently only internal links are checked."
  (interactive
   (progn
     (html-site-current-ensure-site-defined)
     (if (y-or-n-p "Start from a given file and check links from there? ")
         (let* ((default-start (if (html-site-current-contains buffer-file-name)
                                   buffer-file-name
                                 (car (directory-files (html-site-current-site-dir)
                                                       nil
                                                       "\\.html?$"))))
                (start-file
                 (read-file-name "Start checking from file: "
                                 (html-site-current-site-dir)
                                 nil
                                 nil
                                 default-start)))
           (unless (html-site-dir-contains (html-site-current-site-dir) start-file)
             (error "File %s is not in the site %s" start-file html-site-current))
           (list start-file))
       (list nil))))
  (let* ((default-directory html-chklnk-dir)
         (compile-cmd (concat "perl link_checker.pl "
                              "--site="
                              ;;(html-chklnk-convert-file-name
                               (html-site-current-site-dir)
                               ;;)
                              (if start-file
                                  (concat " --start="
                                          ;;(html-chklnk-convert-file-name
                                           start-file
                                           ;;)
                                          )
                                "")))
         (compilation-buffer-name-function
          '(lambda (dummy) (concat "** Checking links in site "
                                  html-site-current " **")))
         (compilation-scroll-output t)
         (compilation-error-regexp-alist-alist
          '(
            (html-chklnk
             "^\\(.*\\)\\s-+at line \\([0-9]+\\):"
               1 ;; file
               2 ;; line
               )))
         (compilation-error-regexp-alist '(html-chklnk))
         ;;(shell-file-name "cmd")
         ;;(explicit-shell-file-name "cmd")
         ;;(shell (concat exec-directory "cmdproxy.exe"))
         ;;(old-w32shell nil)
         )
    ;; There are trouble with perl paths
;;     (when (featurep 'w32shell)
;;       (when w32shell-current-shell-path
;;         (setq old-w32shell w32shell-current-shell-path)
;;         (w32shell-set-shell "cmd")))
    ;;(message "uses-cygwin=%s" uses-cygwin)(sit-for 8)

    (if (fboundp 'w32shell-save-shell)
        (w32shell-save-shell
          "cmd"
          (compile compile-cmd))
      (compile compile-cmd))

;;     (when old-w32shell
;;       (cond ((string= old-w32shell w32shell-cygwin-bin)
;;              (w32shell-set-shell "cygwin"))
;;             ((string= old-w32shell w32shell-msys-bin)
;;              (w32shell-set-shell "msys"))))
    ))

(defun html-chklnk-convert-file-name (filename)
  (let ((uses-cygwin (and (featurep 'w32shell)
                          (string= w32shell-current-shell-path
                                   w32shell-cygwin-bin)))
        (case-fold-search t)
        )
    (save-match-data
      (if (and uses-cygwin
               (string-match "^\\([a-z]\\):" filename))
          (concat "/cygdrive/" (match-string 1 filename)
                  (substring filename 2))
        filename))))




(provide 'html-chklnk)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; html-chklnk.el ends here
