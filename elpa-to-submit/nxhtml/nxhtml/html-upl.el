;;; html-upl.el --- Uploading of web sites

;; Copyright (C) 2006, 2007 Lennart Borgman

;; Author: Lennart Borgman <lennartDOTborgmanDOT073ATstudentDOTluDOTse>
;; Created: Mon Mar 06 19:09:19 2006
(defconst html-upl:version "0.3") ;; Version:
;; Last-Updated: 2008-03-22T01:23:01+0100 Sat
;; Keywords:
;; Compatibility:
;;
;; Features that might be required by this library:
;;
;;   `cl', `html-site', `html-upl', `mail-prsvr', `mm-util', `timer',
;;   `url-c', `url-parse', `url-vars'.
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
(require 'html-site)

(defgroup html-upl nil
  "Customization group for html-upl."
  :group 'nxhtml)

(defcustom html-upl-dir
  (file-name-as-directory
   (expand-file-name
    "html-upl"
    (file-name-directory
     (if load-file-name load-file-name buffer-file-name))))

  "Directory where the tools needed are located.
The tools for html-upl includes:

- ftpsync.pl
"
  :type 'directory
  :group 'html-upl)

(defun html-upl-browse-remote ()
  (interactive)
  (let ((url (html-site-local-to-web html-site-current
                                     ;;buffer-file-name
                                     (html-site-buffer-or-dired-file-name)
                                     nil)))
    (browse-url url)))
(defun html-upl-browse-remote-with-toc ()
  (interactive)
  (let ((url (html-site-local-to-web html-site-current
                                     ;;buffer-file-name
                                     (html-site-buffer-or-dired-file-name)
                                     t)))
    (browse-url url)))
(defun html-upl-browse-remote-frames ()
  (interactive)
  (let ((url (html-site-local-to-web (html-site-current-frames-file)
                                     ;;buffer-file-name
                                     (html-site-buffer-or-dired-file-name)
                                     nil)))
    (browse-url url)))

(defun html-upl-upload-site-with-toc ()
  (interactive)
  (html-upl-upload-site1 t))
(defun html-upl-upload-site ()
  (interactive)
  (html-upl-upload-site1 nil))
(defun html-upl-upload-site1(with-toc)
  (html-site-current-ensure-site-defined)
  (html-upl-ensure-site-has-host)
  (let ((local-dir (if with-toc
                       (html-site-current-merge-dir)
                     (html-site-current-site-dir)))
        (ftp-host (html-site-current-ftp-host))
        (ftp-user (html-site-current-ftp-user))
        (ftp-pw (html-site-current-ftp-password))
        (ftp-dir (if with-toc
                     (html-site-current-ftp-wtoc-dir)
                   (html-site-current-ftp-dir)))
        (ftpsync-pl (expand-file-name "ftpsync.pl" html-upl-dir))
        )
    (unless (< 0 (length ftp-host))
      (error "Ftp host not defined"))
    (unless (< 0 (length ftp-user))
      (error "Ftp user not defined"))
    (unless (< 0 (length ftp-dir))
      (if with-toc
          (error "Ftp remote directory for pages with TOC not defined")
        (error "Ftp remote directory not defined")))
    (unless (< 0 (length ftp-pw))
      (setq ftp-pw (html-site-get-ftp-pw)))
    (let* (
           (buffer (noshell-procbuf-setup "subprocess for upload"))
           (remote-url (concat "ftp://" ftp-user ":" ftp-pw "@" ftp-host ftp-dir))
           (opt (list
                 "-v"
                 "-p"
                 local-dir
                 remote-url)))
      (apply 'noshell-procbuf-run
             buffer
             "perl" "-w"
             ftpsync-pl
             opt
             ))))

(defun html-upl-ensure-site-has-host ()
  (let ((host (html-site-current-ftp-host)))
    (unless (and host (< 0 (length host)))
      (error "Site %s has no ftp host defined" html-site-current))))

(defun html-upl-remote-dired (dirname)
  "Start dired for remote directory or its parent/ancestor."
  (interactive (list
                (read-directory-name "Local directory: " nil nil t)))
  (html-site-current-ensure-file-in-site dirname)
  (html-upl-ensure-site-has-host)
  (let* ((local-dir dirname)
         (remote-dir (html-site-current-local-to-remote local-dir nil))
         to-parent
         res
         msg)
    (while (not res)
      (condition-case err
          (progn
            (dired remote-dir)
            (setq res t))
        (error ;;(lwarn 't :warning "err=%s" err)
               (setq msg (error-message-string err))))
      ;; It does not look like we always get an error. Check where we are:
      (when res
        (unless (string= default-directory remote-dir)
          (setq res nil)
          (setq msg "")))
      (unless res
        ;; 450  Requested file action not taken File unavailable (e.g. file busy).
        ;; 550  Requested action not taken File unavailable (e.g. file not found, no access).
        (if (or (string= msg "")
                (save-match-data (string-match " \\(?:550\\|450\\) " msg)))
            (progn
              (if (not to-parent)
                  (setq to-parent (concat
                                   (file-name-nondirectory remote-dir)
                                   "/.."))
                (setq to-parent (concat
                                 (file-name-nondirectory remote-dir)
                                 "/"
                                 to-parent "/..")))
              ;;(setq local-dir (directory-file-name (file-name-directory (directory-file-name local-dir))))
              ;;(html-site-current-ensure-file-in-site local-dir)
              ;;(setq remote-dir (html-site-current-local-to-remote local-dir nil))
              (setq remote-dir (directory-file-name (file-name-directory remote-dir)))
              )
          (setq res msg))))
    (if (stringp res)
       (error "%s" msg)
      (when to-parent
        (message "Remote dir not found, showing ancestor %s" to-parent)))))

(defun html-upl-upload-file (filename)
  "Upload a single file in a site.
For the definition of a site see `html-site-current'."
  (interactive (list
                (let ((use-dialog-box nil)
                      (f (file-relative-name
                          ;;(if (derived-mode-p 'dired-mode) (dired-get-file-for-visit) buffer-file-name)
                          (html-site-buffer-or-dired-file-name)
                          )))
                  (read-file-name "File: " nil nil t f))
                ))
  (html-site-current-ensure-file-in-site filename)
  (html-upl-ensure-site-has-host)
  (let* ((buffer (get-file-buffer filename))
         (remote-file (html-site-current-local-to-remote filename nil))
         (remote-buffer (get-file-buffer remote-file))
         (local-file filename))
    (when (or (not buffer-file-name)
              (not (buffer-modified-p buffer))
              (and
               (y-or-n-p (format "Buffer %s is modified. Save buffer and copy? "
                                (buffer-name buffer)))
               (with-current-buffer buffer
                 (save-buffer)
                 (not (buffer-modified-p)))))
      (when (= ?~ (string-to-char local-file))
        (setq local-file (expand-file-name local-file)))
      (when (and (fboundp 'w32-short-file-name)
                 (string-match " " local-file))
        (setq local-file (w32-short-file-name local-file)))
      (copy-file local-file
                 ;;(html-site-current-local-to-remote filename nil)
                 remote-file
                 0)
      (when remote-buffer
        (with-current-buffer remote-buffer
          (revert-buffer nil t t)))
      (message "Upload ready")
      )))

(defun html-upl-edit-remote-file ()
  (interactive)
  (html-upl-edit-remote-file1 nil))
(defun html-upl-edit-remote-file-with-toc ()
  (interactive)
  (html-upl-edit-remote-file1 t))

(defun html-upl-edit-remote-file1(with-toc)
  (html-site-current-ensure-buffer-in-site)
  (html-upl-ensure-site-has-host)
  (let* ((remote-root (concat "/ftp:"
                              (html-site-current-ftp-user)
                              "@" (html-site-current-ftp-host)
                              ":"
                              (if with-toc
                                  (html-site-current-ftp-wtoc-dir)
                                (html-site-current-ftp-dir))))
;;          (remote-file (html-site-path-in-mirror (html-site-current-site-dir)
;;                                                 buffer-file-name
;;                                                 remote-root))
         (remote-file (html-site-current-local-to-remote buffer-file-name nil))
         )
    (find-file remote-file)))

(defun html-upl-ediff-file (filename)
  "Run ediff on local and remote file.
FILENAME could be either the remote or the local file."
  ;;(interactive "fFile (local or remote): ")
  (interactive (list
                (or (html-site-buffer-or-dired-file-name)
                    (read-file-name "File: "))))
  (html-upl-ensure-site-has-host)
  (let* ((is-local (html-site-file-is-local filename))
         remote-name
         local-name)
    (if is-local
        (progn
          (html-site-current-ensure-file-in-site filename)
          (setq remote-name (html-site-current-local-to-remote filename nil))
          (setq local-name filename))
      (setq local-name (html-site-current-remote-to-local filename nil))
      (html-site-current-ensure-file-in-site local-name)
      (setq remote-name filename))
    (let ((local-buf (find-file local-name))
          (remote-buf (find-file remote-name)))
      (ediff-buffers local-buf remote-buf))))

;;(defun html-site-buffer-or-dired-file-name ()
;; (defun html-upl-ediff-buffer ()
;;   "Run ediff on local and remote buffer file.
;; The current buffer must contain either the local or the remote file."
;;   (interactive)
;;   (html-upl-ediff-file (buffer-file-name)))

(provide 'html-upl)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; html-upl.el ends here

;; (defun html-site-local-to-remote-path (local-file protocol with-toc)
;;   (let ((remote-dir (if (eq protocol 'ftp)
;;                         (if with-toc
;;                             (html-site-current-ftp-wtoc-dir)
;;                           (html-site-current-ftp-dir))
;;                       (if with-toc
;;                           (html-site-current-web-wtoc-dir)
;;                         (html-site-current-web-dir)))))
;;     (html-site-path-in-mirror
;;      (html-site-current-site-dir) local-file remote-dir)))

;; (defun html-site-local-to-web (local-file with-toc)
;;   (let ((web-file (html-site-local-to-remote-path local-file 'http with-toc))
;;         (web-host (html-site-current-web-host)))
;;     (save-match-data
;;       (unless (string-match "^https?://" web-host)
;;         (setq web-host (concat "http://" web-host))))
;;     (when (string= "/" (substring web-host -1))
;;       (setq web-host (substring web-host 0 -1)))
;;     (concat web-host web-file)
;;     ))
;;
;;; Use tramp-tramp-file-p instead:
;; (defun html-upl-file-name-is-local (file-name)
;;   "Return nil unless FILE-NAME is a Tramp file name."
;;   (save-match-data
;;     (not (string-match "^/[a-z]+:" file-name))))

;; (defun html-upl-remote-to-local (remote-file)
;;   (let ((remote-site-dir (html-site-current-web-dir)))
;;     (unless (html-site-dir-contains remote-site-dir remote-file)
;;       (error "")))
;;   )

