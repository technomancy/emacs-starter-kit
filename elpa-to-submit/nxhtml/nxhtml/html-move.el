;;; html-move.el --- Move a file in a local file web site.

;; Copyright (C) 2005 by Lennart Borgman

;; Author:  Lennart Borgman <lennart DOT borgman DOT 073 AT student DOT lu DOT se>
;; Created: Thu Jan 12 08:11:30 2006
(defconst html-move:version "0.31") ;; Version:
;; Last-Updated: Tue Feb 20 23:59:43 2007 (3600 +0100)
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
;;DO NOT USE YET!
;;
;;  Functions for moving a file in a local file web site. Moves the
;;  file and fixes the local affected links after the move.
;;
;;  To use this file you may in your .emacs put
;;
;;      (require 'html-move)
;;
;;  Call the function `html-move-buffer-file' to move a file.
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
(eval-when-compile (require 'html-site))
(require 'url-parse)

(defun html-move-make-new-url (old-url from-dir to-dir)
  "Make new relative url.
If OLD-URL is an absolute path then return it.  Otherwise OLD-URL
is assumed to be relative FROM-DIR. Return a new url relative
TO-DIR that gives the same absolute path."
  (if (or (file-name-absolute-p old-url)
          (char-equal ?# (string-to-char old-url))
          (let ((urlobj (url-generic-parse-url old-url)))
            (url-host urlobj)))
      (progn
        nil)
    (let* (
           (relative-path (file-relative-name from-dir to-dir))
           (new-abs-url (expand-file-name (concat relative-path old-url) to-dir))
           (new-url (file-relative-name new-abs-url to-dir)))
      new-url)))


(defun html-move-in-dir-tree (file tree)
  (let ((rel-path (file-relative-name file tree)))
    (or (string= "." rel-path)
        (not (string= ".." (substring rel-path 0 2))))))

(defun html-move-buffer-file (to)
  "Move current buffer file to another directory and/or name.
Correct the affected relative links in the moved file and the
links to the file moved in the directory tree
`html-site-current-site-dir'."
  ;;(interactive "GMove to: ")
  (interactive
    (let* ((use-dialog-box nil)
           (name (read-file-name "Move to (directory or file name): "
                                 ))
           )
      (list (expand-file-name name))))
  (html-site-current-ensure-site-defined)
  (let ((from (buffer-file-name))
        (site-directory (html-site-current-site-dir)))
    (unless from
      (error "No buffer file name, can't move file!"))
    (let* ((from-dir (file-name-directory from))
           (from-ext (file-name-extension from))
           to-dir
           to-ext
           new-name
           new-file
           new-buffer
           relative-path)
      (unless (html-move-in-dir-tree from-dir site-directory)
        (error "Buffer file is not in site directory tree"))
      (if (file-directory-p to)
          (progn
            (setq to-dir to)
            (setq new-name (file-name-nondirectory from))
            )
        (setq to-ext (file-name-extension to))
        (unless (string= to-ext from-ext)
          (if (not to-ext)
              (error "Can't find directory %s (or missing extension?)" to)
            (error "Move must not change file extension")))
        (setq to-dir (file-name-directory to))
        (unless (file-directory-p to-dir)
          (if (file-exists-p to-dir)
              (error "Not a directory: %s" to-dir)
            (error "Can't find directory %s" to-dir)))
        (setq new-name (file-name-nondirectory to))
        )

      (unless (html-move-in-dir-tree to-dir site-directory)
        (error "Target is not in site directory tree"))


      (setq relative-path (file-relative-name to-dir from-dir))
      (when (file-name-absolute-p relative-path)
        (error "Can't make a relative path from %s to %s" from to))
      (setq new-file (expand-file-name new-name to))
      (let ((moved-buffer (current-buffer))
            (moved-contents (buffer-substring-no-properties
                             (point-min)
                             (point-max))))
        (when (file-exists-p new-file)
          (error "File already exists: %s" new-file))
        ;; Open in new location
        (find-file new-file)
        (setq new-buffer (current-buffer))
        (erase-buffer)
        (insert moved-contents)
        (goto-char (point-min))
        (while (re-search-forward "\\(?:href\\|src\\)\\s-*=\\s-*\"\\([^\"]*\\)\"" nil t)
          (let ((old-url (match-string 1))
                (new-url))
            (unless (or (> 11 (length old-url))
                        (string= "javascript:"
                                 (downcase (substring old-url 0 11))))
              (setq new-url (html-move-make-new-url old-url from-dir to-dir))
              (when new-url
                (replace-match new-url t t nil 1)))))
        (save-buffer)
        (html-move-fix-site-backlinks from to-dir from-dir)
        ;; Make backup at current location of "from" file
        (with-current-buffer moved-buffer
          (set-buffer-modified-p t)
          (save-buffer))
        (kill-buffer moved-buffer)
        ;; Delete moved
        (delete-file from))
      (set-buffer new-buffer)
      (goto-char (point-min))
      (lwarn '(html-move) :warning "Moved to %s" new-file)
      )))

(defun html-move-fix-site-backlinks (to-moved-file to-dir from-dir)
  "Fix all links back to TO-MOVED-FILE.
This is called by `html-move-buffer-file' to fix all links back
to the moved file.  TO-MOVED-FILE is the old location of the
moved file. FROM-DIR is the old directory and TO-DIR the target
directory for the move."
  (html-move-fix-all-backlinks to-moved-file (html-site-current-site-dir) to-dir from-dir)
  (when (html-move-fix-page-list to-moved-file to-dir from-dir)
    (message "Page list for site TOC changed. You need to update TOC.")
    (lwarn '(html-move-fix-site-backlinks) :warning "Page list for site TOC changed. You need to update TOC.")
    ))

(defun html-move-fix-all-backlinks (to-moved-file for-dir to-dir from-dir)
  ;;(message "for-dir=%s" for-dir);(sit-for 2)
  (let ((html-files (directory-files                for-dir t ".*\\.html?$"))
        (sub-dirs   (directory-files-and-attributes for-dir t)))
    (dolist (html-file html-files)
      (html-move-fix-backlinks to-moved-file html-file to-dir from-dir))
    (dolist (sub-entry sub-dirs)
      (let* ((sub-dir (car sub-entry))
             (sub-name (file-name-nondirectory sub-dir)))
        (when (and (eq t (car (cdr sub-entry)))
                   (not (string= "." sub-name))
                   (not (string= ".." sub-name)))
          (html-move-fix-all-backlinks to-moved-file sub-dir to-dir from-dir))))))

(defun html-move-fix-backlinks (to-moved-file for-file to-dir from-dir)
  (when (file-exists-p for-file)
    (let ((old-file-buffer (get-file-buffer for-file))
          (buffer (find-file-noselect for-file)))
      (with-current-buffer buffer
        (goto-char (point-min))
        (while
            (re-search-forward
             "\\(?:href\\|src\\)\\s-*=\\s-*\"\\([^#\"]*\\)\\(?:#[^\"]*\\|\\)\""
             nil t)
          (let* ((old-url (match-string 1))
               (old-absolute-url (expand-file-name
                                  old-url
                                  (file-name-directory for-file)))
               new-url)
            (when (string= old-absolute-url to-moved-file)
              (setq new-url (html-move-make-new-url old-url to-dir from-dir))
              ;;(message "new-backlink=%s" new-url);(sit-for 2)
              (replace-match new-url t t nil 1)
              )))
        (save-buffer)
        (unless old-file-buffer
          (kill-this-buffer))))))

(defun html-move-fix-page-list (to-moved-file to-dir from-dir)
  (let ((for-file (html-site-current-page-list))
        some-change)
    (when (file-exists-p for-file)
      (let ((old-file-buffer (get-file-buffer for-file))
            (buffer (find-file-noselect for-file)))
        (with-current-buffer buffer
          (goto-char (point-min))
          (while
              (re-search-forward
               ;;"\\(?:href\\|src\\)\\s-*=\\s-*\"\\([^#\"]*\\)\\(?:#[^\"]*\\|\\)\""
               "\\s-+###\\s-+\\([^#]*?\\)\\(?:#[^#]*\\|\\)[:space:]*$"
               nil t)
            (let* ((old-url (match-string 1))
                   (old-absolute-url (expand-file-name
                                      old-url
                                      (file-name-directory for-file)))
                   new-url)
              (when (string= old-absolute-url to-moved-file)
                (setq new-url (html-move-make-new-url old-url to-dir from-dir))
                ;;(message "new-backlink=%s" new-url);(sit-for 2)
                (replace-match new-url t t nil 1)
                (setq some-change t)
                )))
          (save-buffer)
          (unless old-file-buffer
            (kill-this-buffer)))))
    some-change))

(provide 'html-move)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; html-move.el ends here
