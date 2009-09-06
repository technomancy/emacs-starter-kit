;;; html-wtoc.el --- Creating pages with site TOC

;; Copyright (C) 2005 by Lennart Borgman

;; Author: Lennart Borgman <lennartDOTborgmanDOT073ATstudentDOTluDOTse>
;; Created: Sat Feb 11 00:06:14 2006
(defconst html-wtoc:version "0.2") ;; Version:
;; Last-Updated: Sun Nov 04 21:49:34 2007 (3600 +0100)
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
(eval-when-compile (require 'html-toc))

(defgroup html-wtoc nil
  "Customization group for html-wtoc."
  :group 'nxhtml)

(defcustom html-wtoc-dir
  (file-name-as-directory
   (expand-file-name
    "html-wtoc"
    (file-name-directory
     (if load-file-name load-file-name buffer-file-name))))

  "Directory where the tools needed are located.
The tools for html-wtoc includes:

- html-wtoc.pl
- html-wtoc.js
- html-wtoc.css
- html-wtoc-template.htm
- html-wtoc-template.js
- html-wtoc-template.css
- img/

"
  :type 'directory
  :group 'html-wtoc)

;; (defun html-wtoc-get-parsed-html-toc ()
;;   (save-excursion
;;     (let ((toc-file (html-toc-file)))
;;       (unless (file-exists-p toc-file)
;;         (html-toc-write-toc-file))
;;       (with-current-buffer (find-file-noselect toc-file)
;;         (goto-char (point-min))
;;         (let ((toc-begin  (search-forward html-toc-mark-begin nil t))
;;               (toc-middle (search-forward html-toc-mark-middle nil t))
;;               toc-parsed)
;;           (unless (and toc-begin toc-middle)
;;             (error "Can't find table of contents in %s" toc-file))
;;           (setq toc-parsed (html-toc-parse-toc
;;                             (buffer-substring-no-properties
;;                              toc-begin toc-middle))))))))

;; (defun html-wtoc-get-atags (parsed-ul level)
;;   (assert (eq 'ul (car parsed-ul)))
;;   (let (atags)
;;     (dolist (l parsed-ul)
;;       (when (and (listp l)
;;                  (eq 'li (car l)))
;;         (dolist (ll l)
;;           (when (listp ll)
;;             (when (eq 'a (car ll))
;;               (setq atags
;;                     (cons
;;                      (list level
;;                            (caddr ll)
;;                            (cdaadr ll))
;;                      atags)))
;;             (when (eq 'ul (car ll))
;;               (let ((subs (html-wtoc-get-atags ll (1+ level))))
;;                 (dolist (s subs)
;;                   (setq atags (cons s atags)))))))))
;;     (reverse atags)))

;; (defcustom html-wtoc-pages-default-name "html-wtoc-pages.txt"
;;   "Default file name sans directory for list of pages file.
;; This file is located in the same directory as `html-toc-file'."
;;   :type 'string)

;; (defun html-wtoc-pages-file ()
;;   (expand-file-name html-wtoc-pages-default-name
;;                     (file-name-directory (html-toc-file))))

(defun html-wtoc-browse-page-with-toc ()
  (interactive)
  (unless buffer-file-name
    (error "This buffer is not visiting a file"))
  (html-site-current-ensure-site-defined)
  (let ((merge-dir (html-site-current-merge-dir))
        merged-file
        (in-site (html-site-dir-contains
                  (html-site-current-site-dir)
                  buffer-file-name)))
    (unless merge-dir
      (error "There is no output dir for pages with TOC defined for the site %s"
             html-site-current))
    (unless in-site
      (error "This buffer's file is not in %s" (html-site-current-site-dir)))
    (setq merged-file
          (expand-file-name
           (file-relative-name buffer-file-name
                               (html-site-current-site-dir))
           (html-site-current-merge-dir)))
    (unless (file-exists-p merged-file)
      (error "The file %s does not yet exist.\nPlease do use `html-wtoc-write-merged' to create it."
             merged-file))
    (browse-url-of-file merged-file)))


(defun html-wtoc-write-pages-with-toc (allow-overwrite)
  "Merge the TOC with the pages.

If an entry with the name MERGE-NAME exists in `html-wtoc-merges'
then this is chosen.  Otherwise a new entry is created and added
to `html-wtoc-merges'.  The entry has all necessary information to
do the merge.

If `html-move-site-directory' has a non-nil value then the list
of completions when prompting for MERGE-NAME contains only those
merge names from `html-wtoc-merges' where the site directory has
the same value.  Otherwise the completion list contains all merge
names and `html-move-site-directory' will be set to the chosen
merge's site directory.

The merging of the pages and the table of contents is done in a
subprocess using a Perl script named html-wtoc.pl the directory
`html-wtoc-dir'.
"
  (interactive (list (y-or-n-p "Allow overwrite? ")))
  (html-site-current-ensure-site-defined)
  (let ((pag-file (html-site-current-page-list))
        (out-dir  (html-site-current-merge-dir))
        (tpl-file (html-site-current-merge-template))
        (html-wtoc-pl (expand-file-name "html-wtoc.pl" html-wtoc-dir))
        )
    (unless (< 0 (length pag-file))
      (error "Page list file not defined for site %s" html-site-current))
    (unless (file-exists-p pag-file)
      (error "Can't find page file for site %s.\nHave you done M-x html-toc-create-pages-file?"
             html-site-current))
    (unless (< 0 (length tpl-file))
      ;;(error "Template file not defined for site %s.\nPlease use customize to add this in `html-site-list'." html-site-current)
      (setq tpl-file (expand-file-name "html-wtoc-template.html" html-wtoc-dir))
      )
    (let (
          (buffer (noshell-procbuf-setup "*Merging pages and TOC*"))
          (opt (list
                (concat "pages=" pag-file)
                (concat "outroot=" out-dir)
                (concat "template=" tpl-file))))
      (when allow-overwrite
        (setq opt (cons "update=1" opt)))
      (apply 'noshell-procbuf-run
             buffer
             "perl" "-w"
             html-wtoc-pl "merge"
             opt
             ))))

(provide 'html-wtoc)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; html-wtoc.el ends here
