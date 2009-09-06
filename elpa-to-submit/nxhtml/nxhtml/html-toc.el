;;; html-toc.el --- Building and updating TOC for a site

;; Copyright (C) 2005, 2006, 2007 Lennart Borgman

;; Author: Lennart Borgman <lennartDOTborgmanDOT073ATstudentDOTluDOTse>
;; Created: Wed Feb 01 14:40:13 2006
(defconst html-toc:version "0.4");; Version:
;; Last-Updated: Tue Apr 10 04:09:29 2007 (7200 +0200)
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
;;  Create table of contents for a static web site.  See
;;  `html-toc-write-toc-file' and `html-toc-write-frames-file' for
;;  more info.
;;
;;  To use this you can add (require 'html-toc) to your .emacs.
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

(eval-when-compile (require 'cl))
(eval-when-compile (add-to-list 'load-path default-directory load-path))
(eval-when-compile (require 'fupd))
;;(require 'html-move)
(eval-when-compile (require 'html-site))
;;(require 'dom)
(require 'xml)

(defconst html-toc-mark-begin  "<!-- html-toc START -->")
(defconst html-toc-mark-middle "<!-- html-toc MIDDLE -->")
(defconst html-toc-mark-end    "<!-- html-toc END -->")

(defun html-toc-create-pages-file ()
  "Write a list of pages to be used for table of contents.
Return the file name."
  (interactive)
  (html-site-current-ensure-site-defined)
  (let* (
         (site-dir (html-site-current-site-dir))
         (page-file (html-site-current-page-list))
         (page-file-dir (file-name-directory page-file))
         (page-file-exists (file-exists-p page-file))
         (sub-files (html-site-get-sub-files
                     site-dir
                     html-site-files-re))
         (pages-text)
         )
    (setq sub-files
          (sort (mapcar (lambda (full-file)
                          (assert (file-exists-p full-file))
                          (file-relative-name full-file page-file-dir))
                        sub-files)
                'string<))
    ;;(setq sub-files (delete html-toc-file-default-name sub-files))
    (with-temp-buffer
      (let ((this-level)
            (dir-title)
            (title)
            (full-file))
        (dolist (file sub-files)
          (setq full-file (expand-file-name file page-file-dir))
          (setq dir-title (file-name-nondirectory
                           (substring (file-name-directory full-file) 0 -1)))
          (setq title (html-toc-get-title full-file))
          (setq this-level 0)
          (mapc (lambda (c) (when (eq c ?/) (setq this-level (1+ this-level)))) file)
          (insert (format "%s ### %s ### %s\n" this-level title file))))
      (setq pages-text (buffer-string)))
    (with-current-buffer (find-file page-file)
      (if (string= pages-text (buffer-string))
          (message "List of pages is already the default list")
        (if (= 0 (length (buffer-string)))
            (progn
              (insert pages-text)
              (save-buffer)
              )
          (if (y-or-n-p "Replace old list of pages? ")
              (progn
                (erase-buffer)
                (insert pages-text)
                (save-buffer)
                )
            (message "Keeping old list of pages.")))))
    page-file))
(defun html-toc-dir ()
  (let* ((this-file (if load-file-name
                       load-file-name
                      buffer-file-name))
         (this-dir (file-name-directory this-file))
         )
    (expand-file-name "html-toc" this-dir)))

(defgroup html-toc nil
  "Customization group for html-toc."
  :group 'nxhtml)

(defcustom html-toc-template-file
  (expand-file-name "html-toc-template.html" (html-toc-dir))
  "Template file for table of contents file."
  :type 'file
  :group 'html-toc)


(defun html-toc-write-toc-file ()
  "Write a table of contents for a web site.
Build the table of content from the information in
`html-site-current-page-list'.  Write it to the file
`html-site-current-toc-file' and return that file name.

When viewed in a browser the table of contents can be
expanded/collapsed (if JavaScript is allowed)."
  (interactive)
  (html-site-current-ensure-site-defined)
  (let* ((toc-file (html-site-current-toc-file))
         (page-file (html-site-current-page-list))
         page-lines
         toc)
    (unless (< 0 (length toc-file))
      (error "There is no name for the table of content file in site \"%s\""
             html-site-current))
    (unless (< 0 (length page-file))
      (error "There is no name for the pages file in site \"%s\""
             html-site-current))
      (with-temp-buffer
        (insert-file-contents page-file)
        (goto-char (point-min))
        (while (not (eobp))
          (let* ((line (buffer-substring (point) (line-end-position)))
                 (line-parts (split-string line "\\s-+###\\s-+")))
            (setq page-lines (cons line-parts page-lines)))
          (forward-line)))
      (setq page-lines (reverse page-lines))
      (with-temp-buffer
        (html-toc-insert-toc page-lines toc-file)
        (setq toc (buffer-substring-no-properties (point-min) (point-max))))
      (with-current-buffer (find-file-noselect toc-file)
        (erase-buffer)
        (insert-file-contents-literally html-toc-template-file)
        (let (toc-start)
          (while (search-forward "%%TOC%%" nil t)
            (unless toc-start
              (setq toc-start (match-beginning 0)))
            (replace-match toc t t))
          (forward-line) ;; for indentation
          (indent-region toc-start (point-marker)))
        (goto-char (point-min))
        (save-buffer))
      toc-file))


(defun html-toc-insert-toc (page-lines toc-file)
  (let* ((curr-level)
         (min-level 100)
         div-levels
         (site-directory (html-site-current-site-dir))
         (toc-rel-file (file-relative-name toc-file site-directory)))
    (dolist (line page-lines)
      (let ((level (string-to-number (nth 0 line))))
        (when (< level min-level)
          (setq min-level level))))
    (setq curr-level min-level)
    (while page-lines
      (let* ((line (car page-lines))
             (file (nth 2 line))
             (title (nth 1 line))
             (this-level (string-to-number (nth 0 line)))
             (next-level (progn
                          ;; Note:
                           (setq page-lines (cdr page-lines))
                           (let ((next-line (car page-lines)))
                             (when next-line
                               (string-to-number (nth 0 next-line))))))
             (full-file (expand-file-name file site-directory))
             (dir-title (file-name-nondirectory
                         (substring (file-name-directory full-file) 0 -1))))
        ;;(insert "<!-- " (format "%s, %s, %s" curr-level this-level div-levels) " -->\n")
        ;; Don't insert a link to the toc file
        (unless (string= toc-rel-file file)
          ;; If there are childs then insert a <div> before them. Save
          ;; the level so we can close the div-tag later.
          (when (< curr-level this-level)
            ;; Save level so we can find the end of the <div>.
            (setq div-levels (cons this-level div-levels))
            (insert "<div class=\"html-toc-childs\">\n"))
          ;; Close div-tags if this level is lower when the previous.
          (when (> curr-level this-level)
            (while (and div-levels
                        (> (car div-levels) this-level))
              (insert "</div>\n")
              (setq div-levels (cdr div-levels))))
          (setq curr-level this-level)
          (insert "<div class=\"html-toc-link\">"
                  "<span style=\"display:table-cell; width:15em; background-color:yellow;\">"
                  "<a style=\"padding-left:" (number-to-string (1+ (- curr-level min-level))) "em;\" "
                  (format "href=\"%s\">%s</a>" file title)
                  "</span>")
          (when (and next-level (> next-level this-level))
            (insert "<span onclick=\"html_toc_hs(this)\" class=\"html-toc-hs\""
                    " style=\"display:table-cell; background-color:white;\">HS</span>"))
          (insert "</div>"
                  "\n")
          )))
    (while div-levels
      (insert "</div>\n")
      (setq div-levels (cdr div-levels)))))

(defun html-toc-get-title (file)
  (save-excursion
    (with-temp-buffer
      (insert-file-contents file nil 0 1000)
      (goto-char (point-min))
      (when (search-forward-regexp "<title>\\(.*\\)</title>" nil t)
        (match-string 1)))))

(defun html-toc-parse-toc (toc-str)
  (let ((nodes))
    (with-temp-buffer
      (insert toc-str)
      (setq nodes (xml-parse-region (point-min) (point-max))))
    ))

(defun html-toc-get-hrefs (nodes)
  (let ((atags (html-toc-get-atags nodes)))
    (mapcar (lambda (atag)
              (xml-get-attribute atag 'href))
            atags)))
(defun html-toc-get-atags (nodes)
  (let ((atags))
    (dolist (node nodes)
      (when (listp node)
        (setq atags (append atags (xml-get-children node 'a)))
        (setq atags (append atags (html-toc-get-atags (xml-node-children node))))))
    atags))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Frames and viewing
(defcustom html-toc-frames-default-name "html-toc-frames.html"
  "Default file name sans directory for frames file."
  :type 'string
  :group 'html-toc)

(defvar html-toc-frames-contents
    "<?xml version=\"1.0\" encoding=\"iso-8859-1\"?>
<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Frameset//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-frameset.dtd\">
<html xmlns=\"http://www.w3.org/1999/xhtml\">
  <head>
    <title>Frames for html-toc</title>
  </head>
  <frameset cols=\"20%, 80%\">
    <frame name=\"html-toc-TOC\" src=\"%%TOCFILE%%\"/>
    <frame name=\"html-toc-Document\" />
    <noframes>
      <body>
        Html frame support required
      </body>
    </noframes>
  </frameset>
</html>
")

(defun html-toc-browse-frames-file ()
  "View frames file written by `html-toc-write-frames-file'."
  (interactive)
  (html-site-current-ensure-site-defined)
  (let ((frames-file (html-site-current-frames-file)))
    (unless (< 0 (length frames-file))
      (error "There is no frames file set for site \"%s\"" html-site-current))
    ;;(message "frames-file=%s" frames-file)(sit-for 4)
    (unless (file-exists-p frames-file)
      (html-toc-write-frames-file))
    (browse-url-of-file frames-file)))

;; (defun html-toc-frames-file-name ()
;;   "Return name of file written by `html-toc-write-frames-file'."
;;   (html-toc-get-site)
;;   (expand-file-name html-toc-frames-default-name html-move-site-directory))

(defun html-toc-write-frames-file ()
  "Write a frames file.
This frames file should load the table of contents build by
`html-toc-write-toc-file' in one frame and shows the documents in
another.

The contents of the frames file is defined by
`html-toc-frames-contents'.

Returns the file name of the written or existing frames file.

You may also want to look at `html-wtoc-write-pages-with-toc'."
  (interactive)
  ;;(html-toc-get-site)
  (html-site-current-ensure-site-defined)
  (let* ((frames-file (html-site-current-frames-file))
         (frames-cont html-toc-frames-contents)
         (toc-file (html-toc-write-toc-file))
         toc-file-relative)
    (when toc-file
      (setq toc-file-relative (file-relative-name
                               toc-file
                               (file-name-directory frames-file)))
      (save-match-data
        (unless (string-match "%%TOCFILE%%" frames-cont)
          (error "Can't find %%TOCFILE%% in html-toc-frames-contents"))
        (setq frames-cont (replace-match toc-file-relative t t frames-cont)))
      (with-current-buffer (find-file-noselect frames-file)
        (erase-buffer)
        (insert frames-cont)
        (save-buffer))
      frames-file)))

;;;###autoload
(defconst html-toc-menu-map
  (let ((map (make-sparse-keymap)))
    (define-key map [html-toc-browse-frames-file]
      (list 'menu-item "Browse Frames File" 'html-toc-browse-frames-file))
    (define-key map [html-toc-write-frames-file]
      (list 'menu-item "Write Frames File" 'html-toc-write-frames-file))
    (define-key map [html-toc-write-toc-file]
      (list 'menu-item "Write TOC File for Frames" 'html-toc-write-toc-file))
    (define-key map [html-toc-sep1] (list 'menu-item "--"))
    (define-key map [html-toc-edit-pages-file]
      (list 'menu-item "Edit List of Pages for TOC" 'html-site-edit-pages-file))
    (define-key map [html-toc-create-pages-file]
      (list 'menu-item "Write List of Pages for TOC" 'html-toc-create-pages-file))
    map))



(provide 'html-toc)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; html-toc.el ends here
