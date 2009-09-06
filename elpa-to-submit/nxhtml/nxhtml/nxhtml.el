;;; nxhtml.el --- Keeping nXhtml together
;;
;; Author: Lennart Borgman (lennart O borgman A gmail O com)
;; Created: 2009-01-01 Thu
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

(defgroup nxhtml nil
  "Customization of `nxhtml-mode'."
  :group 'nxml)

;;;###autoload
(defun nxhtml-customize ()
  "Customize nXhtml."
  (interactive)
  (customize-group 'nxhtml))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Features

;; Fix-me: add help links
(defvar nxhtml-req-features
  (let ((req-features
         '(
           "XHTML/HTML"
           (nxml-mode    "XML Completion" "nxml-mode.el")
           (nxhtml       "Additional XHTML Completion" "nxhtml.el")
           (mlinks       "Live XHTML links" "mlinks.el" "0.28")
           (tidy-xhtml   "Run HTML tidy program" "tidy-xhtml.el" "2.24")
           (xhtml-help   "HTML+CSS help" "xhtml-help.el" "0.57")
           (nxml-where   "Shows XML path" "nxml-where.el" "0.52")
           (html-imenu   "Table of content in menus" "html-imenu.el" "0.9")
           (html-pagetoc "Page TOC" "html-pagetoc.el" "0.85")
           (html-site    "Web sites you define" "html-site.el" "0.2")
           (html-upl     "Upload web sites" "html-upl.el" "0.2")
           (html-chklnk  "Checking links in site" "html-chklnk.el" "0.2")
           (html-move    "Moving files in web sites" "html-move.el" "0.31")
           (html-toc     "Web site TOC" "html-toc.el" "0.4")
           (html-wtoc    "Merge pages and web Site TOC" "html-wtoc.el" "0.2")
           (html-write   "Show <i> as italic etc" "html-write.el" "0.6")
           "General"
           (mumamo       "Multiple major modes in buffer" "mumamo.el" "0.73")
           (majmodpri    "Major mode priorities" "majmodpri.el" "0.5")
           (tabkey2      "Tab completion" "tabkey2.el" "1.12")
           (fold-dwim    "Folding on headers and tags" "fold-dwim.el" "1.3")
           (appmenu      "General popup menu" "appmenu.el" "0.53")
           (appmenu-fold "Popup menu entries for folding" "appmenu-fold.el" "0.51" appmenu fold-dwim)
           (winsize      "Resizing and window handling" "winsize.el" "0.98")
           (winsav       "Save/restore for windows/frames" "winsav.el" "0.77")
           (viper-tut    "Viper try-out tutorial" "viper-tut.el" "0.2")
           (ourcomments-util "Some minor utilities" "ourcomments-util.el" "0.25")
           "External applications / Emacs as dito"
           (as-external  "Emacs as an external editor" "as-external.el" "0.5")
           (sex-mode     "Send to EXternal program" "sex-mode.el" "0.71")
           (freemind     "Export/import freemind maps" "freemind.el" "0.60")
           (hfyview      "Print with browser/copy to html" "hfyview.el" "0.63")
           (mozadd       "Mirroring in Firefox" "mozadd.el" "0.2")
           "Images and Colors"
           (gimp         "Edit images with GIMP" "gimp.el" "0.2")
           (inlimg       "Inline images" "inlimg.el" "0.7")
	   (css-color 	 "Css color help functions" "css-color.el" "0.02")
           (chart        "Easy google charts" "chart.el" "0.2")
           "Fetching and using elisp from repositories"
           (udev         "Fetch and from elisp repostories" "udev.el" "0.5")
           (udev-cedet   "CEDET fetcher and loader" "udev-cedet.el" "0.2")
           (udev-ecb     "ECB fetcher and loader" "udev-ecb.el" "0.2")
           (udev-rinari  "Rinari fetcher and loader" "udev-rinari.el" "0.2")
           "Games and life"
           (pause        "Take a break! I wish you some fun!" "pause.el" "0.64")
           (n-back       "n-back game for fun and brain" "n-back.el" "0.5")
           )
         ))
    req-features))

(defun nxhtml-load-req-features ()
  (dolist (extf nxhtml-req-features)
    (unless (or (stringp extf)
                (eq (car extf) 'nxhtml))
      (require (car extf) nil t))))



(defun nxhtml-make-library-link (beg end)
  (let ((library (buffer-substring-no-properties beg end)))
    (make-text-button beg end
                      'action (lambda (button)
                                (find-library
                                 (button-get button 'lib-name)))
                      'lib-name library
                      'face 'button)))

(defun nxhtml-feature-insert (ok msg)
  (put-text-property 0 (length msg)
                     'face (if ok font-lock-type-face font-lock-warning-face)
                     msg)
  (insert msg))

(defun nxhtml-feature-check (feat-entry silent)
  (require 'loadhist)
  (let ((feature     (nth 0 feat-entry))
        (description (nth 1 feat-entry))
        (file        (nth 2 feat-entry))
        (need-ver    (nth 3 feat-entry))
        (need-list   (cddddr feat-entry))
        (ok))
    (if (featurep feature)
        (let* (
               (feat-versym (read (format "%s:version" feature)))
               (feat-ver (condition-case err
                             (symbol-value feat-versym)
                           (error nil)))
               (feat-vok (or (not need-ver)
                             (and feat-ver
                                  (version<= need-ver feat-ver))))
               (need-ok (or (not need-list)
                            (let ((has t))
                              (dolist (n need-list)
                                (unless (featurep n)
                                  (setq has nil)))
                              has))))
          (setq ok (and feat-vok need-ok))
          (unless silent
            (nxhtml-feature-insert
             ok
             (concat (format "%34s -- " description)
                     (if ok
                         (format "supported by %s%s\n"
                                 file
                                 (if (not need-ver)
                                     ""
                                   (if (string= feat-ver need-ver)
                                       (format " (%s)" feat-ver)
                                     (format " (%s/%s)" feat-ver need-ver))))
                       (concat "found " file
                               " but needs"
                               (if feat-vok ""
                                 (format " version %s" need-ver))
                               (if (or feat-vok need-ok) "" " and")
                               (if need-ok ""
                                 (format " also %s" need-list))
                               "\n"))))
            (unless (string= (file-name-sans-extension file)
                             (file-name-sans-extension
                              (file-name-nondirectory (feature-file feature))))
              (insert (make-string (+ 34 4) ?\ ) "** Bad file name: " file "\n"))))
      (unless silent
        (nxhtml-feature-insert
         nil (format "%34s -- support missing, can't find %s\n"
                     description file))))
    ok))

;; Fix-me: move help here from `nxhtml-mode'?

;;;###autoload
(defun nxhtml-features-check ()
  "Check if external modules used by nXhtml are found."
  (interactive)
  (with-output-to-temp-buffer (help-buffer)
    (help-setup-xref (list #'nxhtml-features-check) (interactive-p))
    (with-current-buffer (help-buffer)
      (nxhtml-minor-mode 1)
      (erase-buffer)
      (let ((s (concat "Elisp modules used by nXhtml version " nxhtml-menu:version ":")))
        (put-text-property 0 (length s)
                           'face '( :weight bold :height 1.4)
                           s)
        (insert s "\n\n"))
      (nxhtml-load-req-features)
      (nxhtml-load-req-features)
      (nxhtml-load-req-features)
      (nxhtml-load-req-features)
      (dolist (feat-entry nxhtml-req-features)
        (if (stringp feat-entry)
            (insert "==== " (propertize feat-entry 'face 'font-lock-comment-face 'face '(:weight bold)) "\n")
          (nxhtml-feature-check feat-entry nil)))
      (goto-char (point-min))
      (while (search-forward-regexp "[-a-zA-Z0-9]+\\.el" nil t)
        (nxhtml-make-library-link
         (match-beginning 0)
         (match-end 0)))
      (goto-char (point-min)))
    (set-buffer-modified-p nil)))

(defun nxhtml-all-features-found ()
  (let ((all t))
    (dolist (feat-entry nxhtml-req-features)
      ;;(unless (featurep (car extf))
      (unless (stringp feat-entry)
        (unless (nxhtml-feature-check feat-entry t)
          (setq all nil))))
    all))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Link saving and pasting

(defun nxhtml-find-base-href ()
  "Return base href found in the current file."
  (let ((base-href))
    (save-excursion
      (goto-char (point-min))
      (while (and (not base-href)
                  (search-forward-regexp "<!--[^!]*-->\\|<base[[:space:]]" nil t))
        (when (equal " " (char-to-string (char-before)))
          (backward-char 6)
          (when (looking-at "<base [^>]*href *= *\"\\(.*?\\)\"")
            (setq base-href (match-string-no-properties 1))))))
    base-href))


(defvar nxhtml-saved-link-file nil
  "Saved buffer file name for use in `nxhtml-paste-link'.")
(defvar nxhtml-saved-link-anchor nil
  "Saved anchor name for use in `nxhtml-paste-link'.")

;; Fix-me: same line???
(defun nxhtml-save-link-to-here ()
  "Save buffer file name+anchor for `nxhtml-paste-link'."
  (interactive)
  (if (not buffer-file-name)
      (message "Current buffer has no file name")
    (setq nxhtml-saved-link-file (buffer-file-name))
    (setq nxhtml-saved-link-anchor nil)
    (save-excursion
      (let ((here (point)))
        (while (not (or (bolp) (looking-at "\\(?:id\\|name\\)[[:space:]]*=[[:space:]]*\".*?\"")))
          (backward-char))
        (when (and (looking-at "\\(?:id\\|name\\)[[:space:]]*=[[:space:]]*\"\\(.*?\\)\"")
                   (<= (match-beginning 0) here)
                   (< here (match-end 0)))
          (setq nxhtml-saved-link-anchor (match-string-no-properties 1)))))
    (message "Saved link: %s%s" nxhtml-saved-link-file
             (if nxhtml-saved-link-anchor
                 (concat "#" nxhtml-saved-link-anchor)
               ""))))

(defun nxhtml-paste-link-as-a-tag ()
  "Paste link saved by `nxhtml-save-link-to-here' as an <a> tag.
Takes into account the relative position of the saved link."
  (interactive)
  (let ((paste-text (nxhtml-get-saved-link)))
    (when paste-text
      (let ((link-text (read-string "Link text: ")))
        (insert "<a href=\"" paste-text "\">" link-text "</a>")))))

(defun nxhtml-paste-link ()
  "Paste link saved by `nxhtml-save-link-to-here'.
Takes into account the relative position of the saved link."
  (interactive)
  (let ((paste-text (nxhtml-get-saved-link)))
    (when paste-text
      (insert paste-text))))

(defun nxhtml-get-saved-link ()
  (if nxhtml-saved-link-file
      (let* (
             (base-href (nxhtml-find-base-href))
             (rel (file-relative-name nxhtml-saved-link-file
                                      (if base-href
                                          base-href
                                        (file-name-directory (buffer-file-name)))))
             (to-file (file-name-nondirectory (buffer-file-name)))
             (anchor nxhtml-saved-link-anchor)
             )
        (when (equal to-file rel) (setq rel ""))
        (when anchor (setq rel (concat rel "#" anchor)))
        rel)
    (message "There is no saved link")
    nil))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Misc

(defun nxhtml-update-mark-today (date-str)
  "Update marks for today's date.
The mark has this form

  <!-- today -->zzz<!-- end today -->"
  (interactive (list (format-time-string "%Y-%m-%d")))
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward (rx
                               "<!-- today -->"
                               (submatch (0+ anything))
                               "<!-- end today -->")
                              nil t)
      (replace-match date-str nil nil nil 1))))


(provide 'nxhtml)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; nxhtml.el ends here
