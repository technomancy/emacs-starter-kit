;;; nxhtml.el --- Edit XHTML files

;; Copyright (C) 2005-2008 by Lennart Borgman
;; Parts are from Peter Heslin (see below)

;; Author:  Lennart Borgman <lennart DOT borgman DOT 073 AT student DOT lu DOT se>
;; Created: 2005-08-05
;;(defconst nxhtml:version "1.45") ;;Version:
;; Last-Updated: 2008-09-30T11:46:26+0200 Tue
;; Keywords: languages
;;
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;  The purpose of nxhtml.el is to add some features that are useful
;;  when editing XHTML files to nxml-mode.  For more information see
;;  `nxhtml-mode'.
;;
;;
;;  Usage:
;;
;;  See the file readme.txt in the directory above this file. Or, if
;;  you do not have that follow the instructions below.
;;
;;  Put this file in `load-path'. In your .emacs:
;;
;;     ;; Load nxml according to the instructions, ie something like:
;;     (load "your-path/nxml-mode-20041004/rng-auto.el")
;;
;;     ;; Then autoload nxhtml-mode:
;;     (autoload 'nxhtml-mode "nxhtml" "Mode for editing XHTML files - based on nxml-mode." t)
;;
;;     ;; For file associations you can use:
;;     (majmodpri-apply-priorities t)
;;
;;
;;  Tip: Why not put all these in a .nxml file that you load in your
;;  .emacs?


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; History:
;;
;; 2006-04-25: Added completion for href, src etc. Removed xhtmlin.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This file is not part of Emacs
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(eval-when-compile
  (require 'cl)
  (unless (featurep 'nxhtml-autostart)
    (let ((efn (expand-file-name
                "../autostart.el"
                (file-name-directory
                 (if load-file-name
                     load-file-name
                   (buffer-file-name))))))
      (load efn))
    (require 'rng-valid)
    (require 'rng-nxml)
    (require 'html-toc nil t)
    (require 'html-pagetoc nil t)))

(require 'button)
(require 'loadhist)
(require 'nxml-mode)
(require 'url-parse)
(require 'url-expand)
(require 'popcmp)
(require 'rngalt)
(require 'html-imenu)
(require 'fold-dwim)
(require 'tidy-xhtml)
;;(require 'nxhtml-menu)
(require 'html-quote)
;;(require 'nxhtml-mumamo)

(defun nxhtml-version ()
  "Show nxthml version."
  (interactive)
  (message "nXhtml mode version %s" nxhtml-menu:version))

(defgroup nxhtml nil
  "Customization of nxhtml-mode."
  :group 'nxml)

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
           "General"
           (mumamo       "Multiple major modes in buffer" "mumamo.el" "0.73")
           (majmodpri    "Major mode priorities" "majmodpri.el" "0.5")
           (tabkey2      "Tab completion" "tabkey2.el" "1.12")
           (fold-dwim    "Folding on headers and tags" "fold-dwim.el" "1.3")
           (appmenu      "General popup menu" "appmenu.el" "0.53")
           (appmenu-fold "Popup menu entries for folding" "appmenu-fold.el" "0.51" appmenu fold-dwim)
           "External applications / Emacs as dito"
           (as-external  "Emacs as an external editor" "as-external.el" "0.5")
           (sex-mode     "Send to EXternal program" "sex-mode.el" "0.71")
           "Images and Colors"
           (gimp         "Edit images with GIMP" "gimp.el" "0.2")
           (hexcolor     "Hex color help functions" "hexcolor.el" "0.51")
           "Fetching and using elisp from repositories"
           (udev         "Fetch and load from elisp repostories" "udev.el" "0.5")
           (udev-cedet   "CEDET fetcher and loader" "udev-cedet.el" "0.2")
           (udev-ecb     "ECB fetcher and loader" "udev-ecb.el" "0.2")
           (udev-rinari  "Rinari fetcher and loader" "udev-rinari.el" "0.2")
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
                     'face (list
                            (cons 'foreground-color
                                  (if ok "RGB:00/cc/00"
                                    "RGB:cc/00/00")))
                     msg)
  (insert msg))

(defun nxhtml-feature-check (feat-entry silent)
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

(defun nxhtml-features-check ()
  "Check if external modules used by nXhtml are found."
  (interactive)
  (switch-to-buffer-other-window (get-buffer-create "*nXhtml Optional Features Check*") t)
  (help-mode)
  (setq buffer-read-only t)
  (let ((inhibit-read-only t))
    (erase-buffer)
    (let ((s (concat "Elisp modules used by nXhtml version " nxhtml-menu:version ":")))
      (put-text-property 0 (length s)
                         'face '( :weight bold :height 1.4)
                         s)
      (insert s "\n\n"))
    ;;(nxhtml-load-req-features)
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
  (set-buffer-modified-p nil))

(defun nxhtml-all-features-found ()
  (let ((all t))
    (dolist (feat-entry nxhtml-req-features)
      ;;(unless (featurep (car extf))
      (unless (stringp feat-entry)
        (unless (nxhtml-feature-check feat-entry t)
          (setq all nil))))
    all))

;;(defun nxhtml-nxml-fontify-attribute (att &optional namespace-declaration)
;;"Holds the original `nxml-fontify-attribute' function.")
;;(fset 'nxhtml-nxml-fontify-attribute (symbol-function 'nxml-fontify-attribute))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Folding etc. This part is taken from
;; http://www.emacswiki.org/cgi-bin/wiki/NxmlModeForXHTML and was
;; originally written by Peter Heslin. It requires fold-dwim.el.

(when (featurep 'fold-dwim)

  (defun nxhtml-setup-for-fold-dwim ()
    (make-local-variable 'outline-regexp)
    (setq outline-regexp "\\s *<\\([h][1-6]\\|html\\|body\\|head\\)\\b")
    (make-local-variable 'outline-level)
    (setq outline-level 'nxhtml-outline-level)
    (outline-minor-mode 1)
    (hs-minor-mode 1)
    (add-to-list 'hs-special-modes-alist
                 '(nxhtml-mode
                   "<!--\\|<[^/>]>\\|<[^/][^>]*[^/]>"
                   "</\\|-->"
                   "<!--" ;; won't work on its own; uses syntax table
                   (lambda (arg) (nxhtml-hs-forward-element))
                   nil))
    (when (featurep 'appmenu-fold)
      (appmenu-fold-setup))
    )

  (defun nxhtml-outline-level ()
    ;;(message "nxhtml-outline-level=%s" (buffer-substring (match-beginning 0) (match-end 0)))(sit-for 2)
    ;; Fix-me: What did I intend to do???
    (let ((tag (buffer-substring (match-beginning 1) (match-end 1))))
      (if (eq (length tag) 2)
          (- (aref tag 1) ?0)
        0))
    8)


  (defun nxhtml-hs-forward-element ()
    (let ((nxml-sexp-element-flag))
      (setq nxml-sexp-element-flag (not (looking-at "<!--")))
      (unless (looking-at outline-regexp)
        (condition-case nil
            (nxml-forward-balanced-item 1)
          (error nil)))))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


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


(defcustom nxhtml-use-imenu t
  "Use imenu in nxhtml-mode."
  :type 'boolean
  :group 'nxhtml)



(defcustom nxhtml-default-encoding 'iso-8859-1
  "Default encoding."
  :type 'coding-system
  :group 'nxhtml)

(defun nxhtml-insert-empty-frames-page ()
  "Insert an empty frames page."
  (interactive)
  ;;(unless (= 0 (buffer-size))
  (unless (nxhtml-can-insert-page-here)
    (error "Buffer is not empty"))
  (insert
   "<?xml version=\"1.0\" encoding=\""
   (symbol-name nxhtml-default-encoding)
   "\"?>
<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Frameset//EN\"
          \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-frameset.dtd\">
<html xmlns=\"http://www.w3.org/1999/xhtml\">
  <head>
    <title></title>
  </head>
  <frameset cols=\"50%, 50%\">
    <frame src=\"about:blank\" />
    <frame src=\"about:blank\" />
  </frameset>
</html>")
  (search-backward "</title>"))

(defun nxhtml-insert-empty-page ()
  "Insert an empty XHTML page."
  (interactive)
  ;;(unless (= 0 (buffer-size))
  (unless (nxhtml-can-insert-page-here)
    (error "Buffer is not empty"))
  (insert
   "<?xml version=\"1.0\" encoding=\""
   (symbol-name nxhtml-default-encoding)
   "\"?>
<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.1//EN\"
\"http://www.w3.org/TR/xhtml11/DTD/xhtml11.dtd\">
<html xmlns=\"http://www.w3.org/1999/xhtml\">
  <head>
    <title></title>
  </head>
  <body>
  </body>
</html>")
  (search-backward "</title>"))

(defun nxhtml-empty-page-completion ()
  ;;(unless (= 0 (buffer-size)) (error "Buffer is not empty"))
  (let* ((frames "Frameset page")
         (normal "Normal page")
         ;;(vlhead "Validation header")
         ;;popcmp-popup-completion
         (initial (unless popcmp-popup-completion normal))
         (hist (if mumamo-multi-major-mode
                   ;;(list vlhead frames normal)
                   (list frames normal)
                 (list frames normal)))
         res)
    (setq res (popcmp-completing-read "Insert: " hist nil t initial (cons 'hist (length hist))))
    (cond ((string= res frames)
           (nxhtml-insert-empty-frames-page))
          ((string= res normal)
           (nxhtml-insert-empty-page))
          ((string= res vlhead)
           (nxhtml-validation-header-mode))
          (t
           (error "Bad res=%s" res))))
  (rng-auto-set-schema))



(defvar nxhtml-mode-hook nil)
;;(add-hook 'nxhtml-mode-hook 'nxml-fontify-buffer)

(defun nxhtml-help ()
  (interactive)
  (describe-function 'nxhtml-mode))


(defun nxhtml-browse-file (file)
  "View file in web browser."
  (interactive (list
                (or (html-site-buffer-or-dired-file-name)
                    (read-file-name "File: "))))
  (let* ((buf (if (buffer-file-name)
                  (current-buffer)
                (find-buffer-visiting file)))
         (use-temp (and (buffer-file-name)
                        (or nxhtml-current-validation-header
                            (buffer-modified-p)
                            (not buffer-file-name)
                            (not (file-exists-p buffer-file-name))))))
    (if use-temp
        (browse-url (nxhtml-save-browseable-temp-file nil nil use-temp))
      (browse-url-of-file file))))

(defun nxhtml-browse-region ()
  "View region in web browser."
  (interactive)
  (unless mark-active
    (error "The region is not active"))
  (browse-url (nxhtml-save-browseable-temp-file (region-beginning) (region-end))))

(defun nxhtml-customize ()
  "Customize nXhtml."
  (interactive)
  (customize-group 'nxhtml))

;; FIX-ME: When should this be done? Get tidy-menu-symbol:
(when (featurep 'tidy-xhtml)
  (tidy-build-menu))


;; (eval-after-load 'css-mode
;;   '(when (featurep 'xhtml-help)
;;     (define-key css-mode-map [(control ?c) ?? ?c] 'xhtml-help-show-css-ref)
;;     ))
;; (add-hook 'css-mode-hook
;;           (lambda ()
;;             (and (featurep 'xhtml-help)
;;                  (boundp 'css-mode-map)
;;                  (define-key css-mode-map [(control ?c) ?? ?c]
;;                    'xhtml-help-show-css-ref))))

;; This should be run in `change-major-mode-hook'."
(defun nxhtml-change-mode ()
  (when (fboundp 'mlinks-mode)
    (mlinks-mode 0)))

;; This should be run in `change-major-mode-hook'."
;; Should be part of nxml.el IMO
(defun nxml-change-mode ()
  ;; Remove overlays used by nxml-mode.
  (save-excursion
    (save-restriction
      (widen)
      (rng-validate-mode -1)
      (let ((inhibit-read-only t)
            (buffer-undo-list t)
            (modified (buffer-modified-p)))
        (nxml-with-invisible-motion
          (remove-text-properties (point-min) (point-max) '(face nil)))
        (set-buffer-modified-p modified)))))

(defcustom nxhtml-heading-element-name-regexp "[a-z]*"
  "Used for `nxml-heading-element-name-regexp."
  :type 'regexp
  :group 'nxhtml)

(put 'nxhtml-mode 'flyspell-mode-predicate 'sgml-mode-flyspell-verify)
;;;###autoload
(define-derived-mode nxhtml-mode nxml-mode "nXhtml"
  "Major mode for editing XHTML documents.
It is based on `nxml-mode' and adds some features that are useful
when editing XHTML files.\\<nxhtml-mode-map>

To see an overview in html format do \\[nxhtml-overview].

* Note: Please observe that when loading nXhtml some file
  associations are done, see `nxhtml-auto-mode-alist'.

The nXhtml menu is added by this mode \(or actually the minor
mode `nxhtml-minor-mode') and gives quick access and an overview
of some other important features. These includes:

- multiple major modes, see `define-mumamo-multi-major-mode'
- easy uploading and viewing of files, see for example
  `html-upl-upload-file'
- validation in XHTML part for php etc, see
  `nxhtml-validation-header-mode' (you probably also want to know about
  `nxhtml-toggle-visible-warnings' for this!)
- converting of html to xhtml, see `tidy-buffer'

The XML menu contains functionality added by `nxml-mode' \(on
which this major mode is based).  There is also a popup menu
added to the \[apps] key.

The most important features are probably completion and
validation, which is inherited from `nxml-mode' with some small
addtions.  In very many situation you can use completion. To
access it type \\[nxml-complete]. Completion has been enhanced in
the following way:

- If region is active and visible then completion will surround the
  region with the chosen tag's start and end tag.  However only the
  starting point is checked for validity. If something is wrong after
  insertion you will however immediately see it if you have validation
  on.
- It can in some cases give assistance with attribute values.
- Completion can be customized, see the menus XHTML - Completion:
  * You can use a menu popup style completion.
  * You can have alternatives grouped.
  * You can get a short help text shown for each alternative.
- There does not have to be a '<' before point for tag name
  completion. (`nxml-mode' requires a '<' before point for tag name
  completion.)
- Completes xml version and encoding.
- Completes in an empty buffer, ie inserts a skeleton.

Some smaller, useful, but easy-to-miss features:

* Following links. The href and src attribute names are
  underlined and a special keymap is bound to
  them:\\<mlinks-mode-map>

    \\[mlinks-backward-link], \\[mlinks-forward-link] Move
        between underlined href/src attributes

    \\[mlinks-goto], Mouse-1 Follow link inside Emacs
        (if possible)

  It is even a little bit quicker when the links are in an active
  state (marked with the face `isearch'):\\<mlinks-active-hilight-keymap>

    \\[mlinks-backward-link], \\[mlinks-forward-link] Move
        between underlined href/src attributes
    \\[mlinks-goto], Mouse-1  Follow link inside Emacs (if possible)

  If the link is not into a file that you can edit (a mailto link
  for example) you will be prompted for an alternative action.

* Creating links. To make it easier to create links to id/name
  attribute in different files there are two special
  functions:\\<nxhtml-mode-map>

    \\[nxhtml-save-link-to-here] copy link to id/name (you must
        be in the tag to get the link)
    \\[nxhtml-paste-link-as-a-tag] paste this as an a-tag.

Here are all key bindings in nxhtml-mode itself:

\\{nxhtml-mode-map}

The minor mode `nxhtml-minor-mode' adds some bindings:

\\{nxhtml-minor-mode-map}

Notice that other minor mode key bindings may also be active, as
well as emulation modes. Do \\[describe-bindings] to get a list
of all active key bindings. Also, *VERY IMPORTANT*, if mumamo is
used in the buffer each mumamo chunk has a different major mode
with different key bindings. You can however still see all
bindings with \\[describe-bindings], but you have to do that with
point in the mumamo chunk you want to know the key bindings in.

---------
* Note: Some of the features supported by this mode are optional
  and available only if other Emacs modules are found.  Use
  \\[nxhtml-features-check] to get a list of these optional
  features and modules needed. You should however have no problem
  with this if you have followed the installation instructions
  for nXhtml."
  (set (make-local-variable 'nxml-heading-element-name-regexp)
       nxhtml-heading-element-name-regexp)
  (add-hook 'change-major-mode-hook 'nxml-change-mode nil t)
  (add-hook 'change-major-mode-hook 'nxhtml-change-mode nil t)
  (when (featurep 'rngalt)
    (add-hook 'nxml-completion-hook 'rngalt-complete nil t))
  ;;(define-key nxhtml-mode-map [(meta tab)] 'nxml-complete)
  (nxhtml-minor-mode 1)
  (when (and nxhtml-use-imenu
             (featurep 'html-imenu))
    (add-hook 'nxhtml-mode-hook 'html-imenu-setup nil t))
  (when (fboundp 'mlinks-mode)
    (mlinks-mode 1))
  (when (featurep 'fold-dwim)
    (nxhtml-setup-for-fold-dwim))
  (when (featurep 'rngalt)
    (set (make-local-variable 'rngalt-completing-read-tag) 'nxhtml-completing-read-tag)
    (set (make-local-variable 'rngalt-completing-read-attribute-name) 'nxhtml-completing-read-attribute-name)
    (set (make-local-variable 'rngalt-completing-read-attribute-value) 'nxhtml-completing-read-attribute-value)
    (set (make-local-variable 'rngalt-complete-first-try) 'nxhtml-complete-first-try)
    (set (make-local-variable 'rngalt-complete-last-try) 'nxhtml-complete-last-try)
    ))

;; Fix-me: The nxhtml-mode-map is define by define-derived-mode, but
;; how should keys be added?

;; Replace the Insert End Tag function:
(define-key nxhtml-mode-map [(control ?c) (control ?f)] 'rngalt-finish-element)

;; Put completion on the normal key?
(define-key nxhtml-mode-map [(meta tab)] 'nxml-complete)
;; Paragraphs (C-p mnemonic for paragraph)
(define-key nxhtml-mode-map [(control ?c) (control ?p) ?l] 'longlines-mode)
(define-key nxhtml-mode-map [(control ?c) (control ?p) ?f] 'fill-paragraph)
(define-key nxhtml-mode-map [(control ?c) (control ?p) ?u] 'unfill-paragraph)
;; Html related (C-h mnemonic for html)
(define-key nxhtml-mode-map [(control ?c) (control ?h) ?c] 'nxhtml-save-link-to-here)
(define-key nxhtml-mode-map [(control ?c) (control ?h) ?v] 'nxhtml-paste-link-as-a-tag)
(define-key nxhtml-mode-map [(control ?c) (control ?h) ?b] 'nxhtml-browse-file)
(define-key nxhtml-mode-map [(control ?c) ?<] 'nxml-untag-element)
(when (featurep 'html-quote)
  (define-key nxhtml-mode-map [(control ?c) (control ?q)] 'nxhtml-quote-html)
  )
;; Fix-me: Is pagetoc really that important to have its own keybindings?
(when (featurep 'html-pagetoc)
  (define-key nxhtml-mode-map [(control ?c) (control ?h) ?t ?i] 'html-pagetoc-insert-toc)
  (define-key nxhtml-mode-map [(control ?c) (control ?h) ?t ?r] 'html-pagetoc-rebuild-toc)
  (define-key nxhtml-mode-map [(control ?c) (control ?h) ?t ?s] 'html-pagetoc-insert-style-guide)
  )

(defun nxhtml-quote-html()
  "Quote character(s) unsafe in html text parts.
If region is visible quote all characters in region. Otherwise
just quote current char.

Note to CUA users: See `cua-mode' for how to prevent CUA from
just copying region when you press C-c."
  (interactive)
  (if (and mark-active
           transient-mark-mode)
      (let* ((rb (region-beginning))
             (re (region-end))
             (qr (html-quote-html-string
                  (buffer-substring-no-properties rb re))))
        (delete-region rb re)
        (insert qr))
    (let ((cs (html-quote-html-char (char-after))))
      (delete-char 1)
      (insert cs))))

(defvar nxhtml-single-tags
  '("base"
    "meta"
    "link"
    "br"
    "hr"
    "frame"
    "img"
    "input"
    "option"
    "param"))

(defun nxthml-is-single-tag (tag)
  (member tag nxhtml-single-tags))

(defvar nxhtml-help-attribute-name
  '(("title"    "Element title")
    ("class"   "Style class of element")
    ("charset"  "Encoding of target")
    ("coords"   "Defining shape")
    ("href"   "Target URL")
    ("hreflang"   "Language of target")
    ("name"   "(DEPRECEATED)")
    ("rel"   "Target's relation to document")
    ("rev"   "Document's relation to target")
    ("shape"   "Area shape")
    ("target"   "Where to open target")
    ("type"   "MIME type of target")

    ("id"   "Unique id of element")
    ("lang"   "Language code")
    ("dir"   "Text direction")
    ("accesskey"   "Keyboard shortcut")
    ("tabindex"   "Tab order of element")

    ("style"   "Inline style")
    ("disabled"   "Tag initially disabled")
    ("readonly"   "User can not modify")
    ;;(""   "")

    ("alink" "(DEPRECEATED)")
    ("background" "(DEPRECEATED)")
    ("bgcolor" "(DEPRECEATED)")
    ("link" "(DEPRECEATED)")
    ("text" "(DEPRECEATED)")
    ("vlink" "(DEPRECEATED)")
    ("xml:lang" "Tag content language")
    ("cite" "URL with more info")
    ("method" "HTTP method for sending")
    ("accept" "Content types")
    ("accept-charset" "Character sets")
    ("enctype" "Encoding")
    ))
(defvar nxhtml-help-attribute-name-tag
  '(("textarea"
     ("name" "Name for textarea")
     )
    ))

(defvar nxhtml-help-tag
  (let ((h (make-hash-table :test 'equal)))
    (puthash "html"     "Document" h)
    (puthash "head"     "Document head" h)
    (puthash "title"    "Document title" h)
    (puthash "base"     "Base URL/target" h)
    (puthash "meta"     "Meta information" h)
    (puthash "style"    "Inline style sheet" h)
    (puthash "link"     "Style sheet etc" h)
    (puthash "script"   "(Java)Script code" h)
    (puthash "noscript" "Script disabled part" h)
    (puthash "isindex"  "(DEPRECEATED)" h)

    (puthash "iframe"   "Inline frame" h)
    (puthash "frameset" "Organize frames" h)
    (puthash "frame"    "Sub window" h)
    (puthash "noframes" "Substitute for frames" h)

    (puthash "bdo"      "Text direction" h)

    (puthash "body"     "Document body" h)
    (puthash "a"        "Link" h)
    (puthash "p"        "Paragraph" h)
    (puthash "span"     "Group inline elements" h)
    (puthash "br"       "Line break" h)
    (puthash "hr"       "Horizontal rule" h)
    (puthash "div"      "Division/section" h)
    (puthash "img"      "Image" h)
    (puthash "h1"       "Header 1" h)
    (puthash "del"      "Deleted text" h)
    (puthash "strike"   "(DEPRECEATED)" h)
    (puthash "u"        "(DEPRECEATED)" h)
    (puthash "s"        "(DEPRECEATED)" h)
    (puthash "ins"      "Inserted text" h)
    (puthash "sup"      "Superscript text" h)
    (puthash "center"   "(DEPRECEATED)" h)
    (puthash "dir"      "(DEPRECEATED)" h)

    (puthash "blockquote" "Long quotation" h)
    (puthash "q"          "Short quotation" h)
    (puthash "pre"      "Preformatted text" h)
    (puthash "applet"   "(DEPRECEATED)" h)
    (puthash "basefont" "(DEPRECEATED)" h)
    (puthash "font"     "(DEPRECEATED)" h)

    ;; The following elements are all font style elements. They are
    ;; not deprecated, but it is possible to achieve richer effects
    ;; using style sheets.
    (puthash "tt"       "Renders as teletype or mono spaced text" h)
    (puthash "i"        "Renders as italic text" h)
    (puthash "b"        "Renders as bold text" h)
    (puthash "big"      "Renders as bigger text" h)
    (puthash "small"    "Renders as smaller text" h)


    ;; The following tags are not deprecated, but it is possible to
    ;; achieve a much richer effect using style sheets:
    (puthash "em"       "Renders as emphasized text" h)
    (puthash "strong"   "Renders as strong emphasized text" h)
    (puthash "dfn"      "Defines a definition term" h)
    (puthash "code"     "Defines computer code text" h)
    (puthash "samp"     "Defines sample computer code" h)
    (puthash "kbd"      "Defines keyboard text" h)
    (puthash "var"      "Defines a variable" h)
    (puthash "cite"     "Defines a citation" h)

    (puthash "ul"       "Unordered list" h)
    (puthash "ol"       "Ordered list" h)
    (puthash "li"       "List element" h)
    (puthash "dl"       "Definition list" h)
    (puthash "dt"       "Definition term" h)
    (puthash "dd"       "Definition description" h)


    (puthash "fieldset" "Draw box around" h)
    (puthash "form"     "User input form" h)
    (puthash "input"    "Input field/checkbox etc" h)
    (puthash "textarea" "Input multiline field" h)
    (puthash "button"   "Push button" h)
    (puthash "label"    "Label for control" h)
    (puthash "map"      "Client side image map" h)
    (puthash "select"   "Drop down list" h)
    (puthash "option"   "Option in drop down list" h)
    (puthash "menu"     "(DEPRECEATED)" h)

    (puthash "object"   "Embedded object" h)
    (puthash "param"    "Object settings" h)

    (puthash "abbr"     "Abbreviation" h)
    (puthash "address"  "For addresses etc" h)
    (puthash "acronym"  "May be used for lookup etc" h)

    (puthash "table"    "Table" h)
    (puthash "caption"  "Table caption" h)
    (puthash "col"      "Table column attributes" h)
    (puthash "colgroup"  "Table column group" h)
    (puthash "thead"    "Table header" h)
    (puthash "tbody"    "Table body" h)
    (puthash "tfoot"    "Table footer" h)
    (puthash "tr"       "Table row" h)
    (puthash "td"       "Table cell" h)

    h))

(defun nxhtml-short-tag-help (tag)
  "Display description of tag TAG.  If TAG is omitted, try tag at point."
  (interactive
   (let ((tag (xhtml-help-tag-at-point)))
     (unless (stringp tag)
       (setq tag (read-string "No tag at point. Give tag name: ")))
     (list tag)))
  (setq tag (downcase tag))
  (let ((desc (gethash tag nxhtml-help-tag))
        (use-dialog-box nil))
    (unless desc
      (setq desc (concat tag " -- No short description available")))
    (when (y-or-n-p (concat desc ". Fetch more information from the Internet? "))
      (xhtml-help-browse-tag tag))))

(defvar nxhtml-no-single-tags nil)
(defvar nxhtml-no-end-tags nil)

(defadvice rng-complete-qname-function (around nxhtml-rng-complete-qname-function-ad
                                               (string predicate flag)
                                               disable)
  ;;(if (not (eq major-mode 'nxhtml-mode))
  (if (not nxhtml-completing-with-help)
      ad-do-it
    (setq ad-return-value
          (let ((alist (mapcar (lambda (name) (cons name nil))
                               (nxhtml-rng-generate-qname-list string))))
            (cond ((not flag)
                   (try-completion string alist predicate))
                  ((eq flag t)
                   (all-completions string alist predicate))
                  ((eq flag 'lambda)
                   (and (assoc string alist) t)))))))




(defvar nxhtml-predicate-error nil)

(defun nxhtml-find-ids (file)
  (let ((buf (find-file-noselect file)))
    (when buf
      (with-current-buffer buf
        (when (eq major-mode 'nxhtml-mode)
          (save-excursion
            (let ((ids nil)
                  (id-ptrn
                   (rx space
                       "id"
                       (0+ space)
                       ?=
                       (0+ space)
                       ?\"
                       (submatch
                        (1+ (not (any ?\")))
                        )
                       ?\"
                       )))
              (goto-char (point-min))
              (while (re-search-forward id-ptrn nil t)
                (add-to-list 'ids (match-string-no-properties 1)))
              ids)))))))

(defun nxhtml-read-url (&optional allowed-types initial-contents extra-predicate prompt-prefix)
  (popcmp-mark-completing initial-contents)
  (let ((local-ovl popcmp-mark-completing-ovl))
    (setq popcmp-mark-completing-ovl nil)
    (unwind-protect
        (let* ((url-type (nxhtml-read-url-type allowed-types initial-contents))
               (base-prompt (cond ((eq url-type 'local-file-url)
                                   "File: ")
                                  ((eq url-type 'id-url)
                                   "Id: ")
                                  ((eq url-type 'web-url)
                                   "Web URL: ")
                                  ((eq url-type 'mail-url)
                                   "e-Mail address: ")
                                  ((eq url-type 'any-url)
                                   "Any URL-type: ")
                                  (t
                                   ;;(error "Internal error: bad url-type=%s" url-type)
                                   "Unknown URL-type: ")
                                  ))
               prompt
               type-predicate
               url
               (bad-url initial-contents)
               (default-directory (if buffer-file-name
                                      (file-name-directory buffer-file-name)
                                    default-directory)))
          (when prompt-prefix
            (setq base-prompt (concat prompt-prefix " " base-prompt)))
          (setq nxhtml-predicate-error "")
          (cond ((eq url-type 'local-file-url)
                 )
                ((eq url-type 'web-url)
                 )
                ((eq url-type 'mail-url)
                 (setq type-predicate 'nxhtml-mailto-predicate)
                 (when (and (stringp bad-url)
                            (<= 7 (length bad-url))
                            (string= "mailto:" (substring bad-url 0 7)))
                   (setq bad-url (substring bad-url 7)))))
          (while (not url)
            (setq prompt (concat nxhtml-predicate-error " " base-prompt))
            (cond ((eq url-type 'local-file-url)
                   (setq url (read-file-name prompt nil "" nil bad-url extra-predicate))
                   (when (< 0 (length url))
                     ;; Fix-me: prompt for id here
                     (setq url (file-relative-name
                                (expand-file-name url)))))
                  ((eq url-type 'id-url)
                   (setq url (completing-read prompt (nxhtml-find-ids buffer-file-name)))
                   (when url
                     (setq url (concat "#" url))))
                  ((eq url-type 'web-url)
                   (setq url (nxhtml-read-from-minibuffer prompt bad-url nil nil
                                                          'nxhtml-read-web-url-history
                                                          t)))
                  ((eq url-type 'mail-url)
                   (setq url (nxhtml-read-from-minibuffer prompt bad-url nil nil
                                                          'nxhtml-read-mail-url-history
                                                          t)))
                  (t
                   (setq url (nxhtml-read-from-minibuffer prompt bad-url nil nil
                                                          'nxhtml-read-url-history
                                                          t))))
            (when (or (and type-predicate
                           (not (funcall type-predicate url)))
                      (and extra-predicate
                           (not (funcall extra-predicate url))))
              (setq bad-url url)
              (setq url)))
          (when (eq url-type 'mail-url)
            (setq url (concat "mailto:" url)))
          url)
      (delete-overlay local-ovl)
      )))

(defun nxhtml-read-url-type (allowed url-beginning)
  (assert (or (listp allowed) (eq t allowed)) t)
  (let* ((prompt "URL-type: ")
         (parsed-url (url-generic-parse-url url-beginning))
         (beg-type (url-type parsed-url))
         (allowed-u allowed)
         choices
         choice)
    ;; (url-type (url-generic-parse-url "#some-id"))
    ;;(lwarn t :warning "url-type=%s, pu=%s" (url-type parsed-url) parsed-url)
    ;; Emacs 23 bug workaround Sat Jan 26 2008
    ;;(when (eq beg-type 'cl-struct-url) (setq beg-type (elt parsed-url 1)))
    (cond ((string= "mailto" beg-type)
           (setq allowed-u '(?m)))
          ((or (string= "http"  beg-type)
               (string= "https" beg-type)
               (string= "ftp"   beg-type))
           (setq allowed-u '(?w)))
          ((= 1 (length beg-type)) ;; w32
           (setq allowed-u '(?f)))
          ((and (null beg-type)
                url-beginning
                (= ?# (string-to-char url-beginning)))
           (setq allowed-u '(?i)))
          )
    ;; Be a bit picky and hopefully helpful, check if really allowed:
    (unless (or (eq allowed t)
                (equal allowed allowed-u))
      (let ((temp-u (copy-sequence allowed-u)))
        (dolist (a allowed)
          (setq temp-u (delq a temp-u)))
        (dolist (u temp-u)
          (setq allowed-u (delq u allowed-u)))))
    (if allowed-u
        (when (eq allowed-u t)
          (setq allowed-u '(?f ?i ?w ?m)))
      (setq allowed-u '(?f ?w)))
    (dolist (a allowed-u)
      (cond
       ((= a ?f)
        (setq choices (cons "File" choices)))
       ((= a ?i)
        (setq choices (cons "Id" choices)))
       ((= a ?w) (setq choices (cons "Url" choices)))
       ((= a ?m) (setq choices (cons "Mail" choices)))
       ))
    (if (= 1 (length allowed-u))
        (setq choice (car choices))
    (setq choice (popcmp-completing-read prompt choices nil t
                                           "" nil nil t)))
    (cond ((string= choice "Id")
           'id-url)
          ((string= choice "File")
           'local-file-url)
          ((string= choice "Url")
           'web-url)
          ((string= choice "Mail")
           'mail-url)
          )))

(defvar nxhtml-read-url-history nil)
(defvar nxhtml-read-web-url-history nil)
(defvar nxhtml-read-mail-url-history nil)

(defconst nxhtml-in-xml-attribute-value-regex
  (replace-regexp-in-string
   "w"
   xmltok-ncname-regexp
   ;;"<w\\(?::w\\)?\
   "<\\?xml\
\\(?:[ \t\r\n]+w\\(?::w\\)?[ \t\r\n]*=\
\[ \t\r\n]*\\(?:\"[^\"]*\"\\|'[^']*'\\)\\)*\
\[ \t\r\n]+\\(w\\(:w\\)?\\)[ \t\r\n]*=[ \t\r\n]*\
\\(\"[^\"]*\\|'[^']*\\)\\="
   t
   t))

(defun nxhtml-mailto-predicate (url)
  "Tries to match a mailto url.
This is not supposed to be entirely correct."
  (setq nxhtml-predicate-error nil)
  ;; Local pattern copied from gnus.
  (let ((r (concat "^"
                   ;;"mailto:"
                   "[a-z0-9$%(*-=?[_][^<>\")!;:,{}]*"
                   "\@"
                   "\\(?:[a-z0-9\-]+\.\\)+[a-z0-9]\\{2,4\\}$"))
        (case-fold-search t))
    ;;(message "mailpred") (sit-for 1)
    (if (string-match r url)
        t
      (setq nxhtml-predicate-error "Malformed email address.")
      nil)))

(defcustom nxhtml-image-completion-pattern
  "\\.\\(?:png\\|jpg\\|jpeg\\|gif\\)$"
  "Pattern for matching image URLs in completion."
  :type 'regexp
  :group 'nxhtml)

(defun nxhtml-image-url-predicate (url)
  (setq nxhtml-predicate-error nil)
  (if (or (file-directory-p url)
          (string-match nxhtml-image-completion-pattern url))
      t
    (setq nxhtml-predicate-error "Does not match image file name pattern.")
    nil
    ))

(defcustom nxhtml-css-completion-pattern
  "\\.\\(?:css\\)$"
  "Pattern for matching css URLs in completion."
  :type 'regexp
  :group 'nxhtml)

(defun nxhtml-css-url-predicate (url)
  (setq nxhtml-predicate-error nil)
  (if (or (file-directory-p url)
          (string-match nxhtml-css-completion-pattern url))
      t
    (setq nxhtml-predicate-error "Does not match css file name pattern.")
    nil
    ))

(defcustom nxhtml-script-completion-pattern
  "\\.\\(?:js\\)$"
  "Pattern for matching src URLs in completion in script tags."
  :type 'regexp
  :group 'nxhtml)

(defun nxhtml-script-url-predicate (url)
  (setq nxhtml-predicate-error nil)
  (if (or (file-directory-p url)
          (string-match nxhtml-script-completion-pattern url))
      t
    (setq nxhtml-predicate-error "Does not match script file name pattern.")
    nil
    ))

(defun nxhtml-coding-systems-complete (init default)
  (let (coding-systems
        hist-num
        (n 0)
        hist)
    (unless (and init (< 0 (length init)))
      (setq init default))
    (mapc (lambda (coding-system)
            (let ((mime-charset (coding-system-get coding-system 'mime-charset)))
              (when mime-charset
                (setq coding-systems (cons
                                      (symbol-name mime-charset)
                                      coding-systems)))))
          (coding-system-list t))
    (setq coding-systems (sort coding-systems 'string=))
    (mapc (lambda (coding-system)
            (unless (< 0 (length coding-system))
              (error "len=0"))
            (setq n (1+ n))
            (when (string= coding-system init) (setq hist-num n)))
          coding-systems)
    (if hist-num
        (setq hist (cons 'coding-systems hist-num))
      (setq hist 'coding-systems))
    (completing-read "Encoding (coding system): "
                     coding-systems nil t init hist)))


;; Note: This function does not currently use the state provided by
;; the nxml and rng functions directly.  Instead it searches the
;; environment near point to decide what to do.
;; (defun nxhtml-complete-and-insert ()
;;   "Perform XHTML completion at point.
;; This is merely an extended version of `nxml-complete' with the following changes:

;; - If region is visible and active then completion will surround the
;;   region with the chosen tag's start and end tag.  However only the
;;   starting point is checked for validity. If something is wrong after
;;   insertion you will however immediately see it if you have validation
;;   on.
;; - Can in some cases give completion help inside attribute values.
;; - There does not have to be a '<' before point for tag name
;;   completion. (`nxml-mode' requires a '<' before point for tag name
;;   completion.)
;; - For tag names there is a popup style completion available. This
;;   gives a bit more guiding since it groups the alternative tags. Set
;;   `popcmp-popup-completion' to use this.
;; - Completes xml version and encoding.
;; - Completes an empty file, ie inserts a skeleton."
;;   (interactive)
;;   (let (res
;;         (where (nxhtml-check-where)))
;;     (or (when (eq where 'in-empty-page)
;;           (nxhtml-empty-page-completion))
;;         (when (and mark-active
;;                    transient-mark-mode
;;                    (eq where 'in-text))
;;           (nxhtml-insert-tag))
;;         (progn
;;           (cond ((memq where '(in-start-tag in-closed-start-tag in-end-tag))
;;                  (re-search-forward "\\=/?[a-z]*" nil t))
;;                 ((memq where '(in-attr))
;;                  (re-search-forward "\\=[a-z]*=" nil t))
;;                 ((memq where '(in-attr-val in-xml-attr-val))
;;                  (re-search-forward "\\=[^<>\" \t\r\n]*" nil t))
;;                 )
;;           (when (run-hook-with-args-until-success 'nxml-completion-hook)
;;             (when (re-search-backward "[^=]\"\\=" nil t)
;;               (forward-char) (delete-char 1)
;;               ;;(undo-start) (undo-more 1)
;;               )
;;             t))
;;         (when (and (not where)
;;                    (char-before)
;;                    (= ?\" (char-before)))
;;           nil)
;;         (when (or (when (char-before) (= ?> (char-before)))
;;                   (eq where 'in-text))
;;           (setq res t)
;;           (nxhtml-insert-tag))
;;         ;; Eventually we will complete on entity names here.
;;         res
;;         (progn
;;           (ding)
;;           (message "Cannot complete in this context")))))

(defvar nxhtml-in-proc-instr-back-regex "<\\?[^<>]*\\=")
(defvar nxhtml-in-proc-instr-forw-regex "\\=[^<>]*\\?>")

(defconst rngalt-in-pre-attribute-value-regex
  (replace-regexp-in-string
   "w"
   xmltok-ncname-regexp
   "<w\\(?::w\\)?\
\\(?:[ \t\r\n]+w\\(?::w\\)?[ \t\r\n]*=\
\[ \t\r\n]*\\(?:\"[^\"]*\"\\|'[^']*'\\)\\)*\
\[ \t\r\n]+\\(w\\(:w\\)?\\)[ \t\r\n]*=[ \t\r\n]*\
\\="
   t
   t))

(defun nxhtml-check-where ()
  "Get a state for `nxhtml-complete-last-try'."
  (let ((p (point))
        (lt-pos (save-excursion (search-backward "<" nil t)))
        res)
    (cond ((= 0 (buffer-size))
           (setq res 'in-empty-page))
          ((looking-back "<!--[^<>]*\\=" 1 t)
           (setq res 'in-comment))
          ((let ((face (get-char-property (point) 'face)))
             (when (memq face '(nxml-comment-content-face
                                nxml-comment-delimiter-face))
               (setq res 'in-comment)))
           t)
          ((looking-back nxhtml-in-xml-attribute-value-regex lt-pos t)
           (setq res 'in-xml-attr-val))
          ((looking-back nxhtml-in-proc-instr-back-regex 1 t)
           (setq res 'in-proc-instr))
          ((looking-back "<!D[^>]*\\=" 1 t)
           (setq res 'in-doctype))
          ((looking-back ">[^<]*" 1 t)
           (setq res 'in-text))
          ((looking-back rng-in-start-tag-name-regex 1 t)
           (setq res 'in-tag-start)
           (when (looking-at "\\=[^<]*>")
             (setq res 'in-closed-start-tag)))
          ((looking-back rng-in-end-tag-name-regex 1 t)
           (setq res 'in-tag-end))
          ((looking-back rng-in-attribute-regex 1 t)
           (setq res 'in-attr))
          ((looking-back rng-in-attribute-value-regex 1 t)
           (setq res 'in-attr-val))
          ((looking-back rngalt-in-pre-attribute-value-regex 1 t)
           (setq res 'in-pre-attr-val))
          ((looking-back "\"")
           (setq res 'after-attr-val))
          ((and rngalt-validation-header
                (looking-back "\\`[^<]*"))
           ;; FIX-ME: This is treated the same as in text currently,
           ;; but this should be checked. Maybe it is best to test
           ;; this here and return the relevant value?
           (setq res 'after-validation-header))
          )
    ;;(message "res=%s" res)(sit-for 1)
    (unless res
      (error "Could not find a state for completion"))
    res))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Make the completions additions cleaner:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst nxhtml-tag-sets
  '(("logical"
     "del"
     "ins"
     "abbr"
     "acronym"
     "fieldset"
     "blockquote"
     "q"
     "code"
     "samp"
     "cite"
     "kbd"
     "var"
     "dfn"
     "address"
     "em"
     "strong"
     "pre"
     )
    ("physical"
     "hr"
     "sup"
     "sub"
     "font"
     "basefont"
     "br"
     "big"
     "small"
     "strike"
     "u"
     "i"
     "b"
     "s"
     "tt"
     "center"
     "bdo"
     )
    ("scripting"
     "script"
     "noscript"
     "object"
     "applet"
     )
   ("structure"
    "iframe"
    "p"
    "div"
    "span"
    "h6"
    "h5"
    "h4"
    "h3"
    "h2"
    "h1"
    )

   ("form"
    "isindex"
    "label"
    "button"
    "option"
    "select"
    "input"
    "textarea"
    "form"
    )

   ("list"
    "dt"
    "dd"
    "li"
    "dir"
    "menu"
    "ol"
    "dl"
    "ul"
    )

   ("link"
    "a"
    )

   ("image"
    "img"
    "map"
    )

   ("table"
    "table"
    "tr"
    "th"
    "td"
    "caption"
    "col"
    "colgroup"
    "thead"
    "tbody"
    "tfoot"
    )

   ("document"
    "base"
    "style"
    "link"
    "head"
    "body"
    "frame"
    "frameset"
    "noframes"
    "isindex"
    "nextid"
    "meta"
    "title"
    )
   ))

(defvar nxhtml-attr-sets
  '(("scripting"
     "onblur"
     "onchange"
     "onclick"
     "ondblclick"
     "onfocus"
     "onkeydown"
     "onkeypress"
     "onkeyup"
     "onload"
     "onunload"
     "onmousedown"
     "onmousemove"
     "onmouseout"
     "onmouseover"
     "onmouseup"
     "onreset"
     "onselect"
     "onsubmit"
     )
    ("form"
     "method"
     "accept"
     "accept-charset"
     "enctype"
     )
    ("access"
     "id"
     "name"
     "disabled"
     "readonly")
    ("layout"
     "accesskey"
     "class"
     "coords"
     "shape"
     "style"
     "tabindex"
     "title"
     "align"
     "valign"
     "alink"
     "background"
     "bgcolor"
     "link"
     "text"
     "vlink"
     "compact"
     )
    ("target"
     "charset"
     "href"
     "hreflang"
     "rel"
     "rev"
     "target"
     "type"
     )
    ("language"
     "dir"
     "lang"
     "xml:lang"
     )
    ;; id
    ;; name
    ;; xml:lang
    ))

(defun nxhtml-complete-last-try ()
  (when rng-current-schema-file-name
    (let ((where (nxhtml-check-where)))
      (cond
       ;;((eq where 'after-attr-val)
        ;;(insert " ")
        ;;)
       ((eq where 'in-pre-attr-val)
        (insert ?\"))
       ((eq where 'in-comment)
        (if (not (looking-at "[^>]*<"))
            nil
          (insert " -->")
          t))
       ((eq where 'in-xml-attr-val)
        (let (attr
              delimiter
              val)
        (save-excursion
          (save-match-data
            (re-search-forward "\\=[^<> \t\r\n\"]*" nil t)))
        (let* ((name-start (match-beginning 1))
               (name-end (match-end 1))
               (colon (match-beginning 2))
               (attr (buffer-substring-no-properties name-start
                                                     (or colon name-end)))
               (value-start (1+ (match-beginning 3)))
               (tag (save-excursion
                      (when (search-backward-regexp "<[[:alpha:]]+" nil t)
                        (match-string 0))))
               (init (buffer-substring-no-properties value-start (point))))
          (setq delimiter (char-before value-start))
          (cond ((string= "encoding" attr)
                 ;; Give a default that works in browsers today
                 (setq val (nxhtml-coding-systems-complete
                            init
                            (symbol-name nxhtml-default-encoding))))
                ((string= "version" attr)
                 (setq val "1.0")))
          (when val
            (insert val)
            t)
          )))
       ((or (memq where '(in-text
                          after-validation-header
                          in-empty-page)))
        (rngalt-complete-tag-region-prepare)
        (insert "<")
        (condition-case err
            (nxhtml-redisplay-complete)
          (quit
           (message "%s" (error-message-string err))
           (undo-start)
           (undo-more 1)
           (rngalt-complete-tag-region-cleanup)))
        t)
       (t
        ;;(message "LAST TRY where=%s" (nxhtml-check-where))(sit-for 1)
        nil)
       ))))

(defun nxhtml-img-tag-do-also ()
  (insert "alt=\"")
  (rngalt-validate)
  (insert (read-string "Alt attribute: ")
          "\" ")
  (insert "src=\"")
  (rngalt-validate)
  (let ((src (nxhtml-read-url nil nil 'nxhtml-image-url-predicate "Image")))
    (insert src)
    (insert "\"")
    (when (file-exists-p src)
           (let ((sizes (image-size (create-image (expand-file-name src)) t)))
             (insert
              " width=\""  (format "%d" (car sizes)) "\""
              " height=\"" (format "%d" (cdr sizes)) "\"")
             )))
  (unless (save-match-data (looking-at "[^<]\\{,200\\}>"))
    (insert " />")))

(defun nxhtml-redisplay-complete ()
  (rngalt-validate)
  (rng-cancel-timers)
  (message "")
  (redisplay t)
  (nxml-complete)
  (rng-activate-timers))

(defun nxhtml-read-from-minibuffer (prompt &optional
                                          initial-contents keymap
                                          read hist default-value
                                          inherit-input-method)
  (rng-cancel-timers)
  (message "")
  (let ((res (read-from-minibuffer prompt initial-contents keymap
              read hist default-value inherit-input-method)))
    (rng-activate-timers)
    res))

(defun nxhtml-meta-tag-do-also ()
  (let ((type (popcmp-completing-read
               "Type: "
               '(
                 ;;"Refresh/Redirect"
                 "HTTP Message Headers"
                 "Robot Rules"
                 "Description for Search Engines"
                 ))))
    (cond
     ((string= type "Description for Search Engines")
      (insert " name=\"Description\"")
      (insert " content=\"")
      (insert (nxhtml-read-from-minibuffer "Description: "))
      (insert "\" />"))
     ((string= type "Robot Rules")
      (insert " name=\"Robots\"")
      (insert " content=\"")
      (nxhtml-redisplay-complete)
      (insert " />"))
     ((string= type "HTTP Message Headers")
      (insert " http-equiv=\"")
      (nxhtml-redisplay-complete)
      (insert " content=\"")
      (insert (nxhtml-read-from-minibuffer "Content: "))
      (insert "\" />")))))

(defun nxhtml-style-tag-do-also ()
  (insert "type=\"text/css\"")
  (insert " media=\"")
  (nxhtml-redisplay-complete)
  (insert ">")
  (indent-according-to-mode)
  (insert "\n/* <![CDATA[ */")
  (indent-according-to-mode)
  (insert "\n")
  (indent-according-to-mode)
  (insert "\n/* ]] */")
  (indent-according-to-mode)
  (insert "\n</style>")
  (indent-according-to-mode)
  (insert "\n")
  (end-of-line -2))

(defun nxhtml-script-tag-do-also ()
  (let ((type (popcmp-completing-read
               "Type: "
               '("Inlined"
                 "Linked"))))
    (cond
     ((string= type "Inlined")
      (insert "type=\"text/javascript\">")
      (indent-according-to-mode)
      (insert "\n// <![CDATA[")
      (indent-according-to-mode)
      (insert "\n")
      (indent-according-to-mode)
      (insert "\n// ]]>")
      (indent-according-to-mode)
      (insert "\n</script>")
      (indent-according-to-mode)
      (end-of-line -1))
     ((string= type "Linked")
      (insert "type=\"text/javascript\"")
      (insert " src=\"")
      (nxhtml-redisplay-complete)
      (insert "></script>")))))

(defun nxhtml-link-tag-do-also ()
  (let ((type (popcmp-completing-read "Type: "
                          '(
                            "Other"
                            "Shortcut icon"
                            "Style sheet"
                            ))))
    (cond
     ((string= type "Style sheet")
      (insert " rel=\"Stylesheet\" ")
      (insert "type=\"text/css\" ")
      (insert "href=\"")
      (nxhtml-redisplay-complete)
      (insert " media=\"")
      (nxhtml-redisplay-complete)
      (insert " />"))
     ((string= type "Shortcut icon")
      (insert " rel=\"Shortcut Icon\" ")
      (insert "href=\"")
      (nxhtml-redisplay-complete)
      (insert " />"))
     (t
      (insert " ")
      (nxhtml-redisplay-complete)
      ))))

(defun nxhtml-input-tag-do-also ()
  (insert " ")
  (rngalt-validate)
  ;; type=
  (insert "type=\"")
  (nxhtml-redisplay-complete)
  (insert " ")

  (let* ((choice (save-match-data
                   (when (looking-back "type=\"\\(.*\\)\" ")
                     (match-string 1)))))
    ;;(insert "type=\"" choice "\" ")
    (rngalt-validate)
    ;;(message "choice=%s" choice)(sit-for 2)
    ;; name=
    (when (member choice '("button" "checkbox" "file" "hidden" "image"
                           "password" "radio" "text"))
      (insert "name=\""
              (read-string "Name (name): ")
              "\" ")
      (rngalt-validate))
    ;; checked=
    (when (member choice '("checkbox" "radio"))
      (when (y-or-n-p "Checked? (checked): ")
        (insert "checked=\"checked\" ")
        (rngalt-validate)))
    ;; disabled=
    (unless (string= choice "hidden")
      (unless (y-or-n-p "Enabled? : ")
        (insert "disabled=\"disabled\" ")
        (rngalt-validate)))
    ;; readonly=
    (when (string= choice "text")
      (when (y-or-n-p "Readonly? (readonly): ")
        (insert "readonly=\"readonly\" "))
      (rngalt-validate))
    (when (string= choice "file")
      ;; accept=
      (require 'mailcap)
      (condition-case err
          (let ((prompt (concat
                         "Accept mime type, RET to stop ("
                         "C-g to skip"
                         "): "))
                (mime " ")
                mimes
                (types (when (boundp 'mailcap-mime-extensions)
                         (mapcar (lambda (elt)
                                   (cdr elt))
                                 mailcap-mime-extensions))))
            (while (< 0 (length mime))
              (setq mime
                    (if types
                        (completing-read prompt types)
                      (read-string prompt)))
              (when (< 0 (length mime))
                (if mimes
                    (setq mimes (concat mimes "," mime))
                  (setq mimes mime))))
            (when (and mimes
                       (< 0 (length mimes)))
              (insert "accept=\"" mimes "\" ")))
        (quit (message "Skipped accept attribute")))
      (rngalt-validate))
    (when (string= choice "image")
      ;; alt=
      (insert "alt=\"")
      (rngalt-validate)
      (insert (read-string "Alt attribute: ")
              "\" ")
      (rngalt-validate)
      ;; src=
      (insert "src=\"")
      (rngalt-validate)
      (let ((src (nxhtml-read-url nil nil 'nxhtml-image-url-predicate "Image")))
        (insert src)
        (insert "\" "))
      (rngalt-validate))
    ;; value=
    (cond
     ((member choice '("button" "reset" "submit"))
      (nxhtml-do-also-value "Label"))
     ((member choice '("checkbox" "radio"))
      (nxhtml-do-also-value "Result"))
     ((member choice '("hidden" "password" "text"))
      (nxhtml-do-also-value "Value"))
     )
    (insert "/>")
    ;;(message "type=%s" choice)(sit-for 2)
  ))

(defun nxhtml-do-also-value (label)
  (let ((v (read-string (concat label " (value): "))))
    (when (and v
               (< 0 (length v)))
      (insert " value=\"" v "\" "))))

(defun nxhtml-form-tag-do-also ()
  (insert "action=\"")
  (rngalt-validate)
  (let ((src (nxhtml-read-url nil nil nil "Action")))
    (insert src "\" "))
  )

(defconst nxhtml-complete-tag-do-also
  '(("a"
     (lambda ()
       (insert " href=\"")
       (rngalt-validate)
       (insert (nxhtml-read-url t))
       (insert "\"")))
    ("form" nxhtml-form-tag-do-also)
    ("img" nxhtml-img-tag-do-also)
    ("input" nxhtml-input-tag-do-also)
    ("link" nxhtml-link-tag-do-also)
    ("script" nxhtml-script-tag-do-also)
    ("style" nxhtml-style-tag-do-also)
    ("meta" nxhtml-meta-tag-do-also)
    )
  "List of functions to call at tag completion.
Each element of the list have the form

  \(TAG-NAME TAG-FUN)

If `nxhtml-tag-do-also' is non-nil then TAG-FUN is called after
by `nxml-complete' (with the special setup of this function for
`nxhtml-mode') when completing a tag with the name TAG-NAME.

The list is handled as an association list, ie only the first
occurence of a tag name is used.")


(defun nxhtml-complete-tag-do-also (tag)
  ;; First required attributes:
  (let ((tagrec (assoc tag nxhtml-complete-tag-do-also)))
    (when tagrec
      (funcall (cadr tagrec))))
  )

(defun nxhtml-check-tag-do-also ()
  (when nxhtml-tag-do-also
    (nxhtml-turn-onoff-tag-do-also t)))

(defun nxhtml-turn-onoff-tag-do-also (on)
  (add-hook 'nxhtml-mode-hook 'nxhtml-check-tag-do-also)
  (dolist (b (buffer-list))
    (when (with-current-buffer b
            (eq major-mode 'nxhtml-mode))
      (if on
          (progn
            (add-hook 'rngalt-complete-tag-hooks 'nxhtml-complete-tag-do-also t t)
            )
          (remove-hook 'rngalt-complete-tag-hooks 'nxhtml-complete-tag-do-also t)
        ))))

(define-toggle nxhtml-tag-do-also t
  "When completing tag names do some more if non-nil.
For some tag names additional things can be done at completion to
speed writing up.  For example for an <img ...> tag `nxhtml-mode'
can prompt for src attribute and add width and height attributes
if this attribute points to a local file.

You can add additional elisp code for completing to
`nxhtml-complete-tag-do-also'."
  :set (lambda (symbol value)
         (set-default symbol value)
         (nxhtml-turn-onoff-tag-do-also value))
  :group 'nxhtml)

(defun nxhtml-can-insert-page-here ()
   (and (not nxhtml-validation-header-mode)
        (= 1 (point))
       (or (= 0 (buffer-size))
           (save-restriction
             (widen)
             (save-match-data
               (looking-at (rx buffer-start
                               (0+ space)
                               buffer-end)))))))

(defun nxhtml-complete-first-try ()
  (when (nxhtml-can-insert-page-here)
    (nxhtml-empty-page-completion)))

(defun nxhtml-completing-read-tag (prompt
                                  table
                                  &optional predicate require-match
                                  initial-input hist def inherit-input-method)
  (popcmp-completing-read prompt
                          table
                          predicate require-match
                          initial-input hist def inherit-input-method
                          nxhtml-help-tag
                          nxhtml-tag-sets))

(defun nxhtml-add-required-to-attr-set (tag)
  (let ((missing (when tag
                   (rngalt-get-missing-required-attr
                    (nxthml-is-single-tag tag)))))
    (if (not missing)
        nxhtml-attr-sets
      (cons (cons "Required" missing)
            nxhtml-attr-sets))))

(defun nxhtml-get-tag-specific-attr-help (tag)
  (append (cdr (assoc tag nxhtml-help-attribute-name-tag)) nxhtml-help-attribute-name)
  )

(defconst nxhtml-in-start-tag-regex
;;(defconst rng-in-start-tag-name-regex
  (replace-regexp-in-string
   "w"
   xmltok-ncname-regexp
   ;; Not entirely correct since < could be part of attribute value:
   "<\\(w\\(?::w?\\)?\\)+ [^<]*"
   t
   t))

(defun nxhtml-completing-read-attribute-name (prompt
                                              table
                                              &optional predicate require-match
                                              initial-input hist def inherit-input-method)
  (let* ((tag (save-match-data
                ;;(when (looking-back "<\\([a-z1-6]+\\) [^<]*")
                (when (looking-back nxhtml-in-start-tag-regex)
                  (match-string 1))))
         (attr-sets (nxhtml-add-required-to-attr-set tag))
         (help-attr (nxhtml-get-tag-specific-attr-help tag))
         )
    (popcmp-completing-read prompt
                            table
                            predicate require-match
                            initial-input hist def inherit-input-method
                            help-attr
                            attr-sets)))

(defun nxhtml-completing-read-attribute-value (prompt
                                               table
                                               &optional predicate require-match
                                               initial-input hist def inherit-input-method)
  (let (val)
    (if table
        (setq val (popcmp-completing-read prompt table
                                          predicate require-match
                                          initial-input hist def inherit-input-method))
      (let* (init
             delimiter
             (lt-pos (save-excursion (search-backward "<" nil t)))
             (in-attr-val
              (save-excursion
                (re-search-backward rng-in-attribute-value-regex lt-pos t)))
             (in-xml-attr-val
              (unless in-attr-val
                (save-excursion
                  (re-search-backward nxhtml-in-xml-attribute-value-regex lt-pos t))))
         )
        (when (or in-attr-val in-xml-attr-val)
          ;;(save-match-data (save-excursion (re-search-forward "\\=[^<> \t\r\n\"]*" nil t)))
          (let* ((name-start (match-beginning 1))
                 (name-end (match-end 1))
                 (colon (match-beginning 2))
                 (attr (buffer-substring-no-properties name-start
                                                       (or colon name-end)))
                 (value-start (1+ (match-beginning 3)))
                 tag-start-end
                 (tag (save-excursion
                        (when (search-backward-regexp "<[[:alpha:]]+" nil t)
                          (setq tag-start-end (match-end 0))
                          (match-string-no-properties 0)))))
            (setq init (buffer-substring-no-properties value-start (point)))
            (setq delimiter (char-before value-start))
            (if in-xml-attr-val
                (error "in-xml-attr-val should not be true here!")
              ;;             (cond ((string= "encoding" attr)
              ;;                    ;; Give a default that works in browsers today
              ;;                    (setq val (nxhtml-coding-systems-complete
              ;;                               init
              ;;                               (symbol-name nxhtml-default-encoding))))
              ;;                   ((string= "version" attr)
              ;;                    (setq val "1.0")))
              (cond ((string= "rel" attr)
                     (cond ((string= "<link" tag)
                            (setq val (nxhtml-read-link-rel))
                            )))
                    ((string= "media" attr)
                     (cond ((string= "<link" tag)
                            (setq val (nxhtml-read-link-media)))
                           ((string= "<style" tag)
                            (setq val (nxhtml-read-link-media)))
                           ))
                    ((string= "type" attr)
                     (cond ((string= "<link" tag)
                            (setq val (nxhtml-read-link-type))
                            )))
                    ((string= "http-equiv" attr)
                     (cond ((string= "<meta" tag)
                            (setq val (nxhtml-read-meta-http-equiv)))))
                    ((string= "content" attr)
                     (cond ((string= "<meta" tag)
                            (setq val (nxhtml-read-meta-content)))))
                    ((string= "scheme" attr)
                     (cond ((string= "<meta" tag)
                            (setq val (nxhtml-read-meta-scheme)))))
                    ((string= "name" attr)
                     (cond ((string= "<meta" tag)
                            (setq val (nxhtml-read-meta-name)))))
                    ((string= "href" attr)
                     (cond ((string= "<a" tag)
                            (setq val (nxhtml-read-url t init)))
                           ((string= "<base" tag)
                            (setq val (nxhtml-read-url nil init nil "Base")))
                           ((string= "<area" tag)
                            (setq val (nxhtml-read-url nil init)))
                           ((string= "<link" tag)
                            (let (predicate
                                  (here (point)))
                              (save-excursion
                                (goto-char tag-start-end)
                                (cond
                                 ((search-forward "text/css" here nil)
                                  (setq predicate 'nxhtml-css-url-predicate))
                                 ))
                              (setq val (nxhtml-read-url nil init predicate))))
                           (t
                            (setq val (nxhtml-read-url nil init)))))
                    ((string= "src" attr)
                     (cond ((string= "<img" tag)
                            (setq val (nxhtml-read-url nil init 'nxhtml-image-url-predicate "Image")))
                           ((string= "<script" tag)
                            (setq val (nxhtml-read-url nil init 'nxhtml-script-url-predicate "Script")))
                           ((string= "<input" tag)
                            (setq val (nxhtml-read-url nil init 'nxhtml-image-url-predicate "Image")))
                           ((string= "<frame" tag)
                            (setq val (nxhtml-read-url nil init nil "Frame Source")))
                           ((string= "<iframe" tag)
                            (setq val (nxhtml-read-url nil init nil "Frame Source")))
                           (t
                            (setq val (nxhtml-read-url nil init)))))))))))
    ;;(unless val (setq val (read-from-minibuffer prompt init)))
    (if (not val)
        (progn
          (message "No completion of attribute value available here")
          nil)
      val)))

(defun nxhtml-read-link-type ()
  (require 'mailcap)
  (let ((types (when (boundp 'mailcap-mime-extensions)
                 (mapcar (lambda (elt)
                           (cdr elt))
                         mailcap-mime-extensions))))
  (completing-read "Link type: " types nil t)))

(defun nxhtml-read-link-media ()
  (let ((types '(
                 "screen"
                 "tty"
                 "tv"
                 "projection"
                 "handheld"
                 "print"
                 "braille"
                 "aural"
                 "all"
                 )))
    (popcmp-completing-read "For media type: " types nil t)))

(defun nxhtml-read-link-rel ()
  (let ((predefined-linktypes '(
                               "Alternate"
                               "Appendix"
                               "Bookmark"
                               "Chapter"
                               "Contents"
                               "Copyright"
                               "Glossary"
                               "Help"
                               "Index"
                               "Next"
                               "Prev"
                               "Section"
                               "Shortcut Icon"
                               "Start"
                               "Stylesheet"
                               "Subsection"
                               )))
    (popcmp-completing-read "Predefined LinkTypes: " predefined-linktypes nil t)))

(defun nxhtml-read-meta-name ()
  (let ((types '(
                 "author"
                 "description"
                 "keywords"
                 "generator"
                 "revised"
                 ;;"others"
                 )))
    (popcmp-completing-read "Meta name: " types nil t)))

(defun nxhtml-read-meta-content ()
  (nxhtml-read-from-minibuffer "Meta content: "))

(defun nxhtml-read-meta-scheme ()
  (nxhtml-read-from-minibuffer "Meta scheme: "))

(defun nxhtml-read-meta-http-equiv ()
  (let ((types '(
                 "content-type"
                 "expires"
                 "refresh"
                 "set-cookie"
                 )))
    (popcmp-completing-read "Meta http-equiv: " types nil t)))

(when nil
  (setq rngalt-completing-read-tag nil)
  (setq rngalt-complete-last-try nil)
  )


(require 'typesetter nil t)
(when (featurep 'typesetter)
  (defun typesetter-init-nxhtml-mode ()
    (typesetter-init-html-mode))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Validation start state

(defcustom nxhtml-validation-headers
  '(
    ("body-iso-8859-1" .
     "<?xml version=\"1.0\" encoding=\"iso-8859-1\"?>
<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.1//EN\"
\"http://www.w3.org/TR/xhtml11/DTD/xhtml11.dtd\">
<html xmlns=\"http://www.w3.org/1999/xhtml\">
  <head>
    <title>Fictive XHTML Validation Header</title>
  </head>
  <body>
"
     )
    ("head-iso-8859-1" .
     "<?xml version=\"1.0\" encoding=\"iso-8859-1\"?>
<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.1//EN\"
\"http://www.w3.org/TR/xhtml11/DTD/xhtml11.dtd\">
<html xmlns=\"http://www.w3.org/1999/xhtml\">
  <head>
"
     )
    ("html-iso-8859-1" .
     "<?xml version=\"1.0\" encoding=\"iso-8859-1\"?>
<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.1//EN\"
\"http://www.w3.org/TR/xhtml11/DTD/xhtml11.dtd\">
<html xmlns=\"http://www.w3.org/1999/xhtml\">
"
     )
;;     ("doctype-iso-8859-1" .
;;      "<?xml version=\"1.0\" encoding=\"iso-8859-1\"?>
;; <!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.1//EN\"
;; \"http://www.w3.org/TR/xhtml11/DTD/xhtml11.dtd\">
;; "
;;      )
;;     ("xml-iso-8859-1" .
;;      "<?xml version=\"1.0\" encoding=\"iso-8859-1\"?>
;; "
;;      )

    ("body-utf-8" .
     "<?xml version=\"1.0\" encoding=\"utf-8\"?>
<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.1//EN\"
\"http://www.w3.org/TR/xhtml11/DTD/xhtml11.dtd\">
<html xmlns=\"http://www.w3.org/1999/xhtml\">
  <head>
    <title>Fictive XHTML Validation Header</title>
  </head>
  <body>
"
     )
    ("head-utf-8" .
     "<?xml version=\"1.0\" encoding=\"utf-8\"?>
<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.1//EN\"
\"http://www.w3.org/TR/xhtml11/DTD/xhtml11.dtd\">
<html xmlns=\"http://www.w3.org/1999/xhtml\">
  <head>
"
     )
    ("head-closed-utf-8" .
     "<?xml version=\"1.0\" encoding=\"utf-8\"?>
<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.1//EN\"
\"http://www.w3.org/TR/xhtml11/DTD/xhtml11.dtd\">
<html xmlns=\"http://www.w3.org/1999/xhtml\">
  <head>
    <title></title>
  </head>
"
     )
    ("html-utf-8" .
     "<?xml version=\"1.0\" encoding=\"utf-8\"?>
<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.1//EN\"
\"http://www.w3.org/TR/xhtml11/DTD/xhtml11.dtd\">
<html xmlns=\"http://www.w3.org/1999/xhtml\">
"
     )
;;     ("doctype-utf-8" .
;;      "<?xml version=\"1.0\" encoding=\"utf-8\"?>
;; <!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.1//EN\"
;; \"http://www.w3.org/TR/xhtml11/DTD/xhtml11.dtd\">
;; "
;;      )
;;     ("xml-utf-8" .
;;      "<?xml version=\"1.0\" encoding=\"utf-8\"?>
;; "
;;      )
    )
  "Fictive XHTML validation headers.
Used by `nxhtml-set-validation-header'."
  :type '(alist :key-type string :value-type string)
  :group 'nxhtml)

(defcustom nxhtml-default-validation-header nil
  "Default Fictive XHTML validation header.
Must be nil or one of the key values in
`nxhtml-validation-headers'."
  :type 'string
  :set (lambda (sym val)
         (if (or (null val)
                 (assoc val nxhtml-validation-headers))
             (set-default sym val)
           (lwarn 'nxhtml-default-validation-header
                  :warning "There is no Fictive XHTML Validation Header named %s" val)))
  :group 'nxhtml)

(defun nxhtml-must-have-validation-headers ()
  (unless nxhtml-validation-headers
    (error
     "No XHTML validation headers. Please customize nxhtml-validation-headers.")))

(defvar nxhtml-set-validation-header-hist nil)

(defvar nxhtml-current-validation-header nil)
(make-variable-buffer-local 'nxhtml-current-validation-header)
(put 'nxhtml-current-validation-header 'permanent-local t)

(defcustom nxhtml-guess-validation-header-alist
  ;;(rx line-start (0+ blank) "<body")
  '(
    ("^[[:blank:]]*<body"   . "body-utf-8")
    ("^[[:blank:]]*</head>" . "head-closed-utf-8")
    ("^[[:blank:]]*<head"   . "head-utf-8")
    ("^[[:blank:]]*<html"   . "html-utf-8")
    )
  "Alist used by `nxhtml-guess-validation-header'.
Alternatives are tried from top to bottom until one fits."
  :type '(alist :key-type (regexp :tag "If NOT found in buffer")
                :value-type (string :tag "Use Fictive XHTML Validation Header"))
  :group 'nxhtml)

(defun nxhtml-guess-validation-header ()
  "Return Fictive XHTML validation that could fit current buffer.
This guess is made by matching the entries in
`nxhtml-guess-validation-header-alist' against the buffer."
  (nxhtml-must-have-validation-headers)
  (save-excursion
    (save-restriction
      (save-match-data
        (widen)
        (let (rec
              regexp
              key
              (guesses nxhtml-guess-validation-header-alist))
          (goto-char (point-min))
          (if (not (search-forward "</" 2000 t))
              (progn
                (setq rec (car guesses))
                (setq key (cdr rec)))
            (while (and guesses
                        (not key))
              (setq rec (car guesses))
              (setq guesses (cdr guesses))
              (setq regexp (car rec))
              (goto-char (point-min))
              ;; Fix-me: check for chunk and check if in string.
              (let (found)
                (while (and (not found)
                            (re-search-forward regexp nil t))
                  ;; ensure fontified, but how?
                  (when mumamo-multi-major-mode
                    (let ((mumamo-just-changed-major nil))
                      (unless (and (mumamo-get-existing-chunk-at (point))
                                   (eq t (get-text-property (point) 'fontified)))
                        (mumamo-fontify-region (point-min) (+ 1000 (point))))))
                  (unless (memq (get-text-property (point) 'face)
                                '(font-lock-comment-face
                                  font-lock-comment-delimiter-face
                                  font-lock-doc-face
                                  font-lock-string-face
                                  ))
                    (setq found t)))
                (unless found
                  (setq key (cdr rec))))))
          ;;(unless (re-search-forward regexp nil t) (setq key (cdr rec)))))
          key)))))

(defun nxhtml-open-dir-saved-validation-headers (must-exist)
  "Open file with saved validation headers and return buffer."
  ;;(lwarn 't :warning "must-exist=%s" must-exist)
  (when (buffer-file-name)
    (let* ((dir-name (file-name-directory (buffer-file-name)))
           (file-name (expand-file-name "nxhtml-val-headers.el"))
           emacs-lisp-mode-hook)
      (when (or (not must-exist)
                (file-exists-p file-name))
        (find-file-noselect file-name)))))

(defun nxhtml-get-saved-validation-header ()
  (when (buffer-file-name)
    (let* ((val-buf (nxhtml-open-dir-saved-validation-headers t))
           (file-name (file-name-nondirectory (buffer-file-name)))
           validation-headers)
      (when val-buf
        (with-current-buffer val-buf
          (eval-buffer))
        (cadr (assoc file-name validation-headers))))))

(defun nxhtml-remove-saved-validation-header ()
  "Removed the saved validation header.
Reverse the action done by `nxhtml-save-validation-header'."
  (interactive)
  (nxhtml-update-saved-validation-header nil))

(defun nxhtml-save-validation-header ()
  "Save the current validation header.
The current validation is saved for the next time you open the
current file.  It is then used by `nxhtml-validation-header-mode'
and `nxhtml-set-validation-header'. This means that if you have
turned on `nxhtml-global-validation-header-mode' this validation
header will be set automatically.

The saved validation header can be removed with
`nxhtml-remove-saved-validation-header'.

* Note: There is normally no need to save the validation headers
  since `nxhtml-global-validation-header-mode' will add
  validation headers as needed most of the time."
  (interactive)
  (nxhtml-update-saved-validation-header t))

(defun nxhtml-update-saved-validation-header (save)
  (unless (buffer-file-name)
    (error "Validation Header can only be saved if buffer contains a file."))
  (let* ((val-buf (nxhtml-open-dir-saved-validation-headers nil))
          ;;(get-buffer-create "temp val head"))
         validation-headers
         (file-name (file-name-nondirectory (buffer-file-name)))
         (entry (list file-name nxhtml-current-validation-header))
         ;;entry-list
         removed
         )
    ;; Get old headers
    (with-current-buffer val-buf
      (eval-buffer))
    ;; Remove old value
    (setq validation-headers
          (delq nil
                (mapcar (lambda (elt)
                          (if (string= file-name (car elt))
                              (progn
                                (setq removed t)
                                nil)
                            elt))
                        validation-headers)))
    ;; Add new value
    (when save
      (setq validation-headers (cons entry validation-headers)))
    (with-current-buffer val-buf
      (erase-buffer)
      ;;(print file-name val-buf)
      ;;(print nxhtml-current-validation-header val-buf)
      ;;(print entry val-buf)
      (insert "(setq validation-headers (quote")
      (print validation-headers val-buf)
      (insert "))")
      (basic-save-buffer)
      )
    (if save
        (message "Current validation header for file saved")
      (if removed
          (message "Removed saved validation header")
        (message "There was no saved validation header")))))

(defun nxhtml-get-default-validation-header ()
  "Return default Fictive XHTML validation header key for current buffer.
If `nxhtml-default-validation-header' is non-nil then return
this.  Otherwise return saved validation header if there is one
or guess using `nxhtml-guess-validation-header'."
  (or nxhtml-default-validation-header
      (nxhtml-get-saved-validation-header)
      (nxhtml-guess-validation-header)))

(defun nxhtml-set-validation-header (&optional key)
  "Set a Fictive XHTML validation header in the buffer.
Such a header is not inserted in the buffer, but is only used by
validation and XHTML completion by `nxhtml-mode'.

The header is active for validation and completion if and only if
`nxhtml-validation-header-mode' is on.

Note that Fictive XHTML Validation Headers are normally chosen
automatically, but you can use this function to override that choice.

The header is chosen from `nxhtml-validation-headers'. If there
is more than one you will be prompted. To set the default fictive
XHTML validation header customize `nxhtml-validation-headers'.

If called non-interactive then the header corresponding to key
KEY will be used.  If KEY is nil then it is set to
`nxhtml-default-validation-header'.

This header can be visible or invisible in the buffer, for more
information see `rngalt-show-validation-header'."
  (interactive
   (list
    (let ((nh (length nxhtml-validation-headers))
          (default (nxhtml-get-default-validation-header)))
      (if (> nh 1)
          (completing-read "XHTML validation header: "
                           nxhtml-validation-headers
                           nil
                           t
                           default
                           nxhtml-set-validation-header-hist)
        (if (not (y-or-n-p "Only one XHTML validation header is defined. Define more? "))
            default
          (customize-option 'nxhtml-validation-headers)
          'adding)))))
  ;;(lwarn 'svh2 :warning "key=%s" key)
  (or key
      (setq key (nxhtml-get-default-validation-header))
      (setq key (cons 'schema "XHTML")))
  (unless (eq key 'adding)
    (setq nxhtml-current-validation-header key)
    (nxhtml-validation-header-mode 1)
    (nxhtml-apply-validation-header)))

(defun nxhtml-apply-validation-header ()
  (when nxhtml-current-validation-header
    (setq rngalt-major-mode
          (if mumamo-multi-major-mode
              (mumamo-main-major-mode)
            major-mode))
    (let* ((key nxhtml-current-validation-header)
           (rec (unless (listp key)
                  (assoc key nxhtml-validation-headers)))
           (header (cdr rec)))
      (if (listp key)
          (let ((schema-file (rng-locate-schema-file (cdr key))))
            (unless schema-file
              (error "Could not locate schema for type id `%s'" key)) ;type-id))
            (rng-set-schema-file-1 schema-file))
        (rngalt-set-validation-header header)
        ))))

(defun nxhtml-update-validation-header ()
  "Update the validation header in the buffer as needed."
  (interactive)
  (let ((mode-on nxhtml-validation-header-mode))
    (when mode-on (nxhtml-validation-header-mode 0))
    (setq nxhtml-current-validation-header nil)
    (when mode-on (nxhtml-validation-header-mode 1))))

;;;###autoload
(define-minor-mode nxhtml-validation-header-mode
  "If on use a Fictive XHTML Validation Header for the buffer.
See `nxhtml-set-validation-header' for information about Fictive XHTML Validation Headers.

This mode may be turned on automatically in two ways:
- If you try to do completion of a XHTML tag or attribute then
  `nxthml-mode' may ask you if you want to turn this mode on if
  needed.
- You can also choose to have it turned on automatically whenever
  mumamo is used, see `nxhtml-validation-header-if-mumamo' for
  further information."
  :global nil
  :lighter " VH"
  :group 'nxhtml
  (if nxhtml-validation-header-mode
      (progn
        (unless nxhtml-current-validation-header
          (setq nxhtml-current-validation-header
                (nxhtml-get-default-validation-header)))
        ;;(message "nxhtml-current-validation-header=%s" nxhtml-current-validation-header)
        (if nxhtml-current-validation-header
            (progn
              (nxhtml-apply-validation-header)
              (add-hook 'change-major-mode-hook 'nxhtml-vhm-change-major nil t)
              (when (featurep 'mumamo)
                (add-hook 'mumamo-change-major-mode-hook 'nxhtml-vhm-mumamo-change-major nil t)
                (add-hook 'mumamo-after-change-major-mode-hook 'nxhtml-vhm-mumamo-after-change-major nil t)))
          (run-with-idle-timer 0 nil 'nxhtml-validation-header-empty (current-buffer))))
    (rngalt-set-validation-header nil)
    (setq nxhtml-current-validation-header nil)
    (remove-hook 'after-change-major-mode-hook 'nxhtml-vhm-after-change-major t)
    (when (featurep 'mumamo)
      (remove-hook 'mumamo-change-major-mode-hook 'nxhtml-vhm-mumamo-change-major t)
      (remove-hook 'mumamo-after-change-major-mode-hook 'nxhtml-vhm-mumamo-after-change-major t))))

(defun nxhtml-vhm-change-major ()
  "Turn off `nxhtml-validation-header-mode' after change major."
  ;;(message "nxhtml-vhm-change-major here")
  (unless mumamo-multi-major-mode
    (setq nxhtml-current-validation-header nil))
  (run-with-idle-timer 0 nil 'nxhtml-validation-header-empty (current-buffer)))
(put 'nxhtml-vhm-change-mode 'permanent-local-hook t)

(defun nxhtml-recheck-validation-header ()
  "Just turn off and on again `nxhtml-validation-header-mode'.
This will adjust the XHTML validation to the code currently in
the buffer."
  (interactive)
  (nxhtml-validation-header-mode -1)
  (nxhtml-validation-header-mode 1))

(defun nxhtml-validation-header-empty (buffer)
  "Turn off validation header mode.
This is called because there was no validation header."
  (with-current-buffer buffer
    (unless nxhtml-current-validation-header
      ;;(message "nxhtml-validation-header-empty")
      (nxhtml-validation-header-mode -1)
      ;;(message "No validation header was needed")
      )))

(defun nxhtml-turn-on-validation-header-mode ()
  "Turn on `nxhtml-validation-header-mode'."
  (nxhtml-validation-header-mode 1))

;;(defvar nxhtml-browseable-buffer-name "*nXhtml Browsing Buffer*")
(defvar nxhtml-browseable-buffer-file "~/.temp-nxhtml-browse.htm")
;; Fix-me: Handle base href here!
(defun nxhtml-save-browseable-temp-file (start end &optional doit-anyway)
  "Return a temporary file for viewing in web browser."
  ;; When using this either region should be active or there should be
  ;; a validation header or both.
  (or doit-anyway
      (and start end) ;mark-active
      (and nxhtml-validation-header-mode
           nxhtml-current-validation-header)
      (error "Neither region nor validation header"))
  (save-excursion
    (let ((curbuf (current-buffer))
          (view-buffer (find-file-noselect nxhtml-browseable-buffer-file))
          header
          content)
      ;; Get header and content
      (save-restriction
        (widen)
        (setq header
              (if nxhtml-validation-header-mode
                  (let* ((key nxhtml-current-validation-header)
                         (rec (unless (listp key)
                                (assoc key nxhtml-validation-headers)))
                         (header (cdr rec)))
                    header)
                (goto-char (point-min))
                (save-match-data
                  (let ((body (re-search-forward "<body[^>]*>")))
                    (if body
                        (buffer-substring-no-properties (point-min) (match-end 0))
                      "")))))
        (setq content
              (if start
                  (buffer-substring-no-properties start end)
                (buffer-substring-no-properties (point-min) (point-max))))
        )
      ;; Switch to view buffer
      (set-buffer view-buffer)
;;       (unless buffer-file-name
;;         (set-visited-file-name nxhtml-browseable-buffer-file)
;;         (rename-buffer nxhtml-valhead-view-buffer-name))
      (erase-buffer)
      (insert header content)
      ;;(when (fboundp 'emacsw32-eol-set) (emacsw32-eol-set nil))
      (nxhtml-mode)
      (save-buffer)
      ;;(current-buffer)
      (kill-buffer view-buffer)
      (expand-file-name nxhtml-browseable-buffer-file)
      )))


(defun nxhtml-vhm-mumamo-change-major ()
  (put 'rngalt-validation-header 'permanent-local t)
  (put 'nxhtml-validation-header-mode 'permanent-local t)
  (put 'nxhtml-current-validation-header 'permanent-local t)
  (put 'nxhtml-validation-header-mode-major-mode 'permanent-local t)
  (setq nxhtml-validation-header-mode-major-mode mumamo-set-major-running)
  )

(defun nxhtml-vhm-mumamo-after-change-major ()
  (put 'rngalt-validation-header 'permanent-local nil)
  (put 'nxhtml-validation-header-mode 'permanent-local nil)
  (put 'nxhtml-current-validation-header 'permanent-local nil)
  (put 'nxhtml-validation-header-mode-major-mode 'permanent-local nil)
  )

(defcustom nxhtml-validation-headers-check 'html
  "Defines what check the function with the same name does.
The function returns true if the condition here is met."
  :type '(choice :tag "Add Fictive XHTML Validation Header if:"
                (const :tag "If buffer contains html" html)
                (const :tag "If buffer contains html or is empty" html-empty))
  :group 'nxhtml)

;; (defun nxhtml-validation-headers-check (buffer)
;;   "Return non-nil if buffer contains a html tag or is empty.
;; This is for use with `nxhtml-validation-header-filenames'.

;; The variable `nxhtml-validation-headers-check' determines how the
;; check is made."
;;   (if (= 0 (buffer-size buffer))
;;       (eq 'html-empty nxhtml-validation-headers-check)
;;     (save-match-data
;;       (save-restriction
;;         (let ((here (point))
;;               (html nil))
;;           (goto-char (point-min))
;;           (setq html (re-search-forward "</?[a-z]+>" nil t))
;;           (goto-char here)
;;           html)))))

;; (defcustom nxhtml-validation-header-filenames
;;   '(
;;     ("\.php\\'" nxhtml-validation-headers-check)
;;     ("\.rhtml\\'" nxhtml-validation-headers-check)
;;     ("\.jsp\\'" nxhtml-validation-headers-check)
;;     )
;;   "Alist for turning on `nxhtml-validation-mode'.
;; The entries in the list should have the form

;;   \(FILE-REGEXP CHECK-FUNCION)

;; If buffer file name matches the regexp FILE-REGEXP and the
;; function CHECK-FUNCTION returns non-nil when called with the
;; buffer as an argument \(or CHECK-FUNCTION is nil) then
;; `nxhtml-global-validation-header-mode' will turn on
;; `nxhtml-validation-header-mode' in buffer.

;; The function `nxhtml-validation-headers-check' may be a useful
;; value for CHECK-FUNCTION.

;; See also `nxhtml-maybe-turn-on-validation-header'."
;;   :type '(alist :key-type regexp :tag "File name regexp"
;;                 :value-type (group (choice (const :tag "No more check" nil)
;;                                            (function :tag "Check buffer with"))))
;;   :group 'nxhtml)



;; (defun nxhtml-maybe-turn-on-validation-header ()
;;   "Maybe turn on `nxhtml-validation-header-mode' in buffer.
;; This is called by `nxhtml-global-validation-header-mode'.

;; See `nxhtml-validation-header-filenames' for how the check
;; is made."
;;   (or (and (or (and mumamo-mode
;;                     (eq (mumamo-main-major-mode) 'nxhtml-mode))
;;                (eq major-mode 'nxhtml-mode))
;;            rngalt-validation-header
;;            nxhtml-current-validation-header
;;            nxhtml-validation-header-mode
;;            (progn
;;              ;;(lwarn 'maybe :warning "quick, buffer=%s" (current-buffer))
;;              (nxhtml-validation-header-mode 1)
;;              t))
;;       (when (buffer-file-name)
;;         (unless (or ;;nxhtml-validation-header-mode
;;                  (minibufferp (current-buffer))
;;                  (string= " " (substring (buffer-name) 0 1))
;;                  (string= "*" (substring (buffer-name) 0 1))
;;                  )
;;           (when (catch 'turn-on
;;                   (save-match-data
;;                     (dolist (rec nxhtml-validation-header-filenames)
;;                       (when (string-match (car rec) (buffer-file-name))
;;                         (let ((fun (nth 1 rec)))
;;                           (if (not fun)
;;                               (progn
;;                                 ;;(lwarn 't :warning "matched %s to %s, nil" (car rec) (buffer-file-name))
;;                                 (throw 'turn-on t))
;;                             (when (funcall fun (current-buffer))
;;                               ;;(lwarn 't :warning "matched %s to %s" (car rec) (buffer-file-name))
;;                               (throw 'turn-on t))))))))
;;             ;;(lwarn 't :warning "turn on %s, buffer=%s" major-mode (current-buffer))
;;             (nxhtml-validation-header-mode 1))))))


;; ;; Fix-me: Is this really the way to do it? Would it not be better to
;; ;; tie this to mumamo-mode in the turn on hook there? After all
;; ;; validation headers are probably not used unless mumamo-mode is on.
;; (define-globalized-minor-mode nxhtml-global-validation-header-mode
;;   nxhtml-validation-header-mode
;;   nxhtml-maybe-turn-on-validation-header
;;   :group 'nxhtml)
;; ;; The problem with global minor modes:
;; (when (and nxhtml-global-validation-header-mode
;;            (not (boundp 'define-global-minor-mode-bug)))
;;   (nxhtml-global-validation-header-mode 1))


(defcustom nxhtml-validation-header-mumamo-modes
  '(nxhtml-mode)
  "Main major modes for which to turn on validation header.
Turn on Fictive XHTML Validation Header if main major mode for the
used mumamo chunk family is any of those in this list.

See `mumamo-set-chunk-family' for information about mumamo chunk
families."
  :type '(repeat (function :tag "Main major mode in mumamo"))
  :group 'nxhtml)

(defun nxhtml-add-validation-header-if-mumamo ()
  "Maybe turn on validation header.
See `nxhtml-validation-header-if-mumamo' for more information."
  ;;(nxhtml-validation-headers-check (current-buffer))
  (when (memq (mumamo-main-major-mode) nxhtml-validation-header-mumamo-modes)
    (nxhtml-validation-header-mode 1)))

(define-toggle nxhtml-validation-header-if-mumamo nil
  "Add a fictive validation header when mumamo is used.
If this variable is t then add a Fictive XHTML Validation Header
\(see `nxhtml-validation-header-mode') in buffer when mumamo is
used. However do this only if `mumamo-main-major-mode' is one of
those in `nxhtml-validation-header-mumamo-modes'.

Changing this variable through custom adds/removes the function
`nxhtml-add-validation-header-if-mumamo' to
`mumamo-turn-on-hook'."
  :set '(lambda (sym val)
          (set-default sym val)
          (if val
              (add-hook 'mumamo-turn-on-hook 'nxhtml-add-validation-header-if-mumamo)
            (remove-hook 'mumamo-turn-on-hook 'nxhtml-add-validation-header-if-mumamo)))
  :group 'nxhtml)

(defun nxhtml-warnings-are-visible ()
  (get 'rng-error 'face))

(defvar nxhtml-old-rng-error-face nil)
(defun nxhtml-toggle-visible-warnings ()
  "Toggle the red underline on validation errors.
Those can be quite disturbing when using mumamo because there
will probably be many validation errors in for example a php
buffer, since unfortunately the validation routines in
`rng-validate-mode' from `nxml-mode' tries to validate the whole
buffer as XHTML.

Also, because of a \(normally unimportant) bug in Emacs 22,
the red underline that marks an error will sometimes span several
lines instead of just marking a single character as it
should. \(This bug is a problem with overlays in Emacs 22.)"
  (interactive)
  (let ((face (get 'rng-error 'face)))
    (if face
        (progn
          (setq nxhtml-old-rng-error-face (get 'rng-error 'face))
          (put 'rng-error 'face nil))
      (put 'rng-error 'face nxhtml-old-rng-error-face))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Bug corrections
;; (defun nxml-indent-line ()
;;   "Indent current line as XML."
;;   (let ((indent (nxml-compute-indent))
;;         (from-end (- (point-max) (point))))
;;     (when indent
;;       (beginning-of-line)
;;       (let ((bol (point)))
;;         (skip-chars-forward " \t")
;;         ;; There is a problem with some lines, try a quick fix:
;;         (when (and (= 0 indent)
;;                    (not (eq (char-after) ?<)))
;;           (save-excursion
;;             (save-match-data
;;               (when (re-search-backward "^<" nil t)
;;                 (when (search-forward " ")
;;                   (setq indent (current-column))))))
;;           (when (= 0 indent)
;;             (setq indent nxml-child-indent)))
;;         ;; And sometimes nxml-compute-indent get very upset, check for
;;         ;; that:
;;         (let ((here (point)))
;;           (beginning-of-line 0)
;;           (back-to-indentation)
;;           (when (and (= indent (current-column))
;;                      (eq (char-after) ?\"))
;;             (setq indent 0))
;;           (goto-char here))
;;         (unless (= (current-column) indent)
;;           (delete-region bol (point))
;;           (indent-to indent)))
;;       (when (> (- (point-max) from-end) (point))
;;         (goto-char (- (point-max) from-end))))))


;; FIX-ME: untag should be in nxml-mode.el since it is in no way
;; specific to nxhtml-mode, but I do not want to change nxml-mode.el
;; at the moment.

(defcustom nxml-untag-select 'yes
  "Decide whether to select an element untagged by `nxml-untag-element'.
If this variable is 'yes the element is selected after untagging
the element. The mark is set at the end of the element and point
at the beginning of the element.

If this variable is 'no then the element is not selected and
point is not moved. If it is 'ask the user is asked what to do."
  :type '(choice (const :tag "Yes" yes)
                 (const :tag "No" no)
                 (const :tag "Ask" ask))
  :group 'nxml)

(defun nxml-untag-element (arg)
  "Remove start and end tag from current element.
The mark is by default set to the end of the former element and
point is moved to the beginning. Mark is also activated so that
it is easy to surround the former element with a new tag.

Whether to select the old element is controlled by
`nxml-untag-select'. The meaning of the values 'yes and 'no for
this variable is flipped by using a universal argument.

Note: If you want to `undo' the untag and you use
`transient-mark-mode' then you must first do something so that
the region is not highlighted (for example C-g)."
  (interactive "*P")
  (let ((here (point-marker))
        el-start
        el-start-end
        el-end
        el-end-end
        (select t))
    (nxml-backward-up-element)
    (setq el-start (point))
    (nxml-forward-balanced-item)
    (setq el-start-end (point))
    (goto-char el-start)
    (nxml-forward-element)
    (setq el-end-end (point-marker))
    (nxml-backward-single-balanced-item)
    (setq el-end (point))
    (delete-region el-end el-end-end)
    (delete-region el-start el-start-end)
    ;; Select the element or not?
    (if (eq nxml-untag-select 'ask)
        (setq select (y-or-n-p "Select the old element? "))
      (when (eq nxml-untag-select 'no)
        (setq select nil))
      (when arg
        (setq select (not select))))
    (if (not select)
        (goto-char here)
      (goto-char el-end-end)
      (push-mark nil t t)
      (setq mark-active t)
      (setq deactivate-mark nil)
      (goto-char el-start))))

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
      (replace-match date-str nil nil nil 1))
    (goto-char here)))

(defun nxhtml-rollover-insert-2v ()
  "Insert CSS rollover images.
The upper half of the image will be used when mouse is out and
the lower half when mouse is over the image.

Only CSS is used for the rollover. The CSS code is written to the
header part of the file if possible, otherwise it is copied to
the kill ring/clipboard.

The CSS code is built from a template file and the image size.

This might be used for example for creating a menu with
alternatives vertically or horizontally.

Usage example:

  If you want to make a small button style menu with images you
  can start like this:

      <div id=\"mylinks\">
        <ul>
          <li>
     X      <a href=\"news.html\">News and Notes</a>
          </li>
          <li>
            <a href=\"doc.html\">Documentation</a>
          </li>
        <ul>
      </div>

  Then put point at the X above (this is just a mark, should not
  be in your code) and call this function.

  It will add some CSS code to in the header of your file. You
  may want to tweak this a little bit, see below (or place it
  somewhere else). It may look like this:

    #mylinks a {
        /* Image */
        display: block;
        background: transparent url(\"img/mybutton.png\") 0 0 no-repeat;
        overflow: hidden;
        width: 200px;
        /* Text placement and size, etc */
        text-align: center;
        /* You may need to change top and bottom padding depending
           on font size. */
        padding-top: 11px;
        font-size: 12px;
        padding-bottom: 9px;
        text-decoration: none;
        white-space: nowrap;
        border: none;
    }
    #mylinks a:hover {
        background-position: 0 -35px;
    }
    #mylinks li {
        display: inline;
        padding: 0;
        margin: 0;
        float: none;
    }

For an example of usage see the file nxhtml.html that comes with
nXhtml and can be opened from the nXhtml menu under

  nXhtml / nXhtml Help and Setup / nXhtml version nn Overview"
  (interactive)
;; Fix-me: not quite ready yet, but should work OK."
  (save-excursion
    (let* ((tag (progn
                  (search-forward ">" nil t)
                  (unless (re-search-backward (rx "<"
                                                  (1+ (any "a-zA-Z:"))
                                                  (1+ (not (any ">")))
                                                  " id=\""
                                                  (submatch (+? anything))
                                                  "\"")
                                              nil t)
                    (error "Can't find tag with id backwards"))
                  (match-string-no-properties 0)))
           (tagid (match-string-no-properties 1))
           (tagovl (let ((ovl (make-overlay
                               (match-beginning 0) (match-end 0))))
                     (overlay-put ovl 'face 'highlight)
                     ovl))
           (head-end (save-excursion (search-backward "</head" nil t))))
      (unless head-end
        (error "Can't find end of head tag. Need this to insert css."))
      (sit-for 1)
      (unwind-protect
          (condition-case err
              (let* ((img-src (nxhtml-read-url
                           '(?f) nil 'nxhtml-image-url-predicate
                           (concat "Rollover image for \"" tag "\",")))
                     (img-sizes (when (file-exists-p img-src)
                                  (image-size (create-image
                                               (expand-file-name img-src))
                                              t)))
                     (class (read-string
                             (concat
                              "Class name for rollover (empty to use id="
                              tagid "): ")))
                     (rollover-spec (if (< 0 (length class))
                                        (concat "." class)
                                      (concat "#" tagid)))
                     img-width img-height
                     img-h2
                     img-w2
                     padding-top
                     padding-bottom
                     (font-size (read-number "Font size (px): " 12))
                     (css-template-file (read-file-name
                                         "CSS template file: "
                                         (expand-file-name
                                          "../etc/templates/"
                                          nxhtml-src-dir)
                                         nil
                                         t
                                         "rollover-2v.css"
                                         ))
                     (center-or-pad
                      (if (y-or-n-p "Do you want to center the text? ")
                          "text-align: center"
                        (format "padding: %spx" (/ font-size 2))))
                     (hor-or-ver
                      (if (y-or-n-p "Do you want the alternatives shown in a vertical list? ")
                          "float: none"
                        "float: left"))
                     (css-template-buffer (find-file-noselect
                                           css-template-file))
                     (css-template (with-current-buffer css-template-buffer
                                     ;; Do not widen, let user decide.
                                     (buffer-substring-no-properties
                                      (point-min) (point-max))))
                     (css css-template))
                (unless (file-exists-p css-template-file)
                  (error "Can't find file %s" css-template-file))
                (if img-sizes
                    (progn
                      (setq img-width (car img-sizes))
                      (setq img-height (cdr img-sizes)))
                  (setq img-width (read-number "Width: "))
                  (setq img-height (read-number "Width: ")))
                (setq img-h2 (/ img-height 2))
                (setq img-w2 (/ img-width 2))
                (setq padding-top (/ (- img-h2 font-size) 2))
                ;; Fix-me: I have no idea why I have to subtract 3
                ;; from bottom, but inspection with Firebug seems to
                ;; say so:
                (setq padding-bottom (- img-h2 padding-top font-size 3))
                (setq css (replace-regexp-in-string "ROLLOVER_SPEC" rollover-spec css t t))
                (setq css (replace-regexp-in-string "IMG_WIDTH_2" (number-to-string img-h2) css t t))
                (setq css (replace-regexp-in-string "IMG_HEIGHT_2" (number-to-string img-h2) css t t))
                (setq css (replace-regexp-in-string "IMG_WIDTH" (number-to-string img-width) css t t))
                (setq css (replace-regexp-in-string "IMG_HEIGHT" (number-to-string img-height) css t t))
                (setq css (replace-regexp-in-string "IMG_URL" img-src css t t))
                (setq css (replace-regexp-in-string "FONT_SIZE" (number-to-string font-size) css t t))
                (setq css (replace-regexp-in-string "PADDING_TOP" (number-to-string padding-top) css t t))
                (setq css (replace-regexp-in-string "PADDING_BOTTOM" (number-to-string padding-bottom) css t t))
                (setq css (replace-regexp-in-string "CENTER_OR_PAD" center-or-pad css t t))
                (setq css (replace-regexp-in-string "HOR_OR_VER" hor-or-ver css t t))
                (if head-end
                    (let ((this-window (selected-window)))
                      (find-file-other-window buffer-file-name)
                      (goto-char head-end)
                      (beginning-of-line)
                      (insert "<style type=\"text/css\">\n"
                              css
                              "\n</style>\n")
                      (select-window this-window))
                  (kill-new css)
                  (message "No place to insert CSS, copied to clipboard instead"))))
        (delete-overlay tagovl)
        ))))

;; Fix-me: image border 0
;; Fix-me: SSI <!--#include file="file:///C|/EmacsW32/nxml/nxhtml/bug-tests/bug-080609.html" -->
;; Fix-me: Better a tag completion, target etc.
;; Fix-me: image map - is that possible now?
;; Fix-me: Special chars - completing on &? Or popup? Use nxml-insert-named-char
;; Fix-me: Quick table insert? A form?
;; Fix-me: Quick object insert? (applet is depreceated)
;; Fix-me: Better meta insert? Quick meta?
;; Fix-me: Quick div! Better div completion with position: static,
;;         relative, absolute and fixed - with some explanations.
;; Fix-me: Quick hr?
;; Fix-me: Import CSS? Export CSS?
;; Fix-me: Use nxhtml-js.el?
;; Fix-me: Scroll bar colors etc? See 1stPage.
;;   body {
;;     scrollbar-arrow-color: #FF6699;
;;     scrollbar-3dlight-color: #00FF33;
;;     scrollbar-highlight-color: #66FFFF;
;;     scrollbar-face-color: #6699FF;
;;     scrollbar-shadow-color: #6633CC;
;;     scrollbar-darkshadow-color: #660099;
;;     scrollbar-track-color: #CC6633;
;;     }
;; Fix-me: Built with nXhtml button!
;; Fix-me: More quick menus: http://www.cssplay.co.uk/menus/

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'nxhtml)

;;; nxhtml.el ends here
