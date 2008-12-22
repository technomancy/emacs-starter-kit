;;; xmlpe.el --- Xml Part Edit - edit fragmens of XML using nxml-mode

;; Copyright (C) 2005, 2006, 2007 by Lennart Borgman

;; Author:  Lennart Borgman <lennart DOT borgman DOT 073 AT student DOT lu DOT se>
;; Created: 2005-06-17
(defconst xmlpe:version "0.58") ;; Version:
;; Last-Updated: Tue Feb 27 12:06:07 2007 (3600 +0100)
;; Keywords: languages
;; -Features that might be required by this library:
;;
;;   `browse-url', `cl'.
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; This file contains a minor mode `xmlpe-mode' that can be used to
;; edit part of an XML file, but still use the DTD.  This is
;; accomplished by adding a header and a footer so that the part to
;; edit is in the correct XML context.  When nxml-mode is used as the
;; major mode syntax can now be checked.
;;
;; You can however use any major mode together with `xmlpe-mode'.
;; When saving the file the header and footer part is not saved.
;;
;; The header and the footer XML parts can be customized.  Default
;; values are choosen so that the XML context is inside the <body>-tag
;; in an XHTML document.
;;
;; To use xmlpe.el in your .emacs or elsewhere enter
;;
;;    (require 'xmlpe)
;;
;; Then open the file with the XML fragment, choose major mode and
;; then enter M-x xmlpe-mode.
;;
;; Alternatively if you use a special file names (or extensions) you
;; can use `auto-mode-alist' together with `xmlpe-mode-alist' to
;; automatically set major mode and enter `xmlpe-mode'.
;;
;;    (add-to-list 'auto-mode-alist  '("\\.htmlf" . xmlpe-auto-mode))
;;    (xmlpe-set-mode-alist-entry '("\\.htmlf" . nxhtml-mode))
;;
;; or even
;;
;;    (add-to-list 'auto-mode-alist  '("\\.htmlf" . xmlpe-auto-mode))
;;    (xmlpe-set-mode-alist-entry '("\\.htmlf" nxhtml-mode nil
;;                                  "xhtml-iso-8859-1"))
;;
;; The first form will open files with extension .htmlf in
;; `nxhtml-mode' and set the minor mode `xmlpe-mode'. The second form
;; also chooses what header-footer to use in `xmlpe-mode'.
;;
;; ***Note***: If you use this for XHTML (blogging for example) you
;; may want to put the file xmlpe.css in the same directory as
;; xmlpe.el.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; History:
;;
;; 2005-12-25: Tried to use correct coding system for write.
;; 2005-12-27: Better view functions.
;;             Saving does not screw up undos now.
;; 2006-01-23: Added `xmlpe-auto-mode'. Removed several functions.
;;             Removed `xmlpe-extension-list'.
;;             Added `xmlpe-mode-alist'.
;;             New way to choose mode, more like `auto-mode-alist'.
;; 2006-03-27: Corrected bugs in use of `xmlpe-mode-alist'.
;;
;; 2007-02-26: Removed 'intangible property. I believe this caused the
;;             fontification to loop and it was not used any more.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This file is not part of Emacs

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(require 'browse-url)
(eval-when-compile (require 'cl))
(eval-when-compile
  (unless (featurep 'nxml-nxhtml-autostart)
    (let ((efn (expand-file-name "../autostart.el")))
      (load efn))
    (require 'rng-valid)
    (require 'nxml-mode)
    ;;(require 'rng-nxml)
    ))


(defgroup xmlpe nil
  "Customization group for xmlpe (Xml Part Editor)."
  :group 'languages)

(defcustom xmlpe-header-footer-alist
  '(
    ;;;;;; XHTML iso-8859-1
    ("xhtml-iso-8859-1"
    "<?xml version=\"1.0\" encoding=\"iso-8859-1\"?>
<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.1//EN\"
\"http://www.w3.org/TR/xhtml11/DTD/xhtml11.dtd\">
<html xmlns=\"http://www.w3.org/1999/xhtml\">
  <head>
    <base href=\"\" />
    <link href=\"xmlpe.css\" rel=\"StyleSheet\" />
    <title></title>
  </head>
  <body>
"
  "
  </body>
</html>\n")

    ;;;;;; XHTML utf-8
    ("xhtml-utf-8"
    "<?xml version=\"1.0\" encoding=\"utf-8\"?>
<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.1//EN\"
\"http://www.w3.org/TR/xhtml11/DTD/xhtml11.dtd\">
<html xmlns=\"http://www.w3.org/1999/xhtml\">
  <head>
    <base href=\"\" />
    <link href=\"xmlpe.css\" rel=\"StyleSheet\" />
    <title></title>
  </head>
  <body>
"
  "
  </body>
</html>\n")

    )

  "Available headers+footers to insert in buffer around XML code.
Each entry in the list has the form

   (key header footer)

KEY is used by `xmlpe-header-footer-name' to find an entry in the
list.  The value of HEADER and FOOTER in this entry is used by
`xmlpe-mode' to temporary insert headers and footers in the
buffer edited.

Default header is a header for XHTML upto and including the
<body> starting tag.

Default is footer for XHTML, ie </body></html>."
  :type '(repeat
          (list
           (string :tag "Key")
           (string :tag "Header")
           (string :tag "Footer")))
  :group 'xmlpe)

(defvar xmlpe-header-footer-hist nil)

(defcustom xmlpe-header-footer-name "xhtml-iso-8859-1"
  "Name of header+footer to use.
This should be a key name in the association list
`xmlpe-header-footer-alist'.

Note that this is buffer local.  What you change here is the
global value that is inherited when you start `xmlpe-mode'."
  :type 'string
  :set (lambda(symbol value)
         (unless (assoc value xmlpe-header-footer-alist)
           (lwarn '(xmlpe-header-footer-name) :error "Can not find %s in xmlpe-header-footer-alist" value))
         (set-default symbol value))
  :group 'xmlpe)

(defun xmlpe-switch-header-footer-name(name)
  (interactive (list
                (let ((names))
                  (unless xmlpe-mode
                    (error "This can only be done in xmlpe-mode!"))
                  (dolist (entry xmlpe-header-footer-alist)
                    (add-to-list 'names (car entry)))
                  (completing-read "Header-footer name: " names nil nil xmlpe-header-footer-name 'xmlpe-header-footer-hist ""))))
  (if (string= xmlpe-header-footer-name name)
      (message "Not changed (same as before)")
    (xmlpe-mode 0)
    (setq xmlpe-header-footer-name name)
    (xmlpe-mode t)))


(defcustom xmlpe-invisible-hf nil
  "When non-nil header and footer are invisible.

Note that this is buffer local. What you change here is the
global value that is inherited when you start `xmlpe-mode'."
  :type 'boolean
  :group 'xmlpe)

(defcustom xmlpe-hf-bgcolor "RGB:87/CE/FA"
  "Background color for header and footer."
  :type 'string
  :group 'xmlpe)

(defcustom xmlpe-base-bgcolor "RGB:A7/EE/FA"
  "Background color for <base>-tag."
  :type 'string
  :group 'xmlpe)

(defvar xmlpe-header-overlay nil)
(defvar xmlpe-footer-overlay nil)
(defvar xmlpe-base-overlay nil)
(defvar xmlpe-base-href-overlay nil)
(defvar xmlpe-css-overlay nil)
(defvar xmlpe-css-href-overlay nil)

(defun xmlpe-edit-min()
  (1+ (overlay-end xmlpe-header-overlay)))

(defun xmlpe-edit-max()
  (- (overlay-start xmlpe-footer-overlay) 2))

(defun xmlpe--post-command()
  "Function for `post-command-hook'."
  (condition-case nil
      (let ((edit-min (xmlpe-edit-min))
            (edit-max (xmlpe-edit-max)))
        (when (< (point) edit-min)
          (goto-char edit-min))
        (when (> (point) edit-max)
          (goto-char edit-max))
        (when mark-active
          (when (< (mark) edit-min)
            (set-mark edit-min))
          (when (> (mark) edit-max)
            (set-mark edit-max))
          )
        (when (< (point) edit-min)
          (insert "\n")
          (goto-char edit-min)))
    (error (message "%s" (error-message-string err)))))

(defun xmlpe-toggle-invisible-hf()
  "Toggle visibilty of the XML header and footer inserted by xmlpe-mode."
  (interactive)
  (setq xmlpe-invisible-hf (not xmlpe-invisible-hf))
  (when (and xmlpe-header-overlay (overlay-buffer xmlpe-header-overlay))
    (overlay-put xmlpe-header-overlay 'invisible xmlpe-invisible-hf))
  (when (and xmlpe-footer-overlay (overlay-buffer xmlpe-footer-overlay))
    (overlay-put xmlpe-footer-overlay 'invisible xmlpe-invisible-hf)))


(defvar xmlpe-accept-modes
  '(fundamental-mode text-mode nxml-mode nxhtml-mode html-mode sgml-mode)
  "Modes from which `xmlpe-mode' can be entered.")


;;;;;;;;;;;;;;;;
;;; Viewing
(defvar xmlpe-view-buffer-name "*XmlPE View Buffer*")
;; Fix-me: Handle base href here!
(defun xmlpe-view-save-temp-file()
  "Save a temporary file for viewing in web browser."
  (let ((curbuf (current-buffer))
        (view-buffer (get-buffer-create xmlpe-view-buffer-name)))
    (save-excursion
      (set-buffer view-buffer)
      (unless buffer-file-name
        (set-visited-file-name "~/.temp-xmlpe-view.htm")
        (rename-buffer xmlpe-view-buffer-name))
      (erase-buffer)
      (insert
       (with-current-buffer curbuf
           (buffer-substring-no-properties (point-min) (point-max))))
      (when (fboundp 'emacsw32-eol-set)
        (emacsw32-eol-set nil))
      (save-buffer)
      (current-buffer))))

(defcustom xmlpe-view-region-hf-name "xhtml-iso-8859-1"
  "Header-footer for `xmlpe-view-region'."
  :type 'string
  :group 'xmlpe)

(defun xmlpe-view-region(start end)
  "View the region in browser."
  (interactive "r")
  (unless (mark)
    (error "The Region is not active"))
  (let ((view-buffer (find-file-noselect "~/.temp-xmlpe-view-region.htm"))
        (region (buffer-substring-no-properties start end)))
    (with-current-buffer view-buffer
      (erase-buffer)
      (let* ((hf (assoc xmlpe-view-region-hf-name xmlpe-header-footer-alist))
             (header (nth 1 hf))
             (footer (nth 2 hf)))
        (insert header)
        (insert region)
        (insert footer))
      (save-buffer))
    (browse-url (buffer-file-name view-buffer))
    (kill-buffer view-buffer)))

(defun xmlpe-view()
  "View in web browser."
  (interactive)
  (let ((view-buffer (xmlpe-view-save-temp-file)))
    (browse-url (buffer-file-name view-buffer))
    (kill-buffer view-buffer)))

;;;;;;;;;;;;;;;;;
;;; Base hrefs
(defcustom xmlpe-base-href ""
  "URL to insert in <base href=\"\">-tag."
  :type 'string
  :group 'xmlpe)

;;(setq xmlpe-base-hrefs '("some base" "some other base"))
(defcustom xmlpe-base-hrefs nil
  "List of hrefs to be used for <base>-tag when viewing.
This list is used by `xmlpe-set-base' and is displayed as choices when reading the base URL."
  :type '(repeat string)
  :group 'xmlpe)

(defvar xmlpe-set-base-hist nil)


(defun xmlpe-set-base()
  (interactive)
  (let ((base (completing-read "Base href URL: " xmlpe-base-hrefs nil nil xmlpe-base-href 'xmlpe-set-base-hist "")))
    (setq xmlpe-base-href base))
  (xmlpe-update-header-hrefs))



(defcustom xmlpe-css-href
  (let* ((el-file (if load-file-name
                     load-file-name
                   (buffer-file-name)))
         (el-dir (file-name-directory el-file)))
    (concat el-dir "xmlpe.css"))
  "URL to insert in <link href=\"\" rel=\"StyleSheet\">-tag."
  :type 'string
  :group 'xmlpe)

(defcustom xmlpe-css-hrefs nil
  "List of hrefs to be used for CSS <link>-tag when viewing.
This list is used by `xmlpe-set-css' and is displayed as choices when reading the base URL."
  :type '(repeat string)
  :group 'xmlpe)

(defvar xmlpe-set-css-hist nil)

(defun xmlpe-set-css()
  (interactive)
  (setq xmlpe-css-href
        (let ((has-nxhtml (fboundp 'nxhtml-read-url))
              read-with-nxhtml)
          (when has-nxhtml
            (setq read-with-nxhtml
                  (not (y-or-n-p "Choose one of your predefined CSS links? "))))
          (if read-with-nxhtml
              (nxhtml-read-url nil nil nil "CSS")
            (completing-read "CSS URL: " xmlpe-css-hrefs nil nil xmlpe-css-href 'xmlpe-set-css-hist ""))))
  (xmlpe-update-header-hrefs))

;;(xmlpe-update-header-hrefs xmlpe-base-href))
(defun xmlpe-update-header-hrefs()
  (when xmlpe-base-overlay
    (remove-hook 'post-command-hook 'xmlpe--post-command t)
    ;;(overlay-put xmlpe-header-overlay 'intangible nil)
    (let ((inhibit-read-only t)
	  (old-modified (buffer-modified-p))
	  )
      (delete-region (+ (overlay-start xmlpe-base-href-overlay) 1) (- (overlay-end xmlpe-base-href-overlay) 1))
      (delete-region (+ (overlay-start xmlpe-css-href-overlay) 1) (- (overlay-end xmlpe-css-href-overlay) 1))
      (save-excursion
	(goto-char (+ (overlay-start xmlpe-base-href-overlay) 1))
	(insert (browse-url-file-url xmlpe-base-href))
	(goto-char (+ (overlay-start xmlpe-css-href-overlay) 1))
	(insert (browse-url-file-url xmlpe-css-href)))
      (set-buffer-modified-p old-modified))
    (add-hook 'post-command-hook 'xmlpe--post-command t t)
    ;;(overlay-put xmlpe-header-overlay 'intangible t)
    ))



;;;;;;;;;;;;;;;;;;;;
;;; File extensions

(defvar xmlpe-mode-alist
  ;;nil
  '(("\\.htmlf" . nxhtml-mode))
  "Alist of filename patterns vs corresponding major mode chooser.
Each element looks like \(REGEXP . CHOOSER) or \(REGEXP CHOOSER
MATCH-AGAIN HEADER-FOOTER).  CHOOSER may be a major mode function
or a string.

MATCH-AGAIN works the same way as the third element in
`auto-mode-alist'.  \(Which is called NON-NIL.\) If HEADER-FOOTER
is non-nil it should be a string to use as the name of the
header-footer combination to use by `xmlpe-mode'.

This alist is matched by `xmlpe-auto-mode' against
`buffer-file-name' the same way as `find-file' matches against
`auto-mode-alist'.  If a match is found then if CHOOSER is a
function it is supposed to be the major mode function searched
for.  If CHOOSER is a string it is supposed to be a file name and
the major mode function is found by matching the string against
`auto-mode-alist'.")

(defun xmlpe-set-mode-alist-entry(entry)
  "Add ENTRY to `xmlpe-mode-alist' and remove entries with same car."
  (let* ((name (car entry))
         (old  (assoc-string name xmlpe-mode-alist)))
    (while old
      (setq xmlpe-mode-alist (delete old xmlpe-mode-alist))
      (setq old  (assoc-string name xmlpe-mode-alist))))
  (setq xmlpe-mode-alist (cons entry xmlpe-mode-alist)))


(defconst xmlpe-mode-keymap
  (let ((map (make-sparse-keymap "XmlPE")))
    (define-key map [menu-bar xmlpe] (cons "XmlPE" (make-sparse-keymap "second")))
    (define-key map [menu-bar xmlpe custom]
      '("Customize XmlPE" . (lambda()
                              (interactive)
                              (customize-group 'xmlpe))))
    (define-key map [menu-bar xmlpe div1] '("-" . nil))
    (define-key map [menu-bar xmlpe switch-hf] '("Switch header/footer" . xmlpe-switch-header-footer-name))
    (define-key map [menu-bar xmlpe set-base] '("Set Base Href" . xmlpe-set-base))
    (define-key map [menu-bar xmlpe set-css]  '("Set CSS Href" . xmlpe-set-css))
    (define-key map [menu-bar xmlpe separator-1]  '(menu-item "--"))
    (define-key map [menu-bar xmlpe toggle]   '("Toggle Invisible Header and Footer" . xmlpe-toggle-invisible-hf))
    (define-key map [menu-bar xmlpe view]
      (list 'menu-item "View with header+footer in Web Browser" 'xmlpe-view
            :enable 'xmlpe-mode))
    (define-key map [menu-bar xmlpe div2] '("-" . nil))

    (define-key map [(control ?c) ?t] 'xmlpe-toggle-invisible-hf)
    (define-key map [(control ?x) ?#] 'xmlpe-done)
    map))

(defvar xmlpe-base-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-1] 'xmlpe-set-base)
    map))

(defvar xmlpe-css-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-1] 'xmlpe-set-css)
    map))

(defvar xmlpe-started nil)

(define-minor-mode xmlpe-mode
  "xmlpe-mode is a minor mode for editing XML fragments.

The purpose of xmlpe-mode (Xml Part Edit Mode) is to make it
possible to edit parts of an XML document using the syntax
checking provided by nxml-mode.

To achive this the XML code in `xmlpe-header' and `xmlpe-footer'
are temporary during xmlpe-mode inserted in the buffer around the
XML fragment to edit.  They are made readonly and have a special
background color \(`xmlpe-hf-bgcolor').  You can also hide them
using `xmlpe-toggle-invisible-hf'.
"
  :initial-value nil
  :lighter " XmlPE"
  :keymap xmlpe-mode-keymap
  (if xmlpe-mode
      (progn
	(if (not xmlpe-started)
	    (progn
	      (setq xmlpe-mode nil)
	      (xmlpe-mode-start))
	  (xmlpe-mode-enter))
        (when (or (eq major-mode 'nxml-mode)
                  (eq major-mode 'nxhtml-mode))
          ;; Select coding system:
          (nxml-prepare-to-save)))
    (xmlpe-mode-exit)))

(define-derived-mode xmlpe-auto-mode fundamental-mode "Xmlpe-auto-mode"
  "Mode to use for file associations with Xmlpe.
This mode is not meant to be used for editing. Instead it should
be used in `auto-mode-alist' \(and maybe `magic-mode-alist'\) for
file associations.

When this mode is entered `xmlpe-mode-alist' is searched for a
major mode to use.

After setting the found major mode the minor mode `xmlpe-mode' is
set."

  (let (mode
        header-footer)
    (if buffer-file-name
        (let ((name buffer-file-name)
              (case-fold-search
               (memq system-type '(vax-vms windows-nt cygwin))))
          ;; Remove backup-suffixes from file name.
          (setq name (file-name-sans-versions name))
          (while name
            ;; Find first matching alist entry.
            (if (and (setq mode (assoc-default name xmlpe-mode-alist
                                               'string-match))
                     (consp mode)
                     (cadr mode))
                (progn
                  (setq mode (car mode)
                        name (substring name 0 (match-beginning 0))))
              (setq name)))
          (setq header-footer (caddr mode))
          (setq mode (car mode))
          (when (stringp name)
            (setq name mode)
            (setq mode)
            (while name
              ;; Find first matching alist entry.
              (if (and (setq mode (assoc-default name auto-mode-alist
                                                 'string-match))
                       (consp mode)
                       (cadr mode))
                  (setq mode (car mode)
                        name (substring name 0 (match-beginning 0)))
                (setq name)))))
      (error "xmlpe-auto-mode: This buffer has no file name"))
    (when mode (set-auto-mode-0 mode t))
    (unless mode (error "No major mode found to use"))
    (let ((xmlpe-header-footer-name (if header-footer
                                        header-footer
                                      xmlpe-header-footer-name)))
      (xmlpe-mode t))
    mode))

(defvar xmlpe-done-function nil)
(defun xmlpe-mode-start()
  (unless (memq major-mode xmlpe-accept-modes)
    (error "Can enter xmlpe-mode only from major modes in xmlpe-accept-modes (%s)." major-mode))
  ;;(when (fboundp 'nxml-mode) (unless (eq major-mode 'nxml-mode) (nxml-mode)) )
  ;;(make-variable-buffer-local 'xmlpe-this-major)
  ;;(when xmlpe-this-major (unless (eq major-mode xmlpe-this-major) (funcall xmlpe-this-major)))
  (make-local-variable 'xmlpe-header-footer-name)
  (make-local-variable 'xmlpe-invisible-hf)
  (make-local-variable 'xmlpe-started)
  (make-local-variable 'xmlpe-header-overlay)
  (make-local-variable 'xmlpe-footer-overlay)
  (make-local-variable 'xmlpe-base-overlay)
  (make-local-variable 'xmlpe-base-href-overlay)
  (make-local-variable 'xmlpe-css-overlay)
  (make-local-variable 'xmlpe-css-href-overlay)
  ;; Fetch the keybinding for C-x # before entering xmlpe-mode:
  (setq xmlpe-done-function (key-binding [(control ?x) ?#]))
  (setq xmlpe-started t)
  (xmlpe-mode t)
  )

(defun xmlpe-mode-enter()
  (add-hook 'post-command-hook 'xmlpe--post-command t t)
  (add-hook 'write-contents-functions 'xmlpe-write-file-function t t)
  ;; Remove hook for coding system:
  (remove-hook 'write-contents-functions 'nxml-prepare-to-save t)
  (let* ((here (point-marker))
	(old-modified (buffer-modified-p))
        (header-footer (assoc xmlpe-header-footer-name xmlpe-header-footer-alist))
        (header-string (nth 1 header-footer))
        (footer-string (nth 2 header-footer))
	)
    (unless xmlpe-header-overlay
      (goto-char (point-min))
      (insert header-string)
      ;;(when (fboundp 'nxml-mode) (rng-auto-set-schema-and-validate))
      (when (memq major-mode '(nxml-mode nxhtml-mode)) (rng-auto-set-schema-and-validate))
      (put-text-property (point-min) (- (point) 1) 'read-only t)
      (setq xmlpe-header-overlay (make-overlay (point-min) (point)))
      (when xmlpe-invisible-hf (overlay-put xmlpe-header-overlay 'invisible t))
      ;;(overlay-put xmlpe-header-overlay 'intangible t)
      (overlay-put xmlpe-header-overlay 'face (list (cons 'background-color xmlpe-hf-bgcolor)))
      (overlay-put xmlpe-header-overlay 'pointer void-text-area-pointer))
    (unless xmlpe-base-overlay
      (setq xmlpe-base-overlay nil)
      (goto-char (point-min))
      (when (search-forward-regexp "<base\\s-*href *= *\\(\".*?\"\\)[^>]*>" (overlay-end xmlpe-header-overlay) t)
	(setq xmlpe-base-overlay (make-overlay (match-beginning 0) (match-end 0)))
	(setq xmlpe-base-href-overlay (make-overlay (match-beginning 1) (match-end 1)))
	(overlay-put xmlpe-base-overlay 'face (list (cons 'background-color xmlpe-base-bgcolor)))
	(overlay-put xmlpe-base-overlay 'keymap xmlpe-base-keymap)
	;;(overlay-put xmlpe-base-overlay 'intangible nil)
	)
      (setq xmlpe-css-overlay nil)
      (goto-char (point-min))
      (when (search-forward-regexp "<link[^>]*href *= *\\(\".*?\"\\)[^>]*rel=\"StyleSheet\"[^>]*>" (overlay-end xmlpe-header-overlay) t)
	(setq xmlpe-css-overlay (make-overlay (match-beginning 0) (match-end 0)))
	(setq xmlpe-css-href-overlay (make-overlay (match-beginning 1) (match-end 1)))
	(overlay-put xmlpe-css-overlay 'face (list (cons 'background-color xmlpe-base-bgcolor)))
	(overlay-put xmlpe-css-overlay 'keymap xmlpe-css-keymap)
	;;(overlay-put xmlpe-css-overlay 'intangible nil)
	)
      )
    (when (equal here (point-min-marker))
      (setq here (point-marker)))
    (unless xmlpe-footer-overlay
      (goto-char (point-max))
      (let ((old-end (point-max)))
	(insert footer-string)
	(let ((old-end1 (+ old-end 1)))
	  (put-text-property old-end (point-max) 'read-only t)
	  (setq xmlpe-footer-overlay (make-overlay old-end1 (point-max)))))
      (when xmlpe-invisible-hf (overlay-put xmlpe-footer-overlay 'invisible t))
      ;;(overlay-put xmlpe-footer-overlay 'intangible t)
      (overlay-put xmlpe-footer-overlay 'face (list (cons 'background-color xmlpe-hf-bgcolor)))
      (overlay-put xmlpe-footer-overlay 'pointer void-text-area-pointer))
    (set-buffer-modified-p old-modified)
    (goto-char here)
    (xmlpe-update-header-hrefs)))

(defun xmlpe-mode-exit()
  (remove-hook 'post-command-hook 'xmlpe--post-command t)
  (remove-hook 'write-contents-functions 'xmlpe-write-file-function t)
  (let ((inhibit-read-only t)
	(old-modified (buffer-modified-p)))
    (when (and xmlpe-header-overlay (overlay-buffer xmlpe-header-overlay))
      (delete-region (overlay-start xmlpe-header-overlay) (overlay-end xmlpe-header-overlay))
      (delete-overlay xmlpe-header-overlay))
    (when (and xmlpe-footer-overlay (overlay-buffer xmlpe-footer-overlay))
      (delete-region (- (overlay-start xmlpe-footer-overlay) 1)
		     (overlay-end xmlpe-footer-overlay))
      (delete-overlay xmlpe-footer-overlay))
    (set-buffer-modified-p old-modified))
  (setq xmlpe-header-overlay nil)
  (setq xmlpe-footer-overlay nil)
  (setq xmlpe-base-overlay nil))
  ;(gnuserv-edit)

;; Fix-me: What should be done when using Emacs server??
(defun xmlpe-done()
  "This does what C-x # normally does.
\(It is supposed to call `gnuserv-edit' normally. I am not sure
however how this is handled when using emacs-server instead.\)

Before it exits `xmlpe-mode' removes the header and footer that
was added."
  (interactive)
  (xmlpe-mode nil)
  (funcall xmlpe-done-function))


;;;;;;;;;;;;;;;;;
;;; Saving
(defun xmlpe-write-file-function()
  "Writes the editable part of the buffer to the visited file.
Also write to the temporary file for viewing so that it is easier
to view the contents after saving.

Added to hook `write-contents-functions'."
  (unless xmlpe-mode
    (error "xmlpe-write-file-function called but not in xmlpe-mode"))
  ;; Save to temp file too to make it easier to view
  (xmlpe-view-save-temp-file)
  ;; Save the editable region
  ;;(when (eq major-mode 'nxml-mode) (setq buffer-file-coding-system nil) (nxml-prepare-to-save) )
  (let ((coding-system-for-write buffer-file-coding-system))
    (write-region (overlay-end xmlpe-header-overlay)
                  (1- (overlay-start xmlpe-footer-overlay))
                  (buffer-file-name)))
  (set-buffer-modified-p nil)
  (clear-visited-file-modtime)
  t ;; Already written!
  )


(provide 'xmlpe)

;;; xmlpe.el ends here
