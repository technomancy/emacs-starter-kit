;;; nxhtml-part.el --- edit fragments of XHTML/XML with nxhtml/nxml-mode

;; Copyright (C) 2005, 2006, 2007 by Lennart Borgman

;; Author:  Lennart Borgman <lennart DOT borgman DOT 073 AT student DOT lu DOT se>
;; Created: 2007-02-27
(defconst nxhtml-part:version "0.58") ;; Version:
;; Lxast-Updated: Tue Feb 27 23:55:23 2007 (3600 +0100)
;; Keywords: languages
;; -Features that might be required by this library:
;;
;;   `browse-url', `cl'.
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; This file contains two minor modes `nxml-part-mode' and
;; `nxhtml-part-mode' that can be used to edit part of an XML/XHTML
;; file using `nxml-mode' / `nxhtml-mode' features.  This is
;; accomplished by adding a header and a footer so that the part to
;; edit is in the correct XML context.
;;
;; When saving the file the header and footer part is not saved.
;;
;; The header and the footer XML parts can be customized.  Default
;; values are chosen so that the XML context is inside the <body>-tag
;; in an XHTML document.
;;
;; ***Note***: If you use this for XHTML (blogging for example) you
;; may want to put the file nxhtml-part.css in the same directory as
;; nxhtml-part.el.


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
;; 2007-02-27: Moved this from xmlpe.el to nxhtml-part.el.
;;             Added `nxml-part-mode' and `nxhtml-part-mode'.

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


(defgroup nxhtml-part nil
  "Customization group for `nxhtml-part-mode' and `nxml-part-mode'."
  :group 'languages
  :group 'nxhtml)

(defcustom nxhtml-part-header-footer-alist
  '(
    ;;;;;; XHTML iso-8859-1
    ("xhtml-iso-8859-1"
    "<?xml version=\"1.0\" encoding=\"iso-8859-1\"?>
<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.1//EN\"
\"http://www.w3.org/TR/xhtml11/DTD/xhtml11.dtd\">
<html xmlns=\"http://www.w3.org/1999/xhtml\">
  <head>
    <base href=\"\" />
    <link href=\"nxhtml-part.css\" rel=\"StyleSheet\" />
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
    <link href=\"nxhtml-part.css\" rel=\"StyleSheet\" />
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

KEY is used by `nxhtml-part-header-footer-name' to find an entry in the
list.  The value of HEADER and FOOTER in this entry is used by
`nxhtml-part-mode' to temporary insert headers and footers in the
buffer edited.

Default header is a header for XHTML upto and including the
<body> starting tag.

Default is footer for XHTML, ie </body></html>."
  :type '(repeat
          (list
           (string :tag "Key")
           (string :tag "Header")
           (string :tag "Footer")))
  :group 'nxhtml-part)

(defvar nxhtml-part-header-footer-hist nil)

(defcustom nxhtml-part-header-footer-name "xhtml-iso-8859-1"
  "Name of header+footer to use.
This should be a key name in the association list
`nxhtml-part-header-footer-alist'.

Note that this is buffer local.  What you change here is the
global value that is inherited when you start `nxhtml-part-mode'."
  :type 'string
  :set (lambda(symbol value)
         (unless (assoc value nxhtml-part-header-footer-alist)
           (lwarn '(nxhtml-part-header-footer-name) :error "Can not find %s in nxhtml-part-header-footer-alist" value))
         (set-default symbol value))
  :group 'nxhtml-part)
(make-variable-buffer-local 'nxhtml-part-header-footer-name)
(put 'nxhtml-part-header-footer-name 'permanent-local t)

(defun nxhtml-part-switch-header-footer-name(name)
  (interactive (list
                (let ((names))
                  (unless nxhtml-part-mode
                    (error "This can only be done in nxhtml-part-mode!"))
                  (dolist (entry nxhtml-part-header-footer-alist)
                    (add-to-list 'names (car entry)))
                  (completing-read "Header-footer name: " names nil nil nxhtml-part-header-footer-name 'nxhtml-part-header-footer-hist ""))))
  (if (string= nxhtml-part-header-footer-name name)
      (message "Not changed (same as before)")
    (fundamental-mode)
    (setq nxhtml-part-header-footer-name name)
    (nxhtml-part-mode)))


(defcustom nxhtml-part-invisible-hf t
  "When non-nil header and footer are invisible.

Note that this is buffer local. What you change here is the
global value that is inherited when you start `nxhtml-part-mode'."
  :type 'boolean
  :group 'nxhtml-part)
(make-variable-buffer-local 'nxhtml-part-invisible-hf)
(put 'nxhtml-part-invisible-hf 'permanent-local t)

(defcustom nxhtml-part-hf-bgcolor "RGB:87/CE/FA"
  "Background color for header and footer."
  :type 'color
  :group 'nxhtml-part)

(defcustom nxhtml-part-base-bgcolor "RGB:A7/EE/FA"
  "Background color for <base>-tag."
  :type 'color
  :group 'nxhtml-part)

(defvar nxhtml-part-header-overlay nil)
(make-variable-buffer-local 'nxhtml-part-header-overlay)
(put 'nxhtml-part-header-overlay 'permanent-local t)

(defvar nxhtml-part-footer-overlay nil)
(make-variable-buffer-local 'nxhtml-part-footer-overlay)
(put 'nxhtml-part-footer-overlay 'permanent-local t)

(defvar nxhtml-part-base-overlay nil)
(make-variable-buffer-local 'nxhtml-part-base-overlay)
(put 'nxhtml-part-base-overlay 'permanent-local t)

(defvar nxhtml-part-base-href-overlay nil)
(make-variable-buffer-local 'nxhtml-part-base-href-overlay)
(put 'nxhtml-part-base-href-overlay 'permanent-local t)

(defvar nxhtml-part-css-overlay nil)
(make-variable-buffer-local 'nxhtml-part-css-overlay)
(put 'nxhtml-part-css-overlay 'permanent-local t)

(defvar nxhtml-part-css-href-overlay nil)
(make-variable-buffer-local 'nxhtml-part-css-href-overlay)
(put 'nxhtml-part-css-href-overlay 'permanent-local t)


(defun nxhtml-part-edit-min()
  (1+ (overlay-end nxhtml-part-header-overlay)))

(defun nxhtml-part-edit-max()
  (- (overlay-start nxhtml-part-footer-overlay) 2))

(defun nxhtml-part--post-command()
  "Function for `post-command-hook'."
  (condition-case err
      (let ((edit-min (nxhtml-part-edit-min))
            (edit-max (nxhtml-part-edit-max)))
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

(defun nxhtml-part-toggle-invisible-hf()
  "Toggle visibilty of the XML header and footer inserted by nxhtml-part-mode."
  (interactive)
  (setq nxhtml-part-invisible-hf (not nxhtml-part-invisible-hf))
;;   (let ((empty-line-char (1+ (overlay-end nxhtml-part-header-overlay))))
;;     (put-text-property (- empty-line-char 1) (+ empty-line-char 0) 'invisible nxhtml-part-invisible-hf))
  (when (and nxhtml-part-header-overlay (overlay-buffer nxhtml-part-header-overlay))
    (overlay-put nxhtml-part-header-overlay 'invisible nxhtml-part-invisible-hf))
  (when (and nxhtml-part-footer-overlay (overlay-buffer nxhtml-part-footer-overlay))
    (overlay-put nxhtml-part-footer-overlay 'invisible nxhtml-part-invisible-hf)))



;;;;;;;;;;;;;;;;
;;; Viewing
(defvar nxhtml-part-view-buffer-name "*nxhtml-part View Buffer*")
;; Fix-me: Handle base href here!
(defun nxhtml-part-view-save-temp-file()
  "Save a temporary file for viewing in web browser."
  (let ((curbuf (current-buffer))
        (view-buffer (get-buffer-create nxhtml-part-view-buffer-name)))
    (save-excursion
      (set-buffer view-buffer)
      (unless buffer-file-name
        (set-visited-file-name "~/.temp-nxhtml-part-view.htm")
        (rename-buffer nxhtml-part-view-buffer-name))
      (erase-buffer)
      (insert
       (with-current-buffer curbuf
           (buffer-substring-no-properties (point-min) (point-max))))
      ;;(when (fboundp 'emacsw32-eol-set) (emacsw32-eol-set nil))
      (save-buffer)
      (current-buffer))))

(defcustom nxhtml-part-view-region-hf-name "xhtml-iso-8859-1"
  "Header-footer key name for `nxhtml-part-view-region'."
  :type 'string
  :group 'nxhtml-part)

(defun nxhtml-part-view-region(start end)
  "View the region in a web browser."
  (interactive "r")
  (unless mark-active
    (error "The Region is not active"))
  (let ((view-buffer (find-file-noselect "~/.temp-nxhtml-part-view-region.htm"))
        (region (buffer-substring-no-properties start end)))
    (with-current-buffer view-buffer
      (erase-buffer)
      (let* ((hf (assoc nxhtml-part-view-region-hf-name nxhtml-part-header-footer-alist))
             (header (nth 1 hf))
             (footer (nth 2 hf)))
        (insert header)
        (insert region)
        (insert footer))
      (save-buffer))
    (browse-url (buffer-file-name view-buffer))
    ;;(kill-buffer view-buffer)
    ))

(defun nxhtml-part-view()
  "View file in web browser."
  (interactive)
  (let ((view-buffer (nxhtml-part-view-save-temp-file)))
    (browse-url (buffer-file-name view-buffer))
    ;;(kill-buffer view-buffer)
    ))

;;;;;;;;;;;;;;;;;
;;; Base hrefs
(defcustom nxhtml-part-base-href ""
  "URL to insert in <base href=\"\">-tag."
  :type 'string
  :group 'nxhtml-part)

;;(setq nxhtml-part-base-hrefs '("some base" "some other base"))
(defcustom nxhtml-part-base-hrefs nil
  "List of hrefs to be used for <base>-tag when viewing.
This list is used by `nxhtml-part-set-base' and is displayed as choices when reading the base URL."
  :type '(repeat string)
  :group 'nxhtml-part)

(defvar nxhtml-part-set-base-hist nil)


(defun nxhtml-part-set-base()
  (interactive)
  (let ((base (completing-read "Base href URL: " nxhtml-part-base-hrefs nil nil nxhtml-part-base-href 'nxhtml-part-set-base-hist "")))
    (setq nxhtml-part-base-href base))
  (nxhtml-part-update-header-hrefs))



(defconst nxhtml-part-standard-css-href
  (let* ((el-file (if load-file-name
                      load-file-name
                    (buffer-file-name)))
         (el-dir (file-name-directory el-file)))
    (concat el-dir "nxhtml-part.css")))

(defcustom nxhtml-part-css-href nxhtml-part-standard-css-href
  "URL to insert in <link href=\"\" rel=\"StyleSheet\">-tag."
  :type 'string
  :group 'nxhtml-part)


(defcustom nxhtml-part-css-hrefs nil
  "List of hrefs to be used for CSS <link>-tag when viewing.
This list is used by `nxhtml-part-set-css' and is displayed as choices when reading the base URL."
  :type '(repeat string)
  :group 'nxhtml-part)

(defvar nxhtml-part-set-css-hist nil)

(defun nxhtml-part-set-css()
  (interactive)
  (setq nxhtml-part-css-href
        (let* ((new-loc-map (copy-keymap minibuffer-local-completion-map))
               (minibuffer-local-completion-map new-loc-map)
               (hrefs nxhtml-part-css-hrefs))
          (add-to-list 'hrefs nxhtml-part-standard-css-href)
          (add-to-list 'hrefs nxhtml-part-css-href)
          (message "hrefs=%S" hrefs)(sit-for 4)
          (define-key minibuffer-local-completion-map [(control ?x) (control ?f)]
            (lambda()
              "Choose a file."
              (interactive)
              (throw 'file nil)))
          (let (f)
            (catch 'file
              (setq f
                    (completing-read "CSS URL (C-x C-f to browse files): "
                                     hrefs nil nil nxhtml-part-css-href
                                     'nxhtml-part-set-css-hist "")))
            (unless f
              (setq f (read-file-name "CSS file: ")))
            f)))
  (let ((hrefs nxhtml-part-css-hrefs))
    (unless (string= nxhtml-part-standard-css-href
                     nxhtml-part-css-href)
      (add-to-list 'hrefs nxhtml-part-css-href)
      (customize-set-value 'nxhtml-part-css-hrefs hrefs)
      (customize-set-variable 'nxhtml-part-css-hrefs hrefs)
      (customize-save-variable 'nxhtml-part-css-hrefs hrefs)))
  (nxhtml-part-update-header-hrefs))

;;(nxhtml-part-update-header-hrefs nxhtml-part-base-href))
(defun nxhtml-part-update-header-hrefs()
  (when nxhtml-part-base-overlay
    (remove-hook 'post-command-hook 'nxhtml-part--post-command t)
    ;;(overlay-put nxhtml-part-header-overlay 'intangible nil)
    (let ((inhibit-read-only t)
	  (old-modified (buffer-modified-p))
	  )
      (delete-region (+ (overlay-start nxhtml-part-base-href-overlay) 1) (- (overlay-end nxhtml-part-base-href-overlay) 1))
      (delete-region (+ (overlay-start nxhtml-part-css-href-overlay) 1) (- (overlay-end nxhtml-part-css-href-overlay) 1))
      (save-excursion
	(goto-char (+ (overlay-start nxhtml-part-base-href-overlay) 1))
	(insert (browse-url-file-url nxhtml-part-base-href))
	(goto-char (+ (overlay-start nxhtml-part-css-href-overlay) 1))
	(insert (browse-url-file-url nxhtml-part-css-href)))
      (set-buffer-modified-p old-modified))
    (add-hook 'post-command-hook 'nxhtml-part--post-command nil t)
    ;;(overlay-put nxhtml-part-header-overlay 'intangible t)
    ))



;;;;;;;;;;;;;;;;;;;;
;;; File extensions






(defun nxml-part-mode-enter()
  (add-hook 'post-command-hook 'nxhtml-part--post-command t t)
  (add-hook 'change-major-mode-hook 'nxml-part-change-mode nil t)
  (add-hook 'write-contents-functions 'nxhtml-part-write-file-function t t)
  (put 'write-contents-functions 'permanent-local t)
  ;; Remove hook for coding system:
  (remove-hook 'write-contents-functions 'nxml-prepare-to-save t)
  (let* ((here (point-marker))
	(old-modified (buffer-modified-p))
        (header-footer (assoc nxhtml-part-header-footer-name nxhtml-part-header-footer-alist))
        (header-string (nth 1 header-footer))
        (footer-string (nth 2 header-footer))
	)
    (unless nxhtml-part-header-overlay
      (goto-char (point-min))
      (insert header-string)
      ;;(when (fboundp 'nxml-mode) (rng-auto-set-schema-and-validate))
      (when (memq major-mode '(nxml-mode nxhtml-mode)) (rng-auto-set-schema-and-validate))
      (put-text-property (point-min) (- (point) 1) 'read-only t)
      (setq nxhtml-part-header-overlay (make-overlay (point-min) (point)))
      (overlay-put nxhtml-part-header-overlay 'invisible nxhtml-part-invisible-hf)
      ;;(overlay-put nxhtml-part-header-overlay 'intangible t)
      (overlay-put nxhtml-part-header-overlay 'face (list (cons 'background-color nxhtml-part-hf-bgcolor)))
      (overlay-put nxhtml-part-header-overlay 'pointer void-text-area-pointer)
      (insert "\n"))
    (unless nxhtml-part-base-overlay
      (setq nxhtml-part-base-overlay nil)
      (goto-char (point-min))
      (when (search-forward-regexp "<base\\s-*href *= *\\(\".*?\"\\)[^>]*>" (overlay-end nxhtml-part-header-overlay) t)
	(setq nxhtml-part-base-overlay (make-overlay (match-beginning 0) (match-end 0)))
	(setq nxhtml-part-base-href-overlay (make-overlay (match-beginning 1) (match-end 1)))
	(overlay-put nxhtml-part-base-overlay 'face (list (cons 'background-color nxhtml-part-base-bgcolor)))
	(overlay-put nxhtml-part-base-overlay 'keymap nxhtml-part-base-keymap)
	;;(overlay-put nxhtml-part-base-overlay 'intangible nil)
	)
      (setq nxhtml-part-css-overlay nil)
      (goto-char (point-min))
      (when (search-forward-regexp "<link[^>]*href *= *\\(\".*?\"\\)[^>]*rel=\"StyleSheet\"[^>]*>" (overlay-end nxhtml-part-header-overlay) t)
	(setq nxhtml-part-css-overlay (make-overlay (match-beginning 0) (match-end 0)))
	(setq nxhtml-part-css-href-overlay (make-overlay (match-beginning 1) (match-end 1)))
	(overlay-put nxhtml-part-css-overlay 'face (list (cons 'background-color nxhtml-part-base-bgcolor)))
	(overlay-put nxhtml-part-css-overlay 'keymap nxhtml-part-css-keymap)
	;;(overlay-put nxhtml-part-css-overlay 'intangible nil)
	)
      )
    (when (equal here (point-min-marker))
      (setq here (point-marker)))
    (unless nxhtml-part-footer-overlay
      (goto-char (point-max))
      (let ((old-end (point-max)))
	(insert footer-string)
	(let ((old-end1 (+ old-end 1)))
	  (put-text-property old-end (point-max) 'read-only t)
	  (setq nxhtml-part-footer-overlay (make-overlay old-end1 (point-max)))))
      (overlay-put nxhtml-part-footer-overlay 'invisible nxhtml-part-invisible-hf)
      ;;(overlay-put nxhtml-part-footer-overlay 'intangible t)
      (overlay-put nxhtml-part-footer-overlay 'face (list (cons 'background-color nxhtml-part-hf-bgcolor)))
      (overlay-put nxhtml-part-footer-overlay 'pointer void-text-area-pointer))
    (set-buffer-modified-p old-modified)
    (goto-char here)
    (nxhtml-part-update-header-hrefs))
  ;; Run the hook for the case when this is not run as a command, ie
  ;; for example when opening a file with emacsclient.
  ;;(switch-to-buffer (current-buffer))
  ;;(message "before auto-set-schema")(sit-for 2)
  (rng-auto-set-schema)
  ;;(message "before post command hook in nxhtml-part-mode-enter")(sit-for 2)
  (nxhtml-part--post-command)
  ;;(message "after post command hook in nxhtml-part-mode-enter")(sit-for 2)
  )

(defun nxhtml-part-mode-exit()
  (remove-hook 'post-command-hook 'nxhtml-part--post-command t)
  (put 'write-contents-functions 'permanent-local nil)
  (remove-hook 'write-contents-functions 'nxhtml-part-write-file-function t)
  (let ((inhibit-read-only t)
	(old-modified (buffer-modified-p)))
    (when (and nxhtml-part-header-overlay (overlay-buffer nxhtml-part-header-overlay))
      (delete-region (overlay-start nxhtml-part-header-overlay) (1+ (overlay-end nxhtml-part-header-overlay)))
      (delete-overlay nxhtml-part-header-overlay))
    (when (and nxhtml-part-footer-overlay (overlay-buffer nxhtml-part-footer-overlay))
      (delete-region (- (overlay-start nxhtml-part-footer-overlay) 1)
		     (overlay-end nxhtml-part-footer-overlay))
      (delete-overlay nxhtml-part-footer-overlay))
    (set-buffer-modified-p old-modified))
  (setq nxhtml-part-header-overlay nil)
  (setq nxhtml-part-footer-overlay nil)
  (setq nxhtml-part-base-overlay nil))

(defvar nxml-part-keep-me nil)
(make-variable-buffer-local 'nxml-part-keep-me)

(defun nxml-part-change-mode()
  ;; Fix-me: In some case this might be desireable to do? Or should it ever be done??
  (unless (or (boundp 'mumamo-mode)
              (and (boundp 'xhtml-multi-mode)
                   xhtml-multi-mode))
    (nxhtml-part-mode-exit)))

(define-derived-mode nxhtml-part-mode nxhtml-mode "nXhtmlPart"
  "Major mode for editing a part of XHTML documents.
OBSOLETE. Use just `nxhtml-mode' instead.

Based on `nxhtml-mode."
  (local-set-key [(control ?c) ?t] 'nxhtml-part-toggle-invisible-hf)
  (nxml-part-mode-enter))

(defvar nxhtml-part-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map nxhtml-mode-map)
    (define-key map [(control ?c) ?t] 'nxhtml-part-toggle-invisible-hf)
    map)
 "Keymap for `nxhtml-part-mode'.")

(defvar nxhtml-part-base-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-1] 'nxhtml-part-set-base)
    map))

(defvar nxhtml-part-css-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-1] 'nxhtml-part-set-css)
    map))

(define-derived-mode nxml-part-mode nxml-mode "nXmlPart"
  "Mode for editing part of an XML file.
Based on `nxml-mode."
  (progn
    (add-hook 'change-major-mode-hook 'nxml-part-change-mode nil t)
    (let ((upl-map (lookup-key nxhtml-part-mode-map [menu-bar nxhtml-mode nxhtml-upl-map])))
      (define-key upl-map [test1] 
        (list 'menu-item "TESTING" 'nxhtml-part-view)))
    (nxml-part-mode-enter)
    )
  )


;; Fix-me: What should be done when using Emacs server??
;; (defun xmlpe-done()
;;   "This does what C-x # normally does.
;; \(It is supposed to call `gnuserv-edit' normally. I am not sure
;; however how this is handled when using emacs-server instead.\)

;; Before it exits `xmlpe-mode' removes the header and footer that
;; was added."
;;   (interactive)
;;   (xmlpe-mode nil)
;;   (funcall xmlpe-done-function))


;;;;;;;;;;;;;;;;;
;;; Saving
(defun nxhtml-part-write-file-function()
  "Writes the editable part of the buffer to the visited file.
Also write to the temporary file for viewing so that it is easier
to view the contents after saving.

Added to hook `write-contents-functions'."
  ;;(unless (eq major-mode 'nxhtml-part-mode) (error "nxhtml-part-write-file-function called but not in nxhtml-part-mode"))
  ;; Save to temp file too to make it easier to view
  (when (memq major-mode '(nxhtml-part-mode nxml-part-mode))
    (nxhtml-part-view-save-temp-file))
  ;; Save the editable region
  ;;(when (eq major-mode 'nxml-mode) (setq buffer-file-coding-system nil) (nxml-prepare-to-save) )
  (let ((coding-system-for-write buffer-file-coding-system))
    (write-region (1+ (overlay-end nxhtml-part-header-overlay))
                  (1- (overlay-start nxhtml-part-footer-overlay))
                  (buffer-file-name)))
  (set-buffer-modified-p nil)
  (clear-visited-file-modtime)
  t ;; Already written!
  )

(defun nxhtml-part-new()
  "Create a temporary buffer in `nxhtml-part-mode'."
  (interactive)
  (switch-to-buffer (generate-new-buffer "*Nxthml Part Temporary Buffer*"))
  (nxhtml-part-mode)
  )

(provide 'nxhtml-part)

;;; nxhtml-part.el ends here
