;;; nxhtml-menu.el --- Defines menus for nXhtml
;;
;; Author: Lennart Borgman (lennart O borgman A gmail O com)
;; Created: Sat Apr 21 13:49:41 2007
(defconst nxhtml-menu:version "1.61") ;;Version:
;; Last-Updated: 2008-08-26T23:28:00+0200 Tue
;; URL:
;; Keywords:
;; Compatibility:
;;
;; Features that might be required by this library:
;;
;;   `compile', `flymake', `flymake-js', `flymake-php', `hexcolor',
;;   `tool-bar', `xhtml-help'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; Menus for nXhtml to be used in different major modes.
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

(require 'hexcolor)
(require 'flymake)
(require 'flymake-php)
(require 'flymake-js)

(defun nxhtml-nxhtml-in-buffer ()
  ;;(message "nxhtml-nxhtml-in-buffer ()")
  (or (derived-mode-p 'nxhtml-mode)
      (when (and (boundp 'mumamo-multi-major-mode)
                 mumamo-multi-major-mode)
        (let ((major-mode (mumamo-main-major-mode)))
          (derived-mode-p 'nxhtml-mode)))))

(defun nxhtml-nxml-in-buffer ()
  ;;(message "nxhtml-nxml-in-buffer ()")
  (or (derived-mode-p 'nxml-mode)
      (when (and (boundp 'mumamo-multi-major-mode)
                 mumamo-multi-major-mode)
        (let ((major-mode (mumamo-main-major-mode)))
          (derived-mode-p 'nxml-mode)))))

(defun nxhtml-html-in-buffer ()
  ;;(message "nxhtml-html-in-buffer ()")
  (or (derived-mode-p 'html-mode)
      (when (and (boundp 'mumamo-multi-major-mode)
                 mumamo-multi-major-mode)
        (let ((major-mode (mumamo-main-major-mode)))
          (derived-mode-p 'html-mode)))
      (nxhtml-nxhtml-in-buffer)))

(defun nxhtml-nxml-html-in-buffer ()
  ;;(message "nxhtml-nxml-html-in-buffer")
  (or (derived-mode-p 'html-mode)
      (when (and (boundp 'mumamo-multi-major-mode)
                 mumamo-multi-major-mode)
        (let ((major-mode (mumamo-main-major-mode)))
          (derived-mode-p 'html-mode)))
      (nxhtml-nxml-in-buffer)))

(defun nxhtml-this-file-can-have-toc (&optional file)
  (unless file
    (setq file (html-site-buffer-or-dired-file-name)))
  (and (nxhtml-buffer-possibly-local-viewable file)
       (html-site-current-merge-dir)
       (html-site-current-ensure-file-in-site file)))

(defun nxhtml-buffer-possibly-local-viewable (&optional file)
  ;;(message "nxhtml-buffer-possibly-local-viewable ()")
  (unless file
    (setq file (html-site-buffer-or-dired-file-name)))
  (or (and file
           (member (file-name-extension file)
                   '("html" "htm" "gif" "png")))))

(defun nxhtml-buffer-possibly-remote-viewable ()
  ;;(message "nxhtml-buffer-possibly-remote-viewable ()")
  ;; Fix-me
  (let* ((fmt "nxhtml-buffer-possibly-remote-viewable.dgffv: %s")
         (file (or buffer-file-name
                   (and (derived-mode-p 'dired-mode)
                        (condition-case err
                            (dired-get-file-for-visit)
                          (error
                           (message fmt (error-message-string err))
                           nil))))))
    (and (featurep 'html-upl)
         file
         (member (downcase (file-name-extension file))
                 '("html" "htm" "gif" "png" "pl" "php")))))

(defun nxhtml-insert-menu-dynamically (real-binding)
  ;;(message "nxhtml-insert-menu-dynamically (%s)" real-binding)
  (when (and (symbolp real-binding)
             (boundp real-binding))
    (symbol-value real-binding)))

(defun nxhtml-menu-image-file ()
  (or (get-char-property (point) 'image-file)
      buffer-file-name))

(defun nxhtml-gimp-can-edit ()
  (gimp-can-edit (nxhtml-menu-image-file)))

(defun nxhtml-edit-with-gimp ()
  "Edit with GIMP buffer or file at point."
  (interactive)
  (gimp-edit-file (nxhtml-menu-image-file)))

(defconst nxhtml-minor-mode-menu-map
  (let ((map (make-sparse-keymap "nxhtml-minor-mode-menu")))

    (let ((help-map (make-sparse-keymap)))
      (define-key help-map [emacs-Q-nxhtml]
        (list 'menu-item "Start 'emacs -Q' and load nXhtml" 'emacs-Q-nxhtml))
      (define-key help-map [nxhtmltest-run]
        (list 'menu-item "Run nXhtml Tests in Current Emacs" 'nxhtmltest-run))
      (define-key help-map [nxhtmltest-run-Q]
        (list 'menu-item "Run nXhtml Tests in a Fresh Emacs" 'nxhtmltest-run-Q))
      (define-key help-map [nxhtml-report-bug]
        (list 'menu-item "Report a Bug in nXhtml ..." 'nxhtml-report-bug))
      (define-key help-map [nxhtml-help-separator2] (list 'menu-item "--"))
      (define-key help-map [nxhtml-features-check]
        (list 'menu-item "Check Optional Features" 'nxhtml-features-check))
      (define-key help-map [nxhtml-customize]
        (list 'menu-item "Customize nXhtml ..." 'nxhtml-customize))
;;;       (define-key help-map [nxhtml-quick-customize]
;;;         (list 'menu-item "Quick Customize nXhtml ..." 'nxhtml-quick-customize))
      (define-key help-map [nxhtml-help-separator3] (list 'menu-item "--"))
;;;       (define-key help-map [nxhtml-help]
;;;         (list 'menu-item "nXhtml Help" 'nxhtml-help))
      (define-key help-map [nxhtml-tutorials]
        (list 'menu-item "nXhtml Tutorials" 'nxhtml-tutorials))
      (define-key help-map [nxhtml-overview]
        (list 'menu-item (concat "nXhtml Version " nxhtml-menu:version " Overview") 'nxhtml-overview))
      (define-key help-map [nxhtml-welcome]
        (list 'menu-item "Welcome to nXhtml" 'nxhtml-welcome))
      (define-key map [nxhtml-help-map]
        (list 'menu-item "nXhtml Help and Setup" help-map))
      (define-key map [nxhtml-info-separator] (list 'menu-item "--"))
      )




    (let ((tools-map (make-sparse-keymap)))
      (define-key map [nxhtml-tools-map]
        (list 'menu-item "Tools" tools-map
              :visible `(not (derived-mode-p 'dired-mode))
              ))
      (let ((fill-map (make-sparse-keymap)))
        (define-key tools-map [nxhtml-filling]
          (list 'menu-item "Writing Text" fill-map))
        (define-key fill-map [nxhtml-unfill-paragraph]
          (list 'menu-item "Unfill Paragraph" 'unfill-paragraph))
        (define-key fill-map [nxhtml-fill-paragraph]
          (list 'menu-item "Fill Paragraph" 'fill-paragraph))
        (define-key fill-map [nxhtml-wrap-to-fill-separator]
          (list 'menu-item "--" nil))
        (define-key fill-map [nxhtml-html-write-mode]
          (list 'menu-item "HTML Write Mode"
                'html-write-mode
                :button '(:toggle . (and (boundp 'html-write-mode)
                                         html-write-mode))))
        (define-key fill-map [nxhtml-wrap-to-fill-column-mode]
          (list 'menu-item "Wrap To Fill Column Mode"
                'wrap-to-fill-column-mode
                :button '(:toggle . wrap-to-fill-column-mode)))
        )
      (define-key tools-map [nxhtml-ecb-separator]
        (list 'menu-item "--" nil))
      (let ((ecb-map (make-sparse-keymap)))
        (define-key tools-map [nxhtml-ecb-map]
          (list 'menu-item "ECB" ecb-map))
        (define-key ecb-map [nxhtml-rinari-homepage]
          (list 'menu-item "ECB Home Page"
                (lambda ()
                  "Open ECB home page in your web browser."
                  (interactive)
                  (browse-url "http://ecb.sourceforge.net/"))))
        (define-key ecb-map [nxhtml-ecb-home-separator]
          (list 'menu-item "--" nil))
        (define-key ecb-map [nxhtml-update-ecb]
          (list 'menu-item "Fetch/update ECB dev sources"
                'udev-ecb-update))
        (define-key ecb-map [nxhtml-custom-ecb]
          (list 'menu-item "Customize ECB dev startup"
                (lambda () (interactive)
                  (require 'udev-ecb)
                  (customize-group-other-window 'udev-ecb))))
        (define-key ecb-map [nxhtml-custom-important-ecb]
          (list 'menu-item "Customize important ECB things"
                (lambda () (interactive)
                  (customize-group-other-window 'ecb-most-important))
                :enable (featurep 'ecb)))
        )
      ;;(define-key tools-map [nxhtml-cedet-separator] (list 'menu-item "--" nil))
      (let ((cedet-map (make-sparse-keymap)))
        (define-key tools-map [nxhtml-cedet-map]
          (list 'menu-item "CEDET" cedet-map))
        (define-key cedet-map [nxhtml-rinari-homepage]
          (list 'menu-item "CEDET Home Page"
                (lambda ()
                  "Open CEDET home page in your web browser."
                  (interactive)
                  (browse-url "http://cedet.sourceforge.net/"))))
        (define-key cedet-map [nxhtml-cedet-home-separator]
          (list 'menu-item "--" nil))
        (define-key cedet-map [nxhtml-update-cedet]
          (list 'menu-item "Fetch/update CEDET dev sources"
                'udev-cedet-update))
        (define-key cedet-map [nxhtml-custom-cedet]
          (list 'menu-item "Customize CEDET dev startup"
                (lambda () (interactive)
                  (require 'udev-cedet)
                  (customize-group-other-window 'udev-cedet))))
        )
      (let ((rinari-map (make-sparse-keymap)))
        (define-key tools-map [nxhtml-rinari-map]
          (list 'menu-item "Rinari" rinari-map))
        (define-key rinari-map [nxhtml-rinari-homepage]
          (list 'menu-item "Rinari Home Page"
                (lambda ()
                  "Open Rinari home page in your web browser."
                  (interactive)
                  (browse-url "http://rubyforge.org/projects/rinari/"))))
        (define-key rinari-map [nxhtml-rinari-home-separator]
          (list 'menu-item "--" nil))
        (define-key rinari-map [nxhtml-update-rinari]
          (list 'menu-item "Fetch/update Rinari dev sources"
                'udev-rinari-update))
        (define-key rinari-map [nxhtml-custom-rinari]
          (list 'menu-item "Customize Rinari startup"
                (lambda () (interactive)
                  (require 'udev-rinari)
                  (customize-group-other-window 'udev-rinari))))
        )
      (let ((mozrepl-map (make-sparse-keymap)))
        (define-key tools-map [nxhtml-mozrepl-map]
          (list 'menu-item "MozRepl for Javascript" mozrepl-map
              ))
        (define-key mozrepl-map [nxhtml-mozrepl-home-page]
          (list 'menu-item "MozLab/MozRepl Home Page"
                (lambda ()
                  "Open MozLab/MozRepl home page in your web browser."
                  (interactive)
                  (browse-url "http://hyperstruct.net/projects/mozlab"))))
        (define-key mozrepl-map [nxhtml-mozrepl-separator2]
          (list 'menu-item "--" nil))
        (define-key mozrepl-map [nxhtml-mozrepl-run-mozilla]
          (list 'menu-item "Display/Start MozRepl Process" 'run-mozilla
              :enable '(and (boundp 'moz-minor-mode) moz-minor-mode)))
        (define-key mozrepl-map [nxhtml-mozrepl-separator1]
          (list 'menu-item "--" nil))
        (define-key mozrepl-map [nxhtml-mozrepl-save-and-send]
          (list 'menu-item "Save Buffer and Send it" 'moz-save-buffer-and-send
                :enable '(or (not (boundp 'mumamo-multi-major-mode))
                             (not mumamo-multi-major-mode))))



        (define-key mozrepl-map [nxhtml-mozrepl-send-defun-and-go]
          (list 'menu-item "Send Current Function, Go to MozRepl"
                'moz-send-defun-and-go
                :enable '(and (boundp 'moz-minor-mode) moz-minor-mode)))
        (define-key mozrepl-map [nxhtml-mozrepl-send-defun]
          (list 'menu-item "Send Current Function" 'moz-send-defun
                :enable '(and (boundp 'moz-minor-mode) moz-minor-mode)))
        (define-key mozrepl-map [nxhtml-mozrepl-send-region]
          (list 'menu-item "Send the Region" 'moz-send-region
                :enable '(and mark-active
                              (boundp 'moz-minor-mode) moz-minor-mode)))
        )
      (define-key tools-map [nxhtml-majpri-separator]
        (list 'menu-item "--" nil))
      (let ((majpri-map (make-sparse-keymap)))
        (define-key tools-map [nxhtml-majpri-map]
          (list 'menu-item "Major Modes Priorities" majpri-map))
        (define-key majpri-map [nxhtml-majpri-act]
          (list 'menu-item "Apply Major Modes Priorities"
                'majmodpri-apply-priorities))
        (define-key majpri-map [nxhtml-majpri-cust]
          (list 'menu-item "Customize Major Mode Priorities"
                (lambda () (interactive)
                  (customize-group-other-window 'majmodpri))))
        )
      (define-key tools-map [nxhtml-tidy-separator]
        (list 'menu-item "--" nil))
      (define-key tools-map [nxhtml-tidy-map]
        (list 'menu-item "Tidy XHTML" 'tidy-menu-symbol
              :filter 'nxhtml-insert-menu-dynamically
              :visible '(featurep 'tidy-xhtml)
              :enable '(and (featurep 'tidy-xhtml)
                            (or (derived-mode-p 'html-mode)
                                (nxhtml-nxhtml-in-buffer)))))
      (define-key tools-map [nxhtml-flymake]
        (list 'menu-item "Flymake Mode" 'flymake-mode
              :button '(:toggle . flymake-mode)
              :enable '(and buffer-file-name
                            (flymake-get-init-function buffer-file-name)
                            )))
      (let ((flyspell-map (make-sparse-keymap)))
        (define-key tools-map [nxhtml-flyspell-map]
          (list 'menu-item "Flyspell" flyspell-map))
        (define-key flyspell-map [nxhtml-flyspell-goto-next]
          (list 'menu-item "Flyspell Go To Next Error" 'flyspell-goto-next-error
                :enable 'flyspell-mode))
        (define-key flyspell-map [nxhtml-flyspell-region]
          (list 'menu-item "Flyspell Region" 'flyspell-region
                :enable 'flyspell-mode))
        (define-key flyspell-map [nxhtml-flyspell-div-1]
          (list 'menu-item "--"))
        (define-key flyspell-map [nxhtml-flyspell]
          (list 'menu-item "Flyspell Mode" 'flyspell-mode
                :button '(:toggle . flyspell-mode)))
        )
      (define-key tools-map [nxhtml-flyspell-separator]
        (list 'menu-item "--"))
      (let ((img-map (make-sparse-keymap)))
        (define-key tools-map [nxhtml-img-map]
          (list 'menu-item "Images" img-map))
        (define-key img-map [nxhtml-gimp-edit]
          (list 'menu-item "Edit with GIMP" 'nxhtml-edit-with-gimp
                :enable '(nxhtml-gimp-can-edit)))
        (define-key img-map [nxhtml-gimp-separator]
          (list 'menu-item "--"))
        (define-key img-map [nxhtml-inlimg-toggle-img]
          (list 'menu-item "Toggle Display of Image" 'inlimg-toggle-img-display))
        (define-key img-map [nxhtml-inlimg-mode]
          (list 'menu-item "Show <img ...> Images" 'inlimg-mode
                :button '(:toggle . (and (boundp 'inlimg-mode) inlimg-mode)))))
      (define-key tools-map [nxhtml-img-separator]
        (list 'menu-item "--"))
      (let ((some-help-map (make-sparse-keymap)))
        (define-key tools-map [nxhtml-some-help-map]
          (list 'menu-item "Help for Item at Point" some-help-map))
        (define-key some-help-map [nxhtml-css-help]
          (list 'menu-item "CSS Help" 'xhtml-help-show-css-ref
                :enable '(featurep 'xhtml-help)))
        (define-key some-help-map [nxhtml-tag-help]
          (list 'menu-item "XHTML Tag Help" 'nxhtml-short-tag-help
                :enable '(featurep 'xhtml-help))))

      (let ((hexclr-map (make-sparse-keymap)))
        (define-key tools-map [nxhtml-hexcolor]
          (list 'menu-item "Color Help" hexclr-map))
        (define-key hexclr-map [nxhtml-hexcolor-mode]
          (list 'menu-item "Hex Color Mode"
                'hexcolor-mode
                :filter 'nxhtml-insert-menu-dynamically
                :enable '(and font-lock-mode
                              (or (not (boundp 'mumamo-multi-major-mode))
                                  (not mumamo-multi-major-mode))
                              (featurep 'hexcolor))
                :button '(:toggle . (and (boundp 'hexcolor-mode) hexcolor-mode))))
        (define-key hexclr-map [nxhtml-hexcolor-test]
          (list 'menu-item "Color Test" 'hexcolor-test
                :enable '(featurep 'hexcolor))))

      (let ((where-map (make-sparse-keymap)))
        (define-key tools-map [nxml-where]
          (list 'menu-item "XML Path" where-map
                :enable '(and (fboundp 'nxml-where-mode)
                              (or (derived-mode-p 'nxml-mode)
                                  (nxhtml-nxhtml-in-buffer)))))
        (define-key where-map [nxml-where-id]
          (list 'menu-item "Show tag ids in path" 'nxml-where-tag+id-toggle
                :enable '(boundp 'nxml-where-tag+id)
                :button '(:toggle . (and (boundp 'nxml-where-tag+id)
                                         nxml-where-tag+id))))
        (define-key where-map [nxml-where-header]
          (list 'menu-item "Show XML path in header" 'nxml-where-header-toggle
                :enable '(boundp 'nxml-where-header)
                :button '(:toggle . (and (boundp 'nxml-where-header)
                                         'nxml-where-header))))
        (define-key where-map [nxml-where-marks]
          (list 'menu-item "Show XML path marks" 'nxml-where-marks-toggle
                :enable '(boundp 'nxml-where-marks)
                :button '(:toggle . (and (boundp 'nxml-where-marks)
                                         nxml-where-marks))))
        (define-key where-map [where-separator] (list 'menu-item "--"))
        (define-key where-map [nxml-where-global-toggle]
          (list 'menu-item "Show XML path" 'nxml-where-global-mode
                :button '(:toggle . nxml-where-global-mode)))
        (define-key where-map [nxml-where-toggle]
          (list 'menu-item "Show XML path in buffer" 'nxml-where-mode
                :button '(:toggle . (and (boundp 'nxml-where-mode)
                                         nxml-where-mode))))
        ))

    (let ((quick-map (make-sparse-keymap)))
      (define-key map [nxhtml-quick-map]
        (list 'menu-item "Quick Inserts etc" quick-map
              :visible '(or (derived-mode-p 'html-mode)
                            (nxhtml-nxhtml-in-buffer))))
      (let ((sometoc-map (make-sparse-keymap)))
        (let ((toc-map (make-sparse-keymap)))
          (define-key sometoc-map [nxhtml-toc-map]
            (list 'menu-item "For Site" toc-map
                  :enable '(featurep 'html-toc)))
          (define-key toc-map [nxhtml-html-wtoc]
            (list 'menu-item "Merge Pages and TOC"
                  'html-wtoc-write-pages-with-toc
                  :enable '(and (html-site-current-page-list))))
          (define-key toc-map [nxthml-html-toc]
            (list 'menu-item "With Frames" 'html-toc-menu-map
                  :filter 'nxhtml-insert-menu-dynamically)))
        (define-key sometoc-map [nxhtml-html-pagetoc]
          (list 'menu-item "For Page" 'html-pagetoc-menu-map
                :filter 'nxhtml-insert-menu-dynamically
                :enable '(featurep 'html-pagetoc)))
        (define-key quick-map [nxhtml-sometoc-map]
          (list 'menu-item "Table of Contents" sometoc-map
                :visible '(or (derived-mode-p 'html-mode)
                              (nxhtml-nxhtml-in-buffer)))))
      (define-key quick-map [nxhtml-quick-sep-1]
        (list 'menu-item "--"))
      (define-key quick-map [nxhtml-spec-chars]
        (list 'menu-item "Insert special character"
              'nxml-insert-named-char))
      (define-key quick-map [nxhtml-css-rollover]
        (list 'menu-item "Insert CSS Rollover Images"
              'nxhtml-rollover-insert-2v)))

    (define-key map [nxhtml-help-tools-separator]
      ;; Notice that removing nil below gives an error that is quite
      ;; hard to catch:
      ;;
      ;; Wrong type argument: arrayp, not
      (list 'menu-item "--" nil
            :visible `(not (derived-mode-p 'dired-mode))
            ))


    (let ((upl-map (make-sparse-keymap "html-upl")))
      (define-key map [nxhtml-upl-map]
        (list 'menu-item "File Transfer" upl-map
              :enable '(featurep 'html-upl)))
      (define-key upl-map [nxhtml-upl-remote-dired]
        (list 'menu-item "Remote Dired" 'html-upl-remote-dired))
      (define-key upl-map [nxhtml-upl-dired-sep] (list 'menu-item "--"))
      (define-key upl-map [nxhtml-upl-edit-remote-wtoc]
        (list 'menu-item "Edit Remote File With TOC" 'html-upl-edit-remote-file-with-toc
              :visible '(nxhtml-this-file-can-have-toc)))
      (define-key upl-map [nxhtml-upl-edit-remote]
        (list 'menu-item "Edit Remote File" 'html-upl-edit-remote-file))
      (define-key upl-map [nxhtml-upl-ediff-file]
        (list 'menu-item "Ediff Remote/Local Files" 'html-upl-ediff-file))
      (define-key upl-map [nxhtml-upl-sep] (list 'menu-item "--"))
      (define-key upl-map [nxhtml-upl-upload-site-with-toc]
        (list 'menu-item "Upload Site with TOC" 'html-upl-upload-site-with-toc
              :visible '(and (html-site-current-merge-dir)
                             (html-site-current-ensure-file-in-site file))))
      (define-key upl-map [nxhtml-upl-upload-site]
        (list 'menu-item "Upload Site" 'html-upl-upload-site))
      (define-key upl-map [nxhtml-upl-upload-file]
        (list 'menu-item "Upload Single File" 'html-upl-upload-file))
      )


    (let ((browse-map (make-sparse-keymap)))
      (define-key map [nxhtml-browse-map]
        (list 'menu-item "Browse" browse-map
              '(or buffer-file-name
                   (eq major-mode 'nxhtml-mode))
              :enable '(nxhtml-buffer-possibly-local-viewable)))
      (define-key browse-map [nxhtml-browse-region]
        (list 'menu-item "Browse the Region Only" 'nxhtml-browse-region
              :enable 'mark-active))
      (define-key browse-map [nxhtml-upl-sep3] (list 'menu-item "--"))
      (define-key browse-map [nxhtml-upl-browse-remote-wtoc]
        (list 'menu-item "Browse Uploaded File With TOC" 'html-upl-browse-remote-with-toc
              :visible '(and (nxhtml-buffer-possibly-local-viewable)
                             (featurep 'html-wtoc)
                             (html-site-current-merge-dir)
                             (html-site-current-ensure-file-in-site file)
                             (nxhtml-buffer-possibly-remote-viewable)
                             )))
      (define-key browse-map [nxhtml-upl-browse-remote-frame-file]
        (list 'menu-item "Browse Uploaded Frames File" 'html-upl-browse-remote-frames
              :enable '(nxhtml-buffer-possibly-remote-viewable)))
      (define-key browse-map [nxhtml-upl-browse-remote]
        (list 'menu-item "Browse Uploaded File" 'html-upl-browse-remote
              :enable '(nxhtml-buffer-possibly-remote-viewable)))
      (define-key browse-map [nxhtml-upl-sep2]
        (list 'menu-item "--"))
      (define-key browse-map [nxhtml-browse-merged-file]
        (list 'menu-item "Browse File With TOC" 'html-wtoc-browse-page-with-toc
              :visible '(and (nxhtml-buffer-possibly-local-viewable)
                             (featurep 'html-wtoc)
                             (html-site-current-merge-dir)
                             (html-site-current-ensure-file-in-site file)
                             )))
      (define-key browse-map [nxhtml-browse-frame-file]
        (list 'menu-item "Browse Frames File" 'html-toc-browse-frames-file
              :enable '(and (featurep 'html-toc)
                            (nxhtml-buffer-possibly-local-viewable))))
      (define-key browse-map [nxhtml-browse-file]
        (list 'menu-item "Browse File" 'nxhtml-browse-file
              :enable '(nxhtml-buffer-possibly-local-viewable)))
      )


    (let ((link-map (make-sparse-keymap)))

      (define-key link-map [nxhtml-chklnk]
        (list 'menu-item "Check Links" 'html-chklnk-check-site-links
              :enable '(featurep 'html-chklnk)))

      (let ((move-map (make-sparse-keymap)))
        (define-key move-map [html-move-buffer-file]
          (list 'menu-item "Move Buffer File" 'html-move-buffer-file
                :help "Move buffer file and update links"
                :enable '(and buffer-file-name
                              (featurep 'html-move))))
        (define-key link-map [move-map]
          (list 'menu-item "Moving Files" move-map))
        (define-key link-map [move-map-separator] (list 'menu-item "--"))
        )


      (define-key link-map [nxhtml-paste-link]
        (list 'menu-item "Paste Saved Relative Link" 'nxhtml-paste-link
              :help "Paste link"
              :enable '(and (boundp 'nxhtml-saved-link-file)
                                    nxhtml-saved-link-file)))
      (define-key link-map [nxhtml-paste-link-as-a-tag]
        (list 'menu-item "Paste Saved Relative Link as <a href=...>" 'nxhtml-paste-link-as-a-tag
              :help "Paste link as <a ...> tag"
              :enable '(and (boundp 'nxhtml-saved-link-file)
                            nxhtml-saved-link-file
                            (nxhtml-nxml-html-in-buffer))))
      (define-key link-map [nxhtml-save-link-to-here]
        (list 'menu-item "Save Relative Link to Current File" 'nxhtml-save-link-to-here
              :help "Save link info for current file"
              :enable 'buffer-file-name))

      (define-key link-map [nxhtml-separator-1] (list 'menu-item "--"))
      (define-key link-map [mlinks-goto-link-other-frame]
        (list 'menu-item "Follow MLink Link in New Frame" 'mlinks-goto-other-frame
              :enable '(and (boundp 'mlinks-mode)
                            mlinks-mode)
              :help "Follow MLinks Link in New Frame"))
      (define-key link-map [mlinks-goto-link-other-window]
        (list 'menu-item "Follow MLink Link in Other Window" 'mlinks-goto-other-window
              :enable '(and (boundp 'mlinks-mode)
                            mlinks-mode)
              :help "Follow MLinks Link in Other Window"))
      (define-key link-map [mlinks-goto-link]
        (list 'menu-item "Follow MLink Link" 'mlinks-goto
              :enable '(and (boundp 'mlinks-mode)
                            mlinks-mode)
              :help "Follow MLinks Link"))
      (define-key link-map [nxhtml-separator-follow-mlink] (list 'menu-item "--"))
      (define-key link-map [mlinks-next-link]
        (list 'menu-item "Next MLink Link" 'mlinks-forward-link
              :enable '(and (boundp 'mlinks-mode)
                            mlinks-mode)
              :help "Go to next MLinks link"))
      (define-key link-map [mlinks-prev-link]
        (list 'menu-item "Previous MLink Link" 'mlinks-backward-link
              :enable '(and (boundp 'mlinks-mode)
                            mlinks-mode)
              :help "Go to previous MLinks link"))

;;       (define-key link-map [nxhtml-next-href]
;;         (list 'menu-item "Next Link" 'nxhtml-next-href
;;          :help "Go to next href field"))
;;       (define-key link-map [nxhtml-prev-href]
;;         (list 'menu-item "Previous Link" 'nxhtml-prev-href
;;          :help "Go to previous href field"))

      (define-key map [nxhtml-link-map]
        (list 'menu-item "Links" link-map
              :visible `(not (derived-mode-p 'dired-mode))
              )))


    (let ((site-map (make-sparse-keymap)))
      (define-key map [nxhtml-site-map]
        (list 'menu-item "Site" site-map))
      (define-key site-map [html-site-global-mode]
        (list 'menu-item "HTML Site Global Mode"
              'html-site-global-mode
              :button '(:toggle . (and (boundp 'html-site-global-mode)
                                       html-site-global-mode))))
      (define-key site-map [nxhtml-site-separator] (list 'menu-item "--"))
      (define-key site-map [nxhtml-customize-site-list]
        (list 'menu-item "Edit Sites" (lambda () (interactive)
                                        (customize-option-other-window 'html-site-list))))
      (define-key site-map [nxhtml-set-site]
        (list 'menu-item "Set Current Site" 'html-site-set-site))
      (define-key site-map [nxhtml-site-separator-1] (list 'menu-item "--"))
      (define-key site-map [nxhtml-dired-site-top]
        (list 'menu-item "Dired Site" 'html-site-dired-current))
      (define-key site-map [nxhtml-find-site-file]
        (list 'menu-item "Find File in Site" 'html-site-find-file))
      (define-key site-map [nxhtml-site-search-separator]
        (list 'menu-item "--" nil))
      (define-key site-map [nxhtml-replace-in-site]
        (list 'menu-item "Replace in Site Files" 'html-site-query-replace))
      (define-key site-map [nxhtml-rgrep-in-site]
        (list 'menu-item "Search Site Files" 'html-site-rgrep))
      )

    (define-key map [nxhtml-insert-separator]
      (list 'menu-item "--" nil
            :visible `(not (derived-mode-p 'dired-mode))
            ))
    (let ((chunk-map (make-sparse-keymap)))
      (define-key map [nxhtml-chunk-map]
        (list 'menu-item "Chunk" chunk-map
              :visible `(not (derived-mode-p 'dired-mode))
              :enable '(and (boundp 'mumamo-multi-major-mode)
                            mumamo-multi-major-mode)))
      (define-key chunk-map [mumamo-mark-chunk]
        (list 'menu-item "Mark Chunk"
              'mumamo-mark-chunk))
      (define-key chunk-map [nxhtml-separator-mark-chunk] (list 'menu-item "--"))
      (define-key chunk-map [mumamo-backward-chunk]
        (list 'menu-item "Backward Chunk"
              'mumamo-backward-chunk))
      (define-key chunk-map [mumamo-forward-chunk]
        (list 'menu-item "Forward Chunk"
              'mumamo-forward-chunk)))
    (let ((tag-map (make-sparse-keymap)))
      (define-key map [nxhtml-tag-map]
        (list 'menu-item "Move by Tag" tag-map
              :visible '(or (derived-mode-p 'nxml-mode)
                            (derived-mode-p 'sgml-mode))
              :enable '(or (derived-mode-p 'nxml-mode)
                           (nxhtml-nxhtml-in-buffer))))
      (define-key tag-map [nxml-forward-par]
          (list 'menu-item "Forward Paragraph"
                'nxml-forward-paragraph))
      (define-key tag-map [nxml-backward-par]
          (list 'menu-item "Backward Paragraph"
                'nxml-backward-paragraph))
      (define-key tag-map [nxml-insert-separator-move2] (list 'menu-item "--"))
      (define-key tag-map [nxml-down]
          (list 'menu-item "Forward Into Tag"
                'nxml-down-element))
      (define-key tag-map [nxml-backward-up]
          (list 'menu-item "Backward Out of Tag"
                'nxml-backward-up-element))
      (define-key tag-map [nxml-insert-separator-move] (list 'menu-item "--"))
      (define-key tag-map [nxml-forward]
          (list 'menu-item "Forward Balanced Tag"
                'nxml-forward-element))
      (define-key tag-map [nxml-backward]
          (list 'menu-item "Backward Balanced Tag"
                'nxml-backward-element))
      )

    (let ((cmpl-map (make-sparse-keymap)))
      (define-key map [nxhtml-cmpl-map]
        (list 'menu-item "Completion and Validation" cmpl-map
              ;; :enable '(or (derived-mode-p 'nxml-mode) (nxhtml-nxhtml-in-buffer))
              :visible `(not (derived-mode-p 'dired-mode))
              ))
      (let ((val-map (make-sparse-keymap)))
        (define-key cmpl-map [nxhtml-cmpl-val-map]
          (list 'menu-item "Validation Helpers (for php etc)" val-map
                :enable '(nxhtml-nxhtml-in-buffer)
                :visible '(nxhtml-nxml-html-in-buffer)))
;;;         (define-key val-map [nxhtml-strval-mode]
;;;           (list 'menu-item "Allow attr=\"<?php...?>\" etc"
;;;                 'nxhtml-strval-mode
;;;                 :button '(:toggle . nxhtml-strval-mode)))
        (define-key val-map [nxhtml-toggle-warnings]
          (list 'menu-item "Hide Validation Errors"
                'nxhtml-toggle-visible-warnings
                :button '(:toggle . (not (nxhtml-warnings-are-visible)))
                ))
        (define-key val-map [nxhtml-error-separator] (list 'menu-item "--"))
        (define-key val-map [nxhtml-remove-saved-validation-header]
          (list 'menu-item "Remove File's Fictive XHTML Validation Header"
                'nxhtml-remove-saved-validation-header
                ;; Fix-me: maybe a better enable here?
                :enable 'nxhtml-validation-header-mode))
        (define-key val-map [nxhtml-save-validation-header]
          (list 'menu-item "Save File's Fictive XHTML Validation Header"
                'nxhtml-save-validation-header
                :enable 'nxhtml-validation-header-mode))
        (define-key val-map [nxhtml-set-validation-header]
          (list 'menu-item "Choose Fictive XHTML Validation Header for Buffer"
                'nxhtml-set-validation-header))
        (define-key val-map [nxhtml-update-validation-header]
          (list 'menu-item "Update Fictive XHTML Validation Header for Buffer"
                'nxhtml-update-validation-header))
        (define-key val-map [nxhtml-use-saved-val-separator] (list 'menu-item "--"))
;;;         (let ((afic-map (make-sparse-keymap)))
;;;           (define-key val-map [nxhtml-afic-map]
;;;             (list 'menu-item "Automatic Fictive XHTML Validation Header" afic-map))
;;;           (define-key afic-map [nxhtml-validation-header-mumamo-set]
;;;             (list 'menu-item "Customize Automatic XHTML Validation Turn On"
;;;                   (lambda () (interactive) (customize-option 'nxhtml-validation-header-mumamo-modes))))
;;;           (define-key afic-map [nxhtml-validation-header-mumamo]
;;;             (list 'menu-item "Turn on Fictive XHTML Validation Header with MuMaMo"
;;;                   'nxhtml-validation-header-if-mumamo-toggle
;;;                   :button '(:toggle . nxhtml-validation-header-if-mumamo))))
        (define-key val-map [nxhtml-show-validation-header]
          (list 'menu-item "Display Fictive XHTML Validation Header"
                'rngalt-display-validation-header-toggle
                :help-echo "Displays the Fictive XHTML validation header (if any) at top of buffer"
                :button '(:toggle . rngalt-display-validation-header)))
        (define-key val-map [nxhtml-recheck-validation-header]
          (list 'menu-item "Recheck Fictive XHTML Validation Header in Buffer"
                'nxhtml-recheck-validation-header
                :enable 'nxhtml-validation-header-mode))
        (define-key val-map [nxhtml-validation-header-mode]
          (list 'menu-item "Use Fictive XHTML Validation Header in Buffer"
                'nxhtml-validation-header-mode
                :button '(:toggle . nxhtml-validation-header-mode)))
      )
      (define-key cmpl-map [nxhtml-validation-separator]
        (list 'menu-item "--" nil
              :visible '(nxhtml-nxml-html-in-buffer)))
      (let ((style-map (make-sparse-keymap)))
        (define-key cmpl-map [nxhtml-cmpl-style-map]
          (list 'menu-item "Completion Style" style-map
                :visible '(nxhtml-nxml-html-in-buffer)
                :enable '(nxhtml-nxhtml-in-buffer)))
        ;;(defun nxhtml-nxml-html-in-buffer ()
        (define-key style-map [popcmp-with-help]
          (list 'menu-item "Show Short Help Beside Alternatives"
                'popcmp-short-help-beside-alts-toggle
                :button '(:toggle . popcmp-short-help-beside-alts)))
        (define-key style-map [nxhtml-tag-do-also]
          (list 'menu-item "Complete Tag Extras"
                'nxhtml-tag-do-also-toggle
                :button '(:toggle . nxhtml-tag-do-also)))
        (define-key style-map [popcmp-group-alternatives]
          (list 'menu-item "Group Alternatives"
                'popcmp-group-alternatives-toggle
                :button '(:toggle . popcmp-group-alternatives)))
        (define-key style-map [popcmp-popup-completion]
          (list 'menu-item "Popup Style Completion"
                'popcmp-popup-completion-toggle
                :button '(:toggle . popcmp-popup-completion)))
        )
      (define-key cmpl-map [nxhtml-cmpl-separator]
        (list 'menu-item "--" nil
              :visible '(nxhtml-nxml-html-in-buffer)))
      (define-key cmpl-map [nxhtml-untag-element]
        (list 'menu-item "Untag Element" 'nxml-untag-element
              :enable '(nxhtml-nxhtml-in-buffer)
              :visible '(nxhtml-nxml-html-in-buffer)))
      (define-key cmpl-map [rngalt-finish-element]
        (list 'menu-item "Insert End Tag" 'rngalt-finish-element
              :enable '(nxhtml-nxhtml-in-buffer)
              :visible '(nxhtml-nxml-html-in-buffer)))
      (define-key cmpl-map [nxhtml-complete]
        (list 'menu-item "Complete tag, attribute etc" 'nxml-complete
              :enable '(nxhtml-nxml-in-buffer)
              :visible '(nxhtml-nxml-html-in-buffer)))
      (define-key cmpl-map [nxhtml-tab-complete-div]
        (list 'menu-item "--" nil
              :visible '(nxhtml-nxml-html-in-buffer)))
      (define-key cmpl-map [nxhtml-tab-complete]
        (list 'menu-item "Indent and then Complete" 'tabkey2-first))
      )

    map))

(defvar nxhtml-minor-mode-map
  (let ((map (make-sparse-keymap)))
    (require 'xhtml-help nil t)
    (when (featurep 'xhtml-help)
      (define-key map [(control ?c) ?? ?x] 'nxhtml-short-tag-help)
      (define-key map [(control ?c) ?? ?c] 'xhtml-help-show-css-ref)
      )
    (define-key map [(control ?c) ?_] 'nxhtml-toggle-visible-warnings)
    (define-key map [menu-bar nxhtml-minor-mode]
      (list 'menu-item "nXhtml" nxhtml-minor-mode-menu-map))
    map))

;;;###autoload
(define-minor-mode nxhtml-minor-mode
  "Minor mode to turn on some key and menu bindings.
See `nxhtml-mode' for more information."
  :keymap nxhtml-minor-mode-map
  :group 'nxhtml
  ;;(if nxhtml-minor-mode (message "+++> Turning on nxhtml-minor-mode") (message "---> Turning off nxhtml-minor-mode"))
  )
;;(put 'nxhtml-minor-mode 'permanent-local t)

(defcustom nxhtml-minor-mode-modes
  '(
    nxhtml-mode
    nxml-mode
    html-mode
    sgml-mode
    xml-mode
    php-mode
    css-mode
    javascript-mode
    java-mode ;; jsp
    image-mode
    ;;
    dired-mode
    )
  "List for turning on `nxhtml-minor-mode'.
If the buffer's major modes is any of those in this list then
`nxhtml-global-minor-mode' will turn on `nxhtml-minor-mode' in
the buffer."
  :type '(repeat (symbol :tag "Major mode"))
  :group 'nxhtml)

(defun nxhtml-maybe-turn-on-minor-mode ()
  "Maybe turn on `nxhtml-minor-mode'.
See `nxhtml-minor-mode-modes'."
  (unless (or (minibufferp (current-buffer))
              (string= " " (substring (buffer-name) 0 1))
              (string= "*" (substring (buffer-name) 0 1))
              )
    (let ((on (and (boundp 'mumamo-multi-major-mode)
                   mumamo-multi-major-mode)))
      (dolist (major nxhtml-minor-mode-modes)
        (when (derived-mode-p major)
          (setq on t)))
;;       (when (string= "php" (file-name-extension (buffer-file-name)))
;;         (lwarn 't :warning "on=%s, major-mode=%s" on major-mode))
      (when on
        (nxhtml-minor-mode 1)))))

;;;###autoload
(define-globalized-minor-mode nxhtml-global-minor-mode
  nxhtml-minor-mode
  nxhtml-maybe-turn-on-minor-mode
  :require 'nxhtml-menu
  :group 'nxhtml)
(custom-reevaluate-setting 'nxhtml-global-minor-mode)
(when nxhtml-global-minor-mode (nxhtml-global-minor-mode 1))


(defun nxhtml-docfile ()
  (let* ((libfile (locate-library "nxhtml"))
         (docfile (expand-file-name "doc/nxhtml.html"
                                    (file-name-directory libfile))))
    docfile))

(defun nxhtml-docfile-url ()
  (concat "file://" (nxhtml-docfile)))

(defun nxhtml-overview ()
  "Show a HTML page with an overview of nXhtml."
  (interactive)
  (browse-url (nxhtml-docfile-url)))

(defun nxhtml-tutorials ()
  "Show a HTML page with a list of tutorials for nXhtml'."
  (interactive)
  (browse-url "http://ourcomments.org/Emacs/nXhtml/tut/tutorials.html"))

(defun nxhtml-custom-valfaced (value &optional bgcolor)
  (let ((v (if (sequencep value)
               (copy-seq value)
             value))
        (bgcolor (if bgcolor bgcolor "RGB:FF/FF/AA")))
    (put-text-property 0 (length v)
                       'face (list
                              'bold
                               (cons 'background-color bgcolor)
                               )
                       v)
    v))
(defun nxhtml-custom-insert-nxhtml-row (symbol nxhtml-value description)
  (let ((desc (if description
                   (format "%s (%s)" description symbol)
                 (format "%s" (custom-unlispify-tag-name symbol)))))
    (widget-insert "  " description " (")
    (nxhtml-custom-describe-defun symbol)
    (widget-insert "): "
                   (nxhtml-custom-valfaced
                    (format "%s" (symbol-value symbol))
                    (if (eq (symbol-value symbol)
                            nxhtml-value)
                        "GreenYellow"
                      "gainsboro"))
                   "\n")))

(defun nxhtml-custom-h1(title &optional divider top-newline)
  (let ((s title))
    (put-text-property 0 (length s)
                       'face '(:weight bold
                               :height 1.4
                               :foreground "DarkGreen"
                               ;;:underline t
                               )
                       s)
    (when top-newline (widget-insert "\n"))
    ;;(when divider (widget-insert (nxhtml-custom-divider (length s))))
    (widget-insert s)
    ))

(defun widget-button-notify (widget &rest ignore)
  (apply (widget-get widget 'function) (widget-get widget 'data)))

(defun widget-insert-link (txt function data)
  (widget-insert-button txt function data
                        :button-face 'link
                        :mouse-face 'highlight
                        :button-prefix ""
                        :button-suffix ""))

(defun widget-insert-button (txt function data &rest keywords)
  (let ((btn (apply 'widget-create
                    (append
                     '(push-button
                       :notify
                       widget-button-notify)
                     keywords
                     (list txt)))))
    (widget-put btn 'data data)
    (widget-put btn 'function function)))

(defun nxhtml-custom-url-link (txt url)
  (let ((plain-url (substring-no-properties url)))
    (unless (equal txt url)
      (put-text-property 0 (length txt) 'help-echo plain-url txt))
    (put-text-property 0 (length txt) 'mouse-face 'highlight txt)
    (widget-insert-link txt 'browse-url (list url))))

(defun nxhtml-custom-describe-defun (sym &optional help)
  (let ((txt (symbol-name sym)))
    (when help
      (put-text-property 0 (length txt) 'help-echo help txt))
    (put-text-property 0 (length txt) 'mouse-face 'highlight txt)
    (widget-insert-link txt 'describe-function (list sym))))

;; (defun nxhtml-quick-customize (&optional same-window)
;;   "Show page for Quick Customize of nXhtml."
;;   (interactive)
;;   (require 'nxhtml)
;;   (require 'custom)
;;   (require 'cus-edit)
;;   (if same-window
;;       (switch-to-buffer "*Quick Customize nXhtml*")
;;     (switch-to-buffer-other-window "*Quick Customize nXhtml*"))
;;   (kill-all-local-variables)
;;   (custom-mode)
;;   (let ((inhibit-read-only t))
;;     (erase-buffer))
;;   (let ((sFound "found")
;;         (sError "error"))
;;     (put-text-property 0 (length sFound)
;;                        'face '(bold
;;                                (foreground-color . "green")) sFound)
;;     (put-text-property 0 (length sError)
;;                        'face '(bold
;;                                (foreground-color . "red")) sError)
;;     (let* (
;;            (default-used "(not set yet - default used)")
;;            )
;;       (nxhtml-custom-h1 "Quick Customize for nXhtml" t)
;;       (widget-insert "

;; This page is for a quick and easy setup of some ")
;;       (nxhtml-custom-url-link "nXhtml" (nxhtml-docfile-url))
;;       (widget-insert " features
;; that I did not want to turn on by default since they alter what
;; happens when you open a file.  I suggest however that you turn
;; them on since they are quite useful if you just understands what
;; is happening.

;; The values you set here are saved so that they will be used next
;; time you start Emacs too.")
;;       ;;(widget-insert-link "customize nXhtml" 'customize-group (list 'nxhtml))
;;       (widget-insert "\n\n")

;;       (nxhtml-custom-insert-nxhtml-row 'nxhtml-global-minor-mode t "Show the nXhtml menu in all relevant buffers\n\t")
;;       ;;(nxhtml-custom-insert-nxhtml-row 'mumamo-global-mode t "Turn on Multiple Major Mode in all relevant buffers\n\t")
;;       ;;(nxhtml-custom-insert-nxhtml-row 'mlinks-global-mode t "Make link of lins, for example href=\"...\"\n\t")
;;       ;;(nxhtml-custom-insert-nxhtml-row 'indent-region-mode t "Use TAB to indent region when it is selected\n\t")

;;       (widget-insert "\n")
;;       (widget-insert-button " Turn them all on "
;;                           (lambda ()
;;                             (nxhtml-quick-all t)
;;                             (nxhtml-quick-customize t))
;;                           nil)
;;       (widget-insert "  ")
;;       (widget-insert-button " Turn them all off "
;;                           (lambda ()
;;                             (nxhtml-quick-all nil)
;;                             (nxhtml-quick-customize t))
;;                           nil)
;;       (beginning-of-line)
;;       )))

;; (defun nxhtml-quick-all (on)
;;   (custom-set-and-prepare-save 'nxhtml-global-minor-mode on)
;;   ;;(custom-set-and-prepare-save 'mumamo-global-mode on)
;;   (custom-set-and-prepare-save 'indent-region-mode on)
;;   (when custom-file
;;     (custom-save-all)))

(defun custom-set-and-prepare-save (symbol value)
  "Set SYMBOL to VALUE and add to customize.
Both the current value and the value to save is set, but
`custom-save-all' must be called to save customization."
  (customize-set-variable symbol value)
  (customize-set-value symbol value)
  (customize-mark-to-save symbol))


;;(nxhtml-quick-customize)

(defun nxhtml-welcome ()
  "Show welcome information."
  (interactive)
  (require 'cus-edit)
  (let* ((bufnam "*nXhtml Welcome*")
         (oldbuf (get-buffer bufnam))
         (curwin (selected-window)))
    (switch-to-buffer-other-window bufnam)
    (unless oldbuf
      (let ((inhibit-read-only t)
            (here (point)))
        (custom-mode)
        (setq cursor-in-non-selected-windows nil)
        (nxhtml-custom-h1 "Welcome to nXhtml - a package for web editing" t)
        (insert "\n\n")
        (setq here (point))
        (insert "If you have not done it already it might "
                "be a good time to read at least The Quick Guide in the ")
        (nxhtml-custom-url-link "nXhtml overview" (nxhtml-docfile-url))
        (insert " now.\n\n")
        (fill-region here (point))
        (setq here (point))
        (insert "And oh, wait! If you are new to Emacs too you might want "
                "to take a quick ")
        (nxhtml-custom-url-link
         "Emacs tour"
         "http://www.gnu.org/software/emacs/tour/")
        (insert ".  And then perhaps the Emacs tutorial "
                "(which is in the Help menu above).\n\n")
        (fill-region here (point))
        (setq here (point))

;;;         (insert "

;;; To make the use of nXhtml as smooth as possible I also recommend
;;; that you go to ")

;;;         (widget-insert-link "Quick Customize nXhtml"
;;;                             (lambda ()
;;;                               (nxhtml-quick-customize))
;;;                             nil)

;;;         (insert " and follow the instructions
;;; there.")

        (unless (nxhtml-skip-welcome)
          (insert "Click to ")
          (widget-insert-link "remove this message"
                              (lambda ()
                                (customize-option 'nxhtml-skip-welcome))
                              nil)
          (insert " at startup.  (This page is still "
                  "available in the nXhtml menu, at the bottom.)"))
        (fill-region here (point))
        (setq here (point))
        (goto-char (point-min))))
    (select-window curwin)))

(defcustom nxhtml-skip-welcome nil
  "Turn this on to always skip the nXhtml welcome message."
  :type 'boolean
  :group 'nxhtml)

(defun nxhtml-skip-welcome ()
  "Return t if nXhtml welcome message should be skipped.
If nil then the message will be shown when you open the first
file using nxhtml-mode."
   (or nxhtml-skip-welcome
       (and nxhtml-global-minor-mode
            ;;mumamo-global-mode
            ;;indent-region-mode
            )))

(defun nxhtml-say-welcome-unless-skip ()
  (unless (nxhtml-skip-welcome)
    (nxhtml-welcome)))

;; Show welcome screen once after loading nxhtml:
(eval-after-load 'nxhtml
  ;; Use a short delay if something like desktop is used:
  '(run-with-idle-timer 0.5 nil 'nxhtml-say-welcome-unless-skip))

(provide 'nxhtml-menu)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; nxhtml-menu.el ends here
