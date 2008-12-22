;; nxhtml-autoload.el -- Autoloading of nxthml-mode

;; Copyright (C) 2005, 2006, 2007 by Lennart Borgman

;; Author: Lennart Borgman <lennartDOTborgmanDOT073ATstudentDOTluDOTse>
;; Created: Sat Feb 11 00:06:14 2006
;; Version: 0.51
;; Last-Updated: 2008-02-13T01:21:14+0100 Wed
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

;;(require 'ourcomments-util)

(unless (featurep 'nxml-enc)
  ;; This is for the case when nXml is included in Emacs
  (require 'nxml-mode))

(if (not (or (featurep 'nxml-enc) ;; nXml not in Emacs
             (featurep 'nxml-mode))) ;; nXml in Emacs
    (progn
      (lwarn
       '(nxhtml-autoload)
       :emergency
       (concat
        "\n\n\nERROR: nxml not loaded!\n\n"
        "    Please load nxml before loading nxhtml!\n"
        "    Load nxml by loading rng-auto.el in the nxml distribution.\n\n\n\n"))
      (sit-for 10))

  (add-to-list 'load-path
               (file-name-directory
                (if load-file-name load-file-name buffer-file-name)))

  ;;(autoload 'nxhtml-report-bug "nxhtml-bug" "Report a bug in nXhtml." t)
  ;;(autoload 'nxhtml-mode "nxhtml" "Major mode for editing XHTML documents." t)
  ;;(autoload 'nxhtml-global-minor-mode "nxhtml-menu" "Toggle `nxhtml-minor-mode' in every buffer." t)
  ;;(autoload 'gimp-edit-buffer "gimp" "Edit image file in current buffer with GIMP." t)
  ;;(autoload 'gimp-edit-file "gimp" "Edit file with GIMP." t)
  ;; Testing
  (let* ((this-dir (file-name-directory (if load-file-name load-file-name buffer-file-name)))
         (test-q-file (expand-file-name "../tests/nxhtmltest-Q.el" this-dir))
         (test-file (expand-file-name "../tests/nxhtmltest-suites.el" this-dir)))
    ;;(autoload 'nxhtmltest-run-Q      test-q-file "Run all tests defined for nXhtml in fresh Emacs." t)
    ;;(autoload 'nxhtmltest-run        test-file   "Run all tests defined for nXhtml." t)
    ;;(autoload 'nxhtmltest-run-indent test-file   "Run all indentation tests defined for nXhtml." t)
    )

  ;;   (require 'fmode)
  ;;   (fmode-replace-default-mode 'html-mode 'nxhtml-mode)
  ;;   (fmode-replace-default-mode 'xml-mode 'nxml-mode)

  ;; Replaced all the major mode file stuff here with majmodpri and
  ;; just adding to auto-mode-alist etc:
  ;;(require 'majmodpri)

  ;; Add entries similar to those that are already there for html-mode
  ;; and xml-mode.
  (dolist (mode-list '(auto-mode-alist magic-fallback-mode-alist))
    (dolist (rec (symbol-value mode-list))
      (when (eq (cdr rec) 'html-mode)
        (add-to-list mode-list (cons (car rec) 'nxhtml-mode)))
      (when (eq (cdr rec) 'html-mode)
        (add-to-list mode-list (cons (car rec) 'nxhtml-mumamo-mode)))
      (when (eq (cdr rec) 'html-mode)
        (add-to-list mode-list (cons (car rec) 'html-mumamo-mode)))
      (when (eq (cdr rec) 'xml-mode)
        (add-to-list mode-list (cons (car rec) 'nxml-mode)))
      ))

  ;;(require 'html-site)
  ;;(require 'nxhtml-menu)

  ;;; Change below if you need to:
  ;;(autoload 'rnc-mode "rnc-mode" "Major mode for editing RELAX NG" t)
  ;;(autoload 'css-mode "css-mode" "Mode for editing css files" t)
  ;;(autoload 'javascript-mode "javascript" "Mode for JavaScript" t)
  ;;(autoload 'js2-fl-mode "js2-font-lock-new" "Mode for JavaScript" t)
  ;;(autoload 'php-mode "php-mode" "Mode for editing php files" t)
  ;;(autoload 'smarty-mode "smarty-mode" "Mode for editing php smarty files" t)
  ;;(autoload 'csharp-mode "csharp-mode" "Mode for editing C# code" t)
  ;;(autoload 'django-mode "django" "Simple Django mode for use with mumamo." t)
  (eval-after-load 'nxml-mode
    '(progn
      (define-key nxml-mode-map [C-M-left]  'nxml-backward-element)
      (define-key nxml-mode-map [C-M-right] 'nxml-forward-element)
      (define-key nxml-mode-map [C-M-up]    'nxml-backward-up-element)
      (define-key nxml-mode-map [C-M-down]  'nxml-down-element)))

  ;; MozLab support, for more info see moz.el
  ;;(autoload 'inferior-moz-mode "moz" "MozRepl Inferior Mode" t)
  ;;(autoload 'moz-minor-mode "moz" "MozRepl Minor Mode" t)
  (defun javascript-moz-setup () (moz-minor-mode 1))
  (add-hook 'javascript-mode-hook 'javascript-moz-setup)
  (add-hook 'js2-fl-mode          'javascript-moz-setup)

  ;; Development versions support
  ;;(autoload 'udev-rinari-update "udev-rinari" "Fetch and install rinari from the devel sources." t)
  ;;(autoload 'udev-cedet-update "udev-cedet" "Fetch and install CEDET from the devel sources." t)
  ;;(autoload 'udev-ecb-update "udev-ecb" "Fetch and install ECB from the devel sources." t)

  ;;(require 'mumamo-fun)
  ;;(require 'nxhtml-mumamo)
  ;;(require 'as-external)
  )

;; (setq magic-mode-alist nil)
;; (defcustom nxhtml-magic-mode-alist
;;   '(

(add-to-list 'magic-mode-alist
             '("\\(?:.\\|\n\\)\\{,200\\}xmlns:py=\"http://genshi.edgewall.org/\""
               . genshi-nxhtml-mumamo-mode))
;;;     )
;;;   "List to add to `magic-mode-alist'.
;;; Works similar to `nxhtml-auto-mode-alist'.  Note that
;;; `magic-mode-alist' is the first thing tried when choosing a major
;;; mode."
;;;   :type '(repeat (cons :tag "Enter file name pattern and major mode"
;;;                        (regexp :tag "Regexp for file name")
;;;                        (major-mode-function :tag "Major mode")))
;;;   :set (lambda (sym val)
;;;          (set-default sym val)
;;;          (dolist (v val)
;;;            (add-to-list 'magic-mode-alist v)))
;;;   :group 'nxhtml)

;; (defcustom nxhtml-auto-mode-alist
;;   '(
(add-to-list 'auto-mode-alist '("\\.htm\\'"      . nxhtml-mumamo-mode))
(add-to-list 'auto-mode-alist '("\\.html\\'"     . nxhtml-mumamo-mode))
(add-to-list 'auto-mode-alist '("\\.xhtm\\'"     . nxhtml-mumamo-mode))
(add-to-list 'auto-mode-alist '("\\.xhtml\\'"    . nxhtml-mumamo-mode))
(add-to-list 'auto-mode-alist '("\\.html\\'"     . nxhtml-mumamo-mode))
(add-to-list 'auto-mode-alist '("\\.htmlf\\'"    . nxhtml-mumamo-mode))
(add-to-list 'auto-mode-alist '("\\.xhtml\\'"    . nxhtml-mumamo-mode))
(add-to-list 'auto-mode-alist '("\\.xhtmlf\\'"   . nxhtml-mumamo-mode))
(add-to-list 'auto-mode-alist '("\\.php\\'"      . nxhtml-mumamo-mode))
(add-to-list 'auto-mode-alist '("\\.phtml\\'"    . nxhtml-mumamo-mode))
(add-to-list 'auto-mode-alist '("\\.jsp\\'"      . jsp-nxhtml-mumamo-mode))
(add-to-list 'auto-mode-alist '("\\.asp\\'"      . asp-nxhtml-mumamo-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'"   . django-nxhtml-mumamo-mode))
(add-to-list 'auto-mode-alist '("\\.rhtml\\'"    . eruby-nxhtml-mumamo-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'"      . eruby-nxhtml-mumamo-mode))
(add-to-list 'auto-mode-alist '("\\.phps\\'"     . smarty-nxhtml-mumamo-mode))
(add-to-list 'auto-mode-alist '("\\.epl\\'"      . embperl-nxhtml-mumamo-mode))
(add-to-list 'auto-mode-alist '("\\.lzx\\'"       . laszlo-nxml-mumamo-mode))
(add-to-list 'auto-mode-alist '("\\.ghtml\\'"     . genshi-nxhtml-mumamo-mode))
(add-to-list 'auto-mode-alist '("\\.js\\'"       . javascript-mode))
(add-to-list 'auto-mode-alist '("\\.css\\'"      . css-mode))
(add-to-list 'auto-mode-alist '("\\.rnc\\'"      . rnc-mode))
;;;     )
;;;   "List to add to `auto-mode-alist'.
;;; This list is added to `auto-mode-alist' when loading
;;; nxhtml-autostart.el and will therefore help Emacs to determine
;;; which major mode a file will be opened in.

;;; Please notice that `mumamo-mode' may override this choice of
;;; major mode when setting the chunk family.  The chunk family then
;;; determines the major mode.  The chunk family is set from
;;; `mumamo-filenames-list'.  You may want to synch the two list, but
;;; it is not necessary.  However not synching may perhaps lead to
;;; surpricing results.  To synch the lists means that the mode in
;;; this list should correspond to the main major mode in the mumamo
;;; chunk family.

;;; * Note: This variable (nxhtml-auto-mode-alist) is just for your
;;;   convenience.  Probably most users normally just adds to
;;;   `auto-mode-alist' in their .emacs with lines like

;;;     \(add-to-list 'auto-mode-alist '(\"\\.x?html?\\'\"  . nxhtml-mode))

;;;   but doing something like that here would make it impossible to
;;;   customize that easily for you."
;;;   ;;:type '(alist :key-type regexp :tag "hej" :value-type major-mode-function)
;;;   :type '(repeat (cons :tag "Enter file name pattern and major mode"
;;;                        (regexp :tag "Regexp for file name")
;;;                        (major-mode-function :tag "Major mode")))
;;;                        ;;(command :tag "Major mode")))
;;;   :set (lambda (sym val)
;;;          (set-default sym val)
;;;          (dolist (v val)
;;;            (add-to-list 'auto-mode-alist v)))
;;;   :group 'nxhtml)

(majmodpri-sort-lists)

(defvar nxhtml-src-dir (file-name-directory
                        (if load-file-name load-file-name buffer-file-name)))

(provide `nxhtml-autoload)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; nxhtml-autoload.el ends here
