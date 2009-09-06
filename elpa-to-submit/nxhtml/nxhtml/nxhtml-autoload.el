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

(message "nxhtml-autoload starting ... (hm, should maybe be renamed ...)")

(eval-when-compile (require 'majmodpri))
(eval-when-compile (require 'moz))

;;; Convenient moving by tags:
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
;;(add-hook 'js2-fl-mode-hook     'javascript-moz-setup)


;; Add nXhtml entries similar to those that are already there for
;; html-mode and xml-mode.
(dolist (mode-list '(auto-mode-alist magic-fallback-mode-alist magic-mode-alist))
  (dolist (rec (symbol-value mode-list))
    (when (eq (cdr rec) 'html-mode)
      (add-to-list mode-list (cons (car rec) 'nxhtml-mode)))
    (when (eq (cdr rec) 'html-mode)
      (add-to-list mode-list (cons (car rec) 'nxhtml-mumamo-mode)))
    ;; (when (eq (cdr rec) 'html-mode)
    ;;   (add-to-list mode-list (cons (car rec) 'html-mumamo-mode)))
    (when (eq (cdr rec) 'xml-mode)
      (add-to-list mode-list (cons (car rec) 'nxml-mode)))
    ))

;; Add multi major mode entries.
(add-to-list 'magic-mode-alist
             '("\\(?:.\\|\n\\)\\{,200\\}xmlns:py=\"http://genshi.edgewall.org/\""
               . genshi-nxhtml-mumamo-mode))
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
(add-to-list 'auto-mode-alist '("\\.ghtml\\'"     . genshi-nxhtml-mumamo-mode))

;; Add html-mumamo style entry if there is an nxhtml-mumamo style entry.
(save-match-data
  (dolist (mode-list '(auto-mode-alist magic-fallback-mode-alist magic-mode-alist))
    (dolist (rec (symbol-value mode-list))
      (let* ((mode (cdr rec))
             (name (when (symbolp mode) (symbol-name mode)))
             nxmode)
        (when (and name
                   (string-match "nxhtml-mumamo" name))
          (setq name (replace-regexp-in-string "nxhtml-mumamo" "html-mumamo" name t t))
          (setq nxmode (intern-soft name))
          (when nxmode
            (add-to-list mode-list (cons (car rec) nxmode))))))))

(add-to-list 'auto-mode-alist '("\\.lzx\\'"       . laszlo-nxml-mumamo-mode))
(add-to-list 'auto-mode-alist '("\\.js\\'"       . javascript-mode))
(add-to-list 'auto-mode-alist '("\\.css\\'"      . css-mode))
(add-to-list 'auto-mode-alist '("\\.rnc\\'"      . rnc-mode))

(majmodpri-sort-lists)

(defvar nxhtml-src-dir (file-name-directory
                        (if load-file-name load-file-name buffer-file-name)))

(message "nxhtml-autoload finished")

(provide 'nxhtml-autoload)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; nxhtml-autoload.el ends here
