;;; nxhtml-mumamo.el --- Multi major modes using nxhtml
;;
;; Author: Lennart Borgman (lennart O borgman A gmail O com)
;; Created: 2008-03-10T19:04:20+0100 Mon
(defconst nxhtml-mumamo:version "0.5")
;; Last-Updated: 2009-01-06 Tue
;; URL:
;; Keywords:
;; Compatibility:
;;
;; Features that might be required by this library:
;;
  ;; `backquote', `bytecomp', `mumamo', `mumamo-fun'.
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

(eval-when-compile (require 'cl))
(eval-when-compile (require 'nxhtml))
(eval-when-compile (require 'rng-valid nil t))
(require 'mumamo-fun)

;; (defgroup nxhtml-auto-val-head nil
;;   "Automatic turn on of XHTML validation headers."
;;   :group 'nxhtml)

;; (defmacro define-fictive-validation-header-toggle (fun-sym default-value)
;;   (let* ((fun-name (symbol-name fun-sym))
;;          (custom-sym (intern (concat fun-name "-auto-val-head")))
;;          (hook-sym (intern-soft (concat fun-name "-hook")))
;;          (docstring
;;           (concat "Automatic XHTML validation header for `" fun-name "'.
;; ´")))
;;     (assert hook-sym)
;;     `(defcustom ,custom-sym ,default-value
;;        ,docstring
;;        :type 'boolean
;;        :set (lambda (sym val)
;;               (set-default sym val)
;;               (if val
;;                   (add-hook ',hook-sym 'nxhtml-turn-on-validation-header-mode)
;;                 (remove-hook ',hook-sym 'nxhtml-turn-on-validation-header-mode)))
;;        :group 'nxhtml-auto-val-head)
;;     ))

;; Fix-me: add chunk type attr string as last alternative. This will
;; allow things like myattr="<?php echo ?>".

;;;###autoload
(define-mumamo-multi-major-mode nxhtml-mumamo-mode
  "Turn on multiple major modes for (X)HTML with main mode `nxhtml-mode'.
This covers inlined style and javascript and PHP.

See also `mumamo-alt-php-tags-mode'."
  ("nXhtml Family" nxhtml-mode
   (mumamo-chunk-xml-pi
    mumamo-chunk-alt-php
    mumamo-chunk-inlined-style
    mumamo-chunk-inlined-script
    mumamo-chunk-style=
    mumamo-chunk-onjs=
    )))
(add-hook 'nxhtml-mumamo-mode-hook 'mumamo-define-html-file-wide-keys)
;;(define-fictive-validation-header-toggle nxhtml-mumamo-mode t)

;;;###autoload
(define-mumamo-multi-major-mode embperl-nxhtml-mumamo-mode
  "Turn on multiple major modes for Embperl files with main mode `nxhtml-mode'.
This also covers inlined style and javascript."
  ("Embperl nXhtml Family" nxhtml-mode
   (mumamo-chunk-embperl-<-
    mumamo-chunk-embperl-<+
    mumamo-chunk-embperl-<!
    mumamo-chunk-embperl-<$
    mumamo-chunk-inlined-style
    mumamo-chunk-inlined-script
    mumamo-chunk-style=
    mumamo-chunk-onjs=
    )))

;;;###autoload
(define-mumamo-multi-major-mode django-nxhtml-mumamo-mode
  "Turn on multiple major modes for Django with main mode `nxhtml-mode'.
This also covers inlined style and javascript."
  ("Django nXhtml Family" nxhtml-mode
   (mumamo-chunk-django4
    mumamo-chunk-django
    mumamo-chunk-django2
    mumamo-chunk-django3
    mumamo-chunk-inlined-style
    mumamo-chunk-inlined-script
    mumamo-chunk-style=
    mumamo-chunk-onjs=
    )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Genshi / kid

(define-derived-mode nxhtml-genshi-mode nxhtml-mode "gXhtml"
  "Like `nxhtml-mode' but with Genshi rnc.
You should not use this! This is just a part of
`genshi-nxhtml-mumamo-mode', use that instead."
  (let* ((schema-dir (expand-file-name "../etc/schema/" nxhtml-src-dir))
         (genshi-rnc (expand-file-name "qtmstr-xhtml.rnc" schema-dir)))
    (message "nxhtml-src-dir =%s" nxhtml-src-dir)
    (message "schema-dir =%s" schema-dir)
    (when (or (not rng-current-schema-file-name)
              (string= "xhtml.rnc" (file-name-nondirectory rng-current-schema-file-name)))
      (condition-case err
          (progn
            (rng-set-schema-file-1 genshi-rnc)
            (rng-what-schema)
            ;;(rng-save-schema-location-1 t)
            )
        (nxml-file-parse-error
         (nxml-display-file-parse-error err)))
      (when rng-validate-mode
        (rng-validate-mode -1)
        (rng-validate-mode 1)))))

;;;###autoload
(define-mumamo-multi-major-mode genshi-nxhtml-mumamo-mode
  "Turn on multiple major modes for Genshi with main mode `nxhtml-mode'.
This also covers inlined style and javascript."
  ("Genshi HTML Family" nxhtml-genshi-mode
   (mumamo-chunk-genshi%
    mumamo-chunk-genshi$
    mumamo-chunk-xml-pi
    mumamo-chunk-alt-php
    mumamo-chunk-inlined-style
    mumamo-chunk-inlined-script
    mumamo-chunk-style=
    mumamo-chunk-onjs=
    )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; MJT

;; MJT is run in the browser. Some new tags and attributes are used.

(define-derived-mode nxhtml-mjt-mode nxhtml-mode "mjtXhtml"
  "Like `nxhtml-mode' but with genshi rnc.
You should not use this! This is just a part of
`mjt-nxhtml-mumamo-mode', use that instead."
  (let* ((schema-dir (expand-file-name "../etc/schema/" nxhtml-src-dir))
         (genshi-rnc (expand-file-name "mjt.rnc" schema-dir)))
    (message "nxhtml-src-dir =%s" nxhtml-src-dir)
    (message "schema-dir =%s" schema-dir)
    (when (or (not rng-current-schema-file-name)
              (string= "xhtml.rnc" (file-name-nondirectory rng-current-schema-file-name)))
      (condition-case err
          (progn
            (rng-set-schema-file-1 genshi-rnc)
            (rng-what-schema)
            ;;(rng-save-schema-location-1 t)
            )
        (nxml-file-parse-error
         (nxml-display-file-parse-error err)))
      (when rng-validate-mode
        (rng-validate-mode -1)
        (rng-validate-mode 1)))))

;;;###autoload
(define-mumamo-multi-major-mode mjt-nxhtml-mumamo-mode
  "Turn on multiple major modes for MJT with main mode `nxhtml-mode'.
This also covers inlined style and javascript."
  ("MJT nXhtml Family" nxhtml-mjt-mode
   (
    mumamo-chunk-mjt$
    mumamo-chunk-xml-pi
    mumamo-chunk-inlined-style
    mumamo-chunk-inlined-script
    mumamo-chunk-style=
    mumamo-chunk-onjs=
    )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Smarty

;;;###autoload
(define-mumamo-multi-major-mode smarty-nxhtml-mumamo-mode
  "Turn on multiple major modes for Smarty with main mode `nxhtml-mode'.
This also covers inlined style and javascript."
  ("Smarty nXhtml Family" nxhtml-mode
   (mumamo-chunk-xml-pi
    mumamo-chunk-smarty
    mumamo-chunk-style=
    mumamo-chunk-onjs=
    )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; JSP

;;;###autoload
(define-mumamo-multi-major-mode jsp-nxhtml-mumamo-mode
  "Turn on multiple major modes for JSP with main mode `nxhtml-mode'.
This also covers inlined style and javascript."
  ("JSP nXhtml Family" nxhtml-mode
   (mumamo-chunk-jsp
    mumamo-chunk-inlined-style
    mumamo-chunk-inlined-script
    mumamo-chunk-style=
    mumamo-chunk-onjs=
    )))

;;;###autoload
(define-mumamo-multi-major-mode eruby-nxhtml-mumamo-mode
  "Turn on multiple major modes for eRuby with main mode `nxhtml-mode'.
This also covers inlined style and javascript."
  ("eRuby nXhtml Family" nxhtml-mode
   (mumamo-chunk-eruby
    mumamo-chunk-inlined-style
    mumamo-chunk-inlined-script
    mumamo-chunk-style=
    mumamo-chunk-onjs=
    )))

;;;###autoload
(define-mumamo-multi-major-mode asp-nxhtml-mumamo-mode
  "Turn on multiple major modes for ASP with main mode `nxhtml-mode'.
This also covers inlined style and javascript."
  ("ASP nXhtml Family" nxhtml-mode
   (mumamo-chunk-asp
    mumamo-asp-chunk-inlined-script
    mumamo-chunk-inlined-script
    mumamo-chunk-style=
    mumamo-chunk-onjs=
    )))

;;;###autoload
(define-mumamo-multi-major-mode mako-nxhtml-mumamo-mode
  "Turn on multiple major modes for Mako with main mode `nxhtml-mode'.
This also covers inlined style and javascript."
;; Fix-me: test case
;;
;; Fix-me: Add chunks for the tags, but make sure these are made
;; invisible to nxml-mode parser.
;;
;; Fix-me: Maybe finally add that indentation support for one-line chunks?
  ("Mako nXhtml Family" nxhtml-mode
   (
    mumamo-chunk-mako-one-line-comment
    mumamo-chunk-mako-<%doc
    mumamo-chunk-mako-<%include
    mumamo-chunk-mako-<%inherit
    mumamo-chunk-mako-<%namespace
    mumamo-chunk-mako-<%page

    ;;mumamo-chunk-mako-<%def
    ;;mumamo-chunk-mako-<%call
    ;;mumamo-chunk-mako-<%text

    mumamo-chunk-mako-<%
    mumamo-chunk-mako-%
    mumamo-chunk-mako$

    mumamo-chunk-xml-pi
    mumamo-chunk-inlined-style
    mumamo-chunk-inlined-script
    mumamo-chunk-style=
    mumamo-chunk-onjs=
    )))

;; Fix-me: This caused mumamo to loop during fontification since
;; fmode-replace-default-mode was not defined. Mumamo tried to load
;; the function in mumamo-fetch-major-mode-setup in (funcall major)
;; where major mode is php-mode.

;;(eval-after-load 'php-mode '(fmode-replace-default-mode 'php-mode 'nxhtml-mumamo-mode))



(provide 'nxhtml-mumamo)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; nxhtml-mumamo.el ends here
