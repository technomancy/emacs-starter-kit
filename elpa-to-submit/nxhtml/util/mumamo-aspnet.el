;;; mumamo-aspnet.el --- Support for ASP .Net in `mumamo-mode'.
;;
;;;;; John: Please change here to what you want:
;; Author: John J Foerch (jjfoerch A earthlink O net)
;; Maintainer:
;; Created: ??
;; Version: ==
;; Last-Updated: Wed Dec 12 21:55:11 2007 (3600 +0100)
;; URL: http://OurComments.org/Emacs/Emacs.html
;; Keywords:
;; Compatibility:
;;
;; Features that might be required by this library:
;;
;;   None
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; Support for ASP .Net in `mumamo-mode'.  If you want to use VB then
;; you have to get the vb mode that this is written for here:
;;
;;   http://www.emacswiki.org/cgi-bin/wiki/VbDotNetMode
;;
;; A C# mode is already included in nXhtml. That is the one that this
;; library has been tested with.
;;
;;
;;; Usage:
;;
;; Put this file in you Emacs `load-path' and add in your .emacs:
;;
;;   (eval-after-load 'mumamo
;;     (require 'mumamo-aspnet)
;;     (mumamo-aspnet-add-me))
;;
;; A file with the extension .aspx will no be opened with nxhtml-mode
;; as the main major mode and with chunks in csharp-mode etc.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;

(defun mumamo-aspnet-add-me()
  "Make mumamo aware of the ASP.Net extension."
  (add-to-list 'mumamo-chunk-family-list
               '("ASP.Net nXhtml Family" nxhtml-mode
                 (mumamo-chunk-aspnet
                  mumamo-chunk-aspnet-script
                  mumamo-chunk-inlined-style
                  mumamo-chunk-inlined-script
                  mumamo-chunk-style=
                  mumamo-chunk-onjs=
                  ))
               t)
  (add-to-list 'mumamo-chunk-family-list
               '("ASP.Net XHTML Family" html-mode
                 (mumamo-chunk-aspnet
                  mumamo-chunk-aspnet-script
                  mumamo-chunk-inlined-style
                  mumamo-chunk-inlined-script
                  mumamo-chunk-style=
                  mumamo-chunk-onjs=
                  ))
               t)


  (add-to-list 'mumamo-filenames-list
               '("\\.aspx\\'" "ASP.Net nXhtml Family"))
  ;; Make it SET for current session in Custom.
  (customize-set-variable 'mumamo-filenames-list mumamo-filenames-list)
  (customize-set-value 'mumamo-filenames-list mumamo-filenames-list)

  ;; this is how to set up mode aliases, should we need them.
  (add-to-list 'mumamo-major-modes '(csharp-mode csharp-mode))
  (add-to-list 'mumamo-major-modes '(vbnet-mode vbnet-mode))
  ;; Make it SET for current session in Custom.
  (customize-set-variable 'mumamo-major-modes mumamo-major-modes)
  (customize-set-value 'mumamo-major-modes mumamo-major-modes)
  )


;;; aspnet

(defvar mumamo-aspnet-page-language-mode-spec nil
  "A mumamo mode-spec for the default language of an ASP.Net page.
This is what is set with the directive `@ Page Language' on the
page.

Internal variable.")
(make-variable-buffer-local 'mumamo-aspnet-page-language-mode-spec)
(add-to-list 'mumamo-survive 'mumamo-aspnet-page-language-mode-spec)

(defconst mumamo-aspnet-language-regex
  (rx (0+ (not (any ">")))
      word-start "language" (0+ space) "=" (0+ space) ?\" (submatch (0+ (not (any ?\" ?>)))) ?\"
      ))

(defun mumamo-aspnet-get-page-language-mode-spec ()
  (or mumamo-aspnet-page-language-mode-spec
      (save-excursion
        (goto-char (point-min))
        (when (search-forward "<%@ Page")
          (let ((case-fold-search t))
            (when (looking-at mumamo-aspnet-language-regex)
              (mumamo-aspnet-mode-spec-for-language (match-string 1))))))
      'fundamental-mode))

(defun mumamo-aspnet-get-mode-for-chunk (&optional chunk-type)
  (cond ((eq chunk-type 'script)
         (mumamo-mode-from-modespec
          (or (if (looking-at mumamo-aspnet-language-regex)
                  (mumamo-aspnet-mode-spec-for-language (match-string 1))
                  (mumamo-aspnet-get-page-language-mode-spec))
              'fundamental-mode)))
        ((eq chunk-type 'directive)
         'fundamental-mode)
        (t (mumamo-mode-from-modespec
            (mumamo-aspnet-get-page-language-mode-spec)))))


(defun mumamo-chunk-aspnet(pos min max)
  "Find <% ... %>."
  (mumamo-find-possible-chunk pos min max
                              'mumamo-search-bw-exc-start-aspnet
                              'mumamo-search-bw-exc-end-jsp
                              'mumamo-search-fw-exc-start-jsp
                              'mumamo-search-fw-exc-end-jsp))

(defun mumamo-search-bw-exc-start-aspnet(pos min)
  (let ((exc-start (mumamo-search-bw-exc-start-str pos min "<%")))
    (when (and exc-start
               (<= exc-start pos))
      (cons exc-start
            (mumamo-aspnet-get-mode-for-chunk
             (if (eq (char-after exc-start) ?@)
                 'directive))))))

(defconst mumamo-aspnet-script-tag-start-regex
  (rx "<script" word-end
      (0+ (not (any ">")))
      word-start "runat" (0+ space) "=" (0+ space) ?\" "server" ?\"
      (0+ (not (any ">")))
      ">"
      ))

(defun mumamo-aspnet-mode-spec-for-language (language)
  (let ((language (downcase language)))
    (cond ((equal language "c#") 'csharp-mode)
          ((equal language "vb") 'vbnet-mode)
          (t 'fundamental-mode))))

(defun mumamo-search-bw-exc-start-aspnet-script(pos min)
  (goto-char (+ pos 7))
  (let ((marker-start (search-backward "<script" min t))
        exc-mode
        exc-start)
    (when marker-start
      (when (looking-at mumamo-aspnet-script-tag-start-regex)
        (setq exc-start (match-end 0))
        (setq exc-mode (mumamo-aspnet-get-mode-for-chunk 'script))
        (goto-char exc-start)
        (when (<= exc-start pos)
          (cons (point) exc-mode))))))

(defun mumamo-search-fw-exc-start-aspnet-script(pos max)
  (goto-char (1+ pos))
  (skip-chars-backward "^<")
  ;; Handle <![CDATA[
  (when (and
         (eq ?< (char-before))
         (eq ?! (char-after))
         (not (bobp)))
    (backward-char)
    (skip-chars-backward "^<"))
  (unless (bobp)
    (backward-char 1))
  (let ((exc-start (search-forward "<script" max t))
        exc-mode)
    (when exc-start
      (goto-char (- exc-start 7))
      (when (looking-at mumamo-aspnet-script-tag-start-regex)
        (goto-char (match-end 0))
        (point)
        ))))

(defun mumamo-chunk-aspnet-script(pos min max)
  "Find inlined script, <script runat=\"server\">...</script>."
  (mumamo-find-possible-chunk pos min max
                              'mumamo-search-bw-exc-start-aspnet-script
                              'mumamo-search-bw-exc-end-inlined-script
                              'mumamo-search-fw-exc-start-aspnet-script
                              'mumamo-search-fw-exc-end-inlined-script))


(provide 'mumamo-aspnet)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; mumamo-aspnet.el ends here
