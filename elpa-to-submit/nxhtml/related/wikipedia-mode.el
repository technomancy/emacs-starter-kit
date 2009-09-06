;;; wikipedia-mode.el --- Mode for editing Wikipedia articles off-line
;; Copyright (C) 2003, 2004, 2006 Chong Yidong, Uwe Brauer

;; Author: Chong Yidong <cyd at stupidchicken com>
;; Maintainer: Uwe Brauer <oub at mat.ucm.es>
;; Version: 0.51
;; Keywords: wiki
;; $Id: wikipedia-mode.el,v 1.5 2006/05/30 15:16:45 oub Exp oub $


;; This file is not part of GNU Emacs.

;;{{{ GPL2

;; This file is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of
;; the License, or (at your option) any later version.

;; This file is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE.  See the GNU General Public License for more details.

;; You should have received a copy of the GNU General Public
;; License along with GNU Emacs; if not, write to the Free
;; Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
;; MA 02111-1307 USA

;;}}}

;;; Commentary:

;; This is `wikipedia-mode', a major mode for editing articles written
;; in the markup language used by Wikipedia, the free on-line
;; encyclopedia (http://www.wikipedia.org). It is intended to work
;; with GNU Emacs 21.x, and Xemacs 21.4.x. See below for details.

;; wikipedia mode can be found also at:
;; http://en.wikipedia.org/wiki/Wikipedia:Wikipedia-mode.el

;;{{{ INSTALLING WIKIPEDIA-MODE

;; Installing wikipedia-mode
;; =========================
;;
;; Save wikipedia-mode.el in a convenient directory, preferably in
;; your `load-path'. Add the following to your `user-init-file':
;;
;;   (autoload 'wikipedia-mode
;;     "wikipedia-mode.el"
;;     "Major mode for editing documents in Wikipedia markup." t)
;;
;; If you did not save wikipedia-mode.el in your `load-path', you must
;; use the full pathname. On MS Windows, use forward slashes (/)
;; rather than back slashes (\) to indicate the directory, e.g.:
;;
;;   (autoload 'wikipedia-mode
;;     "C:/Documents and Settings/USERNAME/.emacs.d/Wikipedia-mode.el"
;;     "Major mode for editing documents in Wikipedia markup." t)
;;
;; If you want to associate filenames ending in ".wiki" with
;; wikipedia-mode, add the following to your init file:
;;
;;   (setq auto-mode-alist
;;     (cons '("\\.wiki\\'" . wikipedia-mode) auto-mode-alist))

;;}}}

;;{{{ REQUIREMENTS

;; This  is not a real requirements but I highly recommend to use
;; outline-magic written by Carsten Dominik. If you don't want to use it
;; you have to comment out the relevant reference to outline magic.
;; It can be found at
;; http://www.astro.uva.nl/~dominik/Tools/outline-magic.el




;;}}}

;;{{{ RECOMMENDATIONS INSTALLING LONGLINES-MODE

;; Installing longlines-mode
;; =========================
;;
;; If you are using Emacs 22 or later longlines-mode is included so
;; please skip this section!
;;
;; Wikipedia articles don't use newline characters to break paragraphs
;; into lines, so each paragraph looks like a super-long line to
;; Emacs. To let Emacs handle "soft word wrapping", you need to
;; download a third-party package, longlines-mode.
;;
;; Download longlines.el, saving into your `load-path':
;;
;;   http://www.emacswiki.org/elisp/longlines.el
;;
;; Add the following to your `user-init-file':
;;
;;   (autoload 'longlines-mode "longlines.el"
;;     "Minor mode for editing long lines." t)
;;
;;
;; WARNING: if you insert text from one file in wikipedia-mode to
;; another file in wikipedia-mode I strongly recommend, to turn
;; longlines-mode off, before the copying!

;;}}}

;;{{{ RECOMMENDATIONS INSTALLING PABBREV-MODE

;; Installing pabbrev-mode
;; =========================
;;
;; You may find pabbrev.el useful, which can be found at
;; http://www.russet.org.uk/download/emacs/pabbrev.el


;;}}}

;;{{{ Xemacs or (GNU) Emacs

;; Xemacs or (GNU) Emacs
;; =====================
;; Usually that is a question of taste. However almost all wikipedia
;; articles nowadays use UTF8 coding, so the question which of the
;; Macsen to use, boils down to which degree UTF8 support is
;; implemented (no mule Xemacs is ruled out). While Xemacs has the
;; better font support, the UTF8 support still is not complete and
;; hence at the time being it is sad for the maintainer (a long time
;; Xemacs user) to recommend NOT to use Xemacs, even not 21.5.x, which
;; has a much better implemented UTF8 coding engine. That might
;; however change in the foreseeable future....
;; WARNING: at least for me in Debian testing/unstable Emacs does not
;; ship all fonts necessary for a flawless editing of UTF8  files. For
;; example you can chose Greek input, write Greek text, but then when
;; you close and open the file again, the Greek symbol are not
;; displayed but you see empty blocks. The reason seems that emacs
;; chooses for the input fonts other fonts as for the display (don't
;; ask me). However for installing the (ugly) UTF8 compatible fonts
;; from ..... solved that problem.


;;}}}

;;{{{ INSTALLING EE-HELPER or MOZEX

;; Installing the helper programs.
;; =========================
;; Helper Programs: MozEx and EE-HELPER. There are three possibilities
;; in order to use Emacs as an external editor
;;
;;     (1) Firefox add-on It's All Text: Recommended. The elisp
;;         library its-all-text.el makes it easier to use this.
;;
;;         PROS: Easy to intall, supported. (You need to add Emacs
;;               client as the editor.) Can be used for editing other
;;               text fields with Emacs too.
;;
;;     (2) EE-HELPER: This is perl script which will communicate with
;;         the  wikipedia server. However that sometimes be slow.

;;         PROS: if the editor supports UTF8, then ee-helper will
;;               pass the coding flawlessly.
;;
;;         CONTRA: the problem with this script is that it directly
;;                 communicates with the wikipedia site and does not
;;                 warn you about simultaneous editing. Use it with
;;                 care!!! Moreover section editing is not implemented.

;;     (3) MozEx: this is a Java-script which allows to communicate
;;         Mozilla (or Firefox) directly with Emacs.

;;         PROS: After finishing editing you use the wikipedia
;;               software to submit your changes and not the script,
;;               so you are warned about possible conflicting editing.
;;
;;         CONTRA: the official version does not support UTF8,
;;                 however there is now a new semi official version which
;;                 does support UTF8.

;; Installing It's All Text
;; ========================
;;
;; Go to the home page and follow the instructions there:
;;
;;   https://addons.mozilla.org/en-US/firefox/addon/4125
;;
;; Then open It's All Text preferences from the Firefox Add-ons page
;; and choose emacsclient as your editor (emacsclientw.exe on
;; Windows).

;; Installing ee-helper
;; ====================
;;
;; Download   the perl script  from
;;
;;   http://meta.wikimedia.org/wiki/Help:External_editors
;;
;; and follow the instructions. configure the .ee-ini file.  chance in
;; your personal wikipedia-mode-map account setting the editing
;; functions: activate the `external editor' option.

;; Installing MozEx
;; ================
;;
;; If your web browser is Mozilla or Firefox, take a look at the MozEx
;; extension, which allows you to call Emacs for editing text boxes:
;;
;;   http://mozex.mozdev.org/development.html
;;
;; See also
;;
;;   http://www.emacswiki.org/cgi-bin/wiki/FireFox
;;
;; If you mostly use MozEx to edit Wikipedia articles, it might be
;; worthwhile to tell Emacs to enter wikipedia-mode whenever it is
;; called by MozEx. Just add this to your `user-init-file':
;;
;;   (add-to-list 'auto-mode-alist '("mozex.\\.*" . wikipedia-mode))

;;     Recall: you have to click on edit (either edit article or edit
;;             section), then use mouse3 (or shift f10), then select
;;             mozex, then edit textarea: Edit-->mouse3-->mozex-->Edit
;;             Textarea. After editing, you have to _click_ on the
;;             text in the browser otherwise Mozilla will ignore your
;;             typing.

;;}}}

;;{{{ NEWS


;; NEWS
;; ==================================
;;     (1) Font setting has changed.
;;     (2) Some makeup formats have been added: italics, bold, strong
;;         emphasise, links.
;;     (3) outline-cycle from Carsten Dominiks outline-magic has been
;;         added.
;;     (4) "Draft", "send" and "reply" (for discussion pages)
;;         abilities 'based' on ideas of John Wigleys remember.el: see
;;         the functions wikipedia-draft-*
;;         RATIONALE: This comes handy in 2 situations
;;            1. You are editing articles which various authors (this I
;;               think is the usual case), you then want not to submit
;;               your edit immediately but want to copy it somewhere and
;;               to continue later. You can use the following functions
;;               for doing that:
;;               wikipedia-draft-buffer \C-c\C-b
;;               wikipedia-draft-region \C-c\C-r
;;               then the buffer/region will be appended to the
;;               wikipedia-draft-data-file (default is
;;               "~/Wiki/discussions/draft.wiki", which you can visit via
;;               wikipedia-draft-view-draft) and it will be
;;               surrounded by the ^L marks in order to set a page.
;;               moreover on top on that a section header == will be
;;               inserted, which consists of the Word Draft, a subject
;;               you are asked for and a date stamp.
;;
;;               Another possibility consists in using the function
;;               wikipedia-draft, bound to \C-c \C-m then a new buffer
;;               will opened already in wikipedia mode. You edit and then
;;               either can send the content of the buffer to the
;;               wikipedia-draft-data-file in the same manner as
;;               described above using the function
;;               wikipedia-draft-buffer (bound to \C-c\C-k)
;;
;;               BACK: In order to copy/send the content of temporary
;;               buffer or of a page in the wikipedia-draft-data-file
;;               back in to your wikipedia file, use the function
;;               `wikipedia-send-to-mozex'. You
;;               will be asked to which buffer to copy your text!
;;
;;
;;            2. You want to reply  in a discussion page to a specific
;;               contribution, you can use either the function
;;
;;               \\[wikipedia-reply-at-point-simple] bound to [(meta shift r)]
;;               which inserts a newline, a hline, and the signature of
;;               the author. Or can use
;;               \\[wikipedia-draft-reply] bound  [(meta r)]
;;               which does the same as wikipedia-reply-at-point-simple
;;               but in a temporary draft buffer.
;;
;;               BACK: In order to copy/send the content of that buffer
;;               back in to your wikipedia file, use the function
;;               \\[wikipedia-send-to-mozex] bound to "\C-c\C-c". You
;;               will be asked to which buffer to copy your text! If
;;               you want a copy to be send to your draft file, use
;;               the variable  wikipedia-draft-send-archive
;;

;;}}}

;;{{{ NEW FUNCTIONS AND VARIABLES


;; VERSION 0.4
;;==================
;; NEW FUNCTIONS
;; ------------------
;; wikipedia-insert-enumerate
;; wikipedia-insert-itemize
;; wikipedia-insert-strong-emphasis (renamed to wikipedia-insert-bold-italic)
;; wikipedia-insert-bold
;; wikipedia-insert-italics
;; wikipedia-insert-header
;; wikipedia-insert-link-wiki
;; wikipedia-turn-on-outline-minor-mode
;; wikipedia-insert-signature
;; wikipedia-insert-hline
;; wikipedia-unfill-paragraph-or-region
;; wikipedia-start-paragraph
;; wikipedia-hardlines
;; wikipedia-outline-magic-keys
;; wikipedia-enhance-indent
;; wikipedia-yank-prefix
;; wikipedia-simple-outline-promote
;; wikipedia-simple-outline-demote
;; wikipedia-next-long-line
;; wikipedia-unfill-paragraph
;; wikipedia-rename-buffer
;; wikipedia-draft
;; wikipedia-draft-buffer-desc
;; wikipedia-draft-append-to-file
;; wikipedia-draft-page
;; wikipedia-draft-region (&optional beg end)
;; wikipedia-draft-buffer
;; wikipedia-draft-clipboard
;; wikipedia-draft-mode
;; wikipedia-draft-view-draft
;; wikipedia-mark-section
;; wikipedia-activate-region
;; wikipedia-copy-page-to-register
;; wikipedia-insert-page-to-register
;; wikipedia-send-to-mozex (target-buffer)
;; wikipedia-reply-at-point-simple
;; wikipedia-draft-reply
;; wikipedia-insert-quotation-with-signature
;; wikipedia-insert-quotation

;; NEW VARIABLES
;;---------------------
;; wikipedia-enumerate-with-terminate-paragraph
;; wikipedia-draft-buffer "*Wikipedia-Draft*"
;; wikipedia-draft-mode-map
;; wikipedia-draft-mode-hook
;; wikipedia-draft-register ?R
;; wikipedia-draft-filter-functions
;; wikipedia-draft-handler-functions '(wikipedia-draft-append-to-file)
;; wikipedia-draft-data-file "~/Wiki/discussions/draft.wiki"
;; wikipedia-draft-leader-text "== "
;; wikipedia-draft-page ?S
;; wikipedia-draft-send-archive
;; wikipedia-reply-with-quote


;; VERSION 0.5
;;====================================
;; NEW FUNCTIONS
;; ------------------------------------
;;  wikipedia-insert-audio
;;  wikipedia-insert-image
;;  wikipedia-insert-link-www (renamed to wikipedia-insert-link-external)
;;  wikipedia-insert-user
;;  wikipedia-mark-signature
;;  wikipedia-outline-cycle
;;  wikipedia-reply-at-signature
;;  wikipedia-terminate-paragraph-and-indent
;;  wikipedia-yank-prefix

;; NEW VARIABLES (defvar, defcustom, defconst)
;; ----------------------
;; wikipedia-reply-with-hline
;; wikipedia-user-simplify-signature
;; wikipedia-english-or-german
;; wikipedia-draft-reply-register ?M
;; wikipedia-mode-version

;; VERSION 0.51
;;====================================
;;
;; - Now requires Emacs 22 or higher.
;; - Cleaned the code in various ways.
;; - Removed wikipedia-english-or-german
;; - Removed some private stuff
;; - Simplified some key bindings.
;; - Changed some key bindings to those used in org-mode.
;; - Added wikipedia-lang etc
;; - Added support for templates
;; - Adjusted header end after inserting heading/promoting/demoting
;; - Removed some functions that are already supported by outline.el
;; - Changed wikipedia-mode menus
;; - Added support for bullets and numbering

;;}}}

;;{{{ TODO

;; Todo
;; ----


;; * Implement TeX highlighting in <math> environment
;; * Implement (La)TeX input syntax, following the ideas of CDlatex.el
;; * Make outline-cycle work correctly
;; * wikipedia-reply-at-point-simple should use regexp!

;;}}}



;;; Code:

(require 'org)

(defconst wikipedia-mode-version (concat "0." (substring "$Revision: 1.5 $" 13 14))
  "$Id: wikipedia-mode.el,v 1.5 2006/05/30 15:16:45 oub Exp oub $

Report bugs to: Uwe Brauer oub at mat.ucm.es")

;;{{{ LANGS

;; (defvar wikipedia-english-or-german t
;; "*Variable in order to set the english (t) or german (nil) environment.")

(require 'tutorial) ;; for lang strings

(defvar wikipedia-lang "English")

(defvar wikipedia-langs-added nil)
;;(defconst xlang-strings nil)

(unless wikipedia-langs-added
  (defun add-lang-strings (lang new-strings)
    (let ((lang-rec (assoc lang lang-strings)))
      (if lang-rec
          (dolist (str new-strings)
            (nconc (cdr lang-rec) (list str)))
        (setq lang-rec (cons lang new-strings))
        (add-to-list 'lang-strings lang-rec))))

  (add-lang-strings "English"
                    '(
                      (wikip-username-prompt . "Name of user: ")
                      (wikip-image-mark . "[[Image:")
                      (wikip-media-mark . "[[Media:")
                      (wikip-utc . "(UTC)")
                      (wikip-user-mark . "[[User:")
                      ))
  (add-lang-strings "Deutsch"
                    '(
                      (wikip-username-prompt . "Name des Benutzers: ")
                      (wikip-image-mark . "[[Bild:")
                      (wikip-media-mark . "[[Bild:")
                      (wikip-utc . "(CET)")
                      (wikip-user-mark . "[[Benutzer:")
                      ))
  (setq wikipedia-langs-added t))

;;}}}

;;{{{ TAGS

(defvar wikipedia-simple-tags
  '("b" "big" "blockquote" "br" "caption" "code" "center" "cite" "del"
    "dfn" "dl" "em" "i" "ins" "kbd" "math" "nowiki" "ol" "pre" "samp"
    "small" "strike" "strong" "sub" "sup" "tt" "u" "ul" "var")
  "Tags that do not accept arguments.")

(defvar wikipedia-complex-tags
  '("a" "div" "font" "table" "td" "th" "tr")
  "Tags that accept arguments.")

(defvar wikipedia-url-protocols
  '("ftp" "gopher" "http" "https" "mailto" "news")
  "Valid protocols for URLs in Wikipedia articles.")

;;}}}

;;{{{ FACES

(defvar font-wikipedia-sedate-face                      'font-wikipedia-sedate-face
  "Face to use for Wikipedia minor keywords.")

(defvar font-wikipedia-italic-face                      'font-wikipedia-italic-face
  "Face to use for Wikipedia italics.")
(defvar font-wikipedia-bold-face                        'font-wikipedia-bold-face
  "Face to use for Wikipedia bolds.")
(defvar font-wikipedia-math-face                        'font-wikipedia-math-face
  "Face to use for Wikipedia math environments.")
(defvar font-wikipedia-string-face                  'font-wikipedia-string-face
  "Face to use for strings.  This is set by Font Wikipedia.")
(defvar font-wikipedia-verbatim-face                'font-wikipedia-verbatim-face
  "Face to use for text in verbatim macros or environments.")




(defface font-wikipedia-bold-face
  (let ((font (cond ((assq :inherit custom-face-attributes) '(:inherit bold))
                    ((assq :weight custom-face-attributes) '(:weight bold))
                    (t '(:bold t)))))
    `((((class grayscale) (background light))
       (:foreground "DimGray" ,@font))
      (((class grayscale) (background dark))
       (:foreground "LightGray" ,@font))
      (((class color) (background light))
       (:foreground "DarkOliveGreen" ,@font))
      (((class color) (background dark))
       (:foreground "OliveDrab" ,@font))
      (t (,@font))))
  "Face used to highlight text to be typeset in bold."
  :group 'font-wikipedia-highlighting-faces)

(defface font-wikipedia-italic-face
  (let ((font (cond ((assq :inherit custom-face-attributes) '(:inherit italic))
                    ((assq :slant custom-face-attributes) '(:slant italic))
                    (t '(:italic t)))))
    `((((class grayscale) (background light))
       (:foreground "DimGray" ,@font))
      (((class grayscale) (background dark))
       (:foreground "LightGray" ,@font))
      (((class color) (background light))
       (:foreground "DarkOliveGreen" ,@font))
      (((class color) (background dark))
       (:foreground "OliveDrab" ,@font))
      (t (,@font))))
  "Face used to highlight text to be typeset in italic."
  :group 'font-wikipedia-highlighting-faces)

(defface font-wikipedia-math-face
  (let ((font (cond ((assq :inherit custom-face-attributes)
                     '(:inherit underline))
                    (t '(:underline t)))))
    `((((class grayscale) (background light))
       (:foreground "DimGray" ,@font))
      (((class grayscale) (background dark))
       (:foreground "LightGray" ,@font))
      (((class color) (background light))
       (:foreground "SaddleBrown"))
      (((class color) (background dark))
       (:foreground "burlywood"))
      (t (,@font))))
  "Face used to highlight math."
  :group 'font-wikipedia-highlighting-faces)

(defface font-wikipedia-sedate-face
  '((((class grayscale) (background light)) (:foreground "DimGray"))
    (((class grayscale) (background dark))  (:foreground "LightGray"))
    (((class color) (background light)) (:foreground "DimGray"))
    (((class color) (background dark))  (:foreground "LightGray"))
;;;(t (:underline t))
    )
  "Face used to highlight sedate stuff."
  :group 'font-wikipedia-highlighting-faces)

(defface font-wikipedia-string-face
  (let ((font (cond ((assq :inherit custom-face-attributes) '(:inherit italic))
                    ((assq :slant custom-face-attributes) '(:slant italic))
                    (t '(:italic t)))))
    `((((type tty) (class color))
       (:foreground "green"))
      (((class grayscale) (background light))
       (:foreground "DimGray" ,@font))
      (((class grayscale) (background dark))
       (:foreground "LightGray" ,@font))
      (((class color) (background light))
       (:foreground "RosyBrown"))
      (((class color) (background dark))
       (:foreground "LightSalmon"))
      (t (,@font))))
  "Face used to highlight strings."
  :group 'font-wikipedia-highlighting-faces)

(defface font-wikipedia-warning-face
  (let ((font (cond ((assq :inherit custom-face-attributes) '(:inherit bold))
                    ((assq :weight custom-face-attributes) '(:weight bold))
                    (t '(:bold t)))))
    `((((class grayscale)(background light))
       (:foreground "DimGray" ,@font))
      (((class grayscale)(background dark))
       (:foreground "LightGray" ,@font))
      (((class color)(background light))
       (:foreground "red" ,@font))
      (((class color)(background dark))
       (:foreground "red" ,@font))
      (t (,@font))))
  "Face for important keywords."
  :group 'font-wikipedia-highlighting-faces)

(defface font-wikipedia-verbatim-face
  (let ((font (if (and (assq :inherit custom-face-attributes)
                       (if (featurep 'xemacs)
                           (find-face 'fixed-pitch)
                         (facep 'fixed-pitch)))
                  '(:inherit fixed-pitch)
                '(:family "courier"))))
    `((((class grayscale) (background light))
       (:foreground "DimGray" ,@font))
      (((class grayscale) (background dark))
       (:foreground "LightGray" ,@font))
      (((class color) (background light))
       (:foreground "SaddleBrown" ,@font))
      (((class color) (background dark))
       (:foreground "burlywood" ,@font))
      (t (,@font))))
  "Face used to highlight TeX verbatim environments."
  :group 'font-wikipedia-highlighting-faces)


(defvar wikipedia-font-lock-keywords
  (list

   ;; Apostrophe-style text markup
   (cons "''''\\([^']\\|[^']'\\)*?\\(''''\\|\n\n\\)"
         'font-lock-builtin-face)
   (cons "'''\\([^']\\|[^']'\\)*?\\('''\\|\n\n\\)"
                                        ;           'font-lock-builtin-face)
         'font-wikipedia-bold-face)
   (cons "''\\([^']\\|[^']'\\)*?\\(''\\|\n\n\\)"
         'font-wikipedia-italic-face)

   ;; Headers and dividers
   (list "^\\(==+\\)\\(.*\\)\\(\\1\\)"
         '(1 font-lock-builtin-face)
                                        ;                  '(2 wikipedia-header-face)
         '(2 font-wikipedia-sedate-face)
         '(3 font-lock-builtin-face))
   (cons "^-----*" 'font-lock-builtin-face)

   ;; Bare URLs and ISBNs
   (cons (concat "\\(^\\| \\)" (regexp-opt wikipedia-url-protocols t)
                 "://[-A-Za-z0-9._\/~%+&#?!=()@]+")
         'font-lock-variable-name-face)
   (cons "\\(^\\| \\)ISBN [-0-9A-Z]+" 'font-lock-variable-name-face)

   ;; Colon indentation, lists, definitions, and tables
   (cons "^\\(:+\\|[*#]+\\||[}-]?\\|{|\\)" 'font-lock-builtin-face)

   (list "^\\(;\\)\\([^:\n]*\\)\\(:?\\)"
         '(1 font-lock-builtin-face)
         '(2 font-lock-keyword-face)
         '(3 font-lock-builtin-face))



   ;; Tags and comments

   (list (concat "\\(</?\\)"
                 (regexp-opt wikipedia-simple-tags t) "\\(>\\)")
         '(1 font-lock-builtin-face t t)
         '(2 font-lock-function-name-face t t)
         '(3 font-lock-builtin-face t t))
   (list (concat "\\(</?\\)"
                 (regexp-opt wikipedia-complex-tags t)
                 "\\(\\(?: \\(?:[^\"'/><]\\|\"[^\"]*\"\\|'[^']*'\\)*\\)?\\)\\(>\\)")
         '(1 font-lock-builtin-face t t)
         '(2 font-lock-function-name-face t t)
         '(3 font-lock-keyword-face t t)
         '(4 font-lock-builtin-face t t))
   (cons (concat "<!-- \\([^->]\\|>\\|-\\([^-]\\|-[^>]\\)\\)*-->")
         '(0 font-lock-comment-face t t))



   ;; External Links

   (list (concat "\\(\\[\\)\\(\\(?:"
                 (regexp-opt wikipedia-url-protocols)
                 "\\)://[-A-Za-z0-9._\/~%-+&#?!=()@]+\\)\\(\\(?: [^]\n]*\\)?\\)\\(\\]\\)")
         '(1 font-lock-builtin-face t t)
         '(2 font-lock-variable-name-face t t)
         '(3 font-lock-keyword-face t t)
         '(4 font-lock-builtin-face t t))




   ;; Wiki links
   '("\\(\\[\\[\\)\\([^]\n|]*\\)\\(|?\\)\\([^]\n]*\\)\\(\\]\\]\\)"
     (1 font-lock-builtin-face t t)
     (2 font-lock-variable-name-face t t)
     (3 font-lock-builtin-face t t)
     (4 font-lock-keyword-face t t)
     (5 font-lock-builtin-face t t))

   ;; Wiki variables
   '("\\({{\\)\\(.+?\\)\\(}}\\)"
     (1 font-lock-builtin-face t t)
     (2 font-lock-variable-name-face t t)
     (3 font-lock-builtin-face t t))

   ;; Character entity references
   (cons "&#?[a-zA-Z0-9]+;" '(0 font-lock-type-face t t))

   ;; Preformatted text
   (cons "^ .*$" '(0 font-lock-constant-face t t))

   ;; Math environment (uniform highlight only, no TeX markup)
   (list "<math>\\(\\(\n?.\\)*\\)</math>"
         '(1 font-lock-keyword-face t t))))

                                        ; )

;;}}}

;;{{{ Menu and header stuff

(defvar wikipedia-imenu-generic-expression
  ;;(list '(nil "^==+ *\\(.*[^\n=]\\)==+" 1))
  (list '(nil "^=+ *\\(.*[^\n=]\\)=+" 1))
  "Imenu expression for `wikipedia-mode'.  See `imenu-generic-expression'.")

;; (defun wikipedia-next-header ()
;;   "Move point to the end of the next section header."
;;   (interactive)
;;   (let ((oldpoint (point)))
;;     (end-of-line)
;;     (if (re-search-forward "\\(^==+\\).*\\1" (point-max) t)
;;         (beginning-of-line)
;;       (goto-char oldpoint)
;;       (message "No section headers after point."))))

;; (defun wikipedia-prev-header ()
;;   "Move point to the start of the previous section header."
;;   (interactive)
;;   (unless (re-search-backward "\\(^==+\\).*\\1" (point-min) t)
;;     (message "No section headers before point.")))

;;}}}

;;{{{ Paragraph terminate and filling stuff (Chong)

(defun wikipedia-terminate-paragraph () ;Version:1.58
  "New list item or paragraph.
In a list, start a new list item. In a paragraph, start a new
paragraph.

If the current paragraph is colon indented, the new paragraph
will be indented in the same way."
  (interactive)
  (let (indent-chars)
    (save-excursion
      (beginning-of-line)
      (while (cond ((looking-at "^$") nil)
                   ((looking-at "^\\(\\(?: \\|:+\\|[#*]+\\) *\\)")
                    (setq indent-chars (match-string 1)) nil)
                   ((eq (point) (point-min)) nil)
                   ((progn (forward-line -1) t)))
        t))
    (newline) (if (not indent-chars) (newline)
                (insert indent-chars))))

(defun wikipedia-terminate-paragraph-and-indent ()
  "New list item or paragraph, ignore *,#.
In a list, start a new list item. In a paragraph, start a new
paragraph but *,# will be ignored.

If the current paragraph is colon ; indented, the new paragraph
will be indented in the same way."
  (interactive)
  (let (indent-chars)
    (save-excursion
      (beginning-of-line)
      (while (cond ((looking-at "^$") nil)
                   ((looking-at "^\\(\\(?: \\|:+\\) *\\)")
                    (setq indent-chars (match-string 1)) nil)
                   ((eq (point) (point-min)) nil)
                   ((progn (forward-line -1) t)))
        t))
    (newline) (if (not indent-chars) (newline)
                (insert indent-chars))))


(defun wikipedia-link-fill-nobreak-p ()
  "Function for `fill-nobreak-predicate'.
When filling, don't break the line for preformatted (fixed-width)
text or inside a Wiki link."
  (save-excursion
    (let ((pos (point)))
      (or (eq (char-after (line-beginning-position)) ? )
          (if (re-search-backward "\\[\\[" (line-beginning-position) t)
              ;; Break if the link is really really long.
              ;; You often get this with captioned images.
              (null (or (> (- pos (point)) fill-column)
                        (re-search-forward "\\]\\]" pos t))))))))

(defun wikipedia-fill-article ()
  "Fill the entire article."
  (interactive)
  (save-excursion
    (fill-region (point-min) (point-max))))

(defun wikipedia-unfill-article ()
  "Unfill article.
Undo filling, deleting stand-alone newlines (newlines that do not
end paragraphs, list entries, etc.)"
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward ".\\(\n\\)\\([^# *;:|!\n]\\|----\\)" nil t)
      (replace-match " " nil nil nil 1)))
  (message "Stand-alone newlines deleted"))


(defun wikipedia-unfill-paragraph-with-newline (&optional justifyp)
  (interactive "P")
  (let ((before (point)))               ;Version:1.3
    (save-excursion
      (forward-paragraph)
      (or (bolp) (newline 1))
      (let ((end (point))
            (start (progn (backward-paragraph) (point))))
        (goto-char before)
        (while (re-search-forward ".\\(\n\\)\\([^# *;:|!\n]\\|----\\)" nil t)
          (replace-match " " nil nil nil 1))))))
                                        ;  (message "Stand-alone newlines IN PARAGRAPH deleted"))

(defun wikipedia-unfill-region ()
  "Unfill region.
Undo filling, deleting stand-alone newlines (newlines that do not
end paragraphs, list entries, etc.) see also the function
\\[wikipedia-unfill-paragraph-or-region] and the even simpler
function \\[wikipedia-unfill-paragraph-simple]."
  (interactive)
  (save-excursion
    (narrow-to-region (point) (mark))
    (goto-char (point-min))
    (while (re-search-forward ".\\(\n\\)\\([^# *;:|!\n]\\|----\\)" nil t)
      (replace-match " " nil nil nil 1)))
  (message "Stand-alone newlines deleted")
  (widen))

;;}}}

;;{{{ Main function wikipedia mode (using define-derived mode)

;;{{{ KEY SETTING
;; (defvar wikipedia-outline-map
;;   (let ((map (make-sparse-keymap)))
;;     (define-key map [(down)] 'outline-next-visible-heading)
;;     (define-key map [(up)] 'outline-previous-visible-heading)
;;     (define-key map "n" 'outline-next-visible-heading)
;;     (define-key map "p" 'outline-previous-visible-heading)
;;     (define-key map "f" 'outline-forward-same-level)
;;     (define-key map "b" 'outline-backward-same-level)
;;     (define-key map "u" 'outline-up-heading)
;;     (define-key map "/" 'org-occur)
;;     (define-key map "\C-c\C-n" 'outline-next-visible-heading)
;;     (define-key map "\C-c\C-p" 'outline-previous-visible-heading)
;;     (define-key map "\C-c\C-f" 'outline-forward-same-level)
;;     (define-key map "\C-c\C-b" 'outline-backward-same-level)
;;     (define-key map "\C-c\C-u" 'outline-up-heading)
;;     map))


(defvar wikipedia-mode-map
  (let ((map (make-sparse-keymap)))
    ;;(define-key map "\M-n" 'wikipedia-next-header)
    (define-key map "\C-c\C-n" 'wikipedia-next-long-line)
    ;; (define-key map "\M-p" 'wikipedia-prev-header)
    ;;  (define-key map [(meta down)] 'wikipedia-next-header)
    ;;  (define-key map [(meta up)]   'wikipedia-prev-header)
    (define-key map "\C-j" 'wikipedia-terminate-paragraph)
    ;;(define-key map "RET" 'wikipedia-newline)
    (define-key map [(shift return)] 'wikipedia-newline)

      ;;(define-key mm [separator-format] '("--"))
;;       (define-key mm [outline]
;;         '("Toggle Outline Mode..." . outline-minor-mode))
      ;;(define-key mm [separator-edit-structure] '("--"))


    ;; Use some mnemonic
    ;;(define-key map "\C-c\C-q"    'wikipedia-unfill-article)
    (define-key map "\C-c\M-qua"    'wikipedia-unfill-article)
    ;;(define-key map "\C-c\M-q"    'wikipedia-fill-article)
    (define-key map "\C-c\M-qfa"    'wikipedia-fill-article)
    ;;(define-key map "\M-u" 'wikipedia-unfill-paragraph-or-region)
    (define-key map "\C-c\M-qur" 'wikipedia-unfill-paragraph-or-region)
    ;;(define-key map "\C-c\C-u" 'wikipedia-unfill-paragraph-simple)
    (define-key map "\C-c\M-qup" 'wikipedia-unfill-paragraph-simple)

    (define-key map "\C-c\C-fs" 'wikipedia-insert-bold-italic)
    (define-key map "\C-c\C-fb" 'wikipedia-insert-bold) ;Version:1.3
    (define-key map "\C-c\C-fi" 'wikipedia-insert-italics)
    (define-key map "\C-c\C-fn" 'wikipedia-insert-nowiki)

    (define-key map "\C-c\C-ts" 'wikipedia-insert-signature)
    (define-key map "\C-c\C-tt" 'wikipedia-insert-template)
    (define-key map "\C-c\C-tu" 'wikipedia-insert-user)
    ;;(define-key map "\C-c\C-fq" 'wikipedia-insert-quotation)
    ;;(define-key map "\C-c\C-fh" 'wikipedia-insert-header)
    (define-key map "\C-c\C-fr" 'wikipedia-insert-hline) ;Version:1.30
    (define-key map "\C-c\C-li" 'wikipedia-insert-link-wiki)
    (define-key map "\C-c\C-le" 'wikipedia-insert-link-external)

    ;; Breaks key binding conventions:
    ;;(define-key map [(meta f7)] 'wikipedia-draft)
    ;;(define-key map [(meta f8)] 'wikipedia-reply-at-point-simple)
    ;;(define-key map [(meta f9)]  'wikipedia-draft-view-draft)

    (define-key map "\C-c\C-r"  'wikipedia-reply-at-point-simple)

    ;; Breaks key binding conventions:
    ;;(define-key map "\C-cr" 'wikipedia-draft-region)

    (define-key map [(meta r)]  'wikipedia-draft-reply)
    (define-key map "\C-c\C-m" 'wikipedia-draft) ;Version:1.25
    (define-key map "\C-c\C-b" 'wikipedia-draft-region)
    (define-key map "\C-c\C-d" 'wikipedia-draft-buffer)
    (define-key map "\C-c\C-k" 'wikipedia-draft-buffer)
    (define-key map "\C-c\C-p" 'wikipedia-draft-copy-page-to-register) ;Version:1.39
    ;; (define-key map "\C-c\C-c" 'wikipedia-draft-send-to-mozex)
    (define-key map "\C-c\C-s" 'wikipedia-draft-yank-page-to-register)

    (define-key map [(control meta prior)] 'wikipedia-enhance-indent)
    (define-key map [(control meta next)] 'wikipedia-yank-prefix)
    (define-key map [(meta return)] 'wikipedia-insert-enumerate)
    (define-key map [(meta control return)] 'wikipedia-insert-enumerate-nonewline)
    ;; private setting
    ;; This is bound to C-j by default:
    ;;(define-key map [(shift return)] 'newline-and-indent) ;Version:1.24
    (define-key map "\C-\\" 'wikipedia-insert-itemize) ;Version:1.28
    (define-key map [(control return)] 'wikipedia-insert-itemize)

    ;; The next three breaks Emacs key binding conventions, are they really necessary?
    ;;(define-key map "\C-ca" 'auto-capitalize-mode)
    ;;(define-key map "\C-ci" 'set-input-method)
    ;;(define-key map "\C-ct" 'toggle-input-method) ;Version:1.23

    (define-key map [(shift tab)] 'org-shifttab)
    (define-key map [backtab]     'org-shifttab)
    (define-key map [tab]         'org-cycle)

    map))

(defvar wikipedia-org-menu nil)
;; From org.el:
(easy-menu-define wikipedia-org-menu wikipedia-mode-map "Wikipedia menu"
  '("Wikipedia"
    ("Show/Hide"
     ["Cycle Visibility" org-cycle (or (bobp) (outline-on-heading-p))]
     ["Cycle Global Visibility" org-shifttab]
     ["Sparse Tree" org-occur t]
     ["Reveal Context" org-reveal t]
     ["Show All" show-all t]
     "--"
     ["Subtree to indirect buffer" org-tree-to-indirect-buffer t])
    "--"
    ["New Heading" outline-insert-heading t]
    ("Navigate Headings"
     ["Up" outline-up-heading t]
     ["Next" outline-next-visible-heading t]
     ["Previous" outline-previous-visible-heading t]
     ["Next Same Level" outline-forward-same-level t]
     ["Previous Same Level" outline-backward-same-level t]
     "--"
     ["Jump" org-goto t])
    ("Edit Structure"
     ["Move Subtree Up" outline-move-subtree-up]
     ["Move Subtree Down" outline-move-subtree-down]
     "--"
     ["Copy Subtree"  org-copy-special]
     ["Cut Subtree"  org-cut-special]
     ["Paste Subtree"  org-paste-special]
     "--"
     ["Promote Heading" wikipedia-simple-outline-promote]
     ["Promote Subtree" outline-promote]
     ["Demote Heading" wikipedia-simple-outline-demote]
     ["Demote Subtree"  outline-demote])
    ("Filling"
     ["Unfill article" wikipedia-unfill-article]
     ["Fill article" wikipedia-fill-article])
    "--"
    ("Format"
     ["Horizontal line" wikipedia-insert-hline]
     "--"
     ["Indent Paragraph" wikipedia-indent-paragraph]
     ["Deindent Paragraph" wikipedia-deindent-paragraph]
     "--"
     ["Insert No Wiki Formatting" wikipedia-insert-nowiki]
     ["Bold-Italic" wikipedia-insert-bold-italic]
     ["Italic" wikipedia-insert-italics]
     ["Bold" wikipedia-insert-bold]
     "--"
     ("Insert"
      ("Templates"
       ["Site Specific Template" wikipedia-insert-template]
       ["Signature" wikipedia-insert-signature]
       )
      ("Links"
       ["External Link" wikipedia-insert-link-external]
       ["Internal Wiki Link" wikipedia-insert-link-wiki]
       )
      ("Bullets and numbering"
       ["Bullet" wikipedia-insert-bullet]
       ["Numbering" wikipedia-insert-numbering]
       )
      ))
    ))
;;}}}


;;;###autoload
;;{{{ Main function wikipedia-mode

(define-derived-mode wikipedia-mode outline-mode "Wikipedia"
  "Major mode for editing wikimedia style wikis.
Major mode for editing articles written in the markup language
used by Wikipedia, the free on-line
encyclopedia (see URL `http://www.wikipedia.org').

There are several ways to use wikipedia-mode:

- You can simply cut and paste articles between Emacs and your
  web browser's text box.
- If you are using Firefox you can use the It's All Text add-on
  for Firefox.
- You can use MozEx, a Mozilla/Firefox web browser extension that
  allows you to call Emacs from a text
  box (see URL `http://mozex.mozdev.org/').
- Another way is to use the PERL script ee-helper, which allows
  you to up and download wiki texts.

Wikipedia articles are usually unfilled: newline characters are not
used for breaking paragraphs into lines. Unfortunately, Emacs does not
handle word wrapping yet. As a workaround, wikipedia-mode turns on
longlines-mode automatically. In case something goes wrong, the
following commands may come in handy:

\\[wikipedia-fill-article] fills the buffer.
\\[wikipedia-unfill-article] unfills the buffer.
Be warned that function can be dead  slow, better use wikipedia-unfill-paragraph-or-region.
\\[wikipedia-unfill-paragraph-or-region] unfills the paragraph
\\[wikipedia-unfill-paragraph-simple] doehe same but simpler.



The following commands put in markup structures.

\\[wikipedia-insert-bold-italic] bold+italic
\\[wikipedia-insert-bold] bold text
\\[wikipedia-insert-italics] italics
\\[wikipedia-insert-nowiki] no wiki markup
\\[wikipedia-insert-link-wiki] inserts a link

The following commands are also defined:
\\[wikipedia-insert-user] inserts user name
\\[wikipedia-insert-signature] inserts ~~~~
\\[wikipedia-insert-enumerate] inserts enumerate type structures
\\[wikipedia-insert-itemize] inserts itemize type structures
\\[wikipedia-insert-hline] inserts a hline

The draft functionality
\\[wikipedia-draft]
\\[wikipedia-draft-region]
\\[wikipedia-draft-view-draft]
\\[wikipedia-draft-page]
\\[wikipedia-draft-buffer]

Replying and sending functionality
\\[wikipedia-reply-at-point-simple]
\\[wikipedia-draft-reply]


The register functionality
\\[wikipedia-copy-page-to-register]
\\[defun wikipedia-insert-page-to-register]


Some simple editing commands.
\\[wikipedia-enhance-indent]
\\[wikipedia-yank-prefix]
\\[wikipedia-unfill-paragraph-or-region]



\\[wikipedia-terminate-paragraph]     starts a new list item or paragraph in a context-aware manner."

  (set (make-local-variable 'adaptive-fill-regexp) "[ ]*")
  (set (make-local-variable 'comment-start-skip) "\\(?:<!\\)?-- *")
  (set (make-local-variable 'comment-end-skip) " *--\\([ \n]*>\\)?")
  (set (make-local-variable 'comment-start) "<!-- ")
  (set (make-local-variable 'comment-end) " -->")
  (set (make-local-variable 'paragraph-start)
       "\\*\\| \\|#\\|;\\|:\\||\\|!\\|$")
  (set (make-local-variable 'sentence-end-double-space) nil)
  (set (make-local-variable 'font-lock-multiline) t)
  (set (make-local-variable 'font-lock-defaults)
       '(wikipedia-font-lock-keywords t nil nil nil))
  (set (make-local-variable 'fill-nobreak-predicate)
       'wikipedia-link-fill-nobreak-p)
  (set (make-local-variable 'auto-fill-inhibit-regexp) "^[ *#:|;]")

  ;; Support for outline-minor-mode. No key conflicts, so we'll use
  ;; the normal outline-mode prefix.
  ;;(set (make-local-variable 'outline-regexp) "==+")
  (set (make-local-variable 'outline-regexp) "=+")
  ;;(set (make-local-variable 'outline-heading-end-regexp) "=+") ;; For betting fixing
  ;;  (set (make-local-variable 'outline-regexp) "=+")
  ;;  (set (make-local-variable 'outline-regexp) ":")

  ;; Fix-me: Why change this?? Should not the user do this globally instead?
  ;;(set (make-local-variable 'outline-minor-mode-prefix) "\C-c\C-o")

  ;; Fix-mde For longlines-mode??:
  (set (make-local-variable 'auto-fill-inhibit-regexp) "^[ *#:|;]")

  ;: From org.el:
  ;; Get rid of Outline menus, they are not needed
  ;; Need to do this here because define-derived-mode sets up
  ;; the keymap so late.  Still, it is a waste to call this each time
  ;; we switch another buffer into org-mode.
  (if (featurep 'xemacs)
      (when (boundp 'outline-mode-menu-heading)
        ;; Assume this is Greg's port, it used easymenu
        (easy-menu-remove outline-mode-menu-heading)
        (easy-menu-remove outline-mode-menu-show)
        (easy-menu-remove outline-mode-menu-hide))
    (define-key wikipedia-mode-map [menu-bar headings] 'undefined)
    (define-key wikipedia-mode-map [menu-bar hide] 'undefined)
    (define-key wikipedia-mode-map [menu-bar show] 'undefined))

  (easy-menu-add wikipedia-org-menu)

  ;; Turn on the Imenu automatically.
;;   (when menu-bar-mode
;;     (set (make-local-variable 'imenu-generic-expression)
;;          wikipedia-imenu-generic-expression)
;;     (imenu-add-to-menubar "TOC"))

  (modify-syntax-entry ?< "(>" wikipedia-mode-syntax-table)
  (modify-syntax-entry ?> ")<" wikipedia-mode-syntax-table)

  ;;}}}

  ;;; This is the wrong way to do it, see add-hook:
  ;; (make-local-variable 'change-major-mode-hook)

  ;; Check if our version of outline.el has the new header hooks:
  (require 'outline)
  (when (boundp 'outline-demote-hook)
    (add-hook 'outline-demote-hook 'wikipedia-outline-insert-heading-f nil t)
    (add-hook 'outline-promote-hook 'wikipedia-outline-insert-heading-f nil t)
    (add-hook 'outline-insert-heading-hook 'wikipedia-outline-insert-heading-f nil t)
    )
  )

(defun wikipedia-outline-insert-heading-f ()
  (insert "  ")
  (backward-char)
  (wikipedia-adjust-header-end))

;; wikipedia-mode ends here
;;}}}

;;{{{ longlines-mode

(defun wikipedia-turn-on-longlines ()   ;Version:1.58
  "Turn on longlines-mode if it is defined."
  (if (functionp 'longlines-mode)
      (longlines-mode 1)))

(defcustom wikipedia-use-longlines-mode nil
  "Turn on longlines-mode' if non-nil.
Unfortunately there are some bugs in `longlines-mode' so turning
it on is an option currently."
  :type 'boolean
  :set (lambda (sym val)
         (set-default sym val)
         (if val
             (add-hook 'wikipedia-mode-hook 'wikipedia-turn-on-longlines)
           (remove-hook 'wikipedia-mode-hook 'wikipedia-turn-on-longlines)))
  :group 'wikipedia)

;;}}}

;; New formating stuff for inserting simple formating structures such

;;{{{ Insert makeup and templates

(defvar wikipedia-enumerate-with-terminate-paragraph nil
  "*Before insert enumerate/itemize do \\[wikipedia-terminate-paragraph].")

(defun wikipedia-region-active-p ()
  (or (and (boundp 'zmacs-region-active-p) zmacs-region-active-p)
      (and (boundp 'transient-mark-mode) transient-mark-mode mark-active)))

(defun wikipedia-insert-around-region (before after)
  (if (wikipedia-region-active-p)
      (save-excursion
        (let ((beginning (region-beginning))
              (end       (region-end))
              (check     (= (string-to-char before) ?')))
          ;; When we are inserting ' do not mix them:
          (if (or (not check)
                  (not (memq ?'
                             (append (buffer-substring-no-properties
                                      beginning end) nil))))
              (progn
                (goto-char end)
                (insert after)
                (goto-char beginning)
                (insert before))
            (message "Sorry, the region already contains the char '.")
            )))
    (insert before)
    (insert after)
    (backward-char (length after))))

(defun wikipedia-insert-enumerate ()
  "Insert enumerated items.
Format depends on `wikipedia-enumerate-with-terminate-paragraph'.
Note however that `wikipedia-terminate-paragraph' does not work
very well will longlines-mode."
  (interactive)
  (if wikipedia-enumerate-with-terminate-paragraph
      (progn
        (wikipedia-terminate-paragraph)
        (insert "#"))
    (newline nil)
    (insert ":#")))





(defun wikipedia-insert-itemize ()
  "Insert not enumerated items.
Format depends on `wikipedia-enumerate-with-terminate-paragraph'.
Note however that the `wikipedia-terminate-paragraph' does not
work very well will longlines-mode."
  (interactive)
  (if wikipedia-enumerate-with-terminate-paragraph
      (progn
        (wikipedia-terminate-paragraph)
        (insert "*"))
    (newline nil)
    (insert ":*")))


(defun wikipedia-insert-bold-italic ()
  "Insert strong emphasis.
Uses four apostrophes (e.g. ''''FOO''''.) When mark is active, surrounds region."
  (interactive)
  (wikipedia-insert-around-region "''''" "''''"))


(defun wikipedia-insert-bold ()
  "Insert bold.
Uses three apostrophes (e.g. '''FOO'''.) When mark is active,
surrounds region."
  (interactive)
  (wikipedia-insert-around-region "'''" "'''"))


(defun wikipedia-insert-italics ()
  "Insert italics.
Uses two apostrophes (e.g. ''FOO''.) When mark is active,
surrounds region."
  (interactive)
  (wikipedia-insert-around-region "''" "''"))

(defun wikipedia-indent-paragraph ()
  (interactive)
  (backward-paragraph)
  )
(defun wikipedia-deindent-paragraph ()
  (interactive)
  )
;;}}}
;;{{{ Templates

;; http://en.wikipedia.org/wiki/Template:Quotation
;; http://en.wikipedia.org/wiki/Help:A_quick_guide_to_templates
(defvar wikipedia-site nil)
(make-variable-buffer-local 'wikipedia-site)
(defvar wikipedia-site-history nil)

(defcustom wikipedia-templates nil
  "Templates for different wikis."
  :type '(repeat (list
                  (string :tag "Wiki site")
                  (repeat
                   (list
                    (string :tag "Template name")
                    (string :tag "Template code")))))
  :group 'wikipedia)

(defun wikipedia-insert-template ()
  "Prompts for a template and inserts it."
  (interactive)
  (let* ((t-name-code (wikipedia-get-template))
         (t-name (car t-name-code))
         ;; Ask how to insert:
         (choices '("Evaluate when page is created"
                    "Substitute when saving this source page"
                    "Show template when page is fetched"
                    "Insert template itself"))
         (hist (copy-sequence choices))
         (default (car choices))
         (choice (completing-read
                  "How do you want to insert the template? "
                  choices
                  nil
                  t
                  default
                  (cons 'hist 1))))
;;     (lwarn 't :warning "t-name=%s" t-name)
;;     (lwarn 't :warning "choice=%s" choice)
;;     (lwarn 't :warning "0=%s" (nth 0 choices))
;;     (lwarn 't :warning "1=%s" (nth 1 choices))
;;     (lwarn 't :warning "2=%s" (nth 2 choices))
    (cond
     ((string= choice (nth 0 choices))
      ;;(lwarn 't :warning "evaluate=>%s" (concat "{{" t-name "}}"))
      (insert "{{" t-name "}}")
      )
     ((string= choice (nth 1 choices))
      ;;(lwarn 't :warning "subst=>%s" (concat "{{subst:" t-name "}}"))
      (insert "{{subst:" t-name "}}")
      )
     ((string= choice (nth 2 choices))
      ;;(lwarn 't :warning "raw=>%s" (concat "{{msgnw:" t-name "}}"))
      (insert "{{msgnw:" t-name "}}")
      )
     ((string= choice (nth 3 choices))
      (insert (cdr t-name-code))
      ))))

(defun wikipedia-get-template ()
  (let* ((sites (mapcar (lambda (t-sites)
                         (car t-sites))
                        wikipedia-templates))
         (hist (copy-sequence sites))
         (default-site wikipedia-site)
         (histpos (if (not default-site)
                       1
                    (catch 'pos
                      (let ((n 0))
                        (dolist (elt sites)
                          (setq n (1+ n))
                          (when (string= default-site elt)
                            (throw 'pos n)))))))
         (site (if (= 1 (length sites))
                   (car sites)
                 (completing-read "Wiki site: "
                                  sites
                                  nil
                                  t
                                  default-site
                                  (cons 'hist histpos))))
         (s-t (assoc site wikipedia-templates))
         (templates (car (cdr s-t)))
         (t-names (mapcar (lambda (t-for-site)
                            ;;(lwarn 't :warning "t-for-site=%s" t-for-site)
                            (car t-for-site))
                          templates))
         (t-name (wikipedia-get-template-name site templates))
         (code (car (cdr (assoc t-name templates))))
         )
    (setq wikipedia-site site)
;;     (lwarn 't :warning "site=%s" site)
;;     (lwarn 't :warning "s-t=%s" s-t)
;;     (lwarn 't :warning "templates=%s" templates)
;;     (lwarn 't :warning "t-names=%s" t-names)
;;     (lwarn 't :warning "t-name=%s" t-name)
;;     (lwarn 't :warning "code=%s" code)
    (cons t-name code)))

(defun wikipedia-get-template-name (site templates)
  ""
  (let* ((prompt "Template: ")
         (minibuffer-local-must-match-map (copy-keymap minibuffer-local-must-match-map))
         (hist (mapcar (lambda (elt)
                         (car elt))
                       templates))
         (desc-fun (lambda ()
                     (let ((s (minibuffer-contents-no-properties)))
                       (when (< 0 (length s))
                         (wikipedia-describe-template s site templates)))))
         (up-fun (lambda () (interactive)
                   (previous-history-element 1)
                   (funcall desc-fun)))
         (down-fun (lambda () (interactive)
                     (next-history-element 1)
                     (funcall desc-fun)))
         (default nil)
         (histpos (if (not default) 1))
         (default-name (if default
                           default
                         (car (nth (1- histpos) templates))))
         (tpl-name nil)
         )
    (define-key minibuffer-local-must-match-map [up] up-fun)
    (define-key minibuffer-local-must-match-map [down] down-fun)
    (save-window-excursion
      (wikipedia-describe-template default-name site templates)
      (setq tpl-name (completing-read prompt
                                      templates
                                      nil ; predicate
                                      t   ; require-match
                                      default-name ;; initial-input
                                      (cons 'hist histpos) ;; hist
                                      )))
  (when (= 0 (length tpl-name))
    (error "No template name given"))
  (let ((tpl (assoc tpl-name templates)))
    (unless tpl
      (error "There is no template named %s for site %s" tpl-name site))
    ;;(lwarn 't :warning "tpl=%s" tpl)
    )
  tpl-name))

(defun wikipedia-describe-template (name site templates)
  (let ((tpl-rec (assoc name templates)))
    (with-output-to-temp-buffer (help-buffer)
      (with-current-buffer (help-buffer)
        (help-setup-xref (list #'wikipedia-describe-template name site templates) (interactive-p))
        (let ((inhibit-read-only t)
              start end
              here
              (tpl (assoc name templates)))
          ;;(insert (format "%S\n\n" family))
          (insert (format "%s - a wiki template for site %s" name site))
          (insert "\n\n")
          (setq start (point))
          (insert (cadr tpl))
          (setq end (point))
          (print-help-return-message)
          ;;(put-text-property start end 'face 'highlight)
          (goto-char start)
          (setq here (point))
          (while (re-search-forward "\\(?:<onlyinclude>\\|</onlyinclude>\\|<noinclude>.*</noinclude>\\)" end t)
            (put-text-property here (match-beginning 0) 'face 'highlight)
            (setq here (point))
            )
          (put-text-property (point) end 'face 'highlight)
          )))))

(defun wikipedia-insert-nowiki ()
  "Mark the region as 'nowiki'.
When mark is active, surrounds region."
  (interactive)
  (wikipedia-insert-around-region "<nowiki>" "</nowiki>"))



(defun wikipedia-insert-user ()
  "Prompt for a user name, insert [[User:foo]]"
  (interactive)
  (let ((user (read-string (get-lang-string wikipedia-lang 'wikip-username-prompt)))
        (user-mark (get-lang-string wikipedia-lang 'wikip-user-mark)))
    (insert (concat user-mark user "|" user "]]"))))

(defun wikipedia-insert-signature ()    ;Version:1.4
  "Insert \"~~~~\".
This will be shown as your user identity when showing the page."
  (interactive)
  (insert "~~~~"))



(defun wikipedia-insert-reply-prefix () ;Version:1.60
  "Quotation box of the form {{Quotation}}{{}}. When mark is active,
surrounds region."
  (interactive)
  (beginning-of-line 1)
  (search-forward "[[")
  (backward-char 2)
  (mark-sexp 1)
  (copy-to-register wikipedia-draft-reply-register (region-beginning) (region-end) nil)
  (end-of-line 1)
  (wikipedia-terminate-paragraph)
  (beginning-of-line 1)
  (kill-line nil)
  (insert "----")
  (newline 1)
  (yank)
  (insert ":'''Re: ")
  (insert-register wikipedia-draft-reply-register 1)
  (insert "''' ")
  (end-of-line 1))

;; (defun wikipedia-insert-header ()
;;   "Insert subheader via  == (e.g. == FOO ==.)"
;;   (interactive)
;;   (unless (bolp)
;;     (beginning-of-line))
;;   (insert "== ")
;;   (end-of-line)
;;   (insert " ==")
;;   (backward-char 3))

(defvar wikipedia-link-wiki-history nil)

(defun wikipedia-insert-link-wiki ()
  "Insert link via [[ (e.g. [[FOO]].) When mark is active, surround region."
  (interactive)
  (if (wikipedia-region-active-p)
      (wikipedia-insert-around-region "[[" "]]")
    (let* ((link (read-string "Wiki link: " nil wikipedia-link-wiki-history))
           (name (read-string "Name (optional): ")))
      (insert "[[" link)
      (when (< 0 (length name))
        (insert "|" name))
      (insert "]]"))))

(defun wikipedia-insert-link-external ()
  "Insert link via [[ (e.g. [http://FOO].) When mark is active, surround region."
  (interactive)
  (if (wikipedia-region-active-p)
      (wikipedia-insert-around-region "[" "]")
    (let* ((choices '("Plain" "Footnote" "Named"))
           (hist (copy-sequence choices))
           (style (completing-read
                   "Link style: " ; prompt
                   choices ; collection
                   nil ; predicate
                   t ; requite-match
                   "Plain" ; initial-input
                   (cons 'hist 1) ; hist
                   ))
           (url (read-string "URL: "))
           name)
      ;;(lwarn 't :warning "style=%s" style)
      (cond
       ((string= style "Plain")
        (insert url))
       ((string= style "Footnote")
        (insert "[" url "]"))
       ((string= style "Named")
        (let ((name (read-string "Link name: ")))
          (insert "[" url)
          (when (< 0 (length name))
            (insert " " name))
          (insert "]")))
       (t
        (error "Internal error, bad style=%s" style))))))


(defun wikipedia-insert-image ()
  "Insert link image, e.g. [[Image:FOO]].
When mark is active, surround region."
  (interactive)
  (let ((img-mark (get-lang-string wikipedia-lang 'wikip-image-mark)))
    (wikipedia-insert-around-region img-mark "]]")))

(defun wikipedia-insert-audio ()
  "Insert audio link, e.g. [[Media:FOO]].
When mark is active, surround region."
  (interactive)
  (let ((aud-mark (get-lang-string wikipedia-lang 'wikip-audio-mark)))
    (wikipedia-insert-around-region aud-mark "]]")))




;; (defun wikipedia-turn-on-outline-minor-mode ()
;;   "Turn on outline minor mode."
;;   ;;(interactive)
;;   (outline-minor-mode nil))






(defun wikipedia-insert-hline ()        ;Version:1.29
  "Insert \"----\"  "
  (interactive)
  (end-of-line)
  (insert hard-newline "----" hard-newline))

(defun wikipedia-newline (&optional arg)
  "Insert newline and check for bullets and numbering."
  (interactive "*P")
  (let ((here (point))
        (line-type nil))
    (beginning-of-line)
    (when (eq ?* (char-after))
      (setq line-type 'bullet))
    (when (eq ?# (char-after))
      (setq line-type 'numbered))
    (if (and line-type
             (looking-at ".\\s-*$"))
        (progn
          (delete-region (match-beginning 0) (match-end 0))
          (newline arg))
      (goto-char here)
      (newline arg)
      (cond
       ((eq line-type 'bullet)
        (insert "* "))
       ((eq line-type 'numbered)
        (insert "# "))))))

;;}}}

;;{{{ bullets and numbering

;; Fix-me: Seems like this and my newline stuff already was there ... ;-)
(defun wikipedia-insert-bullet ()
  "Insert a bullet."
  (interactive)
  (end-of-line)
  (newline)
  (insert "* "))

(defun wikipedia-insert-numbering ()
  "Insert numbering."
  (interactive)
  (end-of-line)
  (newline)
  (insert "# "))

;;}}}

;;{{{ filling and longline

(defun wikipedia-unfill-paragraph-or-region () ;Version:1.7
  "Unfill region.
This function does NOT explicitly search for \"soft newlines\" as
does wikipedia-unfill-region."
  (interactive)
  (when use-hard-newlines
    ;;  (backward-paragraph 1)
    ;;  (next-line 1)
    (beginning-of-line 1)
    (set-fill-prefix)
    ;; Fix-me: The use of make-local-variable here looks incorrect,
    ;; use lexical binding instead:
;;     (set (make-local-variable 'use-hard-newlines) nil)
;;     (set (make-local-variable 'sentence-end-double-space) t)
;;     (set (make-local-variable 'paragraph-start) "[ 　        \n]")
    (let ((use-hard-newlines nil)
          (sentence-end-double-space t)
          (paragraph-start nil))
      (when  (featurep 'xemacs)
        (let ((fill-column (point-max)))
          (fill-paragraph-or-region nil)))
      (unless  (featurep 'xemacs)
        (let ((fill-column (point-max)))
          (fill-paragraph nil)))))
;;     (set (make-local-variable 'use-hard-newlines) t)
;;     (set (make-local-variable 'sentence-end-double-space) nil)
;;     (set (make-local-variable 'paragraph-start) "\\*\\| \\|#\\|;\\|:\\||\\|!\\|$"))
  (unless use-hard-newlines
    ;;  (backward-paragraph 1)
    ;;  (next-line 1)
    (beginning-of-line 1)
    (set-fill-prefix)
;;     (set (make-local-variable 'sentence-end-double-space) t)
;;     (set (make-local-variable 'paragraph-start)
    (let ((sentence-end-double-space t)
          (paragraph-start nil))
      (when  (featurep 'xemacs)
        (let ((fill-column (point-max)))
          (fill-paragraph-or-region nil)))
      (unless  (featurep 'xemacs)
        (let ((fill-column (point-max)))
          (fill-paragraph nil))))
;;     (set (make-local-variable 'sentence-end-double-space) nil)
;;     (set (make-local-variable 'paragraph-start) "\\*\\| \\|#\\|;\\|:\\||\\|!\\|$")
    ))




;; (defun wikipedia-start-paragraph ()
;;   (interactive)
;;   (set (make-local-variable 'paragraph-start)
;;        "\\*\\| \\|#\\|;\\|:\\||\\|!\\|$"))

;; Use function use-hard-newlines instead:
;; (defun wikipedia-hardlines ()
;;   "Set use-hard-newlines to NIL."
;;   (interactive)
;;   (setq use-hard-newlines nil))

;; from emacs wiki
(defun wikipedia-next-long-line ()
  "Move forward to the next long line with column-width greater
  than `fill-column'.

  TODO: When function reaches end of buffer, save-excursion to
  starting point.
  Generalise to make `previous-long-line'."
  (interactive)
  ;; global-variable: fill-column
  (if (= (forward-line) 0)
      (let ((line-length
             (save-excursion
               (end-of-line)
               (current-column))))
        (if (<= line-length fill-column)
            (wikipedia-next-long-line)
          (message "Long line found")))
    ;; Stop, end of buffer reached.
    (error "Long line not found")))


(defun wikipedia-unfill-paragraph-simple ()
  "A very simple function for unfilling a paragraph."
  (interactive)
  (if (functionp 'filladapt-mode)
      (filladapt-mode nil))
  (let ((fill-column (point-max)))
    (fill-paragraph nil)
    (if (functionp 'filladapt-mode)
        (filladapt-mode nil))))

;;}}}

;;{{{ outline and outline-magic stuff

;;(add-hook 'wikipedia-mode-hook 'wikipedia-turn-on-outline-minor-mode)
;;(remove-hook 'wikipedia-mode-hook 'wikipedia-turn-on-outline-minor-mode)

(defun wikipedia-outline-cycle ()
  (interactive)
  (if (functionp 'outline-cycle)
      (outline-cycle)))

;; Fix-me: Unfortunately outline maybe does not take care of the
;; heading endings when promoting and demoting (I have submitted a bug
;; report for this and hooks will be added). To work around this we
;; defadvice outline-demote/promote:

(require 'outline)
(unless (boundp 'outline-demote-hook)

  (defadvice outline-demote (after wikipedia-outline-demote-advice
                                   (&optional which))
    "Adjust heading after demote."
    (unless which
      (wikipedia-adjust-header-end)))

  (defadvice outline-promote (after wikipedia-outline-promote-advice
                                    (&optional which))
    "Adjust heading after promote."
    (unless which
      (wikipedia-adjust-header-end)))

  (defadvice outline-insert-heading (after wikipedia-outline-insert-heading-advice
                                           ())
    "Adjust heading after insert new heading."
    (wikipedia-adjust-header-end))

  )

(defun wikipedia-adjust-header-end ()
  (when (eq major-mode 'wikipedia-mode)
    (let ((here (point))
          (end-pos (line-end-position))
          bgn-mark
          bgn-len
          end-mark
          end-len
          )
      (beginning-of-line)
      (when (looking-at outline-regexp)
        (setq bgn-mark (match-string-no-properties 0))
        (setq bgn-len (length bgn-mark))
        (end-of-line)
        (if (looking-back outline-regexp nil t)
            (when (progn
                    (setq end-mark (match-string-no-properties 0))
                    (setq end-len (length end-mark))
                    (/= end-len bgn-len))
              (replace-match bgn-mark))
          (insert bgn-mark)))
      ;;(lwarn 't :warning "bgn-len=%s, end-len=%s" bgn-len end-len)
      (goto-char here))))



;;(add-hook 'outline-minor-mode-hook  'wikipedia-outline-magic-keys)
(add-hook 'wikipedia-mode-hook  'wikipedia-outline-magic-keys)

(defun wikipedia-outline-magic-keys ()
  (interactive)
  (unless  (featurep 'xemacs)
    (local-set-key [(shift iso-lefttab)] 'wikipedia-outline-cycle))
  (local-set-key [iso-left-tab] 'wikipedia-outline-cycle)
  ;;(local-set-key [(meta left)]  'outline-promote)
  (local-set-key [(meta shift left)]  'outline-promote)
  ;;(local-set-key [(meta right)] 'outline-demote)
  (local-set-key [(meta shift right)] 'outline-demote)
  ;;(local-set-key [(control left)]  'wikipedia-simple-outline-promote)
  (local-set-key [(meta left)]  'wikipedia-simple-outline-promote)
  ;;(local-set-key [(control right)] 'wikipedia-simple-outline-demote)
  (local-set-key [(meta right)] 'wikipedia-simple-outline-demote)
  (local-set-key [(shift return)] 'newline-and-indent)
  ;;(local-set-key [(control up)] 'outline-move-subtree-up)
  (local-set-key [(meta shift up)] 'outline-move-subtree-up)
  ;;(local-set-key [(control down)] 'outline-move-subtree-down))
  (local-set-key [(meta shift down)] 'outline-move-subtree-down))

(defun wikipedia-enhance-indent ()      ;Version:1.26
  (interactive)
  (string-rectangle (region-beginning) (region-end) ":"))

(defun wikipedia-yank-prefix ()         ;Version:1.26
  (interactive)
  (string-rectangle (region-beginning) (region-end) ":"))

;; modification for outline-magic

(defun wikipedia-simple-outline-promote ()
  "Function simple deletes \"=\" and the end and the beginning of line,
does not promote the whole tree!"
  (interactive)
  (save-excursion
    (progn
      (beginning-of-line 1)
      (search-forward "=")
      (delete-char 1 nil)
      (end-of-line 1)
      (search-backward "=")
      (delete-char 1 nil))))

(defun wikipedia-simple-outline-demote ()
  "Function simple adds \"=\" and the end and the beginning of line,
does not promote the whole tree!"
  (interactive)
  (save-excursion
    (progn
      (beginning-of-line 1)
      (search-forward "=")
      (insert "=")
      (end-of-line 1)
      (search-backward "=")
      (insert "="))))


(defun wikipedia-rename-buffer ()       ;Version:1.5
  "Make sure that the option UNIQUE is used."
  (interactive)
  (rename-buffer (read-string "Name of new buffer (unique): " ) 1))

;;}}}

;;{{{ wikipedia drafts functionality: `stolen' from remember.el:

(defgroup wikipedia-draft nil
  "A mode to wikipedia-draft information."
  :group 'data)

;;; User Variables:

(defcustom wikipedia-draft-mode-hook nil
  "*Functions run upon entering wikipedia-draft-mode."
  :type 'hook
  :group 'wikipedia-draft)

(defcustom wikipedia-draft-register ?R
  "The register in which the window configuration is stored."
  :type 'character
  :group 'wikipedia-draft)

(defcustom wikipedia-draft-filter-functions nil
  "*Functions run to filter wikipedia-draft data.
All functions are run in the wikipedia-draft buffer."
  :type 'hook
  :group 'wikipedia-draft)

(defcustom wikipedia-draft-handler-functions '(wikipedia-draft-append-to-file)
  "*Functions run to process wikipedia-draft data.
Each function is called with the current buffer narrowed to what the
user wants wikipedia-drafted.
If any function returns non-nil, the data is assumed to have been
recorded somewhere by that function. "
  :type 'hook
  :group 'wikipedia-draft)

(defcustom wikipedia-draft-data-file "~/Wiki/discussions/draft.wiki"
  "*The file in which to store the wikipedia drafts."
  :type 'file
  :group 'wikipedia-draft)

(defcustom wikipedia-draft-reply-register ?M
  "The register in which the window configuration is stored."
  :type 'character
  :group 'wikipedia-draft)

(defcustom wikipedia-draft-page ?S      ;Version:1.37
  "The register in which the a page of the wiki draft file is stored."
  :type 'character
  :group 'wikipedia-draft)


(defcustom wikipedia-draft-leader-text "== "
  "*The text used to begin each wikipedia-draft item."
  :type 'string
  :group 'wikipedia-draft)


;;; Internal Variables:

(defvar wikipedia-draft-buffer "*Wikipedia-Draft*"
  "The name of the wikipedia-draft (temporary) data entry buffer.")

;;; User Functions:

;;;###autoload
(defun wikipedia-draft ()
  "Open a temporary buffer in wikipedia mode for editing an
 wikipedia draft, which an arbitrary piece of data. After
 finishing the editing either use \\[wikipedia-draft-buffer] to
 send the data into the wikipedia-draft-data-file, or send the
 buffer using `wikipedia-draft-send-to-mozex' and insert it later
 into a wikipedia article."
  (interactive)
  (window-configuration-to-register wikipedia-draft-register)
  (let ((buf (get-buffer-create wikipedia-draft-buffer)))
    (switch-to-buffer-other-window buf)
    (wikipedia-mode)
    (message " C-c C-k sends to draft file, C-c C-c sends to org buffer.")))





(defsubst wikipedia-draft-time-to-seconds (time)
  "Convert TIME to a floating point number."
  (+ (* (car time) 65536.0)
     (cadr time)
     (/ (or (car (cdr (cdr time))) 0) 1000000.0)))

(defsubst wikipedia-draft-mail-date (&optional rfc822-p)
  "Return a simple date.  Nothing fancy."
  (if rfc822-p
      (format-time-string "%a, %e %b %Y %T %z" (current-time))
    (format-time-string "%c" (current-time))))

(defun wikipedia-draft-buffer-desc ()
  "Using the first line of the current buffer, create a short description."
  (buffer-substring (point-min)
                    (save-excursion
                      (goto-char (point-min))
                      (end-of-line)
                      (if (> (- (point) (point-min)) 60)
                          (goto-char (+ (point-min) 60)))
                      (point))))


;; Wikipedia-Drafting to plain files:


(defun wikipedia-draft-append-to-file ()
  "Add a header together with a subject to the text and add it to the
draft file. It might be better if longlines-mode is off."
  (let ((text (buffer-string))
        (desc (wikipedia-draft-buffer-desc)))
    (with-temp-buffer
      (insert "\n\n")
      (insert wikipedia-draft-leader-text)
      (insert "Draft: ")                ;Version:1.39
      (insert (read-string "Enter Subject: "))
      (insert " ")
      (insert (current-time-string))
      (insert " ")
      (insert wikipedia-draft-leader-text)
      (insert "\n\n")                   ;Version:1.27
      (insert "")
      (insert "\n\n")
      (insert text)
      (insert "\n")
      (insert "")
      (insert "\n")
      (if (not (bolp))
          (insert "\n\n"))
      (if (find-buffer-visiting wikipedia-draft-data-file)
          (let ((wikipedia-draft-text (buffer-string)))
            (set-buffer (get-file-buffer wikipedia-draft-data-file))
            (save-excursion
              (goto-char (point-max))
              (insert "\n")
              (insert wikipedia-draft-text)
              (insert "\n")
              (save-buffer)))
        (append-to-file (point-min) (point-max) wikipedia-draft-data-file)))))


(setq wikipedia-draft-handler-functions 'wikipedia-draft-append-to-file)


(custom-add-option 'wikipedia-draft-handler-functions 'wikipedia-draft-append-to-file)

;;;###autoload
(defun wikipedia-draft-page ()          ;Version:1.32
  (interactive)
  (mark-page)
  (copy-region-as-kill (region-beginning) (region-end))
  (wikipedia-draft)
  (yank nil))


(defun wikipedia-draft-region (&optional beg end)
  "Wikipedia-Draft the data from BEG to END.
If called from within the wikipedia-draft buffer, BEG and END are ignored,
and the entire buffer will be wikipedia-drafted.  If called from any other
buffer, that region, plus any context information specific to that
region, will be wikipedia-drafted."
  (interactive)
  (let ((b (or beg (min (point) (or (mark) (point-min)))))
        (e (or end (max (point) (or (mark) (point-max))))))
    (save-restriction
      (narrow-to-region b e)
      (run-hook-with-args-until-success 'wikipedia-draft-handler-functions)
      (when (equal wikipedia-draft-buffer (buffer-name))
        (kill-buffer (current-buffer))
        (jump-to-register wikipedia-draft-register)))))

;;
;;;###autoload
(defun wikipedia-draft-buffer ()
  "Wikipedia-draft-buffer sends the contents of the current (temporary)
buffer to the wikipedia-draft-buffer, see the variable
wikipedia-draft-data-file."
  (interactive)
  (wikipedia-draft-region  (point-min) (point-max)))

(defun wikipedia-draft-clipboard ()
  "Wikipedia-Draft the contents of the current clipboard.
Most useful for wikipedia-drafting things from Netscape or other X Windows
application."
  (interactive)
  (with-temp-buffer
    (insert (x-get-clipboard))
    (run-hook-with-args-until-success 'wikipedia-draft-handler-functions)))

;;;###autoload


;;; Internal Functions:
(defvar wikipedia-draft-send-archive t  ;Version:1.56
  "*Archive the reply.")

(defvar wikipedia-draft-mode-map
  (let ((m (make-sparse-keymap)))
    (define-key m "\C-c\C-k" 'wikipedia-draft-buffer)
    (define-key m "\C-c\C-d" 'wikipedia-draft-buffer)
    m))

(defun wikipedia-draft-mode ()
  "Major mode for output from \\[wikipedia-draft].
\\<wikipedia-draft-mode-map> This buffer is used to collect data that
you want wikipedia-draft.  Just hit \\[wikipedia-draft-region] when
you're done entering, and it will go ahead and file the data for
latter retrieval, and possible indexing.
\\{wikipedia-draft-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (indented-text-mode)
  (use-local-map wikipedia-draft-mode-map)
  (setq major-mode 'wikipedia-draft-mode
        mode-name "Wikipedia-Draft")
  (run-hooks 'wikipedia-draft-mode-hook))


(defun wikipedia-draft-view-draft ()
  (interactive)
  "Simple shortcut to visit the file, which contains the wikipedia drafts."
  (find-file wikipedia-draft-data-file))

;;}}}

;;{{{ functions for marking regions

(defun wikipedia-mark-section ()        ;Version:1.36
  "Set mark at end of current logical section, and point at top."
  (interactive)
  (re-search-forward (concat  "== "  "[a-z,A-z \t]*"
                              " =="))
  (re-search-backward "^")
  (set-mark (point))
  (re-search-backward (concat "== "  "[a-z,A-z \t]*"
                              " "))
  (wikipedia-activate-region))

(defun wikipedia-mark-signature ()      ;Version:1.36
  "Set mark at end of current logical section, and point at top."
  (interactive)
  (re-search-forward "]]") ;;[[ ]]
  (re-search-backward "^")
  (set-mark (point))
  (re-search-backward "[[")
  (wikipedia-activate-region))

(when (featurep 'xemacs)
  (fset 'wikipedia-activate-region (symbol-function 'zmacs-activate-region)))

(unless (featurep 'xemacs)
  (defun wikipedia-activate-region ()
    nil))

;;}}}

;;{{{ `reply' and `send' functions

(defun wikipedia-draft-copy-page-to-register () ;Version:1.47
  "Copy a page via the wikipedia-draft-register."
  (interactive)
  (save-excursion
    (narrow-to-page nil)
    (copy-to-register wikipedia-draft-page (point-min) (point-max) nil)
    (message "draft page copied to wikipedia register wikipedia-draft-page.")
    (widen)))

                                        ;aux function
(defun wikipedia-draft-yank-page-to-register () ;Version:1.50
  "Insert a page   via the  wikipedia-draft-register."
  (interactive)
  (insert-register wikipedia-draft-page nil))



(defun wikipedia-draft-send-to-mozex (target-buffer) ;Version:1.56
  "Copy the current page from the wikipedia draft file  to
  TARGET-BUFFER, this buffer is named  something like  mozex.textarea.
Check the variable wikipedia-draft-send-archive.  If it is t, then
additionally the text will be archived in the draft.wiki file. Check
longlines-mode, it might be better if it is set off."
  (interactive "bTarget buffer: ")
  (let ((src-buf (current-buffer)))
    (wikipedia-draft-copy-page-to-register)
    (switch-to-buffer target-buffer)
    (end-of-line 1)
    (newline 1)
    (wikipedia-draft-yank-page-to-register)
    (message "The page has been sent (copied) to the mozex file!")
    (switch-to-buffer "*Wikipedia-Draft*")
    (when wikipedia-draft-send-archive          ;Version:1.56
      (let ((text (buffer-string))              ;Version:1.59
            (desc (wikipedia-draft-buffer-desc)))
        (with-temp-buffer
          (insert "\n\n")
          (insert wikipedia-draft-leader-text)
          (insert-register wikipedia-draft-reply-register 1)
          (insert " ")
          (insert (current-time-string))
          (insert " ")
          (insert wikipedia-draft-leader-text)
          (insert "\n\n")
          (insert "")
          (insert "\n\n")
          (insert text)
          (insert "\n")
          (insert "")
          (insert "\n")
          (if (not (bolp))
              (insert "\n\n"))
          (if (find-buffer-visiting wikipedia-draft-data-file)
              (let ((wikipedia-draft-text (buffer-string)))
                (set-buffer (get-file-buffer wikipedia-draft-data-file))
                (save-excursion
                  (goto-char (point-max))
                  (insert "\n")
                  (insert wikipedia-draft-text)
                  (insert "\n")
                  (save-buffer)))
            (append-to-file (point-min) (point-max) wikipedia-draft-data-file)))))
    (when (equal wikipedia-draft-buffer (buffer-name))
      (kill-buffer (current-buffer)))
    (switch-to-buffer target-buffer)))


;;Apr_22_2006
(defvar wikipedia-reply-with-hline nil
  "*Whether to use a hline as a header seperator in the reply.")

(defvar wikipedia-reply-with-quote nil  ;Version:1.60
  "*Whether to use a quotation tempalate or not.")

(defvar wikipedia-user-simplify-signature t
  "*Simple varible in order to threat complicated signatures of users, which uses
fonts and other makeup.")

(defun wikipedia-reply-at-signature ()  ;Version:1.40
  "Very simple function to add the reply prefix to the signature,
sorrounded by the boldface makeup. You have to set the point BEFORE
the signature, then the functions inserts the following
:'''Re: [[User:foo]]'''."
  (interactive)
  (beginning-of-line 1)
  (search-forward "[[")
  (mark-word 3)
  (yank)
  (end-of-line 1)
  (wikipedia-terminate-paragraph)
  (insert ":'''Re: ")
  (insert "[[")
  (yank)
  (insert "]]")
  (insert "'''"))




(defun wikipedia-draft-reply ()         ;Version:1.62
  "Open a temporary buffer in wikipedia mode for editing an wikipedia
draft, with an arbitrary piece of data. After finishing the editing
|]]:either use \"C-c C-k\" \\[wikipedia-draft-buffer] to send the data into
the wikipedia-draft-data-file, or send the buffer \"C-c\C-c\",
\\[wikipedia-draft-send-to-mozex] to the current wikipedia article via
mozex. Check the varibale wikipedia-draft-send-archive."
  (interactive)
  (wikipedia-reply-at-point-simple)
  (beginning-of-line 1)
  (kill-line nil)
  (save-excursion
    (window-configuration-to-register wikipedia-draft-register)
    (let ((buf (get-buffer-create wikipedia-draft-buffer)))
      (switch-to-buffer-other-window buf)
      (wikipedia-mode)
      (if (functionp 'pabbrev-mode)
          (pabbrev-mode))
      (when (not wikipedia-reply-with-quote)
        (when  wikipedia-reply-with-hline
          (insert "----")
          (newline 1))
        (yank)
        (end-of-line 1))
      (when wikipedia-reply-with-quote
        (insert "{{Quotation|")
        (yank)
        (insert "'''Re: ")
        (insert-register wikipedia-draft-reply-register 1)
        (insert "''' |~~~~}}")
        (backward-char 7))
      (message " C-c C-k sends to draft, C-c C-c sends to org buffer."))))

(defun wikipedia-reply-at-point-simple () ;Version:1.65
  "Reply to posts in discussion forums.
You have to put the region around the signature, then the
functions inserts the following
:'''Re: [[User:foo]]'''."
  (interactive)
  (beginning-of-line 1)
  (search-forward (get-lang-string wikipedia-lang 'wikip-utc))
  (search-backward (get-lang-string wikipedia-lang 'wikip-user-mark))
  (when (not wikipedia-user-simplify-signature)
    (mark-word 3))
  (when  wikipedia-user-simplify-signature
    (mark-word 2))
  (copy-to-register wikipedia-draft-reply-register (region-beginning) (region-end) nil)
  (end-of-line 1)
  (wikipedia-terminate-paragraph-and-indent)
  (insert ":'''Re: ")
  (insert-register wikipedia-draft-reply-register 1)
  (when  wikipedia-user-simplify-signature
    (insert "|]]''' "))
  (when  (not wikipedia-user-simplify-signature)
    (insert "]]''' ")))

;;}}}

;;{{{ Optional private stuff:

;; (defun wikipedia-insert-quotation-with-signature () ;Version:1.60
;;   "Insert quotation with signature.
;; When mark is active, surrounds region."
;;   (interactive)
;;   (wikipedia-insert-around-region "{{Quotation|" "}}{{~~~~}}"))

;; (defun wikipedia-insert-quotation () ;Version:1.60
;;   "Insert quotation box.
;; When mark is active, surrounds region."
;;   (interactive)
;;   ;; Fix-me: This uses a template, is that really always available???
;;   (wikipedia-insert-around-region "{{Quotation|" "}}"))

;; (define-key wikipedia-mode-map "\C-c\C-fv" 'wikipedia-insert-bible-verse-template)
;;
;; (defun wikipedia-insert-bible-verse-template ()
;;   "Insert a template for the quotation of bible verses."
;;   (interactive)
;;   (insert "({{niv|")
;; (let ((name    (read-string "Name: ")))
;;      (insert (concat name "|"))
;; (let ((verse (read-string "Verse: ")))
;;   (insert (concat verse "|" name " " verse "}})")))))

;;}}}

(provide 'wikipedia-mode)
;;; wikipedia-mode.el ends here.


