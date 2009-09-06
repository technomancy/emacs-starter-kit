;;; xhtml-help.el --- Browse XHTML reference sites

;; Copyright (C) 2005 by Lennart Borgman

;; Author:  Lennart Borgman <lennart DOT borgman DOT 073 AT student DOT lu DOT se>
;; Created: 2005-08-16
;; Last-Updated: Wed Aug 01 14:24:07 2007 (7200 +0200)
(defconst xhtml-help:version "0.57") ;; Version:
;; Keywords: languages

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; Use when editing XHTML file to get tag references or CSS property
;; name references (like background-color) from web sources.
;;
;; Usage:
;;
;;   (require 'fmode)
;;
;; Then call `xhtml-help-show-tag-ref' or `xhtml-help-show-css-ref'.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; History:
;;
;; 2005-12-02: Corrected fetching margin-*.
;; 2006-01-08: Prompt for tag and property name before fetching help.


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

(defun xhtml-help-css-prop-at-point ()
  "Get possible css name property at point."
  (save-excursion
    (let ((ch (char-after))
          (in-word))
      (when (and (not (bolp))
                 (or (not ch)
                     (member ch '(10 9 32 ?\:))))
        (backward-char)
        (setq ch (char-after)))
      (while (string-match "[a-z-]" (char-to-string ch))
        (setq in-word t)
        (backward-char)
        (setq ch (char-after)))
      (when in-word
        (forward-char)
        (when (looking-at "[a-z-]+")
          (match-string-no-properties 0))))))

;;;###autoload
(defun xhtml-help-show-css-ref ()
  "Show CSS reference for CSS property name at point."
  (interactive)
  (let ((css-prop (xhtml-help-css-prop-at-point)))
    (setq css-prop (read-from-minibuffer "Get help for CSS property: " css-prop))
    (when css-prop
      (xhtml-help-browse-css css-prop))))

;;;###autoload
(defun xhtml-help-tag-at-point ()
  "Get xhtml tag name at or before point."
  (save-excursion
    (when (eq (following-char) ?<)
      (forward-char))
    (when (and (search-backward "<" nil t)
               (looking-at "</?\\([[:alnum:]]+\\)"))
      (match-string-no-properties 1))))

;;;###autoload
(defun xhtml-help-show-tag-ref ()
  "Show xhtml reference for tag name at or before point."
  (interactive)
  (let ((tag (xhtml-help-tag-at-point)))
    (setq tag (read-from-minibuffer "Get help for tag name: " tag))
    (when (< 0 (length tag))
      (xhtml-help-browse-tag tag))))

(defgroup xhtml-help nil
  "Customization group for xhtml-help."
  :group 'nxhtml
  :group 'hypermedia)

(defcustom xhtml-help-refurl "http://www.w3.org/"
  "Web url to get references from."
  :type '(choice
          (const "http://www.w3.org/")
          (const "http://xhtml.com/")
          (const "http://www.w3schools.com/")
          ;;(const "http://learningforlife.fsu.edu/")
          )
  :group 'xhtml-help)

(defcustom xhtml-help-query-refurl t
  "Query for reference url.
This is used in `xhtml-help-browse-tag' and `xhtml-help-browse-css'."
  :type 'boolean
  :group 'xhtml-help)

(defun xhtml-help-query-refurl (prompt &optional notvalid)
  (let ((choices (get 'xhtml-help-refurl 'custom-type))
        (default xhtml-help-refurl))
    (unless (eq 'choice (car choices))
      (error "Custom type of xhtml-help-refurl is not choices"))
    (setq choices (cdr choices))
    (setq choices (mapcar (lambda (elt)
                            (car (cdr elt)))
                          choices))
    (mapc (lambda (elt)
            (setq choices (delete elt choices)))
          notvalid)
    (when (member default notvalid)
      (setq default (car choices)))
    (completing-read (concat "Fetch " prompt " reference from: ")
                     choices
                     nil
                     t
                     default
                     '(choices . 1))))

(defun xhtml-match (target str)
  (let ((len (length target)))
    (when (<= len (length str))
      (equal target (substring str 0 len)))))

(defun xhtml-match-member (target str-list)
  (let (m)
    (mapc (lambda (elt)
            (when (xhtml-match elt target)
              (setq m t)))
          str-list)
    m))

(defun xhtml-help-browse-css (css-prop)
  (let* ((refurl (if xhtml-help-query-refurl
                     (xhtml-help-query-refurl (concat "CSS property '" css-prop "'")
                                              (list "http://xhtml.com/"))
                   xhtml-help-refurl))
         (url
          (cond
           (  (equal refurl "http://www.w3schools.com/")
              (concat
               refurl "css/pr_"
               (cond
                (  (member css-prop '("clear" "cursor" "display" "float" "position" "visibility"))
                   "class_")
                (  (member css-prop '("height" "line-height" "max-width" "min-height" "min-width" "width"))
                   "dim_")
                (  (xhtml-match "font-weight" css-prop)
                   (setq css-prop "") "font_weight")
                (  (xhtml-match "font" css-prop)
                   "font_")
                (  (member css-prop '("content" "counter-increment" "counter-reset" "quotes"))
                   "gen_")
                (  (xhtml-match "list" css-prop)
                   "list_")
                (  (xhtml-match "margin" css-prop)
                   "")
                (  (xhtml-match "outline" css-prop)
                   "outline_")
                (  (equal "padding" css-prop)
                   "")
                (  (xhtml-match "padding" css-prop)
                   "padding_")
                (  (member css-prop '("bottom" "clip" "left" "overflow" "right" "top"
                                      "vertical-align" "z-index"))
                   "pos_")
                (  (member css-prop '("border-collapse"))
                   "tab_")
                (  (member css-prop '("color" "direction" "letter-spacing" "text-align"
                                      "text-decoration" "text-indent" "text-transform"
                                      "white-space" "word-spacing"))
                   "text_")
                (  t ""))
               css-prop ".asp"))
           ;; (  (equal refurl "http://learningforlife.fsu.edu/")
           ;;    (let ((css-prop2 css-prop)
           ;;          (cc)
           ;;          (ii 0))
           ;;      (while (< ii (length css-prop))
           ;;        (setq cc (substring css-prop2 ii (1+ ii)))
           ;;        (when (equal cc "-")
           ;;          (store-substring css-prop2 ii "_"))
           ;;        (setq ii (1+ ii)))
           ;;      (concat
           ;;       refurl "webmaster/references/css/" css-prop2 ".cfm")))
           (  (equal refurl "http://www.w3.org/")
              (let ((properties "")
                    (prop-def ""))
                (concat
                 refurl "TR/REC-CSS2/"
                 (cond
                  (  (xhtml-match-member css-prop '("margin" "padding" "border"))
                     "box.html#propdef-")
                  (  (member css-prop '("display" "position"
                                        "top" "right" "bottom" "left"
                                        "float" "clear"
                                        "z-index"
                                        "direction" "unicode-bidi"))
                     "visuren.html#propdef-")
                  (  (member css-prop '("width" "min-width" "max-width"
                                        "height" "min-height" "max-height"
                                        "line-height" "vertical-align"))
                     "visudet.html#propdef-")
                  (  (member css-prop '("overflow" "clip" "visibility"))
                     "visufx.html#propdef-")
                  (  (member css-prop '("content" "quotes"))
                     "generate.html#propdef-")
                  (  (or (xhtml-match css-prop "counter")
                         (member css-prop '("marker-offset")))
                     (setq css-prop "")
                     "generate.html#counters")
                  (  (xhtml-match-member css-prop '("list"))
                     "generate.html#propdef-")
                  (  (member css-prop '("size" "marks"
                                        "page-break-before" "page-break-after" "page-break-inside"
                                        "page"
                                        "orphans" "widows"
                                        ))
                     "page.html#propdef-")
                  (  (member css-prop '("color"
                                        "background-color" "background-image" "background-repeat"
                                        "background-attachment" "background-position" "background"
                                        ))
                     "colors.html#propdef-")
                  (  (xhtml-match "font" css-prop)
                     "fonts.html#propdef-")
                  (  (xhtml-match "text" css-prop)
                     "text.html#")
                  (  (member css-prop '("letter-spacing" "word-spacing"))
                     "text.html#")
                  ;; Fix-me: tables???
                  (  (member css-prop '("cursor"))
                     "ui.html#propdef-")
                  (  (xhtml-match "outline" css-prop)
                     "ui.html#dynamic-outlines")
                  (  (or (xhtml-match "speak" css-prop)
                         (xhtml-match "pause" css-prop)
                         (xhtml-match "cue" css-prop)
                         (xhtml-match "pitch" css-prop)
                         (member css-prop '("volume" "play-during" "azimuth" "elevation"
                                            "speech-rate" "voice-family" "richness")))
                     "aural.html#propdef-")
                  )
                 css-prop
                 )))
           (  t (error "Bad value for xhtml-help-refurl: %s" refurl)))))
    (browse-url url)))





(defun xhtml-help-browse-tag (tag)
  (let* ((refurl (if xhtml-help-query-refurl
                     (xhtml-help-query-refurl (concat "XHTML tag '" tag "'")
                                              (list "http://www.w3.org/"))
                   xhtml-help-refurl))
         (url
          (cond
           (  (equal refurl "http://xhtml.com/")
              (concat
               refurl "en/xhtml/reference/"
               tag
               "/")
              )
           (  (equal refurl "http://www.w3schools.com/")
              (concat
               refurl "tags/"
               (cond
                (  (member tag '("tt" "i" "b" "big" "small"))
                   "tag_font_style.asp")
                (  (member tag '("em" "strong" "dfn" "code" "samp" "kbd" "var" "cite"))
                   "tag_phrase_elements.asp")
                (  (member tag '("h1" "h2" "h3" "h4" "h5" "h6"))
                   "tag_hn.asp")
                (  (member tag '("sub" "sup"))
                   "tag_sup.asp")
                (  t
                   (concat "tag_" tag ".asp")
                   ))))
           ;; (  (equal refurl "http://learningforlife.fsu.edu/")
           ;;     (concat
           ;;      refurl "webmaster/references/xhtml/tags/"
           ;;      (cond
           ;;       (   (member tag '("body" "head" "html" "title"))
           ;;           "structure/")
           ;;       (   (member tag '("abbr" "acronym" "address" "blockquote" "br" "cite"
           ;;                         "code" "dfn" "div" "em" "h1" "h2" "h3" "h4" "h5" "h6"
           ;;                         "kbd" "p" "pre" "q" "samp" "span" "strong" "var"))
           ;;           "text/")
           ;;       (   (member tag '("a"))
           ;;           "hypertext/")
           ;;       (   (member tag '("dl" "dd" "dt" "ol" "ul" "li"))
           ;;           "list/")
           ;;       (   (member tag '("object" "param"))
           ;;           "object/")
           ;;       (   (member tag '("b" "big" "hr" "i" "small" "sub" "sup" "tt"))
           ;;           "presentation/")
           ;;       (   (member tag '("del" "ins"))
           ;;           "edit/")
           ;;       (   (member tag '("bdo"))
           ;;           "bidirectional/")
           ;;       (   (member tag '("button" "fieldset" "form" "input" "label" "legend"
           ;;                         "select" "optgroup" "option" "textarea"))
           ;;           "forms/")
           ;;       (   (member tag '("caption" "col" "colgroup" "table" "tbody" "td"
           ;;                         "tfoot" "th" "thead" "tr"))
           ;;           "table/")
           ;;       (   (member tag '("img"))
           ;;           "image/")
           ;;       (   (member tag '("area" "map"))
           ;;           "client/")
           ;;       (   (member tag '("area" "map"))
           ;;           "client/")
           ;;       (   (member tag '("meta"))
           ;;           "meta/")
           ;;       (   (member tag '("noscript" "script"))
           ;;           "scripting/")
           ;;       (   (member tag '("style"))
           ;;           "stylesheet/")
           ;;       (   (member tag '("link"))
           ;;           "link/")
           ;;       (   (member tag '("base"))
           ;;           "base/")
           ;;       (   (member tag '("base"))
           ;;           "base/")
           ;;       (   (member tag '("ruby" "rbc" "rtc" "rb" "rt" "rp"))
           ;;           "ruby/")
           ;;       )
           ;;      tag ".cfm"))
           (  t (error "Bad value for xhtml-help-refurl: %s" refurl))
           )))
    (browse-url url)))

(defconst xhtml-help-mode-keymap
  (let ((map (make-sparse-keymap "XHTML Help")))
    (define-key map [menu-bar xh-help] (cons "XHTML Help" (make-sparse-keymap "second")))
    (define-key map [menu-bar xh-help css-help] '("CSS Help" . xhtml-help-show-css-ref))
    (define-key map [menu-bar xh-help tag-help] '("XHTML Tag Help" . xhtml-help-show-tag-ref))
    map))

(define-minor-mode xhtml-help-mode
  "Minor mode that adds keys for accessing xhtml and css help."
  :keymap xhtml-help-mode-keymap)

(provide 'xhtml-help)

;;; xhtml-help.el ends here
