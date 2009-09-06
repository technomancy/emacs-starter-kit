;;; inlimg.el --- Display images inline
;;
;; Author: Lennart Borgman (lennart O borgman A gmail O com)
;; Created: 2008-09-27
(defconst inlimg:version "0.7") ;; Version:
;; Last-Updated: 2009-07-14 Tue
;; URL:
;; Keywords:
;; Compatibility:
;;
;; Features that might be required by this library:
;;
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; Display images inline.  See `inlimg-mode' for more information.
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
(eval-when-compile (require 'mumamo nil t))
(eval-when-compile (require 'ourcomments-util nil t))

(defvar inlimg-assoc-ext
  '((png  (".png"))
    (gif  (".gif"))
    (tiff (".tiff"))
    (jpeg (".jpg" ".jpeg"))
    (xpm  (".xpm"))
    (xbm  (".xbm"))
    (pbm  (".pbm"))))

(defvar inlimg-img-regexp nil)
(make-variable-buffer-local 'inlimg-img-regexp)
(put 'inlimg-img-regexp 'permanent-local t)

(defvar inlimg-img-regexp-html
  (rx (or (and "<img"
               (1+ space)
               (0+ (1+ (not (any " <>")))
                   (1+ space))
               "src=\""
               (group (1+ (not (any "\""))))
               "\""
               (*? anything)
               "/>")
          (and "url("
               ?\"
               (group (1+ (not (any "\)"))))
               ?\"
               ")"
               )
          (and "url("
               (group (+? (not (any ")"))))
               ")"
               )
          )))

(defvar inlimg-img-regexp-org
  (rx-to-string
   `(and "[[file:"
         (group (+? (not (any "\]")))
                ,(let ((types nil))
                   (dolist (typ image-types)
                     (when (image-type-available-p typ)
                       (dolist (ext (cadr (assoc typ inlimg-assoc-ext)))
                         (setq types (cons ext types)))))
                   (cons 'or types)))
         "]"
         (optional "["
                   (+? (not (any "\]")))
                   "]")
         "]"
         )))

(defconst inlimg-modes-img-values
  '(
    (html-mode inlimg-img-regexp-html)
    (org-mode  inlimg-img-regexp-org)
    ))

(defun inlimg-img-spec-p (spec)
  (assoc spec inlimg-modes-img-values))

(defgroup inlimg nil
  "Customization group for inlimg."
  :group 'nxhtml)

(defcustom inlimg-margins '(50 . 5)
  "Margins when displaying image."
  :type '(cons (integer :tag "Left margin")
               (integer :tag "Top margin"))
  :set (lambda (sym val)
         (set-default sym val)
         (when (fboundp 'inlimg-update-all-buffers)
           (inlimg-update-all-buffers)))
  :group 'inlimg)

(defcustom inlimg-slice '(0 0 400 100)
  "How to slice images."
  :type '(choice (const :tag "Show whole images" nil)
                 (list :tag "Show slice of image"
                       (integer :tag "Top")
                       (integer :tag "Left")
                       (integer :tag "Width")
                       (integer :tag "Height")))
  :set (lambda (sym val)
         (set-default sym val)
         (when (fboundp 'inlimg-update-all-buffers)
           (inlimg-update-all-buffers)))
  :group 'inlimg)

(define-widget 'inlimg-spec-widget 'symbol
  "A major mode lisp function."
  :complete-function (lambda ()
                       (interactive)
                       (lisp-complete-symbol 'inlimg-img-spec-p))
  :prompt-match 'inlimg-img-spec-p
  :prompt-history 'widget-function-prompt-value-history
  :match-alternatives '(inlimg-img-spec-p)
  :validate (lambda (widget)
              (unless (inlimg-img-spec-p (widget-value widget))
                (widget-put widget :error (format "Invalid function: %S"
                                                  (widget-value widget)))
                widget))
  :value 'org-mode
  :tag "Inlimg image values spec name")

;; (customize-option 'inlimg-mode-specs)
(defcustom inlimg-mode-specs
  '(
    (xml-mode html-mode)
    (sgml-mode html-mode)
    (nxml-mode html-mode)
    (php-mode html-mode)
    (css-mode html-mode)
    )
  "Equivalent mode for image tag search.
Note that derived modes \(see info) are recognized by default.

To add new image tag patterns modify `inlimg-modes-img-values'."
  :type '(repeat
          (list (major-mode-function :tag "Major mode")
                (inlimg-spec-widget :tag "Use tags as specified in")))
  :group 'inlimg)

(defface inlimg-img-tag '((t :inherit 'lazy-highlight))
  "Face added to img tag when displaying image."
  :group 'inlimg)

(defface inlimg-img-remote '((t :inherit 'isearch-fail))
  "Face used for notes telling image is remote."
  :group 'inlimg)

(defface inlimg-img-missing '((t :inherit 'trailing-whitespace))
  "Face used for notes telling image is missing."
  :group 'inlimg)

(defvar inlimg-img-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map [(control ?c) ?+] 'inlimg-toggle-display)
    (define-key map [(control ?c) ?%] 'inlimg-toggle-slicing)
    map)
  "Keymap on image overlay.")

(eval-after-load 'gimp
  '(gimp-add-point-bindings inlimg-img-keymap))

(defsubst inlimg-ovl-p (ovl)
  "Return non-nil if OVL is an inlimg image overlay."
  (overlay-get ovl 'inlimg-img))

(defun inlimg-ovl-valid-p (ovl)
  (and (overlay-get ovl 'inlimg-img)
       (save-match-data
         (let ((here (point)))
           (goto-char (overlay-start ovl))
           (prog1
               (looking-at (symbol-value inlimg-img-regexp))
             (goto-char here))))))

(defun inlimg-next (pt display-image)
  "Display or hide next image after point PT.
If DISPLAY-IMAGE is non-nil then display image, otherwise hide it.

Return non-nil if an img tag was found."
  (let (src dir beg end img ovl remote beg-face)
    (goto-char pt)
    (save-match-data
      (when (re-search-forward (symbol-value inlimg-img-regexp) nil t)
        (setq src (or (match-string-no-properties 1)
                      (match-string-no-properties 2)
                      (match-string-no-properties 3)))
        (setq beg (match-beginning 0))
        (setq beg-face (get-text-property beg 'face))
        (setq remote (string-match "^https?://" src))
        (setq end (- (line-end-position) 0))
        (setq ovl (catch 'old-ovl
                    (dolist (ovl (overlays-at beg))
                      (when (inlimg-ovl-p ovl)
                        (throw 'old-ovl ovl)))
                    nil))
        (unless ovl
          (setq ovl (make-overlay beg end))
          (overlay-put ovl 'inlimg-img t)
          (overlay-put ovl 'priority 100)
          (overlay-put ovl 'face 'inlimg-img-tag)
          (overlay-put ovl 'keymap inlimg-img-keymap))
        (overlay-put ovl 'image-file src)
        (overlay-put ovl 'inlimg-slice inlimg-slice)
        (if display-image
            (unless (memq beg-face '(font-lock-comment-face font-lock-string-face))
              (unless remote
                (setq dir (if (buffer-file-name)
                              (file-name-directory (buffer-file-name))
                            default-directory))
                (setq src (expand-file-name src dir)))
              (if (or remote (not (file-exists-p src)))
                  (setq img (propertize
                             (if remote " Image is on the web " " Image not found ")
                             'face (if remote 'inlimg-img-remote 'inlimg-img-missing)))
                (setq img (create-image src nil nil
                                        :relief 5
                                        :margin inlimg-margins))
                (setq img (inlimg-slice-img img inlimg-slice)))
              (let ((str (copy-sequence "\nX")))
                (setq str (propertize str 'face 'inlimg-img-tag))
                (put-text-property 1 2 'display img str)
                (overlay-put ovl 'after-string str)))
          (overlay-put ovl 'after-string nil))))
    ovl))

(defun inlimg-slice-img (img slice)
  (if (not slice)
      img
    (let* ((sizes (image-size img t))
           (width  (car sizes))
           (height (cdr sizes))
           (sl-left (nth 0 slice))
           (sl-top (nth 1 slice))
           (sl-width (nth 2 slice))
           (sl-height (nth 3 slice)))
      (when (> sl-left width) (setq sl-left 0))
      (when (> (+ sl-left sl-width) width) (setq sl-width (- width sl-left)))
      (when (> sl-top height) (setq sl-top 0))
      (when (> (+ sl-top sl-height) height) (setq sl-height (- height sl-top)))
      (setq img (list img))
      (setq img (cons (append '(slice)
                              slice
                              (list sl-top sl-left sl-width sl-height)
                              nil)
                      img)))))

;;;###autoload
(define-minor-mode inlimg-mode
  "Display images inline.
Search buffer for image tags.  Display found images.

Image tags are setup per major mode in `inlimg-mode-specs'.

Images are displayed on a line below the tag referencing them.
The whole image or a slice of it may be displayed, see
`inlimg-slice'.  Margins relative text are specified in
`inlimg-margins'.

See also the commands `inlimg-toggle-display' and
`inlimg-toggle-slicing'.

Note: This minor mode uses `font-lock-mode'."
  :keymap nil
  :group 'inlimg
  (if inlimg-mode
      (progn
        (let ((major-mode (or (and (boundp 'mumamo-multi-major-mode)
                                   mumamo-multi-major-mode
                                   (mumamo-main-major-mode))
                              major-mode)))
          (add-hook 'font-lock-mode-hook 'inlimg-on-font-lock-off)
          (inlimg-get-buffer-img-values))
        (inlimg-font-lock t))
    (inlimg-font-lock nil)
    (inlimg-delete-overlays)))
(put 'inlimg-mode 'permanent-local t)

(defun inlimg-delete-overlays ()
  (save-restriction
    (widen)
    (let (ovl)
      (dolist (ovl (overlays-in (point-min) (point-max)))
        (when (inlimg-ovl-p ovl)
          (delete-overlay ovl))))))

(defun inlimg-get-buffer-img-values ()
  (let* (rec
         (spec (or (catch 'spec
                     (dolist (rec inlimg-mode-specs)
                       (when (derived-mode-p (car rec))
                         (throw 'spec (nth 1 rec)))))
                   major-mode))
        (values (when spec (nth 1 (assoc spec inlimg-modes-img-values))))
        )
    (setq inlimg-img-regexp values)
    ))

(defun inlimg--global-turn-on ()
  (inlimg-get-buffer-img-values)
  (when inlimg-img-regexp
    (inlimg-mode 1)))

;;;###autoload
(define-globalized-minor-mode inlimg-global-mode inlimg-mode inlimg--global-turn-on)

;;;###autoload
(defun inlimg-toggle-display (point)
  "Toggle display of image at point POINT.
See also the command `inlimg-mode'."
  (interactive (list (point)))
  (let ((here (point))
        (ovl
         (catch 'ovl
           (dolist (ovl (overlays-at (point)))
             (when (inlimg-ovl-p ovl)
               (throw 'ovl ovl)))))
        is-displayed)
    (if (not ovl)
        (message "No image at point %s" here)
      (setq is-displayed (overlay-get ovl 'after-string))
      (inlimg-next (overlay-start ovl) (not is-displayed))
      (goto-char here))))

;;;###autoload
(defun inlimg-toggle-slicing (point)
  "Toggle slicing of image at point POINT.
See also the command `inlimg-mode'."
  (interactive (list (point)))
  (let* ((here (point))
         (ovl
         (catch 'ovl
           (dolist (ovl (overlays-at (point)))
             (when (inlimg-ovl-p ovl)
               (throw 'ovl ovl)))))
         (inlimg-slice inlimg-slice)
        is-displayed)
    (if (not ovl)
        (message "No image at point %s" here)
      (setq is-displayed (overlay-get ovl 'after-string))
      (when (overlay-get ovl 'inlimg-slice)
        (setq inlimg-slice nil))
      (inlimg-next (overlay-start ovl) is-displayed)
      (goto-char here))))


(defun inlimg-font-lock-fun (bound)
  (let ((here (point))
        old-ovls new-ovls ovl)
    (goto-char (line-beginning-position))
    (dolist (ovl (overlays-in (point) bound))
      (when (inlimg-ovl-p ovl)
        (setq old-ovls (cons ovl old-ovls))))
    (while (and (< (point) bound)
                (setq ovl (inlimg-next (point) t)))
      (setq new-ovls (cons ovl new-ovls)))
    (dolist (ovl old-ovls)
      (unless (inlimg-ovl-valid-p ovl)
        (delete-overlay ovl)
        ))))

;; Fix-me: This stops working for changes with nxhtml-mumamo-mode, but
;; works for nxhtml-mode and html-mumamo-mode...
(defvar inlimg-this-is-not-font-lock-off nil)
(defun inlimg-font-lock (on)
  (let ((add-or-remove (if on 'font-lock-add-keywords 'font-lock-remove-keywords))
        (link-fun))
    (funcall add-or-remove nil
             `((inlimg-font-lock-fun
                1
                mlinks-link
                prepend)))
    (let ((inlimg-this-is-not-font-lock-off t)
          (mumamo-multi-major-mode nil))
      (font-lock-mode -1)
      (font-lock-mode 1))))

(defun inlimg-on-font-lock-off ()
  (unless (or inlimg-this-is-not-font-lock-off
              (and (boundp 'mumamo-multi-major-mode)
                   mumamo-multi-major-mode))
    (when inlimg-mode
      (inlimg-mode -1)
      )))
(put 'inlimg-on-font-lock-off 'permanent-local-hook t)


(provide 'inlimg)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; inlimg.el ends here
