;;; nxhtml-strval.el ---
;;
;; Author: Lennart Borgman (lennart O borgman A gmail O com)
;; Created: Wed Jun 06 12:42:09 2007
(defconst nxhtml-strval:version "0.3") ;;Version:
;; LXast-Updated: Sun Jun 10 14:52:50 2007 (7200 +0200)
;; URL:
;; Keywords:
;; Compatibility:
;;
;; FXeatures that might be required by this library:
;;
;;   None
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; This is a workaround for a problem caused by that the parser from
;; `nxml-mode' parses the whole buffer. This workaround handles things
;; like
;;
;;   <a href="<?php title(); ?>">...</a>
;;
;; where the string value is not valid XHTML (because of the <).  When
;; the minor mode `nxhtml-strval-mode' is on this construct will be
;; replaced by text that are valid XHTML. When writing to file or
;; copying/yanking this will be replaced with the intended text.
;;
;; For a long term solution the parser should be broken up, see also
;;
;;   http://sourceforge.net/mailarchive/forum.php?thread_name=4638A428.9010408%40pareto.nl&forum_name=cedet-devel
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

(define-minor-mode nxhtml-strval-mode
  "Handle some useful, but not XHTML compliant attribute values.
This is mainly for PHP and similar.

Things like

   <a href=\"<?php title(); ?>\">...</a>

may be very useful in PHP.  However the string value is not valid
XHTML (because of the <).  This makes it difficult to use XHTML
completion and validation.

This minor mode tries to take care of that by substituting the
<?php and ?> in the buffer while editing with something else.
The screen still shows <?php and ?> and when writing the buffer
to a file the substitutes are reverted to <?php and ?>.

Note that this is a workaround. See the comments in the source
file. There are several \(I hope minor) problems with it. For
example is the buffer marked as modified when turning on/off this
minor mode.

IMPORTANT: Do not edit the replaced string <? or ?>."
  :lighter nil
  :gropu 'nxhtml
  (if nxhtml-strval-mode
      (nxhtml-strval-mode-turn-on)
    (nxhtml-strval-mode-turn-off)))
(put 'nxhtml-strval-mode 'permanent-local t)

(defcustom nxhtml-strval-replface 'nxhtml-strval-replface
  "Face used to mark replaced characters in strings."
  :type 'face
  :group 'nxhtml)

(defface nxhtml-strval-replface '((t :inherit font-lock-warning-face))
  "Default face used to mark replaced characters in strings."
  :group 'nxhtml)

(defun nxthml-strval-add-ovl (start end)
  (let ((ovl (make-overlay start end nil t nil)))
    (overlay-put ovl 'nxhtml-strval t)
    (overlay-put ovl 'face font-lock-warning-face)))
    ;;(overlay-put ovl 'face 'highlight)))

(defun nxhtml-strval-remove-all-ovls ()
  (remove-overlays (point-min) (point-max) 'nxhtml-strval t))

(defun nxhtml-strval-replace-match ()
  (let ((s (compose-string "{" nil nil ?<)))
    (replace-match s t t nil 1))
  (put-text-property (1- (point)) (point) 'font-lock-face 'font-lock-warning-face)
  (put-text-property (1- (point)) (point) 'nxhtml-strval-> t)
  (nxthml-strval-add-ovl (1- (point)) (point))
  (let ((s (compose-string "}" nil nil ?>)))
    (replace-match s t t nil 2))
  (nxthml-strval-add-ovl (1- (point)) (point))
  (put-text-property (1- (point)) (point) 'font-lock-face 'font-lock-warning-face)
  (put-text-property (1- (point)) (point) 'nxhtml-strval-> nil))

(defun nxhtml-strval-revert-match ()
  (replace-match "<" t t nil 1)
  (put-text-property (1- (point)) (point) 'font-lock-face nil)
  (put-text-property (1- (point)) (point) 'nxhtml-strval-> nil)
  (replace-match ">" t t nil 2)
  (put-text-property (1- (point)) (point) 'font-lock-face nil)
  (put-text-property (1- (point)) (point) 'nxhtml-strval-> nil))

(defconst nxhtml-strval-on-re  "\"\\(<\\)[^>]*\\(>\\)\"")
(defconst nxhtml-strval-off-re "\"\\({\\)[^>]*\\(}\\)\"")

(defun nxhtml-strval-mode-turn-on ()
  (unless (derived-mode-p 'nxml-mode 'php-mode)
    (error "%s is not derived from nxml-mode" major-mode))
  (add-hook 'write-contents-functions 'nxhtml-strval-write-contents nil t)
  (make-local-variable 'buffer-substring-filters)
  (add-to-list 'buffer-substring-filters 'nxhtml-strval-buffer-substring-filter)
  (nxhtml-strval-replace-values)
  (add-hook 'after-change-functions 'nxhtml-strval-after-change nil t))

(defun nxhtml-strval-replace-values ()
  (save-excursion
    (save-match-data
      (save-restriction
        (widen)
        (goto-char (point-min))
        (while (re-search-forward nxhtml-strval-on-re nil t)
          (nxhtml-strval-replace-match))))))

(defun nxhtml-strval-mode-turn-off ()
  (remove-hook 'after-change-functions 'nxhtml-strval-after-change t)
  (nxhtml-strval-revert-values)
  (remove-hook 'write-contents-functions 'nxhtml-strval-write-contents t)
  (kill-local-variable 'buffer-substring-filters))

(defun nxhtml-strval-revert-values ()
  (save-excursion
    (save-match-data
      (save-restriction
        (widen)
        (goto-char (point-min))
        (while (re-search-forward nxhtml-strval-off-re nil t)
          (nxhtml-strval-revert-match))
        (nxhtml-strval-remove-all-ovls)))))

(defun nxhtml-strval-write-contents ()
  (let ((nxhtml-strval-no-after-change t))
    ;;(setq write-contents-functions (delq 'nxhtml-strval-write-contents write-contents-functions))
    (remove-hook 'write-contents-functions 'nxhtml-strval-write-contents t)
    (undo-boundary)
    (nxhtml-strval-revert-values)
    ;;(write-file (buffer-file-name))
    (save-buffer)
    ;; Fix-me: undo
    ;;(nxhtml-strval-replace-values)
    (undo-start)
    (undo-more 1)
    (add-hook 'write-contents-functions 'nxhtml-strval-write-contents nil t)
    (set-buffer-modified-p nil)
    t))

;;; Clip board etc.
(defun nxhtml-strval-buffer-substring-filter (orig-str)
  (let ((str (replace-regexp-in-string "\"{" "\"<" orig-str)))
    (setq str (replace-regexp-in-string "}\"" ">\"" str))
    str
    ))

;;; Changes
; after-change-functions
(defun nxhtml-strval-after-change (beg end len)
  (unless (and (boundp 'nxhtml-strval-no-after-change)
               nxhtml-strval-no-after-change)
    (let ((here (point))
          (new-beg beg)
          (new-end end))
      (goto-char beg)
      (setq new-beg (line-beginning-position))
      (goto-char end)
      (setq new-end (line-end-position))
      ;; Fix-me: examine old replacements here
      (remove-text-properties new-beg new-end '(nxhtml-strval-< nxhtml-strval->))
      (goto-char new-beg)
      (while (re-search-forward nxhtml-strval-on-re new-end t)
        (nxhtml-strval-replace-match))
      (goto-char here)
      )))

(provide 'nxhtml-strval)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; nxhtml-strval.el ends here
