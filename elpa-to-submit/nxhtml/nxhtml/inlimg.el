;;; inlimg.el --- Display images inline
;;
;; Author: Lennart Borgman (lennart O borgman A gmail O com)
;; Created: 2008-09-27
(defconst inlimg:version "0.5") ;; Version:
;; Last-Updated: 2008-09-27T13:26:46+0200 Sat
;; URL:
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
;; Display images inline. By default it can handle image referenced in
;; <img src="..." /> inline.
;;
;; See `inlimg-mode' and `inlimg-toggle-img-display'.
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

(defvar inlimg-img-regexp
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
               (group (1+ (not (any "\""))))
               ?\"
               ")"
               )
          )))

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

(defface inlimg-img-tag '((t :inherit 'lazy-highlight))
  "Face added to img tag when displaying image."
  :group 'inlimg)

(defface inlimg-img-remote '((t :inherit 'isearch-fail))
  "Face used for notes telling image is remote."
  :group 'inlimg)

(defface inlimg-img-missing '((t :inherit 'trailing-whitespace))
  "Face used for notes telling image is missing."
  :group 'inlimg)

(defun inlimg-next (pt display-image)
  "Display or hide next image after point PT.
If DISPLAY-IMAGE is non-nil then display image, otherwise hide it.

Return non-nil if an img tag was found."
  (let (src beg end end-str img ovl remote beg-face)
    (goto-char pt)
    (when (setq res (re-search-forward inlimg-img-regexp nil t))
      (setq src (or (match-string-no-properties 1)
                    (match-string-no-properties 2)))
      (setq beg (match-beginning 0))
      (setq end (match-end 0))
      (setq end-str (buffer-substring-no-properties (- end 2) end))
      (setq beg-face (get-text-property beg 'face))
      (setq remote (string-match "^https?://" src))
      (if display-image
          (unless (memq beg-face '(font-lock-comment-face font-lock-string-face))
            (setq ovl (make-overlay beg end))
            (overlay-put ovl 'inlimg-img t)
            (overlay-put ovl 'priority 100)
            (overlay-put ovl 'face 'inlimg-img-tag)
            (overlay-put ovl 'keymap inlimg-img-keymap)
            (overlay-put ovl 'image-file (expand-file-name src))
            (if (or remote (not (file-exists-p src)))
                (mumamo-with-buffer-prepared-for-jit-lock
                 (put-text-property (- end 2) (- end 1)
                                    'display
                                    end-str ;;"/>"
                                    )
                 (put-text-property (- end 1) end
                                    'display
                                    (propertize (if remote
                                                    " Image is on the web "
                                                  " Image not found ")
                                                'face
                                                (if remote
                                                    'inlimg-img-remote
                                                  ;;'custom-invalid
                                                  ;;'match
                                                  'inlimg-img-missing))))
              ;; Get src value before macro since buffer-file-name is nil inside.
              (setq src (expand-file-name
                         src
                         (file-name-directory (buffer-file-name))))
              (mumamo-with-buffer-prepared-for-jit-lock
               (put-text-property (- end 2) (- end 1)
                                  'display
                                  ;;"/>\n"
                                  (concat end-str "\n")
                                  )
               (setq img (create-image src nil nil
                                       :relief 5
                                       :margin inlimg-margins))
               (when inlimg-slice
                 (let* ((sizes (image-size img t))
                        (width  (car sizes))
                        (height (cdr sizes))
                        (sl-left (nth 0 inlimg-slice))
                        (sl-top (nth 1 inlimg-slice))
                        (sl-width (nth 2 inlimg-slice))
                        (sl-height (nth 3 inlimg-slice))
                        )
                   (when (> sl-left width) (setq sl-left 0))
                   (when (> (+ sl-left sl-width) width)
                     (setq sl-width (- width sl-left)))
                   (when (> sl-top height) (setq sl-top 0))
                   (when (> (+ sl-top sl-height) height)
                     (setq sl-height (- height sl-top)))
                   (setq img (list img))
                   (setq img (cons
                              (append '(slice)
                                      inlimg-slice
                                      (list sl-top sl-left sl-width sl-height)
                                      nil)
                              img))))
               (put-text-property (- end 1) end
                                  'display img))))
        (mumamo-with-buffer-prepared-for-jit-lock
         (put-text-property (- end 2) end
                            'display nil)))
      (mumamo-with-buffer-prepared-for-jit-lock
       (put-text-property (- end 2) end
                          'inlimg-display display-image)))
    res))

(defvar inlimg-img-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map [(control ?c) ?+] 'inlimg-toggle-img-display)
    map))

(eval-after-load 'gimp
  '(gimp-add-point-bindings inlimg-img-keymap))

(defvar inlimg-timer nil)
(make-variable-buffer-local 'inlimg-timer)
(put 'inlimg-timer 'permanent-local t)

(defun inlimg-cancel-timer ()
  "Cancel timer for displaying/hiding images."
  (when inlimg-timer
    (cancel-timer inlimg-timer)
    (setq inlimg-timer nil)))

(defun inlimg-request-update (start end)
  "Request update of images display between START and END."
  (inlimg-cancel-timer)
  (setq inlimg-timer
        (run-with-idle-timer idle-update-delay
                             nil
                             'inlimg-update-in-timer
                             start
                             end
                             (current-buffer))))

(defun inlimg-update-in-timer (start end buffer)
  "Update image display between START and END in buffer BUFFER."
  (with-current-buffer buffer
    (let ((here (point))
          res
          ovls
          prop-pos1
          prop-pos2)
      (save-restriction
        (widen)
        (goto-char start)
        (setq ovls (overlays-in start end))
        (dolist (ovl ovls)
          (when (overlay-get ovl 'inlimg-img)
            (delete-overlay ovl)))
        (goto-char start)
        (setq prop-pos1 start)
        (mumamo-with-buffer-prepared-for-jit-lock
         (while (setq prop-pos1
                      (next-single-property-change prop-pos1 'inlimg-display))
           (put-text-property prop-pos1 (+ 2 prop-pos1) 'inlimg-display nil)
           (put-text-property prop-pos1 (+ 2 prop-pos1) 'display nil)))
        (goto-char start)
        (setq res (save-match-data
                    (inlimg-next (point) inlimg-mode))))
      (when (and res
                 (< res end))
        (inlimg-request-update res end))
      (goto-char here))))

(defun inlimg-after-change (beg end pre-len)
  "Actions to take after a change in buffer.
This is put in `after-change-functions'.  For BEG, END and
PRE-LEN see that function."
  (let ((here (point)))
    (goto-char beg)
    (setq beg (line-beginning-position -2))
    (goto-char end)
    (setq end (line-end-position 3))
    (inlimg-request-update beg end)))

(defun inlimg-update-whole-buffer ()
  "Request update of image display in the current buffer."
  (save-restriction
    (widen)
    (inlimg-request-update (point-min) (point-max))))

(defun inlimg-update-all-buffers ()
  "Request update of image display in all buffers.
Update image display in all buffers where the option
`inlimg-mode' is on."
  (dolist (buff (buffer-list))
    (with-current-buffer buff
      (when inlimg-mode
        (inlimg-update-whole-buffer)))))

;;;###autoload
(define-minor-mode inlimg-mode
  "Display <img ...> images inline.
Images are displayed below the <img ...> tag using the margins in
`inlimg-margins'.  The whole image or a slice of it may be
displayed, see `inlimg-slice'.

See also the command `inlimg-toggle-img-display'."
  :keymap nil
  :group 'inlimg
  (if inlimg-mode
      (add-hook 'after-change-functions 'inlimg-after-change nil t)
    (remove-hook 'after-change-functions 'inlimg-after-change t))
  (inlimg-cancel-timer)
  (inlimg-update-whole-buffer))
(put 'inlimg-mode 'permanent-local t)

;;;###autoload
(defun inlimg-toggle-img-display (point)
  "Toggle display of img image at point POINT.
See also the command `inlimg-mode'."
  (interactive (list (point)))
  (save-match-data
    (let ((here (point))
          img-start
          img-end
          (ovls (overlays-at (point)))
          iovl
          )
      (dolist (ovl ovls)
        (when (overlay-get ovl 'inlimg-img)
          (setq iovl ovl)))
      (if iovl
          (progn
            (setq img-start (overlay-start iovl))
            (setq img-end (- (overlay-end iovl) 2)))
        (skip-chars-backward "^<")
        (unless (and (> (point) (point-min))
                     (= ?\< (char-before))
                     (progn
                       (backward-char)
                       (looking-at inlimg-img-regexp)))
          (goto-char here)
          (error "No image here"))
        (setq img-start (point))
        (setq img-end (- (match-end 0) 2)))
      (setq is-displayed (get-text-property img-end 'inlimg-display))
      (inlimg-next img-start (not is-displayed))
      (when (and iovl
                 (not inlimg-mode)
                 is-displayed)
        (delete-overlay iovl))
      (goto-char here))))


(provide 'inlimg)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; inlimg.el ends here
