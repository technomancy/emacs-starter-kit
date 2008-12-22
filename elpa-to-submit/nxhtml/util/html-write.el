;;; html-write.el --- Hide some tags for writing text in XHTML
;;
;; Author: Lennart Borgman (lennart O borgman A gmail O com)
;; Created: 2008-10-03T01:29:44+0200 Thu
(defconst html-write:version "0.5") ;; Version:
;; Last-Updated: 2008-10-04 Sat
;; URL:
;; Keywords:
;; Compatibility:
;;
;; Features that might be required by this library:
;;
;;   `backquote', `bytecomp', `cl', `flyspell', `ispell', `mumamo',
;;   `sgml-mode'.
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

(require 'mumamo) ;; Just for the defmacro ...
(require 'mlinks nil t)

(defgroup html-write nil
  "Customization group for html-write."
  :group 'nxhtml
  :group 'convenience)

(defface html-write-base
  '((t (:inherit font-lock-type-face)))
  "Face from which other faces inherits."
  :group 'html-write)

(defface html-write-em
  '((t (:inherit html-write-base :slant italic)))
  "Face used for <em> tags."
  :group 'html-write)

(defface html-write-strong
  '((t (:inherit html-write-base :weight bold)))
  "Face used for <strong> tags."
  :group 'html-write)

(defface html-write-link
  '((t (:inherit html-write-base :underline t)))
  "Face used for <a> tags."
  :group 'html-write)

(defconst html-write-tag-list
  '(("i"      html-write-em-tag-actions)
    ("b"      html-write-strong-tag-actions)
    ("em"     html-write-em-tag-actions)
    ("strong" html-write-strong-tag-actions)
    ("a"      html-write-a-tag-actions)
    ;;("img"    html-write-img-tag-actions t)
    )
  "List of tags that should be hidden.
A record in the list has the format

  \(TAG HANDLE [SINGLE])

where
- TAG is the tag name string.

- HANDLE is a function to call when hiding the tag. It takes
  three parameters, TAG-BEGIN, TAG-END and OVERLAY.  TAG-BEGIN
  and TAG-END are start and end of the start tag.  OVERLAY is an
  overlay used for faces, keymaps etc that covers the whole tag."
  )

(defun html-write-em-tag-actions (tag-begin tag-end overlay)
  "Do actions for <em> tags for tag between TAG-BEGIN and TAG-END.
OVERLAY is the overlay added by `html-write-hide-tags' for this tag."
  (overlay-put overlay 'face 'html-write-em))

(defun html-write-strong-tag-actions (tag-begin tag-end overlay)
  "Do actions for <strong> tags for tag between TAG-BEGIN and TAG-END.
OVERLAY is the overlay added by `html-write-hide-tags' for this tag."
  (overlay-put overlay 'face 'html-write-strong))

;; Fix-me
(defun html-write-img-tag-actions (tag-begin tag-end overlay)
  "Do actions for <img> tags for tag between TAG-BEGIN and TAG-END.
OVERLAY is the overlay added by `html-write-hide-tags' for this tag."
  (save-match-data
    (let ((here (point-marker))
          href)
      (save-restriction
        (narrow-to-region tag-begin tag-end)
        (goto-char tag-begin)
        (when (looking-at (rx (*? anything)
                              (1+ space)
                              "src=\""
                              (submatch
                               (+ (not (any "\"\n"))))
                              "\""))
          (setq href (match-string-no-properties 1))))
      (when href
        (overlay-put overlay 'display (concat "image " href))
        (overlay-put overlay 'html-write-url href))
      (goto-char (point)))))

(defun html-write-a-tag-actions (tag-begin tag-end overlay)
  "Do actions for <a> tags for tag between TAG-BEGIN and TAG-END.
OVERLAY is the overlay added by `html-write-hide-tags' for this tag."
  (save-match-data
    (let ((here (point-marker))
          href)
      (save-restriction
        (narrow-to-region tag-begin tag-end)
        (goto-char tag-begin)
        (when (looking-at (rx (*? anything)
                              (1+ space)
                              "href=\""
                              (submatch
                               (+ (not (any "\"\n"))))
                              "\""))
          (setq href (match-string-no-properties 1))))
      (when href
        (overlay-put overlay 'face 'html-write-link)
        (overlay-put overlay 'help-echo href)
        (overlay-put overlay 'mouse-face 'highlight)
        (if (eq ?# (string-to-char href))
            (setq href (concat "file:///" buffer-name href))
          (when (file-exists-p href)
            (setq href (expand-file-name href))))
        (overlay-put overlay 'html-write-url href))
      (goto-char (point)))))

(defun html-write-get-tag-ovl ()
  "Get tag overlay at current point."
  (catch 'ranges
    (dolist (ovl (overlays-at (point)))
      (let ((ranges (overlay-get ovl 'html-write)))
        (when ranges
          (throw 'ranges ovl))))))

(defun html-write-toggle-current-tag ()
  "Toggle display of tag at current point."
  (interactive)
  (let* ((ovl (html-write-get-tag-ovl))
         (hiding-ranges (overlay-get ovl 'html-write))
         (invis (get-text-property (caar hiding-ranges) 'invisible))
         (ovl-start (overlay-start ovl))
         (ovl-end (overlay-end ovl)))
    (if invis
        (progn
          (overlay-put ovl 'html-face (overlay-get ovl 'face))
          (overlay-put ovl 'face 'highlight)
          (dolist (range hiding-ranges)
            (let ((start (car range))
                  (end   (cdr range)))
              (mumamo-with-buffer-prepared-for-jit-lock
               (put-text-property start end 'invisible nil)))))
      (delete-overlay ovl)
      (html-write-hide-tags ovl-start ovl-end))))

(defun html-write-browse-link ()
  "Browse link in current tag."
  (interactive)
  (let* ((ovl (html-write-get-tag-ovl))
         (url (overlay-get ovl 'html-write-url)))
    (unless url
      (error "No link in this tag"))
    (browse-url url)
    ))

(defvar html-write-keymap
  (let ((map (make-sparse-keymap))
        keys)
    (define-key map [(control ?c) ?+] 'html-write-toggle-current-tag)
    (define-key map [(control ?c) ?!] 'html-write-browse-link)
    (define-key map [mouse-1] 'html-write-browse-link)
    (when (featurep 'mlinks)
      (setq keys (where-is-internal 'mlinks-goto mlinks-mode-map))
      (dolist (key keys)
        (define-key map key 'html-write-mlinks-goto))
      (setq keys (where-is-internal 'mlinks-goto-other-window mlinks-mode-map))
      (dolist (key keys)
        (define-key map key 'html-write-mlinks-goto-other-window))
      (setq keys (where-is-internal 'mlinks-goto-other-frame mlinks-mode-map))
      (dolist (key keys)
        (define-key map key 'html-write-mlinks-goto-other-frame))
      )
    map))

(defun html-write-mlinks-goto ()
  "Goto link."
  (interactive)
  (html-write-mlinks-goto-1 'mlinks-goto))

(defun html-write-mlinks-goto-other-window ()
  "Goto link in other window."
  (interactive)
  (html-write-mlinks-goto-1 'mlinks-goto-other-window))

(defun html-write-mlinks-goto-other-frame ()
  "Goto link in other frame."
  (interactive)
  (html-write-mlinks-goto-1 'mlinks-goto-other-frame))

(defun html-write-mlinks-goto-1 (goto-fun)
  (let* ((ovl (html-write-get-tag-ovl))
         (ovl-start (overlay-start ovl))
         (ovl-end (overlay-end ovl))
         (here (point-marker)))
    (goto-char ovl-start)
    (skip-chars-forward "^\"" ovl-end)
    (forward-char)
    (unless (funcall goto-fun) (goto-char here))
    ))

;;(html-write-make-hide-tags-regexp)
(defun html-write-make-hide-tags-regexp ()
  "Make regexp used for finding tags to hide."
  ;; fix-me: single tags
  (let ((tags-re
         (mapconcat 'identity
                    (mapcar (lambda (elt)
                              (if (stringp elt)
                                  elt
                                (car elt)))
                            html-write-tag-list)
                    "\\|")))
    (concat
     "<\\(?1:"
     "\\(?:" tags-re "\\)"
     "\\)[^>]*>\\(?3:[^<]*\\)\\(?2:</\\1>\\)"
     )))

(defvar html-write-pending-changes nil)
(make-variable-buffer-local 'html-write-pending-changes)
(put 'html-write-pending-changes 'permanent-local t)

(defun html-write-after-change (start end pre-len)
  "Function to put in `after-change-functions'.
See that variable for START, END and PRE-LEN."
  (add-to-list 'html-write-pending-changes
               ;; Add +1 for deletions (and don't worry about other
               ;; cases ...
               (cons (copy-marker start) ;; Stay before
                     (copy-marker end t) ;; Follow inserted text
                     )))
(put 'html-write-after-change 'permanent-hook t)

(defun html-write-post-command ()
  "Function for `post-command-hook'."
  (condition-case err
      (html-write-post-command-1)
    (error (message "html-write-post-command error: %s" err))))
(put 'html-write-post-command 'permanent-hook t)

(defun html-write-post-command-1 ()
  "Inner function for `html-write-post-command'."
  (save-restriction
    (widen)
    (dolist (pend html-write-pending-changes)
      (assert (markerp (car pend)))
      (assert (markerp (cdr pend))))
    (let ((pending html-write-pending-changes)
          pending2
          pend pend-next
          (here (point-marker))
          (min-ovl (point-max))
          (max-ovl (point-min))
          our-overlays
          our-visible-overlays
          (dbg nil))
      (setq html-write-pending-changes nil)
      (dolist (pend pending)
        (when (< (car pend) min-ovl)
          (setq min-ovl (car pend)))
        (when (> (cdr pend) max-ovl)
          (setq max-ovl (cdr pend))))
      ;; Get our overlays
      (when dbg (message "========================================"))
      (when dbg (message "Get our overlays"))
      (dolist (ovl (append (overlays-in min-ovl max-ovl)
                           (overlays-at min-ovl)
                           nil))
        (when (overlay-get ovl 'html-write)
          (if (invisible-p (overlay-start ovl))
              (add-to-list 'our-overlays ovl)
            (add-to-list 'our-visible-overlays ovl))))
      ;; Skip changes inside visible overlays
      (when dbg (message "Skip changes inside visible overlays"))
      (while pending
        (setq pend (car pending))
        (setq pending (cdr pending))
        (unless (catch 'vis
                  (dolist (ovl our-visible-overlays)
                    (and (<= (overlay-start ovl) (car pend))
                         (<= (cdr pend) (overlay-end ovl))
                         (throw 'vis t))))
          (setq pending2 (cons pend pending2))))
      ;; Extend to normal overlays
      (when dbg (message "Extend to normal overlays"))
      (dolist (pend pending2)
        (let ((pend-min (car pend))
              (pend-max (cdr pend)))
          (dolist (ovl our-overlays)
            (let ((ovl-min (overlay-start ovl))
                  (ovl-max (overlay-end ovl)))
              (when (and (> pend-min ovl-min)
                         (<= pend-min ovl-max))
                (setcar pend (copy-marker ovl-min)))
              (when (and (< pend-max ovl-max)
                         (>= pend-max ovl-min))
                (setcdr pend (copy-marker ovl-max)))
              ))))
      ;; Sort
      (when dbg (message "Sort"))
      (setq pending (sort pending2
                           (lambda (rec-a rec-b)
                             (if (= (car rec-a) (car rec-b))
                                 (< (cdr rec-a) (cdr rec-b))
                               (< (car rec-a) (car rec-b))))))
      ;; Extend end
      (when dbg (message "== Extend end"))
      (let ((high-end (point-min))
            end
            next-<c-pos
            this->-pos+1
            )
        (dolist (pend pending)
          (setq end (cdr pend))
          (if (> end high-end)
              ;; Look further
              (progn
                ;; Is last tested <C..> useful?
                (when dbg (message "Is last tested <C..> useful?"))
                (and next-<c-pos
                     (< next-<c-pos end)
                     (setq next-<c-pos nil))
                (unless next-<c-pos
                  (goto-char end)
                  (skip-chars-forward "^<>")
                  (if (eq ?< (char-after))
                      (progn
                        (forward-char)
                        (if (not (eq ?/ (char-after)))
                            ;; Start tag, we need not search after it.
                            (progn
                              (when dbg (message "Start tag, we need not search after it."))
                              (setq next-<c-pos (point)))
                          ;; End tag, must include it.
                          (when dbg (message "End tag, must include it."))
                          ;;(forward-char)
                          (skip-chars-forward "^>")
                          (when dbg (message "after skip ^>"))
                          (setq end (min (point-max) (1+ (point))))))
                    ;; Inside tag or unfinished tag
                    (when dbg (message "Inside tag or unfinished tag"))
                    (setq this->-pos+1 (min (point-max) (1+ (point))))
                    (skip-chars-backward "^<>")
                    (if (eq ?< (char-after (1- (point))))
                        ;; Inside finished tag
                        (when dbg (message "Inside finished tag"))
                        (if (eq ?/ (char-after))
                            ;; End tag
                            (progn
                              (when dbg (message "End tag"))
                              (setq end this->-pos+1))
                          ;; Start tag
                          (when dbg (message "Start tag 2"))
                          (unless (eobp) (forward-char))
                          (skip-chars-forward "^>")
                          (setq end (point)))
                      ;; Between > and >
                      (when dbg (message "Between > and >"))
                      (setq end this->-pos+1)
                      )))
                (setq high-end end)
                (setcdr pend high-end))
            ;; This ends after so we can extend it to high-end
            (when dbg (message "This ends after so we can extend it to high-end"))
            (setcdr pend high-end))))
      ;; Extend start
      (when dbg (message "== Extend start"))
      (setq pending (nreverse pending))
      (let ((low-start (point-max))
            start
            last-/>-pos
            this->-pos-1)
        (dolist (pend pending)
          (setq start (car pend))
          (if (< start low-start)
              ;; Look further
              (progn
                ;; Is last tested </...> useful?
                (when dbg (message "Is last tested </...> useful?"))
                (and last-/>-pos
                     (> last-/>-pos start)
                     (setq last-/>-pos nil))
                (unless last-/>-pos
                  (goto-char start)
                  (skip-chars-backward "^<>")
                  (if (eq ?< (char-after (1- (point))))
                      ;; Inside tag
                      (if (eq ?/ (char-after))
                          ;; Inside end tag, need to goto start tag
                          (progn
                            (when dbg (message "Inside end tag, need to goto start tag"))
                            (backward-char)
                            (skip-chars-backward "^<")
                            (unless (eq ?/ (char-after))
                              (setq start (max (point-min) (1- (point))))))
                        ;; Inside start tag
                        (when dbg (message "Inside start tag"))
                        (setq start (max (point-min) (1- (point)))))
                    ;; Outside tag, check tag before
                    (unless (bobp)
                      (when dbg (message "Outside tag, check tag before"))
                      (setq this->-pos-1 (point))
                      (backward-char)
                      (skip-chars-backward "^<")
                      (if (eq ?/ (char-after))
                          ;; Tag before is end tag, don't include
                          (progn
                            (when dbg (message "Tag before is end tag, don't include"))
                            nil)
                        ;; Tag before is start tag, include
                        (when dbg (message "Tag before is start tag, include"))
                        (setq start (max (point-min) (1- (point))))))))
                (setq low-start start)
                (setcar pend low-start)
                )
            ;; This starts before so we can extend it to low-start
            (when dbg (message "This starts before so we can extend it to low-start"))
            (setcar pend low-start))))

      ;; delete dublicates, merge
      (when dbg (message "== delete dublicates, merge"))
      (setq pending2 pending)
      (setq pending nil)
      (while pending2
        (setq pend2 (car pending2))
        (setq pending2 (cdr pending2))
          ;; The list is sorted
          (when dbg (message "The list is sorted"))
          (setq pend (car pending))
          (if (not pend)
              (setq pending (cons pend2 pending))
            (cond
             ((= (car pend2) (car pend))
              (when (> (cdr pend2) (cdr pend))
                (setcdr pend (cdr pend2))))
             ((<= (car pend2) (1+ (cdr pend)))
              (setcdr pend (cdr pend2)))
             ;; Probably never happens?
             ((equal pend2 pend) nil)
             (t
              (setq pending (cons pend2 pending)))
             )))
      (setq pending (reverse pending))
      (setq pending (assq-delete-all nil pending))
      (dolist (pend pending)
        (goto-char (car pend)) (goto-char (cdr pend))
        (html-write-reveal-tags (car pend) (cdr pend))
        (html-write-hide-tags (car pend) (cdr pend)))
      (setq html-write-pending-changes nil)
      (goto-char here))))

(defun html-write-hide-tags (start end)
  "Hide tags matching `html-write-tag-list' between START and END."
  (let ((here (point-marker))
        (buffer-name (buffer-file-name)))
    (save-restriction
      (widen)
      (goto-char start)
      (save-match-data
        (let ((hide-tags-regexp (html-write-make-hide-tags-regexp)))
          (while (re-search-forward hide-tags-regexp end t)
            (let* ((ovl (make-overlay (match-beginning 0) (match-end 0)
                                      nil t nil))
                   (tag-fun (cadr (assoc (match-string-no-properties 1)
                                         html-write-tag-list)))
                   hiding-ranges)
              (overlay-put ovl 'face 'font-lock-variable-name-face)
              (overlay-put ovl 'keymap html-write-keymap)
              (setq hiding-ranges
                    (list (cons (1- (match-beginning 1)) (match-beginning 3))
                          (cons (match-beginning 2) (match-end 2))))
              (overlay-put ovl 'html-write hiding-ranges)
              (mumamo-with-buffer-prepared-for-jit-lock
               (dolist (range hiding-ranges)
                 (let ((start (car range))
                       (end   (cdr range)))
                   (put-text-property start end 'invisible 'html-write)
                   ;; Fix-me: more careful rear-nonsticky?
                   (put-text-property (1- end) end
                                      'rear-nonsticky '(invisible)))))
              ;; Let tag-fun override
              (when tag-fun
                (funcall tag-fun (match-end 1) (match-beginning 3) ovl))
              )))))
    (goto-char here)))

(defun html-write-reveal-tags (start end)
  "Reveal tags between START and END."
  (let ((here (point-marker)))
    (save-restriction
      (widen)
      (goto-char (point-min))
      (save-match-data
        (mumamo-with-buffer-prepared-for-jit-lock
         (remove-text-properties start
                                 end
                                 '(invisible html-write))
         (dolist (ovl (overlays-in start end))
           (when (overlay-get ovl 'html-write)
             (let ((end (overlay-end ovl)))
               (remove-list-of-text-properties (1- end) end '(rear-nonsticky))
               (delete-overlay ovl)))))))
    (goto-char here)))

;;;###autoload
(define-minor-mode html-write-mode
  "Minor mode for convenient display of some HTML tags.
When this mode is on a tag in `html-write-tag-list' is displayed as
the inner text of the tag with a face corresponding to the tag.
By default for example <i>...</i> is displayed as italic and
<a>...</a> is displayed as an underlined clickable link.

Only non-nested tags are hidden.  The idea is just that it should
be easier to read and write, not that it should look as html
rendered text.

See the customization group `html-write' for more information about
faces.

IMPORTANT: Most commands you use works also on the text that is
hidden.  The movement commands is an exception, but as soon as
you edit the buffer you may also change the hidden parts."
  :group 'html-write
  (save-restriction
    (widen)
    (if html-write-mode
        (progn
          (setq html-write-pending-changes nil)
          (html-write-hide-tags (point-min) (point-max))
          (add-hook 'after-change-functions 'html-write-after-change nil t)
          (add-hook 'post-command-hook 'html-write-post-command nil t)
          (setq buffer-invisibility-spec
                (if (listp buffer-invisibility-spec)
                    (cons 'html-write buffer-invisibility-spec)
                  (list 'html-write buffer-invisibility-spec))))
      (html-write-reveal-tags (point-min) (point-max))
      (remove-hook 'after-change-functions 'html-write-after-change t)
      (remove-hook 'post-command-hook 'html-write-post-command t)
      (setq buffer-invisibility-spec
            (delq 'html-write buffer-invisibility-spec)))))
(put html-write-mode 'permanent-local t)


;;;; Visible point

;; I do not think this can be used. There is a built in feature for
;; this anyway (which works a little bit differently).

;; (defun visible-point-pre-command ()
;;   (condition-case err
;;       (visible-point-pre-command-1)
;;     (error (message "visible-point-pre-command error: %s" err))))
;; (put 'visible-point-pre-command 'permanent-hook t)

;; (defun visible-point-pre-command-1 ()
;;   ;; Fix-me: widen?
;;   (when visible-point-mode
;;     (setq visible-point-pre-point (point-marker))
;;     (remove-hook 'post-command-hook 'visible-point-post-command t)
;;     (add-hook 'post-command-hook 'visible-point-post-command t t))
;;   )

;; ;;(defvar visible-point-pre-column nil)
;; ;;(put 'visible-point-pre-column 'permanent-local t)
;; (defvar visible-point-pre-point nil)
;; (put 'visible-point-pre-point 'permanent-local t)
;; ;;(defvar visible-point-pre-line nil)
;; ;;(put 'visible-point-pre-line 'permanent-local t)

;; (defun visible-point-post-command ()
;;   (condition-case err
;;       (visible-point-post-command-1)
;;     (error (message "visible-point-post-command error: %s" err))))
;; (put 'visible-point-post-command 'permanent-hook t)

;; (defun visible-point-post-command-1 ()
;;   ;;(setq visible-point-pre-column (current-column))
;;   ;;(setq visible-point-pre-line (line-number-at-pos))
;;   ;; If in invisible portion move out
;;   (let* ((pre-point-line (line-number-at-pos visible-point-pre-point))
;;          (current-line (line-number-at-pos))
;;          (point-dir
;;           (cond ((> (point) visible-point-pre-point)
;;                  1)
;;                 ((<= (point) visible-point-pre-point)
;;                  -1)))
;;          (line-dir
;;           (cond ((> current-line pre-point-line)
;;                  1)
;;                 ((< current-line pre-point-line)
;;                  -1)))
;;          (prev-point -1)
;;          next-pos
;;          )
;;     (when (invisible-p (1- (point)))
;;       (message "\ninvis A %s, point-dir=%s, line-dir=%s" (point) point-dir line-dir)
;;       ;; First by point
;;       (when point-dir
;;         (while (invisible-p (point)) ;; -1 because sticky at the end
;;           (message "while invis B %s, %s" (point) prev-point)
;;           (when (eq (point) prev-point) (error "B: prev-point = (point)"))
;;           (setq prev-point (point))
;;           (cond ((=  1 point-dir)
;;                  (setq next-pos
;;                        (next-single-char-property-change (1+ (point)) 'invisible))
;;                  (message "next-pos=%s, point=%s" next-pos (point))
;;                  (goto-char next-pos)
;;                  )
;;                 ((= -1 point-dir)
;;                  (goto-char
;;                   (previous-single-char-property-change (1- (point)) 'invisible))
;;                  ;; Unlike the forward version this stops just before
;;                  ;; the change so we must go back one step.
;;                  (backward-char))
;;                 (t (error "point-dir=%s" point-dir)))))
;;       (message "invis C %s, %s" (point) prev-point)
;;       ;; By line
;;       (when (and line-dir
;;                  (= pre-point-line (line-number-at-pos)))
;;         (cond ((=  1 line-dir) (forward-line))
;;               ((= -1 line-dir) (forward-line -1))
;;               (t (error "line-dir=%s" line-dir))))
;;       (when (and point-dir line-dir)
;;         (setq prev-point -1)
;;         (while (invisible-p (1- (point)))
;;           (message "while invis D %s, %s" (point) prev-point)
;;           (when (eq (point) prev-point) (error "D: prev-point = (point)"))
;;           (setq prev-point (point))
;;           (cond ((=  1 point-dir)
;;                  (goto-char
;;                   (next-single-char-property-change (point) 'invisible)))
;;                 ((= -1 point-dir)
;;                  (goto-char
;;                   (previous-single-char-property-change (point) 'invisible)))
;;                 (t (error "point-dir=%s" point-dir)))))
;;       (message "invis E %s, %s" (point) prev-point)
;;       (when (invisible-p (1- (point)))
;;         (error "point invisible at exit: %s" (point)))
;;       )))

;; ;;;###autoload
;; (define-minor-mode visible-point-mode
;;   "doc"
;;   :group 'nxhtml
;;   (if visible-point-mode
;;       (progn
;;         (add-hook 'pre-command-hook 'visible-point-pre-command nil t)
;;         (add-hook 'post-command-hook 'visible-point-post-command nil t)
;;         )
;;     (remove-hook 'pre-command-hook 'visible-point-pre-command t)
;;     (remove-hook 'post-command-hook 'visible-point-post-command t)
;;     ))

(provide 'html-write)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; html-write.el ends here
