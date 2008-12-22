;;; mlinks.el --- Minor mode making major mode dependent links
;;
;; Author: Lennar Borgman
;; Created: Tue Jan 16 22:17:34 2007
(defconst mlinks:version "0.28") ;;Version:
;; Lxast-Updated: Mon Apr 09 14:31:03 2007 (7200 +0200)
;; Keywords:
;; Compatibility:
;;
;; Fxeatures that might be required by this library:
;;
;;   `appmenu', `cl', `mail-prsvr', `mm-util', `ourcomments-util',
;;   `url-expand', `url-methods', `url-parse', `url-util',
;;   `url-vars'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; This file implements the minor mode `mlinks-mode' that create
;; hyperlinks for different major modes.
;;
;; To-do: Underline all links?
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change log:
;;
;; FIX-ME: url-hexify-string etc
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

(eval-when-compile (require 'cl))
(require 'ourcomments-util)
(require 'url-parse)
(require 'url-expand)
(require 'appmenu nil t)

(defgroup mlinks-mode nil
  "Customization group for `mlinks-mode'."
  :group 'nxhtml
  :group 'hypermedia)

;;(customize-option mlinks-mode-functions)
(defcustom mlinks-mode-functions
  '(
    ;; For message buffer etc.
    (fundamental-mode
     ((goto mlinks-elisp-goto)
      (hili mlinks-elisp-hili)
      hion
      )
     )
    (emacs-lisp-mode
     ((goto mlinks-elisp-goto)
      (hili mlinks-elisp-hili)
      hion
      )
     )
    ;; *scractch*
    (lisp-interaction-mode
     ((goto mlinks-elisp-goto)
      (hili mlinks-elisp-hili)
      hion
      )
     )
    (help-mode
     ((goto mlinks-elisp-goto)
      (hili mlinks-elisp-hili)
      hion
      )
     )
    (Info-mode
     ((goto mlinks-elisp-goto)
      (hili mlinks-elisp-hili)
      hion
      )
     )
    (custom-mode
     ((goto mlinks-elisp-custom-goto)
      (hili mlinks-elisp-hili)
      hion
      (next-mark mlinks-custom-next-mark)
      )
     )
    (text-mode
     ((goto mlinks-html-style-goto)
      (hili mlinks-html-style-hili)
      hion
      (next mlinks-html-forward-link)
      (prev mlinks-html-backward-link)
      )
     )
    (nxhtml-mode
     ((goto mlinks-html-style-goto)
      (hili mlinks-html-style-hili)
      hion
      (next mlinks-html-forward-link)
      (prev mlinks-html-backward-link)
      )
     )
    (nxml-mode
     ((goto mlinks-html-style-goto)
      (hili mlinks-html-style-hili)
      hion
      (next mlinks-html-forward-link)
      (prev mlinks-html-backward-link)
      )
     )
    (sgml-mode
     ((goto mlinks-html-style-goto)
      (hili mlinks-html-style-hili)
      hion
      (next mlinks-html-forward-link)
      (prev mlinks-html-backward-link)
      )
     )
;; This is an alias for sgml-mode:
;;     (xml-mode
;;      ((goto mlinks-html-style-goto)
;;       (hili mlinks-html-style-hili)
;;       hion
;;       (next mlinks-html-forward-link)
;;       (prev mlinks-html-backward-link)
;;       )
;;      )
    (html-mode
     ((goto mlinks-html-style-goto)
      (hili mlinks-html-style-hili)
      hion
      (next mlinks-html-forward-link)
      (prev mlinks-html-backward-link)
      )
     )
    )
  "Defines MLinks hyperlinks for major modes.
"
;; Each element in the list is a list with two elements

;;   \(MAJOR-MODE SETTINGS)

;; where MAJOR-MODE is the major mode for which the settings SETTINGS should be used.
;; SETTINGS is an association list which can have the following element types

;;   \(hili HILIGHT-FUN)  ;; Mandatory
;;   \(goto GOTO-FUN)     ;; Mandatory
;;   \(hion HION-BOOL)    ;; Optional
;;   \(next NEXT-FUN)     ;; Optional
;;   \(prev PREV-FUN)     ;; Optional

;; Where
;; - HILIGHT-FUN is the function to hilight a link when point is
;;   inside the link. This is done when Emacs is idle.
;; - GOTO-FUN is the function to follow the link at point.
;; - HION-BOOL is t or nil depending on if hilighting should be on
;;   by default.
;; - NEXT-FUN is the function to go to the next link.
;; - PREV-FUN is the function to go to the previous link."
;;   ;;:type '(repeat (alist :key-type symbol :value-type (alist :key-type symbol :value symbol)))
  :type '(alist :key-type major-mode-function
                :value-type (list
                             (set
                              (const :tag "Enable MLinks in this major mode" hion)
                              (const :tag "Mark All Links" mark)
                              (list :tag "Enable" (const :tag "Hilighting" hili) function)
                              (list :tag "Enable" (const :tag "Follow Link" goto) function)
                              (list :tag "Enable" (const :tag "Goto Next Link" next) function)
                              (list :tag "Enable" (const :tag "Goto Previous Link" prev) function)
                              )))
  :group 'mlinks-mode)

;; (defcustom temp
;;   nil
;;   "Defines MLinks hyperlinks for major modes.
;; "
;;   :type '(alist :key-type major-mode-function
;;                 :value-type (list
;;                              (set
;;                               (const :tag "MLinks on"
;;                                      :help-echo "Wether to enable MLinks in this major mode"
;;                                      hion)
;;                               (list :tag "Use" (const :tag "Hilighting" hili) function)
;;                               (function :tag "Forward")
;;                               )))
;;   :group 'mlinks-mode)

;; nxhtml-mode-hook

(defvar mlinks-mode-map
  (let ((m (make-sparse-keymap)))
    (define-key m [(control ?c) ?\r ?\r]   'mlinks-goto)
    (define-key m [(control ?c) ?\r ?w]    'mlinks-goto-other-window)
    (define-key m [(control ?c) ?\r ?f]    'mlinks-goto-other-frame)
    (define-key m [(control ?c) ?\r ?n]    'mlinks-next-saved-position)
    (define-key m [(control ?c) ?\r ?p]    'mlinks-prev-saved-position)
    (define-key m [(control ?c) ?\r S-tab] 'mlinks-backward-link)
    (define-key m [(control ?c) ?\r tab]   'mlinks-forward-link)
    (define-key m [(control ?c) ?\r ?h]    'mlinks-toggle-hilight)
    m))

(defun mlinks-want-marked-links ()
  (or (mlinks-get-action 'next)
      (mlinks-get-action 'next-mark)))

(defun mlinks-after-change-major-mode ()
  (let ((hion (when (mlinks-get-boolean 'hion) t)))
    (setq mlinks-hilight-this-buffer hion)
    (when (mlinks-want-marked-links)
      (message "mlinks-after-change-major-mode")
      (mlinks-start-marking-links)
      (add-hook 'after-change-functions 'mlinks-after-change t nil))
    ))


(defvar mlinks-hilight-this-buffer nil)
(make-variable-buffer-local 'mlinks-hilight-this-buffer)

(defvar mlinks-hilight-point-ovl nil)
(make-variable-buffer-local 'mlinks-hilight-point-ovl)

(defvar mlinks-hilighter-timer nil)
(make-variable-buffer-local 'mlinks-hilighter-timer)
(put 'mlinks-hilighter-timer 'permanent-local t)

(defun mlinks-toggle-hilight ()
  "Toggle hilighting of links in current buffer."
  (interactive)
  (setq mlinks-hilight-this-buffer (not mlinks-hilight-this-buffer))
  (if mlinks-hilight-this-buffer
      (message "MLinks hilighter was turned on in buffer")
    (message "MLinks hilighter was turned off in buffer")))

(defun mlinks-stop-hilighter ()
  ;;(message "stop-hilighter, mlinks-hilighter-timer=%s, timerp=%s" mlinks-hilighter-timer (timerp mlinks-hilighter-timer))
  (when (and mlinks-hilighter-timer
             (timerp mlinks-hilighter-timer))
    (cancel-timer mlinks-hilighter-timer))
  (setq mlinks-hilighter-timer nil)
  (when mlinks-hilight-point-ovl
    (delete-overlay mlinks-hilight-point-ovl)))

(defun mlinks-start-hilighter ()
  (mlinks-stop-hilighter)
  ;;(message "start-hilighter")
  (setq mlinks-hilighter-timer (run-with-idle-timer 0 t 'mlinks-hilighter (current-buffer)))
  )

(defun mlinks-make-point-ovl (bounds)
  (unless mlinks-hilight-point-ovl
    (setq mlinks-hilight-point-ovl
          (make-overlay (car bounds) (cdr bounds)))
    (overlay-put mlinks-hilight-point-ovl 'priority 100)
    (overlay-put mlinks-hilight-point-ovl 'mouse-face 'highlight)
    (mlinks-deactivate-hilight)
    ;;(overlay-put mlinks-hilight-point-ovl 'face 'highlight)
    ;;(overlay-put mlinks-hilight-point-ovl 'mouse-face 'highlight)
    ))

(defun mlinks-link-at-point ()
  (let* ((funs (mlinks-get-action 'hili))
         bounds)
    (when funs
      (setq bounds (run-hook-with-args-until-success 'funs)))
    (when bounds
      (buffer-substring-no-properties (car bounds) (cdr bounds)))))

(defun mlinks-hilighter (buffer)
  ;;(message "mlinks-hilighter, buffer=%s, p=%s, live-p=%s" buffer (bufferp buffer) (buffer-live-p buffer))
  (if (or (not (bufferp buffer))
          (not (buffer-live-p buffer)))
      ;;(mlinks-stop-hilighter)
      (cancel-timer timer-event-last)
    (with-current-buffer buffer
      (when mlinks-mode ;t ;mlinks-hilight-this-buffer
        (let* ((funs-- (mlinks-get-action 'hili))
               bounds--)
          (when funs--
            (setq bounds-- (run-hook-with-args-until-success 'funs--)))
          (if bounds--
              (if mlinks-hilight-point-ovl
                  (move-overlay mlinks-hilight-point-ovl (car bounds--) (cdr bounds--))
                (mlinks-make-point-ovl bounds--))
            (when mlinks-hilight-point-ovl
              (delete-overlay mlinks-hilight-point-ovl))))))))

(defvar mlinks-active-hilight-keymap
  (let ((m (make-sparse-keymap)))
    (define-key m [S-tab]   'mlinks-backward-link)
    (define-key m [tab]     'mlinks-forward-link)
    (define-key m "\t"      'mlinks-forward-link)
    (define-key m [?\r]     'mlinks-goto)
    (define-key m [?w]      'mlinks-goto-other-window)
    (define-key m [?f]      'mlinks-goto-other-frame)
    (define-key m [mouse-1] 'mlinks-goto)
    m))

(defvar mlinks-inactive-hilight-keymap
  (let ((m (make-sparse-keymap)))
    (define-key m [mouse-1] 'mlinks-goto)
    m))

(defun mlinks-pre-command ()
  (unless (let ((map (overlay-get mlinks-hilight-point-ovl 'keymap)))
            (where-is-internal this-command
                               (list
                                map)))
    (mlinks-deactivate-hilight)
    (unless mlinks-hilighter-timer
      (delete-overlay mlinks-hilight-point-ovl))))
(put 'mlinks-pre-command 'permanent-local t)

(defun mlinks-activate-hilight ()
  (add-hook 'pre-command-hook 'mlinks-pre-command nil t)
  (mlinks-hilighter (current-buffer))
  (overlay-put mlinks-hilight-point-ovl 'face 'isearch)
  (overlay-put mlinks-hilight-point-ovl 'keymap mlinks-active-hilight-keymap))

(defun mlinks-deactivate-hilight ()
  (remove-hook 'pre-command-hook 'mlinks-pre-command t)
  (mlinks-hilighter (current-buffer))
  (overlay-put mlinks-hilight-point-ovl 'face 'highlight)
  (overlay-put mlinks-hilight-point-ovl 'keymap mlinks-inactive-hilight-keymap))

(defun mlinks-someactivate-hilight ()
  (if mlinks-active-links
      (mlinks-activate-hilight)
    (mlinks-deactivate-hilight)))

(defun mlinks-backward-link ()
  "Find previous `mlinks-mode' link in buffer."
  (interactive)
  (let ((funs (mlinks-get-action 'prev)))
    (if (not funs)
        (message "There is no way to go to previous link for this major mode")
      (let (
            ;;(res (run-hook-with-args-until-success 'funs))
            (res (funcall (car funs)))
            )
        (if res
            (progn
              (goto-char (car res))
              (mlinks-someactivate-hilight))
          (message "No previous link found"))))))

(defun mlinks-forward-link ()
  "Find next `mlinks-mode' link in buffer."
  (interactive)
  (let ((funs (mlinks-get-action 'next)))
    (if (not funs)
        (message "There is no way to go to next link for this major mode")
      (let ((res (funcall (car funs))))
        (if res
            (progn
              (goto-char (car res))
              (mlinks-someactivate-hilight))
          (message "No next link found"))))))


(defun mlinks-goto ()
  "Follow `mlinks-mode' link at current point.
Save the current position so that they can be move to again by
`mlinks-prev-saved-position' and `mlinks-next-saved-position'.

Return non-nil if link was followed, otherewise nil."
  (interactive)
  (mlinks-goto-1 nil))

(defun mlinks-goto-other-window ()
  "Like `mlinks-goto' but opens in other window.
Uses `switch-to-buffer-other-window'."
  (interactive)
  (mlinks-goto-1 'other-window))

(defun mlinks-goto-other-frame ()
  "Like `mlinks-goto' but opens in other frame.
Uses `switch-to-buffer-other-frame'."
  (interactive)
  (mlinks-goto-1 'other-frame))

(defun mlinks-goto-1(where)
  (push-mark)
  (let* ((funs (mlinks-get-action 'goto))
         (old (point-marker))
         (mlinks-temp-buffer-where where)
         (res (run-hook-with-args-until-success 'funs)))
    (if (not res)
        (progn
          (message "No MLink link here")
          nil)
      (unless (= old (point-marker))
        (let* ((prev (car mlinks-places)))
          (when (or (not prev)
                    ;;(not (markerp prev))
                    (not (marker-buffer prev))
                    (/= old prev))
            (setq mlinks-places (cons old mlinks-places))
            (setq mlinks-places-n (length mlinks-places))))))))

(defun mlinks-get-boolean (which)
  (let ((mode-rec (assoc major-mode mlinks-mode-functions)))
    (when mode-rec
      (let* ((mode (car mode-rec))
             (funs-alist (cadr mode-rec)))
        (member which funs-alist)))))

(defun mlinks-get-action (which)
  (let ((mode-rec (assoc major-mode mlinks-mode-functions)))
    (when mode-rec
      (let* ((mode (car mode-rec))
             (funs-alist (cadr mode-rec))
             (funs (assoc which funs-alist)))
        (cdr funs)))))


(defun mlinks-prev-saved-position ()
  "Go to previous position saved by `mlinks-goto'."
  (interactive)
  (unless (mlinks-goto-n (1- mlinks-places-n))
    (message "No previous MLink position")))

(defun mlinks-next-saved-position ()
  "Go to next position saved by `mlinks-goto'."
  (interactive)
  (unless (mlinks-goto-n (1+ mlinks-places-n))
    (message "No next MLink position")))

(defun mlinks-goto-n (to)
  (if (not mlinks-places)
      (message "No saved MLinks positions")
    (let ((minp 1)
          (maxp (length mlinks-places)))
      (if (<= to minp)
          (progn
            (setq to minp)
            (message "Going to first MLinks position"))
        (if (>= to maxp)
            (progn
              (setq to maxp)
              (message "Going to last MLinks position"))))
      (setq mlinks-places-n to)
      (let ((n (- maxp to))
            (places mlinks-places)
            place
            buffer
            point)
        (while (> n 0)
          (setq places (cdr places))
          (setq n (1- n)))
        (setq place (car places))
        (mlinks-switch-to-buffer (marker-buffer place))
        (goto-char place)))))

(defvar mlinks-places-n 0)
(defvar mlinks-places nil)

(defvar mlinks-temp-buffer-where nil)
(defun mlinks-switch-to-buffer (buffer)
  (mlinks-switch-to-buffer-1 buffer mlinks-temp-buffer-where))

(defun mlinks-switch-to-buffer-1(buffer where)
  (cond
   ((null where)
    (switch-to-buffer buffer))
   ((eq where 'other-window)
    (switch-to-buffer-other-window buffer))
   ((eq where 'other-frame)
    (switch-to-buffer-other-frame buffer))
   (t
    (error "Invalid argument, where=%s" where))))

;; FIXME: face, var
(defun mlinks-custom (var)
  (customize-option var)
  )

(defun mlinks-appmenu ()
  (let ((link-val (mlinks-link-at-point))
        (map (make-sparse-keymap "mlinks-appmenu")))
    (when (mlinks-get-action 'prev)
      (define-key map [mlinks-next-link]
        (list 'menu-item "Next Link" 'mlinks-forward-link)))
    (when (mlinks-get-action 'next)
      (define-key map [mlinks-prev-link]
        (list 'menu-item "Previous Link" 'mlinks-backward-link)))
    (when link-val
      (let* ((possible (when (member major-mode '(html-mode nxhtml-mode nxml-mode sqml-mode text-mode))
                         (mlinks-html-possible-href-actions link-val)))
             (mailto (assoc 'mailto possible))
             (view-web (assoc 'view-web possible))
             (view-web-base (assoc 'view-web-base possible))
             (edit (assoc 'edit possible))
             (file (nth 1 edit))
             (anchor (nth 2 edit))
             (choices)
             (answer)
             )
        (define-key map [mlinks-href-sep] (list 'menu-item "--"))
        (when view-web
          (define-key map [mlinks-href-view-web]
            (list 'menu-item "Browse Link Web Url"
                  `(lambda () (interactive)
                     (browse-url ,link-val)))))
        (when view-web-base
          (define-key map [mlinks-href-view-web-based]
            (list 'menu-item "Browse Link Web Url (base URL found)"
                  `(lambda () (interactive)
                     (browse-url (cdr ,view-web-base))))))
        (when mailto
          (define-key map [mlinks-href-mail]
            (list 'menu-item (concat "&Mail to " (substring link-val 7))
                  `(lambda () (interactive)
                     (mlinks-html-mail-to ,link-val)))))
        (when edit
          (when (and (file-exists-p file)
                     (not anchor)
                     (assoc 'upload possible))
            (let ((abs-file (expand-file-name file)))
              (define-key map [mlinks-href-upload]
                (list 'menu-item "Upload Linked File"
                      `(lambda () (interactive)
                         (html-upl-upload-file ,abs-file))))))
          (when (and (file-exists-p file)
                     (not anchor)
                     (assoc 'edit-gimp possible))
            (let ((abs-file (expand-file-name file)))
              (define-key map [mlinks-href-edit-gimp]
                (list 'menu-item "Edit Linked File with GIMP"
                      `(lambda () (interactive)
                         (gimp-edit-file ,abs-file))))))
          (when (and (file-exists-p file)
                     (assoc 'view-local possible))
            (let ((url (concat "file:///" (expand-file-name file))))
              (when anchor
                (let ((url-anchor (concat url "#" anchor)))
                  (define-key map [mlinks-href-view-file-at]
                    (list 'menu-item (concat "Browse Linked File URL at #" anchor)
                          `(lambda () (interactive)
                             (browse-url ,url-anchor))))))
              (define-key map [mlinks-href-view-file]
                (list 'menu-item "&Browse Linked File URL"
                      `(lambda () (interactive)
                         (browse-url ,url))))))
          (define-key map [mlinks-href-sep-2] (list 'menu-item "--"))
          (unless (equal file (buffer-file-name))
            (define-key map [mlinks-href-edit]
              (list 'menu-item "&Open Linked File"
                    `(lambda () (interactive) (mlinks-goto))))
            (define-key map [mlinks-href-edit-window]
              (list 'menu-item "&Open Linked File in Other Window"
                    `(lambda () (interactive) (mlinks-goto-other-window))))
            (define-key map [mlinks-href-edit-frame]
              (list 'menu-item "&Open Linked File in New Frame"
                    `(lambda () (interactive) (mlinks-goto-other-frame))))
            )
          (when (and (file-exists-p file) anchor)
            (define-key map [mlinks-href-edit-at]
              (list 'menu-item (concat "Open Linked File &at #" anchor)
                    `(lambda () (interactive)
                       (mlinks-goto)))))
          )
        (define-key map [mlinks-href-sep-1] (list 'menu-item "--"))
        (define-key map [mlinks-href-copy-link]
          (list 'menu-item "&Copy Link"
                `(lambda () (interactive)
                   (x-select-text ,link-val))))))
    (when (> (length map) 2)
      map)))

(defun mlinks-add-appmenu ()
  "Add entries for MLinks to AppMenu."
  (when (featurep 'appmenu)
    ;;(add-to-list 'appmenu-alist (cons 'mlinks-mode (cons "Current MLink" 'mlinks-appmenu)))
    (appmenu-add 'mlinks 100 'mlinks-mode "Current MLink" 'mlinks-appmenu)
    ))

(defun mlinks-remove-overlays (&optional min max)
  (save-excursion
    (save-restriction
      (widen)
      (unless min (setq min (point-min)))
      (unless max (setq max (point-max)))
      (dolist (o (overlays-in min max))
        (when (overlay-get o 'mlinks)
          (when (< (overlay-start o) min)
            (setq min (overlay-start o)))
          (when (> (overlay-end o) max)
            (setq max (overlay-end o)))
          (delete-overlay o)))))
  (cons min max))

;;;###autoload
(define-minor-mode mlinks-mode
  "Recognizes certain parts of a buffer as hyperlinks.
The hyperlinks are created in different ways for different major
modes with the help of the functions in the list
`mlinks-mode-functions'.

The hyperlinks can be hilighted when point is over them.  Use
`mlinks-toggle-hilight' to toggle this feature for the current
buffer.

All keybindings in this mode are by default done under the prefi§x
key

  C-c RET

which is supposed to be a kind of mnemonic for link (alluding to
the RET key commonly used in web browser to follow a link).
\(Unfortunately this breaks the rules in info node `Key Binding
Conventions'.) Below are the key bindings defined by this mode:

\\{mlinks-mode-map}

For some major modes `mlinks-backward-link' and
`mlinks-forward-link' will take you to the previous/next link.
By default the link moved to will be active, see
`mlinks-active-links'.

"
  nil
  " L"
  nil
  :keymap mlinks-mode-map
  :group 'mlinks
  (if mlinks-mode
      (progn
        ;;(message "mlinks-mode t")
        (mlinks-add-appmenu)
        (mlinks-start-hilighter)
        ;;(message "define-minor mlinks")
        (mlinks-start-marking-links)
        ;;(add-hook 'after-change-major-mode-hook 'mlinks-after-change-major-mode)
        ;;(mlinks-add-overlays)
        )
    ;;(message "mlinks-mode nil")
    (mlinks-stop-hilighter)
    (mlinks-stop-marking-links)
    ;;(remove-hook 'after-change-major-mode-hook 'mlinks-after-change-major-mode)
    (mlinks-remove-overlays)))
(put 'mlinks-mode 'permanent-local t)

(defun mlinks-turn-on-in-buffer ()
  (let ((hion (unless (and (boundp 'mumamo-set-major-running)
                           mumamo-set-major-running)
                (when (mlinks-get-boolean 'hion) t))))
    ;;(message "mtoinb: buffer=%s, hion=%s, mlinks-mode=%s" (current-buffer) hion mlinks-mode)
    (when hion
      (mlinks-mode 1)
      )))

(define-globalized-minor-mode mlinks-global-mode mlinks-mode
  mlinks-turn-on-in-buffer
  :group 'mlinks-mode)
;; The problem with global minor modes:
(when (and mlinks-global-mode
           (not (boundp 'define-global-minor-mode-bug)))
  (mlinks-global-mode 1))

(define-toggle mlinks-active-links t
  "Use quick movement keys on active links if non-nil.
When moving to an mlink with `mlinks-forward-link' or
`mlinks-backward-link' the link moved to will be in an active
state.  This is marked with a new color \(the face `isearch').
When the new color is shown the following keys are active

\\{mlinks-active-hilight-keymap}
Any command cancels this state."
  ;;:tag "mlinks quick keys"
  :group 'mlinks)

(defface mlinks-link
  ;;'((t :underline t :foreground "blue1"))
  '((t :inherit link))
  "Default face for MLinks' links."
  :group 'mlinks)

;; (defface mlinks-nomarking
;;   '(())
;;   "Nomarking face for MLinks' links."
;;   :group 'mlinks)

(defcustom mlinks-link 'mlinks-link
  "Marking of MLinks links."
  :type '(choice (const :tag "No marking at all" nil) face)
  :group 'mlinks)

(defun mlinks-mark-link (lnk)
  (let* ((start (car lnk))
         (end   (cdr lnk))
         ovl)
    (dolist (o (overlays-at (point)))
      (when (overlay-get o 'mlink)
        (if ovl
            (delete-overlay o)
        (setq ovl o))))
    (if ovl
        (unless (and (= start (overlay-start ovl))
                     (= end   (overlay-end   ovl)))
          (move-overlay ovl start end))
      (setq ovl (make-overlay start end)))
    (when mlinks-link
      (overlay-put ovl 'face mlinks-link))
    (overlay-put ovl 'mouse-face 'highlight)
    (overlay-put ovl 'keymap mlinks-inactive-hilight-keymap)
    (overlay-put ovl 'mlinks t)))

(defvar mlinks-mark-links-timer nil)
(make-variable-buffer-local 'mlinks-mark-links-timer)
(put 'mlinks-mark-links-timer 'permanent-local t)

(defun mlinks-mark-next-link (buffer)
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when (timerp mlinks-mark-links-timer)
        (cancel-timer mlinks-mark-links-timer))
      (let ((funs (mlinks-get-action 'next))
            res
            start-at)
        (unless funs
          (setq funs (mlinks-get-action 'next-mark)))
        ;; When using mumamo-mode we can not be sure that funs is
        ;; non-nil here:
        (when funs
          (save-excursion
            (setq start-at
                  (if mlinks-link-update-pos-min
                      mlinks-link-update-pos-min
                    (point-min)))
            (goto-char start-at)
            (save-match-data
              (setq res (funcall (car funs)))))
          ;; Fix-me: old links
          (when res
            (let ((ovl-chg start-at)
                  (end-at (cdr res)))
              (while (< ovl-chg end-at)
                (dolist (o (overlays-at ovl-chg))
                  (when (overlay-get o 'mlink)
                    (if (= (overlay-start o) (car res))
                        (delete-overlay o)
                      (move-overlay o (car res) (cdr res)))))
                (setq ovl-chg (next-overlay-change (+ ovl-chg)))))
            (setq mlinks-link-update-pos-min (cdr res))
            (mlinks-mark-link res)
            (when (or (not mlinks-link-update-pos-max)
                      (< (point) mlinks-link-update-pos-max))
              (setq mlinks-mark-links-timer (run-with-idle-timer 0 nil 'mlinks-mark-next-link buffer))
              )))))))

(defvar mlinks-link-update-pos-min nil)
(make-variable-buffer-local 'mlinks-link-update-pos-min)
(put 'mlinks-link-update-pos-min 'permanent-local t)

(defvar mlinks-link-update-pos-max nil)
(make-variable-buffer-local 'mlinks-link-update-pos-max)
(put 'mlinks-link-update-pos-max 'permanent-local t)

(defun mlinks-stop-marking-links ()
  (when (timerp mlinks-mark-links-timer)
    (cancel-timer mlinks-mark-links-timer)))

(defun mlinks-start-marking-links ()
  (when (mlinks-want-marked-links)
    ;;(message "start-marking-links, buffer=%s" (current-buffer))
    (mlinks-stop-marking-links)
    (setq mlinks-link-update-pos-min nil)
    (setq mlinks-link-update-pos-max nil)
    (setq mlinks-mark-links-timer (run-with-idle-timer 0 nil 'mlinks-mark-next-link (current-buffer))))
  )

;; Fix-me: old links, range handling?
(defvar mlinks-after-change-extra 100)

(defun mlinks-after-change (beg end len)
  (let ((funs (mlinks-get-action 'next)))
    (unless funs
      (setq funs (mlinks-get-action 'next-mark)))
    (when funs
      (setq mlinks-link-update-pos-min (- beg mlinks-after-change-extra))
      (setq mlinks-link-update-pos-max (+ end mlinks-after-change-extra))
      (let* ((range (mlinks-remove-overlays mlinks-link-update-pos-min mlinks-link-update-pos-max))
             (min (car range))
             (max (cdr range)))
        (when (< min mlinks-link-update-pos-min) (setq mlinks-link-update-pos-min (- min mlinks-after-change-extra)))
        (when (> max mlinks-link-update-pos-max) (setq mlinks-link-update-pos-max (+ max mlinks-after-change-extra))))
      (mlinks-mark-next-link (current-buffer)))))
(put 'mlinks-after-change 'permanent-local t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;; nxhtml-mode ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mlinks-html-style-goto ()
  (mlinks-html-style-mode-fun t))

(defun mlinks-html-style-hili ()
  (mlinks-html-style-mode-fun nil))

;; Fix-me: All must be on one line now. Can that be changed?
;; This is where Shaowei Dai got
;;   Debugger entered--Lisp error: (invalid-read-syntax "] in a list")
(require 'rx)
(defvar mlinks-html-link-regex
  ;; This value takes care of nxhtml-strval-mode (and is therefore a little bit incorrect ...)
  ;;"\\(?:^\\|[[:space:]]\\)\\(?:href\\|src\\)[[:space:]]*=[[:space:]]*\"\\([^<«\"]*\\)\""
  (rx (or "^" space)
      (or "href" "src")
      (0+ space)
      "="
      (0+ space)
      (or
        (seq "\""
             (submatch
              (0+ (not (any "\""))))
             "\"")
        (seq "'"
             (submatch
              (0+ (not (any "'"))))
             "'"))))

;;(require 'rx)
;;(rx

(defun mlinks-html-forward-link (&optional from)
  (when (if from
            (save-excursion
              (goto-char from)
              (re-search-forward mlinks-html-link-regex nil t))
          (re-search-forward mlinks-html-link-regex nil t))
    ;;(message "mlinks-html-link-regex match-string0=%s, 1=%s, 2=%s" (match-string-no-properties 0) (match-string-no-properties 1) (match-string-no-properties 2))
    (let ((which (if (match-beginning 1) 1 2)))
      (cons (match-beginning which) (match-end which)))))

(defun mlinks-html-backward-link (&optional from)
  (when (if from
            (save-excursion
              (goto-char from)
              (re-search-backward mlinks-html-link-regex nil t))
          (re-search-backward mlinks-html-link-regex nil t))
    ;;(cons (match-beginning 1) (match-end 1))))
    (let ((which (if (match-beginning 1) 1 2)))
      (cons (match-beginning which) (match-end which)))))

(defun mlinks-html-style-mode-fun (goto)
  (let (start
        end
        bounds)
    (save-excursion
      ;;(when (search-forward "\"" (line-end-position) t)
      (when (< 0 (skip-chars-forward "^\"'" (line-end-position)))
        (forward-char)
        (save-match-data
          (when (looking-back
                 mlinks-html-link-regex
                 (line-beginning-position -1))
            (let ((which (if (match-beginning 1) 1 2)))
              (setq start (match-beginning which))
              (setq end   (match-end which)))
            (setq bounds (cons start end))))))
    (when start
      (if (not goto)
          bounds
        (let ((href-val (buffer-substring-no-properties start end)))
          (mlinks-html-href-act-on href-val))
        t))))

(defun mlink-check-file-to-edit (file)
  (assert (file-name-absolute-p file))
  (let ((file-dir (file-name-directory file)))
    (unless (file-directory-p file-dir)
      (if (file-directory-p (file-name-directory file))
          (if (yes-or-no-p (format "Directory %s does not exist. Create it? " file-dir))
              (make-directory file-dir)
            (setq file nil))
        (if (yes-or-no-p (format "Directory %s and its parent does not exist. Create them? " file-dir))
            (make-directory file-dir t)
          (setq file nil))))
    file))

(defun mlinks-html-edit-at (file &optional anchor)
  (let ((abs-file (if (file-name-absolute-p file)
                      file
                    (expand-file-name file))))
    (if (or (file-directory-p abs-file)
            (string= abs-file
                     (file-name-as-directory abs-file)))
        (if (file-directory-p abs-file)
            (when (y-or-n-p (format "Do you want to edit the directory %s? : " abs-file))
              (dired abs-file))
          (message "Can't find directory %s" abs-file))
      (when (mlink-check-file-to-edit abs-file)
        (let ((b (find-file-noselect abs-file)))
          (mlinks-switch-to-buffer b))
        (when anchor
          (let ((here (point))
                (anchor-regexp (concat "\\(?:id\\|name\\)[[:space:]]*=[[:space:]]*\"" anchor "\"")))
            (goto-char (point-min))
            (if (search-forward-regexp anchor-regexp nil t)
                (backward-char 2)
              (message "Anchor \"%s\" not found" anchor)
              (goto-char here))))))))

(defun mlinks-html-mail-to (addr)
  (cond ((fboundp 'w32-shell-execute)
         (w32-shell-execute "open" addr))
        (t (message "Don't know how to how to start mail"))))

(defun mlinks-html-href-act-on (href-val)
  (if href-val
      (let* ((possible (mlinks-html-possible-href-actions href-val))
             (edit (assoc 'edit possible))
             (file (nth 1 edit))
             (anchor (nth 2 edit))
             )
        (cond (edit
               (mlinks-html-edit-at file anchor)
               t)
              ((assoc 'mailto possible)
               (when (y-or-n-p "This is a mail address.  Do you want to send a message to this mail address? ")
                 (mlinks-html-mail-to href-val)))
              ((assoc 'view-web possible)
               (when (y-or-n-p "Can't edit this URL, it is on the web.  View the URL in your web browser? ")
                 (browse-url href-val)))
              ((assoc 'view-web-base possible)
               (when (y-or-n-p "Can't edit, based URL is to the web.  View resulting URL in your web browser? ")
                 (browse-url (cdr (assoc 'view-web-base possible)))))
              (t
               (message "Do not know how to handle this URL"))
              ))
    (message "No value for href attribute")))

(defun mlinks-html-possible-href-actions (link)
  (let ((urlobj (url-generic-parse-url link))
        (edit nil)
        (possible nil))
    (cond ((member (url-type urlobj) '("http" "https"))
           (add-to-list 'possible (cons 'view-web link)))
          ((member (url-type urlobj) '("mailto"))
           (add-to-list 'possible (cons 'mailto link)))
          ((url-host urlobj)
           (message "Do not know how to handle this URL"))
          (t (setq edit t)))
    (when edit
      (let ((base-href (mlinks-html-find-base-href)))
        (when base-href
          (let ((baseobj (url-generic-parse-url base-href)))
            (setq edit nil)
            (cond ((member (url-type baseobj) '("http" "https"))
                   (add-to-list 'possible (cons 'view-web-base (url-expand-file-name link base-href))))
                  ((url-host urlobj)
                   (message "Do not know how to handle this URL"))
                  (t (setq edit t)))))
        (when edit
          (let* ((full (split-string (url-filename urlobj) "#"))
                 (file (nth 0 full))
                 (anchor (nth 1 full))
                 )
            (when (equal file "")
              (setq file (buffer-file-name)))
            (when base-href
              ;; We know at this point it is not a http url
              (setq file (expand-file-name file base-href)))
            (let ((ext (downcase (file-name-extension file))))
              (when (member ext '("htm" "html"))
                (add-to-list 'possible (cons 'view-local (list file anchor))))
              (when (and (featurep 'gimp)
                         (member ext '("gif" "png" "jpg" "jpeg")))
                (add-to-list 'possible (cons 'edit-gimp (list file anchor)))))
            (when (featurep 'html-upl)
              (add-to-list 'possible (cons 'upload (list file anchor))))
            (add-to-list 'possible (cons 'edit (list file anchor)))))))
    possible))

(defun mlinks-html-find-base-href ()
  "Return base href found in the current file."
  (let ((base-href))
    (save-excursion
      (goto-char (point-min))
      (while (and (not base-href)
                  (search-forward-regexp "<!--[^!]*-->\\|<base[[:space:]]" nil t))
        (when (equal " " (char-to-string (char-before)))
          (backward-char 6)
          (when (looking-at "<base [^>]*href *= *\"\\(.*?\\)\"")
            (setq base-href (match-string-no-properties 1))))))
    base-href))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;; Custom-mode ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mlinks-elisp-custom-goto ()
  (mlinks-elisp-mode-fun 'custom))

(defun mlinks-custom-next-mark ()
  (catch 'stop
    (while (search-forward "`" nil t)
      (forward-char)
      (let ((this (mlinks-elisp-mode-fun nil)))
        (when this
          (throw 'stop this))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;; emacs-lisp-mode ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mlinks-elisp-goto ()
  (mlinks-elisp-mode-fun 'source))

(defun mlinks-elisp-hili ()
  (mlinks-elisp-mode-fun nil))

(defun mlinks-elisp-mode-fun (goto)
  (let ((symbol-name (thing-at-point 'symbol)))
    (when symbol-name
      (let ((bounds-- (bounds-of-thing-at-point 'symbol))
            ret--)
        (if (save-excursion
              (goto-char (cdr bounds--))
              (looking-back (concat "(\\(?:require\\|featurep\\)\s+'" symbol-name)
                            (line-beginning-position)))
            (progn
              (setq ret-- bounds--)
              (when goto
                (mlinks-elisp-mode-require symbol-name)))
          (when (mlinks-elisp-mode-symbol symbol-name goto)
            (setq ret-- bounds--)))
        ret--))))

(defun mlinks-elisp-function (symbol)
  "Go to an elisp function."
  (interactive "aElisp function: ")
  (mlinks-elisp-mode-symbol (symbol-name symbol) 'source))

(defun mlinks-elisp-mode-symbol (symbol-name-- goto--)
  ;; Fix-me: use uninterned variables (see mail from Miles)
  ;; Make these names a bit strange because they are boundp at the time of checking:
  (let ((symbol-- (intern-soft symbol-name--))
        defs--)
    (when (and symbol-- (boundp symbol--))
      (add-to-list 'defs-- 'variable))
    (when (fboundp symbol--)
      (add-to-list 'defs-- 'function))
    (when (facep symbol--)
      (add-to-list 'defs-- 'face))
    ;; Avoid some fails hits
    (when (memq symbol--
                '(goto t
                       bounds-- funs-- ret--
                       symbol-- defs-- symbol-name-- goto--))
      (setq defs-- nil))
    (let (defs-places
           def)
      (if (not goto--)
          (progn
            defs--)
        (if (not defs--)
            (progn
              (message "Could not find definition of '%s" symbol-name--)
              nil)
          (dolist (type (cond
                         ((eq goto-- 'source)
                          '(nil defvar defface))
                         ((eq goto-- 'custom)
                          '(defvar defface))
                         (t
                          (error "Bad goto-- value: %s" goto--))))
            (condition-case err
                (add-to-list 'defs-places
                             (cons
                              type
                              (save-excursion
                                (let* ((bp (find-definition-noselect symbol-- type))
                                       (b (car bp))
                                       (p (cdr bp)))
                                  (unless p
                                    (with-current-buffer b
                                      (save-restriction
                                        (widen)
                                        (setq bp (find-definition-noselect symbol-- type)))))
                                  bp))))
              (error
               ;;(lwarn '(mlinks) :error "%s" (error-message-string err))
               (when t
                 (cond
                  ((eq (car err) 'search-failed))
                  ((and (eq (car err) 'error)
                        (string= (error-message-string err)
                                 (format "Don't know where `%s' is defined" symbol--))))
                  (t
                   (message "%s: %s" (car err) (error-message-string err))))))))
          (if (= 1 (length defs-places))
            (setq def (car defs-places))
            (let ((many nil)
                  lnk)
              (dolist (d defs-places)
                (if (not lnk)
                    (setq lnk (cdr d))
                  (unless (equal lnk (cdr d))
                    (setq many t))))
              (if (not many)
                  (setq def (car defs-places))
                (let* ((alts (mapcar (lambda (elt)
                                       (let ((type (car elt))
                                             str)
                                         (setq str
                                               (cond
                                                ((not type)
                                                 "Function")
                                                ((eq type 'defvar)
                                                 "Variable")
                                                ((eq type 'defface)
                                                 "Face")))
                                         (cons str elt)))
                                     defs-places))
                       (stralts (mapcar (lambda (elt)
                                          (car elt))
                                        alts))
                       (case-fold-search t)
                       (stralt (completing-read "Type: " stralts nil t))
                       (alt (assoc stralt alts)))
                  (setq def (cdr alt))))))
          (when def
            (cond
             ((eq goto-- 'source)
              ;; Be sure to go to the real sources from CVS:
              (let* ((buf (car (cdr def)))
                     ;; Avoid going to source
                     ;;(file (find-source-lisp-file (with-current-buffer buf buffer-file-name)) )
                     (file (with-current-buffer buf buffer-file-name))
                     (orig-buf (find-file-noselect file)))
                (mlinks-switch-to-buffer orig-buf))
              (let ((p (cdr (cdr def))))
                (if (or (< p (point-min))
                        (> p (point-max)))
                    (when (y-or-n-p (format "%s is invisible because of narrowing. Widen? " symbol--))
                      (widen)
                      (goto-char p))
                  (goto-char p))))
             ((eq goto-- 'custom)
              (mlinks-custom symbol--))
             (t
              (error "Back goto-- value again: %s" goto--)))))))))

(defun mlinks-elisp-mode-require (module)
  (find-library module))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;; Helpers when adopting for modes ;;;;;;;;;;;;;;;;;
(defun mlinks-hit-test ()
  "Just a helper function for adding support for new modes."
  (let* (
         (s0 (if (match-string 0) (match-string 0) ""))
         (s1 (if (match-string 1) (match-string 1) ""))
         (s2 (if (match-string 2) (match-string 2) ""))
         (s3 (if (match-string 3) (match-string 3) ""))
         )
  (message "match0=%s, match1=%s, match2=%s, match3=%s" s0 s1 s2 s3)))

(defun mlinks-handle-reg-fun-list (reg-fun-list)
  "Just a helper function."
  (let (done
        regexp
        hitfun
        m
        p
        b
        )
    (dolist (rh reg-fun-list)
      (message "rh=%s" rh);(sit-for 2)
      (unless done
        (setq regexp (car rh))
        (setq hitfun (cadr rh))
        (message "regexp=%s, hitfun=%s" regexp hitfun);(sit-for 1)
        (when (and (setq m (re-search-backward regexp (line-beginning-position) t))
                 (> p (match-beginning 0)))
          (setq done t)
          (setq b (match-beginning 0))
          (setq e (match-end 0))
          )
        (if (not (and b e
                      (< b p)
                      (< p e)))
            (message "MLinks Mode did not find any link here")
          (goto-char b)
          (if (not (looking-at regexp))
              (error "Internal error, regexp %s, no match looking-at" regexp)
            (let ((last (car mlinks-places))
                  (m (make-marker)))
              (set-marker m (line-beginning-position))
              (when (or (not last)
                        (/= m last))
                (setq mlinks-places (cons m mlinks-places))))
            (funcall hitfun))
          )))))

;;   (message "kb tab=%s, \\t=%s, ovl=%s, keymap=%s"
;;            (key-binding [tab])
;;            (key-binding "\t")
;;            (overlays-at (point))
;;            (overlay-get mlinks-hilight-point-ovl 'keymap))

;;; This is for the problem reported by some Asian users:
;;;
;;;   Lisp error: (invalid-read-syntax "] in a list")
;;;
;; Local Variables:
;; coding: utf-8
;; End:

(provide 'mlinks)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; mlinks.el ends here
