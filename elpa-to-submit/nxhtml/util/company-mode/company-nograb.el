;;; company-nograb.el --- Company menu without any word matching in buffer
;;
;; Author: Lennart Borgman (lennart O borgman A gmail O com)
;; Created: 2009-07-18 Sat
;; Version:
;; Last-Updated:
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
;; published by the Free Software Foundation; either version 3, or
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

(require 'company)
(eval-when-compile (require 'cl))

(defvar company-nograb-old-blink-cursor nil)

(defvar company-nograb-overlay nil)
(make-variable-buffer-local 'company-nograb-overlay)

(defvar company-nograb-overlay-field nil)
(make-variable-buffer-local 'company-nograb-overlay-field)

(defconst company-nograb-result nil)

(defconst company-nograb-menu-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-g" 'company-abort)
    (define-key map "\r"          'company-complete-selection)
    (define-key map [remap next-line]     'company-nograb-select-next)
    (define-key map [remap previous-line] 'company-nograb-select-previous)
    map))

(defconst company-nograb-field-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-g" 'company-abort)
    (define-key map "\r"          'company-complete-selection)
    (define-key map [remap self-insert-command]           'company-nograb-to-ghost)
    (define-key map [remap forward-char]                  'company-nograb-to-ghost)
    (define-key map [remap backward-char]                 'company-nograb-to-ghost)
    (define-key map [remap move-beginning-of-line]        'company-nograb-to-ghost)
    (define-key map [remap move-end-of-line]              'company-nograb-to-ghost)
    (define-key map [remap move-beginning-of-line]        'company-nograb-to-ghost)
    (define-key map [remap move-end-of-line]              'company-nograb-to-ghost)
    (define-key map [remap ourcomments-move-beginning-of-line] 'company-nograb-to-ghost)
    (define-key map [remap ourcomments-move-end-of-line]       'company-nograb-to-ghost)

    ;;(define-key map [remap backward-delete-char-untabify] 'company-nograb-to-ghost)
    ;;
    ;; Fix-me: For read-only buffers this is needed, I have no idea why:
    (define-key map "\d"        'company-nograb-to-ghost-bs-del)
    (define-key map [backspace] 'company-nograb-to-ghost-bs-del)

    (define-key map [remap next-line]     'company-nograb-select-next)
    (define-key map [remap previous-line] 'company-nograb-select-previous)
    (define-key map "\t"    'company-complete-common)
    (define-key map [(tab)] 'company-complete-common)
    (set-keymap-parent map global-map)
    map))

(defconst company-nograb-host nil)
(make-variable-buffer-local 'company-nograb-host)

(defconst company-nograb-ghost nil)
(make-variable-buffer-local 'company-nograb-ghost)

(defconst company-nograb-field-len 5)
(make-variable-buffer-local 'company-nograb-field-len)

(defconst company-nograb-field-len-default 5)
(make-variable-buffer-local 'company-nograb-field-len-default)

(defconst company-nograb-field-pos 1)
(make-variable-buffer-local 'company-nograb-field-pos)

(defvar company-nograb-candidates nil)
(make-variable-buffer-local 'company-nograb-candidates)

(defvar company-nograb-overlay-before nil)
(defvar company-nograb-overlay-keymap nil)
(defvar company-nograb-overlay-after  nil)

(defvar company-nograb-menu-label-overlay nil)
(make-variable-buffer-local 'company-nograb-menu-label-overlay)

(defvar company-nograb-menu-label nil)
(make-variable-buffer-local 'company-nograb-menu-label)

(defvar company-nograb-point nil)
(make-variable-buffer-local 'company-nograb-point)

(defun company-nograb-select-next ()
  (interactive)
  (company-select-next)
  ;;(company-nograb-select-to-field)
  )
(defun company-nograb-select-previous ()
  (interactive)
  (company-select-previous)
  ;;(company-nograb-select-to-field)
  )
(defun company-nograb-RET-action ()
  (interactive)
  (if (= 1 num-cand)
      (company-complete-selection)
    (company-nograb-select-to-field)))

(defun company-nograb-select-to-field ()
  (interactive)
  (let ((sel (nth company-selection company-candidates)))
    (with-current-buffer company-nograb-ghost
      (erase-buffer)
      (insert sel)
      (company-nograb-update-field)
      ;;(goto-char 1) (insert " ")
      )))

(defface company-nograb-field
  '((default :box t :inherit company-tooltip))
  "Face for popup messages."
  :group 'tabkey2)

(defun company-nograb-to-ghost (&optional command)
  (interactive)
  (let ((ghost-win (get-buffer-window company-nograb-ghost)))
    (save-window-excursion
      (if ghost-win
          (select-window ghost-win)
        (switch-to-buffer company-nograb-ghost))
      (goto-char (company-nograb-grab-word-pos))
      (message "ghost:this-original-command=%s, cb=%s" this-original-command (current-buffer))
      (call-interactively (or command this-original-command))
      (company-nograb-grab-word-pos-set (point)))
    (when ghost-win (with-selected-window ghost-win (goto-char (company-nograb-grab-word-pos)))))
  (company-nograb-update-field))

(defun company-nograb-to-ghost-bs-del ()
    (interactive)
    (company-nograb-to-ghost 'backward-delete-char))

(defun company-nograb-grab-word ()
  (with-current-buffer company-nograb-ghost
    ;;(message "nograb word:`%s'" (buffer-string))
    (buffer-string)))

(defun company-nograb-grab-word-pos ()
  (with-current-buffer (or company-nograb-host (current-buffer))
    company-nograb-field-pos))

(defun company-nograb-grab-word-pos-set (pos)
  (with-current-buffer (or company-nograb-host (current-buffer))
    (setq company-nograb-field-pos pos)))

(defun company-nograb-update-field ()
  (let* ((maxlen company-nograb-field-len)
         (current (company-nograb-grab-word))
         (pos     (company-nograb-grab-word-pos))
         (curlen (length current))
         (trail-len (max 1 (- maxlen curlen)))
         (trail  (make-string trail-len ?\ ))
         (face 'company-nograb-field)
         (txt (propertize (concat current trail) 'face face)))
    (setq company-nograb-field-len (max company-nograb-field-len
                                        (- (length txt) 1)))
    (put-text-property (1- pos) pos 'face `(,face :underline t) txt)
    (overlay-put company-nograb-overlay-field 'after-string txt)))

(defvar company-nograb-state-is-saved nil)
(make-variable-buffer-local 'company-nograb-state-is-saved)

(defun company-nograb-save-state ()
  (unless company-nograb-state-is-saved
    (setq company-nograb-state-is-saved t)
    (setq company-nograb-old-blink-cursor blink-cursor-mode)))

(defun company-nograb-reset-state ()
  (when company-nograb-state-is-saved
    (setq company-nograb-state-is-saved nil)
    (blink-cursor-mode (if company-nograb-old-blink-cursor 1 -1))))

(defun company-nograb-common-setup (candidates finish-function)
  (unless candidates (error "company-nograb-common-setup: candidates is nil"))
  (unless finish-function (error "company-nograb-common-setup: finish-function is nil"))
  (company-nograb-save-state)
  (blink-cursor-mode -1)
  (setq company-nograb-point (point))
  (setq company-nograb-candidates candidates)
  (setq company-nograb-finish-fun finish-function)
  (add-hook 'post-command-hook 'company-nograb-post-command nil t)
  (add-hook 'company-completion-finished-hook 'company-nograb-get-result nil t)
  (add-hook 'company-completion-cancelled-hook 'company-nograb-cleanup nil t))

(defun company-nograb-field-setup (label candidates finish-function)
  (company-nograb-cleanup)
  (company-nograb-common-setup candidates finish-function)
  (setq company-nograb-candidates (copy-sequence candidates))
  (setq company-nograb-finish-fun finish-function)
  (setq company-nograb-field-len company-nograb-field-len-default)
  (let* ((end (min (line-end-position) (+ company-nograb-field-len 1 (point))))
         (ovl (make-overlay (point) end))
         (ovl-field (make-overlay (+ 1 (point)) end)))
    (overlay-put ovl 'keymap company-nograb-field-map)
    (overlay-put ovl       'priority 100)
    (overlay-put ovl-field 'priority 101)
    (overlay-put ovl-field 'invisible t)
    (setq company-nograb-ghost (generate-new-buffer "company-nograb"))
    (company-nograb-grab-word-pos-set 1)
    (let ((this-buffer (current-buffer)))
      (with-current-buffer company-nograb-ghost
        (setq company-nograb-host this-buffer)))
    (setq company-nograb-overlay-field ovl-field)
    (setq company-nograb-overlay ovl)
    (company-nograb-update-field)
    ))

(defun company-nograb-cleanup (&optional ign)
  (company-nograb-reset-state)
  (remove-hook 'company-completion-finished-hook 'company-nograb-get-result t)
  (remove-hook 'company-completion-cancelled-hook 'company-nograb-cleanup t)
  (setq company-nograb-finish-fun nil)
  (remove-hook 'post-command-hook 'company-nograb-post-command t)
  (setq company-nograb-point nil)
  (when (overlayp company-nograb-overlay)
    (delete-overlay company-nograb-overlay)
    (setq company-nograb-overlay nil))
  (when (overlayp company-nograb-overlay-field)
    (delete-overlay company-nograb-overlay-field)
    (setq company-nograb-overlay-field))
  ;;(setq company-nograb-menu-label nil)
  (when company-nograb-menu-label-overlay
    (delete-overlay company-nograb-menu-label-overlay)
    (setq company-nograb-menu-label-overlay))
  (when company-nograb-ghost
    (kill-buffer company-nograb-ghost)
    (setq company-nograb-ghost)))

(defvar company-nograb-finish-fun nil)
(make-variable-buffer-local 'company-nograb-finish-fun)

(defun company-nograb-get-result (result)
  (setq company-nograb-result result)
  (funcall company-nograb-finish-fun result)
  (company-nograb-cleanup))

(defun company-nograb-field-backend (command &optional arg &rest ignored)
  "A completion back-end using a field for user input.
Nothing is changed in the current buffer."
  (case command
    ('start (if (not (and company-nograb-candidates company-nograb-finish-fun))
                (error "Call company-nograb-field instead")
              (company-begin-backend 'company-nograb-field-backend)))
    ('prefix (company-nograb-grab-word))
    ('candidates company-nograb-candidates)
    ('ignore-case t)
    ('require-match t)
    ('sorted nil)
    ('no-insert t)
    ('duplicates t)))

(defun company-nograb-field (label candidates finish-function)
  "Display a field tighed to a menu at point.
Display string LABEL at point and start a menu backend using
CANDIDATES as choices.  When the backend finishes remove LABEL.
If the user made a choice then call FINISH-FUNCTION with the
choice as a parameter."
  (company-nograb-field-setup label candidates finish-function)
  (company-nograb-field-backend 'start))



(defun company-nograb-menu-setup (label candidates finish-function)
  (company-nograb-cleanup)
  (company-nograb-common-setup candidates finish-function)
  (let* ((lbl (concat " " label " "))
         (len (length lbl))
         ;; Fix-me: needs two overlays to fix this better and end of line.
         (beg (min (- (line-end-position) 1)
                   (1+ (point))))
         (end (min (- (line-end-position) 0)
                   (+ beg len)))
         (ovl (make-overlay beg end)))
    ;;(apply 'message "b/e=%s/%s, p=%s le=%s" (mapcar (lambda (v) (- v (line-beginning-position))) (list beg end (point) (line-end-position))))
    (overlay-put ovl 'after-string (propertize lbl 'face 'menu))
    (overlay-put ovl 'face 'secondary-selection)
    (overlay-put ovl 'invisible t)
    (setq company-nograb-menu-label-overlay ovl))
  (let ((ovl (make-overlay (point) (1+ (point)))))
    (overlay-put ovl 'keymap company-nograb-menu-map)
    (setq company-nograb-overlay ovl)))

(defun company-nograb-post-command ()
  (unless (= (point) company-nograb-point)
    (company-nograb-cleanup)
    (company-abort)))

(defun company-nograb-menu-backend (command &optional arg &rest ignored)
  "A completion back-end using just a menu for user input.
Nothing is changed in the current buffer."
  (case command
    ('start (company-begin-backend 'company-nograb-menu-backend))
    ('prefix (concat ""))
    ('candidates company-nograb-candidates)
    ('ignore-case t)
    ('require-match t)
    ('sorted nil)
    ('no-insert t)
    ('duplicates t)))

(defun company-nograb-menu (label candidates finish-function)
  "Display a menu at point.
Display string LABEL at point and start a menu backend using
CANDIDATES as choices.  When the backend finishes remove LABEL.
If the user made a choice then call FINISH-FUNCTION with the
choice as a parameter."
  (company-nograb-menu-setup label candidates finish-function)
  (company-nograb-menu-backend 'start))



(defun my-nograb-field ()
  "Test of field backend."
  (interactive)
  (company-nograb-field "My field label" '("some" "things" "that" "sometimes" "fits")
                              (lambda (res) (message "res=%s" res))))

(defun my-nograb-menu ()
  "Test of menu backend."
  (interactive)
  (company-nograb-menu "My menu label" '("some" "things" "that" "sometimes" "fits")
                        (lambda (res) (message "res=%s" res))))

;;;###autoload
(defun company-choose (candidates)
  (setq company-nograb-candidates candidates))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; company-nograb.el ends here
