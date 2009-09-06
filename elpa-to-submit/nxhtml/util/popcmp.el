;;; popcmp.el --- Completion enhancements, popup etc
;;
;; Author: Lennart Borgman
;; Created: Tue Jan 09 12:00:29 2007
;; Version: 1.00
;; Last-Updated: 2008-03-08T03:30:15+0100 Sat
;; Keywords:
;; Compatibility:
;;
;; Features that might be required by this library:
;;
;;   `ourcomments-util'.
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

(eval-when-compile (require 'ourcomments-util))

(defgroup popcmp nil
  "Customization group for popup completion."
  :tag "Completion Style \(popup etc)"
  :group 'nxhtml
  :group 'convenience)

;; (define-toggle popcmp-popup-completion t
;;   "Use a popup menu for some completions if non-nil.

;; ***** Obsolete: Use `popcmp-completion-style' instead.

;; When completion is used for alternatives tighed to text at the
;; point in buffer it may make sense to use a popup menu for
;; completion.  This variable let you decide whether normal style
;; completion or popup style completion should be used then.

;; This style of completion is not implemented for all completions.
;; It is implemented for specific cases but the choice of completion
;; style is managed generally by this variable for all these cases.

;; See also the options `popcmp-short-help-beside-alts' and
;; `popcmp-group-alternatives' which are also availabe when popup
;; completion is available."
;;   :tag "Popup style completion"
;;   :group 'popcmp)

(defun popcmp-cant-use-style (style)
  (save-match-data ;; runs in timer
    (describe-variable 'popcmp-completion-style)
    (message (propertize "popcmp-completion-style: style `%s' is not available"
                         'face 'secondary-selection)
             style)))





;;(popcmp-set-completion-style 'company-mode)
(defun popcmp-set-completion-style (val)
  "Internal use, set `popcmp-completion-style' to VAL."
  (assert (memq val '(popcmp-popup emacs-default company-mode anything)) t)
  (case val
    ('company-mode (unless (fboundp 'company-mode)
                     (require 'company-mode nil t))
                   (unless (fboundp 'company-mode)
                     (run-with-idle-timer 1 nil 'popcmp-cant-use-style val)
                     (setq val 'popcmp-popup)))
    ('anything     (unless (fboundp 'anything)
                     (require 'anything nil t))
                   (unless (fboundp 'anything)
                     (run-with-idle-timer 1 nil 'popcmp-cant-use-style val)
                     (setq val 'popcmp-popup))))
  (set-default 'popcmp-completion-style val)
  (unless (eq val 'company-mode)
    (when (and (boundp 'global-company-mode)
               global-company-mode)
      (global-company-mode -1))
    (remove-hook 'after-change-major-mode-hook 'company-set-major-mode-backend)
    (remove-hook 'mumamo-after-change-major-mode-hook 'mumamo-turn-on-company-mode))
  (when (eq val 'company-mode)
    (unless (and (boundp 'global-company-mode)
                 global-company-mode)
      (global-company-mode 1))
    (add-hook 'after-change-major-mode-hook 'company-set-major-mode-backend)
    (add-hook 'mumamo-after-change-major-mode-hook 'mumamo-turn-on-company-mode)))

;; fix-me: move to mumamo.el
(defun mumamo-turn-on-company-mode ()
  (when (and (boundp 'company-mode)
             company-mode)
    (company-mode 1)
    (company-set-major-mode-backend)))

(defcustom popcmp-completion-style (cond
                                    ;;((and (fboundp 'global-company-mode) 'company-mode) 'company-mode)
                                    (t 'popcmp-popup))
  "Completion style.
The currently available completion styles are:

- popcmp-popup: Use OS popup menus (default).
- emacs-default: Emacs default completion.
- Company Mode completion.
- anything: The Anything elisp lib completion style.

The style of completion set here is not implemented for all
completions.  The scope varies however with which completion
style you have choosen.

For information about Company Mode and how to use it see URL
`http://www.emacswiki.org/emacs/CompanyMode'.

For information about Anything and how to use it see URL
`http://www.emacswiki.org/emacs/Anything'.

See also the options `popcmp-short-help-beside-alts' and
`popcmp-group-alternatives' which are also availabe when popup
completion is available."
  :type '(choice (const company-mode)
                 (const popcmp-popup)
                 (const emacs-default)
                 (const anything))
  :set (lambda (sym val)
         (popcmp-set-completion-style val))
  :group 'popcmp)

(define-toggle popcmp-short-help-beside-alts t
  "Show a short help text beside each alternative.
If this is non-nil a short help text is shown beside each
alternative for which such a help text is available.

This works in the same circumstances as
`popcmp-completion-style'."
  :tag "Short help beside alternatives"
  :group 'popcmp)

(define-toggle popcmp-group-alternatives t
  "Do completion in two steps.
For some completions the alternatives may have been grouped in
sets. If this option is non-nil then you will first choose a set
and then an alternative within this set.

This works in the same circumstances as
`popcmp-completion-style'."
  :tag "Group alternatives"
  :group 'popcmp)

(defun popcmp-getsets (alts available-sets)
  (let ((sets nil))
    (dolist (tg alts)
      (let (found)
        (dolist (s available-sets)
          (when (member tg (cdr s))
            (setq found t)
            (let ((sets-entry (assq (car s) sets)))
              (unless sets-entry
                (setq sets (cons (list (car s)) sets))
                (setq sets-entry (assq (car s) sets)))
                  (setcdr sets-entry (cons tg (cdr sets-entry))))))
        (unless found
          (let ((sets-entry (assq 'unsorted sets)))
            (unless sets-entry
              (setq sets (cons (list 'unsorted) sets))
              (setq sets-entry (assq 'unsorted sets)))
            (setcdr sets-entry (cons tg (cdr sets-entry)))))))
    (setq sets (sort sets (lambda (a b)
                            (string< (format "%s" b)
                                     (format "%s" a)))))
    ;;(dolist (s sets) (setcdr s (reverse (cdr s))))
    sets))

(defun popcmp-getset-alts (set-name sets)
  ;; Allow both strings and symbols as keys:
  (let ((set (or (assoc (downcase set-name) sets)
                 (assoc (read (downcase set-name)) sets))))
    (cdr set)))

(defvar popcmp-completing-with-help nil)

(defun popcmp-add-help (alt alt-help-hash)
  (if alt-help-hash
      (let ((h (if (hash-table-p alt-help-hash)
                   (gethash alt alt-help-hash)
                 (let ((hh (assoc alt alt-help-hash)))
                   (cadr hh)))
                 ))
        (if h
            (concat alt " -- " h)
          alt))
    alt))

(defun popcmp-remove-help (alt-with-help)
  (when alt-with-help
    (replace-regexp-in-string " -- .*" "" alt-with-help)))

(defun popcmp-anything (prompt collection
                               predicate require-match
                               initial-input hist def inherit-input-method
                               alt-help alt-sets)
  (let* ((table collection)
         (alt-sets2 (apply 'append (mapcar 'cdr alt-sets)))
         (cands (cond ((not (listp table)) alt-sets2)
                     (t table)))
         ret-val
         (source `((name . "Completion candidates")
                   (candidates . ,cands)
                   (action . (("Select current alternative (press TAB to see it again)" . (lambda (candidate)
                                            (setq ret-val candidate))))))))
    (anything (list source) initial-input prompt)
    ret-val))

(defun popcmp-completing-read-1 (prompt collection
                                        predicate require-match
                                        initial-input hist2 def inherit-input-method alt-help alt-sets)
  ;; Fix-me: must rename hist to hist2 in par list. Emacs bug?
  (cond
   ((eq popcmp-completion-style 'emacs-default)
    (completing-read prompt collection predicate require-match initial-input hist2 def inherit-input-method))
   ((eq popcmp-completion-style 'anything)
    (popcmp-anything prompt collection predicate require-match initial-input hist2 def inherit-input-method
                     alt-help alt-sets))
   ((eq popcmp-completion-style 'company-mode)
    ;; No way to read this from company-mode, use emacs-default
    (completing-read prompt collection predicate require-match initial-input hist2 def inherit-input-method))
   (t (error "Do not know popcmp-completion-style %S" popcmp-completion-style))))

(defun popcmp-completing-read-other (prompt
                                    table
                                    &optional predicate require-match
                                    initial-input pop-hist def inherit-input-method
                                    alt-help
                                    alt-sets)
  (let ((alts
         (if (and popcmp-group-alternatives alt-sets)
             (all-completions initial-input table predicate)
           (if popcmp-short-help-beside-alts
               (all-completions "" table predicate)
             table))))
    (when (and popcmp-group-alternatives alt-sets)
      (let* ((sets (popcmp-getsets alts alt-sets))
             (set-names (mapcar (lambda (elt)
                                  (capitalize (format "%s" (car elt))))
                                sets))
             set)
        (setq set
              (popcmp-completing-read-1 (concat
                                         (substring prompt 0 (- (length prompt) 2))
                                         ", select group: ")
                                        set-names
                                        nil t
                                        nil nil nil inherit-input-method nil nil))
        (if (or (not set) (= 0 (length set)))
            (setq alts nil)
          (setq set (downcase set))
          (setq alts (popcmp-getset-alts set sets)))))
    (if (not alts)
        ""
      (if (= 1 (length alts))
          (car alts)
        (when popcmp-short-help-beside-alts
          (setq alts (mapcar (lambda (a)
                               (popcmp-add-help a alt-help))
                             alts)))
        (popcmp-remove-help
         ;;(completing-read prompt
         (popcmp-completing-read-1 prompt
                                   alts ;table
                                   predicate require-match
                                   initial-input pop-hist def inherit-input-method
                                   ;;alt-help alt-sets
                                   nil nil
                                   ))))))

(defun popcmp-completing-read-pop (prompt
                                  table
                                  &optional predicate require-match
                                  initial-input hist def inherit-input-method
                                  alt-help
                                  alt-sets)
  (unless initial-input
    (setq initial-input ""))
  (let ((matching-alts (all-completions initial-input table predicate))
        completion)
    (if (not matching-alts)
        (progn
          (message "No alternative found")
          nil)
      (let ((pop-map (make-sparse-keymap prompt))
            (sets (when (and popcmp-group-alternatives alt-sets)
                    (popcmp-getsets matching-alts alt-sets)))
            (add-alt (lambda (k tg)
                       (define-key k
                         (read (format "[popcmp-%s]" (replace-regexp-in-string " " "-" tg)))
                         (list 'menu-item
                               (popcmp-add-help tg alt-help)
                               `(lambda ()
                                  (interactive)
                                  (setq completion ,tg)))))))
        (if sets
            (dolist (s sets)
              (let ((k (make-sparse-keymap)))
                (dolist (tg (cdr s))
                  (funcall add-alt k tg))
                (define-key pop-map
                  (read (format "[popcmps-%s]" (car s)))
                  (list 'menu-item
                        (capitalize (format "%s" (car s)))
                        k))))
          (dolist (tg matching-alts)
            (funcall add-alt pop-map tg)))
        (popup-menu-at-point pop-map)
        completion))))

(defvar popcmp-in-buffer-allowed nil)
(defun popcmp-completing-read (prompt
                              table
                              &optional predicate require-match
                              initial-input pop-hist def inherit-input-method
                              alt-help
                              alt-sets)
  "Read a string in the minubuffer with completion, or popup a menu.
This function can be used instead `completing-read'. The main
purpose is to provide a popup style menu for completion when
completion is tighed to text at point in a buffer. If a popup
menu is used it will be shown at window point. Whether a popup
menu or minibuffer completion is used is governed by
`popcmp-completion-style'.

The variables PROMPT, TABLE, PREDICATE, REQUIRE-MATCH,
INITIAL-INPUT, POP-HIST, DEF and INHERIT-INPUT-METHOD all have the
same meaning is for `completing-read'.

ALT-HELP should be nil or a hash variable or an association list
with the completion alternative as key and a short help text as
value.  You do not need to supply help text for all alternatives.
The use of ALT-HELP is set by `popcmp-short-help-beside-alts'.

ALT-SETS should be nil or an association list that has as keys
groups and as second element an alternative that should go into
this group.
"
  (if (and popcmp-in-buffer-allowed
           (eq popcmp-completion-style 'company-mode)
           (boundp 'company-mode)
           company-mode)
      (progn
        (add-hook 'company-completion-finished-hook 'nxhtml-complete-tag-do-also-for-state-completion t)
        ;;(remove-hook 'company-completion-finished-hook 'nxhtml-complete-tag-do-also-for-state-completion)
        (call-interactively 'company-nxml)
        initial-input)

    (popcmp-mark-completing initial-input)
    (let ((err-sym 'quit)
          (err-val nil)
          ret)
      (unwind-protect
          (if (eq popcmp-completion-style 'popcmp-popup)
              (progn
                (setq err-sym nil)
                (popcmp-completing-read-pop
                 prompt
                 table
                 predicate require-match
                 initial-input pop-hist def inherit-input-method
                 alt-help
                 alt-sets))
            ;;(condition-case err
                (prog1
                    (setq ret (popcmp-completing-read-other
                               prompt
                               table
                               predicate require-match
                               initial-input pop-hist def inherit-input-method
                               alt-help
                               alt-sets))
                  ;; Unless quit or error in Anything we come here:
                  ;;(message "ret=(%S)" ret)
                  (when (and ret (not (string= ret "")))
                    (setq err-sym nil)))
              ;; (error
              ;;  ;;(message "err=%S" err)
              ;;  (setq err-sym (car err))
              ;;  (setq err-val (cdr err))))
                )
        (popcmp-unmark-completing)
        (when err-sym (signal err-sym err-val))))))

(defvar popcmp-mark-completing-ovl nil)

(defun popcmp-mark-completing (initial-input)
  (let ((start (- (point) (length initial-input)))
        (end (point)))
    (if (overlayp popcmp-mark-completing-ovl)
        (move-overlay popcmp-mark-completing-ovl start end)
      (setq popcmp-mark-completing-ovl (make-overlay start end))
      (overlay-put popcmp-mark-completing-ovl 'face 'match)))
  (sit-for 0))

(defun popcmp-unmark-completing ()
  (when popcmp-mark-completing-ovl
    (delete-overlay popcmp-mark-completing-ovl)))


;; (defun popcmp-temp ()
;;   (interactive)
;;   (let* ((coord (point-to-coord (point)))
;;          (x (nth 0 (car coord)))
;;          (y (nth 1 (car coord)))
;;          (emacsw32-max-frames nil)
;;          (f (make-frame
;;              (list '(minibuffer . only)
;;                    '(title . "Input")
;;                    '(name . "Input frame")
;;                    (cons 'left x)
;;                    (cons 'top y)
;;                    '(height . 1)
;;                    '(width . 40)
;;                    '(border-width . 1)
;;                    '(internal-border-width . 2)
;;                    '(tool-bar-lines . nil)
;;                    '(menu-bar-lines . nil)
;;                    ))))
;;     f))


(provide 'popcmp)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; popcmp.el ends here
