;;; ourcomments-util.el --- Utility routines
;;
;; Author: Lennart Borgman <lennart dot borgman at gmail dot com>
;; Created: Wed Feb 21 2007
(defconst ourcomments-util:version "0.25") ;;Version:
;; Last-Updated: 2009-08-04 Tue
;; Keywords:
;; Compatibility: Emacs 22
;;
;; Features that might be required by this library:
;;
;;   None
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; The functionality given by these small routines should in my
;; opinion be part of Emacs (but they are not that currently).
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

(eval-when-compile (require 'apropos))
(eval-when-compile (require 'cl))
(eval-when-compile (require 'grep))
(eval-when-compile (require 'ido))
;;(eval-when-compile (require 'mumamo))
(eval-when-compile (require 'recentf))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Popups etc.

(defun point-to-coord (point)
  "Return coordinates of POINT in selected window.
The coordinates are in the form \(\(XOFFSET YOFFSET) WINDOW).
This form is suitable for `popup-menu'."
  ;; Fix-me: showtip.el adds (window-inside-pixel-edges
  ;; (selected-window)). Why?
  (let* ((pn (posn-at-point point))
         (x-y (posn-x-y pn))
         (x (car x-y))
         (y (cdr x-y))
         (pos (list (list x (+ y 20)) (selected-window))))
    pos))

;;;###autoload
(defun popup-menu-at-point (menu &optional prefix)
  "Popup the given menu at point.
This is similar to `popup-menu' and MENU and PREFIX has the same
meaning as there.  The position for the popup is however where
the window point is."
  (let ((where (point-to-coord (point))))
    (popup-menu menu where prefix)))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Toggles in menus

;;;###autoload
(defmacro define-toggle (symbol value doc &rest args)
  "Declare SYMBOL as a customizable variable with a toggle function.
The purpose of this macro is to define a defcustom and a toggle
function suitable for use in a menu.

The arguments have the same meaning as for `defcustom' with these
restrictions:

- The :type keyword cannot be used.  Type is always 'boolean.
- VALUE must be t or nil.

DOC and ARGS are just passed to `defcustom'.

A `defcustom' named SYMBOL with doc-string DOC and a function
named SYMBOL-toggle is defined.  The function toggles the value
of SYMBOL.  It takes no parameters.

To create a menu item something similar to this can be used:

    \(define-key map [SYMBOL]
      \(list 'menu-item \"Toggle nice SYMBOL\"
            'SYMBOL-toggle
            :button '(:toggle . SYMBOL)))"
  (declare (doc-string 3))
  (list
   'progn
   (let ((var-decl (list 'custom-declare-variable
                         (list 'quote symbol)
                         (list 'quote value)
                         doc)))
     (while args
       (let ((arg (car args)))
         (setq args (cdr args))
         (unless (symbolp arg)
           (error "Junk in args %S" args))
         (let ((keyword arg)
               (value (car args)))
           (unless args
             (error "Keyword %s is missing an argument" keyword))
           (setq args (cdr args))
           (cond
            ((not (memq keyword '(:type)))
             (setq var-decl (append var-decl (list keyword value))))
            (t
             (lwarn '(define-toggle) :error "Keyword %s can't be used here"
                    keyword))))))
     (when (assoc :type var-decl) (error ":type is set.  Should not happen!"))
     (setq var-decl (append var-decl (list :type '(quote boolean))))
     var-decl)
   (let* ((SYMBOL-toggle (intern (concat (symbol-name symbol) "-toggle")))
          (SYMBOL-name (symbol-name symbol))
          (fun-doc (concat "Toggles the \(boolean) value of `"
                           SYMBOL-name
                           "'.\n"
                           "For how to set it permanently see this variable.\n"
                           ;;"\nDescription of `" SYMBOL-name "':\n" doc
                           )))
     `(defun ,SYMBOL-toggle ()
        ,fun-doc
        (interactive)
        (customize-set-variable (quote ,symbol) (not ,symbol)))
     )))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Indentation of regions

;; From an idea by weber <hugows@gmail.com>
;; (defun indent-line-or-region ()
;;   "Indent line or region.
;; Only do this if indentation seems bound to \\t.

;; Call `indent-region' if region is active, otherwise
;; `indent-according-to-mode'."
;;   (interactive)
;;   ;; Do a wild guess if we should indent or not ...
;;   (let* ((indent-region-mode)
;;          ;; The above hides the `indent-line-or-region' binding
;;          (t-bound (key-binding [?\t])))
;;     (if (not
;;          (save-match-data
;;            (string-match "indent" (symbol-name t-bound))))
;;         (call-interactively t-bound t)
;;       (if (and mark-active ;; there is a visible region selected
;;                transient-mark-mode)
;;           (indent-region (region-beginning) (region-end))
;;         (indent-according-to-mode))))) ;; indent line

;; (define-minor-mode indent-region-mode
;;   "Use \\t to indent line or region.
;; The key \\t is bound to `indent-line-or-region' if this mode is
;; on."
;;   :global t
;;   :keymap '(([?\t] . indent-line-or-region)))
;; (when indent-region-mode (indent-region-mode 1))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Minor modes

;; (defmacro define-globalized-minor-mode-with-on-off (global-mode mode
;;                                                     turn-on turn-off
;;                                                     &rest keys)
;;   "Make a global mode GLOBAL-MODE corresponding to buffer-local minor MODE.
;; This is a special variant of `define-globalized-minor-mode' for
;; mumamo.  It let bounds the variable GLOBAL-MODE-checking before
;; calling TURN-ON or TURN-OFF.

;; TURN-ON is a function that will be called with no args in every buffer
;;   and that should try to turn MODE on if applicable for that buffer.
;; TURN-OFF is a function that turns off MODE in a buffer.
;; KEYS is a list of CL-style keyword arguments.  As the minor mode
;;   defined by this function is always global, any :global keyword is
;;   ignored.  Other keywords have the same meaning as in `define-minor-mode',
;;   which see.  In particular, :group specifies the custom group.
;;   The most useful keywords are those that are passed on to the
;;   `defcustom'.  It normally makes no sense to pass the :lighter
;;   or :keymap keywords to `define-globalized-minor-mode', since these
;;   are usually passed to the buffer-local version of the minor mode.

;; If MODE's set-up depends on the major mode in effect when it was
;; enabled, then disabling and reenabling MODE should make MODE work
;; correctly with the current major mode.  This is important to
;; prevent problems with derived modes, that is, major modes that
;; call another major mode in their body."

;;   (let* ((global-mode-name (symbol-name global-mode))
;;          (pretty-name (easy-mmode-pretty-mode-name mode))
;;          (pretty-global-name (easy-mmode-pretty-mode-name global-mode))
;;          (group nil)
;;          (extra-keywords nil)
;;          (MODE-buffers (intern (concat global-mode-name "-buffers")))
;;          (MODE-enable-in-buffers
;;           (intern (concat global-mode-name "-enable-in-buffers")))
;;          (MODE-check-buffers
;;           (intern (concat global-mode-name "-check-buffers")))
;;          (MODE-cmhh (intern (concat global-mode-name "-cmhh")))
;;          (MODE-major-mode (intern (concat (symbol-name mode)
;;                                           "-major-mode")))
;;          (MODE-checking (intern (concat global-mode-name "-checking")))
;;          keyw)

;;     ;; Check keys.
;;     (while (keywordp (setq keyw (car keys)))
;;       (setq keys (cdr keys))
;;       (case keyw
;;         (:group (setq group (nconc group (list :group (pop keys)))))
;;         (:global (setq keys (cdr keys)))
;;         (t (push keyw extra-keywords) (push (pop keys) extra-keywords))))

;;     (unless group
;;       ;; We might as well provide a best-guess default group.
;;       (setq group
;;             `(:group ',(intern (replace-regexp-in-string
;;                                 "-mode\\'" "" (symbol-name mode))))))

;;     `(progn

;;        ;; Define functions for the global mode first so that it can be
;;        ;; turned on during load:

;;        ;; List of buffers left to process.
;;        (defvar ,MODE-buffers nil)

;;        ;; The function that calls TURN-ON in each buffer.
;;        (defun ,MODE-enable-in-buffers ()
;;          (let ((,MODE-checking nil))
;;            (dolist (buf ,MODE-buffers)
;;              (when (buffer-live-p buf)
;;                (with-current-buffer buf
;;                  (if ,mode
;;                      (unless (eq ,MODE-major-mode major-mode)
;;                        (setq ,MODE-checking t)
;;                        (,mode -1)
;;                        (,turn-on)
;;                        (setq ,MODE-checking nil)
;;                        (setq ,MODE-major-mode major-mode))
;;                    (setq ,MODE-checking t)
;;                    (,turn-on)
;;                    (setq ,MODE-checking nil)
;;                    (setq ,MODE-major-mode major-mode)))))))
;;        (put ',MODE-enable-in-buffers 'definition-name ',global-mode)

;;        (defun ,MODE-check-buffers ()
;;          (,MODE-enable-in-buffers)
;;          (setq ,MODE-buffers nil)
;;          (remove-hook 'post-command-hook ',MODE-check-buffers))
;;        (put ',MODE-check-buffers 'definition-name ',global-mode)

;;        ;; The function that catches kill-all-local-variables.
;;        (defun ,MODE-cmhh ()
;;          (add-to-list ',MODE-buffers (current-buffer))
;;          (add-hook 'post-command-hook ',MODE-check-buffers))
;;        (put ',MODE-cmhh 'definition-name ',global-mode)


;;        (defvar ,MODE-major-mode nil)
;;        (make-variable-buffer-local ',MODE-major-mode)

;;        ;; The actual global minor-mode
;;        (define-minor-mode ,global-mode
;;          ,(format "Toggle %s in every possible buffer.
;; With prefix ARG, turn %s on if and only if ARG is positive.
;; %s is enabled in all buffers where `%s' would do it.
;; See `%s' for more information on %s."
;;                   pretty-name pretty-global-name pretty-name turn-on
;;                   mode pretty-name)
;;          :global t ,@group ,@(nreverse extra-keywords)

;;          ;; Setup hook to handle future mode changes and new buffers.
;;          (if ,global-mode
;;              (progn
;;                (add-hook 'after-change-major-mode-hook
;;                          ',MODE-enable-in-buffers)
;;                ;;(add-hook 'find-file-hook ',MODE-check-buffers)
;;                (add-hook 'find-file-hook ',MODE-cmhh)
;;                (add-hook 'change-major-mode-hook ',MODE-cmhh))
;;            (remove-hook 'after-change-major-mode-hook ',MODE-enable-in-buffers)
;;            ;;(remove-hook 'find-file-hook ',MODE-check-buffers)
;;            (remove-hook 'find-file-hook ',MODE-cmhh)
;;            (remove-hook 'change-major-mode-hook ',MODE-cmhh))

;;          ;; Go through existing buffers.
;;          (let ((,MODE-checking t))
;;            (dolist (buf (buffer-list))
;;              (with-current-buffer buf
;;                ;;(if ,global-mode (,turn-on) (when ,mode (,mode -1)))
;;                (if ,global-mode (,turn-on) (,turn-off))
;;                ))))

;;        )))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Unfilling
;;
;; The idea is from
;;   http://interglacial.com/~sburke/pub/emacs/sburke_dot_emacs.config

;;;###autoload
(defun unfill-paragraph ()
  "Unfill the current paragraph."
  (interactive) (with-unfilling 'fill-paragraph))
;;(defalias 'unwrap-paragraph 'unfill-paragraph)

;;;###autoload
(defun unfill-region ()
  "Unfill the current region."
  (interactive) (with-unfilling 'fill-region))
;;(defalias 'unwrap-region 'unfill-region)

;;;###autoload
(defun unfill-individual-paragraphs ()
  "Unfill individual paragraphs in the current region."
  (interactive) (with-unfilling 'fill-individual-paragraphs))
;;(defalias 'unwrap-individual-paragraphs 'unfill-individual-paragraphs)

(defun with-unfilling (fn)
  "Unfill using the fill function FN."
  (let ((fill-column 10000000)) (call-interactively fn)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Widgets


;; (rassq 'genshi-nxhtml-mumamo-mode mumamo-defined-turn-on-functions)
;; (major-modep 'nxhtml-mode)
;; (major-modep 'nxhtml-mumamo-mode)
;; (major-modep 'jsp-nxhtml-mumamo-mode)
;; (major-modep 'asp-nxhtml-mumamo-mode)
;; (major-modep 'django-nxhtml-mumamo-mode)
;; (major-modep 'eruby-nxhtml-mumamo-mode)
;; (major-modep 'eruby-nxhtml-mumamo-mode)
;; (major-modep 'smarty-nxhtml-mumamo-mode)
;; (major-modep 'embperl-nxhtml-mumamo-mode)
;; (major-modep 'laszlo-nxml-mumamo-mode)
;; (major-modep 'genshi-nxhtml-mumamo-mode)
;; (major-modep 'javascript-mode)
;; (major-modep 'espresso-mode)
;; (major-modep 'css-mode)

;;;###autoload
(defun major-or-multi-majorp (value)
  (or (mumamo-multi-major-modep value)
      (major-modep value)))

;;;###autoload
(defun major-modep (value)
  "Return t if VALUE is a major mode function."
  (let ((sym-name (symbol-name value)))
    ;; Do some reasonable test to find out if it is a major mode.
    ;; Load autoloaded mode functions.
    ;;
    ;; Fix-me: Maybe test for minor modes? How was that done?
    (when (and (fboundp value)
               (commandp value)
               (not (memq value '(flyspell-mode
                                  isearch-mode
                                  savehist-mode
                                  )))
               (< 5 (length sym-name))
               (string= "-mode" (substring sym-name (- (length sym-name) 5)))
               (if (and (listp (symbol-function value))
                        (eq 'autoload (car (symbol-function value))))
                   (progn
                     (message "loading ")
                     (load (cadr (symbol-function value)) t t))
                 t)
               (or (memq value
                         ;; Fix-me: Complement this table of known major modes:
                         '(fundamental-mode
                           xml-mode
                           nxml-mode
                           nxhtml-mode
                           css-mode
                           javascript-mode
                           espresso-mode
                           php-mode
                           ))
                   (and (intern-soft (concat sym-name "-hook"))
                        (boundp (intern-soft (concat sym-name "-hook"))))
                   (progn (message "Not a major mode: %s" value)
                          ;;(sit-for 4)
                          nil)
                   ))
      t)))

;;;###autoload
(define-widget 'major-mode-function 'function
  "A major mode lisp function."
  :complete-function (lambda ()
                       (interactive)
                       (lisp-complete-symbol 'major-or-multi-majorp))
  :prompt-match 'major-or-multi-majorp
  :prompt-history 'widget-function-prompt-value-history
  :match-alternatives '(major-or-multi-majorp)
  :validate (lambda (widget)
              (unless (major-or-multi-majorp (widget-value widget))
                (widget-put widget :error (format "Invalid function: %S"
                                                  (widget-value widget)))
                widget))
  :value 'fundamental-mode
  :tag "Major mode function")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Lines

;; Changed from move-beginning-of-line to beginning-of-line to support
;; physical-line-mode.
;;;###autoload
(defun ourcomments-move-beginning-of-line(arg)
  "Move point to beginning of line or indentation.
See `beginning-of-line' for ARG.

If `line-move-visual' is non-nil then the visual line beginning
is first tried."
  (interactive "p")
  (let ((pos (point))
        vis-pos)
    (when line-move-visual
      (line-move-visual -1 t)
      (beginning-of-line)
      (setq vis-pos (point))
      (goto-char pos))
    (call-interactively 'beginning-of-line arg)
    (when (and vis-pos
               (= vis-pos (point)))
      (while (and (> pos (point))
                  (not (eobp)))
        (let (last-command)
          (line-move-visual 1 t)))
      (line-move-visual -1 t))
    (when (= pos (point))
      (if (= 0 (current-column))
          (skip-chars-forward " \t")
        (backward-char)
        (beginning-of-line)))))
(put 'ourcomments-move-beginning-of-line 'CUA 'move)

;;;###autoload
(defun ourcomments-move-end-of-line(arg)
  "Move point to end of line or after last non blank char.
See `end-of-line' for ARG.

Similar to `ourcomments-move-beginning-of-line' but for end of
line."
  (interactive "p")
  (or arg (setq arg 1))
  (let ((pos (point))
        vis-pos
        eol-pos)
    (when line-move-visual
      (let (last-command) (line-move-visual 1 t))
      (end-of-line)
      (setq vis-pos (point))
      (goto-char pos))
    (call-interactively 'end-of-line arg)
    (when (and vis-pos
               (= vis-pos (point)))
      (setq eol-pos (point))
      (beginning-of-line)
      (let (last-command) (line-move-visual 1 t))
      ;; move backwards if we moved to a new line
      (unless (= (point) eol-pos)
        (backward-char)))
    (when (= pos (point))
      (if (= (line-end-position) (point))
          (skip-chars-backward " \t")
        (forward-char)
        (end-of-line)))))
(put 'ourcomments-move-end-of-line 'CUA 'move)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Keymaps

(defun ourcomments-find-keymap-variables (key--- binding--- keymap---)
  "Return a list of matching keymap variables.
They should have key KEY--- bound to BINDING--- and have value
KEYMAP---.

Ignore `special-event-map', `global-map', `overriding-local-map'
and `overriding-terminal-local-map'."
  (let ((vars--- nil)
        (ancestors--- nil))
    (let ((parent (keymap-parent keymap---)))
      (while parent
        (setq ancestors--- (cons parent ancestors---))
        (setq parent (keymap-parent parent))))
    (mapatoms (lambda (symbol)
                (unless (memq symbol '(keymap---
                                       ancestors---
                                       vars---
                                       special-event-map
                                       global-map
                                       overriding-local-map
                                       overriding-terminal-local-map
                                       ))
                  (let (val)
                    (if (boundp symbol)
                        (setq val (symbol-value symbol))
                      (when (keymapp symbol)
                        (setq val (symbol-function symbol))))
                    (when (and val
                               (keymapp val)
                               (eq binding--- (lookup-key val key--- t)))
                      (if (equal val keymap---)
                          (push symbol vars---)
                        (when ancestors---
                          (catch 'found
                            (dolist (ancestor ancestors---)
                              (when (equal val ancestor)
                                (push symbol vars---)
                                (throw 'found nil)))))))))))
;;;     (let ((childs nil))
;;;       (dolist (var vars---)
;;;         (dolist (ancestor ancestors---)
;;;         (when (equal (keymap-parent var)
;;;                      (
    vars---))

;; This is modelled after `current-active-maps'.
(defun key-bindings (key &optional olp position)
  "Return list of bindings for key sequence KEY in current keymaps.
The first binding is the active binding and the others are
bindings shadowed by this in the order of their priority level
\(see Info node `(elisp) Searching Keymaps').

The entries in the list have the form

  \(BINDING (MAPS) MORE-INFO)

where BINDING is the command bound to and MAPS are matching maps
\(according to `ourcomments-find-keymap-variables').

MORE-INFO is a list with more information

  \(PRIORITY-LEVEL \[ACTIVE-WHEN])

where PRIORITY-LEVEL is a symbol matching the level where the
keymap is found and ACTIVE-WHEN is a symbol which must be non-nil
for the keymap to be active \(minor mode levels only)."
  ;;(message "\nkey-bindings %s %s %s" key olp position)
  (let* ((bindings nil)
        (maps (current-active-maps))
        map
        map-sym
        map-rec
        binding
        keymaps
        minor-maps
        where
        map-where
        where-map
        (local-map (current-local-map))
        (pt (or position (point)))
        (point-keymap (get-char-property pt 'keymap))
        (point-local-map (get-char-property pt 'local-map))
        )
    (setq keymaps
          (cons (list global-map 'global-map)
                keymaps))
    (when overriding-terminal-local-map
      (setq keymaps
            (cons (list overriding-terminal-local-map 'overriding-terminal-local-map)
                  keymaps)))
    (when overriding-local-map
      (setq keymaps
            (cons (list overriding-local-map 'overriding-local-map)
                  keymaps)))
    (unless (cdr keymaps)
      (when point-local-map
        (setq keymaps
              (cons (list point-local-map 'point-local-map)
                    keymaps)))
      ;; Fix-me:
      ;;/* If on a mode line string with a local keymap,

      (when local-map
        (setq keymaps
              (cons (list local-map 'local-map)
                    keymaps)))

      ;; Minor-modes
      ;;(message "================ Minor-modes")
      (dolist (list '(emulation-mode-map-alists
                      minor-mode-overriding-map-alist
                      minor-mode-map-alist))
        ;;(message "------- %s" list)
        (let ((alists (if (eq list 'emulation-mode-map-alists)
                          (symbol-value list)
                        (list (symbol-value list)))))
          (dolist (alist alists)
            ;;(message "\n(symbolp alist)=%s alist= %s (symbol-value alist)=%s" (symbolp alist) "dum" "dum2") ;alist "dummy");(when (symbolp alist) (symbol-value alist)))
            (when (symbolp alist)
              (setq alist (symbol-value alist)))
            (dolist (assoc alist)
              (let* (;(assoc (car alist-rec))
                     (var (when (consp assoc) (car assoc)))
                     (val (when (and (symbolp var)
                                     (boundp var))
                            (symbol-value var))))
                ;;(message "var= %s, val= %s" var val)
                (when (and
                       val
                       (or (not (eq list 'minor-mode-map-alist))
                           (not (assq var minor-mode-overriding-map-alist))))
                  ;;(message "** Adding this")
                  (setq minor-maps
                        (cons (list (cdr assoc) list var)
                              minor-maps)))
                )))))
      (dolist (map minor-maps)
        ;;(message "cdr map= %s" (cdr map))
        (setq keymaps
              (cons map
                    keymaps)))
      (when point-keymap
        (setq keymaps
              (cons (list point-keymap 'point-keymap)
                    keymaps))))

    ;; Fix-me: compare with current-active-maps
    (let ((ca-maps (current-active-maps))
          (wh-maps keymaps)
          ca
          wh)
      (while (or ca-maps wh-maps)
        (setq ca (car ca-maps))
        (setq wh (car wh-maps))
        (setq ca-maps (cdr ca-maps))
        (setq wh-maps (cdr wh-maps))
        ;;(message "\nca= %s" ca)
        ;;(message "cdr wh= %s" (cdr wh))
        (unless (equal ca (car wh))
          (error "Did not match: %s" (cdr wh)))))

    (while keymaps
      (setq map-rec (car keymaps))
      (setq map (car map-rec))
      (when (setq binding (lookup-key map key t))
        (setq map-sym (ourcomments-find-keymap-variables key binding map))
        (setq map-sym (delq 'map map-sym))
        (setq map-sym (delq 'local-map map-sym))
        (setq map-sym (delq 'point-keymap map-sym))
        (setq map-sym (delq 'point-local-map map-sym))
        (setq bindings (cons (list binding map-sym (cdr map-rec)) bindings)))
      (setq keymaps (cdr keymaps)))

    (nreverse bindings)))

(defun describe-keymap-placement (keymap-sym)
  "Find minor mode keymap KEYMAP-SYM in the keymaps searched for key lookup.
See Info node `Searching Keymaps'."
  ;;(info "(elisp) Searching Keymaps")
  (interactive (list (ourcomments-read-symbol "Describe minor mode keymap symbol"
                                              (lambda (sym)
                                                (and (boundp sym)
                                                     (keymapp (symbol-value sym)))))))
  (unless (symbolp keymap-sym)
    (error "Argument KEYMAP-SYM must be a symbol"))
  (unless (keymapp (symbol-value keymap-sym))
    (error "The value of argument KEYMAP-SYM must be a keymap"))
  (with-output-to-temp-buffer (help-buffer)
    (help-setup-xref (list #'describe-keymap-placement keymap-sym) (interactive-p))
    (with-current-buffer (help-buffer)
      (insert "Placement of keymap `")
      (insert-text-button (symbol-name keymap-sym)
                          'action
                          (lambda (btn)
                            (describe-variable keymap-sym)))
      (insert "'\nin minor modes activation maps:\n")
      (let (found)
        (dolist (map-root '(emulation-mode-map-alists
                            minor-mode-overriding-map-alist
                            minor-mode-map-alist
                            ))
          (dolist (emul-alist (symbol-value map-root))
            ;;(message "emul-alist=%s" emul-alist)
            (dolist (keymap-alist
                     (if (memq map-root '(emulation-mode-map-alists))
                         (symbol-value emul-alist)
                       (list emul-alist)))
              (let* ((map (cdr keymap-alist))
                     (first (catch 'first
                              (map-keymap (lambda (key def)
                                            (throw 'first (cons key def)))
                                          map)))
                     (key (car first))
                     (def (cdr first))
                     (keymap-variables (when (and key def)
                                         (ourcomments-find-keymap-variables
                                          (vector key) def map)))
                     (active-var (car keymap-alist))
                     )
                (assert (keymapp map))
                ;;(message "keymap-alist=%s, %s" keymap-alist first)
                ;;(message "active-var=%s, %s" active-var keymap-variables)
                (when (memq keymap-sym keymap-variables)
                  (setq found t)
                  (insert (format "\n`%s' " map-root))
                  (insert (propertize "<= Minor mode keymap list holding this map"
                                      'face 'font-lock-doc-face))
                  (insert "\n")
                  (when (symbolp emul-alist)
                    (insert (format "  `%s' " emul-alist))
                    (insert (propertize "<= Keymap alist variable" 'face 'font-lock-doc-face))
                    (insert "\n"))
                  ;;(insert (format "    `%s'\n" keymap-alist))
                  (insert (format "      `%s' " active-var))
                  (insert (propertize "<= Activation variable" 'face 'font-lock-doc-face))
                  (insert "\n")
                  )))))
        (unless found
          (insert (propertize "Not found." 'face 'font-lock-warning-face)))
        ))))

;; This is a replacement for describe-key-briefly.
;;(global-set-key [f1 ?c] 'describe-key-and-map-briefly)
;;;###autoload
(defun describe-key-and-map-briefly (&optional key insert untranslated)
  "Try to print names of keymap from which KEY fetch its definition.
Look in current active keymaps and find keymap variables with the
same value as the keymap where KEY is bound.  Print a message
with those keymap variable names.  Return a list with the keymap
variable symbols.

When called interactively prompt for KEY.

INSERT and UNTRANSLATED should normall be nil (and I am not sure
what they will do ;-)."
  ;; >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  ;; From describe-key-briefly. Keep this as it is for easier update.
  (interactive
   (let ((enable-disabled-menus-and-buttons t)
	 (cursor-in-echo-area t)
	 saved-yank-menu)
     (unwind-protect
	 (let (key)
	   ;; If yank-menu is empty, populate it temporarily, so that
	   ;; "Select and Paste" menu can generate a complete event.
	   (when (null (cdr yank-menu))
	     (setq saved-yank-menu (copy-sequence yank-menu))
	     (menu-bar-update-yank-menu "(any string)" nil))
	   (setq key (read-key-sequence "Describe key (or click or menu item): "))
	   ;; If KEY is a down-event, read and discard the
	   ;; corresponding up-event.  Note that there are also
	   ;; down-events on scroll bars and mode lines: the actual
	   ;; event then is in the second element of the vector.
	   (and (vectorp key)
		(let ((last-idx (1- (length key))))
		  (and (eventp (aref key last-idx))
		       (memq 'down (event-modifiers (aref key last-idx)))))
		(read-event))
	   (list
	    key
	    (if current-prefix-arg (prefix-numeric-value current-prefix-arg))
	    1
            ))
       ;; Put yank-menu back as it was, if we changed it.
       (when saved-yank-menu
	 (setq yank-menu (copy-sequence saved-yank-menu))
	 (fset 'yank-menu (cons 'keymap yank-menu))))))
  (if (numberp untranslated)
      (setq untranslated (this-single-command-raw-keys)))
  (let* ((event (if (and (symbolp (aref key 0))
			 (> (length key) 1)
			 (consp (aref key 1)))
		    (aref key 1)
		  (aref key 0)))
	 (modifiers (event-modifiers event))
	 (standard-output (if insert (current-buffer) t))
	 (mouse-msg (if (or (memq 'click modifiers) (memq 'down modifiers)
			    (memq 'drag modifiers)) " at that spot" ""))
	 (defn (key-binding key t))
	 key-desc)
    ;; Handle the case where we faked an entry in "Select and Paste" menu.
    (if (and (eq defn nil)
	     (stringp (aref key (1- (length key))))
	     (eq (key-binding (substring key 0 -1)) 'yank-menu))
	(setq defn 'menu-bar-select-yank))
    ;; Don't bother user with strings from (e.g.) the select-paste menu.
    (if (stringp (aref key (1- (length key))))
	(aset key (1- (length key)) "(any string)"))
    (if (and (> (length untranslated) 0)
	     (stringp (aref untranslated (1- (length untranslated)))))
	(aset untranslated (1- (length untranslated)) "(any string)"))
    ;; Now describe the key, perhaps as changed.
    (setq key-desc (help-key-description key untranslated))
    ;;
    ;; End of part from describe-key-briefly.
    ;; <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

    ;;(message "bindings=%s" (key-bindings key)) (sit-for 2)
    ;; Find the keymap:
    (let* ((maps (current-active-maps))
           ret
           lk)
      (if (or (null defn) (integerp defn) (equal defn 'undefined))
          (setq ret 'not-defined)
        (catch 'mapped
          (while (< 1 (length maps))
            (setq lk (lookup-key (car maps) key t))
            (when (and lk (not (numberp lk)))
              (setq ret (ourcomments-find-keymap-variables key lk (car maps)))
              (when ret
                (throw 'mapped (car maps))))
            (setq maps (cdr maps))))
        (unless ret
          (setq lk (lookup-key global-map key t))
          (when (and lk (not (numberp lk)))
            (setq ret '(global-map)))))
      (cond
       ((eq ret 'not-defined)
        (message "%s%s not defined in any keymap" key-desc mouse-msg))
       ((listp ret)
        (if (not ret)
            (message "%s%s is bound to `%s', but don't know where"
                     key-desc mouse-msg defn)
          (if (= 1 (length ret))
              (message "%s%s is bound to `%s' in `%s'"
                       key-desc mouse-msg defn (car ret))
            (message "%s%s is bound to `%s' in keymap variables `%s'"
                     key-desc mouse-msg defn ret))))
       (t
        (error "ret=%s" ret)))
      ret)))

;; (ourcomments-find-keymap-variables (current-local-map))
;; (keymapp 'ctl-x-4-prefix)
;; (equal 'ctl-x-4-prefix (current-local-map))
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Wrapping

;;;###autoload
(defcustom wrap-to-fill-left-marg nil
  "Left margin handling for `wrap-to-fill-column-mode'.
Used by `wrap-to-fill-column-mode'. If nil then center the
display columns. Otherwise it should be a number which will be
the left margin."
  :type '(choice (const :tag "Center" nil)
                 (integer :tag "Left margin"))
  :group 'convenience)
(make-variable-buffer-local 'wrap-to-fill-left-marg)

(defvar wrap-to-fill-left-marg-use 0)
(make-variable-buffer-local 'wrap-to-fill-left-marg-use)
(put 'wrap-to-fill-left-marg-use 'permanent-local t)

;;;###autoload
(defcustom wrap-to-fill-left-marg-modes
  '(text-mode
    fundamental-mode)
  "Major modes where `wrap-to-fill-left-margin' may be nil."
  :type '(repeat commandp)
  :group 'convenience)

(defun wrap-to-fill-set-prefix (min max)
  ;;(wrap-to-fill-set-prefix-1 min max)
  )

(defun wrap-to-fill-set-prefix-1 (min max)
  "Set `wrap-prefix' text property from point MIN to MAX."
  ;; Fix-me: If first word gets wrapped we have a problem.
  ;;(message "wrap-to-fill-set-prefix here")
  (let ((here (point))
        beg-pos
        end-pos
        ind-str
        max-word-len
        (inhibit-field-text-motion t)
        )
    (goto-char min)
    (forward-line 0)
    (when (< (point) min) (forward-line))
    (mumamo-with-buffer-prepared-for-jit-lock
     (while (and (<= (point) max)
                 (< (point) (point-max)))
       (setq beg-pos (point))
       (setq end-pos (line-end-position))
       (when (equal (get-text-property beg-pos 'wrap-prefix)
                    (get-text-property beg-pos 'wrap-to-fill-prefix))
         (skip-chars-forward "[:blank:]")
         (setq ind-str (buffer-substring-no-properties beg-pos (point)))
         (setq max-word-len
               (apply
                'max
                0
                (mapcar (lambda (word)
                              (length word))
                            (split-string
                             (buffer-substring-no-properties
                              (point) end-pos)))))
         ;;(message "max-word-len=%s, %s, %s" max-word-len (length ind-str) (buffer-substring-no-properties (point) end-pos))
         (unless (< fill-column (+ max-word-len
                                   ;;(current-indentation)
                                   (length ind-str)
                                   5 ;; Fix-me: From where?? This is the diff between the usable area and fill-column ...
                                   ))
           (put-text-property beg-pos end-pos 'wrap-prefix ind-str)
           (put-text-property beg-pos end-pos 'wrap-to-fill-prefix ind-str)))
       (forward-line)))
    (goto-char here)))

(defvar wrap-to-fill-after-change-range nil)

(defun wrap-to-fill-after-change (min max old-len)
  "For `after-change-functions'.
See the hook for MIN, MAX and OLD-LEN."
  (let ((here (point))
        (inhibit-field-text-motion t))
    (goto-char min)
    (setq min (line-beginning-position))
    (goto-char max)
    (setq max (line-end-position))
    ;;(wrap-to-fill-set-prefix min max)
    (wrap-to-fill-save-min-max min max)
    ))

(defun wrap-to-fill-save-min-max (min max)
  (let* ((old-min (car wrap-to-fill-after-change-range))
         (old-max (cdr wrap-to-fill-after-change-range))
         (new-min (if old-min (min old-min min) min))
         (new-max (if old-max (max old-max max) max)))
    (setq wrap-to-fill-after-change-range (cons new-min new-max))))

(defun wrap-to-fill-post-command ()
  (let* ((min (car wrap-to-fill-after-change-range))
         (max (cdr wrap-to-fill-after-change-range)))
    (setq wrap-to-fill-after-change-range nil)
    (wrap-to-fill-set-prefix min max)))

(defun wrap-to-fill-scroll-fun (window start-pos)
  "For `window-scroll-functions'.
See the hook for WINDOW and START-POS."
  (let ((min (or start-pos (window-start window)))
        (max (window-end window t)))
    (wrap-to-fill-save-min-max min max)))
    ;;(wrap-to-fill-set-prefix min max)))

(defun wrap-to-fill-wider ()
  "Increase `fill-column' with 10."
  (interactive)
  (setq fill-column (+ fill-column 10))
  (wrap-to-fill-set-values))

(defun wrap-to-fill-narrower ()
  "Decrease `fill-column' with 10."
  (interactive)
  (setq fill-column (- fill-column 10))
  (wrap-to-fill-set-values))

(defvar wrap-to-fill-column-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [(control ?c) right] 'wrap-to-fill-wider)
    (define-key map [(control ?c) left] 'wrap-to-fill-narrower)
    map))

;;;###autoload
(define-minor-mode wrap-to-fill-column-mode
  "Use `fill-column' display columns in buffer windows.
By default the display columns are centered, but see the option
`wrap-to-fill-left-marg'.

Note 1: When turning this on `visual-line-mode' is also turned on. This
is not reset when turning off this mode.

Note 2: The text property `wrap-prefix' is set by this mode to
indent continuation lines.  This is not recorded in the undo
list.

Key bindings added by this minor mode:

\\{wrap-to-fill-column-mode-map}"
  ;; Fix-me: make the `wrap-prefix' behavior an option.
  :lighter " WrapFill"
  :group 'convenience
  ;;(message "wrap-to-fill-column-mode here %s" wrap-to-fill-column-mode)
  (if wrap-to-fill-column-mode
      (let* ((win (get-buffer-window (current-buffer)))
             (win-margs (when win (window-margins win))))
        (when win-margs
          (setq wrap-to-fill-left-marg (or (car win-margs)
                                           wrap-to-fill-left-marg)))
        (setq wrap-to-fill-left-marg-use wrap-to-fill-left-marg)
        (unless (or wrap-to-fill-left-marg-use
                    (memq major-mode wrap-to-fill-left-marg-modes))
          (setq wrap-to-fill-left-marg-use
                (default-value 'wrap-to-fill-left-marg-use)))
        (add-hook 'window-configuration-change-hook 'wrap-to-fill-set-values nil t)
        (add-hook 'after-change-functions 'wrap-to-fill-after-change nil t)
        (add-hook 'window-scroll-functions 'wrap-to-fill-scroll-fun nil t)
        (add-hook 'post-command-hook 'wrap-to-fill-post-command nil t)
        ;;(add-hook 'post-command-hook window-scroll-functions 'wrap-to-fill-scroll-fun nil t)
        (if (fboundp 'visual-line-mode)
            (visual-line-mode 1)
          (longlines-mode 1))
        (dolist (window (get-buffer-window-list (current-buffer)))
          (wrap-to-fill-scroll-fun window nil)))
    (remove-hook 'window-configuration-change-hook 'wrap-to-fill-set-values t)
    (remove-hook 'after-change-functions 'wrap-to-fill-after-change t)
    (remove-hook 'window-scroll-functions 'wrap-to-fill-scroll-fun t)
    (if (fboundp 'visual-line-mode)
        (visual-line-mode -1)
      (longlines-mode -1))
    (let ((here (point))
          (inhibit-field-text-motion t)
          beg-pos
          end-pos)
      (mumamo-with-buffer-prepared-for-jit-lock
       (save-restriction
         (widen)
         (goto-char (point-min))
         (while (< (point) (point-max))
           (setq beg-pos (point))
           (setq end-pos (line-end-position))
           (when (equal (get-text-property beg-pos 'wrap-prefix)
                        (get-text-property beg-pos 'wrap-to-fill-prefix))
             (remove-list-of-text-properties
              beg-pos end-pos
              '(wrap-prefix)))
           (forward-line))
         (remove-list-of-text-properties
          (point-min) (point-max)
          '(wrap-to-fill-prefix)))
       (goto-char here))))
  (wrap-to-fill-set-values))
(put 'wrap-to-fill-column-mode 'permanent-local t)

;; Fix-me: There is a confusion between buffer and window margins
;; here. Also the doc says that left-margin-width and dito right may
;; be nil. However they seem to be 0 by default, but when displaying a
;; buffer in a window then window-margins returns (nil).
(defun wrap-to-fill-set-values ()
  (condition-case err
      (wrap-to-fill-set-values-1)
    (error (message "ERROR wrap-to-fill-set-values-1: %s" (error-message-string err)))))

(defun wrap-to-fill-set-values-1 ()
  "Use `fill-column' display columns in buffer windows."
  ;;(message "wrap-to-fill-set-values window-configuration-change-hook=%s, wrap-to-fill-column-mode=%s, cb=%s" window-configuration-change-hook wrap-to-fill-column-mode (current-buffer))
  (let ((buf-windows (get-buffer-window-list (current-buffer))))
    ;;(message "buf-windows=%s" buf-windows)
    (dolist (win buf-windows)
      (if wrap-to-fill-column-mode
          (let* ((edges (window-edges win))
                 (win-width (- (nth 2 edges) (nth 0 edges)))
                 (extra-width (- win-width fill-column))
                 (left-marg (if wrap-to-fill-left-marg-use
                                wrap-to-fill-left-marg-use
                              (- (/ extra-width 2) 1)))
                 (right-marg (- win-width fill-column left-marg))
                 (win-margs (window-margins win))
                 (old-left (or (car win-margs) 0))
                 (old-right (or (cdr win-margs) 0)))
            ;;(message "left-marg=%s, right-marg=%s, old-left=%s, old-right=%s" left-marg right-marg old-left old-right)
            (unless (> left-marg 0) (setq left-marg 0))
            (unless (> right-marg 0) (setq right-marg 0))
            (unless (and (= old-left left-marg)
                         (= old-right right-marg))
              (set-window-margins win left-marg right-marg)))
        (set-window-buffer win (current-buffer))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Fringes.

(defvar better-bottom-angles-defaults nil)
(defun better-fringes-bottom-angles (on)
  ;;(bottom bottom-left-angle bottom-right-angle top-right-angle top-left-angle)
  (if (not on)
      (when better-bottom-angles-defaults
        (set-default 'fringe-indicator-alist better-bottom-angles-defaults))
    (unless better-bottom-angles-defaults
      (setq better-bottom-angles-defaults fringe-indicator-alist))
    (let ((better
           '(bottom
             bottom-right-angle bottom-right-angle
             bottom-left-angle bottom-left-angle
             ))
          ;;(indicators (copy-list fringe-indicator-alist)))
          (indicators (copy-sequence fringe-indicator-alist)))
      (setq indicators (assq-delete-all 'bottom indicators))
      (set-default 'fringe-indicator-alist (cons better indicators)))))

(defun better-fringes-faces (face face-important)
  (dolist (bitmap '(bottom-left-angle
                    bottom-right-angle
                    top-left-angle
                    top-right-angle

                    right-curly-arrow
                    left-arrow right-arrow
                    left-curly-arrow right-curly-arrow
                    up-arrow
                    down-arrow
                    left-bracket right-bracket
                    empty-line))
    (set-fringe-bitmap-face bitmap face))
  (dolist (bitmap '(right-triangle
                    question-mark))
    (set-fringe-bitmap-face bitmap face-important)))

(defface better-fringes-bitmap
  '((t (:foreground "khaki")))
  "Face for bitmap fringes."
  :group 'better-fringes
  :group 'nxhtml)

(defface better-fringes-important-bitmap
  '((t (:foreground "red")))
  "Face for bitmap fringes."
  :group 'better-fringes
  :group 'nxhtml)

;;;###autoload
(define-minor-mode better-fringes-mode
  "Choose another fringe bitmap color and bottom angle."
  :global t
  :group 'better-fringes
  (if better-fringes-mode
      (progn
        (better-fringes-faces 'better-fringes-bitmap
                              'better-fringes-important-bitmap)
        (better-fringes-bottom-angles t))
    (better-fringes-faces nil nil)
    (better-fringes-bottom-angles nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Misc.

;;;###autoload
(defun find-emacs-other-file (display-file)
  "Find corresponding file to source or installed elisp file.
If you have checked out and compiled Emacs yourself you may have
Emacs lisp files in two places, the checked out source tree and
the installed Emacs tree.  If buffer contains an Emacs elisp file
in one of these places then find the corresponding elisp file in
the other place. Return the file name of this file.

When DISPLAY-FILE is non-nil display this file in other window
and go to the same line number as in the current buffer."
  (interactive (list t))
  (unless (buffer-file-name)
    (error "This buffer is not visiting a file"))
  (unless source-directory
    (error "Can't find the checked out Emacs sources"))
  (let* ((installed-directory (file-name-as-directory
                               (expand-file-name ".." exec-directory)))
         (relative-installed (file-relative-name
                              (buffer-file-name) installed-directory))
         (relative-source (file-relative-name
                           (buffer-file-name) source-directory))
         (name-nondir (file-name-nondirectory (buffer-file-name)))
         source-file
         installed-file
         other-file
         (line-num (save-restriction
                     (widen)
                     (line-number-at-pos))))
    (cond
     ((and relative-installed
           (not (string= name-nondir relative-installed))
           (not (file-name-absolute-p relative-installed))
           (not (string= ".." (substring relative-installed 0 2))))
      (setq source-file (expand-file-name relative-installed source-directory)))
     ((and relative-source
           (not (string= name-nondir relative-source))
           (not (file-name-absolute-p relative-source))
           (not (string= ".." (substring relative-source 0 2))))
      (setq installed-file (expand-file-name relative-source installed-directory))))
    (setq other-file (or source-file installed-file))
    (unless other-file
      (error "This file is not in Emacs source or installed lisp tree"))
    (unless (file-exists-p other-file)
      (error "Can't find the corresponding file %s" other-file))
    (when display-file
      (find-file-other-window other-file)
      (goto-line line-num))
    other-file))

;;;###autoload
(defun ourcomments-ediff-files (def-dir file-a file-b)
  "In directory DEF-DIR run `ediff-files' on files FILE-A and FILE-B.
The purpose of this function is to make it eaiser to start
`ediff-files' from a shell through Emacs Client.

This is used in EmacsW32 in the file ediff.cmd where Emacs Client
is called like this:

  @%emacs_client% -e \"(setq default-directory \\\"%emacs_cd%\\\")\"
  @%emacs_client% -n  -e \"(ediff-files \\\"%f1%\\\" \\\"%f2%\\\")\"

It can of course be done in a similar way with other shells."
  (let ((default-directory def-dir))
    (ediff-files file-a file-b)))


(defun ourcomments-latest-changelog ()
  "not ready"
  (let ((changelogs
         '("ChangeLog"
           "admin/ChangeLog"
           "doc/emacs/ChangeLog"
           "doc/lispintro/ChangeLog"
           "doc/lispref/ChangeLog"
           "doc/man/ChangeLog"
           "doc/misc/ChangeLog"
           "etc/ChangeLog"
           "leim/ChangeLog"
           "lib-src/ChangeLog"
           "lisp/ChangeLog"
           "lisp/erc/ChangeLog"
           "lisp/gnus/ChangeLog"
           "lisp/mh-e/ChangeLog"
           "lisp/org/ChangeLog"
           "lisp/url/ChangeLog"
           "lwlib/ChangeLog"
           "msdos/ChangeLog"
           "nextstep/ChangeLog"
           "nt/ChangeLog"
           "oldXMenu/ChangeLog"
           "src/ChangeLog"
           "test/ChangeLog"))
	(emacs-root (expand-file-name ".." exec-directory)
        ))))

(defun ourcomments-read-symbol (prompt predicate)
  "Basic function for reading a symbol for describe-* functions.
Prompt with PROMPT and show only symbols satisfying function
PREDICATE.  PREDICATE takes one argument, the symbol."
  (let* ((symbol (symbol-at-point))
	 (enable-recursive-minibuffers t)
	 val)
    (when predicate
      (unless (and symbol
                   (symbolp symbol)
                   (funcall predicate symbol))
        (setq symbol nil)))
    (setq val (completing-read (if symbol
                                   (format
                                    "%s (default %s): " prompt symbol)
                                 (format "%s: " prompt))
                               obarray
                               predicate
                               t nil nil
                               (if symbol (symbol-name symbol))))
    (if (equal val "") symbol (intern val))))

(defun ourcomments-command-at-point ()
  (let ((fun (function-called-at-point)))
    (when (commandp fun)
      fun)))

;;;###autoload
(defun describe-command (command)
  "Like `describe-function', but prompts only for interactive commands."
  (interactive
   (let ((fn (ourcomments-command-at-point))
	 (enable-recursive-minibuffers t)
	 val)
     (setq val (completing-read (if fn
				    (format "Describe command (default %s): " fn)
				  "Describe command: ")
				obarray 'commandp t nil nil
				(and fn (symbol-name fn))))
     (list (if (equal val "")
	       fn (intern val)))))
  (describe-function command))


(defun buffer-narrowed-p ()
  "Return non-nil if the current buffer is narrowed."
  (/= (buffer-size)
      (- (point-max)
         (point-min))))

;;;###autoload
(defun narrow-to-comment ()
  (interactive)
  (let* ((here (point-marker))
         (size 1000)
         (beg (progn (forward-comment (- size))
                     ;; It looks like the wrong syntax-table is used here:
                     ;;(message "skipped %s " (skip-chars-forward "[:space:]"))
                     (message "skipped %s " (skip-chars-forward " \t\r\n"))
                     (point)))
         (end (progn (forward-comment size)
                     ;;(message "skipped %s " (skip-chars-backward "[:space:]"))
                     (message "skipped %s " (skip-chars-backward " \t\r\n"))
                     (point))))
    (goto-char here)
    (if (not (and (>= here beg)
                  (<= here end)))
        (error "Not in a comment")
      (narrow-to-region beg end))))

(defvar describe-symbol-alist nil)

(defun describe-symbol-add-known(property description)
  (when (assq property describe-symbol-alist)
    (error "Already known property"))
  (setq describe-symbol-alist
        (cons (list property description)
              describe-symbol-alist)))

;;(describe-symbol-add-known 'variable-documentation "Doc for variable")
;;(describe-symbol-add-known 'cl-struct-slots "defstruct slots")

(defun property-list-keys (plist)
  "Return list of key names in property list PLIST."
  (let ((keys))
    (while plist
      (setq keys (cons (car plist) keys))
      (setq plist (cddr plist)))
    keys))

(defun ourcomments-symbol-type (symbol)
  "Return a list of types where symbol SYMBOL is used.
The can include 'variable, 'function and variaus 'cl-*."
  (symbol-file symbol)
  )

(defun ourcomments-defstruct-p (symbol)
  "Return non-nil if symbol SYMBOL is a CL defstruct."
  (let ((plist (symbol-plist symbol)))
    (and (plist-member plist 'cl-struct-slots)
         (plist-member plist 'cl-struct-type)
         (plist-member plist 'cl-struct-include)
         (plist-member plist 'cl-struct-print))))

(defun ourcomments-defstruct-slots (symbol)
  (unless (ourcomments-defstruct-p symbol)
    (error "Not a CL defstruct symbol: %s" symbol))
  (let ((cl-struct-slots (get symbol 'cl-struct-slots)))
    (delq 'cl-tag-slot
          (loop for rec in cl-struct-slots
                collect (nth 0 rec)))))

;; (ourcomments-defstruct-slots 'ert-test)

(defun ourcomments-defstruct-file (symbol)
  (unless (ourcomments-defstruct-p symbol)
    (error "Not a CL defstruct symbol: %s" symbol))
  )

(defun ourcomments-member-defstruct (symbol)
  "Return defstruct name if member."
  (when (and (functionp symbol)
             (plist-member (symbol-plist symbol) 'cl-compiler-macro))
    (let* (in-defstruct
           (symbol-file (symbol-file symbol))
           buf
           was-here)
      (unless symbol-file
        (error "Can't check if defstruct member since don't know symbol file"))
      (setq buf (find-buffer-visiting symbol-file))
      (setq was-here (with-current-buffer buf (point)))
      (unless buf
        (setq buf (find-file-noselect symbol-file)))
      (with-current-buffer buf
        (save-restriction
          (widen)
          (let* ((buf-point (find-definition-noselect symbol nil)))
            (goto-char (cdr buf-point))
            (save-match-data
              (when (looking-at "(defstruct (?\\(\\(?:\\sw\\|\\s_\\)+\\)")
                (setq in-defstruct (match-string-no-properties 1))))))
        (if was-here
            (goto-char was-here)
          (kill-buffer (current-buffer))))
      in-defstruct)))
;; (ourcomments-member-defstruct 'ert-test-name)
;; (ourcomments-member-defstruct 'ert-test-error-condition)

(defun ourcomments-custom-group-p (symbol)
  (and (intern-soft symbol)
       (or (and (get symbol 'custom-loads)
                (not (get symbol 'custom-autoload)))
           (get symbol 'custom-group))))

;;;###autoload
(defun describe-custom-group (symbol)
  "Describe customization group SYMBOL."
  (interactive
   (list
    (ourcomments-read-symbol "Customization group"
                             'ourcomments-custom-group-p)))
  ;; Fix-me:
  (message "g=%s" symbol))
;; nxhtml

;; Added this to current-load-list in cl-macs.el
;; (describe-defstruct 'ert-stats)
;;;###autoload
(defun describe-defstruct (symbol)
  (interactive (list (ourcomments-read-symbol "Describe defstruct"
                                              'ourcomments-defstruct-p)))
  (if (not (ourcomments-defstruct-p symbol))
      (message "%s is not a CL defstruct." symbol)
  (with-output-to-temp-buffer (help-buffer)
    (help-setup-xref (list #'describe-defstruct symbol) (interactive-p))
    (with-current-buffer (help-buffer)
      (insert "This is a description of a CL thing.")
      (insert "\n\n")
      (insert (format "%s is a CL `defstruct'" symbol))
      (let ((file (symbol-file symbol)))
        (if file
            ;; Fix-me: .elc => .el
            (let ((name (file-name-nondirectory file)))
              (insert "defined in file %s.\n" (file-name-nondirectory file)))
          (insert ".\n")))
      (insert "\n\nIt has the following slot functions:\n")
      (let ((num-slot-funs 0)
            (slots (ourcomments-defstruct-slots symbol)))
        (dolist (slot slots)
          (if (not (fboundp (intern-soft (format "%s-%s" symbol slot))))
              (insert (format "    Do not know function for slot %s\n" slot))
            (setq num-slot-funs (1+ num-slot-funs))
            (insert (format "    `%s-%s'\n" symbol slot))))
        (unless (= num-slot-funs (length slots))
          (insert "  No information about some slots, maybe :conc-name was used\n")))))))

;;(defun describe-deftype (type)
;;;###autoload
(defun describe-symbol(symbol)
  "Show information about SYMBOL.
Show SYMBOL plist and whether is is a variable or/and a
function."
  (interactive (list (ourcomments-read-symbol "Describe symbol" nil)))
;;;    (let* ((s (symbol-at-point))
;;;           (val (completing-read (if (and (symbolp s)
;;;                                          (not (eq s nil)))
;;;                                     (format
;;;                                      "Describe symbol (default %s): " s)
;;;                                   "Describe symbol: ")
;;;                                 obarray
;;;                                 nil
;;;                                 t nil nil
;;;                                 (if (symbolp s) (symbol-name s)))))
;;;      (list (if (equal val "") s (intern val)))))
  (require 'apropos)
  (with-output-to-temp-buffer (help-buffer)
    (help-setup-xref (list #'describe-symbol symbol) (interactive-p))
    (with-current-buffer (help-buffer)
      (insert (format "Description of symbol %s\n\n" symbol))
      (when (plist-get (symbol-plist symbol) 'cl-compiler-macro)
        (insert "(Looks like a CL thing.)\n"))
      (if (boundp symbol)
          (insert (format "- There is a variable `%s'.\n" symbol))
        (insert "- This symbol is not a variable.\n"))
      (if (fboundp symbol)
          (progn
            (insert (format "- There is a function `%s'" symbol))
            (when (ourcomments-member-defstruct symbol)
              (let ((ds-name (ourcomments-member-defstruct symbol)))
                (insert "\n  which is a member of defstruct ")
                (insert-text-button (format "%s" ds-name)
                                    'symbol (intern-soft ds-name)
                                    'action (lambda (button)
                                              (describe-symbol
                                               (button-get button 'symbol))))))
            (insert ".\n"))
        (insert "- This symbol is not a function.\n"))
      (if (facep symbol)
          (insert (format "- There is a face `%s'.\n" symbol))
        (insert "- This symbol is not a face.\n"))
      (if (ourcomments-custom-group-p symbol)
          (progn
            (insert "- There is a customization group ")
            (insert-text-button (format "%s" symbol)
                                'symbol symbol
                                'action (lambda (button)
                                          (describe-custom-group
                                           (button-get button 'symbol))))
            (insert ".\n"))
        (insert "- This symbol is not a customization group.\n"))
      (if (ourcomments-defstruct-p symbol)
          (progn
            (insert (format "- There is a CL defstruct %s with setf-able slots:\n" symbol))
            (let ((num-slot-funs 0)
                  (slots (ourcomments-defstruct-slots symbol)))
              (dolist (slot slots)
                (if (not (fboundp (intern-soft (format "%s-%s" symbol slot))))
                    (insert (format "    Do not know function for slot %s\n" slot))
                  (setq num-slot-funs (1+ num-slot-funs))
                  (insert (format "    `%s-%s'\n" symbol slot))))
              (unless (= num-slot-funs (length slots))
                (insert "  No information about some slots, maybe :conc-name was used\n"))))
        (insert "- This symbol is not a CL defstruct.\n"))
      (insert "\n")
      (let* ((pl (symbol-plist symbol))
             (pl-not-known (property-list-keys pl))
             any-known)
        (if (not pl)
            (insert (format "Symbol %s has no property list\n\n" symbol))
          ;; Known properties
          (dolist (rec describe-symbol-alist)
            (let ((prop (nth 0 rec))
                  (desc (nth 1 rec)))
              (when (plist-member pl prop)
                (setq any-known (cons prop any-known))
                (setq pl-not-known (delq prop pl-not-known))
                (insert
                 "The following keys in the property list are known:\n\n")
                (insert (format "* %s: %s\n" prop desc))
                )))
          (unless any-known
            (insert "The are no known keys in the property list.\n"))
          (let ((pl (ourcomments-format-plist pl "\n  ")))
            ;;(insert (format "plist=%s\n" (symbol-plist symbol)))
            ;;(insert (format "pl-not-known=%s\n" pl-not-known))
            (insert "\nFull property list:\n\n (")
            (insert (propertize pl 'face 'default))
            (insert ")\n\n")))))))

(defun ourcomments-format-plist (pl sep &optional compare)
  (when (symbolp pl)
    (setq pl (symbol-plist pl)))
  (let (p desc p-out)
    (while pl
      (setq p (format "%s" (car pl)))
      (if (or (not compare) (string-match apropos-regexp p))
          (if apropos-property-face
              (put-text-property 0 (length (symbol-name (car pl)))
                                 'face apropos-property-face p))
        (setq p nil))
      (if p
          (progn
            (and compare apropos-match-face
                 (put-text-property (match-beginning 0) (match-end 0)
                                    'face apropos-match-face
                                    p))
            (setq desc (pp-to-string (nth 1 pl)))
            (setq desc (split-string desc "\n"))
            (if (= 1 (length desc))
                (setq desc (concat " " (car desc)))
              (let* ((indent "    ")
                     (ind-nl (concat "\n" indent)))
                (setq desc
                      (concat
                       ind-nl
                       (mapconcat 'identity desc ind-nl)))))
            (setq p-out (concat p-out (if p-out sep) p desc))))
      (setq pl (nthcdr 2 pl)))
    p-out))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; ido

(defvar ourcomments-ido-visit-method nil)

;;;###autoload
(defun ourcomments-ido-buffer-other-window ()
  "Show buffer in other window."
  (interactive)
  (setq ourcomments-ido-visit-method 'other-window)
  (call-interactively 'ido-exit-minibuffer))

;;;###autoload
(defun ourcomments-ido-buffer-other-frame ()
  "Show buffer in other frame."
  (interactive)
  (setq ourcomments-ido-visit-method 'other-frame)
  (call-interactively 'ido-exit-minibuffer))

;;;###autoload
(defun ourcomments-ido-buffer-raise-frame ()
  "Raise frame showing buffer."
  (interactive)
  (setq ourcomments-ido-visit-method 'raise-frame)
  (call-interactively 'ido-exit-minibuffer))

(defun ourcomments-ido-switch-buffer-or-next-entry ()
  (interactive)
  (if (active-minibuffer-window)
      (ido-next-match)
    (ido-switch-buffer)))

(defun ourcomments-ido-mode-advice()
  (message "ourcomments-ido-mode-advice running")
  (when (memq ido-mode '(both buffer))
    (let ((the-ido-minor-map (cdr ido-minor-mode-map-entry)))
      ;;(define-key the-ido-minor-map [(control tab)] 'ido-switch-buffer))
      (define-key the-ido-minor-map [(control tab)] 'ourcomments-ido-switch-buffer-or-next-entry))
    (dolist (the-map (list ido-buffer-completion-map ido-completion-map ido-common-completion-map))
      (when the-map
        (let ((map the-map))
          (define-key map [(control tab)]       'ido-next-match)
          (define-key map [(control shift tab)] 'ido-prev-match)
          (define-key map [(control backtab)]   'ido-prev-match)
          (define-key map [(shift return)]   'ourcomments-ido-buffer-other-window)
          (define-key map [(control return)] 'ourcomments-ido-buffer-other-frame)
          (define-key map [(meta return)]   'ourcomments-ido-buffer-raise-frame))))))

;; (defun ourcomments-ido-setup-completion-map ()
;;   "Set up the keymap for `ido'."

;;   (ourcomments-ido-mode-advice)

;;   ;; generated every time so that it can inherit new functions.
;;   (let ((map (make-sparse-keymap))
;; 	(viper-p (if (boundp 'viper-mode) viper-mode)))

;;     (when viper-p
;;       (define-key map [remap viper-intercept-ESC-key] 'ignore))

;;     (cond
;;      ((memq ido-cur-item '(file dir))
;;       (when ido-context-switch-command
;; 	(define-key map "\C-x\C-b" ido-context-switch-command)
;; 	(define-key map "\C-x\C-d" 'ignore))
;;       (when viper-p
;; 	(define-key map [remap viper-backward-char] 'ido-delete-backward-updir)
;; 	(define-key map [remap viper-del-backward-char-in-insert] 'ido-delete-backward-updir)
;; 	(define-key map [remap viper-delete-backward-word] 'ido-delete-backward-word-updir))
;;       (set-keymap-parent map
;; 			 (if (eq ido-cur-item 'file)
;; 			     ido-file-completion-map
;; 			   ido-file-dir-completion-map)))

;;      ((eq ido-cur-item 'buffer)
;;       (when ido-context-switch-command
;; 	(define-key map "\C-x\C-f" ido-context-switch-command))
;;       (set-keymap-parent map ido-buffer-completion-map))

;;      (t
;;       (set-keymap-parent map ido-common-completion-map)))

;;     ;; ctrl-tab etc
;;     (define-key map [(control tab)]       'ido-next-match)
;;     (define-key map [(control shift tab)] 'ido-prev-match)
;;     (define-key map [(control backtab)]   'ido-prev-match)
;;     (define-key map [(shift return)]   'ourcomments-ido-buffer-other-window)
;;     (define-key map [(control return)] 'ourcomments-ido-buffer-other-frame)
;;     (define-key map [(meta return)]   'ourcomments-ido-buffer-raise-frame)

;;     (setq ido-completion-map map)))

;; (defadvice ido-setup-completion-map (around
;;                                      ourcomments-advice-ido-setup-completion-map
;;                                      disable)
;;   (setq ad-return-value (ourcomments-ido-setup-completion-map))
;;   )

;;(add-hook 'ido-setup-hook 'ourcomments-ido-mode-advice)
;;(remove-hook 'ido-setup-hook 'ourcomments-ido-mode-advice)
(defvar ourcomments-ido-adviced nil)
(unless ourcomments-ido-adviced
(defadvice ido-mode (after
                     ourcomments-advice-ido-mode
                     ;;activate
                     ;;compile
                     disable)
  "Add C-tab to ido buffer completion."
  (ourcomments-ido-mode-advice)
  ;;ad-return-value
  )
;; (ad-activate 'ido-mode)
;; (ad-deactivate 'ido-mode)

(defadvice ido-visit-buffer (before
                             ourcomments-advice-ido-visit-buffer
                             ;;activate
                             ;;compile
                             disable)
  "Advice to show buffers in other window, frame etc."
  (when ourcomments-ido-visit-method
    (ad-set-arg 1 ourcomments-ido-visit-method)
    (setq ourcomments-ido-visit-method nil)
    ))
(setq ourcomments-ido-adviced t)
)

(message "after advising ido")
;;(ad-deactivate 'ido-visit-buffer)
;;(ad-activate 'ido-visit-buffer)

(defvar ourcomments-ido-old-state ido-mode)

(defun ourcomments-ido-ctrl-tab-activate ()
  (message "ourcomments-ido-ctrl-tab-activate running")
  ;;(ad-update 'ido-visit-buffer)
  ;;(unless (ad-get-advice-info 'ido-visit-buffer)
  ;; Fix-me: The advice must be enabled before activation. Send bug report.
  (ad-enable-advice 'ido-visit-buffer 'before 'ourcomments-advice-ido-visit-buffer)
  (unless (cdr (assoc 'active (ad-get-advice-info 'ido-visit-buffer)))
    (ad-activate 'ido-visit-buffer))
  ;; (ad-enable-advice 'ido-setup-completion-map 'around 'ourcomments-advice-ido-setup-completion-map)
  ;; (unless (cdr (assoc 'active (ad-get-advice-info 'ido-setup-completion-map)))
  ;;   (ad-activate 'ido-setup-completion-map))
  ;;(ad-update 'ido-mode)
  (ad-enable-advice 'ido-mode 'after 'ourcomments-advice-ido-mode)
  (unless (cdr (assoc 'active (ad-get-advice-info 'ido-mode)))
    (ad-activate 'ido-mode))
  (setq ourcomments-ido-old-state ido-mode)
  (ido-mode (or ido-mode 'buffer)))

;;;###autoload
(defcustom ourcomments-ido-ctrl-tab nil
  "Enable buffer switching using C-Tab with function `ido-mode'.
This changes buffer switching with function `ido-mode' the
following way:

- You can use C-Tab.

- You can show the selected buffer in three ways independent of
  how you entered function `ido-mode' buffer switching:

  * S-return: other window
  * C-return: other frame
  * M-return: raise frame

Those keys are selected to at least be a little bit reminiscent
of those in for example common web browsers."
  :type 'boolean
  :set (lambda (sym val)
         (set-default sym val)
         (if val
             (ourcomments-ido-ctrl-tab-activate)
           (ad-disable-advice 'ido-visit-buffer 'before
                              'ourcomments-advice-ido-visit-buffer)
           (ad-disable-advice 'ido-mode 'after
                              'ourcomments-advice-ido-mode)
           ;; For some reason this little complicated construct is
           ;; needed. If they are not there the defadvice
           ;; disappears. Huh.
           ;;(if ourcomments-ido-old-state
           ;;    (ido-mode ourcomments-ido-old-state)
           ;;  (when ido-mode (ido-mode -1)))
           ))
  :group 'emacsw32
  :group 'convenience)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; New Emacs instance

(defun ourcomments-find-emacs ()
  (locate-file invocation-name
               (list invocation-directory)
               exec-suffixes
               ;; 1 ;; Fix-me: This parameter is depreceated, but used
               ;; in executable-find, why?
               ))

(defvar ourcomments-restart-server-mode nil)

(defun emacs-restart-in-kill ()
  "Last step in restart Emacs and start `server-mode' if on before."
  (let ((restart-args (when ourcomments-restart-server-mode
                        ;; Delay 3+2 sec to be sure the old server has stopped.
                        (list "--eval=(run-with-idle-timer 5 nil 'server-mode 1)"))))
    (apply 'call-process (ourcomments-find-emacs) nil 0 nil restart-args)
    ;; Wait to give focus to new Emacs instance:
    (sleep-for 3)))

;;;###autoload
(defun emacs-restart ()
  "Restart Emacs and start `server-mode' if on before."
  (interactive)
  (let ((wait 4))
    (while (> (setq wait (1- wait)) 0)
      (message (propertize (format "Will restart Emacs in %d seconds..." wait)
                           'face 'secondary-selection))
      (sit-for 1)))
  (setq ourcomments-restart-server-mode server-mode)
  (add-hook 'kill-emacs-hook 'emacs-restart-in-kill t)
  (save-buffers-kill-emacs))

;;;###autoload
(defun emacs()
  "Start a new Emacs."
  (interactive)
  (recentf-save-list)
  (call-process (ourcomments-find-emacs) nil 0 nil)
  (message "Started 'emacs' - it will be ready soon ..."))

;;;###autoload
(defun emacs-buffer-file()
  "Start a new Emacs showing current buffer file.
Go to the current line and column in that file.
If there is no buffer file then instead start with `dired'."
  (interactive)
  (recentf-save-list)
  (let ((file (buffer-file-name))
        (lin (line-number-at-pos))
        (col (current-column)))
    ;;(unless file (error "No buffer file name"))
    (if file
        (progn
          (call-process (ourcomments-find-emacs) nil 0 nil "--no-desktop"
                        (format "+%d:%d" lin col) file)
          (message "Started 'emacs buffer-file-name' - it will be ready soon ..."))
      (call-process (ourcomments-find-emacs) nil 0 nil "--no-desktop" "--eval"
                    (format "(dired \"%s\")" default-directory)))))

;;;###autoload
(defun emacs--debug-init()
  (interactive)
  (call-process (ourcomments-find-emacs) nil 0 nil "--debug-init")
  (message "Started 'emacs --debug-init' - it will be ready soon ..."))

;;;###autoload
(defun emacs--no-desktop()
  (interactive)
  (call-process (ourcomments-find-emacs) nil 0 nil "--no-desktop")
  (message "Started 'emacs --no-desktop' - it will be ready soon ..."))

;;;###autoload
(defun emacs-Q()
  "Start new Emacs without any customization whatsoever."
  (interactive)
  (call-process (ourcomments-find-emacs) nil 0 nil "-Q")
  (message "Started 'emacs -Q' - it will be ready soon ..."))

;;;###autoload
(defun emacs-Q-nxhtml()
  "Start new Emacs with -Q and load nXhtml."
  (interactive)
  (let ((autostart (if (boundp 'nxhtml-install-dir)
                       (expand-file-name "autostart.el" nxhtml-install-dir)
                     (expand-file-name "../../EmacsW32/nxhtml/autostart.el"
                                       exec-directory))))
    (call-process (ourcomments-find-emacs) nil 0 nil "-Q"
                  "--debug-init"
                  "--load" autostart
                  )
    (message "Started 'emacs -Q --load \"%s\"' - it will be ready soon ..."
             autostart)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Searching

(defun grep-get-buffer-files ()
  "Return list of files in a `grep-mode' buffer."
  (or (and (compilation-buffer-p (current-buffer))
           (derived-mode-p 'grep-mode))
      (error "Not in a grep buffer"))
  (let ((here (point))
        files
        loc)
    (font-lock-fontify-buffer)
    (goto-char (point-min))
    (while (setq loc
                 (condition-case err
                     (compilation-next-error 1)
                   (error
                    ;; This should be the end, but give a message for
                    ;; easier debugging.
                    (message "%s" err)
                         nil)))
      ;;(message "here =%s, loc=%s" (point) loc)
      (let ((file (caar (nth 2 (car loc)))))
        (setq file (expand-file-name file))
        (add-to-list 'files file)))
    (goto-char here)
    ;;(message "files=%s" files)
    files))

(defvar grep-query-replace-defaults nil
  "Default values of FROM-STRING and TO-STRING for `grep-query-replace'.
This is a cons cell (FROM-STRING . TO-STRING), or nil if there is
no default value.")

;; Mostly copied from `dired-do-query-replace-regexp'. Fix-me: finish, test
;;;###autoload
(defun grep-query-replace(from to &optional delimited)
  "Do `query-replace-regexp' of FROM with TO, on all files in *grep*.
Third arg DELIMITED (prefix arg) means replace only word-delimited matches.
If you exit (\\[keyboard-quit], RET or q), you can resume the query replace
with the command \\[tags-loop-continue]."
  (interactive
   (let ((common
          ;; Use the regexps that have been used in grep
          (let ((query-replace-from-history-variable 'grep-regexp-history)
                (query-replace-defaults (or grep-query-replace-defaults
                                            query-replace-defaults)))
            (query-replace-read-args
             "Query replace regexp in files in *grep*" t t))))
     (setq grep-query-replace-defaults (cons (nth 0 common)
                                             (nth 1 common)))
     (list (nth 0 common) (nth 1 common) (nth 2 common))))
  (dolist (file (grep-get-buffer-files))
    (let ((buffer (get-file-buffer file)))
      (if (and buffer (with-current-buffer buffer
			buffer-read-only))
	  (error "File `%s' is visited read-only" file))))
  (tags-query-replace from to delimited
		      '(grep-get-buffer-files)))

(defun ldir-query-replace (from to files dir &optional delimited)
  "Replace FROM with TO in FILES in directory DIR.
This runs `query-replace-regexp' in selected files.

See `dired-do-query-replace-regexp' for DELIMETED and more
information."
  (interactive (dir-replace-read-parameters nil nil))
  (message "%s" (list from to files dir delimited))
  ;;(let ((files (directory-files root nil file-regexp))) (message "files=%s" files))
  (tags-query-replace from to delimited
                      `(directory-files ,dir t ,files)))

(defun rdir-query-replace (from to file-regexp root &optional delimited)
  "Replace FROM with TO in FILES in directory tree ROOT.
This runs `query-replace-regexp' in selected files.

See `dired-do-query-replace-regexp' for DELIMETED and more
information."
  (interactive (dir-replace-read-parameters nil t))
  (message "%s" (list from to file-regexp root delimited))
  ;;(let ((files (directory-files root nil file-regexp))) (message "files=%s" files))
  (tags-query-replace from to delimited
                      `(rdir-get-files ,root ,file-regexp)))

;; (rdir-get-files ".." "^a.*\.el$")
(defun rdir-get-files (root file-regexp)
  (let ((files (directory-files root t file-regexp))
        (subdirs (directory-files root t)))
    (dolist (subdir subdirs)
      (when (and (file-directory-p subdir)
                 (not (or (string= "/." (substring subdir -2))
                          (string= "/.." (substring subdir -3)))))
        (setq files (append files (rdir-get-files subdir file-regexp) nil))))
    files))

(defun dir-replace-read-parameters (has-dir recursive)
  (let* ((common
          (let (;;(query-replace-from-history-variable 'grep-regexp-history)
                ;;(query-replace-defaults (or grep-query-replace-defaults
                ;;                            query-replace-defaults))
                )
            (query-replace-read-args
             "Query replace regexp in files" t t)))
         (from (nth 0 common))
         (to   (nth 1 common))
         (delimited (nth 2 common))
         (files (replace-read-files from to))
         (root (unless has-dir (read-directory-name (if recursive "Root directory: "
                                                      "In single directory: ")))))
    (list from to files root delimited)))

;; Mostly copied from `grep-read-files'. Could possible be merged with
;; that.
(defvar replace-read-files-history nil)
(defun replace-read-files (regexp &optional replace)
  "Read files arg for replace."
  (let* ((bn (or (buffer-file-name) (buffer-name)))
	 (fn (and bn
		  (stringp bn)
		  (file-name-nondirectory bn)))
	 (default
           (let ((pre-default
                  (or (and fn
                           (let ((aliases grep-files-aliases)
                                 alias)
                             (while aliases
                               (setq alias (car aliases)
                                     aliases (cdr aliases))
                               (if (string-match (wildcard-to-regexp
                                                  (cdr alias)) fn)
                                   (setq aliases nil)
			  (setq alias nil)))
                             (cdr alias)))
                      (and fn
                           (let ((ext (file-name-extension fn)))
                             (and ext (concat "^.*\." ext))))
                      (car replace-read-files-history)
                      (car (car grep-files-aliases)))))
             (if (string-match-p "^\\*\\.[a-zA-Z0-9]*$" pre-default)
                 (concat "\\." (substring pre-default 2) "$")
               pre-default)))
	 (files (read-string
                 (if replace
                     (concat "Replace \"" regexp
                             "\" with \"" replace "\" in files"
                             (if default (concat " (default " default
                                                 ", regexp or *.EXT)"))
                             ": ")
                   (concat "Search for \"" regexp
                           "\" in files"
                           (if default (concat " (default " default ")"))
                           ": "))
		 nil 'replace-read-files-history default)))
    (let ((pattern (and files
                        (or (cdr (assoc files grep-files-aliases))
                            files))))
      (if (and pattern
               (string-match-p "^\\*\\.[a-zA-Z0-9]*$" pattern))
          (concat "\\." (substring pattern 2) "$")
        pattern))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Info

;;;###autoload
(defun info-open-file (info-file)
  "Open an info file in `Info-mode'."
  (interactive
   (let ((name (read-file-name "Info file: "
                               nil ;; dir
                               nil ;; default-filename
                               t   ;; mustmatch
                               nil ;; initial
                               ;; predicate:
                               (lambda (file)
                                 (or (file-directory-p file)
                                     (string-match ".*\\.info\\'" file))))))
     (list name)))
  (info info-file))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Exec path etc

(defun ourcomments-which (prog)
  "Look for first program PROG in `exec-path' using `exec-suffixes'.
Return full path if found."
  (interactive "sProgram: ")
  ;;(let ((path (locate-file prog exec-path exec-suffixes 'executable)))
  (let ((path (executable-find prog)))
    (when (called-interactively-p) (message "%s found in %s" prog path))
    path))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Custom faces and keys

;;;###autoload
(defun use-custom-style ()
  "Setup like in `Custom-mode', but without things specific to Custom."
  (make-local-variable 'widget-documentation-face)
  (setq widget-documentation-face 'custom-documentation)
  (make-local-variable 'widget-button-face)
  (setq widget-button-face custom-button)
  (setq show-trailing-whitespace nil)

  ;; We need this because of the "More" button on docstrings.
  ;; Otherwise clicking on "More" can push point offscreen, which
  ;; causes the window to recenter on point, which pushes the
  ;; newly-revealed docstring offscreen; which is annoying.  -- cyd.
  (set (make-local-variable 'widget-button-click-moves-point) t)

  (set (make-local-variable 'widget-button-pressed-face) custom-button-pressed)
  (set (make-local-variable 'widget-mouse-face) custom-button-mouse)

  ;; When possible, use relief for buttons, not bracketing.  This test
  ;; may not be optimal.
  (when custom-raised-buttons
    (set (make-local-variable 'widget-push-button-prefix) "")
    (set (make-local-variable 'widget-push-button-suffix) "")
    (set (make-local-variable 'widget-link-prefix) "")
    (set (make-local-variable 'widget-link-suffix) ""))

  ;; From widget-keymap
  (local-set-key "\t" 'widget-forward)
  (local-set-key "\e\t" 'widget-backward)
  (local-set-key [(shift tab)] 'advertised-widget-backward)
  (local-set-key [backtab] 'widget-backward)
  (local-set-key [down-mouse-2] 'widget-button-click)
  (local-set-key [down-mouse-1] 'widget-button-click)
  (local-set-key [(control ?m)] 'widget-button-press)
  ;; From custom-mode-map
  (local-set-key " " 'scroll-up)
  (local-set-key "\177" 'scroll-down)
  (local-set-key "n" 'widget-forward)
  (local-set-key "p" 'widget-backward))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Bookmarks

(defun bookmark-next-marked ()
  (interactive)
  (let ((bb (get-buffer "*Bookmark List*"))
        pos)
    (when bb
      (with-current-buffer bb
        (setq pos (re-search-forward "^>" nil t))
        (unless pos
          (goto-char (point-min))
          (setq pos (re-search-forward "^>" nil t)))))
    (if pos
        (with-current-buffer bb
	  ;; Defined in bookmark.el, should be loaded now.
          (bookmark-bmenu-this-window))
      (call-interactively 'bookmark-bmenu-list)
      (message "Please select bookmark for bookmark next command, then press n"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Org Mode

(defun ourcomments-org-complete-and-replace-file-link ()
  "If on a org file link complete file name and replace it."
  (interactive)
  (let* ((here (point-marker))
         (on-link (eq 'org-link (get-text-property (point) 'face)))
         (link-beg (when on-link
                     (previous-single-property-change (1+ here) 'face)))
         (link-end (when on-link
                     (next-single-property-change here 'face)))
         (link (when on-link (buffer-substring-no-properties link-beg link-end)))
         type+link
         link-link
         link-link-beg
         link-link-end
         new-link
         dir
         ovl)
    (when (and on-link
               (string-match (rx string-start "[["
                                 (group (0+ (not (any "]"))))) link))
      (setq type+link (match-string 1 link))
      (when (string-match "^file:\\(.*\\)" type+link)
        (setq link-link (match-string 1 type+link))
        (setq link-link-beg (+ 2 link-beg (match-beginning 1)))
        (setq link-link-end (+ 2 link-beg (match-end 1)))
        (unwind-protect
            (progn
              (setq ovl (make-overlay link-link-beg link-link-end))
              (overlay-put ovl 'face 'highlight)
              (when link-link
                (setq link-link (org-link-unescape link-link))
                (setq dir (when (and link-link (> (length link-link) 0))
                            (file-name-directory link-link)))
                (setq new-link (read-file-name "Org file:" dir nil nil (file-name-nondirectory link-link)))
                (delete-overlay ovl)
                (setq new-link (expand-file-name new-link))
                (setq new-link (file-relative-name new-link))
                (delete-region link-link-beg link-link-end)
                (goto-char link-link-beg)
                (insert (org-link-escape new-link))
                t))
          (delete-overlay ovl)
          (goto-char here))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Menu commands to M-x history

;; (where-is-internal 'mumamo-mark-chunk nil nil)
;; (where-is-internal 'mark-whole-buffer nil nil)
;; (where-is-internal 'save-buffer nil nil)
;; (where-is-internal 'revert-buffer nil nil)
;; (setq extended-command-history nil)
(defun ourcomments-M-x-menu-pre ()
  "Add menu command to M-x history."
  (let ((is-menu-command (equal '(menu-bar)
                                (elt (this-command-keys-vector) 0)))
        (pre-len (length extended-command-history)))
    (when (and is-menu-command
               (not (memq this-command '(ourcomments-M-x-menu-mode))))
      (pushnew (symbol-name this-command) extended-command-history)
      (when (< pre-len (length extended-command-history))
        ;; This message is given pre-command and is therefore likely
        ;; to be overwritten, but that is ok in this case. If the user
        ;; has seen one of these messages s?he knows.
        (message (propertize "(Added %s to M-x history so you can run it from there)"
                             'face 'file-name-shadow)
                 this-command)))))

;;;###autoload
(define-minor-mode ourcomments-M-x-menu-mode
  "Add commands started from Emacs menus to M-x history.
The purpose of this is to make it easier to redo them and easier
to learn how to do them from the command line \(which is often
faster if you know how to do it).

Only commands that are not already in M-x history are added."
  :global t
  (if ourcomments-M-x-menu-mode
      (add-hook 'pre-command-hook 'ourcomments-M-x-menu-pre)
    (remove-hook 'pre-command-hook 'ourcomments-M-x-menu-pre)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Warnings etc

(defvar ourcomments-warnings nil)

(defun ourcomments-display-warnings ()
  (condition-case err
      (let ((msg (mapconcat 'identity (reverse ourcomments-warnings) "\n")))
        (setq ourcomments-warnings nil)
        (message "%s" (propertize msg 'face 'secondary-selection)))
    (error (message "ourcomments-display-warnings: %s" err))))

(defun ourcomments-warning-post ()
  (condition-case err
      (run-with-idle-timer 0.5 nil 'ourcomments-display-warnings)
    (error (message "ourcomments-warning-post: %s" err))))

;;;###autoload
(defun ourcomments-warning (format-string &rest args)
  (setq ourcomments-warnings (cons (apply 'format format-string args)
                                   ourcomments-warnings))
  (add-hook 'post-command-hook 'ourcomments-warning-post))

(provide 'ourcomments-util)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ourcomments-util.el ends here
