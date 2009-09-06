;;; new-key-seq-widget.el --- New key-sequence widget for Emacs
;;
;; Author: Lennart Borgman (lennart O borgman A gmail O com)
;; Created: Tue Dec 25 23:00:43 2007
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
;;  New version of Kim's Emacs key-sequence widget. For inclusion in
;;  Emacs I hope.
;;
;;  Fix-me: check what was included.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change log:
;;
;; I do not know how much I have changed, but I keep it together here
;; for simplicity.
;;
;; Note: I have named made `widget-key-sequence-map' a constant for
;; the moment.
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

(require 'wid-edit)

;;; I'm not sure about what this is good for?  KFS.
;;
;;; This should probably be for customize-set-value etc, but it is not
;;; used. Or for the widget editing, but it is not used there
;;; either. /Lennart
(defvar widget-key-sequence-prompt-value-history nil
  "History of input to `widget-key-sequence-prompt-value'.")

(defvar widget-key-sequence-default-value [ignore]
  "Default value for an empty key sequence.")

(defconst widget-key-sequence-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map widget-field-keymap)
    (define-key map [(control ?q)] 'widget-key-sequence-read-event)
    (define-key map [(control ?t)] 'widget-key-sequence-toggle-input-format)
    map))

(defvar widget-key-sequence-input-formats '(key-description vector))

(defcustom widget-key-sequence-default-input-format 'key-description
  "Format used to edit key sequences.
This is the format shown and edited in a key-sequence widget."
  :type '(choice (const :tag "Key description" 'key-description)
                 (const :tag "Vector" 'vector))
  :group 'widgets)

(define-widget 'key-sequence 'restricted-sexp
  "A key sequence."
  :prompt-value 'widget-field-prompt-value
  :prompt-internal 'widget-symbol-prompt-internal
; :prompt-match 'fboundp   ;; What was this good for?  KFS
  :prompt-history 'widget-key-sequence-prompt-value-history
  :action 'widget-field-action
  :match-alternatives '(stringp vectorp)
  :format "%{%t%}: %v"
  :validate 'widget-key-sequence-validate
  :value-to-internal 'widget-key-sequence-value-to-internal
  :value-to-external 'widget-key-sequence-value-to-external
  :value widget-key-sequence-default-value
  :keymap widget-key-sequence-map
  :help-echo "C-q: insert KEY, EVENT, or CODE; C-t: toggle format"
  :tag "Key sequence")


;;; Leave these here for testing:
;; (edmacro-parse-keys "C-x h" t) => [24 104]
;; (key-description-to-vector "C-x h" ) => [(control 120) 104]
;; (key-description (key-description-to-vector "C-x h")) => "C-x h"
;; (key-description (edmacro-parse-keys "C-x h")) => "C-x h"
;; (key-description [M-mouse-1]) => <M-mouse-1>
;; (edmacro-parse-keys "<M-mouse-1>") => [M-mouse-1]

;; (event-modifiers 'mouse-1) => (click mouse-1 mouse-1 mouse-1 mouse-1 mouse-1)
;; (event-modifiers 'M-mouse-1) =>
;; (event-modifiers '(mouse-1)) => (click mouse-1 mouse-1 mouse-1 mouse-1 mouse-1)
;; (event-modifiers '(down-mouse-1)) => (click mouse-1 mouse-1 mouse-1 mouse-1 mouse-1)
;; (event-modifiers '(S-down-mouse-1)) => (shift down)
;; (event-modifiers 'S-down-mouse-1) => (shift down)
;; (event-modifiers 'down-mouse-1) => (click mouse-1 mouse-1 mouse-1 mouse-1 mouse-1)
;; (event-modifiers '(down-mouse-1)) => (click mouse-1 mouse-1 mouse-1 mouse-1 mouse-1)
;; (let ((m (make-sparse-keymap))) (define-key m [(down mouse-1)] 'hej))
(defun key-description-to-vector (kd)
  "Convert human readable key description KD to vector format.
KD should be in the format returned by `key-description'."
  (let ((v
         (vconcat
          (mapcar (lambda (k)
                    ;; Fix-me: temporarily clean the event here:
                    (when (symbolp k)
                      (let ((esem (get k 'event-symbol-element-mask))) (when esem (lwarn t :warning "kd=%s, k=%s, esem=%s" kd k esem)))
                      (put k 'event-symbol-element-mask nil))
                    (let ((m (event-modifiers k))
                          (b (event-basic-type k)))
                      (setq m (delq 'click m))
                      (if m
                          (nconc m (list b))
                        b)))
                  ;; fix-me: does not always work for menu and tool
                  ;; bar event because they may contains spaces.
                  (edmacro-parse-keys kd t))))
        (m (make-sparse-keymap))
        )
    ;; Test before returning it:
    (define-key m v 'test)
    v))

(defun widget-key-sequence-current-input-format ()
  (let ((fmt (or (widget-get (widget-at (point)) :key-sequence-format)
                 widget-key-sequence-default-input-format)))
    fmt))

(defun widget-key-sequence-toggle-input-format ()
  "Toggle key sequence input format."
  (interactive)
  (let* ((widget (widget-at (point)))
         (value (widget-apply widget :value-get))
         (first (string-to-char value))
         (old-fmt
          (let ((fmt (or (widget-get widget :key-sequence-format)
                         widget-key-sequence-default-input-format)))
            fmt))
         (new-fmt
          (let ((m (cdr (memq old-fmt widget-key-sequence-input-formats))))
            (if m (car m) (car widget-key-sequence-input-formats))))
         (new-value
          (cond
           ((eq new-fmt 'key-description)
            (setq value (replace-regexp-in-string "\\` *\\(.*?\\) *\\'" "\\1" value))
            (if (string= value "")
                ""
              (key-description (read value))))
           ((eq new-fmt 'vector)
            (format "%S" (key-description-to-vector value)))
           (t
            (error "Bad key seq format spec: %s" new-fmt))))
         (state (widget-get (widget-get widget :parent) :custom-state))
         )
    (widget-put widget :key-sequence-format new-fmt)
    (setq new-value (propertize new-value 'face 'highlight))
    (widget-apply widget :value-set new-value)
    (widget-setup)
    (widget-put (widget-get widget :parent) :custom-state state)
    (cond
     ((eq new-fmt 'key-description)
      (message "Switched to human readable format"))
     ((eq new-fmt 'vector)
      (message "Switched to vector format"))
     (t
      (error "Uh? format=%s" new-fmt)))))


(defun widget-key-sequence-read-event (ev)
  "Read event or char code and put description in widget.
The events may come from keyboard, mouse, menu or tool bar.

If the event is a mouse event then multiple entries will be
entered. It is not possible to know which one is wanted. Please
remove those not wanted!

If 0-7 is pressed then code for an event is prompted for."
  (interactive (list
                (let ((inhibit-quit t) quit-flag)
                  (unless (eq 'key-description
                              (widget-key-sequence-current-input-format))
                    (error "Wrong input format, please do C-t first"))
                  (read-event "Insert KEY, EVENT, or CODE: "))))
  (lwarn t :warning "=====> ev=%s" ev)
  (let ((tr (and (keymapp function-key-map)
                 (lookup-key function-key-map (vector ev)))))
    (insert (if (= (char-before) ?\s)  "" " "))
    ;; Fix-me: change to check for ? instead of 0-7 to allow char
    ;; literal input format
    (when (and (integerp ev)
               (or (and (<= ?0 ev) (< ev (+ ?0 (min 10 read-quoted-char-radix))))
                   (and (<= ?a (downcase ev))
                        (< (downcase ev) (+ ?a -10 (min 36 read-quoted-char-radix))))))
      (setq unread-command-events (cons ev unread-command-events)
            ev (read-quoted-char (format "Enter code (radix %d)" read-quoted-char-radix))
            tr nil)
      (if (and (integerp ev) (not (char-valid-p ev)))
          (insert (char-to-string ev))))  ;; throw invalid char error
    (setq ev (key-description (list ev)))
    (when (arrayp tr)
      (setq tr (key-description (list (aref tr 0))))
      (if (y-or-n-p (format "Key %s is translated to %s -- use %s? " ev tr tr))
          (setq ev tr ev2 nil)))
    (insert ev " ")
    (when (or (string-match "mouse-" ev)
              (string-match "menu-bar" ev)
              (string-match "tool-bar" ev))
      (let ((ev2 (read-event nil nil (* 0.001 double-click-time))))
        (while ev2
          (lwarn t :warning "(stringp ev2)=%s, (sequencp ev2)=%s, (symbolp ev2)=%s, ev2=%S" (stringp ev2) (sequencep ev2) (symbolp ev2) ev2)
          (if nil ;(memq 32 (append (symbol-name ev2) nil)) ;; Fix-me: contains space
              (insert ?\" (symbol-name ev2) ?\")
            (insert (key-description (list ev2))))
          (insert " ")
          (setq ev2 (read-event nil nil (* 0.001 double-click-time))))))))

(defun widget-key-sequence-validate (widget)
  "Validate the internal value of the widget.
Actually there is nothing to validate here.  The internal value
is always valid, but it is however maybe not what the user
expects.  Because of this the internal format is rewritten when
the user gives the value in a way that is not the normal
representation of it. A warning is also shown then."
  (condition-case err
      (let* ((int-val (widget-apply widget :value-get))
             (def-desc (key-description (edmacro-parse-keys int-val)))
             (fmt (or (widget-get widget :key-sequence-format)
                      widget-key-sequence-default-input-format)))
        ;; Normalize and compare with default description
        (setq int-val
              (replace-regexp-in-string " *" " " int-val t))
        (setq int-val
              (replace-regexp-in-string "\\` *\\(.*?\\) *\\'" "\\1" int-val t))
        (unless (or
                 (eq fmt 'vector)
                 (string= int-val def-desc))
          ;; Replace with the default description if it is different
          ;; so the user sees what the value actually means:
          (widget-apply widget :value-set def-desc)
          (lwarn t :warning
                 (concat "Key description %s means the same as %s\n"
                         "\tTip: You can type C-q to insert a key or event")
                 int-val def-desc)
          )
        ;; Return nil if there a no problem validating
        nil)
    (error (widget-put widget :error (error-message-string err))
           (lwarn t :warning "invalid %S: %s" widget (error-message-string err))
           ;; Return widget if there was an error
           widget)))

(defun widget-key-sequence-value-to-internal (widget value)
  (if (widget-apply widget :match value)
      (if (equal value widget-key-sequence-default-value)
          ""
        (let ((fmt (or (widget-get widget :key-sequence-format)
                       widget-key-sequence-default-input-format)))
          (if (eq fmt 'vector)
              (format "%S" value)
            (key-description value))))
    value))

(defun widget-key-sequence-value-to-external (widget value)
  (if (stringp value)
      (if (string-match "\\`[[:space:]]*\\'" value)
          widget-key-sequence-default-value
        ;; Give a better error message and a trace back on debug:
        (condition-case err
            (let* ((fmt (or (widget-get widget :key-sequence-format)
                            widget-key-sequence-default-input-format))
                   (first (string-to-char value)))
              (cond
               ((eq fmt 'vector)
                (read value)
                )
               (t
                (key-description-to-vector value))))
          (error (error "Bad value: %s" (error-message-string err)))))
    value))

;; (customize-option 'new-key-seq-widget-test)
(defcustom new-key-seq-widget-test []
  "Testing only!"
  :type 'key-sequence)

(provide 'new-key-seq-widget)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; new-key-seq-widget.el ends here
