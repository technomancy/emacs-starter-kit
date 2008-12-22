;;; rebind.el --- Rebind keys
;;
;; Author: Lennart Borgman (lennart O borgman A gmail O com)
;; Created: 2008-01-20T12:04:37+0100 Sun
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
;; See `rebind-keys-mode' for information.
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

(require 'new-key-seq-widget)


(defun rebind-toggle-first-modifier (orig-key-seq mod)
  (let* ((first (elt orig-key-seq 0))
         (new-key-seq (copy-sequence orig-key-seq)))
    (setq first (if (memq mod first)
                    (delq mod first)
                  (cons mod first)))
    (aset new-key-seq 0 first)
    new-key-seq))
;; (rebind-toggle-first-modifier (key-description-to-vector "C-c a") 'shift)
;; (rebind-toggle-first-modifier (key-description-to-vector "C-S-c a") 'shift)

(defconst rebind-keys-mode-map (make-sparse-keymap))

(defun rebind-update-keymap (symbol value)
  ;(lwarn t :warning "value=%S" value)
  (let ((m (make-sparse-keymap)))
    (dolist (group value)
      ;(lwarn t :warning "group=%S" group)
      (when (nth 1 group)
        (dolist (v (nth 2 group))
          (let* (
                 (orig-key   (nth 0 v))
                 (comment    (nth 1 v))
                 (enabled    (nth 2 v))
                 (new-choice (nth 3 v))
                 (new-fun    (nth 4 v))
                 (orig-fun (lookup-key global-map orig-key))
                 new-key)
            (when new-choice
              (if (memq new-choice '(meta control shift))
                  (setq new-key (rebind-toggle-first-modifier orig-key new-choice))
                (setq new-key new-choice))
              (define-key m new-key orig-fun))
            (when enabled
              (define-key m orig-key new-fun))))
        (setq rebind-keys-mode-map m))
      (set-default symbol value))))

(defvar widget-commandp-prompt-value-history nil)

(define-widget 'command 'function
  "A major mode lisp function."
  :complete-function (lambda ()
                       (interactive)
                       (lisp-complete-symbol 'commandp))
  :prompt-match 'major-modep
  :prompt-history 'widget-commandp-prompt-value-history
  :match-alternatives '(commandp)
  :validate (lambda (widget)
              (unless (major-modep (widget-value widget))
                (widget-put widget :error (format "Invalid function: %S"
                                                  (widget-value widget)))
                widget))
  :value 'fundamental-mode
  :tag "Command function")

;; (customize-option 'rebind-keys)
(defcustom rebind-keys
  ;; (Fetched key bindings from http://www.davidco.com/tips_tools/tip45.html)
  '(
    ("MS Windows - often used key bindings" t
      (
       (
        [(control ?a)]
        "C-a on w32 normally means 'select all'. In Emacs it is `beginning-of-line'."
        t
        shift
        mark-whole-buffer)
      (
       [(control ?o)]
       "C-o on w32 normally means 'open file'. In Emacs it is `open-line'."
       nil
       shift
       find-file)
      (
       [(control ?f)]
       "C-f is commonly search on w32. In Emacs it is `forward-char'."
       nil
       shift
       isearch-forward)
      (
       [(control ?s)]
       "C-s is normally 'save file' on w32. In Emacs it is `isearch-forward'."
       nil
       nil
       save-buffer)
      (
       [(control ?w)]
       "C-w is often something like kill-buffer on w32. In Emacs it is `kill-region'."
       t
       shift
       kill-buffer)
      (
       [(control ?p)]
       "C-p is nearly always print on w32. In Emacs it is `previous-line'."
       t
       shift
       hfyview-buffer)
       )))
  "Normal Emacs keys that are remapped to follow some other standard.
The purpose of this variable is to make it easy to switch between
Emacs key bindings and other standards.

The new bindings is made in the global minor mode
`rebind-keys-mode' and will only have effect when this mode is
on.

You can only move functions bound in the global key map this way."
  :type '(repeat
          (list
           (string :tag "For what")
           (boolean :tag "Group on/off")
           (repeat
            (list
             (key-sequence :tag "Emacs key binding")
             (string :tag "Why rebind")
             (boolean :tag "Rebinding on/off")
             (choice :tag "Move original by"
                     (const :tag "Don't put it on any new binding" nil)
                     (choice :tag "Add key binding modifier"
                             (const meta)
                             (const control)
                             (const shift))
                     (key-sequence :tag "New binding for original function"))
             (command :tag "New command on above key"))
            )))
  :set 'rebind-update-keymap
  :group 'emacsw32
  )

(defconst rebind--emul-keymap-alist (list (cons 'rebind-keys-mode rebind-keys-mode-map)))

(defun rebind-keys-post-command ()
  "Make sure we are first in the list when turned on.
This is reasonable since we are using this mode to really get the
key bindings we want!"
  (setq emulation-mode-map-alists (delq 'rebind--emul-keymap-alist emulation-mode-map-alists))
  (when rebind-keys-mode
    (add-to-list 'emulation-mode-map-alists 'rebind--emul-keymap-alist)))

(define-minor-mode rebind-keys-mode
  "Rebind keys as defined in `rebind-keys'.
The key bindings will override almost all other key bindings."
  :keymap rebind-keys-mode-map
  :global t
  (if rebind-keys-mode
      (add-hook 'post-command-hook 'rebind-keys-post-command)
    (remove-hook 'post-command-hook 'rebind-keys-post-command)
    (setq emulation-mode-map-alists (delq 'rebind--emul-keymap-alist emulation-mode-map-alists))))


(provide 'rebind)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; rebind.el ends here
