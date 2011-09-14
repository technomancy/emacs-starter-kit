;;; starter-kit-lisp.el --- Saner defaults and goodies for lisp languages
;;
;; Copyright (c) 2008-2011 Phil Hagelberg and contributors
;;
;; Author: Phil Hagelberg <technomancy@gmail.com>
;; URL: http://www.emacswiki.org/cgi-bin/wiki/StarterKit
;; Version: 2.0.3
;; Keywords: convenience
;; Package-Requires: ((starter-kit "2.0.2") (elisp-slime-nav "0.1"))

;; This file is not part of GNU Emacs.

;;; Commentary:

;; "Emacs outshines all other editing software in approximately the
;; same way that the noonday sun does the stars. It is not just bigger
;; and brighter; it simply makes everything else vanish."
;; -Neal Stephenson, "In the Beginning was the Command Line"

;; This file contains tweaks specific to Lisp languages.

;;; License:

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Code:

;; Emacs Lisp

;;;###autoload
(progn
  (add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
  (add-hook 'emacs-lisp-mode-hook 'esk-remove-elc-on-save)
  (add-hook 'emacs-lisp-mode-hook 'esk-prog-mode-hook)
  (add-hook 'emacs-lisp-mode-hook 'elisp-slime-nav-mode)

  (defun esk-remove-elc-on-save ()
    "If you're saving an elisp file, likely the .elc is no longer valid."
    (make-local-variable 'after-save-hook)
    (add-hook 'after-save-hook
              (lambda ()
                (if (file-exists-p (concat buffer-file-name "c"))
                    (delete-file (concat buffer-file-name "c"))))))

  (define-key emacs-lisp-mode-map (kbd "C-c v") 'eval-buffer)

;;; Enhance Lisp Modes

  (define-key read-expression-map (kbd "TAB") 'lisp-complete-symbol)
  (define-key lisp-mode-shared-map (kbd "RET") 'reindent-then-newline-and-indent)

  ;; TODO: look into parenface package
  (defface esk-paren-face
    '((((class color) (background dark))
       (:foreground "grey50"))
      (((class color) (background light))
       (:foreground "grey55")))
    "Face used to dim parentheses."
    :group 'starter-kit-faces)

  (eval-after-load 'paredit
    ;; need a binding that works in the terminal
    '(define-key paredit-mode-map (kbd "M-)") 'paredit-forward-slurp-sexp))

  (dolist (mode '(scheme emacs-lisp lisp clojure))
    (when (> (display-color-cells) 8)
      (font-lock-add-keywords (intern (concat (symbol-name mode) "-mode"))
                              '(("(\\|)" . 'esk-paren-face))))
    (add-hook (intern (concat (symbol-name mode) "-mode-hook"))
              'esk-turn-on-paredit)
    (add-hook (intern (concat (symbol-name mode) "-mode-hook"))
              'esk-turn-on-paredit))

  (defun esk-pretty-fn ()
    (font-lock-add-keywords nil `(("(\\(fn\\>\\)"
                                   (0 (progn (compose-region (match-beginning 1)
                                                             (match-end 1)
                                                             "\u0192") nil))))))
  (add-hook 'clojure-mode-hook 'esk-pretty-fn))

(provide 'starter-kit-lisp)
;;; starter-kit-lisp.el ends here
