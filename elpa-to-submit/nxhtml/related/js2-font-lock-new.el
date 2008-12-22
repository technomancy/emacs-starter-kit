;;; js2-fl-mode.el --- Jit lock support for js2
;;
;; Author: Lennart Borgman (lennart O borgman A gmail O com)
;; Created: 2008-03-31T23:17:42+0200 Mon
;; Version:
;; Last-Updated: 2008-04-30T01:28:19+0200 Tue
;; URL:
;; Keywords:
;; Compatibility:
;;
;; Features that might be required by this library:
;;
;;   Required feature `js2-font-lock-new' was not provided.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; Just adds a new major mode `js2-fl-mode' that is just `js2-mode'
;; with some small changes to support `jit-lock-mode'.
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

(let ((dir (file-name-directory
            (or load-file-name (buffer-file-name)))))
  (load (expand-file-name "js2-new" dir)))

(defun js2-fontify-region (beg end &optional loudly)
  ;;(message "js2-fontify-region beg-end=%s-%s, min-max=%s-%s" beg end (point-min) (point-max))
  (let (interrupted
        (js2-compiler-strict-mode js2-mode-show-strict-warnings))
    (unwind-protect
        (progn
          ;;(js2-with-unmodifying-text-property-changes (js2-clear-face (point-min) (point-max)))
          (setq interrupted-p
                (catch 'interrupted
                  (setq js2-mode-ast (js2-parse (current-buffer) nil))
                  (js2-mode-fontify-regions)
                  (js2-mode-show-warnings)
                  (js2-mode-show-errors)
                  ;; fix-me: Can't be used now since font-lock-keywords is not buffer local
                  ;;(when font-lock-keywords (font-lock-fontify-keywords-region (point-min) (point-max)))
                  )))
      (when interrupted
        (js2-with-unmodifying-text-property-changes
          (put-text-property beg end 'fontified nil))))))

(defun js2-unfontify-region (beg end)
  (js2-mode-exit))

(defun js2-extend-jit-lock-region-after-change (beg end old-len)
  "Function meant for `jit-lock-after-change-extend-region-functions'.
Tell the js2 parser where there has been a change.  The parser
should check this list for what to do."
  ;; Handle jit-lock-chunk-size here:
  (setq jit-lock-chunk-size (1+ (buffer-size)))
  (setq js2-fl-changed (cons beg js2-fl-changed)))

(defun js2-fl-mode ()
  "Major mode for editing JavaScript code."
  (interactive)
  (js2-mode-check-compat)
  (kill-all-local-variables)
  (set-syntax-table js2-mode-syntax-table)
  (use-local-map js2-mode-map)
  (setq major-mode 'js2-fl-mode
        mode-name "JavaScript-IDE-fl"
        comment-start "//"  ; used by comment-region; don't change it
        comment-end "")
  (setq local-abbrev-table js2-mode-abbrev-table)
  (set (make-local-variable 'max-lisp-eval-depth)
       (max max-lisp-eval-depth 3000))
  (set (make-local-variable 'indent-line-function) #'js2-indent-line)
  (set (make-local-variable 'indent-region-function) #'js2-indent-region)
  (set (make-local-variable 'fill-paragraph-function) #'js2-fill-paragraph)
  (set (make-local-variable 'before-save-hook) #'js2-before-save)
  (set (make-local-variable 'next-error-function) #'js2-next-error)
  (set (make-local-variable 'beginning-of-defun-function) #'js2-beginning-of-defun)
  (set (make-local-variable 'end-of-defun-function) #'js2-end-of-defun)
  ;; We un-confuse `parse-partial-sexp' by setting syntax-table properties
  ;; for characters inside regexp literals.
  (set (make-local-variable 'parse-sexp-lookup-properties) t)
  ;; this is necessary to make `show-paren-function' work properly
  (set (make-local-variable 'parse-sexp-ignore-comments) t)
  ;; needed for M-x rgrep, among other things
  (put 'js2-mode 'find-tag-default-function #'js2-mode-find-tag)

  ;; some variables needed by cc-engine for paragraph-fill, etc.
  (setq c-buffer-is-cc-mode t
        c-comment-prefix-regexp js2-comment-prefix-regexp
        c-paragraph-start js2-paragraph-start
        c-paragraph-separate "$"
        c-syntactic-ws-start js2-syntactic-ws-start
        c-syntactic-ws-end js2-syntactic-ws-end
        c-syntactic-eol js2-syntactic-eol)
  (set (make-local-variable 'comment-start-skip) js2-comment-start-skip)
  (if js2-emacs22
      (c-setup-paragraph-variables))

;;;   ;; We do our own syntax highlighting based on the parse tree.
;;;   (if (fboundp #'font-lock-mode)
;;;       (font-lock-mode -1))
;;;   ;; Don't let forced fontification ruin our lovely highlighting.
;;;   (dolist (var '(font-lock-fontify-buffer-function
;;;                  font-lock-unfontify-buffer-function
;;;                  font-lock-fontify-region-function
;;;                  font-lock-unfontify-region-function))
;;;     (set (make-local-variable var) (lambda (&rest args) t)))
  ;; Experiment:  make reparse-delay longer for longer files.
  (if (plusp js2-dynamic-idle-timer-adjust)
      (setq js2-idle-timer-delay
            (* js2-idle-timer-delay
               (/ (point-max) js2-dynamic-idle-timer-adjust))))
;;;   (add-hook 'change-major-mode-hook #'js2-mode-exit nil t)
;;;   (add-hook 'after-change-functions #'js2-mode-edit nil t)
  (set (make-local-variable 'font-lock-fontify-region-function)
       'js2-fontify-region)
  (set (make-local-variable 'font-lock-unfontify-region-function)
       'js2-unfontify-region)
  (add-hook 'jit-lock-after-change-extend-region-functions
            'js2-extend-jit-lock-region-after-change t t)
  (set (make-local-variable 'jit-lock-defer-time) js2-idle-timer-delay)
  (set (make-local-variable 'jit-lock-defer-timer) nil)
  (set (make-local-variable 'jit-lock-contextually) nil)
  (set (make-local-variable 'jit-lock-context-time) nil)
  (set (make-local-variable 'jit-lock-context-timer) nil)
  ;; Make jit-lock-chunk-size that big that it is just called one time:
  (set (make-local-variable 'jit-lock-chunk-size) (1+ (buffer-size)))

;;;   ;; From Karl's
;;;   (set-syntax-table javascript-mode-syntax-table)
;;;   (set (make-local-variable 'parse-sexp-ignore-comments) t)
;;;   (set (make-local-variable 'comment-start) "// ")
;;;   (set (make-local-variable 'comment-end) "")
;;;   (set (make-local-variable 'font-lock-defaults) nil)

  (setq imenu-create-index-function #' js2-mode-create-imenu-index)
  (imenu-add-to-menubar (concat "IM-" mode-name))
  (when js2-mirror-mode
    (js2-enter-mirror-mode))
  (add-to-invisibility-spec '(js2-outline . t))
  (set (make-local-variable 'line-move-ignore-invisible) t)
  (set (make-local-variable 'forward-sexp-function)
       #'js2-mode-forward-sexp)
  (setq js2-mode-functions-hidden nil
        js2-mode-comments-hidden nil)
;;;   (setq js2-mode-buffer-dirty-p t
;;;         js2-mode-parsing nil)
;;;   (js2-reparse)
  (jit-lock-register 'font-lock-fontify-region nil)
  ;;(js2-fontify-region (point-min) (point-max))
  (run-hooks 'js2-mode-hook))


(provide 'js2-fl-mode)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; js2-fl-mode.el ends here
