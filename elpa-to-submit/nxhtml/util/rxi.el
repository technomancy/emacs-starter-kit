;;; rxi.el --- Interactive regexp reading using rx format
;;
;; Author: Lennart Borgman (lennart O borgman A gmail O com)
;; Created: 2008-04-07T18:18:39+0200 Mon
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
;; Read regexp as `rx' forms from minibuffer.
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

(defvar rxi-read-hist nil)

(defun rxi-find-definition (rx-sym)
  (let* ((rec (assoc rx-sym rx-constituents))
         )
    (while (symbolp (cdr rec))
      (setq rec (assoc (cdr rec) rx-constituents)))
    (cdr rec)))

(defun rxi-list-type-p (rx-sym)
  (listp (rxi-find-definition rx-sym)))

(defun rxi-complete ()
  "Complete `rx' constituents."
  (interactive)
  ;; Don't care about state for now, there will be an error instead
  (let* ((partial (when (looking-back (rx (1+ (any "a-z01:|=>*?+\\-"))) nil t)
                    (match-string-no-properties 0)))
         (candidates (let ((want-list
                            (= ?\( (char-before (match-beginning 0)))))
                       (delq nil
                             (mapcar (lambda (rec)
                                       (let* ((sym (car rec))
                                              (lst (rxi-list-type-p sym)))
                                         (when (or (and want-list lst)
                                                   (and (not want-list)
                                                        (not lst)))
                                           (symbol-name sym))))
                                     rx-constituents))))
         (match-set (when partial
                      (all-completions
                       partial
                       candidates))))
    (cond
     ((not match-set)
      (message "No completions"))
     ((= 1 (length match-set))
      (insert (substring (car match-set) (length partial))))
     (t
      (with-output-to-temp-buffer "*Completions*"
          (display-completion-list match-set partial))))))

(defvar rxi-read-keymap
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map minibuffer-local-completion-map)
    (define-key map [tab] 'rxi-complete)
    (define-key map [(meta tab)] 'rxi-complete)
    (define-key map [?\ ] 'self-insert-command)
    map))

(defvar rxi-trailing-overlay nil)

(defun rxi-minibuf-setup ()
  (when rxi-trailing-overlay (delete-overlay rxi-trailing-overlay))
  (setq rxi-trailing-overlay
        (make-overlay (point-max) (point-max)
                      (current-buffer)
                      t t))
  (overlay-put rxi-trailing-overlay 'after-string
               (propertize ")"
                           'face
                           (if (and
                                (fboundp 'noticeable-minibuffer-prompts-mode)
                                noticeable-minibuffer-prompts-mode)
                               'minibuffer-noticeable-prompt
                             'minibuffer-prompt)))
  (remove-hook 'minibuffer-setup-hook 'rxi-minibuf-setup))

(defun rxi-minibuf-exit ()
  (when rxi-trailing-overlay
    (delete-overlay rxi-trailing-overlay)
    (setq rxi-trailing-overlay nil))
  (remove-hook 'minibuffer-exit-hook 'rxi-minibuf-exit))

(defun rxi-read (prompt)
  "Read a `rx' regexp form from minibuffer.
Return cons of rx and regexp, both as strings."
  (interactive (list "Test (rx "))
  (let (rx-str rx-full-str res-regexp)
    (while (not res-regexp)
      (condition-case err
          (progn
            (add-hook 'minibuffer-setup-hook 'rxi-minibuf-setup)
            (add-hook 'minibuffer-exit-hook 'rxi-minibuf-exit)
            (setq rx-str (read-from-minibuffer prompt
                                               rx-str ;; initial-contents
                                               rxi-read-keymap
                                               nil ;; read
                                               'rxi-read-hist
                                               nil ;; inherit-input-method - no idea...
                                               ))
            (setq rx-full-str (concat "(rx " rx-str ")"))
            (setq res-regexp (eval (read rx-full-str))))
        (error (message "%s" (error-message-string err))
               (sit-for 2))))
    (when (called-interactively-p) (message "%s => \"%s\"" rx-full-str res-regexp))
    (cons rx-full-str res-regexp)))


(provide 'rxi)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; rxi.el ends here
