;;; gary/bindings.el --- Overrides and additional, global bindings

;; starter-kit overrides
(global-set-key (kbd "C-w") 'backward-kill-word)
(global-set-key (kbd "<C-SPC>") 'hippie-expand)
(define-key global-map (kbd "C-=") 'text-scale-increase)
(global-set-key (kbd "<C-tab>") 'bury-buffer)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key  (kbd "C-h r") 'info-emacs-manual) ; RTFEM

;; starter-kit reversions
(global-unset-key (kbd "C-x C-h")) ; was 'view-url
;; TODO: undo ruby-compilations' clobbering of tranpose-lines (ruby-mode)

;; Emacs overrides
(global-set-key (kbd "M-x") 'ispell-word)
(global-set-key (kbd "C-o") 'vi-open-next-line)
(global-set-key (kbd "M-z") 'zap-up-to-char)
(global-set-key (kbd "C-x C-o") 'delete-blank-lines)
(global-set-key (kbd "<C-return>") 'set-mark-command)
(global-set-key (kbd "C-x C-k") 'kill-region)
(global-set-key (kbd "C-w") 'backward-kill-word)
(global-set-key (kbd "M-/") 'redo)
(global-set-key (kbd "C-x ^") 'join-line)

;; Override OS X's behavior when in Emacs
(if (eq system-type 'darwin)
    (setq mac-command-modifier 'meta
          mac-option-modifier 'alt))

;; More precise character zapping
(global-set-key (kbd "C-M-z") 'zap-to-char)

;; Visualize the kill ring
(global-set-key (kbd "M-y") 'yank-pop-forward)
(global-set-key (kbd "C-M-y") 'yank-pop-backward)

;; Insert skeleton pairs by default
;; TODO: refine per-mode
(global-set-key (kbd "C-c d") 'delete-pair)
(global-set-key (kbd "[") 'skeleton-pair-insert-maybe)
(global-set-key (kbd "(") 'skeleton-pair-insert-maybe)
(global-set-key (kbd "{") 'skeleton-pair-insert-maybe)
(global-set-key (kbd "'") 'skeleton-pair-insert-maybe)
(global-set-key (kbd "\"") 'skeleton-pair-insert-maybe)

;; Frame opacity
(global-set-key (kbd "C-8") '(lambda () ; decrease
                               (interactive)
                               (opacity-modify t)))
(global-set-key (kbd "C-9") '(lambda () ; increase
                               (interactive)
                               (opacity-modify)))
(global-set-key (kbd "C-0") '(lambda ()
                               (interactive)
                               (modify-frame-parameters nil `((alpha . 100)))))

;; Revert the current buffer
(global-set-key (kbd "<f6>") 'refresh-buffer) ; TODO dup of 'revert-buffer?

;; Prefer ack over grep
(global-set-key (kbd "C-c C-k C-a") 'ack)

;; Escape all recursive edits
(global-set-key (kbd "C-c ^") 'top-level)

;; Gisting
(global-set-key (kbd "C-c C-g g") (lambda (arg)
                                    (interactive "P")
                                    (if (null arg)
                                        (gist-region-or-buffer)
                                      (gist-region-or-buffer-private))))
(global-set-key (kbd "C-c C-g f") 'gist-fetch)

;; Time tracking
(global-set-key (kbd "C-c C-k i") 'timeclock-in)
(global-set-key (kbd "C-c C-k o") 'timeclock-out)
(global-set-key (kbd "C-c C-k c") 'timeclock-change)
(global-set-key (kbd "C-c C-k v") 'timeclock-visit-timelog)

(global-set-key [S-f12] 'swap-windows)
(global-set-key [C-f12] 'swap-split)    ; FIXME

;; Indentation help
(global-set-key (kbd "C-c <tab>") 'indent-relative)

;; Smex, bringing ido to execute-extended-command
(global-set-key (kbd "C-x C-m") 'smex)
(global-set-key (kbd "C-c C-m") 'smex-major-mode-commands)

;; Fetch the contents at a URL, display it raw.
(global-set-key (kbd "C-c C-u")
                (lambda (arg)
                  (interactive "P")
                  (if (null arg)
                      (browse-url-at-point (thing-at-point-url-at-point))
                    (view-url))))

(provide 'bindings)
;;; bindings.el ends here
