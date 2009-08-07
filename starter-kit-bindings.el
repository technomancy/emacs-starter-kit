;;; starter-kit-bindings.el --- Set up some handy key bindings
;;
;; Part of the Emacs Starter Kit.

;; Override Mac OS X's behavior when in Emacs
(if (eq system-type 'darwin)
    (setq mac-command-modifier 'meta
          mac-option-modifier 'alt))

;; Smex, bringing ido to execute-extended-command
;; FIXME: half broken
(global-set-key (kbd "C-x RET") 'smex)
(global-set-key (kbd "C-c RET") 'smex-major-mode-commands)

;; Mark and Region usage overrides
(global-set-key (kbd "<C-return>") 'set-mark-command)
(global-set-key (kbd "C-x C-k") 'kill-region)

;; You know, like Readline.
(global-set-key (kbd "C-w") 'backward-kill-word)

;; Align your code in a pretty way.
(global-set-key (kbd "C-x \\") 'align-regexp)

;; Completion that uses many different methods to find options.
(global-set-key (kbd "<C-SPC>") 'hippie-expand)

;; Perform general cleanup.
(global-set-key (kbd "C-c n") 'cleanup-buffer)

;; Font size
(define-key global-map (kbd "C-=") 'text-scale-increase)
(define-key global-map (kbd "C--") 'text-scale-decrease)

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

;; Gracefully redo undone changes
(global-set-key (kbd "M-/") 'redo)

;; Revert the current buffer
(global-set-key (kbd "<f6>") 'refresh-buffer)

;; Prefer ack over grep
(global-set-key (kbd "C-c C-k C-a") 'ack)

;; Use regex searches by default.
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)

;; Jump to a definition in the current file. (This is awesome.)
(global-set-key (kbd "C-x C-i") 'ido-imenu)

;; Gisting
(global-set-key (kbd "C-c C-g g") (lambda (arg)
                                    (interactive "P")
                                    (if (null arg)
                                        (gist-region-or-buffer)
                                      (gist-region-or-buffer-private))))
(global-set-key (kbd "C-c C-g f") 'gist-fetch)

;; File finding
(global-set-key (kbd "C-x M-f") 'ido-find-file-other-window)
(global-set-key (kbd "C-x C-M-f") 'find-file-in-project)
(global-set-key (kbd "C-x f") 'recentf-ido-find-file)
(global-set-key (kbd "<C-tab>") 'bury-buffer)
(global-set-key (kbd "C-c r") 'revert-buffer)
(global-set-key (kbd "M-`") 'file-cache-minibuffer-complete)
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; Time tracking
(global-set-key (kbd "C-c C-k i") 'timeclock-in)
(global-set-key (kbd "C-c C-k o") 'timeclock-out)
(global-set-key (kbd "C-c C-k c") 'timeclock-change)
(global-set-key (kbd "C-c C-k v") 'timeclock-visit-timelog)

;; Window switching. (C-x o goes to the next window)
(windmove-default-keybindings) ;; Shift+direction

;; Indentation help
(global-set-key (kbd "C-x ^") 'join-line)
(global-set-key (kbd "C-c <tab>") 'indent-relative)

;; Start eshell or switch to it if it's active.
(global-set-key (kbd "C-x m") 'eshell)

;; Start a new eshell even if one is active.
(global-set-key (kbd "C-x M") (lambda () (interactive) (eshell t)))

;; Start a regular shell if you prefer that.
(global-set-key (kbd "C-x M-m") 'shell)

;; If you want to be able to M-x without meta
(global-set-key (kbd "C-x C-m") 'execute-extended-command)

;; Fetch the contents at a URL, display it raw.
(global-set-key (kbd "C-x h") 'view-url)

;; Help should search more than just commands
(global-set-key (kbd "C-h a") 'apropos)

;; Should be able to eval-and-replace anywhere.
(global-set-key (kbd "C-c e") 'eval-and-replace)

;; Applications
(global-set-key (kbd "C-x g") 'magit-status)

;; This is a little hacky since VC doesn't support git add internally
(eval-after-load 'vc
  (define-key vc-prefix-map "i" '(lambda () (interactive)
                                   (if (not (eq 'Git (vc-backend buffer-file-name)))
                                       (vc-register)
                                     (shell-command (format "git add %s" buffer-file-name))
                                     (message "Staged changes.")))))

;; Activate occur easily inside isearch
(define-key isearch-mode-map (kbd "C-o")
  (lambda () (interactive)
    (let ((case-fold-search isearch-case-fold-search))
      (occur (if isearch-regexp isearch-string (regexp-quote isearch-string))))))

(provide 'starter-kit-bindings)
;;; starter-kit-bindings.el ends here