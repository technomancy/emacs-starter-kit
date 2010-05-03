;;; broken ido
(defun ido-directory-too-big-p (arg) nil)

;; awesome sometimes, but right now more trouble than it's worth
(setq tramp-mode nil
      tramp-unload-hook nil
      ido-enable-tramp-completion nil)

(add-hook 'eshell-mode-hook
          '(lambda () (fmakunbound 'eshell/sudo)
             (fmakunbound 'eshell/su)))

;; plz not to refresh log buffer when I cherry-pick, mkay?
(eval-after-load 'magit
  '(define-key magit-log-mode-map (kbd "A")
     (lambda ()
       (interactive)
       (flet ((magit-need-refresh (f)))
         (magit-cherry-pick-item)))))
