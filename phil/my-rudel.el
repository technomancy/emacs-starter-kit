;; Pairing magic!

(ignore-errors
  (when (= emacs-major-version 23)
    (load-file "~/src/rudel/rudel-loaddefs.el")))

(setq rudel-configured-sessions
      `((:name "localhost" :backend obby
               :host "localhost" :username ,user-login-name
               :color "light steel blue"
               :encryption t :port 6522
               :global-password "" :user-password "")
        (:name "puyo" :backend obby
               :host "puyo.local" :username ,user-login-name
               :color "light steel blue"
               :encryption t :port 6522)))

;; (add-hook 'rudel-document-attach-hook 'rudel-activate-major-mode)
(add-hook 'rudel-document-attach-hook
          (lambda (doc buffer) (rudel-mode-line-publish-state-minor-mode t)))

(setq rudel-overlay-author-display nil)
