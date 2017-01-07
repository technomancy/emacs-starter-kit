;;; gary/misc.el --- Things that don't fit anywhere else

;; Additional autoloads
(add-to-list 'auto-mode-alist '("\\.applescript$" . applescript-mode))
(add-to-list 'auto-mode-alist '("\\.scpt$"        . applescript-mode))
(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.zsh$" . shell-script-mode))

(ansi-color-for-comint-mode-on)

;; Enable select, disabled features
(put 'narrow-to-region 'disabled nil)   ; narrow enabled
(put 'narrow-to-page 'disabled nil)
(put 'upcase-region 'disabled nil)      ; change case enabled
(put 'eval-expression 'disabled nil)    ; allow eval commands

(setq yank-pop-change-selection t
      ffap-require-prefix t
      skeleton-pair t
      auto-save-default nil
      minibuffer-max-depth nil
      uniquify-buffer-name-style 'reverse
      uniquify-separator "|"
      uniquify-after-kill-buffer-p t
      uniquify-ignore-buffers-re "^\\*")

(setq browse-url-browser-function 'browse-url-default-macosx-browser)

;; Replace the contents of a selection with typed text
(delete-selection-mode t)

;; Incremental minibuffer completion
(icomplete-mode 1)

(when (> emacs-major-version 21)
  (ido-everywhere t)
  (setq ido-confirm-unique-completion t
        ido-default-buffer-method 'other-window
        ido-use-filename-at-point t))

;; ack
(setq ack-prompt-for-directory t)

;; smex
(setq smex-prompt-string "smex "
      smex-save-file (concat dotfiles-dir "smex.save"))

;; YASnippet
(yas/initialize)
(yas/load-directory (concat dotfiles-dir "snippets"))

;; Yeah, the Hippies
(add-to-list 'hippie-expand-try-functions-list 'yas/hippie-try-expand)

;; More precise temporary file management
(setq version-control t
      kept-new-versions 16
      kept-old-versions 2
      delete-old-versions t
      backup-by-copying-when-linked t)

;; Keep and trash selected buffers
(midnight-delay-set 'midnight-delay 4400)
(add-to-list 'clean-buffer-list-kill-never-buffer-names "*msg*")
(add-to-list 'clean-buffer-list-kill-never-regexps "^\\*shell-")
(append clean-buffer-list-kill-buffer-names '("*Shell Command Output*"
                                              "*Completions*"
                                              "*Occur*"
                                              "*Bookmark List*"
                                              "*Ediff Registry*"
                                              "*ack*"
                                              "ri `"
                                              "*markdown-output*"))
(add-to-list 'clean-buffer-list-kill-regexps "\\.rb$")
(add-to-list 'clean-buffer-list-kill-regexps "\\.el$")

(provide 'misc)
;;; gary/misc.el ends here
