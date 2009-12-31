;; don't use tabs
(setq-default indent-tabs-mode nil)
 
; interpret and use ansi color codes in shell output windows
(ansi-color-for-comint-mode-on)
 
;; My data
(setq user-mail-address "acadavid@gmail.com")
(setq user-full-name "Alejandro Cadavid")

;; Font setup
(set-face-font 'default "-unknown-Inconsolata-normal-normal-normal-*-16-*-*-*-m-0-iso10646-1")

;; personal preferences
(mouse-wheel-mode t)
(line-number-mode 1)
(column-number-mode 1)
(global-font-lock-mode t)
(show-paren-mode 1)
(setq show-parent-style 'expression)
(prefer-coding-system 'utf-8)

;; use y/n instead of yes/no
(defalias 'yes-or-no-p 'y-or-n-p)

;; Theme loading
(color-theme-zenburn)

;; default window sizes
(set-frame-position (selected-frame) 0 0)
(set-frame-size (selected-frame) 220 60)

;; Load rspec-mode
(require 'rspec-mode)
