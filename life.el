(add-to-list 'load-path (concat dotfiles-dir "local"))
(add-to-list 'load-path (concat dotfiles-dir "vendor"))
(add-to-list 'load-path "/usr/share/emacs/site-lisp")

(setq vendor-dir (concat dotfiles-dir "vendor"))
(add-to-list 'load-path vendor-dir)

(setq package-archives '(("ELPA" . "http://tromey.com/elpa/") 
                           ("gnu" . "http://elpa.gnu.org/packages/")))
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))

(require 'my-vim)
(require 'my-org-mode)
(require 'sunrise-commander)
(require 'undo-tree)
(require 'ledger)

(global-undo-tree-mode)

(require 'multi-term)
(setq multi-term-program "/bin/bash")

(color-theme-zenburn)

(eval-after-load 'mumamo
  '(eval-after-load 'zenburn
     '(ignore-errors (set-face-background
                      'mumamo-background-chunk-submode1 "#4F3F3F")
                     (set-face-background
                      'mumamo-background-chunk-submode2 "#3F4F3F")
                     (set-face-background
                      'mumamo-background-chunk-submode3 "#3F3F4F"))))
(require 'flymake)

(set-face-attribute 'flymake-warnline nil
                    :background zenburn-bg
                    :underline "#ff7700")
(set-face-attribute 'flymake-errline nil
                    :background zenburn-bg
                    :underline "#ff0000")

(require 'rvm)
(rvm-use-default)

(defun shutdown-emacs-server () (interactive)
  (when (not (eq window-system 'x))
    (message "Initializing x windows system.")
    (x-initialize-window-system)
    (when (not x-display-name) (setq x-display-name (getenv "DISPLAY")))
    (select-frame (make-frame-on-display x-display-name '((window-system . x))))
    )
  (let ((last-nonmenu-event nil)(window-system "x"))(save-buffers-kill-emacs)))

(require 'ibuffer) 
(setq ibuffer-saved-filter-groups
      (quote (("default"      
               ("Org" ;; all org-related buffers
                (mode . org-mode))  
               ("Programming"
                (or
                 (mode . ruby-mode)
                 (mode . perl-mode)
                 (mode . python-mode)
                 (mode . emacs-lisp-mode)
                 )) 
               ))))

(add-hook 'ibuffer-mode-hook
          (lambda ()
            (ibuffer-switch-to-saved-filter-groups "default")))

(global-set-key (kbd "C-z") nil)
(global-set-key (kbd "C-x C-b") 'ibuffer)

(setq browse-url-browser-function (quote browse-url-generic))
(setq browse-url-generic-program "chromium")

(defun djcb-full-screen-toggle ()
  (interactive)
  (shell-command "wmctrl -r :ACTIVE: -btoggle,fullscreen"))
(global-set-key (kbd "<f11>")  'djcb-full-screen-toggle)

(add-hook 'server-switch-hook
  (lambda ()
    (call-process
      "wmctrl" nil nil nil "-i" "-R"
      (frame-parameter (or frame (selected-frame)) 'outer-window-id))))

(setq espresso-indent-level 4)

(if (eq window-system 'x) (add-hook 'window-setup-hook 
          (lambda nil 
            (set-default-font "Bitstream Vera Sans Mono-14")
            (set-fontset-font (frame-parameter nil 'font)
                              'han '("cwTeXHeiBold" . "unicode-bmp")))
          (setq default-frame-alist '((top . 0) (left . 0) (width . 100) (height . 40)))))

(setq default-frame-alist '((font . "Bitstream Vera Sans Mono 14")))
(mouse-avoidance-mode 'exile)

(eval-after-load "wdired"
  '(progn
     (eval-after-load "viper"
       '(progn
          (defadvice wdired-change-to-wdired-mode (after viper activate)
            (unless (eq viper-current-state 'emacs-state)
              (viper-change-state 'vi-state)))
          (defadvice wdired-finish-edit (after viper activate)
            (unless (eq viper-current-state 'emacs-state)
              (viper-change-state-to-vi)) ; back to normal state
            (viper-modify-major-mode    ; back to dired map
             'dired-mode 'vi-state dired-mode-map))))))
  
(when (require 'rainbow-delimiters nil 'noerror) 
  (add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode))

(setq flyspell-issue-welcome-flag nil)

(toggle-debug-on-error)

(load "auctex.el" nil t t)
(load "preview-latex.el" nil t t)

(defun indent-buffer ()
  (interactive)
  (save-excursion
    (indent-region (point-min) (point-max) nil)))

(setenv "DATABASE_URL" "postgresql://localhost:5432/tm_participation")
