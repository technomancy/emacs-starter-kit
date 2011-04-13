(add-to-list 'load-path (concat dotfiles-dir "/local"))

(setq package-archives '(("ELPA" . "http://tromey.com/elpa/") 
                           ("gnu" . "http://elpa.gnu.org/packages/")))
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))

(require 'my-viper)
(require 'my-org-mode)
(require 'my-remember)

(define-key viper-vi-global-user-map "C-c /" 'org-sparse-tree)

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

(global-set-key (kbd "C-x C-b") 'ibuffer)

;; bind Caps-Lock to M-x
;; http://sachachua.com/wp/2008/08/04/emacs-caps-lock-as-m-x/
;; of course, this disables normal Caps-Lock for *all* apps...
(if (eq window-system 'x)
    (shell-command "xmodmap -e 'clear Lock' -e 'keycode 66 = F13'"))
(global-set-key [f13] 'execute-extended-command)

(defun djcb-full-screen-toggle ()
  "toggle full-screen mode"
  (interactive)
  (shell-command "wmctrl -r :ACTIVE: -btoggle,fullscreen"))
(global-set-key (kbd "<f11>")  'djcb-full-screen-toggle)

;; move to current desktop 
(add-hook 'server-switch-hook
  (lambda ()
    (call-process
      "wmctrl" nil nil nil "-i" "-R"
      (frame-parameter (or frame (selected-frame)) 'outer-window-id))))

(setq espresso-indent-level 4)

; (if (display-graphic-p)

(add-hook 'window-setup-hook 
          (lambda nil 
            (set-default-font "Bitstream Vera Sans Mono-14")
            (set-fontset-font (frame-parameter nil 'font)
                              'han '("cwTeXHeiBold" . "unicode-bmp")))
          (setq default-frame-alist '((top . 0) (left . 0) (width . 100) (height . 40)))) 

; (setq default-frame-alist '((font . "Inconsolata-dz-15")))
(setq default-frame-alist '((font . "Bitstream Vera Sans Mono 14")))
(mouse-avoidance-mode 'animate)

;; http://www.emacswiki.org/emacs/OverlaysToText
(defun overlays-to-text ()
  "Create a new buffer called *text* containing the visible text
of the current buffer, ie. it converts overlays containing text
into real text."
  (interactive)
  (let ((tb (get-buffer-create "*text*"))
        (s (point-min))
        (os (overlays-in (point-min) (point-max))))
    (with-current-buffer tb
      (erase-buffer))
    (setq os (sort os (lambda (o1 o2)
                        (< (overlay-start o1)
                           (overlay-start o2)))))
    (mapc (lambda (o)
            (let ((bt (buffer-substring-no-properties s (overlay-start o)))
                  (b (overlay-get o 'before-string))
                  (text (buffer-substring-no-properties (overlay-start o) (overlay-end o)))
                  (a (overlay-get o 'after-string))
                  (inv (overlay-get o 'invisible)))
              (with-current-buffer tb
                (insert bt)
                (unless inv
                  (when b (insert b))
                  (insert text)
                  (when a (insert a))))
              (setq s (overlay-end o))))
          os)
    (let ((x (buffer-substring-no-properties s (point-max))))
      (with-current-buffer tb
        (insert x)))
    (pop-to-buffer tb)))

