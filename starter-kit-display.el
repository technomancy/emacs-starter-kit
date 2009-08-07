;;; starter-kit-display.el --- Tweaking the look and feel of Emacs

;; Spartan appearance
(when window-system
  (setq frame-title-format '(buffer-file-name "%f" ("%b")))
  (tooltip-mode -1)
  (scroll-bar-mode -1)
  (if (eq window-system 'ns)
      ;; Use the Inconsolata font if running OS X
      (progn (setq ns-antialias-text t
                   ns-input-font "Inconsolata"
                   ns-input-fontsize 14
                   ns-pop-up-frames nil)
             (ns-respond-to-change-font))))

(blink-cursor-mode 1)
(menu-bar-mode -1)

(add-hook 'before-make-frame-hook 'turn-off-tool-bar)

;; Vertical fringes of 1 pixel for each window
(set-fringe-mode (quote (nil . (nil . nil))))

;; Show buffer boundaries on the left-hande side of the fringe
(setq-default indicate-buffer-boundaries 'left)

;; Modeline preferences
(timeclock-modeline-display)
(display-time-mode 1)
(column-number-mode 1)

(setq inhibit-startup-message t
      visible-bell nil
      color-theme-is-global t
      font-lock-maximum-decoration t
      whitespace-style '(trailing lines space-before-tab
                                  indentation space-after-tab)
      whitespace-line-column 80
      )

(set-default 'truncate-lines t)
(set-default 'indent-tabs-mode nil)
(set-default 'indicate-empty-lines t)

;; Highlight the current line
;; FIXME:
(global-hl-line-mode t)

;; Enable syntax highlighting for older Emacsen that have it off
(global-font-lock-mode t)

;; Make unkept whitespace painfully annoying
;; TODO: learn more about whitespace-mode's options
(global-whitespace-mode)

;; TODO
(global-hi-lock-mode t)

;; Highlight matching parentheses when the point is on them.
(show-paren-mode 1)

;; Courtesy of http://emacs-fu.blogspot.com/2009/02/transparent-emacs.html
(defun opacity-modify (&optional dec)
  "Modify the transparency of the emacs frame; if DEC is t,
    decrease the transparency, otherwise increase it in 10%-steps"
  (let* ((alpha-or-nil (frame-parameter nil 'alpha)) ; nil before setting
         (oldalpha (if alpha-or-nil alpha-or-nil 100))
         (newalpha (if dec (- oldalpha 10) (+ oldalpha 10))))
    (when (and (>= newalpha frame-alpha-lower-limit) (<= newalpha 100))
      (modify-frame-parameters nil (list (cons 'alpha newalpha))))))

(provide 'starter-kit-display)
;;; starter-kit-display.el ends here