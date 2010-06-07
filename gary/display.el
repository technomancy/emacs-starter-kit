;;; gary/display.el --- Perfecting Emacs's (lack of) veneer

;; Spartan appearance
(when window-system
  (if (eq window-system 'ns)
      ;; Use the Inconsolata font if running OS X
      (progn (setq ns-antialias-text t
                   ns-input-font "Inconsolata"
                   ns-input-fontsize 14
                   ns-pop-up-frames nil)
             (ns-respond-to-change-font))))

(blink-cursor-mode 1)

(set-frame-width (selected-frame) 161)

(if (not (eq window-system nil))
    ;; Vertical fringes of 1 pixel for each window
    (set-fringe-mode (quote (nil . (nil . nil)))))

;; Show buffer boundaries on the left-hande side of the fringe
(setq-default indicate-buffer-boundaries 'left)

(setq visible-bell nil
      display-time-24hr-format t
      whitespace-style '(trailing
                         lines
                         indentation
                         tabs
                         space-before-tab
                         space-after-tab)
      whitespace-line-column 79)

;; Modeline preferences
(display-time-mode)
(timeclock-modeline-display)
(size-indication-mode 1)

(set-default 'truncate-lines t)

;; Make unkept whitespace painfully annoying
(global-whitespace-mode t)

;; Highlight other instances of the symbol at point
(global-hi-lock-mode t)

(provide 'display)
;;; gary/display.el ends here
