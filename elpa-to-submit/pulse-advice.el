;; Taken from CEDET's pulse.el before it was merged to Emacs

(defun pulse-toggle-integration-advice (arg)
  "Toggle activation of advised functions that will now pulse.
Wint no ARG, toggle the pulse advice.
With a negative ARG, disable pulse advice.
With a positive ARG, enable pulse advice.
Currently advised functions include:
  `goto-line'
  `exchange-point-and-mark'
  `find-tag'
  `tags-search'
  `tags-loop-continue'
  `pop-tag-mark'
  `imenu-default-goto-function'
Pulsing via `pulse-line-hook-function' has also been added to
the following hook:
  `next-error-hook'"
  (interactive "P")
  (if (null arg)
      (setq pulse-command-advice-flag (not pulse-command-advice-flag))
    (if (< (prefix-numeric-value arg) 0)
	(setq pulse-command-advice-flag nil)
      (setq pulse-command-advice-flag t)))
  (if pulse-command-advice-flag
      (message "Pulse advice enabled")
    (message "Pulse advice disabled")))

(defadvice goto-line (after pulse-advice activate)
  "Cause the line that is `goto'd to pulse when the cursor gets there."
  (when (and pulse-command-advice-flag (interactive-p))
    (pulse-momentary-highlight-one-line (point))))

(defadvice exchange-point-and-mark (after pulse-advice activate)
  "Cause the line that is `goto'd to pulse when the cursor gets there."
  (when (and pulse-command-advice-flag (interactive-p)
	     (> (abs (- (point) (mark))) 400))
    (pulse-momentary-highlight-one-line (point))))

(defadvice find-tag (after pulse-advice activate)
  "After going to a tag, pulse the line the cursor lands on."
  (when (and pulse-command-advice-flag (interactive-p))
    (pulse-momentary-highlight-one-line (point))))

(defadvice tags-search (after pulse-advice activate)
  "After going to a hit, pulse the line the cursor lands on."
  (when (and pulse-command-advice-flag (interactive-p))
    (pulse-momentary-highlight-one-line (point))))

(defadvice tags-loop-continue (after pulse-advice activate)
  "After going to a hit, pulse the line the cursor lands on."
  (when (and pulse-command-advice-flag (interactive-p))
    (pulse-momentary-highlight-one-line (point))))

(defadvice pop-tag-mark (after pulse-advice activate)
  "After going to a hit, pulse the line the cursor lands on."
  (when (and pulse-command-advice-flag (interactive-p))
    (pulse-momentary-highlight-one-line (point))))

(defadvice imenu-default-goto-function (after pulse-advice activate)
  "After going to a tag, pulse the line the cursor lands on."
  (when pulse-command-advice-flag
    (pulse-momentary-highlight-one-line (point))))

(pulse-toggle-integration-advice t)

(provide 'pulse-advice)