;;; gary/defuns.el --- Tweaking the look and feel of Emacs

;; Daemonization

(if (daemonp)
    (progn
      (defun spartan-gui ()
        "Restore the look and feel that running in daemon mode cannot account
for when attached via a GUI client."
        (interactive)
        (setq ns-command-modifier 'meta)

        ;; FIXME: blows the daemon up when called twice
        ;; (setq ns-antialias-text t
        ;;       ns-input-font "Inconsolata"
        ;;       ns-input-fontsize 14
        ;;       ns-pop-up-frames nil)
        ;; (ns-respond-to-change-font)

        ;; restore look and feel
        (color-theme-blackboard)
        (opacity-modify t)
        (set-frame-width (selected-frame) 161) ; 2 frames @ 80 chars + fringe
        (set-frame-height (selected-frame) 71))

      (defun spartan-cli ()
        "Restore the look and feel that running in daemon mode cannot account
for when attached via a CLI client"
        (interactive)
        ;; dark in a tty, tolerable in the gui
        (color-theme-zenburn))))

(defun refresh-buffer ()
  "Refresh the current buffer from disk"
  (interactive)
  (revert-buffer t t))

(defun rename-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not filename)
        (message "Buffer '%s' is not visiting a file!" name)
      (if (get-buffer new-name)
          (message "A buffer named '%s' already exists!" new-name)
        (progn
          (rename-file name new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil))))))

;; Window-related

(defun swap-windows ()
 "If you have 2 windows, it swaps them."
 (interactive)
 (cond ((not (= (count-windows) 2))
        (message "You need exactly 2 windows to do this."))
       (t
        (let* ((w1 (first (window-list)))
               (w2 (second (window-list)))
               (b1 (window-buffer w1))
               (b2 (window-buffer w2))
               (s1 (window-start w1))
               (s2 (window-start w2)))
          (set-window-buffer w1 b2)
          (set-window-buffer w2 b1)
          (set-window-start w1 s2)
          (set-window-start w2 s1)))))

(defun swap-split () ; TODO: broken as of 23; rewrite
  "Swaps the orientation of two split windows."
  (interactive)
  (save-excursion
    (let ((b2 (window-buffer (second (window-list))))
          (side-by-side (not (window-split-horizontally-p))))
      (if (one-window-p)
          (message "You need exactly 2 windows to do this.")
        (delete-other-windows)
        (if side-by-side
            (progn
              (split-window-vertically)         ; spatial
              (message "Swapped horizontally")) ; visual!
          (split-window-horizontally (/ (third (window-edges)) 2))
          (message "Swapped vertically")))
      (display-buffer b2 t nil))))

(defun vi-open-next-line (arg)
  "Move to the next line (like vi) and then opens a line."
  (interactive "p")
  (if (looking-at "^")
      (open-line arg)
    (end-of-line)
    (open-line arg)
    (next-line 1)
    (indent-according-to-mode)))

(defun zap-up-to-char (arg char)
  "Kill up to and excluding ARG'th occurrence of CHAR.
Goes backward if ARG is negative; error if CHAR not found."
  (interactive "*p\ncZap up to char: ")
  (kill-region (point)
               (progn
                 (search-forward
                  (char-to-string char) nil nil arg)
                 (progn (goto-char
                         (if (> arg 0) (1- (point)) (1+ (point))))
                        (point)))))

;; Courtesy of http://emacs-fu.blogspot.com/2009/02/transparent-emacs.html
(defun opacity-modify (&optional dec)
  "Modify the transparency of the emacs frame; if DEC is t,
    decrease the transparency, otherwise increase it in 10%-steps"
  (let* ((alpha-or-nil (frame-parameter nil 'alpha)) ; nil before setting
         (oldalpha (if alpha-or-nil alpha-or-nil 100))
         (newalpha (if dec (- oldalpha 10) (+ oldalpha 10))))
    (when (and (>= newalpha frame-alpha-lower-limit) (<= newalpha 100))
      (modify-frame-parameters nil (list (cons 'alpha newalpha))))))

(provide 'defuns)
;;; gary/defuns.el ends here
