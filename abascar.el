;;Commands for headless music playing for our home music server.

(defvar mpc-choices
  (split-string (shell-command-to-string
                 "find ~/music -type d | cut -c 18-") "\n"))

(defun mpc (command)
  (shell-command (format "mpc %s" command)))

(defun mpc-choose ()
  (interactive)
  (let ((chosen (ido-completing-read "Play: " mpc-choices)))
    (mpc "clear")
    (mpc (format "add %s" chosen))
    (mpc "play")))

(defun mpc-random ()
  (interactive)
  (mpc "clear")
  (mpc (format "add %s") (nth (random* (length mpc-choices)) mpc-choices))
  (mpc "play"))

(defun mpc-mode ()
  (interactive)
  (switch-to-buffer "*music*")
  (local-set-key (kbd "RET") 'mpc-choose)
  (local-set-key (kbd "<backspace>") 'mpc-random)
  (local-set-key (kbd "SPC") (lambda () (interactive) (mpc "toggle")))
  (local-set-key (kbd "<right>") (lambda () (interactive) (mpc "next")))
  (local-set-key (kbd "<left>") (lambda () (interactive) (mpc "prev"))))
