;;; broken ido
(defun ido-directory-too-big-p (arg) nil)

;; awesome sometimes, but right now more trouble than it's worth
(setq tramp-mode nil
      tramp-unload-hook nil
      ido-enable-tramp-completion nil)
(fmakunbound 'eshell/sudo)
(fmakunbound 'eshell/su)
