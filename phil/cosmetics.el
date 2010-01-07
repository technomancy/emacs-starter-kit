(defalias 'zb 'color-theme-zenburn)
(defalias 'bb 'color-theme-blackboard)
(defalias 'ff 'find-file)

(defun inconsolata (size)
  (interactive "p")
  (set-default-font
   (concat "-unknown-Inconsolata-normal-normal-normal-*-"
           (if (stringp size) size
             (if (= 1 size) "16"
               (read-from-minibuffer "Size: ")))
           "-*-*-*-m-0-*-*")))

;; If we don't have XFT, let's at least pick a decent default.
(if (< emacs-major-version 23)
    (ignore-errors
      (set-default-font (concat "-xos4-terminus-medium-r-normal--"
                                "16-160-72-72-c-80-iso8859-1"))))
