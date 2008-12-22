
(defvar temp-html
  ;; Start-here
  "
<html>
<head>
</head>
</html>
") ;; Stop-here

(defun temp-c-m ()
  (let* ((temp-buffer (get-buffer-create "temp-c-m"))
         (start (save-excursion
                  (goto-char (point-min))
                  (search-forward "Start-here")
                  (forward-line 2)
                  (point)))
         (end (save-excursion
                (goto-char start)
                (search-forward "Stop-here")
                (beginning-of-line)
                (point)))
         (command 
          (concat "c:/dl/programs/Tidy.exe"
                  ))
         (coding-system-for-read 'undecided-dos)
         (coding-system-for-write 'undecided-dos)
         )
    (with-current-buffer temp-buffer
      (erase-buffer))
    (shell-command-on-region start end command temp-buffer nil)
    (with-current-buffer temp-buffer
      (goto-char (point-max))
      (insert (current-time-string) "\n\n"))
    ))

(temp-c-m)
