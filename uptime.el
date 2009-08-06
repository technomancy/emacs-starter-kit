;; http://interglacial.com/~sburke/pub/emacs/sburke_dot_emacs.config

(setq uptime-time-init (current-time))

(defun uptime () "Report emacs's uptime" (interactive)
  (let* ((tm (current-time))
         (diff (list (- (car tm) (car uptime-time-init))
                     (- (cadr tm) (cadr uptime-time-init))))
         (seconds (+ (* (float (car diff)) 65536) (float (cadr diff))))
         (days  (floor (/ seconds 86400)))
         (hours (progn (decf seconds (* days  86400)) (floor (/ seconds 3600))))
         (mins  (progn (decf seconds (* hours 3600))  (floor (/ seconds 60)))))
    (message "Emacs has been running %dd %02dh %02dm" days hours mins)))

(setq start-time (current-time))
