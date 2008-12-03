;; gist.el --- Emacs integration for gist.github.com
;; Copyright (C) 2008  Christian Neukirchen <purl.org/net/chneukirchen>
;; Copyright (C) 2008  Chris Wanstrath <chris@ozmm.org>
;; Licensed under the same terms as Emacs.

;; Version: 0.3
;; 25aug2008  +defunkt+
;; 21jul2008  +chris+

;; Ideas: fork

;;;###autoload
(defun gist-region (begin end)
  "Post the current region as a new paste at gist.github.com
Copies the URL into the kill ring."
  (interactive "r")
  (let* ((file (or (buffer-file-name) (buffer-name)))
         (name (file-name-nondirectory file))
         (ext (or (file-name-extension file) "txt"))
         (output (generate-new-buffer " *gist*")))
    (shell-command-on-region
     begin end
     (format (concat "curl -sS "
                     "-F 'file_ext[gistfile1]=.%s' "
                     "-F 'file_name[gistfile1]=%s' "
                     "-F 'file_contents[gistfile1]=<-' "
                     "http://gist.github.com/gists") ext name)
     output)
    (with-current-buffer output
      (re-search-backward "href=\"\\(.*\\)\"")
      (message "Paste created: %s" (match-string 1))
      (kill-new (match-string 1)))
    (kill-buffer output)))

;;;###autoload
(defun gist-buffer ()
  "Post the current buffer as a new paste at gist.github.com.
Copies the URL into the kill ring."
  (interactive)
  (gist-region (point-min) (point-max)))

(defvar gist-fetch-url "http://gist.github.com/%d.txt"
  "Raw Gist content URL format")

;;;###autoload
(defun gist-fetch (id)
  "Fetches a Gist and inserts it into a new buffer
If the Gist already exists in a buffer, switches to it"
  (interactive "nGist ID: ")

  (let* ((gist-buffer-name (format "*gist %d*" id)) 
         (gist-buffer (get-buffer gist-buffer-name)))
    (if (bufferp gist-buffer)
      (switch-to-buffer-other-window gist-buffer)
      (progn
        (message "Fetching Gist %d..." id)
        (setq gist-buffer 
              (url-retrieve-synchronously (format gist-fetch-url id)))
        (with-current-buffer gist-buffer 
          (rename-buffer gist-buffer-name t)
          (beginning-of-buffer)
          (search-forward-regexp "\n\n")
          (delete-region (point-min) (point))
          (set-buffer-modified-p nil))
        (switch-to-buffer-other-window gist-buffer)))))

(provide 'gist)
;;; gist.el ends here.