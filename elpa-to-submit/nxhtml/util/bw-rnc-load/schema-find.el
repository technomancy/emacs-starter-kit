
(defun find-nxml (lst)
   (if (null lst)
       nil
     (if (string-match "nxml" (car lst))
         (car lst)
       (find-nxml (cdr lst)))))
find-nxml


(defun check-loader ()
  (let ((loader-path "./emacs/site-lisp/nxhtml/etc/schema/xhtml-loader.rnc"))
    (if (file-exists-p loader-path)
        nil
      (progn (find-file loader-path)
             (insert (concat "include \"" (find-nxml load-path) "/../../etc/schema/xhtml.rnc\""))
             (save-buffer)
             (kill-this-buffer)))))
