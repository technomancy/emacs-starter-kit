;; automatically add subdirectories (or symbolic links to directories) to
;; the load-path.  If order is important name directories with numeric
;; prefixes

(dolist (dir (reverse (directory-files init-dir)))
  (let ((adir (expand-file-name dir init-dir)))
    (when (and (file-directory-p adir) (not (equal (substring dir 0 1) ".")))
      (add-to-list 'load-path adir))))

