;;; init.el --- Where all the magic begins
;;
;; Part of the Emacs Starter Kit
;;
;; This is the first thing to get loaded.
;;

;; Load up Org Mode and Org Babel for elisp embedded in Org Mode files
(setq dotfiles-dir (file-name-directory (or (buffer-file-name) load-file-name)))

(add-to-list 'load-path (expand-file-name
                         "lisp" (expand-file-name
                                 "org" (expand-file-name
                                        "src" dotfiles-dir))))

(add-to-list 'load-path (expand-file-name
                         "lisp" (expand-file-name
                                 "org-babel" (expand-file-name
                                              "src" dotfiles-dir))))

(require 'org-babel-init)

;; load up the main file
;; (org-babel-load-file (expand-file-name "starter-kit.org" dotfiles-dir))
(org-babel-load-file "starter-kit.org")

;;; init.el ends here