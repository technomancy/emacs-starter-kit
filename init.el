;; init.el -- customization bootstrap 

(setq dotfiles-dir (file-name-directory
                    (or (buffer-file-name) load-file-name)))

(add-to-list 'load-path dotfiles-dir)

;; Automatically load all .el files in the init.d directory
(setq init-dir (concat dotfiles-dir "init.d"))

(when (file-exists-p init-dir)
    (add-to-list 'load-path init-dir)
    (mapc #'load (directory-files init-dir nil "^[^.].*el$")))


;; KEEP THIS FILE CLEAN
;;
;; Instead of copying and pasting snippets of elisp,
;; lets at the very least increase the distribution 
;; unit to an entire file.  Ideally lets build a 
;; community around elpa and mamalade to share common
;; features with packages rather than snippets.
;; 
;; To add additional customizations create a new file ending in .el
;; in the init.d directory and it will be loaded automatically.
;; prefix files with numeric indexes to control load order

;; end init.el
