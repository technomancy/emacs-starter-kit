(setq package-user-dir (concat dotfiles-dir "elpa"))
(add-to-list 'load-path package-user-dir)
(require 'package)

(add-to-list 'package-archives  '("elpa" . "http://tromey.com/elpa/"))
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)
