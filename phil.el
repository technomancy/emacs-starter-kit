;;; My personalizations

;; elisp libraries I run from source checkouts:

(add-to-list 'load-path "/home/phil/src/emacs-w3m")
(add-to-list 'load-path "/home/phil/src/relax.el")
(add-to-list 'load-path "/home/phil/src/magit")

(autoload 'magit-status "magit" "magit" t)

(add-to-list 'package-archives
             '("technomancy" . "http://repo.technomancy.us/emacs/") t)

(autoload 'w3m "w3m" "w3m browser" t)
(autoload 'relax "relax" "Connect to the CouchDB database at db-url." t)
(autoload 'garak "garak" "Start Garak IM session." t)

(ignore-errors (load "../../paredit/paredit-beta")
               (load "../../paredit/paredit-delimiter-space")
               (load "../../paredit/paredit-semicolon"))

;; Random stuff

(setq-default save-place t)
(setq whitespace-line-column 80)

(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "/home/phil/src/conkeror/contrib/run-conkeror")

(org-remember-insinuate)

(global-set-key (kbd "C-c C-r") 'remember)

(when nil ; use trunk slime?
  (add-to-list 'load-path "/home/phil/src/slime")
  (add-to-list 'load-path "/home/phil/src/slime/contrib")
  (require 'slime))
