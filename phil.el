;;; My personalizations

;; elisp libraries I run from source checkouts:

(add-to-list 'load-path "/home/phil/src/emacs-w3m")
(add-to-list 'load-path "/home/phil/src/relax.el")
(add-to-list 'load-path "/home/phil/src/elim/elisp")

(autoload 'w3m "w3m" "w3m browser" t)
(autoload 'relax "relax" "Connect to the CouchDB database at db-url." t)
(autoload 'garak "garak" "Start Garak IM session." t)

(load "../../paredit/paredit-beta")
(load "../../paredit/paredit-delimiter-space")
(load "../../paredit/paredit-semicolon")

;; Random stuff

(setq-default save-place t)
(setq whitespace-line-column 80)

(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program
      "/home/phil/src/conkeror/contrib/run-conkeror")

