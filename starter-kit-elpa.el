;;; starter-kit-elpa.el --- Install a base set of packages automatically.
;;
;; Part of the Emacs Starter Kit

(defvar starter-kit-packages '("idle-highlight"
                               "ruby-mode"
                               "inf-ruby"
                               "js2-mode"
                               "css-mode"
                               "ert"
                               ;; To submit:
;;;                                "haml"
;;;                                "sass"
;;;                                "clojure-mode"
;;;                                "cheat"
;;;                                "gist"
;;;                                "magit"
;;;                                "yaml"
;;;                                "paredit"
;;;                                "html-fontify"
;;;                                "color-theme"
;;;                                "color-theme-zenburn"
;;;                                "color-theme-vivid-chalk"
                               ;; Complicated ones:
;;;                                "nxhtml"
;;;                                "rinari"
;;;                                "jabber"
;;;                                "slime"
;;;                                "swank-clojure"
                               )
  "Libraries that should be installed by default.")