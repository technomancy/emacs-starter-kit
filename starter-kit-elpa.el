;;; starter-kit-elpa.el --- Install a base set of packages automatically.
;;
;; Part of the Emacs Starter Kit

(defvar starter-kit-packages (list 'idle-highlight
                                   'ruby-mode
                                   'inf-ruby
                                   'js2-mode
                                   'css-mode
                                   'nxml
                                   'gist
                               ;; To submit:
;;;                                "magit"
;;;                                "paredit"
;;;                                "clojure-mode"
;;;                                "yaml"
;;;                                "haml"
;;;                                "sass"
;;;                                "cheat"
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

(defun starter-kit-elpa-install ()
  "Install all starter-kit packages that aren't installed."
  (interactive)
  (dolist (package starter-kit-packages)
    (unless (member package package-activated-list)
      (message "Installing %s" (symbol-name package))
      (package-install package))))

;; On your first run, this should pull in all the base packages.
;; But you might not be online, so ignore errors.
(ignore-errors
  (message "Checking base list of packages...")
  (starter-kit-elpa-install))

(provide 'starter-kit-elpa)