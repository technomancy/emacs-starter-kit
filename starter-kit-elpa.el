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

;; Work around a bug in ELPA
(ignore-errors (load "elpa/inf-ruby-2.0/inf-ruby-autoloads"))

(defun starter-kit-elpa-install ()
  "Install all starter-kit packages that aren't installed."
  (interactive)
  (dolist (package starter-kit-packages)
    (unless (or (member package package-activated-list)
                (functionp package))
      (message "Installing %s" (symbol-name package))
      (package-install package))))

(defun esk-online? ()
  (some (lambda (iface) (unless (equal "lo" (car iface))
                     (member 'up (first (last (network-interface-info
                                               (car iface)))))))
        (network-interface-list)))

;; On your first run, this should pull in all the base packages.
(when (esk-online?) (starter-kit-elpa-install))

(provide 'starter-kit-elpa)