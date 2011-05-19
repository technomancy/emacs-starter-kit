;;; starter-kit-elpa.el --- Install a base set of packages automatically.
;;
;; Part of the Emacs Starter Kit

(require 'cl)

(defvar starter-kit-packages (list 'idle-highlight-mode
                                   'ruby-mode
                                   'inf-ruby
                                   'css-mode
                                   'yaml-mode
                                   'find-file-in-project
                                   'magit
                                   'gist)
  "Libraries that should be installed by default.")

(defun starter-kit-elpa-install ()
  "Install all starter-kit packages that aren't installed."
  (interactive)
  (dolist (package starter-kit-packages)
    (unless (or (member package package-activated-list)
                (functionp package))
      (message "Installing %s" (symbol-name package))
      (package-install package))))

(defun esk-online? ()
  "See if we're online.

Windows does not have the network-interface-list function, so we
just have to assume it's online."
  ;; TODO how could this work on Windows?
  (if (and (functionp 'network-interface-list)
           (network-interface-list))
      (some (lambda (iface) (unless (equal "lo" (car iface))
                         (member 'up (first (last (network-interface-info
                                                   (car iface)))))))
            (network-interface-list))
    t))

;; On your first run, this should pull in all the base packages.
(when (esk-online?)
  (unless package-archive-contents (package-refresh-contents))
  (starter-kit-elpa-install))

;; Workaround for an ELPA bug that people are reporting but I've been
;; unable to reproduce:
(autoload 'paredit-mode "paredit" "" t)

;; Workaround for bug in the ELPA package for yaml-mode
(autoload 'yaml-mode "yaml-mode" "" t)

(provide 'starter-kit-elpa)
