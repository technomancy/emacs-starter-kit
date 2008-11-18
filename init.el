;;; init.el --- Where all the magic begins
;;
;; Part of the Emacs Starter Kit
;;
;; This is the first thing to get loaded.
;;
;; "Emacs outshines all other editing software in approximately the
;; same way that the noonday sun does the stars. It is not just bigger
;; and brighter; it simply makes everything else vanish."
;; -Neal Stephenson, "In the Beginning was the Command Line"

;; Temporary debugging stuff:

(toggle-debug-on-error)
;;; Fix for a bug in CVS Emacs 2 April 08; remove when fixed upstream:
(require 'cl)
(defun handle-shift-selection (&rest args))

;; Load path
(setq dotfiles-dir (file-name-directory
                    (or (buffer-file-name) load-file-name)))
(add-to-list 'load-path dotfiles-dir)
(add-to-list 'load-path (concat dotfiles-dir "/elpa"))
(add-to-list 'load-path (concat dotfiles-dir "/elpa-to-submit"))

;; Autoloads can be regenerated for you automatically if the file is
;; too old:

(let ((autoload-file (concat dotfiles-dir "loaddefs.el")))
  (if (or (not (file-exists-p autoload-file))
          ;; TODO: make this more readable
          (< (+ (car (nth 5 (file-attributes autoload-file))) 20)
             (car (current-time))))
      (let ((generated-autoload-file autoload-file))
        (message "Updating autoloads...")
        (update-directory-autoloads dotfiles-dir)))
  (load autoload-file))

;; Some libraries don't have the necessary autoloads set up.

(autoload 'lisppaste-paste-region "lisppaste" "" t)
(autoload 'jabber-connect "jabber" "" t)
(autoload 'cheat "cheat" "" t)
(autoload 'magit-status "magit" "" t)

;; These should be loaded on startup rather than autoloaded on demand
;; since they are likely to be used in every session:

(require 'cl)
(require 'saveplace)
(require 'ffap)
(require 'uniquify)
(require 'ansi-color)
(require 'recentf)

;; Load up ELPA:

(require 'package)
(package-initialize)

;; Load up starter kit customizations:

(require 'starter-kit-lisp)
(require 'starter-kit-defuns)
(require 'starter-kit-bindings)
(require 'starter-kit-misc)
(require 'starter-kit-registers)
(require 'starter-kit-eshell)
(require 'starter-kit-ruby)

;; You can keep system-specific customizations here:

(setq system-specific-config
      (concat dotfiles-dir system-name ".el"))
(if (file-exists-p system-specific-config)
    (load system-specific-config))

;;; init.el ends here