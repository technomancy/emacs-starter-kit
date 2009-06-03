;;; Fonts

(defun inconsolata (size)
  (interactive "p")
  (set-default-font
   (concat "-unknown-Inconsolata-normal-normal-normal-*-"
           (if (stringp size) size
             (if (= 1 size) "18"
               (read-from-minibuffer "Size: ")))
           "-*-*-*-m-0-*-*")))

(defun envy (size)
  (interactive "p")
  (set-default-font
   (concat "-unknown-Envy Code R-normal-normal-normal-*-"
           (if (stringp size) size
             (if (= 1 size) "16"
               (read-from-minibuffer "Size: ")))
           "-*-*-*-m-0-iso10646-1")))

(defun dejavu (size)
  (interactive "p")
  (set-default-font
   (concat "-dejavu-dejavu sans mono-medium-r-normal--"
           (if (stringp size) size
             (if (= 1 size) "16"
               (read-from-minibuffer "Size: ")))
           "-*-0-0--iso8859-1")))

;;; IRC

(eval-after-load 'rcirc
  '(progn
     (require 'rcirc-color)
     (require 'rcirc-completion)
     (require 'rcirc-reconnect)

     (require 'dbus)
     (dbus-register-signal :system "org.freedesktop.NetworkManager"
                      "/org/freedesktop/NetworkManager" "org.freedesktop.NetworkManager"
                      "StateChanged"
                      'handle-network-state-change)

     (setq rcirc-authinfo '(("freenode" nickserv "technomancy" "technomancy"))
           rcirc-default-nick "technomancy"
           rcirc-server-alist '(("irc.freenode.net" :channels ("#emacs" "#seattle.rb" "#clojure"))))

     (add-hook 'rcirc-mode-hook (lambda ()
                                  (set (make-local-variable 'scroll-conservatively)
                                       8192)
                                  (rcirc-track-minor-mode 1)
                                  (rcirc-omit-mode)
                                  (flyspell-mode 1)))))

(defun handle-network-state-change (&rest state)
  ;; this doesn't seem to work
  (when (member 4 state)
    (message "Disconnected!")
    (rcirc-cmd-quit "disconnected"))
  ;; 3 is magic-dbus-speak for "CONNECTED"
  (when (member 3 state)
    (message "Connected!")
    (rcirc nil)))

(defun irc ()
  (interactive)
  (inconsolata 1)
  (rcirc nil)
  (split-window-horizontally))

;;; elisp libraries I run from source checkouts:

(ignore-errors
  (add-to-list 'load-path "/home/phil/src/elisp/emacs-w3m")
  (require 'w3m))

(ignore-errors
  (add-to-list 'load-path "/home/phil/src/elisp/relax.el")
  (autoload 'relax "relax" "Connect to the CouchDB database at db-url." t))

(ignore-errors
  ;; Instant MESSAGING!
  (add-to-list 'load-path "/home/phil/src/elisp/elim/elisp")
  (autoload 'garak "garak" "Start Garak IM session." t))

;;; Random stuff

;; If we don't have XFT, let's at least pick a decent default.
(if (< emacs-major-version 23)
    (ignore-errors
      (set-default-font "-xos4-terminus-medium-r-normal--16-160-72-72-c-80-iso8859-1")))

(eval-after-load 'ruby-mode
  '(add-hook 'ruby-mode-hook 'ruby-electric-mode))

(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "/home/phil/src/js/conkeror/contrib/run-conkeror")

(setq clojure-src-root "/home/phil/src/clj")
(clojure-slime-config)

(eval-after-load 'clojure-mode
  '(load "../../../elisp/clojure-mode/clojure-test-mode"))

(add-hook 'clojure-mode-hook 'turn-on-whitespace)

;; unfortunately some codebases use tabs. =(
(set-default 'tab-width 4)

;; javadoc
(global-set-key (kbd "C-h j") 'javadoc-lookup)

(setq *jdh-javadocs*
      '(("/home/phil/documents/javadoc/jdk/api/" t t nil nil nil nil nil nil nil)
        ("/home/phil/documents/javadoc/javamail/javadocs/" t t nil nil nil nil nil nil nil)))

;; Get around the emacswiki spam protection
(add-hook 'oddmuse-mode-hook
          (lambda ()
            (unless (string-match "question" oddmuse-post)
              (setq oddmuse-post (concat "uihnscuskc=1;" oddmuse-post)))))

(defun eshell/rm (&rest args)
  "Eshell's built-in rm is ridiculously slow."
  (shell-command (format "rm %s" (mapconcat 'identity args " "))))
