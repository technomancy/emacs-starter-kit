(eval-after-load 'erc
  '(progn
     (setq erc-prompt ">"
           erc-fill-column 70
           erc-hide-list '("JOIN" "PART" "QUIT" "NICK")
           erc-track-exclude-types (append '("324" "329" "332" "333" "353" "477")
                                           erc-hide-list)
           erc-nick "technomancy"
           erc-autojoin-timing :ident
           erc-autojoin-channels-alist
           '(("freenode.net" "#emacs" "#clojure" "#seattle.rb"
              "#leiningen" "#jclouds"))
           erc-prompt-for-nickserv-password nil)
     (require 'erc-services)
     (require 'erc-spelling)
     (erc-services-mode 1)
     (add-to-list 'erc-modules 'highlight-nicknames 'spelling)
     (add-hook 'erc-connect-pre-hook (lambda (x) (erc-update-modules)))
     (set-face-foreground 'erc-input-face "dim gray")
     (set-face-foreground 'erc-my-nick-face "blue")
     (server-start)))

(setq pcomplete-cycle-completions nil)

(defun erc-connect ()
  (interactive)
  (require 'erc)
  (erc :server "home.hagelb.org" :password dirc-password))

(load (expand-file-name "~/.passwords.el"))

(setq erc-nickserv-passwords
      `((freenode     (("technomancy" . ,freenode-password)))))

(defun clean-message (s)
  (setq s (replace-regexp-in-string "'" "&apos;" 
  (replace-regexp-in-string "\"" "&quot;"
  (replace-regexp-in-string "&" "&amp;" 
  (replace-regexp-in-string "<" "&lt;"
  (replace-regexp-in-string ">" "&gt;" s)))))))

(defun call-libnotify (matched-type nick msg)
  (let* ((cmsg  (split-string (clean-message msg)))	      
         (nick   (first (split-string nick "!")))
         (msg    (mapconcat 'identity (rest cmsg) " ")))
    (shell-command-to-string
     (format "notify-send -i /home/phil/src/emacs/etc/images/icons/hicolor/scalable/apps/emacs.svg '%s says:' '%s'"
	     nick msg))))

(add-hook 'erc-text-matched-hook 'call-libnotify)
