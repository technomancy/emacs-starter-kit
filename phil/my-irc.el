(eval-after-load 'erc
  '(progn
     (setq erc-prompt ">"
           erc-fill-column 70
           erc-hide-list '("JOIN" "PART" "QUIT" "NICK")
           erc-track-exclude-types (cons "353" erc-hide-list)
           erc-nick "technomancy"
           erc-autojoin-channels-alist
           '(("freenode.net" "#emacs" "#clojure" "#seattle.rb"
              "#leiningen" "#elpa")
             ("irc.sa2s.us" "#sonian" "#safe"))
           erc-prompt-for-nickserv-password nil)
     (require 'erc-highlight-nicknames)
     ;; (require 'erc-scrolltobottom)
     (require 'erc-services)
     (require 'erc-spelling)
     (erc-services-mode 1)
     (add-to-list 'erc-modules 'highlight-nicknames 'spelling)
     ;; (add-to-list 'erc-modules 'scrolltobottom)
     (add-hook 'erc-connect-pre-hook (lambda (x) (erc-update-modules)))
     ;; TODO: non-cycling-completion
     (set-face-foreground 'erc-input-face "dim gray")
     (set-face-foreground 'erc-my-nick-face "blue")
     (server-start)))
