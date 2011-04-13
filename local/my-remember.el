(org-remember-insinuate)

(setq org-remember-templates( 
                              quote (
                                     ("inbox" ?i "* inbox %?\n  %u\n  %a" "~/Documents/gtd/inbox.org" bottom nil)
                                     ("single task" ?t "** next %^{Task}\n  %u\n%?" "~/Documents/gtd/gtd.org" "Single tasks" nil)
                                     ("journal" ?j "\n* %^{Title} %u \n%?" "~/Documents/gtd/journal.org" bottom nil)
                                     )))

(add-hook 'remember-mode-hook 'org-remember-apply-template)
(define-key global-map "\C-cr" 'org-remember)
(setq org-default-notes-file "~/Documents/gtd/inbox.org")

(provide 'my-remember)
