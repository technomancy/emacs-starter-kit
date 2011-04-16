;; From the rich resource at http://doc.norang.ca/org-mode.html

(require 'org-install)
(require 'org)

;; Files
(setq org-agenda-files (quote ("~/Dropbox/Documents" "~/Dropbox/Documents/gtd")))
(add-to-list 'auto-mode-alist '("\\.\\(org\\|org_archive\\|txt\\)$" . org-mode))
(setq org-default-notes-file "~/Dropbox/Documents/gtd/inbox.org")

;; Keys
(define-key viper-vi-global-user-map "C-c /" 'org-sparse-tree)
(global-set-key "\C-cl" 'org-store-link)
(global-set-key (kbd "<f12>") 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)
(global-set-key (kbd "C-M-r") 'org-capture)

;; Todo config
(setq org-todo-keywords (quote ((sequence "inbox(i)" "next(n)" "maybe(m)" "started(s)"  "waiting(w)" "|" "done(d)" "cancelled(c)"))))
(setq org-use-fast-todo-selection t)

(setq org-capture-templates (quote (("p" "New Project" entry (file "~/Dropbox/Documents/gtd/gtd.org") "* %^{Project name}
** next %^{First task}%?" :clock-in t :clock-keep nil)
                                    ("j" "Journal" entry (file "~/Dropbox/Documents/gtd/journal.org") "* %?" :clock-in t :clock-resume t)
                                    ("l" "liam" entry (file "~/Dropbox/Documents/liam.org") "* %?" :clock-in t :clock-resume t)
                                    ("i" "inbox" entry (file "~/Dropbox/Documents/gtd/inbox.org") "* inbox %? %u %a")
                                    ("n" "note" entry (file "~/Dropbox/Documents/gtd/inbox.org") "* note %? %u %a"))))

;; Agenda
(setq org-stuck-projects
      '("+LEVEL=1+project/-done" ("next" "waiting") ()))

(setq org-tag-alist (quote ((:startgroup)
                            ("@errands" . ?e)
                            ("@calls" . ?c)
                            ("@home" . ?h)
                            (:endgroup)
                            ("crypt" . ?s)
                            )))

(setq org-agenda-custom-commands (quote (("n" "Next and Started tasks" tags-todo "-waiting-cancelled/!next|started"
               ((org-agenda-overriding-header "Next Tasks")))
                                         ("e" tags "@errands-todo=\"done\"" nil)
                                         ("c" tags "@calls-todo=\"done\"" nil)
                                         ("E" "Todo items without context (in error)" 
                                          (( tags "+project+todo=\"next\"-{@.*}")))
                                         ("p" "projects"   
                                          ((tags "+LEVEL=1+project"))))))

;; Refile
(setq org-completion-use-ido t)
(setq org-refile-targets (quote ((org-agenda-files :maxlevel . 1) (nil :maxlevel . 1))))
(setq org-refile-use-outline-path (quote file))
(setq org-outline-path-complete-in-steps t)
(setq org-refile-allow-creating-parent-nodes (quote confirm))

;; Clocking
(org-clock-persistence-insinuate)
(setq org-clock-history-length 28)
(setq org-clock-in-resume t)
(setq org-clock-in-switch-to-state (quote life/clock-in-to-started))
(setq org-drawers (quote ("PROPERTIES" "LOGBOOK")))
(setq org-clock-into-drawer t)
(setq org-clock-out-remove-zero-time-clocks t)
(setq org-clock-out-when-done t)
(setq org-clock-persist (quote history))
(setq org-clock-auto-clock-resolution (quote when-no-clock-is-running))
(setq org-clock-report-include-clocking-task t)

(defun life/clock-in-to-started (kw)
  (if (member (org-get-todo-state) (list "inbox" "next"))
      "started"))
  
;; Crypt
(require 'org-crypt)
(org-crypt-use-before-save-magic)
(setq org-tags-exclude-from-inheritance (quote ("crypt")))
(setq org-crypt-key "978D4E9F")

(provide 'my-org-mode)
