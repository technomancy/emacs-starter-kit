;; From the rich resource at http://doc.norang.ca/org-mode.html

(add-to-list 'load-path (concat vendor-dir "/org-7.5/lisp"))
(add-to-list 'load-path (concat vendor-dir "/org-7.5/contrib/lisp"))

(require 'org-install)
(require 'org)

;; Files
(setq org-agenda-files (quote ("~/Dropbox/Documents" "~/Dropbox/Documents/gtd")))
(add-to-list 'auto-mode-alist '("\\.\\(org\\|org_archive\\|txt\\)$" . org-mode))
(setq org-default-notes-file "~/Dropbox/Documents/gtd/notes.org")

(define-key viper-vi-global-user-map "C-c /" 'org-sparse-tree)
(global-set-key "\C-cl" 'org-store-link)
(global-set-key (kbd "<f12>") 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)
(global-set-key (kbd "C-M-r") 'org-capture)

(setq org-blank-before-new-entry nil)
(setq org-enforce-todo-dependencies t)
(setq org-fast-tag-selection-include-todo t)
(setq org-fast-tag-selection-single-key t)
(setq org-use-fast-todo-selection t)
(setq org-agenda-skip-additional-timestamps-same-entry t)

(setq org-hide-leading-stars t)
;; Todo config
(setq org-todo-keywords (quote ((sequence "next(n)" "maybe(m)" "started(s)"  "waiting(w)" "|" "done(d)" "cancelled(z) | note(t)"))))
(setq org-use-fast-todo-selection t)

(setq org-capture-templates (quote (("p" "New Project" entry (file "~/Dropbox/Documents/gtd/gtd.org") "* %^{Project name}
** next %^{First task}%?" :clock-in t :clock-keep nil)
                                    ("j" "Journal" entry (file "~/Dropbox/Documents/gtd/journal.org") "* %?" :clock-in t :clock-resume t)
                                    ("l" "liam" entry (file "~/Dropbox/Documents/liam.org") "* %?" :clock-in t :clock-resume t)
                                    ("i" "inbox" entry (file "~/Dropbox/Documents/gtd/inbox.org") "* next %?
	%u %a")
                                    ("n" "note" entry (file "~/Dropbox/Documents/gtd/inbox.org") "* note %?
	%u %a"))))

;; Agenda
(setq org-stuck-projects
      '("+LEVEL=1+project/-done" ("next" "started") ()))

(setq org-tag-alist (quote ((:startgroup)
                            ("@errands" . ?e)
                            ("@calls" . ?c)
                            ("@home" . ?h)
                            (:endgroup)
                            ("crypt" . ?s)
                            )))

(setq org-agenda-custom-commands (quote
                                  (("n" "Next and Started tasks" tags-todo "+project-waiting-cancelled/!next|started"
                                    ((org-agenda-overriding-header "Next Tasks")
                                     (org-agenda-tags-todo-honor-ignore-options t)
                                     (org-agenda-todo-ignore-scheduled 'future)))
                                   ("e" tags-todo "@errands-TODO=\"done\"" nil)
                                   ("c" tags "@calls-TODO=\"done\"" nil)
                                   ("w" tags "@waiting-TODO=\"done\"" nil)
                                   ("r" tags "refile" nil)
                                   ("s" tags "+LEVEL=1+maybe" nil)
                                   ("E" "Todo items without context (in error)" 
                                    (( tags "+project+TODO=\"next\"-{@.*}")))
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

;; Google calendars
(setq mark-diary-entries-in-calendar t)
(defun getcal (url)
  "Download ics file and add to diary"
  (let ((tmpfile (url-file-local-copy url)))
    (icalendar-import-file tmpfile "~/diary" t)
    (kill-buffer (car (last (split-string tmpfile "/"))))
    )
  )
(setq google-calendars
      '(
        "http://www.google.com/calendar/ical/danie%40danieroux.com/private-5a6e5d28a8201dd4e05086d629435d1f/basic.ics"
))

(defun getcals ()
  (interactive)
  (find-file "~/diary")
  (flush-lines "^[& ]")
  (dolist (url google-calendars) (getcal url))
  (kill-buffer "diary"))

(global-set-key (kbd "<f9>c") 'getcals)
(setq org-agenda-include-diary t)

(provide 'my-org-mode)
