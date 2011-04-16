; (autoload 'wl "wl" "Wanderlust" t)
(autoload 'wl-other-frame "wl" "Wanderlust on new frame." t)
; (autoload 'wl-draft "wl-draft" "Write draft with Wanderlust." t)

(require 'wl)
(require 'wl-draft)

;; IMAP
(setq elmo-imap4-default-server "imap.gmail.com")
(setq elmo-imap4-default-user "danie@danieroux.com")
(setq elmo-imap4-default-authenticate-type 'clear)
(setq elmo-imap4-default-port '993)
(setq elmo-imap4-default-stream-type 'ssl)

(setq elmo-imap4-use-modified-utf7 t)

;; SMTP
(setq wl-from "Danie Roux <danie@danieroux.com>")
(setq wl-smtp-connection-type 'starttls)
(setq wl-smtp-posting-port 587)
(setq wl-smtp-authenticate-type "plain")
(setq wl-smtp-posting-user "danie@danieroux.com")
(setq wl-smtp-posting-server "smtp.gmail.com")
(setq wl-local-domain "danieroux.com")
(setq wl-message-id-domain "danieroux.com")

;; (add-to-list 'wl-summary-sort-specs 'rdate)

(setq wl-default-folder "%inbox")
; (setq wl-default-spec "%")
(setq wl-draft-folder ".drafts")
(setq wl-trash-folder ".trash")
(setq wl-forward-subject-prefix "Fwd: " ) 
(setq wl-folder-check-async t)
(setq elmo-imap4-use-modified-utf7 t)

(autoload 'wl-user-agent-compose "wl-draft" nil t)

(if (boundp 'mail-user-agent)
    (setq mail-user-agent 'wl-user-agent))
(if (fboundp 'define-mail-user-agent)
    (define-mail-user-agent
      'wl-user-agent
      'wl-user-agent-compose
      'wl-draft-send
      'wl-draft-kill
            'mail-send-hook))

(setq 
wl-message-ignored-field-list '("^.*:")
wl-message-visible-field-list
'("^\\(To\\|Cc\\):"
  "^Subject:"
  "^\\(From\\|Reply-To\\):"
  "^Organization:"
  "^Message-Id:"
  "^\\(Posted\\|Date\\):")

wl-message-sort-field-list
'("^From"
  "^Organization:"
  "^X-Attribution:"
  "^Subject"
  "^Date"
  "^To"
  "^Cc"))

(require 'bbdb-wl)
(bbdb-wl-setup)
;; (setq bbdb-wl-folder-regexp "^\.inbox$\\|^.sent")
(define-key wl-draft-mode-map (kbd "<C-tab>") 'bbdb-complete-name)

(defun djcb-wl-summary-refile (&optional folder)
  (interactive)
  (wl-summary-refile (wl-summary-message-number) folder)
  (wl-summary-next))

(define-key wl-summary-mode-map (kbd "y")
  '(lambda()(interactive)(djcb-wl-summary-refile "%[Gmail]/All Mail"))) 

(provide 'my-wl)
