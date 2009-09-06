;;; nxhtml-bug.el --- Reporting nXhtml bugs
;;
;; Author: Lennar Borgman
;; Maintainer:
;; Created: Wed Mar 07 15:57:15 2007
;; Version:
;; Lxast-Updated: Wed Mar 07 16:00:22 2007 (3600 +0100)
;; Keywords:
;; Compatibility:
;;
;; Fxeatures that might be required by this library:
;;
;;   None
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change log:
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(eval-when-compile (require 'nxhtml-menu))

(require 'sendmail)

;;(require 'emacsbug)
(autoload 'report-emacs-bug-info "emacsbug" "Go to the Info node on reporting Emacs bugs." t)

(defvar nxhtml-report-bug-orig-text nil
  "The automatically-created initial text of bug report.")

(defvar nxhtml-report-bug-no-confirmation nil
  "*If non-nil, suppress the confirmations asked for the sake of novice users.")

(defvar nxhtml-report-bug-no-explanations nil
  "*If non-nil, suppress the explanations given when reporting bugs.")

;; (defvar nxhtml-bug-launchpad-mode-map
;;   (let ((map (make-sparse-keymap)))
;;     (define-key map [(control ?c) (control ?c)] 'nxhtml-bug-maybe-to-launchpad)
;;     map))

;; (define-minor-mode nxhtml-bug-launchpad-mode
;;   "Changes C-c C-c to ask to report on Launchpad."
;;   nil
;;   :keymap 'nxhtml-bug-launchpad-mode-map
;;   nil)

;; (defun nxhtml-bug-maybe-to-launchpad ()
;;   (interactive)
;;   (if (y-or-n-p "Do you want to report bug on Launchad (preferred): ")
;;       (browse-url "https://bugs.launchpad.net/nxhtml")
;;     (mail-send-and-exit)))

;;;###autoload
(defun nxhtml-report-bug ()
  "Report a bug in nXhtml."
  (interactive)
  (with-output-to-temp-buffer (help-buffer)
    (help-setup-xref (list #'nxhtml-report-bug) (interactive-p))
    (with-current-buffer (help-buffer)
      (let ((here (point)))
        (insert
         "A lot of things can interfere with nXhtml/MuMaMo to cause bugs.\n"
         "Therefore when reporting a bug please try to describe\n"
         "how to show it without your own Emacs initializations.\n"
         "To do that start from")
        (fill-region here (point))
        (insert "\n\n  M-x `emacs-Q-nxhtml'\n\n")
        (setq here (point))
        (insert
         "If you want to find out if your initialization files interfere\n"
         "then you can try the both test commands `nxhtmltest-run' and\n"
         "`nxhtmltest-run-Q' and see if there is any difference.\n\n")
        (fill-region here (point))
        (setq here (point))
        (insert
         "You may also want to look at the installation part of the ")
        (insert-text-button "The Quick Guide"
                            'action (lambda (btn)
                                      (browse-url
                                       (concat
                                        (nxhtml-docfile-url)
                                        "#qg"))))
        (insert " or the file ")
        (insert-text-button
         "nxhtml/readme.txt"
         'action (lambda (btn)
                   (find-file-other-window
                    (expand-file-name "../readme.txt"
                                      (file-name-directory
                                       (symbol-file 'nxhtml-report-bug))))))
        (insert ".\n\n")
        (fill-region here (point))
        (setq here (point))
        (insert
         "If you know Emacs lisp and are reporting a bug it would be nice "
         "if you wrote a new unit test case.\n"
         "Please see the file ")
        (insert-text-button "nxhtmltest-suites.el"
                            'help-echo "Visit file"
                            'action (lambda (button)
                                      (find-file (symbol-file 'nxhtmltest-run))))
        (insert " for examples.\n\n")
        (fill-region here (point))
        (setq here (point))
        (insert
         "There are several ways to report a bug, use the links below:\n"
         "- By visiting URL `https://bugs.launchpad.net/nxhtml'.\n")
        (setq here (point))
        (insert
         "- By ")
        (insert-text-button "email to Launchpad"
                            'help-echo "Send email to Launchpad bug system"
                            'action (lambda (button)
                                      (call-interactively
                                       'nxhtml-report-bug-by-mail)))
        (insert
         ".\n")
        (insert
         "  This requires PGP signing the email\n"
         "  and that you have told your PGP key to Launchpad.\n")
        (fill-region here (point))
        (setq here (point))
        (insert
         "- The above ways are best since the bug get into the database,\n"
         "  and it is easy to communicate about it, but if they does\n"
         "  not work for you please go to \n"
         "  URL `http://www.emacswiki.org/cgi-bin/wiki/NxhtmlMode'.\n")
        (fill-region here (point))
        (setq here (point))
        )
      (print-help-return-message))))

(defun nxhtml-report-bug-by-mail (topic)
  "Report a bug by mail.
Prompts for bug subject.  Leaves you in an Emacs mail
buffer. However when you send the bug your normal mail client
will take over the job (with your help)."
  (interactive (list (read-string "nXhtml Bug Subject: ")))
  ;; If there are four numbers in emacs-version, this is a pretest
  ;; version.
  (require 'nxhtml-menu)
  (let* ((pretest-p (string-match "\\..*\\..*\\." emacs-version))
        (from-buffer (current-buffer))
        ;;(reporting-address "lennart.borgman@gmail.com")
        ;;(reporting-address "emacs-nxml-mode@yahoogroups.com")
        (reporting-address "new@bugs.launchpad.net")
        ;; Put these properties on semantically-void text.
        (prompt-properties '(field nxhtml-bug-prompt
                                   intangible but-helpful
                                   rear-nonsticky t))
        user-point message-end-point)
    (setq message-end-point
          (with-current-buffer (get-buffer-create "*Messages*")
            (point-max-marker)))
    (compose-mail reporting-address
                  topic)
    ;; The rest of this does not execute
    ;; if the user was asked to confirm and said no.
    (rfc822-goto-eoh)
    (forward-line 1)

    (let ((signature (buffer-substring (point) (point-max))))
      (delete-region (point) (point-max))
      (insert signature)
      (backward-char (length signature)))
    ;;(nxhtml-bug-launchpad-mode 1)
    (insert
     "\nThis is a bug report for nXhtml mode.\n")
    (unless nxhtml-report-bug-no-explanations
      ;; Insert warnings for novice users.
      (when (string-match "nxml-mode" reporting-address)
        (insert "This bug report will be sent to the nXhtml maintainers,\n")
        (let ((pos (point)))
          (insert "not to your local site managers!\n")
          (put-text-property pos (point) 'face 'highlight)))
      (insert "\nPlease write in ")
      (let ((pos (point)))
        (insert "English")
        (put-text-property pos (point) 'face 'highlight))
      (insert " if possible, because the nXhtml maintainers
usually do not have translators to read other languages for them.\n\n")
      )
    (insert "Please describe exactly what actions triggered the bug\n"
            "and the precise symptoms of the bug, preferrably starting\n"
            "from `M-x emacs-Q-nxhtml'\n"
            "(it may also be helpful to include an *EXAMPLE FILE*!).\n\n")
    (add-text-properties (point) (save-excursion (mail-text) (point))
                         prompt-properties)

    (setq user-point (point))
    (insert "\n\n")

    (insert "\n\nnXhtml version " nxhtml-menu:version ", " (emacs-version) "\n\n")
    (insert (format "Major mode: %s\n"
                    (buffer-local-value 'mode-name from-buffer)))
    (insert "\n")
    (insert "Minor modes in effect:\n")
    (dolist (mode minor-mode-list)
      (and (boundp mode) (buffer-local-value mode from-buffer)
           (insert (format "  %s: %s\n" mode
                           (buffer-local-value mode from-buffer)))))
    (insert "\n")
    (let ((message-buf (get-buffer "*Messages*")))
      (if message-buf
          (let (beg-pos
                (end-pos message-end-point))
            (with-current-buffer message-buf
              (goto-char end-pos)
              (forward-line -10)
              (setq beg-pos (point)))
            (insert "\n\nRecent messages:\n")
            (insert-buffer-substring message-buf beg-pos end-pos))))
    ;; This is so the user has to type something
    ;; in order to send easily.
    (use-local-map (nconc (make-sparse-keymap) (current-local-map)))
    (define-key (current-local-map) "\C-c\C-i" 'report-emacs-bug-info)
    (unless nxhtml-report-bug-no-explanations
      (with-output-to-temp-buffer "*Bug Help*"
        (if (eq mail-user-agent 'sendmail-user-agent)
            (princ (substitute-command-keys
                    "Type \\[mail-send-and-exit] to send the bug report.\n")))
        (princ (substitute-command-keys
                "Type \\[kill-buffer] RET to cancel (don't send it).\n"))
        (terpri)
        (princ (substitute-command-keys
                "Type \\[report-emacs-bug-info] to visit in Info the Emacs Manual section
about when and how to write a bug report,
and what information to supply so that the bug can be fixed.

When there type SPC to scroll through this section and its subsections.

Please notice that you are now reporting a bug for nXhtml, not
Emacs itself, so everyting in that manual section might not
apply."))))
    ;; Make it less likely people will send empty messages.
    (make-local-variable 'mail-send-hook)
    (add-hook 'mail-send-hook 'nxhtml-report-bug-hook)
    (save-excursion
      (goto-char (point-max))
      (skip-chars-backward " \t\n")
      (make-local-variable 'nxhtml-report-bug-orig-text)
      (setq nxhtml-report-bug-orig-text (buffer-substring (point-min) (point))))
    (goto-char user-point)))


(defun nxhtml-report-bug-hook ()
  (save-excursion
    (save-excursion
      (goto-char (point-max))
      (skip-chars-backward " \t\n")
      (if (and (= (- (point) (point-min))
                  (length nxhtml-report-bug-orig-text))
               (equal (buffer-substring (point-min) (point))
                      nxhtml-report-bug-orig-text))
          (error "No text entered in bug report")))

    ;; Check the buffer contents and reject non-English letters.
    (save-excursion
      (goto-char (point-min))
      (skip-chars-forward "\0-\177")
      (if (not (eobp))
          (if (or nxhtml-report-bug-no-confirmation
                  (y-or-n-p "Convert non-ASCII letters to hexadecimal? "))
              (while (progn (skip-chars-forward "\0-\177")
                            (not (eobp)))
                (let ((ch (following-char)))
                  (delete-char 1)
                  (insert (format "=%02x" ch)))))))

    ;; The last warning for novice users.
    (if (or nxhtml-report-bug-no-confirmation
            (yes-or-no-p
             "Send this bug report to the nXhtml maintainers? "))
        ;; Just send the current mail.
        nil
      (goto-char (point-min))
      (if (search-forward "To: ")
          (let ((pos (point)))
            (end-of-line)
            (delete-region pos (point))))
      (kill-local-variable 'mail-send-hook)
      (with-output-to-temp-buffer "*Bug Help*"
        (princ (substitute-command-keys "\
You invoked the command nxhtml-report-bug,
but you decided not to mail the bug report to the nXhtml maintainer.

If you want to mail it to someone else instead,
please insert the proper e-mail address after \"To: \",
and send the mail again using \\[mail-send-and-exit].")))
      (error "M-x nxhtml-report-bug was cancelled, please read *Bug Help* buffer"))

    ;; Unclutter
    (mail-text)
    (insert
     "Next line tells Launchpad bug system what this is. Don't change it!\n"
     "  affects nxhtml\n\n")
    (mail-text)
    (let ((pos (1- (point))))
      (while (setq pos (text-property-any pos (point-max)
                                          'field 'nxhtml-bug-prompt))
        (delete-region pos (field-end (1+ pos)))))))

(provide 'nxhtml-bug)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; nxhtml-bug.el ends here
