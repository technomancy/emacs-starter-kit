;;; key-cat.el --- List key bindings by category

;; Copyright (C) 2005 by Lennart Borgman

;; Author: Lennart Borgman <lennartDOTborgmanDOT073ATstudentDOTluDOTse>
;; Created: Sat Jan 28 2006
;; Version: 0.25
;; Last-Updated: 2009-05-09 Sat
;; Keywords:
;; Compatibility:
;;
;;   Requires Emacs 22.
;;
;; Features that might be required by this library:
;;
  ;; `cl'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;  Display help that looks like a reference sheet for common
;;  commands.
;;
;;  To use this in your .emacs put
;;
;;    (require 'key-cat)
;;
;;  Then use the command
;;
;;    M-x key-cat-help
;;
;;  For more information see that command.
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

(eval-when-compile (require 'cl))

(defconst key-cat-cmd-list
  '(
    (error-testing
     (commands
      :visible nil
      hallo
      key-cat-help
      key-cat-where-is
      ))
    ("Help"
     (commands
      help-for-help
      info-emacs-manual
      info
      ))
    ("Special Functions and Keys"
     ;; For similar functions that are most often bound to a specific key
     (commands
      key-cat-tab
      key-cat-complete
      )
     )
    ("Files, Buffers and Windows"
     (commands
      find-file
      save-buffer
      write-file
      split-window-vertically
      split-window-horizontally
      delete-other-windows
      other-window
      buffer-menu
      ))
    ("Search and replace"
     (commands
      isearch-forward
      isearch-backward
      query-replace
      isearch-forward-regexp
      isearch-backward-regexp
      query-replace-regexp
      occur
      lgrep
      rgrep
      ))
    ("Lines"
     (commands
      move-beginning-of-line
      move-end-of-line
      kill-line
      ))
    ("Words"
     (commands
      forward-word
      backward-word
      kill-word
      ))
    ("Region"
     (commands
      set-mark-command
      ;;cua-set-mark
      kill-region
      copy-region-as-kill
      yank
      yank-pop
      ))
    ("Undo"
     (commands
      undo
      ))
    ("Viper"
     (commands
      :visible (lambda()
                 (and (featurep 'viper)
                      viper-mode))
      viper-next-line
      viper-previous-line
      viper-forward-word
      viper-backward-word
      viper-forward-Word
      viper-backward-Word
      viper-repeat
      viper-forward-char
      viper-backward-char
      viper-next-line-at-bol
      viper-previous-line-at-bol
      viper-command-argument
      viper-digit-argument
      ))
    )
  "List with common commands to display by `key-cat-help'.
The elements of this list corresponds to sections to show in the
help.  Each element consists of sublists beginning with the
keyword 'commands.  The sublists may after 'command contain the
keyword :visible which takes a variable or function as argument.
If the argument evaluates to non-nil the list is shown."
  )


(defvar key-cat-cmd-list-1 nil)

(defun key-cat-help()
  "Display reference sheet style help for common commands.
See also `key-cat-cmd-list'."
  (interactive)
  (if (> 22 emacs-major-version)
      (message "Sorry, this requires Emacs 22 or later")
    ;; Delay to get correct bindings when running through M-x
    (setq key-cat-cmd-list-1 key-cat-cmd-list)
    (run-with-timer 0.1 nil 'key-cat-help-internal)))

(defun key-cat-help-internal()                   ;(category)
  (message "Please wait ...")
  (condition-case err
      (save-match-data ;; runs in timer
        (let ((result))
          (help-setup-xref (list #'key-cat-help)
                           (interactive-p))
          ;;         (push (list "Changing commands"
          ;;                     (list
          ;;                      'command
          ;;                      indent-line-function
          ;;                      ))
          ;;               key-cat-cmd-list-1)
          (dolist (catentry key-cat-cmd-list-1)
            (let ((category (car catentry))
                  (commands (cdr catentry))
                  (cmds)
                  (keyw)
                  (visible)
                  (visible-fun)
                  (cmdstr)
                  (doc))
              (dolist (cmdlist commands)
                (setq cmdlist (cdr cmdlist))
                (setq visible t)
                (while (keywordp (setq keyw (car cmdlist)))
                  (setq cmdlist (cdr cmdlist))
                  (case keyw
                    (:visible (setq visible-fun (pop cmdlist))
                              (setq visible (if (symbolp visible-fun)
                                                (progn
                                                  (symbol-value visible-fun))
                                              (funcall visible-fun)))
                              )
                    ))
                (when visible
                  (dolist (cmd cmdlist)
                    (setq cmds (cons cmd cmds)))))
              (when cmds
                (push (format "\n%s:\n"
                              (let ((s (format "%s" category)))
                                (put-text-property 0 (length s)
                                                   'face (list
                                                          'bold
                                                          )
                                                   s)
                                s))
                      result))
              (setq cmds (reverse cmds))
              (dolist (cmd cmds)
                (setq cmdstr
                      (let ((s "Where to find it:" ))
                        (put-text-property 0 (length s)
                                           'face '(:slant italic
                                                          :background "RGB:dd/dd/ff"
                                                          ) s) s))
                (if (not (functionp cmd))
                    (cond
                     ((eq 'key-cat-tab cmd)
                      (let ((s "Indent line"))
                        (put-text-property 0 (length s) 'face '(:foreground "blue") s)
                        (push s result))
                      (push ":\n" result)
                      (push (concat
                             "    "
                             "Indent current line (done by specific major mode function).\n")
                            result)
                      (push (format "    %17s  %s\n" cmdstr (key-description [tab])) result)
                      )
                     ((eq 'key-cat-complete cmd)
                      (let ((s "Completion"))
                        (put-text-property 0 (length s) 'face '(:foreground "blue") s)
                        (push s result))
                      (push ":\n" result)
                      (push (concat
                             "    "
                             "Performe completion at point (done by specific major mode function).\n")
                            result)
                      (push (format "    %17s  %s\n" cmdstr (key-description [meta tab])) result)
                      )
                     (t
                      (let ((s (format "`%s':  (not a function)\n" cmd)))
                        (put-text-property 0 (length s) 'face '(:foreground "red") s)
                        (push s result))))
                  (let ((keys (key-cat-where-is cmd)))
                    (push (format "`%s':\n" cmd) result)
                    (setq doc (documentation cmd t))
                    (push
                     (concat
                      "    "
                      (if doc
                          (substring doc 0 (string-match "\n" doc))
                        "(not documented)")
                      "\n")
                     result)
                    (if (not keys)
                        (if (interactive-form cmd)
                            (push (format "    %17s  M-x %s\n" cmdstr cmd) result)
                          (let ((s "(not an interactive command)"))
                            (put-text-property 0 (length s) 'face '(:foreground "red") s)
                            (push (format "    %17s  %s\n" cmdstr s) result)))
                      (dolist (key keys)
                        (push (format "    %17s  " cmdstr) result)
                        (push (format "%s\n"
                                      (if (eq (elt key 0) 'xmenu-bar)
                                          "Menus"
                                        (key-description key)))
                              result)
                        (setq cmdstr ""))))))))
          (save-excursion
            (with-current-buffer (help-buffer)
              (with-output-to-temp-buffer (help-buffer)
                (insert
                 (let ((s "Some important commands\n"))
                   (put-text-property 0 (length s)
                                      'face '(:weight bold
                                                      :height 1.5
                                                      :foreground "RGB:00/00/66") s)
                   s))
                (setq result (reverse result))
                (dolist (r result)
                  (insert r))
                )))
          (message "")))
    (error (message "%s" (error-message-string err)))))

;; Mostly copied from `where-is':
(defun key-cat-where-is (definition)
  "Return key sequences that invoke the command DEFINITION.
Argument is a command definition, usually a symbol with a function definition."
  (let ((func (indirect-function definition))
        (defs nil)
        (all-keys))
    ;; In DEFS, find all symbols that are aliases for DEFINITION.
    (mapatoms (lambda (symbol)
		(and (fboundp symbol)
		     (not (eq symbol definition))
		     (eq func (condition-case ()
				  (indirect-function symbol)
				(error symbol)))
		     (push symbol defs))))
    ;; Look at all the symbols--first DEFINITION,
    ;; then its aliases.
    (dolist (symbol (cons definition defs))
      (let* ((remapped (command-remapping symbol))
	     (keys (where-is-internal
		    ;;symbol overriding-local-map nil nil remapped)))
		    symbol nil nil nil remapped)))
        (when keys
          (dolist (key keys)
            (setq all-keys (cons key all-keys))))))
    all-keys))



(provide 'key-cat)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; key-cat.el ends here
