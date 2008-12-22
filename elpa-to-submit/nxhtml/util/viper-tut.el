;;; viper-tut.el --- Viper tutorial
;;
;; Author: Lennart Borgman
;; Created: Fri Sep 08 16:55:42 2006
;; Version: 0.1
;; Last-Updated:
;; Keywords:
;; Compatibility: Emacs 22
;;
;; Features that might be required by this library:
;;
;;   `button', `cus-edit', `cus-face', `cus-load', `cus-start',
;;   `help-mode', `tutorial', `view', `wid-edit'.
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

(require 'tutorial)
(require 'cus-edit)

(defvar tutorial--tab-map
  (let ((map (make-sparse-keymap)))
    (define-key map [tab] 'forward-button)
    (define-key map [(shift tab)] 'backward-button)
    (define-key map [(meta tab)] 'backward-button)
    map)
  "Keymap that allows tabbing between buttons.")

(defun viper-tut--detailed-help (button)
  "Give detailed help about changed keys."
  (with-output-to-temp-buffer (help-buffer)
    (help-setup-xref (list #'viper-tut--detailed-help button)
                     (interactive-p))
    (with-current-buffer (help-buffer)
      (let* ((tutorial-buffer  (button-get button 'tutorial-buffer))
             ;;(tutorial-arg     (button-get button 'tutorial-arg))
             (explain-key-desc (button-get button 'explain-key-desc))
             (part             (button-get button 'part))
             (changed-keys (with-current-buffer tutorial-buffer
                             (let ((tutorial--lang "English"))
                               (tutorial--find-changed-keys
                                (if (= part viper-tut--emacs-part)
                                    tutorial--default-keys
                                  viper-tut--default-keys))))))
        (when changed-keys
          (insert
           "The following key bindings used in the tutorial had been changed\n"
           (if (= part viper-tut--emacs-part)
               "from Emacs default in the "
             "from Viper default in the ")
           (buffer-name tutorial-buffer) " buffer:\n\n" )
          (let ((frm "   %-9s %-27s %-11s %s\n"))
            (insert (format frm "Key" "Standard Binding" "Is Now On" "Remark")))
          (dolist (tk changed-keys)
            (let* ((def-fun     (nth 1 tk))
                   (key         (nth 0 tk))
                   (def-fun-txt (nth 2 tk))
                   (where       (nth 3 tk))
                   (remark      (nth 4 tk))
                   (rem-fun (command-remapping def-fun))
                   (key-txt (key-description key))
                   (key-fun (with-current-buffer tutorial-buffer (key-binding key)))
                   tot-len)
              (unless (eq def-fun key-fun)
                ;; Insert key binding description:
                (when (string= key-txt explain-key-desc)
                  (put-text-property 0 (length key-txt) 'face '(:background "yellow") key-txt))
                (insert "   " key-txt " ")
                (setq tot-len (length key-txt))
                (when (> 9 tot-len)
                  (insert (make-string (- 9 tot-len) ? ))
                  (setq tot-len 9))
                ;; Insert a link describing the old binding:
                (insert-button def-fun-txt
                               'value def-fun
                               'action
                               (lambda(button) (interactive)
                                 (describe-function
                                  (button-get button 'value)))
                               'follow-link t)
                (setq tot-len (+ tot-len (length def-fun-txt)))
                (when (> 36 tot-len)
                  (insert (make-string (- 36 tot-len) ? )))
                (when (listp where)
                  (setq where "list"))
                ;; Tell where the old binding is now:
                (insert (format " %-11s " where))
                ;; Insert a link with more information, for example
                ;; current binding and keymap or information about
                ;; cua-mode replacements:
                (insert-button (car remark)
                               'action
                               (lambda(b) (interactive)
                                 (let ((value (button-get b 'value)))
                                   (tutorial--describe-nonstandard-key value)))
                               'value (cdr remark)
                               'follow-link t)
                (insert "\n")))))



        (insert "
It is legitimate to change key bindings, but changed bindings do not
correspond to what the tutorial says.  (See also " )
        (insert-button "Key Binding Conventions"
                       'action
                       (lambda(button) (interactive)
                         (info
                          "(elisp) Key Binding Conventions")
                         (message "Type C-x 0 to close the new window"))
                       'follow-link t)
        (insert ".)\n\n")
        (print-help-return-message)))))


(defconst viper-tut--default-keys
  `(
;;;;;;;;;;;;;; Part 1
    ;; ^D	Move DOWN one half-screen
    ;;(viper-scroll-up [(control ?d)])
    (viper-scroll-up [?\C-d])

    ;; ^U	Move UP one half-screen
    ;;(viper-scroll-down [(control ?u)])
    (viper-scroll-down [?\C-u])

    ;; h	Move left one character
    (viper-backward-char [?h])

    ;; j	Move down one line
    (viper-next-line [?j])

    ;; k	Move up one line
    (viper-previous-line [?k])

    ;; l	Move right one character
    (viper-forward-char [?l])

    ;; dd	DELETE one line
    (viper-command-argument [?d])

    ;; x	X-OUT one character
    (viper-delete-char [?x])

    ;; u	UNDO last change
    (viper-undo [?u])

    ;; :q!<RETURN>	QUIT without saving changes
    (viper-ex [?:])

    ;; ZZ	Exit and save any changes
    (viper-save-kill-buffer [?Z ?Z])

    ;; o	OPEN a line for inserting text
    (viper-open-line [?o])

    ;; i	INSERT starting at the cursor
    (viper-insert [?i])

    ;; ESC	ESCAPE from insert mode
    ;;(viper-intercept-ESC-key [(escape)])
                                        ;(viper-intercept-ESC-key [27])
    (viper-intercept-ESC-key [escape])
    ;; chagned-keys=
    ;;       (([27]
    ;;         viper-intercept-ESC-key
    ;;         viper-intercept-ESC-key
    ;;         <escape>
    ;;         (more info current-binding (keymap (118 . cua-repeat-replace-region)) viper-intercept-ESC-key [27] <escape>)))


;;;;;;;;;;;;;; Part 2
    ;; w       Move to the beginning of the next WORD
    (viper-forward-word [?w])
    ;; e       Move to the END of the next word
    (viper-end-of-word [?e])
    ;; b       Move BACK to the beginning to the previous word
    (viper-backward-word [?b])

    ;; $       Move to the end of the line
    (viper-goto-eol [?$])

    ;; ^       Move to the first non-white character on the line
    (viper-bol-and-skip-white [?^])

    ;; 0       Move to the first column on the line (column zero)
    (viper-beginning-of-line [?0])
    ;; #|      Move to an exact column on the line (column #) e.g.  5| 12|
    (viper-goto-col [?|])

    ;; f char	FIND the next occurrence of char on the line
    (viper-find-char-forward [?f])
    ;; t char	Move 'TIL the next occurrence of char on the line
    (viper-goto-char-forward [?t])

    ;; F char	FIND the previous occurrence of char on the line
    (viper-find-char-backward [?F])
    ;; T char	Move 'TIL the previous occurrence of char on the line
    (viper-goto-char-backward [?T])

    ;; ;	Repeat the last  f, t, F, or T
    (viper-repeat-find [?\;])
    ;; ,	Reverse the last  f, t, F, or T
    (viper-repeat-find-opposite [?,])

    ;; %       Show matching () or {} or []
    (viper-exec-mapped-kbd-macro [?%])

    ;; H	Move to the HIGHEST position in the window
    (viper-window-top [?H])
    ;; M	Move to the MIDDLE position in the window
    (viper-window-middle [?M])
    ;; L	Move to the LOWEST position in the window
    (viper-window-bottom [?L])

    ;; m char	MARK this location and name it char
    (viper-mark-point [?m])
    ;; ' char	(quote character) return to line named char
    ;; ''		(quote quote) return from last movement
    (viper-goto-mark-and-skip-white [?'])

    ;; G      GO to the last line in the file
    ;; #G      GO to line #.  (e.g., 3G , 5G , 175G )
    (viper-goto-line [?G])

    ;; {	(left brace) Move to the beginning of a paragraph
    ;; }	(right brace) Move to the end of a paragraph
    (viper-backward-paragraph [?{])
    (viper-forward-paragraph [?}])

    ;; (	(left paren) Move to the beginning of a sentence
    ;; )	(right paren) Move to the beginning of the next sentence
    (viper-backward-sentence [?\(])
    (viper-forward-sentence [?\)])

    ;; [[      Move to the beginning of a section
    ;; ]]      Move to the end of a section
    (viper-brac-function [?\[])
    (viper-ket-function [?\]])

    ;; /string	 Find string looking forward
    (viper-exec-mapped-kbd-macro [?/])
    ;; ?string	 Find string looking backward
    (viper-search-backward [??])

    ;; n	 Repeat last / or ? command
    ;; N	 Reverse last / or ? command
    (viper-search-next [?n])
    (viper-search-Next [?N])


;;;;;;;;;;;;;; Part 3

    ;; #movement	repeat movement # times
    (viper-digit-argument [?1])
    (viper-digit-argument [?2])
    (viper-digit-argument [?3])
    (viper-digit-argument [?4])
    (viper-digit-argument [?5])
    (viper-digit-argument [?6])
    (viper-digit-argument [?7])
    (viper-digit-argument [?8])
    (viper-digit-argument [?9])

    ;; dmovement	DELETE to where "movement" command specifies
    ;; d#movement	DELETE to where the  #movement  command specifies
    ;; d runs the command viper-command-argument

    ;; ymovement	YANK to where "movement" command specifies
    ;; y#movement	YANK to where the  #movement  command specifies
    (viper-command-argument [?y])

    ;; P	(upper p) PUT the contents of the buffer before the cursor
    ;; p	(lower p) PUT the contents of the buffer after the cursor
    (viper-put-back [?p])
    (viper-Put-back [?P])

    ;; "#P	(upper p) PUT contents of buffer # before the cursor
    ;; "#p	(lower p) PUT contents of buffer # after the cursor
    ;;
    ;; "aDELETE	DELETE text into buffer a
    ;; "aYANK	YANK text into buffer a
    ;; "aPUT	PUT text from named buffer a
    (viper-command-argument [?\"])

    ;; :w<RETURN>	WRITE contents of the file (without quitting)

    ;; :e filename<RETURN>    Begin EDITing the file called "filename"



;;;;;;;;;;;;;; Part 4


    ;; o	OPEN a line below the cursor
    ;; O	OPEN a line above the cursor
    (viper-open-line [?o])
    (viper-Open-line [?O])

    ;; i	INSERT starting before the cursor
    ;; I	INSERT at the beginning of the line
    (viper-insert [?i])
    (viper-Insert [?I])

    ;; a	APPEND starting after the cursor
    ;; A	APPEND at the end of the line
    (viper-append [?a])
    (viper-Append [?A])

    ;; ESC	ESCAPE from insert mode
    (viper-intercept-ESC-key [(escape)])

    ;; J	JOIN two lines
    (viper-join-lines [?J])

    ;; #s	SUBSTITUTE for # characters
    ;; #S	SUBSTITUTE for # whole lines
    (viper-substitute [?s])
    (viper-substitute-line [?S])

    ;; r	REPLACE character (NO need to press ESC)
    ;; R	enter over-type mode
    (viper-replace-char [?r])
    (viper-overwrite [?R])

    ;; cmovement	CHANGE to where the movement commands specifies
    (viper-command-argument [?c])


;;;;;;;;;;;;;; Part 5

    ;; ~	(tilde) Convert case of current character
    (viper-toggle-case [?~])
    ;; U	(upper u) UNDO all changes made to the current line
    ;; not implemented
    ;;(viper-undo [?U])

    ;; .	(dot) repeat last change
    (viper-repeat [?.])

    ;; ^F	Move FORWARD one full-screen
    ;; ^B	Move BACKWARD one full-screen
    ;;(viper-scroll-screen [(control ?f)])
    (viper-scroll-screen [?\C-f])
    ;;(viper-scroll-screen-back [(control ?b)])
    (viper-scroll-screen-back [?\C-b])

    ;; ^E	Move the window down one line without moving cursor
    ;; ^Y	Move the window up one line without moving cursor
    ;;(viper-scroll-up-one [(control ?e)])
    (viper-scroll-up-one [?\C-e])
    ;;(viper-scroll-down-one [(control ?y)])
    (viper-scroll-down-one [?\C-y])

    ;; z<RETURN>	Position the current line to top of window
    ;; z.	Position the current line to middle of window
    ;; z-	Position the current line to bottom of window
    (viper-line-to-top "z\C-m")
    (viper-line-to-middle [?z ?.])
    (viper-line-to-bottom [?z ?-])

    ;; ^G	Show status of current file
    ;;(viper-info-on-file [(control ?c)(control ?g)])
    (viper-info-on-file [?\C-c ?\C-g])
    ;; ^L	Refresh screen
    ;;(recenter [(control ?l)])
    (recenter [?\C-l])

    ;; !}fmt	Format the paragraph, joining and filling lines to
    ;; !}sort	Sort lines of a paragraph alphabetically
    (viper-command-argument [?!])

    ;; >movement	Shift right to where the movement command specifies
    ;; <movement	Shift left to where the movement command specifies
    (viper-command-argument [?>])
    (viper-command-argument [?<])

    ))

(defvar viper-tut--part nil
  "Viper tutorial part.")
(make-variable-buffer-local 'viper-tut--part)

(defun viper-tut--saved-file ()
  "File name in which to save tutorials."
  (let* ((file-name
          (file-name-nondirectory (viper-tut--file viper-tut--part)))
         (ext (file-name-extension file-name)))
    (when (or (not ext)
              (string= ext ""))
      (setq file-name (concat file-name ".tut")))
    (expand-file-name file-name (tutorial--saved-dir))))

(defun viper-tut--save-tutorial ()
  "Save the tutorial buffer.
This saves the part of the tutorial before and after the area
showing changed keys.  It also saves point position and the
position where the display of changed bindings was inserted.

Do not save anything if not `viper-mode' is enabled in the
tutorial buffer."
  ;; This runs in a hook so protect it:
  (condition-case err
      (when (boundp 'viper-mode-string)
        (tutorial--save-tutorial-to (viper-tut--saved-file)))
    (error (warn "Error saving tutorial state: %s" (error-message-string err)))))


(defvar viper-tut--parts
  '(
    (0 "0intro" "Introduction")
    (1 "1basics" "Basic Editing")
    (2 "2moving" "Moving Efficiently")
    (3 "3cutpaste" "Cutting and Pasting")
    (4 "4inserting" "Inserting Techniques")
    (5 "5tricks" "Tricks and Timesavers")
    (6 "(no file)" "Emacs tutorial for Viper Users")
    ))

(defconst viper-tut--emacs-part 6)

(defcustom viper-tut-directory
  (let* ((this-file (if load-file-name
                        load-file-name
                      (buffer-file-name)))
         (this-dir (file-name-directory this-file)))
    (file-name-as-directory
     (expand-file-name "viper-tut" this-dir)))
  "Directory where the Viper tutorial files lives."
  :type 'directory
  :group 'viper)

(defun viper-tut--file(part)
  "Get file name for part."
  (let ((tut-file))
    (mapc (lambda(rec)
            (when (= part (nth 0 rec))
              (setq tut-file
                    (if (= part viper-tut--emacs-part)
                        (expand-file-name (get-language-info "English" 'tutorial) data-directory)
                      (expand-file-name (nth 1 rec) viper-tut-directory)))))
          viper-tut--parts)
    tut-file))

(defun viper-tut--display-changes (changed-keys part)
  "Display changes to some default Viper key bindings.
If some of the default key bindings that the Viper tutorial
depends on have been changed then display the changes in the
tutorial buffer with some explanatory links.

CHANGED-KEYS should be a list in the format returned by
`tutorial--find-changed-keys'."
  (when (or changed-keys
            (not viper-is-on))
    ;; Need the custom button face for viper buttons:
    ;;(when (and (boundp 'viper-mode) viper-mode) (require 'cus-edit))
    (goto-char tutorial--point-before-chkeys)
    (let* ((start (point))
           end
           (viper-is-on (boundp 'viper-mode-string))
           (head
            (if viper-is-on
                (if (= part viper-tut--emacs-part)
                    "
 NOTICE: This part of the Viper tutorial runs the Emacs tutorial.
 Several keybindings are changed from Emacs default (either
 because of Viper or some other customization) and doesn't
 correspond to the tutorial.

 We have inserted colored notices where the altered commands have
 been introduced.  If you change Viper state (vi state, insert
 state, etc) these notices will be changed to reflect the new
 state. ["
                  "
 NOTICE: The main purpose of the Viper tutorial is to teach you
 the most important vi commands (key bindings).  However, your
 Emacs has been customized by changing some of these basic Viper
 editing commands, so it doesn't correspond to the tutorial.  We
 have inserted colored notices where the altered commands have
 been introduced. [")
              "
  NOTICE: You have currently not turned on Viper. Nothing in this
  tutorial \(the Viper Tutorial\) will work unless you do that. ["
              ))
           (head2 (if viper-is-on
                      (get-lang-string tutorial--lang 'tut-chgdhead2)
                    "More information")))
      (when (and head head2)
        (insert head)
        (insert-button head2
                       'tutorial-buffer
                       (current-buffer)
                       ;;'tutorial-arg arg
                       'part part
                       'action
                       (if viper-is-on
                           'viper-tut--detailed-help
                         'go-home-blaha)
                       'follow-link t
                       'face '(:inherit link :background "yellow"))
        (insert "]\n\n" )
        (when changed-keys
          (dolist (tk changed-keys)
            (let* ((def-fun     (nth 1 tk))
                   (key         (nth 0 tk))
                   (def-fun-txt (nth 2 tk))
                   (where       (nth 3 tk))
                   (remark      (nth 4 tk))
                   (rem-fun (command-remapping def-fun))
                   (key-txt (key-description key))
                   (key-fun (key-binding key))
                   tot-len)
              (unless (eq def-fun key-fun)
                ;; Mark the key in the tutorial text
                (unless (string= "Same key" where)
                  (let* ((here (point))
                         (key-desc (key-description key))
                         (vi-char (= 1 (length key-desc)))
                         vi-char-pos
                         hit)
                    (when (string= "RET" key-desc)
                      (setq key-desc "Return"))
                    (when (string= "DEL" key-desc)
                      (setq key-desc "Delback"))
                    (while (if (not vi-char)
                               (progn
                                 (setq hit t)
                                 (re-search-forward
                                  (concat "[^[:alpha:]]\\("
                                          (regexp-quote key-desc)
                                          "\\)[^[:alpha:]]") nil t))
                             (setq vi-char-pos
                                   (next-single-property-change
                                    (point) 'vi-char)))
                      (if (not vi-char)
                          (put-text-property (match-beginning 0)
                                             (match-end 0)
                                             'tutorial-remark 'only-colored)
                        (put-text-property (match-beginning 0)
                                           (match-end 0)
                                           'face '(:background "yellow"))
                        (goto-char (1+ vi-char-pos))
                        (setq hit (string= key-desc (char-to-string (char-before))))
                        (when hit
                          (put-text-property vi-char-pos (1+ vi-char-pos)
                                             'face '(:background "yellow"))))
                      (when hit
                        (forward-line)
                        (let ((s  (get-lang-string tutorial--lang 'tut-chgdkey))
                              (s2 (get-lang-string tutorial--lang 'tut-chgdkey2))
                              (start (point))
                              end)
                          ;;(concat "** The key " key-desc " has been rebound, but you can use " where " instead ["))
                          (when (and s s2)
                            (setq s (format s key-desc where s2))
                            (insert s)
                            (insert-button s2
                                           'tutorial-buffer
                                           (current-buffer)
                                           ;;'tutorial-arg arg
                                           'part part
                                           'action
                                           'viper-tut--detailed-help
                                           'explain-key-desc key-desc
                                           'follow-link t
                                           'face '(:inherit link :background "yellow"))
                            (insert "] **")
                            (insert "\n")
                            (setq end (point))
                            (put-text-property start end 'local-map tutorial--tab-map)
                            (put-text-property start end 'tutorial-remark t)
                            (put-text-property start end
                                               'face '(:background "yellow" :foreground "#c00"))
                            (put-text-property start end 'read-only t)))))
                    (goto-char here)))))))


        (setq end (point))
        ;; Make the area with information about change key
        ;; bindings stand out:
        (put-text-property start end
                           'face
                           ;; The default warning face does not
                           ;;look good in this situation. Instead
                           ;;try something that could be
                           ;;recognized from warnings in normal
                           ;;life:
                           ;; 'font-lock-warning-face
                           (list :background "yellow" :foreground "#c00"))
        ;; Make it possible to use Tab/S-Tab between fields in
        ;; this area:
        (put-text-property start end 'local-map tutorial--tab-map)
        (put-text-property start end 'tutorial-remark t)
        (setq tutorial--point-after-chkeys (point-marker))
        ;; Make this area read-only:
        (put-text-property start end 'read-only t)))))

(defun viper-tut--at-change-state()
  (condition-case err
      (progn
        (save-excursion
          (let ((inhibit-read-only t))
            ;; Delete the remarks:
            (tutorial--remove-remarks)
            ;; Add them again
            (viper-tut--add-remarks)
            )
          )
        )
    (error (message "error in viper-tut--at-change-state: %s" (error-message-string err)))))


;;;###autoload
(defun viper-tutorial(part &optional dont-ask-for-revert)
  "Run a tutorial for Viper.
If any of the standard Viper key bindings that are used in the
tutorial have been changed then an explanatory note about this is
shown in the beginning of the tutorial buffer.

When the tutorial buffer is killed the content and point position
in the buffer is saved so that the tutorial may be resumed
later."
  (interactive (list
;;                 (condition-case nil
;;                     (widget-choose "The following viper tutorials are available"
;;                                    (mapcar (lambda(rec)
;;                                              (cons (nth 2 rec) (nth 0 rec)))
;;                                            viper-tut--parts))
;;                   (error nil))
                0
                ))
  (if (not (boundp 'viper-current-state))
      (let ((prompt
             "
  You can not run the Viper tutorial because you have
  not enabled Viper.

  Do you want to enable Viper and then run the Viper tutorial? "))
        (if (y-or-n-p prompt)
            (progn
              (message "")
              (viper-mode)
              (viper-tutorial 0))
          (message "Tutorial aborted by user")))

    (let* ((filename (viper-tut--file part))
           ;; Choose a buffer name including the language so that
           ;; several languages can be tested simultaneously:
           (tut-buf-name "Viper TUTORIAL")
           (old-tut-buf (get-buffer tut-buf-name))
           (old-tut-part (when old-tut-buf
                           (with-current-buffer old-tut-buf
                             viper-tut--part)))
           (old-tut-win (when old-tut-buf (get-buffer-window old-tut-buf t)))
           (old-tut-is-ok (when old-tut-buf
                            (and
                             (= part old-tut-part)
                             (not (buffer-modified-p old-tut-buf)))))
           old-tut-file
           (old-tut-point 1)
           viper-is-on)
      (unless (file-exists-p filename) (error "Can't fine %s" filename))
      (setq tutorial--point-after-chkeys (point-min))
      ;; Try to display the tutorial buffer before asking to revert it.
      ;; If the tutorial buffer is shown in some window make sure it is
      ;; selected and displayed:
      (if old-tut-win
          (raise-frame
           (window-frame
            (select-window (get-buffer-window old-tut-buf t))))
        ;; Else, is there an old tutorial buffer? Then display it:
        (when old-tut-buf
          (switch-to-buffer old-tut-buf)))
      ;; Use whole frame for tutorial
      (delete-other-windows)
      ;; If the tutorial buffer has been changed then ask if it should
      ;; be reverted:
      (when (and old-tut-buf
                 (not old-tut-is-ok)
                 (= part old-tut-part))
        (setq old-tut-is-ok
              (if dont-ask-for-revert
                  nil
                (not (y-or-n-p
                      "You have changed the Tutorial buffer.  Revert it? ")))))
      ;; (Re)build the tutorial buffer if it is not ok
      (unless old-tut-is-ok
        (switch-to-buffer (get-buffer-create tut-buf-name))
        (unless old-tut-buf (text-mode))
        (setq viper-is-on (boundp 'viper-mode-string))
        (setq viper-tut--part part)
        (setq old-tut-file (file-exists-p (viper-tut--saved-file)))
        (when (= part 0) (setq old-tut-file nil)) ;; You do not edit in the intro
        (let ((inhibit-read-only t))
          (erase-buffer))
        (message "Preparing Viper tutorial ...") (sit-for 0)

        ;; Do not associate the tutorial buffer with a file. Instead use
        ;; a hook to save it when the buffer is killed.
        (setq buffer-auto-save-file-name nil)
        (add-hook 'kill-buffer-hook 'viper-tut--save-tutorial nil t)

        ;; Insert the tutorial. First offer to resume last tutorial
        ;; editing session.
        (when dont-ask-for-revert
          (setq old-tut-file nil))
        (when old-tut-file
          (setq old-tut-file
                (y-or-n-p
                 (format
                  "Resume your last saved Viper tutorial part %s? "
                  part))))
        (if old-tut-file
            (progn
              (insert-file-contents (viper-tut--saved-file))
              (goto-char (point-min))
              (setq old-tut-point
                    (string-to-number
                     (buffer-substring-no-properties
                      (line-beginning-position) (line-end-position))))
              (forward-line)
              (setq tutorial--point-before-chkeys
                    (string-to-number
                     (buffer-substring-no-properties
                      (line-beginning-position) (line-end-position))))
              (forward-line)
              (delete-region (point-min) (point))
              (goto-char tutorial--point-before-chkeys)
              (setq tutorial--point-before-chkeys (point-marker)))
          ;;(insert-file-contents (expand-file-name filename data-directory))
          (insert-file-contents filename)
          (viper-tut--replace-links)
          (save-excursion
            (goto-char (point-min))
            (while (re-search-forward "'\\([][+a-zA-Z~<>!;,:.'\"%/?(){}$^0|-]\\)'" nil t)
              (let ((matched-char (match-string 1)))
                (put-text-property 0 1 'vi-char t matched-char)
                (put-text-property 0 1 'face '(:foreground "blue") matched-char)
                (replace-match matched-char))))
          (forward-line)
          (setq tutorial--point-before-chkeys (point-marker)))

        (viper-tut--add-remarks)

        (goto-char (point-min))
        (when old-tut-file
          ;; Just move to old point in saved tutorial.
          (let ((old-point
                 (if (> 0 old-tut-point)
                     (- old-tut-point)
                   (+ old-tut-point tutorial--point-after-chkeys))))
            (when (< old-point 1)
              (setq old-point 1))
            (goto-char old-point)))

        ;; Clear message:
        (message "") (sit-for 0)

        (setq buffer-undo-list nil)
        (set-buffer-modified-p nil)))))

(defun viper-tut--add-remarks()
  ;; Check if there are key bindings that may disturb the
  ;; tutorial.  If so tell the user.
  (let* ((tutorial--lang "English")
         (changed-keys
          (if (= viper-tut--part viper-tut--emacs-part)
              (tutorial--find-changed-keys tutorial--default-keys)
            (tutorial--find-changed-keys viper-tut--default-keys))))
    (viper-tut--display-changes changed-keys viper-tut--part))

  (if (= viper-tut--part viper-tut--emacs-part)
      (progn
        (add-hook 'viper-vi-state-hook 'viper-tut--at-change-state nil t)
        (add-hook 'viper-insert-state-hook 'viper-tut--at-change-state nil t)
        (add-hook 'viper-replace-state-hook 'viper-tut--at-change-state nil t)
        (add-hook 'viper-emacs-state-hook 'viper-tut--at-change-state nil t)
        )
    (remove-hook 'viper-vi-state-hook 'viper-tut--at-change-state t)
    (remove-hook 'viper-insert-state-hook 'viper-tut--at-change-state t)
    (remove-hook 'viper-replace-state-hook 'viper-tut--at-change-state t)
    (remove-hook 'viper-emacs-state-hook 'viper-tut--at-change-state t)
    )

  (save-excursion
    (goto-char (point-min))
    (viper-tut--insert-goto-row nil)
    (goto-char (point-max))
    (viper-tut--insert-goto-row t))
  )

(defun viper-tut--insert-goto-row(last)
  (let ((start (point))
        end)
    (insert " Go to part: ")
    (dolist (rec viper-tut--parts)
      (let ((n (nth 0 rec))
            (file (nth 1 rec))
            (title (nth 2 rec)))
        (if (= n viper-tut--part)
            (insert (format "%s" n))
          (insert-button (format "%s" n)
                         'help-echo title
                         'follow-link t
                         'action
                         (lambda (button)
                           (let ((part (button-get button 'part)))
                             (viper-tutorial part t)))
                         'part n))
        (insert "  ")))
    (insert "   ")
    (insert-button "Exit Tutorial"
                   'help-echo "Close tutorial buffer"
                   'follow-link t
                   'action
                   (lambda (button)
                     (kill-buffer (current-buffer))))
    (unless last (insert "\n"))
    (setq end (point))
    (put-text-property start end 'local-map tutorial--tab-map)
    (put-text-property start end 'tutorial-remark t)
    (put-text-property start end
                       'face '(:background "yellow" :foreground "#c00"))
    (put-text-property start end 'read-only t)))

(defun viper-tut--replace-links()
  "Replace markers for links with actual links."
  (let ((re-links (regexp-opt '("VIPER-MANUAL"
                                "README-FILE"
                                "DIGIT-ARGUMENT"
                                "KILL-BUFFER"
                                "ISEARCH-FORWARD"
                                "UNIVERSAL-ARGUMENT"
                                "SEARCH-COMMANDS"
                                "R-AND-R"
                                "CUA-MODE"
                                "KEYBOARD-MACROS"
                                "VIPER-TOGGLE-KEY"
                                "* EMACS-NOTICE:")))
        (case-fold-search nil))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward re-links nil t)
        (let ((matched (match-string 0))
              start
              end)
          (replace-match "")
          (setq start (point))
          (cond
           ((string= matched "VIPER-TOGGLE-KEY")
            (insert-button "viper-toggle-key"
                           'action
                           (lambda(button) (interactive)
                             (describe-variable 'viper-toggle-key))
                           'follow-link t))
           ((string= matched "CUA-MODE")
            (insert-button "cua-mode"
                           'action
                           (lambda(button) (interactive)
                             (describe-function 'cua-mode))
                           'follow-link t))
           ((string= matched "ISEARCH-FORWARD")
            (insert-button "isearch-forward"
                           'action
                           (lambda(button) (interactive)
                             (describe-function 'isearch-forward))
                           'follow-link t))
           ((string= matched "KILL-BUFFER")
            (insert-button "kill-buffer"
                           'action
                           (lambda(button) (interactive)
                             (describe-function 'kill-buffer))
                           'follow-link t))
           ((string= matched "UNIVERSAL-ARGUMENT")
            (insert-button "universal-argument"
                           'action
                           (lambda(button) (interactive)
                             (describe-function 'universal-argument))
                           'follow-link t))
           ((string= matched "DIGIT-ARGUMENT")
            (insert-button "digit-argument"
                           'action
                           (lambda(button) (interactive)
                             (describe-function 'digit-argument))
                           'follow-link t))
           ((string= matched "* EMACS-NOTICE:")
            (insert "* Emacs NOTICE:")
            (while (progn
                     (forward-line 1)
                     (not (looking-at "^$"))))
            (put-text-property start (point)
                               'face '(:background
                                       "#ffe4b5"
                                       :foreground "#999999")))
           ((string= matched "SEARCH-COMMANDS")
            (insert-button "search commands"
                           'action
                           (lambda(button) (interactive)
                             (info-other-window "(emacs) Search")
                             (message "Type C-x 0 to close the new window"))
                           'follow-link t))
           ((string= matched "KEYBOARD-MACROS")
            (insert-button "keyboard macros"
                           'action
                           (lambda(button) (interactive)
                             (info-other-window "(emacs) Keyboard Macros")
                             (message "Type C-x 0 to close the new window"))
                           'follow-link t))
           ((string= matched "VIPER-MANUAL")
            (insert-button "Viper manual"
                           'action
                           (lambda(button) (interactive)
                             (info-other-window "(viper)")
                             (message "Type C-x 0 to close the new window"))
                           'follow-link t))
           ((string= matched "R-AND-R")
            (insert-button "r and R"
                           'action
                           (lambda(button) (interactive)
                             (info-other-window "(viper) Basics")
                             (message "Type C-x 0 to close the new window"))
                           'follow-link t))
           ((string= matched "README-FILE")
            (insert-button "README file"
                           'action
                           (lambda(button) (interactive)
                             (find-file-other-window (expand-file-name "README" viper-tut-directory))
                             (message "Type C-x 0 to close the new window"))
                           'follow-link t))
           (t
            (error "Unmatched text: %s" matched)))
          (put-text-property start (point) 'tutorial-remark t)
          (put-text-property start (point) 'tutorial-orig matched)
          (put-text-property start (point) 'local-map tutorial--tab-map)
          (put-text-property start (point) 'read-only t))))))

(provide 'viper-tut)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; viper-tut.el ends here
