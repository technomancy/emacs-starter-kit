;;; cus-new-user.el --- Customize some important options
;;
;; Author: Lennart Borgman (lennart O borgman A gmail O com)
;; Created: 2009-07-10 Fri
;; Version: 0.2
;; Last-Updated: 2009-07-10 Fri
;; URL:
;; Keywords:
;; Compatibility:
;;
;; Features that might be required by this library:
;;
;;   None
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;    Customize significant options for which different user
;;    environment expectations might dictate different defaults.
;;
;;    After an idea of Scot Becker on Emacs Devel.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change log:
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(defvar cusnu-my-skin-widget nil)

(defvar cusnu-insert-os-spec-fun nil)

;;(customize-for-new-user)
;;;###autoload
(defun customize-for-new-user (&optional name)
  "Show special customization page for new user.
"
  (interactive)
  ;;(setq debug-on-error t)
  ;;(setq buffer-read-only t)
  (require 'cus-edit)
  (let ((inhibit-read-only t)
        fill-pos)
    (pop-to-buffer (custom-get-fresh-buffer (or name "*Customizations for New Users*")))
    (buffer-disable-undo)
    (Custom-mode)
    (erase-buffer)
    (widget-insert (propertize "Easy Customization for New Users\n" 'face '(:weight bold :height 1.5)))
    (setq fill-pos (point))
    (widget-insert
     "Below are some custom options that new users often may want to
tweak since they may make Emacs a bit more like what they expect from
using other software in their environment.

After this, at the bottom of this page, is a tool for exporting your own specific options.
You choose which to export, make a description and give the group of options a new and click a button.
Then you just mail it or put it on the web for others to use.

Since Emacs runs in many environment and an Emacs user may use
several of them it is hard to decide by default what a user
wants/expects.  Therefor you are given the possibility to easily
do those changes here.

Note that this is just a collection of normal custom options.
There are no new options here.


")
    (fill-region fill-pos (point))

    ;; Normal custom buffer header
    (let ((init-file (or custom-file user-init-file)))
      ;; Insert verbose help at the top of the custom buffer.
      (when custom-buffer-verbose-help
        (widget-insert "Editing a setting changes only the text in this buffer."
                       (if init-file
                           "
To apply your changes, use the Save or Set buttons.
Saving a change normally works by editing your init file."
                         "
Currently, these settings cannot be saved for future Emacs sessions,
possibly because you started Emacs with `-q'.")
                       "\nFor details, see ")
        (widget-create 'custom-manual
                       :tag "Saving Customizations"
                       "(emacs)Saving Customizations")
        (widget-insert " in the ")
        (widget-create 'custom-manual
                       :tag "Emacs manual"
                       :help-echo "Read the Emacs manual."
                       "(emacs)Top")
        (widget-insert "."))
      (widget-insert "\n")
      ;; The custom command buttons are also in the toolbar, so for a
      ;; time they were not inserted in the buffer if the toolbar was in use.
      ;; But it can be a little confusing for the buffer layout to
      ;; change according to whether or nor the toolbar is on, not to
      ;; mention that a custom buffer can in theory be created in a
      ;; frame with a toolbar, then later viewed in one without.
      ;; So now the buttons are always inserted in the buffer.  (Bug#1326)
;;;    (when (not (and (bound-and-true-p tool-bar-mode) (display-graphic-p)))
      (if custom-buffer-verbose-help
          (widget-insert "\n
 Operate on all settings in this buffer that are not marked HIDDEN:\n"))
      (let ((button (lambda (tag action active help icon)
                      (widget-insert " ")
                      (if (eval active)
                          (widget-create 'push-button :tag tag
                                         :help-echo help :action action))))
            (commands custom-commands))
        (apply button (pop commands)) ; Set for current session
        (apply button (pop commands)) ; Save for future sessions
        (if custom-reset-button-menu
            (progn
              (widget-insert " ")
              (widget-create 'push-button
                             :tag "Reset buffer"
                             :help-echo "Show a menu with reset operations."
                             :mouse-down-action 'ignore
                             :action 'custom-reset))
          (widget-insert "\n")
          (apply button (pop commands)) ; Undo edits
          (apply button (pop commands)) ; Reset to saved
          (apply button (pop commands)) ; Erase customization
          (widget-insert "  ")
          (pop commands) ; Help (omitted)
          (apply button (pop commands)))) ; Exit
      (widget-insert "\n\n")

      (widget-insert (propertize "\nThis part is for your own use\n" 'face '(:weight bold :height 1.5)))
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;; Editor emulator level

      (widget-insert "\n")
      (setq fill-pos (point))
      (widget-insert
"Emacs can emulate some common editing behaviours (and some uncommon too).
For the most common ones you can decide if you want to use them here:
")
      (fill-region fill-pos (point))
      (cusnu-mark-part-desc fill-pos (point))

      ;; CUA Mode
      (cusnu-insert-options '((cua-mode custom-variable)))

      ;; Viper Mode
      (widget-insert "\n")
      (widget-insert (propertize "Viper" 'face 'custom-variable-tag))
      (widget-insert ":")
      (setq fill-pos (point))
      (widget-insert "
   Viper is currently set up in a special way, please see the
   command `viper-mode'.  You can use custom to set up most of
   it.  However if you want to load Viper at startup you must
   explicitly include \(require 'viper) in your .emacs.
")
      (fill-region fill-pos (point))

      ;; Viper Mode
      (backward-delete-char 1)
      (cusnu-insert-options '((viper-mode custom-variable)))

      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;; OS specific

      (widget-insert "\n")
      (setq fill-pos (point))
      (widget-insert (format "OS specific options (%s): \n" system-type))
      (fill-region fill-pos (point))
      (cusnu-mark-part-desc fill-pos (point))

      (if cusnu-insert-os-spec-fun
          (funcall cusnu-insert-os-spec-fun)
       (widget-insert "No OS specific customizations.\n"))

      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;; Disputed settings

      (widget-insert "\n")
      (setq fill-pos (point))
      (widget-insert
"Some old time Emacs users want to change the options below:
")
      (fill-region fill-pos (point))
      (cusnu-mark-part-desc fill-pos (point))

      (cusnu-insert-options '((global-visual-line-mode custom-variable)))
      (cusnu-insert-options '((word-wrap custom-variable)))
      (cusnu-insert-options '((blink-cursor-mode custom-variable)))
      (cusnu-insert-options '((tool-bar-mode custom-variable)))
      (cusnu-insert-options '((tooltip-mode custom-variable)))
      ;;(cusnu-insert-options '((initial-scratch-message custom-variable)))

      (widget-insert "\n")
      (widget-insert (propertize "\n\nThis part is for exporting to others\n\n" 'face '(:weight bold :height 1.5)))
      (setq fill-pos (point))
      (widget-insert
"My skin options - This is for exporting custom options to other users
\(or maybe yourself on another computer).
This works the following way:

- You add a description of your options and the options you want to export below.
Then you click on `Export my skin options'.
This creates a file that you can send to other Emacs users.
They simply open that file in Emacs and follow the instructions there to test your options
and maybe save them for later use if they like them.
\(You can follow the instructions yourself to see how it works.)

Please change the group symbol name to something specific for you.
")
      (fill-region fill-pos (point))
      (cusnu-mark-part-desc fill-pos (point))

      (widget-insert "\n")
      (set (make-local-variable 'cusnu-my-skin-widget)
           (car
            (cusnu-insert-options '((cusnu-my-skin-options custom-variable)))))
      (widget-insert "\n")
      (widget-create 'push-button
                     :tag "Export my skin options             "
                     :action (lambda (&rest ignore)
                               (let ((use-dialog-box nil))
                                 (call-interactively 'cusnu-export-my-skin-options))))
      (widget-insert "\n")
      (widget-create 'push-button
                     :tag "Customize my skin options          "
                     :action (lambda (&rest ignore)
                               (let ((use-dialog-box nil))
                                 (call-interactively 'cusnu-customize-my-skin-options))))
      (widget-insert "\n")
      (widget-create 'push-button
                     :tag "Reset those options to saved values"
                     :action (lambda (&rest ignore)
                               (let ((use-dialog-box nil))
                                 (call-interactively 'cusnu-reset-my-skin-options))))

      ;; Finish setup buffer
      (mapc 'custom-magic-reset custom-options)
      (cusnu-make-xrefs)
      (widget-setup)
      (buffer-enable-undo)
      (goto-char (point-min)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Example on Emacs+Emacw32
(eval-when-compile (require 'emacsw32 nil t))
(when (fboundp 'emacsw32-version)
  (defun cusnu-emacsw32-show-custstart (&rest args)
    (emacsw32-show-custstart))
  (setq cusnu-insert-os-spec-fun 'cusnu-insert-emacsw32-specific-part)
  (defun cusnu-insert-emacsw32-specific-part ()
    (cusnu-insert-options '((w32-meta-style custom-variable)))
    (widget-insert "\n")
    (widget-insert (propertize "EmacsW32" 'face 'custom-variable-tag))
    (widget-insert "
   Easy setup for Emacs+EmacsW32.")
    (widget-insert "\n   ")
    (widget-create 'push-button :tag "Customize EmacsW32"
                   ;;:help-echo help
                   :action 'cusnu-emacsw32-show-custstart)
    (widget-insert "\n")))
;; End example
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun cusnu-mark-part-desc (beg end)
  (let ((ovl (make-overlay beg end)))
    (overlay-put ovl 'face 'highlight)))

(defun cusnu-make-xrefs (&optional beg end)
  (save-restriction
    (when (or beg end)
      (unless beg (setq beg (point-min)))
      (unless end (setq end (point-max)))
      (narrow-to-region beg end))
    (let ((here (point)))
      (goto-char (point-min))
      (cusnu-help-insert-xrefs 'cusnu-help-xref-button)
      (goto-char here))))

(defun widget-info-link-action (widget &optional event)
  "Open the info node specified by WIDGET."
  (info-other-window (widget-value widget)))

(defun widget-documentation-string-value-create (widget)
  ;; Insert documentation string.
  (let ((doc (widget-value widget))
	(indent (widget-get widget :indent))
	(shown (widget-get (widget-get widget :parent) :documentation-shown))
	(start (point)))
    (if (string-match "\n" doc)
	(let ((before (substring doc 0 (match-beginning 0)))
	      (after (substring doc (match-beginning 0)))
	      button)
	  (when (and indent (not (zerop indent)))
	    (insert-char ?\s indent))
	  (insert before ?\s)
	  (widget-documentation-link-add widget start (point))
	  (setq button
		(widget-create-child-and-convert
		 widget (widget-get widget :visibility-widget)
		 :help-echo "Show or hide rest of the documentation."
		 :on "Hide Rest"
		 :off "More"
		 :always-active t
		 :action 'widget-parent-action
		 shown))
	  (when shown
	    (setq start (point))
	    (when (and indent (not (zerop indent)))
	      (insert-char ?\s indent))
	    (insert after)
	    (widget-documentation-link-add widget start (point))
            (cusnu-make-xrefs start (point))
            )
	  (widget-put widget :buttons (list button)))
      (when (and indent (not (zerop indent)))
	(insert-char ?\s indent))
      (insert doc)
      (widget-documentation-link-add widget start (point))))
  (insert ?\n))
(defun cusnu-help-xref-button (match-number type what &rest args)
  (let ((beg (match-beginning match-number))
        (end (match-end match-number)))
  (if nil
      (let ((ovl (make-overlay beg end)))
        (overlay-put ovl 'face 'highlight))
    (let* ((tag (match-string match-number))
           (value what)
            (wid-type (cond
                       ((eq type 'help-variable)
                        'variable-link)
                       ((eq type 'help-function)
                        'function-link)
                       ((eq type 'help-info)
                        'custom-manual)
                       (t nil)))
          )
      (when wid-type
        (delete-region beg end)
        (backward-char)
        ;;(tag action active help icon)
        (widget-create wid-type
                       ;;tag
                       :value value
                       :tag tag
                       :keymap custom-mode-link-map
                       :follow-link 'mouse-face
                       :button-face 'custom-link
                       :mouse-face 'highlight
                       :pressed-face 'highlight
                       ;;:help-echo help
                       )))))
    )

;; Override default ... ;-)
(define-widget 'documentation-link 'link
  "Link type used in documentation strings."
  ;;:tab-order -1
  :help-echo "Describe this symbol"
  :button-face 'custom-link
  :action 'widget-documentation-link-action)

(defun cusnu-xref-niy (&rest ignore)
  (message "Not implemented yet"))

(defun cusnu-describe-function (wid &rest ignore)
  (let ((fun (widget-get wid :what))
        )
    (describe-function fun)))

(defun cusnu-help-insert-xrefs (help-xref-button)
  ;; The following should probably be abstracted out.
  (unwind-protect
      (progn
        ;; Info references
        (save-excursion
          (while (re-search-forward help-xref-info-regexp nil t)
            (let ((data (match-string 2)))
              (save-match-data
                (unless (string-match "^([^)]+)" data)
                  (setq data (concat "(emacs)" data))))
              (funcall help-xref-button 2 'help-info data))))
        ;; URLs
        (save-excursion
          (while (re-search-forward help-xref-url-regexp nil t)
            (let ((data (match-string 1)))
              (funcall help-xref-button 1 'help-url data))))
        ;; Mule related keywords.  Do this before trying
        ;; `help-xref-symbol-regexp' because some of Mule
        ;; keywords have variable or function definitions.
        (if help-xref-mule-regexp
            (save-excursion
              (while (re-search-forward help-xref-mule-regexp nil t)
                (let* ((data (match-string 7))
                       (sym (intern-soft data)))
                  (cond
                   ((match-string 3) ; coding system
                    (and sym (coding-system-p sym)
                         (funcall help-xref-button 6 'help-coding-system sym)))
                   ((match-string 4) ; input method
                    (and (assoc data input-method-alist)
                         (funcall help-xref-button 7 'help-input-method data)))
                   ((or (match-string 5) (match-string 6)) ; charset
                    (and sym (charsetp sym)
                         (funcall help-xref-button 7 'help-character-set sym)))
                   ((assoc data input-method-alist)
                    (funcall help-xref-button 7 'help-character-set data))
                   ((and sym (coding-system-p sym))
                    (funcall help-xref-button 7 'help-coding-system sym))
                   ((and sym (charsetp sym))
                    (funcall help-xref-button 7 'help-character-set sym)))))))
        ;; Quoted symbols
        (save-excursion
          (while (re-search-forward help-xref-symbol-regexp nil t)
            (let* ((data (match-string 8))
                   (sym (intern-soft data)))
              (if sym
                  (cond
                   ((match-string 3)  ; `variable' &c
                    (and (or (boundp sym) ; `variable' doesn't ensure
                                        ; it's actually bound
                             (get sym 'variable-documentation))
                         (funcall help-xref-button 8 'help-variable sym)))
                   ((match-string 4)   ; `function' &c
                    (and (fboundp sym) ; similarly
                         (funcall help-xref-button 8 'help-function sym)))
                   ((match-string 5) ; `face'
                    (and (facep sym)
                         (funcall help-xref-button 8 'help-face sym)))
                   ((match-string 6)) ; nothing for `symbol'
                   ((match-string 7)
;;;  this used:
;;; 			  #'(lambda (arg)
;;; 			      (let ((location
;;; 				     (find-function-noselect arg)))
;;; 				(pop-to-buffer (car location))
;;; 				(goto-char (cdr location))))
                    (funcall help-xref-button 8 'help-function-def sym))
                   ((and
                     (facep sym)
                     (save-match-data (looking-at "[ \t\n]+face\\W")))
                    (funcall help-xref-button 8 'help-face sym))
                   ((and (or (boundp sym)
                             (get sym 'variable-documentation))
                         (fboundp sym))
                    ;; We can't intuit whether to use the
                    ;; variable or function doc -- supply both.
                    (funcall help-xref-button 8 'help-symbol sym))
                   ((and
                     (or (boundp sym)
                         (get sym 'variable-documentation))
                     (or
                      (documentation-property
                       sym 'variable-documentation)
                      (condition-case nil
                          (documentation-property
                           (indirect-variable sym)
                           'variable-documentation)
                        (cyclic-variable-indirection nil))))
                    (funcall help-xref-button 8 'help-variable sym))
                   ((fboundp sym)
                    (funcall help-xref-button 8 'help-function sym)))))))
        ;; An obvious case of a key substitution:
        (save-excursion
          (while (re-search-forward
                  ;; Assume command name is only word and symbol
                  ;; characters to get things like `use M-x foo->bar'.
                  ;; Command required to end with word constituent
                  ;; to avoid `.' at end of a sentence.
                  "\\<M-x\\s-+\\(\\sw\\(\\sw\\|\\s_\\)*\\sw\\)" nil t)
            (let ((sym (intern-soft (match-string 1))))
              (if (fboundp sym)
                  (funcall help-xref-button 1 'help-function sym)))))
        ;; Look for commands in whole keymap substitutions:
        (save-excursion
          ;; Make sure to find the first keymap.
          (goto-char (point-min))
          ;; Find a header and the column at which the command
          ;; name will be found.

          ;; If the keymap substitution isn't the last thing in
          ;; the doc string, and if there is anything on the
          ;; same line after it, this code won't recognize the end of it.
          (while (re-search-forward "^key +binding\n\\(-+ +\\)-+\n\n"
                                    nil t)
            (let ((col (- (match-end 1) (match-beginning 1))))
              (while
                  (and (not (eobp))
                       ;; Stop at a pair of blank lines.
                       (not (looking-at "\n\\s-*\n")))
                ;; Skip a single blank line.
                (and (eolp) (forward-line))
                (end-of-line)
                (skip-chars-backward "^ \t\n")
                (if (and (>= (current-column) col)
                         (looking-at "\\(\\sw\\|\\s_\\)+$"))
                    (let ((sym (intern-soft (match-string 0))))
                      (if (fboundp sym)
                          (funcall help-xref-button 0 'help-function sym))))
                (forward-line))))))
    ;;(set-syntax-table stab)
    ))

(defun cusnu-insert-options (options)
  (widget-insert "\n")
  (setq custom-options
        (append
         (if (= (length options) 1)
             (mapcar (lambda (entry)
                       (widget-create (nth 1 entry)
                                      ;;:documentation-shown t
                                      :custom-state 'unknown
                                      :tag (custom-unlispify-tag-name
                                            (nth 0 entry))
                                      :value (nth 0 entry)))
                     options)
           (let ((count 0)
                 (length (length options)))
             (mapcar (lambda (entry)
                       (prog2
                           (message "Creating customization items ...%2d%%"
                                    (/ (* 100.0 count) length))
                           (widget-create (nth 1 entry)
                                          :tag (custom-unlispify-tag-name
                                                (nth 0 entry))
                                          :value (nth 0 entry))
                         (setq count (1+ count))
                         (unless (eq (preceding-char) ?\n)
                           (widget-insert "\n"))
                         (widget-insert "\n")))
                     options)))
         custom-options))
  (unless (eq (preceding-char) ?\n)
    (widget-insert "\n"))
  custom-options
  )

(defun cusnu-is-custom-obj (sym)
  "Return non-nil if symbol SYM is customizable."
  (or (get sym 'custom-type)
      (get sym 'face)
      (get sym 'custom-group)
      ))

(define-widget 'custom-symbol 'symbol
  "A customizable symbol."
  :prompt-match 'cusnu-is-custom-obj
  :prompt-history 'widget-variable-prompt-value-history
  :complete-function (lambda ()
		       (interactive)
		       (lisp-complete-symbol 'cusnu-is-custom-obj))
  :tag "Custom option")

(defun cusnu-set-my-skin-options (sym val)
  (set-default sym val)
  (let ((group (nth 0 val))
        (doc   (nth 1 val))
        (members (nth 2 val)))
    (custom-declare-group group nil doc)
    (put group 'custom-group nil)
    (dolist (opt members)
      (let ((type (cusnu-get-opt-main-type opt)))
        (when type
          (custom-add-to-group group opt type))))))

(defun cusnu-get-opt-main-type (opt)
  (when opt
    (cond ((get opt 'face) 'custom-face)
          ((get opt 'custom-type) 'custom-variable)
          ((get opt 'custom-group) 'custom-group))))

(defgroup all-my-loaded-skin-groups nil
  "All your loaded skin groups."
  :group 'environment
  :group 'convenience)

(defun cusnu-custom-group-p (symbol)
  (and (intern-soft symbol)
       (or (and (get symbol 'custom-loads)
                (not (get symbol 'custom-autoload)))
           (get symbol 'custom-group))))

(defcustom cusnu-my-skin-options '(my-skin-group "My skin group.\n\n\n\n\n" nil)
  "Your custom skin-like options.
The purpose of this variable is to provide for easy export a
selection of variables you choose to set to other users.

To send these values to other users you export them to a file
with `cusnu-export-my-skin-options'."
  :type '(list (symbol :tag "My custom group symbol name (should be specific to you)")
               (string :tag "My custom group description")
               (repeat :tag "Add your custom options below"
                       (custom-symbol :tag "My custom option")))
  :set 'cusnu-set-my-skin-options
  :group 'all-my-loaded-skin-groups)

;;(cusnu-ring-bell "bell")
(defun cusnu-ring-bell (format-string &rest args)
  (message "%s" (propertize (apply
                             'format format-string args) 'face 'secondary-selection))
  (ding)
  (throw 'bell nil))

;;;###autoload
(defun cusnu-export-my-skin-options (file)
  "Export to file FILE custom options in `cusnu-my-skin-options'.
The options is exported to elisp code that other users can run to
set the options that you have added to `cusnu-my-skin-options'.

For more information about this see `cusnu-export-cust-group'."
  (interactive '(nil))
  (catch 'bell
    (let ((grp (nth 0 cusnu-my-skin-options))
          buf)
      (let ((state (plist-get (cdr cusnu-my-skin-widget) :custom-state)))
        (case state
          ((set saved) nil) ;;(error "test, state=%s" state))
          (standard (cusnu-ring-bell "Please enter your options first"))
          (t (cusnu-ring-bell "My Skin Options must be saved or set, use the State button, %s" state))))
      (unless (nth 2 cusnu-my-skin-options)
        (cusnu-ring-bell "You have not added any of your options"))
      (unless file
        (setq file (read-file-name "Save to file: ")))
      (when (file-exists-p file)
        (cusnu-ring-bell "File %s already exists, choose another file name" file))
      (setq buf (find-file-other-window file))
      (with-current-buffer buf
        (unless (eq major-mode 'emacs-lisp-mode) (emacs-lisp-mode))
        (unless (file-exists-p (buffer-file-name))
          (erase-buffer)))
      (cusnu-export-cust-group grp buf))))

(defun cusnu-customize-my-skin-options ()
  (interactive)
  (customize-group-other-window (nth 0 cusnu-my-skin-options)))

(defun cusnu-reset-my-skin-options ()
  "Reset to my defaults for those options.
"
  (interactive)
  (cusnu-reset-group-options-to-my-defaults (nth 0 cusnu-my-skin-options)))

(defun cusnu-reset-group-options-to-my-defaults (group)
  (dolist (sym-typ (get group 'custom-group))
    (let ((symbol (nth 0 sym-typ))
          ;;(type (cusnu-get-opt-main-type symbol))
          (type   (nth 1 sym-typ))
          defval)
      (cond
       ((eq type 'custom-variable)
        ;; First try reset to saved.
        (let* ((set (or (get symbol 'custom-set) 'set-default))
               (value (get symbol 'saved-value))
               (comment (get symbol 'saved-variable-comment)))
          (cond ((or comment value)
                 (put symbol 'variable-comment comment)
                 (custom-push-theme 'theme-value symbol 'user 'set (car-safe value))
                 (condition-case err
                     (funcall set symbol (eval (car value)))
                   (error (message "%s" err))))
                ;; If symbol was not saved then reset to standard.
                (t
                 (unless (get symbol 'standard-value)
                   (error "No standard setting known for %S" symbol))
                 (put symbol 'variable-comment nil)
                 (put symbol 'customized-value nil)
                 (put symbol 'customized-variable-comment nil)
                 (custom-push-theme 'theme-value symbol 'user 'reset)
                 (custom-theme-recalc-variable symbol)
                 (put symbol 'saved-value nil)
                 (put symbol 'saved-variable-comment nil)
                 ))))
       ((eq type 'custom-face)
        ;; First try reset to saved
        (let* ((value (get symbol 'saved-face))
               (comment (get symbol 'saved-face-comment)))
          (cond ((or value comment)
                 (put symbol 'customized-face nil)
                 (put symbol 'customized-face-comment nil)
                 (custom-push-theme 'theme-face symbol 'user 'set value)
                 (face-spec-set symbol value t)
                 (put symbol 'face-comment comment))
                ;; If symbol was not saved then reset to standard.
                (t
                 (setq value (get symbol 'face-defface-spec))
                 (unless value
                   (error "No standard setting for this face"))
                 (put symbol 'customized-face nil)
                 (put symbol 'customized-face-comment nil)
                 (custom-push-theme 'theme-face symbol 'user 'reset)
                 (face-spec-set symbol value t)
                 (custom-theme-recalc-face symbol)
                 ;; Do this later.
                 (put symbol 'saved-face nil)
                 (put symbol 'saved-face-comment nil)
                 ))))
       (t (error "not iy"))))))

(defun cusnu-export-cust-group (group buf)
  "Export custom group GROUP to end of buffer BUF.
Only the options that has been customized will be exported.

The group is exported as elisp code.  Running the code will
create a group with just those members.  After this it opens a
customization buffer with the new group.

The code will also set the options to the customized values, but
it will not save them in the users init file.

See also the comment in the exported file."
  (let (start
        (doc (get group 'group-documentation))
        groups options faces
        (members (mapcar (lambda (rec)
                           (car rec))
                         (get group 'custom-group))))
    (with-current-buffer buf
      (insert (format-time-string ";; Here is my skin custom group %Y-%m-%d.\n"))
      (font-lock-mode 1)
      (insert (format ";;;;;; Customization group name:  %s\n" group))
      (insert ";;\n")
      (let ((here (point)))
        (insert doc "\n")
        (comment-region here (point))
        (fill-region here (point)))
      (cusnu-get-options-and-faces members 'groups 'options 'faces)
      (unless (or options faces)
        (cusnu-ring-bell "There are no options or faces in %s customized by you" group))
      (insert "
;; This file defines the group and sets the options in it, but does
;; not save the values to your init file.
;;
;; To set the values evaluate this file.  To do that open this file in Emacs and to
;;
;;   M-x eval-buffer
;;
;; To go back to your default evaluate next line (place point at the end and to C-x C-e):
")
      (insert (format ";; (cusnu-reset-group-options-to-my-defaults '%s)\n\n"  group))
      (insert (format "(let ((grp '%s))\n" group))
      (insert (format "  (custom-declare-group grp nil %S)\n" doc))
      (insert "  (put grp 'custom-group nil)\n")
      (insert (format "  (custom-add-to-group 'all-my-loaded-skin-groups '%s 'custom-group)\n" group))
      (dolist (opt members)
        (let ((type (cusnu-get-opt-main-type opt)))
          (when type
            (insert (format "  (custom-add-to-group grp '%s '%s)\n"
                            opt type)))))
      (insert "  (custom-set-variables\n")
      (dolist (opt options)
        (let ((my-val (or (get opt 'saved-value)
                          (get opt 'customized-value))))
          (when my-val
            (insert (format "   '(%s %S)\n" opt (custom-quote (symbol-value opt)))))))
      (insert "   )\n")
      (insert "  (custom-set-faces\n")
      (dolist (opt faces)
        (let ((my-val (get opt 'customized-face)))
          (when my-val
            (insert (format "   '(%s %S)\n" opt my-val)))))
      (insert "   ))\n")
      (insert (format "\n(customize-group '%s)\n" group))
      )))

(defun cusnu-get-options-and-faces (members groups-par options-par faces-par)
  (dolist (sym members)
    (insert (format ";; sym=%s\n" sym))
    (cond ((and (get sym 'custom-type)
           (or (get sym 'saved-value)
               (get sym 'customize-value)))
           (add-to-list options-par sym))
          ((and (get sym 'face)
                (get sym 'customized-face))
           (add-to-list faces-par sym))
          ((get sym 'custom-group)
           (unless (memq sym groups-par) ;; Don't loop
             (cusnu-get-options-and-faces groups-par options-par faces-par)))
          (t (insert ";; Not a custom variable or face: %s\n" sym)))))

(provide 'cus-new-user)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; cus-new-user.el ends here
