;;; rngalt.el --- Tools for making completion addition to nxml mode
;;
;; Author: Lennart Borgman
;; Created: Wed Jan 10 17:17:18 2007
(defconst rngalt:version "0.51") ;;Version:
;; Last-Updated: 2008-03-08T03:33:56+0100 Sat
;; Keywords:
;; Compatibility:
;;
;; Features that might be required by this library:
;;
;;   `nxml-enc', `nxml-ns', `nxml-parse', `nxml-util',
;;   `ourcomments-util', `rng-dt', `rng-loc', `rng-match',
;;   `rng-parse', `rng-pttrn', `rng-uri', `rng-util', `rng-valid',
;;   `xmltok'.
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

(eval-when-compile (require 'rng-valid))
(eval-when-compile (require 'rng-nxml))
(eval-when-compile (unless load-file-name (require 'nxhtml-mode)))

(require 'rng-valid)
;;(require 'ourcomments-util)

(defvar rngalt-complete-first-try nil
  "First function to try for completion.
If non-nil should be a function with no parameters.  Used by
`rngalt-complete'.")

(defvar rngalt-complete-last-try nil
  "Last function to try for completion.
If non-nil should be a function with no parameters.  Used by
`rngalt-complete'.")

(defvar rngalt-completing-read-tag nil
  "Alternate function for completing tag name.
If non-nil should be a function with the same parameters as
`completing-read'.  Used by `rngalt-complete'.")

(defvar rngalt-completing-read-attribute-name nil
  "Alternate function for completing attribute name.
If non-nil should be a function with the same parameters as
`completing-read'.  Used by `rngalt-complete'.")

(defvar rngalt-completing-read-attribute-value nil
  "Alternate function for completing attribute value.
If non-nil should be a function with the same parameters as
`completing-read'.  Used by `rngalt-complete'.")


(defun rngalt-finish-element ()
  "Finish the current element by inserting an end-tag.
Like `nxml-finish-element' but takes `rngalt-validation-header'
into account."
  (interactive "*")
  (rngalt-finish-element-1 nil))

;; Fix-me: Check the other uses of `nxml-finish-element-1'. But this
;; is maybe not necessary since the only other use is in
;; `nxml-split-element' and that will anyway work - I believe ...
(defun rngalt-finish-element-1 (startp)
  "Insert an end-tag for the current element and optionally a start-tag.
The start-tag is inserted if STARTP is non-nil.  Return the position
of the inserted start-tag or nil if none was inserted.

This is like `nxml-finish-element-1' but takes
`rngalt-validation-header' into account."
  (interactive "*")
  (let (token-end
        start-tag-end
        starts-line
        ends-line
        start-tag-indent
        qname
        inserted-start-tag-pos)
    ;; Temporary insert the fictive validation header if any.
    (let ((buffer-undo-list nil)
          (here (point-marker)))
      (when rngalt-validation-header
        (let ((vh (nth 2 rngalt-validation-header)))
          (set-marker-insertion-type here t)
          (save-restriction
            (widen)
            (goto-char (point-min))
            (insert vh)))
        (goto-char here))
      (setq token-end (nxml-token-before))
      (setq start-tag-end
            (save-excursion
              (when (and (< (point) token-end)
                         (memq xmltok-type
                               '(cdata-section
                                 processing-instruction
                                 comment
                                 start-tag
                                 end-tag
                                 empty-element)))
                (error "Point is inside a %s"
                       (nxml-token-type-friendly-name xmltok-type)))
              (nxml-scan-element-backward token-end t)))
      (when start-tag-end
        (setq starts-line
              (save-excursion
                (unless (eq xmltok-type 'start-tag)
                  (error "No matching start-tag"))
                (goto-char xmltok-start)
                (back-to-indentation)
                (eq (point) xmltok-start)))
        (setq ends-line
              (save-excursion
                (goto-char start-tag-end)
                (looking-at "[ \t\r\n]*$")))
        (setq start-tag-indent (save-excursion
                                 (goto-char xmltok-start)
                                 (current-column)))
        (setq qname (xmltok-start-tag-qname)))

      ;; Undo the insertion of the fictive header:
      (undo-start)
      (while (and (not (eq t pending-undo-list))
                  pending-undo-list)
        (undo-more 1))
      (goto-char here))

    (unless start-tag-end (error "No more start tags"))

    (when (and starts-line ends-line)
      ;; start-tag is on a line by itself
      ;; => put the end-tag on a line by itself
      (unless (<= (point)
                  (save-excursion
                    (back-to-indentation)
                    (point)))
        (insert "\n"))
      (indent-line-to start-tag-indent))
    (insert "</" qname ">")
    (when startp
      (when starts-line
        (insert "\n")
        (indent-line-to start-tag-indent))
      (setq inserted-start-tag-pos (point))
      (insert "<" qname ">")
      (when (and starts-line ends-line)
        (insert "\n")
        (indent-line-to (save-excursion
                          (goto-char xmltok-start)
                          (forward-line 1)
                          (back-to-indentation)
                          (if (= (current-column)
                                 (+ start-tag-indent nxml-child-indent))
                              (+ start-tag-indent nxml-child-indent)
                            start-tag-indent)))))
    inserted-start-tag-pos))

(defun rngalt-complete ()
  "Complete the string before point using the current schema.
Return non-nil if in a context it understands.

This function should be added to `nxml-completion-hook' before
`rng-complete'. By default it works just like this function, but
you can add your own completion by setting the variables
`rngalt-complete-first-try', `rngalt-completing-read-tag',
`rngalt-completing-read-attribute-name',
`rngalt-completing-read-attribute-value' and
`rngalt-complete-last-try'."
  (interactive)
  (unless rng-validate-mode
    (when (y-or-n-p
           "XML Validation is not on. Do you want to turn it on? ")
      (rng-validate-mode 1)))
  (when rng-validate-mode
    ;; schema file may mismatch if user sets it explicitly:
    (rngalt-reapply-validation-header)
    (when rng-current-schema-file-name
      (rngalt-validate))
    (or (when rngalt-complete-first-try
          (funcall rngalt-complete-first-try))
        (progn
          (unless rng-current-schema-file-name
            (when (eq major-mode 'nxhtml-mode)
              (when (y-or-n-p
                     "There is currently no DTD specified for the buffer.
This makes XHTML completion impossible. You can add a fictive
XHTML validation header that sets the DTD to XHTML.  This will
not be inserted in the buffer but completion and XHTML validation
will assume it is there so both error checking and completion
will work.

Do you want to add a fictive XHTML validation header? ")
                (message "") ;; Get rid of the large minibuffer message window
                (nxhtml-validation-header-mode)
                )))
          (let ((lt-pos (save-excursion (search-backward "<" nil t)))
                xmltok-dtd)
            (or (and lt-pos
                     (= (rng-set-state-after lt-pos) lt-pos)
                     (or (rngalt-complete-tag lt-pos)
                         (rng-complete-end-tag lt-pos)
                         (rngalt-complete-attribute-name lt-pos)
                         (rngalt-complete-attribute-value lt-pos)))
                (when rngalt-complete-last-try
                  (funcall rngalt-complete-last-try))))))))

(defun rngalt-validate ()
  (unless (= (buffer-size) 0)
    (let ((while-n1 0)
          (maxn1 20))
      (condition-case err
          (while (and (> maxn1 (setq while-n1 (1+ while-n1)))
                      (rng-do-some-validation))
            nil)
        (error
         ;; FIX-ME: for debugging:
         ;;(lwarn 'rngalt-validate :error "%s" (error-message-string err))
         (message "rngalt-validate: %s" (error-message-string err))
         nil))
      (when (>= while-n1 maxn1)
        (error "rngalt-validate: Could not validate")))
    (rng-validate-done)))

(defvar rngalt-region-ovl nil)
(defvar rngalt-region-prepared nil)
(defun rngalt-complete-tag-region-prepare ()
  (unless rngalt-region-prepared
    (when rngalt-region-ovl
      (when (overlayp rngalt-region-ovl)
        (delete-overlay rngalt-region-ovl))
      (setq rngalt-region-ovl nil))
    (when (and mark-active
               transient-mark-mode)
      (let ((beginning (region-beginning))
            (end       (region-end)))
        (unless (= (point) (region-beginning))
          (goto-char beginning))
        (when (save-excursion
                (when (re-search-forward "\\=[^<]*\\(?:<[^<]*>\\)*[^>]*" end t)
                  (= end (point))))
          (setq rngalt-region-ovl (make-overlay beginning end))
          (overlay-put rngalt-region-ovl 'face 'region)
          )))
    (setq rngalt-region-prepared t)))

(defun rngalt-complete-tag-region-cleanup ()
  (when rngalt-region-prepared
    (when (overlayp rngalt-region-ovl)
      (delete-overlay rngalt-region-ovl))
    (deactivate-mark)
    (setq rngalt-region-prepared nil)))

(defun rngalt-complete-tag-region-finish ()
  (when (and rngalt-region-prepared
             (overlayp rngalt-region-ovl))
    (let ((here (point)))
      (insert ">")
      (goto-char (overlay-end rngalt-region-ovl))
      (nxml-finish-element)
      (rngalt-validate)
      (goto-char here)))
  (rngalt-complete-tag-region-cleanup))

(defun rngalt-complete-tag (lt-pos)
  "Like `rng-complete-tag' but with some additions.
The additions are:
- Alternate completion.
- Complete around highlighted region.

See also the variable `rngalt-completing-read-tag'."
  (let (rng-complete-extra-strings)
    (when (and (= lt-pos (1- (point)))
               rng-complete-end-tags-after-<
               rng-open-elements
               (not (eq (car rng-open-elements) t))
               (or rng-collecting-text
                   (rng-match-save
                     (rng-match-end-tag))))
      (setq rng-complete-extra-strings
            (cons (concat "/"
                          (if (caar rng-open-elements)
                              (concat (caar rng-open-elements)
                                      ":"
                                      (cdar rng-open-elements))
                            (cdar rng-open-elements)))
                  rng-complete-extra-strings)))
    (when (save-excursion
            (re-search-backward rng-in-start-tag-name-regex
                                lt-pos
                                t))
      (and rng-collecting-text (rng-flush-text))
      (rngalt-complete-tag-region-prepare)
      (let ((completion
             (let ((rng-complete-target-names
                    (rng-match-possible-start-tag-names))
                   (rng-complete-name-attribute-flag nil))
               (rngalt-complete-before-point (1+ lt-pos)
                                             'rng-complete-qname-function
                                             "Insert tag: "
                                             nil
                                             'rng-tag-history
                                             rngalt-completing-read-tag)))
            name)
        (when completion
          (cond ((rng-qname-p completion)
                 (setq name (rng-expand-qname completion
                                              t
                                              'rng-start-tag-expand-recover))
                 (when (and name
                            (rng-match-start-tag-open name)
                            (or (not (rng-match-start-tag-close))
                                ;; need a namespace decl on the root element
                                (and (car name)
                                     (not rng-open-elements))))
                   ;; attributes are required
                   (insert " "))
                 (rngalt-complete-tag-region-finish)
                 (run-hook-with-args 'rngalt-complete-tag-hooks completion)
                 )
                ((member completion rng-complete-extra-strings)
                 (insert ">")))))
      (rngalt-complete-tag-region-finish)
      t)))

(defvar rngalt-complete-tag-hooks nil
  "Hook run after completing a tag.
Each function is called with the last name of the last tag
completed.")

(defun rngalt-complete-attribute-name (lt-pos)
  "Like `rng-complete-attribute-name' but with alternate completion.
See the variable `rngalt-completing-read-attribute-name'."
  (when (save-excursion
          (re-search-backward rng-in-attribute-regex lt-pos t))
    (let ((attribute-start (match-beginning 1))
          rng-undeclared-prefixes)
      (and (rng-adjust-state-for-attribute lt-pos
                                           attribute-start)
           (let ((rng-complete-target-names
                  (rng-match-possible-attribute-names))
                 (rng-complete-extra-strings
                  (mapcar (lambda (prefix)
                            (if prefix
                                (concat "xmlns:" prefix)
                              "xmlns"))
                          rng-undeclared-prefixes))
                 (rng-complete-name-attribute-flag t)
                 completion)
             (setq completion
                   (rngalt-complete-before-point attribute-start
                                                 'rng-complete-qname-function
                                                 "Attribute: "
                                                 nil
                                                 'rng-attribute-name-history
                                                 rngalt-completing-read-attribute-name))
             (when (and completion
                        (< 0 (length completion)))
               (insert "=\"")))))
    t))

(defun rngalt-complete-attribute-value (lt-pos)
  "Like `rng-complete-attribute-value' but with alternate completion.
See the variable `rngalt-completing-read-attribute-value'."
  (when (save-excursion
          (re-search-backward rng-in-attribute-value-regex lt-pos t))
    (let ((name-start (match-beginning 1))
          (name-end (match-end 1))
          (colon (match-beginning 2))
          (value-start (1+ (match-beginning 3))))
      (and (rng-adjust-state-for-attribute lt-pos
                                           name-start)
           (if (string= (buffer-substring-no-properties name-start
                                                        (or colon name-end))
                        "xmlns")
               (rngalt-complete-before-point
                value-start
                (rng-strings-to-completion-alist
                 (rng-possible-namespace-uris
                  (and colon
                       (buffer-substring-no-properties (1+ colon) name-end))))
                "Namespace URI: "
                nil
                'rng-namespace-uri-history
                rngalt-completing-read-attribute-value) ;; fix-me
             (rng-adjust-state-for-attribute-value name-start
                                                   colon
                                                   name-end)
             (rngalt-complete-before-point
              value-start
              (rng-strings-to-completion-alist
               (rng-match-possible-value-strings))
              "Value: "
              nil
              'rng-attribute-value-history
              rngalt-completing-read-attribute-value))
           (unless (eq (char-after) (char-before value-start))
             (insert (char-before value-start)))))
    t))

(defun rngalt-complete-before-point (start table prompt &optional predicate hist altcompl)
  "Complete text between START and point.
Works like `rng-complete-before-point' if ALTCOMPL is nil.  When
ALTCOMPL is a function symbol and no completion alternative is
available from table then this is called instead of
`compleating-read' with the same parameters."
  (let* ((orig (buffer-substring-no-properties start (point)))
         (completion (try-completion orig table predicate))
         (completing-fun (if altcompl altcompl 'completing-read))
         (completion-ignore-case t))
    (cond ((not (or completion completing-fun))
           (if (string= orig "")
               (message "No completions available")
             (message "No completion for %s" (rng-quote-string orig)))
           (ding)
           nil)
          ((eq completion t) orig)
          ((and completion
                (not (string= completion orig)))
           (delete-region start (point))
           (insert completion)
           (cond ((not (rng-completion-exact-p completion table predicate))
                  (message "Incomplete")
                  nil)
                 ((eq (try-completion completion table predicate) t)
                  completion)
                 (t
                  (message "Complete but not unique")
                  nil)))
          (t
           (setq completion
                 (let ((saved-minibuffer-setup-hook
                        (default-value 'minibuffer-setup-hook)))
                   (add-hook 'minibuffer-setup-hook
                             'minibuffer-completion-help
                             t)
                   (unwind-protect
                       (funcall completing-fun
                                prompt
                                table
                                predicate
                                nil
                                orig
                                hist)
                     (setq-default minibuffer-setup-hook
                                   saved-minibuffer-setup-hook))))
           (when completion
             (delete-region start (point))
             (insert completion))
           completion))))

(defun rngalt-get-missing-required-attr (single-tag)
  "Get a list of missing required attributes.
This is to be used when completing attribute names.
SINGLE-TAG should be non-nil if the tag has no end tag.

For a typical use see `nxhtml-completing-read-attribute-name' in
nxhtml.el.
"
  ;; FIX-ME: This is a terrible cludge. One day I hope I will
  ;; understand how to write this ;-)
  ;;
  ;; I currently fetch the missing tags from the error message in the
  ;; error overlay set by rng validate.
  (let ((here (point)))
    (unless (save-match-data (looking-at "[^<]\\{,200\\}>"))
      ;; We can probably add a >, so let us do it:
      (when single-tag
        (insert "/"))
      (insert ">")
      (rngalt-validate))
    (goto-char here))
  (let ((ovl (rng-error-overlay-message (or (rng-error-overlay-after (point))
                                            (rng-error-overlay-after (1- (point)))))))
    ;;(message "ovl=%s" ovl)(sit-for 1)
    ;;(message "prop ovl=%s" (overlay-properties ovl))(sit-for 1)
    (when (and ovl
               (eq (overlay-get ovl 'category) 'rng-error))
      ;;(message "rng-error")(sit-for 1)
      (let ((msg (overlay-get ovl 'help-echo)))
        ;;(message "msg=%s" msg);(sit-for 1)
        (when (string-match "Missing attributes? \\(.*\\)" msg)
          ;;(message "0=%s" (match-string 0 msg));(sit-for 1)
          ;;(message "1=%s" (match-string 1 msg));(sit-for 1)
          (let* ((matches (match-string 1 msg))
                 (lst (split-string (substring matches 1 (- (length matches) 1)) "\", \"")))
            ;;(message "matches=%s" matches);(sit-for 2)
            ;;(message "lst=%s" lst);(sit-for 1)
            lst))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Validation start state

;; FIX-ME: remember to clear these variable, but where?
(defvar rngalt-validation-header nil)
(make-variable-buffer-local 'rngalt-validation-header)
(put 'rngalt-validation-header 'permanent-local t)

(defvar rngalt-current-schema-file-name nil)
(make-variable-buffer-local 'rngalt-current-schema-file-name)
(put 'rngalt-current-schema-file-name 'permanent-local t)

(defvar rngalt-validation-header-overlay nil)
(make-variable-buffer-local 'rngalt-validation-header-overlay)
(put 'rngalt-validation-header-overlay 'permanent-local t)

(defvar rngalt-major-mode nil)
(make-variable-buffer-local 'rngalt-major-mode)
(put 'rngalt-major-mode 'permanent-local t)

(defun rngalt-after-change-major ()
  (unless (and (boundp 'mumamo-set-major-running)
               mumamo-set-major-running)
    (setq rngalt-major-mode major-mode)
    (when (and (derived-mode-p 'nxml-mode)
               rngalt-validation-header)
      (rngalt-reapply-validation-header))
    (rngalt-update-validation-header-overlay)))

(defvar rngalt-validation-header-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-1] 'rngalt-minimal-validation-header-toggle)
    map))

(defun rngalt-update-validation-header-overlay ()
  (if (and rngalt-display-validation-header
           rngalt-validation-header
           (or (derived-mode-p 'nxml-mode)
               (let ((major-mode rngalt-major-mode))
                 (and major-mode
                      (derived-mode-p 'nxml-mode))))
           )
      (progn
        (if rngalt-validation-header-overlay
            (move-overlay rngalt-validation-header-overlay 1 1)
          (setq rngalt-validation-header-overlay (make-overlay 1 1)))
        (overlay-put rngalt-validation-header-overlay
                     'priority 1000)
        ;; Other properties should go to the 'before-string
        (let* ((validation-header (nth 2 rngalt-validation-header))
               (header
               (if rngalt-minimal-validation-header
                   (propertize
                    (concat
                     "*** Fictive XHTML/XML Validation Header: ... "
                     (save-match-data
                       (if (string-match "\\(<[^[:space:]>]+\\)[^>]*>[^<>]*\\'"
                                         validation-header)
                           (concat (match-string 1 validation-header) ">")
                         "Error"))
                     "\n")
                    'face 'rngalt-validation-header-bottom)
                 (concat
                  (propertize "*** Fictive XHTML/XML Validation Header:\n"
                              'face 'rngalt-validation-header-top)
                  (propertize (concat validation-header "\n")
                              'face 'rngalt-validation-header-bottom)))))
          (setq header
                (propertize
                 header
                 'help-echo
                 "Click to toggle full/minimal display of header"
                 'keymap rngalt-validation-header-keymap))
          (overlay-put rngalt-validation-header-overlay
                       'before-string header)))
    (when rngalt-validation-header-overlay
      (delete-overlay rngalt-validation-header-overlay))))

(defun rngalt-update-validation-header-overlay-everywhere ()
  (dolist (b (buffer-list))
    (when (buffer-live-p b)
      (with-current-buffer b
        (when rngalt-validation-header
          (rngalt-update-validation-header-overlay))))))

(define-toggle rngalt-display-validation-header t
  "Display XML validation headers at the top of buffer when t.
The validation header is only displayed in buffers where the main
major mode is derived from `nxml-mode'."
  :set (lambda (sym val)
         (set-default sym val)
         (rngalt-update-validation-header-overlay-everywhere))
  :group 'nxml
  :group 'nxhtml)

(define-toggle rngalt-minimal-validation-header t
  "If non-nil display only a short informaion about the XML validation header.
See also `rngalt-display-validation-header'."
  :set (lambda (sym val)
         (set-default sym val)
         (rngalt-update-validation-header-overlay-everywhere))
  :group 'nxml
  :group 'nxhtml)

(defface rngalt-validation-header-top
  '((t (:foreground "RGB:87/CE/FA" :background "white")))
  "Face first line of validation header."
  :group 'nxhtml)

(defface rngalt-validation-header-bottom
  '((t (:foreground "white" :background "RGB:87/CE/FA")))
  "Face first line of validation header."
  :group 'nxhtml)

;; This is exactly the same as the original `rng-set-initial-state'
;; except when `rngalt-validation-header' is non-nil."
(defadvice rng-set-initial-state (around
                                  rngalt-set-initial-state
                                  activate
                                  compile
                                  )
  (nxml-ns-init)
  (rng-match-start-document)
  (setq rng-open-elements nil)
  (setq rng-pending-contents nil)
  (when rngalt-validation-header
      (let ((state (car rngalt-validation-header)))
        (rng-restore-state state)))
  (setq ad-return-value (goto-char (point-min))))

;; (defun rng-new-validate-prepare ()
;;   "Prepare to do some validation, initializing point and the state.
;; Return t if there is work to do, nil otherwise.

;; This is exactly the same as the original-insert-directory
;; `rng-validate-prepare' with the difference that the state at
;; point 1 is set differently if `rngalt-validation-header' is
;; non-nil.

;; See also `rng-set-initial-state'."
;;   (cond ((= rng-validate-up-to-date-end 1)
;;          (rng-set-initial-state)
;;       t)
;;      ((= rng-validate-up-to-date-end (point-max))
;;       nil)
;;      (t (let ((state
;;                   (if (and rngalt-validation-header
;;                            (= rng-validate-up-to-date-end 1))
;;                       (car rngalt-validation-header)
;;                     (get-text-property (1- rng-validate-up-to-date-end)
;;                                        'rng-state))))
;;              (cond (state
;;                  (rng-restore-state state)
;;                  (goto-char rng-validate-up-to-date-end))
;;                 (t
;;                  (let ((pos (previous-single-property-change
;;                              rng-validate-up-to-date-end
;;                              'rng-state)))
;;                    (cond (pos
;;                           (rng-restore-state
;;                            (or (get-text-property (1- pos) 'rng-state)
;;                                (error "Internal error: state null")))
;;                           (goto-char pos))
;;                          (t (rng-set-initial-state))))))))))


;; For as-external.el
;;;###autoload
(defun rngalt-set-validation-header (start-of-doc)
  (rng-validate-mode -1)
  (if start-of-doc
      (progn
        (add-hook 'after-change-major-mode-hook 'rngalt-after-change-major nil t)
        (setq rngalt-validation-header (rngalt-get-state-after start-of-doc))
        (rng-set-schema-file-1 (cadr rngalt-validation-header))
        (setq rngalt-current-schema-file-name rng-current-schema-file-name)
        (setq rng-compile-table nil)
        (setq rng-ipattern-table nil)
        (setq rng-last-ipattern-index nil))
    (remove-hook 'after-change-major-mode-hook 'rngalt-after-change-major t)
    (setq rngalt-validation-header nil)
    (rng-set-vacuous-schema)
    (rng-auto-set-schema))
  (rng-validate-mode 1)
  (rngalt-update-validation-header-overlay)
  (rngalt-update-validation-header-buffer))

(defun rngalt-reapply-validation-header ()
  (when rngalt-validation-header
    (when (or (not rng-current-schema-file-name)
              (unless (string= rngalt-current-schema-file-name rng-current-schema-file-name)
                (lwarn 'schema-mismatch :warning
                       "XHTML validation header schema %s reapplied (replaces %s)"
                       (file-name-nondirectory rngalt-current-schema-file-name)
                       (file-name-nondirectory rng-current-schema-file-name))
                t))
      (rngalt-set-validation-header (nth 2 rngalt-validation-header)))))

;; (defun rngalt-clear-validation-header ()
;;   "Remove XML validation header from current buffer.
;; For more information see `rngalt-show-validation-header'."
;;   (interactive)
;;   (rngalt-set-validation-header nil)
;;   (rng-auto-set-schema t))

;; FIX-ME: Add edit header?

(defun rngalt-get-validation-header-buffer ()
  (let ((b (get-buffer "*XML Validation Header*")))
    (unless b
      (setq b (get-buffer-create "*XML Validation Header*"))
      (with-current-buffer b
        ;;(fundamental-mode)
        (nxml-mode)))
    b))

(defun rngalt-get-state-after (start-of-doc)
  ;; FIX-ME: better buffer name?
  (let ((statebuf (rngalt-get-validation-header-buffer)))
    (with-current-buffer statebuf
      (when rng-validate-mode (rng-validate-mode -1))
      (erase-buffer)
      (insert start-of-doc)
      ;; From rng-get-state
      (setq rng-match-state nil)
      (setq nxml-ns-state nil)
      (setq rng-open-elements nil)
      ;; From rng-match-init-buffer
      (setq rng-compile-table nil)
      (setq rng-ipattern-table nil)
      (setq rng-last-ipattern-index nil)

      (nxml-mode)
      (rng-validate-mode 1)
      (rngalt-validate)
      (let* ((state (rng-get-state))
             (cp-state (copy-tree state)))
        ;;(if (equal state cp-state) (message "(equal state cp-state)=t") (message "(equal state cp-state)=nil"))
        ;; Fix-me: is the copy-tree necessary here?
        (list
         cp-state
         (rng-locate-schema-file)
         start-of-doc)))))

(defun rngalt-show-validation-header ()
  "Show XML validation header used in current buffer.
The XML validation header is used in `nxhtml-mode' to set a state
for XML validation at the start of the buffer.

The purpose is to make it possible to use `nxml-mode' completion
in buffers where you do not actually have a full XML file. This
could for example be a buffer with PHP code or a buffer with a
blog entry.

More techhnical info: This can be used by any mode derived from
`nxml-mode'. To use it in other modes than `nxhtml-mode' replace
`rng-complete' by `rngalt-complete' in `nxml-completion-hook'."
  (interactive)
  (unless (derived-mode-p 'nxml-mode)
    (error "Buffer mode is not an nXml type major mode: %s" major-mode))
  (rngalt-update-validation-header-buffer)
  (display-buffer (rngalt-get-validation-header-buffer) t))

(defun rngalt-update-validation-header-buffer ()
  (let ((vh (nth 2 rngalt-validation-header))
        (cb (current-buffer)))
    (with-current-buffer (rngalt-get-validation-header-buffer)
      (erase-buffer)
      (if (not vh)
          (setq header-line-format (concat " No XML validation header in buffer "
                                           (buffer-name cb)))
        (insert vh)
        (setq header-line-format (concat " XML validation header in buffer "
                                         (buffer-name cb)))))))

;; <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<



(provide 'rngalt)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; rngalt.el ends here
