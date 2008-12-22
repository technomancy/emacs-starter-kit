
(defun color-digits-p (color)
  (save-match-data
    (string-match (rx bos
                      "#"
                      (1+ (repeat 3 3 hex-digit))
                      eos)
                  color)))

(defun widget-color-validate (widget)
  (let ((value (widget-value widget)))
    (unless (color-defined-p value)
      (widget-put widget :error (format "Invalid color: %S" value))
      widget)))

(define-widget 'color 'editable-field
  "Choose a color (with sample)."
  :format "%{%t%}: %v (%{sample%})\n"
  :size 30
  :tag "Color"
  :validate 'widget-color-validate
  :value "black"
  :complete 'widget-color-complete
  :sample-face-get 'widget-color-sample-face-get
  :notify 'widget-color-notify
  :action 'widget-color-action)

(defun custom-type-is-super (super-type base-type)
  (unless (get super-type 'widget-type) (error "Not a widget type: %s" super-type))
  (unless (get base-type 'widget-type) (error "Not a widget type: %s" base-type))
  (let ((found nil)
        (type super-type))
    (while (and (not found) type)
      (setq found (eq type base-type))
      (unless found
        (setq type (car (get type 'widget-type)))))
    found))
;; (custom-type-simple-is-super 'color 'string)
;; (custom-type-simple-is-super 'color 'editable-field)

(defun custom-type-symbol-p (symbol custom-type)
  "Return t if value of SYMBOL should fit CUSTOM-TYPE."
  (let ((found nil)
        (type (get symbol 'custom-type)))
    ;; Fix-me: How should different things that are not directly
    ;; value-related be handled?
    (while (and (not found) type)
      (setq found (equal type custom-type))
      (unless nil ;found
        (if (symbolp type)
            (setq found (custom-type-simple-is-super type custom-type))
          (unless (symbolp custom-type)
            (let ((symbol-comp-type (car type))
                  (symbol-childs (cdr type))
                  (custom-comp-type (car custom-type))
                  (custom-childs (cdr custom-type)))
              (setq found
                    (and (eq symbol-comp-type custom-comp-type)
                         (let ((ok t))
                           (while (and ok symbol-childs custom-childs)
                             (setq ok (custom-type-symbol-p (caar symbol-childs) (caar custom-childs)))
                             (setq symbol-childs (cdr symbol-childs))
                             (setq custom-childs (cdr custom-childs)))
                           ok)))
            )))))
    found))

;; (custom-type-p 'test-color 'color)
;; (custom-type-symbol-p 'bar (get 'bar 'custom-type))
;; (custom-type-value-p bar (get 'bar 'custom-type))
;; (custom-type-value-p "b" (get 'bar 'custom-type))
;; (custom-type-value-p 10 (get 'bar 'custom-type))
;; (custom-type-value-p 'b (get 'bar 'custom-type))

;;(insert (format "%S" (symbol-plist 'bar)))
;;(standard-value ("b") variable-documentation "yyyyyyy" custom-type (choice (integer :tag "WWWWWWWWW") (string :tag "ZZZZZZZZZZZZZ")) custom-requests nil)

;; (custom-get-type 'bar)
;; (defvar bar-var 'bar)
;; (custom-get-type bar-var)
;; (custom-get-type 'bar-var)
;; (custom-variable-p bar)
;; (custom-variable-type 'bar)
;; (custom-variable-type bar-var)
;; (custom-variable-type 'bar-var)

;; Borrowed from `set-variable'. It might be useful in
;; `custom-variable-type':
(defun custom-get-type (variable)
  "Get the :type declaration from custom VARIABLE."
  (and (custom-variable-p variable)
       (not (get variable 'custom-type))
       (custom-load-symbol variable))
  (get variable 'custom-type))

;; This function tests the same way as `set-variable' does and could
;; be used there. However `set-variable' does (require 'cus-edit),
;; shouldn't that be (require 'wid-edit)?
(defun custom-type-value-p (value custom-type)
  "Return non-nil if VALUE matches CUSTOM-TYPE.
CUSTOM-TYPE could be anything that can be used for the :type
keyword in `defcustom'.

See `custom-get-type' for how to get CUSTOM-TYPE form a
custom variable."
  (require 'wid-edit)
  (let ((widget (widget-convert custom-type)))
    (widget-apply widget :match value)))

(defun custom-type-p (val-or-sym custom-type)
  "Return non-nil if VAL-OR-SYM fits CUSTOM-TYPE.
VAL-OR-SYM may be either a variable or a symbol. If it is a
variable then return non-nil if the value fits custom type
CUSTOM-TYPE.

If it is a symbol then return non-nil if the values this symbol's
variable can have fits CUSTOM-TYPE."
  (if (symbolp val-or-sym)
      (custom-type-symbol-p val-or-sym custom-type)
    (custom-type-value-p val-or-sym custom-type)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Tests

;; (custom-type-p 'test-color 'color)
;; (custom-type-p 'test-color 'edit)
;; (custom-type-p 'test-color 'editable-field)
;; (custom-type-p test-color 'color)
;; (get 'test-color 'custom-type)
;; (setq test-color "bla")
;; (setq test-color "black")

(defcustom test-color "black"
  "color test"
  :type 'color)

(defun max-color-length()
  (let ((len 0)
        (longest ""))
    (mapc (lambda (color)
            (when (< len (length color))
              (setq len (length color))
              (setq longest color)))
          x-colors)
    (cons len longest)))
;; (max-color-length)

(defcustom bar "b[" "yyyyyyy"
  :type '(choice (integer :tag "WWWWWWWWW")
                 (regexp :tag "ZZZZZZZZZZZZZ")))

;; (custom-type-symbol-p 'bar (get 'bar 'custom-type))


