;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; For wid-edit.el

(defun widget-color-validate (widget)
  (let ((value (widget-value widget)))
    (unless
        ;; Drews suggestion is to use display-color-p instead, but
        ;; unfortunately this does not match #rgb:
        ;;(display-color-p value)
        (color-defined-p value)
      (widget-put widget :error (format "Invalid color: %S" value))
      widget)))

(defun widget-color-match (widget value)
  (and (stringp value)
       (color-defined-p value)))

(define-widget 'color 'string
  "Choose a color (with sample)."
  :format "%{%t%}: %v (%{sample%})\n"
  :size 30
  :tag "Color"
  :match 'widget-color-match
  :validate 'widget-color-validate
  :value "black"
  :complete 'widget-color-complete
  :sample-face-get 'widget-color-sample-face-get
  :notify 'widget-color-notify
  :action 'widget-color-action)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; For custom.el

;; Borrowed from `set-variable'. It might be useful in
;; `custom-variable-type':
(defun custom-get-type (variable)
  "Get the :type declaration used when creating custom VARIABLE."
  (and (custom-variable-p variable)
       (not (get variable 'custom-type))
       (custom-load-symbol variable))
  (get variable 'custom-type))

;; external values = those stored in the variables
;; internal values = those shown in "buffer widgets"

;; This function tests the same way `set-variable' does it.  However
;; `set-variable' also does (require 'cus-edit), shouldn't that be
;; (require 'wid-edit)?
(defun custom-type-value-p (value custom-type)
  "Return non-nil if VALUE matches CUSTOM-TYPE.
CUSTOM-TYPE could be anything that can be used for the :type
keyword in `defcustom'.

See `custom-get-type' for how to get CUSTOM-TYPE from a
custom variable."
  (require 'wid-edit) ;; for widget-convert, but why is not that in widget.el?
  (let* ((widget (widget-convert custom-type)))
    (widget-apply widget :match value)))

         ;;(match-fun (widget-get widget :match))
         ;;(valid-fun (widget-get widget :validate))
;;     (if nil ;(listp custom-type)
;;         ;; Fix-me: Some type of conversion of the list must be
;;         ;; done. And it actually looks like :validate must be
;;         ;; used. Otherwise the children here get called with the
;;         ;; :match and that does nothing for simple type. (What is
;;         ;; match actually useful for??)
;;         (progn
;;           (widget-apply widget :match value)
;;           )
;;       (let ((int-val (widget-apply widget :value-to-internal value)))
;;         (widget-put widget :value int-val)
;;         (with-current-buffer (get-buffer-create "temp dummy")
;;           (lwarn t :warning "result=%s" (widget-choice-value-create widget)))
;;         ;;(null (widget-apply widget :validate))
;;         ))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Tests

;; (custom-type-value-p "something" 'color)
;; (custom-type-value-p "#f" 'color)
;; (custom-type-value-p "#fff" 'color)
;; (custom-type-value-p "red" 'color)
;; (custom-get-type 'test-color)
;; (setq test-color "bla")
;; (setq test-color "black")

(defcustom test-color "red"
  "color test"
  :type 'color)

;; (customize-option 'test-color)
;; (custom-get-type 'test-more)
;; (custom-type-value-p "something" (custom-get-type 'test-more))
;; (custom-type-value-p "some[" (custom-get-type 'test-more))
;; (custom-type-value-p 10 (custom-get-type 'test-more))
;; (custom-type-value-p 1.0 (custom-get-type 'test-more))
(defcustom test-more "b["
  "choice and regexp text"
  :type '(choice (integer)
                 (regexp)))


;; (custom-get-type 'ks-test7)
;; (custom-type-value-p ks-test7 'key-sequence)


(defcustom ks-test7
  [(control ?a) ?2]
  ""
  :type 'key-sequence
  )

(defcustom ks-test8
  [(control ?a) ?2]
  "testing internal/external with a choice"
  :type '(choice (key-sequence)
                 (color))
  )
;; (customize-option 'ks-test8)
;; (custom-get-type 'ks-test8)
;; (setq ks-test8 [(control ?a) ?2])
;; (setq ks-test8 "reda")
;; (setq ks-test8 8)
;; (custom-type-value-p ks-test8 'key-sequence)
;; (custom-type-value-p ks-test8 (custom-get-type 'ks-test8))


;; (insert (format "%S" (symbol-plist 'ks-test8)))
;; (standard-value ([(control 97) 50])
;;                 variable-documentation "testing internal/external with a choice"
;;                 custom-type (choice (key-sequence) (color))
;;                 custom-requests nil
;;                 backup-value (8)
;;                 variable-comment nil
;;                 customized-value nil
;;                 customized-variable-comment nil)

(defun test8 ()
  (let ((widget (widget-convert (custom-get-type 'ks-test8))))
    (widget-choice-match widget ks-test8)
    (widget-choice-match widget 8)
    (widget-apply widget :match 8)
    (widget-apply widget :match ks-test8)
    ))
;; (test8)
