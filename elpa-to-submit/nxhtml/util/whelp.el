;; This is a test file for some enhancement to the possibilities to
;; find out about widgets or buttons at point in a buffer.
;;
;; To use this just load the file. Then put point on a widget or
;; button and do
;;
;;    M-x describe-field
;;
;; You find a lot of widgets in a Custom buffer. You can find buttons
;; in for example a help buffer. (Please tell me more places so I can
;; test!)
;;
;; TODO: Add backtrace collecting to some more functions!

;; For widget-get-backtrace-info
;;(require 'debug)
(eval-when-compile (require 'cl))  ;; gensym
(require 'help-mode)

;; Last wins!
(require 'wid-browse)

(intern ":created-in-function")

(define-widget 'widget-browse-link 'item
  "Button for creating a link style button.
The :value of the widget shuld be the widget to be browsed."
  :format "%[%v%]"
  ;;:value-create 'widget-browse-value-create
  ;;:action 'widget-browse-action
  )

(defun define-button-type (name &rest properties)
  "Define a `button type' called NAME.
The remaining arguments form a sequence of PROPERTY VALUE pairs,
specifying properties to use as defaults for buttons with this type
\(a button's type may be set by giving it a `type' property when
creating the button, using the :type keyword argument).

In addition, the keyword argument :supertype may be used to specify a
button-type from which NAME inherits its default property values
\(however, the inheritance happens only when NAME is defined; subsequent
changes to a supertype are not reflected in its subtypes)."
  (let ((catsym (make-symbol (concat (symbol-name name) "-button")))
	(super-catsym
	 (button-category-symbol
	  (or (plist-get properties 'supertype)
	      (plist-get properties :supertype)
	      'button))))
    ;; Provide a link so that it's easy to find the real symbol.
    (put name 'button-category-symbol catsym)
    ;; Initialize NAME's properties using the global defaults.
    (let ((default-props (symbol-plist super-catsym))
          (where-fun (widget-get-backtrace-info 8)))
      (setq default-props
            (cons :created-in-function
                  (cons where-fun
                        default-props)))
      (while default-props
	(put catsym (pop default-props) (pop default-props))))
    ;; Add NAME as the `type' property, which will then be returned as
    ;; the type property of individual buttons.
    (put catsym 'type name)
    ;; Add the properties in PROPERTIES to the real symbol.
    (while properties
      (let ((prop (pop properties)))
	(when (eq prop :supertype)
	  (setq prop 'supertype))
	(put catsym prop (pop properties))))
    ;; Make sure there's a `supertype' property
    (unless (get catsym 'supertype)
      (put catsym 'supertype 'button))
    name))

(defun define-widget (name class doc &rest args)
  "Define a new widget type named NAME from CLASS.

NAME and CLASS should both be symbols, CLASS should be one of the
existing widget types, or nil to create the widget from scratch.

After the new widget has been defined, the following two calls will
create identical widgets:

* (widget-create NAME)

* (apply 'widget-create CLASS ARGS)

The third argument DOC is a documentation string for the widget."
  (put name 'widget-type (cons class args))
  (put name 'widget-documentation doc)
  (put name :created-in-function (widget-get-backtrace-info  8))
  name)

(defvar describe-temp-help-buffer nil)
(defun describe-get-temp-help-buffer ()
  (setq describe-temp-help-buffer (get-buffer-create "*Copy of *Help* Buffer for Description*")))

(defun describe-field (pos)
  "Describe field at marker POS."
  (interactive (list (point)))
  (unless (markerp pos) (setq pos (copy-marker pos)))
  (when (eq (marker-buffer pos) (get-buffer (help-buffer)))
    (with-current-buffer (describe-get-temp-help-buffer)
      (erase-buffer)
      (insert (with-current-buffer (help-buffer)
                (buffer-string)))
      (goto-char (marker-position pos))
      (setq pos (point-marker))))
  (let (field wbutton doc button widget)
    (with-current-buffer (marker-buffer pos)
      (setq field (get-char-property pos 'field))
      (setq wbutton (get-char-property pos 'button))
      (setq doc (get-char-property pos 'widget-doc))
      (setq button (button-at pos))
      (setq widget (or field wbutton doc)))
    (cond ((and widget
                (if (symbolp widget)
                    (get widget 'widget-type)
                  (and (consp widget)
                       (get (widget-type widget) 'widget-type))))
           (describe-widget pos))
          (button
           (describe-button pos))
          ((and (eq major-mode 'Info-mode)
                (memq (get-text-property pos 'font-lock-face)
                      '(info-xref info-xref-visited)))
           (message "info link"))
          (t
           (message "No widget or button at point")))))

(defun describe-insert-header (pos)
  (widget-insert
   (add-string-property
    (concat
     (format "Description of the field at position %d in "
             (marker-position pos))
     (format "\"%s\"" (marker-buffer pos))
     ":\n\n")
    'face '(italic))))

(defun describe-widget (pos)
  ;;(interactive (list (point-marker)))
  (unless (markerp pos) (setq pos (copy-marker pos)))
  (with-output-to-temp-buffer (help-buffer)
    (help-setup-xref (list #'describe-widget pos) (interactive-p))
    (with-current-buffer (help-buffer)
      (let ((inhibit-read-only t))
        (describe-insert-header pos)
        (insert-text-button "This field"
                            'action (lambda (button)
                                      (let* ((m (button-get button 'field-location))
                                             (p (marker-position m))
                                             (b (marker-buffer m)))
                                        (if (not (buffer-live-p b))
                                            (message "Sorry the markers buffer is gone")
                                          (switch-to-buffer b)
                                          (goto-char p))))
                            'field-location pos)
        (princ " is of type ")
        (insert-text-button "widget"
                            'action (lambda (button)
                                      (info "(widget)")))
        (princ ". You can ")
        (insert-text-button "browse the widget's properties"
                            'action (lambda (button)
                                      (widget-browse-at
                                       (button-get button 'field-location)))
                            'field-location pos))
      (princ " to find out more about it.")
      (fill-region (point-min) (point-max))
      )
    (print-help-return-message)))

(defun describe-button (pos)
  (let ((button (button-at pos)))
    (with-output-to-temp-buffer (help-buffer)
      (help-setup-xref (list #'describe-button pos) (interactive-p))
      (with-current-buffer (help-buffer)
        (let ((inhibit-read-only t)
              (button-marker (gensym)))
          (describe-insert-header pos)
          (insert-text-button "This field"
                              'action (lambda (button)
                                        (let* ((m (button-get button 'field-location))
                                               (p (marker-position m))
                                               (b (marker-buffer m)))
                                          (switch-to-buffer b)
                                          (goto-char p)))
                              'field-location pos)
          (princ " is of type ")
          (insert-text-button "button"
                              'action (lambda (button)
                                        (info "(elisp) Buttons")))
          (princ ". You can ")
          (set button-marker pos)
          (insert-text-button "browse the button's properties"
                              'action `(lambda (button)
                                         (button-browse-at (symbol-value ',button-marker)))))
        (princ " to find out more about it.")
        (fill-region (point-min) (point-max))
        )
      (print-help-return-message))))

;; Obsolete
;; (defun whelp-describe-symbol (sym)
;;   (interactive "SSymbol: ")
;;   (with-output-to-temp-buffer (help-buffer)
;;     (help-setup-xref (list #'describe-symbol sym) (interactive-p))
;;     (with-current-buffer (help-buffer)
;;       (let ((inhibit-read-only t))
;;         (if (not (symbolp sym))
;;             (progn
;;               (princ "Argument does not look like it is a ")
;;               (insert-text-button "symbol"
;;                                   'action (lambda (button)
;;                                             (info "(elisp) Symbols")))
;;               (princ "."))
;;           (let ((n 0))
;;             (when (fboundp sym)        (setq n (1+ n)))
;;             (when (boundp sym)         (setq n (1+ n)))
;;             (when (facep sym)          (setq n (1+ n)))
;;             (when (custom-group-p sym) (setq n (1+ n)))
;;             (if (= n 0)
;;                 (progn
;;                   (princ "Can't determine usage for the ")
;;                   (insert-text-button "symbol"
;;                                       'action (lambda (button)
;;                                                 (info "(elisp) Symbols")))
;;                   (princ " '")
;;                   (princ (symbol-name sym))
;;                   (princ "."))
;;               (princ "The ")
;;               (insert-text-button "symbol"
;;                                   'action (lambda (button)
;;                                             (info "(elisp) Symbols")))
;;               (princ " '")
;;               (princ (symbol-name sym))
;;               (if (= n 1)
;;                   (progn
;;                     (princ " is a ")
;;                     (cond ((fboundp sym)
;;                            (princ "function (")
;;                            (insert-text-button
;;                             "describe it"
;;                             'action (lambda (button)
;;                                       (let ((value (button-get button 'value)))
;;                                         (describe-function value)))
;;                             'value sym)
;;                            (insert ")"))
;;                           ((boundp sym)
;;                            (insert "variable (")
;;                            (insert-text-button
;;                             "describe it"
;;                             'action (lambda (button)
;;                                       (let ((value (button-get button 'value)))
;;                                         (describe-variable value)))
;;                             'value sym)
;;                            (insert ")"))
;;                           ((facep sym)
;;                            (insert "face (")
;;                            (insert-text-button
;;                             "describe it"
;;                             'action (lambda (button)
;;                                       (let ((value (button-get button 'value)))
;;                                         (describe-face value)))
;;                             'value sym)
;;                            (insert ")"))
;;                           ((custom-group-p sym)
;;                            (insert "customize group (")
;;                            (insert-text-button
;;                             "customize it"
;;                             'action (lambda (button)
;;                                       (let ((value (button-get button 'value)))
;;                                         (customize-group value)))
;;                             'value sym)
;;                            (insert ")")))
;;                     (princ "."))
;;                 (princ " has several usages currently.")
;;                 (princ " It can be:\n\n")
;;                 (when (fboundp sym)
;;                   (princ "  - A function (")
;;                   (insert-text-button "describe it"
;;                                       'action (lambda (button)
;;                                                 (let ((value (button-get button 'value)))
;;                                                   (describe-function value)))
;;                                       'value sym)
;;                   (princ ")\n"))
;;                 (when (boundp sym)
;;                   (princ "  - A variable (")
;;                   (insert-text-button "describe it"
;;                                       'action (lambda (button)
;;                                                     (let ((value (button-get button 'value)))
;;                                                       (describe-variable value)))
;;                                       'value sym)
;;                   (princ ")\n"))
;;                 (when (facep sym)
;;                   (princ "  - A face (")
;;                   (insert-text-button "describe it"
;;                                       'action (lambda (button)
;;                                                 (let ((value (button-get button 'value)))
;;                                                   (describe-face value)))
;;                                       'value sym)
;;                   (princ ")\n"))
;;                 (when (custom-group-p sym)
;;                   (princ "  - A customization group (")
;;                   (insert-text-button "customize it"
;;                                       'action (lambda (button)
;;                                                 (let ((value (button-get button 'value)))
;;                                                   (customize-group value)))
;;                                       'value sym)
;;                   (princ ")\n"))
;;                 )))
;;           (princ "\n\nSymbol's property list:\n\n")
;;           (let ((pl (symbol-plist sym))
;;                 key
;;                 val)
;;             (princ (format "  %25s   %s\n" "Key" "Value"))
;;             (princ (format "  %25s   %s\n" "---" "-----"))
;;             (while pl
;;               (setq key (car pl))
;;               (setq pl (cdr pl))
;;                 (setq val (car pl))
;;                 (setq pl (cdr pl))
;;                 (let ((first (point-marker))
;;                       last)
;;                   (princ (format "  %25s - %s" key val))
;;                   (setq last (point-marker))
;;                   (let ((adaptive-fill-function
;;                          (lambda ()
;;                            (format "  %25s - " key))))
;;                     (fill-region first last)
;;                     ))
;;                 (princ "\n")
;;                 )))
;;         (print-help-return-message)))))



(defun widget-browse-sexp (widget key value)
  "Insert description of WIDGET's KEY VALUE.
Nothing is assumed about value."
  (let ((pp (condition-case signal
		(pp-to-string value)
	      (error (prin1-to-string signal)))))
    (when (string-match "\n\\'" pp)
      (setq pp (substring pp 0 (1- (length pp)))))
    (if (cond ((string-match "\n" pp)
	       nil)
	      ((> (length pp) (- (window-width) (current-column)))
	       nil)
	      (t t))
        (cond
         (  (and value
                 (symbolp value)
                 (or (fboundp value)
                     (boundp value)
                     (facep value)))
            (widget-create 'push-button
                           :tag pp
                           :value value
                           :action '(lambda (widget &optional event)
                                      (let ((value (widget-get widget :value))
                                            (n 0))
                                        (when (fboundp value) (setq n (1+ n)))
                                        (when (boundp value) (setq n (1+ n)))
                                        (when (facep value) (setq n (1+ n)))
                                        (if (= n 1)
                                            (cond ((fboundp value)
                                                   (describe-function value))
                                                  ((boundp value)
                                                   (describe-variable value))
                                                  ((facep value)
                                                   (describe-face value)))
                                          (describe-symbol value))))))
         (  (markerp value)
            (widget-create 'push-button
                           :tag pp
                           :value (list (marker-position value) (marker-buffer value))
                           :action '(lambda (widget &optional event)
                                      (let ((value (widget-get widget :value)))
                                        (let ((pos (car value))
                                              (buf (cadr value)))
                                          (switch-to-buffer-other-window buf)
                                          (goto-char pos))))))
         (  (overlayp value)
            (widget-create 'push-button
                           :tag pp
                           :value (list (overlay-start value) (overlay-buffer value))
                           :action '(lambda (widget &optional event)
                                      (let ((value (widget-get widget :value)))
                                        (let ((pos (car value))
                                              (buf (cadr value)))
                                          (switch-to-buffer-other-window buf)
                                          (goto-char pos))))))
         (  t
            (widget-insert pp)))

      (widget-create 'push-button
		     :tag "show"
		     :action (lambda (widget &optional event)
			       (with-output-to-temp-buffer
				   "*Pp Eval Output*"
				 (princ (widget-get widget :value))))
		     pp))))


(defvar widget-get-backtrace-active t
  "Whether to collect backtrace info for widgets and buttons.
Turn this on only for debugging purposes.

Note: This must be t when Emacs is loading to collect the needed
information.")

(defun widget-get-backtrace-info (n)
  (if widget-get-backtrace-active
      (let ((frame-n t)
            fun)
        (while (and frame-n
                    (not fun))
          (setq frame-n (backtrace-frame n))
          (when frame-n
            ;;(message "**BT %s:  %s" n (cadr frame-n))
            (when (car frame-n)
              (setq fun (cadr frame-n))
              (when (or (listp fun)
                        (member fun
                                '(
                                  backtrace-frame
                                  widget-get-backtrace-info

                                  eval
                                  eval-expression
                                  call-interactively
                                  apply
                                  funcall
                                  ;;lambda

                                  if
                                  when
                                  cond
                                  condition
                                  mapc
                                  mapcar
                                  while

                                  let
                                  let*
                                  set
                                  setq
                                  set-variable
                                  set-default

                                  widget-create
                                  widget-create-child-and-convert
                                  widget-create-child
                                  widget-create-child-value
                                  define-button-type
                                  define-widget
                                  make-text-button
                                  insert-text-button
                                  make-button
                                  insert-button
                                  )))
                (setq fun)))
            (setq n (1+ n))))
        ;;(message "---------- fun=%s" fun)
        fun)
    "Set widget-get-backtrace-info to show this"))

(defun widget-create (type &rest args)
  "Create widget of TYPE.
The optional ARGS are additional keyword arguments."
  (unless (keywordp :created-in-function) (error ":wcw not interned"))
  (let ((where-fun (widget-get-backtrace-info 8))
        yargs)
    (setq args
          (cons :created-in-function
                (cons where-fun
                      args)))
    (let ((widget (apply 'widget-convert type args)))
      (widget-apply widget :create)
      widget)))


(defun widget-create-child-and-convert (parent type &rest args)
  "As part of the widget PARENT, create a child widget TYPE.
The child is converted, using the keyword arguments ARGS."
  (let ((widget (apply 'widget-convert type args)))
    (widget-put widget :parent parent)
    (widget-put widget :created-in-function (widget-get-backtrace-info  15))
    (unless (widget-get widget :indent)
      (widget-put widget :indent (+ (or (widget-get parent :indent) 0)
				    (or (widget-get widget :extra-offset) 0)
				    (widget-get parent :offset))))
    (widget-apply widget :create)
    widget))

(defun widget-create-child (parent type)
  "Create widget of TYPE."
  (let ((widget (widget-copy type)))
    (widget-put widget :parent parent)
    (widget-put widget :created-in-function (widget-get-backtrace-info  15))
    (unless (widget-get widget :indent)
      (widget-put widget :indent (+ (or (widget-get parent :indent) 0)
				    (or (widget-get widget :extra-offset) 0)
				    (widget-get parent :offset))))
    (widget-apply widget :create)
    widget))

(defun widget-create-child-value (parent type value)
  "Create widget of TYPE with value VALUE."
  (let ((widget (widget-copy type)))
    (widget-put widget :value (widget-apply widget :value-to-internal value))
    (widget-put widget :parent parent)
    (widget-put widget :created-in-function (widget-get-backtrace-info  15))
    (unless (widget-get widget :indent)
      (widget-put widget :indent (+ (or (widget-get parent :indent) 0)
				    (or (widget-get widget :extra-offset) 0)
				    (widget-get parent :offset))))
    (widget-apply widget :create)
    widget))

(defvar widget-browse-fb-history nil
  "Forward/backward history.")
(setq widget-browse-fb-history nil)

(defun widget-fb-button-action (widget &ignore)
  (let* ((num (widget-get widget :history-number))
         (rec (nth num widget-browse-fb-history))
         (fun (nth 0 rec))
         (val (nth 1 rec))
         (loc (nth 2 rec)))
    ;;(message "fun=%s, val=%s, loc=%s" fun val loc)(sit-for 4)
    (funcall fun num)))

(defun widget-insert-fb-buttons (current-number)
  ;;(message "current-number=%s" current-number)(sit-for 2)
  (if (<= 0 (1- current-number))
      (widget-create 'push-button
                     :action 'widget-fb-button-action
                     :history-number (1- current-number)
                     :format "%[%v%]"
                     "back")
    (widget-insert (add-string-property "[back]"
                                        'face 'shadow)))
  (widget-insert " ")
  (if (< (1+ current-number) (length widget-browse-fb-history))
      (widget-create 'push-button
                     :action 'widget-fb-button-action
                     :history-number (1+ current-number)
                     :format "%[%v%]"
                     "forward")
    (widget-insert (add-string-property "[forward]"
                                        'face 'shadow)))
  (widget-insert "\n"))

(defun widget-add-fb-history (elt)
  (let ((last (car widget-browse-fb-history)))
    (unless (equal elt last)
      (setq widget-browse-fb-history
            (reverse (cons elt
                           (reverse widget-browse-fb-history)))))))

(defun widget-browse (widget &optional location)
  "Create a widget browser for WIDGET."
  (interactive (list (completing-read "Widget: "
				      obarray
				      (lambda (symbol)
					(get symbol 'widget-type))
				      t nil 'widget-browse-history)))
  (let (history-number)
    (if (integerp widget)
        (progn
          ;;(message "was integer=%s" widget)(sit-for 2)
          (setq history-number widget)
          (setq widget (nth 1 (nth widget widget-browse-fb-history))))
      ;;(message "was NOT integer=%s" widget)(sit-for 2)
      (widget-add-fb-history (list 'widget-browse widget location))
      (setq history-number (1- (length widget-browse-fb-history))))
    ;;(message "history-number=%s" history-number)(sit-for 2)

    (if (stringp widget)
        (setq widget (intern widget)))
    (unless (if (symbolp widget)
                (get widget 'widget-type)
              (and (consp widget)
                   (get (widget-type widget) 'widget-type)))
      (error "Not a widget"))

    ;; Create the buffer.
    (if (symbolp widget)
        (let ((buffer (format "*Browse %s Widget*" widget)))
          (kill-buffer (get-buffer-create buffer))
          (switch-to-buffer (get-buffer-create buffer)))
      (kill-buffer (get-buffer-create "*Browse Widget*"))
      (switch-to-buffer (get-buffer-create "*Browse Widget*")))
    (widget-browse-mode)

    (make-local-variable 'widget-button-face)
    (setq widget-button-face 'link)
    (set (make-local-variable 'widget-push-button-prefix) "")
    (set (make-local-variable 'widget-push-button-suffix) "")
    (set (make-local-variable 'widget-link-prefix) "")
    (set (make-local-variable 'widget-link-suffix) "")

    ;; Top text indicating whether it is a class or object browser.
    (widget-insert-fb-buttons history-number)
    (widget-insert "----------------\n")
    (if (listp widget)
        (progn
          (widget-insert (add-string-property
                          "Widget object browser"
                          'face 'widget-browse-h1))
          (widget-insert "\n\n")
          (when location
            (let ((b (marker-buffer location))
                  (p (marker-position location)))
              (widget-insert (add-string-property "Location: "
                                                  'face 'italic))
              (widget-create 'push-button
                             :tag (format "position %s in buffer %s" p b)
                             :value (list p b)
                             :action '(lambda (widget &optional event)
                                        (let ((value (widget-get widget :value)))
                                          (let ((pos (car value))
                                                (buf (cadr value)))
                                            (switch-to-buffer-other-window buf)
                                            (goto-char pos)))))
              (widget-insert "\n\n")))
          (widget-insert (add-string-property "Class: "
                                              'face 'italic)))
      (widget-insert (add-string-property "Widget class browser"
                                          'face 'widget-browse-h1))
      (widget-insert ".\n\n")
      (widget-insert (add-string-property "Class: " 'face 'italic))
      (widget-insert (add-string-property (format "%s\n" widget)
                                          'face '(bold)))
      (widget-insert (format "%s" (get widget 'widget-documentation)))
      (unless (eq (preceding-char) ?\n) (widget-insert "\n"))
      (widget-insert (add-string-property "\nSuper: " 'face 'italic))
      (setq widget (get widget 'widget-type))
      )

    ;(widget-insert (format "%s\n" widget))

    ;; Now show the attributes.
    (let ((name (car widget))
          (items (cdr widget))
          key value printer)
      (if (not name)
          (widget-insert "none\n")
        (let ((ancestors (list name))
              a
              (i1 7)
              i
              )
          (setq i i1)
          (while name
            (setq a (intern-soft name))
            (if a
                (progn
                  (setq a (get a 'widget-type))
                  (setq name (car a))
                  (when (intern-soft name)
                    (push name ancestors)))
              (setq name)))
          ;;(widget-insert (format "ancestors=%s\n" ancestors))
          (mapc (lambda (w)
                  (widget-insert (make-string (if (= i i1) 0 i) ? ))
                  (widget-create 'widget-browse
                                 :format "%[%v%]"
                                 w)
                  (widget-insert "\n")
                  (setq i (+ i 2)))
                ancestors)))
      (while items
        (setq key (nth 0 items)
              value (nth 1 items)
              printer (or (get key 'widget-keyword-printer)
                          'widget-browse-sexp)
              items (cdr (cdr items)))
        (widget-insert "\n"
                       (add-string-property (symbol-name key)
                                            'face 'italic))
        (when (widget-browse-explained key)
          (widget-insert " (")
          (widget-create
           ;;'push-button
           ;;:tag "explain"
           ;;:format "%[%v%]"
           ;;:button-prefix ""
           ;;:button-suffix ""
           'widget-browse-link
           :value key
           :tag "explain"
           :format "%[%t%]"
           :action '(lambda (widget &optional event)
                      (widget-browse-explain
                       ;;(widget-get widget :value)
                       (widget-value widget)
                       ))
           )
          (widget-insert ")"))
        (widget-insert "\n\t")
        (funcall printer widget key value)
        (widget-insert "\n")))

    (widget-insert "\n-----------\n")
    (widget-insert-fb-buttons history-number)

    (widget-setup)
    (goto-char (point-min))
;;     (when wid-to-history
;;       (setq widget-browse-fb-history
;;             (reverse (cons (list 'widget-browse wid-to-history location)
;;                            (reverse widget-browse-fb-history)))))
    ))

(defun widget-browse-at (pos)
  "Browse the widget under point."
  (interactive "d")
  (let ((mp pos)
        (b (if (markerp pos) (marker-buffer pos)
             (current-buffer))))
    (if (not (buffer-live-p b))
        (message "Sorry the markers buffer is gone")
      (with-current-buffer b
        (when (markerp pos)
          (setq pos (marker-position pos)))
        (let* ((field (get-char-property pos 'field))
               (button (get-char-property pos 'button))
               (doc (get-char-property pos 'widget-doc))
               (text (cond (field "This is an editable text area.")
                           (button "This is an active area.")
                           (doc "This is documentation text.")
                           (t "This is unidentified text.")))
               (widget (or field button doc)))
          (when widget
            (widget-browse widget mp))
          (message text))))))

(defun button-at (pos)
  "Return the button at marker or position POS, or nil.
If not a marker use the current buffer."
  (with-current-buffer (if (markerp pos) (marker-buffer pos)
                         (current-buffer))
    (when (markerp pos)
      (setq pos (marker-position pos)))
    (let ((button (get-char-property pos 'button)))
      (if (or (overlayp button) (null button))
          button
        ;; Must be a text-property button; return a marker pointing to it.
        (copy-marker pos t)))))

(defun button-browse-at (pos)
  (interactive "d")
  (let ((b (if (markerp pos) (marker-buffer pos)
             (current-buffer))))
    (if (not (buffer-live-p b))
        (message "Sorry the button's buffer is gone")
      (button-browse (button-at pos)))))

(defun button-browse (button)
  "Create a widget browser for WIDGET."
  (interactive (list (completing-read "Button: "
				      obarray
				      (lambda (symbol)
                                        (or (get symbol 'button-category-symbol)
                                            (get symbol 'supertype)))
				      t nil 'button-browse-history)))
  (let (history-number)
    (if (integerp button)
        (progn
          (setq history-number button)
          (setq button (nth 1 (nth button widget-browse-fb-history))))
      (widget-add-fb-history (list 'button-browse button))
      (setq history-number (1- (length widget-browse-fb-history))))

    (when (stringp button)
      (setq button (intern-soft button)))
    (when (symbolp button)
      (unless (and button
                   (or (eq button 'default-button)
                       (get button 'supertype)
                       (get button 'button-category-symbol)
                       (save-match-data
                         (string-match "-button$" (symbol-name button)))))
        (error "Not a button")))
    ;; Create the buffer.
    (kill-buffer (get-buffer-create "*Browse Button*"))
    (switch-to-buffer (get-buffer-create "*Browse Button*"))
    (widget-browse-mode)

    (make-local-variable 'widget-button-face)
    (setq widget-button-face 'link)

    (widget-insert-fb-buttons history-number)
    (widget-insert "----------------\n")

    ;; Top text indicating whether it is a class or object browser.
    (if (or (overlayp button)
            (markerp button))
        (progn
          (widget-insert (add-string-property "Button object browser"
                                              'face 'widget-browse-h1))
          (widget-insert "\n\n")
          (let ((b (if (markerp button)
                       (marker-buffer button)
                     (overlay-buffer button)))
                (p (if (markerp button)
                       (marker-position button)
                     (overlay-start button))))
            (widget-insert (add-string-property "Location: "
                                                'face 'italic))
            (widget-create 'push-button
                           :tag (format "position %s in buffer %s" p b)
                           :value (list p b)
                           :action '(lambda (widget &optional event)
                                      (let ((value (widget-get widget :value)))
                                        (let ((pos (car value))
                                              (buf (cadr value)))
                                          (switch-to-buffer-other-window buf)
                                          (goto-char pos)))))
            (widget-insert "\n\n")))
      (widget-insert (add-string-property "Button class browser"
                                          'face 'widget-browse-h1))
      (widget-insert "\n\n")
      (widget-insert (add-string-property "Type: "
                                          'face 'italic))
      (widget-insert (add-string-property (symbol-name button)
                                          'face 'bold))
      (widget-insert "\n"))

    ;; Now show the attributes.
    (let (
          (items
           (if (symbolp button)
               (if (get button 'button-category-symbol)
                   (symbol-plist (get button 'button-category-symbol))
                 (symbol-plist button))
             (if (markerp button)
                 (let ((pos (marker-position button))
                       (buf (marker-buffer button)))
                   (text-properties-at pos buf))
               (overlay-properties button))))
          rest-items
          name
          key value printer)
      ;;(insert (format "\n%s\n\n" items))
      (let ((copied-items (copy-seq items)))
        (while copied-items
          (setq key   (nth 0 copied-items)
                value (nth 1 copied-items)
                copied-items (cdr (cdr copied-items)))
          (if (eq key 'category)
              (setq name value)
            (if (eq key 'supertype)
                (setq name (make-symbol (concat (symbol-name value) "-button")))
              (push value rest-items)
              (push key   rest-items)))))
      ;;(insert "\nname=" (symbol-name value) "\n\n")
      (when name
        (widget-insert (add-string-property
                        (if (symbolp button)
                            (if (get button 'supertype)
                                "Supertype: "
                              "")
                          "Category:  ")
                        'face 'italic))
        (let* (a
               (ancestors
                (list name))
               (i1 11)
               (i i1))
          (while name
            (setq a (or (get name 'supertype)
                        (get name :supertype)))
            ;;(message "name=%s, a=%s\n    name plist=%s" name a (symbol-plist name));(sit-for 4)
            (if (or (not a)
                    (eq a 'default-button))
                (setq name)
              (setq name (make-symbol (concat (symbol-name a) "-button")))
              (setq ancestors (cons name ancestors))))
          ;;(message "ancestors=%s" ancestors)(sit-for 2)
          (mapc (lambda (w)
                  (widget-insert (make-string (if (= i i1) 0 i) ? ))
                  (widget-create 'button-browse
                                 :format "%[%v%]"
                                 w)
                  (widget-insert "\n")
                  (setq i (+ i 2)))
                ancestors)))
      (while rest-items
        (setq key   (nth 0 rest-items)
              value (nth 1 rest-items)
              printer (or (get key 'widget-keyword-printer)
                          'widget-browse-sexp)
              rest-items (cdr (cdr rest-items)))
        (widget-insert "\n"
                       (add-string-property (symbol-name key)
                                            'face 'italic))
        (when (widget-browse-explained key)
          (widget-insert " (")
          (widget-create 'push-button
                         :tag "explain"
                         :value key
                         :action '(lambda (widget &optional event)
                                    (widget-browse-explain
                                     (widget-get widget :value))))
          (widget-insert ")"))
        (widget-insert "\n\t")
        (funcall printer button key value)
        (widget-insert "\n")))
    (widget-setup)
    (goto-char (point-min))

;;     (when button-to-history
;;       (setq widget-browse-fb-history
;;             (reverse (cons (list 'button-browse button-to-history)
;;                            (reverse widget-browse-fb-history)))))
    ))


(defgroup whelp nil
  "Customization group for whelp."
  :group 'emacs)

(defface widget-browse-h1
  '((t (:weight bold :height 1.5)))
  "Face for top header in widget/button browse buffers."
  :group 'whelp)

(defun add-string-property (str prop val)
  (let ((s (copy-seq str)))
    (put-text-property 0 (length s)
                       prop val
                       s)
    s))

;;; The `button-browse' Widget.

(define-widget 'button-browse 'push-button
  "Widget button for creating a button browser.
The :value of the widget shuld be the button to be browsed."
  :format "%[[%v]%]"
  :value-create 'widget-browse-button-value-create
  :action 'widget-browse-button-action)

(defun widget-browse-button-action (widget &optional event)
  ;; Create widget browser for WIDGET's :value.
  (button-browse (widget-get widget :value)))

(defun widget-browse-button-value-create (widget)
  ;; Insert type name.
  (let ((value (widget-get widget :value)))
    (cond ((symbolp value)
	   (insert (symbol-name value)))
	  ((consp value)
	   (insert (symbol-name (widget-type value))))
	  (t
	   (insert "strange")))))


(defun widget-browse-explained (property)
  (memq property
        '(
          :created-in-function
        )))

(defun widget-browse-explain (property)
  (with-output-to-temp-buffer (help-buffer)
    (help-setup-xref (list #'widget-browse-explain property) (interactive-p))
    (with-current-buffer (help-buffer)
      (let ((inhibit-read-only t))
        (cond
         ( (eq property :created-in-function)
           (princ "Property :created-in-function tells where a field object or class is created.")
           )
         ( t
           (princ (format "No explanation found for %s" property))
           )
         )
        (print-help-return-message)))))

(provide 'whelp)
