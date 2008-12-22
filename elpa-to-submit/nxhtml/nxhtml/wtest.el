(require 'wid-edit)
(require 'help-mode)

(defun test-widget-formats ()
  (interactive)
  (let ((widget-button-prefix "<<<<")
        (widget-button-suffix ""))
    (with-output-to-temp-buffer (help-buffer)
      (with-current-buffer (help-buffer)
        (set (make-local-variable 'widget-push-button-prefix) "")
        (set (make-local-variable 'widget-push-button-suffix) "")
        (set (make-local-variable 'widget-link-prefix) "")
        (set (make-local-variable 'widget-link-suffix) "")

        (widget-create 'push-button
                       :action '(lambda (w &optional e) (message "1"))
                       :button-face 'emacsw32-link-face
                       :button-prefix ""
                       :button-suffix ""
                       "One"
                       )
        (widget-create 'push-button
                       :action '(lambda (w &optional e) (message "2"))
                       :format "%[%v%]"
                       :value "two"
                       )
        (widget-create 'push-button
                       :action '(lambda (w &optional e) (message "3"))
                       :format "%[%v%]"
                       :button-prefix ""
                       :button-suffix ""
                       "three"
                       )
        (widget-create 'push-button
                       :action '(lambda (w &optional e) (message "4"))
                       :format "%v"
                       :button-prefix ""
                       :button-suffix ""
                       "four"
                       )
        (widget-create 'push-button
                       :action '(lambda (w &optional e) (message "5"))
                       :format "%{%v%}"
                       :button-prefix ""
                       :button-suffix ""
                       "five"
                       )
        (widget-create 'push-button
                       :action '(lambda (w &optional e) (message "6"))
                       ;;:format "%[[%v]%]"
                       :value "Six"
                       ;;:value-create 'widget-browse-value-create
                       :value-create (lambda (widget) (insert (widget-get widget :value)))
                       )
        (widget-setup)
        ))))
