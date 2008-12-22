(define-widget 'my-assoc-widget 'lazy
  "wdoc"
  :offset 4
  :tag "the tag"
;;  :type '(vector string string)
  :type '(list (cons symbol string)
               (cons symbol string))
;;   :type '(vector (cons symbol string)
;;                  (cons symbol string))

  ;; :convert-widget 'widget-value-convert-widget
  :value-to-external 
  (lambda (widget value)
    (let (
          (vec (make-vector 2 0))
          )
    (dolist (v value)
      ;;(message "v=%s, car=%s, cdr=%s" v (car v)(cdr v))(sit-for 2)
      (let ((k (car v))
            )
        (cond ((eq k 'zero)
               (aset vec 0 (cdr v)))
              ((eq k 'one)
               (aset vec 1 (cdr v)))
              (t
               (error "Bad value")))
        ))
    (message "vec=%s" vec)(sit-for 2)
    vec))
;;   :value-to-internal 
;;   (lambda (widget value)
;;     (message "vti: value=%s" value)(sit-for 2)
;;     (let ((iv (list
;;                (cons 'zero (elt value 0))
;;                (cons 'one  (elt value 1)))))
;;     (message "iv=%s" iv)(sit-for 2)
;;       iv))
  )

;; (eval-buffer)  
;; (customize-option 'my-cust)  
(defcustom my-cust
  '((zero . "init0") (one . "init1"))
  ;;(list (cons 'zero "init0") (cons 'one "init1"))
  ;;["init0" "init1"]
  "doc"
  :type 'my-assoc-widget)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-widget 'binary-tree-of-string 'lazy
  "A binary tree made of cons-cells and strings."
  :offset 4
  :tag "Node"
  :type '(choice (string :tag "Leaf" :value "")
                 (cons :tag "Interior"
                       :value ("" . "")
                       binary-tree-of-string
                       binary-tree-of-string)))

(defcustom foo-bar ""
  "Sample variable holding a binary tree of strings."
  :type 'binary-tree-of-string)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defcustom my-cust4 ["" ""]
  "doc"
  :type '(vector string string)
  )
(defcustom my-cust5 '("aa" "bbb")
  "doc"
  :type '(list string string)
  )
