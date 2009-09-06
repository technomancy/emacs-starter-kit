;; 3. Add a new c-indentation-style:

(defconst drupal
  '((c-basic-offset . 2)
    (c-offsets-alist . ((arglist-close . c-lineup-close-paren)
                        (case-label . +)
                        (arglist-intro . +)
                        (arglist-cont-nonempty . c-lineup-math))))
  "My Drupal Programming style")

(c-add-style "drupal" drupal)

;; 4. Open file test.php, attached.

;; 5. Run `c-set-style' and select "drupal"

;; 6. Select the whole buffer and press "C-M-\" (or any other indentation command,
;;    for that matter) and watch as the array elements are lined up with "array(",
;;    whereas they should be indented by 2.

;; 7. Run M-x php-mode and c-set-style to drupal

;; 8. Try indenting again to see that indentation now works properly.
