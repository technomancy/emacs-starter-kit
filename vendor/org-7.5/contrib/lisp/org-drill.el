;;; org-drill.el - Self-testing with org-learn
;;;
;;; Author: Paul Sexton <eeeickythump@gmail.com>
;;; Version: 1.4
;;; Repository at http://bitbucket.org/eeeickythump/org-drill/
;;;
;;;
;;; Synopsis
;;; ========
;;;
;;; Uses the spaced repetition algorithm in `org-learn' to conduct interactive
;;; "drill sessions", where the material to be remembered is presented to the
;;; student in random order. The student rates his or her recall of each item,
;;; and this information is fed back to `org-learn' to schedule the item for
;;; later revision.
;;;
;;; Each drill session can be restricted to topics in the current buffer
;;; (default), one or several files, all agenda files, or a subtree. A single
;;; topic can also be drilled.
;;;
;;; Different "card types" can be defined, which present their information to
;;; the student in different ways.
;;;
;;; See the file README.org for more detailed documentation.


(eval-when-compile (require 'cl))
(eval-when-compile (require 'hi-lock))
(require 'org)
(require 'org-learn)


(defgroup org-drill nil
  "Options concerning interactive drill sessions in Org mode (org-drill)."
  :tag "Org-Drill"
  :group 'org-link)



(defcustom org-drill-question-tag 
  "drill"
  "Tag which topics must possess in order to be identified as review topics
by `org-drill'."
  :group 'org-drill
  :type 'string)



(defcustom org-drill-maximum-items-per-session
  30
  "Each drill session will present at most this many topics for review.
Nil means unlimited."
  :group 'org-drill
  :type '(choice integer (const nil)))



(defcustom org-drill-maximum-duration
  20
  "Maximum duration of a drill session, in minutes.
Nil means unlimited."
  :group 'org-drill
  :type '(choice integer (const nil)))


(defcustom org-drill-failure-quality
  2
  "If the quality of recall for an item is this number or lower,
it is regarded as an unambiguous failure, and the repetition
interval for the card is reset to 0 days.  By default this is
2. For Mnemosyne-like behaviour, set it to 1.  Other values are
not really sensible."
  :group 'org-drill
  :type '(choice (const 2) (const 1)))


(defcustom org-drill-leech-failure-threshold
  15
  "If an item is forgotten more than this many times, it is tagged
as a 'leech' item."
  :group 'org-drill
  :type '(choice integer (const nil)))


(defcustom org-drill-leech-method
  'skip
  "How should 'leech items' be handled during drill sessions?
Possible values:
- nil :: Leech items are treated the same as normal items.
- skip :: Leech items are not included in drill sessions.
- warn :: Leech items are still included in drill sessions,
  but a warning message is printed when each leech item is
  presented."
  :group 'org-drill
  :type '(choice (const 'warn) (const 'skip) (const nil)))


(defface org-drill-visible-cloze-face
  '((t (:foreground "darkseagreen")))
  "The face used to hide the contents of cloze phrases."
  :group 'org-drill)


(defface org-drill-visible-cloze-hint-face
  '((t (:foreground "dark slate blue")))
  "The face used to hide the contents of cloze phrases."
  :group 'org-drill)


(defcustom org-drill-use-visible-cloze-face-p
  nil
  "Use a special face to highlight cloze-deleted text in org mode
buffers?"
  :group 'org-drill
  :type 'boolean)


(defface org-drill-hidden-cloze-face
  '((t (:foreground "deep sky blue" :background "blue")))
  "The face used to hide the contents of cloze phrases."
  :group 'org-drill)


(defcustom org-drill-new-count-color
  "royal blue"
  "Foreground colour used to display the count of remaining new items
during a drill session."
  :group 'org-drill
  :type 'color)

(defcustom org-drill-mature-count-color
  "green"
  "Foreground colour used to display the count of remaining mature items
during a drill session. Mature items are due for review, but are not new."
  :group 'org-drill
  :type 'color)

(defcustom org-drill-failed-count-color
  "red"
  "Foreground colour used to display the count of remaining failed items
during a drill session."
  :group 'org-drill
  :type 'color)

(defcustom org-drill-done-count-color
  "sienna"
  "Foreground colour used to display the count of reviewed items
during a drill session."
  :group 'org-drill
  :type 'color)


(setplist 'org-drill-cloze-overlay-defaults
          '(display "[...]"
                    face org-drill-hidden-cloze-face
                    window t))


(defvar org-drill-cloze-regexp
  ;; ver 1   "[^][]\\(\\[[^][][^]]*\\]\\)"
  ;; ver 2   "\\(\\[.*?\\]\\|^[^[[:cntrl:]]*?\\]\\|\\[.*?$\\)"
  ;; ver 3!  "\\(\\[.*?\\]\\|\\[.*?[[:cntrl:]]+.*?\\]\\)"
  "\\(\\[[[:cntrl:][:graph:][:space:]]*?\\)\\(\\||.+?\\)\\(\\]\\)")

(defvar org-drill-cloze-keywords
  `((,org-drill-cloze-regexp
     (1 'org-drill-visible-cloze-face nil)
     (2 'org-drill-visible-cloze-hint-face t)
     (3 'org-drill-visible-cloze-face nil)
     )))


(defcustom org-drill-card-type-alist
  '((nil . org-drill-present-simple-card)
    ("simple" . org-drill-present-simple-card)
    ("twosided" . org-drill-present-two-sided-card)
    ("multisided" . org-drill-present-multi-sided-card)
    ("multicloze" . org-drill-present-multicloze)
    ("spanish_verb" . org-drill-present-spanish-verb))
  "Alist associating card types with presentation functions. Each entry in the
alist takes the form (CARDTYPE . FUNCTION), where CARDTYPE is a string
or nil, and FUNCTION is a function which takes no arguments and returns a
boolean value."
  :group 'org-drill
  :type '(alist :key-type (choice string (const nil)) :value-type function))


(defcustom org-drill-spaced-repetition-algorithm
  'sm5
  "Which SuperMemo spaced repetition algorithm to use for scheduling items.
Available choices are SM2 and SM5."
  :group 'org-drill
  :type '(choice (const 'sm2) (const 'sm5)))

(defcustom org-drill-add-random-noise-to-intervals-p
  nil
  "If true, the number of days until an item's next repetition
will vary slightly from the interval calculated by the SM2
algorithm. The variation is very small when the interval is
small, and scales up with the interval. The code for calculating
random noise is adapted from Mnemosyne."
  :group 'org-drill
  :type 'boolean)

(defcustom org-drill-cram-hours
  12
  "When in cram mode, items are considered due for review if
they were reviewed at least this many hours ago."
  :group 'org-drill
  :type 'integer)


(defvar *org-drill-session-qualities* nil)
(defvar *org-drill-start-time* 0)
(defvar *org-drill-new-entries* nil)
(defvar *org-drill-mature-entries* nil)
(defvar *org-drill-failed-entries* nil)
(defvar *org-drill-again-entries* nil)
(defvar *org-drill-done-entries* nil)
(defvar *org-drill-cram-mode* nil
  "Are we in 'cram mode', where all items are considered due
for review unless they were already reviewed in the recent past?")



;;;; Utilities ================================================================


(defun free-marker (m)
  (set-marker m nil))


(defmacro pop-random (place)
  (let ((elt (gensym)))
    `(if (null ,place)
         nil
       (let ((,elt (nth (random (length ,place)) ,place)))
         (setq ,place (remove ,elt ,place))
         ,elt))))


(defun shuffle-list (list)
  "Randomly permute the elements of LIST (all permutations equally likely)."
  ;; Adapted from 'shuffle-vector' in cookie1.el
  (let ((i 0)
	j
	temp
	(len (length list)))
    (while (< i len)
      (setq j (+ i (random (- len i))))
      (setq temp (nth i list))
      (setf (nth i list) (nth j list))
      (setf (nth j list) temp)
      (setq i (1+ i))))
  list)
    

(defun time-to-inactive-org-timestamp (time)
  (format-time-string 
   (concat "[" (substring (cdr org-time-stamp-formats) 1 -1) "]")
   time))



(defmacro with-hidden-cloze-text (&rest body)
  `(progn
     (org-drill-hide-clozed-text)
     (unwind-protect
         (progn
           ,@body)
       (org-drill-unhide-clozed-text))))


(defun org-drill-days-since-last-review ()
  "Nil means a last review date has not yet been stored for
the item.
Zero means it was reviewed today.
A positive number means it was reviewed that many days ago.
A negative number means the date of last review is in the future --
this should never happen."
  (let ((datestr (org-entry-get (point) "DRILL_LAST_REVIEWED")))
    (when datestr
      (- (time-to-days (current-time))
         (time-to-days (apply 'encode-time
                              (org-parse-time-string datestr)))))))


(defun org-drill-hours-since-last-review ()
  "Like `org-drill-days-since-last-review', but return value is
in hours rather than days."
  (let ((datestr (org-entry-get (point) "DRILL_LAST_REVIEWED")))
    (when datestr
      (floor
       (/ (- (time-to-seconds (current-time))
             (time-to-seconds (apply 'encode-time
                                     (org-parse-time-string datestr))))
          (* 60 60))))))


(defun org-drill-entry-p ()
  "Is the current entry a 'drill item'?"
  (or (org-entry-get (point) "LEARN_DATA")
      ;;(assoc "LEARN_DATA" (org-entry-properties nil))
      (member org-drill-question-tag (org-get-local-tags))))


(defun org-part-of-drill-entry-p ()
  "Is the current entry either the main heading of a 'drill item',
or a subheading within a drill item?"
  (or (org-drill-entry-p)
      ;; Does this heading INHERIT the drill tag
      (member org-drill-question-tag (org-get-tags-at))))


(defun org-drill-goto-drill-entry-heading ()
  "Move the point to the heading which hold the :drill: tag for this
drill entry."
  (unless (org-at-heading-p)
    (org-back-to-heading))
  (unless (org-part-of-drill-entry-p)
    (error "Point is not inside a drill entry"))
  (while (not (org-drill-entry-p))
    (unless (org-up-heading-safe)
      (error "Cannot find a parent heading that is marked as a drill entry"))))



(defun org-drill-entry-leech-p ()
  "Is the current entry a 'leech item'?"
  (and (org-drill-entry-p)
       (member "leech" (org-get-local-tags))))


(defun org-drill-entry-due-p ()
  (cond
   (*org-drill-cram-mode*
    (let ((hours (org-drill-hours-since-last-review)))
      (and (org-drill-entry-p)
           (or (null hours)
               (>= hours org-drill-cram-hours)))))
   (t
    (let ((item-time (org-get-scheduled-time (point))))
      (and (org-drill-entry-p)
           (or (not (eql 'skip org-drill-leech-method))
               (not (org-drill-entry-leech-p)))
           (or (null item-time)
               (not (minusp             ; scheduled for today/in future
                     (- (time-to-days (current-time))
                        (time-to-days item-time))))))))))


(defun org-drill-entry-new-p ()
  (and (org-drill-entry-p)
       (let ((item-time (org-get-scheduled-time (point))))
         (null item-time))))



(defun org-drill-entry-last-quality ()
  (let ((quality (org-entry-get (point) "DRILL_LAST_QUALITY")))
    (if quality
        (string-to-number quality)
      nil)))


;;; SM2 Algorithm =============================================================


(defun determine-next-interval-sm2 (last-interval n ef quality of-matrix)
  "Arguments:
- LAST-INTERVAL -- the number of days since the item was last reviewed.
- N -- the number of times the item has been successfully reviewed
- EF -- the 'easiness factor'
- QUALITY -- 0 to 5
- OF-MATRIX -- a matrix of values, used by SM5 but not by SM2.

Returns a list: (INTERVAL N EF OFMATRIX), where:
- INTERVAL is the number of days until the item should next be reviewed
- N is incremented by 1.
- EF is modified based on the recall quality for the item.
- OF-MATRIX is not modified."
  (assert (> n 0))
  (assert (and (>= quality 0) (<= quality 5)))
  (if (<= quality org-drill-failure-quality)
      ;; When an item is failed, its interval is reset to 0,
      ;; but its EF is unchanged
      (list -1 1 ef of-matrix)
    ;; else:
    (let* ((next-ef (modify-e-factor ef quality))
           (interval
            (cond
             ((<= n 1) 1)
             ((= n 2)
              (cond
               (org-drill-add-random-noise-to-intervals-p
                (case quality
                  (5 6)
                  (4 4)
                  (3 3)
                  (2 1)
                  (t -1)))
               (t 6)))
             (t (ceiling (* last-interval next-ef))))))
      (list (round
             (if org-drill-add-random-noise-to-intervals-p
                 (+ last-interval (* (- interval last-interval)
                                     (org-drill-random-dispersal-factor)))
               interval))
            (1+ n) next-ef of-matrix))))


;;; SM5 Algorithm =============================================================

;;; From http://www.supermemo.com/english/ol/sm5.htm
(defun org-drill-random-dispersal-factor ()
  (let ((a 0.047)
        (b 0.092)
        (p (- (random* 1.0) 0.5)))
    (flet ((sign (n)
                 (cond ((zerop n) 0)
                       ((plusp n) 1)
                       (t -1))))
      (/ (+ 100 (* (* (/ -1 b) (log (- 1 (* (/ b a ) (abs p)))))
                   (sign p)))
         100))))
      

(defun inter-repetition-interval-sm5 (last-interval n ef &optional of-matrix)
  (let ((of (get-optimal-factor n ef of-matrix)))
    (if (= 1 n)
	of
      (* of last-interval))))


(defun determine-next-interval-sm5 (last-interval n ef quality of-matrix)
  (assert (> n 0))
  (assert (and (>= quality 0) (<= quality 5)))
  (let ((next-ef (modify-e-factor ef quality))
        (interval nil))
    (setq of-matrix
          (set-optimal-factor n next-ef of-matrix
                              (modify-of (get-optimal-factor n ef of-matrix)
                                         quality org-learn-fraction))
          ef next-ef)
    
    (cond
     ;; "Failed" -- reset repetitions to 0, 
     ((<= quality org-drill-failure-quality)
      (list -1 1 ef of-matrix))      ; Not clear if OF matrix is supposed to be
                                     ; preserved
     ;; For a zero-based quality of 4 or 5, don't repeat
     ((and (>= quality 4)
           (not org-learn-always-reschedule))
      (list 0 (1+ n) ef of-matrix))     ; 0 interval = unschedule
     (t
      (setq interval (inter-repetition-interval-sm5
                      last-interval n ef of-matrix))
      (if org-drill-add-random-noise-to-intervals-p
          (setq interval (+ last-interval
                            (* (- interval last-interval)
                               (org-drill-random-dispersal-factor)))))
      (list (round interval) (1+ n) ef of-matrix)))))


;;; Essentially copied from `org-learn.el', but modified to
;;; optionally call the SM2 function above.
(defun org-drill-smart-reschedule (quality)
  (interactive "nHow well did you remember the information (on a scale of 0-5)? ")
  (let* ((learn-str (org-entry-get (point) "LEARN_DATA"))
	 (learn-data (or (and learn-str
			      (read learn-str))
			 (copy-list initial-repetition-state)))
	 closed-dates)
    (setq learn-data
          (case org-drill-spaced-repetition-algorithm
            (sm5 (determine-next-interval-sm5 (nth 0 learn-data)
                                              (nth 1 learn-data)
                                              (nth 2 learn-data)
                                              quality
                                              (nth 3 learn-data)))
            (sm2 (determine-next-interval-sm2 (nth 0 learn-data)
                                              (nth 1 learn-data)
                                              (nth 2 learn-data)
                                              quality
                                              (nth 3 learn-data)))))
    (org-entry-put (point) "LEARN_DATA" (prin1-to-string learn-data))
    (cond
     ((= 0 (nth 0 learn-data))
      (org-schedule t))
     ((minusp (first learn-data))
      (org-schedule nil (current-time)))
     (t
      (org-schedule nil (time-add (current-time)
				  (days-to-time (nth 0 learn-data))))))))


(defun org-drill-reschedule ()
  "Returns quality rating (0-5), or nil if the user quit."
  (let ((ch nil))
    (while (not (memq ch '(?q ?e ?0 ?1 ?2 ?3 ?4 ?5)))
      (setq ch (read-char-exclusive
                (if (eq ch ??)
                    "0-2 Means you have forgotten the item.
3-5 Means you have remembered the item.
 
0 - Completely forgot. 
1 - Even after seeing the answer, it still took a bit to sink in. 
2 - After seeing the answer, you remembered it. 
3 - It took you awhile, but you finally remembered.
4 - After a little bit of thought you remembered.
5 - You remembered the item really easily.

How well did you do? (0-5, ?=help, e=edit, t=tags, q=quit)"
                  "How well did you do? (0-5, ?=help, e=edit, q=quit)")))
      (if (eql ch ?t)
          (org-set-tags-command)))
    (cond
     ((and (>= ch ?0) (<= ch ?5))
      (let ((quality (- ch ?0))
            (failures (org-entry-get (point) "DRILL_FAILURE_COUNT")))
        (save-excursion
          (org-drill-smart-reschedule quality))
        (push quality *org-drill-session-qualities*)
        (cond
         ((<= quality org-drill-failure-quality)
          (when org-drill-leech-failure-threshold
            (setq failures (if failures (string-to-number failures) 0))
            (org-set-property "DRILL_FAILURE_COUNT"
                              (format "%d" (1+ failures)))
            (if (> (1+ failures) org-drill-leech-failure-threshold)
                (org-toggle-tag "leech" 'on))))
         (t
          (let ((scheduled-time (org-get-scheduled-time (point))))
            (when scheduled-time
              (message "Next review in %d days"
                       (- (time-to-days scheduled-time)
                          (time-to-days (current-time))))
              (sit-for 0.5)))))
        (org-set-property "DRILL_LAST_QUALITY" (format "%d" quality))
        (org-set-property "DRILL_LAST_REVIEWED"
                          (time-to-inactive-org-timestamp (current-time)))
        quality))
     ((= ch ?e)
      'edit)
     (t
      nil))))


(defun org-drill-hide-all-subheadings-except (heading-list)
  "Returns a list containing the position of each immediate subheading of
the current topic."
  (let ((drill-entry-level (org-current-level))
        (drill-sections nil)
        (drill-heading nil))
    (org-show-subtree)
    (save-excursion
      (org-map-entries
       (lambda ()
         (when (= (org-current-level) (1+ drill-entry-level))
           (setq drill-heading (org-get-heading t))
           (unless (member drill-heading heading-list)
             (hide-subtree))
           (push (point) drill-sections)))
       "" 'tree))
    (reverse drill-sections)))



(defun org-drill-presentation-prompt (&rest fmt-and-args)
  (let* ((item-start-time (current-time))
         (ch nil)
         (last-second 0)
         (prompt
          (if fmt-and-args
              (apply 'format
                     (first fmt-and-args)
                     (rest fmt-and-args))
            (concat "Press key for answer, "
                    "e=edit, t=tags, s=skip, q=quit."))))
    (setq prompt
          (format "%s %s %s %s %s"
                  (propertize
                   (number-to-string (length *org-drill-done-entries*))
                   'face `(:foreground ,org-drill-done-count-color)
                   'help-echo "The number of items you have reviewed this session.")
                  (propertize
                   (number-to-string (+ (length *org-drill-again-entries*)
                                        (length *org-drill-failed-entries*)))
                   'face `(:foreground ,org-drill-failed-count-color)
                   'help-echo (concat "The number of items that you failed, "
                                      "and need to review again."))
                  (propertize
                   (number-to-string (length *org-drill-mature-entries*))
                   'face `(:foreground ,org-drill-mature-count-color)
                   'help-echo "The number of old items due for review.")
                  (propertize
                   (number-to-string (length *org-drill-new-entries*))
                   'face `(:foreground ,org-drill-new-count-color)
                   'help-echo (concat "The number of new items that you "
                                      "have never reviewed."))
                  prompt))
    (if (and (eql 'warn org-drill-leech-method)
             (org-drill-entry-leech-p))
        (setq prompt (concat
                      (propertize "!!! LEECH ITEM !!!
You seem to be having a lot of trouble memorising this item.
Consider reformulating the item to make it easier to remember.\n"
                                  'face '(:foreground "red"))
                      prompt)))
    (while (memq ch '(nil ?t))
      (while (not (input-pending-p))
        (message (concat (format-time-string
                          "%M:%S " (time-subtract
                                   (current-time) item-start-time))
                         prompt))
        (sit-for 1))
      (setq ch (read-char-exclusive))
      (if (eql ch ?t)
          (org-set-tags-command)))
    (case ch
      (?q nil)
      (?e 'edit)
      (?s 'skip)
      (otherwise t))))


(defun org-pos-in-regexp (pos regexp &optional nlines)
  (save-excursion
    (goto-char pos)
    (org-in-regexp regexp nlines)))


(defun org-drill-hide-clozed-text ()
  (save-excursion
    (while (re-search-forward org-drill-cloze-regexp nil t)
      ;; Don't hide org links, partly because they might contain inline
      ;; images which we want to keep visible
      (unless (org-pos-in-regexp (match-beginning 0)
                                 org-bracket-link-regexp 1)
        (org-drill-hide-matched-cloze-text)))))


(defun org-drill-hide-matched-cloze-text ()
  "Hide the current match with a 'cloze' visual overlay."
  (let ((ovl (make-overlay (match-beginning 0) (match-end 0))))
    (overlay-put ovl 'category
                 'org-drill-cloze-overlay-defaults)
    (when (find ?| (match-string 0))
      (overlay-put ovl
                   'display
                   (format "[...%s]"
                           (substring-no-properties
                            (match-string 0)
                            (1+ (position ?| (match-string 0)))
                            (1- (length (match-string 0)))))))))


(defun org-drill-unhide-clozed-text ()
  (save-excursion
    (dolist (ovl (overlays-in (point-min) (point-max)))
      (when (eql 'org-drill-cloze-overlay-defaults (overlay-get ovl 'category))
        (delete-overlay ovl)))))



;;; Presentation functions ====================================================

;; Each of these is called with point on topic heading.  Each needs to show the
;; topic in the form of a 'question' or with some information 'hidden', as
;; appropriate for the card type. The user should then be prompted to press a
;; key. The function should then reveal either the 'answer' or the entire
;; topic, and should return t if the user chose to see the answer and rate their
;; recall, nil if they chose to quit.

(defun org-drill-present-simple-card ()
  (with-hidden-cloze-text 
   (org-drill-hide-all-subheadings-except nil)
   (org-display-inline-images t)
   (org-cycle-hide-drawers 'all)
   (prog1 (org-drill-presentation-prompt)
     (org-show-subtree))))


(defun org-drill-present-two-sided-card ()
  (with-hidden-cloze-text 
   (let ((drill-sections (org-drill-hide-all-subheadings-except nil)))
     (when drill-sections
       (save-excursion
         (goto-char (nth (random (min 2 (length drill-sections)))
                         drill-sections))
         (org-show-subtree)))
     (org-display-inline-images t)
     (org-cycle-hide-drawers 'all)
     (prog1
         (org-drill-presentation-prompt)
       (org-show-subtree)))))



(defun org-drill-present-multi-sided-card ()
  (with-hidden-cloze-text 
   (let ((drill-sections (org-drill-hide-all-subheadings-except nil)))
     (when drill-sections
       (save-excursion
         (goto-char (nth (random (length drill-sections)) drill-sections))
         (org-show-subtree)))
     (org-display-inline-images t)    
     (org-cycle-hide-drawers 'all)
     (prog1
         (org-drill-presentation-prompt)
       (org-show-subtree)))))


(defun org-drill-present-multicloze ()
  (let ((item-end nil)
        (match-count 0)
        (body-start (or (cdr (org-get-property-block))
                        (point))))
    (org-drill-hide-all-subheadings-except nil)
    (save-excursion
      (outline-next-heading)
      (setq item-end (point)))
    (save-excursion
      (goto-char body-start)
      (while (re-search-forward org-drill-cloze-regexp item-end t)
        (incf match-count)))
    (when (plusp match-count)
      (save-excursion
        (goto-char body-start)
        (re-search-forward org-drill-cloze-regexp
                           item-end t (1+ (random match-count)))
        (org-drill-hide-matched-cloze-text)))
    (org-display-inline-images t)
    (org-cycle-hide-drawers 'all)
    (prog1 (org-drill-presentation-prompt)
      (org-show-subtree)
      (org-drill-unhide-clozed-text))))

  
(defun org-drill-present-spanish-verb ()
  (let ((prompt nil)
        (reveal-headings nil))
    (with-hidden-cloze-text 
     (case (random 6)
       (0
        (org-drill-hide-all-subheadings-except '("Infinitive"))
        (setq prompt
              (concat "Translate this Spanish verb, and conjugate it "
                      "for the *present* tense.")
              reveal-headings '("English" "Present Tense" "Notes")))
       (1
        (org-drill-hide-all-subheadings-except '("English"))
        (setq prompt (concat "For the *present* tense, conjugate the "
                             "Spanish translation of this English verb.")
              reveal-headings '("Infinitive" "Present Tense" "Notes")))
       (2
        (org-drill-hide-all-subheadings-except '("Infinitive"))
        (setq prompt (concat "Translate this Spanish verb, and "
                             "conjugate it for the *past* tense.")
              reveal-headings '("English" "Past Tense" "Notes")))
       (3
        (org-drill-hide-all-subheadings-except '("English"))
        (setq prompt (concat "For the *past* tense, conjugate the "
                             "Spanish translation of this English verb.")
              reveal-headings '("Infinitive" "Past Tense" "Notes")))
       (4
        (org-drill-hide-all-subheadings-except '("Infinitive"))
        (setq prompt (concat "Translate this Spanish verb, and "
                             "conjugate it for the *future perfect* tense.")
              reveal-headings '("English" "Future Perfect Tense" "Notes")))
       (5
        (org-drill-hide-all-subheadings-except '("English"))
        (setq prompt (concat "For the *future perfect* tense, conjugate the "
                             "Spanish translation of this English verb.")
              reveal-headings '("Infinitive" "Future Perfect Tense" "Notes"))))
     (org-cycle-hide-drawers 'all)
     (prog1
         (org-drill-presentation-prompt prompt)
       (org-drill-hide-all-subheadings-except reveal-headings)))))



(defun org-drill-entry ()
  "Present the current topic for interactive review, as in `org-drill'.
Review will occur regardless of whether the topic is due for review or whether
it meets the definition of a 'review topic' used by `org-drill'.

Returns a quality rating from 0 to 5, or nil if the user quit, or the symbol
EDIT if the user chose to exit the drill and edit the current item.

See `org-drill' for more details."
  (interactive)
  (org-drill-goto-drill-entry-heading)
  ;;(unless (org-part-of-drill-entry-p)
  ;;  (error "Point is not inside a drill entry"))
  ;;(unless (org-at-heading-p)
  ;;  (org-back-to-heading))
  (let ((card-type (org-entry-get (point) "DRILL_CARD_TYPE"))
        (cont nil))
    (save-restriction
      (org-narrow-to-subtree) 
      (org-show-subtree)
      (org-cycle-hide-drawers 'all)
      
      (let ((presentation-fn (cdr (assoc card-type org-drill-card-type-alist))))
        (cond
         (presentation-fn
          (setq cont (funcall presentation-fn)))
         (t
          (error "Unknown card type: '%s'" card-type))))
      
      (cond
       ((not cont)
        (message "Quit")
        nil)
       ((eql cont 'edit)
        'edit)
       ((eql cont 'skip)
        'skip)
       (t
        (save-excursion
          (org-drill-reschedule)))))))


;; (defun org-drill-entries (entries)
;;   "Returns nil, t, or a list of markers representing entries that were
;; 'failed' and need to be presented again before the session ends."
;;   (let ((again-entries nil))
;;     (setq *org-drill-done-entry-count* 0
;;           *org-drill-pending-entry-count* (length entries))
;;     (if (and org-drill-maximum-items-per-session
;;              (> (length entries)
;;                 org-drill-maximum-items-per-session))
;;         (setq entries (subseq entries 0
;;                               org-drill-maximum-items-per-session)))
;;     (block org-drill-entries
;;       (dolist (m entries)
;;         (save-restriction
;;           (switch-to-buffer (marker-buffer m))
;;           (goto-char (marker-position m))
;;           (setq result (org-drill-entry))
;;           (cond
;;            ((null result)
;;             (message "Quit")
;;             (return-from org-drill-entries nil))
;;            ((eql result 'edit)
;;             (setq end-pos (point-marker))
;;             (return-from org-drill-entries nil))
;;            (t
;;             (cond
;;              ((< result 3)
;;               (push m again-entries))
;;              (t
;;               (decf *org-drill-pending-entry-count*)
;;               (incf *org-drill-done-entry-count*)))
;;             (when (and org-drill-maximum-duration
;;                        (> (- (float-time (current-time)) *org-drill-start-time*)
;;                           (* org-drill-maximum-duration 60)))
;;               (message "This drill session has reached its maximum duration.")
;;               (return-from org-drill-entries nil))))))
;;       (or again-entries
;;           t))))


(defun org-drill-entries-pending-p ()
  (or *org-drill-again-entries*
      (and (not (org-drill-maximum-item-count-reached-p))
           (not (org-drill-maximum-duration-reached-p))
           (or *org-drill-new-entries*
               *org-drill-failed-entries*
               *org-drill-mature-entries*
               *org-drill-again-entries*))))


(defun org-drill-pending-entry-count ()
  (+ (length *org-drill-new-entries*)
     (length *org-drill-failed-entries*)
     (length *org-drill-mature-entries*)
     (length *org-drill-again-entries*)))


(defun org-drill-maximum-duration-reached-p ()
  "Returns true if the current drill session has continued past its
maximum duration."
  (and org-drill-maximum-duration
       *org-drill-start-time*
       (> (- (float-time (current-time)) *org-drill-start-time*)
          (* org-drill-maximum-duration 60))))


(defun org-drill-maximum-item-count-reached-p ()
  "Returns true if the current drill session has reached the
maximum number of items."
  (and org-drill-maximum-items-per-session
       (>= (length *org-drill-done-entries*)
           org-drill-maximum-items-per-session)))


(defun org-drill-pop-next-pending-entry ()
  (cond
   ;; First priority is items we failed in a prior session.
   ((and *org-drill-failed-entries*
         (not (org-drill-maximum-item-count-reached-p))
         (not (org-drill-maximum-duration-reached-p)))
    (pop-random *org-drill-failed-entries*))
   ;; Next priority is newly added items, and items which
   ;; are not new and were not failed when they were last
   ;; reviewed.
   ((and (or *org-drill-new-entries*
             *org-drill-mature-entries*)
         (not (org-drill-maximum-item-count-reached-p))
         (not (org-drill-maximum-duration-reached-p)))
    (if (< (random (+ (length *org-drill-new-entries*)
                      (length *org-drill-mature-entries*)))
           (length *org-drill-new-entries*))
        (pop-random *org-drill-new-entries*)
      ;; else
      (pop-random *org-drill-mature-entries*)))
   ;; After all the above are done, last priority is items
   ;; that were failed earlier THIS SESSION.
   (*org-drill-again-entries*
    (pop-random *org-drill-again-entries*))
   (t
    nil)))


(defun org-drill-entries ()
  "Returns nil, t, or a list of markers representing entries that were
'failed' and need to be presented again before the session ends."
  (block org-drill-entries
    (while (org-drill-entries-pending-p)
      (setq m (org-drill-pop-next-pending-entry))
      (unless m
        (error "Unexpectedly ran out of pending drill items"))
      (save-excursion
        (set-buffer (marker-buffer m))
        (goto-char m)
        (setq result (org-drill-entry))
        (cond
         ((null result)
          (message "Quit")
          (return-from org-drill-entries nil))
         ((eql result 'edit)
          (setq end-pos (point-marker))
          (return-from org-drill-entries nil))
         ((eql result 'skip)
          nil)   ; skip this item
         (t
          (cond
           ((<= result org-drill-failure-quality)
            (push m *org-drill-again-entries*))
           (t
            (push m *org-drill-done-entries*)))))))))



(defun org-drill-final-report ()
  (read-char-exclusive
   (format
    "%d items reviewed
%d items awaiting review (%s, %s, %s)
Session duration %s

Recall of reviewed items:
 Excellent (5):     %3d%%   |   Near miss (2):     %3d%%
 Good (4):          %3d%%   |   Failure (1):       %3d%%
 Hard (3):          %3d%%   |   Total failure (0): %3d%% 

Session finished. Press a key to continue..." 
    (length *org-drill-done-entries*)
    (org-drill-pending-entry-count)
    (propertize
     (format "%d failed"
             (+ (length *org-drill-failed-entries*)
                (length *org-drill-again-entries*)))
     'face `(:foreground ,org-drill-failed-count-color))
    (propertize
     (format "%d old"
             (length *org-drill-mature-entries*))
     'face `(:foreground ,org-drill-mature-count-color))
    (propertize
     (format "%d new"
             (length *org-drill-new-entries*))
     'face `(:foreground ,org-drill-new-count-color))
    (format-seconds "%h:%.2m:%.2s"
                    (- (float-time (current-time)) *org-drill-start-time*))
    (round (* 100 (count 5 *org-drill-session-qualities*))
           (max 1 (length *org-drill-session-qualities*)))
    (round (* 100 (count 2 *org-drill-session-qualities*))
           (max 1 (length *org-drill-session-qualities*)))
    (round (* 100 (count 4 *org-drill-session-qualities*))
           (max 1 (length *org-drill-session-qualities*)))
    (round (* 100 (count 1 *org-drill-session-qualities*))
           (max 1 (length *org-drill-session-qualities*)))
    (round (* 100 (count 3 *org-drill-session-qualities*))
           (max 1 (length *org-drill-session-qualities*)))
    (round (* 100 (count 0 *org-drill-session-qualities*))
           (max 1 (length *org-drill-session-qualities*)))
    )))



(defun org-drill (&optional scope)
  "Begin an interactive 'drill session'. The user is asked to
review a series of topics (headers). Each topic is initially
presented as a 'question', often with part of the topic content
hidden. The user attempts to recall the hidden information or
answer the question, then presses a key to reveal the answer. The
user then rates his or her recall or performance on that
topic. This rating information is used to reschedule the topic
for future review using the `org-learn' library.

Org-drill proceeds by:

- Finding all topics (headings) in SCOPE which have either been
  used and rescheduled by org-learn before (i.e. the LEARN_DATA
  property is set), or which have a tag that matches
  `org-drill-question-tag'.

- All matching topics which are either unscheduled, or are
  scheduled for the current date or a date in the past, are
  considered to be candidates for the drill session.

- If `org-drill-maximum-items-per-session' is set, a random
  subset of these topics is presented. Otherwise, all of the
  eligible topics will be presented.

SCOPE determines the scope in which to search for
questions.  It is passed to `org-map-entries', and can be any of:

nil     The current buffer, respecting the restriction if any.
        This is the default.
tree    The subtree started with the entry at point
file    The current buffer, without restriction
file-with-archives
        The current buffer, and any archives associated with it
agenda  All agenda files
agenda-with-archives
        All agenda files with any archive files associated with them
 (file1 file2 ...)
        If this is a list, all files in the list will be scanned."

  (interactive)
  (let ((entries nil)
        (failed-entries nil)
        (result nil)
        (results nil)
        (end-pos nil)
        (cnt 0))
    (block org-drill
      (setq *org-drill-done-entries* nil
            *org-drill-new-entries* nil
            *org-drill-mature-entries* nil
            *org-drill-failed-entries* nil
            *org-drill-again-entries* nil)
      (setq *org-drill-session-qualities* nil)
      (setq *org-drill-start-time* (float-time (current-time)))
      (unwind-protect
          (save-excursion
            (let ((org-trust-scanner-tags t))
              (org-map-entries
               (lambda ()
                 (when (zerop (% (incf cnt) 50))
                   (message "Processing drill items: %4d%s"
                            (+ (length *org-drill-new-entries*)
                               (length *org-drill-mature-entries*)
                               (length *org-drill-failed-entries*))
                            (make-string (ceiling cnt 50) ?.)))
                 (when (org-drill-entry-due-p)
                   (cond
                    ((org-drill-entry-new-p)
                     (push (point-marker) *org-drill-new-entries*))
                    ((and (org-drill-entry-last-quality)
                          (<= (org-drill-entry-last-quality)
                              org-drill-failure-quality))
                     (push (point-marker) *org-drill-failed-entries*))
                    (t
                     (push (point-marker) *org-drill-mature-entries*)))))
               (concat "+" org-drill-question-tag) scope))
            ;; Failed first, then random mix of old + new
            (setq entries (append (shuffle-list *org-drill-failed-entries*)
                                  (shuffle-list (append *org-drill-mature-entries*
                                                        *org-drill-new-entries*))))
            (cond
             ((and (null *org-drill-new-entries*)
                   (null *org-drill-failed-entries*)
                   (null *org-drill-mature-entries*))
              (message "I did not find any pending drill items."))
             (t
              (org-drill-entries)
              (message "Drill session finished!"))))
        ;; (cond
        ;; ((null entries)
        ;;  (message "I did not find any pending drill items."))
        ;; (t
        ;;  (let ((again t))
        ;;    (while again
        ;;      (when (listp again)
        ;;        (setq entries (shuffle-list again)))
        ;;      (setq again (org-drill-entries entries))
        ;;      (cond
        ;;       ((null again)
        ;;        (return-from org-drill nil))
        ;;       ((eql t again)
        ;;        (setq again nil))))
        ;;    (message "Drill session finished!")
        ;;    ))))
        (progn
          (dolist (m (append *org-drill-new-entries*
                             *org-drill-failed-entries*
                             *org-drill-again-entries*
                             *org-drill-mature-entries*))
            (free-marker m)))))
    (cond
     (end-pos
      (switch-to-buffer (marker-buffer end-pos))
      (goto-char (marker-position end-pos))
      (message "Edit topic."))
     (t
      (org-drill-final-report)))))


(defun org-drill-cram (&optional scope)
  "Run an interactive drill session in 'cram mode'. In cram mode,
all drill items are considered to be due for review, unless they
have been reviewed within the last `org-drill-cram-hours'
hours."
  (interactive)
  (let ((*org-drill-cram-mode* t))
    (org-drill scope)))



(add-hook 'org-mode-hook
          (lambda ()
            (if org-drill-use-visible-cloze-face-p
                (font-lock-add-keywords
                 'org-mode
                 org-drill-cloze-keywords
                 t))))



(provide 'org-drill)
