;;; chart.el --- Google charts (and maybe other)
;;
;; Author: Lennart Borgman (lennart O borgman A gmail O com)
;; Created: 2008-04-06T15:09:33+0200 Sun
;; Version:
;; Last-Updated:
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
;;
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
;; published by the Free Software Foundation; either version 2, or
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

(eval-when-compile (require 'cl))

(defconst chart-types
  '((line-chart-x  lc)
    (line-chart-xy lxy)
    (line-chart    ls)

    (bar-chart-horizontal         bhs)
    (bar-chart-vertical           bvs)
    (bar-chart-horizontal-grouped bhg)
    (bar-chart-vertical-grouped   bvg)

    (pie-2-dimensional p)
    (pie-3-dimensional p3)

    (venn-diagram v)
    (scatter-plot s)

    (radar-chart           r)
    (radar-chart-w-splines rs)

    (geographical-map t)
    (meter gom)))

(defconst chart-types-keywords
  (mapcar (lambda (rec)
            (symbol-name (car rec)))
          chart-types))

(defun chart-create (provider out-file size data type
                              title legends &optional extras)
  "Create a chart image.
PROVIDER is what to use for creating the chart. Currently only
`google' for Google's chart API is supported.

OUT-FILE is where the image goes.

SIZE is a cons cell with pixel width and height.

DATA is the data to draw the chart from. It is a list of data
sets where each data set has the form:

  (list (list NUMBERS ...) (MIN . MAX)))

TYPE can be the following:

* Line charts

  - lc: Line chart with only y values. Each dataset is a new
    line.

  - lxy: Line chart with both x and y values. For each line there
    should be a pair of datasets, the first for x and the second
    for y. If the x dataset just contains a single -1 then values
    are evenly spaced along the x-axis.

  - ls: Like above, but axis are not drawn.

* Bar charts:

  - bhs: horizontal bars.
  - bvs: vertical bars.
  - bhg, bvg: dito grouped.

* Pie charts:

  - cht=p: one dimensional
  - cht=p3: three dimensional

* Venn diagrams

  - cht=v: data should be specified as
    * the first three values specify the relative sizes of three
      circles, A, B, and C
    * the fourth value specifies the area of A intersecting B
    * the fifth value specifies the area of A intersecting C
    * the sixth value specifies the area of B intersecting C
    * the seventh value specifies the area of A intersecting B
      intersecting C

* Scatter plots

  - cht=s: Supply a pair of datasets, first for x and second for
    y coordinates.

* Radar charts

  - cht=r: straight lines.
  - cht=rs: splines.

    You will have to find out the format of the datasets
    yourself, I don't understand it ;-)

    Or perhaps mail google?

* Maps

  - cht=t

  together with

  - chtm=AREA: AREA for provider `google' is currently one of
    *  africa
    * asia
    * europe
    * middle_east
    * south_america
    * usa
    * world

* Meter

  - cht=gom: A speed meter type meter. Takes a single value.

TITLE is a string to use as title.

LEGENDS is a list of labels to put on the data.

EXTRAS is a list of extra arguments with the form

  (EXTRA-TYPE EXTRA-VALUE)

Where EXTRA-TYPE is the extra argument type and EXTRA-VALUE the
value. The following EXTRA-TYPEs are supported:

* COLORS: value is a list of colors corresponding to the list of
  DATA. Each color have the format RRGGBB or RRGGBBTT where the
  first form is the normal way to specify colors in rgb-format
  and the second has an additional TT for transparence. TT=00
  means completely transparent and TT=FF means completely opaque.

FILL-AREA are fill colors for data sets in line charts. It should
be a list

  (list COLOR START-INDEX END-INDEX)

"
  (interactive)
  (unless (symbolp type)
    (error "Argument TYPE should be a symbol"))
  (unless (assoc type chart-types)
    (error "Unknown chart type: %s" type))
  (cond
   ((eq provider 'google)
    (let* ((g-type (nth 1 (assoc type chart-types)))
           (width  (car size))
           (height (cdr size))
           ;;(size-par (format "&chs=%sx%s" width height))
           ;;
           numbers
           scales
           colors-par
           ;;
           url
           content
           )
      (setq url
            (format
             "http://chart.apis.google.com/chart?cht=%s&chs=%dx%d" g-type width height))
      ;;(setq url (concat url size-par))
      ;; Data and scales
      (unless data
        (error "No data"))
      (dolist (rec data)
        (let* ((rec-numbers (car rec))
               (number-str
                (let (str)
                  (dolist (num rec-numbers)
                    (setq str
                          (if (not str)
                              (number-to-string num)
                            (concat str "," (number-to-string num)))))
                  str))
               (rec-scale (cdr rec))
               (rec-min  (car rec-scale))
               (rec-max  (cdr rec-scale))
               (scale-str (when rec-scale (format "%s,%s" rec-min rec-max)))
               )
          (if (not numbers)
            (progn
              (setq numbers (concat "&chd=t:" number-str))
              (when scale-str
                (setq scales (concat "&chds=" scale-str))))
          (setq numbers (concat numbers "|" number-str)))
          (when scale-str
            (setq scales (concat scales "," scale-str)))))
      (setq url (concat url numbers))
      (when scales (setq url (concat url scales)))
      ;; fix-me: encode the url
      (when title (setq url (concat url "&chtt=" (url-hexify-string title))))
      (when legends
        (let ((url-legends (mapconcat 'url-hexify-string legends "|"))
              (arg (if (memq g-type '(p p3 gom))
                       "&chl="
                     "&chdl=")))
        (setq url (concat url arg url-legends))))
      (dolist (extra extras)
        (let ((extra-type (car extra))
              (extra-value (cdr extra)))
          (cond
           ((eq extra-type 'GOOGLE-RAW)
            (setq url (concat url extra-value)))
           ((eq extra-type 'colors)
            ;; Colors
            (dolist (color extra-value)
              (if (not colors-par)
                  (setq colors-par (concat "&chco=" color))
                (setq colors-par (concat colors-par "," color))))
            (when colors-par (setq url (concat url colors-par))))
           (t (error "Unsupported extra type: %s" extra-type)))))

      ;;(lwarn t :warning "url=%s" url)(top-level)
      ;;(setq url (concat url "&chxt=y"))
      (message "Sending %s" url)
      (setq content
            (save-excursion
              (set-buffer (url-retrieve-synchronously url))
              (goto-char (point-min))
              (if (search-forward "\n\n" nil t)
                  (buffer-substring-no-properties (point) (point-max))
                (view-buffer-other-window (current-buffer))
                (error "Bad content"))))
      (let* ((fname (expand-file-name out-file))
             (do-it (or (not (file-exists-p fname))
                        (y-or-n-p
                         (concat "File " fname " exists. Replace it? "))))
             (buf (find-buffer-visiting fname))
             (this-window (selected-window)))
        (when do-it
          (when buf (kill-buffer buf))
          (with-temp-file fname
            (insert content))
          (view-file-other-window fname)
          (select-window this-window)))))
   (t (error "Unknown provider: %s" provider)))
  )

(defvar chart-mode-keywords
  '("Output-file:"
    ;;"Provider:"
    "Size:" "Data:" "Type:")
  )

(defvar chart-extra-keywords
  '(
    "Colors:"
    "Solid-fill:"
    "Linear-gradient:"
    "Linear-stripes:"
    "Chart-title:"
    "Legends:"
    "Axis-types:"
    "Axis-labels:"
    "Axis-ranges:"
    "Axis-styles:"
    "Bar-thickness:"
    "Bar-chart-zero-line:"
    "Bar-chart-zero-line-2:"
    "Line-styles-1:"
    "Line-styles-2:"
    "Grid-lines:"
    "Shape-markers:"
    "Range-markers:"
    ))

(defvar chart-raw-keywords
  '(
    "Google-chart-raw:"
    ))

(defvar chart-mode-keywords-re (regexp-opt chart-mode-keywords))
(defvar chart-extra-keywords-re (regexp-opt chart-extra-keywords))
(defvar chart-types-keywords-re (regexp-opt chart-types-keywords))
(defvar chart-raw-keywords-re (regexp-opt chart-raw-keywords))

(defvar chart-font-lock-keywords
  `((,chart-mode-keywords-re . font-lock-keyword-face)
    (,chart-extra-keywords-re . font-lock-variable-name-face)
    (,chart-types-keywords-re . font-lock-function-name-face)
    (,chart-raw-keywords-re . font-lock-preprocessor-face)
    ))

(defvar chart-font-lock-defaults
  '(chart-font-lock-keywords nil t))

(defvar chart-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?\n ">   " table)
    (modify-syntax-entry ?\; "<   " table)
    table))

(defvar chart-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [(meta tab)] 'chart-complete)
    (define-key map [(control ?c) (control ?c)] 'chart-make-chart)
    map))

(defun chart-missing-keywords ()
  (let ((collection (copy-sequence chart-mode-keywords)))
    (save-excursion
      (save-restriction
        (widen)
        (goto-char (point-min))
        (while (re-search-forward chart-mode-keywords-re nil t)
          (setq collection
                (delete (match-string-no-properties 0)
                        collection)))))
    collection))

(defun chart-complete ()
  (interactive)
  (let* ((here (point))
         (partial (when (looking-back (rx word-start
                                          (optional ?\")
                                          (0+ (any "[a-z]"))))
                    (match-string-no-properties 0)))
         (part-pos (if partial
                       (match-beginning 0)
                     (setq partial "")
                     (point)))
         (state (catch 'pos-state (chart-get-state (point))))
         (msg "No completions")
         collection
         all
         prompt
         res)
    (when state
      (cond
       ((equal state '(accept number))
        (setq res nil)
        (setq msg (propertize "Needs a number here!"
                              'face 'secondary-selection)))
       ((equal state 'need-label)
        (setq collection (append (chart-missing-keywords)
                                 chart-extra-keywords
                                 chart-raw-keywords
                                 nil))
        (setq prompt "Label: "))
       ((equal state '(accept chart-type))
        (setq collection chart-types-keywords)
        (setq prompt "Chart type: "))
       ((equal state '(accept file-name))
        (setq res
              (concat "\"" (read-file-name "Output-file: "
                                           nil
                                           ;; fix-me: handle partial
                                           partial)
                      "\""))))
      (when collection
        (let ((all (if partial
                       (all-completions partial collection)
                     collection)))
          (setq res (when all
                      (if (= (length all) 1)
                          (car all)
                        (completing-read prompt collection nil t partial)))))))
    (if (not res)
        (message "%s" msg)
      (insert (substring res (length partial))))))


(defun chart-get-state (want-pos-state)
  (let* (par-output-file
         par-provider
         par-size
         par-data par-data-temp
         par-type
         par-title
         par-legends
         par-google-raw
         (here (point))
         token-before-pos
         pos-state
         (state 'need-label)
         (problems
         (catch 'problems
             (save-restriction
               (widen)
               (if want-pos-state
                   (unless (re-search-backward chart-mode-keywords-re nil t)
                     (goto-char (point-min)))
                 (goto-char (point-min)))
               (let (this-keyword
                     this-start
                     this-end
                     params
                     token
                     token-pos
                     next-token
                     found-labels
                     current-label)
                 (while (or token
                            (progn
                              (setq pos-state state)
                              (setq token-before-pos (point))
                              (condition-case err
                                  (setq token (read (current-buffer)))
                                (error
                                 (if (eq (car err) 'end-of-file)
                                     (unless (eq state 'need-label)
                                       (throw 'problems (format "Unexpected end, state=%s" state)))
                                   (throw 'problems
                                          (error-message-string err)))))))
                   (message "token=%s, label=%s, state=%s" token current-label state)
                   (when (and want-pos-state
                              (>= (point) want-pos-state))
                     (when (= (point) want-pos-state)
                       ;; right after item
                       (setq pos-state nil))
                     (goto-char here)
                     (throw 'pos-state pos-state))
                   (cond ;; state
                    ;; Label
                    ((eq state 'need-label)
                      (unless (symbolp token)
                       (throw 'problems (format "Expected label, got %s" token)))
                      (unless (member (symbol-name token)
                                      (append chart-mode-keywords
                                              chart-extra-keywords
                                              chart-raw-keywords
                                              nil))
                        (throw 'problems (format "Unknown label %s" token)))
                      (when (member (symbol-name token) found-labels)
                        (throw 'problems (format "Label %s defined twice" token)))
                      (setq current-label token)
                      (setq found-labels (cons current-label found-labels))
                      (setq token nil)
                      ;;(setq state 'need-value)
                      (case current-label
                        ('Output-file:
                         (setq state '(accept file-name)))
                        ('Size:
                         (setq state '(accept number)))
                        ('Data:
                         (setq state '(accept number)))
                        ('Type:
                         (setq state '(accept chart-type)))
                        ('Chart-title:
                         (setq state '(accept string)))
                        ('Legends:
                         (setq state '(accept string)))
                        ('Google-chart-raw:
                         (setq state '(accept string)))
                        ))
                    ;;;; Values
                    ;; Alt
                    ((equal state '(accept '| symbol))
                     (if (eq '| token)
                         (case current-label
                           ('Legends:
                            (setq token nil)
                            (setq state '(accept string)))
                           (t (error "internal error, current-label=%s, state=%s" current-label state)))
                       (if (symbolp token)
                           (progn
                             ;;(setq token nil)
                             (setq state 'need-label))
                         (throw 'problems (format "Expected | or label, got %s" token)))))
                    ;; Strings
                    ((equal state '(accept string))
                     (unless (stringp token)
                       (throw 'problems "Expected string"))
                     (case current-label
                       ('Chart-title:
                        (setq par-title token)
                        (setq token nil)
                        (setq state 'need-label))
                       ('Legends:
                        (setq par-legends (reverse (cons token par-legends)))
                        (setq token nil)
                        (setq state '(accept '| symbol)))
                       ('Google-chart-raw:
                        (setq par-google-raw token)
                        (setq token nil)
                        (setq state '(accept '| symbol)))
                       (t (error "internal error, current-label=%s, state=%s" current-label state))))
                    ;; Output file
                    ((equal state '(accept file-name))
                     (unless (stringp token)
                       (throw 'problems "Expected file name string"))
                     (assert (eq current-label 'Output-file:))
                     (setq par-output-file token)
                     (setq token nil)
                     (setq state 'need-label))
                    ;; Numbers
                    ((equal state '(accept number))
                     (unless (numberp token)
                       (throw 'problems "Expected number"))
                     (case current-label
                       ('Size:
                        (if (not par-size)
                            (progn
                              (setq par-size token)
                              (setq token nil)
                              (setq state '(accept number 'x 'X)))
                          (setq par-size (cons par-size token))
                          (setq token nil)
                          (setq state 'need-label)))
                       ('Data:
                        ;;(assert (not par-data-temp))
                        (setq par-data-temp (cons token par-data-temp))
                        (setq token nil)
                        (setq state '(accept number ', '| symbol))
                        )
                       (t (error "internal error, state=%s, current-label=%s" state current-label)))
                     )
                    ;; Numbers or |
                    ((equal state '(accept number ', '| symbol))
                     (if (numberp token)
                         (progn
                           (setq par-data-temp (cons token par-data-temp))
                           (setq token nil))
                       (if (eq ', token)
                           (setq token nil)
                         (if (or (eq '| token)
                                 (symbolp token))
                             (progn
                               (unless par-data-temp
                                 (throw 'problems "Empty data set"))
                               (setq par-data (cons (list (reverse par-data-temp)) par-data))
                               (setq par-data-temp nil)
                               (if (not (eq '| token))
                                   (setq state 'need-label)
                                 (setq state '(accept number))
                                 (setq token nil)))
                           (throw 'problems "Expected | or EOF")
                           ))))
                    ;; Numbers or x/X
                    ((equal state '(accept number 'x 'X))
                     (assert (eq current-label 'Size:))
                     (let ((is-n (numberp token))
                           (is-x (memq token '(x X))))
                       (unless (or is-n is-x)
                         (throw 'problems "Expected X or number"))
                       (if is-x
                           (progn
                             (setq token nil)
                             (setq state '(accept number)))
                         (setq par-size (cons par-size token))
                         (setq token nil)
                         (setq state 'need-label))))
                    ;; Chart type
                    ((equal state '(accept chart-type))
                     (setq par-type token)
                     (unless (assoc par-type chart-types)
                       (throw 'problems (format "Unknown chart type: %s" par-type)))
                     (setq token nil)
                     (setq state 'need-label))
                    (t (error "internal error, state=%s" state))))))
           ;; fix-me here

           nil)))
    (when want-pos-state
      (goto-char here)
      (throw 'pos-state state))
    (unless problems
      (let ((missing-lab (chart-missing-keywords)))
        (when missing-lab
          (setq problems (format "Missing required labels: %s" missing-lab)))))
    (if problems
        (let ((msg   (if (listp problems)
                         (nth 1 problems)
                       problems))
              (where (if (listp problems)
                         (nth 0 problems)
                       token-before-pos)))
          (goto-char where)
          (message msg))
      (goto-char here)
      ;;(defun chart-create (out-file provider size data type &rest extras)
      (setq par-provider 'google)
      (let ((extras nil))
        (when par-google-raw
          (setq extras (cons (cons 'GOOGLE-RAW par-google-raw) extras)))
        (chart-create par-provider par-output-file par-size
                      par-data par-type par-title par-legends extras))
      nil)))

(defun chart-make-chart ()
  (interactive)
  (chart-get-state nil))

(define-derived-mode chart-mode fundamental-mode "Chart"
  "Mode for specifying charts."
  (set (make-local-variable 'font-lock-defaults) chart-font-lock-defaults)
  (set (make-local-variable 'comment-start) ";")
  ;; Look within the line for a ; following an even number of backslashes
  ;; after either a non-backslash or the line beginning.
  (set (make-local-variable 'comment-start-skip)
       "\\(\\(^\\|[^\\\\\n]\\)\\(\\\\\\\\\\)*\\);+ *")
  ;; Font lock mode uses this only when it KNOWS a comment is starting.
  (set (make-local-variable 'font-lock-comment-start-skip) ";+ *")
  (set (make-local-variable 'comment-add) 1) ;default to `;;' in comment-region
  (set (make-local-variable 'comment-column) 40)
  ;; Don't get confused by `;' in doc strings when paragraph-filling.
  (set (make-local-variable 'comment-use-global-state) t)
  (set-syntax-table chart-mode-syntax-table)
  (when (looking-at (rx buffer-start (0+ whitespace) buffer-end))
    (insert ";; Type C-c C-c to make a chart\n"))
  (let ((missing (chart-missing-keywords)))
    (when missing
      (save-excursion
        (goto-char (point-max))
        (dolist (miss missing)
          (insert "\n\n" miss))))))

;; Tests
;;(chart-create 'google "temp.png" '(200 . 150) '(((90 70) . nil)) 'p3 '(colors "FFFFFF" "00FF00"))

(add-to-list 'auto-mode-alist '("\\.mx-chart\\'" . chart-mode))

(provide 'chart)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; chart.el ends here
