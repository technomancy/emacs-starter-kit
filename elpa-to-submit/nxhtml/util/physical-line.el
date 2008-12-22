;; $Id: physical-line.el,v 1.11 2004/04/16 02:29:04 komatsu Exp $
;;
;; physical-line.el            by Hiroyuki Komatsu <komatsu@taiyaki.org>
;;
;; Don't you get annoyed with Emacs cursor skipping lines?
;;
;; Physical-line-mode.el provides a minor mode to move a cursor
;; across "physical lines", which are actual lines displayed on your
;; Emacs screen. Usually Emacs takes a line as "logical" one, which
;; is defined as a string between two newline characters. So if you
;; type C-p (or C-n) on a sentence which stretches for several lines,
;; it seems to skip the part you really want to move
;; to. Physical-line-mode.el makes your Emacs recognize physical
;; lines correctly in the following commands: previous-line,
;; next-line, beginning-of-line, end-of-line. It will make your
;; Emacs-life much more comfortable!
;;
;; Physical-line-mode.el comes with ABSOLUTELY NO WARRANTY.
;; This software is distributed under the GNU General Public License.
;;
;; = How To Use =
;; M-x load-file [physical-line.el]
;; M-x physical-line-mode
;;
; TODO
;  物理行表示の復活.
;
; BUGS
;  dired-mode とは相性が悪い.

;; ============================================================
;; Mell (2002-05-18)
;; ============================================================

(or (boundp 'running-xemacs)
    (setq running-xemacs nil))

(defun mell-set-minor-mode (name modeline &optional key-map)
  (make-variable-buffer-local name)
  (setq minor-mode-alist
	(mell-alist-add minor-mode-alist (list name modeline)))
  (and key-map
       (setq minor-mode-map-alist
	     (mell-alist-add minor-mode-map-alist (cons name key-map)))
       )
  )

(or (fboundp 'add-local-hook)
    (defun add-local-hook (hook function &optional append)
      (make-local-hook hook)
      (add-hook hook function append t))
    )

(or (fboundp 'remove-local-hook)
    (defun remove-local-hook (hook function)
      (if (local-variable-p hook (current-buffer))
	  (remove-hook hook function t)))
    )

(defun mell-alist-add! (alist new-cons)
  (if (null alist)
      (error "mell-alist-add! can not deal nil as an alist.")
    (let ((current-cons (assoc (car new-cons) alist)))
      (if current-cons
	  (setcdr current-cons (cdr new-cons))
	(if (car alist)
	    (nconc alist (list new-cons))
	  (setcar alist new-cons))
	)
      alist)))

(defun mell-alist-add (alist new-cons)
  (if (null alist)
      (list new-cons)
    (let ((return-alist (copy-alist alist)))
      (mell-alist-add! return-alist new-cons)
      return-alist)))

(defun mell-transient-region-stay ()
  (and running-xemacs
       (setq zmacs-region-stays t))
  )
;;;; ------------------------------------------------------------
;(defvar physical-line-mode-map (make-sparse-keymap))
;(substitute-key-definition 'beginning-of-line 'physical-line-beginning-of-line
;			   physical-line-mode-map global-map)
;(substitute-key-definition 'end-of-line 'physical-line-end-of-line
;			   physical-line-mode-map global-map)

(defvar physical-line nil
  "*Non-nil means move cursor to same column of frame line.
-1 means move cursor to same column of logical line")

(defvar physical-line-mode nil
  "*Non-nil means move cursor to same column of frame line buffer-locally.")
(mell-set-minor-mode 'physical-line-mode " PL" nil)
;(mell-set-minor-mode 'physical-line-mode " PL" physical-line-mode-map)

(defcustom physical-line-keep-goal-column-command-list
  '(previous-line next-line
    physical-line-previous-line physical-line-next-line
    physical-line-previous-logical-line physical-line-next-logical-line
    )
  "Keep tempororay-goal-column as physical line goal column
after the function in the list")

(defvar physical-line-ignoring-mode-list '(dired-mode)
  "physical-line-mode does not work under a mode in the list.")

(defvar physical-line-mode-exception '(dired-mode)
  "physical-line-move-without-exception except modes of this list.")

(defun physical-line-init (&optional arg)
  (if (> (prefix-numeric-value arg) 0)
      (progn
	(ad-enable-advice 'line-move 'around 'physical-line-move)
	(ad-enable-advice 'beginning-of-line
			  'around 'physical-line-beginning-of-line)
	(ad-enable-advice 'end-of-line
			  'around 'physical-line-end-of-line)
	)
    (ad-disable-advice 'line-move 'around 'physical-line-move)
    (ad-disable-advice 'beginning-of-line
		       'around 'physical-line-beginning-of-line)
    (ad-disable-advice 'end-of-line
		       'around 'physical-line-end-of-line)
    )
  (ad-activate 'line-move)
  (ad-activate 'beginning-of-line)
  (ad-activate 'end-of-line)
  )

(defun physical-line-on ()
  (interactive)
  (setq physical-line t)
  (physical-line-init)
  )

(defun physical-line-off ()
  (interactive)
  (setq physical-line nil)
  )

(defun physical-line-mode (&optional arg)
  "Toggle Physical Line mode.
With ARG, turn Physical Line mode on iff ARG is positive.
When Physical Line mode is enabled, cursor moving style is changed
from Logical to Physical."
  (interactive "P")
  (setq physical-line-mode (if (null arg)
                              (not physical-line-mode)
                            (> (prefix-numeric-value arg) 0)))
  )

(defun physical-line-mode-on ()
  "Turn on Physical Line mode."
  (interactive)
  (physical-line-mode t))

(defun physical-line-mode-off ()
  "Turn off Physical Line mode."
  (interactive)
  (physical-line-mode -1))

(defun physical-line-mode-without-exception ()
  "Turn on Physical Line mode without modes which are members of physical-line-move-exceptionthe."
  (physical-line-mode
   (if (memq major-mode physical-line-mode-exception) -1 t)
   ))

(defadvice line-move (around physical-line-move disable)
  "Move cursor to same column of frame line down ARG lines."
  (if (not (physical-line-active-p))
      ;; physical-line が有効でなければ何もしない
      ad-do-it
    (or (physical-line-keep-goal-column-p)
	 ;; temporary-goal-column は pline-mode 以前の line-move と共用
	(setq temporary-goal-column (physical-current-column)))
    (physical-line-line-move-function (ad-get-arg 0))
    ))

(defun physical-line-line-move-function (arg)
  (let ((moved (physical-line-the-vertical-motion arg)))
    (cond
     ((> moved arg)
      (signal 'beginning-of-buffer nil))
     ((< moved arg)
      (goto-char (point-max))
      (signal 'end-of-buffer nil))
     ))
  (physical-line-move-to-column temporary-goal-column)
  )

(defadvice beginning-of-line (around physical-line-beginning-of-line disable)
  (if (or (eq last-command this-command)
	  (null (physical-line-active-p))
	  (null (interactive-p))
	  (/=  (prefix-numeric-value (ad-get-arg 0)) 1)
	  )
      ad-do-it
    (physical-line-the-vertical-motion
     (1- (prefix-numeric-value (ad-get-arg 0))))
    )
  (mell-transient-region-stay)
  )

(defadvice end-of-line (around physical-line-end-of-line disable)
  (if (or (eq last-command this-command)
	  (null (physical-line-active-p))
	  (null (interactive-p))
	  (/=  (prefix-numeric-value (ad-get-arg 0)) 1)
	  )
      ad-do-it
    (if (= (physical-line-the-vertical-motion
	    (prefix-numeric-value (ad-get-arg 0)))
	   (prefix-numeric-value (ad-get-arg 0)))
	(backward-char 1)
      ))
  (mell-transient-region-stay)
  )

(defun physical-line-active-p ()
  (and (not (eq physical-line -1))
       (or  physical-line
	    physical-line-mode)
       (null (member major-mode physical-line-ignoring-mode-list))
       ))

(defun physical-line-keep-goal-column-p ()
  (member last-command physical-line-keep-goal-column-command-list)
  )

(defun physical-line-previous-logical-line (&optional count)
  (interactive "P")
  (let ((physical-line -1))
      (call-interactively 'previous-line)
    ))

(defun physical-line-next-logical-line (&optional count)
  (interactive "P")
  (let ((physical-line -1))
      (call-interactively 'next-line)
    ))

(defun physical-line-end-of-logical-line (&optional count)
  (interactive "P")
  (let ((physical-line -1))
      (call-interactively 'end-of-line)
    ))

(defun physical-line-beginning-of-logical-line (&optional count)
  (interactive "P")
  (let ((physical-line -1))
      (call-interactively 'beginning-of-line)
    ))

(defun physical-line-previous-line (&optional count)
  (interactive "P")
  (let ((physical-line t))
      (call-interactively 'previous-line)
    ))

(defun physical-line-next-line (&optional count)
  (interactive "P")
  (let ((physical-line t))
      (call-interactively 'next-line)
    ))

(defun physical-line-end-of-line (&optional count)
  (interactive "P")
  (let ((physical-line t))
      (call-interactively 'end-of-line)
    ))

(defun physical-line-beginning-of-line (&optional count)
  (interactive "P")
  (let ((physical-line t))
      (call-interactively 'beginning-of-line)
    ))


(defun physical-current-column ()
  (let ((cur (current-column))
	(bol (progn (physical-line-the-vertical-motion 0)
		    (current-column))))
    (move-to-column cur)
    (- cur bol)))

(defun physical-line-move-to-column (arg)
  (and (> arg 0)
       (move-to-column (+ (current-column) arg -1))
       (not (eolp))
       (let ((column (physical-current-column)))
	 (catch 'loop
	   (while (not (or (> column arg)
			   (and (= column 0) (> arg 1))))
	     (forward-char 1)
	     (setq column (physical-current-column))
	     (and (= column 0) (throw 'loop t))
	     )
	   )
	 (forward-char -1)
	 )))

;;;; The vertical motion
(if (and (boundp 'running-xemacs) running-xemacs)
    (defalias 'physical-line-the-vertical-motion 'vertical-motion)
  (defalias 'physical-line-the-vertical-motion
    'physical-line-the-vertical-motion--for-emacs21)
  )

;; Emacs21 の vertical-motion は WIDE 文字とタブに未対応
;; Emacs21 で信頼できるのは「行頭での vertical-motion arg (> 0)」 のみ
(defun physical-line-the-vertical-motion--for-emacs21 (arg)
  (let* ((bol-list (physical-line-get-bol-list arg))
	 (last-index (1- (length bol-list)))
	 (i 0))

    ;; (vertical-motion 0) の場所を見つける.
    (while (and (<= i last-index)
		(>= (point) (nth i bol-list)))
      (setq i (1+ i)))
    (setq i (1- i))

    ;; バッファ末の特別処理
    (if (and (eobp) (not (bolp)))
	(setq i (1- i)))

    ;; arg 分移動させる.
    (if (< (+ i arg) 0)
	(setq arg (- i)))
    (if (> (+ i arg) last-index)
	(setq arg (- last-index i)))

    (goto-char (nth (+ i arg) bol-list))
    (if (and (eobp) (not (bolp)))
	(1+ arg)
      arg)))


(defun physical-line-get-bol-list (&optional arg)
  (let ((cur-point (point))
	(eol-point (point-at-eol))
	(bol-point (point-at-bol))
	phy-bol-list)
    (if (integerp arg)
	(progn
	  ;; Though this function is only used on FSF Emacs21,
	  ;; forward-visible-line is not available on XEmacs21.
	  (forward-visible-line arg)
	  (if (> arg 0)
	      (setq eol-point (point-at-eol))
	    (setq bol-point (point-at-bol))
	    )))
    (goto-char bol-point)
    (while (< (point) eol-point)
      (setq phy-bol-list (cons (point) phy-bol-list))
      (physical-line-the-vertical-motion-one--for-emacs21)
      )
    (if (= (point) eol-point)
	(setq phy-bol-list (cons (point) phy-bol-list)))
    (goto-char cur-point)
    (reverse phy-bol-list)
    ))

(defun physical-line-the-vertical-motion-one--for-emacs21 ()
  ;; FIXME: Adhoc solution for FSF Emacs 21.3.1.
  ;; FIXME: (2004-06-16) <komatsu@taiyaki.org>
  (and
   (or (string= "21.3.1" emacs-version)
       (string< "21.3.1" emacs-version))
   (or (eolp) (forward-char 1)))
  ;;
  (vertical-motion 1))

; 物理行表示は別のマイナーモードにする.
;(defun physical-line-mode (&optional arg refresh)
;  "Toggle Physical Line mode.
;With ARG, turn Physical Line mode on iff ARG is positive.
;When Physical Line mode is enabled, cursor moving style is changed
;from Logical to Physical.
;And with REFRESH, reset lisp function line-move for Logical Line
;which is normal cursor moving style."
;  (interactive "P")
;  (setq physical-line-mode (if (null arg)
;                             (not physical-line-mode)
;                           (> (prefix-numeric-value arg) 0)))
;  (or (boundp 'original-mode-line-format)
;      (progn
;       (make-local-variable 'original-mode-line-format)
;       (setq original-mode-line-format mode-line-format)
;       (setq physical-mode-line-format
;             (physical-line-modeline mode-line-format))))
;  (make-local-hook 'post-command-hook)
;  (if physical-line-mode
;      (progn
;       (ad-enable-advice 'line-move 'around 'physical-line-move)
;       (setq mode-line-format physical-mode-line-format)
;       (add-hook 'post-command-hook 'physical-line-set-position))
;    (ad-disable-advice 'line-move 'around 'physical-line-move)
;    (setq mode-line-format original-mode-line-format)
;    (remove-hook 'post-command-hook 'physical-line-set-position))
;  (ad-activate 'line-move))

;物理行表示用なので, 現在未使用.
;(defun physical-line-modeline (&optional ml)
;  "Make modeline for display Physical Line and Column."
;  (make-local-variable 'physical-line)
;  (make-local-variable 'physical-column)
;  (physical-line-set-position)
;  (or ml (setq ml mode-line-format))
;  (mapcar
;   (function (lambda (arg)
;	       (cond ((and (listp arg) (eq (car arg) 'line-number-mode))
;		      '(line-number-mode ("PL" physical-line "--")))
;		     ((and (listp arg) (eq (car arg) 'column-number-mode))
;		      '(column-number-mode ("PC" physical-column "--")))
;		     (t arg))))
;   ml))

;物理行表示用なので, 現在未使用.
;(defun physical-line-set-position ()
;  "Set strings of Physical Column and Line for modeline."
;  (setq physical-column (number-to-string (physical-current-column)))
;  (setq physical-line
;	(let ((pos (point))
;	      (line (vertical-motion -10000)))
;	  (goto-char pos)
;	  (if (= line -10000)
;	      "****"
;	    (number-to-string (- 1 line))))))


(physical-line-init)
(provide 'physical-line)

