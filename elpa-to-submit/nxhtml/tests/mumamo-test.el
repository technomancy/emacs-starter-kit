;;; mumamo-test.el --- Test routines for mumamo
;;
;; Author: Lennart Borgman
;; Created: Sat Mar 31 03:59:26 2007
;; Version: 0.1
;; Last-Updated:
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; This file defines some test for mumamo.el and a the minor mode
;; `mumamu-test-mode' to bind the test functions to some keys for
;; convenient use. This will define F3 to run
;; `mumamo-test-create-chunk-at' and Shift-F3 to
;; `mumamo-test-create-chunks-at-all-points'.
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change log:
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

;;(eval-when-compile (require 'mumamo))
(require 'mumamo)
(require 'whelp)

;;;;;;; TESTS, run in fundamental-mode buffer

(defvar mumamo-test-mode-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map [f11] 'goto-char)
    (define-key map [(meta f3)] 'mumamo-test-fontify-region)
    (define-key map [(shift f3)] 'mumamo-test-create-chunks-at-all-points)
    (define-key map [f3] 'mumamo-test-create-chunk-at-point)
    map))

(defvar mumamo-test-current-chunk-family nil)
(make-variable-buffer-local 'mumamo-test-current-chunk-family)

(define-minor-mode mumamo-test-mode
  "For testing creating mumamo-mode chunks.
When this mode is on the following keys are defined:

  \\{mumamo-test-mode-keymap}

"
  nil
  " MuMaMo-TEST"
  :keymap mumamo-test-mode-keymap
  (if mumamo-test-mode
      (progn
        (setq mumamo-test-current-chunk-family mumamo-current-chunk-family)
        (setq mumamo-use-condition-case nil)
        (setq mumamo-debugger nil)
        (run-with-idle-timer 0 nil 'mumamo-test-tell-bindings))
    (setq mumamo-use-condition-case t)
    (setq mumamo-debugger (default-value 'mumamo-debugger)))
  )

(defun mumamo-test-tell-bindings ()
  (save-match-data ;; runs in timer
    (let ((s "mumamo-test-mode is on, use F3/shift-F3 for simple testing"))
      (put-text-property 0 (length s)
                         'face 'font-lock-warning-face
                         s)
      (message "%s" s))))

;;(mumamo-test-mode 1)


;; (defun mumamo-test-fontify-buffer ()
;;   (interactive)
;;   (unless mumamo-current-chunk-family
;;     (mumamo-select-chunk-family))
;;   ;;(when mumamo-mode (mumamo-mode 0))
;;   (when mumamo-multi-major-mode (mumamo-turn-off-actions))
;;   (save-excursion
;;     (mumamo-remove-all-chunk-overlays)
;;     (mumamo-save-buffer-state nil
;;       (put-text-property (point-min) (point-max) 'face nil))
;;     (mumamo-fontify-buffer)))

(defun mumamo-test-create-chunk-at-point ()
  (interactive)
  (remove-hook 'post-command-hook 'mumamo-post-command t)
  (font-lock-mode -1)
  (setq fontification-functions nil)
  (save-excursion
    (mumamo-remove-all-chunk-overlays)
    (mumamo-save-buffer-state nil
      (remove-text-properties (point-min) (point-max) '(face nil syntax-table nil)))
    (let* ((mumamo-current-chunk-family mumamo-test-current-chunk-family)
           (here (point))
           chunk
           chunk2)
      (mumamo-save-buffer-state nil
        ;;(setq chunk (mumamo-create-chunk-at here)))
        (setq chunk (mumamo-find-chunks here "test1")))
      ;;(setq chunk2 (mumamo-get-chunk-at here))
      (setq chunk2 (mumamo-find-chunks here "set chunk2"))
      ;;(message "mumamo-test-create-chunk-at-point.chunk 1=%s" chunk)
      ;;(lwarn 'test-create-chunk-at :warning "chunk=%s, chunk2=%s" chunk chunk2)
      ;;(when (overlay-buffer chunk)
        (assert (eq chunk chunk2))
        ;;)
      ;;(message "mumamo-test-create-chunk-at-point.chunk 2=%s" chunk)
      ;;(syntax-ppss-flush-cache (1- (overlay-start chunk)))
      (syntax-ppss-flush-cache (overlay-start chunk))
      (let ((start (overlay-start chunk))
            (end   (overlay-end chunk)))
        ;;(setq syntax-ppss-last (cons 319 (parse-partial-sexp 1 1)))
        ;;(message "mumamo-test-create-chunk-at-point.chunk 2a=%s" chunk)
        (mumamo-save-buffer-state nil
          (mumamo-fontify-region-1 start end nil)))
      ;;(message "mumamo-test-create-chunk-at-point.chunk 3=%s" chunk)
      (unless mumamo-test-mode (mumamo-test-mode 1))
      ;;(message "mumamo-test-create-chunk-at-point.chunk 4=%s" chunk)
      chunk
      ;;(message "test 2.debugger=%s" debugger)
      ;;(mumamo-get-chunk-at here)
      (mumamo-find-chunks here "return value")
      )))

(defun mumamo-test-create-chunks-at-all-points ()
  (interactive)
  ;;(goto-char (point-min))
  (let (last-ovl
        this-ovl)
    (while (< (point) (point-max))
      ;;(setq this-ovl (mumamo-test-create-chunk-at-point))
      (setq this-ovl (mumamo-find-chunks (point) "test loop"))
      ;;(message "this-ovl=%s" this-ovl)
      (sit-for 0.01)
      ;;(sit-for 0)
      (when last-ovl
        (if (= (point) (overlay-end last-ovl))
            (assert (= (overlay-end last-ovl) (overlay-start this-ovl)))
          (assert (= (overlay-start last-ovl) (overlay-start this-ovl)))
          (assert (= (overlay-end last-ovl) (overlay-end this-ovl)))
          ))
      (if last-ovl
          (move-overlay last-ovl (overlay-start this-ovl) (overlay-end this-ovl))
        (setq last-ovl (make-overlay (overlay-start this-ovl) (overlay-end this-ovl))))
      (forward-char 1)
      )
    (message "No problems found")))

(defun mumamo-test-fontify-region ()
  (interactive)
  (let ((font-lock-mode t))
    ;;(mumamo-fontify-region-with (point-min) (point-max) nil 'php-mode nil)
    (mumamo-fontify-region (point-min) (point-max) t)))

;; Fix-me: can't byte compile:
;; (defun mumamo-test-easy-make ()
;;   (interactive)
;;   (let ((start-str "--Start Submode:")
;;         (end-str "--End Submode--")
;;         (start-reg nil))
;;     (setq start-reg
;;           ;; (rx
;;           ;;  (eval start-str)
;;           ;;  (0+ space)
;;           ;;  (submatch
;;           ;;   (0+ (any "a-z-")))
;;           ;;  (0+ space)
;;           ;;  "--"
;;           ;;  )
;;           (rx-to-string
;;            `(and
;;              ,start-str
;;              (0+ space)
;;              (submatch
;;               (0+ (any "a-z-")))
;;              (0+ space)
;;              "--"
;;              ))
;;           )
;;     (mumamo-easy-make-chunk-fun testchunk
;;       start-str
;;       start-reg
;;       end-str))
;;   (setq mumamo-current-chunk-family
;;         (list "testing"
;;               'text-mode
;;               (list
;;                'testchunk
;;                ))))

;; (defun mumamo-test-emb-perl ()
;;   (interactive)
;;   (let ((start-str "[-")
;;         (end-str "-]")
;;         (start-reg nil))
;;     (mumamo-easy-make-chunk-fun testchunk-ep
;;       start-str
;;       start-reg
;;       end-str))
;;   (setq mumamo-current-chunk-family
;;         (list "emb perl test"
;;               'perl-mode
;;               (list
;;                'testchunk-ep
;;                ))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; These are for testing bad initialization in mumamo. They can be
;; used for example with php-mode. (They are mainly for development
;; purposes.)
;;
;; (mumamo-bad-c-init)
(defun mumamo-bad-c-init() (/ 1 0))
(defun mumamo-setup-bad-c-init ()
  (interactive)
  (add-hook 'c-mode-common-hook 'mumamo-bad-c-init))
(defun mumamo-teardown-bad-c-init ()
  (interactive)
  (remove-hook 'c-mode-common-hook 'mumamo-bad-c-init))


;; (defmacro mumamo-get-backtrace (bodyform)
;;   "Evaluate BODYFORM, return backtrace as a string.
;; If there is an error in BODYFORM then return the backtrace as a
;; string, otherwise return nil."
;;   `(let* ((debugger-ret nil)
;;           (debugger (lambda (&rest debugger-args)
;;                       (message "DEBUGGER CALLED BEFORE")
;;                       (setq debugger-ret (with-output-to-string (backtrace)))
;;                       (message "DEBUGGER CALLED AFTER, debugger-ret=%s" debugger-ret)
;;                       ))
;;           (debug-on-error t)
;;           (debug-on-signal t)
;;           )
;;      (condition-case err
;;          (progn
;;            ,bodyform
;;            nil)
;;        (error
;;         (message "err=%S" err)
;;         (message "debugger-ret=%S\n\n\n" debugger-ret)
;;         (let* ((errmsg (error-message-string err))
;;                (debugger-lines (split-string debugger-ret "\n"))
;;                (dbg-ret (mapconcat 'identity (nthcdr 6 debugger-lines) "\n")))
;;           (concat errmsg "\n" dbg-ret))))))

;; (defun mumamo-test3-debug()
;;   (interactive)
;;   (message "%s"
;;            (mumamo-get-backtrace
;;             (mumamo-test-major-mode-init 'php-mode))))

;; (defun mumamo-test2-debug()
;;   (interactive)
;;   (mumamo-condition-case var
;;                          (mumamo-test-major-mode-init 'php-mode)
;;                          handlers))

(defun mumamo-test-debug()
  (interactive)
  (condition-case err
      (let ((debugger 'mumamo-debug)
            (debug-on-error t)
            (debug-on-signal t))
        ;;(message "here d")(sit-for 1)
        (mumamo-test-major-mode-init 'php-mode))
    (error (message "here 2 err=%S" err))))

(defun mumamo-debug (&rest debugger-args)
  (let ((s (with-output-to-string (backtrace))))
    (message "mumamo-debug: %s" s)))

(defun mumamo-list-timers ()
  (interactive)
  (mapatoms (lambda (s)
              (when (timerp s)
                (message "%s: %s" (symbol-name s) (symbol-value s))))))

;; (defun mumamo-bt-to-msg (msg)
;;   (mumamo-msgfntfy "%s: %s" msg
;;            (with-output-to-string
;;              (backtrace))))

(defun mumamo-test-major-mode-init (major)
  "Turn on major mode MAJOR in a temp buffer.
This function should be used after getting errors during
fontification where the message in the *Message* buffer tells
that you should call it to get a traceback.

Send the traceback you get, if any, together with the message in
the message buffer when reporting the error."
  (interactive "CMajor mode: ")
  (with-temp-buffer
    ;;(setq mumamo-explicitly-turned-on-off t)
    (setq debug-on-error t)
    (funcall major)))

(provide 'mumamo-test)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; mumamo-test.el ends here
