(require 'rcirc)

;;; rewritted completion stuff!

(defun rcirc-complete-nick ()
  "Complete nick from list of nicks in channel."
  (interactive)
  (if (eq last-command this-command)
      ;; cycle or insert depending on ambiguity
      (if (rcirc-currently-ambiguous-p)
          (rcirc-cycle-complete-nick)
        (rcirc-insert-nick))
    ;; first push of the tab key needs to set up some vars
    (setq rcirc-nick-completion-start-offset
          (- (save-excursion
               (if (re-search-backward " " rcirc-prompt-end-marker t)
                   (1+ (point))
                 rcirc-prompt-end-marker))
             rcirc-prompt-end-marker))
    (setq rcirc-nick-completion-end-offset (point))
    (setq rcirc-nick-completions (rcirc-currently-valid-completions))
    ;; get the unambiguous part and insert it
    (let ((unambiguous (car rcirc-nick-completions)))
      (dolist (nick (cdr rcirc-nick-completions))
        (setq unambiguous (rcirc-get-unambiguous nick unambiguous)))
      (setq foo unambiguous)
      (rcirc-insert-nick unambiguous t))
    ;; then echo the remainder
    (if (rcirc-currently-ambiguous-p)
        (rcirc-echo-nicks))))

(defun rcirc-currently-ambiguous-p ()
  (> (length (rcirc-currently-valid-completions)) 1))

(defun rcirc-currently-valid-completions ()
  (let ((completion-ignore-case t))
    (all-completions
     (rcirc-typed-substring)
     (mapcar (lambda (x) (cons x nil))
             (rcirc-channel-nicks (rcirc-buffer-process)
                                  rcirc-target)))))

(defun rcirc-get-unambiguous (nick target)
   (if (> (length nick) (length target))
       (setq nick (substring nick 0 (length target))))
   (if (string= (upcase nick) (upcase (substring target 0 (length nick))))
       nick
     (rcirc-get-unambiguous (substring nick 0 (- (length nick) 1)) target)))

(defun rcirc-typed-substring ()
  (buffer-substring
   (+ rcirc-prompt-end-marker rcirc-nick-completion-start-offset)
   rcirc-nick-completion-end-offset))


(defun rcirc-echo-nicks ()
  (let ((some-nicks (subseq rcirc-nick-completions 0 15)))
    (if (> (length rcirc-nick-completions) 15)
        (add-to-list 'some-nicks "[more...]" t))
    (message (mapconcat 'identity some-nicks  " "))))

(defun rcirc-cycle-complete-nick ()
  "Complete nick by cycling through possibilities."
  (let ((completion (car rcirc-nick-completions)))
    (add-to-list 'rcirc-nick-completions (pop rcirc-nick-completions) t)
    (when completion
      (rcirc-put-nick-channel (rcirc-buffer-process) completion rcirc-target)
      (rcirc-insert-nick completion))))

(defun rcirc-insert-nick (&optional nick incomplete)
  (setq nick (or nick (first rcirc-nick-completions)))
  (when nick
    (delete-region (+ rcirc-prompt-end-marker
                      rcirc-nick-completion-start-offset)
                   (point))
    (insert nick)
    (if (not incomplete)
        (insert ": "))))

(provide 'rcirc-completion)