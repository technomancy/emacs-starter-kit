;;; smex.el --- A smart M-x enhancement for Emacs.
;; (C) 2009 Cornelius Mika
;; Licensed under the same terms as Emacs.

;;; Quick Start:
;; (require 'smex)
;; Run this after all your other Emacs packages have been loaded:
;; (smex-initialize)
;;
;; Bind the following commands:
;; smex, smex-major-mode-commands, smex-update-and-run
;; 
;; For a detailed introduction see:
;; http://github.com/nonsequitur/smex/blob/master/README.markdown
                     
(require 'ido)
;; Needed for `union'.
(require 'cl)

(defcustom smex-save-file "~/smex.save"
  "File in which the smex state is saved between Emacs sessions.
Variables stored are: `smex-data', `smex-history'.
Must be set before initializing Smex."
  :type 'string
  :group 'smex)

(defcustom smex-history-length 7
  "Determines on how many recently executed commands
Smex should keep a record.
Must be set before initializing Smex."
  :type 'integer
  :group 'smex)

(defcustom smex-prompt-string "smex "
  "String to display in the Smex prompt."
  :type 'string
  :group 'smex)

(defvar smex-cache)
(defvar smex-ido-cache)
(defvar smex-data)
(defvar smex-history)

;;--------------------------------------------------------------------------------
;; Smex Interface

(defun smex (&optional commands)
  (interactive)
  (unless commands (setq commands smex-ido-cache))
  (let ((chosen (intern (smex-completing-read commands))))
    (unwind-protect
        (call-interactively chosen)
      (smex-rank chosen)
      (smex-show-key-advice chosen))))

(defun smex-major-mode-commands ()
  "Like `smex', but limited to commands that are relevant to the active major mode."
  (interactive)
  (let ((commands (union (extract-commands-from-keymap major-mode)
                         (extract-commands-from-features major-mode))))
    (setq commands (smex-sort-according-to-cache commands))
    (setq commands (mapcar (lambda (command) (symbol-name command)) commands))
    (smex commands)))

(defun smex-completing-read (choices)
  (let ((ido-completion-map ido-completion-map)
        (ido-setup-hook (cons 'smex-prepare-ido-bindings ido-setup-hook))
        (ido-enable-prefix nil)
        (ido-enable-flex-matching t)
        (ido-max-prospects 10))
    (ido-completing-read smex-prompt-string choices)))

(defun smex-prepare-ido-bindings ()
  (define-key ido-completion-map (kbd "C-h f") 'smex-describe-function)
  (define-key ido-completion-map (kbd "C-a") 'move-beginning-of-line))

;;--------------------------------------------------------------------------------
;; Cache and Maintenance

(defun smex-rebuild-cache ()
  (interactive)
  (setq smex-cache nil)

  ;; Build up list 'new-commands' and later put it at the end of 'smex-cache'.
  ;; This speeds up sorting.
  (let (new-commands)
    (mapatoms (lambda (symbol)
                (when (commandp symbol)
                  (let ((known-command (assq symbol smex-data)))
                    (if known-command
                        (setq smex-cache (cons known-command smex-cache))
                      (setq new-commands (cons (list symbol) new-commands)))))))
    (if (eq (length smex-cache) 0)
        (setq smex-cache new-commands)
      (setcdr (last smex-cache) new-commands)))

  (setq smex-cache (sort smex-cache 'smex-sorting-rules))
  (smex-restore-history)
  (setq smex-ido-cache (smex-convert-for-ido smex-cache)))

(defun smex-convert-for-ido (command-items)
  (mapcar (lambda (command-item) (symbol-name (car command-item))) command-items))

(defun smex-restore-history ()
  "Rearranges `smex-cache' according to `smex-history'"
  (if (> (length smex-history) smex-history-length)
      (setcdr (nthcdr (- smex-history-length 1) smex-history) nil))
  (mapc (lambda (command)
          (unless (eq command (caar smex-cache))
            (let ((command-cell-position (smex-detect-position smex-cache (lambda (cell)
                                                                (eq command (caar cell))))))
              (if command-cell-position
                (let ((command-cell (smex-remove-nth-cell command-cell-position smex-cache)))
                  (setcdr command-cell smex-cache)
                  (setq smex-cache command-cell))))))
        (reverse smex-history)))

(defun smex-update ()
  (interactive)
  (smex-save-history)
  (smex-rebuild-cache))

(defun smex-update-and-run ()
  (interactive)
  (smex-update)
  (smex))

(defun smex-auto-update (&optional idle-time)
  "Update Smex when Emacs has been idle for IDLE-TIME."
  (unless idle-time (setq idle-time 60))
  (run-with-idle-timer idle-time t 'smex-update))

;;;###autoload
(defun smex-initialize ()
  (interactive)
  (unless ido-mode (smex-initialize-ido))
  (let ((save-file (expand-file-name smex-save-file)))
    (if (file-readable-p save-file)
      (with-temp-buffer
        (insert-file-contents save-file)
        (setq smex-history (read (current-buffer))
              smex-data (read (current-buffer))))
      (setq smex-history nil smex-data nil))
  (smex-rebuild-cache)
  (add-hook 'kill-emacs-hook 'smex-save-to-file)))

(defun smex-initialize-ido ()
  "Sets up a minimal Ido environment for `ido-completing-read'."
  (ido-init-completion-maps)
  (add-hook 'minibuffer-setup-hook 'ido-minibuffer-setup))

(defun smex-save-history ()
  "Updates `smex-history'"
  (setq smex-history nil)
  (let ((cell smex-cache))
    (dotimes (i smex-history-length)
      (setq smex-history (cons (caar cell) smex-history))
      (setq cell (cdr cell))))
  (setq smex-history (nreverse smex-history)))

(defun smex-save-to-file ()
  (interactive)
  (smex-save-history)
  (with-temp-file (expand-file-name smex-save-file)
    (ido-pp 'smex-history)
    (ido-pp 'smex-data)))

;;--------------------------------------------------------------------------------
;; Ranking

(defun smex-sorting-rules (command-item other-command-item)
  "Returns true if COMMAND-ITEM should sort before OTHER-COMMAND-ITEM."
  (let* ((count        (or (cdr command-item      ) 0))
         (other-count  (or (cdr other-command-item) 0))
         (name         (car command-item))
         (other-name   (car other-command-item))
         (length       (length (symbol-name name)))
         (other-length (length (symbol-name other-name))))
    (or (> count other-count)                         ; 1. Frequency of use
        (and (= count other-count)
             (or (< length other-length)              ; 2. Command length
                 (and (= length other-length)
                      (string< name other-name))))))) ; 3. Alphabetical order

(defun smex-rank (command)
  (let ((command-item (assq command smex-cache)))
    ;; TODO: Should we first update the cache and
    ;; then try again if command-item is nil?
    (when command-item
      (smex-update-count command-item)

      ;; Don't touch the cache order if the chosen command
      ;; has just been execucted previously.
      (unless (eq command-item (car smex-cache))
        (let (command-cell
              (pos (smex-detect-position smex-cache (lambda (cell)
                                                      (eq command-item (car cell))))))
          ;; Remove the just executed command.
          (setq command-cell (smex-remove-nth-cell pos smex-cache))
          ;; And put it on top of the cache.
          (setcdr command-cell smex-cache)
          (setq smex-cache command-cell)

          ;; Repeat the same for the ido cache. Should this be DRYed?
          (setq command-cell (smex-remove-nth-cell pos smex-ido-cache))
          (setcdr command-cell smex-ido-cache)
          (setq smex-ido-cache command-cell)

          ;; Now put the last history item back to its normal place.
          (smex-sort-item-at smex-history-length))))))

(defun smex-update-count (command-item)
  (let ((count (cdr command-item)))
    (setcdr command-item
            (if count
                (1+ count)
              ;; Else: Command has just been executed for the first time.
              ;; Add it to `smex-data'.
              (if smex-data
                  (setcdr (last smex-data) (list command-item))
                (setq smex-data (list command-item)))
              1))))

(defun smex-sort-item-at (n)
  "Sorts item at position N in `smex-cache'."
  (let* ((command-cell (nthcdr n smex-cache))
         (command-item (car command-cell))
         (command-count (cdr command-item)))
    (let ((insert-at (smex-detect-position command-cell (lambda (cell)
                                                          (smex-sorting-rules command-item (car cell))))))
      ;; TODO: Should we handle the case of 'insert-at' being nil?
      ;; This will never happen in practice.
      (when (> insert-at 1)
        (setq command-cell (smex-remove-nth-cell n smex-cache))
        ;; smex-cache just got shorter by one element, so subtract '1' from insert-at.
        (setq insert-at (+ n (- insert-at 1)))
        (smex-insert-cell command-cell insert-at smex-cache)

        ;; Repeat the same for the ido cache. DRY?
        (setq command-cell (smex-remove-nth-cell n smex-ido-cache))
        (smex-insert-cell command-cell insert-at smex-ido-cache)))))

(defun smex-detect-position (cell function)
  "Detects, relatively to CELL, the position of the cell
on which FUNCTION returns true.
Only checks cells after CELL, starting with the cell right after CELL.
Returns nil when reaching the end of the list."
  (let ((pos 1))
    (catch 'break
      (while t
        (setq cell (cdr cell))
        (if (not cell)
            (throw 'break nil)
          (if (funcall function cell) (throw 'break pos))
          (setq pos (1+ pos)))))))

(defun smex-remove-nth-cell (n list)
  "Removes and returns the Nth cell in LIST."
  (let* ((previous-cell (nthcdr (- n 1) list))
         (result (cdr previous-cell)))
    (setcdr previous-cell (cdr result))
    result))

(defun smex-insert-cell (new-cell n list)
  "Inserts cell at position N in LIST."
  (let* ((cell (nthcdr (- n 1) list))
         (next-cell (cdr cell)))
    (setcdr (setcdr cell new-cell) next-cell)))

;;--------------------------------------------------------------------------------
;; Help and Reference

(defun smex-describe-function ()
  (interactive)
  (describe-function (intern (car ido-matches)))
  (pop-to-buffer "*Help*"))

(defvar smex-old-message nil
  "A temporary storage used by `smex-show-key-advice'")

(defun smex-show-key-advice (command)
  "Shows the keybinding for command, if available. Like `execute-extended-command'."
  (let ((advice (smex-key-advice command)))
    (when advice
      (if (current-message)
          (progn
            (run-at-time 2 nil (lambda (advice)
                                 (setq smex-old-message (current-message))
                                 (smex-unlogged-message advice)) advice)

            (run-at-time 4.5 nil (lambda (advice)
                                 (if (equal (current-message) advice)
                                     (smex-unlogged-message smex-old-message))) advice))
        (smex-unlogged-message advice)))))

(defun smex-key-advice (command)
  (let ((keys (where-is-internal command)))
    (if keys
        (format "You can run the command `%s' with %s"
                command
                (mapconcat 'key-description keys ", ")))))

(defun smex-unlogged-message (string)
  "Bypasses logging in *Messages*"
  (let (message-log-max)
    (message "%s" string)))

(defun extract-commands-from-keymap (mode)
  (let ((keymap (intern (concat (symbol-name mode) "-map"))))
    (if keymap
        (parse-keymap (symbol-value keymap)))))

(defun parse-keymap (map)
  (let (commands)
    (map-keymap (lambda (binding element)
                  (if (listp element)
                      (if (eq 'keymap (car element))
                          (parse-keymap element))
                    (setq commands (cons element commands)))) map)
    commands))

(defun extract-commands-from-features (mode)
  (let ((library-path (symbol-file mode))
        (mode-name (symbol-name mode))
        commands)
    (string-match "\\(.+?\\)\\(-mode\\)?$" mode-name)
    ;; 'lisp-mode' -> 'lisp'
    (setq mode-name (match-string 1 mode-name))
    (dolist (feature load-history)
      (let ((feature-path (car feature)))
        (when (and feature-path (or (equal feature-path library-path)
                                    (string-match (regexp-quote mode-name)
                                                  (file-name-nondirectory feature-path))))
          (dolist (item (cdr feature))
            (if (and (listp item) (eq 'defun (car item)))
                (let ((function (cdr item)))
                  (when (commandp function)
                    (setq commands (append commands (list function))))))))))
    commands))

(defun smex-sort-according-to-cache (list)
  (let (sorted)
    (dolist (command-item smex-cache)
      (let ((command (car command-item)))
        (when (memq command list)
          (setq sorted (cons command sorted))
          (setq list (delq command list)))))
    (nreverse (append list sorted))))

(defun smex-show-unbound-commands ()
  "Shows unbound commands in a new buffer,
sorted by frequency of use."
  (interactive)
  (setq smex-data (sort smex-data 'smex-sorting-rules))
  (let ((unbound-commands (delq nil
                                (mapcar (lambda (command-item)
                                          (unless (where-is-internal (car command-item))
                                            command-item))
                                        smex-data))))
    (view-buffer-other-window "*Smex: Unbound Commands*")
    (toggle-read-only nil)
    (erase-buffer)
    (ido-pp 'unbound-commands)
    (beginning-of-buffer)))

(provide 'smex)