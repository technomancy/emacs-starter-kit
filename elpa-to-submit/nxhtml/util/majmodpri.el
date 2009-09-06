;;; majmodpri.el --- Major mode priorities handling
;;
;; Author: Lennart Borgman (lennart O borgman A gmail O com)
;; Created: 2008-08-26
(defconst majmodpri:version "0.62") ;;Version:
;; Last-Updated: 2009-04-30 Thu
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
;; Different elisp libraries may try to handle the same type of files.
;; They normally do that by entering their major mode for a file type
;; in `auto-mode-alist' or the other lists affecting `normal-mode'.
;; Since the libraries may be loaded in different orders in different
;; Emacs sessions this can lead to rather stochastic choices of major
;; mode.
;;
;; This library tries to give the control of which major modes will be
;; used back to the user.  It does that by letting the user set up
;; priorities among the major modes.  This priorities are used to sort
;; the lists used by `normal-mode'.
;;
;; To setup this libray and get more information do
;;
;;   M-x customize-group RET majmodpri RET
;;
;; Or, see the commands `majmodpri-sort-lists'.
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


;;;; Idle sorting

(defvar majmodpri-idle-sort-timer nil)

(defun majmodpri-cancel-idle-sort ()
  "Cancel idle sorting request."
  (when majmodpri-idle-sort-timer
    (cancel-timer majmodpri-idle-sort-timer)
    (setq majmodpri-idle-sort-timer nil)))

(defun majmodpri-start-idle-sort ()
  "Request idle sorting."
  (majmodpri-cancel-idle-sort)
  (setq majmodpri-idle-sort-timer
        (run-with-idle-timer 0 nil 'majmodpri-sort-lists-in-timer)))

(defun majmodpri-sort-lists-in-timer ()
  (condition-case err
      (save-match-data ;; runs in timer
        (majmodpri-sort-lists))
    (error (message "(majmodpri-sort-lists): %s" err))))


;;;; Sorting

(defvar majmodpri-schwarzian-ordnum nil)
(defun majmodpri-schwarzian-in (rec)
  "Transform REC before sorting."
  (setq majmodpri-schwarzian-ordnum (1+ majmodpri-schwarzian-ordnum))
  (let ((mode (cdr rec)))
    (list
     (list mode majmodpri-schwarzian-ordnum)
     rec)))

(defun majmodpri-schwarzian-out (rec)
  "Get original value of REC after sorting."
  (cadr rec))

;; Fix-me: default for Emacs 22??
(defcustom majmodpri-no-nxml (< emacs-major-version 23)
  "Don't use multi major modes with nxml if non-nil.
The default for Emacs prior to version 23 is to not use this
multi major modes by default since there are some problems.

This gives those multi major mode lower priority, but it does not
prevent use of them."
  :type 'boolean
  :group 'majmodpri)

;; (majmodpri-priority 'html-mumamo-mode)
;; (majmodpri-priority 'nxhtml-mumamo-mode)
(defsubst majmodpri-priority (mode)
  "Return major mode MODE priority."
  (if (and majmodpri-no-nxml
           (symbolp mode)
           (save-match-data
             (string-match "nxhtml-mumamo" (symbol-name mode))))
      0
    (length (memq mode majmodpri-mode-priorities))))

(defun majmodpri-compare-auto-modes (rec1 rec2)
  "Compare record REC1 and record REC2.
Comparision:

- First check `majmodpri-mode-priorities'.
- Then use old order in list."
  (let* ((schw1 (car rec1))
         (schw2 (car rec2))
         (mod1     (nth 0 schw1))
         (mod2     (nth 0 schw2))
         (ord1     (nth 1 schw1))
         (ord2     (nth 1 schw2))
         (pri1 (majmodpri-priority mod1))
         (pri2 (majmodpri-priority mod2)))
    (cond
     ((/= pri1 pri2) (> pri1 pri2))
     (t (> ord1 ord2)))))

;;(benchmark 100 (quote (majmodpri-sort-lists)))
;;(defvar my-auto-mode-alist nil)
(defun majmodpri-sort-auto-mode-alist ()
  "Sort `auto-mode-alist' after users priorities."
  (setq majmodpri-schwarzian-ordnum 0)
  ;; Do not reorder function part, but put it first.
  (let (fun-list
        mod-list)
    (dolist (rec auto-mode-alist)
      (if (listp (cdr rec))
          (setq fun-list (cons rec fun-list))
        (setq mod-list (cons rec mod-list))))
    (setq fun-list (nreverse fun-list))
    (setq auto-mode-alist
          (append
           fun-list
           (mapcar 'majmodpri-schwarzian-out
                   (sort
                    (mapcar 'majmodpri-schwarzian-in mod-list)
                    'majmodpri-compare-auto-modes))))))

(defun majmodpri-sort-magic-list (magic-mode-list-sym)
  "Sort list MAGIC-MODE-LIST-SYM after users priorities."
  (let ((orig-ordnum 0))
    (set magic-mode-list-sym
         ;; S out
         (mapcar (lambda (rec)
                   (cadr rec))
                 ;; Sort
                 (sort
                  ;; S in
                  (mapcar (lambda (rec)
                            (setq orig-ordnum (1+ orig-ordnum))
                            (let ((mode (cdr rec)))
                              (list
                               (list mode orig-ordnum)
                               rec)))
                          (symbol-value magic-mode-list-sym))
                  (lambda (rec1 rec2)
                    (let* ((schw1 (car rec1))
                           (schw2 (car rec2))
                           (mod1 (nth 0 schw1))
                           (mod2 (nth 0 schw2))
                           (ord1 (nth 1 schw1))
                           (ord2 (nth 1 schw2))
                           (pri1 (majmodpri-priority mod1))
                           (pri2 (majmodpri-priority mod2)))
                      (cond
                       ((/= pri1 pri2) (> pri1 pri2))
                       (t (> ord1 ord2))))))))))

;;;###autoload
(defun majmodpri-sort-lists ()
  "Sort the list used when selecting major mode.
Only sort those lists choosen in `majmodpri-lists-to-sort'.
Sort according to priorities in `majmodpri-mode-priorities'.
Keep the old order in the list otherwise.

The lists can be sorted when loading elisp libraries, see
`majmodpri-sort-after-load'.

See also `majmodpri-apply-priorities'."
  (interactive)
  (message "majmodpri-sort-lists running ...")
  (majmodpri-cancel-idle-sort)
  (when (memq 'magic-mode-alist majmodpri-lists-to-sort)
    (majmodpri-sort-magic-list 'magic-mode-alist))
  (when (memq 'auto-mode-alist majmodpri-lists-to-sort)
    (majmodpri-sort-auto-mode-alist))
  (when (memq 'magic-fallback-mode-alist majmodpri-lists-to-sort)
    (majmodpri-sort-magic-list 'magic-fallback-mode-alist))
  (message "majmodpri-sort-lists running ... (done)"))


;;;###autoload
(defun majmodpri-apply ()
  "Sort major mode lists and apply to existing buffers.
Note: This function is suitable to add to
`desktop-after-read-hook'. It will restore the multi major modes
in buffers."
  (majmodpri-apply-priorities t))

(defun majmodpri-sort-apply-to-current ()
  "Sort lists and apply to current buffer."
  (majmodpri-sort-lists)
  (add-hook 'find-file-hook 'normal-mode t t))

(defun majmodpri-check-normal-mode ()
  "Like `normal-mode', but keep major mode if same."
  (let ((old-major-mode major-mode)
        (old-mumamo-multi-major-mode (when (boundp 'mumamo-multi-major-mode)
                                       mumamo-multi-major-mode)))
    (report-errors "File mode specification error: %s"
      (set-auto-mode t))
    (unless (and (eq old-major-mode major-mode)
                 (eq old-mumamo-multi-major-mode mumamo-multi-major-mode))
      (report-errors "File local-variables error: %s"
        (hack-local-variables))
      ;; Turn font lock off and on, to make sure it takes account of
      ;; whatever file local variables are relevant to it.
      (when (and font-lock-mode
                 ;; Font-lock-mode (now in font-core.el) can be ON when
                 ;; font-lock.el still hasn't been loaded.
                 (boundp 'font-lock-keywords)
                 (eq (car font-lock-keywords) t))
        (setq font-lock-keywords (cadr font-lock-keywords))
        (font-lock-mode 1))
      (message "majmodpri-apply-priorities: buffer=%s, %s,%s => %s,%s"
               (current-buffer)
               old-major-mode
               old-mumamo-multi-major-mode
               major-mode
               (when (boundp 'mumamo-multi-major-mode)
                 mumamo-multi-major-mode)))))

;;;###autoload
(defun majmodpri-apply-priorities (change-modes)
  "Apply major mode priorities.
First run `majmodpri-sort-lists' and then if CHANGE-MODES is
non-nil apply to existing file buffers.  If interactive ask
before applying."
  (interactive '(nil))
  (message "majmodpri-apply-priorities running...")
  (majmodpri-sort-lists)
  (when (or change-modes
            (called-interactively-p))
    (let (file-buffers)
      (dolist (buffer (buffer-list))
        (with-current-buffer buffer
          (let ((name (buffer-name))
                (file buffer-file-name))
            (or (string= (substring name 0 1) " ") ;; Internal
                (not file)
                (setq file-buffers (cons buffer file-buffers))))))
      (if (not file-buffers)
          (when change-modes
            (message "majmodpri-apply-priorities: No file buffers to change modes in"))
        (when (called-interactively-p)
          (setq change-modes
                (y-or-n-p "Check major mode in all file visiting buffers? ")))
        (when change-modes
          (dolist (buffer file-buffers)
            (with-current-buffer buffer
              (let ((old-major major-mode))
                (majmodpri-check-normal-mode)
                )))))))
  (message "majmodpri-apply-priorities running ... (done)"))


;;;; Custom

;;;###autoload
(defgroup majmodpri nil
  "Customization group for majmodpri.el"
  :group 'nxhtml
  )

(defcustom majmodpri-mode-priorities
  '(
    cperl-mumamo-mode
    csound-sgml-mumamo-mode
    django-nxhtml-mumamo-mode
    django-html-mumamo-mode
    embperl-nxhtml-mumamo-mode
    embperl-html-mumamo-mode
    eruby-nxhtml-mumamo-mode
    eruby-html-mumamo-mode
    genshi-nxhtml-mumamo-mode
    genshi-html-mumamo-mode
    jsp-nxhtml-mumamo-mode
    jsp-html-mumamo-mode
    laszlo-nxml-mumamo-mode
    metapost-mumamo-mode
    mjt-nxhtml-mumamo-mode
    mjt-html-mumamo-mode
    noweb2-mumamo-mode
    ;;org-mumamo-mode
    perl-mumamo-mode
    smarty-nxhtml-mumamo-mode
    smarty-html-mumamo-mode
    ;;tt-html-mumamo-mode

    nxhtml-mumamo-mode
    html-mumamo-mode
    nxml-mumamo-mode
    nxml-mode

    javascript-mode
    espresso-mode
    rhtml-mode
    )
  "Priority list for major modes.
Modes that comes first have higher priority.
See `majmodpri-sort-lists' for more information."
  :type '(repeat symbol)
  :set (lambda (sym val)
         (set-default sym val)
         (when (and (boundp 'majmodpri-sort-after-load)
                    majmodpri-sort-after-load)
           (majmodpri-start-idle-sort)))
  :group 'majmodpri)

(defcustom majmodpri-lists-to-sort
  '(magic-mode-alist auto-mode-alist magic-fallback-mode-alist)
  ;;nil
  "Which major mode lists to sort.
See `majmodpri-sort-lists' for more information."
  :type '(set (const magic-mode-alist)
              (const auto-mode-alist)
              (const magic-fallback-mode-alist))
  :set (lambda (sym val)
         (set-default sym val)
         (when (and (boundp 'majmodpri-sort-after-load)
                    majmodpri-sort-after-load)
           (majmodpri-start-idle-sort)))
  :group 'majmodpri)

(defcustom majmodpri-sort-after-load
  '(
    chart
    gpl
    nxhtml-autoload
    php-mode
    rnc-mode
    ruby-mode
    )
  "Sort major mode lists after loading elisp libraries if non-nil.
This should not really be needed since just loading a library
should not change how Emacs behaves.  There are however quite a
few thirt party libraries that does change `auto-mode-alist'
\(including some of my own) since that sometimes seems
reasonable.  Some of them are in the default value of this
variable.

There are two possibilities for sorting here:

- Value=list of features (default). Sort immediately after loading a
  library in the list.  Apply to current buffer.

- Value=t. Sort after loading any library. Sorting is then not
  done immediately.  Instead it runs in an idle timer.  This
  means that if several elisp libraries are loaded in a command
  then the sorting will only be done once, after the command has
  finished.  After sorting apply to all buffers.

Note that the default does break Emacs rule that loading a
library should not change how Emacs behave.  On the other hand
the default tries to compensate for that the loaded libraries
breaks this rule by changing `auto-mode-alist'.

See `majmodpri-sort-lists' for more information."
  :type '(choice (const :tag "Never" nil)
                 (const :tag "After loading any elisp library" t)
                 (repeat :tag "After loading specified features" symbol))
  :set (lambda (sym val)
         (set-default sym val)
         ;; Clean up `after-load-alist' first.
         (setq after-load-alist
               (delq nil
                     (mapcar (lambda (rec)
                               (unless (member (cadr rec)
                                               '((majmodpri-start-idle-sort)
                                                 (majmodpri-sort-lists)))
                                 rec))
                             after-load-alist)))
         (when val
           ;;(message "majmodpri-sort-after-load: val=%s" val)
           (let ((sort-and-apply nil))
             (if (not (listp val))
                 (add-to-list 'after-load-alist
                              (if (eq val t)
                                  '(".*" (majmodpri-start-idle-sort))
                                '("." (majmodpri-sort-lists))))
               (dolist (feat val)
                 ;;(message "feat=%s" feat)
                 (if (featurep feat)
                     (setq sort-and-apply t)
                   (if (eq val t)
                       (eval-after-load feat '(majmodpri-start-idle-sort))
                     (eval-after-load feat '(majmodpri-sort-apply-to-current))))))
             (when sort-and-apply
               ;;(message "majmodpri-sort-after-load: sort-and-apply")
               (majmodpri-apply-priorities t))
             (if (eq val t)
                 (majmodpri-start-idle-sort)
               (majmodpri-apply-priorities t)))))
  :group 'majmodpri)


(provide 'majmodpri)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; majmodpri.el ends here
