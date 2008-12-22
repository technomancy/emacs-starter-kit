;;; majmodpri.el --- Major mode priorities handling
;;
;; Author: Lennart Borgman (lennart O borgman A gmail O com)
;; Created: 2008-08-26
(defconst majmodpri:version "0.6") ;;Version:
;; Last-Updated: 2008-08-26T19:21:00+0200 Tue
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
      (majmodpri-sort-lists)
    (error (message "(majmodpri-sort-lists): %s" err))))

;;;; Custom

;;;###autoload
(defgroup majmodpri nil
  "Customization group for majmodpri.el"
  :group 'nxhml
  )

(defcustom majmodpri-sort-after-load
  nil
  "Sort major mode lists after loading elisp libraries.
Sorting is not done immediately.  Instead it runs in an idle
timer.  This means that if several elisp libraries are loaded in
a command then the sorting will only be done once, after the
command has finished.

See `majmodpri-sort-lists' for more information."
  :type '(choice (const :tag "Never" nil)
                 (const :tag "After loading any elisp library" t)
                 (repeat :tag "After loading specified libraries" symbol))
  :set (lambda (sym val)
         (set-default sym val)
         ;; Clean up `after-load-alist' first.
         (setq after-load-alist
               (delq nil
                     (mapcar (lambda (rec)
                               (unless (equal (cadr rec)
                                              '(majmodpri-start-idle-sort))
                                 rec))
                             after-load-alist)))
         (when val
           (if (not (listp val))
               (add-to-list 'after-load-alist
                            '(".*" (majmodpri-start-idle-sort)))
             (dolist (feat val)
               (unless (featurep feat)
                 (eval-after-load feat '(majmodpri-start-idle-sort)))))
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
         (when majmodpri-sort-after-load
           (majmodpri-start-idle-sort)))
  :group 'majmodpri)

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

    rhtml-mode
    )
  "Priority list for major modes.
Modes that comes first have higher priority.
See `majmodpri-sort-lists' for more information."
  :type '(repeat symbol)
  :set (lambda (sym val)
         (set-default sym val)
         (when majmodpri-sort-after-load
           (majmodpri-start-idle-sort)))
  :group 'majmodpri)


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

(defsubst majmodpri-priority (mode)
  "Return major mode MODE priority."
  (length (memq mode majmodpri-mode-priorities)))

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
    (majmodpri-sort-magic-list 'magic-fallback-mode-alist)))


;;;###autoload
(defun majmodpri-apply-priorities (change-modes)
  "Apply major mode priorities.
First run `majmodpri-sort-lists' and then if CHANGE-MODES is
non-nil apply to existing file buffers.  If interactive ask
before applying."
  (interactive '(nil))
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
            (message "No file buffers to change modes in"))
        (when (called-interactively-p)
          (setq change-modes
                (y-or-n-p "Check major mode in all file visiting buffers? ")))
        (when change-modes
          (dolist (buffer file-buffers)
            (with-current-buffer buffer (normal-mode))))))))


(provide 'majmodpri)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; majmodpri.el ends here
