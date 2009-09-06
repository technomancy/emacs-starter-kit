;;; nxhtmltest-suites.el --- Test suites for mumamo / nXhtml
;;
;; Author: Lennart Borgman (lennart O borgman A gmail O com)
;; Created: 2008-07-08T20:17:36+0200 Tue
;; Version: 0.12
;; Last-Updated: 2008-09-01T01:13:28+0200 Sun
;; URL:
;; Keywords:
;; Compatibility:
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; Defines `nxhtmltest-run'.  When (getenv "nxhtmltest-run-Q")
;; returns non-nil also runs this function.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change log:
;;
;; Added code from Christian Ohler for writing ert tests.
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

;;(eval-when-compile (require 'cl))
(require 'cl)
(require 'mumamo)
(require 'nxhtml)
(require 'nxhtml-mumamo)
(when (fboundp 'nxml-mode)
  (require 'rng-valid)
  (require 'rngalt))

(setq debug-on-error t)

(defvar nxhtmltest-bin
  (file-name-directory (if load-file-name load-file-name buffer-file-name)))

(pushnew nxhtmltest-bin load-path)
(require 'nxhtmltest-helpers)
;;(require 'ert)

(defvar nxhtmltest-files-root
  (let* ((this-dir nxhtmltest-bin)
         (root (expand-file-name "in/" this-dir)))
    (unless (file-accessible-directory-p root)
          (error (if (file-exists-p root)
                     "Can't read files in test directory %s"
                   "Can't find test directory %s")
                 root))
    root))

(let ((distr-in "c:/EmacsW32/nxhtml/tests/in/"))
  (when (file-directory-p distr-in)
    (setq nxhtmltest-files-root distr-in)))

;; (setq nxhtmltest-update-method
;;       ;;'font-lock-wait
;;       'font-lock-run-timers
;;       ;;'font-lock-fontify-buffer
;;       )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Define tests using ert.el

(ert-deftest nxhtml-ert-bug-300946-index ()
  "Test for bug 300946 in Launchpad.
See URL `https://bugs.launchpad.net/nxhtml/+bug/300946'.  This is
a test for the file attached by Chris on 2008-12-02."
  (ert-with-temp-buffer-include-file "bug-300946-index.html"
    (add-hook 'ert-simulate-command-post-hook
              'nxhtmltest-should-no-mumamo-errors
              nil t)
    (ert-simulate-command '(nxhtml-mumamo-mode) t)
    (font-lock-mode 1)
    ))

(ert-deftest nxhtml-ert-indent-bug290364 ()
  "Test for bug 290364 in Launchpad.
See URL `https://bugs.launchpad.net/nxhtml/+bug/290364'.

Note: If this test fails Emacs loops.  Therefore the whole test
is included in a when clause so you can avoid it easily."
  ;;(when t
    (ert-with-temp-buffer-include-file "bug-290364.php"
      (add-hook 'ert-simulate-command-post-hook
                'nxhtmltest-should-no-mumamo-errors
                nil t)
      (ert-simulate-command '(nxhtml-mumamo-mode) t)
      (font-lock-mode 1)
      ))
;)

(ert-deftest nxhtml-ert-indent-bug271497 ()
  "Test for bug 271497 in Launchpad.
This is a bug in Emacs 22. It should work in Emacs 23 though.
See URL `https://bugs.launchpad.net/nxhtml/+bug/271497'."
  (ert-with-temp-buffer-include-file "bug271497.txt"
    (add-hook 'ert-simulate-command-post-hook
              'nxhtmltest-should-no-mumamo-errors
              nil t)
    (load-file (ert-get-test-file-name "bug271497.el"))
    (ert-simulate-command '(bug271497-mumamo) t)
    ;;(font-lock-mode 1)
    (nxhtmltest-fontify-default-way 2 "trans")
    (ert-simulate-command '(goto-char 42) t)
    (message "after goto-char 42")
    (let ((ac42 after-change-functions)
          ac88)
      (ert-simulate-command '(goto-char 88) t)
      (message "after goto-char 88")
      (setq ac88 after-change-functions)
      (ert-should (not (equal ac88 ac42))))))

(ert-deftest nxhtml-ert-indent-question43320 ()
  "Test for question 43320 in Launchpad.
See URL `https://answers.launchpad.net/nxhtml/+question/43320'.

Note: This fails in Emacs 22, but should work in Emacs 23."
;; I did see some problem here:

;; - nXhtml 081222 + unpatched Emacs 081219 => ok
;; - nXhtml 081222 + unpatched Emacs 081124 => ok
;; - nXhtml 081222 + patched Emacs 081219 => ok

;; - nXhtml 081222 + patched Emacs 081124 => ok, but it fails if I
;;   use `nxhtmltest-run-Q'! I e, it fails if the autostart.el from
;;   the nxhtml dir in 081222 is used - but not if the copy in
;;   c:/EmacsW32 is used??? Which turned out to be if the old
;;   php-mode was used ...

  (ert-with-temp-buffer-include-file "question43320.html"
    (add-hook 'ert-simulate-command-post-hook
              'nxhtmltest-should-no-mumamo-errors
              nil t)
    (ert-simulate-command '(nxhtml-mumamo-mode) t)
    (font-lock-mode 1)
    (goto-line 25)  (ert-should (/= 14 (current-indentation)))
    (put 'mumamo-submode-indent-offset-0 'permanent-local t)
    (put 'mumamo-submode-indent-offset 'permanent-local t)
    ;;
    (set (make-local-variable 'mumamo-submode-indent-offset-0) nil)
    (set (make-local-variable 'mumamo-submode-indent-offset) nil)
    (ert-simulate-command '(mark-whole-buffer) t)
    (ert-simulate-command '(indent-for-tab-command) t)
    (goto-line 8)   (ert-should (= 8 (current-indentation)))
    (goto-line 9)   (ert-should (= 0 (current-indentation)))
    (goto-line 15)  (ert-should (= 8 (current-indentation)))
    (goto-line 16)  (ert-should (= 8 (current-indentation)))
    (goto-line 22)  (ert-should (= 6 (current-indentation)))
    (goto-line 25)  (ert-should (= 4 (current-indentation)))
    (goto-line 8) (indent-line-to 0)
    ;;(message "before indent-for-tab-command")
    (ert-simulate-command '(indent-for-tab-command) t)
    ;;(message "after indent-for-tab-command")
    (ert-should (= 8 (current-indentation)))
    ;;
    (set (make-local-variable 'mumamo-submode-indent-offset-0) 0)
    (set (make-local-variable 'mumamo-submode-indent-offset) 2)
    (ert-simulate-command '(mark-whole-buffer) t)
    (ert-simulate-command '(indent-for-tab-command) t)
    (goto-line 8)   (ert-should (= 8 (current-indentation)))
    (goto-line 9)   (ert-should (= 10 (current-indentation)))
    (goto-line 15)  (ert-should (= 8 (current-indentation)))
    (goto-line 16)  (ert-should (= 8 (current-indentation)))
    (goto-line 22)  (ert-should (= 16 (current-indentation)))
    (goto-line 25)  (ert-should (= 14 (current-indentation)))
    ))

(ert-deftest nxhtml-ert-only-php-no-end ()
  "Check for nXml error."
  (ert-with-temp-buffer-include-file "no-php-end-4.php"
    (nxhtml-mumamo-mode)
    (run-hooks 'after-change-major-mode-hook)
    (run-hooks 'post-command-hook)
    (nxhtmltest-fontify-default-way 2 "trans")
    (rng-validate-mode 1)
    ;;(rngalt-validate)
    (ert-should (eq rng-validate-mode t))
    (nxhtmltest-should-no-mumamo-errors)
    (nxhtmltest-should-no-nxml-errors)
    (goto-char 324)
    (message "before insert, after-change-functions local=%s" after-change-functions)
    (insert "\n")
    (nxhtmltest-should-no-mumamo-errors)
    (nxhtmltest-should-no-nxml-errors)))

(ert-deftest nxhtml-ert-xhtml-1.0-transitional ()
  "Test XHTML 1.0 Transitional with `nxhtml-mumamo-mode'.
NOTICE: This test SHOULD FAIL because there is currently no rng
schema for transitional. The schema for strict is used instead
and the file is invalid then."
  (ert-with-temp-buffer-include-file "lg-080813-label.html"
    (nxhtml-mumamo-mode)
    (nxhtmltest-fontify-default-way 2 "trans")
    (rng-validate-mode 1)
    (rngalt-validate)
    (ert-should (eq rng-validate-mode t))
    (nxhtmltest-should-no-mumamo-errors)
;;;     (ert-should
;;;      (not (eq (get-char-property 398 'category)
;;;               'rng-error)))
    (ert-should
     (eq (get-text-property 398 'face)
         'font-lock-function-name-face))
    (ert-should-not
     (= 0 rng-error-count))
    ))

(ert-deftest nxhtml-ert-genshi-valid-in-genshi ()
  (ert-with-temp-buffer-include-file "genshi-auto-mode.html"
    (message "\n")
    (genshi-nxhtml-mumamo-mode)
    (font-lock-mode 1)
    (mumamo-post-command)
    (ert-should (eq font-lock-mode t))
    (ert-should (eq major-mode 'nxhtml-genshi-mode))
    (ert-should
     (memq mumamo-multi-major-mode '(genshi-nxhtml-mumamo-mode
                                     genshi-html-mumamo-mode)))
    (nxhtmltest-fontify-default-way 2 "sheit")
    (rng-validate-mode 1)
    (rngalt-validate)
    (ert-should (eq rng-validate-mode t))
    (nxhtmltest-should-no-mumamo-errors)
    (ert-should
      (= 0 rng-error-count))))

(ert-deftest nxhtml-ert-genshi-invalid-in-nxhtml ()
  (ert-with-temp-buffer-include-file "genshi-auto-mode.html"
    (message "\n")
    (nxhtml-mumamo-mode)
    (nxhtmltest-fontify-default-way 2 "sheit")
    (font-lock-mode 1)
    (mumamo-post-command)
    (rng-validate-mode 1)
    (rngalt-validate)
    (ert-should (eq rng-validate-mode t))
    (nxhtmltest-should-no-mumamo-errors)
    (ert-should
      (= 2 rng-error-count))))

(ert-deftest nxhtml-ert-genshi-magic-mode ()
  "Test if genshi file is recognized."
  (let ((file1 (ert-get-test-file-name "genshi-auto-mode.html"))
        buf1)
    ;; Ensure we open the files
    (setq buf1 (find-buffer-visiting file1))
    (when buf1 (kill-buffer buf1))
    ;; Open file 1
    (setq buf1 (find-file file1))
    (nxhtmltest-fontify-default-way 2 "mod")
    (nxhtmltest-should-no-mumamo-errors)
    (ert-should
     (with-current-buffer buf1
       (memq mumamo-multi-major-mode '(genshi-nxhtml-mumamo-mode
                                       genshi-html-mumamo-mode))))
    (kill-buffer buf1)))

(ert-deftest nxhtml-ert-genshi-auto-mode ()
  "Test if file extension .ghtml is recognized."
  (let ((file1 (ert-get-test-file-name "genshi-HelloWorldPage.ghtml"))
        buf1)
    ;; Ensure we open the files
    (setq buf1 (find-buffer-visiting file1))
    (when buf1 (kill-buffer buf1))
    ;; Open file 1
    (setq buf1 (find-file file1))
    (nxhtmltest-fontify-default-way 2 "mod")
    (nxhtmltest-should-no-mumamo-errors)
    (ert-should
     (with-current-buffer buf1
       (memq mumamo-multi-major-mode '(genshi-nxhtml-mumamo-mode
                                       genshi-html-mumamo-mode))))
    (kill-buffer buf1)))

(ert-deftest nxhtml-ert-opened-modified ()
  "Test if buffer get modified when opening a file."
  (let ((file1 (ert-get-test-file-name "cvd-080805-ac.php"))
        (file2 (ert-get-test-file-name "cvd-080805-cc.php"))
        buf1
        buf2)
    ;; Ensure we open the files
    (setq buf1 (find-buffer-visiting file1))
    (when buf1 (kill-buffer buf1))
    (setq buf2 (find-buffer-visiting file2))
    (when buf2 (kill-buffer buf2))
    ;; Open file 1
    (setq buf1 (find-file file1))
    (nxhtmltest-fontify-default-way 2 "mod")
    (nxhtmltest-should-no-mumamo-errors)
    ;; Open file 2
    (setq buf2 (find-file file2))
    (nxhtmltest-fontify-default-way 2 "mod")
    (nxhtmltest-should-no-mumamo-errors)
    (ert-should
     (not (or (buffer-modified-p buf1)
              (buffer-modified-p buf2))))
    (kill-buffer buf1)
    (kill-buffer buf2)))

(ert-deftest nxhtml-ert-wiki-strange-hili-080629 ()
  "From a report on EmacsWiki."
  (ert-with-temp-buffer-include-file "wiki-strange-hili-080629.html"
    ;;(ert-should (not font-lock-mode))
    (nxhtml-mumamo-mode)
    ;;(ert-should (not font-lock-mode))
    (nxhtmltest-fontify-default-way 2 "hili")
    (goto-char 44)
    (nxhtmltest-should-no-mumamo-errors)
    (message "face at 44=%s" (get-text-property 44 'face))
    (ert-should
     (eq (get-text-property 44 'face)
         'font-lock-function-name-face))))

(ert-deftest nxhtml-ert-indent-wiki-080708-ind-problem ()
  (ert-with-temp-buffer-include-file "wiki-080708-ind-problem.rhtml"
    (require 'ruby-mode nil t)
    (if (not (featurep 'ruby-mode))
        ;; Fix-me: ert should maybe have some way to just display
        ;; informational messages?
        (error "ruby-mode not available, skipping test")
      ;;(ert-should (not font-lock-mode))
      (eruby-nxhtml-mumamo-mode)
      ;;(ert-should (not font-lock-mode))
      (nxhtmltest-fontify-default-way 2 "ind")
      (mark-whole-buffer)
      (indent-for-tab-command)
      (goto-line 3)
      (nxhtmltest-should-no-mumamo-errors)
      (ert-should (= (current-indentation) 0)))))

(ert-deftest nxhtml-ert-indent-wiki-080708-ind-problem-a ()
  "From a report on EmacsWiki.
NOTICE: This SHOULD FAIL. There is currently no support for the
kind of indentation needed here.

Notice 2: For some reason I sometimes get the error overlayp, nil
here."
  (ert-with-temp-buffer-include-file "wiki-080708-ind-problem.rhtml"
    (require 'ruby-mode nil t)
    (if (not (featurep 'ruby-mode))
        (error "ruby-mode not available, skipping test")
      ;;(ert-should (not font-lock-mode))
      (eruby-nxhtml-mumamo-mode)
      ;;(ert-should (not font-lock-mode))
      (nxhtmltest-fontify-default-way 2 "ind")
      (insert "  ")
      (mark-whole-buffer)
      (indent-for-tab-command)
      (goto-line 3)
      ;; Test
      (nxhtmltest-should-no-mumamo-errors)
      (ert-should-not (= (current-indentation) 2)))))

(ert-deftest nxhtml-ert-sheit-2007-12-26 ()
  (ert-with-temp-buffer-include-file "sheit-2007-12-26.php"
    ;;(ert-should (not font-lock-mode))
    (nxhtml-mumamo-mode)
    ;;(ert-should (not font-lock-mode))
    (nxhtmltest-fontify-default-way 2 "sheit")
    (nxhtmltest-should-no-mumamo-errors)
    (ert-should
     (and
      (eq (get-text-property 21 'face)
          'font-lock-comment-face)
      (eq (get-text-property 22 'face)
          'font-lock-comment-face)
      (eq (get-text-property 35 'face)
          'font-lock-comment-face)))))


;; Now some tests with a big file. Jumping backwards can fail.

(defun nxhtml-ert-nxhtml-changes-jump-back-2 (pos)
  ;;(ert-should (not font-lock-mode))
  (nxhtml-mumamo-mode)
  (run-hooks 'post-command-hook)
  ;;(ert-should (not font-lock-mode))
  (goto-char (- (point-max) (- 64036 63869)))
  (nxhtmltest-fontify-default-way 2)
  (nxhtmltest-should-no-mumamo-errors)
  (ert-should
   (eq (get-text-property (point) 'face)
       'font-lock-variable-name-face))
  (message "here 1")
  (goto-char pos)
  (nxhtmltest-fontify-default-way 2)
  (nxhtmltest-should-no-mumamo-errors)
  (message "here 2")
  (ert-should
   (eq (get-text-property (point) 'face)
       'font-lock-function-name-face)))

;; Fix-me: forgot to copy nxhtml-changes.html. I can't find any
;; similar error now.
;;
;; (ert-deftest nxhtml-ert-nxhtml-changes-jump-back-7014-2 ()
;;   "this is a docstring.
;; wonder how that works now ..."
;;   (ert-with-temp-buffer-include-file "../../nxhtml/doc/nxhtml-changes.html"
;;     (nxhtml-ert-nxhtml-changes-jump-back-2 7014)))

;; (ert-deftest nxhtml-ert-nxhtml-changes-jump-back-10488-2 ()
;;   (ert-with-temp-buffer-include-file "../../nxhtml/doc/nxhtml-changes.html"
;;     (nxhtml-ert-nxhtml-changes-jump-back-2 10488)))

;; (ert-deftest nxhtml-ert-nxhtml-changes-jump-2 ()
;;   (ert-with-temp-buffer-include-file "../../nxhtml/doc/nxhtml-changes.html"
;;     ;;(ert-should (not font-lock-mode))
;;     (nxhtml-mumamo-mode)
;;     ;;(ert-should (not font-lock-mode))
;;     (goto-char 10420)
;;     (nxhtmltest-fontify-default-way 2 "jump-2")
;;     (nxhtmltest-should-no-mumamo-errors)
;;     (ert-should
;;      (eq (get-text-property (point) 'face)
;;          'font-lock-variable-name-face))))

;;; Indentation tests

(ert-deftest nxhtml-ert-php-indent-bug-1 ()
  "Test indentation in php only file.
The indentation on line 7 should be 0."
  (ert-with-temp-buffer-include-file "only-php.php"
    (nxhtml-mumamo-mode)
    ;; No fontification needed for indentation.
    (goto-line 7)
    (indent-for-tab-command)
    (nxhtmltest-should-no-mumamo-errors)
    (ert-should
     (= 0
        (current-indentation)))))

;;; Scroll tests

;; (ert-deftest nxhtml-ert-scroll-jump-test ()
;;   "Test if `scroll-conservatively' eq 1 works."
;;   (ert-with-temp-buffer-include-file "../../nxhtml/doc/nxhtml-changes.html"
;;     (ert-should (not font-lock-mode))
;;     (nxhtml-mumamo-mode)
;;     (ert-should (not font-lock-mode))
;;     (nxhtmltest-fontify-default-way 2 "jump-2")
;;     (let ((scroll-conservatively 1)
;;           (ws (list (window-start)))
;;           (xi (loop for ii from 1 to 100 by 1
;;                     do
;;                     (next-line)
;;                     (sit-for 0.01)
;;                     collect (list (window-start)
;;                                   (let ((here (point)))
;;                                     (goto-char (window-start))
;;                                     (prog1 (line-end-position)
;;                                       (goto-char here)))
;;                                   (point))
;;                     ))
;;           (jumps 0)
;;           prev-win-start
;;           prev-win-start-le
;;           )
;;       (loop for xx in xi
;;             do
;;             (message "xx=%s" xx)
;;             (let ((win-start (nth 0 xx))
;;                   (win-start-le (nth 1 xx))
;;                   (cur-point (nth 2 xx)))
;;               (unless (or (not prev-win-start)
;;                           (= prev-win-start win-start)
;;                           (= (1+ prev-win-start-le) win-start))
;;                 (setq jumps (1+ jumps)))
;;               (setq prev-win-start win-start)
;;               (setq prev-win-start-le win-start-le)
;;               )
;;             )
;;       (ert-should (= 0 jumps))
;;       )))

;;(defvar ert-error-on-test-redefinition nil)

;;; End of test definitions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun nxhtmltest-run-ert ()
  "Run test with ert library."
  (setq ert-test-files-root nxhtmltest-files-root)
  (let ((selector "nxhtml-ert-"))
    (if noninteractive
        (ert-run-tests-batch selector)
      (ert-kill-temp-test-buffers)
      (ert-run-tests-interactively selector)
      (other-window 1)
      (ert-list-temp-test-buffers))))

;;;###autoload
(defun nxhtmltest-run-indent ()
  "Run indentation tests."
  (interactive)
  (setq ert-test-files-root nxhtmltest-files-root)
  (let ((selector "nxhtml-ert-indent-"))
    (ert-kill-temp-test-buffers)
    (nxhtmltest-get-fontification-method)
    (ert-run-tests-interactively selector))
  (other-window 1)
  (ert-list-temp-test-buffers))

;;;###autoload
(defun nxhtmltest-run ()
  "Run all tests defined for nXhtml.
Currently there are only tests using ert.el defined.

Note that it is currently expected that the following tests will
fail (they corresponds to known errors in nXhtml/Emacs):

  `nxhtml-ert-nxhtml-changes-jump-back-10549'
  `nxhtml-ert-nxhtml-changes-jump-back-7014'
"
  (interactive)
  (setq message-log-max t)
  (when (called-interactively-p)
    (nxhtmltest-get-fontification-method))
  (nxhtmltest-run-ert))

(when (getenv "nxhtmltest-run-Q")
  (nxhtmltest-run))

(provide 'nxhtmltest-suites)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; nxhtmltest-suites.el ends here
