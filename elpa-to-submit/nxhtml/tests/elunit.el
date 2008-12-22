;;; elunit.el --- Emacs Lisp Unit Testing framework

;; Modified by Lennart Borgman 2008-07-09 to make all global symbols
;; use the "ert-" prefix.

;; Copyright (C) 2006 - 2008 Phil Hagelberg

;; Author: Phil Hagelberg <technomancy@gmail.com>
;; URL: http://www.emacswiki.org/cgi-bin/wiki/ElUnit
;; Version: 1.1
;; Created: 2006-08-17
;; Keywords: unit test tdd
;; EmacsWiki: ElUnit

;; This file is NOT part of GNU Emacs.

;; Last-Updated: Fri Nov 16 16:23:06 2007 PST
;; By: Phil Hagelberg
;; Update #: 1

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING. If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; Inspired by regress.el by Wayne Mesard and Tom Breton, Test::Unit
;; by Nathaniel Talbott, and xUnit by Kent Beck

;; ElUnit exists to accomodate test-driven development of Emacs Lisp
;; programs.  Tests are divided up into suites.  Each test makes a
;; number of assertions to ensure that things are going according to
;; expected.

;; Tests are divided into suites for the purpose of hierarchical
;; structure and hooks.  The hierarchy allows suites to belong to
;; suites, in essence creating test trees.  The hooks are meant to
;; allow for extra setup that happens once per test, for both before
;; and after it runs.

;; You may use Emacs' built-in `assert' function for checking such
;; things, but the assertions at the bottom of this file provide much
;; better reporting if you use them.  Using `elunit-assert-that' is
;; preferred over built-in `assert'.

;;; Usage:

;; See http://www.emacswiki.org/cgi-bin/wiki/ElUnit for discussion and usage.
;; The file `elunit-test.el' contains meta-tests that you may find helpful
;; to refer to as samples.

;; Add the lines:
;; (make-local-variable 'after-save-hook)
;; (add-hook 'after-save-hook (lambda () (elunit "meta-suite")))
;; to the file containing your tests for convenient auto-running.

;; Unit tests are meant to test single low-level functions. If you
;; find yourself wanting to write higher-level tests, you may find
;; mode-unit.el (http://www.emacswiki.org/cgi-bin/wiki/ModeUnit)
;; useful as it is designed to help test whole Emacs modes.

;; TODO:

;; - allow test definitions to be nested in suites
;; - improve readability of failure reports
;; - store suites as a tree instead of a list?

;;; Code:

(eval-when-compile (require 'cl))
(require 'compile)

(defstruct elunit-test-suite name children tests setup-hooks teardown-hooks)
(defstruct elunit-test name body file line problem message)

(defface elunit-pass-face
  `((t (:background "green")))
  "Face for passing unit tests" :group 'elunit-faces)

(defface elunit-fail-face
  `((t (:background "red1")))
  "Face for failed unit tests" :group 'elunit-faces)

(defface elunit-error-face
  `((t (:background "chocolate1")))
  "Face for errored unit tests" :group 'elunit-faces)

(put 'elunit-test-failed 'error-conditions '(failure))

(defvar elunit-default-suite
  "default-suite"
  "Choice to use for default suite to run (gets updated to last suite run).")

(defvar elunit-suites (list (make-test-suite :name 'default-suite))
  "A list of every suite that's been defined.")

(defvar elunit-test-count 0)
(defvar elunit-failures nil
  "A list of tests that have failed.")

(defvar elunit-done-running-hook nil
  "Runs when the tests are finished; passed a total test count and a failure count.")

(defun elunit-clear-suites ()
  "Reset the internal suite list."
  (interactive)
  (setq elunit-suites (list (make-test-suite :name 'default-suite))))

(defun elunit-clear ()
  "Clear overlays from buffer."
  (interactive) (remove-overlays))

;;; Defining tests

(defmacro* elunit-defsuite (suite-name suite-ancestor &key setup-hooks teardown-hooks)
  "Define a suite, which may be hierarchical."
  `(let ((suite (make-test-suite :name ',suite-name
                                 :setup-hooks ,setup-hooks :teardown-hooks ,teardown-hooks)))
     (elunit-delete-suite ',suite-name)
     (if ',suite-ancestor
         (push suite (elunit-test-suite-children (elunit-get-suite ',suite-ancestor))))
     (add-to-list 'elunit-suites suite)
     suite))

(defun elunit-get-suite (suite)
  "Fetch a SUITE by its name."
  (if (elunit-test-suite-p suite)
      suite
    (find suite elunit-suites :test (lambda (suite asuite)
                                     (equal suite (elunit-test-suite-name asuite))))))

(defun elunit-delete-suite (name)
  "Remove a suite named NAME."
  ;; TODO: why doesn't delete work here?
  ;; (delete (elunit-get-suite name) elunit-suites))
  (setq elunit-suites (remove (elunit-get-suite name) elunit-suites)))

(defmacro elunit-deftest (name suite &rest body)
  "Define a test NAME in SUITE with BODY."
  (save-excursion
    ;; TODO: Use backtrace info to get line number
    (search-backward (concat "elunit-deftest " (symbol-name name)) nil t)
    (let ((line (line-number-at-pos))
          (file buffer-file-name)
          (suite-sym (gensym)))
      `(let ((,suite-sym (elunit-get-suite ',suite)))
         ;; not a foolproof heuristic to get line number, but good enough.
         (elunit-delete-test ',name ,suite-sym)
         (push (make-test :name ',name :body (lambda () ,@body)
                                       :file ,file :line ,line)
               (elunit-test-suite-tests ,suite-sym))))))

(defun elunit-get-test (name suite)
  "Return a test given a NAME and SUITE."
  (if (elunit-test-p name) name
    (find name (elunit-test-suite-tests (elunit-get-suite suite))
          :test (lambda (name test) (equal name (elunit-test-name test))))))

(defun elunit-delete-test (name suite)
  "Delete test named NAME in SUITE."
  (let ((suite (elunit-get-suite suite)))
    (setf (elunit-test-suite-tests suite)
          (delete (elunit-get-test name suite) (elunit-test-suite-tests suite)))))

(defun elunit-total-test-count (suite)
  "Return the total number of tests in a SUITE."
  (let ((suite (elunit-get-suite suite)))
    (if suite
        (+ (apply #'+ (elunit-total-test-count (elunit-test-suite-children suite)))
           (length (elunit-test-suite-tests suite))))))

(defun elunit-test-docstring (test)
  "Return a TEST's docstring."
  (if (equal (car (elunit-test-body test)) 'lambda)
      (if (stringp (caddr (elunit-test-body test)))
          (caddr (elunit-test-body test))
        "")))

;;; Running the tests

(defun elunit (suite)
  "Ask for a single SUITE, run all its tests, and display the results."
  (interactive (list (completing-read
		      (concat "Run test suite (default " elunit-default-suite "): " )
		      (mapcar (lambda (suite) (symbol-name (elunit-test-suite-name suite)))
			      elunit-suites) nil t nil nil elunit-default-suite)))
  
  (elunit-run-suite (elunit-get-suite (intern suite)))
  (message "%d tests with %d problems." elunit-test-count (length elunit-failures)))

(defun elunit-run-suite (suite)
  "Run a SUITE's tests and children."
  (setq elunit-default-suite (symbol-name (elunit-test-suite-name suite))
	elunit-test-count 0
	elunit-failures nil)

  (dolist (test (reverse (elunit-test-suite-tests suite)))
    (if (elunit-test-suite-setup-hooks suite) (apply #'funcall (elunit-test-suite-setup-hooks suite)))
    (elunit-run-test test)
    (if (elunit-test-suite-teardown-hooks suite) (apply #'funcall (elunit-test-suite-teardown-hooks suite))))
  (dolist (child-suite (elunit-test-suite-children suite))
    (elunit-run-suite child-suite))
  (run-hook-with-args 'elunit-done-running-hook elunit-test-count (length elunit-failures)))

(defun elunit-run-test (test)
  "Run a single `TEST'."
  (condition-case err
      (progn
        (incf elunit-test-count)
        (funcall (elunit-test-body test))
	(elunit-highlight-test test 'elunit-pass-face))
    (failure
     (elunit-failure test err 'elunit-fail-face))
    (error
     (elunit-failure test err 'elunit-error-face))))

(defun elunit-failure (test err face)
  "Record a failing TEST and store ERR info."
  (setf (elunit-test-problem test) err
	(elunit-test-message test) (or (cadr err) (format "%s" err)))
  (push test elunit-failures)
  (elunit-highlight-test test face))

(defun elunit-highlight-test (test face)
  (save-excursion
    ;; (switch-to-buffer (file-name-nondirectory (elunit-test-file test)))
    (goto-line (elunit-test-line test))
    (beginning-of-line)
    (let ((line-start (point)))
      (end-of-line)
      (overlay-put (make-overlay line-start (point)) 'face face))))

(defun elunit-explain-problem ()
  "Display a message explaining the problem with the test at point."
  (interactive)
  (save-excursion
    (search-backward-regexp "(elunit-deftest \\([-a-z]+\\) \\([-a-z]+\\)" nil t)
    (if (and (match-string 1) (match-string 2))
	(elunit-test-message (elunit-get-test (intern (match-string 1))
				       (intern (match-string 2)))))))

;;; Helper functions

(defmacro elunit-with-test-buffer (&rest body)
  "Execute BODY in a test buffer named `*elunit-output*'."
  `(save-excursion
     (switch-to-buffer "*elunit-output*")
     ,@body
     (kill-buffer "*elunit-output*")))

(defun elunit-fail (&rest args)
  "Signal a test failure in a way that elunit understands.
Takes the same ARGS as `error'."
    (signal 'elunit-test-failed (list (apply 'format args))))

(font-lock-add-keywords 'emacs-lisp-mode
			;; Make elunit tests look like defuns.
			'(("elunit-defsuite"   . 'font-lock-keyword-face)
			  ("elunit-deftest"    . 'font-lock-keyword-face)
			  ("\\<fail\\>" . 'font-lock-warning-face)))

;;; General assertions

;; These are preferred over stuff like (assert (equal [...] because
;; they use the `fail' function, which reports errors nicely.

(defun elunit-assert-that (actual)
  "Fails if ACTUAL is nil."
  (unless actual
    (elunit-fail "%s expected to be non-nil" actual)))

(defun elunit-assert-nil (actual)
  "Fails if ACTUAL is non-nil."
  (when actual
    (elunit-fail "%s expected to be nil" actual)))

(defun elunit-assert-equal (expected actual)
  "Fails if EXPECTED is not equal to ACTUAL."
  (unless (equal expected actual)
    (elunit-fail "%s expected to be %s" actual expected)))

(defun elunit-assert-not-equal (expected actual)
  "Fails if EXPECTED is equal to ACTUAL."
  (when (equal expected actual)
    (elunit-fail "%s expected to not be %s" actual expected)))

(defun elunit-assert-member (elt list)
  "Fails if ELT is not a member of LIST."
  (unless (member elt list)
    (elunit-fail "%s expected to include %s" list elt)))

(defun elunit-assert-match (regex string)
  "Fails if REGEX does not match STRING."
  (unless (string-match regex string)
    (elunit-fail "%s expected to match %s" string regex)))

(defmacro elunit-assert-error (&rest body)
  "Fails if BODY does not signal an error."
  `(condition-case err
       (progn
	 ,@body
	 (elunit-fail "%s expected to signal an error" body))
     (error t)))

(defmacro elunit-assert-changed (form &rest body)
  "Fails if FORM does not return a different value after BODY is evaled."
  `(elunit-assert-not-equal (eval ,form)
		     (progn
		       ,@body
		       (eval ,form))))

(defmacro elunit-assert-not-changed (form &rest body)
  "Fails if FORM returns a different value after BODY is evaled."
  `(elunit-assert-equal (eval ,form)
		     (progn
		       ,@body
		       (eval ,form))))

(provide 'elunit)

;;; elunit.el ends here
