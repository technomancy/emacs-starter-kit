;;; ert.el --- Emacs Lisp Regression Testing

;; Modified by Lennart Borgman 2008-07-13 to make all global symbols
;; use the "ert-" prefix.

;; Copyright (C) 2007, 2008 Christian M. Ohler

;; Author: Christian M. Ohler
;; Version: 0.2
;; Keywords: lisp, tools

;; This file is NOT part of GNU Emacs.

;; This program is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see `http://www.gnu.org/licenses/'.

;;; Commentary:

;; ERT is a tool for automated testing in Emacs Lisp.  Its main
;; features are facilities for defining and running test cases and
;; reporting the results as well as for debugging test failures
;; interactively.
;;
;; The main entry points are `ert-deftest', which is similar to
;; `defun' but defines a test, and `ert-run-tests-interactively',
;; which runs tests and offers an interactive interface for inspecting
;; results and debugging.  There is also `ert-run-tests-batch' for
;; non-interactive use.
;;
;; The body of `ert-deftest' forms resembles a function body, but the
;; additional operators `should', `should-not' and `should-error' are
;; available.  `should' is similar to cl's `assert', but signals a
;; different error when its condition is violated that is caught and
;; processed by ERT.  In addition, it analyzes its argument form and
;; records information that helps debugging (`assert' tries to do
;; something similar when its second argument SHOW-ARGS is true, but
;; `should' is more sophisticated).  For information on `should-not'
;; and `should-error', see their docstrings.
;;
;; For example,
;;
;;     ;; Define a test named `foo'.
;;     (ert-deftest foo ()
;;       (ert-should (= (+ 1 2) 4)))
;;
;;     ;; Run it.
;;     (ert-run-tests-interactively 'foo)
;;
;; generates the following output (in addition to some statistics) in
;; the *ert* results buffer:
;;
;;     F foo
;;         (ert-test-failed
;;          ((ert-should
;;            (=
;;             (+ 1 2)
;;             4))
;;           :form
;;           (= 3 4)
;;           :value nil))
;;
;; This indicates that the test failed.  The `should' form that failed
;; was (ert-should (= (+ 1 2) 4)), because its inner form, after
;; evaluation of its arguments, was the function call (= 3 4), which
;; returned nil.
;;
;; Obviously, this is a bug in the test case, not in the functions `+'
;; or `='.  In the results buffer, with point on the test result, the
;; key "." can be used to jump to the definition of the test to modify
;; it to correct the bug.  After evaluating the modified definition
;; and switching back to the results buffer, the key "r" will re-run
;; the test and show the new result.


;; Test selectors
;;
;; Functions like `ert-run-tests-interactively' accept a test
;; selector, which is a Lisp expression specifying a set of tests.
;; Each test name is a selector that refers to that test, the selector
;; `t' refers to all tests, and the selector `:failed' refers to all
;; tests that failed; but more complex selectors are available.  Test
;; selector syntax is similar to cl's type specifier syntax.  See the
;; docstring of `ert-select-tests' for details.


;; Comparison with other testing tools
;;
;; ERT allows test-driven development similar to *Unit frameworks for
;; other languages.  However, two common *Unit features are notably
;; absent from ERT: fixtures and test suites.
;;
;; Fixtures, as used e.g. in SUnit or JUnit, have two main purposes:
;; Setting up (and tearing down) an environment for a set of test
;; cases, and making that environment accessible through object
;; attributes that can be used like local variables.
;;
;; While fixtures are a great syntactic simplification in other
;; languages, they are not very useful in Lisp, where higher-order
;; functions and `unwind-protect' are available.  One way to implement
;; and use a fixture in ERT is
;;
;;    (defun my-fixture (body)
;;      (unwind-protect
;;          (progn ...set up...
;;                 (funcall body))
;;        ...tear down...))
;;
;;    (ert-deftest my-test ()
;;      (my-fixture
;;       (lambda ()
;;         ...test code...)))
;;
;; (Another way would be a `with-my-fixture' macro.)  This solves the
;; set-up and tear-down part, and additionally allows any test case to
;; use any combination of fixtures, so it is more general than what
;; other tools typically allow.
;;
;; If the test case needs access to the environment the fixture sets
;; up, the fixture can be modified to pass arguments to the body.
;;
;; These are standard Lisp idioms.  Special syntax for them could be
;; added easily enough, but would provide only a minor simplification.
;;
;; (Note that splitting set-up and tear-down into separate functions,
;; like *Unit tools usually do, makes it impossible to establish
;; dynamic `let' bindings as part of the fixture.  So, blindly
;; imitating the way fixtures are implemented in other languages would
;; be counter-productive in Lisp.)
;;
;;
;; The purpose of test suites is to group related test cases together.
;; The most common use of this is to run just the tests for one
;; particular module.  Since symbol prefixes are the usual way of
;; separating module namespaces in Emacs Lisp, test selectors already
;; solve this by allowing regexp matching on test names; e.g., the
;; selector "^ert-" selects ERT's self-tests.
;;
;; If test suites containing arbitrary sets of tests are found to be
;; desirable, it would be easy to add a `define-test-selector'
;; mechanism that introduces a new selector, defined in terms of
;; existing ones; e.g.
;;
;;     ;; Note that `define-test-selector' does not exist yet.
;;     (define-test-selector my-test-suite () `(member foo-test bar-test))
;;
;; would define a test suite named `my-test-suite' consisting of
;; `foo-test' and `bar-test'.  See also `deftype' in Common Lisp.


;; TODO: Add `skip' feature for tests that can't run in current environment.


;;; Code:

(eval-when-compile (require 'cl))
(require 'ewoc)
(require 'find-func)
(require 'debug)

(defvar ert-debug-on-error nil
  "Non-nil means enter debugger when a test fails or terminates with an error.")


;;; Defining and locating tests.

;; The data structure that represents a test case.
(defstruct ert-test
  (name nil)
  (documentation nil)
  (body (assert nil))
  (most-recent-result nil)
  (expected-result-type 'ert-test-passed))

(defun ert-test-boundp (symbol)
  "Return non-nil if SYMBOL names a test."
  (and (get symbol 'ert-test) t))

(defun ert-get-test (symbol)
  "If SYMBOL names a test, return that.  Signal an error otherwise."
  (assert (ert-test-boundp symbol) t)
  (get symbol 'ert-test))

(defun ert-set-test (symbol doc definition)
  "Make SYMBOL name the test DEFINITION, and return DEFINITION."
  (when doc
    (put symbol 'ert-test-documentation doc))
  (put symbol 'ert-test definition)
  definition)

(defun ert-make-test-unbound (symbol)
  "Make SYMBOL name no test.  Return SYMBOL."
  (remprop symbol 'ert-test)
  symbol)

(defun ert-test-result-expected-p (test result)
  "Return non-nil if RESULT matches the expected result type for TEST."
  (typep result (ert-test-expected-result-type test)))

(defvar ert-find-test-regexp
  (concat "^\\s-*(ert-deftest"
          find-function-space-re
          "%s\\(\\s-\\|$\\)")
  "The regexp the `find-function' mechanisms use for locating test definitions.")

(eval-and-compile
  (defun ert-parse-keys-and-body (docstr keys-and-body)
    "Split KEYS-AND-BODY into keyword-and-value pairs and the remaining body.

KEYS-AND-BODY should have the form of a property list, with the
exception that only keywords are permitted as keys and that the
tail -- the body -- is a list of forms that does not start with a
keyword.

Returns a two-element list containing the keys-and-values plist
and the body."
    (unless (stringp docstr)
      (when docstr
        (setq keys-and-body (cons docstr keys-and-body))
        (setq docstr nil)))
    (let ((extracted-key-accu '())
          (remaining keys-and-body))
      (while (and (consp remaining) (keywordp (first remaining)))
        (let ((keyword (pop remaining)))
          (unless (consp remaining)
            (error "Value expected after keyword %S in %S"
                   keyword keys-and-body))
          (when (assoc keyword extracted-key-accu)
            (warn "Keyword %S appears more than once in %S" keyword
                  keys-and-body))
          (push (cons keyword (pop remaining)) extracted-key-accu)))
      (setq extracted-key-accu (nreverse extracted-key-accu))
      (list (loop for (key . value) in extracted-key-accu
                  collect key
                  collect value)
            docstr
            remaining))))

(defvar ert-error-on-test-redefinition nil)

;;;###autoload
(defmacro* ert-deftest (name ()
                             &optional docstr
                             &body keys-and-body)
  "Define NAME (a symbol) as a test.

\(fn NAME () [:documentation DOCSTRING] [:expected-result TYPE] BODY...)"
  ;; The :documentation would be unreadable.  I have therefore added
  ;; docstr that will look like documentation use to in Emacs.  Maybe
  ;; add function ert-describe-test?
  (declare (indent 2)
           (debug (&define :name test name sexp
                           [&optional [":documentation" stringp]]
                           [&optional [":expected-result" sexp]]
                           def-body)))
  (destructuring-bind ((&key (expected-result nil expected-result-supplied-p)
                             (documentation nil documentation-supplied-p))
                       doc
                       body)
      (ert-parse-keys-and-body docstr keys-and-body)
    `(progn
       ;; Guard against missing/badly named tests:
       (when (and ert-error-on-test-redefinition
                  (symbolp ',name)
                  (get ',name 'ert-test))
         (with-output-to-temp-buffer "*Ert Error*"
           (with-current-buffer "*Ert Error*"
             (insert "Test "
                     (format "%s" ',name)
                     " is already defined in "
                     (format "%s" (find-definition-noselect ',name 'ert-deftest))
                     "\n\n"
                     "Tip: Use `ert-delete-all-tests' or `ert-delete-test' before redefining tests."
                     )))
         (if (y-or-n-p "Do you want to call ert-delete-all-tests and then continue? ")
             ;; Fix-me: This does not work, why?
             (ert-delete-all-tests)
           (error "Test %s is already defined in %s"
                  ',name
                  (find-definition-noselect ',name 'ert-deftest))))
       (ert-set-test ',name
                     nil ;;doc
                     (make-ert-test
                      :name ',name
                      :body (lambda () ,@body)
                      ,@(when expected-result-supplied-p
                          `(:expected-result-type ,expected-result))
                      ,@(when documentation-supplied-p
                          `(:documentation ,documentation))))
       ;; This hack allows `symbol-file' to associate `ert-deftest'
       ;; forms with files, and therefore enables `find-function' to
       ;; work with tests.  However, it leads to warnings in
       ;; `unload-feature', which doesn't know how to undefine tests
       ;; and has no mechanism for extension.
       (push '(ert-deftest . ,name) current-load-list)
       ',name)))

(defun ert-read-test-name (prompt &optional default-value history)
  "Read the name of a test and return it as a symbol.
Prompt with PROMPT.  By default, return DEFAULT-VALUE."
  (when (symbolp default-value) (setq default-value (symbol-name default-value)))
  (intern (completing-read prompt obarray #'ert-test-boundp
                           t nil history default-value nil)))

(defun ert-find-test-other-window (test-name)
  "Find, in another window, the definition of TEST-NAME."
  (interactive (list (ert-read-test-name "Find test definition: ")))
  (find-function-do-it test-name 'ert-deftest 'switch-to-buffer-other-window))

(defun ert-delete-test (test-name)
  "An interactive interface to `ert-make-test-unbound'."
  (interactive (list (let ((default (thing-at-point 'symbol)))
                       (when default
                         (set-text-properties 0 (length default) nil default)
                         (when (or (string= default "nil") (intern-soft default))
                           (setq default (intern default)))
                         (unless (ert-test-boundp default)
                           (setq default nil)))
                       (completing-read (if (null default)
                                            "Delete test: "
                                          (format "Delete test (default %s): "
                                                  default))
                                        obarray #'ert-test-boundp
                                        'really-require-match
                                        nil nil default nil))))
  (ert-make-test-unbound test-name))

(defun ert-delete-all-tests ()
  "Make all symbols in `obarray' name no test."
  (interactive)
  (when (interactive-p)
    (unless (y-or-n-p "Delete all tests? ")
      (error "Aborted")))
  (mapc #'ert-delete-test (mapcar #'ert-test-name (ert-select-tests t t)))
  t)


(defun ert-make-end-marker (buffer must-exist)
  "Return a marker to the end of buffer BUFFER.
BUFFER may be a string or a buffer. If BUFFER does not exist
return nil.

The buffer must exist if MUST-EXIST is non-nil.

See also:
 `ert-end-of-messages'
 `ert-end-of-warnings'"
  (let ((buf (if must-exist
                 (get-buffer buffer)
               (get-buffer-create buffer))))
    (when (and buf
               (bufferp buf)
               (buffer-live-p buf))
      (with-current-buffer buf
        (save-restriction
          (widen)
          (point-max-marker))))))

(defun ert-end-of-messages ()
  "Return a marker to the end of *Messages* buffer."
  (ert-make-end-marker "*Messages*" nil))

(defun ert-end-of-warnings ()
  "Return a marker to the end of *Warnings* buffer."
  (ert-make-end-marker "*Warnings*" nil))

(defun ert-search-after (after regexp)
  "Search after marker in AFTER for regular expression REGEXP.
Return a alist of position and matches.  AFTER should have been
created with `ert-make-end-marker'.

This is supposed to be used for messages and trace buffers.

See also
 `ert-get-messages'"
  (let ((buf (marker-buffer after)))
    (with-current-buffer buf
      (let ((here (point))
            res)
        (goto-char after)
        (save-match-data
          (while (re-search-forward regexp nil t)
            (setq res (cons (match-data) res))))
        (goto-char here)
        (reverse res)))))
;; fix-me: add a conventient way to look at the result of
;; `ert-search-after'. Probably this means adding something more to
;; the returned result.

(defun ert-get-messages (regexp)
  "Search *Messages* buffer for regular expression REGEXP.
This should be used within `ert-deftest'.  Search begins where
the buffer ended when test started.

See also:
 `ert-get-warnings'
 `ert-search-after'"
  (ert-search-after messages-mark regexp))

(defun ert-get-warnings (regexp)
  "Search *Warnings* buffer for regular expression REGEXP.
See `ert-get-messages' for more information."
  (ert-search-after messages-mark regexp))


;;; Test selectors.

(defun ert-select-tests (selector universe)
  "Select, from UNIVERSE, a set of tests according to SELECTOR.

UNIVERSE should be a list of tests, or t, which refers to all
tests named by symbols in `obarray'.

Returns the set of tests as a list.

Valid selectors:

nil -- Selects the empty set.
t -- Selects UNIVERSE.
:new -- Selects all tests that have not been run yet.
:failed, :passed, :error -- Select tests according to their most recent result.
:expected, :unexpected -- Select tests according to their most recent result.
a string -- Selects all tests that have a name that matches the string, a regexp.
a test -- Selects that test.
a symbol -- Selects the test that the symbol names, errors if none.
\(member TESTS...\) -- Selects TESTS, a list of tests or symbols naming tests.
\(eql TEST\) -- Selects TEST, a test or a symbol naming a test.
\(and SELECTORS...\) -- Selects the tests that match all SELECTORS.
\(or SELECTORS...\) -- Selects the tests that match any SELECTOR.
\(not SELECTOR\) -- Selects all tests that do not match SELECTOR.
\(satisfies PREDICATE\) -- Selects all tests that satisfy PREDICATE.

Only selectors that require a superset of tests, such
as (satisfies ...), strings, :new, etc. make use of UNIVERSE.
Selectors that do not, such as \(member ...\), just return the
set implied by them without checking whether it is really
contained in UNIVERSE."
  ;; This code needs to match the etypecase in
  ;; `ert-insert-human-readable-selector'.
  (etypecase selector
    ((member nil) nil)
    ((member t) (etypecase universe
                  (list universe)
                  ((member t) (ert-select-tests "" universe))))
    ((member :new) (ert-select-tests
                    `(satisfies ,(lambda (test)
                                   (typep (ert-test-most-recent-result test)
                                          'null)))
                    universe))
    ((member :failed) (ert-select-tests
                       `(satisfies ,(lambda (test)
                                      (typep (ert-test-most-recent-result test)
                                             'ert-test-failed)))
                       universe))
    ((member :passed) (ert-select-tests
                       `(satisfies ,(lambda (test)
                                      (typep (ert-test-most-recent-result test)
                                             'ert-test-passed)))
                       universe))
    ((member :error) (ert-select-tests
                      `(satisfies ,(lambda (test)
                                     (typep (ert-test-most-recent-result test)
                                            'ert-test-error)))
                      universe))
    ((member :expected) (ert-select-tests
                         `(satisfies
                           ,(lambda (test)
                              (ert-test-result-expected-p
                               test
                               (ert-test-most-recent-result test))))
                         universe))
    ((member :unexpected) (ert-select-tests `(not :expected) universe))
    (string
     (etypecase universe
       ((member t) (mapcar #'ert-get-test
                           (apropos-internal selector #'ert-test-boundp)))
       (list (remove-if-not (lambda (test)
                              (and (ert-test-name test)
                                   (string-match selector (ert-test-name test))))
                            universe))))
    (ert-test (list selector))
    (symbol
     (assert (ert-test-boundp selector))
     (list (ert-get-test selector)))
    (cons
     (destructuring-bind (operator &rest operands) selector
       (ecase operator
         (member
          (mapcar (lambda (purported-test)
                    (etypecase purported-test
                      (symbol (assert (ert-test-boundp purported-test))
                              (ert-get-test purported-test))
                      (ert-test purported-test)))
                  operands))
         (eql
          (assert (eql (length operands) 1))
          (ert-select-tests `(member ,@operands) universe))
         (and
          ;; Do these definitions of AND, NOT and OR satisfy de
          ;; Morgan's rules?  Should they?
          (case (length operands)
            (0 (ert-select-tests 't universe))
            (t (ert-select-tests `(and ,@(rest operands))
                                 (ert-select-tests (first operands) universe)))))
         (not
          (assert (eql (length operands) 1))
          (set-difference (ert-select-tests 't universe)
                          (ert-select-tests (first operands) universe)))
         (or
          (case (length operands)
            (0 (ert-select-tests 'nil universe))
            (t (union (ert-select-tests (first operands) universe)
                      (ert-select-tests `(or ,@(rest operands)) universe)))))
         (satisfies
          (assert (eql (length operands) 1))
          (remove-if-not (first operands) (ert-select-tests 't universe))))))))

(defun ert-insert-human-readable-selector (selector)
  "Insert a human-readable presentation of SELECTOR into the current buffer."
  ;; This is needed to avoid printing the (huge) contents of the
  ;; `backtrace' slot of the result objects in the
  ;; `most-recent-result' slots of test case objects in (eql ...) or
  ;; (member ...) selectors.
  (labels ((rec (selector)
             ;; This code needs to match the etypecase in `ert-select-tests'.
             (etypecase selector
               ((or (member nil t
                            :new :failed :passed :error
                            :expected :unexpected)
                    string
                    symbol)
                selector)
               (ert-test
                (if (ert-test-name selector)
                    (make-symbol (format "<%S>" (ert-test-name selector)))
                  (make-symbol "<unnamed test>")))
               (cons
                (destructuring-bind (operator &rest operands) selector
                  (ecase operator
                    ((member eql and not or)
                     `(,operator ,@(mapcar #'rec operands)))
                    (satisfies
                     selector)))))))
    (insert (format "%S" (rec selector)))))


;;; Running tests.

(put 'ert-test-failed 'error-conditions '(error ert-test-failed))
(put 'ert-test-failed 'error-message "Test failed")

(defun ert-pass ()
  "Terminate the current test and mark it passed.  Does not return."
  (throw 'ert-pass nil))

(defun ert-fail (data)
  "Terminate the current test and mark it failed.  Does not return.
DATA is displayed to the user and should state the reason of the failure."
  (signal 'ert-test-failed (list data)))

;; The data structures that represent the result of running a test.
(defstruct ert-test-result
  (messages nil)
  )
(defstruct (ert-test-passed (:include ert-test-result)))
(defstruct (ert-test-result-with-condition (:include ert-test-result))
  (condition (assert nil))
  (backtrace (assert nil)))
(defstruct (ert-test-error (:include ert-test-result-with-condition)))
(defstruct (ert-test-quit (:include ert-test-result-with-condition)))
(defstruct (ert-test-failed (:include ert-test-result-with-condition)))
(defstruct (ert-test-aborted-with-non-local-exit (:include ert-test-result)))


(defun ert-record-backtrace ()
  "Record the current backtrace (as a list) and return it."
  ;; Since the backtrace is stored in the result object, result
  ;; objects must only be printed with appropriate limits
  ;; (`print-level' and `print-length') in place.  For interactive
  ;; use, the cost of ensuring this possibly outweighs the advantage
  ;; of storing the backtrace for
  ;; `ert-results-pop-to-backtrace-for-test-at-point' given that we
  ;; already have `ert-results-rerun-test-debugging-errors-at-point'.
  ;; For batch use, however, printing the backtrace may be useful.
  (loop
   ;; 6 is the number of frames our own debugger adds (when
   ;; compiled; more when interpreted).  FIXME: Need to describe a
   ;; procedure for determining this constant.
   for i from 6
   for frame = (backtrace-frame i)
   while frame
   collect frame))

;; A container for the state of the execution of a single test and
;; environment data needed during its execution.
(defstruct ert-test-execution-info
  (test (assert nil))
  (result (assert nil))
  ;; A thunk that may be called when RESULT has been set to its final
  ;; value and test execution should be terminated.  Should not
  ;; return.
  (exit-continuation (assert nil))
  ;; The binding of `debugger' outside of the execution of the test.
  next-debugger
  ;; The binding of `ert-debug-on-error' that is in effect for the
  ;; execution of the current test.  We store it to avoid being
  ;; affected by any new bindings the test itself may establish.  (I
  ;; don't remember whether this feature is important.)
  ert-debug-on-error)

(defun ert-run-test-debugger (info debugger-args)
  "The function that `debugger' is bound to during the execution of tests.

Records failures and errors and either terminates the test
silently or calls the interactive debugger, as appropriate."
  (destructuring-bind (first-debugger-arg &rest more-debugger-args) debugger-args
    (ecase first-debugger-arg
      ((lambda debug t exit nil)
       (apply (ert-test-execution-info-next-debugger info) debugger-args))
      (error
       (let* ((condition (first more-debugger-args))
              (type (case (car condition)
                      ((quit) 'quit)
                      ((ert-test-failed) 'failed)
                      (otherwise 'error)))
              (backtrace (ert-record-backtrace)))
         (setf (ert-test-execution-info-result info)
               (ecase type
                 (quit
                  (make-ert-test-quit :condition condition
                                      :backtrace backtrace))
                 (failed
                  (make-ert-test-failed :condition condition
                                        :backtrace backtrace))
                 (error
                  (make-ert-test-error :condition condition
                                       :backtrace backtrace))))
         ;; Work around Emacs' heuristic (in eval.c) for detecting
         ;; errors in the debugger.
         (incf num-nonmacro-input-events)
         ;; FIXME: We should probably implement more fine-grained
         ;; control a la non-t `debug-on-error' here.
         (cond
          ((ert-test-execution-info-ert-debug-on-error info)
           (apply (ert-test-execution-info-next-debugger info) debugger-args))
          (t))
         (funcall (ert-test-execution-info-exit-continuation info)))))))

(defun ert-run-test-internal (ert-test-execution-info)
  (lexical-let ((info ert-test-execution-info))
    (setf (ert-test-execution-info-next-debugger info) debugger
          (ert-test-execution-info-ert-debug-on-error info) ert-debug-on-error)
    (catch 'ert-pass
      ;; For now, each test gets its own temp buffer and its own
      ;; window excursion, just to be safe.  If this turns out to be
      ;; too expensive, we can remove it.
      (with-temp-buffer
        (save-window-excursion
          (let ((debugger (lambda (&rest debugger-args)
                            (ert-run-test-debugger info debugger-args)))
                (debug-on-error t)
                (debug-on-quit t)
                ;; FIXME: Do we need to store the old binding of this
                ;; and consider it in `ert-run-test-debugger'?
                (debug-ignored-errors nil)
                (messages-mark (ert-end-of-messages))
                (warnings-mark (ert-end-of-warnings)))
            (funcall (ert-test-body (ert-test-execution-info-test info))))))
      (ert-pass))
    (setf (ert-test-execution-info-result info) (make-ert-test-passed)))
  nil)

(defun ert-make-marker-in-messages-buffer ()
  (with-current-buffer (get-buffer-create "*Messages*")
    (set-marker (make-marker) (point-max))))

(defun ert-force-message-log-buffer-truncation ()
  (with-current-buffer (get-buffer-create "*Messages*")
    ;; This is a reimplementation of this part of message_dolog() in xdisp.c:
    ;; if (NATNUMP (Vmessage_log_max))
    ;;   {
    ;;     scan_newline (Z, Z_BYTE, BEG, BEG_BYTE,
    ;;                   -XFASTINT (Vmessage_log_max) - 1, 0);
    ;;     del_range_both (BEG, BEG_BYTE, PT, PT_BYTE, 0);
    ;;   }
    (when (and (integerp message-log-max) (>= message-log-max 0))
      (let ((begin (point-min))
            (end (save-excursion
                   (goto-char (point-max))
                   (forward-line (- message-log-max))
                   (point))))
        (delete-region begin end)))))

(defun ert-run-test (test)
  "Run TEST.  Return the result and store it in TEST's `most-recent-result' slot."
  (setf (ert-test-most-recent-result test) nil)
  (block error
    (lexical-let* ((begin-marker (ert-make-marker-in-messages-buffer))
                   (info (make-ert-test-execution-info
                         :test test
                         :result (make-ert-test-aborted-with-non-local-exit)
                         :exit-continuation (lambda ()
                                              (return-from error nil)))))
      (unwind-protect
          (let ((message-log-max t))
            (ert-run-test-internal info))
        (let ((result (ert-test-execution-info-result info)))
          (setf (ert-test-result-messages result)
                (with-current-buffer (get-buffer-create "*Messages*")
                  (buffer-substring begin-marker (point-max))))
          (ert-force-message-log-buffer-truncation)
          (setf (ert-test-most-recent-result test) result)))))
  (ert-test-most-recent-result test))


;;; The `should' macros.

(eval-and-compile
  (defun ert-special-operator-p (thing)
    "Return non-nil if THING is a symbol naming a special operator."
    (and (symbolp thing)
         (let ((definition (indirect-function thing t)))
           (and (subrp definition)
                (eql (cdr (subr-arity definition)) 'unevalled)))))
  (defun ert-expand-should (whole form env inner-expander)
    "Helper function for the `should' macro and its variants.

Analyzes FORM and produces an expression that has the same
semantics under evaluation but records additional debugging
information.  INNER-EXPANDER adds the actual checks specific to
the particular variant of `should'."
    (let ((form (macroexpand form env)))
      ;; It's sort of a wart that `inner-expander' can't influence the
      ;; value the expansion returns.
      (cond
       ((atom form)
        (funcall inner-expander form `(list ',whole :form ',form :value ,form)))
       ((ert-special-operator-p (car form))
        (let ((value (gensym "value-")))
          `(let ((,value (make-symbol "ert-form-evaluation-aborted")))
             ,(funcall inner-expander
                       `(setq ,value ,form)
                       `(list ',whole :form ',form :value ,value))
             ,value)))
       (t
        (let ((fn-name (car form))
              (arg-forms (cdr form)))
          (assert (or (symbolp fn-name)
                      (and (consp fn-name)
                           (eql (car fn-name) 'lambda)
                           (listp (cdr fn-name)))))
          (let ((fn (gensym "fn-"))
                (args (gensym "args-"))
                (value (gensym "value-"))
                (default-value (gensym "ert-form-evaluation-aborted-")))
            `(let ((,fn (function ,fn-name))
                   (,args (list ,@arg-forms)))
               (let ((,value ',default-value))
                 ,(funcall inner-expander
                           `(setq ,value (apply ,fn ,args))
                           `(nconc (list ',whole)
                                   (list :form `(,,fn ,@,args))
                                   (unless (eql ,value ',default-value)
                                     (list :value ,value))
                                   (let ((-explainer-
                                          (and (symbolp ',fn-name)
                                               (get ',fn-name
                                                    'ert-explainer))))
                                     (when -explainer-
                                       (list :explanation
                                             (apply -explainer- ,args))))))
                 ,value)))))))))

(defmacro* ert-should (form &environment env)
  "Evaluate FORM.  If it returns nil, abort the current test as failed.

Returns the value of FORM."
  (ert-expand-should `(ert-should ,form) form env
                     (lambda (inner-form form-description-form)
                       `(unless ,inner-form
                          (ert-fail ,form-description-form)))))

(defmacro* ert-should-not (form &environment env)
  "Evaluate FORM.  If it returns non-nil, abort the current test as failed.

Returns nil."
  (ert-expand-should `(ert-should-not ,form) form env
                     (lambda (inner-form form-description-form)
                       `(unless (not ,inner-form)
                          (ert-fail ,form-description-form)))))

(defun ert-should-error-handle-error (form-description-fn
                                      condition type exclude-subtypes test)
  "Helper function for `should-error'.

Determines whether CONDITION matches TYPE, EXCLUDE-SUBTYPES and
TEST, and aborts the current test as failed if it doesn't."
  (let ((signalled-conditions (get (car condition) 'error-conditions))
        (handled-conditions (etypecase type
                              (list type)
                              (symbol (list type)))))
    (assert signalled-conditions)
    (unless (intersection signalled-conditions handled-conditions)
      (ert-fail (append
                 (funcall form-description-fn)
                 (list
                  :condition condition
                  :fail-reason (concat "the error signalled did not"
                                       " have the expected type")))))
    (when exclude-subtypes
      (unless (member (car condition) handled-conditions)
        (ert-fail (append
                   (funcall form-description-fn)
                   (list
                    :condition condition
                    :fail-reason (concat "the error signalled was a subtype"
                                         " of the expected type"))))))
    (unless (funcall test condition)
      (ert-fail (append
                 (funcall form-description-fn)
                 (list
                  :condition condition
                  :fail-reason "the error signalled did not pass the test"))))))

;; FIXME: The expansion will evaluate the keyword args (if any) in
;; nonstandard order.
(defmacro* ert-should-error (form &rest keys &key type exclude-subtypes test
                              &environment env)
  "Evaluate FORM.  Unless it signals an error, abort the current test as failed.

The error signalled additionally needs to match TYPE and satisfy
TEST.  TYPE should be a condition name or a list of condition
names.  If EXCLUDE-SUBTYPES is nil, the error matches TYPE if one
of its condition names is an element of TYPE.  If
EXCLUDE-SUBTYPES is non-nil, the error matches TYPE if it is an
element of TYPE.  TEST should be a predicate."
  ;; Returns a gensym named `ert-form-evaluation-aborted-XXX', but
  ;; that's a wart, so let's not document it.
  (unless type (setq type ''error))
  (unless test (setq test '(lambda (condition) t)))
  (ert-expand-should
   `(ert-should-error ,form ,@keys)
   form env
   (lambda (inner-form form-description-form)
     (let ((errorp (gensym "errorp"))
           (form-description-fn (gensym "form-description-fn-")))
       `(let ((,errorp nil)
              (,form-description-fn (lambda () ,form-description-form)))
          (condition-case -condition-
              ,inner-form
            ;; We can't use ,type here because we want to evaluate it.
            (error
             (setq ,errorp t)
             (ert-should-error-handle-error ,form-description-fn
                                            -condition-
                                            ,type ,exclude-subtypes ,test)
             ;; It would make sense to have the `should-error' form
             ;; return the error in this case, but `ert-expand-should'
             ;; doesn't allow that at the moment.
             ))
          (unless ,errorp
            (ert-fail (append
                       (funcall ,form-description-fn)
                       (list
                        :fail-reason "did not signal an error")))))))))


;;; Explanation of `should' failures.

(defun ert-proper-list-p (x)
  "Return non-nil if X is a proper list, nil otherwise."
  (loop
   for firstp = t then nil
   for fast = x then (cddr fast)
   for slow = x then (cdr slow) do
   (when (null fast) (return t))
   (when (not (consp fast)) (return nil))
   (when (null (cdr fast)) (return t))
   (when (not (consp (cdr fast))) (return nil))
   (when (and (not firstp) (eq fast slow)) (return nil))))

(defun ert-explain-not-equal (a b)
  "Return a programmer-readable explanation of why A and B are not `equal'.

Returns nil if they are equal."
  (if (not (equal (type-of a) (type-of b)))
      `(different-types ,a ,b)
    (etypecase a
      (cons
       (let ((a-proper-p (ert-proper-list-p a))
             (b-proper-p (ert-proper-list-p b)))
         (if (not (eql (not a-proper-p) (not b-proper-p)))
             `(one-list-proper-one-improper ,a ,b)
           (if a-proper-p
               (if (not (equal (length a) (length b)))
                   ;; This would be even more helpful if it showed
                   ;; something like what `set-difference' would
                   ;; return.
                   `(proper-lists-of-different-length ,a ,b)
                 (loop for i from 0
                       for ai in a
                       for bi in b
                       for xi = (ert-explain-not-equal ai bi)
                       do (when xi (return `(list-elt ,i ,xi)))))
             (let ((car-x (ert-explain-not-equal (car a) (car b))))
               (if car-x
                   `(car ,car-x)
                 (let ((cdr-x (ert-explain-not-equal (cdr a) (cdr b))))
                   (if cdr-x
                       `(cdr ,cdr-x))
                   nil)))))))
      (array (if (not (equal (length a) (length b)))
                 `(arrays-of-different-length ,a ,b)
               (loop for i from 0
                     for ai across a
                     for bi across b
                     for xi = (ert-explain-not-equal ai bi)
                     do (when xi (return `(array-elt ,i ,xi))))))
      (atom (if (not (equal a b))
                `(different-atoms ,a ,b)
              nil)))))
(put 'equal 'ert-explainer 'ert-explain-not-equal)


;;; Results display.

;; The data structure that contains the set of tests being executed
;; during one particular test run, their results, the state of the
;; execution, and some statistics.
;;
;; The data about results and expected results of tests may seem
;; redundant here, since the test objects also carry such information.
;; However, the information in the test objects may be more recent, it
;; may correspond to a different test run.  We need the information
;; that corresponds to this run in order to be able to update the
;; statistics correctly when a test is re-run interactively and has a
;; different result than before.
(defstruct ert-stats
  (selector (assert nil))
  ;; The tests, in order.
  (tests (assert nil) :type vector)
  ;; A map of test names (or the test objects themselves for unnamed
  ;; tests) to indices into the `tests' vector.
  (test-map (assert nil) :type hash-table)
  ;; The results of the tests during this run, in order.
  (test-results (assert nil) :type vector)
  ;; The expected result types of the tests, in order.
  (test-results-expected (assert nil) :type vector)
  (total (assert nil))
  (passed-expected 0)
  (passed-unexpected 0)
  (failed-expected 0)
  (failed-unexpected 0)
  (error-expected 0)
  (error-unexpected 0)
  (start-time (assert nil))
  (end-time nil)
  (aborted-p nil)
  (current-test nil))

;; An entry in the results buffer ewoc.  There is one entry per test.
(defstruct ert-ewoc-entry
  (test (assert nil))
  (result nil)
  ;; If the result of this test was expected, its ewoc entry is hidden
  ;; initially.
  (hidden-p (assert nil))
  ;; An ewoc entry may be collapsed to hide details such as the error
  ;; condition.
  ;;
  ;; I'm not sure the ability to expand and collapse entries is still
  ;; a useful feature.
  (expanded-p t)
  ;; By default, the ewoc entry presents the error condition with
  ;; certain limits on how much to print (`print-level',
  ;; `print-length').  The user can interactively switch to a set of
  ;; higher limits.
  (extended-printer-limits-p nil))

;; Variables local to the results buffer.

;; The ewoc.
(defvar ert-results-ewoc)
;; The stats object.
(defvar ert-results-stats)
;; A string with one character per test.  Each character represents
;; the result of the corresponding test.  The string is displayed near
;; the top of the buffer and serves as a progress bar.
(defvar ert-results-progress-bar-string)
;; The position where the progress bar button begins.
(defvar ert-results-progress-bar-button-begin)
;; The test result listener that updates the buffer when tests are run.
(defvar ert-results-listener)

;; The same as `ert-results-stats', but dynamically bound.  Used for
;; the mode line progress indicator.
(defvar ert-current-run-stats nil)

(defun ert-format-time-iso8601 (time)
  "Format TIME in the particular variant of ISO 8601 used for timestamps in ERT."
  (format-time-string "%Y-%m-%d %T%z" time))

(defun ert-insert-test-name-button (test-name)
  (insert-text-button (format "%S" test-name)
                      :type 'ert-test-name-button
                      'ert-test-name test-name))

(defun ert-results-update-ewoc-hf (ewoc stats)
  "Update the header and footer of EWOC to show certain information from STATS.

Also sets `ert-results-progress-bar-button-begin'."
  (let ((run-count (+ (ert-stats-passed-expected stats)
                      (ert-stats-passed-unexpected stats)
                      (ert-stats-failed-expected stats)
                      (ert-stats-failed-unexpected stats)
                      (ert-stats-error-expected stats)
                      (ert-stats-error-unexpected stats)))
        (results-buffer (current-buffer)))
    (ewoc-set-hf
     ewoc
     ;; header
     (with-temp-buffer
       (insert "Selector: ")
       (ert-insert-human-readable-selector (ert-stats-selector stats))
       (insert "\n")
       (insert
        (format (concat "Passed: %s (%s unexpected)\n"
                        "Failed: %s (%s unexpected)\n"
                        "Error:  %s (%s unexpected)\n"
                        "Total:  %s/%s\n\n")
                (+ (ert-stats-passed-expected stats)
                   (ert-stats-passed-unexpected stats))
                (ert-stats-passed-unexpected stats)
                (+ (ert-stats-failed-expected stats)
                   (ert-stats-failed-unexpected stats))
                (ert-stats-failed-unexpected stats)
                (+ (ert-stats-error-expected stats)
                   (ert-stats-error-unexpected stats))
                (ert-stats-error-unexpected stats)
                run-count
                (ert-stats-total stats)))
       (insert
        (format "Started at:   %s\n"
                (ert-format-time-iso8601 (ert-stats-start-time stats))))
       ;; FIXME: This is ugly.  Need to properly define invariants of
       ;; the `stats' data structure.
       (let ((state (cond ((ert-stats-aborted-p stats)
                           'aborted)
                          ((ert-stats-current-test stats)
                           'running)
                          ((ert-stats-end-time stats)
                           'finished)
                          (t
                           'preparing))))
         (ecase state
           (preparing
            (insert ""))
           (aborted
            (cond ((ert-stats-current-test stats)
                   (insert "Aborted during test: ")
                   (ert-insert-test-name-button
                    (ert-test-name (ert-stats-current-test stats))))
                  (t
                   (insert "Aborted."))))
           (running
            (assert (ert-stats-current-test stats))
            (insert "Running test: ")
            (ert-insert-test-name-button (ert-test-name
                                          (ert-stats-current-test stats))))
           (finished
            (assert (not (ert-stats-current-test stats)))
            (insert "Finished.")))
         (insert "\n")
         (if (ert-stats-end-time stats)
             (insert
              (format "%s%s\n"
                      (if (ert-stats-aborted-p stats)
                          "Aborted at:   "
                        "Finished at:  ")
                      (ert-format-time-iso8601 (ert-stats-end-time stats))))
           (insert "\n"))
         (insert "\n"))
       (let ((progress-bar-string (with-current-buffer results-buffer
                                    ert-results-progress-bar-string)))
         (let ((progress-bar-button-begin
                (insert-text-button (substring progress-bar-string 0 run-count)
                                    :type 'ert-results-progress-bar-button)))
           (with-current-buffer results-buffer
             (set (make-local-variable 'ert-results-progress-bar-button-begin)
                  progress-bar-button-begin)))
         (insert (substring progress-bar-string run-count)))
       (insert "\n\n")
       (buffer-string))
     ;; footer
     ;;
     ;; We actually want an empty footer, but that would trigger a bug
     ;; in ewoc, sometimes clearing the entire buffer.
     "\n")))

(defun ert-results-update-stats-display (ewoc stats)
  "Update EWOC and the mode line to show data from STATS."
  (ert-results-update-ewoc-hf ewoc stats)
  (force-mode-line-update)
  (redisplay t))

(defun ert-char-for-test-result (result expectedp)
  "Return a character that represents the test result RESULT."
  (let ((char
         (etypecase result
           (ert-test-passed ?.)
           (ert-test-failed ?f)
           (ert-test-error ?e)
           (null ?-)
           (ert-test-aborted-with-non-local-exit ?a))))
    (if expectedp
        char
      (upcase char))))

(defun ert-string-for-test-result (result expectedp)
  "Return a string that represents the test result RESULT."
  (etypecase result
    (ert-test-passed "passed")
    (ert-test-failed "failed")
    (ert-test-error "error")
    (null "unknown")
    (ert-test-aborted-with-non-local-exit "aborted")))

(defun ert-tests-running-mode-line-indicator ()
  (let* ((stats ert-current-run-stats)
         (tests-total (ert-stats-total stats))
         (tests-completed (+ (ert-stats-passed-expected stats)
                             (ert-stats-passed-unexpected stats)
                             (ert-stats-failed-expected stats)
                             (ert-stats-failed-unexpected stats)
                             (ert-stats-error-expected stats)
                             (ert-stats-error-unexpected stats))))
    (if (>= tests-completed tests-total)
        (format " ERT(%s/%s,finished)" tests-completed tests-total)
      (format " ERT(%s/%s):%s"
              (1+ tests-completed)
              tests-total
              (if (null (ert-stats-current-test stats))
                  "?"
                (format "%S"
                        (ert-test-name (ert-stats-current-test stats))))))))

(defun ert-pp-with-indentation-and-newline (object)
  "Pretty-print OBJECT, indenting it to the current column of point.
Ensures a final newline is inserted."
  (let ((begin (point)))
    (pp object (current-buffer))
    (unless (bolp) (insert "\n"))
    (save-excursion
      (goto-char begin)
      (indent-sexp))))

(defun ert-print-test-for-ewoc (entry)
  "The ewoc print function for ewoc test entries."
  (let* ((test (ert-ewoc-entry-test entry))
         (result (ert-ewoc-entry-result entry))
         (hiddenp (ert-ewoc-entry-hidden-p entry))
         (expandedp (ert-ewoc-entry-expanded-p entry))
         (extended-printer-limits-p (ert-ewoc-entry-extended-printer-limits-p
                                     entry)))
    (cond (hiddenp)
          (t
           (insert-text-button (format "%c"
                                       (ert-char-for-test-result
                                        result
                                        (ert-test-result-expected-p test
                                                                    result)))
                               :type 'ert-results-expand-collapse-button)
           (insert " ")
           (ert-insert-test-name-button (ert-test-name test))
           (insert "\n")
           (when (and expandedp (not (eql result 'nil)))
             (etypecase result
               (ert-test-passed
                (insert "    passed\n")
                (insert ""))
               (ert-test-result-with-condition
                (insert "    ")
                (let ((print-escape-newlines t)
                      (print-level (if extended-printer-limits-p 10 5))
                      (print-length (if extended-printer-limits-p 100 10)))
                  (let ((begin (point)))
                    (ert-pp-with-indentation-and-newline
                     (ert-test-result-with-condition-condition result))
                    (save-restriction
                      (narrow-to-region begin (point))
                      ;; Inhibit optimization in `debugger-make-xrefs'
                      ;; that sometimes inserts unrelated backtrace
                      ;; info into our buffer.
                      (let ((debugger-previous-backtrace nil))
                        (debugger-make-xrefs))))))
               (ert-test-aborted-with-non-local-exit
                (insert "    aborted\n")))
             (insert "\n")))))
  nil)

(defun ert-setup-results-buffer (stats listener buffer-name)
  "Set up a test results buffer."
  (unless buffer-name (setq buffer-name "*ert*"))
  (let ((buffer (let ((default-major-mode 'fundamental-mode))
                  (get-buffer-create buffer-name))))
    (with-current-buffer buffer
      (setq buffer-read-only t)
      (let ((inhibit-read-only t))
        (buffer-disable-undo)
        (erase-buffer)
        (ert-results-mode)
        (set (make-local-variable 'ert-results-ewoc)
             (ewoc-create 'ert-print-test-for-ewoc nil nil t))
        (set (make-local-variable 'ert-results-stats) stats)
        (set (make-local-variable 'ert-results-progress-bar-string)
             (make-string (ert-stats-total stats)
                          (ert-char-for-test-result nil t)))
        (set (make-local-variable 'ert-results-listener) listener)
        (ert-results-update-ewoc-hf ert-results-ewoc ert-results-stats)
        (goto-char (1- (point-max)))
        buffer))))

(defun ert-run-or-rerun-test (stats test listener)
  "Run the single test TEST and record the result using STATS and LISTENER."
  (let ((ert-current-run-stats stats)
        (pos (ert-stats-test-index stats test))
        (results (ert-stats-test-results stats))
        (expected (ert-stats-test-results-expected stats)))
    ;; Adjust stats to remove previous result.
    (if (aref expected pos)
        (etypecase (aref results pos)
          (ert-test-passed (decf (ert-stats-passed-expected stats)))
          (ert-test-failed (decf (ert-stats-failed-expected stats)))
          (ert-test-error (decf (ert-stats-error-expected stats)))
          (null)
          (ert-test-aborted-with-non-local-exit))
      (etypecase (aref results pos)
        (ert-test-passed (decf (ert-stats-passed-unexpected stats)))
        (ert-test-failed (decf (ert-stats-failed-unexpected stats)))
        (ert-test-error (decf (ert-stats-error-unexpected stats)))
        (null)
        (ert-test-aborted-with-non-local-exit)))
    (setf (aref results pos) nil)
    ;; Call listener after setting/before resetting
    ;; (ert-stats-current-test stats); the listener might refresh the
    ;; mode line display, and if the value is not set yet/any more
    ;; during this refresh, the mode line will flicker unnecessarily.
    (setf (ert-stats-current-test stats) test)
    (funcall listener 'test-started stats test)
    (setf (ert-test-most-recent-result test) nil)
    (unwind-protect
        (ert-run-test test)
      (let* ((result (ert-test-most-recent-result test))
             (expectedp (typep result (ert-test-expected-result-type test))))
        ;; Adjust stats to add new result.
        (if expectedp
            (etypecase result
              (ert-test-passed (incf (ert-stats-passed-expected stats)))
              (ert-test-failed (incf (ert-stats-failed-expected stats)))
              (ert-test-error (incf (ert-stats-error-expected stats)))
              (null)
              (ert-test-aborted-with-non-local-exit))
          (etypecase result
            (ert-test-passed (incf (ert-stats-passed-unexpected stats)))
            (ert-test-failed (incf (ert-stats-failed-unexpected stats)))
            (ert-test-error (incf (ert-stats-error-unexpected stats)))
            (null)
            (ert-test-aborted-with-non-local-exit)))
        (setf (aref results pos) result
              (aref expected pos) expectedp)
        (funcall listener 'test-ended stats test result))
      (setf (ert-stats-current-test stats) nil))))

(defun ert-run-tests (selector listener)
  "Run the tests specified by SELECTOR, sending progress updates to LISTENER."
  (let* ((tests (coerce (ert-select-tests selector t) 'vector))
         (map (let ((map (make-hash-table :size (length tests))))
                (loop for i from 0
                      for test across tests
                      for key = (or (ert-test-name test) test) do
                      (assert (not (gethash key map)))
                      (setf (gethash key map) i))
                map))
         (stats (make-ert-stats :selector selector
                                :tests tests
                                :test-map map
                                :test-results (make-vector (length tests) nil)
                                :test-results-expected (make-vector
                                                        (length tests) nil)
                                :total (length tests)
                                :start-time (current-time))))
    (funcall listener 'run-started stats)
    (let ((abortedp t))
      (let ((ert-current-run-stats stats))
        (force-mode-line-update)
        (unwind-protect
            (progn
              (loop for test across tests do
                    (ert-run-or-rerun-test stats test listener))
              (setq abortedp nil))
          (setf (ert-stats-aborted-p stats) abortedp)
          (setf (ert-stats-end-time stats) (current-time))
          (funcall listener 'run-ended stats abortedp)))
      stats)))

(defun ert-stats-test-index (stats test)
  "Return the index of TEST in the run represented by STATS."
  (gethash (or (ert-test-name test) test) (ert-stats-test-map stats)))

(defvar ert-selector-history nil
  "List of recent test selectors read from terminal.")

;; Fix-me: return (regep (list of matches))?
;; Fix-me: Add prompt parameter?
(defun ert-read-test-selector ()
  "Read a regexp for test selection from minibuffer.
The user can use TAB to see which tests match."
  (let ((all-tests
         (mapcar (lambda (rec) (format "%s" (elt rec 1)))
                 (ert-select-tests "" t))
         ;;'("ert-group1-1" "ert-group1-2" "ert-other")
         )
        regexp
        ret
        (get-completions
         (lambda ()
           (let* ((ret (save-match-data
                         (mapcar (lambda (alt)
                                   (when (string-match regexp alt)
                                     alt))
                                 all-tests))))
             (setq ret (delq nil ret))
             ret))))
    (setq all-tests (append all-tests
                            '(":new"
                              ":failed" ":passed" ":error"
                              )
                            nil))
    (let ((mini-map (copy-keymap minibuffer-local-map)))
      (define-key mini-map [?\t]
        (lambda () (interactive)
          (with-output-to-temp-buffer "*Completions*"
            (display-completion-list
             (progn
               (setq regexp (minibuffer-contents))
               (set-text-properties 0 (length regexp) nil regexp)
               (funcall get-completions))))))
      (setq regexp
            (let* ((sym-here (thing-at-point 'symbol))
                   (test-here (when (and sym-here
                                         (memq sym-here all-tests))
                                sym-here))
                   (default (if sym-here
                                (substring-no-properties sym-here)
                              (if ert-selector-history
                                  (first ert-selector-history)
                                "t"))))
              (read-from-minibuffer
               (if (null default)
                   "Run tests, use TAB to see matches: "
                 (format "Run tests, use TAB to see matches (default %s): "
                         default))
               nil ;; initial-contents
               mini-map ;; keymap
               nil ;; read
               'ert-selector-history
               default nil))))
    (setq ret regexp)
    (when (string= "t" ret)
      (setq ret t))
    ret))

;; Should OUTPUT-BUFFER-NAME and MESSAGE-FN really be arguments here?
;; They are needed only for our automated self-tests at the moment.
;; Or should there be some other mechanism?
;;;###autoload
(defun ert-run-tests-interactively (selector
                                    &optional output-buffer-name message-fn)
  "Run the tests specified by SELECTOR and display the results in a buffer."
  (interactive
;;;    (list (let ((default (if ert-selector-history
;;;                             (first ert-selector-history)
;;;                           "t")))
;;;            (read-from-minibuffer (if (null default)
;;;                                      "Run tests: "
;;;                                    (format "Run tests (default %s): " default))
;;;                                  ;;nil nil t 'ert-selector-history
;;;                                  ;;
;;;                                  ;; fix-me: seems like I am misunderstanding Christians intent here.
;;;                                  nil nil nil 'ert-selector-history
;;;                                  default nil))
;;;          nil nil))
   (list (ert-read-test-selector)
         nil nil))
  (unless message-fn (setq message-fn 'message))
  (lexical-let ((output-buffer-name output-buffer-name)
                buffer
                listener
                (message-fn message-fn))
    (setq listener
          (lambda (event-type &rest event-args)
            (ecase event-type
              (run-started
               (destructuring-bind (stats) event-args
                 (setq buffer (ert-setup-results-buffer stats
                                                        listener
                                                        output-buffer-name))
                 (pop-to-buffer buffer)))
              (run-ended
               (destructuring-bind (stats abortedp) event-args
                 (funcall message-fn
                          "%sRan %s tests, %s results were as expected%s"
                          (if (not abortedp)
                              ""
                            "Aborted: ")
                          (ert-stats-total stats)
                          (+ (ert-stats-passed-expected stats)
                             (ert-stats-failed-expected stats)
                             (ert-stats-error-expected stats))
                          (let ((unexpected
                                 (+ (ert-stats-passed-unexpected stats)
                                    (ert-stats-failed-unexpected stats)
                                    (ert-stats-error-unexpected stats))))
                            (if (zerop unexpected)
                                ""
                              (format ", %s unexpected" unexpected))))
                 (ert-results-update-stats-display (with-current-buffer buffer
                                                     ert-results-ewoc)
                                                   stats)))
              (test-started
               (destructuring-bind (stats test) event-args
                 (with-current-buffer buffer
                   (let* ((ewoc ert-results-ewoc)
                          (pos (ert-stats-test-index stats test))
                          (node (ewoc-nth ewoc pos)))
                     (unless node
                       ;; FIXME: How expensive is this assertion?
                       (assert (or (zerop pos) (ewoc-nth ewoc (1- pos)))
                               t)
                       (setq node (ewoc-enter-last
                                   ewoc
                                   (make-ert-ewoc-entry :test test
                                                        :hidden-p t))))
                     (setf (ert-ewoc-entry-test (ewoc-data node)) test)
                     (setf (ert-ewoc-entry-result (ewoc-data node)) nil)
                     (aset ert-results-progress-bar-string pos
                           (ert-char-for-test-result nil t))
                     (ert-results-update-stats-display ewoc stats)
                     (ewoc-invalidate ewoc node)))))
              (test-ended
               (destructuring-bind (stats test result) event-args
                 (with-current-buffer buffer
                   (let* ((ewoc ert-results-ewoc)
                          (pos (ert-stats-test-index stats test))
                          (node (ewoc-nth ewoc pos)))
                     (setf (ert-ewoc-entry-result (ewoc-data node)) result)
                     (when (ert-ewoc-entry-hidden-p (ewoc-data node))
                       (setf (ert-ewoc-entry-hidden-p (ewoc-data node))
                             (ert-test-result-expected-p test result)))
                     (aset ert-results-progress-bar-string pos
                           (ert-char-for-test-result result
                                                     (ert-test-result-expected-p
                                                      test result)))
                     (ert-results-update-stats-display ewoc stats)
                     (ewoc-invalidate ewoc node))))))))
    (ert-run-tests
     selector
     listener)))

(defvar ert-batch-backtrace-right-margin 70
  "*The maximum line length for printing backtraces in `ert-run-tests-batch'.")

(defun ert-run-tests-batch (selector)
  "Run the tests specified by SELECTOR, printing results to the terminal.

Returns the stats object."
  (ert-run-tests
   selector
   (lambda (event-type &rest event-args)
     (ecase event-type
       (run-started
        (destructuring-bind (stats) event-args
          (message "Running %s tests (%s)"
                   (length (ert-stats-tests stats))
                   (ert-format-time-iso8601 (ert-stats-start-time stats)))))
       (run-ended
        (destructuring-bind (stats abortedp) event-args
          (let ((unexpected (+ (ert-stats-passed-unexpected stats)
                               (ert-stats-failed-unexpected stats)
                               (ert-stats-error-unexpected stats))))
            (message "\n%sRan %s tests, %s results were as expected%s (%s)\n"
                     (if (not abortedp)
                         ""
                       "Aborted: ")
                     (ert-stats-total stats)
                     (+ (ert-stats-passed-expected stats)
                        (ert-stats-failed-expected stats)
                        (ert-stats-error-expected stats))
                     (if (zerop unexpected)
                         ""
                       (format ", %s unexpected" unexpected))
                     (ert-format-time-iso8601 (ert-stats-end-time stats)))
            (unless (zerop unexpected)
              (message "%s unexpected results:" unexpected)
              (loop for test across (ert-stats-tests stats)
                    for result = (ert-test-most-recent-result test) do
                    (when (not (ert-test-result-expected-p test result))
                      (message "%9s  %S"
                               (ert-string-for-test-result result nil)
                               (ert-test-name test))))
              (message "%s" "")))))
       (test-started
        )
       (test-ended
        (destructuring-bind (stats test result) event-args
          (etypecase result
            (ert-test-passed)
            (ert-test-result-with-condition
             (message "Test %S backtrace:" (ert-test-name test))
             (with-temp-buffer
               (ert-print-backtrace (ert-test-result-with-condition-backtrace result))
               (goto-char (point-min))
               (while (not (eobp))
                 (let ((start (point))
                       (end (progn (end-of-line) (point))))
                   (setq end (min end
                                  (+ start ert-batch-backtrace-right-margin)))
                   (message "%s" (buffer-substring-no-properties
                                  start end)))
                 (forward-line 1)))
             (with-temp-buffer
               (insert "  ")
               (let ((print-escape-newlines t)
                     (print-level 5)
                     (print-length 10))
                 (let ((begin (point)))
                   (ert-pp-with-indentation-and-newline
                    (ert-test-result-with-condition-condition result))))
               (goto-char (1- (point-max)))
               (assert (looking-at "\n"))
               (delete-char 1)
               (message "Test %S condition:" (ert-test-name test))
               (message "%s" (buffer-string))))
            (ert-test-aborted-with-non-local-exit))
          (let* ((max (prin1-to-string (length (ert-stats-tests stats))))
                 (format-string (concat "%9s  %"
                                        (prin1-to-string (length max))
                                        "s/" max "  %S")))
            (message format-string
                     (ert-string-for-test-result result
                                                 (ert-test-result-expected-p
                                                  test result))
                     (1+ (ert-stats-test-index stats test))
                     (ert-test-name test)))))))))


;;; Commands and button actions for the results buffer.

(define-derived-mode ert-results-mode fundamental-mode "ERT-Results"
  "Major mode for viewing results of ERT test runs.")

(loop for (key binding) in
      '(("j" ert-results-jump-between-summary-and-result)
        ("." ert-results-find-test-at-point-other-window)
        ("r" ert-results-rerun-test-at-point)
        ("d" ert-results-rerun-test-at-point-debugging-errors)
        ("b" ert-results-pop-to-backtrace-for-test-at-point)
        ("m" ert-results-pop-to-messages-for-test-at-point)
        ("p" ert-results-toggle-printer-limits-for-test-at-point)
        ("D" ert-delete-test)
        ([?\t] forward-button)
        ([backtab] backward-button)
        )
      do
      (define-key ert-results-mode-map key binding))

(define-button-type 'ert-results-progress-bar-button
  'action #'ert-results-progress-bar-button-action
  'help-echo "mouse-2, RET: Reveal test result")

(define-button-type 'ert-test-name-button
  'action #'ert-test-name-button-action
  'help-echo "mouse-2, RET: Find test definition")

(define-button-type 'ert-results-expand-collapse-button
  'action #'ert-results-expand-collapse-button-action
  'help-echo "mouse-2, RET: Expand/collapse test result")

(defun ert-results-test-node-or-null-at-point ()
  "If point is on a valid ewoc node, return it; return nil otherwise.

To be used in the ERT results buffer."
  (let* ((ewoc ert-results-ewoc)
         (node (ewoc-locate ewoc)))
    ;; `ewoc-locate' will return an arbitrary node when point is on
    ;; header or footer, or when all nodes are invisible.  So we need
    ;; to validate its return value here.
    (if (and (>= (point) (ewoc-location node))
             (not (ert-ewoc-entry-hidden-p (ewoc-data node))))
        node
      nil)))

(defun ert-results-test-node-at-point ()
  "If point is on a valid ewoc node, return it; signal an error otherwise.

To be used in the ERT results buffer."
  (or (ert-results-test-node-or-null-at-point)
      (error "No test at point")))

(defun ert-results-expand-collapse-button-action (button)
  "Expand or collapse the test node BUTTON belongs to."
  (let* ((ewoc ert-results-ewoc)
         (node (save-excursion
                 (goto-char (ert-button-action-position))
                 (ert-results-test-node-at-point)))
         (entry (ewoc-data node)))
    (setf (ert-ewoc-entry-expanded-p entry)
          (not (ert-ewoc-entry-expanded-p entry)))
    (ewoc-invalidate ewoc node)))

(defun ert-results-find-test-at-point-other-window ()
  "Find the definition of the test at point in another window.

To be used in the ERT results buffer."
  (interactive)
  (let* ((node (ert-results-test-node-at-point))
         (entry (ewoc-data node))
         (test (ert-ewoc-entry-test entry))
         (name (ert-test-name test)))
    (ert-find-test-other-window name)))

(defun ert-test-name-button-action (button)
  "Find the definition of the test BUTTON belongs to, in another window."
  (let ((name (button-get button 'ert-test-name)))
    (ert-find-test-other-window name)))

(defun ert-ewoc-position (ewoc node)
  "Return the position of NODE in EWOC, or nil if NODE is not in EWOC."
  (loop for i from 0
        for node-here = (ewoc-nth ewoc 0) then (ewoc-next ewoc node-here)
        do (when (eql node node-here)
             (return i))
        finally (return nil)))

(defun ert-results-jump-between-summary-and-result ()
  "Jump back and forth between the test run summary and individual test results.

From an ewoc node, jumps to the character that represents the
same test in the progress bar, and vice versa.

To be used in the ERT results buffer."
  ;; Maybe this command isn't actually needed much, but if it is, it
  ;; seems like an indication that the UI design is not optimal.  If
  ;; jumping back and forth between a summary at the top of the buffer
  ;; and the error log in the remainder of the buffer is useful, then
  ;; the summary apparently needs to be easily accessible from the
  ;; error log, and perhaps it would be better to have it in a
  ;; separate buffer to keep it visible.
  (interactive)
  (let ((ewoc ert-results-ewoc)
        (progress-bar-begin ert-results-progress-bar-button-begin))
    (cond ((ert-results-test-node-or-null-at-point)
           (let* ((node (ert-results-test-node-at-point))
                  (pos (ert-ewoc-position ewoc node)))
             (goto-char (+ progress-bar-begin pos))))
          ((and (<= progress-bar-begin (point))
                (< (point) (button-end (button-at progress-bar-begin))))
           (let* ((node (ewoc-nth ewoc (- (point) progress-bar-begin)))
                  (entry (ewoc-data node)))
             (when (ert-ewoc-entry-hidden-p entry)
               (setf (ert-ewoc-entry-hidden-p entry) nil)
               (ewoc-invalidate ewoc node))
             (ewoc-goto-node ewoc node)))
          (t
           (goto-char progress-bar-begin)))))

(defun ert-button-action-position ()
  "The buffer position where the last button action was triggered."
  (cond ((integerp last-command-event)
         (point))
        ((eventp last-command-event)
         (posn-point (event-start last-command-event)))
        (t (assert nil))))

(defun ert-results-progress-bar-button-action (button)
  "Find the ewoc node that represents the same test as the character clicked on."
  (goto-char (ert-button-action-position))
  (ert-results-jump-between-summary-and-result))

(defun ert-results-rerun-test-at-point ()
  "Re-run the test at point.

To be used in the ERT results buffer."
  (interactive)
  (let* ((ewoc ert-results-ewoc)
         (node (ert-results-test-node-at-point))
         (entry (ewoc-data node))
         (old-test (ert-ewoc-entry-test entry))
         (test-name (ert-test-name old-test))
         ;; FIXME: Write a test for this lookup.
         (test (if test-name
                   (if (ert-test-boundp test-name)
                       (ert-get-test test-name)
                     (error "No such test: %S" test-name))
                 old-test))
         (stats ert-results-stats)
         (pos (gethash test (ert-stats-test-map stats)))
         (progress-message (format "Running test %S" (ert-test-name test))))
    ;; Need to save and restore point manually here: When point is on
    ;; the first visible ewoc entry while the header is updated, point
    ;; moves to the top of the buffer.  This is undesirable, and a
    ;; simple `save-excursion' doesn't prevent it.
    (let ((point (point)))
      (unwind-protect
          (unwind-protect
              (progn
                (message "%s..." progress-message)
                (ert-run-or-rerun-test stats test
                                       ert-results-listener))
            (ert-results-update-stats-display ewoc stats)
            (message "%s...%s"
                     progress-message
                     (let ((result (ert-test-most-recent-result test)))
                       (ert-string-for-test-result
                        result (ert-test-result-expected-p test result)))))
        (goto-char point)))))

(defun ert-results-rerun-test-at-point-debugging-errors ()
  "Re-run the test at point with `ert-debug-on-error' bound to t.

To be used in the ERT results buffer."
  (interactive)
  (let ((ert-debug-on-error t))
    (ert-results-rerun-test-at-point)))

(defun ert-print-backtrace (backtrace)
  "Format the backtrace BACKTRACE to the current buffer."
  ;; This is essentially a reimplementation of Fbacktrace
  ;; (src/eval.c), but for a saved backtrace, not the current one.
  (let ((print-escape-newlines t)
        (print-level 8)
        (print-length 50))
    (dolist (frame backtrace)
      (ecase (first frame)
        ((nil)
         ;; Special operator.
         (destructuring-bind (special-operator &rest arg-forms)
             (cdr frame)
           (insert
            (format "  %S\n" (list* special-operator arg-forms)))))
        ((t)
         ;; Function call.
         (destructuring-bind (fn &rest args) (cdr frame)
           (insert (format "  %S(" fn))
           (loop for firstp = t then nil
                 for arg in args do
                 (unless firstp
                   (insert " "))
                 (insert (format "%S" arg)))
           (insert ")\n")))))))

(defun ert-results-pop-to-backtrace-for-test-at-point ()
  "Display the backtrace for the test at point.

To be used in the ERT results buffer."
  (interactive)
  (let* ((node (ert-results-test-node-at-point))
         (entry (ewoc-data node))
         (test (ert-ewoc-entry-test entry))
         (result (ert-ewoc-entry-result entry)))
    (etypecase result
      (ert-test-passed (error "Test passed, no backtrace available"))
      (ert-test-result-with-condition
       (let ((backtrace (ert-test-result-with-condition-backtrace result))
             (buffer
              (let ((default-major-mode 'fundamental-mode))
                (get-buffer-create "*ERT Backtrace*"))))
         (pop-to-buffer buffer)
         (setq buffer-read-only t)
         (let ((inhibit-read-only t))
           (erase-buffer)
           ;; Use unibyte because `debugger-setup-buffer' also does so.
           (set-buffer-multibyte nil)
           (setq truncate-lines t)
           (ert-print-backtrace backtrace)
           (debugger-make-xrefs)
           (goto-char (point-min))))))))

(defun ert-results-pop-to-messages-for-test-at-point ()
  "Display the part of the *Messages* buffer generated during the test at point.

To be used in the ERT results buffer."
  (interactive)
  (let* ((node (ert-results-test-node-at-point))
         (entry (ewoc-data node))
         (test (ert-ewoc-entry-test entry))
         (result (ert-ewoc-entry-result entry)))
    (let ((buffer
           (let ((default-major-mode 'fundamental-mode))
             (get-buffer-create "*ERT Messages*"))))
      (pop-to-buffer buffer)
      (setq buffer-read-only t)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (ert-test-result-messages result))
        (goto-char (point-min))
        (insert "Messages for test `")
        (ert-insert-test-name-button (ert-test-name test))
        (insert "':\n")))))

(defun ert-results-toggle-printer-limits-for-test-at-point ()
  "Toggle how much of the condition to print for the test at point.

To be used in the ERT results buffer."
  (interactive)
  (let* ((ewoc ert-results-ewoc)
         (node (ert-results-test-node-at-point))
         (entry (ewoc-data node)))
    (setf (ert-ewoc-entry-extended-printer-limits-p entry)
          (not (ert-ewoc-entry-extended-printer-limits-p entry)))
    (ewoc-invalidate ewoc node)))

(defun ert-activate-font-lock-keywords ()
  (font-lock-add-keywords
   nil
   '(("(\\(\\<ert-deftest\\)\\>\\s *\\(\\sw+\\)?"
      (1 font-lock-keyword-face nil t)
      (2 font-lock-function-name-face nil t)))))

(defun* ert-remove-from-list (list-var element &key key test)
  "Remove ELEMENT from the value of LIST-VAR if present.

This is an inverse of `add-to-list'."
  (unless key (setq key #'identity))
  (unless test (setq test #'equal))
  (setf (symbol-value list-var)
        (remove* element
                 (symbol-value list-var)
                 :key key
                 :test test)))


;;; Actions on load/unload.

(add-to-list 'find-function-regexp-alist '(ert-deftest . ert-find-test-regexp))
(add-to-list 'minor-mode-alist '(ert-current-run-stats
                                 (:eval
                                  (ert-tests-running-mode-line-indicator))))
(add-to-list 'emacs-lisp-mode-hook 'ert-activate-font-lock-keywords)

(defun ert-unload-function ()
  (ert-remove-from-list 'find-function-regexp-alist 'ert-deftest :key #'car)
  (ert-remove-from-list 'minor-mode-alist 'ert-current-run-stats :key #'car)
  (ert-remove-from-list 'emacs-lisp-mode-hook 'ert-activate-font-lock-keywords)
  nil)

(defvar ert-unload-hook '())
(add-hook 'ert-unload-hook 'ert-unload-function)


;;; Self-tests.

(ert-delete-all-tests)

;; Test that test bodies are actually run.
(defvar ert-test-body-was-run)
(ert-deftest ert-test-body-runs ()
  (setq ert-test-body-was-run t))


;; Test that nested test bodies run.
(ert-deftest ert-nested-test-body-runs ()
  (lexical-let ((was-run nil))
    (let ((test (make-ert-test :body (lambda ()
                                       (setq was-run t)))))
      (assert (not was-run))
      (ert-run-test test)
      (assert was-run))))


;; Test that pass/fail works.
(ert-deftest ert-test-pass ()
  (let ((test (make-ert-test :body (lambda ()))))
    (let ((result (ert-run-test test)))
      (assert (typep result 'ert-test-passed)))))

(ert-deftest ert-test-fail ()
  (let ((test (make-ert-test :body (lambda () (ert-fail "failure message")))))
    (let ((result (let ((ert-debug-on-error nil))
                    (ert-run-test test))))
      (assert (typep result 'ert-test-failed) t)
      (assert (equal (ert-test-result-with-condition-condition result)
                     '(ert-test-failed "failure message"))
              t))))

(ert-deftest ert-test-fail-debug-with-condition-case ()
  (let ((test (make-ert-test :body (lambda () (ert-fail "failure message")))))
    (condition-case condition
        (progn
          (let ((ert-debug-on-error t))
            (ert-run-test test))
          (assert nil))
      ((error)
       (assert (equal condition '(ert-test-failed "failure message")) t)))))

(ert-deftest ert-test-fail-debug-with-debugger-1 ()
  (let ((test (make-ert-test :body (lambda () (ert-fail "failure message")))))
    (let ((debugger (lambda (&rest debugger-args)
                      (assert nil))))
      (let ((ert-debug-on-error nil))
        (ert-run-test test)))))

(ert-deftest ert-test-fail-debug-with-debugger-2 ()
  (let ((test (make-ert-test :body (lambda () (ert-fail "failure message")))))
    (block nil
      (let ((debugger (lambda (&rest debugger-args)
                        (return-from nil nil))))
        (let ((ert-debug-on-error t))
          (ert-run-test test))
        (assert nil)))))

(ert-deftest ert-test-fail-debug-nested-with-debugger ()
  (let ((test (make-ert-test :body (lambda ()
                                     (let ((ert-debug-on-error t))
                                       (ert-fail "failure message"))))))
    (let ((debugger (lambda (&rest debugger-args)
                      (assert nil nil "Assertion a"))))
      (let ((ert-debug-on-error nil))
        (ert-run-test test))))
  (let ((test (make-ert-test :body (lambda ()
                                     (let ((ert-debug-on-error nil))
                                       (ert-fail "failure message"))))))
    (block nil
      (let ((debugger (lambda (&rest debugger-args)
                        (return-from nil nil))))
        (let ((ert-debug-on-error t))
          (ert-run-test test))
        (assert nil nil "Assertion b")))))

(ert-deftest ert-test-error ()
  (let ((test (make-ert-test :body (lambda () (error "error message")))))
    (let ((result (let ((ert-debug-on-error nil))
                    (ert-run-test test))))
      (assert (typep result 'ert-test-error) t)
      (assert (equal (ert-test-result-with-condition-condition result)
                     '(error "error message"))
              t))))

(ert-deftest ert-test-error-debug ()
  (let ((test (make-ert-test :body (lambda () (error "error message")))))
    (condition-case condition
        (progn
          (let ((ert-debug-on-error t))
            (ert-run-test test))
          (assert nil))
      ((error)
       (assert (equal condition '(error "error message")) t)))))


;; Test that `should' works.
(ert-deftest ert-test-should ()
  (let ((test (make-ert-test :body (lambda () (ert-should nil)))))
    (let ((result (let ((ert-debug-on-error nil))
                    (ert-run-test test))))
      (assert (typep result 'ert-test-failed) t)
      (assert (equal (ert-test-result-with-condition-condition result)
                     '(ert-test-failed ((ert-should nil) :form nil :value nil)))
              t)))
  (let ((test (make-ert-test :body (lambda () (ert-should t)))))
    (let ((result (ert-run-test test)))
      (assert (typep result 'ert-test-passed) t))))

(ert-deftest ert-test-should-value ()
  (ert-should (eql (ert-should 'foo) 'foo))
  (ert-should (eql (ert-should 'bar) 'bar)))

(ert-deftest ert-test-should-not ()
  (let ((test (make-ert-test :body (lambda () (ert-should-not t)))))
    (let ((result (let ((ert-debug-on-error nil))
                    (ert-run-test test))))
      (assert (typep result 'ert-test-failed) t)
      (assert (equal (ert-test-result-with-condition-condition result)
                     '(ert-test-failed ((ert-should-not t) :form t :value t)))
              t)))
  (let ((test (make-ert-test :body (lambda () (ert-should-not nil)))))
    (let ((result (ert-run-test test)))
      (assert (typep result 'ert-test-passed)))))


(ert-deftest ert-test-should-error ()
  ;; No error.
  (let ((test (make-ert-test :body (lambda () (ert-should-error (progn))))))
    (let ((result (let ((ert-debug-on-error nil))
                    (ert-run-test test))))
      (ert-should (typep result 'ert-test-failed))
      (ert-should (equal (ert-test-result-with-condition-condition result)
                     '(ert-test-failed
                       ((ert-should-error (progn))
                        :form (progn)
                        :value nil
                        :fail-reason "did not signal an error"))))))
  ;; A simple error.
  (let ((test (make-ert-test :body (lambda () (ert-should-error (error "foo"))))))
    (let ((result (ert-run-test test)))
      (ert-should (typep result 'ert-test-passed))))
  ;; Error of unexpected type, no test.
  (let ((test (make-ert-test :body (lambda ()
                                     (ert-should-error (error "foo")
                                                   :type 'singularity-error)))))
    (let ((result (ert-run-test test)))
      (ert-should (typep result 'ert-test-failed))
      (ert-should (equal
               (ert-test-result-with-condition-condition result)
               '(ert-test-failed
                 ((ert-should-error (error "foo") :type 'singularity-error)
                  :form (error "foo")
                  :condition (error "foo")
                  :fail-reason
                  "the error signalled did not have the expected type"))))))
  ;; Error of the expected type, no test.
  (let ((test (make-ert-test :body (lambda ()
                                     (ert-should-error (signal 'singularity-error
                                                           nil)
                                                   :type 'singularity-error)))))
    (let ((result (ert-run-test test)))
      (ert-should (typep result 'ert-test-passed))))
  ;; Error that fails the test, no type.
  (let ((test (make-ert-test :body (lambda ()
                                     (ert-should-error
                                      (error "foo")
                                      :test (lambda (error) nil))))))
    (let ((result (ert-run-test test)))
      (ert-should (typep result 'ert-test-failed))
      (ert-should (equal (ert-test-result-with-condition-condition result)
                     '(ert-test-failed
                       ((ert-should-error (error "foo") :test (lambda (error) nil))
                        :form (error "foo")
                        :condition (error "foo")
                        :fail-reason
                        "the error signalled did not pass the test"))))))
  ;; Error that passes the test, no type.
  (let ((test (make-ert-test :body (lambda ()
                                     (ert-should-error (error "foo")
                                                   :test (lambda (error) t))))))
    (let ((result (ert-run-test test)))
      (ert-should (typep result 'ert-test-passed))))
  ;; Error that has the expected type but fails the test.
  (let ((test (make-ert-test :body (lambda ()
                                     (ert-should-error
                                      (signal 'singularity-error nil)
                                      :type 'singularity-error
                                      :test (lambda (error) nil))))))
    (let ((result (ert-run-test test)))
      (ert-should (typep result 'ert-test-failed))
      (ert-should (equal (ert-test-result-with-condition-condition result)
                     '(ert-test-failed
                       ((ert-should-error (signal 'singularity-error nil)
                                      :type 'singularity-error
                                      :test (lambda (error) nil))
                        :form (signal singularity-error nil)
                        :condition (singularity-error)
                        :fail-reason
                        "the error signalled did not pass the test"))))))
  ;; Error that has the expected type and passes the test.
  (let ((test (make-ert-test :body (lambda ()
                                     (ert-should-error
                                      (signal 'singularity-error nil)
                                      :type 'singularity-error
                                      :test (lambda (error) t))))))
    (let ((result (ert-run-test test)))
      (ert-should (typep result 'ert-test-passed))))
  )

(ert-deftest ert-test-should-error-subtypes ()
  (let ((test (make-ert-test
               :body (lambda ()
                       (ert-should-error (signal 'singularity-error nil)
                                     :type 'singularity-error
                                     :exclude-subtypes t)))))
    (let ((result (ert-run-test test)))
      (ert-should (typep result 'ert-test-passed))))
  (let ((test (make-ert-test
               :body (lambda ()
                       (ert-should-error (signal 'arith-error nil)
                                     :type 'singularity-error)))))
    (let ((result (ert-run-test test)))
      (ert-should (typep result 'ert-test-failed))
      (ert-should (equal
               (ert-test-result-with-condition-condition result)
               '(ert-test-failed
                 ((ert-should-error (signal 'arith-error nil)
                                :type 'singularity-error)
                  :form (signal arith-error nil)
                  :condition (arith-error)
                  :fail-reason
                  "the error signalled did not have the expected type"))))))
  (let ((test (make-ert-test
               :body (lambda ()
                       (ert-should-error (signal 'arith-error nil)
                                     :type 'singularity-error
                                     :exclude-subtypes t)))))
    (let ((result (ert-run-test test)))
      (ert-should (typep result 'ert-test-failed))
      (ert-should (equal
               (ert-test-result-with-condition-condition result)
               '(ert-test-failed
                 ((ert-should-error (signal 'arith-error nil)
                                :type 'singularity-error
                                :exclude-subtypes t)
                  :form (signal arith-error nil)
                  :condition (arith-error)
                  :fail-reason
                  "the error signalled did not have the expected type"))))))
  (let ((test (make-ert-test
               :body (lambda ()
                       (ert-should-error (signal 'singularity-error nil)
                                     :type 'arith-error
                                     :exclude-subtypes t)))))
    (let ((result (ert-run-test test)))
      (ert-should (typep result 'ert-test-failed))
      (ert-should (equal
               (ert-test-result-with-condition-condition result)
               '(ert-test-failed
                 ((ert-should-error (signal 'singularity-error nil)
                                :type 'arith-error
                                :exclude-subtypes t)
                  :form (signal singularity-error nil)
                  :condition (singularity-error)
                  :fail-reason
                  "the error signalled was a subtype of the expected type"))))))
  )

;; Test that `should' errors contain the information we expect them to.
(defmacro ert-test-my-list (&rest args)
  `(list ,@args))

(ert-deftest ert-test-should-failure-debugging ()
  (loop for (body expected-condition) in
        `((,(lambda () (let ((x nil)) (ert-should x)))
           (ert-test-failed ((ert-should x) :form x :value nil)))
          (,(lambda () (let ((x t)) (ert-should-not x)))
           (ert-test-failed ((ert-should-not x) :form x :value t)))
          (,(lambda () (let ((x t)) (ert-should (not x))))
           (ert-test-failed ((ert-should (not x)) :form (not t) :value nil)))
          (,(lambda () (let ((x nil)) (ert-should-not (not x))))
           (ert-test-failed ((ert-should-not (not x)) :form (not nil) :value t)))
          (,(lambda () (let ((x t) (y nil)) (ert-should-not (ert-test-my-list x y))))
           (ert-test-failed
            ((ert-should-not (ert-test-my-list x y))
             :form (list t nil)
             :value (t nil))))
          (,(lambda () (let ((x t)) (ert-should (error "foo"))))
           (error "foo")))
        do
        (let ((test (make-ert-test :body body)))
          (condition-case actual-condition
              (progn
                (let ((ert-debug-on-error t))
                  (ert-run-test test))
                (assert nil))
            ((error)
             (ert-should (equal actual-condition expected-condition)))))))

(ert-deftest ert-test-messages ()
  (let* ((message-string "Test message")
         (messages-buffer (get-buffer-create "*Messages*"))
         (test (make-ert-test :body (lambda () (message "%s" message-string)))))
    (with-current-buffer messages-buffer
      (let ((result (ert-run-test test)))
        (ert-should (equal (concat message-string "\n")
                       (ert-test-result-messages result)))))))

(defun ert-call-with-temporary-messages-buffer (thunk)
  (lexical-let ((new-buffer-name (generate-new-buffer-name
                                  "*Messages* orig buffer")))
    (unwind-protect
        (progn
          (with-current-buffer (get-buffer-create "*Messages*")
            (rename-buffer new-buffer-name))
          (get-buffer-create "*Messages*")
          (funcall thunk))
      (kill-buffer "*Messages*")
      (with-current-buffer new-buffer-name
        (rename-buffer "*Messages*")))))

(ert-deftest ert-test-messages-on-log-truncation ()
  (let ((test (make-ert-test
               :body (lambda ()
                       ;; Emacs would combine messages if we
                       ;; generate the same message multiple
                       ;; times.
                       (message "a")
                       (message "b")
                       (message "c")
                       (message "d")))))
    (let (result)
      (ert-call-with-temporary-messages-buffer
       (lambda ()
         (let ((message-log-max 2))
           (setq result (ert-run-test test)))
         (ert-should (equal (with-current-buffer "*Messages*"
                          (buffer-string))
                        "c\nd\n"))))
      (ert-should (equal (ert-test-result-messages result) "a\nb\nc\nd\n")))))

;; Test `ert-select-tests'.
(ert-deftest ert-test-select-regexp ()
  (ert-should (equal (ert-select-tests "^ert-test-select-regexp$" t)
                 (list (ert-get-test 'ert-test-select-regexp)))))

(ert-deftest ert-test-test-boundp ()
  (ert-should (ert-test-boundp 'ert-test-test-boundp))
  (ert-should-not (ert-test-boundp (make-symbol "ert-not-a-test"))))

(ert-deftest ert-test-select-member ()
  (ert-should (equal (ert-select-tests '(member ert-test-select-member) t)
                 (list (ert-get-test 'ert-test-select-member)))))

(ert-deftest ert-test-select-test ()
  (ert-should (equal (ert-select-tests (ert-get-test 'ert-test-select-test) t)
                 (list (ert-get-test 'ert-test-select-test)))))

(ert-deftest ert-test-select-symbol ()
  (ert-should (equal (ert-select-tests 'ert-test-select-symbol t)
                 (list (ert-get-test 'ert-test-select-symbol)))))

(ert-deftest ert-test-select-and ()
  (let ((test (make-ert-test
               :name nil
               :body nil
               :most-recent-result (make-ert-test-failed
                                    :condition nil
                                    :backtrace nil))))
    (ert-should (equal (ert-select-tests `(and (member ,test) :failed) t)
                   (list test)))))


;; Test utility functions.
(ert-deftest ert-proper-list-p ()
  (ert-should (ert-proper-list-p '()))
  (ert-should (ert-proper-list-p '(1)))
  (ert-should (ert-proper-list-p '(1 2)))
  (ert-should (ert-proper-list-p '(1 2 3)))
  (ert-should (ert-proper-list-p '(1 2 3 4)))
  (ert-should (not (ert-proper-list-p 'a)))
  (ert-should (not (ert-proper-list-p '(1 . a))))
  (ert-should (not (ert-proper-list-p '(1 2 . a))))
  (ert-should (not (ert-proper-list-p '(1 2 3 . a))))
  (ert-should (not (ert-proper-list-p '(1 2 3 4 . a))))
  (let ((a (list 1)))
    (setf (cdr (last a)) a)
    (ert-should (not (ert-proper-list-p a))))
  (let ((a (list 1 2)))
    (setf (cdr (last a)) a)
    (ert-should (not (ert-proper-list-p a))))
  (let ((a (list 1 2 3)))
    (setf (cdr (last a)) a)
    (ert-should (not (ert-proper-list-p a))))
  (let ((a (list 1 2 3 4)))
    (setf (cdr (last a)) a)
    (ert-should (not (ert-proper-list-p a))))
  (let ((a (list 1 2)))
    (setf (cdr (last a)) (cdr a))
    (ert-should (not (ert-proper-list-p a))))
  (let ((a (list 1 2 3)))
    (setf (cdr (last a)) (cdr a))
    (ert-should (not (ert-proper-list-p a))))
  (let ((a (list 1 2 3 4)))
    (setf (cdr (last a)) (cdr a))
    (ert-should (not (ert-proper-list-p a))))
  (let ((a (list 1 2 3)))
    (setf (cdr (last a)) (cddr a))
    (ert-should (not (ert-proper-list-p a))))
  (let ((a (list 1 2 3 4)))
    (setf (cdr (last a)) (cddr a))
    (ert-should (not (ert-proper-list-p a))))
  (let ((a (list 1 2 3 4)))
    (setf (cdr (last a)) (cdddr a))
    (ert-should (not (ert-proper-list-p a)))))

(ert-deftest ert-parse-keys-and-body ()
  (ert-should (equal (ert-parse-keys-and-body "doc" '(foo))
                     '(nil "doc" (foo))))
  (ert-should (equal (ert-parse-keys-and-body "doc" '(:bar foo))
                     '((:bar foo) "doc" nil)))
  (ert-should (equal (ert-parse-keys-and-body nil '(:bar foo))
                     '((:bar foo) nil nil)))
  (ert-should (equal (ert-parse-keys-and-body "doc" '(:bar foo))
                     '((:bar foo) "doc" nil)))
  (ert-should (equal (ert-parse-keys-and-body nil '(:bar foo a (b)))
                     '((:bar foo) nil (a (b)))))
  (ert-should (equal (ert-parse-keys-and-body nil '(:bar foo :a (b)))
                     '((:bar foo :a (b)) nil nil)))
  (ert-should (equal (ert-parse-keys-and-body nil '(bar foo :a (b)))
                     '(nil nil (bar foo :a (b)))))
  (ert-should-error (ert-parse-keys-and-body nil '(:bar foo :a))))



;; Test `ert-run-tests'.
(ert-deftest ert-test-run-tests ()
  (let ((passing-test (make-ert-test :name 'passing-test
                                     :body (lambda () (ert-pass))))
        (failing-test (make-ert-test :name 'failing-test
                                     :body (lambda () (ert-fail
                                                       "failure message"))))
        )
    (let ((ert-debug-on-error nil))
      (let* ((buffer-name (generate-new-buffer-name " *ert-test-run-tests*"))
             (messages nil)
             (mock-message-fn
              (lambda (format-string &rest args)
                (push (apply #'format format-string args) messages))))
        (save-window-excursion
          (unwind-protect
              (let ((case-fold-search nil))
                (ert-run-tests-interactively
                 `(member ,passing-test ,failing-test) buffer-name
                 mock-message-fn)
                (ert-should (equal messages `(,(concat
                                            "Ran 2 tests, 1 results were "
                                            "as expected, 1 unexpected"))))
                (with-current-buffer buffer-name
                  (goto-char (point-min))
                  (ert-should (equal
                           (buffer-substring (point-min)
                                             (save-excursion
                                               (forward-line 5)
                                               (point)))
                           (concat
                            "Selector: (member <passing-test> <failing-test>)\n"
                            "Passed: 1 (0 unexpected)\n"
                            "Failed: 1 (1 unexpected)\n"
                            "Error:  0 (0 unexpected)\n"
                            "Total:  2/2\n")))))
            (when (get-buffer buffer-name)
              (kill-buffer buffer-name))))))))

(ert-deftest ert-test-special-operator-p ()
  (ert-should (ert-special-operator-p 'if))
  (ert-should-not (ert-special-operator-p 'car))
  (ert-should-not (ert-special-operator-p 'ert-special-operator-p))
  (let ((b (gensym)))
    (ert-should-not (ert-special-operator-p b))
    (fset b 'if)
    (ert-should (ert-special-operator-p b))))

;; This test attempts to demonstrate that there is no way to force
;; immediate truncation of the *Messages* buffer from Lisp (and hence
;; justifies the existence of
;; `ert-force-message-log-buffer-truncation'): The only way that came
;; to my mind was (message ""), which doesn't have the desired effect.
(ert-deftest ert-test-builtin-message-log-flushing ()
  (ert-call-with-temporary-messages-buffer
   (lambda ()
     (with-current-buffer "*Messages*"
       (let ((message-log-max 2))
         (let ((message-log-max t))
           (loop for i below 4 do
                 (message "%s" i))
           (ert-should (eql (count-lines (point-min) (point-max)) 4)))
         (ert-should (eql (count-lines (point-min) (point-max)) 4))
         (message "")
         (ert-should (eql (count-lines (point-min) (point-max)) 4))
         (message "Test message")
         (ert-should (eql (count-lines (point-min) (point-max)) 2)))))))

(ert-deftest ert-test-force-message-log-buffer-truncation ()
  (labels ((body ()
             (loop for i below 5 do
                   (message "%s" i)))
           (c (x)
             (ert-call-with-temporary-messages-buffer
              (lambda ()
                (let ((message-log-max x))
                  (body))
                (with-current-buffer "*Messages*"
                  (buffer-string)))))
           (lisp (x)
             (ert-call-with-temporary-messages-buffer
              (lambda ()
                (let ((message-log-max t))
                  (body))
                (let ((message-log-max x))
                  (ert-force-message-log-buffer-truncation))
                (with-current-buffer "*Messages*"
                  (buffer-string))))))
    (loop for x in '(0 1 2 3 4 5 6 t) do
          (ert-should (equal (c x) (lisp x))))))

;; Run tests and make sure they actually ran.
(let ((window-configuration (current-window-configuration)))
  (let ((ert-test-body-was-run nil))
    ;; The buffer name chosen here should not compete with the default
    ;; results buffer name for completion in `switch-to-buffer'.
    (let ((stats (ert-run-tests-interactively "^ert-" " *ert self-tests*")))
      (assert ert-test-body-was-run)
      (when (zerop (+ (ert-stats-passed-unexpected stats)
                      (ert-stats-failed-unexpected stats)
                      (ert-stats-error-unexpected stats)))
        ;; Hide results window only when everything went well.
        (set-window-configuration window-configuration)))))

(provide 'ert)

;;; ert.el ends here
