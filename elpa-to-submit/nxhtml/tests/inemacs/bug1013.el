;; Setup
(defvar word-wrap2 nil)
(make-variable-buffer-local 'word-wrap2)
(set-default 'word-wrap2 nil)

(defcustom word-wrap3 nil
  "doc 3"
  :type 'boolean)
(make-variable-buffer-local 'word-wrap3)
(set-default 'word-wrap3 nil)

(set-default 'word-wrap nil)
(set-default 'truncate-lines nil)

(put 'truncate-lines 'permanent-local t)
(put 'word-wrap  'permanent-local t)
(put 'word-wrap2 'permanent-local t)
(put 'word-wrap3 'permanent-local t)

(setq truncate-lines t)
(setq word-wrap t)
(setq word-wrap2 t)
(setq word-wrap3 t)

(kill-all-local-variables)

;; Test
(ert-should (eq (default-value 'word-wrap3) nil))
(ert-should (eq word-wrap3 t))
(ert-should (eq (default-value 'word-wrap2) nil))
(ert-should (eq word-wrap2 t))
(ert-should (eq (default-value 'truncate-lines) nil))
(ert-should (eq truncate-lines t))
(ert-should (eq (default-value 'word-wrap) nil))
(ert-should (eq word-wrap t))
