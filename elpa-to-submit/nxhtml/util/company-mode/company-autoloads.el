;;; company-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (company-mode) "company" "company.el" (19036 64075))
;;; Generated autoloads from company.el

(autoload 'company-mode "company" "\
\"complete anything\"; in in-buffer completion framework.
Completion starts automatically, depending on the values
`company-idle-delay' and `company-minimum-prefix-length'.

Completion can be controlled with the commands:
`company-complete-common', `company-complete-selection', `company-complete',
`company-select-next', `company-select-previous'.  If these commands are
called before `company-idle-delay', completion will also start.

Completions can be searched with `company-search-candidates' or
`company-filter-candidates'.  These can be used while completion is
inactive, as well.

The completion data is retrieved using `company-backends' and displayed using
`company-frontends'.  If you want to start a specific back-end, call it
interactively or use `company-begin-backend'.

regular keymap (`company-mode-map'):

\\{company-mode-map}
keymap during active completions (`company-active-map'):

\\{company-active-map}

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads (company-abbrev) "company-abbrev" "company-abbrev.el"
;;;;;;  (19036 64074))
;;; Generated autoloads from company-abbrev.el

(autoload 'company-abbrev "company-abbrev" "\
A `company-mode' completion back-end for abbrev.

\(fn COMMAND &optional ARG &rest IGNORED)" t nil)

;;;***

;;;### (autoloads (company-css) "company-css" "company-css.el" (19036
;;;;;;  64074))
;;; Generated autoloads from company-css.el

(autoload 'company-css "company-css" "\
A `company-mode' completion back-end for `css-mode'.

\(fn COMMAND &optional ARG &rest IGNORED)" t nil)

;;;***

;;;### (autoloads (company-dabbrev) "company-dabbrev" "company-dabbrev.el"
;;;;;;  (19036 64074))
;;; Generated autoloads from company-dabbrev.el

(autoload 'company-dabbrev "company-dabbrev" "\
A dabbrev-like `company-mode' completion back-end.

\(fn COMMAND &optional ARG &rest IGNORED)" t nil)

;;;***

;;;### (autoloads (company-dabbrev-code) "company-dabbrev-code" "company-dabbrev-code.el"
;;;;;;  (19036 64074))
;;; Generated autoloads from company-dabbrev-code.el

(autoload 'company-dabbrev-code "company-dabbrev-code" "\
A dabbrev-like `company-mode' back-end for code.
The back-end looks for all symbols in the current buffer that aren't in
comments or strings.

\(fn COMMAND &optional ARG &rest IGNORED)" t nil)

;;;***

;;;### (autoloads (company-elisp) "company-elisp" "company-elisp.el"
;;;;;;  (19036 64075))
;;; Generated autoloads from company-elisp.el

(autoload 'company-elisp "company-elisp" "\
A `company-mode' completion back-end for `emacs-lisp-mode'.

\(fn COMMAND &optional ARG &rest IGNORED)" t nil)

;;;***

;;;### (autoloads (company-etags) "company-etags" "company-etags.el"
;;;;;;  (19036 64075))
;;; Generated autoloads from company-etags.el

(autoload 'company-etags "company-etags" "\
A `company-mode' completion back-end for etags.

\(fn COMMAND &optional ARG &rest IGNORED)" t nil)

;;;***

;;;### (autoloads (company-files) "company-files" "company-files.el"
;;;;;;  (19036 64075))
;;; Generated autoloads from company-files.el

(autoload 'company-files "company-files" "\
a `company-mode' completion back-end existing file names.

\(fn COMMAND &optional ARG &rest IGNORED)" t nil)

;;;***

;;;### (autoloads (company-gtags) "company-gtags" "company-gtags.el"
;;;;;;  (19036 64075))
;;; Generated autoloads from company-gtags.el

(autoload 'company-gtags "company-gtags" "\
A `company-mode' completion back-end for GNU Global.

\(fn COMMAND &optional ARG &rest IGNORED)" t nil)

;;;***

;;;### (autoloads (company-ispell) "company-ispell" "company-ispell.el"
;;;;;;  (19036 64075))
;;; Generated autoloads from company-ispell.el

(autoload 'company-ispell "company-ispell" "\
A `company-mode' completion back-end using ispell.

\(fn COMMAND &optional ARG &rest IGNORED)" t nil)

;;;***

;;;### (autoloads (company-keywords) "company-keywords" "company-keywords.el"
;;;;;;  (19036 64075))
;;; Generated autoloads from company-keywords.el

(autoload 'company-keywords "company-keywords" "\
A `company-mode' back-end for programming language keywords.

\(fn COMMAND &optional ARG &rest IGNORED)" t nil)

;;;***

;;;### (autoloads (company-nxml) "company-nxml" "company-nxml.el"
;;;;;;  (19036 64075))
;;; Generated autoloads from company-nxml.el

(autoload 'company-nxml "company-nxml" "\
A `company-mode' completion back-end for `nxml-mode'.

\(fn COMMAND &optional ARG &rest IGNORED)" t nil)

;;;***

;;;### (autoloads (company-oddmuse) "company-oddmuse" "company-oddmuse.el"
;;;;;;  (19036 64075))
;;; Generated autoloads from company-oddmuse.el

(autoload 'company-oddmuse "company-oddmuse" "\
A `company-mode' completion back-end for `oddmuse-mode'.

\(fn COMMAND &optional ARG &rest IGNORED)" t nil)

;;;***

;;;### (autoloads (company-pysmell) "company-pysmell" "company-pysmell.el"
;;;;;;  (19036 64075))
;;; Generated autoloads from company-pysmell.el

(autoload 'company-pysmell "company-pysmell" "\
A `company-mode' completion back-end for pysmell.
This requires pysmell.el and pymacs.el.

\(fn COMMAND &optional ARG &rest IGNORED)" t nil)

;;;***

;;;### (autoloads (company-semantic) "company-semantic" "company-semantic.el"
;;;;;;  (19036 64075))
;;; Generated autoloads from company-semantic.el

(autoload 'company-semantic "company-semantic" "\
A `company-mode' completion back-end using CEDET Semantic.

\(fn COMMAND &optional ARG &rest IGNORED)" t nil)

;;;***

;;;### (autoloads (company-tempo) "company-tempo" "company-tempo.el"
;;;;;;  (19036 64075))
;;; Generated autoloads from company-tempo.el

(autoload 'company-tempo "company-tempo" "\
A `company-mode' completion back-end for tempo.

\(fn COMMAND &optional ARG &rest IGNORED)" t nil)

;;;***

;;;### (autoloads (company-xcode) "company-xcode" "company-xcode.el"
;;;;;;  (19036 64075))
;;; Generated autoloads from company-xcode.el

(autoload 'company-xcode "company-xcode" "\
A `company-mode' completion back-end for Xcode projects.

\(fn COMMAND &optional ARG &rest IGNORED)" t nil)

;;;***

;;;### (autoloads nil nil ("company-eclim.el" "company-pkg.el" "company-ropemacs.el")
;;;;;;  (19036 64075 906000))

;;;***

(provide 'company-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; company-autoloads.el ends here
