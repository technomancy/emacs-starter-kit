;;; This file is from a link on EmacsWiki to http://paste.lisp.org/display/59495
;;
;; As far as I can see this is a ruby-mode bug, not a mumamo bug.

;;; The problem is that when a ruby ERB template is loaded with an
;;; after-hook that modifies the font-lock keywords, nxhtml causes
;;; font-lock not to occur on strings and comments. What's more is
;;; this affects regular ruby-mode as well, not just within mumamo.

(require 'ruby-mode)

;; Extra keyword fontification for ruby
(defun emacswiki-erb-bug-keywords ()
  (font-lock-add-keywords nil
                          '(("\\<\\(FIX\\|TODO\\|FIXME\\|HACK\\|REFACTOR\\):"
                             1 font-lock-warning-face t))))

;; Adding the extra keywords at the beginning of ruby-mode-hook breaks
;; ruby-mode fontification (use the test case at the bottom):
(add-hook 'ruby-mode-hook 'emacswiki-erb-bug-keywords)
;; removing this hook makes it not break anymore:
(remove-hook 'ruby-mode-hook 'emacswiki-erb-bug-keywords)
;; However adding the extra keywords at the end of the hook works fine:
(add-hook 'ruby-mode-hook 'emacswiki-erb-bug-keywords t)


;; run this to test:
(progn
  (find-file "bar.rb")
  (insert "# Comments should be font-locked, but are not.
class Bar
  def baz
    \"strings should also be font-locked but are not.\"
  end
end"))

