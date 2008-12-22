;;; csharp-mode.el --- C# mode derived mode

;; Author:     2005Dylan R. E. Moonfire
;; Maintainer: Dylan R. E. Moonfire <contact@mfgames.com>
;; Created:    Feburary 2005
;; Modified:   September 2007
;; Version:    0.7.0
;; Keywords:   c# languages oop

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:
;;
;;    This is a separate mode to implement the C# constructs and
;;    font-locking. It is based on the java-mode example from cc-mode.
;;
;;    Note: The interface used in this file requires CC Mode 5.30 or
;;    later.

;;; Bugs:
;;
;;   Literal strings @"" do not fontify correctly.
;;
;;   Method names are not fontified if you have an attribute before it.
;;
;;   This code doesn't seem to work when you compile it, then
;;   load/require in the emacs file. You will get an error (error
;;   "`c-lang-defconst' must be used in a file") which happens because
;;   cc-mode doesn't think it is in a buffer while loading directly
;;   from the init. However, if you call it based on a file extension,
;;   it works properly. Interestingly enough, this doesn't happen if
;;   you don't byte-compile cc-mode.

;;; .emacs (don't put in (require 'csharp-mode))
;; (autoload 'csharp-mode "csharp-mode" "Major mode for editing C# code." t)
;; (setq auto-mode-alist
;;    (append '(("\\.cs$" . csharp-mode)) auto-mode-alist))

;;; Versions:
;;
;;    0.1.0 - Initial release.
;;    0.2.0 - Fixed the identification on the "enum" keyword.
;;          - Fixed the font-lock on the "base" keyword
;;    0.3.0 - Added a regex to fontify attributes. It isn't the
;;            the best method, but it handles single-like attributes
;;            well.
;;          - Got "super" not to fontify as a keyword.
;;          - Got extending classes and interfaces to fontify as something.
;;    0.4.0 - Removed the attribute matching because it broke more than
;;            it fixed.
;;          - Corrected a bug with namespace not being properly identified
;;            and treating the class level as an inner object, which screwed
;;            up formatting.
;;          - Added "partial" to the keywords.
;;    0.5.0 - Found bugs with compiled cc-mode and loading from init files.
;;          - Updated the eval-when-compile to code to let the mode be
;;            compiled.
;;    0.6.0 - Added the c-filter-ops patch for 5.31.1 which made that
;;            function in cc-langs.el unavailable.
;;          - Added a csharp-lineup-region for indention #region and
;;            #endregion block differently.
;;    0.7.0 - Added autoload so update-directory-autoloads words
;;            (Thank you, Nikolaj Schumacher)
;;          - Fontified the entire #region and #endregion lines.
;;          - Initial work to get get, set, add, remove font-locked.

;; This is a copy of the function in cc-mode which is used to handle
;; the eval-when-compile which is needed during other times.
(defun c-filter-ops (ops opgroup-filter op-filter &optional xlate)
  ;; See cc-langs.el, a direct copy.
  (unless (listp (car-safe ops))
    (setq ops (list ops)))
  (cond ((eq opgroup-filter t)
         (setq opgroup-filter (lambda (opgroup) t)))
        ((not (functionp opgroup-filter))
         (setq opgroup-filter `(lambda (opgroup)
                                 (memq opgroup ',opgroup-filter)))))
  (cond ((eq op-filter t)
         (setq op-filter (lambda (op) t)))
        ((stringp op-filter)
         (setq op-filter `(lambda (op)
                            (string-match ,op-filter op)))))
  (unless xlate
    (setq xlate 'identity))
  (c-with-syntax-table (c-lang-const c-mode-syntax-table)
    (delete-duplicates
     (mapcan (lambda (opgroup)
               (when (if (symbolp (car opgroup))
                         (when (funcall opgroup-filter (car opgroup))
                           (setq opgroup (cdr opgroup))
                           t)
                       t)
                 (mapcan (lambda (op)
                           (when (funcall op-filter op)
                             (let ((res (funcall xlate op)))
                               (if (listp res) res (list res)))))
                         opgroup)))
             ops)
     :test 'equal)))

;; This inserts the bulk of the code.
(require 'cc-mode)

;; These are only required at compile time to get the sources for the
;; language constants.  (The cc-fonts require and the font-lock
;; related constants could additionally be put inside an
;; (eval-after-load "font-lock" ...) but then some trickery is
;; necessary to get them compiled.)
(eval-when-compile
  (let ((load-path
         (if (and (boundp 'byte-compile-dest-file)
                  (stringp byte-compile-dest-file))
             (cons (file-name-directory byte-compile-dest-file) load-path)
           load-path)))
    (load "cc-mode" nil t)
    (load "cc-fonts" nil t)
    (load "cc-langs" nil t)))

(eval-and-compile
  ;; Make our mode known to the language constant system.  Use Java
  ;; mode as the fallback for the constants we don't change here.
  ;; This needs to be done also at compile time since the language
  ;; constants are evaluated then.
  (c-add-language 'csharp-mode 'java-mode))

;; Indention: csharp-mode follows normal indention rules except for
;; when indenting the #region and #endregion blocks. This function
;; defines a custom indention to indent the #region blocks as normal
;; text.
;;
;; To use this indenting just put the following in your emacs file:
;;   (c-set-offset 'cpp-macro 'csharp-lineup-region)
(defun csharp-lineup-region (langelem)
  "Indent all #region and #endregion blocks inline with code while
retaining normal column-zero indention for #if and the other
processing blocks."
  (save-excursion
    (back-to-indentation)
    (if (re-search-forward "#\\(end\\)?region" (c-point 'eol) [0]) 0  [0])))

;; TODO
;; Defines our constant for finding attributes.
;;(defconst csharp-attribute-regex "\\[\\([XmlType]+\\)(")
;;(defconst csharp-attribute-regex "\\[\\(.\\)")
;; This doesn't work because the string regex happens before this point
;; and getting the font-locking to work before and after is fairly difficult
;;(defconst csharp-attribute-regex
;;  (concat
;;   "\\[[a-zA-Z][ \ta-zA-Z0-9.]+"
;;   "\\((.*\\)?"
;;))

;; Java uses a series of regexes to change the font-lock for class
;; references. The problem comes in because Java uses Pascal (leading
;; space in names, SomeClass) for class and package names, but
;; Camel-casing (initial lowercase, upper case in words,
;; i.e. someVariable) for variables. The notation suggested by EMCA is
;; to use Pasacal notation for everything, except inner variables. So,
;; the regex and formatting actually produces very wrong results.
;;(error (byte-compile-dest-file))
;;(error (c-get-current-file))
(c-lang-defconst c-opt-after-id-concat-key
  csharp (if (c-lang-const c-opt-identifier-concat-key)
             (c-lang-const c-symbol-start)))

(c-lang-defconst c-basic-matchers-before
  csharp `(
           ;;;; Font-lock the attributes by searching for the
           ;;;; appropriate regex and marking it as TODO.
           ;;,`(,(concat "\\(" csharp-attribute-regex "\\)")
           ;;   0 font-lock-function-name-face)

           ;; Put a warning face on the opener of unclosed strings that
           ;; can't span lines.  Later font
           ;; lock packages have a `font-lock-syntactic-face-function' for
           ;; this, but it doesn't give the control we want since any
           ;; fontification done inside the function will be
           ;; unconditionally overridden.
           ,(c-make-font-lock-search-function
             ;; Match a char before the string starter to make
             ;; `c-skip-comments-and-strings' work correctly.
             (concat ".\\(" c-string-limit-regexp "\\)")
             '((c-font-lock-invalid-string)))

           ;; Fontify keyword constants.
           ,@(when (c-lang-const c-constant-kwds)
               (let ((re (c-make-keywords-re nil
                           (c-lang-const c-constant-kwds))))
                 `((eval . (list ,(concat "\\<\\(" re "\\)\\>")
                                 1 c-constant-face-name)))))

           ;; Fontify all keywords except the primitive types.
           ,`(,(concat "\\<" (c-lang-const c-regular-keywords-regexp))
              1 font-lock-keyword-face)

           ;; Fontify leading identifiers in fully qualified names like
           ;; "Foo.Bar".
           ,@(when (c-lang-const c-opt-identifier-concat-key)
               `((,(byte-compile
                    `(lambda (limit)
                       (while (re-search-forward
                               ,(concat "\\(\\<" ; 1
                                        "\\(" (c-lang-const c-symbol-key)
                                        "\\)" ; 2
                                        "[ \t\n\r\f\v]*"
                                        (c-lang-const
                                         c-opt-identifier-concat-key)
                                        "[ \t\n\r\f\v]*"
                                        "\\)"
                                        "\\("
                                        (c-lang-const
                                         c-opt-after-id-concat-key)
                                        "\\)")
                               limit t)
                         (unless (progn
                                   (goto-char (match-beginning 0))
                                   (c-skip-comments-and-strings limit))
                           (or (get-text-property (match-beginning 2) 'face)
                               (c-put-font-lock-face (match-beginning 2)
                                                     (match-end 2)
                                                     c-reference-face-name))
                           (goto-char (match-end 1)))))))))
           ))

;; C# does not allow a leading qualifier operator. It also doesn't
;; allow the ".*" construct of Java. So, we redo this regex without
;; the "\\|\\*" regex.
(c-lang-defconst c-identifier-key
  csharp (concat "\\(" (c-lang-const c-symbol-key) "\\)" ; 1
                 (concat "\\("
                         "[ \t\n\r\f\v]*"
                         (c-lang-const c-opt-identifier-concat-key)
                         "[ \t\n\r\f\v]*"
                         (concat "\\("
                                 "\\(" (c-lang-const c-symbol-key) "\\)"
                                 "\\)")
                         "\\)*")))

;; C# has a few rules that are slightly different than Java for
;; operators. This also removed the Java's "super" and replaces it
;; with the C#'s "base".
(c-lang-defconst c-operators
  csharp `((prefix "base")))

;; C#, unlike Java, does use CPP prefixes for the regions and other
;; directives. This matches everything to the end of the line (Closes
;; #76).
(c-lang-defconst c-opt-cpp-prefix
  csharp "^\\s *#.*")

;; C# uses the following assignment operators
(c-lang-defconst c-assignment-operators
  csharp '("=" "*=" "/=" "%=" "+=" "-=" ">>=" "<<=" "&=" "^=" "|="))

;; This defines the primative types for C#
(c-lang-defconst c-primitive-type-kwds
  ;; ECMA-344, S8
  csharp '("object" "string" "sbyte" "short" "int" "long" "byte"
           "ushort" "uint" "ulong" "float" "double" "bool" "char"
           "decimal" "void"))

;; The keywords that define that the following is a type, such as a
;; class definition.
(c-lang-defconst c-type-prefix-kwds
  ;; ECMA-344, S?
  csharp '("class" "interface" "enum" "struct"))

;; Type modifier keywords. They appear anywhere in types, but modifiy
;; instead create one.
(c-lang-defconst c-type-modifier-kwds
  ;; EMCA-344, S?
  csharp '("readonly" "const"))

;; Structures that are similiar to classes.
(c-lang-defconst c-class-decl-kwds
  ;; EMCA-344, S?
  csharp '("class" "interface"))

;; The various modifiers used for class and method descriptions.
(c-lang-defconst c-modifier-kwds
  csharp '("public" "partial" "private" "const" "abstract"
           "protected" "ref" "out" "static" "virtual"
           "override" "params" "internal"))

;; We don't use the protection level stuff because it breaks the
;; method indenting. Not sure why, though.
(c-lang-defconst c-protection-kwds
  csharp nil)

;; Define the keywords that can have something following after them.
(c-lang-defconst c-type-list-kwds
  csharp '("struct" "class" "interface" "is" "as"
           "delegate" "event" "set" "get" "add" "remove"))

;; This allows the classes after the : in the class declartion to be
;; fontified.
(c-lang-defconst c-typeless-decl-kwds
  csharp '(":"))

;; Sets up the enum to handle the list properly
(c-lang-defconst c-brace-list-decl-kwds
  csharp '("enum"))

;; We need to remove Java's package keyword
(c-lang-defconst c-ref-list-kwds
  csharp '("using" "namespace"))

;; Follow-on blocks that don't require a brace
(c-lang-defconst c-block-stmt-2-kwds
  csharp '("for" "if" "switch" "while" "catch" "foreach"
           "checked" "unchecked" "lock"))

;; Statements that break out of braces
(c-lang-defconst c-simple-stmt-kwds
  csharp '("return" "continue" "break" "throw" "goto"))

;; Statements that allow a label
;; TODO?
(c-lang-defconst c-before-label-kwds
  csharp nil)

;; Constant keywords
(c-lang-defconst c-constant-kwds
  csharp '("true" "false" "null"))

;; Keywords that start "primary expressions."
(c-lang-defconst c-primary-expr-kwds
  csharp '("this" "base"))

;; We need to treat namespace as an outer block to class indenting
;; works properly.
(c-lang-defconst c-other-block-decl-kwds
  csharp '("namespace"))

;; We need to include the "as" for the foreach
(c-lang-defconst c-other-kwds
  csharp '("in" "sizeof" "typeof"))

(c-lang-defconst c-overloadable-operators
  ;; EMCA-344, S14.2.1
  csharp '("+" "-" "*" "/" "%" "&" "|" "^"
           "<<" ">>" "==" "!=" ">" "<" ">=" "<="))

;; No cpp in this language, but there's still a "sharppragma" directive to
;; fontify.  (The definitions for the extra keywords above are enough
;; to incorporate them into the fontification regexps for types and
;; keywords, so no additional font-lock patterns are required.)
(c-lang-defconst c-cpp-matchers
  csharp (cons
      ;; Use the eval form for `font-lock-keywords' to be able to use
      ;; the `c-preprocessor-face-name' variable that maps to a
      ;; suitable face depending on the (X)Emacs version.
      '(eval . (list "^\\s *\\(sharppragma\\)\\>\\(.*\\)"
                     (list 1 c-preprocessor-face-name)
                     '(2 font-lock-string-face)))
      ;; There are some other things in `c-cpp-matchers' besides the
      ;; preprocessor support, so include it.
      (c-lang-const c-cpp-matchers)))

(defcustom csharp-font-lock-extra-types nil
  "*List of extra types (aside from the type keywords) to recognize in C# mode.
Each list item should be a regexp matching a single identifier.")

(defconst csharp-font-lock-keywords-1 (c-lang-const c-matchers-1 csharp)
  "Minimal highlighting for C# mode.")

(defconst csharp-font-lock-keywords-2 (c-lang-const c-matchers-2 csharp)
  "Fast normal highlighting for C# mode.")

(defconst csharp-font-lock-keywords-3 (c-lang-const c-matchers-3 csharp)
  "Accurate normal highlighting for C# mode.")

(defvar csharp-font-lock-keywords csharp-font-lock-keywords-3
  "Default expressions to highlight in C# mode.")

(defvar csharp-mode-syntax-table nil
  "Syntax table used in csharp-mode buffers.")
(or csharp-mode-syntax-table
    (setq csharp-mode-syntax-table
          (funcall (c-lang-const c-make-mode-syntax-table csharp))))

(defvar csharp-mode-abbrev-table nil
  "Abbreviation table used in csharp-mode buffers.")
(c-define-abbrev-table 'csharp-mode-abbrev-table
  ;; Keywords that if they occur first on a line might alter the
  ;; syntactic context, and which therefore should trig reindentation
  ;; when they are completed.
  '(("else" "else" c-electric-continued-statement 0)
    ("while" "while" c-electric-continued-statement 0)
    ("catch" "catch" c-electric-continued-statement 0)
    ("finally" "finally" c-electric-continued-statement 0)))

(defvar csharp-mode-map (let ((map (c-make-inherited-keymap)))
                      ;; Add bindings which are only useful for C#
                      map)
  "Keymap used in csharp-mode buffers.")

;;(easy-menu-define csharp-menu csharp-mode-map "C# Mode Commands"
;;                ;; Can use `csharp' as the language for `c-mode-menu'
;;                ;; since its definition covers any language.  In
;;                ;; this case the language is used to adapt to the
;;                ;; nonexistence of a cpp pass and thus removing some
;;                ;; irrelevant menu alternatives.
;;                (cons "C#" (c-lang-const c-mode-menu csharp)))

;;; Autoload mode trigger
(add-to-list 'auto-mode-alist '("\\.cs" . csharp-mode))

;; Custom variables
(defcustom csharp-mode-hook nil
  "*Hook called by `csharp-mode'."
  :type 'hook
  :group 'c)

;;; The entry point into the mode
;;;###autoload
(defun csharp-mode ()
  "Major mode for editing C# (pronounced \"see sharp\") code.
This is a simple example of a separate mode derived from CC Mode to
support a language with syntax similar to C/C++/ObjC/Java/IDL/Pike.

The hook `c-mode-common-hook' is run with no args at mode
initialization, then `csharp-mode-hook'.

Key bindings:
\\{csharp-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (c-initialize-cc-mode t)
  (set-syntax-table csharp-mode-syntax-table)
  (setq major-mode 'csharp-mode
        mode-name "C#"
        local-abbrev-table csharp-mode-abbrev-table
        abbrev-mode t)
  (use-local-map c-mode-map)
  ;; `c-init-language-vars' is a macro that is expanded at compile
  ;; time to a large `setq' with all the language variables and their
  ;; customized values for our language.
  (c-init-language-vars csharp-mode)
  ;; `c-common-init' initializes most of the components of a CC Mode
  ;; buffer, including setup of the mode menu, font-lock, etc.
  ;; There's also a lower level routine `c-basic-common-init' that
  ;; only makes the necessary initialization to get the syntactic
  ;; analysis and similar things working.
  (c-common-init 'csharp-mode)
  ;;(easy-menu-add csharp-menu)
  (run-hooks 'c-mode-common-hook)
  (run-hooks 'csharp-mode-hook)
  (c-update-modeline))


(provide 'csharp-mode)

;;; csharp-mode.el ends here
