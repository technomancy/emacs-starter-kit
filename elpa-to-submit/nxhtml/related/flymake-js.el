;;; flymake-js.el --- Flymake setup for javascript files
;;
;; Author: Lennart Borgman
;; Created: Sun Dec 02 07:52:52 2007
;; Version:
;; Last-Updated:
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
;; *** NOTE *** I have not been able to get this to work. Something is
;;     wrong, please see
;;     http://www.emacswiki.org/cgi-bin/wiki/FlymakeJavaScript
;;
;; This library provides basic setup for using `flymake-mode' with
;; javascript files.  To use this you must have a javascript
;; installed.  There are (at least) two free javascript engines (both
;; from Mozill) you can use, Rhino (implemented in Java) or
;; SpiderMonkey (implemented in C). Both are supported in this
;; library.
;;
;; I have not been able to find binaries for SpiderMonkeys to
;; download. However the Rhino engine seems fast enough and is easy to
;; install. You find them at
;;
;;    http://www.mozilla.org/rhino/
;;    http://www.mozilla.org/js/spidermonkey/
;;
;; Put this file in your Emacs `load-path' and then in .emacs
;;
;;    (require 'flymake-js)
;;
;; If you want to turn on `flymake-mode' automatically for javascript
;; files then you set `flymake-js-on' through Emacs' custom:
;;
;;    M-x customize-option RET flymake-js-on RET
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

;; Flymake JS mode

(require 'flymake)

(defcustom flymake-allowed-js-file-name-masks '(("\\.json\\'" flymake-js-init)
                                                ("\\.js\\'" flymake-js-init))
  "Filename extensions that switch on js syntax checks."
  :type '(repeat (list (regexp :tag "File name regexp")
                       (function :tag "Init function")
                       (choice (const :tag "No cleanup function" nil)
                               (function :tag "Cleanup function"))))
  :group 'flymake)


(defvar flymake-js-err-line-pattern-re '(
                                         ;; These pattern are probably for Rhino:
                                         ("^js: \"\\(.+\\)\", line \\([0-9]+\\): \\(.+\\)$" 1 2 nil 3)
                                         ("^js: uncaught JavaScript \\(.+\\)$" nil nil nil 1)
                                         ;; These pattern are probably for SpiderMonkey:
                                         ("^\\(.+\\)\:\\([0-9]+\\)\: \\(SyntaxError\:.+\\)\:$" 1 2 nil 3)
                                         ("^\\(.+\\)\:\\([0-9]+\\)\: \\(strict warning: trailing comma.+\\)\:$" 1 2 nil 3)
                                         )
  "Regexp matching JavaScript error messages")

(defcustom flymake-js-rhino-jar ""
  "Path to Rihno jar file.
The file seems to be named js.jar now and be in the top directory
of the Rhino dir tree."
  :type '(file :must-match t)
  :group 'flymake)

(defcustom flymake-js-rhino-js
  (let ((dir (file-name-as-directory
              (file-name-directory (if load-file-name
                                       load-file-name
                                     buffer-file-name)))))
      (expand-file-name "rhino.js" dir))
  "Path to rhino.js"
  :type '(file :must-match t)
  :group 'flymake)

(defcustom flymake-js-engine 'rhino
  "Javascript engine to use.
You may have to restart Emacs after changing this - if you can
not figure out what buffers and processes to kill."
  :type '(choice (const :tag "Rhino" 'rhino)
                 (const :tag "SpiderMonkey" 'spidermonkey))
  :group 'flymake)

(defun flymake-js-init ()
  (let* ((temp-file (flymake-init-create-temp-buffer-copy
                     'flymake-create-temp-inplace))
         (local-file (file-relative-name
                      temp-file
                      (file-name-directory buffer-file-name))))
    (unless (file-exists-p flymake-js-rhino-jar)
      (lwarn 't :warning "Can't find Rhino's jar file: %s" flymake-js-rhino-jar))
    (unless (file-exists-p flymake-js-rhino-js)
      (lwarn 't :warning "Can't find rhino.js: %s" flymake-js-rhino-js))
    (cond
     ((eq flymake-js-engine 'rhino)
      (list "java" (list "-jar" flymake-js-rhino-jar flymake-js-rhino-js local-file)))
     ((eq flymake-js-engine 'spidermonkey)
      (list "js" (list "-s" local-file)))
     (t
      (error "Bad value: %s" flymake-js-engine)))))

(defun flymake-js-load ()
  (dolist (rec flymake-allowed-js-file-name-masks)
    (add-to-list 'flymake-allowed-file-name-masks rec))
  (dolist (rec flymake-js-err-line-pattern-re)
    (add-to-list 'flymake-err-line-patterns rec)))

(defvar flymake-js-has-engine nil)

(defun flymake-js-has-engine ()
  "Check for the needed files."
  (if flymake-js-has-engine
      t
    (cond
     ((eq flymake-js-engine 'rhino)
      (unless (executable-find "java")
        (error "Could not find java program"))
      (unless (file-exists-p flymake-js-rhino-jar)
        (error "Could not find file %s" flymake-js-rhino-jar))
      (unless (file-exists-p flymake-js-rhino-js)
        (error "Could not find file %s" flymake-js-rhino-js)))
     ((eq flymake-js-engine 'spidermonkey)
      (unless (executable-find "js")
        (error "Could not find js program")))
     (t
      (error "Bad value: %s" flymake-js-engine)))
    (setq flymake-js-has-engine t)))

(defun flymake-js-turn-on ()
  (when buffer-file-name
    (flymake-js-has-engine)
    (unless flymake-mode
      (flymake-mode 1))))

(defcustom flymake-js-on nil
  "Turn on flymake for new js file buffers if non-nil."
  :type 'boolean
  :set (lambda (sym val)
         (set-default sym val)
         (if val
             (add-hook 'javascript-mode-hook 'flymake-js-turn-on)
           (remove-hook 'javascript-mode-hook 'flymake-js-turn-on)))
  :group 'flymake)

(eval-after-load 'javascript
  (flymake-js-load))

(provide 'flymake-js)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; flymake-js.el<2> ends here
