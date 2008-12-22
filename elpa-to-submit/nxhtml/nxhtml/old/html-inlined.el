;; html-inlined.el --- narrow/widen embedded blocks of css etc
;;
;; Copyright (C) 2005 by P J Heslin
;;
;; Author: Peter Heslin <p.j.heslin@dur.ac.uk>
;; Additions by: Lennart Borgman
(defconst html-inlined:version "2.2") ;; Version:
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Massachusettes Ave,
;; Cambridge, MA 02139, USA.

;;; Commentary:
;;
;; When using html-mode, nxhtml-mode or nxml-mode to edit (X)HTML,
;; this file enables you to narrow the visible buffer to show just a
;; php/javascript/visual basic script block, or a css style block, and
;; then to switch from html-mode or nxml-mode to the appropriate mode
;; for that block.  When finished editing that block, it enables you
;; easily to show the whole buffer and switch back to the original
;; mode again.
;;
;; The user interface consists of a single keystroke: by default, C-c %.
;; To change it, customize the value of 'html-inlined-key.  In html-mode
;; or nxml-mode, this key looks to see if the cursor is inside a
;; script/style block, and if it is, it narrows the buffer to that
;; block and switches to the appropriate mode.  Hit the same key again
;; to go back to editing the whole file in the original mode.
;;
;; This file used to be called nxml-script.el, when it only supported
;; nxml-mode; now it has been completely rewritten and made a bit more
;; general, so it can be used with any major mode for editing (X)HTML.
;;
;; Based on html-script.el:
;; URL: http://www.dur.ac.uk/p.j.heslin/Software/Emacs/Download/html-script.el
;; The original idea was taken from html-helper-mode.el

;;; Installation:
;;
;; To install, put this file in your load-path, and require it via
;; your .emacs.
;;
;;  (require 'html-inlined)
;;  (html-inlined-add-key-to-modes)
;;
;; There are various customizations available, including a list of
;; modes in which to install html-inlined (defaults to html-mode and
;; nxml-mode).
;;
;; You also need to have the relevant autoloads set up for the script
;; modes you want to use, like so:
;;
;;  (autoload 'php-mode "php-mode" "PHP mode" t)
;;
;; Supported major modes for script/style blocks include php-mode,
;; css-mode, javascript-generic-mode, jde-mode, visual-basic-mode.
;;
;; You might also need to tell emacs to open .php, .jsp and such files
;; in html-mode or nxml-mode, like so:
;;
;;  (setq auto-mode-alist (cons '("\\.php[34]?$" . nxml-mode) auto-mode-alist))
;;
;; BUGS:
;;
;; I regularly use only CSS and PHP, very occasionally Javascript, and
;; Visual Basic not at all, and so the code for these last two has not
;; been tested.
;;
;; Changes:
;;
;; 1.0 First public release of nxml-script.el
;;
;; 2.0 Changed name to html-inlined.el and completely rewritten.
;; Removed 'nxml-script-function and 'nxml-script-region-function, as
;; too confusing.  Various user customization variables have changed.
;;
;; 2.1 Bugfix for javascript and vbscript and added ecmascript mode --
;; thanks to Mark Takacs.
;;
;; 2.2
;; 2006-02-08  Lennart Borgman
;;   * Renamed to html-inlined.el
;;   * (html-inlined-install-modes): Added nxhtml-mode
;;   * (html-inlined-narrow, html-inlined-narrow-handler):
;;      Only inner part of script or css tag shown in narrowed state.
;;   * (html-inlined-indentation, html-inlined-indent-line-function-orig,
;;      html-inlined-indent-line): Support for indentation

(eval-when-compile (require 'cl))

(defgroup html-inlined nil
  "Narrow \(X)HTML documents to script blocks and widen again"
  :tag "Narrow (X)HTML documents to script blocks and widen again"
  :group 'hypermedia
  :prefix "html-inlined-")

(defcustom html-inlined-install-modes '(nxml-mode nxhtmlmode html-mode)
  "A list of modes in which to install a binding for html-inlined.
The key to bind is defined by html-inlined-key"
  :group 'html-inlined)

(defvar html-inlined-inlined-keymap nil)

(defun html-inlined-make-inlined-keymap(inlined-key)
  (let ((m (make-sparse-keymap)))
    (define-key m inlined-key 'html-inlined-widen)
    (define-key m [(control ?x) ?n ?w] 'html-inlined-widen)
    (setq html-inlined-inlined-keymap m)))

(defcustom html-inlined-key [(control c) ?%]
  "Key to use to toggle narrowing and widening"
  :set (lambda(symbol value)
         (set-default symbol value)
         (html-inlined-make-inlined-keymap value))
  :group 'html-inlined)

(defcustom html-inlined-regions
  '(
    ;; Those two seems better to handle other ways:
    ;;("<%" "%>" visual-basic-mode)
    ;;("<\\?" "\\?>" php-mode c-mode)

    ("<style[ \t]+type=\"text/css\".*>" "</style>" css-mode c-mode)
    ("<style[ \t]+type=\"css\".*>" "</style>" css-mode c-mode)
    ("[ \t]style=\"" "\"" css-mode c-mode)
    ("<script[ \t]+language=\"vbscript\"[ \t]*>" "</script>" visual-basic-mode)
    ("<script[ \t]+type=\"text/vbscript\"[ \t]*>" "</script>" visual-basic-mode)
    ("<script[ \t]+language=\"javascript\"[ \t]*>" "</script>"
     ecmascript-mode jde-mode java-mode javascript-generic-mode c-mode)
    ("<script[ \t]+type=\"text/javascript\"[ \t]*>" "</script>"
     ecmascript-mode jde-mode java-mode javascript-generic-mode c-mode)
    ;;("\s-on[a-z]+=\"javascript:" "\""
    ("[ \t]on[a-z]+=\"javascript:" "\""
     ecmascript-mode jde-mode java-mode javascript-generic-mode c-mode)
    )
  "Define script regions.  Each entry should consist of a list of
  a starting regexp, an ending regexp, and a list of modes to try
  for that region in that order."
  :group 'html-inlined)

(defvar html-inlined-start-regexp
  (concat "\\(" (mapconcat (lambda (x) (car x)) html-inlined-regions "\\|") "\\)"))

(defvar html-inlined-original-mode nil)
(make-variable-buffer-local 'html-inlined-original-mode)
;; This immunizes it against kill-all-local-variables, which is run
;; when the new major-mode is called.
(put 'html-inlined-original-mode 'permanent-local t)

(defvar html-inlined-original-header nil)
(make-variable-buffer-local 'html-inlined-original-header)
(put 'html-inlined-original-header 'permanent-local t)


(defvar html-inlined-indentation nil)
(make-variable-buffer-local 'html-inlined-indentation)
(put 'html-inlined-indentation 'permanent-local t)


(defun html-inlined-find-indentation(narrow-start)
  (setq html-inlined-indentation 0)
  (save-excursion
    (goto-char narrow-start)
    (setq html-inlined-indentation (current-indentation)))
  html-inlined-indentation)

(defvar html-inlined-indent-line-function-orig nil)
(make-variable-buffer-local 'html-inlined-indent-line-function-orig)
(put 'html-inlined-indent-line-function-orig 'permanent-local t)

(defcustom html-indent-inline-indentation 4
  "Extra indentation for inlined part."
  :group 'html-inlined
  :type 'integer)

(fset 'html-inlined-indent-to-orig (symbol-function 'indent-to))
(defun html-inlined-indent-line(&optional something)
  (interactive)
  (let ((is-first-line
         (save-excursion
           (beginning-of-line)
           (string-match "\\`[ \n\r]*\\'"
                         (buffer-substring-no-properties
                          (point-min)(point))))))
    (if is-first-line
        (save-excursion
          (beginning-of-line)
          (let ((start-of-line (point)))
            (skip-syntax-forward "\\s-")
            (delete-region start-of-line (point)))
          (indent-to (+ html-inlined-indentation html-inlined-indentation) 0))
;;       ;; This does not work??
;;       (flet ((indent-to (column &optional minimum)
;;                         (let ((colnew
;;                                (if (= 0 column)
;;                                    (+ column html-inlined-indentation html-inlined-indentation)
;;                                  column)))
;;                           (message "flet ind-to: cn=%s" colnew)(sit-for 2)
;;                           (html-inlined-indent-to-orig colnew))))
      (beginning-of-line)
      (funcall html-inlined-indent-line-function-orig)
      ;; If 0 then it was not a relative indentation:
      (when (= 0 (current-indentation))
        (indent-to (+ html-inlined-indentation
                      html-indent-inline-indentation))))))

(defvar html-inlined-mode-map
  (let ((map (make-sparse-keymap))
        (menu-map (make-sparse-keymap)))
    (define-key map [tab] 'html-inlined-indent-line)
    (define-key menu-map [html-inlined-back]
      (list 'menu-item "Back to whole file" 'html-inlined-widen))
    (define-key map [menu-bar inlined-mode]
      (list 'menu-item "Inlined" menu-map))
    map))

(define-minor-mode html-inlined-mode
  "Temporary mode for keymap when editing inlined code."
  :keymap 'html-inlined-mode-map)

(defun html-inlined-narrow ()
  (interactive)
  (save-match-data
    (let* ((orig (point))
           (case-fold-search t)
           (handler-list
            (if (re-search-backward html-inlined-start-regexp nil t)
                (loop for x in html-inlined-regions
                      when (looking-at (car x)) return x
                      finally return nil)
              nil)))
      (if handler-list
          (progn
            (html-inlined-find-indentation (point))
            (html-inlined-narrow-handler orig handler-list (match-beginning 0)))
        (message "Not in an inlined region.")
        (goto-char orig)))))

(defvar html-inlined-overlay nil)
(make-variable-buffer-local 'html-inlined-overlay)
(put 'html-inlined-overlay 'permanent-local t)
(defvar html-inlined-overlay-markers-face
  (list
   (cons 'background-color "OliveDrab3")))

(defun html-inlined-narrow-handler (orig arg-list start)
  (let ((is-attr (not (memq ?< (append (car arg-list) nil)))))
    (if is-attr
        (beginning-of-line 1)
      (beginning-of-line 2))
    (let ((start-str
           (if is-attr
               (buffer-substring (point) (match-end 0))
             (buffer-substring (match-beginning 0) (point))))
          end-str
          (ending-re (cadr arg-list))
          (modes (cddr arg-list))
          (beg (if is-attr
                   (match-end 0)
                 (point)))
          end
          (case-fold-search t))
      (when is-attr (goto-char orig))
      (if (re-search-forward ending-re nil t)
          (if (>= (point) orig)
              (progn
                (setq html-inlined-original-mode major-mode)
                (setq html-inlined-original-header header-line-format)
                (when (overlayp html-inlined-overlay)
                  (delete-overlay html-inlined-overlay))
                (if is-attr
                    (setq end (1- (point)))
                  (beginning-of-line)
                  (setq end (point)))
                (goto-char orig)
                (setq html-inlined-overlay (make-overlay
                                            beg end
                                            nil
                                            nil t))
                (let ((s start-str))
                  (put-text-property 0 (length s)
                                     'face html-inlined-overlay-markers-face
                                     s)
                  (overlay-put html-inlined-overlay 'before-string s))
                ;; Add to blanks to avoid a bug that lets the cursor pass the after-string
                ;; Suggested by Kim Storm 2006-03-21
                (let ((s (concat (buffer-substring (match-beginning 0)
                                                   (save-excursion
                                                     (goto-char (match-end 0))
                                                     (beginning-of-line 2)
                                                     (point)))
                                 "  ")))
                  (put-text-property 0 (- (length s) 2)
                                     'face html-inlined-overlay-markers-face
                                     s)
                  ;;(put-text-property 0 1 'cursor t s)
                  (overlay-put html-inlined-overlay 'after-string s))
                ;; Kim suggests that this should hold the cursor in the "after-string", but it does not work
                ;; (though Kim's example work???):
                (put-text-property 0 1 'cursor t (overlay-get html-inlined-overlay 'after-string))
                (overlay-put html-inlined-overlay 'keymap html-inlined-inlined-keymap)
                ;;(narrow-to-region beg end)
                (loop for x in modes when (fboundp x) do (funcall x) and return nil
                      finally do (error "html-inlined: no relevant mode found."))
                (narrow-to-region beg end)
                (setq html-inlined-indent-line-function-orig indent-line-function)
                (setq indent-line-function 'html-inlined-indent-line)
                (setq indent-region-function nil)
                (html-inlined-mode t)
                (setq header-line-format
                      (replace-regexp-in-string
                       "%" "%%"
                       (concat
                        "%%%% Editing inlined code -- use "
                        (key-description html-inlined-key)
                        " to edit whole file again %%%%  ")))
                )
            (message "No inlined code or style found here."))
        (message "No inlined code or style here."))
      (goto-char orig)
      )))

(defun html-inlined-widen ()
  (interactive)
  (when (overlayp html-inlined-overlay)
    (delete-overlay html-inlined-overlay))
  (widen)
  (scroll-down 1)
  (html-inlined-mode 0)
  (funcall html-inlined-original-mode)
  (setq header-line-format html-inlined-original-header)
  (setq html-inlined-original-mode nil))

(defun html-inlined-install-narrow-key ()
  (local-set-key html-inlined-key 'html-inlined-narrow))

(defun html-inlined-add-key-to-modes()
  "Add `html-inlined-key' to supported major modes."
  (dolist (x html-inlined-install-modes)
    (add-hook
     (intern (concat (symbol-name x) "-hook"))
     'html-inlined-install-narrow-key)))

(provide 'html-inlined)

