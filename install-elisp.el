;;; install-elisp.el --- Simple Emacs Lisp installer
;; $Id: install-elisp.el,v 1.12 2007/07/25 20:38:08 rubikitch Exp $

;; Copyright (C) 2007  rubikitch

;; Author: rubikitch <rubikitch@ruby-lang.org>
;; Keywords: lisp, convenience, maint
;; URL: http://www.emacswiki.org/cgi-bin/wiki/download/install-elisp.el

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; Automate Emacs Lisp installation.
;; (1) download Emacs Lisp
;; (!) confirm installation
;; (2) save to your repository
;; (3) byte compile
;; (4) load
;; (5) show Emacs Lisp

;;; Installation:

;; If you use Emacs21 and under, you must have wget or other
;; command-line HTTP client.

;; You need to add to .emacs:  
;;   (require 'install-elisp)
;;   (setq install-elisp-repository-directory "~/.emacs.d/")

;;; Usage:

;; M-x install-elisp
;; M-x install-elisp-from-emacswiki
;; M-x dired-install-elisp-from-emacswiki
;;
;; It is convenient to add to your Emacs Lisp programs:
;;   (install-elisp "http://your.site/hogehoge.el")
;; because users have only to evaluate this sexp by C-x C-e.

;; If you want to complete EmacsWiki pagename, eval:
;;   (install-elisp "http://www.emacswiki.org/cgi-bin/wiki/download/oddmuse.el")
;; It is very convenient to access EmacsWiki with oddmuse.el.

;;; Upgrade this program:

;; Simply eval:
;;  (install-elisp-from-emacswiki "install-elisp.el")

;;; Related project:

;; Emacs Lisp Package Archive: http://tromey.com/elpa/

;;; History:

;; $Log: install-elisp.el,v $
;; Revision 1.12  2007/07/25 20:38:08  rubikitch
;; use defgroup.
;; install-elisp-retrieval-program:  "wget -q -O- '%s'" by default.
;;
;; Revision 1.11  2007/07/25 19:43:22  rubikitch
;; New variable: install-elisp-retrieval-program
;;
;; use `defcustom'.
;;
;; Revision 1.10  2007/07/25 19:27:43  rubikitch
;; update Commentary and Usage
;;
;; Revision 1.9  2007/07/25 19:20:08  rubikitch
;; install-elisp-confirmation-minor-mode: pretty mode-line-format
;;
;; Revision 1.8  2007/07/25 19:11:50  rubikitch
;; EmacsWiki HTTP workaround
;;
;; Revision 1.7  2007/07/25 19:02:57  rubikitch
;; Install confirmation for security.
;;
;; Revision 1.6  2007/07/25 17:58:39  rubikitch
;; New command: dired-install-elisp-from-emacswiki
;;
;; Revision 1.5  2007/07/25 17:50:30  rubikitch
;; `install-elisp-repository-directory': "~/.emacs.d/" by default.
;;
;; Revision 1.4  2007/07/25 17:47:21  rubikitch
;; rename function: %install-elisp-from -> install-elisp-from
;;
;; Revision 1.3  2007/07/25 17:46:13  rubikitch
;; use `url-retrieve-synchronously' if available
;;
;; Revision 1.2  2007/07/24 10:44:31  rubikitch
;; Fixed a serious bug.
;; New variable: install-elisp-use-view-mode
;;
;; Revision 1.1  2007/07/24 10:39:40  rubikitch
;; Initial revision
;;

;;; Code:

(defgroup install-elisp nil
  "Simple Emacs Lisp installer."
  :group 'hypermedia)

(defcustom install-elisp-repository-directory "~/.emacs.d/"
  "Directory to save Emacs Lisp programs downloaded by install-elisp.el. "
  :group 'install-elisp)

(defcustom install-elisp-use-view-mode t
  "If non-nil, turn on `view-mode' for installed Emacs Lisp program."
  :group 'install-elisp)

(defcustom install-elisp-use-url-retrieve (fboundp 'url-retrieve-synchronously)
  "If nil, use external command-line HTTP client instead.
See also `install-elisp-retrieval-program'."
  :group 'install-elisp)

(defcustom install-elisp-confirm-flag t
  "If non-nil, do install confirmation.
You should set it non-nil for security reason."
  :group 'install-elisp)

(defcustom install-elisp-retrieval-program "wget -q -O- '%s'"
  "URL retrieving program used when `install-elisp-use-url-retrieve' is nil.
If you use curl, set it to \"curl --silent '%s'\"."
  :group 'install-elisp)

(defvar install-elisp-filename nil)
(make-variable-buffer-local 'install-elisp-filename)

(defvar install-elisp-confirmation-minor-mode-map (make-sparse-keymap))

(defun %install-elisp-create-buffer (url)
  (cond (install-elisp-use-url-retrieve
         (switch-to-buffer (url-retrieve-synchronously url))
         (goto-char (point-min))
         (re-search-forward "^$" nil 'move)
         (delete-region (point-min) (1+ (point)))
         (%install-elisp-emacswiki-http-workaround))
        (t
         (let ((buffer (generate-new-buffer " *install-elisp-tmp*")))
           (shell-command (format install-elisp-retrieval-program url) buffer)
           (switch-to-buffer buffer)))))

;; I do not know why!!
(defun %install-elisp-emacswiki-http-workaround ()
  (save-excursion
    (let (buffer-read-only)
      (goto-char (1- (point-max)))
      (when (eq (char-before) ?\C-m)
        (delete-backward-char 2)))))

(defun install-elisp-proceed ()
  (interactive)
  (write-file install-elisp-filename)
  (byte-compile-file buffer-file-name t))

;;;###autoload
(defun install-elisp (url &optional filename)
  "Retrieve Emacs Lisp program from URL and save and byte-compile and load.
If optional FILENAME is supplied, save URL as FILENAME, otherwise URL's basename."
  (interactive "sInstall Emacs Lisp from URL: ")
  (if (null install-elisp-repository-directory)
      (with-output-to-temp-buffer "*Help*"
        (princ "You must prepare to use install-elisp program!
Set `install-elisp-repository-directory' to your local Emacs Lisp repository directory in your ~/.emacs.

For example: (setq install-elisp-repository-directory \"~/emacs/lisp/\")"))
    (%install-elisp-create-buffer url)
    (and install-elisp-use-view-mode (view-mode 1))
    (setq install-elisp-filename
          (expand-file-name (or filename (file-name-nondirectory url))
                            install-elisp-repository-directory))
    (if (not install-elisp-confirm-flag)
        (install-elisp-proceed)
      (install-elisp-confirmation-minor-mode)
      (message "Type C-c C-c to do installation!"))))

(defun install-elisp-from (baseurl)
  "Return higher-order function installing from BASEURL, which accepts an argument FILENAME."
  `(lambda (filename)
     (install-elisp (concat ,baseurl filename) filename)))

;;;###autoload
(defun install-elisp-from-emacswiki (filename)
  "Install Emacs Lisp program from the EmacsWiki."
  (interactive (list (if (fboundp 'oddmuse-read-pagename)
                         (oddmuse-read-pagename "EmacsWiki")
                       (read-string "PageName: "))))
  (funcall (install-elisp-from "http://www.emacswiki.org/cgi-bin/wiki/download/") filename))

;;;###autoload
(defun dired-install-elisp-from-emacswiki (&optional filename)
  "Upgrade the current Emacs Lisp program from the EmacsWiki."
  (interactive (list (dired-get-filename t)))
  (install-elisp-from-emacswiki filename))

(define-key install-elisp-confirmation-minor-mode-map "\C-c\C-c" 'install-elisp-proceed)

(define-minor-mode install-elisp-confirmation-minor-mode
  "Emacs Lisp install confirmation."
  nil "" install-elisp-confirmation-minor-mode-map
  (setq mode-line-format
        (format "%s: Type C-c C-c to install!"
                (file-name-nondirectory install-elisp-filename))))


(provide 'install-elisp)

;; How to save (DO NOT REMOVE!!)
;; (emacswiki-post "install-elisp.el")
;;; install-elisp.el ends here
