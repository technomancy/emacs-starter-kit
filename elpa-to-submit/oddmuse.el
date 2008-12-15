;;; oddmuse.el -- edit pages on an Oddmuse wiki
;; $Id: oddmuse.el,v 2.2 2007/11/15 14:59:39 rubikitch Exp $

;; Copyright (C) 2006  Alex Schroeder
;;           (C) 2007  rubikitch <rubikitch@ruby-lang.org>

;; Latest version: http://www.emacswiki.org/cgi-bin/wiki/download/oddmuse.el
;; Discussion, feedback: http://www.emacswiki.org/cgi-bin/wiki/OddmuseMode

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of
;; the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE.  See the GNU General Public License for more details.

;; You should have received a copy of the GNU General Public
;; License along with this program; if not, write to the Free
;; Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
;; MA 02111-1307 USA

;;; Commentary:

;; A simple mode to edit pages on Oddmuse wikis using Emacs.

;; Since text formatting rules depend on the wiki you're writing for,
;; the font-locking can only be an approximation.

;; Put this file in a directory on your `load-path' and
;; add this to your init file:
;; (require 'oddmuse)
;; (setq url-proxy-services '(("http" . "your.proxy.host:portnumber")) ; if needed
;; (oddmuse-mode-initialize)
;; And then use M-x oddmuse-edit to start editing.

;;; History:

;; $Log: oddmuse.el,v $
;; Revision 2.2  2007/11/15 14:59:39  rubikitch
;; Changed command behavior: oddmuse-post
;;  With prefix argument, prompts pagename, otherwise set pagename as basename of `buffer-file-name'.
;;
;; Revision 2.1  2007/11/08 17:12:10  rubikitch
;; New implementation! oddmuse.el uses url.el instead of curl.
;; If you use Emacs21, try http://www.emacswiki.org/cgi-bin/wiki/download/oddmuse-curl.el
;; (patch by Taiki SUGAWARA <buzz.taiki@gmail.com>)
;;
;; Revision 1.23  2007/11/08 16:55:12  rubikitch
;; oddmuse-make-completion-table: bugfix
;;
;; Revision 1.22  2007/10/27 08:39:45  rubikitch
;; New command: oddmuse-compute-pagename-completion-table (patch by Stefan Kamphausen)
;;
;; Revision 1.21  2007/10/27 08:20:47  rubikitch
;; fixed shell-quoting problem: "unmatched '"
;;
;; Revision 1.20  2007/10/27 07:39:47  rubikitch
;; New functions: oddmuse-read-wiki-and-pagename, oddmuse-url
;; New commands: oddmuse-browse-page, oddmuse-browse-this-page, oddmuse-kill-url
;;
;; Revision 1.19  2007/10/27 03:08:44  rubikitch
;; * s/egrep/grep -E/
;; * oddmuse-revision-check-command test
;;
;; Revision 1.18  2007/07/12 06:52:56  rubikitch
;; new command: emacswiki-post
;; Use emacswiki-post at `How to save'.
;;
;; Revision 1.17  2007/07/11 20:01:20  rubikitch
;; oddmuse-revision-check-command: use curl instead of w3m
;;
;; Revision 1.16  2007/06/16 15:12:09  rubikitch
;; oddmuse-current-free-link-contents: s/ /_/g
;;
;; Revision 1.15  2007/06/16 14:53:59  rubikitch
;; * refactored `oddmuse-follow'.
;; * fixed WikiName determination bug.
;;
;; Revision 1.14  2007/06/16 14:21:35  rubikitch
;; added RatpoisonWiki and StumpwmWiki to oddmuse-wikis.
;;
;; Revision 1.13  2007/04/24 10:13:26  rubikitch
;; * silence byte compiler (free variables)
;; * `C-u C-c C-o' uses `oddmuse-read-pagename' now.
;;
;; Revision 1.12  2007/04/12 09:02:28  rubikitch
;; applied `Enable "this is a minor change"' patch by Xavier Maillard.
;; new command: oddmuse-toggle-minor (C-c C-m)
;; new variable: oddmuse-use-always-minor
;;
;; Revision 1.11  2007/04/10 16:40:25  rubikitch
;; use autoload cookies. use eval-when-compile. (patch by Xavier Maillard)
;;
;; Revision 1.10  2007/01/17 16:30:11  rubikitch
;; added URLs
;;
;; Revision 1.9  2007/01/17 16:28:20  rubikitch
;; added `(set-buffer-modified-p nil)' in oddmuse-edit.
;;
;; Revision 1.8  2007/01/15 14:28:42  rubikitch
;; new command `oddmuse-insert-pagename'
;;
;; Revision 1.7  2007/01/14 18:15:05  rubikitch
;; fixed `rename-buffer' problem
;;
;; Revision 1.6  2007/01/14 18:07:22  rubikitch
;; fixed typo
;;
;; Revision 1.5  2007/01/14 18:05:02  rubikitch
;; Pages are stored in files. (for fault tolerance)
;;
;; Revision 1.4  2007/01/14 17:31:42  rubikitch
;; refactored.
;; `oddmuse-wikis' is a list of triplets(wikiname url coding-system).
;;
;; Revision 1.3  2007/01/14 16:57:59  rubikitch
;; Maintained by rubikitch
;;

;;; Code:
(require 'cl)
(eval-when-compile  (require 'sgml-mode) (require 'skeleton))
(require 'url)
(require 'url-http)

(defcustom oddmuse-directory "~/emacs/oddmuse"
  "Directory to store oddmuse pages."
  :type '(string)
  :group 'oddmuse)

(defcustom oddmuse-wikis
  '(("TestWiki" "http://www.emacswiki.org/cgi-bin/test" utf-8)
    ("EmacsWiki" "http://www.emacswiki.org/cgi-bin/emacs" utf-8)
    ("CommunityWiki" "http://www.communitywiki.org/cw" utf-8)
    ("RatpoisonWiki" "http://ratpoison.antidesktop.net/cgi-bin/wiki" utf-8)
    ("StumpwmWiki" "http://stumpwm.antidesktop.net/cgi-bin/wiki" utf-8)
    ("OddmuseWiki" "http://www.oddmuse.org/cgi-bin/oddmuse" utf-8))
  "Alist mapping wiki names to URLs."
  :type '(repeat (list (string :tag "Wiki")
                       (string :tag "URL")
                       (symbol :tag "Coding System")))
  :group 'oddmuse)

(defcustom oddmuse-username user-full-name
  "Username to use when posting.
Setting a username is the polite thing to do."
  :type '(string)
  :group 'oddmuse)

(defcustom oddmuse-password ""
  "Password to use when posting.
You only need this if you want to edit locked pages and you
know an administrator password."
  :type '(string)
  :group 'oddmuse)

(defcustom oddmuse-use-always-minor nil
  "When t, set all the minor mode bit to all editions.
This can be changed for each edition using `oddmuse-toggle-minor'."
 :type '(boolean)
 :group 'oddmuse)

(defvar oddmuse-get
  "action=browse;raw=1;id=%t"
  "Command to use for publishing pages.
It must print the page to stdout.

%t  URL encoded pagename, eg. HowTo, How_To, or How%20To")

(defvar oddmuse-post
  (concat "title=%t;"
          "summary=%s;"
          "username=%u;"
          "password=%p;"
          "recent_edit=%m;"
          "text=%x")
  "URL arguments to use for publishing pages.

%t  pagename
%s  summary
%u  username
%p  password
%x  text")

(defvar oddmuse-link-pattern
  "\\<[A-Z\xc0-\xde]+[a-z\xdf-\xff]+\\([A-Z\xc0-\xde]+[a-z\xdf-\xff]*\\)+\\>"
  "The pattern used for finding WikiName.")

(defvar oddmuse-wiki nil
  "The current wiki.
Must match a key from `oddmuse-wikis'.")

(defvar oddmuse-page-name nil
  "Pagename of the current buffer.")

(defvar oddmuse-pages-hash (make-hash-table :test 'equal)
  "The wiki-name / pages pairs.")

(defvar oddmuse-index-get
  "action=index;raw=1"
  "URL arguments to use for publishing index pages.")

(defvar oddmuse-revision-check
  "action=rc;raw=1;showedit=1;rcidonly=%t"
  "URL arguments to use for checking current revision of this page.

%t  URL encoded pagename, eg. HowTo, How_To, or How%20To")

(defvar oddmuse-revision-check-regexp "^\\(generator\\|last-modified\\|revision\\).+"
  "A revision check regexp used to show prompt.")

(defvar oddmuse-minor nil
  "Is this editing a minor change ?")

(defun oddmuse-mode-initialize ()
  (add-to-list 'auto-mode-alist
               `(,(expand-file-name oddmuse-directory) . oddmuse-mode)))

(define-derived-mode oddmuse-mode text-mode "Odd"
  "Simple mode to edit wiki pages.

Use \\[oddmuse-follow] to follow links. With prefix, allows you
to specify the target page yourself.

Use \\[oddmuse-post] to post changes. With prefix, allows you to
post the page to a different wiki.

Use \\[oddmuse-edit] to edit a different page. With prefix,
forces a reload of the page instead of just popping to the buffer
if you are already editing the page.

Customize `oddmuse-wikis' to add more wikis to the list.

\\{oddmuse-mode-map}"
  (font-lock-add-keywords
   nil
   '(("^ .+?$" . font-lock-comment-face)
     ("<\\(/?[a-z]+\\)" 1 font-lock-function-name-face)
     ("^[*#]\\([*#]+\\)" . font-lock-constant-face)
     ("^\\([*#]\\)[^*#]" 1 font-lock-builtin-face)))
  (font-lock-add-keywords
   nil
   (list (cons (symbol-value 'oddmuse-link-pattern)
               'font-lock-keyword-face)))
  (font-lock-mode 1)
  (goto-address)
  (set (make-local-variable 'sgml-tag-alist)
       `(("b") ("code") ("em") ("i") ("strong") ("nowiki")
         ("pre" \n) ("tt") ("u")))
  (set (make-local-variable 'skeleton-transformation) 'identity)
  (and buffer-file-name
       (set (make-local-variable 'oddmuse-wiki)
            (file-name-nondirectory
             (substring (file-name-directory buffer-file-name) 0 -1)))
       (set (make-local-variable 'oddmuse-page-name)
            (file-name-nondirectory buffer-file-name))
       ;; Initialize oddmuse-minor according to `oddmuse-use-always-minor'
       (set (make-local-variable 'oddmuse-minor)
            oddmuse-use-always-minor))

  (setq indent-tabs-mode nil))


(autoload 'sgml-tag "sgml-mode" t)
(define-key oddmuse-mode-map (kbd "C-c C-t") 'sgml-tag)
(define-key oddmuse-mode-map (kbd "C-c C-o") 'oddmuse-follow)
(define-key oddmuse-mode-map (kbd "C-c C-m") 'oddmuse-toggle-minor)
(define-key oddmuse-mode-map (kbd "C-c C-c") 'oddmuse-post)
(define-key oddmuse-mode-map (kbd "C-x C-v") 'oddmuse-revert)
(define-key oddmuse-mode-map (kbd "C-c C-f") 'oddmuse-edit)
(define-key oddmuse-mode-map (kbd "C-c C-i") 'oddmuse-insert-pagename)

;; This has been stolen from simple-wiki-edit
;;;###autoload
(defun oddmuse-toggle-minor (&optional arg)
  "Toggle minor mode state."
  (interactive)
  (let ((num (prefix-numeric-value arg)))
    (cond
     ((or (not arg) (equal num 0))
      (setq oddmuse-minor (not oddmuse-minor)))
     ((> num 0) (set 'oddmuse-minor t))
     ((< num 0) (set 'oddmuse-minor nil)))
    (message "Oddmuse Minor set to %S" oddmuse-minor)
    oddmuse-minor))

(add-to-list 'minor-mode-alist
             '(oddmuse-minor " [MINOR]"))

(defun oddmuse-format (args coding)
  "Internal: Substitute oddmuse format flags according to `oddmuse-page-name',
  `summary', `oddmuse-username',`oddmuse-password', `text'
Each ARGS is url-encoded with CODING."
  (dolist (pair '(("%t" . oddmuse-page-name) ("%s" . summary)
                  ("%u" . oddmuse-username) ("%m" . oddmuse-minor)
                  ("%p" . oddmuse-password) ("%x" . text)))
    (when (and (boundp (cdr pair)) (stringp (symbol-value (cdr pair))))
      (setq args
            (replace-regexp-in-string
             (car pair)
             (url-hexify-string
              (encode-coding-string (symbol-value (cdr pair))
                                    coding))
             args t t))))
  args)

(defun oddmuse-format-url (url args coding)
  "Internal: Substitute oddmuse format flags and concatnate URL.
see: `oddmuse-format'"
  (concat url "?" (oddmuse-format args coding)))

(defun oddmuse-read-wiki-and-pagename ()
  "Read an wikiname and a pagename of `oddmuse-wikis' with completion."
  (let ((wiki (completing-read "Wiki: " oddmuse-wikis nil t oddmuse-wiki)))
    (list wiki (oddmuse-read-pagename wiki))))

(defun oddmuse-retrieve (url method buffer coding &optional data)
  "Retrieve URL with METHOD, and put response content to BUFFER.
Content is decoded with CODING.
If DATA is non-nil, it is used to POST data."
  (let* ((url-request-extra-headers
          (and (string= method "POST")
               '(("Content-type: application/x-www-form-urlencoded;"))))
         (url-request-method method)
         (url-request-data data))
    (oddmuse-process-response (url-retrieve-synchronously url) buffer coding)))

(defun oddmuse-retrieve-to-string (url method coding &optional data)
  "Return a string retrieved with URL.
see: `oddmuse-retrieve'"
  (with-temp-buffer
    (oddmuse-retrieve url method (current-buffer) coding data)
    (buffer-string)))

(defun oddmuse-process-response (response-buffer buffer coding)
  "Put response body from RESPONSE-BUFFER to BUFFER.
It is decoded with CODING."
  (declare (special url-http-end-of-headers))
  (when buffer
    (with-current-buffer buffer
      (insert
       (with-current-buffer response-buffer
         (set-buffer-multibyte t)
         (goto-char (1+ url-http-end-of-headers))
         (decode-coding-region
          (point) (point-max)
          (coding-system-change-eol-conversion coding 'dos))
         (buffer-substring (point) (point-max))))
      (goto-char (point-min))))
  (kill-buffer response-buffer))

;;;###autoload
(defun oddmuse-edit (wiki pagename)
  "Edit a page on a wiki.
WIKI is the name of the wiki as defined in `oddmuse-wikis',
PAGENAME is the pagename of the page you want to edit.
Use a prefix argument to force a reload of the page."
  (interactive (oddmuse-read-wiki-and-pagename))
  (make-directory (concat oddmuse-directory "/" wiki) t)
  (let ((name (concat wiki ":" pagename)))
    (if (and (get-buffer name)
             (not current-prefix-arg))
        (pop-to-buffer (get-buffer name))
      (let* ((triplet (assoc wiki oddmuse-wikis))
             (url (cadr triplet))
             (oddmuse-page-name pagename)
             (coding (caddr triplet))
             (buf (find-file-noselect
                   (concat oddmuse-directory "/" wiki "/" pagename))))
        (setq url (oddmuse-format-url url oddmuse-get coding))
        (set-buffer buf)
        (unless (equal name (buffer-name)) (rename-buffer name))
        (erase-buffer)
        (oddmuse-retrieve url "GET" buf coding)
        (set-buffer-modified-p nil)
        (pop-to-buffer buf)))
    (oddmuse-mode)))

(defalias 'oddmuse-go 'oddmuse-edit)

(autoload 'word-at-point "thingatpt")

;;;###autoload
(defun oddmuse-follow (arg)
  "Figure out what page we need to visit
and call `oddmuse-edit' on it."
  (interactive "P")
  (let ((pagename (if arg (oddmuse-read-pagename oddmuse-wiki)
                    (oddmuse-pagename-at-point))))
    (oddmuse-edit (or oddmuse-wiki
                      (read-from-minibuffer "URL: "))
                  pagename)))

(defun oddmuse-current-free-link-contents ()
  "Free link contents if the point is between [[ and ]]."
  (save-excursion
    (let* ((pos (point))
           (start (search-backward "[[" nil t))
           (end (search-forward "]]" nil t)))
      (and start end (>= end pos)
           (replace-regexp-in-string
            " " "_"
            (buffer-substring (+ start 2) (- end 2)))))))

(defun oddmuse-pagename-at-point ()
  "Page name at point."
  (let ((pagename (word-at-point)))
    (cond ((oddmuse-current-free-link-contents))
          ((oddmuse-wikiname-p pagename)
           pagename)
          (t
           (error "No link found at point")))))

(defun oddmuse-wikiname-p (pagename)
  "Whether PAGENAME is WikiName or not."
  (let (case-fold-search)
    (string-match (concat "^" oddmuse-link-pattern "$") pagename)))

;; (oddmuse-wikiname-p "WikiName")
;; (oddmuse-wikiname-p "not-wikiname")
;; (oddmuse-wikiname-p "notWikiName")

(defun oddmuse-revision-check-prompt (url coding)
  (with-temp-buffer
    (oddmuse-retrieve url "GET" (current-buffer) coding)
    (goto-char (point-min))
    (let (list)
      (while (re-search-forward oddmuse-revision-check-regexp nil t)
        (push (match-string 0) list))
      (concat
       (mapconcat 'identity (last (nreverse list) 3) "\n")
       "\nSave ? "))))

;;;###autoload
(defun oddmuse-post (summary)
  "Post the current buffer to the current wiki.
The current wiki is taken from `oddmuse-wiki'."
  (interactive "sSummary: ")
  ;; when using prefix or on a buffer that is not in oddmuse-mode
  (when (or (not oddmuse-wiki) current-prefix-arg)
    (set (make-local-variable 'oddmuse-wiki)
         (completing-read "Wiki: " oddmuse-wikis nil t)))
  (when (not oddmuse-page-name)
    (set (make-local-variable 'oddmuse-page-name)
         (read-from-minibuffer "Pagename: " (buffer-name))))
  (let* ((triplet (assoc oddmuse-wiki oddmuse-wikis))
         (url (cadr triplet))
         (oddmuse-minor (if oddmuse-minor "on" "off"))
         (coding (caddr triplet))
         (check-url (oddmuse-format-url url oddmuse-revision-check coding)))
    (when (y-or-n-p (oddmuse-revision-check-prompt check-url coding))
      (let ((text (buffer-string)))
        (and buffer-file-name (basic-save-buffer))
        (oddmuse-retrieve url "POST" nil coding
                          (oddmuse-format oddmuse-post coding))
        (message "Posted")))))

(defun oddmuse-make-completion-table (wiki)
  "Create pagename completion table for WIKI. if available, return precomputed one."
  (or (gethash wiki oddmuse-pages-hash)
      (oddmuse-compute-pagename-completion-table wiki)))

(defun oddmuse-compute-pagename-completion-table (&optional wiki-arg)
  "Really fetch the list of pagenames from WIKI.
This command is used to reflect new pages to `oddmuse-pages-hash'."
  (interactive)
  (let* ((wiki (or wiki-arg
                   (completing-read "Wiki: " oddmuse-wikis nil t oddmuse-wiki)))
         (url (cadr (assoc wiki oddmuse-wikis)))
         (coding (caddr (assoc wiki oddmuse-wikis)))
         table)
    (setq url (oddmuse-format-url url oddmuse-index-get coding))
    (prog1
        (setq table (mapcar 'list
                            (split-string
                             (oddmuse-retrieve-to-string url "GET" coding))))
      (puthash wiki table oddmuse-pages-hash))))

(defun oddmuse-read-pagename (wiki)
  "Read a pagename of WIKI with completion."
  (completing-read "Pagename: " (oddmuse-make-completion-table wiki)))

;;;###autoload
(defun oddmuse-revert ()
  "Revert this oddmuse page."
  (interactive)
  (let ((current-prefix-arg 4))
    (oddmuse-edit oddmuse-wiki oddmuse-page-name)))

;;;###autoload
(defun oddmuse-insert-pagename (pagename)
  "Insert a PAGENAME of current wiki with completion."
  (interactive (list (oddmuse-read-pagename oddmuse-wiki)))
  (insert pagename))

;;;###autoload
(defun emacswiki-post (&optional pagename summary)
  "Post the current buffer to the EmacsWiki.
If this command is invoked interactively: with prefix argument, prompts pagename,
otherwise set pagename as basename of `buffer-file-name'.

This command is intended to post current EmacsLisp program easily."
  (interactive)
  (let* ((oddmuse-wiki "EmacsWiki")
         (oddmuse-page-name (or pagename
                                (and (not current-prefix-arg)
                                     buffer-file-name
                                     (file-name-nondirectory buffer-file-name))
                                (oddmuse-read-pagename oddmuse-wiki)))
         (summary (or summary (read-string "Summary: "))))
    (oddmuse-post summary)))

(defun oddmuse-url (wiki pagename)
  "Get the URL of oddmuse wiki."
  (condition-case v
      (concat (or (cadr (assoc wiki oddmuse-wikis)) (error)) "/" pagename)
    (error nil)))

;; (oddmuse-url "EmacsWiki" "OddmuseMode")
;; (oddmuse-url "a" "OddmuseMode")

;;;###autoload
(defun oddmuse-browse-page (wiki pagename)
  "Ask a WWW browser to load an oddmuse page.
WIKI is the name of the wiki as defined in `oddmuse-wikis',
PAGENAME is the pagename of the page you want to browse."
  (interactive (oddmuse-read-wiki-and-pagename))
  (browse-url (oddmuse-url wiki pagename)))

;;;###autoload
(defun oddmuse-browse-this-page ()
  "Ask a WWW browser to load current oddmuse page."
  (interactive)
  (oddmuse-browse-page oddmuse-wiki oddmuse-page-name))

;;;###autoload
(defun oddmuse-kill-url ()
  "Make the URL of current oddmuse page the latest kill in the kill ring."
  (interactive)
  (kill-new (oddmuse-url oddmuse-wiki oddmuse-page-name)))

(provide 'oddmuse)

;; How to save (DO NOT REMOVE!!)
;; (emacswiki-post "oddmuse.el")
;;; oddmuse.el ends here
