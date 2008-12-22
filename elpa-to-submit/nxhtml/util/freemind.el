;;; freemind.el --- Export to FreeMind
;;
;; Author: Lennart Borgman (lennart O borgman A gmail O com)
;; Created: 2008-02-19T17:52:56+0100 Tue
;; Version: 0.57
;; Last-Updated: 2008-05-04T13:05:33+0200 Sun
;; URL:
;; Keywords:
;; Compatibility:
;;
;; Features that might be required by this library:
;;
;;   `cl', `easymenu', `font-lock', `noutline', `org', `org-compat',
;;   `org-faces', `org-macs', `outline', `syntax', `time-date',
;;   `xml'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; This file tries to implement some functions useful for
;; transformation between org-mode and FreeMind files.
;;
;; To use this library you can add to your .emacs
;;
;;   (require 'freemind)
;;
;; Here are the commands you can use:
;;
;;    M-x `freemind-from-org-mode'
;;    M-x `freemind-from-org-mode-node'
;;    M-x `freemind-from-org-sparse-tree'
;;
;;    M-x `freemind-to-org-mode'
;;
;;    M-x `freemind-show'
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

(require 'xml)
(require 'org)
(eval-when-compile (require 'cl))

;; Fix-me: I am not sure these are useful:
;;
;; (defcustom freemind-main-fgcolor "black"
;;   "Color of main node's text."
;;   :type 'color
;;   :group 'freemind)

;; (defcustom freemind-main-color "black"
;;   "Background color of main node."
;;   :type 'color
;;   :group 'freemind)

;; (defcustom freemind-child-fgcolor "black"
;;   "Color of child nodes' text."
;;   :type 'color
;;   :group 'freemind)

;; (defcustom freemind-child-color "black"
;;   "Background color of child nodes."
;;   :type 'color
;;   :group 'freemind)


;;;###autoload
(defun freemind-show (mm-file)
  "Show file MM-FILE in Freemind."
  (interactive
   (list
    (save-match-data
      (let ((name (read-file-name "FreeMind file: "
                                  nil nil nil
                                  (if (buffer-file-name)
                                      (file-name-nondirectory (buffer-file-name))
                                    "")
                                  ;; Fix-me: Is this an Emacs bug?
                                  ;; This predicate function is never
                                  ;; called.
                                  (lambda (fn)
                                    (string-match "^mm$" (file-name-extension fn))))))
        (setq name (expand-file-name name))
        name))))
  (cond
   ((fboundp 'w32-shell-execute) (w32-shell-execute "open" mm-file))
   (t (message "Don't know how to show %s" mm-file))))

(defconst freemind-org-nfix "--org-mode: ")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Format converters

(defun freemind-escape-str-from-org (org-str)
  (let ((chars (append org-str nil))
        (fm-str ""))
    (dolist (cc chars)
      (setq fm-str
            (concat fm-str
                    (if (< cc 256)
                        (cond
                         ((= cc ?\") "&quot;")
                         ((= cc ?\&) "&amp;")
                         ((= cc ?\<) "&lt;")
                         (t (char-to-string cc)))
                      ;; Formatting as &#number; is maybe needed
                      ;; according to a bug report from kazuo
                      ;; fujimoto, but I have now instead added a xml
                      ;; processing instruction saying that the mm
                      ;; file is utf-8:
                      ;;
                      ;; (format "&#x%x;" (- cc ;; ?\x800))
                      (char-to-string cc)
                      ))))
    fm-str))

(defun freemind-unescape-str-to-org (fm-str)
  (let ((org-str fm-str))
    (setq org-str (replace-regexp-in-string "&quot;" "\"" org-str))
    (setq org-str (replace-regexp-in-string "&amp;" "&" org-str))
    (setq org-str (replace-regexp-in-string "&lt;" "<" org-str))
    (setq org-str (replace-regexp-in-string
               "&#x\\([a-f0-9]\\{2\\}\\);"
               (lambda (m)
                 (char-to-string (+ (string-to-number (match-string 1 str) 16)
                                    ?\x800)))
               org-str))))

;; (freemind-test-escape)
;; (defun freemind-test-escape ()
;;   (let* ((str1 "a quote: \", an amp: &, lt: <; over 256: öåäÖÅÄ")
;;          (str2 (freemind-escape-str-from-org str1))
;;          (str3 (freemind-unescape-str-to-org str2))
;;         )
;;     (unless (string= str1 str3)
;;       (error "str3=%s" str3))
;;     ))

(defun freemind-convert-links-from-org (org-str)
  (let ((fm-str (replace-regexp-in-string
                 (rx (not (any "[\""))
                     (submatch
                      "http"
                      (opt ?\s)
                      "://"
                      (1+
                       (any "-%.?@a-zA-Z0-9()_/:~=&#"))))
                 "[[\\1][\\1]]"
                 org-str)))
    (replace-regexp-in-string (rx "[["
                                  (submatch (*? nonl))
                                  "]["
                                  (submatch (*? nonl))
                                  "]]")
                              "<a href=\"\\1\">\\2</a>"
                              fm-str)))

;;(freemind-convert-links-to-org "<a href=\"http://www.somewhere/\">link-text</a>")
(defun freemind-convert-links-to-org (fm-str)
  (let ((org-str (replace-regexp-in-string
                  (rx "<a"
                      space
                      (0+
                       (0+ (not (any ">")))
                       space)
                      "href=\""
                      (submatch (0+ (not (any "\""))))
                      "\""
                      (0+ (not (any ">")))
                       ">"
                       (submatch (0+ (not (any "<"))))
                       "</a>")
                  "[[\\1][\\2]]"
                  fm-str)))
    org-str))

(defun freemind-convert-drawers-from-org (text)
  )

;; (freemind-test-links)
;; (defun freemind-test-links ()
;;   (let* ((str1 "[[http://www.somewhere/][link-text]")
;;          (str2 (freemind-convert-links-from-org str1))
;;          (str3 (freemind-convert-links-to-org str2))
;;         )
;;     (unless (string= str1 str3)
;;       (error "str3=%s" str3))
;;     ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Org => FreeMind

(defun freemind-org-text-to-freemind-subnode/note (node-name start end)
  ;; fix-me: doc
  (let ((text (buffer-substring-no-properties start end))
        (node-res "")
        (note-res ""))
    (save-match-data
      (setq text (freemind-escape-str-from-org text))
      ;; First see if there is something that should be moved to the
      ;; note part:
      (let (drawers)
        (while (string-match drawers-regexp text)
          (setq drawers (cons (match-string 0 text) drawers))
          (setq text
                (concat (substring text 0 (match-beginning 0))
                        (substring text (match-end 0))))
          )
        (when drawers
          (dolist (drawer drawers)
            (let ((lines (split-string drawer "\n")))
              (dolist (line lines)
                (setq note-res (concat
                                note-res
                                freemind-org-nfix line "<br />\n")))
              ))))

      (when (> (length note-res) 0)
        (setq note-res (concat
                        "<richcontent TYPE=\"NOTE\"><html>\n"
                        "<head>\n"
                        "</head>\n"
                        "<body>\n"
                        note-res
                        "</body>\n"
                        "</html>\n"
                        "</richcontent>\n"))
        )

      ;; There is always an LF char:
      (when (> (length text) 1)
        (setq node-res (concat
                        "<node style=\"bubble\" background_color=\"#eeee00\">\n"
                        "<richcontent TYPE=\"NODE\"><html>\n"
                        "<head>\n"
                        "<style type=\"text/css\">\n"
                        "<!--\n"
                        "p { margin-top: 0 }\n"
                        "-->\n"
                        "</style>\n"
                        "</head>\n"
                        "<body>\n"))
        (setq node-res (concat node-res "<p>"))
        (setq text (replace-regexp-in-string (rx "\n" (0+ blank) "\n") "</p><p>\n" text))
        ;;(setq text (replace-regexp-in-string (rx bol (1+ blank) eol) "" text))
        ;;(setq text (replace-regexp-in-string (rx bol (1+ blank)) "<br />" text))
        (setq text (replace-regexp-in-string "\n" "<br />" text))
        (freemind-convert-links-from-org text)
        (setq node-res (concat node-res text))
        (setq node-res (concat node-res "</p>\n"))
        (setq node-res (concat
                        node-res
                        "</body>\n"
                        "</html>\n"
                        "</richcontent>\n"
                        ;; Put a note that this is for the parent node
                        "<richcontent TYPE=\"NOTE\"><html>"
                        "<head>"
                        "</head>"
                        "<body>"
                        "<p>"
                        "-- This is more about \"" node-name "\" --"
                        "</p>"
                        "</body>"
                        "</html>"
                        "</richcontent>\n"
                        "</node>\n"
                        )))
      (list node-res note-res))))

(defun freemind-write-node ()
  (let* (this-icons
         this-bg-color
         this-m2-escaped
         this-rich-node
         this-rich-note
         )
    (when (string-match "TODO" this-m2)
      (setq this-m2 (replace-match "" nil nil this-m2))
      (add-to-list 'this-icons "button_cancel")
      (setq this-bg-color "#ffff88")
      (when (string-match "\\[#\\(.\\)\\]" this-m2)
        (let ((prior (string-to-char (match-string 1 this-m2))))
          (setq this-m2 (replace-match "" nil nil this-m2))
          (cond
           ((= prior ?A)
            (add-to-list 'this-icons "full-1")
            (setq this-bg-color "#ff0000"))
           ((= prior ?B)
            (add-to-list 'this-icons "full-2")
            (setq this-bg-color "#ffaa00"))
           ((= prior ?C)
            (add-to-list 'this-icons "full-3")
            (setq this-bg-color "#ffdd00"))
           ((= prior ?D)
            (add-to-list 'this-icons "full-4")
            (setq this-bg-color "#ffff00"))
           ((= prior ?E)
            (add-to-list 'this-icons "full-5"))
           ((= prior ?F)
            (add-to-list 'this-icons "full-6"))
           ((= prior ?G)
            (add-to-list 'this-icons "full-7"))
           ))))
    (setq this-m2 (org-trim this-m2))
    (setq this-m2-escaped (freemind-escape-str-from-org this-m2))
    (let ((node-notes (freemind-org-text-to-freemind-subnode/note
                       this-m2-escaped
                       this-node-end (1- next-node-start))))
      (setq this-rich-node (nth 0 node-notes))
      (setq this-rich-note (nth 1 node-notes)))
    (with-current-buffer mm-buffer
      (insert "<node text=\"" this-m2-escaped "\"")
      ;;(when (and (> current-level base-level) (> next-level current-level))
      (when (> next-level current-level)
        (unless (or this-children-visible
                    next-has-some-visible-child)
          (insert " folded=\"true\"")))
      (when (and (= current-level (1+ base-level))
                 (> num-left-nodes 0))
        (setq num-left-nodes (1- num-left-nodes))
        (insert " position=\"left\""))
      (when this-bg-color
        (insert " background_color=\"" this-bg-color "\""))
      (insert ">\n")
      (when this-icons
        (dolist (icon this-icons)
          (insert "<icon builtin=\"" icon "\"/>\n")))
      )
    (with-current-buffer mm-buffer
      (when this-rich-note (insert this-rich-note))
      (when this-rich-node (insert this-rich-node))
      )
  ))

(defun freemind-check-overwrite (file interactively)
  (if (file-exists-p file)
      (if interactively
          (y-or-n-p (format "File %s exists, replace it? " file))
        (error "File %s already exists" mm-file))
    t))

(defun freemind-look-for-visible-child (node-level)
  (save-excursion
    (save-match-data
      (let ((found-visible-child nil))
        (while (and (not found-visible-child)
                    (re-search-forward node-pattern nil t))
          (let* ((m1 (match-string-no-properties 1))
                 (level (length m1)))
            (if (>= node-level level)
                (setq found-visible-child 'none)
              (unless (get-char-property (line-beginning-position) 'invisible)
                (setq found-visible-child 'found)))))
        (eq found-visible-child 'found)
        ))))

(defun freemind-write-mm-buffer (org-buffer mm-buffer node-at-line)
  (with-current-buffer org-buffer
    (save-match-data
      (let* ((drawers (copy-sequence org-drawers))
             drawers-regexp
             (node-pattern (rx bol
                               (submatch (1+ "*"))
                               (1+ space)
                               (submatch (*? nonl))
                               eol))
             (num-top1-nodes 0)
             (num-top2-nodes 0)
             num-left-nodes
             (unclosed-nodes 0)
             (first-time t)
             (current-level 1)
             base-level
             prev-node-end
             rich-text
             unfinished-tag
             node-at-line-level
             node-at-line-last)
        (with-current-buffer mm-buffer
          (erase-buffer)
          (insert "<?xml version=\"1.0\" encoding=\"utf-8\"?>\n")
          (insert "<map version=\"0.9.Beta_15\">\n")
          (insert "<!-- FreeMind file, see http://freemind.sourceforge.net -->\n"))
        (save-excursion
          ;; Get special buffer vars:
          (goto-char (point-min))
          (while (re-search-forward (rx bol "#+DRAWERS:") nil t)
            (let ((dr-txt (buffer-substring-no-properties (match-end 0) (line-end-position))))
              (setq drawers (append drawers (split-string dr-txt) nil))))
          (setq drawers-regexp
                (concat (rx bol (0+ blank) ":")
                        (regexp-opt drawers)
                        (rx ":" (0+ blank)
                            "\n"
                            (*? anything)
                            "\n"
                            (0+ blank)
                            ":END:"
                            (0+ blank)
                            eol)
                        ))

          (if node-at-line
              ;; Get number of top nodes and last line for this node
              (progn
                (goto-line node-at-line)
                (unless (looking-at node-pattern)
                  (error "No node at line %s" node-at-line))
                (setq node-at-line-level (length (match-string-no-properties 1)))
                (forward-line)
                (setq node-at-line-last
                      (catch 'last-line
                        (while (re-search-forward node-pattern nil t)
                          (let* ((m1 (match-string-no-properties 1))
                                 (level (length m1)))
                            (if (<= level node-at-line-level)
                                (progn
                                  (beginning-of-line)
                                  (throw 'last-line (1- (point))))
                              (if (= level (1+ node-at-line-level))
                                  (setq num-top2-nodes (1+ num-top2-nodes))))))))
                (setq current-level node-at-line-level)
                (setq num-top1-nodes 1)
                (goto-line node-at-line))

            ;; First get number of top nodes
            (goto-char (point-min))
            (while (re-search-forward node-pattern nil t)
              (let* ((m1 (match-string-no-properties 1))
                     (level (length m1)))
                (if (= level 1)
                    (setq num-top1-nodes (1+ num-top1-nodes))
                  (if (= level 2)
                      (setq num-top2-nodes (1+ num-top2-nodes))))))
            ;; If there is more than one top node we need to insert a node
            ;; to keep them together.
            (goto-char (point-min))
            (when (> num-top1-nodes 1)
              (setq num-top2-nodes num-top1-nodes)
              (setq current-level 0)
              (let ((orig-name (if buffer-file-name
                                   (file-name-nondirectory (buffer-file-name))
                                 (buffer-name))))
                (with-current-buffer mm-buffer
                  (insert "<node text=\"" orig-name "\" background_color=\"#00bfff\">\n"
                          ;; Put a note that this is for the parent node
                          "<richcontent TYPE=\"NOTE\"><html>"
                          "<head>"
                          "</head>"
                          "<body>"
                          "<p>"
                          freemind-org-nfix "WHOLE FILE"
                          "</p>"
                          "</body>"
                          "</html>"
                          "</richcontent>\n")))))

          (setq num-left-nodes (floor num-top2-nodes 2))
          (setq base-level current-level)
          (let (this-m2
                this-node-end
                this-children-visible
                next-m2
                next-level
                next-has-some-visible-child
                next-children-visible)
            (while (and
                    (re-search-forward node-pattern nil t)
                    (if node-at-line-last (<= (point) node-at-line-last) t)
                    )
              (let* ((next-m1 (match-string-no-properties 1))
                     (next-node-start (match-beginning 0))
                     (next-node-end (match-end 0))
                     )
                (setq next-m2 (match-string-no-properties 2))
                (setq next-level (length next-m1))
                (setq next-children-visible
                      (not (eq 'outline
                               (get-char-property (line-end-position) 'invisible))))
                (setq next-has-some-visible-child
                      (if next-children-visible t
                        (freemind-look-for-visible-child next-level)))
                (when this-m2
                  (freemind-write-node))
                (when (if (= num-top1-nodes 1) (> current-level base-level) t)
                  (while (>= current-level next-level)
                    (with-current-buffer mm-buffer
                      (insert "</node>\n")
                      (setq current-level (1- current-level)))))
                (setq this-node-end (1+ next-node-end))
                (setq this-m2 next-m2)
                (setq current-level next-level)
                (setq this-children-visible next-children-visible)
                (forward-char)
                ))
;;;             (unless (if node-at-line-last
;;;                         (>= (point) node-at-line-last)
;;;                       nil)
              ;; Write last node:
              (setq this-m2 next-m2)
              (setq current-level next-level)
              (setq next-node-start (if node-at-line-last
                                        (1+ node-at-line-last)
                                      (point-max)))
              (freemind-write-node)
              (with-current-buffer mm-buffer (insert "</node>\n"))
              ;)
            )
          (with-current-buffer mm-buffer
            (while (> current-level base-level)
              (insert "</node>\n")
              (setq current-level (1- current-level))))
          (with-current-buffer mm-buffer
            (insert "</map>")
            (delete-trailing-whitespace)
            (goto-char (point-min))
            ))))))

;;;###autoload
(defun freemind-from-org-mode-node (node-line mm-file)
  "Convert node at line NODE-LINE to the FreeMind file MM-FILE."
  (interactive
   (progn
     (unless (org-back-to-heading nil)
       (error "Can't find org-mode node start"))
     (let* ((line (line-number-at-pos))
            (default-mm-file (concat (if buffer-file-name
                                         (file-name-nondirectory buffer-file-name)
                                       "nofile")
                                     "-line-" (number-to-string line)
                                     ".mm"))
            (mm-file (read-file-name "Output FreeMind file: " nil nil nil default-mm-file)))
       (list line mm-file))))
  (when (freemind-check-overwrite mm-file (called-interactively-p))
    (let ((org-buffer (current-buffer))
          (mm-buffer (find-file-noselect mm-file)))
      (freemind-write-mm-buffer org-buffer mm-buffer node-line)
      (with-current-buffer mm-buffer
        (basic-save-buffer)
        (when (called-interactively-p)
          (switch-to-buffer-other-window mm-buffer)
          (freemind-show buffer-file-name))))))

;;;###autoload
(defun freemind-from-org-mode (org-file mm-file)
  "Convert the `org-mode' file ORG-FILE to the FreeMind file MM-FILE."
  ;; Fix-me: better doc, include recommendations etc.
  (interactive
   (let* ((org-file buffer-file-name)
          (default-mm-file (concat
                            (if org-file
                                (file-name-nondirectory org-file)
                              "nofile")
                            ".mm"))
          (mm-file (read-file-name "Output FreeMind file: " nil nil nil default-mm-file)))
     (list org-file mm-file)))
  (when (freemind-check-overwrite mm-file (called-interactively-p))
    (let ((org-buffer (if org-file (find-file-noselect org-file) (current-buffer)))
          (mm-buffer (find-file-noselect mm-file)))
      (freemind-write-mm-buffer org-buffer mm-buffer nil)
      (with-current-buffer mm-buffer
        (basic-save-buffer)
        (when (called-interactively-p)
          (switch-to-buffer-other-window mm-buffer)
          (freemind-show buffer-file-name))))))

;;;###autoload
(defun freemind-from-org-sparse-tree (org-buffer mm-file)
  "Convert visible part of buffer ORG-BUFFER to FreeMind file MM-FILE."
  (interactive
   (let* ((org-file buffer-file-name)
          (default-mm-file (concat
                            (if org-file
                                (file-name-nondirectory org-file)
                              "nofile")
                            "-sparse.mm"))
          (mm-file (read-file-name "Output FreeMind file: " nil nil nil default-mm-file)))
     (list (current-buffer) mm-file)))
  (when (freemind-check-overwrite mm-file (called-interactively-p))
    (let (org-buffer
          (mm-buffer (find-file-noselect mm-file)))
      (save-window-excursion
        (org-export-visible ?\  nil)
        (setq org-buffer (current-buffer)))
      (freemind-write-mm-buffer org-buffer mm-buffer nil)
      (with-current-buffer mm-buffer
        (basic-save-buffer)
        (when (called-interactively-p)
          (switch-to-buffer-other-window mm-buffer)
          (freemind-show buffer-file-name))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; FreeMind => Org

;; (sort '(b a c) 'freemind-lt-symbols)
(defun freemind-lt-symbols (sym-a sym-b)
  (string< (symbol-name sym-a) (symbol-name sym-b)))
;; (sort '((b . 1) (a . 2) (c . 3)) 'freemind-lt-xml-attrs)
(defun freemind-lt-xml-attrs (attr-a attr-b)
  (string< (symbol-name (car attr-a)) (symbol-name (car attr-b))))

;; xml-parse-region gives things like
;; ((p nil "\n"
;;     (a
;;      ((href . "link"))
;;      "text")
;;     "\n"
;;     (b nil "hej")
;;     "\n"))

;; '(a . nil)

;; (freemind-symbols= 'a (car '(A B)))
(defsubst freemind-symbols= (sym-a sym-b)
  (or (eq sym-a sym-b)
      (string= (downcase (symbol-name sym-a))
               (downcase (symbol-name sym-b)))))

(defun freemind-get-children (parent path)
  "Find children node to PARENT from PATH.
PATH should be a list of steps, where each step has the form

  '(NODE-NAME (ATTR-NAME . ATTR-VALUE))
"
  ;; Fix-me: maybe implement op? step: Name, number, attr, attr op val
  ;; Fix-me: case insensitive version for children?
  (let* ((children (if (not (listp (car parent)))
                       (cddr parent)
                     (let (cs)
                       (dolist (p parent)
                         (dolist (c (cddr p))
                           (add-to-list 'cs c)))
                       cs)
                     ))
         (step (car path))
         (step-node (if (listp step) (car step) step))
         (step-attr-list (when (listp step) (sort (cdr step) 'freemind-lt-xml-attrs)))
         (path-tail (cdr path))
         path-children)
    (dolist (child children)
      ;; skip xml.el formatting nodes
      (unless (stringp child)
        ;; compare node name
        (when (if (not step-node)
                  t ;; any node name
                (freemind-symbols= step-node (car child)))
          (if (not step-attr-list)
              ;;(throw 'path-child child) ;; no attr to care about
              (add-to-list 'path-children child)
            (let* ((child-attr-list (cadr child))
                   (step-attr-copy (copy-sequence step-attr-list)))
              (dolist (child-attr child-attr-list)
                                   ;; Compare attr names:
                (when (freemind-symbols= (caar step-attr-copy) (car child-attr))
                  ;; Compare values:
                  (let ((step-val (cdar step-attr-copy))
                        (child-val (cdr child-attr)))
                    (when (if (not step-val)
                              t ;; any value
                            (string= step-val child-val))
                      (setq step-attr-copy (cdr step-attr-copy))))))
              ;; Did we find all?
              (unless step-attr-copy
                ;;(throw 'path-child child)
                (add-to-list 'path-children child)
                ))))))
    (if path-tail
        (freemind-get-children path-children path-tail)
      path-children)))

(defun freemind-get-richcontent-node (node)
  (let ((rc-nodes
         (freemind-get-children node '((richcontent (type . "NODE")) html body))))
    (when (> (length rc-nodes) 1)
      (lwarn t :warning "Unexpected structure: several <richcontent type=\"NODE\" ...>"))
    (car rc-nodes)))

(defun freemind-get-richcontent-note (node)
  (let ((rc-notes
         (freemind-get-children node '((richcontent (type . "NOTE")) html body))))
    (when (> (length rc-notes) 1)
      (lwarn t :warning "Unexpected structure: several <richcontent type=\"NOTE\" ...>"))
    (car rc-notes)))

(defun freemind-test-get-tree-text ()
  (let ((node '(p nil "\n"
                 (a
                  ((href . "link"))
                  "text")
                 "\n"
                 (b nil "hej")
                 "\n")))
    (freemind-get-tree-text node)))
;; (freemind-test-get-tree-text)

(defun freemind-get-tree-text (node)
  (when node
    (let ((ntxt "")
          (link nil)
          (lf-after nil))
      (dolist (n node)
        (case n
          ;;(a (setq is-link t) )
          ((h1 h2 h3 h4 h5 h6 p)
           ;;(setq ntxt (concat "\n" ntxt))
           (setq lf-after 2)
           )
          (br
           (setq lf-after 1)
           )
          (t
           (cond
            ((stringp n)
             (when (string= n "\n") (setq n ""))
             (if link
                 (setq ntxt (concat ntxt
                                    "[[" link "][" n "]]"))
               (setq ntxt (concat ntxt n))))
            ((and n (listp n))
             (if (symbolp (car n))
                 (setq ntxt (concat ntxt (freemind-get-tree-text n)))
               ;; This should be the attributes:
               (dolist (att-val n)
                 (let ((att (car att-val))
                       (val (cdr att-val)))
                   (when (eq att 'href)
                     (setq link val)))))
             )))))
      (if lf-after
          (setq ntxt (concat ntxt (make-string lf-after ?\n)))
        (setq ntxt (concat ntxt " ")))
      ;;(setq ntxt (concat ntxt (format "{%s}" n)))
      ntxt)))

(defun freemind-get-richcontent-node-text (node)
  "Get the node text as from the richcontent node."
  (save-match-data
    (let* ((rc (freemind-get-richcontent-node node))
           (txt (freemind-get-tree-text rc)))
      ;;(when txt (setq txt (replace-regexp-in-string (rx (1+ whitespace)) " " txt)))
      txt
      )))

(defun freemind-get-richcontent-note-text (node)
  "Get the node text as from the richcontent node."
  (save-match-data
    (let* ((rc (freemind-get-richcontent-note node))
           (txt (when rc (freemind-get-tree-text rc))))
      ;;(when txt (setq txt (replace-regexp-in-string (rx (1+ whitespace)) " " txt)))
      txt
      )))

(defun freemind-get-icon-names (node)
  (let* ((icon-nodes (freemind-get-children node '((icon ))))
         names)
    (dolist (icn icon-nodes)
      (setq names (cons (cdr (assq 'builtin (cadr icn))) names)))
    ;; (icon (builtin . "full-1"))
    names))

(defun freemind-node-to-org (node level skip-levels)
  (let ((qname (car node))
        (attributes (cadr node))
        text
        (note (freemind-get-richcontent-note-text node))
        (mark "-- This is more about ")
        (icons (freemind-get-icon-names node))
        (children (cddr node)))
    (when (< 0 (- level skip-levels))
      (dolist (attrib attributes)
        (case (car attrib)
          ('TEXT (setq text (cdr attrib)))
          ('text (setq text (cdr attrib)))))
      (unless text
        ;; There should be a richcontent node holding the text:
        (setq text (freemind-get-richcontent-node-text node)))
      (when icons
        (when (member "full-1" icons) (setq text (concat "[#A] " text)))
        (when (member "full-2" icons) (setq text (concat "[#B] " text)))
        (when (member "full-3" icons) (setq text (concat "[#C] " text)))
        (when (member "full-4" icons) (setq text (concat "[#D] " text)))
        (when (member "full-5" icons) (setq text (concat "[#E] " text)))
        (when (member "full-6" icons) (setq text (concat "[#F] " text)))
        (when (member "full-7" icons) (setq text (concat "[#G] " text)))
        (when (member "button_cancel" icons) (setq text (concat "TODO " text)))
        )
      (if (and note
               (string= mark (substring note 0 (length mark))))
          (progn
            (setq text (replace-regexp-in-string "\n $" "" text))
            (insert text))
        (case qname
          ('node
           (insert (make-string (- level skip-levels) ?*) " " text "\n")
           ))))
    (dolist (child children)
      (unless (stringp child)
        (freemind-node-to-org child (1+ level) skip-levels)))))

;; Fix-me: put back special things, like drawers that are stored in
;; the notes. Should maybe all notes contents be put in drawers?
;;;###autoload
(defun freemind-to-org-mode (mm-file org-file)
  "Convert FreeMind file MM-FILE to `org-mode' file ORG-FILE."
  (interactive
   (save-match-data
     (let* ((mm-file (buffer-file-name))
            (default-org-file (concat (file-name-nondirectory mm-file) ".org"))
            (org-file (read-file-name "Output org-mode file: " nil nil nil default-org-file)))
       (list mm-file org-file))))
  (when (freemind-check-overwrite org-file (called-interactively-p))
    (let ((mm-buffer (find-file-noselect mm-file))
          (org-buffer (find-file-noselect org-file)))
      (with-current-buffer mm-buffer
        (let* ((xml-list (xml-parse-file mm-file))
               (top-node (cadr (cddar xml-list)))
               (note (freemind-get-richcontent-note-text top-node))
               (skip-levels
                (if (and note
                         (string-match (rx bol "--org-mode: WHOLE FILE" eol) note))
                    1
                  0)))
          (with-current-buffer org-buffer
            (erase-buffer)
            (freemind-node-to-org top-node 1 skip-levels))
          (org-set-tags t t) ;; Align all tags
          (switch-to-buffer-other-window org-buffer)
          )))))

(provide 'freemind)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; freemind.el ends here
