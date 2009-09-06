;;; search-form.el --- Search form
;;
;; Author: Lennart Borgman (lennart O borgman A gmail O com)
;; Created: 2008-05-05T01:50:20+0200 Sun
;; Version: 0.11
;; Last-Updated:
;; URL:
;; Keywords:
;; Compatibility:
;;
;; Features that might be required by this library:
;;
;;   `cus-edit', `cus-face', `cus-load', `cus-start', `wid-edit'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; After an idea by Eric Ludlam on Emacs Devel:
;;
;;  http://lists.gnu.org/archive/html/emacs-devel/2008-05/msg00152.html
;;
;; NOT QUITE READY! Tagged files have not been tested.
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

(require 'cus-edit)

(defvar search-form-sfield nil)
(make-variable-buffer-local 'search-form-sfield)
(defvar search-form-rfield nil)
(make-variable-buffer-local 'search-form-rfield)

(defun search-form-multi-occur-get-buffers ()
  (let* ((bufs (list (read-buffer "First buffer to search: "
                                  (current-buffer) t)))
         (buf nil)
         (ido-ignore-item-temp-list bufs))
    (while (not (string-equal
                 (setq buf (read-buffer
                            (if (eq read-buffer-function 'ido-read-buffer)
                                "Next buffer to search (C-j to end): "
                              "Next buffer to search (RET to end): ")
                            nil t))
                 ""))
      (add-to-list 'bufs buf)
      (setq ido-ignore-item-temp-list bufs))
    (nreverse (mapcar #'get-buffer bufs))))

(defun search-form-notify-1 (use-search-field
                             use-replace-field
                             w
                             hide-form
                             display-orig-buf)
  (let ((search-string  (when use-search-field  (widget-value search-form-sfield)))
        (replace-string (when use-replace-field (widget-value search-form-rfield)))
        (search-form-buffer (current-buffer))
        (this-search (widget-get w :do-search))
        (do-it t))
    (if (and use-search-field
               (= 0 (length search-string)))
        (progn
          (setq do-it nil)
          (message "Please specify a search string"))
      (when (and use-replace-field
                 (= 0 (length replace-string)))
        (setq do-it nil)
        (message "Please specify a replace string")))
    (when do-it
      (if hide-form
          (progn
            (set-window-configuration search-form-win-config)
            (funcall this-search search-string)
            ;;(kill-buffer search-form-buffer)
            )
        (when display-orig-buf
          (let ((win (display-buffer search-form-current-buffer t)))
            (select-window win t)))
        ;;(funcall this-search search-string))
        (funcall this-search w)
        ))))

(defun search-form-notify-no-field (w &rest ignore)
  (search-form-notify-1 nil nil w nil t))

(defun search-form-notify-sfield (w &rest ignore)
  (search-form-notify-1 t nil w nil t))

(defun search-form-notify-sfield-nobuf (w &rest ignore)
  (search-form-notify-1 t nil w nil nil))

(defun search-form-notify-both-fields (w &rest ignore)
  (search-form-notify-1 t t w nil t))

(defvar search-form-current-buffer nil)

(defun search-form-insert-button (title function descr do-fun)
  (widget-insert "  ")
  (let ((button-title (format " %-15s " title)))
    (widget-create 'push-button
                   :do-search do-search-fun
                   :notify 'search-form-notify-no-field
                   :current-buffer search-form-current-buffer
                   button-title))
  (widget-insert " " descr)
  (widget-insert "\n"))

(defun search-form-insert-search (title search-fun descr do-search-fun no-buf)
  (widget-insert "  ")
  (let ((button-title (format " %-15s " title)))
    (if no-buf
        (widget-create 'push-button
                       :do-search do-search-fun
                       :notify 'search-form-notify-sfield-nobuf
                       :current-buffer search-form-current-buffer
                       button-title)
      (widget-create 'push-button
                     :do-search do-search-fun
                     :notify 'search-form-notify-sfield
                     :current-buffer search-form-current-buffer
                     button-title)
      ))
  (widget-insert " " descr " ")
  (search-form-insert-help search-fun)
  (widget-insert "\n"))

(defun search-form-insert-fb (descr
                              use-sfield
                              forward-fun
                              do-forward-fun
                              backward-fun
                              do-backward-fun)
  (widget-insert (format "  %s: " descr))
  (widget-create 'push-button
                 :do-search do-forward-fun
                 :use-sfield use-sfield
                 :notify '(lambda (widget &rest event)
                            (if (widget-get widget :use-sfield)
                                (search-form-notify-sfield widget)
                              (search-form-notify-no-field widget)))
                 :current-buffer search-form-current-buffer
                 " Forward ")
  (widget-insert " ")
  (search-form-insert-help forward-fun)
  (widget-insert "  ")
  (widget-create 'push-button
                 :do-search do-backward-fun
                 :use-sfield use-sfield
                 :notify '(lambda (widget &rest event)
                            (if (widget-get widget :use-sfield)
                                (search-form-notify-sfield widget)
                              (search-form-notify-no-field widget)))
                 :current-buffer search-form-current-buffer
                 " Backward ")
  (widget-insert " ")
  (search-form-insert-help backward-fun)
  (widget-insert "\n"))

(defun search-form-insert-replace (title replace-fun descr do-replace-fun)
  (widget-insert "  ")
  (let ((button-title (format " %-15s " title)))
    (widget-create 'push-button
                   :do-search do-replace-fun
                   :notify 'search-form-notify-both-fields
                   :current-buffer search-form-current-buffer
                   button-title))
  (widget-insert " " descr " ")
  (search-form-insert-help replace-fun)
  (widget-insert "\n"))

(defun search-form-insert-help (fun)
  (widget-insert "(")
  (widget-create 'function-link
                 :value fun
                 :tag "help"
                 :button-face 'link)
  (widget-insert ")"))

(defvar search-form-win-config nil)
(make-variable-buffer-local 'search-form-win-config)
(put 'search-form-win-config 'permanent-local t)

;;;###autoload
(defun search-form ()
  "Display a form for search and replace."
  (interactive)
  (let* ((buf-name "*Search Form*")
         (cur-buf (current-buffer))
         (buffer (get-buffer-create buf-name))
         (win-config (current-window-configuration)))
    (setq search-form-current-buffer (current-buffer))
    (with-current-buffer buffer
      (set (make-local-variable 'search-form-win-config) win-config))
    (switch-to-buffer-other-window buffer)

    (kill-all-local-variables) ;; why???
    (let ((inhibit-read-only t))
      (erase-buffer))
    ;;(Custom-mode)
    (remove-overlays)

    (make-local-variable 'widget-button-face)
    (setq widget-button-face custom-button)
    (setq show-trailing-whitespace nil)
    (when custom-raised-buttons
      (set (make-local-variable 'widget-push-button-prefix) "")
      (set (make-local-variable 'widget-push-button-suffix) "")
      (set (make-local-variable 'widget-link-prefix) "")
      (set (make-local-variable 'widget-link-suffix) ""))

    (widget-insert (propertize "Search/Replace, buffer: " 'face 'font-lock-comment-face))
    (widget-insert (format "%s" (buffer-name search-form-current-buffer)))
    (let ((file (buffer-file-name search-form-current-buffer)))
      (when file
        (insert " (" file ")")))
    (widget-insert "\n\n")
    (search-form-insert-fb
     "Incremental String Search" nil
     'isearch-forward
     (lambda (w) (call-interactively 'isearch-forward))
     'isearch-backward
     (lambda (w) (call-interactively 'isearch-backward)))

    (search-form-insert-fb
     "Incremental Regexp Search" nil
     'isearch-forward-regexp
     (lambda (w) (call-interactively 'isearch-forward-regexp))
     'isearch-backward-regexp
     (lambda (w) (call-interactively 'isearch-backward-regexp)))

    ;; Fix-me: in multiple buffers, from buffer-list

    (widget-insert (make-string (window-width) ?-) "\n")

    (widget-insert "Search: ")
    (setq search-form-sfield
          (widget-create 'editable-field
                         :size 58))
    (widget-insert "\n\n")
    (widget-insert (propertize "* Buffers:" 'face 'font-lock-comment-face) "\n")
    (search-form-insert-fb "String Search" t
                           'nonincremental-search-forward
                           (lambda (w) (nonincremental-search-forward search-string))
                           'nonincremental-search-backward
                           (lambda (w) (nonincremental-search-backward search-string)))

    (search-form-insert-fb "Regexp Search" t
                           'nonincremental-re-search-forward
                           (lambda (w) (nonincremental-re-search-forward search-string))
                           'nonincremental-re-search-backward
                           (lambda (w) (nonincremental-re-search-backward search-string)))

    ;; occur
    (search-form-insert-search "Occur" 'occur
                               "Lines in buffer"
                               (lambda (w)
                                 (with-current-buffer (widget-get w :current-buffer)
                                   (occur search-string)))
                               t)

    ;; multi-occur
    ;; Fix-me: This should be done from buffer-list. Have juri finished that?
    (search-form-insert-search "Multi-Occur" 'multi-occur
                               "Lines in specified buffers"
                               (lambda (w)
                                 (let ((bufs (search-form-multi-occur-get-buffers)))
                                   (multi-occur bufs search-string)))
                               t)
    ;;
    (widget-insert "\n")
    (widget-insert (propertize "* Files:" 'face 'font-lock-comment-face)
                   "\n")

    (search-form-insert-search "Search in Dir" 'lgrep
                               "Grep in directory"
                               'search-form-lgrep
                               t)
    (search-form-insert-search "Search in Tree" 'rgrep
                               "Grep in directory tree"
                               'search-form-rgrep
                               t)

    (widget-insert "\n")

    (search-form-insert-search "Tagged Files" 'tags-search
                               "Search files in tags table"
                               (lambda (w)
                                 (with-current-buffer (widget-get w :current-buffer)
                                   (tags-search search-string)))
                               t)

    (widget-insert (make-string (window-width) ?-) "\n")

    (widget-insert "Replace: ")
    (setq search-form-rfield
          (widget-create 'editable-field
                         :size 58))
    (widget-insert "\n\n")

    (widget-insert (propertize "* Buffers:" 'face 'font-lock-comment-face) "\n")
    (search-form-insert-replace "Replace String"
                                'query-replace
                                "In buffer from point"
                                (lambda (w)
                                  (query-replace search-string replace-string)))

    (search-form-insert-replace "Replace Regexp"
                                'query-replace-regexp
                                "In buffer from point"
                                (lambda (w)
                                  (query-replace-regexp search-string replace-string)))

    (widget-insert "\n" (propertize "* Files:" 'face 'font-lock-comment-face) "\n")

    ;; fix-me: rdir-query-replace (from to file-regexp root &optional delimited)
    (search-form-insert-replace "Replace in Dir"
                                'query-replace-regexp
                                "Replace in files in directory"
                                'search-form-ldir-replace)
    (search-form-insert-replace "Replace in Tree"
                                'query-replace-regexp
                                "Replace in files in directory tree"
                                'search-form-rdir-replace)

    (widget-insert "\n")

    (search-form-insert-replace "Tagged Files"
                                'query-replace-regexp
                                "Replace in files in tags tables"
                                (lambda (w)
                                  (tags-query-replace search-string replace-string)))

    (buffer-disable-undo)
    (widget-setup)
    (buffer-enable-undo)
    (use-local-map widget-keymap)
    (fit-window-to-buffer)
    (widget-forward 1)
    ))

(defun search-form-lgrep (w)
  (search-form-r-or-lgrep w t))

(defun search-form-rgrep (w)
  (search-form-r-or-lgrep w nil))

(defun search-form-r-or-lgrep (w l)
  (with-current-buffer (widget-get w :current-buffer)
    (let* ((regexp search-string)
           (files (grep-read-files regexp))
           (dir (read-directory-name (if l "In directory: "
                                       "Base directory: ")
                                     nil default-directory t)))
      (if l
          (lgrep regexp files dir)
        (rgrep regexp files dir)
        ))))

(defun search-form-ldir-replace (w)
  (search-form-l-or-r-dir-replace w t))

(defun search-form-rdir-replace (w)
  (search-form-l-or-r-dir-replace w nil))

(defun search-form-l-or-r-dir-replace (w l)
  (let ((files (replace-read-files search-string replace-string))
        (dir (read-directory-name (if l
                                      "In directory: "
                                    "In directory tree: ")
                                  nil
                                  (file-name-directory
                                   (buffer-file-name search-form-current-buffer))
                                  t)))
    (if l
        (ldir-query-replace search-string replace-string files dir)
      (rdir-query-replace search-string replace-string files dir))))

(provide 'search-form)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; search-form.el ends here
