;;; hfyview.el --- View current buffer as html in web browser

;; Copyright (C) 2005, 2006, 2007 by Lennart Borgman

;; Author: Lennart Borgman
;; Created: Fri Oct 21 2005
(defconst hfyview:version "0.63") ;; Version:
;; Last-Updated: 2009-08-04 Tue
;; Keywords: printing
;; URL: http://OurComments.org/Emacs/DL/elisp/hfyview.el
;; Compatibility:
;;
;;
;; Features that might be required by this library:
;;
;; `easymenu'.
;;
;;
;; You can find htmlfontify.el at
;;   http://www.emacswiki.org/cgi-bin/wiki/HtmlFontify
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;  This file shows the current buffer in your web browser with all
;;  the colors it has. The purpose is mainly to make it possible to
;;  easily print what you see in Emacs in colors on different
;;  platforms.
;;
;;  Put this file in your load-path and in your .emacs this:
;;
;;      (require 'hfyview)
;;
;;  This defines the commands `hfyview-buffer', `hfyview-region' and
;;  `hfyview-window' which will show the whole or a part of the buffer
;;  in your web browser.
;;
;;  You can add those commands to the menus by customizing
;;  `hfyview-quick-print-in-files-menu' to t. This will add an entry
;;  "Quick Print (Using Web Browser)" to the files menu.
;;
;;
;;  There is also a command `hfyview-frame' to take a "screen shot" of
;;  your current frame and produce an html look-alike page. If you
;;  turn on `hfyview-mode' you get this function on the <apps> key in
;;  most situations.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change log:
;;
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; To find out more about the GNU General Public License you can visit
;; Free Software Foundation's website http://www.fsf.org/.  Or, write
;; to the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(eval-when-compile (require 'cl))
(eval-when-compile (require 'htmlfontify))
(require 'easymenu)

(defvar hfyview-selected-window)

(defvar hfyview-mode-emulation-map
  (let ((m (make-sparse-keymap)))
    ;;(define-key m [apps] 'hfyview-frame)
    m))

;;(define-key hfyview-mode-emulation-map [apps] 'hfy-show-grabbed)

(defvar hfyview-mode-emulation-maps
  (list (cons 'hfyview-mode hfyview-mode-emulation-map)))

;; Fix-me: which are needed?
(defconst hfyview-mode-other-maps
  '(
    hfyview-mode-emulation-map
    minibuffer-local-completion-map
    minibuffer-local-filename-completion-map
    minibuffer-local-isearch-map
    minibuffer-local-map
    minibuffer-local-must-match-filename-map
    minibuffer-local-must-match-map
    minibuffer-local-ns-map
    viper-minibuffer-map
    isearch-mode-map))

(define-minor-mode hfyview-mode
  "Define some useful keys for `hfyview-frame' etc.
Put this mode in `emulation-mode-map-alists' so they can be used
at any time."
  :global t
  :group 'htmlfontify
  (if hfyview-mode
      (progn
        (add-hook 'pre-command-hook 'hfy-grab-minibuffer-content)
        (add-hook 'post-command-hook 'hfy-grab-echo-content)
        (add-to-list 'emulation-mode-map-alists 'hfyview-mode-emulation-maps)
        (dolist (map hfyview-mode-other-maps)
          (define-key (symbol-value map) [(apps)] 'hfyview-frame)
          ;;(define-key (symbol-value map) [(apps)] 'hfy-show-grabbed)
          )
        )
    (remove-hook 'pre-command-hook 'hfy-grab-minibuffer-content)
    (remove-hook 'post-command-hook 'hfy-grab-echo-content)
    (setq emulation-mode-map-alists (delq 'hfyview-mode-emulation-maps emulation-mode-map-alists))
    (dolist (map hfyview-mode-other-maps)
      (define-key (symbol-value map) [(apps)] nil))))

(defun hfyview-fontify-region (start end)
  ;; If the last command in mumamo resulted in a change of major-mode
  ;; the big bug watcher in mumamo will get us if we do not tell that
  ;; we know what we are doing:
  (let ((mumamo-just-changed-major nil))
    (if start
        (save-restriction
          (widen)
          (narrow-to-region start end)
          (assert (= end (point-max)))
          (assert (= start (point-min)))
          (htmlfontify-buffer))
      (htmlfontify-buffer))))

(defun hfyview-buffer-1(start end show-source)
  (let ((hbuf (hfyview-fontify-region start end)))
    (with-current-buffer hbuf
      (setq buffer-file-name nil)
      (browse-url-of-buffer))
    (when show-source (switch-to-buffer-other-window hbuf))
    hbuf))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; Menus

(defvar hfyview-print-menu (make-sparse-keymap "QP"))
(defvar hfyview-print-region-menu (make-sparse-keymap "QPR"))
(defvar hfyview-print-window-menu (make-sparse-keymap "QPW"))
(defun hfyview-add-to-files-menu ()
  "Add \"Quick Print\" entry to file menu."
  ;; Why did I redo this???
  (setq hfyview-print-menu (make-sparse-keymap "QP"))
  (setq hfyview-print-region-menu (make-sparse-keymap "QPR"))
  (setq hfyview-print-window-menu (make-sparse-keymap "QPW"))
  ;; Main
  (define-key-after menu-bar-file-menu [hfyview-print]
    (list 'menu-item
          "Quick Print (Using Web Browser)"
          hfyview-print-menu
          :visible 'hfyview-print-visible)
    'separator-print)
  ;; Main submenu
  (define-key hfyview-print-menu [hfyview-browser-frame-pre]
    '(menu-item "Print Preview Frame" hfyview-frame
                :help "Print preview frame with web browser"))
  (define-key hfyview-print-menu [hfyview-browser-window-pre]
    '(menu-item "Print Preview Window" hfyview-window
                :help "Print preview window with web browser"))
  (define-key hfyview-print-menu [hfyview-browser-region-pre]
    (list 'menu-item "Print Preview Region" 'hfyview-region
          :help "Print preview region with web browser"
          :enable 'mark-active))
  (define-key hfyview-print-menu [hfyview-separator-pre]
    '(menu-item "--"))
  (define-key hfyview-print-menu [hfyview-browser-pre]
    '(menu-item "Print Preview Buffer" hfyview-buffer
                :help "Print preview buffer with web browser"
                :visible t))
  )

(defcustom hfyview-quick-print-in-files-menu nil
  "Add Quick print entries to File menu if non-nil.
If you set this to nil you have to restart Emacs to get rid of
the Quick Print entry."
  :type 'boolean
  :set (lambda (sym val)
         (set-default sym val)
         (if val
             (hfyview-add-to-files-menu)))
  :group 'hfy-view)

(defvar hfyview-print-visible t
  "Non-nil means show Quick Print entry on the file menu.")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; Interactive commands

;;;###autoload
(defun hfyview-buffer (arg)
  "Convert buffer to html preserving faces and show in web browser.
With command prefix also show created HTML source in other window."
  (interactive "P")
  (hfyview-buffer-1 nil nil arg))

;;;###autoload
(defun hfyview-region (arg)
  "Convert region to html preserving faces and show in web browser.
With command prefix also show created HTML source in other window."
  (interactive "P")
  (hfyview-buffer-1 (region-beginning) (region-end) arg))

;;;###autoload
(defun hfyview-window (arg)
  "Convert window to html preserving faces and show in web browser.
With command prefix also show created HTML source in other window."
  (interactive "P")
  (hfyview-buffer-1 (window-start) (window-end) arg))

;;;###autoload
(defun hfyview-frame (whole-buffers)
  "Convert frame to html preserving faces and show in web browser.
Make an XHTML view of the current Emacs frame. Put it in a buffer
named *hfyview-frame* and show that buffer in a web browser.

If WHOLE-BUFFERS is non-nil then the whole content of the buffers
is shown in the XHTML page, otherwise just the part that is
visible currently on the frame.

With command prefix also show created HTML source in other window."
  (interactive (list (y-or-n-p "Enter y for whole buffers, n for only visible part: ")))
  (let ((title "Emacs - Frame Dump")
        buf)
    (setq title (frame-parameter (selected-frame) 'name))
    (setq buf (hfyview-frame-1 whole-buffers title))
    (when current-prefix-arg
      (switch-to-buffer-other-window buf))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; Internal commands

(defconst hfyview-modline-format
  ;; There seems to be a bug in Firefox that prevents this from
  ;; displaying correctly.  Anyway this is just a quick and reasonable
  ;; approximation.
  (concat "<div style=\"width:%sem; color:%s; background:%s; white-space:pre; overflow:hidden; font-family:monospace;\">"
          ;; Using <pre> gives empty line above and below
          ;;"<pre>"
          "-- (Unix)%s   <b>%s</b>    (%s%s) "
          (make-string 6 ?-)
          "%s" ;; Viper
          (make-string 200 ?-)
          ;;"</pre>"
          "</div>"))

(defun hfyview-get-minors ()
  (let ((minors ""))
    (dolist (mr minor-mode-alist)
      (let ((mm (car mr))
            (ml (cadr mr)))
        (when (symbol-value mm)
          (when (stringp ml)
            (setq minors (concat minors ml))))))
    minors))

;; (hfyview-dekludge-string "<i> ")
(defun hfyview-dekludge-string (str)
  (mapconcat (lambda (c)
               (hfy-html-quote
                (char-to-string c)))
             (append str)
             ""))

(defun hfyview-fontify-win-to (win tag whole-buffer)
  (let* ((bstart (unless whole-buffer (window-start win)))
         (bend   (unless whole-buffer (window-end win)))
         (hbuf (hfyview-fontify-region bstart bend))
         (edges (window-edges win))
         (width  (- (nth 2 edges) (nth 0 edges)))
         (height (- (nth 3 edges) (nth 1 edges)))
         (border-color (or (hfy-triplet "SystemActiveBorder")
                          "gray"))
         start
         end
         css-start
         css-end
         mod-fgcolor
         mod-bgcolor
         mod-width
         mod
         bu-name
         ma-name
         minors
         (window-start-line (point-min))
         (window-end-line   (point-max))
         (is-selected-window (eq win hfyview-selected-window))
         (mark-viper "")
         )
    ;; Fix-me: fetch style too
    (with-current-buffer (window-buffer win)
      (unless whole-buffer
        (save-restriction
          (widen)
          (setq window-start-line (line-number-at-pos bstart))
          (setq window-end-line   (line-number-at-pos bend))
          (unless (or (< (line-number-at-pos (point-min)) window-start-line)
                      (> (line-number-at-pos (point-max)) window-end-line))
            (setq whole-buffer t))
          )
        )
      (setq mod-fgcolor (face-attribute (if is-selected-window 'mode-line 'mode-line-inactive) :foreground))
      (setq mod-bgcolor (face-attribute (if is-selected-window 'mode-line 'mode-line-inactive) :background))
      (setq mod-fgcolor (hfy-triplet mod-fgcolor))
      (setq mod-bgcolor (hfy-triplet mod-bgcolor))
      (setq mod (if (buffer-modified-p) "**" "--"))
      (when buffer-read-only
        (setq mod "%%"))
      (setq bu-name (buffer-name))
      (setq ma-name mode-name)
      (setq minors (hfyview-get-minors))
      (when (and (local-variable-p 'viper-mode-string) viper-mode-string)
        (setq mark-viper viper-mode-string))
      )
    ;; Compensate for scroll-bars
    (setq mod-width (+ width 1))
    (with-current-buffer hbuf
      (setq width (- width 2.5))
      (setq width (* 0.57 width))
      (setq height (+ height 2)) ;; For pre
      ;;(setq height (+ height 1.2)) ;; For horisontal scrollbar
      (setq height (* 1.16 height))
      (goto-char (point-min))
      (re-search-forward "<body.*?>")
      (setq start (point))
      (insert
       (format "<%s style=\"width:%sem; height:%sem; border: 1px solid %s; overflow:%s; padding:4px;\">\n"
               tag width height border-color
               (if whole-buffer "auto" "hidden") ;; overflow
               ))
      (goto-char (point-max))
      (setq end (search-backward "</body>"))
      (unless whole-buffer
        (insert
         (format "\n<div style=\"margin-top:2em; color: red; text-align: center; \"> Truncated to line %s - %s! </div>\n"
                 window-start-line window-end-line)))
      (insert "</" tag ">\n")
      ;;(lwarn t :warning "%s" mark-viper)
      (insert (format hfyview-modline-format
                      width
                      mod-fgcolor mod-bgcolor mod
                      (hfyview-dekludge-string bu-name)
                      (hfyview-dekludge-string ma-name)
                      (hfyview-dekludge-string minors)
                      (hfyview-dekludge-string mark-viper)))
      (setq end (point))
      (goto-char (point-min))
      (search-forward "<style type=\"text/css\"><!--")
      (beginning-of-line)
      (setq css-start (point))
      (search-forward "--></style>")
      (setq css-end (point))
      (set-buffer-modified-p nil)
      (setq buffer-file-name nil))
    (list hbuf start end css-start css-end)))

;; (defun hfyview-window-framed ()
;;   "Just a test"
;;   (interactive)
;;   (let* ((res (hfyview-fontify-win-to (selected-window) "div" nil))
;;          (hbuf (nth 0 res)))
;;     (with-current-buffer hbuf
;;       (browse-url-of-buffer))))

(defun hfyview-ffy-tree-win (win whole-buffer)
  (with-selected-window win
    (let* ((start (window-start))
           (end (window-end))
           (res (hfyview-fontify-win-to win "div" whole-buffer))
           (hbuf (nth 0 res)))
      (with-current-buffer hbuf
        (rename-buffer (generate-new-buffer-name (format "%s %s-%s" win start end))))
      ;;(lwarn t :warning "win=%s, hbuf=%s" win hbuf)
      res)))

(defun hfyview-ffy-tree (wt whole-buffers)
  (if (not (listp wt))
      (hfyview-ffy-tree-win wt whole-buffers)
    (let ((ret))
      (dolist (w (cddr wt))
        (setq ret (cons (hfyview-ffy-tree w whole-buffers) ret)))
      (list (car wt) ret))))

(defun hfyview-frame-to-html (res whole-buffers)
  (let ((html "")
        (css "")
        (first (car res))
        (td "<td style=\"vertical-align:top;\">")
        h)
    (cond
     ((memq first '(nil t))
      (dolist (sub (reverse (cadr res)))
        (let* ((res (hfyview-frame-to-html sub whole-buffers))
               (h   (nth 0 res))
               (c   (nth 1 res)))
          (when first (setq h (concat "<tr>\n" h "</tr>\n")))
          (setq html (concat html h))
          (setq css  (concat css c))))
      (unless first
        (setq html (concat "<tr>" html "</tr>\n")))
      (setq html (concat "<table border=\"0\" cellpadding=\"0\" cellspacing=\"0\">\n" html "</table>\n"))
      (setq html (concat td html "</td>\n"))
      )
     ((bufferp first)
      ;; (buf start end)
      (let* ((buf (nth 0 res))
             (sta (nth 1 res))
             (end (nth 2 res))
             (cst (nth 3 res))
             (cnd (nth 4 res))
             (h
              ;;(concat "<td>" "temp" "</td>\n")
              (with-current-buffer buf (buffer-substring-no-properties sta end)))
             (c
              ;;(concat "<td>" "temp" "</td>\n")
              (with-current-buffer buf (buffer-substring-no-properties cst cnd))))
        (setq h (concat td h
                        "</td>\n"))
        (setq html (concat html h))
        (setq css c)
        (kill-buffer buf)))
     (t
      (error "Uh?")))
    (list html css)))

(defconst hfyview-xhtml-header
  "<?xml version=\"1.0\" encoding=\"utf-8\"?>
<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.1//EN\"
\"http://www.w3.org/TR/xhtml11/DTD/xhtml11.dtd\">
<html xmlns=\"http://www.w3.org/1999/xhtml\">
  <head>
    <title>%s</title>
<style type=\"text/css\"><!--
body { font-family: outline-courier new;  font-stretch: normal;  font-weight: 500;  font-style: normal;  color: rgb(0, 0, 0);  font-size: 10pt;  text-decoration: none; }
 --></style>
%s
  </head>
  <body>\n")

(defvar hfyview-xhtml-footer "</body>\n</html>\n")

(defun hfyview-wm-border-color ()
  (or (hfy-triplet "SystemActiveTitle")
      (hfy-triplet "blue")))

(defvar hfy-grabbed-echo-content nil)
(defvar hfy-grabbed-minibuffer-content nil)
(defvar hfyview-prompt-face nil)

(defun hfyview-frame-minibuff (use-grabbed)
  (if (and use-grabbed
           (or hfy-grabbed-echo-content
               hfy-grabbed-minibuffer-content))
      (let* ((str (if hfy-grabbed-echo-content
                      hfy-grabbed-echo-content
                    hfy-grabbed-minibuffer-content))
             (tmpbuf (get-buffer-create "*hfy-minibuff-temp*"))
             (hbuf (with-current-buffer tmpbuf
                     (let ((inhibit-read-only t))
                       (erase-buffer)
                       (insert (propertize str 'read-only nil))
                       (htmlfontify-buffer))))
             bdy-start
             bdy-end
             bdy-txt
             css-start
             css-end
             css-txt)
        (with-current-buffer hbuf
          (goto-char (point-min))
          (search-forward "<style type=\"text/css\"><!--")
          (beginning-of-line)
          (setq css-start (point))
          (search-forward "--></style>")
          (setq css-end (point))
          (goto-char (point-min))
          (search-forward "<pre>")
          (setq bdy-start (point))
          (goto-char (point-max))
          (search-backward "</pre>")
          (setq bdy-end (point))
          (list (buffer-substring css-start css-end)
                (buffer-substring bdy-start bdy-end))))
    (let ((mini-bg (face-attribute hfyview-prompt-face :background))
          (mini-fg (face-attribute hfyview-prompt-face :foreground)))
      (if (eq mini-fg 'unspecified)
          (setq mini-fg "")
        (setq mini-fg (concat "color:" (hfy-triplet mini-fg) "; ")))
      (if (eq mini-bg 'unspecified)
          (setq mini-bg "")
        (setq mini-bg (concat "background:" (hfy-triplet mini-bg) "; ")))
      (list nil
            (concat
             "<span style=\"" mini-fg mini-bg "\">"
             "&nbsp;M-x "
             "</span>"
             "&nbsp;"
             "hfyview-frame"
             )))))

(defun hfyview-frame-1(whole-buffers frame-title)
  (let* ((wt (window-tree))
         (hfyview-selected-window (selected-window))
         (res (hfyview-ffy-tree (car wt) whole-buffers))
         (title-bg-color (hfyview-wm-border-color))
         (title-color (or (hfy-triplet "SystemHilightText")
                               "white"))
         (title-style (concat (format "background-color:%s; color:%s;" title-bg-color title-color)
                              "border: none; padding:4px; vertical-align: middle;"))
         (outbuf (get-buffer-create "frame"))
         html
         css
         ;; (face-attribute 'minibuffer-prompt :foreground)
         (hfyview-prompt-face (plist-get minibuffer-prompt-properties 'face))
         minibuf
         (frame-width (* 0.56 (frame-width)))
         table-style
         (icon-file (expand-file-name "../etc/images/icons/emacs_16.png" exec-directory))
         (img-tag (if (file-exists-p icon-file)
                      (concat "<img src=\"file://" icon-file "\" height=\"16\" width=\"16\" />")))
	 mini-css
	 mini-html
         )
    (setq table-style
          (format "border: solid %s; width:%sem;"
                  (hfyview-wm-border-color)
                  frame-width
                  ))
    (setq minibuf (hfyview-frame-minibuff hfyview-mode))
    (setq mini-css  (nth 0 minibuf))
    (setq mini-html (nth 1 minibuf))
    (when (string= mini-html "") (setq mini-html "&nbsp;"))
    (setq res (hfyview-frame-to-html res whole-buffers))
    (setq html (nth 0 res))
    (setq css  (nth 1 res))
    (with-current-buffer outbuf
      ;;(lwarn t :warning "outbuf=%s" outbuf)
      (erase-buffer)
      (insert (format hfyview-xhtml-header
                      (concat "Emacs frame dump - " frame-title)
                      css)
              (if mini-css mini-css "")
              (format "<table border=\"0\" cellpadding=\"0\" cellspacing=\"0\" style=\"%s\">\n" table-style)
              "<tr>\n"
              (format "<td style=\"%s\">%s&nbsp;&nbsp;%s</td>\n" title-style img-tag
                      (hfyview-dekludge-string frame-title))
              "</tr>\n"
              "<tr>\n"
              html
              "</tr>\n"
              "<tr>\n"
              "<td style=\"padding:1px;\">\n"
              mini-html
              "</td>\n"
              "</tr>\n"
              "</table>\n"
              hfyview-xhtml-footer)
      (browse-url-of-buffer)
      outbuf)))

;; (global-set-key [f7] '(lambda () (interactive) (message "grabbed=%s" hfy-grabbed-minibuffer-content)))
;; (global-set-key [f7] '(lambda () (interactive) (message "grabbed cm=%s" hfy-grabbed-echo-content)))
;; (global-set-key [f7] '(lambda () (interactive) (message "grabbed cm=%s, mb=%s" hfy-grabbed-echo-content hfy-grabbed-minibuffer-content)))
;; (defun hfy-show-grabbed ()
;;   (interactive)
;;   (message "grabbed cm=%s, mb=%s" hfy-grabbed-echo-content hfy-grabbed-minibuffer-content))

(defun hfy-grab-echo-content ()
  (setq hfy-grabbed-echo-content (current-message)))

(defun hfy-grab-minibuffer-content ()
  ;;(interactive)
  (let* ((mw (minibuffer-window))
         (mb (window-buffer mw)))
    (setq hfy-grabbed-minibuffer-content
          (with-current-buffer mb
              (buffer-substring
               (point-min) (point-max)))
            )))

;;(add-hook 'pre-command-hook 'grab-minibuffer-content nil t)
;;(remove-hook 'pre-command-hook 'grab-minibuffer-content) t)

(provide 'hfyview)
;;; hfyview.el ends here
