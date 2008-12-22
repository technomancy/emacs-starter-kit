;;; nxhtml-js.el --- Javascript support functions
;;
;; Author: Lennart Borgman (lennart O borgman A gmail O com)
;; Created: Sat Apr 28 01:51:17 2007
;; Version: 0.0
;; Lxast-Updated:
;; URL:
;; Keywords:
;; Compatibility:
;;
;; Fxeatures that might be required by this library:
;;
;;   None
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;
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

(defun nxhtml-add-link (type src silent)
  ;;<script type="text/javascript" src="EmacsW32.js"></script>
  (catch 'exit
    (save-excursion
      (save-restriction
        (widen)
        (let ((here (point))
              (link (cond
                     ((eq type 'js)
                      (concat "<script type=\"text/javascript\" src=\"" src "\"></script>\n"))
                     ((eq type 'css)
                      (concat "<link rel=\"stylesheet\" href=\"" src "\" type=\"text/css\" media=\"screen\"/>\n"))
                     (t
                      (error "Bad type=%s" type))
                     )))
          (goto-char (point-min))
          (when (search-forward link nil t)
            (unless silent
              (let ((temp-ovl (make-overlay (match-beginning 0)
                                            (match-end 0)))
                    (after-string " <-- It is already here "))
                (condition-case err
                    (progn
                      (put-text-property 0 (length after-string)
                                         'face '(:background "red")
                                         after-string)
                      (overlay-put temp-ovl 'face '(:background "yellow"))
                      (overlay-put temp-ovl 'priority 100)
                      (overlay-put temp-ovl 'after-string after-string)
                      (redisplay t)
                      (sit-for 3))
                  (quit nil)
                  (error (message "%s" (error-message-string err))))
                (delete-overlay temp-ovl))
              (throw 'exit t)))
          (unless (search-forward "</head>" nil t)
            (goto-char here)
            (unless (y-or-n-p "Can't find </head>, insert link to script here? ")
              (throw 'exit nil)))
          (beginning-of-line)
          (insert link)
          (beginning-of-line 0)
          (indent-according-to-mode))))))

(defun nxhtml-smoothgallery-add-base (silent)
  "Add links to javascript and style sheets.
This command add links to the javascript and style sheets that
comes with SmoothGallery, see URL
`http://smoothgallery.jondesign.net/'.

* NOTICE: The files are not added to your project. Instead the
files that comes with nXhtml are linked to directly."
  (interactive (list nil))
  (unless (buffer-file-name)
    (error "Can't add SmoothGallery if buffer has no filename"))
  (unless (memq major-mode '(html-mode nxhtml-mode))
    (error "Wrong major mode"))
  (let* ((libfile (locate-library "nxhtml"))
         (jsdir-abs (expand-file-name "doc/js/smoothgallery/scripts/"
                                    (file-name-directory libfile)))
         (jsdir-rel (file-relative-name jsdir-abs (file-name-directory (buffer-file-name))))
         (cssdir-abs (expand-file-name "doc/js/smoothgallery/css/"
                                       (file-name-directory libfile)))
         (cssdir-rel (file-relative-name cssdir-abs (file-name-directory (buffer-file-name)))))
    (nxhtml-add-link 'js (concat jsdir-rel "mootools.js") silent)
    (nxhtml-add-link 'js (concat jsdir-rel "jd.gallery.js") silent)
    (nxhtml-add-link 'css (concat cssdir-rel "jd.gallery.css") silent)
    (nxhtml-add-link 'css (concat cssdir-rel "layout.css") silent)
    ))

(defconst nxhtml-smoothgallery-mark "<!-- SmoothGallery -->")
(defun nxhtml-smoothgallery-find ()
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (when  (search-forward nxhtml-smoothgallery-mark nil t)
        (back-to-indentation)
        (when (looking-at (rx
                           "<div id=\""
                           (submatch
                            (1+ (not (any ">")))
                            )
                           "\">" (eval nxhtml-smoothgallery-mark)
                           ))
          (cons
           (copy-marker (match-beginning 0))
           (buffer-substring-no-properties
           (match-beginning 1) (match-end 1))))))))

(defun nxhtml-smoothgallery-mk-jsmark (name)
  (concat "new gallery($('" name "'), {"))

(defun nxhtml-smoothgallery-find-script (name)
  (let ((jsmark (nxhtml-smoothgallery-mk-jsmark name)))
    (goto-char (point-min))
    (search-forward jsmark nil t)))

(defun nxhtml-smoothgallery-add (point-name)
  (interactive (list
                (let ((old-name (nxhtml-smoothgallery-find)))
                  (if old-name
                      old-name
                    "myGallery"))))
  (let ((name (if (consp point-name)
                  (cdr point-name)
                point-name))
        (where (when (consp point-name)
                 (car point-name))))
    (unless where
      (goto-char (point-min))
      (search-forward "<body")
      (search-forward ">")
      (insert "\n")
      (setq where (point-marker))
      (insert-and-indent "<div id=\"" name "\">" nxhtml-smoothgallery-mark
                         "\n</div>")
      )
    (unless (nxhtml-smoothgallery-find-script name)
      (goto-char where)
      (beginning-of-line)
      (insert-and-indent "<script type=\"text/javascript\">
          function startGallery() {
            var myGallery = new gallery($('" name "'), {
                timed: true,
                delay: 9000,
                embedLinks: false,
                showArrows: true,
                showCarousel: false,
                showInfopane: true,
            });
          }
          window.onDomReady(startGallery);
        </script>")
      (indent-according-to-mode))
    (goto-char where)))

(defun nxhtml-smoothgallery-add-img (imgsrc thumbsrc title description)
  (interactive (let ((gallery (nxhtml-smoothgallery-find)))
                 (when gallery
                   (goto-char (car gallery)))
                 (list
                  (nxhtml-read-url nil nil 'nxhtml-image-url-predicate "Image")
                  (when (y-or-n-p "Include thumbnail? " )
                    (nxhtml-read-url nil nil 'nxhtml-image-url-predicate "Thumbnail"))
                  (read-string "Title: ")
                  (read-string "Description: ")
                  )))
  (unless thumbsrc (setq thumbsrc imgsrc))
  (let ((gallery (nxhtml-smoothgallery-find)))
    (unless gallery
      (setq gallery (nxhtml-smoothgallery-add)))
    (goto-char (car gallery))
    (end-of-line)
    (insert-and-indent "
       <div class=\"imageElement\">
         <h3>" title "</h3>
         <p>" description "</p>
         <a href=\"#\" title=\"open image\" class=\"open\"></a>
         <img src=\"" imgsrc "\" class=\"full\" alt=\"" title "\" />
         <img src=\"" thumbsrc "\" class=\"thumbnail\" alt=\"" title " (thumbnail)\" />
       </div>")
;;     (when (file-exists-p src)
;;       (let ((sizes (image-size (create-image src) t)))
;;         (insert
;;          " width=\""  (format "%d" (car sizes)) "\""
;;          " height=\"" (format "%d" (cdr sizes)) "\"")
;;         ))
    ))

(defun insert-and-indent (&rest lines)
  (let ((lines (split-string (apply 'concat lines) "[\n\r]")))
    (dolist (line lines)
        (insert "\n" line)
        (indent-according-to-mode))))


(provide 'nxhtml-js)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; nxhtml-js.el ends here
