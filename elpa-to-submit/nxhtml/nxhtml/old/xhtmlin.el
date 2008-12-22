;;; xhtmlin.el --- Insert XHTML code fragment for some tags
;; 
;; Author:  Lennart Borgman <lennart DOT borgman DOT 073 AT student DOT lu DOT se>
;; Created: Thu Dec 29 22:45:47 2005
(setq xhtmlin:version "0.31") ;; Version:
;; Last-Updated: Mon Feb 27 22:28:29 2006 (3600 +0100)
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
;;  Easy insertion of some XHTML tags.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Change log:
;; 
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
;; along with this program; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Code:

(defcustom xhtmlin-files-must-exist t
  "If non-nil then files pointed to must exist."
  :type 'boolean
  :group 'xhtmlin)

(defcustom xhtmlin-check-web-links t
  "If non-nil then web links are checked."
  :type 'boolean
  :group 'xhtmlin)

(defun xhtmlin-get-url(url)
  "Check URL and return it or a relative path to it.
If URL is a local file path and `xhtmlin-files-must-exist' is
non-nil then the file must exist.

If URL is a web link and `xhtmlin-check-web-links' is non-nil
then the user is asked if the file should be checked."
  (if (file-name-absolute-p url)
      (progn
        (when xhtmlin-files-must-exist
          (unless (file-exists-p url)
            (error "Can't find file %s" url)))
        (if (not (buffer-file-name))
            url
          (file-relative-name url
                              (file-name-directory (buffer-file-name)))))
    (if (not (string-match url-nonrelative-link url))
        url
      (if (and xhtmlin-check-web-links nil
               (y-or-n-p "Check web url? "))
          (if (url-http-file-exists-p url)
              url
            (error "Can't find %s" url))
        url))))
 
(defvar xhtmlin-insertable nil
  "Function to check if the tag can be inserted at `point'.")
(defun xhtmlin-insertable (tag)
  (if xhtmlin-insertable
      (unless (funcall xhtmlin-insertable tag)
        (error "Can't insert a %s tag here" tag))
    t))

(defun xhtmlin-insertable(tag)
  (let ((len 0))
    (when (fboundp 'nxhtml-check-insertable)
      (setq len (nxhtml-check-insertable tag)))
    len))

(defun xhtmlin-insert-urlthing(tag prompt creator)
  (let ((len (xhtmlin-insertable tag)))
    (when len
      (let ((url (expand-file-name
                  (read-file-name prompt))))
        (insert (substring (funcall creator url) len))))))

(defun xhtmlin-javascript(url &optional absolute)
;; <script type="text/javascript" src="EmacsW32.js"></script>
  (let ((src (if absolute
                 url
               (xhtmlin-get-url url))))
    (concat "<script"
            " type=\"text/javascript\""
            " src=\"" src "\""
            " />")))

(defun xhtmlin-insert-javascript()
  "Insert an xhtml header javascript href tag.
The link in the tag is checked with `xhtmlin-get-url'."
  (interactive)
  (xhtmlin-insert-urlthing "script" "JavaScript file: " 'xhtmlin-javascript))


(defun xhtmlin-css(url &optional nocheck)
;;    <link href="EmacsW32.css" rel="StyleSheet" />
  (let ((href (if nocheck
                  url
                (xhtmlin-get-url url))))
    (concat "<link"
            " rel=\"StyleSheet\""
            " href=\"" href "\""
            " />")))

(defun xhtmlin-insert-css()
  "Insert an xhtml header css link tag.
The link in the tag is checked with `xhtmlin-get-url'."
  (interactive)
  (xhtmlin-insert-urlthing "link" "CSS file: " 'xhtmlin-css))


(defun xhtmlin-img(url alt &optional title nocheck)
  (let* ((sizes (when (file-exists-p url) (image-size (create-image url) t)))
         (usetit (if title title alt))
         (src (if nocheck
                  url
                (xhtmlin-get-url url))))
    (when (= 0 (length usetit)) (setq usetit nil))
    (concat "<img src=\"" src "\""
            (when sizes
              (concat
               " width=\""  (format "%d" (car sizes)) "\""
               " height=\"" (format "%d" (cdr sizes)) "\""))
            (if alt    (concat " alt=\"" alt "\"")     "")
            (if usetit (concat " title=\"" usetit "\"") "")
            " />")))

(defun xhtmlin-insert-img()
  "Insert an xhtml img tag.
The link in the tag is checked with `xhtmlin-get-url'."
  (interactive)
  (let ((len (xhtmlin-insertable "img")))
    (when len
      (let ((url (expand-file-name
                  (read-file-name "Image file: ")))
            (alt   (read-string "Alt: "))
            (title (read-string "Title: ")))
        (insert (substring (xhtmlin-img url alt title) len))))))


(defun xhtmlin-a(url title &optional nocheck)
  (let ((href (if nocheck
                  url
                (xhtmlin-get-url url))))
    (concat "<a href=\"" href "\">" title "</a>")))

(defun xhtmlin-insert-a()
  "Insert an a tag.
The link in the tag is checked with `xhtmlin-get-url'."
  (interactive)
  (let ((len (xhtmlin-insertable "a")))
    (when len
      (let ((url (expand-file-name
                  (read-file-name "Link to file: ")))
            (title   (read-string "Title: ")))
        (insert (substring (xhtmlin-a url title) len))))))

(defun xhtmlin-insert-object-java()
  (interactive)
  (let ((len (xhtmlin-insertable "a")))
    (when len
      (let ((url (expand-file-name
                  (read-file-name "Class file: ")))
            (jar (when (y-or-n-p "Is there a jar file?")
                   (expand-file-name
                    (read-file-name "Jar file: "))))
            (h (read-string "Height: "))
            (w (read-string "Width: "))
            )
        (insert (substring (xhtmlin-object-java url jar h w) len))))))
  
(defun xhtmlin-object-java(class &optional jar height width nocheck init-params)
  ;; See http://ww2.cs.fsu.edu/~steele/XHTML/appletObject.html
  (let ((class-file (if nocheck
                        class
                      (xhtmlin-get-url class)))
        (jar-file (when jar
                    (if nocheck
                        jar
                      (xhtmlin-get-url jar))))
        )
  (concat
   "<!--[if !IE]> Firefox and others will use outer object -->\n"
   "<object classid=\"java:" class-file "\" \n"
   "        type=\"application/x-java-applet\"\n"
   (if jar-file
       (concat "        archive=\"" jar-file "\" \n")
     "")
   (if height
       "        height=\"300\" width=\"450\"\n"
     "")
   "        >\n"
   (if init-params init-params "")
   "  <!--<![endif]-->\n"
   "  <!-- MSIE (Microsoft Internet Explorer) will use inner object --> \n"
   "  <object classid=\"clsid:8AD9C840-044E-11D1-B3E9-00805F499D93\" \n"
   "          codebase=\"http://java.sun.com/update/1.5.0/jinstall-1_5_0-windows-i586.cab\"\n"
   (if height
       "          height=\"300\" width=\"450\"\n"
     "")
   "          >\n"
   "    <param name=\"code\" value=\"" (file-name-sans-extension class-file) "\" />\n"
   (if jar-file
       (concat "    <param name=\"archive\" value=\"" jar-file "\" />\n")
     "")
   (if init-params init-params "")
   "    <strong>\n"
   "      This browser does not have a Java Plug-in.\n"
   "      <br />\n"
   "      <a href=\"http://java.sun.com/products/plugin/downloads/index.html\">\n"
   "        Get the latest Java Plug-in here.\n"
   "      </a>\n"
   "    </strong>\n"
   "  </object> \n"
   "  <!--[if !IE]> close outer object -->\n"
   "</object>\n"
   "<!--<![endif]-->\n")))

(defconst xhtmlin-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [menu-bar xhtmlin] (cons "Insert Link" (make-sparse-keymap)))
    (define-key map [menu-bar xhtmlin insert-java-obj] '("Insert Java Object" . xhtmlin-insert-object-java))
    (define-key map [menu-bar xhtmlin insert-css] '("Insert CSS Link" . xhtmlin-insert-css))
    (define-key map [menu-bar xhtmlin insert-javascript] '("Insert Javascript or Link" . xhtmlin-insert-javascript))
    (define-key map [menu-bar xhtmlin insert-img] '("Insert Img Tag" . xhtmlin-insert-img))
    (define-key map [menu-bar xhtmlin insert-a] '("Insert A Link Tag" . xhtmlin-insert-a))
    map))


(provide 'xhtmlin)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; xhtmlin.el ends here
