;;; fupd.el --- Helper functions for updating files

;; Author: Lennart Borgman <lennartDOTborgmanDOT073ATstudentDOTluDOTse>
;; Created: Tue Feb 28 17:21:20 2006
;; Version: 0.1
;; Last-Updated: Tue Feb 20 21:09:20 2007 (3600 +0100)
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
;; Helper functions for updating files.
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

(defun fupd-has-contents (file content)
  "Check if file FILE contains CONTENT.
Return a vector with these elements:
- elt 0: t if file contains CONTENT and buffer is not modified.
- elt 1: t if file contains CONTENT.
- elt 2: file buffer if file exists.
- elt 3: nil unless file already was in a buffer."
  (let (ok same buffer old-buffer)
    (when (file-exists-p file)
      (setq buffer (get-file-buffer file))
      (setq old-buffer (when buffer t))
      (unless buffer
        (setq buffer (find-file-noselect file)))
      (with-current-buffer buffer
        (setq same (string=
                    content
                    (buffer-substring-no-properties
                     (point-min) (point-max)))))
      (setq ok (and same
                    (not (buffer-modified-p buffer)))))
    (vector ok same buffer old-buffer)))

(defun fupd-ok (ret-val)
  "Return t if RET-VAL indicate file is uptodate.
RET-VAL should be the return value from `fupd-has-contents'."
  (elt ret-val 0))

(defun fupd-kill-new-buffer (ret-val)
  "Kill new buffer indicated by RET-VAL.
RET-VAL should be the return value from `fupd-has-contents'."
  (unless (elt ret-val 3)
    (let ((buffer (elt ret-val 2)))
      (when (bufferp buffer)
        ;;(message "fupd-kill-new-buffer: %s" (buffer-file-name buffer))(sit-for 4)
        (kill-buffer buffer)))))

;;(fupd-has-contents buffer-file-name (buffer-string))
;;(fupd-update-file buffer-file-name (buffer-string))
(defun fupd-update-file (file content)
  "Update file FILE with content CONTENT.
Do nothing if the file already has that content.  If the file was
not in a buffer before kill the file's buffer afterwards.

Return t if the file was updated, otherwise nil."
  (let* ((osbo (fupd-has-contents file content))
         (ok   (elt osbo 0))
         (same (elt osbo 1))
         (buff (elt osbo 2))
         (oldb (elt osbo 3))
         wrote
         )
    (unless ok
      (if buff
          (with-current-buffer buff
            (unless same
              (erase-buffer)
              (insert content))
            (save-buffer)
            (setq wrote t)
            (unless oldb
              (kill-buffer (current-buffer))))
        (with-temp-buffer
          (insert content)
          (write-file file))))
    wrote))

;; (defun fupd-copy-file (from-file to-file)
;;   (let (
;;         (from-buff (find-buffer-visiting from-file))
;;         (to-buff (find-buffer-visiting to-file))
;;         (from-attr (file-attributes from-file))
;;         (to-attr (file-attributes to-file))
;;         (from-size (nth 7 from-attr))
;;         (to-size (nth 7 to-attr))
;;         (from-mod (nth 5 from-attr))
;;         (to-mode (nth 5 to-attr))
;;         )
;;   ))

(provide 'fupd)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; fupd.el ends here
