;;; ocr-user.el --- Input looong OCR number more safely
;;
;; Author: Lennart Borgman (lennart O borgman A gmail O com)
;; Created: 2008-06-18T23:00:25+0200 Wed
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
;; I just get mad at entering OCR numbers more than twenty digits long
;; so I wrote this litte minor mode that colors up the digits three by
;; tree.
;;
;; To use it do
;;
;;   M-x ocr-user-mode
;;
;; Crazy? Yeah, I get crazy by entering these digits. You would not
;; like to meet me when I have done that!
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

(defconst ocr-keywords
  `((
     ,(concat
      ;;"\\<\\(?:"
      "\\(?1:[0-9]\\{3\\}\\)"
      "\\(?2:[0-9]\\{3\\}\\)?"
      ;;"\\)+"
      )
     (0 (progn
          (put-text-property (match-beginning 1) (match-end 1)
                             'face '(:background "LightBlue1"))
          (when (match-beginning 2)
            (put-text-property (match-beginning 2) (match-end 2)
                               'face '(:background "PaleGreen1"))))))))

;; 23456
;; 1234567890
;; 346789238
(define-minor-mode ocr-user-mode
  "Color up digits three by three."
  :group 'convenience
  (if ocr-user-mode
      (font-lock-add-keywords nil ocr-keywords)
    (font-lock-remove-keywords nil ocr-keywords))
  (font-lock-fontify-buffer))


(provide 'ocr-user)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ocr-user.el ends here
