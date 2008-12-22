;;; nxhtml-anything-1.el ---
;;
;; Author: Lennart Borgman (lennart O borgman A gmail O com)
;; Created: 2008-08-07T14:34:20+0200 Thu
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

;; Found this on the web. Maybe this could be an alternative completion function?
(require 'anything)
(defmacro nxhtml-with-anything-sources (sources &rest body)
  (let ((saved-anything-souces (gensym)))
    `(let ((,saved-anything-souces anything-sources))
       (unwind-protect
           (let ((anything-sources ,sources))
             ,@body)
         (setq anything-sources ,saved-anything-souces)))))

(defadvice popcmp-completing-read (around popcml-with-anything)
  (let* ((table (ad-get-arg 1))
         (alt-sets (apply 'append (mapcar 'cdr (ad-get-arg 9))));(apply 'append (mapcar 'cdr nxhtml-tag-sets)))
         (cands (cond ((not (listp table)) alt-sets)
                      (t table)))
         (source
          `((name . "nxhtml-completion")
            (candidates . ,cands)
            (action . (("select" . (lambda (candidate)
                                     (setq ad-return-value candidate))))))))
    (nxhtml-with-anything-sources (list source)
      (anything))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; nxhtml-anything-1.el ends here
