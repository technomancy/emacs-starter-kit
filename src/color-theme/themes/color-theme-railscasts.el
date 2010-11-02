;; Railscasts color theme for Emacs.
;;
;; To use add the following to your .emacs file:
;;
;; (require 'color-theme)
;; (color-theme-initialize)
;; (load-file "~/.emacs.d/site-lisp/themes/color-theme-railscasts.el")
;; (color-theme-railscasts)
;;
;; MIT License Copyright (c) 2009 Oleg Shaldybin <oleg.shaldybin@gmail.com>
;; Inspired by the brilliant Railscasts theme for TextMate
;;

(defun color-theme-railscasts ()
  (interactive)
  (color-theme-install
   '(color-theme-railscasts
     ((background-color . "#232323")
      (background-mode . dark)
      (cursor-color . "#5A647E")
      (foreground-color . "#E6E1DC"))
     (default ((t (nil))))
     (bold ((t (:bold t))))
     (bold-italic ((t (:italic t :bold t))))
     (fringe ((t (:background "#232323"))))
     (font-lock-builtin-face ((t (:foreground "#D0D0FF"))))
     (font-lock-comment-face ((t (:foreground "#BC9458" :italic t))))
     (font-lock-constant-face ((t (:foreground "#6D9CBE"))))
     (font-lock-doc-string-face ((t (:foreground "#A5C261"))))
     (font-lock-function-name-face ((t (:foreground "#FFC66D"))))
     (font-lock-keyword-face ((t (:foreground "#CC7833"))))
     (font-lock-preprocessor-face ((t (:foreground "#CC7833"))))
     (font-lock-reference-face ((t (:foreground "LightSteelBlue"))))
     (font-lock-string-face ((t (:foreground "#A5C261"))))
     (font-lock-type-face ((t (:foreground "white"))))
     (font-lock-variable-name-face ((t (:foreground "LightSteelBlue"))))
     (font-lock-warning-face ((t (:foreground "Pink"))))
     (paren-face-match-light ((t (:foreground "#FFC66D" :background "#555577"))))
     (highlight ((t (:background "darkolivegreen"))))
     (italic ((t (:italic t))))
     (modeline ((t (:background "#A5BAF1" :foreground "black"))))
     (modeline-buffer-id ((t (:background "#A5BAF1" :foreground 
                                          "black"))))
     (modeline-mousable ((t (:background "#A5BAF1" :foreground 
                                         "black"))))
     (modeline-mousable-minor-mode ((t (:background
                                        "#A5BAF1" :foreground "black"))))
     (region ((t (:background "#555577"))))
     (primary-selection ((t (:background "#555577"))))
     (isearch ((t (:background "#555555"))))
     (zmacs-region ((t (:background "#555577")))) 
     (secondary-selection ((t (:background "darkslateblue")))) 
     (flymake-errline ((t (:background "LightSalmon" :foreground 
                                       "black")))) 
     (flymake-warnline ((t (:background "LightSteelBlue" :foreground 
                                        "black"))))
     (underline ((t (:underline t)))) 
     (minibuffer-prompt ((t (:bold t :foreground "#FF6600"))))
     ;; two org-mode faces
     (org-document-info-keyword ((t (:foreground "#BC9458" :bold t))))
     (org-document-title ((t (:foreground "#BC9458" :bold t))))
     ;; better eshell prompt-face
     (eshell-prompt ((t (:foreground "turquoise1"))))
     )))
