;;; zen-mode.el --- remove/restore Emacs frame distractions quickly

;;; Copyright (C) 2008 Joakim Verona

;;Author: Joakim Verona, [EMAIL PROTECTED]
;;License: GPL V3 or later

;;; Commentary:
;;
;;zen-mode is Emacs in fullscreen without toolbar and menubar
;;the function toggles between previous state of features and zen
;;
;;
;;I bind zen-mode to f11 like this:
;;;(global-set-key [f11] 'zen-modeb)
;;
;;TODO:
;;
;;- zen-master mode, like writeroom mode. (c-u m-x zen-mode), which
;;isnt customizable, it just turns all distractions off
;;
;;- there shouldnt just be a toogle, but a way to enter a specific
;;state, like scroll-bar-mode, probably with a numeric argument
;;
;;Note:
;; There are some problems in the Compiz WM, (and maybe other WM:s) regarding fullscreen:
;; When selecting another workspace temporarily and going back, Emacs
;; does not cover the wm panels as it should.  This can be resolved with alt-tab a bit,
;; but its annoying.  Its unclear if this is Emacs or Compiz fault.
;;
;;

;;; History:
;;
;; 2008.08.17 -- v0.1

;;; Code:

(provide 'zen-mode)

(defvar zen-mode-is-active-p nil "If zen mode is currently active or not.")
(defvar zen-mode-previous-state nil "The state of features to be disabled in
zen-mode, before entering zen-mode.")

(defgroup zen-mode nil "zen-mode"); :group 'some-apropriate-root-group)

(defcustom zen-mode-what-is-not-zen '()
  "These Emacs features are not considered Zen.
They will be disabled when entering Zen.  They will be restored
to their previous settings when leaving Zen."
  :group 'zen-mode
  :type
  '(set :tag "zen"
    (const scroll-bar-mode)
    (const menu-bar-mode)
    (const tool-bar-mode)
    (const windowed-mode) ;windowed is the inverse of fullscreen, for didactic reasons
  )
  )


(defun zen-mode-get-feature-state (feature)
  "An uniform get/set facility for each feature zen handles.
FEATURE is a symbol from 'zen-mode-what-is-not-zen'."
  (cond
   ((eq feature 'scroll-bar-mode)
    scroll-bar-mode)
   ((eq feature 'menu-bar-mode)
    menu-bar-mode)
   ((eq feature 'tool-bar-mode)
    tool-bar-mode)
   ((eq feature 'windowed-mode)
    (if (frame-parameter nil 'fullscreen) nil t))
 )
  )

(defun zen-mode-set-feature-state (feature state)
  "Set zen FEATURE to STATE."
  (let*
      ((modeflag (if state t -1)))
    (cond
     ((eq feature 'scroll-bar-mode)
      (scroll-bar-mode modeflag))
     ((eq feature 'menu-bar-mode)
      (menu-bar-mode modeflag))
     ((eq feature 'tool-bar-mode)
      (tool-bar-mode modeflag))
     ((eq feature 'windowed-mode)
      (set-frame-parameter nil 'fullscreen (if state nil 'fullboth)))
    ))
)


(defun zen-mode-store-state ()
  "Store the state of all features zen is interested in."
  (setq zen-mode-previous-state nil)
  (let
      ((f zen-mode-what-is-not-zen))
    (while f
      (setq zen-mode-previous-state
            (append zen-mode-previous-state
                    (list (list (car f)
                                (zen-mode-get-feature-state (car f))) )))
      (setq f (cdr f))))
  zen-mode-previous-state)

(defun zen-mode-disable-nonzen-features ()
  "Disable all non-zen features."
  (let
      ((f zen-mode-what-is-not-zen))
    (while f
      (zen-mode-set-feature-state (car f) nil)
      (setq f (cdr f)))))

(defun zen-mode-restore-state ()
  "Restore the feature state as before entering zen."
  (let
      ((f zen-mode-previous-state))
    (while f
      (zen-mode-set-feature-state (caar  f) (cadar f))
      (setq f (cdr f))))
  )


(defun zen-mode ()
  "Toggle Zen mode."
  (interactive)
    (if zen-mode-is-active-p
        (progn;deactivate zen
          (setq zen-mode-is-active-p nil)
          (zen-mode-store-state)
          (zen-mode-disable-nonzen-features)
          )
      (progn;activate zen
        (setq zen-mode-is-active-p t)
        (zen-mode-restore-state))))

(provide 'zen-mode)
