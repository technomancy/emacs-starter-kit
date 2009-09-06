;;; appmenu.el --- A framework for [apps] popup menus.

;; Copyright (C) 2008 by Lennart Borgman

;; Author:  Lennart Borgman <lennart DOT borgman AT gmail DOT com>
;; Created: Thu Jan 05 14:00:26 2006
(defconst appmenu:version "0.62") ;; Version:
;; Last-Updated: 2008-06-15T17:54:40+0200 Sun
;; Keywords:
;; Compatibility:
;;
;; Features that might be required by this library:
;;
;;   `cl'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;  appmenu.el is a framework for creating cooperative context
;;  sensitive popup menus with commands from different major and minor
;;  modes.  For more information see `appmenu-mode'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change log:
;;
;; Version 0.61:
;; - Remove support for minor and major menus.
;; - Add support for text and overlay keymaps.
;; - Add customization options.
;;
;; Version 0.62:
;; - Fix problem with keymap at point.
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

(eval-when-compile (require 'cl))

(defgroup appmenu nil
  "Customization group for `appmenu-mode'."
  :group 'convenience)

(defcustom appmenu-show-help nil
  "Non-nil means show AppMenu help on AppMenu popup."
  :type 'boolean
  :group 'appmenu)

(defcustom appmenu-show-point-menu t
  "If non-nil show entries fetched from keymaps at point."
  :type 'boolean
  :group 'appmenu)

(defvar appmenu-alist nil
  "List of additional menu keymaps.
To change this list use `appmenu-add' and `appmenu-remove'.

The entries in this list are lists:

   \(ID PRIORITY TEST TITLE DEFINITION)

ID is a unique identity.

PRIORITY is a number or a variable whose value is a number
telling where to put this entry when showing the menu.

TEST should be a form to evaluate.  The entry is used if \(eval
TEST) returns non-nil.

DEFINITION should be either a keymap or a function that returns a
keymap.

The function must take no argument and return a keymap.  If the
function returns nil then the entry is not shown in the popup
menu.  Using this you can make context sensitive popup menus.

For an example of use see mlinks.el.")

(defun appmenu-sort-by-priority ()
  "Sort `appmenu-alist' entries by priority."
  (setq appmenu-alist
        (sort appmenu-alist
              (lambda (recA recB)
                (let ((priA (nth 1 recA))
                      (priB (nth 1 recB)))
                  (when (symbolp priA) (setq priA (symbol-value priA)))
                  (when (symbolp priB) (setq priB (symbol-value priB)))
                  (< priA priB))))))

;;;###autoload
(defun appmenu-add (id priority test title definition)
  "Add entry to `appmenu-alist'.
Add an entry to this list with ID, PRIORITY, TEST, TITLE and
DEFINITION as explained there."
  (assert (symbolp id))
  (unless priority (setq priority 100))
  (assert (numberp priority))
  (assert (stringp title))
  (let ((rec (list id priority test title definition)))
    (appmenu-remove id)
    (add-to-list 'appmenu-alist rec)))

(defun appmenu-remove (id)
  "Remove entry with id ID from `appmenu-alist'."
  (setq appmenu-alist (assq-delete-all id appmenu-alist)))

(defun appmenu-help ()
  "Show help for minor mode function `appmenu-mode'."
  (interactive)
  (describe-function 'appmenu-mode))

(defun appmenu-keymap-len (map)
  "Return length of keymap MAP."
  (let ((ml 0))
    (map-keymap (lambda (e f) (setq ml (1+ ml))) map)
    ml))

(defvar appmenu-mouse-only
  '((flyspell-correct-word appmenu-flyspell-correct-word-before-point)))

(defun appmenu-flyspell-correct-word-before-point ()
  "Pop up a menu of possible corrections for misspelled word before point.
Special version for AppMenu."
  (interactive)
  (flyspell-correct-word-before-point))

(defcustom appmenu-at-any-point '(ispell-word)
  "Commands that may work at any point in a buffer.
Some important but not too often used commands that may be useful
for most points in a buffer."
  :group 'appmenu)

(defun appmenu-make-menu-for-point ()
  "Construct a menu based on point.
This includes some known commands for point and keymap at
point."
  (let ((point-map (get-char-property (point) 'keymap))
        (funs appmenu-at-any-point)
        (map (make-sparse-keymap "At point"))
        (num 0)
        last-prefix
        this-prefix)
    ;; Known for any point
    (when point-map
      (let ((map-fun (lambda (key fun)
                       (if (keymapp fun)
                           (map-keymap map-fun fun)
                         (when (and (symbolp fun)
                                    (fboundp fun))
                           (let ((mouse-only (assq fun appmenu-mouse-only)))
                             (when mouse-only
                               (setq fun (cadr mouse-only)))
                             (add-to-list 'funs fun)))))))
        (map-keymap map-fun point-map)))
    (dolist (fun funs)
      (let ((desc (when fun (documentation fun))))
        (when desc
          (setq desc (car (split-string desc "[\n]")))
          ;;(lwarn t :warning "pk: %s, %s" fun desc)
          (setq this-prefix
                (car (split-string (symbol-name fun) "[-]")))
          (when (and last-prefix
                     (not (string= last-prefix this-prefix)))
            (define-key map
              (vector (intern (format "appmenu-point-div-%s" num)))
              (list 'menu-item "--")))
          (setq last-prefix this-prefix)
          (setq num (1+ num))
          (define-key map
            (vector (intern (format "appmenu-point-%s" num)))
            (list 'menu-item desc fun)))))
    (when (> num 0) map)))

(defun appmenu-map ()
  "Return menu keymap to use for popup menu."
  (let* ((map (make-sparse-keymap
               "AppMenu"
               ))
         (map-len (appmenu-keymap-len map))
         (map-init-len map-len)
         (num-minor 0)
         (id 0)
         (point-menu (when appmenu-show-point-menu
                       (appmenu-make-menu-for-point))))
    ;; AppMenu itself
    (when appmenu-show-help
      (define-key map [appmenu-customize]
        (list 'menu-item "Customize AppMenu"
              (lambda () (interactive) (customize-group 'appmenu))
              :help "Customize AppMenu"
              :visible 'appmenu-show-help))
      (define-key map [appmenu-help]
        (list 'menu-item "Help for AppMenu" 'appmenu-help
              :help "Help for how to use AppMenu"
              :visible 'appmenu-show-help))
      (define-key map [appmenu-separator-1]
        (list 'menu-item "--")))
    (setq map-len (appmenu-keymap-len map))
    (appmenu-sort-by-priority)
    (dolist (rec appmenu-alist)
      (let* ((test   (nth 2 rec))
             (title  (nth 3 rec))
             (mapdef (nth 4 rec))
             (usedef (if (symbolp mapdef)
                         (funcall mapdef)
                       mapdef)))
        (when (and usedef
                   (eval test))
          (setq id (1+ id))
          (define-key map
            (vector (intern (format "appmenu-%s" id)))
            (list 'menu-item title usedef)))
        ))
    (when point-menu
      (setq map-len (appmenu-keymap-len map))
      (when (> map-len map-init-len)
        (define-key map [appmenu-at-point-div]
          (list 'menu-item "--")))
      (define-key map [appmenu-at-point]
        (list 'menu-item "At current point"
              point-menu)))
    (setq map-len (appmenu-keymap-len map))
    (when (> map-len map-init-len)
      map)))

;; (defun appmenu-get-submenu (menu-command)
;;   (let (subtitle submenumap)
;;     (if (eq 'menu-item (car menu-command))
;;         (progn (setq subtitle   (cadr  menu-command))
;;                (setq submenumap (caddr menu-command)))
;;       (setq subtitle   (car menu-command))
;;       (setq submenumap (cdr menu-command)))
;;     (unless (keymapp submenumap) (error "Submenu not a keymap=%s" submenumap))
;;     (cons subtitle submenumap)))

(defun appmenu-popup ()
  "Pops up the AppMenu menu."
  (interactive)
  (let* ((mod (event-modifiers last-input-event))
         (is-mouse (or (memq 'click mod)
                       (memq 'down  mod)
                       (memq 'drag  mod))))
    (when is-mouse
      (goto-char (posn-point (event-start last-input-event)))
      (sit-for 0.01))
    ;;active-minibuffer-window)
    ;;(condition-case err
        (let ((menu (appmenu-map)))
          (if menu
              (popup-menu-at-point menu)
            (message "Appmenu is empty")))
      ;;(quit nil))
    ))

(defvar appmenu-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [apps] 'appmenu-popup)
    (define-key map [mouse-3] 'appmenu-popup)
    map))

(define-minor-mode appmenu-mode
  "Use a context sensitive popup menu.
AppMenu (appmenu.el) is a framework for creating cooperative
context sensitive popup menus with commands from different major
and minor modes. Using this different modes may cooperate about
the use of popup menus.

By default the popup menu is on [apps] and [mouse-3].

The variable `appmenu-alist' is where the popup menu entries
comes from.

If there is a `keymap' property at point then relevant bindings
from this is also shown in the popup menu.

You can write functions that use whatever information you want in
Emacs to construct these entries. Since this information is only
collected when the popup menu is shown you do not have to care as
much about computation time as for entries in the menu bar."
  :global t
  :keymap appmenu-mode-map
  :group 'appmenu)
(when (and appmenu-mode
           (not (boundp 'define-globa-minor-mode-bug)))
  (appmenu-mode 1))

(provide 'appmenu)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; appmenu.el ends here
