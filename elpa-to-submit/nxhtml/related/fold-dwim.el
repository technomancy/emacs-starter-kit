;;; fold-dwim.el -- Unified user interface for Emacs folding modes
;;
;; Copyright (C) 2004 P J Heslin
;;
;; Author: Peter Heslin <p.j.heslin@dur.ac.uk>
;; URL: http://www.dur.ac.uk/p.j.heslin/Software/Emacs/Download/fold-dwim.el
(defconst fold-dwim:version "1.3")
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
;; If you do not have a copy of the GNU General Public License, you
;; can obtain one by writing to the Free Software Foundation, Inc., 59
;; Temple Place - Suite 330, Boston, MA 02111-1307, USA.

;;; Overview:
;;
;; DWIM stands for "do what I mean", as in the idea that one keystroke
;; can do different things depending on the context. In this package,
;; it means that, if the cursor is in a currently hidden folded
;; construction, we want to show it; if it's not, we want to hide
;; whatever fold the cursor is in.
;;
;; Some editors other than Emacs provide a single mechanism for
;; folding text which various file types can exploit.  The advantage
;; of this arrangement is that the user only has to know one set of
;; folding commands; the disadvantage is that the various file types
;; are limited to using whatever functionality is provided centrally.
;; Emacs by contrast provides a very general and powerful framework
;; for hiding text, which major modes can use as they see fit.  The
;; advantage of this is that each major mode can deal with folding in
;; the way that is suitable for that type of file; the disadvantage is
;; that different major modes have different styles of folding, and
;; provide different key bindings.
;;
;; In practice, matters are simpler than that, since most major modes
;; delegate the task of folding to packages like outline.el and
;; hideshow.el.  The key bindings for these two packages alone,
;; however, are numerous and for some people hard to type.  Another
;; usability complication arises when a package like AucTeX uses
;; outline-minor-mode for some folds, and provides its own
;; key-bindings for other kinds of folds.  Likewise, nXML-mode
;; provides its own style of folding for certain types of files, but
;; for files that don't fit that paradigm (such as XHTML), you may
;; want to use outline-minor-mode instead.
;;
;; The goal of this package is to reduce this complexity to three
;; globally-defined keystrokes: one to toggle the state of the fold at
;; point, whatever its type may be, one to hide all folds of all types
;; in the buffer, and one to show all folds.
;;
;; This package currently knows about folding-mode (from folding.el),
;; hs-minor-mode (from hideshow.el), outline-minor-mode (from
;; outline.el), TeX-fold-mode (from AUCTeX), and nXML-mode outlining.
;; More could be added.  It is not necessary to have folding.el,
;; AUCTeX or nXML-mode installed, if you just want to use it with the
;; built-in modes.

;;; Usage:
;;
;; You will need to have one or more of following minor modes switched
;; on: hs-minor-mode, outline-minor-mode, TeX-fold-mode, folding-mode.
;; Otherwise no folds may be found. There are three functions to try:
;;
;; fold-dwim-toggle: try to show any hidden text at the cursor; if no
;; hidden text is found, try to hide the text at the cursor.
;;
;; fold-dwim-hide-all: hide all folds in the buffer.
;;
;; fold-dwim-show-all: show all folds in the buffer.

;;; Configuration
;;
;; This package binds no keys by default, so you need to find three
;; free and convenient key-bindings.  This is what I use:
;;
;;  (global-set-key (kbd "<f7>")      'fold-dwim-toggle)
;;  (global-set-key (kbd "<M-f7>")    'fold-dwim-hide-all)
;;  (global-set-key (kbd "<S-M-f7>")  'fold-dwim-show-all)
;;

;;; Advanced Configuration
;;
;; With respect to outline-minor-mode (or outline-mode), dwim-fold
;; provides two different styles of usage.  The first is a "nested"
;; style which only shows top-level headings when you fold the whole
;; buffer, and then allows you to drill down progressively through the
;; other levels.  The other is a "flat" style, whereby folding the
;; entire buffer shows all headings at every level.
;;
;; The default is "flat", but if you want to change the default, you
;; can set the value of fold-dwim-outline-style-default to be 'flat or
;; 'nested.  If you wish to override the default for a particular
;; major mode, put a value of either 'flat or 'nested for the
;; fold-dwim-outline-style property of the major-mode symbol, like so:
;;
;;   (put 'org-mode 'fold-dwim-outline-style 'nested)
;;
;; At present, there is no way to customize nXML-mode outlining to use
;; the nested style, since it is not really supported by that mode
;; (there is no function to hide all text and subheadings in the
;; buffer).

;;; Compatibility
;;
;; Tested with GNU Emacs CVS (from Sept. 10, 2004), AUCTeX version
;; 11.53, nxml-mode version 20041004, folding.el version 2.97.
;;
;; If there are any other important major or minor modes that do
;; folding and that could usefully be handled in this package, please
;; let me know.

;;; Bugs
;;
;; It is possible that some of the various folding modes may interact
;; badly if used together; I have not tested all permutations.
;;
;; The function fold-dwim-hide tries various folding modes in
;; succession, and stops when it finds one that successfully makes a
;; fold at point.  This means that the order in which those modes are
;; tried is significant.  I have not spent a lot of time thinking
;; about what the optimal order would be; all I care about is that
;; hideshow and TeX-fold have priority over outline-minor-mode (since
;; for me they usually fold smaller chunks of the file).
;;
;; I don't use folding.el myself, so that functionality is not well
;; tested.

;;; Changes
;;
;; 1.0 Initial release
;; 1.1 Bugfix: test if folding-mode is bound
;; 1.2 fold-dwim-hide-all and -show-all operate only on active region
;;     in transient-mark-mode.
;; 1.3 Added outline-mode (Lennart Borgman)

(require 'outline)
(require 'hideshow)

(defgroup fold-dwim nil
  "Unified interface to folding commands"
  :prefix "fold-dwim-"
  :group 'editing)

(defcustom fold-dwim-outline-style-default 'flat
  "Default style in which to fold in outline-minor-mode: 'nested or
  'flat."
  :type '(choice (const :tag "Flat (show all headings)" flat)
                 (const :tag "Nested (nest headings hierarchically)" nested))
  :group 'fold-dwim)

(defvar fold-dwim-toggle-selective-display 'nil
  "Set this non-nil to make fold-dwim functions use selective
  display (folding of all lines indented as much or more than the
  current line).  Probably only useful for minor modes like
  makefile-mode that don't provide a more intelligent way of
  folding.")

(make-variable-buffer-local
 'fold-dwim-toggle-selective-display)

(defun fold-dwim-maybe-recenter ()
  "It's annoyingly frequent that hiding a fold will leave you
with point on the top or bottom line of the screen, looking at
nothing but an ellipsis.  TODO: only recenter if we end up near
the top or bottom of the screen"
  (recenter))

(defun fold-dwim-toggle-selective-display ()
  "Set selective display to indentation of current line"
  (interactive)
  (if (numberp selective-display)
      (set-selective-display nil)
    (save-excursion
      (beginning-of-line)
      (skip-chars-forward " \t")
      (let ((col (current-column)))
        (if (zerop col)
            (set-selective-display nil)
          (set-selective-display col))))))

(defun fold-dwim-hide-all ()
  "Hide all folds of various kinds in the buffer or region"
  (interactive)
  (save-excursion
    (save-restriction
      (when (and transient-mark-mode mark-active)
        (narrow-to-region (region-beginning) (region-end)))
      (when (and (boundp 'TeX-fold-mode) TeX-fold-mode)
        (TeX-fold-buffer))
      (when hs-minor-mode
        (hs-hide-all))
      (when (or outline-minor-mode (eq major-mode 'outline-mode))
        (if (fold-dwim-outline-nested-p)
            (hide-sublevels 1)
          (hide-body)))
      (when (derived-mode-p 'nxml-mode)
        (nxml-hide-all-text-content))
      (when (and (boundp 'folding-mode) folding-mode)
        (folding-whole-buffer))))
  (fold-dwim-maybe-recenter))

(defun fold-dwim-show-all ()
  "Show all folds of various kinds in the buffer or region"
  (interactive)
  (save-excursion
    (save-restriction
      (when (and transient-mark-mode mark-active)
        (narrow-to-region (region-beginning) (region-end)))
      (when (and (boundp 'TeX-fold-mode) TeX-fold-mode)
        (TeX-fold-clearout-buffer))
      (when hs-minor-mode
        (hs-show-all))
      (when (derived-mode-p 'nxml-mode)
        (nxml-show-all))
      (when (or outline-minor-mode (eq major-mode 'outline-mode))
        (show-all))
      (when (and (boundp 'folding-mode) folding-mode)
        (folding-open-buffer))
      (when fold-dwim-toggle-selective-display
        (set-selective-display 'nil)))))

(defun fold-dwim-hide ()
  "Hide one item"
  (save-excursion
    (or (and (boundp 'TeX-fold-mode)
             TeX-fold-mode
             (let ((type (fold-dwim-auctex-env-or-macro)))
               (when type
                 (TeX-fold-item type))))
        (and hs-minor-mode
             (when (save-excursion
                     (or (hs-find-block-beginning) (hs-inside-comment-p)))
               (hs-hide-block)
               (hs-already-hidden-p)))
        (and (derived-mode-p 'nxml-mode)
             (condition-case nil
                 (save-excursion
                   (nxml-back-to-section-start))
               (error nil))
             (nxml-hide-text-content))
        (and (boundp 'folding-mode)
             folding-mode
             (condition-case nil
                 (save-excursion
                   (folding-hide-current-entry)
                   t)
               (error nil)))
        (when (or outline-minor-mode (eq major-mode 'outline-mode))
          (if (fold-dwim-outline-nested-p)
              (hide-subtree)
              (hide-entry)))))
  (fold-dwim-maybe-recenter))


(defun fold-dwim-show ()
  "If point is in a closed or temporarily open fold,
  open it.  Returns nil if nothing was done"
  (save-excursion
    (let ((stop))
      (when (and (or outline-minor-mode (eq major-mode 'outline-mode))
                 (or (fold-dwim-outline-invisible-p (line-end-position))
                     (and (bolp)
                          (not (bobp))
                          (fold-dwim-outline-invisible-p (1- (point))))))
        (if (not (fold-dwim-outline-nested-p))
            (show-entry)
          (show-children)
          (show-entry))
        (setq stop "outline-minor-mode"))
      (when (and (not stop)
                 hs-minor-mode
                 (hs-already-hidden-p))
        (hs-show-block)
        (setq stop "hs-minor-mode"))
      (when (and (not stop)
                 (boundp 'TeX-fold-mode)
                 TeX-fold-mode)
        (let ((overlays (overlays-at (point))))
          (while overlays
            (when (eq (overlay-get (car overlays) 'category) 'TeX-fold)
              (delete-overlay (car overlays))
              (setq stop "Tex-fold-mode"))
            (setq overlays (cdr overlays)))))
      (when (and (not stop)
                 (derived-mode-p 'nxml-mode))
        (let ((overlays (overlays-at (point))))
          (while (and overlays (not stop))
            (when (overlay-get (car overlays) 'nxml-outline-display)
              (setq stop "nxml folding"))
            (setq overlays (cdr overlays))))
        (when stop
            (nxml-show)))
      (when (and (not stop)
                 (boundp 'folding-mode)
                 folding-mode
                 (save-excursion
                   (beginning-of-line)
                   (let ((current-line-mark (folding-mark-look-at)))
                     (when (and (numberp current-line-mark)
                                (= current-line-mark 0))
                       (folding-show-current-entry)
                       (setq stop "folding-mode"))))))
      stop)))

(defun fold-dwim-toggle ()
  "Try fold-dwim-show to show any hidden text at point; if no
hidden fold is found, try fold-dwim-hide to hide the construction
at the cursor."
  (interactive)
  (cond
   ((eq major-mode 'org-mode)
    (org-cycle))
   ((and (fboundp 'outline-cycle)
         outline-minor-mode)
    (outline-cycle))
   (t
    (if fold-dwim-toggle-selective-display
        (fold-dwim-toggle-selective-display)
      (save-excursion
        (let ((unfolded (fold-dwim-show)))
          (if unfolded
              (message "Fold DWIM showed: %s" unfolded)
            (fold-dwim-hide))))))))


(defun fold-dwim-auctex-env-or-macro ()
  (let ((type (cond
               ;; Fold macro before env, unless it's begin or end
               ((save-excursion
                  (let ((macro-start (TeX-find-macro-start)))
                    (and macro-start
                         (not (= macro-start (point)))
                         (goto-char macro-start)
                         (not (looking-at
                               (concat (regexp-quote TeX-esc)
                                       "\\(begin\\|end\\)[ \t]*{"))))))
                'macro)
               ((and (eq major-mode 'context-mode)
                     (save-excursion
                       (ConTeXt-find-matching-start) (point)))
                'env)
               ((and (eq major-mode 'texinfo-mode)
                     (save-excursion
                       (Texinfo-find-env-start) (point)))
                'env)
               ((and (eq major-mode 'latex-mode)
                     (condition-case nil
                         (save-excursion
                           (LaTeX-find-matching-begin) (point)
                           (not (looking-at "\\\\begin[ \t]*{document}")))
                       (error nil)))
                'env)
               (t
                nil))))
    type))

(defun fold-dwim-outline-invisible-p (pos)
  "The version of this function in outline.el doesn't work so
  well for our purposes, because it doesn't distinguish between
  invisibility caused by outline, and that of other modes."
  (save-excursion
    (goto-char pos)
    (let ((overlays (overlays-at (point)))
          (found-one))
      (while overlays
        (when (eq (overlay-get (car overlays) 'invisible) 'outline)
          (setq found-one t))
        (setq overlays (cdr overlays)))
      found-one)))

(defun fold-dwim-outline-nested-p ()
  "Are we using the flat or nested style for outline-minor-mode?"
  (let ((style (get major-mode 'fold-dwim-outline-style)))
    (if style
        (eq style 'nested)
      (eq fold-dwim-outline-style-default 'nested))))

(provide 'fold-dwim)
