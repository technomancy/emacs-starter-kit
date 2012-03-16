;;; starter-kit-eshell.el --- Saner defaults and goodies: eshell tweaks
;;
;; Copyright (c) 2008-2011 Phil Hagelberg and contributors
;;
;; Author: Phil Hagelberg <technomancy@gmail.com>
;; URL: http://www.emacswiki.org/cgi-bin/wiki/StarterKit
;; Version: 2.0.2
;; Keywords: convenience

;; This file is not part of GNU Emacs.

;;; Commentary:

;; "Emacs outshines all other editing software in approximately the
;; same way that the noonday sun does the stars. It is not just bigger
;; and brighter; it simply makes everything else vanish."
;; -Neal Stephenson, "In the Beginning was the Command Line"

;; This file contains eshell tweaks.

;;; License:

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Code:

(setq eshell-cmpl-cycle-completions nil
      eshell-save-history-on-exit t
      eshell-buffer-shorthand t
      eshell-cmpl-dir-ignore "\\`\\(\\.\\.?\\|CVS\\|\\.svn\\|\\.git\\)/\\'")

;;;###autoload
(eval-after-load 'esh-opt
  '(progn
     (require 'em-prompt)
     (require 'em-term)
     (require 'em-cmpl)
     (setenv "PAGER" "cat")
     (set-face-attribute 'eshell-prompt nil :foreground "turquoise1")
     (add-hook 'eshell-mode-hook ;; for some reason this needs to be a hook
               '(lambda () (define-key eshell-mode-map "\C-a" 'eshell-bol)))
     (add-to-list 'eshell-output-filter-functions 'eshell-handle-ansi-color)
     (setq eshell-cmpl-cycle-completions nil)

     ;; TODO: submit these via M-x report-emacs-bug
     (add-to-list 'eshell-visual-commands "ssh")
     (add-to-list 'eshell-visual-commands "tail")
     (add-to-list 'eshell-command-completions-alist
                  '("gunzip" "gz\\'"))
     (add-to-list 'eshell-command-completions-alist
                  '("tar" "\\(\\.tar|\\.tgz\\|\\.tar\\.gz\\)\\'"))))

;;;###autoload
(defun eshell/cds ()
  "Change directory to the project's root."
  (eshell/cd (locate-dominating-file default-directory "src")))

;;;###autoload
(defun eshell/cds ()
  "Change directory to the project's root."
  (eshell/cd (locate-dominating-file default-directory "src")))

;;;###autoload
(defun eshell/cdl ()
  "Change directory to the project's root."
  (eshell/cd (locate-dominating-file default-directory "lib")))

;;;###autoload
(defun eshell/cdg ()
  "Change directory to the project's root."
  (eshell/cd (locate-dominating-file default-directory ".git")))

;; these two haven't made it upstream yet
;;;###autoload
(when (not (functionp 'eshell/find))
  (defun eshell/find (dir &rest opts)
    (find-dired dir (mapconcat (lambda (arg)
                                 (if (get-text-property 0 'escaped arg)
                                     (concat "\"" arg "\"")
                                   arg))
                               opts " "))))

;;;###autoload
(when (not (functionp 'eshell/rgrep))
  (defun eshell/rgrep (&rest args)
    "Use Emacs grep facility instead of calling external grep."
    (eshell-grep "rgrep" args t)))

;;;###autoload
(defun eshell/extract (file)
  (let ((command (some (lambda (x)
                         (if (string-match-p (car x) file)
                             (cadr x)))
                       '((".*\.tar.bz2" "tar xjf")
                         (".*\.tar.gz" "tar xzf")
                         (".*\.bz2" "bunzip2")
                         (".*\.rar" "unrar x")
                         (".*\.gz" "gunzip")
                         (".*\.tar" "tar xf")
                         (".*\.tbz2" "tar xjf")
                         (".*\.tgz" "tar xzf")
                         (".*\.zip" "unzip")
                         (".*\.Z" "uncompress")
                         (".*" "echo 'Could not extract the file:'")))))
    (eshell-command-result (concat command " " file))))

(defface esk-eshell-error-prompt-face
  '((((class color) (background dark)) (:foreground "red" :bold t))
    (((class color) (background light)) (:foreground "red" :bold t)))
  "Face for nonzero prompt results"
  :group 'eshell-prompt)

(add-hook 'eshell-after-prompt-hook
          (defun esk-eshell-exit-code-prompt-face ()
            (when (and eshell-last-command-status
                       (not (zerop eshell-last-command-status)))
              (let ((inhibit-read-only t))
                (add-text-properties
                 (save-excursion (beginning-of-line) (point)) (point-max)
                 '(face esk-eshell-error-prompt-face))))))

(defun esk-eshell-in-dir (&optional prompt)
  "Change the directory of an existing eshell to the directory of the file in
  the current buffer or launch a new eshell if one isn't running.  If the
  current buffer does not have a file (e.g., a *scratch* buffer) launch or raise
  eshell, as appropriate.  Given a prefix arg, prompt for the destination
  directory."
  (interactive "P")
  (let* ((name (buffer-file-name))
         (dir (cond (prompt (read-directory-name "Directory: " nil nil t))
                    (name (file-name-directory name))
                    (t nil)))
         (buffers (delq nil (mapcar (lambda (buf)
                                      (with-current-buffer buf
                                        (when (eq 'eshell-mode major-mode)
                                          (buffer-name))))
                                    (buffer-list))))
         (buffer (cond ((eq 1 (length buffers)) (first buffers))
                       ((< 1 (length buffers)) (ido-completing-read
                                                "Eshell buffer: " buffers nil t
                                                nil nil (first buffers)))
                       (t (eshell)))))
    (with-current-buffer buffer
      (when dir
        (eshell/cd (list dir))
        (eshell-send-input))
      (end-of-buffer)
      (pop-to-buffer buffer))))

;; Port features from
;; http://blog.peepcode.com/tutorials/2009/shell-method-missing/shell_method_missing.rb
;; * cloning git repos, github repos
;; * downloading http urls
;; * extracting archives
;; * changing to directories

(provide 'starter-kit-eshell)
;;; starter-kit-eshell.el ends here
