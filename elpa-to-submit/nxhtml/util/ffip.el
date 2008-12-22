;;; ffip2.el --- Find files in project
;;
;; Authors: extracted from rinari by Phil Hagelberg and Doug Alcorn
;; Changed by Lennart Borgman
;; Created: 2008-08-14T23:46:22+0200 Thu
;; Version: 0.3
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Project data

;; Fix-me: Change the inner structure of ffip projects
(defvar ffip-project-name nil "Project name.")
(defconst ffip-project-roots nil "Project directory roots.")
(defvar ffip-project-type nil "Project type, `ffip-project-file-types'.")
(defvar ffip-project-file-matcher nil "Project file matcher.")
(defvar ffip-project-files-table nil "Project file cache.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Project handling

(defun ffip-reset-project ()
  "Clear project data."
  (remove-hook 'after-save-hook 'ffip-after-save)
  (setq ffip-project-name nil)
  (setq ffip-project-roots nil)
  (setq ffip-project-files-table nil)
  (setq ffip-project-type nil)
  (setq ffip-project-file-matcher nil))
;;(ffip-reset-project)

(defun ffip-is-current (name root type)
  "Return non-nil if NAME, ROOT and TYPE match current ffip project.
See `ffip-set-current-project'."
  (and name
       (string= ffip-project-name name)
       (eq ffip-project-type type)
       (equal ffip-project-roots root)))

(defun ffip-set-current-project (name root type)
  "Setup ffip project NAME with top directory ROOT of type TYPE.
ROOT can either be just a directory or a list of directory where
the first used just for prompting purposes and the files in the
rest are read into the ffip project.

Type is a type in `ffip-project-file-types'."
  (unless (ffip-is-current name root type)
    (ffip-reset-project)
    (setq ffip-project-name name)
    (setq ffip-project-type type)
    (setq ffip-project-roots root)
    (message "Project %s with %s files setup for find-files-in-project"
             name (length ffip-project-files-table))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; File cache handling

(defun ffip-cache-project-files (file-regexp)
  "Read files and cache their names within the ffip project."
  (let ((root ffip-project-roots))
    (message "... reading files in %s ..." root)
    (add-hook 'after-save-hook 'ffip-after-save)
    (if (not (listp root))
        (ffip-populate-files-table root file-regexp)
      (setq root (cdr root))
      (dolist (r root)
        (ffip-populate-files-table r file-regexp)))))

(defun ffip-file-matcher ()
  (when ffip-project-type
    (cadr (assoc ffip-project-type ffip-project-file-types))))

(defun ffip-project-files ()
  "Get a list of all files in ffip project.
The members in the list has the format

  \(SHORT-NAME . FULL-NAME)

where SHORT-NAME is a unique name (normally file name without
directory) and FULL-NAME is the full file name."
  (unless ffip-project-files-table
    (let ((file-regexp (ffip-file-matcher)))
      (ffip-cache-project-files file-regexp)))
  ffip-project-files-table)

;; Fix-me: Seems better to rewrite this to use
;; project-find-settings-file.
(defun ffip-project-root (&optional dir)
  (setq dir (or dir
                ffip-project-roots
                default-directory))
  ;;(locate-dominating-file "." "\\`\\find-file-in-project.el\\'")
  (let ((root (locate-dominating-file dir
                                      ;;"\\`\\.emacs-project\\'"
                                      "\\`\\.dir-settings\\.el\\'"
                                      )))
    (if root
        (file-name-directory root)
      dir)))

(defcustom ffip-project-file-types
  (list
    '(ruby "\\(\\.el$\\|\\.rb$\\|\\.js$\\|\\.emacs\\)")
    (list 'nxhtml (concat
                   (regexp-opt '(".html" ".htm" ".xhtml"
                                 ".css"
                                 ".js"
                                 ".png" ".gif"
                                 ))
                  "\\'"))
    )
  "Project types and file types.
The values in this list are used to determine if a file belongs
to the current ffip project. Entries have the form

  \(TYPE FILE-REGEXP)

TYPE is the parameter set by `ffip-set-current-project'.  Files
matching FILE-REGEXP within the project roots are members of the
project."
  :type '(repeat (list
                  (symbol :tag "Type")
                  (regexp :tag "File regexp")))
  :group 'ffip)

(defun ffip-populate-files-table (file file-regexp)
  ;;(message "ffip-populate-files-table.file=%s" file)
  (if (file-directory-p file)
      (mapc (lambda (file)
              (ffip-populate-files-table file file-regexp))
            (directory-files (expand-file-name file) t "^[^\.]"))
    (let* ((file-name (file-name-nondirectory file))
	   (existing-record (assoc file-name ffip-project-files-table))
	   (unique-parts (ffip-get-unique-directory-names file
                                                     (cdr existing-record))))
      (when (or (not file-regexp)
              (string-match file-regexp file-name))
          (if existing-record
              (let ((new-key (concat file-name " - " (car unique-parts)))
                    (old-key (concat (car existing-record) " - "
                                     (cadr unique-parts))))
                (setf (car existing-record) old-key)
                (setq ffip-project-files-table
                      (acons new-key file ffip-project-files-table)))
            (setq ffip-project-files-table
                  (acons file-name file ffip-project-files-table)))))))

(defun ffip-get-unique-directory-names (path1 path2)
  (let* ((parts1 (and path1 (split-string path1 "/" t)))
	 (parts2 (and path2 (split-string path2 "/" t)))
	 (part1 (pop parts1))
	 (part2 (pop parts2))
	 (looping t))
    (while (and part1 part2 looping)
      (if (equal part1 part2)
	  (setq part1 (pop parts1) part2 (pop parts2))
	(setq looping nil)))
    (list part1 part2)))

(defun ffip-file-is-in-project (file-name)
  "Return non-nil if file is in current ffip project."
  (save-match-data
    (let ((file-regexp (ffip-file-matcher))
          (roots ffip-project-roots)
          regexp)
      (if (not (listp roots))
          (setq roots (list roots))
        (setq roots (cdr roots)))
      (catch 'found
      (dolist (root roots)
        (setq file-regexp (concat root ".*" file-regexp))
        (when (string-match file-regexp file-name)
          (throw 'found t)))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Updating on file changes

(defun ffip-add-file-if-in-project (file-name)
  "Add file to cache if it in ffip project."
  (when (ffip-file-is-in-project file-name)
    ;; We have already checked so just use nil for the matcher.
    (ffip-populate-files-table file-name nil)))

;; For after-save-hook
(defun ffip-after-save ()
  "Check if a file should be added to cache."
  (condition-case err
      (ffip-add-file-if-in-project buffer-file-name)
    (error (message "%s" (error-message-string err)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Interactive functions

(defun ffip-find-file-in-dirtree (root)
  "Find files in directory tree ROOT."
  (interactive "DFind file in directory tree: ")
  ;; Setup a temporary
  (let ((ffip-project-name nil)
        (ffip-project-roots nil)
        (ffip-project-files-table nil)
        (ffip-project-type nil)
        (ffip-project-file-matcher nil))
    (ffip-set-current-project "(temporary)" root nil)
    (call-interactively 'ffip-find-file-in-project)))

(defun ffip-find-file-in-project (file)
  "Find files in current ffip project."
  (interactive
   (list
    (let* ((prompt (format "Find file in project %s: "
                           ffip-project-name)))
      (if (memq ido-mode '(file 'both))
          (ido-completing-read prompt
                               (mapcar 'car (ffip-project-files)))
        (let ((files (mapcar 'car (ffip-project-files))))
          (completing-read prompt
                           files
                           (lambda (elem) (member elem files))
                           t))))))
  (find-file (cdr (assoc file ffip-project-files-table))))

;;(global-set-key (kbd "C-x C-M-f") 'find-file-in-project)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Fix-me: This part should go somewhere else
(eval-after-load 'ruby-mode
  '(progn
     (defun ffip-rails-project-files (&optional file)
       (let ((default-directory (or file (rails-root))))
         (unless (and ffip-project-roots
                      (string= default-directory ffip-project-roots))
           (ffip-set-current-project
            "Rails proj"
            root
            (list default-directory
                  (expand-file-name "app")
                  (expand-file-name "lib")
                  (expand-file-name "test"))
            'ruby
            )))
       (ffip-project-files))

     (defun ffip-find-file-in-rails (file)
       (interactive
        (list (if (memq ido-mode '(file 'both))
                  (ido-completing-read
                   "Find file in project: "
                   (mapcar 'car (ffip-rails-project-files)))
                (completing-read "Find file in project: "
                                 (mapcar 'car (rails-project-files))))))
       (find-file (cdr (assoc file ffip-project-files-table))))

     (define-key ruby-mode-map (kbd "C-x C-M-f") 'find-file-in-rails)
     (eval-after-load 'nxhtml-mode
       '(define-key nxhtml-mode-map (kbd "C-x C-M-f") 'find-file-in-rails))))

(provide 'ffip)
;;; ffip.el ends here
