;;; package.el --- Simple package system for Emacs

;; Copyright (C) 2007, 2008, 2009 Tom Tromey <tromey@redhat.com>

;; Author: Tom Tromey <tromey@redhat.com>
;; Created: 10 Mar 2007
;; Version: 0.9.4
;; Keywords: tools

;; This file is not (yet) part of GNU Emacs.
;; However, it is distributed under the same license.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; To use this, put package.el somewhere on your load-path.  Then add
;; this to your .emacs:
;;
;;    (load "package")
;;    (package-initialize)
;;
;; This will automatically make available the packages you have
;; installed using package.el.  If your .emacs will refer to these
;; packages, you may want to initialize the package manager near the
;; top.
;;
;; Note that if you want to be able to automatically download and
;; install packages from ELPA (the Emacs Lisp Package Archive), then
;; you will need the 'url' package.  This comes with Emacs 22; Emacs
;; 21 users will have to find it elsewhere.
;;
;; If you installed package.el via the auto-installer:
;;
;;    http://tromey.com/elpa/
;;
;; then you do not need to edit your .emacs, as the installer will
;; have done this for you.  The installer will also install the url
;; package if you need it.

;; Other external functions you may want to use:
;;
;; M-x package-list-packages
;;    Enters a mode similar to buffer-menu which lets you manage
;;    packages.  You can choose packages for install (mark with "i",
;;    then "x" to execute) or deletion (not implemented yet), and you
;;    can see what packages are available.  This will automatically
;;    fetch the latest list of packages from ELPA.
;;
;; M-x package-list-packages-no-fetch
;;    Like package-list-packages, but does not automatically fetch the
;;    new list of packages.
;;
;; M-x package-install-from-buffer
;;    Install a package consisting of a single .el file that appears
;;    in the current buffer.  This only works for packages which
;;    define a Version header properly; package.el also supports the
;;    extension headers Package-Version (in case Version is an RCS id
;;    or similar), and Package-Requires (if the package requires other
;;    packages).
;;
;; M-x package-install-file
;;    Install a package from the indicated file.  The package can be
;;    either a tar file or a .el file.  A tar file must contain an
;;    appropriately-named "-pkg.el" file; a .el file must be properly
;;    formatted as with package-install-from-buffer.

;; The idea behind package.el is to be able to download packages and
;; install them.  Packages are versioned and have versioned
;; dependencies.  Furthermore, this supports built-in packages which
;; may or may not be newer than user-specified packages.  This makes
;; it possible to upgrade Emacs and automatically disable packages
;; which have moved from external to core.  (Note though that we don't
;; currently register any of these, so this feature does not actually
;; work.)

;; This code supports a single package repository out of the box,
;; ELPA.  Additional repositories may be added by appending to the
;; package-archives alist.  To run your own package archive, see the
;; package-maint.el library included.

;; A package is described by its name and version.  The distribution
;; format is either  a tar file or a single .el file.

;; A tar file should be named "NAME-VERSION.tar".  The tar file must
;; unpack into a directory named after the package and version:
;; "NAME-VERSION".  It must contain a file named "PACKAGE-pkg.el"
;; which consists of a call to define-package.  It may also contain a
;; "dir" file and the info files it references.

;; A .el file will be named "NAME-VERSION.el" in ELPA, but will be
;; installed as simply "NAME.el" in a directory named "NAME-VERSION".

;; The downloader will download all dependent packages.  It will also
;; byte-compile the package's Lisp at install time.

;; At activation time we will set up the load-path and the info path,
;; and we will load the package's autoloads.  If a package's
;; dependencies are not available, we will not activate that package.

;; Conceptually a package has multiple state transitions:
;;
;; * Download.  Fetching the package from its source.
;; * Install.  Untar the package, or write the .el file, into
;;   ~/.emacs.d/elpa/ directory.
;; * Byte compile.  Currently this phase is done during install,
;;   but we may change this.
;; * Activate.  Evaluate the autoloads for the package to make it
;;   available to the user.
;; * Load.  Actually load the package and run some code from it.

;;; Thanks:
;;; (sorted by sort-lines):

;; Jim Blandy <jimb@red-bean.com>
;; Karl Fogel <kfogel@red-bean.com>
;; Kevin Ryde <user42@zip.com.au>
;; Lawrence Mitchell
;; Michael Olson <mwolson@member.fsf.org>
;; Sebastian Tennant <sebyte@smolny.plus.com>
;; Stefan Monnier <monnier@iro.umontreal.ca>
;; Vinicius Jose Latorre <viniciusjl@ig.com.br>
;; Phil Hagelberg <phil@hagelb.org>
;; Samuel Bronson <naesten@gmail.com>

;;; History:
;;
;; Originally written by Tom Tromey, multiple archive support added by Phil
;; Hagelberg.

;;; Code:

(defcustom package-archives '(("elpa" . "http://tromey.com/elpa/"))
  "An alist of archives (names and URLs) from which to fetch.
The default points to ELPA, the Emacs Lisp Package Archive.
Note that some code in package.el assumes that this is an http: URL."
  :type '(alist :key-type (string :tag "Archive name")
                :value-type (string :tag "Archive URL"))
  :group 'package
  :package-version '("package.el" . "0.9.3"))

(defconst package-archive-version 1
  "Version number of the package archive understood by this file.
Lower version numbers than this will probably be understood as well.")

(defconst package-el-version "0.9.4"
  "Version of package.el.")

;; We don't prime the cache since it tends to get out of date.
(defvar package-archive-contents
  nil
  "A representation of the contents of the ELPA archive.
This is an alist mapping package names (symbols) to package
descriptor vectors.  These are like the vectors for `package-alist'
but have extra entries: one which is 'tar for tar packages and
'single for single-file packages, and one which is the name of
the archive from which it came.")

(defvar package-user-dir
  (expand-file-name (convert-standard-filename "~/.emacs.d/elpa"))
  "Name of the directory where the user's packages are stored.")

(defvar package-directory-list
  (list (file-name-as-directory package-user-dir)
        "/usr/share/emacs/site-lisp/elpa/")
  "List of directories to search for packages.")

(defun package-version-split (string)
  "Split a package STRING into a version list."
  (mapcar 'string-to-int (split-string string "[.]")))

(defconst package--builtins-base
  ;; We use package-version split here to make sure to pick up the
  ;; minor version.
  `((emacs . [,(package-version-split emacs-version) nil
              "GNU Emacs"])
    (package . [,(package-version-split package-el-version)
                nil "Simple package system for GNU Emacs"]))
  "Packages which are always built-in.")

(defvar package--builtins
  (delq nil
        (append
         package--builtins-base
         (if (>= emacs-major-version 22)
             ;; FIXME: emacs 22 includes tramp, rcirc, maybe
             ;; other things...
             '((erc . [(5 2) nil "An Emacs Internet Relay Chat client"])
               ;; The external URL is version 1.15, so make sure the
               ;; built-in one looks newer.
               (url . [(1 16) nil "URL handling libary"])))
         (if (>= emacs-major-version 23)
             '(;; Strangely, nxml-version is missing in Emacs 23.
               ;; We pick the merge date as the version.
               (nxml . [(20071123) nil "Major mode for editing XML documents."])
               (bubbles . [(0 5) nil "Puzzle game for Emacs."])))))
  "Alist of all built-in packages.
Maps the package name to a vector [VERSION REQS DOCSTRING].")

(defvar package-alist package--builtins
  "Alist of all packages available for activation.
Maps the package name to a vector [VERSION REQS DOCSTRING].")

(defvar package-activated-list
  (mapcar #'car package-alist)
  "List of the names of all activated packages.")

(defvar package-obsolete-alist nil
  "Representation of obsolete packages.
Like `package-alist', but maps package name to a second alist.
The inner alist is keyed by version.")

(defun package-version-join (l)
  "Turn a list L of version numbers into a version string."
  (mapconcat 'int-to-string l "."))

(defun package--version-first-nonzero (l)
  "Find the first non-zero number in the list L.

Returns the value of the first non-zero integer in L, or 0 if
none is found."
  (while (and l (= (car l) 0))
    (setq l (cdr l)))
  (if l (car l) 0))

(defun package-version-compare (v1 v2 fun)
  "Compare two version V1 and V2 lists according to FUN.

FUN can be <, <=, =, >, >=, or /=."
  (while (and v1 v2 (= (car v1) (car v2)))
    (setq v1 (cdr v1)
          v2 (cdr v2)))
  (if v1
      (if v2
          ;; Both not null; we know the cars are not =.
          (funcall fun (car v1) (car v2))
        ;; V1 not null, V2 null.
        (funcall fun (package--version-first-nonzero v1) 0))
    (if v2
        ;; V1 null, V2 not null.
        (funcall fun 0 (package--version-first-nonzero v2))
      ;; Both null.
      (funcall fun 0 0))))

(defun package--test-version-compare ()
  "Test suite for `package-version-compare'."
  (unless (and (package-version-compare '(0) '(0) '=)
               (not (package-version-compare '(1) '(0) '=))
               (package-version-compare '(1 0 1) '(1) '>=)
               (package-version-compare '(1 0 1) '(1) '>)
               (not (package-version-compare '(0 9 1) '(1 0 2) '>=)))
    (error "Failed"))
  t)

(defun package-strip-version (dirname)
  "Strip the version from a combined package name and version.
E.g., if DIRNAME is \"quux-23.0\", will return \"quux\""
  (if (string-match "^\\(.*\\)-[0-9]+\\([.][0-9]+\\)*$" dirname)
      (match-string 1 dirname)))

(defun package-load-descriptor (dir package)
  "Load the description file in directory DIR for a PACKAGE.
Return nil if the package could not be found."
  (let* ((pkg-dir (expand-file-name package dir))
         (pkg-file (expand-file-name
                    (concat (package-strip-version package) "-pkg") pkg-dir)))
    (when (and (file-directory-p pkg-dir)
               (file-exists-p (concat pkg-file ".el")))
        (load pkg-file nil t))))

(defun package-load-all-descriptors ()
  "Load descriptors of all packages.
Uses `package-directory-list' to find packages."
  (mapc (lambda (dir)
          (if (file-directory-p dir)
              (mapc (lambda (name)
                      (package-load-descriptor dir name))
                    (directory-files dir nil "^[^.]"))))
        package-directory-list))

(defsubst package-desc-vers (desc)
  "Extract version from a package description vector DESC."
  (aref desc 0))

(defsubst package-desc-reqs (desc)
  "Extract requirements from a package description vector DESC."
  (aref desc 1))

(defsubst package-desc-doc (desc)
  "Extract doc string from a package description vector DESC."
  (aref desc 2))

(defsubst package-desc-kind (desc)
  "Extract the kind of download from an archive package description vector DESC."
  (aref desc 3))

(defun package-do-activate (package pkg-vec)
  "Set up a single PACKAGE.

Modifies `load-path' to include the package directory and loads
the `autoload' file for the package. PKG-VEC is the package info
as retrieved from the package mirror."
  (let* ((pkg-name (symbol-name package))
         (pkg-ver-str (package-version-join (package-desc-vers pkg-vec)))
         (dir-list package-directory-list)
         (pkg-dir))
    (while dir-list
      (let ((subdir (concat (car dir-list) pkg-name "-" pkg-ver-str "/")))
        (if (file-directory-p subdir)
            (progn
              (setq pkg-dir subdir)
              (setq dir-list nil))
          (setq dir-list (cdr dir-list)))))
    (unless pkg-dir
      (error "Internal error: could not find directory for %s-%s"
             pkg-name pkg-ver-str))
    (if (file-exists-p (concat pkg-dir "dir"))
        (progn
          ;; FIXME: not the friendliest, but simple.
          (require 'info)
          (info-initialize)
          (setq Info-directory-list (cons pkg-dir Info-directory-list))))
    (setq load-path (cons pkg-dir load-path))
    ;; Load the autoloads and activate the package.
    (load (concat pkg-dir (symbol-name package) "-autoloads")
          nil t)
    (setq package-activated-list (cons package package-activated-list))
    ;; Don't return nil.
    t))

(defun package--built-in (package version)
  "Return true if PACKAGE at VERSION is built-in to Emacs."
  (let ((elt (assq package package--builtins)))
    (and elt
         (package-version-compare (package-desc-vers (cdr elt)) version '=))))

;; FIXME: return a reason instead?
(defun package-activate (package version)
  "Try to activate PACKAGE at version VERSION.
Return nil if the package could not be activated.
Recursively activates all dependencies of the named package."
  ;; Assume the user knows what he is doing -- go ahead and activate a
  ;; newer version of a package if an older one has already been
  ;; activated.  This is not ideal; we'd at least need to check to see
  ;; if the package has actually been loaded, and not merely
  ;; activated.  However, don't try to activate 'emacs', as that makes
  ;; no sense.
  (unless (eq package 'emacs)
    (let* ((pkg-desc (assq package package-alist))
           (this-version (package-desc-vers (cdr pkg-desc)))
           (req-list (package-desc-reqs (cdr pkg-desc)))
           ;; If the package was never activated, we want to do it
           ;; now.
           (keep-going (or (not (memq package package-activated-list))
                           (package-version-compare this-version version '>))))
      (while (and req-list keep-going)
        (or (package-activate (car (car req-list))
                              (car (cdr (car req-list))))
            (setq keep-going nil))
        (setq req-list (cdr req-list)))
      (if keep-going
          (package-do-activate package (cdr pkg-desc))
        ;; We get here if a dependency failed to activate -- but we
        ;; can also get here if the requested package was already
        ;; activated.  Return non-nil in the latter case.
        (and (memq package package-activated-list)
             (package-version-compare this-version version '>=))))))

(defun package-mark-obsolete (package pkg-vec)
  "Put PACKAGE on the obsolete list, if not already there.

PKG-VEC describes the version of PACKAGE to mark obsolete."
  (let ((elt (assq package package-obsolete-alist)))
    (if elt
        ;; If this obsolete version does not exist in the list, update
        ;; it the list.
        (unless (assoc (package-desc-vers pkg-vec) (cdr elt))
          (setcdr elt (cons (cons (package-desc-vers pkg-vec) pkg-vec)
                            (cdr elt))))
      ;; Make a new association.
      (setq package-obsolete-alist
            (cons (cons package (list (cons (package-desc-vers pkg-vec)
                                            pkg-vec)))
                  package-obsolete-alist)))))

;; (define-package "emacs" "21.4.1" "GNU Emacs core package.")
;; (define-package "erc" "5.1" "ERC - irc client" '((emacs "21.0")))
(defun define-package (name-str version-string
                                &optional docstring requirements)
  "Define a new package.
NAME-STR is the name of the package, a string.
VERSION-STRING is the version of the package, a dotted sequence
of integers.
DOCSTRING is the optional description.
REQUIREMENTS is a list of requirements on other packages.
Each requirement is of the form (OTHER-PACKAGE \"VERSION\")."
  (let* ((name (intern name-str))
         (pkg-desc (assq name package-alist))
         (new-version (package-version-split version-string))
         (new-pkg-desc
          (cons name
                (vector new-version
                        (mapcar
                         (lambda (elt)
                           (list (car elt)
                                 (package-version-split (car (cdr elt)))))
                         requirements)
                        docstring))))
    ;; Only redefine a package if the redefinition is newer.
    (if (or (not pkg-desc)
            (package-version-compare new-version
                                     (package-desc-vers (cdr pkg-desc))
                                     '>))
        (progn
          (when pkg-desc
            ;; Remove old package and declare it obsolete.
            (setq package-alist (delq pkg-desc package-alist))
            (package-mark-obsolete (car pkg-desc) (cdr pkg-desc)))
          ;; Add package to the alist.
          (setq package-alist (cons new-pkg-desc package-alist)))
      ;; You can have two packages with the same version, for instance
      ;; one in the system package directory and one in your private
      ;; directory.  We just let the first one win.
      (unless (package-version-compare new-version
                                       (package-desc-vers (cdr pkg-desc))
                                       '=)
        ;; The package is born obsolete.
        (package-mark-obsolete (car new-pkg-desc) (cdr new-pkg-desc))))))

;; From Emacs 22.
(defun package-autoload-ensure-default-file (file)
  "Make sure that the autoload file FILE exists and if not create it."
  (unless (file-exists-p file)
    (write-region
     (concat ";;; " (file-name-nondirectory file)
             " --- automatically extracted autoloads\n"
             ";;\n"
             ";;; Code:\n\n"
             "\n;; Local Variables:\n"
             ";; version-control: never\n"
             ";; no-byte-compile: t\n"
             ";; no-update-autoloads: t\n"
             ";; End:\n"
             ";;; " (file-name-nondirectory file)
             " ends here\n")
     nil file))
  file)

(defun package-generate-autoloads (name pkg-dir)
  "Generate autoload definitions for package NAME in PKG-DIR."
  (let* ((auto-name (concat name "-autoloads.el"))
         (ignore-name (concat name "-pkg.el"))
         (generated-autoload-file (concat pkg-dir auto-name))
         (version-control 'never))
    ;; In Emacs 22 `update-directory-autoloads' does not seem
    ;; to be autoloaded...
    (require 'autoload)
    (unless (fboundp 'autoload-ensure-default-file)
      (package-autoload-ensure-default-file generated-autoload-file))
    (update-directory-autoloads pkg-dir)))

(defun package-untar-buffer ()
  "Untar the current buffer.
This uses `tar-untar-buffer' if it is available.
Otherwise it uses an external `tar' program.
`default-directory' should be set by the caller."
  (require 'tar-mode)
  (if (fboundp 'tar-untar-buffer)
      (progn
        ;; tar-mode messes with narrowing, so we just let it have the
        ;; whole buffer to play with.
        (delete-region (point-min) (point))
        (tar-mode)
        (tar-untar-buffer))
    ;; FIXME: check the result.
    (call-process-region (point) (point-max) "tar" nil '(nil nil) nil
                         "xf" "-")))

(defun package-unpack (name version)
  "Unpack a package tar from the current buffer.

Unpack the package, using NAME and VERSION to determine the
target. The current buffer is expected to contain a tarred
package archive."
  (let ((pkg-dir (concat (file-name-as-directory package-user-dir)
                         (symbol-name name) "-" version "/")))
    ;; Be careful!!
    (make-directory package-user-dir t)
    (if (file-directory-p pkg-dir)
        (mapc (lambda (file) nil) ; 'delete-file -- FIXME: when we're
                                        ; more confident
              (directory-files pkg-dir t "^[^.]")))
    (let* ((default-directory (file-name-as-directory package-user-dir)))
      (package-untar-buffer)
      (package-generate-autoloads (symbol-name name) pkg-dir)
      (let ((load-path (cons pkg-dir load-path)))
        (byte-recompile-directory pkg-dir 0 t)))))

(defun package-write-file-no-coding (file-name excl)
  (let ((buffer-file-coding-system 'no-conversion))
    (write-region (point-min) (point-max) file-name nil nil nil excl)))

(defun package-unpack-single (file-name version desc requires)
  "Install the contents of the current buffer as a package.

FILE-NAME is the name of the current file being unpacked.
package.el itself is handled specially, so this information is
important.

VERSION is the version (as a string) of the file being unpacked.

DESC is a brief description of the package.

REQUIRES is a list of symbols which this package needs to run."
  (let* ((dir (file-name-as-directory package-user-dir)))
    ;; Special case "package".
    (if (string= file-name "package")
        (package-write-file-no-coding (concat dir file-name ".el") nil)
      (let ((pkg-dir (file-name-as-directory
                      (concat dir file-name "-" version))))
        (make-directory pkg-dir t)
        (package-write-file-no-coding (concat pkg-dir file-name ".el") 'excl)
        (let ((print-level nil)
              (print-length nil))
          (write-region
           (concat
            (prin1-to-string
             (list 'define-package
                   file-name
                   version
                   desc
                   (list 'quote
                         ;; Turn version lists into string form.
                         (mapcar
                          (lambda (elt)
                            (list (car elt)
                                  (package-version-join (car (cdr elt)))))
                          requires))))
            "\n")
           nil
           (concat pkg-dir file-name "-pkg.el")
           nil nil nil 'excl))
        (package-generate-autoloads file-name pkg-dir)
        (let ((load-path (cons pkg-dir load-path)))
          (byte-recompile-directory pkg-dir 0 t))))))

(defun package-handle-response ()
  "Handle the response from the server.
Parse the HTTP response and throw if an error occurred.
The url package seems to require extra processing for this.
This should be called in a `save-excursion', in the download buffer.
It will move point to somewhere in the headers."
  (let ((type (url-type url-current-object)))
    (cond
     ((equal type "http")
      (let ((response (url-http-parse-response)))
        (when (or (< response 200) (>= response 300))
          (display-buffer (current-buffer))
          (error "Error during download request:%s"
                 (buffer-substring-no-properties (point) (progn
                                                           (end-of-line)
                                                           (point)))))))
     ((equal type "file")
      nil))))

(defun package-download-single (name version desc requires)
  "Download and install a single-file package.

NAME, VERSION, DESC, and REQUIRES are used to build the package
info."
  (let ((buffer (url-retrieve-synchronously
                 (concat (package-archive-for name)
                         (symbol-name name) "-" version ".el"))))
    (save-excursion
      (set-buffer buffer)
      (package-handle-response)
      (re-search-forward "^$" nil 'move)
      (forward-char)
      (delete-region (point-min) (point))
      (package-unpack-single (symbol-name name) version desc requires)
      (kill-buffer buffer))))

(defun package-download-tar (name version)
  "Download and install a tar package NAME at VERSION."
  (let ((tar-buffer (url-retrieve-synchronously
                     (concat (package-archive-for name)
                             (symbol-name name) "-" version ".tar"))))
    (save-excursion
      (set-buffer tar-buffer)
      (package-handle-response)

      ;; Skip past url-retrieve headers, which would otherwise confuse poor
      ;; tar-mode.
      (goto-char (point-min))
      (re-search-forward "^$" nil 'move)
      (forward-char)

      (package-unpack name version)
      (kill-buffer tar-buffer))))

(defun package-installed? (package &optional min-version)
  "Check whether PACKAGE is installed and at least MIN-VERSION."
  (let ((pkg-desc (assq package package-alist)))
    (and pkg-desc
         (package-version-compare min-version
                                  (package-desc-vers (cdr pkg-desc))
                                  '<=))))

(defun package-compute-transaction (result requirements)
  "Recursively prepare a transaction, resolving dependencies.

RESULT is a flattened list of packages to install.
`package-compute-transaction' recursively builds this argument
before passing it up to the caller.

REQUIREMENTS is a list of required packages, to be recursively
processed to resolve all dependencies (if possible)."
  (while requirements
    (let* ((elt (car requirements))
           (next-pkg (car elt))
           (next-version (car (cdr elt))))
      (unless (package-installed? next-pkg next-version)
        (let ((pkg-desc (assq next-pkg package-archive-contents)))
          (unless pkg-desc
            (error "Package '%s' not available for installation"
                   (symbol-name next-pkg)))
          (unless (package-version-compare (package-desc-vers (cdr pkg-desc))
                                           next-version
                                           '>=)
            (error
             "Need package '%s' with version %s, but only %s is available"
             (symbol-name next-pkg) (package-version-join next-version)
             (package-version-join (package-desc-vers (cdr pkg-desc)))))
          ;; Only add to the transaction if we don't already have it.
          (unless (memq next-pkg result)
            (setq result (cons next-pkg result)))
          (setq result
                (package-compute-transaction result
                                             (package-desc-reqs
                                              (cdr pkg-desc)))))))
    (setq requirements (cdr requirements)))
  result)

(defun package-read-from-string (str)
  "Read a Lisp expression from STR.
Signal an error if the entire string was not used."
  (let* ((read-data (read-from-string str))
         (more-left
          (condition-case nil
              ;; The call to `ignore' suppresses a compiler warning.
              (progn (ignore (read-from-string
                              (substring str (cdr read-data))))
                     t)
            (end-of-file nil))))
    (if more-left
        (error "Can't read whole string")
      (car read-data))))

(defun package--read-archive-file (file)
  "Re-read archive file FILE, if it exists.
Will return the data from the file, or nil if the file does not exist.
Will throw an error if the archive version is too new."
  (let ((filename (concat (file-name-as-directory package-user-dir)
                          file)))
    (if (file-exists-p filename)
        (with-temp-buffer
          (insert-file-contents-literally filename)
          (let ((contents (package-read-from-string
                           (buffer-substring-no-properties (point-min)
                                                           (point-max)))))
            (if (> (car contents) package-archive-version)
                (error "Package archive version %d is greater than %d - upgrade package.el"
                       (car contents) package-archive-version))
            (cdr contents))))))

(defun package-read-all-archive-contents ()
  "Read the archive description of each of the archives in `package-archives'."
  (dolist (archive package-archives)
    (package-read-archive-contents (car archive)))
  (let ((builtins (package--read-archive-file
                   (concat "archives/" (caar package-archives)
                           "/builtin-packages"))))
    (if builtins
        ;; Version 1 of 'builtin-packages' is a list where the car is
        ;; a split emacs version and the cdr is an alist suitable for
        ;; package--builtins.
        (let ((our-version (package-version-split emacs-version))
              (result package--builtins-base))
          (setq package--builtins
                (dolist (elt builtins result)
                  (if (package-version-compare our-version (car elt) '>=)
                      (setq result (append (cdr elt) result)))))))))

(defun package-read-archive-contents (archive)
  "Re-read `archive-contents' and `builtin-packages', for ARCHIVE if they exist.

Will set `package-archive-contents' and `package--builtins' if
successful. Will throw an error if the archive version is too
new."
  (let ((archive-contents (package--read-archive-file
                           (concat "archives/" archive
                                   "/archive-contents"))))
    (if archive-contents
        ;; Version 1 of 'archive-contents' is identical to our
        ;; internal representation.
        ;; TODO: merge archive lists
        (dolist (package archive-contents)
          (package--add-to-archive-contents package archive)))))

(defun package--add-to-archive-contents (package archive)
  "Add the PACKAGE from the given ARCHIVE if needed.

Adds the archive from which it came to the end of the package vector."
  (let* ((package-name (car package))
         (package-version (aref (cdr package) 0))
         (package-with-archive (cons (car package)
                                     (vconcat (cdr package) (vector archive))))
         (existing-package (cdr (assq package-name package-archive-contents))))
    (when (or (not existing-package)
              (package-version-compare package-version
                                       (aref existing-package 0) '>))
      (add-to-list 'package-archive-contents package-with-archive))))

(defun package-download-transaction (transaction)
  "Download and install all the packages in the given TRANSACTION."
  (mapc (lambda (elt)
          (let* ((desc (cdr (assq elt package-archive-contents)))
                 (v-string (package-version-join (package-desc-vers desc)))
                 (kind (package-desc-kind desc)))
            (cond
             ((eq kind 'tar)
              (package-download-tar elt v-string))
             ((eq kind 'single)
              (package-download-single elt v-string
                                       (package-desc-doc desc)
                                       (package-desc-reqs desc)))
             (t
              (error "Unknown package kind: " (symbol-name kind))))))
        transaction))

(defun package-install (name)
  "Install the package named NAME.
Interactively, prompts for the package name."
  (interactive
   (list (progn
           (intern (completing-read "Install package: "
                                    (mapcar (lambda (elt)
                                              (cons (symbol-name (car elt))
                                                    nil))
                                            package-archive-contents)
                                    nil t)))))
  (let ((pkg-desc (assq name package-archive-contents)))
    (unless pkg-desc
      (error "Package '%s' not available for installation"
             (symbol-name name)))
    (let ((transaction
           (package-compute-transaction (list name)
                                        (package-desc-reqs (cdr pkg-desc)))))
      (package-download-transaction transaction)))
  ;; Try to activate it.
  (package-initialize))

(defun package-strip-rcs-id (v-str)
  "Strip RCS version ID from the version string V-STR.

If the result looks like a dotted numeric version, return it.
Otherwise return nil."
  (if v-str
      (if (string-match "[ \t]*\\$\\(?:Revision\\|Id\\):[ \t]\\(?:[^ \t]+,v[ \t]+\\)?\\([0-9.]+\\).*\\$$" v-str)
          (match-string 1 v-str)
        (if (string-match "^[0-9.]*$" v-str)
            v-str))))

(defun package-buffer-info ()
  "Return a vector of information about the package in the current buffer.
The vector looks like [FILENAME REQUIRES DESCRIPTION VERSION COMMENTARY]
FILENAME is the file name, a string.  It does not have the \".el\" extension.
REQUIRES is a requires list, or nil.
DESCRIPTION is the package description (a string).
VERSION is the version, a string.
COMMENTARY is the commentary section, a string, or nil if none.
Throws an exception if the buffer does not contain a conforming package.
If there is a package, narrows the buffer to the file's boundaries.
May narrow buffer or move point even on failure."
  (goto-char (point-min))
  (if (re-search-forward "^;;; \\([^ ]*\\)\\.el --- \\(.*\\)$" nil t)
      (let ((file-name (match-string 1))
            (desc (match-string 2))
            (start (progn (beginning-of-line) (point))))
        (if (search-forward (concat ";;; " file-name ".el ends here"))
            (progn
              ;; Try to include a trailing newline.
              (forward-line)
              (narrow-to-region start (point))
              (require 'lisp-mnt)
              ;; Use some headers we've invented to drive the process.
              (let* ((requires-str (lm-header "package-requires"))
                     (requires (if requires-str
                                   (package-read-from-string requires-str)))
                     ;; Prefer Package-Version, because if it is
                     ;; defined the package author probably wants us
                     ;; to use it.  Otherwise try Version.
                     (pkg-version
                      (or (package-strip-rcs-id (lm-header "package-version"))
                          (package-strip-rcs-id (lm-header "version"))))
                     (commentary (lm-commentary)))
                (unless pkg-version
                  (error
                   "Package does not define a usable \"Version\" or \"Package-Version\" header"))
                ;; Turn string version numbers into list form.
                (setq requires
                      (mapcar
                       (lambda (elt)
                         (list (car elt)
                               (package-version-split (car (cdr elt)))))
                       requires))
                (set-text-properties 0 (length file-name) nil file-name)
                (set-text-properties 0 (length pkg-version) nil pkg-version)
                (set-text-properties 0 (length desc) nil desc)
                (vector file-name requires desc pkg-version commentary)))
          (error "Package missing a terminating comment")))
    (error "No starting comment for package")))

(defun package-tar-file-info (file)
  "Find package information for a tar file.
FILE is the name of the tar file to examine.
The return result is a vector like `package-buffer-info'."
  (setq file (expand-file-name file))
  (unless (string-match "^\\(.+\\)-\\([0-9.]+\\)\\.tar$" file)
    (error "`%s' doesn't have a package-ish name" file))
  (let* ((pkg-name (file-name-nondirectory (match-string-no-properties 1 file)))
         (pkg-version (match-string-no-properties 2 file))
         ;; Extract the package descriptor.
         (pkg-def-contents (shell-command-to-string
                            ;; Requires GNU tar.
                            (concat "tar -xOf " file " "
                                    pkg-name "-" pkg-version "/"
                                    pkg-name "-pkg.el")))
         (pkg-def-parsed (package-read-from-string pkg-def-contents)))
    (unless (eq (car pkg-def-parsed) 'define-package)
      (error "%s-pkg.el doesn't contain `define-package' sexp" pkg-name))
    (let ((name-str (nth 1 pkg-def-parsed))
          (version-string (nth 2 pkg-def-parsed))
          (docstring (nth 3 pkg-def-parsed))
          (requires (nth 4 pkg-def-parsed))

          (readme (shell-command-to-string
                   ;; Requires GNU tar.
                   (concat "tar -xOf " file " "
                           pkg-name "-" pkg-version "/README"))))
      (unless (equal pkg-version version-string)
        (error "Inconsistent versions!"))
      (unless (equal pkg-name name-str)
        (error "Inconsistent names!"))
      ;; Kind of a hack.
      (if (string-match ": Not found in archive" readme)
          (setq readme nil))
      ;; Turn string version numbers into list form.
      (if (eq (car requires) 'quote)
          (setq requires (car (cdr requires))))
      (setq requires
            (mapcar
             (lambda (elt)
               (list (car elt)
                     (package-version-split (car (cdr elt)))))
             requires))
      (vector pkg-name requires docstring version-string readme))))

(defun package-install-buffer-internal (pkg-info type)
  "Download and install a single package.

PKG-INFO describes the package to be installed.

TYPE is either `single' or `tar'."
  (save-excursion
    (save-restriction
      (let* ((file-name (aref pkg-info 0))
             (requires (aref pkg-info 1))
             (desc (if (string= (aref pkg-info 2) "")
                       "No description available."
                     (aref pkg-info 2)))
             (pkg-version (aref pkg-info 3)))
        ;; Download and install the dependencies.
        (let ((transaction (package-compute-transaction nil requires)))
          (package-download-transaction transaction))
        ;; Install the package itself.
        (cond
         ((eq type 'single)
          (package-unpack-single file-name pkg-version desc requires))
         ((eq type 'tar)
          (package-unpack (intern file-name) pkg-version))
         (t
          (error "Unknown type: %s" (symbol-name type))))
        ;; Try to activate it.
        (package-initialize)))))

(defun package-install-from-buffer ()
  "Install a package from the current buffer.
The package is assumed to be a single .el file which
follows the elisp comment guidelines; see
info node `(elisp)Library Headers'."
  (interactive)
  (package-install-buffer-internal (package-buffer-info) 'single))

(defun package-install-file (file)
  "Install a package from a FILE.
The file can either be a tar file or an Emacs Lisp file."
  (interactive "fPackage file name: ")
  (with-temp-buffer
    (insert-file-contents-literally file)
    (cond
     ((string-match "\\.el$" file) (package-install-from-buffer))
     ((string-match "\\.tar$" file)
      (package-install-buffer-internal (package-tar-file-info file) 'tar))
     (t (error "Unrecognized extension `%s'" (file-name-extension file))))))

(defun package-delete (name version)
  "Delete package NAME at VERSION."
  (require 'dired)          ; for dired-delete-file
  (dired-delete-file (concat (file-name-as-directory package-user-dir)
                             name "-" version)
                     ;; FIXME: query user?
                     'always))

(defun package--encode (string)
  "Encode a STRING by replacing some characters with XML entities."
  ;; We need a special case for translating "&" to "&amp;".
  (let ((index))
    (while (setq index (string-match "[&]" string index))
      (setq string (replace-match "&amp;" t nil string))
      (setq index (1+ index))))
  (while (string-match "[<]" string)
    (setq string (replace-match "&lt;" t nil string)))
  (while (string-match "[>]" string)
    (setq string (replace-match "&gt;" t nil string)))
  (while (string-match "[']" string)
    (setq string (replace-match "&apos;" t nil string)))
  (while (string-match "[\"]" string)
    (setq string (replace-match "&quot;" t nil string)))
  string)

(defun package--update-file (file location text)
  "Update FILE by finding LOCATION and inserting TEXT."
  (save-excursion
    (let ((old-buffer (find-buffer-visiting file)))
      (with-current-buffer (let ((find-file-visit-truename t))
                             (or old-buffer (find-file-noselect file)))
        (goto-char (point-min))
        (search-forward location)
        (forward-line)
        (insert text)
        (let ((file-precious-flag t))
          (save-buffer))
        (unless old-buffer
          (kill-buffer (current-buffer)))))))

(defun package-archive-for (name)
  "Return the archive containing the package NAME."
  (let ((desc (cdr (assq (intern-soft name) package-archive-contents))))
    (cdr (assoc (aref desc (- (length desc) 1)) package-archives))))

(defun package--download-one-archive (archive file)
  "Download a single archive file and cache it locally.

Downloads the archive index from ARCHIVE and stores it in FILE."
  (let* ((archive-name (car archive))
         (archive-url (cdr archive))
         (buffer (url-retrieve-synchronously (concat archive-url file))))
    (save-excursion
      (set-buffer buffer)
      (package-handle-response)
      (re-search-forward "^$" nil 'move)
      (forward-char)
      (delete-region (point-min) (point))
      (make-directory (concat (file-name-as-directory package-user-dir)
                              "archives/" archive-name) t)
      (setq buffer-file-name (concat (file-name-as-directory package-user-dir)
                                     "archives/" archive-name "/" file))
      (let ((version-control 'never))
        (save-buffer))
      (kill-buffer buffer))))

(defun package-refresh-contents ()
  "Download the ELPA archive description if needed.
Invoking this will ensure that Emacs knows about the latest versions
of all packages.  This will let Emacs make them available for
download."
  (interactive)
  (dolist (archive package-archives)
    (package--download-one-archive archive "archive-contents"))
  (package-read-all-archive-contents))

(defun package-initialize ()
  "Load all packages and activate as many as possible."
  (setq package-obsolete-alist nil)
  (package-load-all-descriptors)
  (package-read-all-archive-contents)
  ;; Try to activate all our packages.
  (mapc (lambda (elt)
          (package-activate (car elt) (package-desc-vers (cdr elt))))
        package-alist))



;;;; Package menu mode.

(defvar package-menu-mode-map
  (let ((map (make-keymap))
	(menu-map (make-sparse-keymap "Package")))
    (suppress-keymap map)
    (define-key map "q" 'quit-window)
    (define-key map "n" 'next-line)
    (define-key map "p" 'previous-line)
    (define-key map "u" 'package-menu-mark-unmark)
    (define-key map "\177" 'package-menu-backup-unmark)
    (define-key map "d" 'package-menu-mark-delete)
    (define-key map "i" 'package-menu-mark-install)
    (define-key map "g" 'package-menu-revert)
    (define-key map "r" 'package-menu-refresh)
    (define-key map "~" 'package-menu-mark-obsolete-for-deletion)
    (define-key map "x" 'package-menu-execute)
    (define-key map "h" 'package-menu-quick-help)
    (define-key map "?" 'package-menu-view-commentary)
    (define-key map [menu-bar package-menu] (cons "Package" menu-map))
    (define-key menu-map [mq]
      '(menu-item "Quit" quit-window
		  :help "Quit package selection"))
    (define-key menu-map [s1] '("--"))
    (define-key menu-map [mn]
      '(menu-item "Next" next-line
		  :help "Next Line"))
    (define-key menu-map [mp]
      '(menu-item "Previous" previous-line
		  :help "Previous Line"))
    (define-key menu-map [s2] '("--"))
    (define-key menu-map [mu]
      '(menu-item "Unmark" package-menu-mark-unmark
		  :help "Clear any marks on a package and move to the next line"))
    (define-key menu-map [munm]
      '(menu-item "Unmark backwards" package-menu-backup-unmark
		  :help "Back up one line and clear any marks on that package"))
    (define-key menu-map [md]
      '(menu-item "Mark for deletion" package-menu-mark-delete
		  :help "Mark a package for deletion and move to the next line"))
    (define-key menu-map [mi]
      '(menu-item "Mark for install" package-menu-mark-install
		  :help "Mark a package for installation and move to the next line"))
    (define-key menu-map [s3] '("--"))
    (define-key menu-map [mg]
      '(menu-item "Update package list" package-menu-revert
		  :help "Update the list of packages"))
    (define-key menu-map [mr]
      '(menu-item "Refresh package list" package-menu-refresh
		  :help "Download the ELPA archive"))
    (define-key menu-map [s4] '("--"))
    (define-key menu-map [mt]
      '(menu-item "Mark obsolete packages" package-menu-mark-obsolete-for-deletion
		  :help "Mark all obsolete packages for deletion"))
    (define-key menu-map [mx]
      '(menu-item "Execute actions" package-menu-execute
		  :help "Perform all the marked actions"))
    (define-key menu-map [s5] '("--"))
    (define-key menu-map [mh]
      '(menu-item "Help" package-menu-quick-help
		  :help "Show short key binding help for package-menu-mode"))
    (define-key menu-map [mc]
      '(menu-item "View Commentary" package-menu-view-commentary
		  :help "Display information about this package"))
    map)
   "Local keymap for `package-menu-mode' buffers.")

(defvar package-menu-sort-button-map
  (let ((map (make-sparse-keymap)))
    (define-key map [header-line mouse-1] 'package-menu-sort-by-column)
    (define-key map [follow-link] 'mouse-face)
    map)
  "Local keymap for package menu sort buttons.")

(put 'package-menu-mode 'mode-class 'special)

(defun package-menu-mode ()
  "Major mode for browsing a list of packages.
Letters do not insert themselves; instead, they are commands.
\\<package-menu-mode-map>
\\{package-menu-mode-map}"
  (kill-all-local-variables)
  (use-local-map package-menu-mode-map)
  (setq major-mode 'package-menu-mode)
  (setq mode-name "Package Menu")
  (setq truncate-lines t)
  (setq buffer-read-only t)
  ;; Support Emacs 21.
  (if (fboundp 'run-mode-hooks)
      (run-mode-hooks 'package-menu-mode-hook)
    (run-hooks 'package-menu-mode-hook)))

(defun package-menu-refresh ()
  "Download the ELPA archive.
This fetches the file describing the current contents of
the Emacs Lisp Package Archive, and then refreshes the
package menu.  This lets you see what new packages are
available for download."
  (interactive)
  (package-refresh-contents)
  (package-list-packages-internal))

(defun package-menu-revert ()
  "Update the list of packages."
  (interactive)
  (package-list-packages-internal))

(defun package-menu-mark-internal (what)
  "Internal function to mark a package.

WHAT is the character used to mark the line."
  (unless (eobp)
    (let ((buffer-read-only nil))
      (beginning-of-line)
      (delete-char 1)
      (insert what)
      (forward-line))))

(defun package-menu-mark-delete (&optional arg)
  "Mark a package for deletion and move to the next line.

ARG is a (currently unused) numeric argument."
  (interactive "p")
  (package-menu-mark-internal "D"))

(defun package-menu-mark-install (&optional arg)
  "Mark a package for installation and move to the next line.

ARG is a (currently unused) numeric argument."
  (interactive "p")
  (package-menu-mark-internal "I"))

(defun package-menu-mark-unmark (&optional arg)
  "Clear any marks on a package and move to the next line.

ARG is a (currently unused) numeric argument."
  (interactive "p")
  (package-menu-mark-internal " "))

(defun package-menu-backup-unmark ()
  "Back up one line and clear any marks on that package."
  (interactive)
  (forward-line -1)
  (package-menu-mark-internal " ")
  (forward-line -1))

(defun package-menu-mark-obsolete-for-deletion ()
  "Mark all obsolete packages for deletion."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (forward-line 2)
    (while (not (eobp))
      (if (looking-at ".*\\s obsolete\\s ")
          (package-menu-mark-internal "D")
        (forward-line 1)))))

(defun package-menu-quick-help ()
  "Show short key binding help for `package-menu-mode'."
  (interactive)
  (message "n-ext, i-nstall, d-elete, u-nmark, x-ecute, r-efresh, h-elp ?-view commentary"))

(defun package-menu-view-commentary ()
  "Display information about this package.
For single-file packages, shows the commentary section from the header.
For larger packages, shows the README file."
  (interactive)
  (let* (start-point ok
                     (pkg-name (package-menu-get-package))
                     (buffer (url-retrieve-synchronously
                              (concat (package-archive-for pkg-name)
                                      pkg-name "-readme.txt"))))
    (with-current-buffer buffer
      ;; FIXME: it would be nice to work with any URL type.
      (setq start-point url-http-end-of-headers)
      (setq ok (eq (url-http-parse-response) 200)))
    (let ((new-buffer (get-buffer-create "*Package Info*")))
      (with-current-buffer new-buffer
        (let ((buffer-read-only nil))
          (erase-buffer)
          (insert "Package information for " pkg-name "\n\n")
          (if ok
              (insert-buffer-substring buffer start-point)
            (insert "This package does not have a README file or commentary comment.\n"))
          (goto-char (point-min))
          (view-mode)))
      (display-buffer new-buffer t))))

(defun package-menu-get-package ()
  "Return the name of the package on the current line."
  (save-excursion
    (beginning-of-line)
    (if (looking-at ". \\([^ \t]*\\)")
        (match-string-no-properties 1))))

(defun package-menu-get-version ()
  "Return the version of the package on the current line."
  (save-excursion
    (beginning-of-line)
    (if (looking-at ". [^ \t]*[ \t]*\\([0-9.]*\\)")
        (match-string 1))))

(defun package-menu-get-status ()
  "Get the status of the current line."
  (save-excursion
    (if (looking-at ". [^ \t]*[ \t]*[^ \t]*[ \t]*\\([^ \t]*\\)")
        (match-string 1)
      "")))

(defun package-menu-execute ()
  "Perform all the marked actions.
Packages marked for installation will be downloaded and
installed.  Packages marked for deletion will be removed.
Note that after installing packages you will want to restart
Emacs."
  (interactive)
  (goto-char (point-min))
  (forward-line 2)
  (while (not (eobp))
    (let ((cmd (char-after))
          (pkg-name (package-menu-get-package))
          (pkg-vers (package-menu-get-version))
          (pkg-status (package-menu-get-status)))
      (cond
       ((eq cmd ?D)
        (when (and (string= pkg-status "installed")
                   (string= pkg-name "package"))
          ;; FIXME: actually, we could be tricky and remove all info.
          ;; But that is drastic and the user can do that instead.
          (error "Can't delete most recent version of `package'"))
        ;; Ask for confirmation here?  Maybe if package status is ""?
        ;; Or if any lisp from package is actually loaded?
        (message "Deleting %s-%s..." pkg-name pkg-vers)
        (package-delete pkg-name pkg-vers)
        (message "Deleting %s-%s... done" pkg-name pkg-vers))
       ((eq cmd ?I)
        (package-install (intern pkg-name)))))
    (forward-line))
  (package-menu-revert))

(defun package-print-package (package version key desc)
  "Print out a single PACKAGE line for the menu buffer.

PACKAGE is the package name as a symbol.

VERSION is the version as an integer vector.

KEY is the installation status of the package; either
\"available\" or \"installed\".

DESC is the short description of the package."
  (let ((face
         (cond ((eq package 'emacs) 'font-lock-builtin-face)
               ((string= key "available") 'default)
               ((string= key "installed") 'font-lock-comment-face)
               (t ; obsolete, but also the default.
                                        ; is warning ok?
                'font-lock-warning-face))))
    (insert (propertize "  " 'font-lock-face face))
    (insert (propertize (symbol-name package) 'font-lock-face face))
    (indent-to 20 1)
    (insert (propertize (package-version-join version) 'font-lock-face face))
    (indent-to 30 1)
    (insert (propertize key 'font-lock-face face))
    ;; FIXME: this 'when' is bogus...
    (when desc
      (indent-to 41 1)
      (insert (propertize desc 'font-lock-face face)))
    (insert "\n")))

(defun package-list-maybe-add (package version status description result)
  "Add PACKAGE to the list if it is not already there.

PACKAGE is the package name as a symbol.

VERSION is the package version, as an integer vector.

STATUS is the installation status of the package, either
\"available\" or \"installed\".

DESCRIPTION is the short description of the package.

RESULT is the list to which to add the package."
  (let ((elt (assoc (cons package version) result)))
    (unless elt
      (setq result (cons (list (cons package version) status description)
                         result))))
  result)

;; This decides how we should sort; nil means by package name.
(defvar package-menu-sort-key nil)

(defun package-list-packages-internal ()
  "List the available and installed packages."
  (package-initialize)          ; FIXME: do this here?
  (with-current-buffer (get-buffer-create "*Packages*")
    (setq buffer-read-only nil)
    (erase-buffer)
    (let ((info-list))
      (mapc (lambda (elt)
              (setq info-list
                    (package-list-maybe-add (car elt)
                                            (package-desc-vers (cdr elt))
                                            ;; FIXME: it turns out to
                                            ;; be tricky to see if
                                            ;; this package is
                                            ;; presently activated.
                                            ;; That is lame!
                                            "installed"
                                            (package-desc-doc (cdr elt))
                                            info-list)))
            package-alist)
      (mapc (lambda (elt)
              (setq info-list
                    (package-list-maybe-add (car elt)
                                            (package-desc-vers (cdr elt))
                                            "available"
                                            (package-desc-doc (cdr elt))
                                            info-list)))
            package-archive-contents)
      (mapc (lambda (elt)
              (mapc (lambda (inner-elt)
                      (setq info-list
                            (package-list-maybe-add (car elt)
                                                    (package-desc-vers
                                                     (cdr inner-elt))
                                                    "obsolete"
                                                    (package-desc-doc
                                                     (cdr inner-elt))
                                                    info-list)))
                    (cdr elt)))
            package-obsolete-alist)
      (let ((selector (cond
                       ((string= package-menu-sort-key "Version")
                        ;; FIXME this doesn't work.
                        #'(lambda (e) (cdr (car e))))
                       ((string= package-menu-sort-key "Status")
                        #'(lambda (e) (car (cdr e))))
                       ((string= package-menu-sort-key "Description")
                        #'(lambda (e) (car (cdr (cdr e)))))
                       (t ; "Package" is default.
                        #'(lambda (e) (symbol-name (car (car e))))))))
        (setq info-list
              (sort info-list
                    (lambda (left right)
                      (let ((vleft (funcall selector left))
                            (vright (funcall selector right)))
                        (string< vleft vright))))))
      (mapc (lambda (elt)
              (package-print-package (car (car elt))
                                     (cdr (car elt))
                                     (car (cdr elt))
                                     (car (cdr (cdr elt)))))
            info-list))
    (goto-char (point-min))
    (current-buffer)))

(defun package-menu-sort-by-column (&optional e)
  "Sort the package menu by the last column clicked, E."
  (interactive (list last-input-event))
  (if e (mouse-select-window e))
  (let* ((pos (event-start e))
         (obj (posn-object pos))
         (col (if obj
                  (get-text-property (cdr obj) 'column-name (car obj))
                (get-text-property (posn-point pos) 'column-name))))
    (setq package-menu-sort-key col))
  (package-list-packages-internal))

(defun package--list-packages ()
  "Display a list of packages.
Helper function that does all the work for the user-facing functions."
  (with-current-buffer (package-list-packages-internal)
    (package-menu-mode)
    ;; Set up the header line.
    (setq header-line-format
          (mapconcat
           (lambda (pair)
             (let ((column (car pair))
                   (name (cdr pair)))
               (concat
                ;; Insert a space that aligns the button properly.
                (propertize " " 'display (list 'space :align-to column)
                            'face 'fixed-pitch)
                ;; Set up the column button.
                (if (string= name "Version")
                    name
                  (propertize name
                              'column-name name
                              'help-echo "mouse-1: sort by column"
                              'mouse-face 'highlight
                              'keymap package-menu-sort-button-map)))))
           ;; We take a trick from buff-menu and have a dummy leading
           ;; space to align the header line with the beginning of the
           ;; text.  This doesn't really work properly on Emacs 21,
           ;; but it is close enough.
           '((0 . "")
             (2 . "Package")
             (20 . "Version")
             (30 . "Status")
             (41 . "Description"))
           ""))

    ;; It's okay to use pop-to-buffer here.  The package menu buffer
    ;; has keybindings, and the user just typed 'M-x
    ;; package-list-packages', suggesting that they might want to use
    ;; them.
    (pop-to-buffer (current-buffer))))

(defun package-list-packages ()
  "Display a list of packages.
Fetches the updated list of packages before displaying.
The list is displayed in a buffer named `*Packages*'."
  (interactive)
  (package-refresh-contents)
  (package--list-packages))

(defun package-list-packages-no-fetch ()
  "Display a list of packages.
Does not fetch the updated list of packages before displaying.
The list is displayed in a buffer named `*Packages*'."
  (interactive)
  (package--list-packages))

;; Make it appear on the menu.
(define-key-after menu-bar-options-menu [package]
  '(menu-item "Manage Packages" package-list-packages
              :help "Install or uninstall additional Emacs packages"))

(provide 'package)
;;; package.el ends here
