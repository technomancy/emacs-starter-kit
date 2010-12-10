;;; starter-kit.el --- Saner defaults and goodies.
;;
;; Copyright (c) 2008-2010 Phil Hagelberg and contributors
;;
;; Author: Phil Hagelberg <technomancy@gmail.com>
;; URL: http://www.emacswiki.org/cgi-bin/wiki/StarterKit
;; Version: 2.0
;; Keywords: convenience

;; This file is not part of GNU Emacs.

;;; Commentary:

;; "Emacs outshines all other editing software in approximately the
;; same way that the noonday sun does the stars. It is not just bigger
;; and brighter; it simply makes everything else vanish."
;; -Neal Stephenson, "In the Beginning was the Command Line"

;; This file just brings together other pieces of the starter kit plus
;; user- and host-specific configs.

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

;; Turn off mouse interface early in startup to avoid momentary display
;; You really don't need these; trust me.
(dolist (mode '(menu-bar-mode tool-bar-mode scroll-bar-mode))
  (if (fboundp mode) (funcall mode -1)))

(dolist (l '(uniquify starter-kit-defuns starter-kit-bindings
                      starter-kit-misc starter-kit-eshell))
  (require l))

;; You can keep system- or user-specific customizations here
(setq esk-dotfiles-dir (file-name-directory
                        (or (buffer-file-name) load-file-name))
      esk-system-specific-config (concat esk-dotfiles-dir system-name ".el")
      esk-user-specific-config (concat esk-dotfiles-dir user-login-name ".el")
      esk-user-specific-dir (concat esk-dotfiles-dir user-login-name))

(add-to-list 'load-path esk-user-specific-dir)

(if (file-exists-p esk-system-specific-config)
    (load esk-system-specific-config))

(if (file-exists-p esk-user-specific-config)
    (load esk-user-specific-config))

(if (file-exists-p esk-user-specific-dir)
  (mapc #'load (directory-files esk-user-specific-dir nil ".*el$")))

(provide 'starter-kit)
;;; starter-kit.el ends here
