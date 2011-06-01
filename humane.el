;; best appearance in Mac OS X 
(when window-system
  (menu-bar-mode 1)
  (setq frame-title-format 't)
  (setq ns-pop-up-frames 'nil))

;; PATH
(add-to-list 'exec-path "/usr/local/bin")
(add-to-list 'exec-path "/usr/local/texlive/2010/bin/universal-darwin")
(setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin:/usr/local/texlive/2010/bin/universal-darwin"))

;; Install RVM
(nconc starter-kit-packages (list 'rvm))
(starter-kit-elpa-install)

;; LaTeX (AucTeX) options
(defun skim-make-displayline-command ()
  (concat
   "/Applications/Skim.app/Contents/SharedSupport/displayline "
   (TeX-current-line)
   " "
   (expand-file-name (funcall file (TeX-output-extension) t)
                    (file-name-directory (TeX-master-file)))
   " "
   (buffer-file-name)))
(defun skim-make-revert-command ()
  (concat
   "osascript -e 'tell application \"Skim\" to revert document \""
   (TeX-master-file (TeX-output-extension))
   "\"'"))
(defun skim-make-command ()
  (concat (skim-make-revert-command) "; " (skim-make-displayline-command)))
(add-hook 'LaTeX-mode-hook (lambda () (add-to-list 'TeX-expand-list '("%q" skim-make-command)))) 
(setq TeX-view-program-list '(("Skim" "%q")))
(setq TeX-view-program-selection '((output-pdf "Skim")))
