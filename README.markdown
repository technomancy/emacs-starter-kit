# Emacs Starter Kit

Version 3 of the Emacs Starter Kit is implemented as a prose guide to
various packages and settings which can greatly improve the Emacs
experience.

## History

Older versions of the Starter Kit attempted to be one-size-fits-all
codebase intended to be dropped into your `~/.emacs.d` directory
wholesale. While this proved very popular, taking a big bundle of
unrelated functionality leads to simply adopting things without
developing any real understanding. When something goes wrong or even
just behaves differently from what you'd like, you don't know where to
look to fix it.

I've since come to realize users are better suited by small, focused
packages which provide specific pieces of new functionality. So rather
than putting up a big pile of code, the Starter Kit has shifted to
become merely a guide. As an Emacs user, you're going to have to get
comfortable seeking out new pieces of elisp, bringing them in,
configuring them, and eventually writing your own. The Starter Kit can
help this process by giving hints as to where to start and what to
look for, but constructing a solid configuration is ultimately a
personal journey for which you must take the first steps.

## Getting Started

I strongly recommend keeping your dotfiles under version control and
installing Emacs packages inside it using [git subtree](https://blogs.atlassian.com/2013/05/alternatives-to-git-submodule-git-subtree/).

This avoids many [common](https://glyph.twistedmatrix.com/2015/11/editor-malware.html)
[problems](https://github.com/melpa/melpa/issues/2342) with
`package.el`, and it allows you to seamlessly roll back upgrades which
introduce breaking changes as well as guaranteeing that all machines you
work on share the same versions.

```lisp
(defun pnh-reinit-libs ()
  (interactive)
  (let ((generated-autoload-file (concat user-emacs-directory "my-autoload.el")))
    (dolist (d (directory-files (concat user-emacs-directory "lib") t "^[^\.]"))
      (dolist (f (directory-files d t "\\.el$"))
        (byte-compile-file f))
      (update-directory-autoloads d))))

(dolist (l (directory-files (concat user-emacs-directory "lib") nil "^[^\.]"))
  (add-to-list 'load-path (concat user-emacs-directory "lib/" l))
  (autoload (intern l) (concat l ".el")))

(when (not (file-exists-p (concat user-emacs-directory "my-autoload.el")))
  (pnh-reinit-libs))

(load (concat user-emacs-directory "my-autoload.el"))
```

### If you liked ... you might also like

These are all libraries that focus on doing one thing and do it consistently well.

* [better-defaults](https://github.com/technomancy/better-defaults) fix bad built-in behavior.
* [magit](https://magit.vc) use git without being driven insane.
* [smex](https://github.com/nonsequitur/smex) for getting ido-style feedback in M-x.
* [ido-ubiquitous](https://github.com/DarwinAwardWinner/ido-ubiquitous) for geting ido goodness everywhere else.
* [paredit](http://www.emacswiki.org/emacs/ParEdit) keeps parentheses under control.
* [idle-highlight-mode](https://github.com/nonsequitur/idle-highlight-mode) for seeing everywhere else an identifier is used at a glance.
* [find-file-in-project](https://github.com/technomancy/find-file-in-project) quick project-scoped navigation
* [scpaste](http://p.hagelb.org) the pastebin of champions.
* [elisp-slime-nav](https://github.com/purcell/elisp-slime-nav) nicer navigation for emacs lisp code.
* [exwm](https://github.com/ch11ng/exwm) a tiling window manager for X.

## Copyright

Copyright © 2008-2017 Phil Hagelberg and contributors

Files are licensed under the same license as Emacs unless otherwise
specified. See the file COPYING for details.

