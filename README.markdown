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
than putting up in a big pile of code, the Starter Kit has shifted to
become merely a guide. As an Emacs user, you're going to have to get
comfortable seeking out new pieces of elisp, bringing them in,
configuring them, and eventually writing your own. The Starter Kit can
help this process by giving hints as to where to start and what to
look for, but constructing a solid configuration is ultimately a
personal journey for which you must take the first steps.

## Getting Started

If you do nothing else, start by adding
[Marmalade](https://marmalade-repo.org/) to your packages list and
installing the
[better-defaults package](https://github.com/technomancy/better-defaults).
Place this in your `~/.emacs.d/init.el` file:

```lisp
(require 'package)
(add-to-list 'package-archives '("marmalade" . "https://marmalade-repo.org/packages/"))
```

Evaluate it with `M-x eval-buffer` and run `M-x package-install [RET] better-defaults`.

### If you liked ... you might also like

These are all libraries that focus on doing one thing and do it consistently well.

* [magit](https://magit.vc) use git without being driven insane.
* [smex](https://github.com/nonsequitur/smex) for getting ido-style feedback in M-x.
* [ido-ubiquitous](https://github.com/DarwinAwardWinner/ido-ubiquitous) for geting ido goodness everywhere else.
* [paredit](http://www.emacswiki.org/emacs/ParEdit) keeps parentheses under control.
* [idle-highlight-mode](https://github.com/nonsequitur/idle-highlight-mode) for seeing everywhere else an identifier is used at a glance.
* [find-file-in-project](https://github.com/technomancy/find-file-in-project) quick project-scoped navigation
* [scpaste](http://p.hagelb.org) the pastebin of champions.

You can drop some code into `init.el` to install these automatically when they are missing:

```lisp
(defvar my-packages '(better-defaults paredit idle-highlight-mode ido-ubiquitous
                                      find-file-in-project magit smex scpaste))

(package-initialize)
(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))
```

## Copyright

Copyright © 2008-2014 Phil Hagelberg and contributors

Files are licensed under the same license as Emacs unless otherwise
specified. See the file COPYING for details.

