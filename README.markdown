# Emacs Starter Kit

The Starter Kit provides a more pleasant set of defaults than you get
normally with Emacs. It was originally intended for beginners, but it
offers a nicely augmented working environment for anyone using Emacs.

The latest release is at http://marmalade-repo.org/packages/starter-kit
with the source at http://github.com/technomancy/emacs-starter-kit

## Learning

This won't teach you Emacs, but it'll make it easier to get
comfortable. To access the tutorial, press control-h followed by t.

You may also find the commercial [PeepCode Meet Emacs
screencast](http://peepcode.com/products/meet-emacs) helpful. The
[Emacs Wiki](http://emacswiki.org) is also very handy.

## Installation

You'll need Emacs 24, which comes with package.el. It's not hard to
compile [from source](http://github.com/emacsmirror/emacs), but
precompiled versions are readily available for
[Debian-based systems](http://emacs.naquadah.org/),
[Mac OS X](http://emacsformacosx.com/builds), and
[Windows](http://code.google.com/p/emacs-for-windows/updates/list).

If you need to maintain compatibility with Emacs 23 or 22, you need to
use [version 1](https://github.com/technomancy/emacs-starter-kit/tree/master).

Add Marmalade as a package archive source in ~/.emacs.d/init.el:

```Lisp
(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(package-initialize)
```

Evaluate that code by hitting `M-x eval-buffer` in init.el, then you
can install it:

    M-x package-refresh-contents
    M-x package-install RET starter-kit RET

Other modules are also available:

* starter-kit-bindings (spun out due to concerns about keybinding conventions)
* starter-kit-eshell
* starter-kit-js
* starter-kit-ruby
* starter-kit-perl
* starter-kit-lisp (enhances Emacs Lisp, Clojure, Scheme, and Common Lisp)

These modules are installed separately from the base Starter Kit package.

It's recommended to create a list of packages in init.el which will be
installed if they are found to not be present:

```Lisp
(when (not package-archive-contents)
  (package-refresh-contents))

;; Add in your own as you wish:
(defvar my-packages '(starter-kit starter-kit-lisp starter-kit-bindings)
  "A list of packages to ensure are installed at launch.")

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))
```
That way you can be ensured of a consistent experience across machines.

There are a few conventions for naming files which will get loaded
automatically. ~/.emacs.d/$USER.el as well as any files in the
~/.emacs.d/$USER/ directory. Finally, the Starter Kit will look for a
file named after the current hostname ending in ".el" which will allow
host-specific configuration.

The Starter Kit used to be a git repository that you checked out and
used as your own personal .emacs.d directory, but it's been
restructured so that it can be treated like any other package, freeing
you up to structure your .emacs.d directory as you wish. See
"Upgrading" below.

## FAQ

**Q:** When I try to create a new file or buffer, the autocompletion is eager and tries to use the name of an existing file or buffer.  
**A:** That's called <tt>ido-mode</tt>, and it's awesome! But sometimes it
  gets in the way. To temporarily disable it, press C-f while the
  prompt is open. You can also press C-j while it's still enabled to
  force the creation of the name.

**Q:** When I'm writing Javascript, all my functions show up as ƒ. Am I going insane?  
**A:** That's actually a render-time hack. The file on disk shows as
  "function", but it's just rendered using the script F in order to
  tone down the verbosity inherent in the language a bit. The same
  happens with <tt>fn</tt> in Clojure and <tt>lambda</tt> to λ in
  other lisps.

**Q:** I can't delete parentheses in Lisp!  
**A:** To be specific, you can't delete parenthesis if deleting the
  parentheses would result in invalid structure. That's called
  Paredit, and once you get used to it, you'll wonder how you ever did
  anything without it. But it can be disorienting at first. When
  paredit tries to stop you from deleting something, you're probably
  trying to delete something you shouldn't. Use C-k to kill whole
  expressions. Two things to remember: you can always use C-w to kill
  a region regardless of Paredit's rules, and you can always insert a
  single character like a close-paren by prefixing it with C-q. You
  may find
  [the Paredit cheat sheet](http://www.emacswiki.org/emacs/PareditCheatsheet)
  or
  [this Paredit walkthrough](https://github.com/technomancy/paredit-screencast/blob/master/outline.markdown)
  helpful. You can also enable paredit for non-lisp modes using the
  <tt>esk-paredit-nonlisp</tt> function.

**Q:** How awesome is Emacs?  
**A:** So awesome.

## Upgrading

Users of the old version of the Starter Kit (version 1) should be able
to upgrade easily. Move your old Starter Kit checkout at
<tt>~/.emacs.d</tt> out of the way and create a new directory
containing <tt>init.el</tt> with the lines above. Copy your
<tt>username.el</tt> and <tt>username</tt> directory from your old
checkout into the new <tt>~/.emacs.d</tt>. You should be able to check
this new directory into your main dotfiles repository instead of
keeping it separate.

The main difference in version 2 is that the new one doesn't pull in a
bunch of other package.el dependencies; users may pick and choose
which they want, including language-specific modules. Read
[more about the differences with version 1](http://technomancy.us/153).

## Copyright

Copyright © 2008-2011 Phil Hagelberg and contributors

Files are licensed under the same license as Emacs unless otherwise
specified. See the file COPYING for details.
