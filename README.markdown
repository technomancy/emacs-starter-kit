# Emacs Starter Kit

The Starter Kit should provide a saner set of defaults than you get
normally with Emacs. It was originally intended for beginners, but it
should provide a reasonable working environment for anyone using Emacs
for dynamic languages.

The latest version is at http://github.com/technomancy/emacs-starter-kit

## Learning

This won't teach you Emacs, but it'll make it easier to get
comfortable. To access the tutorial, press control-h followed by t.

You may also find the [PeepCode Meet Emacs
screencast](http://peepcode.com/products/meet-emacs) helpful. The
[Emacs Wiki](http://emacswiki.org) is also very handy.

## Installation

You'll need Emacs 24, which comes with package.el.

Add the "technomancy" archive source:

    (require 'package)
    (add-to-list 'package-archives
                 '("technomancy" . "http://repo.technomancy.us/emacs/") t)

Then you can install it:

    M-x package-list-packages

Move to the "starter-kit" line and press "i" to mark it for
installation along with any other packages you desire. Press "x" to
perform the installation.

Improved support for various languages are packaged separately.

* Javascript
* Ruby
* Perl
* Lisp (including Emacs Lisp, Clojure, Scheme, and Common Lisp)

The Starter Kit used to be a git repository that you checked out and
used as your own personal .emacs.d directory, but it's been
restructured so that it can be treated like any other package, freeing
you up to structure your .emacs.d directory as you wish.

## FAQ

**Q:** When I try to create a new file or buffer, the autocompletion is eager and tries to use the name of an existing file or buffer.  
**A:** That's called +ido-mode+, and it's awesome! But sometimes it
  gets in the way. To temporarily disable it, press C-f while the
  prompt is open. You can also press C-j while it's still enabled to
  force the creation of the name.

**Q:** When I'm writing Javascript, all my functions show up as ƒ. Am I going insane?  
**A:** That's actually a render-time hack. The file on disk shows as
  "function", but it's just rendered using the script F in order to
  tone down the verbosity inherent in the language a bit.

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
  may find [the Paredit cheat 
  sheet](http://www.emacswiki.org/emacs/PareditCheatsheet) helpful.

**Q:** How awesome is Emacs?  
**A:** So awesome.

## Upgrading

Users of the old version of the Starter Kit should be able to upgrade
with little fuss; the main difference is that the new one doesn't pull
in a bunch of other package.el dependencies; users may pick and choose
which they want. It's also more modular, so language-specific starter
kit modules must be installed separately. User-specific and
host-specific files are still honored.

## Copyright

Copyright (C) 2008-2011 Phil Hagelberg and contributors

Files are licensed under the same license as Emacs unless otherwise
specified. See the file COPYING for details.
