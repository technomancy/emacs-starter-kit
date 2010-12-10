# Emacs Starter Kit

The Starter Kit should provide a saner set of defaults than you get
normally with Emacs. It was originally intended for beginners, but it
should provide a reasonable working environment for anyone using Emacs
for dynamic languages. It also bundles a number of useful libraries
that are not distributed with Emacs for various reasons.

The latest version is at http://github.com/technomancy/emacs-starter-kit/

## Learning

This won't teach you Emacs, but it'll make it easier to get
comfortable. To access the tutorial, press control-h followed by t.

You may also find the [PeepCode Meet Emacs
screencast](http://peepcode.com/products/meet-emacs) helpful. The
[Emacs Wiki](http://emacswiki.org) is also very handy.

## Installation

You'll need Emacs 24, which comes with package.el.

Add the "technomancy" archive source:

    (add-to-list 'package-archives
                 '("technomancy" . "http://repo.technomancy.us/emacs/") t)

Then you can install it:

    M-x package-install RET starter-kit RET

## Upgrading

Users of the old version of the Starter Kit should be able to upgrade
with little fuss; the main difference is that the new one doesn't pull
in a bunch of other package.el dependencies; users may pick and choose
which they want. It's also more modular, so language-specific starter
kit modules must be installed separately. User-specific and
host-specific files are still honored.

## Copyright

Copyright (C) 2008-2010 Phil Hagelberg and contributors

Files are licensed under the same license as Emacs unless otherwise
specified. See the file COPYING for details.
