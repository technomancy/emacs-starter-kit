# Emacs Starter Kit

The Starter Kit should provide a saner set of defaults than you get
normally with Emacs. It was originally intended for beginners, but it
should provide a reasonable working environment for anyone using Emacs
for dynamic languages. It also bundles a number of useful libraries
that are not distributed with Emacs for various reasons.

The latest version is at http://github.com/technomancy/emacs-starter-kit/

This version of the Starter Kit acts as a set of config files you drop
into your home directory. Version 2 (the v2 branch) is a set of
packages you can pull in using package.el instead now that it's been
included in Emacs.

## Learning

This won't teach you Emacs, but it'll make it easier to get
comfortable. To access the tutorial, press control-h followed by t.

You may also find the [PeepCode Meet Emacs
screencast](http://peepcode.com/products/meet-emacs) helpful. The
[Emacs Wiki](http://emacswiki.org) is also very handy.

## Installation

1. Install GNU Emacs (at least version 22, 23 is preferred)
   Use your package manager if you have one.
   Otherwise Mac users may get [some prebuilt binaries](http://emacsformacosx.com/), and
   Windows users can get them [from GNU](http://ftp.gnu.org/pub/gnu/emacs/windows/emacs-23.1-bin-i386.zip).
2. Move the directory containing this file to ~/.emacs.d
   (If you already have a directory at ~/.emacs.d move it out of the
   way and put this there instead.)
3. Launch Emacs!

If you find yourself missing some autoloads after an update (which
should manifest itself as "void function: foobar" errors) try M-x
regen-autoloads. After some updates an M-x recompile-init will be
necessary; this should be noted in the commit messages.

If you want to keep your regular ~/.emacs.d in place and just launch a
single instance using the starter kit, try the following invocation:

  $ emacs -q -l ~/src/emacs-starter-kit/init.el

Note that having a ~/.emacs file might override the starter kit
loading, so if you've having trouble loading it, make sure that file
is not present.

## Structure

The init.el file is where everything begins. It's the first file to
get loaded. The starter-kit-* files provide what I consider to be
better defaults, both for different programming languages and for
built-in Emacs features like bindings or registers.

Files that are pending submission to ELPA are bundled with the starter
kit under the directory elpa-to-submit/. The understanding is that
these are bundled just because nobody's gotten around to turning them
into packages, and the bundling of them is temporary. For these
libraries, autoloads will be generated and kept in the loaddefs.el
file. This allows them to be loaded on demand rather than at startup.

There are also a few files that are meant for code that doesn't belong
in the Starter Kit. First, the user-specific-config file is the file
named after your user with the extension ".el". In addition, if a
directory named after your user exists, it will be added to the
load-path, and any elisp files in it will be loaded. Finally, the
Starter Kit will look for a file named after the current hostname
ending in ".el" which will allow host-specific configuration. This is
where you should put code that you don't think would be useful to
everyone. That will allow you to merge with newer versions of the
starter-kit without conflicts.

## Packages

Libraries from [Marmalade](http://marmalade-repo.org) installed via
package.el are preferred when available since dependencies are handled
automatically, and the burden to update them is removed from the
user.

There's no vendor/ directory in the starter kit because if an external
library is useful enough to be bundled with the starter kit, it should
be useful enough to submit to Marmalade so that everyone can use it, not
just users of the starter kit.

## Variants of Emacs

The Starter Kit is designed to work with GNU Emacs version 22 or
greater. Using it with forks or other variants is not supported. It
probably won't work with XEmacs, though some have reported getting it
to work with Aquamacs. However, since Aquamacs is not portable,
it's difficult to test in it, and breakage is common.

## Contributing

If you know your way around Emacs, please try out the starter kit as a
replacement for your regular dotfiles for a while. If there's anything
you just can't live without, add it or let me know so I can add
it. Take a look at what happens in init.el to get started.

Also: see the file TODO. Helping submit new libraries to Marmalade is the
easiest way to help out. There are two ways you can do this: either
take new libraries and make them ready for Marmalade, dropping them in the
elpa-to-submit directory or take files that are already in
elpa-to-submit, ensuring all their dependencies are correctly loaded
into Marmalade, and uploading them. Grep the project for TODO for other things.

Files are licensed under the same license as Emacs unless otherwise
specified. See the file COPYING for details.

The latest version is at http://github.com/technomancy/emacs-starter-kit/

On Unix, /home/$USER/.emacs.d, on windows Documents and Settings/%your
user name%/Application Data
